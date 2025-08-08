@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
  Empresa : ADOSA
  Programa: limiteCreditoSug.p --  cxcc0100.p
  Funcion : Reporte de Limites de Creditos Sugeridos
  Autor   : 
  Fecha   : /LimiteCreditoSugerido
*/


DEF VAR l-CantCte AS INTE NO-UNDO.
DEF VAR l-Zona AS CHAR NO-UNDO EXTENT 3 INITIAL ['General','Local','Foraneo'].
DEF VAR l-IdxZona AS INTE NO-UNDO.
DEF VAR l-LogZona AS LOGI NO-UNDO.
DEF VAR l-IdxOpc AS INTE NO-UNDO.
DEF VAR l-LogOpc AS LOGI NO-UNDO.
DEF VAR l-Archivo AS CHAR NO-UNDO.
DEF VAR l-LisPrint AS CHAR NO-UNDO FORMAT 'X(50)'.
DEF VAR l-DiasCart AS INTE NO-UNDO FORMAT '-ZZ9'.
DEF VAR l-PromDias AS DECI NO-UNDO FORMAT '-Z9.9'.
DEF VAR l-i AS INTE NO-UNDO.
DEF VAR l-j AS INTE NO-UNDO.
DEF VAR l-NumMes AS INTE NO-UNDO.
DEF VAR l-Ventas AS DECI NO-UNDO.
DEF VAR l-LCSug AS DECI NO-UNDO.
/*
DEF VAR l-VtaMay AS DECI NO-UNDO.
*/
DEF VAR l-Saldo AS DECI NO-UNDO.

DEF TEMP-TABLE tEst NO-UNDO
  FIELD IdCliente LIKE Cliente.Id-Cliente
  FIELD Nombre AS CHAR FORMAT 'X(38)'
  FIELD IdZona LIKE Zona.Id-Zona
  FIELD IdVendedor LIKE Vendedor.Id-Vendedor
  FIELD NomVendedor LIKE empleado.Nombre
  FIELD IdResp LIKE Cliente.Id-Resp
  FIELD NomResp LIKE Resp.Nombre
  FIELD Plazo LIKE Cliente.Plazo
  FIELD DiasCart AS INTE FORMAT '->>9'
  FIELD PromDias AS DECI FORMAT '->9.9'
  FIELD CreditoAcumulado AS DECI FORMAT '->>>>,>>9'
  FIELD VtaCredPromPzo AS INTE FORMAT '->>>,>>9'
  FIELD Saldo LIKE MovCliente.Saldo
  FIELD Limite AS INTE FORMAT '->,>>>,>>9'
  FIELD LineaCreditoProm AS DECI
  FIELD LineaCreditoSug AS DECI
  INDEX Idx-Cte IS UNIQUE IdCliente
  INDEX Idx-Saldo Saldo DESCENDING
  INDEX Idx-Resp IdResp Saldo DESCENDING
  INDEX Idx-Vend IdVendedor Saldo DESCENDING Plazo Limite.  


/* Definir buffers para las tablas utilizadas */
DEF BUFFER bVendedor FOR Vendedor.
DEF BUFFER bEmpleado FOR Empleado.
DEF BUFFER bResp FOR Resp.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetLimiteCreditoSugerido:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER l-FecIni AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER l-FecFin AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER iclase      AS INTEGER NO-UNDO.  
DEFINE INPUT  PARAMETER iVendedor   AS CHARACTER NO-UNDO.  
DEFINE OUTPUT PARAMETER TABLE FOR tEst.

  /* Asignar valores por defecto si son ? */
ASSIGN 
    iclase    = IF iclase    = ? THEN 0 ELSE iclase
    iVendedor = IF iVendedor = ? OR iVendedor = "0" THEN "" ELSE iVendedor
    l-FecIni  = IF l-FecIni  = ? THEN TODAY - 30 ELSE l-FecIni
    l-FecFin  = IF l-FecFin  = ? THEN TODAY ELSE l-FecFin.    
  {cxcc0101.i}   

  PAUSE 0 BEFORE-HIDE.       
  FOR EACH EstCte WHERE EstCte.Anio >= YEAR(l-FecIni)
                    AND EstCte.Anio <= YEAR(l-FecFin) NO-LOCK,
      EACH Cliente OF EstCte NO-LOCK
          WHERE 
            (iclase = 0 OR Cliente.Id-ClaseCte = iclase) 
        AND (iVendedor = "" OR Cliente.Id-Vendedor = iVendedor):

    RUN /usr2/adosa/procs/cxcd0010.p (EstCte.Id-Cliente, OUTPUT l-PromDias).
    RUN /usr2/adosa/procs/cxcd0015.p (EstCte.Id-Cliente,TODAY, OUTPUT l-DiasCart).

    ASSIGN l-Saldo = 0.
    FOR EACH MovCliente OF Cliente WHERE MovCliente.Id-MC < 4 NO-LOCK:
      ACCUMULATE MovCliente.Saldo (TOTAL).
    END.
    ASSIGN l-Saldo = ACCUM TOTAL MovCliente.Saldo.

    FIND FIRST tEst WHERE tEst.IdCliente = Cliente.Id-Cliente
                     NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tEst THEN DO:
      CREATE tEst.
      ASSIGN tEst.IdCliente  = Cliente.Id-Cliente
             tEst.Nombre      = Cliente.RazonSocial
             tEst.IdZona     = Cliente.Id-Zona
             tEst.IdVendedor = Cliente.Id-Vendedor
             tEst.IdResp     = Cliente.Id-Resp
             tEst.Plazo       = Cliente.Plazo
             tEst.DiasCart    = l-DiasCart
             tEst.PromDias    = l-PromDias    
             tEst.Limite      = Cliente.Limite
             tEst.Saldo       = l-Saldo.
             
    /* Asignación del Nombre del Vendedor */
    FIND FIRST bVendedor WHERE bVendedor.Id-Vendedor = Cliente.Id-Vendedor NO-LOCK NO-ERROR.
    IF AVAILABLE bVendedor THEN DO:
      FIND FIRST bEmpleado WHERE bEmpleado.Iniciales = bVendedor.Iniciales NO-LOCK NO-ERROR.
      IF AVAILABLE bEmpleado THEN
        ASSIGN tEst.NomVendedor = bEmpleado.Nombre.
    END.

    /* Asignación del Nombre del Responsable */
    FIND FIRST bResp WHERE bResp.Id = Cliente.Id-Resp NO-LOCK NO-ERROR.
    IF AVAILABLE bResp THEN
      ASSIGN tEst.NomResp = bResp.Nombre.     
                   
      {cxcc0100.i}
      ASSIGN tEst.CreditoAcumulado   = l-Ventas
             tEst.VtaCredPromPzo     = (l-Ventas / l-NumMes) * (tEst.Plazo / 30)
             tEst.LineaCreditoProm   = IF tEst.VtaCredPromPzo = 0 THEN 0 ELSE Cliente.Limite / tEst.VtaCredPromPzo
             tEst.LineaCreditoSug    = IF tEst.VtaCredPromPzo <= 0 THEN 0 ELSE 
              ((ROUND(tEst.VtaCredPromPzo * 1.20 / 1000,0)) * 1000).
    END.
    ELSE DO:
      {cxcc0100.i}
      ASSIGN tEst.CreditoAcumulado         = tEst.CreditoAcumulado + l-Ventas
             tEst.VtaCredPromPzo         = tEst.CreditoAcumulado / l-NumMes
             tEst.VtaCredPromPzo         = (tEst.CreditoAcumulado / l-NumMes) * (tEst.Plazo / 30)
             tEst.LineaCreditoProm        = IF tEst.VtaCredPromPzo = 0 THEN 0
                                 ELSE Cliente.Limite / tEst.VtaCredPromPzo.
    END.
    
  END. /* for each */

RETURN.
END PROCEDURE.      
