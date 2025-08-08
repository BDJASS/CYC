@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
  Programa : concacuse2.p
  Funcion  : PGenera ligas de pago y guarda informacion  (generacion de Preacuses)
  Autor    : Sis10-FLC
  Fecha    : 8 ENE 2025
*/

DEF TEMP-TABLE ttDocto
    FIELD IdUser    AS CHAR
    FIELD IdCliente LIKE PreAcuse.Id-Cliente
    FIELD IdDoc     LIKE PreDocAcuse.Documento
    FIELD FecReg    LIKE Factura.FecReg
    FIELD RazonSocial LIKE Factura.RazonSocial
    FIELD Total       AS DECIMAL
    FIELD SubTotal    LIKE Factura.SubTotal
    FIELD TDoc      AS CHAR
    FIELD ImpPago   AS DECIMAL
    FIELD ImpAnt    AS DECIMAL
    FIELD Desc1     AS DECIMAL
    FIELD Desc2     AS DECIMAL
    FIELD Desc3     AS DECIMAL
    FIELD Desc4     AS DECIMAL
    FIELD Desc5     AS DECIMAL
    FIELD Desc6     AS DECIMAL
    FIELD Desc7     AS DECIMAL
    FIELD Desc8     AS DECIMAL
    FIELD Desc9     AS DECIMAL
    FIELD Desc10    AS DECIMAL
    FIELD Desc11    AS DECIMAL
    FIELD Desc12    AS DECIMAL
    INDEX Idx-Def TDoc IdDoc.
    
DEF TEMP-TABLE ttAnticipos
    FIELD IdAnticipo  LIKE Anticipo.Id-Anticipo
    FIELD ImpAplicado LIKE DetAnticipo.Importe
    FIELD IdDoc       LIKE PreDocAcuse.Documento
    INDEX Idx-Def IdDoc.

DEFINE DATASET dsPreAcuse FOR 
    ttDocto,
    ttAnticipos
    DATA-RELATION drDoctoAnticipos FOR ttDocto, ttAnticipos
        RELATION-FIELDS (IdDoc, IdDoc)
        NESTED.
            


DEFINE TEMP-TABLE ttLigas NO-UNDO
    FIELD Fecha         AS DATE
    FIELD Cliente       AS CHARACTER
    FIELD Monto         AS DECIMAL
    FIELD Liga          AS CHARACTER
    FIELD FecVencimiento AS DATE
    FIELD IdRefer       AS CHAR
    FIELD Pagado        AS LOGICAL
    FIELD Auth          LIKE MITResp.RAuth
    FIELD IdUser        AS CHAR
    FIELD IdAcuse       AS CHAR.
    
DEFINE TEMP-TABLE ttLigasDetalle NO-UNDO
    FIELD Documento     AS CHARACTER
    FIELD Monto         AS DECIMAL
    FIELD Descuentos    AS DECIMAL
    FIELD Anticipos AS DECIMAL
    FIELD IdRefer       AS CHAR.  

DEFINE DATASET dsPreAcuseGet FOR 
    ttLigas, 
    ttLigasDetalle
    DATA-RELATION drPreAcuse FOR ttLigas, ttLigasDetalle
        RELATION-FIELDS (IdRefer, IdRefer)
        NESTED.  
    
/* **********************  Internal Procedures  *********************** */

/* Procedimiento REST */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetLigaPago:

DEFINE INPUT PARAMETER ip-FechaInicio AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ip-FechaFin AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ip-Cliente AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsPreAcuseGet.


// RUN GenerarLigasPago.
/* Consulta de ligas de pago */

IF ip-Cliente = ? THEN ip-Cliente = 0.
FOR EACH LigaPago NO-LOCK 
     WHERE (ip-FechaInicio = ? OR LigaPago.FecReg >= ip-FechaInicio)
      AND (ip-FechaFin = ? OR LigaPago.FecReg <= ip-FechaFin):

    FIND FIRST PreAcuse NO-LOCK 
        WHERE PreAcuse.Id-Acuse = LigaPago.Refer NO-ERROR.

    IF NOT AVAILABLE PreAcuse THEN NEXT.

    /* Filtrar por cliente si se especifica */
    IF ip-Cliente > 0 AND PreAcuse.Id-Cliente <> ip-Cliente THEN NEXT.

    FIND FIRST Cliente NO-LOCK 
        WHERE Cliente.Id-Cliente = PreAcuse.Id-Cliente NO-ERROR.
    
    FIND FIRST MITResp WHERE MITResp.Reference = LigaPago.Refer 
                         AND MITResp.Response = "approved" NO-LOCK NO-ERROR. 
    /* Sumar todos los montos de PreDocAcuse asociados */
    DEFINE VARIABLE totalMonto AS DECIMAL NO-UNDO INITIAL 0.

    FOR EACH PreDocAcuse NO-LOCK 
        WHERE PreDocAcuse.Id-Acuse = PreAcuse.Id-Acuse:
        totalMonto = totalMonto + PreDocAcuse.ImpPago.
    END.

    CREATE ttLigas.
    ASSIGN 
        ttLigas.Fecha         = LigaPago.FecReg
        ttLigas.Cliente       = IF AVAILABLE Cliente THEN Cliente.RazonSocial ELSE "Desconocido"
        ttLigas.Monto         = 0
        ttLigas.Liga          = LigaPago.Liga
        ttLigas.FecVencimiento = LigaPago.FecReg + 3  /* Supongamos 3 días de vencimiento */
        ttLigas.IdRefer        = LigaPago.Refer
        ttLigas.Pagado         = LigaPago.Pagado
        ttLigas.Auth           = IF AVAILABLE MITResp THEN MITResp.RAuth ELSE "" 
        ttLigas.IdUser         = IF AVAILABLE PreAcuse THEN PreAcuse.UsuarioReg ELSE ""
        ttLigas.IdAcuse        = LigaPago.Id-Acuse.
               
    /* Recorrer cada documento de PreDocAcuse asociado a PreAcuse */
    FOR EACH PreDocAcuse NO-LOCK 
        WHERE PreDocAcuse.Id-Acuse = PreAcuse.Id-Acuse:

        /* Calcular total de descuentos */
        DEFINE VARIABLE totalDescuento AS DECIMAL NO-UNDO.
        totalDescuento = PreDocAcuse.ImpDescPP +
                         PreDocAcuse.ImpDescEsp +
                         PreDocAcuse.MDDesc +
                         PreDocAcuse.FMDesc +
                         PreDocAcuse.MRDesc +
                         PreDocAcuse.UPDesc +
                         PreDocAcuse.ManDesc +
                         PreDocAcuse.DPDesc +
                         PreDocAcuse.MTDesc +
                         PreDocAcuse.RebDesc +
                         PreDocAcuse.MenorDesc +
                         PreDocAcuse.CBDesc.

        /* Crear registro de detalle */
        CREATE ttLigasDetalle.   
        ASSIGN 
            ttLigasDetalle.IdRefer    = LigaPago.Refer
            ttLigasDetalle.Documento  = PreDocAcuse.Documento
            ttLigasDetalle.Monto    = PreDocAcuse.ImpPago
            ttLigasDetalle.Descuentos = totalDescuento
            ttLigasDetalle.Anticipos   = PreDocAcuse.AntApl.

        /* Sumar al total del encabezado */
        ttLigas.Monto = ttLigas.Monto + PreDocAcuse.ImpPago.
    END.
END.
   
/* Retornar el DATASET */
RETURN.
   

END PROCEDURE.


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostLigaPago:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER DATASET FOR dsPreAcuse.
DEF OUTPUT PARAMETER p-PreAcuse AS CHAR NO-UNDO.
    

DEF VAR ip-formapago AS CHAR NO-UNDO.
DEF VAR l-Veces     AS INTEGER NO-UNDO.
DEF VAR l-TP        AS INTEGER NO-UNDO.
DEF VAR l-recmov    AS RECID NO-UNDO.
DEF VAR l-FecVence  AS DATE NO-UNDO.
DEF VAR l-Ubic      LIKE MovCliente.Id-Ubic NO-UNDO.

DEF VAR l-folAcuse  LIKE Acuse.Id-Acuse NO-UNDO INITIAL "".
DEF VAR l-AntApl    AS DECIMAL NO-UNDO.
DEF VAR l-FecDep    AS DATE NO-UNDO.
DEF VAR l-UsuApl    AS CHAR NO-UNDO INITIAL "".
DEF VAR meses       AS CHARACTER EXTENT 12 NO-UNDO INITIAL 
    ["ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC"].

/*       */
FIND Folio WHERE Folio.Id-Doc = "PREACU" AND Folio.Id-Alm = "P" 
             EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE Folio THEN DO:
    CREATE Folio.
    ASSIGN Folio.Id-Doc  = "PREACU"
           Folio.Id-Alm  = "P"
           Folio.Prefijo = "P"
           Folio.Folio   = 1.
END.


/* Modificar la generación de ID para que sea más robusta */
REPEAT:
    ASSIGN l-folacuse = STRING(Folio.Folio,"9999999") + TRIM(Folio.PreFijo)
           l-folacuse = SUBSTRING(l-folacuse,LENGTH(l-folacuse) - 6,7).
           
    FIND PreAcuse WHERE PreAcuse.Id-Acuse = l-folacuse NO-LOCK NO-ERROR.
    IF NOT AVAILABLE PreAcuse THEN LEAVE.
    
    /* Si existe, incrementar el folio y volver a intentar */
    ASSIGN Folio.Folio = Folio.Folio + 1.
END.
/*
ASSIGN l-folacuse     = STRING(Folio.Folio,"9999999") + TRIM(Folio.PreFijo)
       l-folacuse     = SUBSTRING(l-folacuse,LENGTH(l-folacuse) - 6,7)
       Folio.Folio    = Folio.Folio + 1.
*/ 



FIND FIRST ttDocto WHERE ttDocto.IdCliente > 0 NO-LOCK NO-ERROR.
IF AVAILABLE ttDocto THEN l-UsuApl = ttDocto.IdUser.

l-FecDep = TODAY.


FIND Usuario WHERE Usuario.Id-User = l-UsuApl NO-LOCK NO-ERROR.
CREATE PreAcuse.
ASSIGN PreAcuse.Id-Acuse   = l-FolAcuse
       PreAcuse.FecReg     = TODAY
       PreAcuse.UsuarioReg = l-UsuApl
       PreAcuse.Estatus    = 4
       PreAcuse.Id-Cliente = IF AVAILABLE ttDocto THEN ttDocto.IdCliente ELSE 3.
          
l-veces = 1.
FOR EACH ttDocto WHERE ttDocto.IdCliente > 0 AND ttDocto.IdDoc <> "" AND ttDocto.TDoc = "PREACU" 
    NO-LOCK BY ttDocto.IdDoc:
    FIND Factura WHERE Factura.Id-Factura = ttDocto.IdDoc NO-LOCK NO-ERROR.
    l-AntApl = 0.
    FOR EACH ttAnticipos WHERE ttAnticipos.IdDoc = ttDocto.IdDoc NO-LOCK:
        l-AntApl = ttAnticipos.ImpAplicado.
    END.
    CREATE PreDocAcuse.
    ASSIGN
        PreDocAcuse.Id-Acuse   = l-folacuse
        PreDocAcuse.Documento  = ttDocto.IdDoc  
        PreDocAcuse.FecDoc     = Factura.FecReg
        PreDocAcuse.Id-MC      = 1
        PreDocAcuse.Sec        = l-veces
        PreDocAcuse.Id-Moneda  = Factura.Id-Moneda
        PreDocAcuse.TipoCambio = Factura.TipoCambio
        PreDocAcuse.ImpPago    = ttDocto.ImpPago // importe pago
        PreDocAcuse.ImpDescPP  = ttDocto.Desc9
        PreDocAcuse.ImpDescEsp = ttDocto.Desc10
        PreDocAcuse.MDDesc     = ttDocto.Desc1
        PreDocAcuse.FMDesc     = ttDocto.Desc2
        PreDocAcuse.MRDesc     = ttDocto.Desc3
        PreDocAcuse.UPDesc     = ttDocto.Desc4
        PreDocAcuse.ManDesc    = ttDocto.Desc5
        PreDocAcuse.DPDesc     = ttDocto.Desc6
        PreDocAcuse.MTDesc     = ttDocto.Desc7
        PreDocAcuse.RebDesc    = ttDocto.Desc8
        PreDocAcuse.MenorDesc  = ttDocto.Desc11
        PreDocAcuse.CBDesc     = ttDocto.Desc12
        PreDocAcuse.AntApl     = l-AntApl.  // anticipos

    l-veces = l-veces + 1.
END.

ASSIGN p-PreAcuse = l-FolAcuse.

 IF AVAILABLE Folio THEN RELEASE Folio.
RETURN.

END PROCEDURE.

/*
PROCEDURE GenerarLigasPago:
    DEFINE VARIABLE v-HoraActual AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-LigaAleatoria AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-RandomNum AS INTEGER NO-UNDO.

    /* Obtener la hora actual en formato HHMM */
    ASSIGN v-HoraActual = TIME / 60.

    /* Recorrer registros en PreAcuse */
    FOR EACH PreAcuse NO-LOCK 
        WHERE PreAcuse.Estatus = 4
          AND PreAcuse.FecReg = TODAY:

        /* Verificar si ya existe en LigaPago */
        FIND FIRST LigaPago NO-LOCK 
            WHERE LigaPago.Refer = PreAcuse.Id-Acuse NO-ERROR.

        /* Si ya existe, continuar con el siguiente registro */
        IF AVAILABLE LigaPago THEN NEXT.

        /* Generar número aleatorio de 3 dígitos */
        v-RandomNum = INTEGER(RANDOM(100, 999)).

        /* Generar la URL de la liga */
        v-LigaAleatoria = "www.adosa.com.mx/liga-" + STRING(v-RandomNum, "999").

        /* Crear registro en LigaPago */
        CREATE LigaPago.
        ASSIGN
            LigaPago.Refer   = PreAcuse.Id-Acuse
            LigaPago.FecReg  = TODAY
            LigaPago.HorReg  = v-HoraActual
            LigaPago.TipoDocto = 3
            LigaPago.Liga    = v-LigaAleatoria.
    END.

END PROCEDURE.
*/
