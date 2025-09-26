/*
  Empresa : Consultoria en Informatica Ejecutiva S.A. de C.V.
  Programa: ausa0004.i
  Funcion : Crea Movimiento en MovCaja de NCR de contado
  Autor   : ALEX
  Fecha   : 15 de Julio del 2019
*/
   
{ifndef {&NoVar}}
DEF VAR l-reccaja AS RECID NO-UNDO.
{endif} */


FIND LAST CtlCaja WHERE CtlCaja.Id-Caja   = l-Id-Caja 
                    AND CtlCaja.Turno     = l-Turno
                    AND CtlCaja.FecCierre = ? NO-LOCK NO-ERROR.
IF NOT AVAILABLE CtlCaja THEN 
DO:
    FIND LAST b-CtlCaja WHERE b-CtlCaja.Id-Caja   = Caja.Id-Caja AND
        b-CtlCaja.FecCierre <> ? NO-LOCK NO-ERROR.

    CREATE CtlCaja.
    ASSIGN 
        CtlCaja.Id-Caja      = Caja.Id-Caja 
        CtlCaja.Turno        = 1
        CtlCaja.FecOper      = TODAY
        CtlCaja.FecApertura  = TODAY
        CtlCaja.HoraApertura = TIME
        CtlCaja.FolioIni     = (IF AVAILABLE b-CtlCaja THEN b-CtlCaja.FolioFin ELSE 0)
        CtlCaja.FolioFin     = CtlCaja.FolioIni
        CtlCaja.Dotacion     = 0.  
END.
    
ASSIGN 
    l-reccaja = RECID(CtlCaja).

FIND CtlCaja WHERE RECID(CtlCaja) = l-reccaja EXCLUSIVE-LOCK.        
 


/**********************************/
/* REGISTRA EL MOVIMIENTO EN CAJA */
/**********************************/

CREATE MovCaja.
ASSIGN 
    MovCaja.Id-Caja    = CtlCaja.Id-Caja
    MovCaja.Turno      = CtlCaja.Turno
    MovCaja.Id-Cliente = {&Cliente}
    MovCaja.FecReg     = TODAY 
    MovCaja.HorReg     = TIME
    MovCaja.Folio      = (IF CtlCaja.FolioFin + 1 >= 999999 THEN 0 ELSE CtlCaja.FolioFin + 1 )
    MovCaja.Referencia = {&Referencia}
    MovCaja.Id-NCR     = IF {&TipoVenta} <> 1 THEN {&Referencia} ELSE ""
    MovCaja.TipoVenta  = {&TipoVenta}
    MovCaja.TotVenta   = {&TotVenta}
    MovCaja.Estatus    = {&Estatus}
    MovCaja.FolioAut   = {&FolioAut}
    MovCaja.FecOper    = CtlCaja.FecOper
    MovCaja.FecDep     = CtlCaja.Fecoper
    MovCaja.Iniciales  = {&EmpIniciales}
    MovCaja.Id-Cajero  = {&Cajero}
    CtlCaja.FolioFin   = (IF CtlCaja.FolioFin + 1 >= 999999 THEN 0 ELSE CtlCaja.FolioFin + 1 ).
RELEASE CtlCaja.