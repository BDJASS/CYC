/*
  Empresa : Consultoria en Informatica Ejecutiva S.A. de C.V.
  Programa: ausa0002.i
  Funcion : Crea Movimiento en MovCaja
  Autor   : LUIS
  Fecha   : 25/06/1997
*/

{ifndef {&NoVar}}
   DEF VAR l-reccaja AS RECID NO-UNDO.
{endif} */

FIND LAST CtlCaja WHERE CtlCaja.Id-Caja   = Caja.Id-Caja AND
                        CtlCaja.FecCierre = ? NO-LOCK NO-ERROR.
ASSIGN l-reccaja = RECID(CtlCaja).

FIND CtlCaja WHERE RECID(CtlCaja) = l-reccaja EXCLUSIVE-LOCK.

/**********************************/
/* REGISTRA EL MOVIMIENTO EN CAJA */
/**********************************/

CREATE MovCaja.
ASSIGN MovCaja.Id-Caja     = CtlCaja.Id-Caja
       MovCaja.Turno       = CtlCaja.Turno
       MovCaja.Id-Cliente  = {&Cliente}
       MovCaja.FecReg      = TODAY //g-today
       MovCaja.Folio       = (IF CtlCaja.FolioFin + 1 >= 999999 THEN 0 ELSE CtlCaja.FolioFin + 1 )
       MovCaja.Referencia  = {&Referencia}
       MovCaja.TipoVenta   = {&TipoVenta}
       MovCaja.TotVenta    = {&TotVenta}
       MovCaja.Estatus     = {&Estatus}
       MovCaja.FolioAut    = {&FolioAut}
       MovCaja.FecOper     = CtlCaja.FecOper
       MovCaja.FecDep      = CtlCaja.Fecoper
       MovCaja.Iniciales   = {&EmpIniciales}
       MovCaja.Id-Cajero   = {&Cajero}
       CtlCaja.FolioFin    = (IF CtlCaja.FolioFin + 1 >= 999999 THEN 0 ELSE CtlCaja.FolioFin + 1 ).
