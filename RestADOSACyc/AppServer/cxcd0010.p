/*
   Empresa : Consultoria en Informatica Ejecutiva
   Modulo  : Cuentas por Cobrar
   Sistema : ADOSA
   Programa: cxcd0010.p
   Funcion : Saca el promedio de pago de un cliente
   Autor   : LUIS
   Fecha   : 11/04/97
*/

/*{/usr2/adosa/includes/sia00000.var}*/
DEF BUFFER b-MovCte FOR MovCliente.
DEF BUFFER b-MovHistCte FOR HistMovCte.
DEF INPUT PARAMETER l-cliente LIKE Cliente.Id-Cliente NO-UNDO.
DEF OUTPUT PARAMETER l-prompag  AS INT               NO-UNDO.
DEF VAR l-fecha AS DATE                               NO-UNDO.
DEF VAR l-dias  AS INTE                               NO-UNDO.
DEF VAR l-mes   AS INTE                               NO-UNDO.
DEF VAR l-mes2  AS INTE                               NO-UNDO.

/*
ASSIGN l-mes   = MONTH(g-today) - 6
       l-mes2  = (IF l-mes <= 0 THEN 12 + l-mes ELSE 0)
       l-fecha = (IF l-mes2 <> 0
                              THEN DATE(l-mes2,DAY(g-today),YEAR(g-today) - 1)
                              ELSE DATE(l-mes,1,YEAR(g-today)) ).
*/
ASSIGN l-Fecha = TODAY - 180.
FOR EACH HistMovCte WHERE HistMovCte.Id-Cliente = l-cliente AND
                          HistMovCte.FecReg     >= l-fecha  AND
                          HistMovCte.Importe    < 0 NO-LOCK:
  FIND TipoPago WHERE TipoPago.Id-Tp = HistMovCte.Id-MC NO-LOCK NO-ERROR.
  IF NOT AVAILABLE TipoPago THEN NEXT.
  FIND FIRST b-MovHistCte WHERE b-MovHistCte.RefSaldo = HistMovCte.RefSaldo AND
                                b-MovHistCte.Id-MC   <= 3 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE b-MovHistCte THEN NEXT.
  ASSIGN l-dias = HistMovCte.FecReg - b-MovHistCte.FecReg.
  ACCUMULATE (HistMovCte.Importe * l-dias) (TOTAL).
  ACCUMULATE HistMovCte.Importe (TOTAL).
END.

FOR EACH MovCliente WHERE MovCliente.Id-Cliente = l-cliente AND
                          MovCliente.FecReg    >= l-fecha   AND
                          MovCliente.Importe    < 0 NO-LOCK:
  FIND TipoPago WHERE TipoPago.Id-Tp = MovCliente.Id-MC NO-LOCK NO-ERROR.
  IF NOT AVAILABLE TipoPago THEN NEXT.
  FIND FIRST b-MovCte WHERE b-MovCte.RefSaldo = MovCliente.RefSaldo AND
                            b-MovCte.Id-MC   <= 3 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE b-MovCte THEN NEXT.
  FIND Acuse WHERE Acuse.Id-Acuse = MovCliente.Documento NO-LOCK NO-ERROR.
  IF (AVAILABLE Acuse) AND Acuse.Estatus <> 4 THEN NEXT.
  ASSIGN l-dias = MovCliente.FecReg - b-MovCte.FecReg.
  ACCUMULATE (MovCliente.Importe * l-dias) (TOTAL).
  ACCUMULATE MovCliente.Importe (TOTAL).
END.

ASSIGN l-prompag = ((ACCUM TOTAL HistMovCte.Importe * l-dias) +
                    (ACCUM TOTAL Movcliente.Importe * l-dias)) /
                    ((ACCUM TOTAL HistMovCte.Importe) +
                    (ACCUM TOTAL MovCliente.Importe)) .
IF l-prompag = ? OR l-prompag < 0 THEN l-prompag = 0.
