/*
   Empresa : Consultoria en Informatica Ejecutiva
   Modulo  : Cuentas por Cobrar
   Sistema : ADOSA
   Programa: cxcd0015.p
   Funcion : Saca los dias en cartera de un cliente
   Autor   : LUIS
   Fecha   : 11/02/1998
*/

{/usr2/adosa/includes/sia00000.var}
DEF BUFFER b-Mov FOR MovCliente.
DEF INPUT PARAMETER l-cliente LIKE Cliente.Id-Cliente 			NO-UNDO.
DEF INPUT PARAMETER l-fecha	AS DATE FORMAT "99/99/9999"		NO-UNDO.
DEF OUTPUT PARAMETER l-diascartera AS DECI				NO-UNDO.
DEF VAR l-saldo 	AS DECI						NO-UNDO.
DEF VAR l-ant		AS INTE						NO-UNDO.

FOR EACH MovCliente WHERE MovCliente.Id-Cliente = l-cliente AND
			  MovCliente.FecReg    <= l-fecha AND
			  MovCliente.Id-MC     <= 3 AND
			  MovCliente.Afectado NO-LOCK:
    FOR EACH b-mov WHERE b-mov.refsaldo = movcliente.refsaldo AND
			 b-mov.id-mc > 3 AND
			 b-mov.afectado AND
			 b-mov.fecreg <= l-fecha NO-LOCK:
	    IF b-mov.Id-Mc <> 65 THEN DO:
	        FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento NO-LOCK NO-ERROR.
	        IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
        END.
	    ACCUMULATE b-mov.importe (TOTAL).
    END. /* del for each b-mov */
    ASSIGN l-saldo = MovCliente.Importe + (ACCUM TOTAL b-mov.importe)
	   l-ant   = l-fecha - movcliente.fecreg.
    ACCUMULATE l-saldo * l-ant (TOTAL).
    ACCUMULATE l-saldo (TOTAL).
END. /* for each movcliente */

ASSIGN l-diascartera = (ACCUM TOTAL l-saldo * l-ant) / (ACCUM TOTAL l-saldo).
IF l-diascartera = ? THEN ASSIGN l-diascartera = 0.
