/*
  Empresa : Consultoria en Informatica Ejecutiva
  Sistema : ADOSA
  Modulo  : Cuentas por Pagar
  Programa: cxcc1306.i
  Funcion : saca el saldo final de cte.
  Autor   : SAUL
  Fecha   : 22-ENE-2010
*/
ASSIGN l-saldo = 0
       l-pagsem = 0.
FOR EACH bb-mov WHERE bb-mov.id-cliente = w-saldo.sec and
                      bb-mov.id-mc >= l-mcini AND
                      bb-mov.id-mc <= l-mcfin AND
                      bb-mov.fecreg <= l-fecha NO-LOCK BREAK BY bb-mov.id-cliente:
    FOR EACH b-mov WHERE b-mov.refsaldo = bb-mov.RefSaldo    AND
                         b-mov.Id-MC    > 3                  AND
                         b-mov.FecReg  <= l-fecha            AND
                         b-mov.afectado NO-LOCK:
        IF b-mov.Id-Mc <> 65 THEN DO:
	        FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento NO-LOCK NO-ERROR.
	        IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
        END.
        ACCUMULATE b-mov.importe (TOTAL).
    END. /* del b-mov */
    /* RNPC - 2019-07-23 - 
        ASSIGN l-saldo = bb-mov.Importe + (ACCUM TOTAL b-mov.Importe)    
           l-pagsem  = l-pagsem + l-saldo.*/
           
    ASSIGN l-saldo = bb-mov.Importe + (ACCUM TOTAL b-mov.Importe).
           
    IF bb-mov.Id-Moneda > 1 THEN        // RNPC - 2019-07-23 
        ASSIGN l-saldo = (l-saldo) * bb-mov.TipoCambio.
    
    ASSIGN l-pagsem  = l-pagsem + l-saldo.
    
END. /* del bb-mov */
