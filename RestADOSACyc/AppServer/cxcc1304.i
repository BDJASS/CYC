/*
  Programa: cxcc1304.i
  Funcion : Para Acomodar el Analisis de Saldos
  Llamador: Analisis de Saldos
  Modificado: RNPC - 2019-07-22 - Ajustes para convertir dolares a pesos
*/

CREATE w-saldo.
ASSIGN w-saldo.Sec = Cliente.Id-Cliente.       
FOR EACH MovCliente WHERE MovCliente.FecReg <= l-fecha        AND
			 (MovCliente.Id-MC >= l-mcini AND
			  MovCliente.Id-MC <= l-mcfin)
                          {ifdef {&Cliente}} AND 
                                MovCliente.Id-Cliente = Cliente.Id-Cliente 
                          {endif} */ NO-LOCK
                BREAK BY MovCliente.Id-Cliente:    
    IF l-index2 = 1 THEN DO:       
       ASSIGN w-saldo.tty = g-tty.
       IF (l-fecha - MovCliente.FecReg) <= l-diascap AND 
           NOT LAST-OF(MovCliente.ID-Cliente) THEN NEXT. 
    END. /* si solo los que cumplen con los diascap */
   /* ELSE DO:
       IF NOT LAST-OF(MovCliente.Id-Cliente) THEN NEXT.
    END. /* si son todos los dias */ */
    FOR EACH b-mov WHERE b-mov.RefSaldo = MovCliente.RefSaldo         AND
                         b-mov.Id-MC > 3                         AND
                         b-mov.FecReg <= l-fecha NO-LOCK:
        IF b-mov.Id-Mc <> 65 THEN DO:
	   FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento no-lock no-error.
	   IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
        END.
        ACCUMULATE b-mov.Importe (TOTAL).
    END. /* del for each b-mov */
    ASSIGN l-saldo = MovCliente.Importe + (ACCUM TOTAL b-mov.Importe).
    
    IF Movcliente.Id-Moneda > 1 THEN        // RNPC - 2019-07-22 
        ASSIGN l-saldo = l-saldo * MovCliente.TipoCambio.

    IF l-diascap <> 0 THEN 
       IF (l-fecha - MovCliente.FecReg) > l-diascap THEN 
          ACCUMULATE l-saldo (TOTAL BY MovCliente.ID-cliente).
          
    IF l-diascap = 0 THEN 
       IF (l-fecha - MovCliente.FecVenc) > 0 THEN 
          ACCUMULATE l-saldo (TOTAL BY MovCliente.Id-Cliente).
    
    IF LAST-OF (MovCliente.Id-Cliente) AND 
       ((ACCUM TOTAL BY MovCliente.Id-Cliente l-saldo) > 0 OR
         l-index2 = 2) THEN DO:
        FIND FIRST w-saldo WHERE w-saldo.Sec = MovCliente.Id-Cliente EXCLUSIVE.
        IF NOT AVAILABLE w-saldo THEN CREATE w-Saldo.
        ASSIGN w-Saldo.Saldo = (ACCUM TOTAL BY MovCliente.Id-cliente l-saldo)
               w-Saldo.TTy   = g-tty
               w-Saldo.Sec   = MovCliente.Id-Cliente
               w-Saldo.Doc   = "0".
               
        {ifdef {&Break}} 
            IF l-indice = 2 THEN 
                ASSIGN w-Saldo.Doc = STRING({&Break}).
            ELSE ASSIGN w-Saldo.Doc = "0".
        {endif} */
        IF l-index = 1 THEN DO: /* por importe */
           ASSIGN w-saldo.Acomodo2 = string(w-saldo.Saldo,"-999,999,999.99")                  
                  w-saldo.Acomodo  = w-saldo.Acomodo2
                  w-saldo.Acomodo3 = w-saldo.Saldo.
        END. /* por importe */
        ELSE DO: /* por alfabetico */
             {ifdef {&Busca}} 
                FIND Cliente OF MovCliente NO-LOCK NO-ERROR.
             {endif} */
           ASSIGN w-Saldo.Acomodo2 = Cliente.RazonSocial
                  w-saldo.Acomodo  = w-Saldo.Acomodo2.
        END. /* si es alfabetico */ 
    END. /* del last-of Movcliente */
END. /* del for each Movcliente */.
/*IF l-index2 = 1 THEN 
   FOR EACH w-saldo WHERE w-saldo.saldo = 0 EXCLUSIVE.
      DELETE w-saldo.
   END.*/
ASSIGN l-totcli = 0.
