/*
  Empresa : Consultoria en Informatica Ejecutiva
  Sistema : ADOSA
  Modulo  : Cuentas por Pagar
  Programa: cxcc1302.i
  Funcion : Impresion del programa cxcc1300.p
  Autor   : LUIS
  Fecha   : 05/12/96
*/

    {ciestatu.i &Campo = w-saldo.sec &Mensaje = "Cliente:"
               &Stream = s-salida}
    VIEW STREAM s-salida FRAME f-pie.

    /* PARA SACAR UN SALDO INICIAL */
    FOR EACH MovCliente WHERE MovCliente.Id-Cliente = w-Saldo.Sec AND
                              MovCliente.FecReg < l-fechaini      AND
                              Movcliente.id-mc >= l-mcini         AND
                              MovCliente.Id-Mc <= l-mcfin NO-LOCK
        USE-INDEX idx-mov:
        FOR EACH b-mov WHERE b-mov.refsaldo = MovCliente.RefSaldo AND
                             b-mov.Id-MC    > 3                   AND
                             b-mov.FecReg  < l-fechaini           AND
                             b-mov.afectado        NO-LOCK:
            IF b-mov.Id-Mc <> 65 THEN DO:
                FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento NO-LOCK NO-ERROR.
                IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
            END.
            ACCUMULATE b-mov.importe (TOTAL).
        END.
        ASSIGN l-saldo = MovCliente.Importe + (ACCUM TOTAL b-mov.Importe).
        
        IF Movcliente.Id-Moneda > 1 THEN        // RNPC - 2019-07-23 
            ASSIGN l-saldo = l-saldo * MovCliente.TipoCambio
                   l-simbolo = "US".
        
        ACCUMULATE l-saldo (TOTAL).  
        ASSIGN l-saldocal  = l-saldocal + l-saldo.
    END. /* del movcliente */    
    FOR EACH HistMovCte WHERE HistMovCte.Id-Cliente = w-Saldo.Sec AND
                              HistMovCte.FecReg < l-fechaini      AND
                              HistMovCte.Id-MC >= l-mcini         AND
                              HistMovCte.Id-MC <= l-mcfin NO-LOCK
        USE-INDEX idx-mov:
        FOR EACH b-hist WHERE b-hist.refsaldo = HistMovCte.RefSaldo AND
                              b-hist.Id-MC    > 3                   AND
                              b-hist.FecReg   < l-fechaini          AND
                              b-hist.afectado  NO-LOCK:
            IF b-hist.Id-Mc <> 65 THEN DO:
                FIND Acuse WHERE Acuse.Id-Acuse = b-hist.documento 
                NO-LOCK NO-ERROR.
                IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
            END.
            ACCUMULATE b-hist.importe (TOTAL).
        END.
        ASSIGN l-saldo = HistMovcte.Importe + (ACCUM TOTAL b-hist.Importe).
        
        IF HistMovCte.Id-Moneda > 1 THEN        // RNPC - 2019-07-23 
            ASSIGN l-saldo = l-saldo * HistMovCte.TipoCambio
                   l-simbolo = "US".
        
        ACCUMULATE l-saldo (TOTAL).  
        ASSIGN l-saldocal  = l-saldocal + l-saldo.
    END. /* del HistMovCte */

    IF /* LAST-OF(STRING(MovCliente.Id-Cliente)) */ TRUE THEN DO:
        FOR EACH CheDev WHERE CheDev.Id-Cliente = w-saldo.sec NO-LOCK:
            ACCUMULATE CheDev.FecCargo (COUNT).
        END.
        /* para sacar las compras */
        FOR EACH Factura WHERE Factura.Id-Cliente = w-saldo.sec AND
                               Factura.FecReg >= l-fechaini     AND
                               Factura.FecReg <= l-fecha        AND
                               Factura.FecCancel = ? NO-LOCK.        
            IF Factura.Id-Moneda > 1 THEN        // RNPC - 2019-07-25 
                ASSIGN l-vtames = l-vtames + (Factura.Tot * Factura.TipoCambio)
                       l-simbolo = "US".
            ELSE
                ASSIGN l-vtames = l-vtames + Factura.Tot.        
        END. /* del for each factura */

        /* para sacar los cargos */
        FOR EACH MovCliente WHERE MovCliente.Id-Cliente = w-saldo.sec AND
                                  MovCliente.FecReg >= l-fechaini     AND
                                  MovCliente.FecReg <= l-fecha        AND
                                  MovCliente.Id-MC <= l-mcfin         AND
                                  MovCliente.Id-MC >= l-mcini         NO-LOCK:
            IF MovCliente.Id-MC = 1 THEN NEXT.
            ASSIGN l-cargos = l-cargos + MovCliente.Importe.
        END. /* del for each factura */

        /* para sacar los pagos y creditos */
        FOR EACH b-mov WHERE b-mov.Id-Cliente = w-saldo.sec AND
                             b-mov.Id-Mc > 3                AND
                             b-mov.fecreg >= l-fechaini     AND
                             b-mov.fecreg <= l-fecha 
                             NO-LOCK:
            FIND TabMC WHERE TabMc.Id-MC = b-mov.Id-MC 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE TabMC THEN NEXT.
            IF TabMC.Sentido THEN NEXT.
        
            IF b-mov.Id-Mc <> 65 THEN DO:
	           FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento NO-LOCK NO-ERROR.
	           IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
            END.
            ELSE RELEASE Acuse.
        
            IF AVAILABLE Acuse AND b-mov.id-mc <> 90 THEN DO:
   	            FIND FIRST DocAcuse WHERE DocAcuse.Id-Acuse = Acuse.Id-Acuse AND
				                          DocAcuse.Id-MC >= l-mcini AND
				                          DocAcuse.Id-Mc <= l-mcfin 
                                          NO-LOCK NO-ERROR.
	           IF NOT AVAILABLE Docacuse THEN NEXT.
            END.
        
            // RNPC - 2019-07-26
            IF b-mov.id-moneda > 1 THEN
                FIND FIRST MovCliente WHERE MovCliente.RefSaldo = b-mov.RefSaldo AND
                                      MovCliente.Id-MC    <= 3              AND
                                      MovCliente.Afectado                  AND
                                      MovCliente.FecReg  <= l-fecha  NO-LOCK NO-ERROR. 
            
            IF b-mov.id-mc <= 62 OR b-mov.id-mc = 90 THEN DO:
                IF b-mov.id-moneda > 1 AND AVAILABLE MovCliente THEN
                    ASSIGN l-pagmes = l-pagmes + ((b-mov.importe * MovCliente.TipoCambio) * -1). 
                ELSE 
                    ASSIGN l-pagmes = l-pagmes + (b-mov.importe * -1).
            END.
            ELSE DO:
                IF b-mov.id-moneda > 1 AND AVAILABLE MovCliente THEN
                    ASSIGN l-vtasem = l-vtasem + ((b-mov.importe * MovCliente.TipoCambio) * -1).
                ELSE
                    ASSIGN l-vtasem = l-vtasem + (b-mov.importe * -1).
            END.
        END. /* del b-mov */

        FOR EACH HistMovCte WHERE HistMovCte.Id-Cliente = w-saldo.sec AND
                                  HistMovCte.Id-Mc  > 3               AND
                                  HistMovCte.fecreg >= l-fechaini     AND
                                  HistMovCte.fecreg <= l-fecha 
                                  NO-LOCK:
            FIND TabMC WHERE TabMc.Id-MC = HistMovCte.Id-MC NO-LOCK NO-ERROR.
            IF NOT AVAILABLE TabMC THEN NEXT.
            IF TabMC.Sentido THEN NEXT.

            IF HistMovCte.Id-Mc <> 65 THEN DO:
                FIND Acuse WHERE Acuse.Id-Acuse = HistMovcte.documento 
                NO-LOCK NO-ERROR.
	   
                IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
            END.
            ELSE RELEASE Acuse.
        
            IF AVAILABLE Acuse THEN DO:
	           FIND FIRST DocAcuse WHERE DocAcuse.Id-Acuse = Acuse.Id-Acuse AND
				                          DocAcuse.Id-MC >= l-mcini AND
				                        DocAcuse.Id-Mc <= l-mcfin 
                                        NO-LOCK NO-ERROR.
	           IF NOT AVAILABLE Docacuse THEN NEXT.
            END.
        
            // RNPC - 2019-07-26
            IF HistMovCte.id-moneda > 1 THEN
                FIND FIRST b-hist WHERE b-hist.RefSaldo = HistMovCte.RefSaldo AND
                                        b-hist.Id-MC    <= 3              AND
                                        b-hist.Afectado                  AND
                                        b-hist.FecReg  <= l-fecha  NO-LOCK NO-ERROR.
    
            IF HistMovCte.id-mc <= 62 OR HistMovCte.Id-Mc = 90 THEN DO: 
                IF HistMovCte.id-moneda > 1 AND AVAILABLE b-hist THEN
                    ASSIGN l-pagmes = l-pagmes + ((HistMovCte.importe * b-hist.TipoCambio) * -1).
                ELSE
                    ASSIGN l-pagmes = l-pagmes + (HistMovCte.importe * -1).
            END.
            ELSE DO:
                IF HistMovCte.id-moneda > 1 AND AVAILABLE b-hist THEN
                    ASSIGN l-vtasem = l-vtasem + ((HistMovCte.importe * b-hist.TipoCambio) * -1). 
                ELSE
                    ASSIGN l-vtasem = l-vtasem + (HistMovCte.importe * -1).
            END.
        END. /* del HistMovCte */

         /*--------------------------------------------------------------------------------- */
         v-anticipo = 0.     
         FOR EACH anticipo WHERE Anticipo.ImpAnticipo - Anticipo.ImpAplicado - Anticipo.ImpContado - Anticipo.ImpDevuelto > 0 AND 
                                 NOT Anticipo.Canc AND 
                                 Anticipo.Id-Cliente = w-saldo.sec
                                 NO-LOCK:
             v-anticipo = v-anticipo + 
                            (anticipo.impanticipo - anticipo.impaplicado - anticipo.impdevuelto - anticipo.impcontado).                         
         END.

          /* para sacar el saldo final y > 90 dias */
         FOR EACH bb-mov WHERE bb-mov.id-cliente = w-saldo.sec and
                               bb-mov.id-mc >= l-mcini AND
                               bb-mov.id-mc <= l-mcfin AND
                               bb-mov.fecreg <= l-fecha NO-LOCK 
                          BREAK BY bb-mov.id-cliente:
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
             ASSIGN l-saldo = bb-mov.Importe + (ACCUM TOTAL b-mov.Importe).
             
             IF bb-mov.Id-Moneda > 1 THEN        // RNPC - 2019-07-22 
                ASSIGN l-saldo = l-saldo * bb-mov.TipoCambio.
             
             IF bb-mov.Id-MC = 2 AND l-saldo > 0 THEN
                ASSIGN l-nc = l-nc + 1.
             IF bb-mov.Id-MC = 3 AND l-saldo > 0 THEN
                ASSIGN l-chedev = l-chedev + 1.
             IF l-diascap <> 0 THEN 
                ASSIGN l-dias      = l-fecha - bb-mov.FecReg
                       l-90dias    = l-90dias + (IF l-dias > l-diascap THEN l-saldo 
                                                                    ELSE 0)
                       l-pagsem  = l-pagsem + l-saldo.
             ELSE ASSIGN l-dias   = l-fecha - bb-mov.fecvenc
                         l-90dias = l-90dias + (IF l-dias > 0 THEN l-saldo ELSE 0)
                         l-pagsem = l-pagsem + l-saldo.
             ASSIGN l-dias = l-fecha - bb-mov.fecreg.
             IF l-dias > 0 THEN
               ACCUMULATE l-dias * l-saldo (TOTAL).
         END. /* del bb-mov */
         
         /*Para sacar los cheques no depositados*/
         ASSIGN l-sumanodep = 0.
         FOR EACH Acuse WHERE Acuse.Id-cliente = w-Saldo.Sec
                         AND Acuse.Estatus < 3 NO-LOCK:
            ASSIGN l-Importe = 0.
            FOR EACH Pagoacuse OF acuse NO-LOCK:
                ASSIGN l-importe = l-importe + pagoacuse.importe.
            END.
            IF l-Importe > 0 THEN DO:
                ASSIGN l-SumaNoDep = l-SumaNoDep + l-Importe.
            END.
         END. /**/
         IF l-pagsem <> 0 THEN DO:  
            ASSIGN l-diascartera = (ACCUM TOTAL l-dias * l-saldo) / l-pagsem
                   l-tsalxant  = l-tsalxant + (ACCUM TOTAL l-dias * l-saldo)
                   l-tsaltotal = l-tsaltotal + l-pagsem.
         END.
         ASSIGN      
                l-entro       = TRUE
                l-totcli      = l-totcli + 1
                l-saldo       = l-saldocal
                l-saldot      = l-saldot + l-saldo
                l-saldocal    = 0
                l-90diast     = l-90diast + l-90dias
                l-vtamest     = l-vtamest + l-vtames
                l-pagmest     = l-pagmest + l-pagmes
                l-cargost     = l-cargost + l-Cargos
                l-vtasemt     = l-vtasemt + l-vtasem
                l-pagsemt     = l-pagsemt + l-pagsem
                l-vtaanot     = l-vtaanot + l-vtaano
                l-paganot     = l-paganot + l-pagano.
         ACCUMULATE l-diascartera (AVERAGE).
         IF l-diascartera < 0 THEN ASSIGN l-diascartera = 0.
          /* para los totales del saldo POR hoja */      
         ASSIGN l-saldocalh = l-saldocalh + l-saldo
                l-90diash   = l-90diash + l-90dias
                l-vtamesh   = l-vtamesh + l-vtames
                l-pagmesh   = l-pagmesh + l-pagmes
                l-cargosh   = l-cargosh + l-cargos
                l-vtasemh   = l-vtasemh + l-vtasem
                l-pagsemh   = l-pagsemh + l-pagsem
                l-vtaanoh   = l-vtaanoh + l-vtaano
                l-paganoh   = l-paganoh + l-pagano
                l-totclih   = l-totclih + 1
                l-diascarterah = l-diascarterah + l-diascartera.
           /*IF l-diascartera < 0 THEN ASSIGN l-diascartera = 0.*/
    
    
         /* saca el promedio de pago */
         RUN cxcd0010.p (INPUT w-saldo.sec, OUTPUT l-prompag). /*para un cliente*/
         RUN cxcd0012.p (INPUT w-saldo.sec, OUTPUT l-impxant, OUTPUT l-imptotal).  /*para ponderados*/
         ASSIGN l-timpxant = l-timpxant + l-impxant
                l-timptotal = l-timptotal + l-imptotal.
         
         ACCUMULATE l-prompag (AVERAGE).
         IF l-indice = 2 THEN
            ASSIGN l-90dias = 0 l-vtasem = 0 l-pagsem = 0 l-nc     = 0
                   l-vtaano = 0 l-pagano = 0 l-vtames = 0 l-pagmes = 0 
                   l-saldo = 0  l-cargos = 0 l-diascartera = 0 l-simbolo = "".  // RNPC - 2019-07-23
    
         IF l-indice = 1 THEN DO:
            DISPLAY STREAM s-salida
                  w-saldo.sec @ MovCliente.Id-Cliente
                  Cliente.RazonSocial WHEN AVAILABLE Cliente
                  l-saldo
                  l-90dias
                  l-vtames
                  l-pagmes
                  l-cargos
                  l-vtasem
                  l-pagsem
                  /* l-vtaano
                  l-pagano */
                  Cliente.Id-Zona WHEN AVAILABLE Cliente
                  Cliente.Id-Resp WHEN AVAILABLE Cliente
                  Cliente.Plazo   WHEN AVAILABLE Cliente
                  Cliente.Limite  WHEN AVAILABLE Cliente
                  l-cheques
                  l-diascartera 
                  l-prompag 
                  Cliente.FactorDesc WHEN AVAILABLE Cliente
                  ROUND(l-SumaNoDep / 1000,0) WHEN ROUND(l-SumaNoDep / 1000,0) <> 0 @ l-SumaNoDep
                  l-simbolo FORMAT "x(2)"   AT  55 WHEN l-simbolo <> ""
                  v-anticipo WHEN v-anticipo > 0
                  Cliente.MontoPagare WHEN (AVAILABLE Cliente AND Cliente.MontoPagare <> 0)
             WITH FRAME f-x DOWN.
             DOWN WITH FRAME f-x.
             ACCUMULATE l-vtames (TOTAL).
             ACCUMULATE l-pagmes (TOTAL).
             ACCUMULATE l-cargos (TOTAL).
             ACCUMULATE l-vtasem (TOTAL).
             ACCUMULATE l-pagsem (TOTAL).
             ACCUMULATE l-vtaano (TOTAL).
             ACCUMULATE l-pagano (TOTAL).
             ACCUMULATE l-90dias (TOTAL).
             
             /*IF Cliente.Id-Cliente <> 31010 AND Cliente.Id-Cliente <> 31396 AND Cliente.Id-Cliente <> 30330 THEN DO:*/
             IF Cliente.Id-Resp <> 99 THEN DO:
                l-ExSaldo  = l-ExSaldo  + l-saldo.
                l-Ex90Dias = l-Ex90Dias + l-90dias.
                l-ExVtames = l-ExVtames + l-vtames.
                l-Excargos = l-ExCargos + l-cargos.
                l-ExPagmes = l-Expagmes + l-pagmes.
                l-ExVtaSem = l-ExVtaSem + l-vtasem.
                l-ExPagSem = l-ExPagSem + l-pagsem.
                l-EXtotcli = l-EXtotcli + 1.
                IF l-pagsem <> 0 THEN DO:  
                    ASSIGN l-EXtsalxant  = l-EXtsalxant + (ACCUM TOTAL l-dias * l-saldo)
                           l-EXtsaltotal = l-EXtsaltotal + l-pagsem.
                END.
                ASSIGN l-EXtimpxant = l-EXtimpxant + l-impxant
                       l-EXtimptotal = l-EXtimptotal + l-imptotal.
             END.
             ASSIGN l-90dias = 0 l-vtasem = 0 l-pagsem = 0 l-nc     = 0
                    l-vtaano = 0 l-pagano = 0 l-vtames = 0 l-pagmes = 0 
                    l-saldo  = 0 l-cargos = 0 l-diascartera = 0 l-simbolo="".   // RNPC -2019-07-23
         END. /* si el l-indice = 1 entonces es detallado  (del if indice = 1) */
    
        IF l-indice = 2 {ifdef {&Break}} 
                 AND LAST-OF({&Break}) {endif} */ AND l-entro THEN DO:
            DISPLAY STREAM s-salida
                  {ifdef {&Mensaje}} {&Mensaje} @ Cliente.RazonSocial {endif} */
                  l-saldot  @ l-saldo
                  l-90diast @ l-90dias
                  l-vtamest @ l-vtames
                  l-pagmest @ l-pagmes
                  l-cargost @ l-cargos
                  l-vtasemt @ l-vtasem
                  l-pagsemt @ l-pagsem
                  /* l-vtaanot @ l-vtaano
                  l-paganot @ l-pagano */
             WITH FRAME f-x DOWN.
             DOWN WITH FRAME f-x.
             ACCUMULATE l-vtamest (TOTAL).
             ACCUMULATE l-pagmest (TOTAL).
             ACCUMULATE l-cargost (TOTAL).
             ACCUMULATE l-vtasemt (TOTAL).
             ACCUMULATE l-pagsemt (TOTAL).
             ACCUMULATE l-vtaanot (TOTAL).
             ACCUMULATE l-paganot (TOTAL).
             ACCUMULATE l-saldot  (TOTAL).
             ACCUMULATE l-90diast (TOTAL).
             ASSIGN l-90dias  = 0 l-vtasem  = 0 l-pagsem  = 0 l-vtaano  = 0
                    l-pagano  = 0 l-pagmes  = 0 l-vtames  = 0 l-90diast = 0
                    l-vtasemt = 0 l-pagsemt = 0 l-vtaanot = 0 l-vtamest = 0
                    l-paganot = 0 l-entro   = FALSE           l-saldot  = 0
                    l-pagmest = 0 l-totcorteh = l-totcorteh + 1
                    l-saldocal = 0 l-saldo  = 0 l-cargos  = 0 l-cargost = 0
                    l-diascartera = 0
                    l-simbolo = "".     // RNPC - 2019-07-22
        END.
       END. /* del LAST-OF(MovCliente) */
    
       IF ((l-totclih = 25 OR LAST(w-saldo.Acomodo )) AND l-indice <> 2) OR
          ((l-totcorteh = 25 OR LAST( w-saldo.Acomodo )) AND l-indice = 2) THEN DO:
          ASSIGN l-diascarterah = l-diascarterah / l-totclih.
          DISPLAY STREAM s-salida
              /*l-diascarterah*/ l-saldocalh
              l-90diash l-vtamesh
              l-pagmesh l-vtasemh l-cargosh
              l-pagsemh /* l-vtaanoh l-paganoh */
          WITH FRAME f-tothoja.
          ASSIGN l-pagina = PAGE-NUMBER(s-salida)
                 l-diascarterah = 0 l-saldocalh    = 0
                 l-totclih      = 0 l-90diash      = 0
                 l-vtamesh      = 0 l-pagmesh      = 0
                 l-vtasemh      = 0 l-pagsemh      = 0
                 l-vtaanoh      = 0 l-paganoh      = 0
                 l-totcorteh    = 0 l-cargosh      = 0.
    
          IF NOT LAST(w-saldo.Acomodo) THEN 
                PAGE STREAM s-salida.
       END. /* si hubo un salto de pagina */ 
       ASSIGN l-saldocal = 0 l-saldo = 0.
