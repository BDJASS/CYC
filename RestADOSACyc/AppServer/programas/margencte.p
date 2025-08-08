/*
  Programa: margencte.p
  Funcion : Backend calculo de margen de un cliente, ultimos 6 meses
  Autor   : FLC
  Fecha   : 22 FEB 2025
*/

DEF INPUT PARAMETER p-Cliente AS INTEGER NO-UNDO.
DEF OUTPUT PARAMETER p-Margen AS DECIMAL NO-UNDO.

DEF VAR l-FecIni AS DATE NO-UNDO.
DEF VAR l-FecFin AS DATE NO-UNDO.

DEF VAR l-SubTot1   AS DECIMAL NO-UNDO.
DEF VAR l-SubTot2   AS DECIMAL NO-UNDO.
DEF VAR l-Costo3    AS DECIMAL NO-UNDO.
DEF VAR l-Descto1   AS DECIMAL NO-UNDO.
DEF VAR l-Descto2   AS DECIMAL NO-UNDO.
DEF VAR l-FactorPP  AS DECIMAL NO-UNDO.
DEF VAR l-MovImp1   AS DECIMAL NO-UNDO.

DEF BUFFER bf-movcliente FOR movcliente.
DEF BUFFER bf-histmovcte FOR histmovcte.

DEF TEMP-TABLE t-Margen NO-UNDO
  FIELD Id-Cliente  LIKE Cliente.Id-Cliente
  FIELD Costo       AS   DECIMAL
  FIELD Importe     AS   DECIMAL
  FIELD Neto        AS   DECIMAL
  FIELD Dif         AS   DECIMAL
  FIELD Margen      AS   DECIMAL
  FIELD Descto      AS   DECIMAL
  FIELD PorDes      AS   DECIMAL 
  INDEX Idx-Cte IS UNIQUE PRIMARY Id-Cliente.


  EMPTY TEMP-TABLE t-Margen.
  p-Margen = 0.
  l-FecFin = TODAY - 1.
  l-FecIni = l-FecFin - 182.
  FIND Cliente WHERE Cliente.Id-Cliente = p-Cliente NO-LOCK NO-ERROR.
  IF AVAILABLE Cliente THEN RUN Calculos.
  FIND FIRST t-Margen WHERE t-Margen.Id-Cliente = p-Cliente NO-LOCK NO-ERROR.
  IF AVAILABLE t-Margen THEN DO:
     ASSIGN p-Margen = t-Margen.Margen.
  END.
RETURN.


PROCEDURE Calculos.
        
    FOR EACH Remision WHERE Remision.Id-Cliente = Cliente.Id-Cliente
                        AND Remision.FecReg >= l-FecIni
                        AND Remision.FecReg <= l-FecFin
                        NO-LOCK:
    
        IF Remision.FecCancel <> ? OR Remision.Pagada = FALSE THEN NEXT.
        ASSIGN l-SubTot1 = 0
               l-SubTot2 = 0
               l-Costo3  = 0
               l-Descto1 = 0
               l-Descto2 = 0.
    
        FOR EACH DetRemis OF Remision WHERE DetRemis.Tipo <= 2 
                 NO-LOCK:
        
            IF DetRemis.Tipo = 1 OR
               (DetRemis.Tipo = 2 AND DetRemis.Importe < 0) THEN DO:                                
                ASSIGN l-Costo3 = l-Costo3 + (DetRemis.Costo * (DetRemis.Cant - DetRemis.CantDev)).
                ASSIGN l-SubTot2 = l-SubTot2 + (DetRemis.PrecUnit * 
                          ((100 - DetRemis.PorcDesc) / 100) *
                          (DetRemis.Cant)).
                ASSIGN l-SubTot1 = l-SubTot1 + (DetRemis.PrecUnit * 
                          ((100 - DetRemis.PorcDesc) / 100) *
                          (DetRemis.Cant - DetRemis.CantDev)).
            END.
        END.
        FOR EACH MovCaja WHERE MovCaja.Refer = Remision.Id-Remision
                           AND MovCaja.TipoVenta = Remision.TipoVenta
                           NO-LOCK,
            EACH DetMovC WHERE DetMovC.Id-Caja = MovCaja.Id-Caja
                           AND DetMovC.Folio = MovCaja.Folio
                           AND DetMovC.Mov = "R"
                           NO-LOCK:
            l-Descto2 = l-Descto2 + DetMovC.MontoPago.
        END.
        ASSIGN l-Descto1 = Remision.Descuento.
        IF l-Descto1 > 0 OR l-Descto2 > 0 THEN
           l-FactorPP = (1 - (l-Descto1 / Remision.Subtotal)) *
                        (1 - (l-Descto2 / Remision.Tot)).
        ELSE l-FactorPP = 1.
                           
        IF l-Subtot1 <= 0 THEN NEXT.
        IF l-Subtot1 = ? THEN ASSIGN l-Subtot1  = 0.
        IF l-Costo3 = ? THEN ASSIGN l-Costo3    = 0.

        FIND FIRST t-Margen WHERE t-Margen.Id-Cliente = cliente.id-cliente
                                  EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE t-Margen THEN DO:
            CREATE t-Margen.
            ASSIGN t-Margen.Id-Cliente = cliente.id-cliente.
            ASSIGN 
                t-Margen.Costo      = 0
                t-Margen.Importe    = 0
                t-Margen.Descto     = 0
                t-Margen.Neto       = 0
                t-Margen.Dif        = 0
                t-Margen.PorDes     = 0
                t-margen.margen     = 0.
        END.

        ASSIGN 
            t-Margen.Costo      = t-Margen.Costo    + l-Costo3
            t-Margen.Importe    = t-Margen.Importe  + l-Subtot1
            t-Margen.Descto     = t-Margen.Descto   + (l-Subtot2 - (l-SubTot2 * l-FactorPP))
            t-Margen.Neto       = t-Margen.Importe  - t-Margen.Descto
            t-Margen.Dif        = t-Margen.Neto     - t-Margen.Costo
            t-Margen.PorDes     = (((t-Margen.Importe - t-Margen.Descto) / t-Margen.Importe) - 1) * 100
            t-margen.margen     = (1 - (t-margen.costo / (t-margen.importe - t-margen.descto) ) ) * 100.

        RELEASE t-Margen.
    END. /* for each remision */

    FOR EACH bf-movcliente WHERE bf-movcliente.afectado                 AND
                                 bf-movcliente.id-cliente = cliente.id-cliente AND
                                 bf-movcliente.fecreg     >= l-fecIni   AND
                                 bf-movcliente.fecreg     <= l-fecFin    AND                                     
                                 bf-movcliente.id-mc > 3
                                 NO-LOCK
                                 BREAK BY bf-movcliente.RefSaldo BY bf-movcliente.fecreg:
        
        IF NOT FIRST-OF(bf-movcliente.RefSaldo) THEN NEXT.
        FIND FIRST MovCliente WHERE MovCliente.RefSaldo = bf-MovCliente.RefSaldo
                                AND MovCliente.Id-MC = 67
                                NO-LOCK NO-ERROR.
        IF AVAILABLE MovCliente THEN NEXT.
    
        FOR EACH movcliente WHERE movcliente.fecreg   <= l-FecFin AND
                                  movcliente.refsaldo = bf-movcliente.refsaldo                                        
                                  NO-LOCK:
            FIND Acuse WHERE Acuse.Id-Acuse = movcliente.Documento
                       NO-LOCK NO-ERROR.
            IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
            ACCUMULATE movcliente.importe(TOTAL).
        END.

        IF (ACCUM TOTAL movcliente.importe) < 1 THEN DO: /* totalmente pagada */
                
            l-Descto2 = 0.
            l-MovImp1 = 0.
            FOR EACH MovCliente WHERE MovCliente.Afectado                           AND
                                      MovCliente.refsaldo = bf-movcliente.refsaldo  AND
                                      MovCliente.Id-MC > 3
                                      NO-LOCK:
                FIND FIRST TipoPago WHERE TipoPago.Id-TP = MovCliente.Id-MC NO-LOCK NO-ERROR.
                IF AVAILABLE TipoPago OR MovCliente.Id-MC = 65 OR MovCliente.Id-MC = 90 THEN NEXT.
                l-MovImp1 = l-MovImp1 + (MovCliente.Importe * -1).
            END.
            IF l-MovImp1 <> 0 THEN DO:
                FOR EACH DistIva WHERE DistIVA.Id-Factura = bf-MovCliente.RefSaldo
                                   AND DistIVA.TipoVenta = 3
                                   NO-LOCK:
                    l-Descto2 = l-Descto2 + (l-MovImp1 * (DistIVA.Participacion / 100) 
                                / (1 + (DistIVA.PorcIVA / 100))).
                END.
            END.    
            FIND Factura WHERE Factura.Id-Factura = bf-MovCliente.RefSaldo 
                               NO-LOCK NO-ERROR.
            IF AVAILABLE Factura THEN RUN DetCredito.
        END. /* si esta totalmente pagada */
    END. /* for each bf-movcliente */        
    
    FOR EACH bf-histmovcte WHERE bf-histmovcte.afectado                         AND
                                 bf-histmovcte.id-cliente = cliente.id-cliente  AND
                                 bf-histmovcte.fecreg     >= l-fecini           AND
                                 bf-histmovcte.fecreg     <= l-FecFin            AND
                                 bf-histmovcte.id-mc > 3
                                 NO-LOCK BREAK BY bf-histmovcte.RefSaldo:
        IF NOT FIRST-OF(bf-histmovcte.RefSaldo) THEN NEXT.          
        FIND FIRST HistMovCte WHERE HistMovCte.RefSaldo = bf-HistMovCte.RefSaldo
                                AND HistMovCte.Id-MC = 67
                                NO-LOCK NO-ERROR.
        IF AVAILABLE HistMovCte THEN NEXT.

        FOR EACH histmovcte WHERE histmovcte.fecreg   <= l-FecFin               AND
                                  histmovcte.refsaldo = bf-histmovcte.refsaldo                                      
                                  NO-LOCK:
            FIND Acuse WHERE Acuse.Id-Acuse = histmovcte.Documento
                       NO-LOCK NO-ERROR.
            IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
            ACCUMULATE histmovcte.importe(TOTAL).
        END.

        IF (ACCUM TOTAL histmovcte.importe) < 1 THEN DO: /* totalmente pagada */
            l-Descto2 = 0.
            l-MovImp1 = 0.
            FOR EACH HistMovCte WHERE HistMovCte.Afectado                           AND
                                      HistMovCte.refsaldo = bf-HistMovCte.refsaldo  AND
                                      HistMovCte.Id-MC > 3
                                      NO-LOCK:
                FIND FIRST TipoPago WHERE TipoPago.Id-TP = HistMovCte.Id-MC NO-LOCK NO-ERROR.
                IF AVAILABLE TipoPago OR HistMovCte.Id-MC = 65 OR HistMovCte.Id-MC = 90 THEN NEXT.
                l-MovImp1 = l-MovImp1 + (HistMovCte.Importe * -1).
            END.    
            IF l-MovImp1 <> 0 THEN DO:
                FOR EACH DistIva WHERE DistIVA.Id-Factura = bf-HistMovCte.RefSaldo
                                   AND DistIVA.TipoVenta = 3
                                   NO-LOCK:
                    l-Descto2 = l-Descto2 + (l-MovImp1 * (DistIVA.Participacion / 100) 
                                / (1 + (DistIVA.PorcIVA / 100))).
                END.
            END.    
            FIND Factura WHERE Factura.Id-Factura = bf-HistMovCte.RefSaldo 
                               NO-LOCK NO-ERROR.
            IF AVAILABLE Factura THEN RUN DetCredito.
        END. /* si esta pagada */
    END. /* for each bf-histmovcte */

END PROCEDURE.




PROCEDURE DetCredito.

    ASSIGN l-Subtot1 = 0
           l-Costo3  = 0
           l-Descto1 = 0.

    FOR EACH DetFactura OF Factura WHERE DetFactura.Tipo <= 2 
             NO-LOCK:
    
        IF DetFactura.Tipo = 1 OR
           (DetFactura.Tipo = 2 AND DetFactura.Descr BEGINS 'REMISION') THEN DO:                                
            ASSIGN l-Costo3 = l-Costo3 + (DetFactura.Costo * (DetFactura.Cant - DetFactura.CantDev)).
            ASSIGN l-SubTot1 = l-SubTot1 + (DetFactura.PrecUnit * 
                      ((100 - DetFactura.Descto) / 100) *
                      (DetFactura.Cant - DetFactura.CantDev)).
        END.
    END.
    
    ASSIGN l-Descto1 = Factura.Descuento.
    IF l-Descto1 > 0 THEN
       l-FactorPP = (1 - (l-Descto1 / Factura.Subtotal)). 
    ELSE l-FactorPP = 1.
                       
    IF l-Subtot1 <= 0 THEN NEXT.
    IF l-Subtot1 = ? THEN ASSIGN l-Subtot1  = 0.
    IF l-Costo3 = ? THEN ASSIGN l-Costo3    = 0.

    FIND FIRST t-Margen WHERE t-Margen.Id-Cliente = cliente.id-cliente
                              EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE t-Margen THEN DO:
        CREATE t-Margen.
        ASSIGN t-Margen.Id-Cliente = cliente.id-cliente.
        ASSIGN 
            t-Margen.Costo      = 0
            t-Margen.Importe    = 0
            t-Margen.Descto     = 0
            t-Margen.Neto       = 0
            t-Margen.Dif        = 0
            t-Margen.PorDes     = 0
            t-margen.margen     = 0.
    END.
    ASSIGN 
        t-Margen.Costo      = t-Margen.Costo    + l-Costo3
        t-Margen.Importe    = t-Margen.Importe  + l-Subtot1
        t-Margen.Descto     = t-Margen.Descto   + ((l-Subtot1 - (l-SubTot1 * l-FactorPP)) + l-Descto2)
        t-Margen.Neto       = t-Margen.Importe  - t-Margen.Descto
        t-Margen.Dif        = t-Margen.Neto     - t-Margen.Costo
        t-Margen.PorDes     = (((t-Margen.Importe - t-Margen.Descto) / t-Margen.Importe) - 1) * 100
        t-margen.margen     = (1 - (t-margen.costo / (t-margen.importe - t-margen.descto) ) ) * 100.                                

    RELEASE t-Margen.

END.

