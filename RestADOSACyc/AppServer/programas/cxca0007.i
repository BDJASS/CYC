/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : General
  Programa : cxca0007.i
  Funcion  : Rutina para calcular DistIva
  Autor    : IOC
  Fecha    : 04-12-1996
*/

DEF VAR l-acum        AS DECIMAL.
DEF VAR l-part        AS DECIMAL DECIMALS 2.
DEF VAR l-iva         AS DECI FORMAT "ZZ9.99%"         NO-UNDO.
DEF VAR l-suma        AS DECI FORMAT "ZZZ,ZZZ,ZZ9.99"  NO-UNDO.
DEF VAR l-TotIVA      LIKE l-suma                      NO-UNDO.
DEF VAR l-TipoIVA1    AS INT                           NO-UNDO.
DEF VAR l-TipoIVA2    AS INT                           NO-UNDO.
DEF VAR l-ivaseg      AS DECIMAL                       NO-UNDO.
DEF VAR l-ivaFlete    AS DECIMAL                       NO-UNDO.
DEF VAR l-numfac      LIKE Factura.id-Factura          NO-UNDO.
DEF VAR l-ivad        AS DECIMAL                       NO-UNDO.

ASSIGN l-numfac = {&factura}.
FIND FIRST SysGeneral NO-LOCK NO-ERROR.

IF {&tipoVenta} = 3 THEN DO:
   FIND Factura WHERE Factura.Id-Factura = l-numfac           NO-LOCK NO-ERROR.
   FIND Cliente WHERE Cliente.Id-Cliente = Factura.Id-Cliente NO-LOCK NO-ERROR.
   FIND Zona OF Cliente                                       NO-LOCK NO-ERROR.
   ASSIGN l-TotIVA   = 0
          l-suma     = 0.

          /* Factura.IVA. */
   FOR EACH DetFactura OF Factura NO-LOCK :
       ACCUMULATE DetFactura.IVA (TOTAL).
   END.

   ASSIGN l-TotIVA = l-TotIVA + (ACCUM TOTAL DetFactura.IVA)
          l-TotIVA = l-TotIVA + Factura.IVASeguro + Factura.IVAFlete.
   FOR EACH DetFactura OF Factura NO-LOCK BREAK BY DetFactura.PorcIVA :
       ASSIGN l-suma = l-suma + DetFactura.IVA .
       ACCUMULATE DetFactura.Importe (total by DetFactura.porciva).

       IF LAST-OF(DetFactura.PorcIVA) AND
          (ACCUM TOTAL BY DetFactura.PorcIva DetFactura.Importe) <> 0 THEN DO :
          FIND FIRST DistIVA WHERE DistIVA.Id-Factura = Factura.Id-factura AND
                                   DistIVA.PorcIVA    = DetFactura.PorcIVA AND
                                   DistIVA.TipoVenta  = {&TipoVenta}
                                   EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAILABLE (DistIVA) THEN
             CREATE DistIVA.
          ASSIGN DistIva.Participacion = Distiva.Participacion +
                                         ACCUM TOTAL BY DetFactura.PorcIva
                                         DetFactura.Importe
                 l-suma  = l-suma + (IF SysGeneral.Porc-IVA = DetFactura.PorcIVA
                                    THEN Factura.IVASeguro ELSE 0)
                                 + (IF Zona.Porc-IVA = DetFactura.PorcIVA
                                    THEN Factura.IVAFlete ELSE 0)
                 DistIVA.IVA           = l-suma
                 DistIVA.PorcIVA       = DetFactura.PorcIVA
                 DistIVA.TipoVenta     = {&TipoVenta}
                 DistIVA.Id-Factura    = l-numfac.

          IF SysGeneral.Porc-Iva = DetFactura.PorcIVA THEN DO:
             ASSIGN Distiva.Participacion = Distiva.Participacion +
                                            Factura.ImpSeguro.
             IF Factura.TipoPrecio = 'II' THEN
                ASSIGN DistIva.Participacion = Distiva.Participacion +
                                               Factura.IvaSeguro.
          END.
          IF Zona.Porc-Iva = DetFactura.PorcIVA THEN DO:
             ASSIGN Distiva.Participacion = Distiva.Participacion +
                                            Factura.ImpFlete.
             IF Factura.TipoPrecio = 'II' THEN
                ASSIGN DistIva.Participacion = Distiva.Participacion +
                                               Factura.IvaFlete.
          END.
          ASSIGN l-suma = 0.
       END.
   END.
   IF Factura.IvaSeguro > 0 THEN DO:
      FIND FIRST DetFactura WHERE
                 DetFactura.Id-Factura = Factura.Id-Factura AND
                 DetFactura.PorcIVA    = SysGeneral.Porc-IVA NO-LOCK NO-ERROR.

      IF NOT AVAILABLE (DetFactura) THEN DO:
         FIND FIRST DistIVA WHERE
                    DistIVA.Id-Factura = Factura.Id-factura AND
                    DistIVA.PorcIVA    = SysGeneral.Porc-IVA AND
                    DistIVA.TipoVenta  = {&TipoVenta} EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE(DistIVA) THEN
            CREATE DistIVA.

         ASSIGN DistIVA.IVA           = DistIVA.IVA + Factura.IVASeguro
                DistIVA.PorcIVA       = SysGeneral.Porc-IVA
                DistIVA.TipoVenta     = {&TipoVenta}
                DistIVA.Id-Factura    = Factura.Id-Factura
                Distiva.Participacion = Distiva.Participacion +
                                        Factura.ImpSeguro.
         IF Factura.TipoPrecio = 'II' THEN
            ASSIGN DistIva.Participacion = Distiva.Participacion +
                                           Factura.IvaSeguro.
      END.
   END.
   IF Factura.IvaFlete > 0 THEN DO:
      FIND FIRST DetFactura WHERE
                 DetFactura.Id-Factura = Factura.Id-Factura AND
                 DetFactura.PorcIVA    = Zona.Porc-IVA NO-LOCK NO-ERROR.
      IF NOT AVAILABLE (DetFactura) THEN DO:
         FIND FIRST DistIVA WHERE
                    DistIVA.Id-Factura = Factura.Id-factura AND
                    DistIVA.PorcIVA    = Zona.Porc-IVA AND
                    DistIVA.TipoVenta  = {&TipoVenta} EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE(DistIVA) THEN
            CREATE DistIVA.

         ASSIGN DistIVA.IVA           = DistIVA.IVA + Factura.IVAFlete
                DistIVA.PorcIVA       = Zona.Porc-IVA
                DistIVA.TipoVenta     = {&TipoVenta}
                DistIVA.Id-Factura    = Factura.Id-Factura
                Distiva.Participacion = Distiva.Participacion +
                                        Factura.ImpFlete.

         IF Factura.TipoPrecio = 'II' THEN
            ASSIGN DistIva.Participacion = Distiva.Participacion +
                                           Factura.IvaFlete.
      END.
   END.
   ASSIGN l-acum = 0
          l-part = 0.
   FOR EACH DistIva WHERE DistIva.Id-Factura = Factura.Id-Factura
                      AND DistIva.TipoVenta = {&TipoVenta} NO-LOCK:
       ASSIGN l-acum = l-acum + Distiva.Participacion.
   END.
   FOR EACH DistIva WHERE DistIva.Id-Factura = Factura.Id-Factura
                      AND DistIva.TipoVenta = {&TipoVenta}
       EXCLUSIVE-LOCK BREAK BY Distiva.Porciva:
       ASSIGN Distiva.Participacion = Distiva.Participacion / l-acum * 100
              l-part  = l-part + Distiva.Participacion.
       IF LAST(Distiva.Porciva) THEN
          IF l-part <> 100 THEN
             ASSIGN Distiva.Participacion = 100 - l-part +
                                            Distiva.Participacion.
   END.
END.
ELSE DO:
   FIND Remision  WHERE Remision.Id-Remision = l-numfac AND
                        Remision.TipoVenta = {&tipoventa} NO-LOCK NO-ERROR.

   ASSIGN l-TotIVA = Remision.Iva
          l-acum   = 0.

   FOR EACH DetRemis OF Remision NO-LOCK BREAK BY DetRemis.PorcIVA :
       ACCUMULATE detremis.importe (total by detremis.porciva).
       ACCUMULATE detremis.iva (total by detremis.porciva).
       IF NOT DetRemis.Nopp AND DetRemis.Tipo <> 6 THEN
          ASSIGN l-ivad = (DetRemis.Iva *
                           ( 1 - (Remision.Descuento1 / 100 )) *
                           ( 1 - (Remision.Descuento2 / 100 )) *
                           ( 1 - (Remision.Descuento3 / 100 )))
                 l-suma = l-suma + l-ivad.
       ELSE
          ASSIGN l-suma = l-suma + DetRemis.IVA.

       IF LAST-OF(DetRemis.PorcIVA) AND 
          (ACCUM TOTAL BY DetRemis.PorcIva DetRemis.Importe) <> 0 THEN DO:
          FIND FIRST DistIVA WHERE DistIVA.Id-Factura = Remision.Id-Remision AND
                                   DistIVA.PorcIVA    = DetRemis.PorcIVA AND
                                   DistIVA.TipoVenta  = {&TipoVenta}
                                   NO-LOCK NO-ERROR.
          IF NOT AVAILABLE (DistIVA) THEN
             CREATE DistIVA.

          ASSIGN l-suma = round(l-suma,2).

          IF Remision.TipoPrecio = 'II' THEN
             ASSIGN l-part = accum total by detremis.porciva detremis.iva
                    l-part = ((accum total by detremis.porciva detremis.importe)
                              - l-part) / Remision.Subtotal * 100.
          ELSE
             ASSIGN l-part = (accum total by detremis.porciva detremis.importe)
                                    / remision.subtotal * 100.

          ASSIGN l-acum = l-acum + l-part.
          IF LAST(Detremis.PorcIva)  THEN DO:
             IF l-acum <> 100 THEN
                ASSIGN l-part = 100 - l-acum  + l-part.
          END.

          ASSIGN DistiVa.Participacion = l-part
                 DistIVA.IVA           = l-suma
                 DistIVA.PorcIVA       = DetRemis.PorcIVA
                 DistIVA.TipoVenta     = {&TipoVenta}
                 DistIVA.Id-Factura    = l-numfac.

          ASSIGN l-suma = 0
                 l-ivad = 0.
       END.
   END.
END.
