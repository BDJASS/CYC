/*
    Empresa  : ADOSA
    Programa : vtac2070.p
    Funcion  : Generacion de Factura Electronica de Contado
    Fecha    : 22/09/2010
    Autor    : Elias Castro
*/

// {/usr2/adosa/includes/sia00000.var}
DEF INPUT PARAMETER numRemision LIKE Remision.Id-Remision NO-UNDO.
DEF INPUT PARAMETER pIdUser   AS CHARACTER NO-UNDO.  

/* Para manejo de conexion con web service */
DEF VAR hWS               AS HANDLE.
DEF VAR hEmiteCFDSoap     AS HANDLE.
DEF VAR vlconnect         AS LOGICAL NO-UNDO.
DEF VAR vldisconnect      AS LOGICAL NO-UNDO.
DEF VAR servidor          AS CHAR NO-UNDO.

/* Variables para funcion del web service */
DEF VAR v-respuesta   AS CHAR NO-UNDO.
DEF VAR v-comprobante AS LONGCHAR NO-UNDO.
DEF VAR v-UUID AS LONGCHAR NO-UNDO.

/* Variables para guardar cada una de las etiquetas del xml */
DEF VAR v-emisor AS CHAR NO-UNDO.
DEF VAR v-domFis AS CHAR NO-UNDO.
DEF VAR v-expEn  AS CHAR NO-UNDO.
DEF VAR v-recep  AS CHAR NO-UNDO.
DEF VAR v-dom    AS CHAR NO-UNDO.
DEF VAR v-enca   AS CHAR NO-UNDO.
DEF VAR v-deta   AS LONGCHAR NO-UNDO.
DEF VAR v-concep AS LONGCHAR NO-UNDO.
DEF VAR v-impsto AS CHAR NO-UNDO.
DEF VAR v-puerto AS CHAR NO-UNDO.
DEF VAR v-pedNo  AS CHAR NO-UNDO.

DEF VAR v-exped   AS CHAR NO-UNDO.
DEF VAR v-rfcCte  AS CHAR NO-UNDO.
DEF VAR v-letras  AS CHAR NO-UNDO.
DEF VAR v-descrip AS CHAR NO-UNDO.
DEF VAR v-reng    AS INT  NO-UNDO.
DEF VAR v-pedido  AS CHAR NO-UNDO.
DEF VAR v-Colonia AS CHAR NO-UNDO.
DEF VAR v-attn    AS CHAR NO-UNDO.
DEF VAR v-razon   AS CHAR NO-UNDO.
DEF VAR v-razon1  AS CHAR NO-UNDO.
DEF VAR v-calleno1 AS CHAR NO-UNDO.
DEF VAR v-colonia1 AS CHAR NO-UNDO.
DEF VAR i         AS INT  NO-UNDO.
DEF VAR l-ClavePS LIKE Articulo.Id-ClavePS NO-UNDO.
DEF VAR v-UsoCFDI LIKE Factura.Id-UsoCFDI NO-UNDO.
DEF VAR v-RFiscal LIKE Remision.Id-RFiscal NO-UNDO.
DEF VAR v-CPFiscal LIKE Remision.CP NO-UNDO.
DEF VAR v-cfdirel AS LONGCHAR NO-UNDO.
DEF VAR v-LExp LIKE Remision.CP NO-UNDO.
DEF VAR v-Referencia LIKE Remision.Referencia NO-UNDO.

DEF VAR v-correo  AS CHAR NO-UNDO.

DEF VAR l-SwContEfec AS CHAR NO-UNDO.
DEF VAR l-DescrAnt   AS CHAR NO-UNDO.
DEF VAR v-limitepf   AS LOGICAL NO-UNDO.

DEF VAR v-rfc     AS CHAR     NO-UNDO.
DEF VAR v-serie   AS CHAR     NO-UNDO.
DEF VAR v-folio   AS CHAR     NO-UNDO. 
DEF VAR v-fecAdu  AS CHAR     NO-UNDO.

/* Variables para extraer los valores de la respuesta del WebService */
DEF VAR v-valores AS CHAR EXTENT 4 NO-UNDO.
DEF VAR v-tam     AS INT NO-UNDO.
DEF VAR v-ind     AS INT NO-UNDO.
DEF VAR v-pos     AS INT NO-UNDO.
DEF VAR v-siz     AS INT NO-UNDO.
DEF VAR v-num     AS INT NO-UNDO.
DEF VAR l-Intent  AS INT NO-UNDO.
DEF VAR l-MaxInt  AS INT NO-UNDO INITIAL 3.

DEF VAR msuma     AS INT NO-UNDO.
DEF VAR mfa_veri  AS INT NO-UNDO.
DEF VAR l-k       AS INT NO-UNDO.
DEF VAR v-okRFC   AS LOGICAL  NO-UNDO.
DEF VAR l-MetodoDePago AS CHAR NO-UNDO.
DEF VAR l-NumCtaPago AS CHAR NO-UNDO.
DEF VAR l-FormaDePago AS CHAR NO-UNDO.
DEF VAR l-FecCheque AS DATE NO-UNDO.
DEF VAR l-LeyendaMetodo AS CHAR NO-UNDO.
DEF VAR l-RutaReq AS CHAR NO-UNDO.
DEF VAR g-Origen AS CHARACTER NO-UNDO.  /* Define the variable */

DEF VAR v-listaMes AS CHAR NO-UNDO 
        INITIAL 'ENE,FEB,MAR,ABR,MAY,JUN,JUL,AGO,SEP,OCT,NOV,DIC'.

DEF BUFFER buffRemis FOR Remision.
DEF BUFFER b-Remision FOR Remision.
DEF BUFFER buffMovCaja FOR MovCaja.

/* Tabla temporal para guardar pedimentos de importacion */
DEF TEMP-TABLE v-pedim
    FIELD PedNo  LIKE DetPedim.PedNo
    FIELD Puerto LIKE DetPedim.Puerto
    FIELD FecPed LIKE DetPedim.FecPed
    INDEX ip-1 AS PRIMARY pedNo Puerto.
    
    
DEF TEMP-TABLE w-Impuestos
    FIELD Tasa AS DECIMAL
    FIELD Base AS DECIMAL
    FIELD Bonifica LIKE DetRemis.Iva
    FIELD Importe LIKE DetRemis.Iva
    INDEX Idx-Def Tasa.
    
DEF TEMP-TABLE tt-DetRemis LIKE DetRemis.
DEF VAR l-TmpDesc LIKE Remision.Descuento NO-UNDO.
DEF VAR l-DifDesc LIKE Remision.Descuento NO-UNDO.
DEF VAR l-TIva LIKE DetRemis.Iva NO-UNDO.

DEF VAR l-ImpExento  AS DECIMAL NO-UNDO.
DEF VAR l-NegImp  AS DECIMAL NO-UNDO.
DEF VAR l-NegIva LIKE DetRemis.Iva NO-UNDO.
DEF VAR l-NegPosible AS DECIMAL NO-UNDO.    
DEF VAR l-CalcIva    AS DECIMAL DECIMALS 4 NO-UNDO INITIAL 0.

FIND Remision WHERE Remision.Id-Remision = numRemision NO-LOCK NO-ERROR.

/* Cuidar que no se genere el folioe electronico 2 veces para una misma factura */
IF Remision.Folioe <> '' THEN LEAVE.

/*
FIND FIRST DetRemis WHERE DetRemis.Id-Remision = Remision.Id-Remision
                      AND DetRemis.Descr BEGINS "VALE"
                      AND DetRemis.Importe < 0
                      NO-LOCK NO-ERROR.
IF AVAILABLE DetRemis AND Remision.RFC = "XAXX010101000" THEN DO:
   FOR EACH Detremis WHERE DetRemis.Id-Remision = Remision.Id-Remision
       AND (Detremis.tipo < 3 OR Detremis.tipo = 6) AND detremis.porcdesc = 0
       EXCLUSIVE-LOCK:
       ASSIGN DetRemis.PrecUnit = DetRemis.Importe / detremis.cant
              DetRemis.Iva = Detremis.Importe * (Detremis.Porciva / 100).
   END.
   l-CalcIva = 0.
   FOR EACH DetRemis WHERE DetRemis.Id-Remision = Remision.Id-Remision
       AND (DetRemis.Tipo < 3 OR DetRemis.Tipo = 6) EXCLUSIVE-LOCK:
       IF Remision.Descuento1 > 0 AND DetRemis.NoPP = FALSE THEN DO:
          ASSIGN l-CalcIva = l-CalcIva + ROUND(DetRemis.iva * 
                                   ((100 - Remision.Descuento1) / 100),4).
       END.
       ELSE l-CalcIva = l-CalcIva + DetRemis.Iva.
   END.
   l-CalcIva = ROUND(l-CalcIva,2).
   IF Remision.Iva <> l-CalcIva THEN DO:
      FIND FIRST b-Remision WHERE RECID(b-Remision) = RECID(Remision)
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE b-Remision THEN DO:
      ASSIGN b-Remision.Iva = l-CalcIva
             b-Remision.Tot = b-Remision.SubTotal - b-Remision.Descuento
                            + b-Remision.Iva.
      FIND FIRST MovCaja WHERE MovCaja.TipoVenta = b-Remision.TipoVenta
                           AND MovCaja.Refer = b-Remision.Id-Remision
                           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE MovCaja THEN 
         ASSIGN MovCaja.Tot = b-Remision.Tot.
      END.
      RELEASE b-Remision.
   END.
   FIND Remision WHERE Remision.Id-Remision = numRemision NO-LOCK NO-ERROR.
END.
*/

v-cfdirel = ''.
IF Remision.SustUUID > "" THEN DO:
  FIND b-Remision WHERE b-Remision.Id-Remision = Remision.SustIdRemision
       NO-LOCK NO-ERROR.
  IF AVAILABLE b-Remision AND TRIM(b-Remision.RFC) = TRIM(Remision.RFC) 
  THEN DO:
    v-cfdirel = '<CfdiRelacionados TipoRelacion = "04">'.
    v-cfdirel = v-cfdirel + '<CfdiRelacionado UUID="' + Remision.SustUUID + '"/>'.
    v-cfdirel = v-cfdirel + '</CfdiRelacionados>'.
  END.
END.

/* 0. Encabezado de XML */
v-comprobante = '<?xml version="1.0" encoding="utf-8"?><Comprobante xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" version="4.0" '.

servidor = "-WSDL http://192.0.1.7/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
IF Remision.Id-Remision MATCHES '*S*' THEN DO:
    
    v-comprobante = v-comprobante + ' serie="SAB" '.
    v-comprobante = v-comprobante + ' LugarExpedicion="25000" '.
    v-LExp = "25000".
    v-exped = "Expedida en Saltillo, Coah.".
    v-expEn = '<ExpedidoEn calle="Hidalgo" noExterior="1126" noInterior="" colonia="Centro" localidad="" referencia="" municipio="Saltillo" estado="Coahuila" pais="Mexico" codigoPostal="25000"/>'.
/*
    IF Remision.FecReg < 10/31/2016 THEN
       servidor = "-WSDL http://192.0.2.22/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
*/
END.
ELSE IF Remision.Id-Remision MATCHES '*J*' THEN DO:

    v-comprobante = v-comprobante + ' serie="CHB" '.
    v-comprobante = v-comprobante + ' LugarExpedicion="31000" '.
    v-LExp = "31000".
    v-exped = "Expedida en Chihuahua, Chih.". 
    v-expEn = '<ExpedidoEn calle="Julian Carrillo" noExterior="806" noInterior="" colonia="Centro" localidad="" referencia="" municipio="Chihuahua" estado="Chihuahua" pais="Mexico" codigoPostal="31000"/>'.
    servidor = "-WSDL http://192.0.5.22/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
END.
ELSE IF Remision.Id-Remision MATCHES '*C*' THEN DO:
    
    v-comprobante = v-comprobante + ' serie="CEB" '.
    v-comprobante = v-comprobante + ' LugarExpedicion="64000" '.
    v-LExp = "64000".
    v-exped = "Expedida en Monterrey, N.L.".
    v-expEn = '<ExpedidoEn calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.

END.
ELSE IF Remision.Id-REMISION MATCHES '*F*' THEN DO:

    v-comprobante = v-comprobante + ' serie="FUB" '.
    v-comprobante = v-comprobante + ' LugarExpedicion="64000" '.
    v-LExp = "64000".
    v-exped = "Expedida en Monterrey, N.L.".
    v-expEn = '<ExpedidoEn calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.
END.   
ELSE IF Remision.Id-Remision MATCHES '*P*' THEN DO:
    
    v-comprobante = v-comprobante + ' serie="PLB" '.
    v-comprobante = v-comprobante + ' LugarExpedicion="67176" '.
    v-LExp = "67176".
    v-exped = "Expedida en Guadalupe, N.L.".
    v-expEn = '<ExpedidoEn calle="Av. Pablo Livas" noExterior="2500" noInterior="" colonia="Local 13 Plaza Mirador Mirador de la Silla" localidad="" referencia="" municipio="Guadalupe" estado="Nuevo Leon" pais="Mexico" codigoPostal="67176"/>'.    
END. 
ELSE IF Remision.Id-Remision MATCHES '*R*' THEN DO:
    
    v-comprobante = v-comprobante + ' serie="RCB" '.
    v-comprobante = v-comprobante + ' LugarExpedicion="64540" '.
    v-LExp = "64540".
    v-exped = "Expedida en Monterrey, N.L.".
    v-expEn = '<ExpedidoEn calle="Av. Ruiz Cortines" noExterior="3280" noInterior="1" colonia="Parque Industrial Regiomontano" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64540"/>'.
END. 
ELSE IF Remision.Id-Remision MATCHES '*Q*' THEN DO:
    
    v-comprobante = v-comprobante + ' serie="CUB" '.
    v-comprobante = v-comprobante + ' LugarExpedicion="64348" '.
    v-LExp = "64348".
    v-exped = "Expedida en Monterrey, N.L.".
    v-expEn = '<ExpedidoEn calle="Ruiz Cortines" noExterior="6410" noInterior="" colonia="Fracc. Portal de Cumbres" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64348"/>'.

END.
ELSE IF Remision.Id-Remision MATCHES '*V*' THEN DO:
    
    v-comprobante = v-comprobante + ' serie="DDB" '.
    v-comprobante = v-comprobante + ' LugarExpedicion="66468" '.
    v-LExp = "66468".
    v-exped = "Expedida en San Nicolas de los Garza, N.L.".
    v-expEn = '<ExpedidoEn calle="Diego Diaz de Berlanga" noExterior="469" noInterior="" colonia="Jardines de Santo Domingo" localidad="" referencia="" municipio="San Nicolas de los Garza" estado="Nuevo Leon" pais="Mexico" codigoPostal="66468"/>'.

END.
ELSE DO:
    /* MAB */
    v-comprobante = v-comprobante + ' serie="MAB" '.
    v-comprobante = v-comprobante + ' LugarExpedicion="64000" '.
    v-LExp = "64000".
    v-exped = "Expedida en Monterrey, N.L.".
    v-expEn = '<ExpedidoEn calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.
END.


FIND Cliente OF Remision NO-LOCK NO-ERROR.
FIND Entrega OF Remision NO-LOCK NO-ERROR.
FIND Transporte OF Remision NO-LOCK NO-ERROR.
FIND Vendedor OF Remision NO-LOCK NO-ERROR.
FIND Zona OF Cliente NO-LOCK NO-ERROR.
FIND FIRST Politica NO-LOCK NO-ERROR.

/*
OUTPUT TO '/usr3/tmp/factaut.txt' APPEND.
    EXPORT
        "        "
        AVAILABLE Remision
        AVAILABLE Cliente
        AVAILABLE Entrega
        AVAILABLE Transporte
        AVAILABLE Vendedor
        AVAILABLE Zona
        AVAILABLE Politica
        SKIP.            
OUTPUT CLOSE.
*/

IF AVAILABLE Vendedor THEN
    FIND Empleado WHERE Empleado.iniciales = Vendedor.iniciales NO-LOCK NO-ERROR.

FIND FIRST MovCaja WHERE MovCaja.TipoVenta = Remision.TipoVenta AND
                         MovCaja.Referencia = Remision.Id-Remision NO-LOCK NO-ERROR.

/* ------- Verifica si tiene excedido el limite de cheques postfechados ( para CEDIS ) -------- */

v-limitepf = FALSE.

FOR EACH ChequePF WHERE ChequePF.Id-Cliente = cliente.id-cliente 
                  AND   ChequePF.Dep = FALSE USE-INDEX Idx-Cli NO-LOCK:                        
    ACCUMULATE chequepf.id-cliente (COUNT).
    ACCUMULATE chequepf.importe (TOTAL).
END.

IF (ACCUM COUNT chequepf.id-cliente) >= cliente.cantcheque AND 
    (ACCUM COUNT chequepf.id-cliente) > 0 THEN
    v-limitepf = YES.

IF cliente.LimCheque > 0 AND 
   (ACCUM TOTAL chequepf.importe) > cliente.limcheque THEN
    v-limitepf = YES.

 /* --------------------------------------------------------------------------------------------- */

/* Determinar el tipo de pago ------------------------------------------------------------------ */

/* Se elimina por cambio a la factura electr�nica FLC
IF AVAILABLE zona AND zona.ubic <> 1
                  AND Remision.id-remis MATCHES '*C'
                  AND Remision.Id-Entrega <> 16 THEN
    ASSIGN l-SwContEfec = "**PAGADO x DEPOSITO**".
ELSE
    ASSIGN l-SwContEfec = (IF AVAILABLE Cliente THEN (IF (Cliente.Id-Calidad = 42  OR Cliente.Id-Calidad = 35 OR
                                                          Cliente.Id-Calidad = 38  OR Cliente.Id-Calidad = 40 OR
                                                          Cliente.Id-calidad = 34) OR Remision.tot < politica.mtochqreg
                                                        THEN "**SOLO EFECTIVO**"
                                                      ELSE IF (Cliente.Id-Calidad = 36 OR Cliente.Id-Calidad = 39) OR v-limitepf
                                                        THEN "**CHEQUE NORMAL**"
                                                      ELSE IF Cliente.Id-Calidad = 37
                                                        THEN "**CH POSFECHADO**"
                                                      ELSE IF (Cliente.Id-Calidad = 43 OR Cliente.Id-Calidad = 44) 
                                                        THEN "**CHEQUE**" ELSE "")
                           ELSE "").
*/

IF l-SwContEfec MATCHES '*POSFECHADO*' THEN DO:
    FOR EACH ChequePF WHERE ChequePF.Id-Cliente = Remision.Id-Cliente
                        AND ChequePF.Dep = FALSE NO-LOCK:
        ACCUMULATE 1 (COUNT).
        ACCUMULATE ChequePF.Importe (TOTAL).
    END.
    IF ((ACCUM COUNT 1) + 1 > Cliente.CantCheque) OR
       ((ACCUMU TOTAL ChequePF.Importe) + Remision.Tot > Cliente.LimCheque) THEN
        SUBSTRING(l-SwContEfec,INDEX(l-SwContEfec,"CH POSFECHADO"),13) = "CHEQUE NORMAL".
END.

IF PROGRAM-NAME(2) MATCHES '*vtaa0399*' AND
    (l-SwContEfec MATCHES '*POSFECHADO*' OR l-SwContEfec MATCHES '*CHEQUE*') THEN DO:
    
    IF (Cliente.CtCheq1 <> '' AND blk1) AND (Cliente.CtaCheq2 <> '' AND blk2) AND (Cliente.CtaCheq3 <> '' AND blk3)   
    OR 
       (Cliente.CtCheq1 <> '' AND blk1) AND 
       (((Cliente.CtaCheq2 <> '' AND blk2) AND Cliente.CtaCheq3 = '') OR
       ((Cliente.CtaCheq3 <> '' AND blk3) AND Cliente.CtaCheq2 = '')) 
    OR 
       (Cliente.CtaCheq2 <> '' AND blk2) AND 
       (((Cliente.CtCheq1 <> '' AND blk1) AND Cliente.CtaCheq3 = '') OR
       ((Cliente.CtaCheq3 <> '' AND blk3) AND Cliente.CtCheq1 = ''))
    OR     
       (Cliente.CtaCheq3 <> '' AND blk3) AND 
       (((Cliente.CtCheq1 <> '' AND blk1) AND Cliente.CtaCheq2 = '') OR
       ((Cliente.CtaCheq2 <> '' AND blk2) AND Cliente.CtCheq1 = ''))    
    OR 
       (Cliente.CtCheq1 <> '' AND blk1) AND Cliente.CtaCheq2 = '' AND Cliente.CtaCheq3 = ''
    OR     
       (Cliente.CtaCheq2 <> '' AND blk2) AND Cliente.CtCheq1 = '' AND Cliente.CtaCheq3 = ''
    OR
       (Cliente.CtaCheq3 <> '' AND blk3) AND Cliente.CtaCheq2 = '' AND Cliente.CtaCheq3 = ''    
        
    THEN ASSIGN l-SwContEfec = "**SOLO EFECTIVO**".   
           
END.
IF AVAILABLE Cliente AND Cliente.Id-Calidad = 35 AND l-SwContEfec MATCHES "*EFECTIVO*" THEN
   ASSIGN l-SwContEfec = "** NO CHEQUES **".
    
ASSIGN l-DescrAnt = l-SWContEfec.
FOR EACH EstPedido WHERE EstPedido.Id-Factura = Remision.Id-remis NO-LOCK,
    FIRST Pedido WHERE Pedido.Id-Pedido = EstPedido.Id-Pedido
                   AND Pedido.Resto = EstPedido.Id-Seq NO-LOCK:
    IF Pedido.TarjetaCR = '' THEN DO:
        ASSIGN l-SwContEfec = l-DescrAnt.
        LEAVE.
    END.
    ASSIGN l-SwContEfec = '**PAGADO CON TC**'.
END.

/*
ASSIGN l-MetodoDePago = "No identificado"
       l-NumCtaPago   = "No identificado"
       l-FecCheque    = ?.
IF AVAILABLE MovCaja THEN DO:
    FIND FIRST DetMovC WHERE DetMovC.Id-Caja = MovCaja.Id-Caja
                         AND DetMovC.Folio = MovCaja.Folio
                         NO-LOCK NO-ERROR.
    IF AVAILABLE DetMovC THEN DO:
       IF DetMovC.Id-TP = 60 THEN                     
          ASSIGN l-MetodoDePago = "EFECTIVO".
       ELSE IF LENGTH(TRIM(DetMovC.CtaCheq)) >= 4 THEN DO: 
           IF DetMovC.Id-TP = 61 THEN                     
              ASSIGN l-MetodoDePago = "CHEQUE NOMINATIVO"
                     l-NumCtaPago   = SUBSTRING(TRIM(DetMovC.CtaCheq),LENGTH(TRIM(DetMovC.CtaCheq)) - 3)
                     l-FecCheque    = IF DetMovC.FecCheque > TODAY THEN DetMovC.FecCheque ELSE ?.
           ELSE IF DetMovC.Id-TP = 62 THEN                     
              ASSIGN l-MetodoDePago = "TARJETA DE CREDITO"
                     l-NumCtaPago   = SUBSTRING(TRIM(DetMovC.CtaCheq),LENGTH(TRIM(DetMovC.CtaCheq)) - 3).
           ELSE IF DetMovC.Id-TP = 52 THEN                     
              ASSIGN l-MetodoDePago = "TARJETA DE DEBITO"
                     l-NumCtaPago   = SUBSTRING(TRIM(DetMovC.CtaCheq),LENGTH(TRIM(DetMovC.CtaCheq)) - 3).
       END.
    END.
END.
ELSE IF Remision.Id-Cliente = 4136 THEN DO:
    ASSIGN l-MetodoDePago = "PAGO CON TARJETA DE CREDITO"
           l-NumCtaPago   = "1004".
END.
ELSE IF AVAILABLE Cliente AND Cliente.FEFormaPago > "" AND Cliente.FEDigitosCuenta > "" THEN DO:
    ASSIGN l-MetodoDePago = TRIM(Cliente.FEFormaPago)
           l-NumCtaPago   = TRIM(Cliente.FEDigitosCuenta).
END.
*/

ASSIGN l-MetodoDePago = "PUE"
       l-NumCtaPago   = ""
       l-FormaDePago  = ""
       l-FecCheque    = ?.

/*
Se elimino la asignacion de lo capturado en telemky. Si no tiene pago, va a PPD
IF Remision.Id-Vendedor = "0100" THEN DO:
    ASSIGN l-NumCtaPago   = Remision.FeDigitosCuenta
           l-FormaDePago  = Remision.FeFormaPago.
END.
ELSE IF NOT AVAILABLE MovCaja THEN DO:
    ASSIGN l-MetodoDePago = "PPD"
           l-FormaDePago  = "99".
END.
*/

IF Remision.FeFormaPago <> "" AND Remision.VersionSAT <> "" THEN DO:
    ASSIGN l-NumCtaPago   = Remision.FeDigitosCuenta
           l-FormaDePago  = Remision.FeFormaPago.
END.
ELSE IF Remision.NumTicket <> "" THEN DO:
        FIND BuffRemis WHERE BuffRemis.Id-Remision = Remision.NumTicket
                         AND BuffRemis.TipoVenta = 1
                         NO-LOCK NO-ERROR.
        IF AVAILABLE buffRemis THEN
           FIND buffMovCaja WHERE buffMovCaja.Referencia = buffRemis.Id-Remision
                              AND buffMovCaja.TipoVenta = buffRemis.TipoVenta
                              NO-LOCK NO-ERROR.
        ELSE RELEASE buffMovCaja.
        IF AVAILABLE buffRemis AND AVAILABLE buffMovCaja THEN DO:
            FOR EACH DetMovC WHERE DetMovC.Id-Caja = buffMovCaja.Id-Caja
                               AND DetMovC.Folio = buffMovCaja.Folio
                               NO-LOCK BY DetMovC.MontoPago DESCENDING:
               IF DetMovC.Id-TP = 60 THEN                     
                  ASSIGN l-FormaDePago = "01".
               ELSE IF DetMovC.Id-TP = 57 THEN                     
                       ASSIGN l-FormaDePago = "03".
                   ELSE DO: 
                       IF DetMovC.Id-TP = 61 THEN                     
                          ASSIGN l-FormaDePago = "02"
                                 l-FecCheque    = IF DetMovC.FecCheque > TODAY THEN DetMovC.FecCheque ELSE ?.
                       ELSE IF DetMovC.Id-TP = 62 THEN                     
                          ASSIGN l-FormaDePago = "04".
                       ELSE IF DetMovC.Id-TP = 52 THEN                     
                          ASSIGN l-FormaDePago = "28".
        
                       ASSIGN l-NumCtaPago = l-NumCtaPago + MINIMUM(l-NumCtaPago,",") +
                                             (IF LENGTH(TRIM(DetMovC.CtaCheq)) >= 4 THEN 
                                              SUBSTRING(TRIM(DetMovC.CtaCheq),LENGTH(TRIM(DetMovC.CtaCheq)) - 3)
                                              ELSE "").
                   END.
               LEAVE.
            END.
        END.
     END.
ELSE IF AVAILABLE MovCaja THEN DO:
    FOR EACH DetMovC WHERE DetMovC.Id-Caja = MovCaja.Id-Caja
                       AND DetMovC.Folio = MovCaja.Folio
                       NO-LOCK BY DetMovC.MontoPago DESCENDING:

       IF DetMovC.Id-TP = 60 THEN                     
          ASSIGN l-FormaDePago = "01".
       ELSE IF DetMovC.Id-TP = 57 THEN                     
               ASSIGN l-FormaDePago = "03".
           ELSE DO: 
               IF DetMovC.Id-TP = 61 THEN                     
                  ASSIGN l-FormaDePago = "02"
                         l-FecCheque    = IF DetMovC.FecCheque > TODAY THEN DetMovC.FecCheque ELSE ?.
               ELSE IF DetMovC.Id-TP = 62 THEN                     
                  ASSIGN l-FormaDePago = "04".
               ELSE IF DetMovC.Id-TP = 52 THEN                     
                  ASSIGN l-FormaDePago = "28".

               ASSIGN l-NumCtaPago = l-NumCtaPago + MINIMUM(l-NumCtaPago,",") +
                                     (IF LENGTH(TRIM(DetMovC.CtaCheq)) >= 4 THEN 
                                      SUBSTRING(TRIM(DetMovC.CtaCheq),LENGTH(TRIM(DetMovC.CtaCheq)) - 3)
                                      ELSE "").
           END.
       LEAVE.
    END.
END.
/*
ELSE IF AVAILABLE Cliente AND Cliente.FEFormaPago > "" THEN DO:
    ASSIGN l-MetodoDePago = TRIM(Cliente.FEFormaPago).
    IF Cliente.FEDigitosCuenta > "" THEN
       ASSIGN l-NumCtaPago = TRIM(Cliente.FEDigitosCuenta).
END.*/

IF l-FormaDePago = '' THEN
   ASSIGN l-FormaDePago = '99'
          l-NumCtaPago = ''.



IF Remision.Id-Entrega = 4 THEN DO:
    l-SwContEfec =  "".
    IF l-FecCheque <> ? THEN l-SwContEfec = l-SwContEfec + "   CHP:" + STRING(l-FecCheque,"99/99/99").
END.
IF Remision.Id-Cliente = 33531 THEN DO:
   l-SwContEfec = "".
END.

/* --------------------------------------------------------------------------------------------- */
    
/* 1. Datos de Facturacion del Cliente o Receptor */
/*
v-razon = (IF Remision.Id-Cliente > 101 AND AVAILABLE Cliente AND Cliente.Tipo = 1 THEN Cliente.Propietario ELSE Remision.RazonSocial).
*/

IF LENGTH(TRIM(Remision.RFC)) = 12 AND Remision.NomEmpresa > "" THEN
   v-razon = TRIM(Remision.NomEmpresa).
ELSE v-razon = TRIM(Remision.RazonSocial).
/*v-razon = REPLACE(v-razon,'&','&amp;').*/
v-razon = REPLACE(v-razon,CHR(38),'&#38;').
v-razon = REPLACE(v-razon, '"', '&quot;').
v-razon = TRIM(REPLACE(v-razon,'|',' ')).    
v-razon = REPLACE(v-razon,"'",'&apos;').
v-razon = REPLACE(v-razon,'<','&lt;').
v-razon = REPLACE(v-razon,'>','&gt;').
v-razon = REPLACE(v-razon,CHR(165),'&#209;').
v-razon = REPLACE(v-razon,CHR(154),'&#220;').
RUN Colapsa(INPUT-OUTPUT v-razon).

v-colonia = Remision.Colonia.
v-colonia = REPLACE(v-colonia,'&','&amp;').
v-colonia = REPLACE(v-colonia, '"', '&quot;').
v-colonia = TRIM(REPLACE(v-colonia,'|',' ')).    
v-colonia = REPLACE(v-colonia,"'",'&apos;').
v-colonia = REPLACE(v-colonia,'<','&lt;').
v-colonia = REPLACE(v-colonia,'>','&gt;').
RUN Colapsa(INPUT-OUTPUT v-colonia).

v-rfcCte = CAPS(TRIM(REPLACE(Remision.rfc,' ',''))).
RUN ValidaRFC(INPUT v-rfcCte, OUTPUT v-okRFC).
IF v-okRFC = FALSE THEN DO:
    MESSAGE "El RFC " + v-rfcCte + " es invalido, favor de modificarlo.".
    PAUSE 2 NO-MESSAGE.
    LEAVE.
END.

FIND FIRST RfcCfdiEsp WHERE RfcCfdiEsp.RFC = v-rfcCte NO-LOCK NO-ERROR.
v-rfcCte = REPLACE(v-rfcCte,'&','&amp;').

IF Remision.BuzonFiscal <> "" THEN 
   v-correo = Remision.BuzonFiscal.
ELSE v-correo = "".
v-correo = REPLACE(v-correo,'&','&amp;').

v-CPFiscal = IF Remision.CP <> '' THEN TRIM(Remision.CP) ELSE TRIM(Cliente.CP).
IF Remision.Id-UsoCFDI = "" OR Remision.RFC = "XAXX010101000" 
THEN v-UsoCFDI = "S01".
ELSE v-UsoCFDI = CAPS(Remision.Id-UsoCFDI).

IF Remision.RFC = "XAXX010101000" 
THEN ASSIGN v-RFiscal = "616"
            v-CPFiscal = v-LExp.
ELSE v-RFiscal = TRIM(Remision.Id-RFiscal).

IF Remision.RFC = "XEXX010101000" THEN
   ASSIGN v-UsoCFDI = "S01"
          v-RFiscal = "616"
          v-CPFiscal = v-LExp.

IF v-UsoCFDI = "P01" THEN v-UsoCFDI = "G03".

v-recep = '<Receptor rfc="' + v-rfcCte + '" RegimenFiscalReceptor="' + v-RFiscal + '" DomicilioFiscalReceptor="' + v-CPFiscal + '" UsoCFDI="' + TRIM(v-usocfdi) + '" nombre="' + v-razon + '" >'.

/*IF AVAILABLE RfcCfdiEsp AND RfcCfdiEsp.Tipo = 2 THEN*/
   v-enca = '<ImprimeEncabezado DecimalesXml="2" '.
/*ELSE v-enca = '<ImprimeEncabezado '.*/
v-enca = v-enca + ' CodigoBarra="' + Remision.Id-Remision + '" RazonSocial="' + v-razon + '" RFC="R.F.C. ' + v-rfcCte + '" '.

IF v-correo <> "" THEN v-enca = v-enca + 'correo="' + v-correo + '" '.

v-dom = '<Domicilio calle="' + TRIM(Remision.CalleNo) + '" noExterior="" noInterior="" colonia="' + v-Colonia + '" '.
/*v-enca = v-enca + 'CalleNo="' + TRIM(Remision.CalleNo) + '" Colonia="' + (IF LENGTH(TRIM(Remision.Colonia)) > 0 THEN 'COL. ' + Remision.Colonia ELSE '') + '" '.*/
v-enca = v-enca + 'CalleNo="" Colonia="" '.

 /* Buscar si ls direccion del cliente esta dentro de alguna delegacion */
IF Remision.Id-Cliente <> 3 AND AVAILABLE Cliente AND LENGTH(TRIM(Cliente.Delegacion)) <> 0 THEN DO:
        
    v-dom = v-dom + 'localidad="' + TRIM(Cliente.Delegacion) + '" '.
    /*v-enca = v-enca + 'Delegacion="' + TRIM(Cliente.Delegacion) + '" '.*/
    v-enca = v-enca + 'Delegacion="" '.
        
END.
ELSE DO:
    
    v-dom = v-dom + 'localidad="' + TRIM(Remision.Delegacion1) + '" '.
    /*v-enca = v-enca + 'Delegacion="' + TRIM(Remision.Delegacion1) + '" '.*/ 
    v-enca = v-enca + 'Delegacion="" '. 
    
END.
    
/*RUN LlenaDomicilio(INPUT-OUTPUT v-enca, INPUT Remision.Id-Ciudad, INPUT 'Ciudad', INPUT 'Estado', INPUT 'Pais').*/
v-enca = v-enca + 'Ciudad="" Estado="" Pais="" '.
RUN LlenaDomicilio(INPUT-OUTPUT v-dom, INPUT Remision.Id-Ciudad, INPUT 'municipio', INPUT 'estado', INPUT 'pais').

v-dom = v-dom + 'codigoPostal="' + (IF Remision.Id-Cliente <> 3 AND AVAILABLE Cliente AND LENGTH(TRIM(Cliente.CP)) <> 0 THEN TRIM(Cliente.CP) ELSE '') + '" />'.
/*v-enca = v-enca + 'CP="' + (IF Remision.Id-Cliente <> 3 AND AVAILABLE Cliente AND LENGTH(TRIM(Cliente.CP)) <> 0 THEN 'CP ' + TRIM(Cliente.CP) ELSE '') + '" '.*/
v-enca = v-enca + 'CP="" '.

RUN Colapsa(INPUT-OUTPUT v-dom).

v-Referencia = TRIM(Remision.Referencia).
v-Referencia = REPLACE(v-Referencia,CHR(38),'&#38;').
v-Referencia = REPLACE(v-Referencia, '"', '&quot;').
v-Referencia = TRIM(REPLACE(v-Referencia,'|',' ')).    
v-Referencia = REPLACE(v-Referencia,"'",'&apos;').
v-Referencia = REPLACE(v-Referencia,'<','&lt;').
v-Referencia = REPLACE(v-Referencia,'>','&gt;').
v-Referencia = REPLACE(v-Referencia,CHR(165),'&#209;').
v-Referencia = REPLACE(v-Referencia,CHR(154),'&#220;').
RUN Colapsa(INPUT-OUTPUT v-Referencia).

/* 1.1 Datos de Embarque */
IF LENGTH(TRIM(Remision.RazonSocial1)) > 0 OR
   LENGTH(TRIM(Remision.CalleNo1)) > 0 OR 
   LENGTH(TRIM(Remision.Colonia1)) > 0 OR 
   LENGTH(TRIM(Remision.Ciudad1)) > 0 OR 
   LENGTH(TRIM(Remision.Estado1)) > 0 /*OR
   LENGTH(TRIM(Remision.Referencia)) > 0*/ THEN DO:
    
    v-razon1 = Remision.RazonSocial1.
    v-razon1 = REPLACE(v-razon1,'&','&amp;').
    v-razon1 = REPLACE(v-razon1, '"', '&quot;').
    v-razon1 = TRIM(REPLACE(v-razon1,'|',' ')).    
    v-razon1 = REPLACE(v-razon1,"'",'&apos;').
    v-razon1 = REPLACE(v-razon1,'<','&lt;').
    v-razon1 = REPLACE(v-razon1,'>','&gt;').

    v-calleno1 = Remision.CalleNo1.
    v-calleno1 = REPLACE(v-calleno1,'&','&amp;').
    v-calleno1 = REPLACE(v-calleno1, '"', '&quot;').
    v-calleno1 = TRIM(REPLACE(v-calleno1,'|',' ')).    
    v-calleno1 = REPLACE(v-calleno1,"'",'&apos;').
    v-calleno1 = REPLACE(v-calleno1,'<','&lt;').
    v-calleno1 = REPLACE(v-calleno1,'>','&gt;').

    v-colonia1 = Remision.Colonia1.
    v-colonia1 = REPLACE(v-colonia1,'&','&amp;').
    v-colonia1 = REPLACE(v-colonia1, '"', '&quot;').
    v-colonia1 = TRIM(REPLACE(v-colonia1,'|',' ')).    
    v-colonia1 = REPLACE(v-colonia1,"'",'&apos;').
    v-colonia1 = REPLACE(v-colonia1,'<','&lt;').
    v-colonia1 = REPLACE(v-colonia1,'>','&gt;').
    
    RUN Colapsa(INPUT-OUTPUT v-razon1).
    RUN Colapsa(INPUT-OUTPUT v-calleno1).
    RUN Colapsa(INPUT-OUTPUT v-colonia1).

    v-attn = Remision.Attn1.
    v-attn = REPLACE(v-Attn,'&','&amp;').
    v-attn = REPLACE(v-Attn, '"', '&quot;').
    v-attn = TRIM(REPLACE(v-Attn,'|',' ')).    
    v-attn = REPLACE(v-Attn,"'",'&apos;').
    v-attn = REPLACE(v-Attn,'<','&lt;').
    v-attn = REPLACE(v-Attn,'>','&gt;').

    v-enca = v-enca + 'EmbAtencion="' + (IF LENGTH(TRIM(v-Attn)) > 0 THEN 'ATTN:' + v-Attn ELSE '') + '" EmbRazonSocial="' + v-razon1 + '" EmbCalleNo="' + v-CalleNo1 + '" '.
    v-enca = v-enca + 'EmbColonia="' + (IF LENGTH(TRIM(v-Colonia1)) > 0 THEN 'COL. ' + v-Colonia1 ELSE '') + '" '.
    v-enca = v-enca + 'EmbDelegacion="' + (IF LENGTH(TRIM(Remision.Delegacion1)) > 0 THEN Remision.Delegacion1 ELSE '') + '" '.
        
    v-enca = v-enca + 'EmbCiudad="' + Remision.Ciudad1 + '" EmbEstado="' + Remision.Estado1 + '" EmbPais="" '.
    v-enca = v-enca + 'EmbCP="' + Remision.cp1 + '" EmbTel="' + Remision.Tel1 + '" EmbFax="' + v-Referencia + '      ' + l-SwContEfec + '" '.
    
END.
ELSE DO:
    v-attn = Remision.Attn.
    v-attn = REPLACE(v-Attn,'&','&amp;').
    v-attn = REPLACE(v-Attn, '"', '&quot;').
    v-attn = TRIM(REPLACE(v-Attn,'|',' ')).    
    v-attn = REPLACE(v-Attn,"'",'&apos;').
    v-attn = REPLACE(v-Attn,'<','&lt;').
    v-attn = REPLACE(v-Attn,'>','&gt;').
    v-enca = v-enca + 'EmbAtencion="' + (IF LENGTH(TRIM(v-Attn)) > 0 THEN 'ATTN:' + v-Attn ELSE '') + '" EmbRazonSocial="' + REPLACE(v-razon,'"','') + '" EmbCalleNo="' + Remision.CalleNo + '" '.
    v-enca = v-enca + 'EmbColonia="' + (IF LENGTH(TRIM(v-Colonia)) > 0 THEN 'COL. ' + v-Colonia ELSE '') + '" '.
    v-enca = v-enca + 'EmbDelegacion="' + (IF LENGTH(TRIM(Remision.Delegacion)) > 0 THEN Remision.Delegacion ELSE '') + '" '.
        
    RUN LlenaDomicilio(INPUT-OUTPUT v-enca, INPUT Remision.Id-Ciudad, INPUT 'EmbCiudad', INPUT 'EmbEstado', INPUT 'EmbPais').
    v-enca = v-enca + 'EmbCP="' + (IF Remision.CP <> '' THEN TRIM(Remision.CP) ELSE TRIM(Cliente.CP)) + 
                       '" EmbTel="' + Remision.Tel + '" EmbFax="' + v-Referencia + '      ' + l-SwContEfec + '" '.
END.

IF AVAILABLE Transporte THEN
    v-enca = v-enca + 'Transporte="E' + (IF AVAILABLE Cliente THEN STRING(Remision.Id-RutaEmb,"99") ELSE '') + ' ' + 
                      STRING(Transporte.Id-Tran) + ' ' + Transporte.Nombre + '" '.
ELSE
    v-enca = v-enca + 'Transporte="E' + (IF AVAILABLE Cliente THEN STRING(Remision.Id-RutaEmb,"99") ELSE '') + '" '.

/*
ASSIGN msuma = 0.
DO l-k = 1 TO 5:
    
    ASSIGN msuma = msuma + (INTEGER(SUBSTRING(STRING(Remision.Id-Cliente,"99999"),l-k,1)) * (7 - l-k)).
    
END.
    
ASSIGN mfa_veri = 7 - (msuma MODULO 7).
*/
RUN /usr2/adosa/procs/vtad1000.p(INPUT Remision.Id-Cliente, OUTPUT mfa_veri).

/* 2. Datos de Venta y Totales */
v-comprobante = v-comprobante + 'condicionesDePago="CONTADO" '.
v-enca = v-enca + 'Condicion="CONTADO" '.

v-enca = v-enca + 'Vencimiento="' + STRING(DAY(Remision.fecreg),"99") + '-' + ENTRY(MONTH(Remision.fecreg),v-listaMes) + '-' + STRING(YEAR(Remision.fecreg),">>>9") + '" '. 
v-enca = v-enca + 'Autorizacion="' + (IF AVAILABLE MovCaja THEN 'T-' + STRING(MovCaja.Turno,"9") + '/C-' + STRING(MovCaja.Id-Caja) + '/OP-' + STRING(MovCaja.Folio,"999999") ELSE '') + '" '.

v-enca = v-enca + 'Pedido="' + SUBSTRING(Remision.Pedidos,1,25) + '" '.
v-enca = v-enca + 'Requisicion="' + SUBSTRING(Remision.Requisicion,1,25) + '" '.
v-enca = v-enca + 'cliente="' + STRING(Remision.Id-Cliente) + "-" + STRING(mfa_veri,'99') + '" '.
v-enca = v-enca + 'Fecha="' + STRING(DAY(Remision.FecReg),">9") + '-' + ENTRY(MONTH(Remision.FecReg),v-listaMes) + '-' + STRING(YEAR(Remision.FecReg),">>>9") + '" '.
v-enca = v-enca + 'Id_Factura="' + Remision.Id-Remision + '" '.
v-enca = v-enca + 'Propietario="' + (IF LENGTH(TRIM(Remision.Propietario)) > 0 AND Cliente.Tipo = 1 THEN STRING(Remision.Propietario, "x(50)") ELSE '') + '" '.
v-enca = v-enca + 'Tarimas="' + STRING(Remision.Tarimas,"zz9") + '" '.
v-enca = v-enca + 'Bultos="' + STRING(Remision.Bultos,"zz9") + '" '.
v-enca = v-enca + 'Entrega="' + STRING(IF AVAILABLE Entrega THEN Entrega.Descr ELSE '',"x(20)") + '" '.
v-enca = v-enca + 'Vendedor="' + STRING(Remision.Id-Vendedor) + ' ' + (IF AVAILABLE Empleado THEN STRING(Empleado.Nombre, "x(14)") ELSE '') + '" '.
v-enca = v-enca + 'Zona="" '.
v-enca = v-enca + 'Cobrador="" '.

v-enca = REPLACE(v-enca,'�','a').
v-enca = REPLACE(v-enca,'�','A').
v-enca = REPLACE(v-enca,'�','e').
v-enca = REPLACE(v-enca,'�','E').
v-enca = REPLACE(v-enca,'�','i').
v-enca = REPLACE(v-enca,'�','I').
v-enca = REPLACE(v-enca,'�','o').
v-enca = REPLACE(v-enca,'�','O').
v-enca = REPLACE(v-enca,'�','u').
v-enca = REPLACE(v-enca,'�','U').

l-LeyendaMetodo =  "METODO DE PAGO: " + l-MetodoDePago.
IF l-NumCtaPago <> "" THEN l-LeyendaMetodo = l-LeyendaMetodo + ", FORMA DE PAGO: " + l-FormaDePago.
/*IF l-NumCtaPago <> "" THEN l-LeyendaMetodo = l-LeyendaMetodo + ", CUENTA: " + l-NumCtaPago.*/

v-enca = v-enca + 'RutaEmbarque="' + TRIM(l-LeyendaMetodo) + '" '.

/* 2.1 Datos de totales de la factura */

/*Cambio Junio 2011: la forma de pago debe ser PAGO EN UNA SOLA EXHIBICION*/


v-comprobante = v-comprobante + 'subTotal="' + STRING(Remision.SubTotal) + '" '.
IF Remision.Descuento > 0 THEN
   v-comprobante = v-comprobante + ' descuento="' + STRING(Remision.Descuento) + '" motivoDescuento="" '.

v-comprobante = v-comprobante + 'total="' + STRING(Remision.Tot) + '" Moneda="MXN" TipoCambio="1" metodoDePago="' + l-MetodoDePago + '" '.
v-comprobante = v-comprobante + 'formaDePago="' + l-FormaDePago + '" '.

IF l-NumCtaPago <> '' THEN
   v-comprobante = v-comprobante + 'NumCtaPago="' + l-NumCtaPago + '" '.
v-comprobante = v-comprobante + 'tipoDeComprobante="ingreso" ReferenciaCFD="' + Remision.Id-Remision + '" xmlns="http://www.sat.gob.mx/cfd/2">'.

v-enca = v-enca + 'SubTotal="' + (IF Remision.Descuento <> 0 THEN STRING(Remision.SubTotal,"$>>,>>>,>>9.99") ELSE "") + '" '. 
v-enca = v-enca + 'Descuento="' + (IF Remision.Descuento <> 0 THEN ('-' + TRIM(STRING(Remision.Descuento1,"ZZ9")) +
                                                                    (IF Remision.Descuento2 <> 0 THEN '-' + TRIM(STRING(Remision.Descuento2,"ZZ9")) ELSE "  ") +
                                                                    (IF Remision.Descuento3 <> 0 THEN '-' + TRIM(STRING(Remision.Descuento3,"ZZ9")) ELSE "  ") +
                                                                    ' ' + TRIM(STRING(Remision.Descuento,"->>,>>>,>>9.99"))) ELSE '') + '" '.
v-enca = v-enca + 'ValorNeto="' + (IF Remision.Descuento <> 0 THEN STRING(Remision.SubTotal - Remision.Descuento,'$>>,>>>,>>9.99') ELSE STRING(Remision.SubTotal,"$>>,>>>,>>9.99")) + '" IVA="'+ STRING(Remision.Iva,"$>>,>>>,>>9.99") + '" '.
v-enca = v-enca + 'FleteEtiqueta="" FleteValor="" Total="' + STRING(Remision.Tot,"$>>,>>>,>>9.99") + '" '.

v-letras = ''.
RUN /usr2/adosa/procs/rtn0005.p(INPUT Remision.Tot, OUTPUT v-letras).
    
v-enca = v-enca + 'DiaPago="" CantLetras="' + v-letras + '" Interes="" Expedida="' + v-exped + '"/>'.
    
/* 3. Datos de Renglones de la Factura */
v-reng = 1.

l-NegImp = 0.
l-NegIva = 0.
EMPTY TEMP-TABLE tt-DetRemis.

FIND FIRST DetRemis WHERE DetRemis.Id-Remision = Remision.Id-Remision
                      AND (DetRemis.Descr BEGINS "VALE"
                      OR DetRemis.Descr MATCHES "*CUPON*")
                      AND DetRemis.Importe < 0
                      NO-LOCK NO-ERROR.
IF AVAILABLE DetRemis /*AND Remision.RFC = "XAXX010101000"*/ THEN DO:
   l-ImpExento = 0.
   FOR EACH DetRemis OF Remision WHERE DetRemis.Tipo = 1
                                    AND DetRemis.PorcIva = 0
                                    AND DetRemis.Importe > 0
                                    NO-LOCK:
       l-ImpExento = l-ImpExento + DetRemis.Importe.
   END.
   IF l-ImpExento > 0 THEN DO:
      FIND Articulo WHERE Articulo.Id-Articulo = "074961" NO-LOCK NO-ERROR.
      CREATE tt-DetRemis.
      FIND FIRST DetRemis OF Remision WHERE DetRemis.Tipo = 1
                                      AND DetRemis.PorcIva = 0
                                      AND DetRemis.Importe > 0
                                      NO-LOCK NO-ERROR.
      BUFFER-COPY DetRemis TO tt-DetRemis
            ASSIGN tt-DetRemis.Id-Articulo = Articulo.Id-Articulo
                   tt-DetRemis.Descr     = Articulo.Descr
                   tt-DetRemis.Cant      = 1
                   tt-DetRemis.Id-Pres   = 3
                   tt-DetRemis.Id-Color  = 0
                   tt-DetRemis.PorcDesc  = 0
                   tt-DetRemis.IndicaPre = "*"
                   tt-DetRemis.PrecUnit  = l-ImpExento
                   tt-DetRemis.Importe   = l-ImpExento
                   tt-DetRemis.PorcIva   = 0
                   tt-DetRemis.Iva       = 0
                   tt-DetRemis.Bonifica  = 0.
   END.
   FOR FIRST DetRemis OF Remision WHERE DetRemis.Tipo = 1
                                    AND DetRemis.PorcIva > 0
                                    AND DetRemis.Importe > 0
                                    NO-LOCK:
       FIND Articulo WHERE Articulo.Id-Articulo = "074962" NO-LOCK NO-ERROR.
       CREATE tt-DetRemis.
       BUFFER-COPY DetRemis TO tt-DetRemis
            ASSIGN tt-DetRemis.Id-Articulo = Articulo.Id-Articulo
                   tt-DetRemis.Descr     = Articulo.Descr
                   tt-DetRemis.Cant      = 1
                   tt-DetRemis.Id-Pres   = 3
                   tt-DetRemis.Id-Color  = 0
                   tt-DetRemis.PorcDesc  = 0
                   tt-DetRemis.IndicaPre = "*"
                   tt-DetRemis.PrecUnit  = Remision.SubTotal - l-ImpExento
                   tt-DetRemis.Importe   = Remision.SubTotal - l-ImpExento
                   tt-DetRemis.PorcIva   = IF Remision.Iva > 0 THEN 16 ELSE 0
                   tt-DetRemis.Iva       = tt-DetRemis.Importe * 
                                           (tt-DetRemis.Porciva / 100)
                   tt-DetRemis.Bonifica  = 0.
   END.
END.
ELSE DO:
   FOR EACH DetRemis OF Remision NO-LOCK:
       CREATE tt-DetRemis.
       BUFFER-COPY DetRemis TO tt-DetRemis
            ASSIGN tt-DetRemis.Bonifica = 0.
       IF DetRemis.Tipo = 2 AND DetRemis.PrecUnit < 0 THEN
          ASSIGN l-NegImp = l-NegImp + ABS(DetRemis.Importe)
                 l-NegIva = l-NegIva + TRUNCATE(ABS(DetRemis.Iva),3).
   END.
END.
IF Remision.Descuento > 0 AND Remision.Descuento1 > 0 THEN DO:
   l-TmpDesc = 0.
   FOR EACH tt-DetRemis WHERE tt-DetRemis.Tipo <= 2 AND tt-DetRemis.NoPP = FALSE AND tt-DetRemis.IndicaPre <> "D" EXCLUSIVE-LOCK:
       ASSIGN tt-DetRemis.Bonifica = ROUND(tt-DetRemis.Importe * (Remision.Descuento1 / 100),2).
       ASSIGN tt-DetRemis.Iva      = ROUND((tt-DetRemis.Importe - tt-DetRemis.Bonifica) * (tt-DetRemis.PorcIva / 100),6).
       l-TmpDesc = l-TmpDesc + tt-DetRemis.Bonifica.
   END.
   IF l-TmpDesc <> Remision.Descuento THEN DO:
      l-DifDesc = Remision.Descuento - l-TmpDesc.
      FOR EACH tt-DetRemis WHERE tt-DetRemis.Tipo <= 2 AND tt-DetRemis.NoPP = FALSE AND tt-DetRemis.IndicaPre <> "D"
          EXCLUSIVE-LOCK BY tt-DetRemis.Bonifica DESCENDING:
          ASSIGN tt-DetRemis.Bonifica = tt-DetRemis.Bonifica + l-DifDesc.
          ASSIGN tt-DetRemis.Iva      = ROUND((tt-DetRemis.Importe - tt-DetRemis.Bonifica) * (tt-DetRemis.PorcIva / 100),6).
          LEAVE.
      END.
   END.        
END.
IF l-NegImp > 0 AND l-NegIva > 0 THEN DO:
   l-TmpDesc = 0.
   FOR EACH tt-DetRemis WHERE tt-DetRemis.Tipo <= 2 AND tt-DetRemis.Iva > 0 
       EXCLUSIVE-LOCK BY tt-DetRemis.Importe DESCENDING:
       l-NegPosible = tt-DetRemis.Importe - tt-DetRemis.Bonifica.
       IF l-NegPosible >= l-NegImp THEN DO:
          ASSIGN tt-DetRemis.Bonifica = ROUND(tt-DetRemis.Bonifica + l-NegImp,2).
          ASSIGN tt-DetRemis.Iva      = ROUND(tt-DetRemis.Iva - l-NegIva,6).
          l-TmpDesc = l-TmpDesc + l-NegImp.
          l-NegImp = l-NegImp - l-NegImp.
          l-NegIva = l-NegIva - l-NegIva.
       END.
       ELSE IF l-NegPosible > 0 THEN DO:
          l-NegIva = l-NegIva - tt-DetRemis.Iva. 
          ASSIGN tt-DetRemis.Bonifica = ROUND(tt-DetRemis.Bonifica + l-NegPosible,2).
          ASSIGN tt-DetRemis.Iva      = 0.
          l-TmpDesc = l-TmpDesc + l-NegPosible.
          l-NegImp = l-NegImp - l-NegPosible.
       END.
       /*IF l-TmpDesc >= l-NegImp THEN LEAVE.*/
       IF l-NegImp <= 0 THEN LEAVE.
   END.
END.


EMPTY TEMP-TABLE w-Impuestos.
FOR EACH tt-DetRemis OF Remision NO-LOCK:

    IF tt-DetRemis.Tipo = 1 THEN DO:
        FIND ArtPres WHERE ArtPres.Id-Pres = tt-DetRemis.Id-Pres
                     AND ArtPres.Id-Art = tt-DetRemis.Id-Articulo NO-LOCK NO-ERROR.
        FIND Kolor WHERE Kolor.Id-Color = tt-DetRemis.Id-Color NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        RELEASE ArtPres.
        RELEASE Kolor.
    END.
        
    v-descrip = TRIM(tt-DetRemis.Descr).
    IF AVAILABLE Kolor AND tt-DetRemis.Id-Color > 0 THEN
        v-descrip = TRIM(v-descrip) + ' ' + TRIM(Kolor.Descr).
    
    v-descrip = REPLACE(v-descrip,'&','&amp;').
    v-descrip = REPLACE(v-descrip,'"','&quot;').
    v-descrip = REPLACE(v-descrip,"'",'&apos;').
    v-descrip = REPLACE(v-descrip,'<','&lt;').
    v-descrip = REPLACE(v-descrip,'>','&gt;').
        
    RUN Colapsa(INPUT-OUTPUT v-descrip).
    
    l-ClavePS = "".
    FIND Articulo WHERE Articulo.Id-Articulo = tt-DetRemis.Id-Articulo NO-LOCK NO-ERROR.
    IF tt-DetRemis.Tipo <= 2 AND tt-DetRemis.PrecUnit > 0 AND
       tt-DetRemis.Importe > 0 THEN DO:
        IF tt-DetRemis.Tipo = 1 AND AVAILABLE Articulo AND Articulo.Id-ClavePS > "" 
        THEN l-ClavePS = Articulo.Id-ClavePS.
        ELSE l-ClavePS = "01010101".

        FIND w-Impuestos WHERE w-Impuestos.Tasa = (tt-DetRemis.PorcIVA / 100) EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-Impuestos THEN DO:
           CREATE w-Impuestos.
           ASSIGN w-Impuestos.Tasa = tt-DetRemis.PorcIva / 100.
        END.
        ASSIGN w-Impuestos.Base = w-Impuestos.Base + tt-DetRemis.Importe
             w-Impuestos.Bonifica = w-Impuestos.Bonifica + tt-DetRemis.Bonifica
             w-Impuestos.Importe = w-Impuestos.Importe + tt-DetRemis.IVA.
        RELEASE w-Impuestos.

        
        v-descrip = TRIM(v-descrip + ' ' + (IF AVAILABLE ArtPres AND NOT ArtPres.Descr BEGINS 'PZ' 
                                                     AND NOT ArtPres.Descr = 'PIEZA' THEN TRIM(ArtPres.Descr) ELSE '')).
        
        v-concep = v-concep + '<Concepto cantidad="' + STRING(tt-DetRemis.Cant) + '" '. 
        v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="1" descripcion="' + TRIM(v-descrip) + '" '.
        
        v-concep = v-concep + 'unidad="PIEZA" ClaveUnidad="H87" ClaveProdServ="' + TRIM(l-ClavePS) + '" '.

        v-deta = v-deta + 'unidad="PIEZA" '.
/*            
        v-concep = v-concep + 'unidad="' + (IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE 'P') + '" '.
        v-deta = v-deta + 'unidad="' + (IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE '') + '" '.
*/
        v-concep = v-concep + 'noIdentificacion="' + tt-DetRemis.Id-Articulo + '" descripcion="' + TRIM(v-descrip) + '" '.
        IF tt-DetRemis.Bonifica > 0 THEN
           v-concep = v-concep + 'Descuento="' + TRIM(STRING(tt-DetRemis.Bonifica,'zzzzzzzzzzz9.99')) + '" '.

        IF tt-DetRemis.PorcDesc > 0 OR tt-DetRemis.IndicaPre = "*" OR
           tt-DetRemis.IndicaPre = "P" THEN
           v-concep = v-concep + 'valorUnitario="' + TRIM(STRING(tt-DetRemis.Importe / tt-DetRemis.Cant,'zzzzzzzzzzz9.999999')) + '" importe="' + TRIM(STRING(tt-DetRemis.Importe,'zzzzzzzzzzz9.99')) + '">'.
        ELSE v-concep = v-concep + 'valorUnitario="' + STRING(tt-DetRemis.PrecUnit) + '" importe="' + TRIM(STRING(tt-DetRemis.Importe,'zzzzzzzzzzz9.99')) + '">'.
        
        IF AVAILABLE Articulo AND Articulo.Pedimento THEN DO:
            FIND Puerto WHERE Puerto.Id-Puerto = Articulo.Id-Puerto NO-LOCK NO-ERROR.
            IF AVAILABLE Puerto THEN DO:
                IF LENGTH(TRIM(Articulo.PedNo)) >= 15 THEN DO: 
                    IF Articulo.FecPed <> ? THEN
                       v-fecAdu = STRING(YEAR(Articulo.FecPed)) + "-" + STRING(MONTH(Articulo.FecPed), "99") + "-" + STRING(DAY(Articulo.FecPed), "99").
                    ELSE v-fecAdu = STRING(YEAR(TODAY)) + "-" + STRING(MONTH(TODAY), "99") + "-" + STRING(DAY(TODAY), "99").
                    v-pedNo = TRIM(SUBSTRING(Articulo.PedNo,1,2) + "  " + 
                                   SUBSTRING(Articulo.PedNo,3,2) + "  " +
                                   SUBSTRING(Articulo.PedNo,5,4) + "  " +
                                   SUBSTRING(Articulo.PedNo,9,7)).
                    
                    
                    RUN Colapsa(INPUT-OUTPUT v-puerto).
                    /*RUN Colapsa(INPUT-OUTPUT v-pedNo).*/
                    v-concep = v-concep + '<InformacionAduanera numero="' + v-pedNo + '" fecha="' + v-fecAdu + '" aduana="' + v-puerto + '" />'.
                    FIND v-pedim WHERE v-pedim.PedNo = v-pedNo NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE v-pedim THEN DO:
                        CREATE v-pedim.
                        ASSIGN v-pedim.PedNo = v-PedNo
                               v-pedim.FecPed = Articulo.FecPed
                               v-pedim.Puerto = Puerto.Nombre.
                    END.
                END.
            END.
        END.
        
        IF tt-DetRemis.PorcIVA > 0 AND (tt-DetRemis.Importe - tt-DetRemis.Bonifica) > 0 THEN DO:
            ASSIGN v-concep = v-concep + '<Impuestos><Traslados><Traslado Base="' + TRIM(STRING(tt-DetRemis.Importe - tt-DetRemis.Bonifica,'zzzzzzzzzzz9.99')) + '" Impuesto="002" TipoFactor="Tasa" TasaOCuota="' +
                   STRING(tt-DetRemis.PorcIva / 100,"9.999999") + '" Importe="' + TRIM(STRING(tt-DetRemis.IVA,"zzzzzzzz9.999999")) + '"/></Traslados></Impuestos>'.
        END.
        /*ELSE ASSIGN v-concep = v-concep + '<Impuestos/>'.*/
        
        v-concep = v-concep + '</Concepto>'.

        v-deta = v-deta + 'porcIVA="' + STRING(tt-DetRemis.PorcIva,">9%") + '" tipoPrecio="' + (IF tt-DetRemis.PorcIVA = 0 THEN 'X' ELSE tt-DetRemis.IndicaPre) + '" '.
            
        v-deta = v-deta + 'noIdentificacion="' + tt-DetRemis.Id-Articulo + '" cantidad="' + STRING(tt-DetRemis.Cant) + '" '.
        v-deta = v-deta + 'valorUnitario="' + STRING(tt-DetRemis.PrecUnit,"->>,>>>,>>9.99") + '" importe="' + STRING(tt-DetRemis.Importe,"->>,>>>,>>9.99") + '" comentario="" '.
        v-deta = v-deta + 'descuento="' + (IF tt-DetRemis.PorcDesc > 0 THEN STRING(tt-DetRemis.PorcDesc,">9%") ELSE '') + '"/>'.
            
        v-reng = v-reng + 1. 
        
    END.
    ELSE IF tt-DetRemis.Tipo = 6 THEN DO:
        /*
        v-concep = v-concep + '<Concepto cantidad="1" unidad="No Aplica" noIdentificacion="" descripcion="' + TRIM(v-descrip) + '" valorUnitario="' + STRING(tt-DetRemis.PrecUnit) + '" importe="' + STRING(tt-DetRemis.Importe) + '"/>'. 
        */
        v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="1" descripcion="' + TRIM(v-descrip) + '" '.
        v-deta = v-deta + 'unidad="No Aplica" porcIVA="" tipoPrecio="' + tt-DetRemis.IndicaPre + '" noIdentificacion="" cantidad="" valorUnitario="" importe="' + STRING(tt-DetRemis.Importe,"->>,>>>,>>9.99") + '" '.
        v-deta = v-deta + 'comentario="" descuento=""/>'. 
            
        v-reng = v-reng + 1.
    
    END.
    ELSE DO:        
        
        v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" '.
        v-deta = v-deta + 'unidad="" porcIVA="" tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
        v-deta = v-deta + 'comentario="' + TRIM(v-descrip) + '" descuento=""/>'. 
            
        v-reng = v-reng + 1.
            
    END.

    FOR EACH DetSerie WHERE DetSerie.Tipo = 'Remision'
                      AND DetSerie.Documento = Remision.Id-Remision
                      AND DetSerie.Reng = tt-DetRemis.Sec NO-LOCK:
        
        v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" '.
        v-deta = v-deta + 'unidad="" porcIVA="" tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
        v-deta = v-deta + 'comentario="No. DE SERIE: ' + TRIM(DetSerie.SerNo) + '" descuento=""/>'.
            
        v-reng = v-reng + 1.
        
    END.
        

END.

IF v-concep = "" THEN LEAVE. /* Factura sin monto no se timbran */

/* 3.1 Agregar los datos de los pedidos */
IF Remision.Pedidos <> "" AND (Remision.Id-Ubic = 'MAS' OR Remision.Id-Ubic = 'MMT') THEN DO:
        
    DO i = 1 TO NUM-ENTRIES(Remision.Pedidos):
    
        v-pedido = ENTRY(i,Remision.Pedidos).
        
        FIND LAST Pedido WHERE Pedido.Id-Pedido = v-pedido NO-LOCK NO-ERROR.
        
        IF NOT AVAILABLE Pedido THEN DO:
        
            v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" unidad="" porcIVA="" '.
            v-deta = v-deta + 'tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
            v-deta = v-deta + 'comentario="' + v-pedido + ' NO ENCONTRADO" descuento=""/>'.
            
            v-reng = v-reng + 1. 
        
        END.
        ELSE DO:
        
            FIND Vend WHERE Vend.Id-Vend = Pedido.Id-Vendedor NO-LOCK NO-ERROR.
            FIND Usuario WHERE Usuario.Id-User = Vendedor.inic NO-LOCK NO-ERROR.
        
            v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" unidad="" porcIVA="" '.
            v-deta = v-deta + 'tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
            v-deta = v-deta + 'comentario="' + Pedido.Id-Pedido + ' ' + Vendedor.Id-Vend + ' ' + Usuario.nom + '" descuento=""/>'.
            
            v-reng = v-reng + 1.
        
        END.
    
    END.
    
END.

/* 3.2 Agregar los datos de los pedimentos de articulos */
FOR EACH v-pedim NO-LOCK BREAK BY v-pedim.PedNo BY v-pedim.Puerto:
    
    v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" unidad="" porcIVA="" '.
    v-deta = v-deta + 'tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
    /*v-deta = v-deta + 'comentario="Pedimento: ' + STRING(v-pedim.PedNo) + ' Fecha: ' + (IF v-pedim.FecPed <> ? THEN STRING(v-pedim.FecPed,"99/99/9999") ELSE " ") + ' Puerto: ' + v-pedim.Puerto + '" descuento=""/>'.*/
    v-deta = v-deta + 'comentario="Pedimento: ' + v-pedim.PedNo  + '" descuento=""/>'.
            
    v-reng = v-reng + 1.
    
END.

/* 3.3 Redondeo */
IF Remision.Redondeo > 0 THEN DO:

    v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" unidad="" porcIVA="" '.
    v-deta = v-deta + 'tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
    v-deta = v-deta + 'comentario="DONATIVO:  ' + STRING(Remision.Redondeo,'z9.99') + '" descuento=""/>'.
            
    v-reng = v-reng + 1.

END.

/* 3.4 Impuestos */
/* ************************************************************************************************************************************* */
/* Cuidar que la tasa este correcta */
v-impsto = ''.
IF Remision.IVA > 0 THEN DO:
    /* ************************************************************************************************************************************* */
    /* Cuidar que la tasa este correcta */
    /*l-TIva = 0.
    FOR EACH w-Impuestos WHERE w-Impuestos.Importe > 0 NO-LOCK:
        l-TIva = l-TIva + w-Impuestos.Importe.
    END.*/
    v-impsto = '<Impuestos totalImpuestosTrasladados="' + TRIM(STRING(Remision.IVA,"zzzzzzzz9.99")) + '" ><Traslados>'.
    /*IF AVAILABLE RfcCfdiEsp AND RfcCfdiEsp.Tipo = 2 THEN DO:*/
        FOR EACH w-Impuestos WHERE w-Impuestos.Importe > 0 NO-LOCK:
            v-impsto = v-impsto + '<Traslado impuesto="IVA" TipoFactor="Tasa" tasa="' + STRING(w-Impuestos.Tasa * 100,"99") + '" importe="' + TRIM(STRING(Remision.IVA,"zzzzzzzz9.99")) + '" Base="' + TRIM(STRING(w-Impuestos.Base - w-Impuestos.Bonifica,"zzzzzzzzz9.99")) + '"/>'.
        END.
    /*END.
    ELSE DO:
        FOR EACH w-Impuestos WHERE w-Impuestos.Importe > 0 NO-LOCK:
            v-impsto = v-impsto + '<Traslado impuesto="IVA" TipoFactor="Tasa" tasa="' + STRING(w-Impuestos.Tasa * 100,"99") + 
                       '" importe="' + TRIM(STRING(w-Impuestos.Importe,"zzzzzzzz9.999999")) + '"/>'.
                       /*'" importe="' + TRIM(STRING(Remision.IVA,"zzzzzzzz9.999999")) + '"/>'.*/
        END.
    END.*/
    v-impsto = v-impsto + '</Traslados></Impuestos>'.
END.
ELSE v-impsto = v-impsto + '<Impuestos/>'.

/* 4. Unir partes de la Factura */ /* Pruebas: AAA010101AAA Prod: AOF870529IU7 */
IF v-cfdirel > "" THEN
   v-comprobante = v-comprobante + v-cfdirel.
v-comprobante = v-comprobante + '<Emisor rfc="AOF870529IU7" nombre="ABASTECEDORA DE OFICINAS">'.
v-comprobante = v-comprobante + '<DomicilioFiscal calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" '.
v-comprobante = v-comprobante + 'municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.
v-comprobante = v-comprobante + v-expEn + ' <RegimenFiscal Regimen="601"/></Emisor>'.
    
v-comprobante = v-comprobante + v-enca + v-deta + v-recep + v-dom + '</Receptor>'.
v-comprobante = v-comprobante + '<Conceptos>' + v-concep + '</Conceptos>'.
v-comprobante = v-comprobante + v-impsto + '</Comprobante>'.

/*
COPY-LOB FROM v-comprobante TO FILE "/usr2/sis3/contado.txt" NO-CONVERT.
    
OUTPUT TO /usr2/sis3/tmp/listoRAdu2.xml.    
    EXPORT v-comprobante.
OUTPUT CLOSE.
*/
l-Intent = 1.
REPEAT:
   l-RutaReq = "/usr2/compartido/request/" + 
               Remision.Id-Remision + STRING(l-Intent,"999") + ".xml".
   IF SEARCH(l-RutaReq) <> ? THEN 
      l-Intent = l-Intent + 1.
   ELSE LEAVE.
END.
OUTPUT TO VALUE(l-RutaReq).
EXPORT v-comprobante.
OUTPUT CLOSE.
  
/* Realizar llamadas y operaciones con el webService de eDoc */
CREATE SERVER hWS.
vlconnect = hWS:CONNECT(servidor) NO-ERROR.

IF vlconnect THEN DO:
    RUN wsEmiteCFDSoap SET hEmiteCFDSoap ON hws.
    /*
    OUTPUT TO '/usr3/tmp/factaut.txt' APPEND.
        EXPORT
            "        "
            "ENTRA AL IntentaCFD"
            SKIP.            
    OUTPUT CLOSE.
    */
    RUN IntentaCFD(INPUT pIdUser).
    vldisconnect = hWS:DISCONNECT().
END.
ELSE
    MESSAGE "No se pudo conectar al Web Service".
    
PAUSE 2 NO-MESSAGE.

PROCEDURE ValidaRFC:
    DEF INPUT PARAMETER v-rfc AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETE rfcOk AS LOGICAL NO-UNDO INITIAL TRUE.
    DEF VAR v-letra  AS INT NO-UNDO.
    DEF VAR v-tot    AS INT  NO-UNDO.
    DEF VAR v-indi   AS INT  NO-UNDO.
    
    v-tot = LENGTH(v-rfc, "CHARACTER").
    
    /* Validar que contenga 12 o 13 posiciones*/
    IF v-tot < 12 OR v-tot > 13 THEN DO:
        rfcOk = FALSE.
        LEAVE.
    END. 
    
    /* Validar que solo sean numeros o letras o & o ;<-por el cambio en el & */
    DO v-indi = 1 TO v-tot:
        v-letra = ASC(SUBSTRING(v-rfc,v-indi,1)).
        
        IF v-letra <> 38 AND NOT (v-letra > 47 AND v-letra < 58) 
                         AND NOT (v-letra > 64 AND v-letra < 91) 
                         AND NOT (v-letra > 96 AND v-letra < 123) THEN
            rfcOk = FALSE.
    END.
    
END.

PROCEDURE Colapsa:
    DEF INPUT-OUTPUT PARAMETER v-colapsa AS CHAR NO-UNDO.
    DEF VAR v-newStr AS CHAR NO-UNDO.
    DEF VAR v-letra  AS CHAR NO-UNDO.
    DEF VAR v-tot    AS INT  NO-UNDO.
    DEF VAR v-indi   AS INT  NO-UNDO.
    DEF VAR espacios AS INT  NO-UNDO INITIAL 0.
    
    v-tot = LENGTH(v-colapsa, "CHARACTER").
    
    DO v-indi = 1 TO v-tot:
        v-letra = SUBSTRING(v-colapsa,v-indi,1).
        
        IF v-letra = CHR(32) THEN
            espacios = espacios + 1.
        ELSE
            espacios = 0.
        
        IF espacios < 2 THEN
            v-newStr = v-newStr + v-letra.
    END.
    
    v-colapsa = v-newStr.
    
END.

PROCEDURE LlenaDomicilio:
    DEF INPUT-OUTPUT PARAMETER v-dom AS CHAR NO-UNDO.
    DEF INPUT PARAMETER v-city AS INT  NO-UNDO.
    DEF INPUT PARAMETER v-lab1 AS CHAR NO-UNDO.
    DEF INPUT PARAMETER v-lab2 AS CHAR NO-UNDO.
    DEF INPUT PARAMETER v-lab3 AS CHAR NO-UNDO.

    FIND Ciudad WHERE Ciudad.Id-Ciudad = v-city NO-LOCK NO-ERROR.
    
    IF AVAILABLE Ciudad THEN DO:
    
        FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
        
        IF AVAILABLE Estado THEN DO:
        
            FIND Pais WHERE Pais.Id-Pais = Estado.Id-Pais NO-LOCK NO-ERROR.
            
            IF AVAILABLE Pais THEN DO:
            
                v-dom = v-dom + v-lab1 + '="' + TRIM(Ciudad.Nombre) + '" '.
                v-dom = v-dom + v-lab2 + '="' + TRIM(Estado.Nombre) + '" '.
                v-dom = v-dom + v-lab3 + '="' + TRIM(Pais.Nombre) + '" '.

            END.
        
        END.
    
    END.
    
END.


PROCEDURE IntentaCFD.
   DEF INPUT PARAMETER pIdUser   AS CHARACTER NO-UNDO.   
   FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
   IF AVAILABLE Usuario THEN ASSIGN g-Origen = Usuario.Id-Ubicacion.
   l-Intent = 1.
   DO WHILE l-Intent <= l-MaxInt:
      RUN EmiteCFD IN hEmiteCFDSoap(INPUT v-comprobante, OUTPUT v-respuesta).
      RUN RecuperaUUID IN hEmiteCFDSoap(INPUT Remision.Id-Remision, OUTPUT v-UUID).
      
      /*MESSAGE "Folio Generado: " + v-respuesta.
      PAUSE 2 NO-MESSAGE.*/
      
      /*
      OUTPUT TO '/usr3/tmp/factaut.txt' APPEND.
          EXPORT
              "            "
              "INTENTO"
              STRING(l-Intent)
              "FOLIO GENERADO"
              STRING(v-Respuesta)
              "COMPROBANTE"
              SKIP.            
      OUTPUT CLOSE.
      */
	  
      v-tam = LENGTH(v-respuesta, "CHARACTER").
      v-ind = 1.
      DO v-num = 1 TO 4:
	  
    	  v-pos = INDEX(v-respuesta,"_",v-ind).
    	  v-siz = v-pos - v-ind.
    	      
    	  IF v-pos = 0 THEN
    	      v-siz = v-tam - v-ind + 1.
    	       
    	  v-valores[v-num] = SUBSTRING(v-respuesta,v-ind,v-siz).
    	      
    	  v-ind = v-pos + 1.
	  
      END.
	  
      IF v-valores[1] <> "ERROR" AND v-valores[1] BEGINS 'A' THEN DO: 

    	  v-rfc = v-valores[2].
    	  v-serie = v-valores[3].
    	  v-folio = v-valores[4].
    	      
    	  DO TRANSACTION:
    	      FIND buffRemis WHERE RECID(buffRemis) = RECID(Remision) EXCLUSIVE-LOCK NO-ERROR.
    	      ASSIGN
    	          buffRemis.Folioe = v-valores[1] + ',' + v-serie + ',' + v-folio
    	          buffRemis.Id-Fiscal = TRIM(v-serie) + TRIM(v-folio)
                  buffRemis.FeFormaPago = l-FormaDePago
                  buffRemis.FeDigitosCuenta = l-NumCtaPago
                  buffRemis.FeMetodoPago = l-MetodoDePago
                  buffRemis.Version = "4.0"
    	          buffRemis.UUID = SUBSTRING(STRING(v-UUID),1,INDEX(STRING(v-UUID),",") - 1).
    	  END.
          RELEASE buffRemis.
    	  LEAVE.
      END.
      ELSE DO:
          IF NOT v-respuesta MATCHES "*interbloqueo*" AND
             NOT v-respuesta MATCHES "*conexion*" AND
             NOT v-respuesta MATCHES "*cerrada*por*el*servidor*" AND
             NOT v-respuesta MATCHES "*error*de*seguridad*" AND
             NOT v-respuesta MATCHES "*fecha*y*hora*" AND
             NOT v-respuesta MATCHES "*anulada*" AND
             NOT v-respuesta MATCHES "*folio*ya*asignado*" THEN 
             ASSIGN l-Intent = l-MaxInt.

          IF l-Intent < l-MaxInt THEN l-Intent = l-Intent + 1.
          ELSE DO:
              IF v-Respuesta MATCHES "*Emisor*obligaciones*" AND NOT PROGRAM-NAME(2) MATCHES "*sisd*" AND NOT PROGRAM-NAME(3) MATCHES "*sisd*" THEN DO:
                  RUN /usr2/sea/appserver/fuentes/ImprimeRemisionTMP.p(INPUT Remision.Id-Remision). /* Imprime Remision temporal */
                  CASE g-Origen:
                      WHEN "02B" THEN DO:
                          UNIX SILENT VALUE("lpr7 /usr2/sea/reportes/" + Remision.Id-Remision + ".pdf").
                          IF Remision.Id-Entrega = 11 THEN DO:
                              UNIX SILENT VALUE("lpr7 /usr2/sea/reportes/" + Remision.Id-Remision + ".pdf").
                          END.
                      END.
                      WHEN "FUG" THEN DO:
                          UNIX SILENT VALUE("lpr7 /usr2/sea/reportes/" + Remision.Id-Remision + ".pdf").
                          IF Remision.Id-Entrega = 11 THEN DO:
                              UNIX SILENT VALUE("lpr7 /usr2/sea/reportes/" + Remision.Id-Remision + ".pdf").
                          END.
                      END.
                      WHEN "11" THEN DO:
                          UNIX SILENT VALUE("lpsatesf /usr2/sea/reportes/" + Remision.Id-Remision + ".pdf").
                      END.
                      WHEN "12" THEN DO:
                          UNIX SILENT VALUE("lpchfact /usr2/sea/reportes/" + Remision.Id-Remision + ".pdf").
                      END.
                  END CASE.
                  LEAVE.
              END.
              ELSE DO:
        	      OUTPUT TO VALUE("/usr2/compartido/request/" + Remision.Id-Remision + ".err").
        	      EXPORT v-respuesta.
        	      OUTPUT CLOSE.
        	      MESSAGE "Ocurrio un error al generar la factura electronica".
                  LEAVE.
              END.
          END.
      END.
      PAUSE 2 NO-MESSAGE.
   END.
END.
