/*
    Empresa  :  ADOSA
    Programa :  vtac2060.p
    Funcion  :  Genera la factura electronica de Credito
    Autor    :  Elias
    Fecha    :  31 Mayo 2010
Modificaci�n : RNPC - 2019-03-11 - Se agrega ajuste para tipo de moneda.
*/

//{sia00000.var}
DEF INPUT PARAMETER numFactIni LIKE Factura.Id-Factura NO-UNDO.
DEF INPUT PARAMETER numFactFin LIKE Factura.Id-Factura NO-UNDO.  
DEF INPUT PARAMETER tasa       AS DECIMAL NO-UNDO INITIAL 0.
DEF INPUT PARAMETER pIdUser   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER v-Resp        AS CHARACTER NO-UNDO.

/* Para manejo de conexion con web service */
DEFINE VARIABLE hWS               AS HANDLE.
DEFINE VARIABLE hEmiteCFDSoap     AS HANDLE.
DEFINE VARIABLE vlconnect         AS LOGICAL NO-UNDO.
DEFINE VARIABLE vldisconnect      AS LOGICAL NO-UNDO.
DEFINE VARIABLE servidor          AS CHARACTER NO-UNDO.

/* Variables para funcion del web service */
DEFINE VARIABLE v-respuesta   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-comprobante AS LONGCHAR NO-UNDO.
DEFINE VARIABLE v-UUID AS LONGCHAR NO-UNDO.

DEFINE VARIABLE v-listaMes AS CHARACTER NO-UNDO 
    INITIAL 'ENE,FEB,MAR,ABR,MAY,JUN,JUL,AGO,SEP,OCT,NOV,DIC'.

/* Variables para guardar cada una de las etiquetas del xml */
DEFINE VARIABLE v-emisor AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-domFis AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-expEn  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-recep  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-dom    AS CHARACTER NO-UNDO.   
DEFINE VARIABLE v-enca   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-deta   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE v-concep AS LONGCHAR NO-UNDO.
DEFINE VARIABLE v-impsto AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-puerto AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-pedNo  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-addnd  AS CHARACTER NO-UNDO.

/* Parametros para generar el pdf */
DEFINE VARIABLE v-rfc     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-serie   AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-folio   AS CHARACTER     NO-UNDO.

DEFINE VARIABLE l-Plazo   AS INTEGER  NO-UNDO.
DEFINE VARIABLE v-fecVen  AS DATE     NO-UNDO.
DEFINE VARIABLE v-letras  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-digi    AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-exped   AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-descrip AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-req     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-razon   AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-RefEmb  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-razon1  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-CalleNo AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-rfcCte  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-reng    AS INTEGER      NO-UNDO.
DEFINE VARIABLE v-pedido  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE i         AS INTEGER      NO-UNDO.
DEFINE VARIABLE msuma     AS INTEGER      NO-UNDO.
DEFINE VARIABLE mfa_veri  AS INTEGER      NO-UNDO.
DEFINE VARIABLE l-k       AS INTEGER      NO-UNDO.
DEFINE VARIABLE v-fecAdu  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE l-IvaZona AS DECIMAL  NO-UNDO.
DEFINE VARIABLE v-okRFC   AS LOGICAL  NO-UNDO.
DEFINE VARIABLE l-MetodoDePago AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-NumCtaPago AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-LeyendaMetodo AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ClavePS LIKE Articulo.Id-ClavePS NO-UNDO.
DEFINE VARIABLE v-UsoCFDI LIKE Factura.Id-UsoCFDI NO-UNDO.
DEFINE VARIABLE v-RFiscal LIKE Factura.Id-RFiscal NO-UNDO.
DEFINE VARIABLE v-CPFiscal LIKE Factura.CP NO-UNDO.
DEFINE VARIABLE v-cfdirel AS LONGCHAR NO-UNDO.
DEFINE VARIABLE l-DetIVA  LIKE DetRemis.IVA NO-UNDO.
DEFINE VARIABLE v-correo  AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-RutaReq AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-LExp LIKE Remision.CP NO-UNDO.
DEFINE VARIABLE v-CCE AS CHARACTER NO-UNDO INITIAL "".

DEFINE TEMP-TABLE v-pedim
    FIELD PedNo  LIKE DetPedim.PedNo
    FIELD Puerto LIKE DetPedim.Puerto
    FIELD FecPed LIKE DetPedim.FecPed
    INDEX ip-1 AS PRIMARY pedNo Puerto.
    
DEFINE TEMP-TABLE w-Impuestos
    FIELD Tasa AS DECIMAL
    FIELD Base AS DECIMAL
    FIELD Importe LIKE DetRemis.IVA
    INDEX Idx-Def Tasa.

DEFINE TEMP-TABLE tt-DetFactura LIKE DetFactura
    FIELD Bonifica LIKE DetFactura.Importe.
DEFINE VARIABLE l-TmpDesc LIKE Factura.Descuento NO-UNDO.
DEFINE VARIABLE l-DifDesc LIKE Factura.Descuento NO-UNDO.
DEFINE VARIABLE l-TIva LIKE DetFactura.Iva NO-UNDO.    

/* Variables para extraer los valores de la respuesta del WebService */
DEFINE VARIABLE v-valores AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-tam     AS INTEGER NO-UNDO.
DEFINE VARIABLE v-ind     AS INTEGER NO-UNDO.
DEFINE VARIABLE v-pos     AS INTEGER NO-UNDO.
DEFINE VARIABLE v-siz     AS INTEGER NO-UNDO.
DEFINE VARIABLE v-num     AS INTEGER NO-UNDO.
DEFINE VARIABLE l-Intent  AS INTEGER NO-UNDO.
DEFINE VARIABLE l-MaxInt  AS INTEGER NO-UNDO INITIAL 3.

DEFINE VARIABLE l-NumTienda LIKE CliEmb.Id-CliEmb NO-UNDO.
DEFINE VARIABLE l-CodCliente AS CHARACTER NO-UNDO.
DEF VAR g-Origen AS CHARACTER NO-UNDO.  /* Define the variable */

DEFINE BUFFER buffFactura FOR Factura.
DEFINE BUFFER b-Factura FOR Factura.

FOR EACH buffFactura WHERE buffFactura.Id-Factura >= numFactIni
    AND buffFactura.Id-Factura <= numFactFin NO-LOCK:
    
    /* Traer al buffer de lectura los registros necesarios que pertenecen a la factura */
    FIND Factura WHERE RECID(Factura) = RECID(buffFactura) NO-LOCK NO-ERROR.
    FIND Transporte OF Factura NO-LOCK NO-ERROR.
    FIND Cliente OF Factura NO-LOCK NO-ERROR.
    FIND Entrega OF Factura NO-LOCK NO-ERROR.
    FIND CondVta OF Factura NO-LOCK NO-ERROR.
    FIND Vendedor OF Factura NO-LOCK NO-ERROR.
    FIND Zona OF Cliente NO-LOCK NO-ERROR.
    FIND FIRST SysGeneral NO-LOCK NO-ERROR.

    /*
    OUTPUT TO '/usr3/tmp/factaut.txt' APPEND.
        EXPORT
            "        "
            AVAILABLE Factura
            AVAILABLE Transporte
            AVAILABLE Cliente
            AVAILABLE Entrega
            AVAILABLE CondVta
            AVAILABLE Vendedor
            AVAILABLE Zona
            AVAILABLE SysGeneral
            SKIP.            
    OUTPUT CLOSE.
    */
    
    IF AVAILABLE Zona THEN 
        ASSIGN l-IvaZona = Zona.Porc-IVA.
    ELSE 
        ASSIGN l-IvaZona = SysGeneral.porc-iva.
        
    /* No permitir generar el folio electronico 2 veces */
    /*IF Factura.Id-Cliente = 48611 THEN NEXT.*/
    IF Factura.Folioe <> '' THEN NEXT.
    v-cfdirel = ''.
    IF Factura.SustUUID > "" THEN DO:
        FIND b-Factura WHERE b-Factura.Id-Factura = Factura.SustIdFactura
            NO-LOCK NO-ERROR.
        IF AVAILABLE b-Factura AND TRIM(b-Factura.RFC) = TRIM(Factura.RFC) THEN DO:
            v-cfdirel = '<CfdiRelacionados TipoRelacion = "04">'.
            v-cfdirel = v-cfdirel + '<CfdiRelacionado UUID="' + Factura.SustUUID + '"/>'.
            v-cfdirel = v-cfdirel + '</CfdiRelacionados>'.
        END.
    END.
    
    IF AVAILABLE Vendedor THEN
        FIND Empleado WHERE Empleado.iniciales = Vendedor.iniciales NO-LOCK NO-ERROR.
    
    /* 1. Datos iniciales de comprobante */
    v-comprobante = '<?xml version="1.0" encoding="utf-8"?><Comprobante xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" version="4.0" '.
    
    v-digi = SUBSTRING(Factura.Id-Factura,1,1).
    
    /*servidor = "-WSDL http://192.0.1.10:8087/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".*/
    servidor = "-WSDL http://192.0.1.7/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
    
    CASE v-digi:
        WHEN '1' THEN DO:
                IF Factura.Id-Ubic BEGINS '12' THEN DO:
                    v-comprobante = v-comprobante + ' serie="1" '.
                    servidor = "-WSDL http://192.0.5.22/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
                END.
                ELSE
                    v-comprobante = v-comprobante + ' serie="1" '.
            
                v-comprobante = v-comprobante + ' LugarExpedicion="88300" '.
                v-LExp = "88300".
                v-exped = "Expedida en Miguel Aleman, Tamps.".
                v-expEn = '<ExpedidoEn calle="Segunda Sur" noExterior="102" noInterior="" colonia="Centro" localidad="" referencia="" municipio="Miguel Aleman" estado="Tamaulipas" pais="Mexico" codigoPostal="88300"/>'.
            END.
        WHEN '2' THEN DO:
                v-comprobante = v-comprobante + ' serie="2" '.
                v-comprobante = v-comprobante + ' LugarExpedicion="64000" '.
                v-LExp = "64000".
                v-exped = "Expedida en Monterrey, N.L.".
                v-expEn = '<ExpedidoEn calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.
            END.
        WHEN '3' THEN DO:
                v-comprobante = v-comprobante + ' serie="3" '.
                v-comprobante = v-comprobante + ' LugarExpedicion="64000" '.
                v-LExp = "64000".
                v-exped = "Expedida en Monterrey, N.L.".
                v-expEn = '<ExpedidoEn calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.
            END.
        WHEN '4' THEN DO:
                v-comprobante = v-comprobante + ' serie="4" '.
                v-comprobante = v-comprobante + ' LugarExpedicion="25000" '.
                v-LExp = "25000".
                v-exped = "Expedida en Saltillo, Coah.".
                v-expEn = '<ExpedidoEn calle="Hidalgo" noExterior="1126" noInterior="" colonia="Centro" localidad="" referencia="" municipio="Saltillo" estado="Coahuila" pais="Mexico" codigoPostal="25000"/>'.
                IF Factura.FecReg < 10/31/2016 THEN
                    servidor = "-WSDL http://192.0.2.22/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
            END.
        WHEN '5' THEN DO: 
                v-comprobante = v-comprobante + ' serie="5" '.
                v-comprobante = v-comprobante + ' LugarExpedicion="31000" '.
                v-LExp = "31000".
                v-exped = "Expedida en Chihuahua, Chih.". 
                v-expEn = '<ExpedidoEn calle="Julian Carrillo" noExterior="806" noInterior="" colonia="Centro" localidad="" referencia="" municipio="Chihuahua" estado="Chihuahua" pais="Mexico" codigoPostal="31000"/>'.
                servidor = "-WSDL http://192.0.5.22/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
            END.
        WHEN '6' THEN DO:
                v-comprobante = v-comprobante + ' serie="6" '.
                v-comprobante = v-comprobante + ' LugarExpedicion="67176" '.
                v-LExp = "67176".
                v-exped = "Expedida en Guadalupe, N.L.".
                v-expEn = '<ExpedidoEn calle="Av. Pablo Livas" noExterior="2500" noInterior="" colonia="Local 13 Plaza Mirador Mirador de la Silla" localidad="" referencia="" municipio="Guadalupe" estado="Nuevo Leon" pais="Mexico" codigoPostal="67176"/>'.
            END.            
        WHEN '7' THEN DO:
                v-comprobante = v-comprobante + ' serie="7" '.
                v-comprobante = v-comprobante + ' LugarExpedicion="64540" '.
                v-LExp = "64540".
                v-exped = "Expedida en Monterrey, N.L.".
                v-expEn = '<ExpedidoEn calle="Av. Ruiz Cortines" noExterior="3280" noInterior="1" colonia="Parque Industrial Regiomontano" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64540"/>'.
            END.   
        WHEN '8' THEN DO:
                v-comprobante = v-comprobante + ' serie="8" '.
                v-comprobante = v-comprobante + ' LugarExpedicion="64348" '.
                v-LExp = "64348".
                v-exped = "Expedida en Monterrey, N.L.".
                v-expEn = '<ExpedidoEn calle="Ruiz Cortines" noExterior="6410" noInterior="" colonia="Fracc. Portal de Cumbres" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64348"/>'.
            END.              
        WHEN '9' THEN DO:
                v-comprobante = v-comprobante + ' serie="9" '.
                v-comprobante = v-comprobante + ' LugarExpedicion="66468" '.
                v-LExp = "66468".
                v-exped = "Expedida en San Nicolas de los Garza, N.L.".
                v-expEn = '<ExpedidoEn calle="Diego Diaz de Berlanga" noExterior="469" noInterior="" colonia="Jardines de Santo Domingo" localidad="" referencia="" municipio="San Nicolas de los Garza" estado="Nuevo Leon" pais="Mexico" codigoPostal="66468"/>'.
            END.              
        WHEN 'N' THEN DO:
                v-comprobante = v-comprobante + ' serie="N" '.
                v-comprobante = v-comprobante + ' LugarExpedicion="66059" '.
                v-LExp = "66059".
                v-exped = "Expedida en Escobedo, N.L.".
                v-expEn = '<ExpedidoEn calle="Concordia Ote." noExterior="100" noInterior="L28E" colonia="Cerradas de Anahuac" localidad="" referencia="" municipio="Escobedo" estado="Nuevo Leon" pais="Mexico" codigoPostal="66059"/>'.
            END.              
        OTHERWISE DO:
            v-comprobante = v-comprobante + ' serie="0" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="64000" '.
            v-LExp = "64000".
            v-exped = "Expedida en Monterrey, N.L.".
            v-expEn = '<ExpedidoEn calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.
        END.
    END CASE.

    /* 11. Datos de Facturacion del Cliente o Receptor */
    IF LENGTH(Factura.RFC) = 12 AND Factura.NomEmpresa > "" THEN
        v-razon = TRIM(Factura.NomEmpresa).
    ELSE v-razon = TRIM(Factura.RazonSocial).
    v-razon = REPLACE(v-razon,CHR(38),'&#38;').
    v-razon = REPLACE(v-razon,'"', '&quot;').
    v-razon = TRIM(REPLACE(v-razon,'|',' ')).
    v-razon = REPLACE(v-razon,"'",'&apos;').
    v-razon = REPLACE(v-razon,'<','&lt;').
    v-razon = REPLACE(v-razon,'>','&gt;').
    v-razon = REPLACE(v-razon,CHR(165),'&#209;').
    v-razon = REPLACE(v-razon,CHR(154),'&#209;').

    v-RefEmb = TRIM(Factura.Referencia).
    v-RefEmb = REPLACE(v-RefEmb,CHR(38),'&#38;').
    v-RefEmb = REPLACE(v-RefEmb,'"', '&quot;').
    v-RefEmb = TRIM(REPLACE(v-RefEmb,'|',' ')).
    v-RefEmb = REPLACE(v-RefEmb,"'",'&apos;').
    v-RefEmb = REPLACE(v-RefEmb,'<','&lt;').
    v-RefEmb = REPLACE(v-RefEmb,'>','&gt;').
    v-RefEmb = REPLACE(v-RefEmb,CHR(165),'&#209;').
    v-RefEmb = REPLACE(v-RefEmb,CHR(154),'&#209;').
    /*
        CASE Factura.Id-Cliente:
            WHEN 31131 THEN
                v-razon = 'VIDRIO PLANO DE MEXICO LAN SA DE CV'.
            WHEN 172 THEN
                v-razon = 'Clínica Vitro A.C.'.
            WHEN 3332 THEN
                v-razon = 'Compañía Vidriera S.A. de C.V.'.
            WHEN 29347 THEN
                v-razon = 'Vitrocar S.A. de C.V.'.
            WHEN 34290 THEN
                v-razon = 'PRODUCTOS DE VALOR AGREGADO EN CRISTAL S.A. DE C.V.'.
            WHEN 34392 THEN
                v-razon = 'Vitro, S.A.B. de C.V.'.
            WHEN 34416 THEN
                v-razon = 'COMERCIALIZADORA ALCALI SA DE CV'.
        END.
    */
    
    RUN Colapsa(INPUT-OUTPUT v-razon).
   
    v-rfcCte = CAPS(TRIM(REPLACE(Factura.rfc,' ',''))).        
    
    RUN ValidaRFC(INPUT v-rfcCte, OUTPUT v-okRFC).
    IF v-okRFC = FALSE THEN DO:
        MESSAGE "El RFC " + v-rfcCte + " es invalido, favor de modificarlo.".
        PAUSE 2 NO-MESSAGE.
        NEXT.
    END.
    
    FIND FIRST RfcCfdiEsp WHERE RfcCfdiEsp.RFC = v-rfcCte NO-LOCK NO-ERROR.
    v-rfcCte = REPLACE(v-rfcCte,'&','&amp;').
    
    /* Corregir caracter especial en la direccion */
    v-CalleNo = REPLACE(Factura.CalleNo,'&','&amp;').
    v-CalleNo = REPLACE(v-CalleNo,'"', '&quot;').
    v-CalleNo = TRIM(REPLACE(v-CalleNo,'|',' ')).
    v-CalleNo = REPLACE(v-CalleNo,"'",'&apos;').
    v-CalleNo = REPLACE(v-CalleNo,'<','&lt;').
    v-CalleNo = REPLACE(v-CalleNo,'>','&gt;').

    IF Factura.BuzonFiscal <> "" THEN 
        v-correo = Factura.BuzonFiscal.
    ELSE v-correo = "".

    /*2-sep-20 incluir correo del contacto para envio de factura (Ing.Sergio)*/
    IF Factura.e-Mail <> "" AND Factura.BuzonFiscal <> Factura.e-Mail THEN DO:
        v-correo = v-correo + MINIMUM(v-correo,";") + Factura.e-Mail.
    END.
    v-correo = REPLACE(v-correo,'&','&amp;').
 
    v-CPFiscal = IF Factura.CP <> '' THEN TRIM(Factura.CP) 
    ELSE TRIM(Cliente.CP).
    IF Factura.Id-UsoCFDI = "" OR Factura.RFC = "XAXX010101000" THEN v-UsoCFDI = "S01".
    ELSE v-UsoCFDI = CAPS(Factura.Id-UsoCFDI).

    IF Factura.RFC = "XAXX010101000" THEN ASSIGN v-RFiscal = "616"
            v-CPFiscal = v-LExp.
    ELSE v-RFiscal = TRIM(Factura.Id-RFiscal).

    IF Factura.RFC = "XEXX010101000" THEN
        ASSIGN v-UsoCFDI = "S01"
            v-RFiscal = "616"
            v-CPFiscal = v-LExp.

    IF v-UsoCFDI = "P01" THEN v-UsoCFDI = "G03".
    
    v-recep = '<Receptor rfc="' + v-rfcCte + '" RegimenFiscalReceptor="' + v-RFiscal + '" DomicilioFiscalReceptor="' + v-CPFiscal + '" UsoCFDI="' + TRIM(v-usocfdi) + '" nombre="' + v-razon.
    
    IF Factura.Id-Cliente = 42239 THEN 
        v-recep = v-recep + '" ResidenciaFiscal="USA" NumRegIdTrib="900781116" >'.
    ELSE v-recep = v-recep + '" >'.
    
    /*
    FIND Asociado WHERE Asociado.Id-Cliente = Factura.Id-Cliente
                    AND Asociado.Id-Asociado = 35383 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Asociado THEN 
       FIND Asociado WHERE Asociado.Id-Cliente = Factura.Id-Cliente
                       AND Asociado.Id-Asociado = 1435 NO-LOCK NO-ERROR.
    IF Factura.Id-Cliente = 35383 OR Factura.Id-Cliente = 1435 OR AVAILABLE Asociado OR
       (AVAILABLE RfcCfdiEsp AND RfcCfdiEsp.Tipo = 2) THEN*/
    v-enca = '<ImprimeEncabezado DecimalesXml="2" '.
    /*ELSE v-enca = '<ImprimeEncabezado '.*/
    
    v-enca = v-enca + ' CodigoBarra="' + Factura.Id-Factura + '" RazonSocial="' + v-razon + '" RFC="R.F.C. ' + v-rfcCte + '" '.
    IF v-correo <> "" THEN v-enca = v-enca + 'correo="' + v-correo + '" '.
    
    v-dom = '<Domicilio calle="' + TRIM(v-CalleNo) + '" noExterior="' + (IF AVAILABLE Cliente AND Cliente.NumExt <> '' THEN Cliente.NumExt ELSE '') + '" noInterior="" colonia="' + TRIM(Factura.Colonia) + '" '.
    v-enca = v-enca + 'CalleNo="' + TRIM(v-CalleNo) + '" Colonia="COL. ' + TRIM(Factura.Colonia) + '" '.
    
    /* Parche para agregar datos de direcciones de vitro con acentos y ñ (esperando solucion de Progress) */
    IF CAN-DO('2400,3332,970,3381,3383,31131,330,3201,1773,172,4064,3537,33717,29347,34290,34392,34416',STRING(Factura.Id-Cliente)) THEN DO:
    
        RUN ClienteVitro(INPUT-OUTPUT v-dom, INPUT Factura.Id-Cliente).
    
    END.
    
    /* Buscar si ls direccion del cliente esta dentro de alguna delegacion */
    IF Factura.Id-Cliente <> 3 AND AVAILABLE Cliente AND LENGTH(TRIM(Cliente.Delegacion)) <> 0 THEN DO:
        
        v-dom = v-dom + 'localidad="' + Cliente.Delegacion + '" '.
        v-enca = v-enca + 'Delegacion="' + Cliente.Delegacion + '" '.
        
    END.
    ELSE DO:
    
        v-dom = v-dom + 'localidad="" '.
        v-enca = v-enca + 'Delegacion="" '. 
    
    END.
    
    RUN LlenaDomicilio(INPUT-OUTPUT v-enca, INPUT Factura.Id-Ciudad, INPUT 'Ciudad', INPUT 'Estado', INPUT 'Pais').

    IF Factura.Id-Cliente = 42239 THEN
        v-dom = v-dom + 'municipio="LAREDO" pais="USA" estado="TX" '.
    ELSE RUN LlenaDomicilio(INPUT-OUTPUT v-dom, INPUT Factura.Id-Ciudad, INPUT 'municipio', INPUT 'estado', INPUT 'pais').
    
    v-dom = v-dom + 'codigoPostal="' + Factura.cp + '" />'.
    v-enca = v-enca + 'CP="' + Factura.cp + '" '.
    
    RUN Colapsa(INPUT-OUTPUT v-dom).
    
    /* 1.1 Datos de Embarque */
    IF LENGTH(TRIM(Factura.RazonSocial1)) > 0 OR
        LENGTH(TRIM(Factura.CalleNo1)) > 0 OR 
        LENGTH(TRIM(Factura.Colonia1)) > 0 OR 
        LENGTH(TRIM(Factura.Ciudad1)) > 0 OR 
        LENGTH(TRIM(Factura.Estado1)) > 0 OR
        LENGTH(TRIM(Factura.Referencia)) > 0 THEN DO:
        
        v-razon1 = REPLACE(Factura.RazonSocial1,'&','&amp;').
        v-razon1 = REPLACE(v-razon1,'"', '&quot;').
        v-razon1 = TRIM(REPLACE(v-razon1,'|',' ')).
        v-razon1 = REPLACE(v-razon1,"'",'&apos;').
        v-razon1 = REPLACE(v-razon1,'<','&lt;').
        v-razon1 = REPLACE(v-razon1,'>','&gt;').
        
        v-enca = v-enca + 'EmbAtencion="ATTN:' + Factura.Attn1 + '" EmbRazonSocial="' + REPLACE(v-razon1,'"','') + '" EmbCalleNo="' + Factura.CalleNo1 + '" '.
        v-enca = v-enca + 'EmbColonia="' + Factura.Colonia1 + '" '.

        IF TRIM(Factura.Delegacion1) <> "" THEN
            v-enca = v-enca + 'EmbDelegacion="' + Factura.Delegacion1 + '" '.
        ELSE
            v-enca = v-enca + 'EmbDelegacion="" '.
        
        v-enca = v-enca + 'EmbCiudad="' + Factura.Ciudad1 + '" EmbEstado="' + Factura.Estado1 + '" EmbPais="" '.
        v-enca = v-enca + 'EmbCP="' + Factura.cp1 + '" EmbTel="' + Factura.Tel1 + '" EmbFax="' + v-RefEmb + '" '.        
        
    END.
    ELSE
        v-enca = v-enca + 'EmbAtencion="" EmbRazonSocial="" EmbCalleNo="" EmbColonia="" EmbDelegacion="" EmbCiudad="" EmbEstado="" EmbPais="" EmbCP="" EmbTel="" EmbFax="" '.
    
    IF AVAILABLE Transporte THEN
        v-enca = v-enca + 'Transporte="E' + (IF AVAILABLE Cliente THEN STRING(Factura.Id-RutaEmb,"99") ELSE '') + ' ' + 
            STRING(Transporte.Id-Tran) + ' ' + Transporte.Nombre + '" '.
    ELSE
        v-enca = v-enca + 'Transporte="E' + (IF AVAILABLE Cliente THEN STRING(Factura.Id-RutaEmb,"99") ELSE '') + '" '.
        
    
    /*Cambio Junio 2011: la forma de pago debe ser PAGO EN UNA SOLA EXHIBICION*/
    /*Cambio Nov 2017: la forma de pago debe ser 99 En Credito*/
    v-comprobante = v-comprobante + 'formaDePago="99" '.
    
    /*
    13-Ago-21 clientes corporativos se les muestra su plazo real, sin tope de 30
    23-Mar-22 todos los clientes plazo real, excepto artes graficas
    */
    IF (AVAILABLE Cliente AND Cliente.Id-Ramo <> "1") THEN l-Plazo = Factura.Plazo.
    ELSE l-Plazo = 30.

    /* 2.1 Condiciones de Pago */
    /*IF CAN-DO("1435,3384,3395,3396,3397,3402,31825,31993",STRING(Factura.Id-Cliente)) THEN DO:*/
    IF Factura.Id-Cliente = 32330 THEN DO: /*Cliente de Saltillo*/
    
        v-comprobante = v-comprobante + 'condicionesDePago="CREDITO' + TRIM(STRING(Factura.Plazo)) + '" '.
        v-enca = v-enca + 'Condicion="CREDITO' + TRIM(STRING(Factura.Plazo)) + '" '.
    
    END.
    ELSE DO:
        IF Factura.Id-Cond = 1 OR Factura.Id-Cond = 4 OR Factura.Id-Cond = 5 THEN DO:
        
            IF AVAILABLE CondVta THEN DO:
            
                v-comprobante = v-comprobante + 'condicionesDePago="CREDITO" '.
                v-enca = v-enca + 'Condicion="VENC. ' + STRING(DAY(CondVta.FecVen),">9") + '/' + STRING(MONTH(CondVta.FecVen),">9") + SUBSTRING(STRING(YEAR(CondVta.FecVen),">>>9"),3,2) + '" '.
            
            END.
        
        END.
        ELSE DO:
            
            CASE Factura.Id-Cond:
                WHEN 0 THEN
                    IF AVAILABLE Cliente THEN DO:
                
                        IF Cliente.Id-Calidad = 42 THEN DO:
                    
                            v-comprobante = v-comprobante + 'condicionesDePago="CONTADO" '.
                            v-enca = v-enca + 'Condicion="EFECTIVO" '.
                    
                        END.
                        ELSE DO:
                            
                            v-comprobante = v-comprobante + 'condicionesDePago="CONTADO" '.
                            v-enca = v-enca + 'Condicion="CONTADO" '.
                            
                        END.
                
                    END.
                    ELSE DO:
                    
                        v-comprobante = v-comprobante + 'condicionesDePago="CONTADO" '.
                        v-enca = v-enca + 'Condicion="CONTADO" '.
                    
                    END.
                WHEN 2 THEN DO:
                    
                        v-comprobante = v-comprobante + 'condicionesDePago="CREDITO" '.
                        v-enca = v-enca + 'Condicion=" ' + STRING(l-Plazo,">9") + ' DIAS F.F. " '.
                    
                    END.
                WHEN 3 THEN DO:    
                    
                        v-comprobante = v-comprobante + 'condicionesDePago="CREDITO" '.
                        v-enca = v-enca + 'Condicion="C.O.D." '.    
                    
                    END.
            END CASE.
            
        END.
    END.
    
    /* 2.2 Datos del plazo */
    IF Factura.Id-Cond <> 0 THEN DO:
        
        /*
                IF Factura.Id-Cond = 2 AND Factura.Plazo > 30 THEN
                    ASSIGN v-fecVen = Factura.FecReg + 30.
                ELSE
                    ASSIGN v-fecVen = Factura.FecReg + Factura.Plazo.
        */
        ASSIGN v-fecVen = Factura.FecReg + l-Plazo.
            
        v-enca = v-enca + 'Vencimiento="' + STRING(DAY(v-fecVen),"99") + '-' + ENTRY(MONTH(v-fecVen),v-listaMes) + '-' + STRING(YEAR(v-fecVen),">>>9") + '" '.        
        
    END.
    ELSE 
        v-enca = v-enca + 'Vencimiento="" '.
    
    
    /*
    ASSIGN msuma = 0.
    DO l-k = 1 TO 5:
    
        ASSIGN msuma = msuma + (INTEGER(SUBSTRING(STRING(Factura.Id-Cliente,"99999"),l-k,1)) * (7 - l-k)).
    
    END.
    ASSIGN mfa_veri = 7 - (msuma MODULO 7).
    */
    RUN /usr2/adosa/procs/vtad1000.p(INPUT Factura.Id-Cliente, OUTPUT mfa_veri).

    v-req = REPLACE(Factura.Requisicion,'&','&amp;').
    v-req = REPLACE(v-req,'"', '&quot;').
    v-req = TRIM(REPLACE(v-req,'|',' ')).
    v-req = REPLACE(v-req,"'",'&apos;').
    v-req = REPLACE(v-req,'<','&lt;').
    v-req = REPLACE(v-req,'>','&gt;').
        
    v-enca = v-enca + 'Autorizacion="' + Factura.Id-Captura + ' ' + (IF Factura.CveAut > 0 THEN 'AUT-' + STRING(Factura.Autorizado-Por,"x(7)") ELSE '') + '" '.
    v-enca = v-enca + 'Pedido="' + SUBSTRING(Factura.Pedidos,1,25) + '" '.
    v-enca = v-enca + 'Requisicion="' + SUBSTRING(v-req,1,20) + '" '.
    v-enca = v-enca + 'cliente="' + STRING(Factura.Id-Cliente) + "-" + STRING(mfa_veri,'99') + '" '.
    v-enca = v-enca + 'Fecha="' + STRING(DAY(Factura.FecReg),">9") + '-' + ENTRY(MONTH(Factura.FecReg),v-listaMes) + '-' + STRING(YEAR(Factura.FecReg),">>>9") + '" '.
    v-enca = v-enca + 'Id_Factura="' + (IF v-digi > '0' THEN STRING(v-digi) + '-' + SUBSTRING(Factura.Id-Factura,2,6) ELSE Factura.Id-Factura) + '" '.
    v-enca = v-enca + 'Propietario="' + (IF LENGTH(TRIM(Factura.Propietario)) > 0 AND Cliente.Tipo = 1 THEN STRING(Factura.Propietario, "x(50)") ELSE '') + '" '.
    v-enca = v-enca + 'Tarimas="' + STRING(Factura.Tarimas,"zz9") + '" '.
    v-enca = v-enca + 'Bultos="' + STRING(Factura.Bultos,"zz9") + '" '.
    v-enca = v-enca + 'Entrega="' + STRING(IF AVAILABLE Entrega THEN Entrega.Descr ELSE '',"x(20)") + '" '.
    v-enca = v-enca + 'Vendedor="' + STRING(Factura.Id-Vendedor) + ' ' + (IF AVAILABLE Empleado THEN STRING(Empleado.Nombre, "x(14)") ELSE '') + '" '.
    v-enca = v-enca + 'Zona="' + (IF AVAILABLE Cliente THEN STRING(Cliente.Id-Zona) ELSE '') + '" '.
    v-enca = v-enca + 'Cobrador="' + (IF AVAILABLE Cliente THEN STRING(Cliente.Id-Cobrador) ELSE '') + '" '.
    
    /* 2.3 Datos de totales de la factura */
    ASSIGN l-MetodoDePago = "PPD"
        l-NumCtaPago   = "".
    /*
IF Factura.FeFormaPago <> "" THEN DO:
ASSIGN l-MetodoDePago = Factura.FeFormaPago
       l-NumCtaPago   = Factura.FeDigitosCuenta.
END.
ELSE IF AVAILABLE Cliente AND Cliente.FEFormaPago > "" THEN DO:
     ASSIGN l-MetodoDePago = TRIM(Cliente.FEFormaPago).
     IF Cliente.FEDigitosCuenta > "" THEN
        ASSIGN l-NumCtaPago   = TRIM(Cliente.FEDigitosCuenta).
END.
*/
    l-LeyendaMetodo =  "METODO DE PAGO: " + l-MetodoDePago.
    v-enca = v-enca + 'RutaEmbarque="' + TRIM(l-LeyendaMetodo) + '" '.

    v-comprobante = v-comprobante + 'subTotal="' + STRING(Factura.SubTotal + Factura.ImpSeguro + Factura.ImpFlete) + '" '.
    IF Factura.Descuento > 0 THEN
        v-comprobante = v-comprobante + ' descuento="' + STRING(Factura.Descuento) + '" motivoDescuento="" '.
    v-comprobante = v-comprobante + 'total="' + STRING(Factura.Tot).
    
    /*_ RNPC - 2019-03-11 _*/
    IF Factura.Id-Moneda = 3 THEN v-comprobante = v-comprobante + '" Moneda="USD" TipoCambio="' + TRIM(STRING(Factura.TipoCambio,">>9.9999")) + '"'.
    ELSE  v-comprobante = v-comprobante + '" Moneda="MXN" TipoCambio="1"'.
    
    v-comprobante = v-comprobante + ' metodoDePago="' + l-MetodoDePago + '" '.
    
    IF l-NumCtaPago <> '' THEN
        v-comprobante = v-comprobante + 'NumCtaPago="' + l-NumCtaPago + '" '.
    v-comprobante = v-comprobante + 'tipoDeComprobante="ingreso" ReferenciaCFD="' + Factura.Id-Factura + '" xmlns="http://www.sat.gob.mx/cfd/2">'.
    
    IF Factura.Descuento > 0 THEN DO:
        v-enca = v-enca + 'SubTotal="' + STRING(Factura.SubTotal + Factura.ImpSeguro + Factura.ImpFlete,"$>>,>>>,>>9.99") + '" Descuento="' + STRING(Factura.Descuento,"$>>,>>>,>>9.99") + '" '.
        IF Factura.RetIva = 0 THEN DO:
            v-enca = v-enca + 'ValorNeto="' + STRING(Factura.SubTotal + Factura.ImpSeguro + Factura.ImpFlete - Factura.Descuento,"$>>,>>>,>>9.99") + '" IVA="'+ STRING(Factura.Iva,"$>>,>>>,>>9.99") + '" '.
            v-enca = v-enca + 'FleteEtiqueta="Maniobras" FleteValor="0" '.
        END.
        ELSE DO:
            v-enca = v-enca + 'ValorNeto="' + STRING(Factura.SubTotal + Factura.ImpSeguro + Factura.ImpFlete - Factura.Descuento,"$>>,>>>,>>9.99") + '" IVA="'+ STRING(Factura.Iva,">>,>>>,>>9.99") + '" '.
            v-enca = v-enca + 'FleteEtiqueta="IVA Retenido" FleteValor="' + STRING(Factura.RetIva,"$>>,>>>,>>9.99") + '" '.
        END.
    
        /*_ RNPC - 2019-03-12 _*/
        IF Factura.Id-Moneda = 3 THEN v-enca = v-enca + 'Total="' + STRING(Factura.Tot,">>,>>>,>>9.99") + ' USD "'.
        ELSE v-enca = v-enca + 'Total="' + STRING(Factura.Tot,">>,>>>,>>9.99") + ' "'.    
    END.
    ELSE DO:
        IF Factura.RetIva = 0 THEN DO:
            v-enca = v-enca + 'SubTotal="" Descuento="" ValorNeto="' + STRING(Factura.SubTotal + Factura.ImpSeguro + Factura.ImpFlete, "$>>,>>>,>>9.99") + '" IVA="' + STRING(Factura.iva,"$>>,>>>,>>9.99") + '" '.
            v-enca = v-enca + 'FleteEtiqueta="Maniobras" FleteValor="0" '.
        END.
        ELSE DO:
            v-enca = v-enca + 'SubTotal="" Descuento="" ValorNeto="' + STRING(Factura.SubTotal + Factura.ImpSeguro + Factura.ImpFlete,"$>>,>>>,>>9.99") + '" IVA="' + STRING(Factura.iva,"$>>,>>>,>>9.99") + '" '.
            v-enca = v-enca + 'FleteEtiqueta="IVA Retenido" FleteValor="' + STRING(Factura.RetIva,"$>>,>>>,>>9.99") + '" '.
        END.
        
        /*_ RNPC - 2019-03-12 _*/
        IF Factura.Id-Moneda = 3 THEN v-enca = v-enca + 'Total="' + STRING(Factura.Tot,"$>>,>>>,>>9.99") + ' USD "'.
        ELSE v-enca = v-enca + 'Total="' + STRING(Factura.Tot,"$>>,>>>,>>9.99") + ' "'.
    END.
    
    v-letras = ''.
    
    /*_ RNPC - 2019-03-11 _*/
    IF Factura.Id-Moneda = 3 THEN RUN /usr2/adosa/procs/rtn0007.p(INPUT Factura.Tot, OUTPUT v-letras).
    ELSE RUN /usr2/adosa/procs/rtn0005.p(INPUT Factura.Tot, OUTPUT v-letras).
    
    v-enca = v-enca + ' DiaPago="" CantLetras="' + v-letras + '" Interes="' + STRING(tasa) + '" Expedida="' + v-exped + '"'.
    
    IF Factura.Id-Cliente = 1186 OR Factura.Id-Cliente = 32851 THEN DO:
        FIND FIRST CliEmb WHERE CliEmb.Id-Cliente = Factura.Id-Cliente
            AND CliEmb.RazonSocial = Factura.RazonSocial1
            NO-LOCK NO-ERROR.
        l-NumTienda = IF AVAILABLE CliEmb THEN CliEmb.Id-CliEmb ELSE "0".
        IF Factura.Id-Cliente = 32851 THEN l-NumTienda = "692".
        v-enca = v-enca + ' NumProveedor="' + TRIM(Cliente.Proveedor) + '" NumTienda="' + TRIM(l-NumTienda) + '"/>'.
    END.
    ELSE v-enca = v-enca + '/>'.
    
    v-reng = 1.
    
    /* 3. Datos de Renglones de la Factura */
    EMPTY TEMP-TABLE tt-DetFactura.
    FOR EACH DetFactura OF Factura NO-LOCK:
        CREATE tt-DetFactura.
        BUFFER-COPY DetFactura TO tt-DetFactura
            ASSIGN tt-DetFactura.Bonifica = 0.
    END.
    IF Factura.Descuento > 0 AND Factura.Descuento1 > 0 THEN DO:
        l-TmpDesc = 0.
        FOR EACH tt-DetFactura WHERE tt-DetFactura.Tipo <= 2 EXCLUSIVE-LOCK:
            ASSIGN tt-DetFactura.Bonifica = ROUND(tt-DetFactura.Importe * (Factura.Descuento1 / 100),2).
            ASSIGN tt-DetFactura.Iva      = ROUND((tt-DetFactura.Importe - tt-DetFactura.Bonifica) * (tt-DetFactura.PorcIva / 100),6).
            l-TmpDesc = l-TmpDesc + tt-DetFactura.Bonifica.
        END.
        IF l-TmpDesc <> Factura.Descuento THEN DO:
            l-DifDesc = Factura.Descuento - l-TmpDesc.
            FOR EACH tt-DetFactura WHERE tt-DetFactura.Tipo <= 2
                EXCLUSIVE-LOCK BY tt-DetFactura.Bonifica DESCENDING:
                ASSIGN tt-DetFactura.Bonifica = tt-DetFactura.Bonifica + l-DifDesc.
                ASSIGN tt-DetFactura.Iva      = ROUND((tt-DetFactura.Importe - tt-DetFactura.Bonifica) * (tt-DetFactura.PorcIva / 100),6).
                LEAVE.
            END.
        END.        
    END.

    EMPTY TEMP-TABLE w-Impuestos.
    FOR EACH tt-DetFactura OF Factura NO-LOCK:
    
        IF tt-DetFactura.Tipo = 1 THEN DO:
            FIND ArtPres WHERE ArtPres.Id-Pres = tt-DetFactura.Id-Pres
                AND ArtPres.Id-Art = tt-DetFactura.Id-Articulo NO-LOCK NO-ERROR.
            FIND Kolor WHERE Kolor.Id-Color = tt-DetFactura.Id-Color NO-LOCK NO-ERROR.
        END.
        ELSE DO:
            RELEASE ArtPres.
            RELEASE Kolor.
        END.
        
        v-descrip = TRIM(tt-DetFactura.Descr).
        IF AVAILABLE Kolor AND tt-DetFactura.Id-Color > 0 THEN
            v-descrip = TRIM(v-descrip) + ' ' + TRIM(Kolor.Descr).
        
        v-descrip = REPLACE(v-descrip,'&','&amp;').
        v-descrip = REPLACE(v-descrip,'"','&quot;').
        v-descrip = REPLACE(v-descrip,"'",'&apos;').
        v-descrip = REPLACE(v-descrip,'<','&lt;').
        v-descrip = REPLACE(v-descrip,'>','&gt;').
        
        RUN Colapsa(INPUT-OUTPUT v-descrip).
        
        l-ClavePS = "".
        FIND Articulo WHERE Articulo.Id-Articulo = tt-DetFactura.Id-Articulo NO-LOCK NO-ERROR.
        IF tt-DetFactura.Tipo <= 2 AND tt-DetFactura.PrecUnit > 0 AND tt-DetFactura.Importe > 0 THEN DO:
            IF tt-DetFactura.Tipo = 1 AND AVAILABLE Articulo AND Articulo.Id-ClavePS > "" THEN
                ASSIGN l-ClavePS = Articulo.Id-ClavePS.
            ELSE IF tt-DetFactura.Descr BEGINS "VALES DE COMPRA ADOSA" THEN
                ASSIGN l-ClavePS = "14111608".
            ELSE 
                ASSIGN l-ClavePS = "01010101".
                  
            l-DetIVA = (tt-DetFactura.Importe - tt-DetFactura.Bonifica) * (tt-DetFactura.PorcIVA / 100).
            FIND w-Impuestos WHERE w-Impuestos.Tasa = (tt-DetFactura.PorcIVA / 100) EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE w-Impuestos THEN DO:
                CREATE w-Impuestos.
                ASSIGN w-Impuestos.Tasa = tt-DetFactura.PorcIva / 100.
            END.
            ASSIGN w-Impuestos.Base = w-Impuestos.Base + (tt-DetFactura.Importe - tt-DetFactura.Bonifica)
                w-Impuestos.Importe = w-Impuestos.Importe + l-DetIVA.
            RELEASE w-Impuestos.
        
            v-descrip = TRIM(v-descrip + ' ' + (IF AVAILABLE ArtPres AND NOT ArtPres.Descr BEGINS 'PZ' 
                AND NOT ArtPres.Descr = 'PIEZA' THEN TRIM(ArtPres.Descr) ELSE '')).
                
            v-concep = v-concep + '<Concepto cantidad="' + STRING(tt-DetFactura.Cant) + '" '. 
            v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="1" descripcion="' + TRIM(v-descrip) + '" '.
            
            IF Factura.Id-Cliente = 36577 THEN DO:
                v-concep = v-concep + 'unidad="UNO" ClaveUnidad="C62" ClaveProdServ="' + TRIM(l-ClavePS) + '" '.
                v-deta = v-deta + 'unidad="UNO" '.
            END.
            ELSE IF Factura.Id-Cliente = 42239 OR
                Factura.Id-Cliente = 48034 THEN DO:
                v-concep = v-concep + 'unidad="KILO" ClaveUnidad="KGM" ClaveProdServ="' + TRIM(l-ClavePS) + '" '.
                v-deta = v-deta + 'unidad="KILOGRAMO" '.
            END.
            ELSE DO:
                v-concep = v-concep + 'unidad="PIEZA" ClaveUnidad="H87" ClaveProdServ="' + TRIM(l-ClavePS) + '" '.
                v-deta = v-deta + 'unidad="PIEZA" '.
            END.
            /*
                            v-concep = v-concep + 'unidad="' + (IF AVAILABLE ArtPres THEN TRIM(ArtPres.Descr) ELSE 'PIEZA') + '" '.
                            v-deta = v-deta + 'unidad="' + (IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE '') + '" '.
            */
            IF tt-DetFactura.Tipo = 1 AND AVAILABLE Articulo AND
                Factura.Id-Cliente = 42239 THEN DO:
                FIND ArtProv WHERE ArtProv.Id-Articulo = Articulo.Id-Articulo
                    AND ArtProv.Id-Prov = Articulo.Id-Prov
                    NO-LOCK NO-ERROR.
                v-concep = v-concep + 'UnidadAduana="01" FraccionArancelaria="' + TRIM(ArtProv.FraccAran) + '" '.
            END.

            v-concep = v-concep + 'noIdentificacion="' + TRIM(tt-DetFactura.Id-Articulo) + '" descripcion="' + TRIM(v-descrip) + '" '.
            IF tt-DetFactura.Bonifica > 0 THEN
                v-concep = v-concep + 'Descuento="' + TRIM(STRING(tt-DetFactura.Bonifica,'zzzzzzzzzzz9.99')) + '" '.
            
            IF tt-DetFactura.Descto > 0 OR (tt-DetFactura.Tipo = 2 AND v-descrip BEGINS 'VALES DE COMPRA ADOSA') OR
                tt-DetFactura.IndicaPre = "P" THEN 
                v-concep = v-concep + 'valorUnitario="' + TRIM(STRING(tt-DetFactura.Importe / tt-DetFactura.Cant,'zzzzzzzzzzz9.999999')) + '" importe="' + STRING(tt-DetFactura.Importe) + '">'.
            ELSE v-concep = v-concep + 'valorUnitario="' + STRING(tt-DetFactura.PrecUnit) + '" importe="' + STRING(tt-DetFactura.Importe) + '">'.

            IF AVAILABLE Articulo AND Articulo.Pedimento AND
                Factura.Id-Cliente <> 42239 THEN DO:
                FIND Puerto WHERE Puerto.Id-Puerto = Articulo.Id-Puerto NO-LOCK NO-ERROR.
                IF AVAILABLE Puerto THEN DO:
                    IF LENGTH(TRIM(Articulo.PedNo)) >= 15 THEN DO: 
                        IF Articulo.FecPed <> ? THEN
                            v-fecAdu = STRING(YEAR(Articulo.FecPed)) + "-" + STRING(MONTH(Articulo.FecPed), "99") + "-" + STRING(DAY(Articulo.FecPed), "99").
                        ELSE v-fecAdu = STRING(YEAR(TODAY)) + "-" + STRING(MONTH(TODAY), "99") + "-" + STRING(DAY(TODAY), "99").
                        v-puerto = TRIM(Puerto.Nombre).
                        
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

            IF tt-DetFactura.PorcIVA > 0 THEN DO:
                IF Factura.RetIva = 0 THEN DO:
                    ASSIGN v-concep = v-concep + '<Impuestos><Traslados><Traslado Base="' + TRIM(STRING(tt-DetFactura.Importe - tt-DetFactura.Bonifica,"zzzzzzzzzzz9.99")) + 
                           '" Impuesto="002" TipoFactor="Tasa" TasaOCuota="' +
                           STRING(tt-DetFactura.PorcIva / 100,"9.999999") + '" Importe="' + TRIM(STRING(l-DetIVA,"zzzzzzzz9.999999")) +
                           '"/></Traslados></Impuestos>'.
                END.
                ELSE DO:
                    ASSIGN v-concep = v-concep + '<Impuestos><Traslados><Traslado Base="' + TRIM(STRING(tt-DetFactura.Importe,"zzzzzzzzzzz9.99")) + 
                           '" Impuesto="002" TipoFactor="Tasa" TasaOCuota="' +
                           STRING(tt-DetFactura.PorcIva / 100,"9.999999") + '" Importe="' + TRIM(STRING(l-DetIVA,"zzzzzzzz9.999999")) +
                           '"/></Traslados>' +
                           '<Retenciones><Retencion Base="' + TRIM(STRING(tt-DetFactura.Importe,"zzzzzzzzzzz9.99")) + 
                           '" Impuesto="002" TipoFactor="Tasa" TasaOCuota="' +
                           STRING(tt-DetFactura.PorcIva / 100,"9.999999") + '" Importe="' + TRIM(STRING(l-DetIVA,"zzzzzzzz9.999999")) +
                           '"/></Retenciones>' +
                           '</Impuestos>'.
                END.
            END.
            /*ELSE ASSIGN v-concep = v-concep + '<Impuestos/>'.*/
            v-concep = v-concep + '</Concepto>'.
            

            v-deta = v-deta + 'porcIVA="' + STRING(tt-DetFactura.PorcIva,">9%") + '" tipoPrecio="' + (IF tt-DetFactura.PorcIVA = 0 THEN 'X' ELSE tt-DetFactura.IndicaPre) + '" '.
            
            v-deta = v-deta + 'noIdentificacion="' + tt-DetFactura.Id-Articulo + '" cantidad="' + STRING(tt-DetFactura.Cant) + '" '.
            IF Factura.Id-Moneda > 1 THEN
                v-deta = v-deta + 'valorUnitario="' + STRING(tt-DetFactura.PrecUnit,"->>,>>>,>>9.999999") + '" importe="' + STRING(tt-DetFactura.Importe,"->>,>>>,>>9.99") + '" comentario="" '.
            ELSE v-deta = v-deta + 'valorUnitario="' + STRING(tt-DetFactura.PrecUnit,"->>,>>>,>>9.99") + '" importe="' + STRING(tt-DetFactura.Importe,"->>,>>>,>>9.99") + '" comentario="" '.
            v-deta = v-deta + 'descuento="' + (IF tt-DetFactura.descto > 0 THEN STRING(tt-DetFactura.descto,">9%") ELSE '') + '"'.

            IF Factura.Id-Cliente = 1186 OR Factura.Id-Cliente = 32851 THEN DO:
                IF tt-DetFactura.Filler-1 <> "" THEN
                    l-CodCliente = tt-DetFactura.Filler-1.
                ELSE DO:
                    FIND ArtBarra WHERE ArtBarra.Id-Cliente = Factura.Id-Cliente
                        AND ArtBarra.Id-Articulo = tt-DetFactura.Id-Articulo
                        AND ArtBarra.Id-Color = tt-DetFactura.Id-Color
                        AND ArtBarra.Id-Pres = tt-DetFactura.Id-Pres
                        NO-LOCK NO-ERROR.
                    l-CodCliente = IF AVAILABLE ArtBarra THEN ArtBarra.CodBarras ELSE '0'.
                END.
                v-deta = v-deta + ' CodigoBarra="' + TRIM(l-CodCliente) + '"/>'.
            END.
            ELSE v-deta = v-deta + '/>'.
            
            v-reng = v-reng + 1. 
        
        END.
        ELSE DO:
            
            v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" '.
            v-deta = v-deta + 'unidad="" porcIVA="" tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
            v-deta = v-deta + 'comentario="' + v-descrip + '" descuento=""/>'. 
            
            v-reng = v-reng + 1.
            
        END.
            
        FOR EACH DetSerie WHERE DetSerie.Tipo = 'Factura'
            AND DetSerie.Documento = Factura.Id-Factura
            AND DetSerie.Reng = tt-DetFactura.Reng NO-LOCK:
        
            v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" '.
            v-deta = v-deta + 'unidad="" porcIVA="" tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
            v-deta = v-deta + 'comentario="No. DE SERIE: ' + TRIM(DetSerie.SerNo) + '" descuento=""/>'.
            
            v-reng = v-reng + 1.
        
        END.
        
        
        
    END.
    
    /* 3.1 Agregar los datos de los pedidos */
    IF Factura.Pedidos <> "" AND Factura.Id-Factura BEGINS "0" AND NOT Factura.Especial THEN DO:
        
        DO i = 1 TO NUM-ENTRIES(Factura.Pedidos):
    
            v-pedido = ENTRY(i,Factura.Pedidos).
        
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
    
    /* 3.3 Agregar el seguro por maniobras */
    IF Factura.ImpSeguro > 0 THEN DO:
    
        v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="1" descripcion="SEGURO DE TRASLADO DE MERCANCIA 5.0/MILLAR" unidad="SERV" porcIVA="' + STRING(l-IvaZona,">9%") + '" '.
        v-deta = v-deta + 'tipoPrecio="" noIdentificacion="" cantidad="1" valorUnitario="' + STRING(Factura.ImpSeguro,"->>,>>>,>>9.99") + '" importe="' + STRING(Factura.ImpSeguro,"->>,>>>,>>9.99") + '" '.
        v-deta = v-deta + 'comentario="" descuento=""/>'.
        
        v-concep = v-concep + '<Concepto cantidad="1" unidad="UNIDAD DE SERVICIO" ClaveUnidad="E48" ClaveProdServ="84131504" noIdentificacion="" '.
        v-concep = v-concep + 'descripcion="SEGURO DE TRASLADO DE MERCANCIA 5.0/MILLAR" valorUnitario="' + STRING(Factura.ImpSeguro) + '" importe="' + STRING(Factura.ImpSeguro) + '"'.
        IF Factura.IvaSeguro > 0 THEN DO:
            ASSIGN l-DetIVA = Factura.ImpSeguro * (l-IvaZona / 100).
            ASSIGN v-concep = v-concep + '><Impuestos><Traslados><Traslado Base="' + TRIM(STRING(Factura.ImpSeguro,"zzzzzzzzzzz9.99")) + 
                   '" Impuesto="002" TipoFactor="Tasa" TasaOCuota="' +
                   STRING(l-IvaZona / 100,"9.999999") + '" Importe="' + TRIM(STRING(l-DetIVA,"zzzzzzzzzzz9.999999")) +
                   '"/></Traslados></Impuestos>'.
        END.
        ELSE v-concep = v-concep + ">".
        v-concep = v-concep + '</Concepto>'.

        FIND w-Impuestos WHERE w-Impuestos.Tasa = 0.16 EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-Impuestos THEN DO:
            CREATE w-Impuestos.
            ASSIGN w-Impuestos.Tasa = 0.16.
        END.
        ASSIGN w-Impuestos.Base = w-Impuestos.Base + Factura.ImpSeguro
            w-Impuestos.Importe = w-Impuestos.Importe + l-DetIVA.
        RELEASE w-Impuestos.

            
        v-reng = v-reng + 1.     
    
    END.

    /* 3.3 Agregar cargo por flete */
    IF Factura.ImpFlete > 0 THEN DO:
    
        v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="1" descripcion="MANIOBRAS DE ENTREGA" unidad="SERV" porcIVA="' + STRING(l-IvaZona,">9%") + '" '.
        v-deta = v-deta + 'tipoPrecio="" noIdentificacion="" cantidad="1" valorUnitario="' + STRING(Factura.ImpFlete,"->>,>>>,>>9.99") + '" importe="' + STRING(Factura.ImpFlete,"->>,>>>,>>9.99") + '" '.
        v-deta = v-deta + 'comentario="" descuento=""/>'.
        
        v-concep = v-concep + '<Concepto cantidad="1" unidad="UNIDAD DE SERVICIO" ClaveUnidad="E48" ClaveProdServ="78121601" noIdentificacion="" '.
        v-concep = v-concep + 'descripcion="MANIOBRAS DE ENTREGA" valorUnitario="' + STRING(Factura.ImpFlete) + '" importe="' + STRING(Factura.ImpFlete) + '"'.
        IF Factura.IvaFlete > 0 THEN DO:
            ASSIGN l-DetIVA = Factura.ImpFlete * (l-IvaZona / 100).
            ASSIGN v-concep = v-concep + '><Impuestos><Traslados><Traslado Base="' + TRIM(STRING(Factura.ImpFlete,"zzzzzzzzzzz9.99")) + 
                   '" Impuesto="002" TipoFactor="Tasa" TasaOCuota="' +
                   STRING(l-IvaZona / 100,"9.999999") + '" Importe="' + TRIM(STRING(l-DetIva,"zzzzzzzzzzz9.999999")) +
                   '"/></Traslados></Impuestos>'.

            FIND w-Impuestos WHERE w-Impuestos.Tasa = (l-IvaZona / 100) EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE w-Impuestos THEN DO:
                CREATE w-Impuestos.
                ASSIGN w-Impuestos.Tasa = (l-IvaZona / 100).
            END.
            ASSIGN w-Impuestos.Base = w-Impuestos.Base + Factura.ImpFlete
                w-Impuestos.Importe = w-Impuestos.Importe + l-DetIva.
            RELEASE w-Impuestos.
        END.
        ELSE v-concep = v-concep + ">".
        v-concep = v-concep + '</Concepto>'.
        v-reng = v-reng + 1.     
    END.

    
    /* 3.2 Agregar los datos de los pedimentos de articulos */
    FOR EACH v-pedim NO-LOCK BREAK BY v-pedim.PedNo BY v-pedim.Puerto:
    
        v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" unidad="" porcIVA="" '.
        v-deta = v-deta + 'tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
        /*v-deta = v-deta + 'comentario="Pedimento: ' + STRING(v-pedim.PedNo) + ' Fecha: ' + (IF v-pedim.FecPed <> ? THEN STRING(v-pedim.FecPed,"99/99/9999") ELSE " ") + ' Puerto: ' + v-pedim.Puerto + '" descuento=""/>'.*/
        v-deta = v-deta + 'comentario="Pedimento: ' + v-pedim.PedNo  + '" descuento=""/>'.
            
        v-reng = v-reng + 1.
    
    END.
    
    /* 3.4 Agregar aviso de IVA Retenido */
    IF Factura.retiva > 0 THEN DO:
    
        v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" unidad="" porcIVA="" '.
        v-deta = v-deta + 'tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
        v-deta = v-deta + 'comentario="IMPUESTO RETENIDO DE CONFORMIDAD CON LA LEY DEL IMPUESTO AL VALOR AGREGADO ' + Cliente.RegAltex + '" descuento=""/>'.
            
        v-reng = v-reng + 1. 
    
    END.
    ELSE IF Cliente.RegAltex <> "" AND Cliente.Id-Zona = 103 AND Factura.Iva = 0 THEN DO:
    
        v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="2" descripcion="" unidad="" porcIVA="" '.
        v-deta = v-deta + 'tipoPrecio="" noIdentificacion="" cantidad="" valorUnitario="" importe="" '.
        v-deta = v-deta + 'comentario="' + Cliente.RegAltex + '" descuento=""/>'.
            
        v-reng = v-reng + 1.
    
    END.
    
    /* 3.5 Impuestos */
    v-impsto = "".
    /*IF Factura.retiva > 0 THEN DO:
    
        v-impsto = '<Impuestos totalImpuestosRetenidos="' + STRING(Factura.retiva) + '" totalImpuestosTrasladados="' + STRING(Factura.iva) + '" ><Retenciones>'.
        v-impsto = v-impsto + '<Retencion impuesto="IVA" importe="' + STRING(Factura.retiva) + '"/></Retenciones>'.
        v-impsto = v-impsto + '<Traslados><Traslado impuesto="IVA" tasa="' + STRING(l-IvaZona) + '" importe="' + STRING(Factura.iva) + '"/></Traslados></Impuestos>'.
    
    END.
    ELSE*/ IF Factura.IVA > 0 THEN DO:
        /* ************************************************************************************************************************************* */
        /* Cuidar que la tasa este correcta */
        IF Factura.RetIva = 0 THEN DO:
            v-impsto = '<Impuestos totalImpuestosTrasladados="' + TRIM(STRING(Factura.IVA,"zzzzzzzz9.99")) + '" ><Traslados>'.
        END.
        ELSE DO:
            v-impsto = '<Impuestos totalImpuestosRetenidos="' + TRIM(STRING(Factura.RetIVA,"zzzzzzzz9.99")) + '" totalImpuestosTrasladados="' + TRIM(STRING(Factura.IVA,"zzzzzzzz9.99")) + '" >'.
            v-impsto = v-impsto + '<Retenciones><Retencion impuesto="IVA" importe="' + STRING(Factura.retiva) + '"/></Retenciones><Traslados>'.
        END.
        
        /*FIND Asociado WHERE Asociado.Id-Cliente = Factura.Id-Cliente
                        AND Asociado.Id-Asociado = 35383 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Asociado THEN 
            FIND Asociado WHERE Asociado.Id-Cliente = Factura.Id-Cliente
                            AND Asociado.Id-Asociado = 1435 NO-LOCK NO-ERROR.
        IF Factura.Id-Cliente = 35383 OR Factura.Id-Cliente = 1435 OR AVAILABLE Asociado OR
           (AVAILABLE RfcCfdiEsp AND RfcCfdiEsp.Tipo = 2) THEN DO:*/
            
        FOR EACH w-Impuestos WHERE w-Impuestos.Importe > 0 NO-LOCK:
            /*v-impsto = v-impsto + '<Traslado impuesto="IVA" TipoFactor="Tasa" tasa="' + STRING(w-Impuestos.Tasa * 100,"99") + '" importe="' + TRIM(STRING(w-Impuestos.Importe,"zzzzzzzz9.999999")) + '"/>'.*/
            v-impsto = v-impsto + '<Traslado impuesto="IVA" TipoFactor="Tasa" tasa="' + STRING(w-Impuestos.Tasa * 100,"99") + '" importe="' + TRIM(STRING(Factura.IVA,"zzzzzzzz9.99")) + '" Base="' + TRIM(STRING(w-Impuestos.Base,"zzzzzzzzz9.99")) + '"/>'.
        END.
        /*END.
        ELSE DO:
            FOR EACH w-Impuestos WHERE w-Impuestos.Importe > 0 NO-LOCK:
                v-impsto = v-impsto + '<Traslado impuesto="IVA" TipoFactor="Tasa" tasa="' + STRING(w-Impuestos.Tasa * 100,"99") + '" importe="' + TRIM(STRING(w-Impuestos.Importe,"zzzzzzzz9.999999")) + '"/>'.
                /*v-impsto = v-impsto + '<Traslado impuesto="IVA" TipoFactor="Tasa" tasa="' + STRING(w-Impuestos.Tasa * 100,"99") + '" importe="' + TRIM(STRING(Factura.IVA,"zzzzzzzz9.999999")) + '"/>'.*/
            END.
        END.*/
        v-impsto = v-impsto + '</Traslados></Impuestos>'.
    END.
    ELSE v-impsto = v-impsto + '<Impuestos/>'.
    
    IF Factura.Id-Cliente = 42239 THEN DO:
        v-cce = '<ComercioExterior TotalUSD="' + TRIM(STRING(Factura.Tot)) + 
            '" TipoCambioUSD="' + TRIM(STRING(Factura.TipoCambio,">>9.9999")) +
            '" Incoterm="EXW" ClaveDePedimento="A1" TipoOperacion="2"/>'.
    END.
    ELSE v-cce = "".
    
    /* 4. Unir partes de la Factura */ /* Pruebas: AAA010101AAA Prod: AOF870529IU7 */
    IF v-cfdirel > "" THEN
       v-comprobante = v-comprobante + v-cfdirel.
    IF {programas/sist0001.i} = 'DESARROLLO' THEN 
        v-comprobante = v-comprobante + '<Emisor rfc="AAA010101AAA" nombre="Abastecedora de Oficinas, S.A. de C.V.">'.
    ELSE   
        v-comprobante = v-comprobante + '<Emisor rfc="AOF870529IU7" nombre="ABASTECEDORA DE OFICINAS">'.
    v-comprobante = v-comprobante + '<DomicilioFiscal calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" '.
    IF Factura.Id-Cliente = 42239 THEN
        v-comprobante = v-comprobante + 'municipio="039" estado="NLE" pais="MEX" codigoPostal="64000"/>'.
    ELSE v-comprobante = v-comprobante + 'municipio="Monterrey" estado="Nuevo León" pais="México" codigoPostal="64000"/>'.
    v-comprobante = v-comprobante + v-expEn + ' <RegimenFiscal Regimen="601"/></Emisor>'.

    v-comprobante = v-comprobante + v-enca + v-deta + v-recep + v-dom + '</Receptor>'.
    v-comprobante = v-comprobante + '<Conceptos>' + v-concep + '</Conceptos>' + v-impsto.
    
    /*
        IF CAN-DO('2400,3332,970,3381,3383,31131,330,3201,1773,172,4064,3537,33717,31630,29347,34290,34392,34416',STRING(Factura.Id-Cliente)) THEN DO:
        
            RUN vtac2064.p(INPUT-OUTPUT v-addnd, INPUT Factura.Id-Cliente, INPUT Factura.Id-Factura).
            v-comprobante = v-comprobante + v-addnd.
        
        END.
    */ 
    
    v-comprobante = v-comprobante + v-cce.
    v-comprobante = v-comprobante + '</Comprobante>'.
    /*
    COPY-LOB FROM v-comprobante TO FILE "/usr2/sis3/ejemplo.txt" NO-CONVERT.
    
    OUTPUT TO /usr2/sis3/vitroFactura.xml.    
        EXPORT v-comprobante.
    OUTPUT CLOSE.
     */

    l-Intent = 1.
    REPEAT:
        l-RutaReq = "/usr2/compartido/request/" + 
            Factura.Id-Factura + STRING(l-Intent,"999") + ".xml".
        IF SEARCH(l-RutaReq) <> ? THEN 
            l-Intent = l-Intent + 1.
        ELSE LEAVE.
    END.
    OUTPUT TO VALUE(l-RutaReq).
    EXPORT v-comprobante.   
    OUTPUT CLOSE.          
    /* Asigna permisos 777 al archivo */
    OS-COMMAND SILENT VALUE("chmod 777 " + l-RutaReq).
    IF buffFactura.Pedidos = "3075066" OR
        buffFactura.Pedidos = "3075067" OR
        buffFactura.Pedidos = "3075068" THEN RETURN.
    IF buffFactura.Pedidos = "3075112" OR
        buffFactura.Pedidos = "3075113" OR
        buffFactura.Pedidos = "3075114" THEN RETURN.
    IF buffFactura.Pedidos = "3075343" OR
        buffFactura.Pedidos = "3075344" THEN RETURN.
    IF buffFactura.Pedidos BEGINS "2665504" OR
        buffFactura.Pedidos BEGINS "2663056" THEN RETURN.
    IF buffFactura.Pedidos BEGINS "2668610" OR
        buffFactura.Pedidos BEGINS "2669216" THEN RETURN.
    
    /* Realizar llamadas y operaciones con el webService de eDoc */
    
    CREATE SERVER hWS.
    vlconnect = hWS:CONNECT(servidor) NO-ERROR.
        
    IF vlconnect THEN DO:
        RUN wsEmiteCFDSoap SET hEmiteCFDSoap ON hws.
        /*  
        OUTPUT TO '/usr2/sis10/factaut.txt' APPEND.
            EXPORT
                "        "
                "ENTRA AL IntentaCFD"
                SKIP.            
        OUTPUT CLOSE.
        OS-COMMAND SILENT VALUE("chmod 777 " + '/usr2/sis10/factaut.txt').
       */
        RUN IntentaCFD(INPUT pIdUser,OUTPUT v-Resp).
        vldisconnect = hWS:DISCONNECT().
    END.
    ELSE
        MESSAGE "No se pudo conectar al Web Service".
        
    PAUSE 2 NO-MESSAGE.
    
END.

PROCEDURE ClienteVitro:
    DEFINE INPUT-OUTPUT PARAMETER v-direc AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER v-cte AS INTEGER NO-UNDO.
    
    CASE v-cte:
        WHEN 172 THEN
            v-direc = '<Domicilio calle="Escobedo Nte" noExterior="1405" noInterior="" colonia="Treviño" '.
        WHEN 330 THEN
            v-direc = '<Domicilio calle="Magallanes Ote" noExterior="517" noInterior="" colonia="TREVIÑO" '.
        WHEN 3332 THEN
            v-direc = '<Domicilio calle="Magallanes Ote" noExterior="517" noInterior="" colonia="TREVIÑO" '.
        WHEN 1773 THEN
            v-direc = '<Domicilio calle="Carr a Garcia Km 9" noExterior="SN" noInterior="" colonia="Sin Colonia" '.
        WHEN 3381 THEN 
            v-direc = '<Domicilio calle="Carr a Garcia Km 10" noExterior="SN" noInterior="" colonia="Sin Colonia" '.
        WHEN 3537 THEN
            v-direc = '<Domicilio calle="Carr a Garcia Km 10" noExterior="SN" noInterior="" colonia="Sin Colonia" '.
        WHEN 3383 THEN
            v-direc = '<Domicilio calle="Carretera a Garcia Km 10.3" noExterior="SN" noInterior="" colonia="Sin Colonia" '.
        WHEN 970 THEN
            v-direc = '<Domicilio calle="Carretera a Garcia Km 10.5" noExterior="SN" noInterior="" colonia="Sin Colonia" '.
        WHEN 31131 THEN
            v-direc = '<Domicilio calle="CARRETERA MUNICIPAL LIBRE A GARCIA" noExterior="KM 10" noInterior="" colonia="SIN COLONIA" '.       
        WHEN 33717 THEN
            v-direc = '<Domicilio calle="Keramos" noExterior="225" noInterior="" colonia="Del Prado" '.       
        WHEN 4064 THEN
            v-direc = '<Domicilio calle="KERAMOS PONIENTE" noExterior="225" noInterior="" colonia="DEL PRADO" '.       
        WHEN 29347 THEN
            v-direc = '<Domicilio calle="Av Madero Pte" noExterior="2950" noInterior="" colonia="Mitras Centro" '.       
        WHEN 34290 THEN
            v-direc = '<Domicilio calle="AV. RIO DE LOS REMEDIOS" noExterior="1" noInterior="" colonia="Col. San Juan Ixhuatepec" '.       
        WHEN 34392 THEN
            v-direc = '<Domicilio calle="KERAMOS PONIENTE" noExterior="225" noInterior="" colonia="DEL PRADO" '.       
        WHEN 34416 THEN
            v-direc = '<Domicilio calle="CARRETERA A GARCIA" noExterior="KM 9" noInterior="" colonia="CENTRO VILLA DE GARCIA (CASCO)" '.
    END.

END.

PROCEDURE ValidaRFC:
    DEFINE INPUT PARAMETER v-rfc AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER rfcOk AS LOGICAL NO-UNDO INITIAL TRUE.
    DEFINE VARIABLE v-letra  AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-tot    AS INTEGER  NO-UNDO.
    DEFINE VARIABLE v-indi   AS INTEGER  NO-UNDO.
    
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
    DEFINE INPUT-OUTPUT PARAMETER v-colapsa AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-newStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-letra  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-tot    AS INTEGER  NO-UNDO.
    DEFINE VARIABLE v-indi   AS INTEGER  NO-UNDO.
    DEFINE VARIABLE espacios AS INTEGER  NO-UNDO INITIAL 0.
    
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
    DEFINE INPUT-OUTPUT PARAMETER v-dom AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER v-city AS INTEGER  NO-UNDO.
    DEFINE INPUT PARAMETER v-lab1 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER v-lab2 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER v-lab3 AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE v-cityName AS CHARACTER NO-UNDO.
    
    FIND Ciudad WHERE Ciudad.Id-Ciudad = v-city NO-LOCK NO-ERROR.
    
    IF AVAILABLE Ciudad THEN DO:
        
        IF CAN-DO('2400,3332,970,3381,3383,330,3201,1773,172,4064,3537,33717,29347,34290,34392',STRING(Factura.Id-Cliente)) THEN DO:
            CASE v-city:
                WHEN 77 THEN
                    v-cityName = 'García'.
                WHEN 78 THEN
                    v-cityName = 'San Pedro Garza García'.
                OTHERWISE
                v-cityName = TRIM(Ciudad.Nombre).
            END.
        END.
        ELSE 
            v-cityName = TRIM(Ciudad.Nombre).
                        
        FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
        
        IF AVAILABLE Estado THEN DO:
        
            FIND Pais WHERE Pais.Id-Pais = Estado.Id-Pais NO-LOCK NO-ERROR.
            
            IF AVAILABLE Pais THEN DO:
                
                v-dom = v-dom + v-lab1 + '="' + v-cityName + '" '.
                IF CAN-DO('2400,3332,970,3381,3383,31131,330,3201,1773,172,4064,3537,33717,29347,34290,34392,34416',STRING(Factura.Id-Cliente)) THEN DO:
                    v-dom = v-dom + v-lab2 + '="' + (IF Estado.Id-Estado = '019' THEN 'Nuevo León' ELSE TRIM(Estado.Nombre)) + '" '.
                    v-dom = v-dom + v-lab3 + '="' + (IF Pais.Id-Pais = '001' THEN 'México' ELSE TRIM(Pais.Nombre)) + '" '.
                END.
                ELSE
                    ASSIGN 
                        v-dom = v-dom + v-lab2 + '="' + TRIM(Estado.Nombre) + '" '
                        v-dom = v-dom + v-lab3 + '="' + TRIM(Pais.Nombre) + '" '.                    
            END.
        
        END.
    
    END.
    
END.

PROCEDURE IntentaCFD.  
   DEF INPUT  PARAMETER pIdUser   AS CHARACTER NO-UNDO.
   DEF OUTPUT PARAMETER pResp     AS CHARACTER .   
   FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
   IF AVAILABLE Usuario THEN ASSIGN g-Origen = Usuario.Id-Ubicacion.
   l-Intent = 1.
   DO WHILE l-Intent <= l-MaxInt:
      RUN EmiteCFD IN hEmiteCFDSoap(INPUT v-comprobante, OUTPUT v-respuesta).
      RUN RecuperaUUID IN hEmiteCFDSoap(INPUT buffFactura.Id-Factura, OUTPUT v-UUID).

        /*MESSAGE "Folio Generado: " + v-respuesta.
        PAUSE 2 NO-MESSAGE.*/
      
        /*  
        OUTPUT TO '/usr2/sis10/facteaut.txt' APPEND.
            EXPORT
                "            "   
                "INTENTO"
                STRING(l-Intent)
                "FOLIO GENERADO"
                STRING(v-Respuesta)
                "COMPROBANTE"
                SKIP.            
        OUTPUT CLOSE.
        OS-COMMAND SILENT VALUE("chmod 777 " + '/usr2/sis10/facteaut.txt').
       */   
      
        /* Generar el archivo PDF para poder imprimir la factura */
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
            /*
            IF (Factura.Id-Entrega >= 12 AND Factura.Id-Entrega <= 19) AND
                Factura.Id-Entrega <> 16 AND (PROGRAM-NAME(2) MATCHES "*vtaa05*" OR 
                              PROGRAM-NAME(2) MATCHES "*vtaa04*" OR
                              PROGRAM-NAME(2) MATCHES "*vtad0601*" ) AND
                PROGRAM-NAME(2) <> 'vtaa0550' THEN 
                RUN vtac0301.p(INPUT Factura.Id-Factura, 6,"").
            */
            DO TRANSACTION:
                FIND Factura WHERE RECID(Factura) = RECID(buffFactura) EXCLUSIVE-LOCK.
                ASSIGN
                    Factura.Folioe = v-valores[1] + ',' + v-serie + ',' + v-folio
                    Factura.Id-Fiscal = TRIM(v-serie) + TRIM(v-folio)
                    Factura.FeMetodoPago = l-MetodoDePago
                    Factura.FeFormaPago = "99"
                    Factura.FeDigitosCuenta = l-NumCtaPago
                    Factura.Version = "4.0"
                    Factura.UUID = SUBSTRING(STRING(v-UUID),1,INDEX(STRING(v-UUID),",") - 1).
            END.
            RELEASE Factura.
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
              IF v-Respuesta MATCHES "*Emisor*obligaciones*"  THEN DO:
                  RUN /usr2/sea/appserver/fuentes/ImprimeFacturaTMP.p(INPUT Factura.Id-Factura). /* Imprime factura temporal */
                  CASE g-Origen:
                      WHEN "02B" THEN DO:
                          UNIX SILENT VALUE("lpr7 /usr2/sea/reportes/" + Factura.Id-Factura + ".pdf").
                          IF Factura.Id-Entrega = 11 THEN DO:
                              UNIX SILENT VALUE("lpr7 /usr2/sea/reportes/" + Factura.Id-Factura + ".pdf").
                          END.
                      END.
                      WHEN "FUG" THEN DO:
                          UNIX SILENT VALUE("lpr7 /usr2/sea/reportes/" + Factura.Id-Factura + ".pdf").
                          IF Factura.Id-Entrega = 11 THEN DO:
                              UNIX SILENT VALUE("lpr7 /usr2/sea/reportes/" + Factura.Id-Factura + ".pdf").
                          END.
                      END.
                      WHEN "11" THEN DO:
                          UNIX SILENT VALUE("lpsatesf /usr2/sea/reportes/" + Factura.Id-Factura + ".pdf").
                      END.
                      WHEN "12" THEN DO:
                          UNIX SILENT VALUE("lpchfact /usr2/sea/reportes/" + Factura.Id-Factura + ".pdf").
                      END.
                  END CASE.
                  LEAVE.
              END.
              ELSE DO:
        	      OUTPUT TO VALUE("/usr2/compartido/request/" + Factura.Id-Factura + ".err").
        	      EXPORT v-respuesta.
        	      OUTPUT CLOSE.
        	      OS-COMMAND SILENT VALUE("chmod 777 " + "/usr2/compartido/request/" + Factura.Id-Factura + ".err").
        	      
        	         ASSIGN pResp = v-respuesta.
        	      MESSAGE "Ocurrio un error al generar la factura electronica".
                  LEAVE.
              END.
          END.
      END.
      PAUSE 2 NO-MESSAGE.
   END.
END.

