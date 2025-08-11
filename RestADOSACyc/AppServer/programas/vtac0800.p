@openapi.openedge.export FILE(type="REST", executionMode="external", useReturnValue="false", writeDataSetBeforeImage="false").
/*
    Empresa : ADOSA
    Programa: vtac0800.p
    Funcion : Correo electronico de confirmacion de alta de pedidos
    Autor   : ALEX
    Fecha   : 15 de Marzo del 2016
*/
 
//{/usr2/adosa/includes/sia00000.var}

DEFINE STREAM s-Salida.

DEFINE BUFFER b-detpedido FOR DetPedido.

DEFINE INPUT PARAMETER l-Pedido LIKE Pedido.Id-Pedido NO-UNDO.
DEFINE INPUT PARAMETER l-Resto LIKE Pedido.Resto NO-UNDO.

DEFINE NEW SHARED VARIABLE l-Arch2       AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-Arch1       AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-MailDe      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-ResponderA  AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-login       AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-Enviado     AS LOGICAL   NO-UNDO.  

DEFINE            VARIABLE l-Archivo     AS CHARACTER.
DEFINE            VARIABLE l-TipoEntrega AS CHARACTER NO-UNDO FORMAT "X(60)".
DEFINE            VARIABLE l-CalleEmb    AS CHARACTER NO-UNDO FORMAT "X(60)".
DEFINE            VARIABLE l-ColEmb      AS CHARACTER NO-UNDO FORMAT "X(60)".
DEFINE            VARIABLE l-CdEmb       AS CHARACTER NO-UNDO FORMAT "X(60)".
DEFINE            VARIABLE l-DirEmb      AS CHARACTER NO-UNDO FORMAT "X(60)".
DEFINE            VARIABLE l-i           AS INTEGER.

DEFINE            VARIABLE l-Asunto      LIKE e-Mail.Asunto NO-UNDO.
DEFINE            VARIABLE l-Contenido   LIKE e-Mail.Contenido NO-UNDO.
DEFINE            VARIABLE l-entrada     LIKE e-Mail.Contenido NO-UNDO.
DEFINE            VARIABLE l-Ciudad      AS CHARACTER FORMAT 'x(60)' NO-UNDO.
DEFINE            VARIABLE l-Estado      AS CHARACTER FORMAT 'x(60)' NO-UNDO.
DEFINE            VARIABLE l-Delegacion  AS CHARACTER FORMAT 'x(60)' NO-UNDO.
DEFINE            VARIABLE l-firma       LIKE usuario.firma NO-UNDO.
DEFINE            VARIABLE l-eMail       LIKE Pedido.e-Mail NO-UNDO.
DEFINE            VARIABLE l-sistemas    LIKE Pedido.e-Mail NO-UNDO.

FIND Pedido WHERE Pedido.Id-Pedido = l-Pedido
    AND Pedido.Resto= l-Resto NO-LOCK NO-ERROR.
              
IF Pedido.Id-Vendedor = "0100" THEN
    RETURN.
              
FIND EstPedido WHERE EstPedido.Id-Pedido = l-Pedido
    AND EstPedido.Id-Seq = l-Resto NO-LOCK NO-ERROR.
                 
FIND Cliente WHERE Cliente.id-cliente = pedido.id-cliente NO-LOCK NO-ERROR.

FIND CondVta WHERE CondVta.Id-Cond = Pedido.Id-Cond NO-LOCK NO-ERROR.

FIND Entrega WHERE Entrega.Id-Entrega = Pedido.Id-Entrega NO-LOCK NO-ERROR.

FIND Vendedor WHERE Vendedor.Id-Vendedor = Pedido.Id-Vendedor NO-LOCK NO-ERROR.
FIND FIRST Empleado WHERE Empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
FIND LAST Usuario WHERE Usuario.Id-User = Vendedor.Iniciales NO-LOCK NO-ERROR.
IF NOT AVAILABLE Usuario THEN 
    FIND LAST Usuario WHERE Usuario.Id-User = USERID("dictdb") NO-LOCK NO-ERROR. 

FIND FIRST Ciudad WHERE Ciudad.Id-Ciudad = Pedido.Id-Ciudad NO-LOCK NO-ERROR.
IF AVAILABLE ciudad THEN 
DO:
    ASSIGN 
        l-Ciudad = Ciudad.Nombre.
    FIND FIRST Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
    IF AVAILABLE Estado THEN
        ASSIGN l-Estado = Estado.Nombre.
END.

FIND cliente OF pedido NO-LOCK NO-ERROR.
IF AVAILABLE cliente THEN
    ASSIGN l-Delegacion = Cliente.Delegacion.

ASSIGN
    l-Archivo   = "/usr2/adosa/tmp/" + l-Pedido + "-" + TRIM(STRING(l-Resto,"99")) + ".lst"
    l-Archivo   = "/home/sis10/confirmacion_pedido.html"
    l-Asunto    = TRIM(Pedido.RazonSocial) + ", Confirmacion de pedido " + l-Pedido + "-" + TRIM(STRING(l-Resto,"99"))
    l-Contenido = "Hola " + TRIM(Pedido.RazonSocial) + ".<br/><br/>" +
                  "Agradecemos tu compra. En este correo encontrar�s un anexo en el cual puedes consultar a detalle tu pedido.<br/><br/>" +
                  "En caso de existir una modificaci�n o error, te pedimos contactar a " + 
                  (IF AVAILABLE Empleado THEN Empleado.Nombre ELSE "tu vendedor") +
                  " en el correo "+ 
                  (IF AVAILABLE Usuario AND Usuario.e-Mail <> "" THEN Usuario.e-Mail ELSE "ventas@adosa.com.mx") +
                  " o al tel�fono 8181581500, en la opci�n 7.<br/><br/>" +
                  '<p><span style="color:red;">IMPORTANTE: </span></p>' +
                  'Si elegiste recolectar tu "pedido en Bodega", <b>s�lo podr�s hacerlo una vez que hayas recibido la factura en tu correo</b>. Recuerda que debes presentar tu factura, de otro modo <b>no podr�s recolectarlo.</b>' 
    l-firma     = (IF AVAILABLE Usuario THEN Usuario.Firma ELSE "").
    
l-entrada = 
    '<div style="font-family:Arial,sans-serif;font-size:14px;line-height:1.6;color:#333;">' +
    'Hola ' + TRIM(Pedido.RazonSocial) + '.<br/><br/>' +
    'Agradecemos tu compra. En este correo encontrar&aacute;s un anexo en el cual puedes consultar a detalle tu pedido.<br/><br/>' +
    'En caso de existir una modificaci&oacute;n o error, te pedimos contactar a ' +
    (IF AVAILABLE Empleado THEN Empleado.Nombre ELSE "tu vendedor") + ' en el correo ' +
    (IF AVAILABLE Usuario AND Usuario.e-Mail <> "" THEN Usuario.e-Mail ELSE "ventas@adosa.com.mx") +
    ' o al tel&eacute;fono 8181581500, en la opci&oacute;n 7.<br/><br/>' +
    '<p><span style="color:red;"><b>IMPORTANTE:</b></span><br/>' +
    'Si elegiste recolectar tu pedido en Bodega, <b>s&oacute;lo podr&aacute;s hacerlo una vez que hayas recibido la factura en tu correo.</b> ' +
    'Recuerda que debes presentar tu factura, de otro modo <b>no podr&aacute;s recolectarlo.</b></p>' +
    '</div>'.
    

IF (Pedido.Id-Entrega = 3 OR Pedido.Id-Entrega = 16) THEN 
DO:
    FIND Almacen WHERE Almacen.Id-Alm = Usuario.Id-Ubicacion NO-LOCK NO-ERROR.
    FIND Ciudad WHERE Ciudad.Id-Ciudad = Almacen.Id-Ciudad NO-LOCK NO-ERROR.
    FIND Estado WHERE Estado.Id-Estado = Almacen.Id-Estado NO-LOCK NO-ERROR.
    FIND Pais WHERE Pais.Id-Pais = Almacen.Id-Pais NO-LOCK NO-ERROR.
END.

ASSIGN
    l-TipoEntrega = Entrega.Descr
    l-CalleEmb    = (IF (Pedido.Id-Entrega = 3 OR Pedido.Id-Entrega = 16)
                     THEN TRIM(Almacen.CalleNo)
                     ELSE TRIM(Pedido.CalleNo1))
    l-ColEmb      = (IF (Pedido.Id-Entrega = 3 OR Pedido.Id-Entrega = 16)
                     THEN TRIM(Almacen.Colonia)
                     ELSE TRIM(Pedido.Colonia1))
    l-CdEmb       = (IF (Pedido.Id-Entrega = 3 OR Pedido.Id-Entrega = 16)
                     THEN TRIM(Ciudad.Nombre) + " " + TRIM(Estado.Nombre) + " " + TRIM(Pais.Nombre) + " " + TRIM(STRING(Almacen.CP))
                     ELSE TRIM(TRIM(Pedido.Ciudad1)) + (IF Pedido.Ciudad1 = "" THEN "" ELSE ', ') + TRIM(TRIM(Pedido.Estado1)) + " " + STRING(Pedido.CP1))
    l-DirEmb      = IF l-CalleEmb <> ""
                    THEN TRIM(l-CalleEmb) + (IF l-CalleEmb = "" THEN "" ELSE ', ') + TRIM(l-ColEmb)
                    ELSE "MISMA DIRECCI�N DE FACTURACI�N".
    
/*    
DISPLAY STREAM s-Salida
    FILL("=",28) FORMAT "x(28)"
    "DATOS DE FACTURACI�N"
    FILL("=",29) FORMAT "x(29)"
    FILL("=",30) FORMAT "x(30)" AT 81
    "DATOS DE EMBARQUE"
    FILL("=",30) FORMAT "x(30)"
    SKIP
    "Cliente        :" Pedido.Id-Cliente Pedido.RazonSocial FORMAT 'x(55)'
    'Tipo de entrega:' AT 81 l-TipoEntrega
    SKIP
    "RFC            :"
    Pedido.RFC
    'Atenci�n a     :' AT 81 Pedido.RazonSocial1
    SKIP
    'Direcci�n      :' TRIM(STRING(Pedido.CalleNo,'X(60)')) + ' ' + TRIM(STRING(Pedido.Colonia,'x(60)')) FORMAT 'x(60)'
    'Direcci�n      :' AT 81 l-DirEmb FORMAT "X(60)"
    SKIP
    'Ciudad y estado:' TRIM(l-Ciudad) + ' ' + TRIM(l-Estado)  + ' ' + Pedido.CP FORMAT 'x(60)'
    'Ciudad y estado:' AT 81 TRIM(l-CdEmb) FORMAT 'x(60)'
    SKIP
    'Delegaci�n     :' l-Delegacion
    'Delegaci�n     :' AT 81 Pedido.Delegacion1
    SKIP
    'Condici�n Vta. :' CondVta.Descr
    SKIP
    FILL("=",79) FORMAT "x(79)"
    FILL("=",79) FORMAT "x(79)" AT 81
    WITH FRAME f-Enca OVERLAY NO-LABEL CENTERED WIDTH 162 NO-BOX.

FOR EACH DetPedido WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido
    AND DetPedido.Resto = Pedido.Resto NO-LOCK BY DetPedido.Reng:
    FIND ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
        AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK NO-ERROR.
    FIND Kolor OF DetPedido NO-LOCK NO-ERROR.

    DISPLAY STREAM s-Salida
        DetPedido.Id-Articulo
        DetPedido.Descr
        Kolor.Abrev       
        WHEN AVAILABLE Kolor
        DetPedido.CantPed COLUMN-LABEL 'Cantidad!Pedida' 
        WHEN DetPedido.CantPed <> 0
        ArtPres.Descr     COLUMN-LABEL 'Present' 
        WHEN AVAILABLE ArtPres
        /*
        DetPedido.Importe WHEN DetPedido.Importe <> 0
        DetPedido.IVA     WHEN DetPedido.IVA <> 0
        (DetPedido.Importe + DetPedido.IVA) (TOTAL) WHEN DetPedido.Importe <> 0 COLUMN-LABEL "Total"
        */
        WITH FRAME f-Imprime DOWN WIDTH 162.
    DOWN STREAM s-Salida WITH FRAME f-Imprime.
END.

OUTPUT STREAM s-Salida CLOSE.
*/
DEFINE VARIABLE cFechaHora AS CHARACTER NO-UNDO.

cFechaHora = STRING(TODAY, "99/99/9999") + " " + STRING(TIME, "hh:mm:ss").

                
DEFINE VARIABLE l-Contenido2 AS LONGCHAR NO-UNDO.
DEFINE VARIABLE l-Detalle    AS LONGCHAR NO-UNDO.  
DEFINE VARIABLE l-Detalle2   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE decTotal     AS DECIMAL   NO-UNDO INITIAL 0.


/* Encabezado del correo */
/* */
ASSIGN 

    l-Contenido2 = '<!DOCTYPE html><html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' +
'<meta name="viewport" content="initial-scale=1.0, width=device-width" />' +
'<meta http-equiv="X-UA-Compatible" content="IE=edge" />' +
'<title>Correo Autorizacion Pedido</title></head>' +
'<body style="margin: 0; padding: 0; background-color: #ffffff">' +
'<!-- Wrapper general -->' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0" style="background-color: #ffffff; padding: 10px 0">' +
'<tr><td align="center">' +
'<!-- Contenedor principal -->' +
'<table width="600" cellpadding="0" cellspacing="0" border="0" style="background-color: #ffffff">' +
'<!-- Cintillo superior -->' +
'<tr>' +
'<td colspan="2" style="background-color: #002278; padding: 5px 10px">' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0">' +
'<tr><td align="left" style="font-family: Arial, sans-serif;font-size: 14px;color: #ffffff;">' +
'<a target="_blank" href="https://wa.me/8181581515" style="text-decoration: none; color: #ffffff">' +
'<img src="https://www.adosa.com.mx/media/wysiwyg/nuevohome/mini-wharsapp.png" alt="WhatsApp" width="16" height="16" style="vertical-align: middle;border: 0;outline: none;text-decoration: none;"/>' +
'<span style="vertical-align: middle; padding-left: 5px">81 8158 1515</span></a>' +
'</td><td align="right" style="font-family: Arial, sans-serif;font-size: 14px;color: #ffffff;">' +
'<a target="_blank" href="https://www.adosa.com.mx/sucursales" style="text-decoration: none; color: #ffffff">Tiendas y sucursales</a>' +
'</td></tr>' +
'</table>' +
'</td>' +
'</tr>' +
'<!-- Header -->' +
'<tr>' +
'<td colspan="2" align="center" style="background-color: #0055b8; padding: 15px 0">' +
'<a target="_blank" href="https://adosa.com.mx" style="display: inline-block; text-decoration: none">' +
'<img src="https://adosa.com.mx/media/email/logo/stores/1/logo.png" alt="Adosa Logo" width="136" style="display: block;border: 0;outline: none;text-decoration: none;"/>' +
'</a>' +
'</td>' +
'</tr>' +
'<!-- Contenido -->' +
'<tr>' +
'<td colspan="2" style="padding: 35px 30px 10px 30px">' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0">' +
'<tr>' +
'<td style="font-family: Arial, sans-serif;font-size: 14px;color: #002278;padding-bottom: 20px;">' +
'<p style="margin: 0 0 30px 0; font-weight: 700;color: #002a77;font-size: 18px;">' + TRIM(Pedido.RazonSocial) + '</p>' +
'<p style="margin: 0 0 25px 0">Gracias por tu pedido en Adosa. Una vez que tu paquete haya sido enviado te mandaremos un numero de rastreo.</p>' +
'<p style="margin: 0 0 25px 0">Si tienes alguna duda sobre tu pedido, envianos un correo electronico a ' +
'<a href="mailto:ventaenlinea@adosa.com.mx" style="color: #333; text-decoration: none">ventaenlinea@adosa.com.mx</a> o llamanos al ' +
'<a href="tel:8181581520" style="color: #333; text-decoration: none">8181581520</a>.</p>' +
'</td>' +
'</tr>' +
'</table>' +
'<!-- Número de pedido y fecha -->' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0">' +
'<tr>' +
'<td style="padding: 20px 30px">' +
'<h1 style="margin: 0;font-family: Arial, sans-serif;font-size: 26px;color: #333;text-align: center;border-bottom: 1px solid #cbd2d6;padding: 10px;font-weight: 500;">' +
'Tu pedido <span style="color: #0093e0; font-weight: 600">' + STRING(l-Pedido) + '</span>' +
'</h1>' +
'<p style="margin: 0;font-family: Arial, sans-serif;font-size: 12px;color: #666666;text-align: center;padding: 10px;">' +
'Colocado el dia: ' + cFechaHora + '</p>' +
'</td>' +
'</tr>' +
'</table>' +
'<!-- Contenedor info cliente y productos + subtotal -->' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0" style="background-color: #f4f5fa; padding: 20px 30px">' +
'<tr>' +
'<td style="padding: 0">' +
'<!-- Info de direccion, facturación y método de pago -->' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0">' +
'<tr>' +
'<td valign="top" style="width: 50%; padding: 35px 25px 10px 25px">' +
'<h3 style="margin: 0 0 10px 0;font-family: Arial, sans-serif;font-size: 14px;color: #002278;">Datos Facturacion</h3>' +
'<p style="margin: 0;font-family: Arial, sans-serif;font-size: 13px;color: #2f4858;line-height: 18px;">' +
STRING(Pedido.Id-Cliente) + ' ' + Pedido.RazonSocial + '<br />' +
'RFC:' + Pedido.RFC + '<br />' +
Pedido.CalleNo + ' ' + Pedido.Colonia + '<br/>' +
l-Ciudad + ' ' + l-Estado + ' ' + STRING(Pedido.CP) + '<br/>' +
'</p>' +
'</td>' +
'<td valign="top" style="width: 50%; padding: 35px 25px 10px 25px">' +
'<h3 style="margin: 0 0 10px 0;font-family: Arial, sans-serif;font-size: 14px;color: #002278;">Datos Embarque</h3>' +
'<p style="margin: 0;font-family: Arial, sans-serif;font-size: 13px;color: #2f4858;line-height: 18px;">' +
Pedido.RFC + '<br />' +
'Atencion a : ' + Pedido.RazonSocial1 + '<br />' +
l-DirEmb + '<br />' +
TRIM(l-CdEmb) + '<br />' +
Pedido.Delegacion1 + '<br />' +
'</p>' +
'</td>' +  
'</tr>' +
'<tr>' +
'<td valign="top" style="width: 50%; padding: 35px 25px 10px 25px">' +
'<h3 style="margin: 0 0 10px 0;font-family: Arial, sans-serif;font-size: 14px;color: #002278;">Condicion Venta:</h3>' +
'<p style="margin: 0;font-family: Arial, sans-serif;font-size: 13px;color: #2f4858;">' + CondVta.Descr + '</p>' +
'</td>' +
'<td valign="top" style="width: 50%; padding: 35px 25px 10px 25px">' +
'<h3 style="margin: 0 0 10px 0;font-family: Arial, sans-serif;font-size: 14px;color: #002278;">Metodo de envio:</h3>' +
'<p style="margin: 0;font-family: Arial, sans-serif;font-size: 13px;color: #2f4858;">' + l-TipoEntrega + '</p>' +
'</td>' +
'</tr>' +
'</table>' +
'<div style="height: 20px; line-height: 20px; font-size: 0">&nbsp;</div>' +
'<!-- Tabla de productos -->' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0" style="border-collapse: collapse">' +
'<thead>' +
'<tr>' +
'<th align="left" style="padding: 10px;font-family: Arial, sans-serif;font-size: 14px;color: #002278;border-bottom: 1px solid #e0e0e0;">Articulos</th>' +
'<th align="center" style="padding: 10px;font-family: Arial, sans-serif;font-size: 14px;color: #002278;border-bottom: 1px solid #e0e0e0;">Cantidad</th>' +
'<th align="right" style="padding: 10px;font-family: Arial, sans-serif;font-size: 14px;color: #002278;border-bottom: 1px solid #e0e0e0;">Precio</th>' +
'</tr>' +
'</thead>' +
'<tbody>' .
  

/* Cuerpo de la tabla */
ASSIGN 
    l-detalle = "".
FOR EACH DetPedido 
    WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido
    AND DetPedido.Resto     = Pedido.Resto
    NO-LOCK BY DetPedido.Reng:

    FIND ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
        AND ArtPres.Id-Pres     = DetPedido.Id-Pres NO-LOCK NO-ERROR.

    FIND Kolor OF DetPedido NO-LOCK NO-ERROR.

    /* Aquí concatenamos una fila del nuevo formato */
    l-detalle = l-detalle +
        '<tr>' +
        /* Columna Artículos con nombre, SKU, presentación y color */
        '<td style="padding: 10px;font-family: Arial, sans-serif;font-size: 14px;color: #2f4858;border-bottom: 1px solid #e0e0e0;">' +
        '<strong style="margin-bottom: 2px; line-height: 1.4;">' + DetPedido.Descr + '</strong><br />' +
        '<span style="font-size: 14px; color: #666666;margin-bottom: 2px; line-height: 1.4;">SKU: ' + DetPedido.Id-Articulo + '</span><br />' +
        '<strong style="font-style: italic;margin-bottom: 2px; line-height: 1.4;">Presentaciones</strong><br />' +
        '<span style="font-size: 14px; color: #666666;margin-bottom: 2px; line-height: 1.4;">' + 
        (IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE '') + '</span>' +
        '</td>' +

        /* Columna Cantidad */
        '<td align="center" style="padding: 10px;font-family: Arial, sans-serif;font-size: 14px;color: #2f4858;border-bottom: 1px solid #e0e0e0;">' +
        STRING(DetPedido.CantPed) +
        '</td>' +

        /* Columna Precio */
        '<td align="right" style="padding: 10px;font-family: Arial, sans-serif;font-size: 14px;color: #2f4858;border-bottom: 1px solid #e0e0e0;font-weight: 700;">' +
        '$' + STRING(DetPedido.Importe + DetPedido.IVA, "->>,>>9.99") +
        '</td>' +
        '</tr>'. 
        
    decTotal =  decTotal + (DetPedido.Importe + DetPedido.IVA).
END.
     
/* Añades el detalle al contenido */
ASSIGN l-Contenido2 = l-Contenido2 + '<tbody>' + l-detalle + '</tbody>'.

ASSIGN l-Contenido2 = l-Contenido2 +       
'<tfoot>' +
'<tr>' + 
'<td colspan="2" align="right" style="padding: 10px;font-family: Arial, sans-serif;font-size: 14px;color: #2f4858;font-weight: 700;">Subtotal</td>' +
'<td align="right" style="padding: 10px;font-family: Arial, sans-serif;font-size: 14px;color: #2f4858;">' + STRING(decTotal, "->>,>>9.99") + '</td>' +
'</tr>' +
'<tr>' +
'<td colspan="2" align="right" style="padding: 10px;font-family: Arial, sans-serif;font-size: 14px;color: #2f4858;font-weight: 700;">Cargos por manejos y envio </td>' +
'<td align="right"style="padding: 10px;font-family: Arial, sans-serif;font-size: 14px;color: #2f4858;">' + STRING(decTotal, "->>,>>9.99") + '</td>' +
'</tr>' +
'<tr style="border-top:1px solid #e0e0e0">' +
'<td colspan="2" align="right" style="padding: 10px;font-family: Arial, sans-serif;font-size: 18px;color: #0d53b1;font-weight: 500;">Total general</td>' +
'<td align="right" style="padding: 10px;font-family: Arial, sans-serif;font-size: 18px;color: #0d53b1;font-weight: 500;">' + STRING(decTotal, "->>,>>9.99") + '</td>' +
'</tr>' +
'</tfoot>' +
'</table>' +
'</td>' +
'</tr>' +
'</table>' +
'<!-- "¿Qué prosigue con mi pedido?" -->' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0" style="margin-top: 30px;border-bottom: 1px solid #cbd2d6;padding-bottom: 50px;"><tr>' +
'<td style="font-family: Arial, sans-serif;font-size: 14px;color: #333333;padding: 0 10px;">' +
'<h2 style="margin: 0 0 10px 0;font-size: 26px;color: #333;text-align: center;font-weight: 500;">¿Que prosigue con mi pedido?</h2>' +
'<ol style="margin: 0 0 20px 40px; padding: 0">' +  
'<li style="margin-bottom: 10px;">Esperar un email de confirmacion de embarque</li>' +
'<li style="margin-bottom: 10px;">Recibir su pedido en un lapso de 3 a 5 dias.</li>' +
'</ol>' +
'<p style="margin: 10px 0 0 0;color: #ff0000;text-align: left;font-size: 18px;">Nota: En pedidos foraneos el envio sera por paqueteria estandar y se proporcionara un numero de guia</p>' +
'</td>' +
'</tr>' +
'</table>' +
'<!-- Botón contacto -->' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0" style="margin-top: 30px">' +
'<tr>' +
'<td align="center" style="padding: 20px 30px">' +
'<p style="margin: 0 0 10px 0;font-family: Arial, sans-serif;font-size: 14px;color: #2f4858;text-align: left;">' +
'<strong style="font-weight: 700;">¿Necesitas ayuda o tienes alguna pregunta?</strong>No dudes en ponerte en contacto con nosotros. Estamos aqui para ayudarte.</p>' +
'<table cellpadding="0" cellspacing="0" border="0" style="margin: 20px auto 0 auto"><tr>' +
'<td align="center" style="background-color: #0080d9;border-radius: 10px;padding: 10px 20px;">' +
'<a href="https://adosa.com.mx/contactanos" target="_blank" style="font-family: Arial, sans-serif;font-size: 14px;color: #ffffff;text-decoration: none;display: block;">CONTACTANOS</a>' +
'</td>' +
'</tr>' +
'</table>' +
'<p style="margin: 10px 0 0 0;font-family: Arial, sans-serif;font-size: 14px;color: #333333;"> ó Llamanos al <a href="tel:8181581520"style="color: #0055b8; text-decoration: none">81 8158 1520</a>' +
'</p>' +
'</td>' +
'</tr>' +
'</table>' +
'</td>' +
'</tr>' +
'<tr>' +
'<td>' +
'<!-- Footer-->' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0" style="margin-top: 20px; background-color: #f9f9f9">' +
'<tr>' +
'<td align="left" style="width: 50%;padding: 15px 0;font-family: Arial, sans-serif;font-size: 14px;color: #333333;text-align: left;">' +
'<a href="https://www.adosa.com.mx/preguntas-frecuentes" target="_blank" style="color: #0055b8;text-decoration: none;margin: 0 10px;">Preguntas frecuentes</a><br/>' +
'<a href="https://www.adosa.com.mx/aviso-de-privacidad" target="_blank" style="color: #0055b8;text-decoration: none;margin: 0 10px;">Aviso de privacidad</a><br/>' +
'<a href="https://www.adosa.com.mx/politicas-de-venta" target="_blank" style="color: #0055b8;text-decoration: none;margin: 0 10px;">Terminos y condiciones</a></td>' +
'<td align="left" style="width: 50%;padding: 15px 0;font-family: Arial, sans-serif;font-size: 14px;color: #333333;text-align: left;">' +
'<strong style="margin: 10px 0 10px 0; color: #004aad">¡Siguenos!</strong><br/>' +
'<table cellpadding="0" cellspacing="0" border="0">' +
'<tr>' +
'<td>' +
'<a target="_blank" href="https://www.facebook.com/AdosaMexico"><img src="https://adosa.com.mx/media/wysiwyg/footer/facebook.png" alt="Facebook" width="40" height="40" style="display: block;border: 0;outline: none;text-decoration: none;margin-right: 10px;"/></a>' +
'</td>' +
'<td>' +
'<a target="_blank" href="https://www.instagram.com/adosamexico"><img src="https://adosa.com.mx/media/wysiwyg/footer/instagram.png" alt="Instagram" width="40" height="40" style="display: block;border: 0;outline: none;text-decoration: none;margin-right: 10px;"/></a>' +
'</td>' +
'<td>' +
'<a target="_blank" href="https://www.tiktok.com/@adosaMexico"><img src="https://adosa.com.mx/media/wysiwyg/footer/ticktok.png" alt="TikTok" width="40" height="40"style="display: block;border: 0;outline: none;text-decoration: none;margin-right: 10px;"/></a>' +
'</td>' +
'</tr>' +
'</table>' +
'</td>' +
'</tr>' +
'</table>' +
'<!-- Termina footer -->' +
'<!-- Copyright ADOSA -->' +
'<table width="100%" cellpadding="0" cellspacing="0" border="0">' +
'<tr>' +
'<td align="left" style="background-color: #002278;padding: 15px 30px;font-family: Arial, sans-serif;font-size: 13px;color: #ffffff;text-align: center;">ABASTECEDORA DE OFICINAS SA DE CV | MEXICO © Todos los derechos reservados.</td>' +
'</tr>' +
'</table>' +
'<!-- Termina copyright ADOSA -->' +
'</td>' +
'</tr>' +
'</table>' +
'<!-- Termina contenido -->' +
'</td>' +
'</tr>' +
'</table>' +
'<!-- Termina contenedor principal -->' +
'</body>' +
'</html>'  .  

/*  
/* Guardar el archivo */
OUTPUT TO "/home/sis10/pedido.html".
PUT UNFORMATTED l-Contenido2.
OUTPUT CLOSE.  
     
OS-COMMAND SILENT VALUE("chmod 777 /home/sis10/pedido.html").

*/
DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
    ASSIGN 
        l-eMail = ''.

    IF Pedido.Compra <> '' THEN 
    DO:
        FIND FIRST CteEmp WHERE CteEmp.Id-Cliente = Pedido.Id-cliente
            AND CteEmp.Nombre BEGINS Pedido.Compra
            AND CteEmp.Nombre <> '' NO-LOCK NO-ERROR.
        IF AVAILABLE CteEmp AND CteEmp.e-Mail <> '' THEN
            ASSIGN l-eMail = CteEmp.e-Mail.
        ELSE
            ASSIGN l-eMail = Pedido.e-Mail.
    END.
    ELSE
        ASSIGN l-eMail = Pedido.e-Mail.
        
    IF l-eMail = "" THEN
        ASSIGN l-eMail = Pedido.BuzonFiscal.

    DO WHILE INDEX(l-eMail,' ') <> 0:
        SUBSTRING(l-eMail,INDEX(l-eMail,' '),1) = ''.
    END.
    
    ASSIGN 
        l-sistemas = "desarrollo10@adosa.com.mx".
   
    IF l-eMail <> '' THEN 
    DO:    
        IF AVAILABLE Usuario THEN                
            ASSIGN
                v-mailde     = Usuario.e-mail + ";" + Usuario.Nom-Usuario
                v-respondera = Usuario.e-mail + ";" + Usuario.Nom-Usuario.
        ELSE 
            ASSIGN
                v-mailde     = "zcorreo@adosa.com.mx;Correo autom�tico ADOSA"
                v-respondera = "zcorreo@adosa.com.mx;Correo autom�tico ADOSA".
        
        /* Asigna el mail de respuesta */
        IF v-respondera <> "" THEN
            ASSIGN
                v-mailde = v-mailde + "^" + v-respondera.
                
        ASSIGN
            v-mailde = v-mailde + CHR(1) + "No,No".
        
         
        l-eMail = l-eMail + ";" + l-sistemas.  
    
    {inva0007.i
            &Asunto     = "l-Asunto"
            &contenido  = "l-Contenido2"
            &Iniciales  = "'SIS10'"
            &Direccion  = "l-eMail"
            &Refer      = "'DIRECTO'"  
            &Attachment = "''"  
      }           
    END.   
END.       
