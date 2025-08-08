@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : FacturaRelacion.p
    Purpose     : 

    Syntax      :/FacturaRelacion

    Description : Servicio el cual ingresan Factura/Remision/Pedido
                   y devuelve de que cliente / RazonSocial pertenece
                   se utiliza para el proceso de Relacion de Documentos CYC.

    Author(s)   : sis10
    Created     : Tue Jun 17 07:32:10 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/
/*

   Integrar que tambien se pueda agregar Acuses
   JASS09072025


*/
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/* Temp-table */
DEFINE TEMP-TABLE ttCliente NO-UNDO
    FIELD IdCliente   AS INTEGER
    FIELD RazonSocial AS CHARACTER.

DEFINE TEMP-TABLE ttRelFacInput NO-UNDO 
    FIELD Factura       AS CHARACTER
    FIELD Tipo          AS INTEGER
    FIELD Observaciones AS CHARACTER.  
    
/* Variables */
DEFINE VARIABLE l-cliente   AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-RazSoc    AS CHARACTER NO-UNDO.




/* Constante de tipos permitidos */
DEFINE VARIABLE l-DescrTipo AS CHARACTER EXTENT 7
    INITIAL ['Factura','Vale','Copia','Cheque','Efectivo','Pedido','Acuse'] NO-UNDO.
DEFINE VARIABLE g-origen    AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Folio     AS CHARACTER NO-UNDO.   

  
    
DEF    BUFFER b-RelFacEnv           FOR RelFacEnv.
DEF    BUFFER b-Usuario             FOR Usuario.    
DEFINE BUFFER b-UsuarioDestinatario FOR Usuario.   
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetFacRelacion:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pFactura   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCliente.
    DEFINE OUTPUT PARAMETER IdError       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER NO-UNDO.



    /* Inicializar salidas */
    IdError      = FALSE.
    Respuesta = "".

    /* Limpiar tabla temporal si ya existiera algo */
    EMPTY TEMP-TABLE ttCliente.

    /* Buscar en Factura */
    FIND Factura 
        WHERE Factura.Id-Factura = pFactura
        AND Factura.FecReg >= (TODAY - 365)
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE Factura THEN 
    DO:
        /* Buscar en Remision */
        FIND Remision 
            WHERE Remision.Id-Remis = pFactura
            AND Remision.FecReg >= (TODAY - 365)
            NO-LOCK NO-ERROR.

        IF NOT AVAILABLE Remision THEN 
        DO:
            /* Buscar en Pedido */
            FIND LAST Pedido 
                WHERE Pedido.Id-Pedido = pFactura
                NO-LOCK NO-ERROR.

            IF NOT AVAILABLE Pedido THEN 
            DO:
            
                /* Buscar en Acuse */
                FIND Acuse
                    WHERE Acuse.Id-Acuse = pFactura
                    NO-LOCK NO-ERROR.

                IF NOT AVAILABLE Acuse THEN 
                DO:
                    ASSIGN
                        IdError   = TRUE
                        Respuesta = "Documento inexistente.".
                    RETURN.
                END.     
            END.
        END.
    END.

    /* Buscar estado del pedido si aplica */
    IF AVAILABLE Factura OR AVAILABLE Remision THEN 
    DO:
        FIND FIRST EstPedido 
            WHERE EstPedido.Id-Factura = pFactura
            NO-LOCK NO-ERROR.
    END.
    ELSE 
    DO:
        FIND EstPedido 
            WHERE EstPedido.Id-Pedido = Pedido.Id-Pedido
            AND EstPedido.Id-Seq = Pedido.Resto
            NO-LOCK NO-ERROR.
    END.

    /* Obtener datos */
    IF AVAILABLE Factura THEN 
    DO:
        ASSIGN
            l-cliente = Factura.Id-Cliente
            l-RazSoc  = Factura.RazonSocial.
    END.
    ELSE IF AVAILABLE Remision THEN 
    DO:
            ASSIGN
                l-cliente = Remision.Id-Cliente
                l-RazSoc  = Remision.RazonSocial.
    END.
    ELSE IF AVAILABLE Pedido THEN 
    DO:
            ASSIGN
                l-cliente = Pedido.Id-Cliente
                l-RazSoc  = Pedido.RazonSocial.
   END.  
   ELSE IF AVAILABLE Acuse THEN DO:
    ASSIGN
        l-cliente = Acuse.Id-Cliente
        l-RazSoc  = "". /* Por si no existe */

    /* Buscar razón social si cliente existe */
    FIND Cliente WHERE Cliente.Id-Cliente = Acuse.Id-Cliente NO-LOCK NO-ERROR.
    IF AVAILABLE Cliente THEN
        ASSIGN l-RazSoc = Cliente.RazonSocial.
   END.    
    /* Crear resultado */
    CREATE ttCliente.
    ASSIGN
        ttCliente.IdCliente   = l-cliente
        ttCliente.RazonSocial = l-RazSoc  
        IdError               = FALSE.
RETURN.
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostGeneraRelacion:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pRemitente    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pDestinatario AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER TABLE FOR ttRelFacInput.
    DEFINE OUTPUT PARAMETER opOK         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opMensaje    AS CHARACTER NO-UNDO.

    /* BASADO EN emba0270.p  */

    /* Buscar al remitente */
    FIND FIRST b-Usuario WHERE b-Usuario.Id-User = pRemitente NO-LOCK NO-ERROR.

    IF NOT AVAILABLE b-Usuario THEN 
    DO:
        ASSIGN
            opOK      = TRUE
            opMensaje = "Remitente no válido. No se encontró el usuario: " + pRemitente.
        RETURN.
    END.
    /* Obtener almacén del remitente */
    ASSIGN 
        g-origen = b-Usuario.id-ubicacion. 

    /* Buscar folio del almacén */
    FIND FIRST Folio 
        WHERE Folio.Id-Doc = "RelFac"
        AND Folio.Id-Alm = b-Usuario.id-ubicacion
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE Folio THEN 
    DO:
        ASSIGN
            opOK      = TRUE
            opMensaje = "El usuario '" + pRemitente + "' no tiene asignado folio para 'RelFac'. Favor de reportar a Sistemas.".
        RETURN.
    END.     


    /* Buscar al Destinatario */
    FIND FIRST b-UsuarioDestinatario WHERE b-UsuarioDestinatario.Id-User = pDestinatario NO-LOCK NO-ERROR.

    IF NOT AVAILABLE b-UsuarioDestinatario THEN 
    DO:
        ASSIGN
            opOK      = TRUE
            opMensaje = "Destinatario no válido. No se encontró el usuario: " + pDestinatario.
        RETURN.
    END.



    ASSIGN
        opMensaje = "".

    /* Generar folio y crear registros */
    DO TRANSACTION:
        FIND Folio WHERE Folio.Id-Doc = "RelFac"
            AND Folio.Id-Alm = g-origen
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE Folio THEN 
        DO:
            ASSIGN 
                opOK      = FALSE
                opMensaje = "Error al bloquear folio.".
            RETURN.
        END. 

        ASSIGN
            l-Folio     = Folio.Prefijo + STRING(Folio.Folio, "999999")
            Folio.Folio = Folio.Folio + 1.

        RELEASE Folio.

        /* Recorrer registros */
        FOR EACH ttRelFacInput:

            CREATE RelFacEnv.
            ASSIGN
                RelFacEnv.FecReg        = TODAY
                RelFacEnv.Id-Factura    = ttRelFacInput.Factura
                RelFacEnv.Id-RelFac     = l-Folio
                RelFacEnv.Tipo          = ttRelFacInput.Tipo
                RelFacEnv.Remitente     = pRemitente
                RelFacEnv.Destinatario  = pDestinatario
                RelFacEnv.Observaciones = ttRelFacInput.Observaciones.
        END. /* EACH */
    END. /* TRANSACTION */

    /* Resultado */
    ASSIGN
        opOK      = FALSE
        opMensaje = "Se genero el Folio: " + l-Folio.
    
    RUN EnviaLiga(INPUT l-Folio, INPUT pRemitente, INPUT pDestinatario, INPUT TABLE ttRelFacInput).  

    /* Limpiar */   
    RELEASE RelFacEnv.   
  
RETURN.
END PROCEDURE.

PROCEDURE EnviaLiga:    
    
    DEFINE INPUT PARAMETER idFolio    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER UserEnvia  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER UserRecibe AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER TABLE FOR ttRelFacInput.

    DEF VAR v-para             AS CHARACTER NO-UNDO.
    DEF VAR v-mailde           AS CHARACTER NO-UNDO.
    DEF VAR l-Asunto           LIKE adosa.e-mail.Asunto NO-UNDO.
    DEF VAR l-contenido        LIKE adosa.e-mail.contenido NO-UNDO.
    DEF VAR v-enviado          AS LOGICAL   NO-UNDO.
    DEF VAR v-respondera       AS CHARACTER NO-UNDO.
    DEF VAR v-html-content     AS CHARACTER NO-UNDO.
    DEF VAR v-html-header      AS CHARACTER NO-UNDO.
    DEF VAR v-html-body        AS CHARACTER NO-UNDO.
    DEF VAR v-html-footer      AS CHARACTER NO-UNDO.
    DEF VAR v-fecha-expiracion AS DATE      NO-UNDO.
    DEF VAR v-datos-pedido     AS CHARACTER NO-UNDO.
    DEF VAR v-monto-formateado AS CHAR      NO-UNDO.
    DEF VAR l-Asunto2           LIKE adosa.e-mail.Asunto NO-UNDO.
    DEF VAR v-para2             AS CHARACTER NO-UNDO.
    DEF VAR l-sistemas          LIKE Pedido.e-Mail NO-UNDO.

    FIND Usuario WHERE Usuario.id-user = UserRecibe NO-LOCK NO-ERROR.
    ASSIGN  
        v-para = usuario.e-mail.

    FIND Usuario WHERE Usuario.id-user = 'CORREO' NO-LOCK NO-ERROR.
    ASSIGN 
        v-mailde = usuario.e-mail + ";" + usuario.nom-usuario.

    FIND Usuario WHERE Usuario.Id-User = UserEnvia NO-LOCK NO-ERROR.

    ASSIGN 
        v-respondera = usuario.e-mail + ";" + usuario.nom-usuario
        v-para2      = usuario.e-mail.

    /* 3. Construir el HTML del correo (compatible con Outlook/Gmail) */
    ASSIGN 
        l-Asunto = "RELACION DE ENVIO DE FACTURAS - Folio #" + STRING(idFolio).

    /* Cabecera HTML */
    ASSIGN 
        v-html-content = '<!DOCTYPE html><html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8">' +
'<meta name="viewport" content="width=device-width, initial-scale=1.0"><title>Liga de Pago</title></head>' +
'<body style="margin:0;padding:0;">' +
'<table width="100%" border="0" cellspacing="0" cellpadding="0" bgcolor="#f7f7f7"><tr><td align="center">' +
'<table width="600" border="0" cellspacing="0" cellpadding="0"><tr><td align="center" style="padding:20px;background:#0066cc;">' +
'<img src="http://anterior.adosa.com.mx/images/AdosaPNG.png" alt="ADOSA" width="200" style="width:200px;height:auto;display:block;border:0;">' +
'<h2 style="margin:10px 0 0 0;color:#ffffff;font-size:20px;">ENV�O DE FACTURAS</h2></td></tr><tr><td style="padding:30px;background:#ffffff;">' +
'<p style="margin:0 0 15px 0;font-size:14px;">Este correo es generado autom�ticamente. Favor de no responder.</p>' +
'<p style="margin:0 0 20px 0;font-size:14px;">Si tiene alguna duda, favor de comunicarse con:</p>' +
'<div style="background:#f5f9ff;padding:15px;margin:0 0 20px 0;border-left:4px solid #0066cc;">' +
'<p style="margin:0 0 5px 0;font-weight:bold;">' + Usuario.Nom-Usuario + '</p>' +
'<p style="margin:0 0 5px 0;">Tel�fono: ' + Usuario.Telefono + '</p>' +
'<p style="margin:0;">Email: ' + Usuario.E-Mail + '</p></div>'.

    /* Datos del pedido */
    ASSIGN 
        v-html-content = v-html-content +
'<h3 style="margin:0 0 15px 0;font-size:16px;color:#0066cc;">Detalles de los documentos</h3>' +
'<table width="100%" border="0" cellspacing="0" cellpadding="0" style="margin:0 0 20px 0;border:1px solid #e0e0e0;">' +
'<tr><td style="padding:10px;border-bottom:1px solid #e0e0e0;font-weight:bold;">No. Folio:</td>' +
'<td style="padding:10px;border-bottom:1px solid #e0e0e0;">' + STRING(idFolio) + '</td></tr>' +
'</table>' +

'<h4 style="margin:15px 0 10px 0;font-size:14px;color:#0066cc;">Documentos relacionados</h4>' +
'<table width="100%" border="1" cellspacing="0" cellpadding="5" style="border-collapse:collapse;margin-bottom:20px;border:1px solid #e0e0e0;">' +
'<thead>' +
'<tr style="background-color:#f5f5f5;">' +
'<th style="padding:8px;text-align:left;border-bottom:2px solid #ddd;">Factura</th>' +
'<th style="padding:8px;text-align:left;border-bottom:2px solid #ddd;">Tipo</th>' +
'<th style="padding:8px;text-align:left;border-bottom:2px solid #ddd;">Observaciones</th>' +
'</tr>' +
'</thead>' +
'<tbody>'.

    /* Recorremos la tabla temporal para agregar las filas */
    FOR EACH ttRelFacInput:
        /* Validamos que el tipo est� dentro del rango del array */
        DEFINE VARIABLE v-tipo-desc AS CHARACTER NO-UNDO.
        v-tipo-desc = IF ttRelFacInput.Tipo >= 1 AND ttRelFacInput.Tipo <= EXTENT(l-DescrTipo) 
            THEN l-DescrTipo[ttRelFacInput.Tipo]
            ELSE "Tipo desconocido (" + STRING(ttRelFacInput.Tipo) + ")".

        ASSIGN 
            v-html-content = v-html-content +
    '<tr>' +
    '<td style="padding:8px;border-bottom:1px solid #ddd;">' + ttRelFacInput.Factura + '</td>' +
    '<td style="padding:8px;border-bottom:1px solid #ddd;">' + v-tipo-desc + '</td>' +
    '<td style="padding:8px;border-bottom:1px solid #ddd;">' + (IF ttRelFacInput.Observaciones > "" THEN ttRelFacInput.Observaciones ELSE "&nbsp;") + '</td>' +
    '</tr>'.
    END.

    /* Pie de p�gina */
    ASSIGN 
        v-html-content = v-html-content +
'</td></tr><tr><td style="padding:15px;text-align:center;font-size:12px;color:#777;background:#f5f5f5;">' +
'<p style="margin:0;">No olvide visitar nuestro sitio <a href="http://www.adosa.com.mx" style="color:#0066cc;">www.adosa.com.mx</a></p>' +
'<p style="margin:10px 0 0 0;">&copy; ' + STRING(YEAR(TODAY)) + ' ADOSA. Todos los derechos reservados.</p>' +
'</td></tr></table></td></tr></table></body></html>'.

    ASSIGN 
        l-Contenido = v-html-content.

   
    /* Asigna el mail de respuesta */
    IF v-respondera <> "" THEN
        ASSIGN
            v-mailde = v-mailde + "^" + v-respondera.

    /* Activa confirmacion de lectura */
    ASSIGN
        v-mailde = v-mailde + CHR(1) + "No,No"
        l-sistemas = "desarrollo10@adosa.com.mx".
        
        v-para   = v-para + ";" + l-sistemas.  
    /* CORREO PARA EL DESTINO */
     {programas/inva0007.i
                &Asunto     = "l-Asunto"
                &contenido  = "l-Contenido"
                &Iniciales  = "'JAGR'"
                &Direccion  = "v-para"
                &Refer      = "'DIRECTO'"
                &Attachment = ""
      }    
      
      /* CORREO DE CONFIRMACION DE ENVIO */ 
      v-para2   = v-para2 + ";" + l-sistemas.  
      l-asunto2 = "CONFIRMACION DE CORREO ENVIADO " + l-Asunto.
      {programas/inva0007.i
                &Asunto     = "l-asunto2"
                &contenido  = "l-contenido"
                &Iniciales  = "'JAGR'"
                &Direccion  = "v-para2"
                &Refer      = "'DIRECTO'"
                &Attachment = ""
      }        
      
    
    /* QUITAMOS EL CORREOO1   
    
    RUN /usr2/adosa/procs/correo01.p (INPUT v-para,
        INPUT v-mailde,
        INPUT "",
        INPUT "", 
        INPUT "", 
        INPUT l-asunto,
        INPUT l-contenido,
        OUTPUT v-enviado).
    
    /* Envio de confirmacion de Correo exitoso o rechazado */
    IF v-enviado THEN 
    DO:
        IF Usuario.e-mail <> "" AND Usuario.e-mail <> v-para THEN 
        DO:
            RUN /usr2/adosa/procs/correo01.p 
                (INPUT Usuario.e-mail,
                INPUT v-mailde,
                INPUT "",
                INPUT "", 
                INPUT "", 
                INPUT "CONFIRMACION DE CORREO   " + l-Asunto, 
                INPUT "ESTA ES UNA CONFIRMACION DE QUE EL CORREO QUE USTED ENVIO A LA DIRECCION  " +
                v-para +
                "  FUE ENTREGADO CON EXITO. <br\><br\>" + 
                "MENSAJE ORIGINAL: <br\><br\>" +
                l-contenido,
                OUTPUT v-enviado).
        END.
    END.
    ELSE 
    DO:
        RUN /usr2/adosa/procs/correo01.p 
            (INPUT Usuario.e-mail,
            INPUT v-mailde,
            INPUT "",
            INPUT "", 
            INPUT "", 
            INPUT "NO SE ENVIO EL CORREO   " + l-Asunto, 
            INPUT l-contenido,
            INPUT "ENVIO FALLIDO A LA DIRECCION " + v-para + "<br\><br\>",
            OUTPUT v-enviado).
    END. 
    
    */
    
RETURN.    
END PROCEDURE.   
