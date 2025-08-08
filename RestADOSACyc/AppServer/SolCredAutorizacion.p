@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : SolCredAutorizacion.p
    Purpose     : Servicios para actualizar y consultar los campos de autorización y revisiones en solicitudes de crédito.
    URL         : /SolCredAutorizacion
    Module      : Gestión de Créditos
------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */



/* Tabla temporal para entrada y salida */
DEFINE TEMP-TABLE ttSolCred NO-UNDO      
    FIELD IdSolicitud            LIKE SolCred.IdSolicitud
    FIELD IdUser                 AS CHAR
    FIELD FecReg                 LIKE SolCred.FecReg  
    FIELD IdEstatus              LIKE SolCred.IdEstatus
    FIELD TipoPersona            LIKE SolCred.TipoPersona
    FIELD CP                     LIKE SolCred.CP
    FIELD Colonia                LIKE SolCred.Colonia
    FIELD Calle                  LIKE SolCred.Calle
    FIELD NumExt                 LIKE SolCred.NumExt
    FIELD NumInt                 LIKE SolCred.NumInt
    FIELD SitioWeb               LIKE SolCred.SitioWeb
    FIELD NumEmpleados           LIKE SolCred.NumEmpleados
    FIELD NumSucursales          LIKE SolCred.NumSucursales
    FIELD DondeRealizaraPago     LIKE SolCred.DondeRealizaraPago
    FIELD LocalPropio            LIKE SolCred.LocalPropio
    FIELD EsAsociado             LIKE SolCred.EsAsociado
    FIELD PronCompraMensual      LIKE SolCred.PronCompraMensual
    FIELD CreditoSol             LIKE SolCred.CreditoSol
    FIELD PlazoSol               LIKE SolCred.PlazoSol
    FIELD Propietario            LIKE SolCred.Propietario
    FIELD CURP                   LIKE SolCred.CURP
    FIELD Correo                 LIKE SolCred.Correo
    FIELD Telefono               LIKE SolCred.Telefono  
    FIELD RFCFiscal              LIKE SolCred.RFCFiscal
    FIELD RazonSocial            LIKE SolCred.RazonSocial
    FIELD RegimenCapital         LIKE SolCred.RegimenCapital
    FIELD RegimenFiscal          LIKE SolCred.RegimenFiscal
    FIELD FormasPago             LIKE SolCred.FormasPago
    FIELD UsoCFDI                LIKE SolCred.UsoCFDI
    FIELD CPFiscal               LIKE SolCred.CPFiscal
    FIELD CapitalSocial          LIKE SolCred.CapitalSocial
    FIELD CapitalContable        LIKE SolCred.CapitalContable
    FIELD RequiereOC             LIKE SolCred.RequiereOC
    FIELD PlataformaFacPago      LIKE SolCred.PlataformaFacPago
    FIELD PortalPlataforma       LIKE SolCred.PortalPlataforma
    FIELD NombreAval             LIKE SolCred.NombreAval
    FIELD ParentescoAval         LIKE SolCred.ParentescoAval
    FIELD CheckDatosCorrectos    LIKE SolCred.CheckDatosCorrectos
    FIELD CheckEnvio             LIKE SolCred.CheckEnvio
    FIELD CheckValMinPed         LIKE SolCred.CheckValMinPed
    FIELD CheckValMinCompMensual LIKE SolCred.CheckValMinCompMensual
    FIELD IdDocumento            LIKE SolCred.IdDocumento /* No va al Inicio */
    FIELD IdClase                LIKE SolCred.IdClase     /* No va al Inicio */
    FIELD CheckRevision1         LIKE SolCred.CheckRevision1 /* No va al Inicio */
    FIELD CheckRevision2         LIKE SolCred.CheckRevision2 /* No va al Inicio */
    FIELD CheckRevision3         LIKE SolCred.CheckRevision3 /* No va al Inicio */
    FIELD CheckRevision4         LIKE SolCred.CheckRevision4 /* No va al Inicio */
    FIELD Estatus                LIKE SolCred.Estatus        /* No va al Inicio */
    FIELD MotivoRechazo          LIKE SolCred.MotivoRechazo /* No va al Inicio */
    FIELD CreditoAutorizado      LIKE SolCred.CreditoAutorizado /* No va al Inicio */
    FIELD PlazoAutorizado        LIKE SolCred.PlazoAutorizado   /* No va al Inicio */
    FIELD Observaciones          LIKE SolCred.Observaciones
    FIELD PagareFirmado          LIKE SolCred.PagareFirmado
    FIELD Vendedor               LIKE SolCred.Vendedor
    FIELD IdCiudad               LIKE SolCred.IdCiudad
    FIELD IdRamo                 LIKE SolCred.IdRamo
    FIELD IdGiro                 LIKE SolCred.IdGiro
    FIELD IdSegmentoCte          LIKE SolCred.IdSegmentoCte
    FIELD IdCalidad              LIKE SolCred.IdCalidad
    FIELD NombreAsociado         LIKE SolCred.NombreAsociado
    FIELD IdCliente              LIKE SolCred.IdCliente 
    FIELD FecAlta                LIKE SolCred.FecAlta   /* No va al Inicio */
    FIELD AltaContado            AS LOGICAL   /* Solo se usa para crear cliente contado si se rechaza */
    FIELD IdCobrador             LIKE Cliente.Id-Cobrador.   
    
/* Tabla temporal para entrada */
DEFINE TEMP-TABLE ttDatosContacto NO-UNDO
    FIELD IdSolicitud  LIKE DatosContacto.IdSolicitud     
    FIELD TipoContacto LIKE DatosContacto.TipoContacto
    FIELD Nombre       LIKE DatosContacto.Nombre
    FIELD Puesto       LIKE DatosContacto.Puesto   
    FIELD Telefono     LIKE DatosContacto.Telefono
    FIELD Correo       LIKE DatosContacto.Correo
    FIELD WhatsApp     LIKE DatosContacto.WhatsApp . /* Incluye todos los campos */
    
/* Tabla temporal para salida */ 
DEFINE TEMP-TABLE ttPrueba NO-UNDO
    LIKE SolCred. /* Incluye todos los campos, incluido FecReg */   
/* Buffer para la tabla persistente */
DEFINE BUFFER bfSolCred       FOR SolCred.   
DEFINE BUFFER bfCliente       FOR Cliente.
DEFINE BUFFER bfCteEmp        FOR CteEmp.
DEFINE BUFFER bfDatosContacto FOR DatosContacto.  

/* Variables internas */
DEFINE VARIABLE lEncontrado    AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iID            AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE l-facturas     AS CHAR    NO-UNDO INITIAL "".
DEFINE VARIABLE l-complemento  AS CHAR    NO-UNDO INITIAL "".
DEFINE VARIABLE l-estadocuenta AS CHAR    NO-UNDO INITIAL "".



/* **********************  Internal Procedures  *********************** */


/* ***************************  Main Procedures **************************** */

/* -------------------------------------------------------------------------- */
/* POST: Servicio para actualizar los campos de autorización y revisiones */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ActualizarAutorizacionRevisiones:
    DEFINE INPUT  PARAMETER TABLE FOR ttSolCred.  /* Tabla temporal de entrada */
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.   
    
     DEFINE VAR l-user AS CHAR.
    /* Procesar cada registro en la tabla temporal */
    FOR EACH ttSolCred:
        /* Validar que la solicitud exista en la tabla SolCred */
        FIND FIRST bfSolCred WHERE bfSolCred.IdSolicitud = ttSolCred.IdSolicitud EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE bfSolCred THEN 
        DO:
            ASSIGN 
                l-Mensaje = "La solicitud con ID " + STRING(ttSolCred.IdSolicitud) + " no existe en la base de datos.".
            RETURN.
        END.
        
        
        /* Validar que el Estatus sea uno de los permitidos */
        IF NOT (ttSolCred.Estatus = "Rechazar" OR ttSolCred.Estatus = "Autorizar" OR ttSolCred.Estatus = "Guardar") THEN 
        DO:
            ASSIGN 
                l-Mensaje = "El Estatus " + ttSolCred.Estatus + " no es válido. Solo se permiten: Rechazar, Autorizar o Guardar.".
            RETURN.
        END.

        /* Validar si el Estatus ya es "Autorizar" (4) */
        IF bfSolCred.Estatus  = "Autorizar" THEN 
        DO:
            ASSIGN 
                l-Mensaje = "La solicitud con ID " + STRING(ttSolCred.IdSolicitud) + " tiene estatus " + bfSolCred.Estatus + " y no puede ser modificada.".
            RETURN.
        END.            
        
        /* Validación de reactivación */
        IF bfSolCred.Estatus = "Rechazar" AND bfSolCred.IdCliente <> 0 THEN 
        DO:
            ASSIGN 
                l-Mensaje = "No procede reactivar la solicitud con ID " + STRING(ttSolCred.IdSolicitud) + 
                          " ya que se creo un cliente de contado con el numero " + STRING(bfSolCred.IdCliente) + ".".
            RETURN.
        END.        
        

        /* Validar campos obligatorios para Autorizar */
        IF ttSolCred.Estatus = "Autorizar" THEN 
        DO:
            IF NOT (ttSolCred.CheckRevision1 AND ttSolCred.CheckRevision2 AND ttSolCred.CheckRevision3 AND ttSolCred.CheckRevision4) THEN 
            DO:
                ASSIGN 
                    l-Mensaje = "La solicitud con ID " + STRING(ttSolCred.IdSolicitud) + " no puede ser autorizada porque no todas las revisiones están completas.".
                RETURN.
            END.
            IF ttSolCred.CreditoAutorizado = 0 OR ttSolCred.PlazoAutorizado = 0 THEN 
            DO:
                ASSIGN 
                    l-Mensaje = "La solicitud con ID " + STRING(ttSolCred.IdSolicitud) + " no puede ser autorizada sin un crédito y plazo autorizados válidos.".
                RETURN.
            END. 
            IF ttSolCred.IdUser = "" THEN
            DO:
                 ASSIGN 
                    l-Mensaje = "La solicitud con ID " + STRING(ttSolCred.IdSolicitud) + " no puede ser autorizada falta IdUser .".
                RETURN.
            END. 
            
            IF ttSolCred.IdUser <> "" THEN
            DO:
                ASSIGN l-user = ttSolCred.IdUser.
            END.
        END.    

        /* Actualizar los campos en la tabla persistente */
        ASSIGN
            bfSolCred.CreditoAutorizado = ttSolCred.CreditoAutorizado
            bfSolCred.PlazoAutorizado   = ttSolCred.PlazoAutorizado
            bfSolCred.CheckRevision1    = ttSolCred.CheckRevision1
            bfSolCred.CheckRevision2    = ttSolCred.CheckRevision2
            bfSolCred.CheckRevision3    = ttSolCred.CheckRevision3
            bfSolCred.CheckRevision4    = ttSolCred.CheckRevision4
            bfSolCred.Estatus           = ttSolCred.Estatus
            bfSolCred.MotivoRechazo     = ttSolCred.MotivoRechazo
            bfSolCred.IdClase           = ttSolCred.IdClase.    

        /* Actualizar IdEstatus basado en Estatus */
        CASE ttSolCred.Estatus:
            WHEN "Rechazar"  THEN 
                DO:
                    bfSolCred.IdEstatus = 4.
                    bfSolCred.FecAlta   = TODAY.
                    bfSolCred.CreditoAutorizado = 0.
                    bfSolCred.PlazoAutorizado   = 0.
                END.
            WHEN "Autorizar" THEN 
                DO: 
                    bfSolCred.IdEstatus = 3. 
                    bfSolCred.FecAlta   = TODAY.
                END.
            WHEN "Guardar"   THEN 
                DO:
                    bfSolCred.IdEstatus = 2.  
                END.
        END.

        RELEASE bfSolCred.
    END.

    /* Llamar a GenerarCliente condicionalmente según el estatus y CrearCliente */
    FIND FIRST ttSolCred NO-LOCK NO-ERROR.
    IF AVAILABLE ttSolCred THEN 
    DO:
        FIND FIRST bfSolCred WHERE bfSolCred.IdSolicitud = ttSolCred.IdSolicitud
            AND (bfSolCred.IdEstatus = 3 OR bfSolCred.IdEstatus = 4) NO-LOCK NO-ERROR.
        IF AVAILABLE bfSolCred THEN 
        DO:
            CASE bfSolCred.IdEstatus:
                WHEN 3 THEN 
                RUN GenerarCliente (INPUT bfSolCred.IdSolicitud, INPUT ttSolCred.IdCalidad,INPUT l-user,OUTPUT l-Mensaje).
                WHEN 4 THEN 
                    IF ttSolCred.AltaContado THEN /* Envian si se crea como contado o no al rechazar */
                        RUN GenerarCliente (INPUT bfSolCred.IdSolicitud,INPUT ttSolCred.IdCalidad,INPUT l-user, OUTPUT l-Mensaje).
                    ELSE 
                        ASSIGN l-Mensaje = "Solicitud rechazada. Cliente no creado.".
            END CASE.
        END.
    END.      
RETURN. 
END PROCEDURE.           

PROCEDURE GenerarCliente:  
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER IdSolicitud AS INT.
    DEFINE INPUT PARAMETER IdCalidad   AS INT.
    DEFINE INPUT PARAMETER IdUser      AS CHAR.
    DEFINE OUTPUT PARAMETER l-Mensaje  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-Responsable LIKE Cliente.Id-Resp.

    EMPTY TEMP-TABLE ttSolCred.

    /* Buscar la solicitud */
    FIND FIRST SolCred WHERE SolCred.IdSolicitud = IdSolicitud NO-LOCK NO-ERROR.
    IF NOT AVAILABLE SolCred THEN 
    DO:
        ASSIGN 
            l-Mensaje = "Error: Solicitud no encontrada con el ID " + STRING(IdSolicitud) + ".".
        RETURN.
    END.
    
    IF IdUser = "" THEN DO:
        ASSIGN 
            l-Mensaje = "Error: Enviar Usuario que Realiza Autorizacion de ID " + STRING(IdSolicitud) + ".".
        RETURN. 
    END.
    
    FOR EACH DatosContacto WHERE DatosContacto.IdSolicitud = IdSolicitud 
                            AND (DatosContacto.TipoContacto = "Envío de Facturas" 
                                 OR DatosContacto.TipoContacto = "Envío de complemento" 
                                 OR DatosContacto.TipoContacto = "Envío de Estado de cuenta")
                            NO-LOCK :

    
        /* Verificamos si el campo correo no está vacío */
        IF DatosContacto.correo <> "" THEN 
        DO:
            IF DatosContacto.TipoContacto = "Envío de Facturas" THEN 
            DO:
                IF l-facturas <> "" THEN
                    l-facturas = l-facturas + ";" + DatosContacto.correo.
                ELSE
                    l-facturas = DatosContacto.correo.
            END.
            IF DatosContacto.TipoContacto = "Envío de complemento" THEN 
            DO:
                IF l-complemento <> "" THEN
                    l-complemento = l-complemento + ";" + DatosContacto.correo.
                ELSE
                    l-complemento = DatosContacto.correo.
            END.
            /* Para el tipo "Envío de Estado de cuenta" */
            IF DatosContacto.TipoContacto = "Envío de Estado de cuenta" THEN 
            DO:
                IF l-estadocuenta <> "" THEN
                    l-estadocuenta = l-estadocuenta + ";" + DatosContacto.correo.
                ELSE
                    l-estadocuenta = DatosContacto.correo.
            END.
        END. 
    END. /* Fin del FOR EACH */          
         

    /* Copiar datos a la tabla temporal `ttSolCred` */
    CREATE ttSolCred.
    BUFFER-COPY SolCred TO ttSolCred.
        
    /* Iterar sobre las solicitudes de entrada */
    FOR EACH ttSolCred :
        
        /* Validar Razon Social duplicada */
        FIND FIRST Cliente WHERE Cliente.RazonSocial = ttSolCred.RazonSocial NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Cliente ya registrado con la razón social: " + ttSolCred.RazonSocial.
            RETURN.
        END.

        /* Validar Email duplicado */
        FIND FIRST Cliente WHERE Cliente.e-mail = ttSolCred.Correo NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Cliente ya registrado con el email: " + ttSolCred.Correo.
            RETURN.
        END.
        
        /* Validar RFC duplicado */   
        FIND FIRST Cliente WHERE Cliente.RFC = ttSolCred.RFCFiscal NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Cliente ya registrado con el RFC: " + ttSolCred.RFCFiscal.
            RETURN.
        END.
        
         // Valida Ciudad
        IF ttSolCred.IdCiudad > 0 THEN    
        DO:
            FIND Ciudad WHERE Ciudad.Id-Ciudad = ttSolCred.IdCiudad NO-LOCK NO-ERROR.

    // Validar si el registro de Ciudad fue encontrado
            IF AVAILABLE Ciudad THEN 
            DO:
        
                IF ttSolCred.IdGiro = 45 THEN /* En Contado, aquí pide también Cobrador 10 */
                    ASSIGN 
                        l-Responsable = 30.
                ELSE 
                DO:
                    IF Ciudad.Id-Estado = "019" AND Ciudad.CveZona = 1 THEN 
                        ASSIGN 
                            l-Responsable = 33.
                    ELSE 
                        ASSIGN 
                            l-Responsable = 9.
                END.
        
            END.
        END.
        /* Se debe agregar la clase antes de Autorizar
           Si no, entra esta regla segun el Responsable */ 
        IF ttSolCred.IdClase = 0 THEN
        DO:
            /* Asignación de IdClase según el valor de l-Responsable */
            IF l-Responsable = 33 THEN 
                ASSIGN ttSolCred.IdClase = 1. /* LOCAL */
            ELSE IF l-Responsable = 9 THEN 
                    ASSIGN ttSolCred.IdClase = 2. /* FORANEO */
                ELSE IF l-Responsable = 30 THEN 
                        ASSIGN ttSolCred.IdClase = 3. /* CORPORATIVO */
                    ELSE 
                        /* Opcional: Un valor predeterminado en caso de que l-Responsable no coincida */
                        ASSIGN ttSolCred.IdClase = 0. /* o cualquier otro valor predeterminado */
        END.

        
        
        /* Generar un ID único */  
        DO TRANSACTION:
            DO iID = 10000 TO 90000:  
                FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = iID NO-LOCK NO-ERROR.
                IF NOT AVAILABLE bfCliente THEN 
                DO:
                    CREATE bfCliente.
                    ASSIGN 
                        bfCliente.Id-Cliente         = iID
                        bfCliente.Id-ClaseCte        = ttSolCred.IdClase
                        bfCliente.Id-Resp            = l-Responsable
                        bfCliente.FecReg             = TODAY
                        bfCliente.Activo             = TRUE
                        bfCliente.Tipo               = IF LOWER(ttSolCred.TipoPersona) BEGINS "f" THEN 1 ELSE 2
                        bfCliente.CP                 = ttSolCred.CP  
                        bfCliente.Colonia            = ttSolCred.Colonia
                        bfCliente.Calle              = ttSolCred.Calle 
                        bfCliente.NumExt             = ttSolCred.NumExt  
                        bfCliente.NumInt             = ttSolCred.NumInt
                        bfCliente.CalleNo            = ttSolCred.Calle + " " + ttSolCred.NumInt
                        bfCliente.SitioWeb           = ttSolCred.SitioWeb    //Nuevo
                        bfCliente.NumEmpleados       = ttSolCred.NumEmpleados  //Nuevo
                        bfCliente.NumSucursales      = ttSolCred.NumSucursales //Nuevo
                        bfCliente.DondeRealizaraPago = ttSolCred.DondeRealizaraPago //Nuevo
                        bfCliente.LocalPropio        = ttSolCred.LocalPropio //Nuevo
                        bfCliente.EsAsociado         = ttSolCred.EsAsociado  //Nuevo
                        bfCliente.PronCompraMensual  = ttSolCred.PronCompraMensual  //Nuevo
                    //    bfCliente.CreditoSol              = ttSolCred.CreditoSol
                    //    bfCliente.PlazoSol                = ttSolCred.PlazoSol
                        bfCliente.Propietario        = ttSolCred.Propietario
                        bfCliente.curp               = ttSolCred.CURP
                        bfCliente.e-mail             = ttSolCred.Correo
                      //  bfCliente.BuzonFiscal        = ttSolCred.Correo
                        bfCliente.Tel1               = ttSolCred.Telefono
                        bfCliente.RFC                = ttSolCred.RFCFiscal
                        bfCliente.RazonSocial        = ttSolCred.RazonSocial
                        bfCliente.RSocietario        = ttSolCred.RegimenCapital /* Rsocietario sa de cv */
                        bfCliente.Id-RFiscal         = ttSolCred.RegimenFiscal /* Valida Catalogo */
                        bfCliente.FEFormaPago        = ttSolCred.FormasPago /* Valida Catalogo */
                        bfCliente.Id-UsoCFDI         = ttSolCred.UsoCFDI /* Valida Catalogo */
                        bfCliente.CP                 = ttSolCred.CPFiscal  /* SOLO HAY UN CAMPO DE CODIGO POSTAL EN CLIENTE */ 
                        bfCliente.CapitalSocial      = ttSolCred.CapitalSocial     //Nuevo    
                        bfCliente.CapitalContable    = ttSolCred.CapitalContable  //Nuevo
                        bfCliente.OCyes              = ttSolCred.RequiereOC   
                        bfCliente.PlataformaFacPago  = ttSolCred.PlataformaFacPago
                        bfCliente.PortalPlataforma   = ttSolCred.PortalPlataforma
                        bfCliente.NombreAval         = ttSolCred.NombreAval
                        bfCliente.ParentescoAval     = ttSolCred.ParentescoAval
                      //  bfCliente.CheckDatosCorrectos     = ttSolCred.CheckDatosCorrectos
                      //  bfCliente.CheckEnvio              = ttSolCred.CheckEnvio
                      //  bfCliente.CheckValMinPed          = ttSolCred.CheckValMinPed
                      //  bfCliente.CheckValMinCompMensual  = ttSolCred.CheckValMinCompMensual
                        bfCliente.Id-Documento       = ttSolCred.IdDocumento
                        bfCliente.Observaciones      = ttSolCred.Observaciones  // Esto no se envia al crear
                        bfCliente.PagareFirmado      = ttSolCred.PagareFirmado  // Esto no se envia al crear
                        bfCliente.Id-Ciudad          = ttSolCred.IdCiudad /* Valida Catalogo */
                        bfCliente.Id-Ramo            = ttSolCred.IdRamo   /* Valida Catalogo */
                        bfCliente.Id-Giro            = ttSolCred.IdGiro   /* Valida Catalogo */
                        bfCliente.Id-SegmentoCte     = ttSolCred.IdSegmentoCte /* Valida Catalogo */
                        bfCliente.NombreAsociado     = ttSolCred.NombreAsociado /* Valida Catalogo */
                        bfCliente.Limite             = ttSolCred.CreditoAutorizado   
                        bfCliente.Plazo              = ttSolCred.PlazoAutorizado 
                        bfCliente.Id-Cobrador        = ttSolCred.IdCobrador // lo agregan en creditoplazo
                        bfCliente.Id-Calidad         = IdCalidad
                        bfCliente.BuzonFiscal        = l-facturas
                        bfCliente.CPBuzonFiscal      = l-complemento
                        bfCliente.ECBuzon            = l-estadocuenta.       
                      
                    /* Actualizar la tabla SolCred con el nuevo IdCliente */
                    FIND FIRST SolCred WHERE SolCred.IdSolicitud = IdSolicitud EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE SolCred THEN 
                    DO:
                        ASSIGN 
                            SolCred.IdCliente = iID  
                            SolCred.IdClase   = ttSolCred.IdClase.
                    END.
                    FOR EACH DocReq WHERE DocReq.IdSolicitud = IdSolicitud EXCLUSIVE-LOCK :
                        ASSIGN 
                            DocReq.IdCliente = iID.
                    END. 
                    FOR EACH ReferProv WHERE ReferProv.IdSolicitud = IdSolicitud EXCLUSIVE-LOCK:
                        ASSIGN 
                            ReferProv.IdCliente = iID.
                    END.
                    FOR EACH ReferComercial WHERE ReferComercial.IdSolicitud = IdSolicitud EXCLUSIVE-LOCK:
                        ASSIGN 
                            ReferComercial.IdCliente = iID.
                    END.
                    FOR EACH DatosContacto WHERE DatosContacto.IdSolicitud = IdSolicitud EXCLUSIVE-LOCK:
                        ASSIGN 
                            DatosContacto.IdCliente = iID.
                        CREATE bfCteEmp.
                        ASSIGN 
                            bfCteEmp.Id-Cliente   = iID
                            bfCteEmp.e-mail       = DatosContacto.Correo
                            bfCteEmp.Nombre       = DatosContacto.Nombre
                            bfCteEmp.Puesto       = DatosContacto.Puesto
                            bfCteEmp.Tel1         = DatosContacto.Telefono
                            bfCteEmp.TipoContacto = DatosContacto.TipoContacto
                            bfCteEmp.WhatsApp     = DatosContacto.WhatsApp.
                    END.  
                    RUN GuardaBitacora(INPUT "NUEVO",INPUT IdUser).          
                    ASSIGN 
                        l-Mensaje = "Cliente Creado con el Numero :" + STRING(iID). 
                    LEAVE.
                END.       
            END.           
        END.  
    END.        
END PROCEDURE.

PROCEDURE GuardaBitacora.
    DEFINE INPUT PARAMETER l-Tipo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER IdUser AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-Campo      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-ValorNuevo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-ValorOld   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE l-NumCampo AS INTEGER NO-UNDO.
    DEFINE VARIABLE l-j        AS INTEGER NO-UNDO.
    
    IF l-Tipo = "NUEVO" THEN DO:
        DO l-j = 1 TO 21:
            CASE l-j:
                WHEN 1 THEN
                    ASSIGN 
                        l-Campo = "Tipo"
                        l-ValorNuevo = IF LOWER(ttSolCred.TipoPersona) BEGINS "f" THEN "1" ELSE "2" 
                        l-ValorOld = ""
                        l-NumCampo = 2.
                WHEN 2 THEN
                    ASSIGN 
                        l-Campo = "RFC"
                        l-ValorNuevo = ttSolCred.RFCFiscal
                        l-ValorOld = ""
                        l-NumCampo = 4.
                WHEN 3 THEN
                    ASSIGN 
                        l-Campo = "CURP"
                        l-ValorNuevo = ttSolCred.CURP
                        l-ValorOld = ""
                        l-NumCampo = 54.
                WHEN 4 THEN
                    ASSIGN 
                        l-Campo = "RazonSocial"
                        l-ValorNuevo = ttSolCred.RazonSocial
                        l-ValorOld = ""
                        l-NumCampo = 1.

                WHEN 5 THEN
                    ASSIGN 
                        l-Campo = "Propietario"
                        l-ValorNuevo = ttSolCred.Propietario
                        l-ValorOld = ""
                        l-NumCampo = 3.
                WHEN 6 THEN
                    ASSIGN 
                        l-Campo = "CP"
                        l-ValorNuevo = ttSolCred.CP
                        l-ValorOld = ""
                        l-NumCampo = 8.
                WHEN 7 THEN
                    ASSIGN 
                        l-Campo = "Calle"
                        l-ValorNuevo = ttSolCred.Calle 
                        l-ValorOld = ""
                        l-NumCampo = 5.
                WHEN 8 THEN
                    ASSIGN 
                        l-Campo = "NumExt"
                        l-ValorNuevo = ttSolCred.NumExt
                        l-ValorOld = ""
                        l-NumCampo = 100.
                WHEN 9 THEN
                    ASSIGN 
                        l-Campo = "NumInt"
                        l-ValorNuevo = ttSolCred.NumInt
                        l-ValorOld = ""
                        l-NumCampo = 110.

                WHEN 10 THEN
                    ASSIGN 
                        l-Campo = "Colonia"
                        l-ValorNuevo = ttSolCred.Colonia
                        l-ValorOld = ""
                        l-NumCampo = 6.
                WHEN 11 THEN
                    ASSIGN 
                        l-Campo = "Id-Ciudad"
                        l-ValorNuevo = STRING(ttSolCred.IdCiudad)
                        l-ValorOld = ""
                        l-NumCampo = 7.
                WHEN 12 THEN
                    ASSIGN 
                        l-Campo = "Tel1"
                        l-ValorNuevo = ttSolCred.Telefono
                        l-ValorOld = ""
                        l-NumCampo = 10.

                WHEN 13 THEN
                    ASSIGN 
                        l-Campo = "Id-RFiscal"
                        l-ValorNuevo = ttSolCred.RegimenFiscal
                        l-ValorOld = ""
                        l-NumCampo = 14.
                WHEN 14 THEN
                    ASSIGN 
                        l-Campo = "Id-Ramo"
                        l-ValorNuevo = STRING(ttSolCred.IdRamo)
                        l-ValorOld = ""
                        l-NumCampo = 34.
                WHEN 15 THEN
                    ASSIGN 
                        l-Campo = "Id-Giro"
                        l-ValorNuevo = STRING(ttSolCred.IdGiro)
                        l-ValorOld = ""
                        l-NumCampo = 33.
  
                WHEN 16 THEN
                    ASSIGN 
                        l-Campo = "Id-UsoCFDI"
                        l-ValorNuevo = ttSolCred.UsoCFDI
                        l-ValorOld = ""
                        l-NumCampo = 19.
                WHEN 17 THEN
                    ASSIGN 
                        l-Campo = "e-Mail"
                        l-ValorNuevo = ttSolCred.Correo
                        l-ValorOld = ""
                        l-NumCampo = 48.
                WHEN 18 THEN
                    ASSIGN 
                        l-Campo = "BuzonFiscal"
                        l-ValorNuevo = l-facturas
                        l-ValorOld = ""
                        l-NumCampo = 24.

                WHEN 19 THEN
                    ASSIGN 
                        l-Campo = "RSocietario"
                        l-ValorNuevo = ttSolCred.RegimenCapital
                        l-ValorOld = ""
                        l-NumCampo = 57.
                        
                 WHEN 20 THEN
                    ASSIGN 
                        l-Campo = "limite"
                        l-ValorNuevo = STRING(ttSolCred.CreditoAutorizado)
                        l-ValorOld = ""
                        l-NumCampo = 500.  
                   
                   WHEN 21 THEN
                    ASSIGN 
                        l-Campo = "Plazo"
                        l-ValorNuevo = STRING(ttSolCred.PlazoAutorizado)
                        l-ValorOld = ""
                        l-NumCampo = 501.     
            END CASE.
            CREATE CambioCte.
            ASSIGN
                CambioCte.Id-Cliente = iID
                CambioCte.Id-User    = IdUser
                CambioCte.Descr      = l-Campo
                CambioCte.ValorNuevo = l-ValorNuevo
                CambioCte.ValorOld   = l-ValorOld
                CambioCte.FecReg     = TODAY
                CambioCte.Hora       = TIME
                CambioCte.Campo      = l-NumCampo.
        END. 
    END.
    RELEASE CambioCte.  
END PROCEDURE.