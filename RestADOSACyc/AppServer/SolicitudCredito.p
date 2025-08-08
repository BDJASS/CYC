@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*--------------------------------------------------------------------------  
 File        : ClienteCredito.p
 URL         : /ClienteCredito
 Module      : Gestión de Créditos - Registro de Solicitud de Créditos
 Sprint      : Sprint 3, Noviembre 2024
 Purpose     : Proceso para generar y consultar solicitudes de crédito.

 Description : 
    - **POST**: Permite registrar solicitudes de crédito asignando un ID único.
    - **GET**: Permite consultar una solicitud específica por su ID.
    
 Notes       : 
    - Este programa está diseñado para operar en un servicio REST.
    - Autor del Sprint: Usuario jsegura.
--------------------------------------------------------------------------*/

/* ***************************  Definitions ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.     

/* Tabla temporal para entrada y salida */
DEFINE TEMP-TABLE ttSolCred NO-UNDO   
    FIELD IdSolicitud            LIKE SolCred.IdSolicitud
    FIELD IdUser                 LIKE Cliente.Id-User
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
    FIELD NombreAsociado         LIKE SolCred.NombreAsociado   /* No va al Inicio */
    FIELD IdPais                 LIKE Pais.Id-Pais   // Se envia para el Get 
    FIELD IdEstado               LIKE Estado.Id-Estado . // Se envia para el Get.    
       
    
/* Tabla temporal para entrada */
DEFINE TEMP-TABLE ttDatosContacto NO-UNDO
    FIELD IdSolicitud  LIKE DatosContacto.IdSolicitud     
    FIELD TipoContacto LIKE DatosContacto.TipoContacto
    FIELD Nombre       LIKE DatosContacto.Nombre
    FIELD Puesto       LIKE DatosContacto.Puesto   
    FIELD Telefono     LIKE DatosContacto.Telefono
    FIELD Correo       LIKE DatosContacto.Correo
    FIELD WhatsApp     LIKE DatosContacto.WhatsApp 
    FIELD IdCliente    LIKE DatosContacto.IdCliente. 
    

DEFINE TEMP-TABLE ttReferProv NO-UNDO
    FIELD TipoProv       LIKE ReferProv.TipoProv
    FIELD Num            LIKE ReferProv.Num
    FIELD NombreContacto LIKE ReferProv.NombreContacto
    FIELD Telefono       LIKE ReferProv.Telefono
    FIELD Celular        LIKE ReferProv.Celular
    FIELD Correo         LIKE ReferProv.Correo
    FIELD IdSolicitud    LIKE ReferProv.IdSolicitud
    FIELD Revisada       LIKE ReferProv.Revisada
    FIELD IdCliente      LIKE ReferProv.IdCliente
    FIELD Observaciones  LIKE ReferProv.Observaciones.      

DEFINE TEMP-TABLE ttReferComercial NO-UNDO
    FIELD IdSolicitud        LIKE ReferComercial.IdSolicitud
    FIELD Num                LIKE ReferComercial.Num
    FIELD TiempoTrabajo      LIKE ReferComercial.TiempoTrabajo
    FIELD LimiteCred         LIKE ReferComercial.LimiteCred
    FIELD CondPago           LIKE ReferComercial.CondPago
    FIELD DemoraPagos        LIKE ReferComercial.DemoraPagos
    FIELD DiasAtraso         LIKE ReferComercial.DiasAtraso
    FIELD ConsumoPromMensual LIKE ReferComercial.ConsumoPromMensual
    FIELD FormaPago          LIKE ReferComercial.FormaPago
    FIELD ChequeDev          LIKE ReferComercial.ChequeDev
    FIELD OrdenCompra        LIKE ReferComercial.OrdenCompra
    FIELD GarantiaPagare     LIKE ReferComercial.GarantiaPagare
    FIELD FecUltCompra       LIKE ReferComercial.FecUltCompra
    FIELD SaldoActualVig     LIKE ReferComercial.SaldoActualVig
    FIELD ConsideracionCte   LIKE ReferComercial.ConsideracionCte
    FIELD IdCliente          LIKE ReferComercial.IdCliente.

DEFINE TEMP-TABLE ttDocReq NO-UNDO
    FIELD IdSolicitud   LIKE DocReq.IdSolicitud
    FIELD TipoDoc       LIKE DocReq.TipoDoc
    FIELD NombreArchivo LIKE DocReq.NombreArchivo
    FIELD PDF           LIKE DocReq.PDF
    FIELD IdCliente     LIKE DocReq.IdCliente
    FIELD DocCargado    LIKE DocReq.DocCargado.      
    
DEFINE DATASET dsSolicitud FOR 
    ttSolCred, /* Tabla principal */
    ttDatosContacto, /* Datos de Contacto */
    ttReferProv,
    ttReferComercial,
    ttDocReq
    DATA-RELATION SolicitudContacto FOR ttSolCred, ttDatosContacto
    RELATION-FIELDS (IdSolicitud, IdSolicitud) /* Relación Solicitud-DatosContacto por IdSolicitud */
    DATA-RELATION SolicitudReferProv FOR ttSolCred, ttReferProv
    RELATION-FIELDS (IdSolicitud, IdSolicitud) /* Relación Solicitud-ReferProv por IdSolicitud */ 
    DATA-RELATION SolicitudReferComercial FOR ttSolCred, ttReferComercial
    RELATION-FIELDS (IdSolicitud, IdSolicitud) /* Relación Solicitud-ReferComercial por IdSolicitud */  
    DATA-RELATION SolicitudReferProv FOR ttSolCred, ttDocReq
    RELATION-FIELDS (IdDocumento, IdSolicitud). /* Relación Solicitud-DocReq por IdSolicitud */  
          
       

/* Tabla temporal para salida */ 
DEFINE TEMP-TABLE ttPrueba NO-UNDO
    LIKE SolCred. /* Incluye todos los campos, incluido FecReg */

/* Buffer para trabajar con la tabla persistente */
DEFINE BUFFER bfSolCred FOR SolCred.
DEFINE BUFFER bfSol     FOR SolCred.
DEFINE BUFFER bDocReq   FOR DocReq.

/* Variables */
DEFINE VARIABLE totalProcesado AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE errores        AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE lNuevoFolio    AS INTEGER NO-UNDO. /* Variable para generar nuevo folio */
DEFINE VARIABLE FolioFinal     AS INTEGER NO-UNDO.

/* **********************  Internal Procedures  *********************** */


/* ***************************  Main Block **************************** */


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE AltaSolicitud:
    DEFINE INPUT PARAMETER DATASET FOR dsSolicitud.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.  


    /* Variables internas */
    DEFINE VARIABLE lEncontrado AS LOGICAL NO-UNDO INITIAL FALSE.
    DEFINE VARIABLE iID         AS INTEGER NO-UNDO INITIAL 0.

    /* Si no hay registros en la tabla temporal principal, devolver mensaje */
    IF NOT CAN-FIND(FIRST ttSolCred) THEN 
    DO:
        ASSIGN 
            l-Mensaje = "No se enviaron datos de la solicitud para procesar.".
        RETURN.
    END.

    /* Validación de datos mínimos requeridos */
    FIND FIRST ttDatosContacto NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttDatosContacto THEN 
    DO:
        ASSIGN 
            l-Mensaje = "La solicitud de crédito no contiene datos de contacto.".
        RETURN.
    END.

    FIND FIRST ttReferProv NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttReferProv THEN 
    DO:
        ASSIGN 
            l-Mensaje = "La solicitud de crédito no contiene referencias de proveedores.".
        RETURN.
    END.

    FIND FIRST ttReferComercial NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttReferComercial THEN 
    DO:
        ASSIGN 
            l-Mensaje = "La solicitud de crédito no contiene referencias comerciales.".
        RETURN.
    END.

    FIND FIRST ttDocReq NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttDocReq THEN 
    DO:
        ASSIGN 
            l-Mensaje = "La solicitud de crédito no contiene documentos adjuntos.".
        RETURN.
    END.
    
    /* Validación y creación del cliente */
    FOR EACH ttSolCred EXCLUSIVE-LOCK:
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
        FIND FIRST Cliente WHERE Cliente.RFC = ttSolCred.RFC NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Cliente ya registrado con el RFC: " + ttSolCred.RFC.
            RETURN.
        END.
        
        /* Validar Asociado */
        IF ttSolCred.NombreAsociado <> 0 THEN    
        DO:
            FIND FIRST Cliente WHERE Cliente.Id-Cliente = ttSolCred.NombreAsociado NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Cliente THEN 
            DO:
                ASSIGN 
                    l-Mensaje = "Asociado Invalido : " + STRING(ttSolCred.NombreAsociado).
                RETURN.
            END.
        END.
        
       
        
    END.   
    

    /* Iterar sobre las solicitudes de entrada */
    FOR EACH ttSolCred WHERE ttSolCred.IdSolicitud = 0:
        /* Generar un ID único */
        DO TRANSACTION:
            DO iID = 1 TO 90000:
                FIND FIRST bfSolCred WHERE bfSolCred.IdSolicitud = iID NO-LOCK NO-ERROR.
                IF NOT AVAILABLE bfSolCred THEN 
                DO:
                    CREATE bfSolCred.
                    ASSIGN 
                        bfSolCred.IdSolicitud            = iID
                        bfSolCred.FecReg                 = TODAY
                        bfSolCred.IdEstatus              = 1
                        bfSolCred.TipoPersona            = ttSolCred.TipoPersona
                        bfSolCred.CP                     = ttSolCred.CP
                        bfSolCred.Colonia                = ttSolCred.Colonia
                        bfSolCred.Calle                  = ttSolCred.Calle 
                        bfSolCred.NumExt                 = ttSolCred.NumExt
                        bfSolCred.NumInt                 = ttSolCred.NumInt
                        bfSolCred.SitioWeb               = ttSolCred.SitioWeb
                        bfSolCred.NumEmpleados           = ttSolCred.NumEmpleados
                        bfSolCred.NumSucursales          = ttSolCred.NumSucursales
                        bfSolCred.DondeRealizaraPago     = ttSolCred.DondeRealizaraPago
                        bfSolCred.LocalPropio            = ttSolCred.LocalPropio
                        bfSolCred.EsAsociado             = ttSolCred.EsAsociado
                        bfSolCred.PronCompraMensual      = ttSolCred.PronCompraMensual
                        bfSolCred.CreditoSol             = ttSolCred.CreditoSol
                        bfSolCred.PlazoSol               = ttSolCred.PlazoSol
                        bfSolCred.Propietario            = ttSolCred.Propietario
                        bfSolCred.CURP                   = ttSolCred.CURP
                        bfSolCred.Correo                 = ttSolCred.Correo
                        bfSolCred.Telefono               = ttSolCred.Telefono
                        bfSolCred.RFCFiscal              = ttSolCred.RFCFiscal   
                        bfSolCred.RazonSocial            = ttSolCred.RazonSocial
                        bfSolCred.RegimenCapital         = ttSolCred.RegimenCapital 
                        bfSolCred.RegimenFiscal          = ttSolCred.RegimenFiscal /* Valida Catalogo */
                        bfSolCred.FormasPago             = ttSolCred.FormasPago /* Valida Catalogo */
                        bfSolCred.UsoCFDI                = ttSolCred.UsoCFDI /* Valida Catalogo */
                        bfSolCred.CPFiscal               = ttSolCred.CPFiscal
                        bfSolCred.CapitalSocial          = ttSolCred.CapitalSocial
                        bfSolCred.CapitalContable        = ttSolCred.CapitalContable
                        bfSolCred.RequiereOC             = ttSolCred.RequiereOC
                        bfSolCred.PlataformaFacPago      = ttSolCred.PlataformaFacPago
                        bfSolCred.PortalPlataforma       = ttSolCred.PortalPlataforma
                        bfSolCred.NombreAval             = ttSolCred.NombreAval
                        bfSolCred.ParentescoAval         = ttSolCred.ParentescoAval
                        bfSolCred.CheckDatosCorrectos    = ttSolCred.CheckDatosCorrectos
                        bfSolCred.CheckEnvio             = ttSolCred.CheckEnvio
                        bfSolCred.CheckValMinPed         = ttSolCred.CheckValMinPed
                        bfSolCred.CheckValMinCompMensual = ttSolCred.CheckValMinCompMensual
                        bfSolCred.IdDocumento            = 0
                        bfSolCred.Observaciones          = ttSolCred.Observaciones
                        bfSolCred.PagareFirmado          = ttSolCred.PagareFirmado
                        bfSolCred.IdCiudad               = ttSolCred.IdCiudad /* Valida Catalogo */
                        bfSolCred.IdRamo                 = ttSolCred.IdRamo   /* Valida Catalogo */
                        bfSolCred.IdGiro                 = ttSolCred.IdGiro   /* Valida Catalogo */
                        bfSolCred.IdSegmentoCte          = ttSolCred.IdSegmentoCte /* Valida Catalogo */
                        bfSolCred.NombreAsociado         = ttSolCred.NombreAsociado /* Valida Catalogo */
                        bfSolCred.IdCliente              = 0
                        bfSolCred.CreditoAutorizado      = 0
                        bfSolCred.PlazoAutorizado        = 0.   
                    ASSIGN 
                        lEncontrado = TRUE.
                    LEAVE.
                END.
            END. 

            /* Validar si no se encontró un ID único disponible */
            IF NOT lEncontrado THEN 
            DO:
                ASSIGN 
                    l-Mensaje = "No se pudo generar un ID único para la solicitud.".
                RETURN.
            END.
             
            RELEASE bfSolCred.
        END.

        /* Actualizar tablas relacionadas con el nuevo ID */
        FOR EACH ttDatosContacto:
            ASSIGN 
                ttDatosContacto.IdSolicitud = iID.
        END.

        FOR EACH ttReferProv WHERE (ttReferProv.NombreContacto <> "" 
           OR ttReferProv.Telefono <> "" 
           OR ttReferProv.Celular <> "" 
           OR ttReferProv.Correo <> ""):
            ASSIGN 
                ttReferProv.IdSolicitud = iID.
        END.

        FOR EACH ttReferComercial:
            ASSIGN 
                ttReferComercial.IdSolicitud = iID.
        END.

        FOR EACH ttDocReq:
            ASSIGN 
                ttDocReq.IdSolicitud = iID.
        END.
    END.

    /* Persistir los datos relacionados */
    FOR EACH ttDatosContacto:
        CREATE DatosContacto.
        BUFFER-COPY ttDatosContacto TO DatosContacto.
    END.

    FOR EACH ttReferProv WHERE (ttReferProv.NombreContacto <> "" 
           OR ttReferProv.Telefono <> "" 
           OR ttReferProv.Celular <> ""   
           OR ttReferProv.Correo <> ""):
        CREATE ReferProv.
        BUFFER-COPY ttReferProv TO ReferProv.
    END.

    FOR EACH ttReferComercial:
        CREATE ReferComercial.
        BUFFER-COPY ttReferComercial TO ReferComercial.
    END.
    FOR EACH ttDocReq:
        /* Generar un folio único solo una vez para todos los documentos */
        FIND FIRST bfSolCred WHERE bfSolCred.IdSolicitud = ttDocReq.IdSolicitud EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bfSolCred THEN 
        DO:
            /* Generar un nuevo folio si no existe */
            IF bfSolCred.IdDocumento = 0 THEN 
            DO:
                DO TRANSACTION:
                    DO lNuevoFolio = 1 TO 99999:
                        FIND FIRST DocReq WHERE DocReq.IdSolicitud = lNuevoFolio EXCLUSIVE-LOCK NO-ERROR.
                        IF NOT AVAILABLE DocReq THEN 
                        DO:
                            /* Asignar el folio único a SolCred */
                            ASSIGN 
                                bfSolCred.IdDocumento = lNuevoFolio.
                            LEAVE. /* Salir del bucle */
                        END.
                    END.
                END.

                /* Validar si no se pudo generar un folio único */
                IF bfSolCred.IdDocumento = 0 THEN 
                DO:
                    ASSIGN 
                        l-Mensaje = "No se pudo generar un folio único para los documentos.".
                    RETURN.
                END.
            END.

            /* Guardar el folio final */
            ASSIGN 
                FolioFinal = bfSolCred.IdDocumento.
            RELEASE bfSolCred.
        END.
        ELSE 
        DO:
            ASSIGN 
                l-Mensaje = "No se encontró la solicitud principal para los documentos.".
            RETURN.
        END.
    END.
    /* Procesar todos los documentos usando el mismo folio */
    FOR EACH ttDocReq:
        /* Verificar si ya existe un documento con el mismo TipoDoc */
        FIND FIRST DocReq 
            WHERE DocReq.IdSolicitud = FolioFinal
            AND DocReq.TipoDoc     = ttDocReq.TipoDoc 
            EXCLUSIVE-LOCK NO-ERROR.

        IF AVAILABLE DocReq THEN 
        DO:
            /* Si ya existe, actualizarlo */
            ASSIGN 
                DocReq.NombreArchivo = ttDocReq.NombreArchivo.
            
            
            IF ttDocReq.PDF <> ? THEN DO:
                COPY-LOB FROM ttDocReq.PDF TO DocReq.PDF.
                ASSIGN DocReq.DocCargado  = TRUE. 
            END.
        END.
        ELSE 
        DO:
            /* Crear un nuevo documento si no existe */
            CREATE DocReq.
            ASSIGN 
                DocReq.IdSolicitud   = FolioFinal
                DocReq.TipoDoc       = ttDocReq.TipoDoc
                DocReq.NombreArchivo = ttDocReq.NombreArchivo.
            
            IF ttDocReq.PDF <> ? THEN DO:
                COPY-LOB FROM ttDocReq.PDF TO DocReq.PDF.
                ASSIGN DocReq.DocCargado  = TRUE. 
            END.    
        END.
        RELEASE DocReq.
    END.

    /* Mensaje de éxito */
    ASSIGN 
        l-Mensaje = "Solicitud creada con ID: " + STRING(iID).
RETURN.
END PROCEDURE.



@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarSolicitud:
    DEFINE INPUT PARAMETER IdSolicitud AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER IdCliente   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER tipoDoc     AS INTEGER NO-UNDO. /* NUEVO PARAMETRO */
    DEFINE OUTPUT PARAMETER DATASET FOR dsSolicitud.

    /* Limpiar las tablas temporales antes de llenarlas */
    EMPTY TEMP-TABLE ttSolCred.
    EMPTY TEMP-TABLE ttDatosContacto.
    EMPTY TEMP-TABLE ttReferProv.
    EMPTY TEMP-TABLE ttReferComercial.
    EMPTY TEMP-TABLE ttDocReq.   
    
    IF IdSolicitud = ? THEN IdSolicitud = 0.
    IF IdCliente   = ? THEN IdCliente   = 0.
    IF tipoDoc     = ? THEN tipoDoc     = 0.
    
    /* Si se pasa el parámetro IdSolicitud, buscar por IdSolicitud */
    IF IdSolicitud <> 0 THEN
    DO:
        FIND FIRST SolCred WHERE SolCred.IdSolicitud = IdSolicitud NO-LOCK NO-ERROR.
        IF NOT AVAILABLE SolCred THEN RETURN. /* Si no se encuentra la solicitud, terminar */
    END.
    /* Si se pasa el parámetro IdCliente, buscar por IdCliente */
    IF IdCliente <> 0 THEN
    DO:
        FIND FIRST SolCred WHERE SolCred.IdCliente = IdCliente NO-LOCK NO-ERROR.
        IF NOT AVAILABLE SolCred THEN RETURN. /* Si no se encuentra la solicitud, terminar */
        IF AVAILABLE SolCred THEN IdSolicitud = SolCred.IdSolicitud. /* Si encuentra cliente Graba el ID */
    END.    


    /* Copiar datos a la tabla temporal `ttSolCred` */
    CREATE ttSolCred.
    BUFFER-COPY SolCred TO ttSolCred.
    
    /* Validar si la tabla SolCred tiene registros en SolCred.Id-Ciudad */
    IF SolCred.IdCiudad <> 0 THEN  
    DO:
        FIND Ciudad WHERE Ciudad.Id-Ciudad = SolCred.IdCiudad NO-LOCK NO-ERROR.
        IF AVAILABLE Ciudad THEN
        DO: 
            FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
            IF AVAILABLE Estado THEN
            DO:
                ASSIGN 
                    ttSolCred.IdEstado = Estado.Id-Estado.
                FIND Pais WHERE Pais.Id-Pais = Estado.Id-Pais NO-LOCK NO-ERROR.
                IF AVAILABLE Pais THEN ASSIGN ttSolCred.IdPais = Pais.Id-Pais.        
            END.
        END.       
    END.  
    

    /* Buscar y copiar los datos de contacto relacionados */
    FOR EACH DatosContacto WHERE DatosContacto.IdSolicitud = IdSolicitud NO-LOCK:
        CREATE ttDatosContacto.
        BUFFER-COPY DatosContacto TO ttDatosContacto.
    END.

    /* Buscar y copiar las referencias de proveedores relacionadas */
    FOR EACH ReferProv WHERE ReferProv.IdSolicitud = IdSolicitud NO-LOCK:
        CREATE ttReferProv.
        BUFFER-COPY ReferProv TO ttReferProv.
    END.

    /* Buscar y copiar las referencias comerciales relacionadas */
    FOR EACH ReferComercial WHERE ReferComercial.IdSolicitud = IdSolicitud NO-LOCK:
        CREATE ttReferComercial.
        BUFFER-COPY ReferComercial TO ttReferComercial.
    END.

    /* Documentos: Si se especifica tipoDoc, solo ese, con PDF */
    IF tipoDoc <> 0 THEN DO:
        FOR EACH DocReq 
            WHERE DocReq.IdSolicitud = IdSolicitud 
              AND DocReq.TipoDoc = tipoDoc 
            NO-LOCK:

            CREATE ttDocReq.
            BUFFER-COPY DocReq TO ttDocReq. /* Incluye PDF */
        END.
    END.
    ELSE DO:
        /* Si no se especifica tipoDoc, traer todos pero sin PDF */
        FOR EACH DocReq WHERE DocReq.IdSolicitud = IdSolicitud  NO-LOCK:
            CREATE ttDocReq.
            BUFFER-COPY DocReq TO ttDocReq
            ASSIGN ttDocReq.PDF = ?. /* Limpiar PDF */
        END.
    END.
RETURN.
END PROCEDURE.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE EditarSolicitud:
    /*Se utiliza para campos de Solicitud que se modifican despues de crear */
    DEFINE INPUT PARAMETER DATASET FOR dsSolicitud.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.
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
        
        /* Actualizar solo los campos con información */
        IF ttSolCred.Observaciones<> "" THEN 
            bfSolCred.Observaciones = ttSolCred.Observaciones.
        
        IF ttSolCred.PagareFirmado <> FALSE THEN      
            bfSolCred.PagareFirmado = ttSolCred.PagareFirmado.
        
        IF ttSolCred.Vendedor <> "" THEN 
        DO:
            FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = ttSolCred.Vendedor NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Vendedor THEN 
            DO:
                ASSIGN   
                    l-Mensaje = "Vendedor No Valido".
                RETURN.
            END.
            IF AVAILABLE Vendedor THEN  bfSolCred.Vendedor = ttSolCred.Vendedor.    
        END.    
            
            
            
    END.  

    /* Liberar buffer */ 
    RELEASE bfSolCred.  
RETURN.   
END PROCEDURE.   

