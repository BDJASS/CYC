@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*--------------------------------------------------------------------------  
 File        : Solicitudcreditocte.p
 URL         : /ClienteCredito
 Module      : Gestión de Créditos - Registro de Solicitud de Créditos
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
DEFINE TEMP-TABLE ttCteCred NO-UNDO              
    FIELD IdCliente          LIKE Cliente.Id-Cliente
    FIELD IdUser             LIKE Cliente.Id-User
    FIELD FecReg             LIKE Cliente.FecReg  
    FIELD TipoPersona        AS CHAR /* SE PONE CHAR PORQUE SE DEVUELVE FISICA / MORAL */
    FIELD CP                 LIKE Cliente.CP
    FIELD Colonia            LIKE Cliente.Colonia
    FIELD Calle              LIKE Cliente.Calle
    FIELD NumExt             LIKE Cliente.NumExt
    FIELD NumInt             LIKE Cliente.NumInt
    FIELD SitioWeb           LIKE Cliente.SitioWeb
    FIELD NumEmpleados       LIKE Cliente.NumEmpleados
    FIELD NumSucursales      LIKE Cliente.NumSucursales
    FIELD DondeRealizaraPago LIKE Cliente.DondeRealizaraPago
    FIELD LocalPropio        LIKE Cliente.LocalPropio
    FIELD EsAsociado         LIKE Cliente.EsAsociado
    FIELD PronCompraMensual  LIKE Cliente.PronCompraMensual
    FIELD Propietario        LIKE Cliente.Propietario
    FIELD CURP               LIKE Cliente.CURP
    FIELD Correo             LIKE Cliente.e-mail
    FIELD Telefono           LIKE Cliente.Tel1
    FIELD RFCFiscal          LIKE Cliente.RFC
    FIELD RazonSocial        LIKE Cliente.RazonSocial
    FIELD RegimenCapital     LIKE Cliente.RSocietario
    FIELD RegimenFiscal      LIKE Cliente.Id-RFiscal
    FIELD FormasPago         LIKE Cliente.FEFormaPago
    FIELD UsoCFDI            LIKE Cliente.Id-UsoCFDI
    FIELD CPFiscal           LIKE Cliente.CP         // Revisar este dato Codigo Postal Fiscal
    FIELD CapitalSocial      LIKE Cliente.CapitalSocial
    FIELD CapitalContable    LIKE Cliente.CapitalContable
    FIELD RequiereOC         LIKE Cliente.OCyes           // revisar este campo 
    FIELD PlataformaFacPago  LIKE Cliente.PlataformaFacPago
    FIELD PortalPlataforma   LIKE Cliente.PortalPlataforma
    FIELD NombreAval         LIKE Cliente.NombreAval
    FIELD ParentescoAval     LIKE Cliente.ParentescoAval
    /* FIELD CheckDatosCorrectos    LIKE Cliente.CheckDatosCorrectos
       FIELD CheckEnvio             LIKE Cliente.CheckEnvio
       FIELD CheckValMinPed         LIKE Cliente.CheckValMinPed
       FIELD CheckValMinCompMensual LIKE Cliente.CheckValMinCompMensual */
    FIELD IdDocumento        LIKE Cliente.Id-Documento
    FIELD IdClase            LIKE Cliente.Id-ClaseCte     
    /*  FIELD CheckRevision1         LIKE Cliente.CheckRevision1 
        FIELD CheckRevision2         LIKE Cliente.CheckRevision2 
        FIELD CheckRevision3         LIKE Cliente.CheckRevision3 
        FIELD CheckRevision4         LIKE Cliente.CheckRevision4  */
    FIELD CreditoAutorizado  LIKE Cliente.Limite      
    FIELD PlazoAutorizado    LIKE Cliente.Plazo   
    FIELD Observaciones      LIKE Cliente.Observaciones
    FIELD PagareFirmado      LIKE Cliente.PagareFirmado
    FIELD Vendedor           LIKE Cliente.Id-Vendedor
    FIELD IdCiudad           LIKE Cliente.Id-Ciudad
    FIELD IdRamo             LIKE Cliente.Id-Ramo
    FIELD IdGiro             LIKE Cliente.Id-Giro
    FIELD IdSegmentoCte      LIKE Cliente.Id-SegmentoCte
    FIELD IdCalidad          LIKE Cliente.Id-Calidad
    FIELD NombreAsociado     LIKE Cliente.NombreAsociado   
    FIELD IdPais             LIKE Pais.Id-Pais  
    FIELD IdEstado           LIKE Estado.Id-Estado   
    FIELD IdResp             LIKE Cliente.Id-Resp
    FIELD Activo             LIKE Cliente.Activo
    FIELD IdCobrador         LIKE Cliente.Id-Cobrador
    FIELD BuzonFiscal        LIKE Cliente.BuzonFiscal   // Envio Facturas
    FIELD CPBuzonFiscal      LIKE Cliente.CPBuzonFiscal // Complemento pago
    FIELD ECBuzon            LIKE Cliente.ECBuzon      // Estado Cuenta
 // FIELD CalleNo                LIKE Cliente.CalleNo.  
    FIELD UsrModLimite       AS CHAR
    FIELD FecModLimite       AS DATE
    FIELD UsrModPlazo        AS CHAR
    FIELD FecModPlazo        AS DATE.   
    
/* Tabla temporal para entrada */
DEFINE TEMP-TABLE ttDatosContacto NO-UNDO
    FIELD IdCliente    LIKE CteEmp.Id-Cliente    
    FIELD TipoContacto LIKE CteEmp.TipoContacto
    FIELD Nombre       LIKE CteEmp.Nombre
    FIELD Puesto       LIKE CteEmp.Puesto
    FIELD Telefono     LIKE CteEmp.Tel1
    FIELD Correo       LIKE CteEmp.e-mail
    FIELD WhatsApp     LIKE CteEmp.WhatsApp
    FIELD Categoria    AS CHAR
    FIELD Coment       LIKE CteEmp.comentario. /* En cliente ya se usa la tabl CteEmp
                                                ya no usamos DatosContacto solo en Soli */
    

DEFINE TEMP-TABLE ttReferProv NO-UNDO
    FIELD TipoProv       LIKE ReferProv.TipoProv
    FIELD Num            LIKE ReferProv.Num
    FIELD NombreContacto LIKE ReferProv.NombreContacto
    FIELD Telefono       LIKE ReferProv.Telefono
    FIELD Celular        LIKE ReferProv.Celular
    FIELD Correo         LIKE ReferProv.Correo
    FIELD IdCliente      LIKE ReferProv.IdCliente
    FIELD Revisada       LIKE ReferProv.Revisada
    FIELD Observaciones  AS CHAR.      

DEFINE TEMP-TABLE ttReferComercial NO-UNDO
    FIELD IdCliente          LIKE ReferComercial.IdCliente
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
    FIELD ConsideracionCte   LIKE ReferComercial.ConsideracionCte.

DEFINE TEMP-TABLE ttDocReq NO-UNDO
    FIELD IdCliente     LIKE DocReq.IdCliente
    FIELD TipoDoc       LIKE DocReq.TipoDoc
    FIELD NombreArchivo LIKE DocReq.NombreArchivo
    FIELD PDF           LIKE DocReq.PDF
    FIELD DocCargado    LIKE DocReq.DocCargado.  
    
DEFINE DATASET dsCliente FOR 
    ttCteCred, /* Tabla principal */
    ttDatosContacto, /* Datos de Contacto */
    ttReferProv,
    ttReferComercial,
    ttDocReq
    DATA-RELATION ClienteContacto FOR ttCteCred, ttDatosContacto
    RELATION-FIELDS (IdCliente, IdCliente) /* Relación Solicitud-DatosContacto por IdCliente  */
    DATA-RELATION SolicitudReferProv FOR ttCteCred, ttReferProv
    RELATION-FIELDS (IdCliente, IdCliente) /* Relación Solicitud-ReferProv por IdCliente  */ 
    DATA-RELATION SolicitudReferComercial FOR ttCteCred, ttReferComercial
    RELATION-FIELDS (IdCliente, IdCliente) /* Relación Solicitud-ReferComercial por IdCliente  */  
    DATA-RELATION SolicitudReferProv FOR ttCteCred, ttDocReq
    RELATION-FIELDS (IdCliente, IdCliente). /* Relación Solicitud-DocReq por IdCliente  */  
          
       

/* Buffer para trabajar con la tabla persistente */
DEFINE BUFFER bfCliente        FOR Cliente.
DEFINE BUFFER bfCteEmp         FOR CteEmp.
DEFINE BUFFER bfReferProv      FOR ReferProv.
DEFINE BUFFER bfReferComercial FOR ReferComercial.
DEFINE BUFFER bfDocReq         FOR DocReq.

/* Buffer para revisar en tabla Cliente Registros */
DEFINE BUFFER bfRFC            FOR Cliente.
DEFINE BUFFER bfRazonSocial    FOR Cliente.
DEFINE BUFFER bfCorreo         FOR Cliente.

DEFINE BUFFER bCambioCte       FOR CambioCte.



/* Variables */
DEFINE VARIABLE totalProcesado AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE errores        AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE lNuevoFolio    AS INTEGER NO-UNDO. /* Variable para generar nuevo folio */
DEFINE VARIABLE FolioFinal     AS INTEGER NO-UNDO.

/* **********************  Internal Procedures  *********************** */



/* ***************************  Main Block **************************** */




@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarSolicitud:
    DEFINE INPUT PARAMETER IdCliente   AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER DATASET FOR dsCliente.

    /* Limpiar las tablas temporales antes de llenarlas */
    EMPTY TEMP-TABLE ttCteCred.
    EMPTY TEMP-TABLE ttDatosContacto.
    EMPTY TEMP-TABLE ttReferProv.
    EMPTY TEMP-TABLE ttReferComercial.
    EMPTY TEMP-TABLE ttDocReq.   
    
    IF IdCliente   = ? THEN IdCliente   = 0.
    
    /* Buscar cliente por ID */
    FIND FIRST Cliente WHERE Cliente.Id-Cliente = IdCliente NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE Cliente THEN 
        RETURN. /* Salir si no se encuentra el cliente */
    
    DEFINE VARIABLE cFEFormaPago AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFormaPago   AS INTEGER   NO-UNDO.
    
    /* cFEFormaPago = REPLACE(Cliente.FEFormaPago, " ", "").  /* Elimina espacios */ 
     iFormaPago = INTEGER(cFEFormaPago).  */   
    CREATE ttCteCred.
    ASSIGN 
        ttCteCred.IdCliente          = Cliente.Id-Cliente         
        ttCteCred.IdClase            = Cliente.Id-ClaseCte        
        ttCteCred.IdResp             = Cliente.Id-Resp              
        ttCteCred.FecReg             = Cliente.FecReg             
        ttCteCred.Activo             = Cliente.Activo             
        ttCteCred.TipoPersona        = IF Cliente.Tipo = 1 THEN "Fisica" ELSE "Moral"            
        ttCteCred.CP                 = Cliente.CP                 
        ttCteCred.Colonia            = Cliente.Colonia           
        ttCteCred.Calle              = Cliente.Calle              
        ttCteCred.NumExt             = Cliente.NumExt             
        ttCteCred.NumInt             = Cliente.NumInt             
        ttCteCred.SitioWeb           = Cliente.SitioWeb           
        ttCteCred.NumEmpleados       = Cliente.NumEmpleados       
        ttCteCred.NumSucursales      = Cliente.NumSucursales      
        ttCteCred.DondeRealizaraPago = Cliente.DondeRealizaraPago  
        ttCteCred.LocalPropio        = Cliente.LocalPropio        
        ttCteCred.EsAsociado         = Cliente.EsAsociado         
        ttCteCred.PronCompraMensual  = Cliente.PronCompraMensual  
        ttCteCred.Propietario        = Cliente.Propietario        
        ttCteCred.CURP               = Cliente.curp               
        ttCteCred.Correo             = Cliente.e-mail             
        ttCteCred.Telefono           = Cliente.Tel1               
        ttCteCred.RFCFiscal          = Cliente.RFC              
        ttCteCred.RazonSocial        = Cliente.RazonSocial        
        ttCteCred.RegimenCapital     = Cliente.RSocietario        
        ttCteCred.RegimenFiscal      = Cliente.Id-RFiscal         
        ttCteCred.FormasPago         = Cliente.FEFormaPago         
        ttCteCred.UsoCFDI            = Cliente.Id-UsoCFDI         
        ttCteCred.CPFiscal           = Cliente.CPBuzonFiscal      
        ttCteCred.CapitalSocial      = Cliente.CapitalSocial      
        ttCteCred.CapitalContable    = Cliente.CapitalContable    
        ttCteCred.RequiereOC         = Cliente.OCyes              
        ttCteCred.PlataformaFacPago  = Cliente.PlataformaFacPago 
        ttCteCred.PortalPlataforma   = Cliente.PortalPlataforma
        ttCteCred.NombreAval         = Cliente.NombreAval         
        ttCteCred.ParentescoAval     = Cliente.ParentescoAval     
        ttCteCred.IdDocumento        = Cliente.Id-Documento       
        ttCteCred.Observaciones      = Cliente.Observaciones      
        ttCteCred.PagareFirmado      = Cliente.PagareFirmado
        ttCteCred.Vendedor           = Cliente.Id-Vendedor      
        ttCteCred.IdCiudad           = Cliente.Id-Ciudad          
        ttCteCred.IdRamo             = Cliente.Id-Ramo            
        ttCteCred.IdGiro             = Cliente.Id-Giro            
        ttCteCred.IdSegmentoCte      = Cliente.Id-SegmentoCte     
        ttCteCred.NombreAsociado     = Cliente.NombreAsociado     
        ttCteCred.CreditoAutorizado  = Cliente.Limite             
        ttCteCred.PlazoAutorizado    = Cliente.Plazo 
        ttCteCred.BuzonFiscal        = Cliente.BuzonFiscal
        ttCteCred.CPBuzonFiscal      = Cliente.CPBuzonFiscal
        ttCteCred.ECBuzon            = Cliente.ECBuzon
        ttCteCred.IdCalidad          = Cliente.Id-Calidad. 
    
    /* Validar si la tabla Cliente tiene registros en Cliente.Id-Ciudad */
    IF Cliente.Id-Ciudad <> 0 THEN  
    DO:
        FIND Ciudad WHERE Ciudad.Id-Ciudad = Cliente.Id-Ciudad NO-LOCK NO-ERROR.
        IF AVAILABLE Ciudad THEN
        DO: 
            FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
            IF AVAILABLE Estado THEN
            DO:
                ASSIGN 
                    ttCteCred.IdEstado = Estado.Id-Estado.
                FIND Pais WHERE Pais.Id-Pais = Estado.Id-Pais NO-LOCK NO-ERROR.
                IF AVAILABLE Pais THEN ASSIGN ttCteCred.IdPais = Pais.Id-Pais.        
            END.
        END.       
    END.  
    FOR EACH bCambioCte WHERE 
        bCambioCte.Id-Cliente = IdCliente AND
        bCambioCte.Campo = 500
        NO-LOCK
        BY bCambioCte.FecReg DESCENDING
        BY bCambioCte.Hora   DESCENDING:

        /* Este es el más reciente */
        FIND FIRST Empleado WHERE Empleado.Iniciales = bCambioCte.Id-User NO-LOCK NO-ERROR.

        ASSIGN 
            ttCteCred.UsrModLimite = IF AVAILABLE Empleado 
                                 THEN Empleado.Nombre 
                                 ELSE bCambioCte.Id-User
            ttCteCred.FecModLimite = bCambioCte.FecReg.

        LEAVE. /* Ya obtuviste el último registro */
    END.
    
    FOR EACH bCambioCte WHERE 
        bCambioCte.Id-Cliente = IdCliente AND
        bCambioCte.Campo = 501
        NO-LOCK
        BY bCambioCte.FecReg DESCENDING
        BY bCambioCte.Hora   DESCENDING:

        /* Este es el más reciente */
        FIND FIRST Empleado WHERE Empleado.Iniciales = bCambioCte.Id-User NO-LOCK NO-ERROR.

        ASSIGN 
            ttCteCred.UsrModPlazo = IF AVAILABLE Empleado 
                                 THEN Empleado.Nombre 
                                 ELSE bCambioCte.Id-User
            ttCteCred.FecModPlazo = bCambioCte.FecReg.

        LEAVE. /* Ya obtuviste el último registro */
    END.


    /* Buscar y copiar los datos de contacto relacionados */
    FOR EACH CteEmp WHERE CteEmp.Id-Cliente  = IdCliente  NO-LOCK:
        CREATE ttDatosContacto.
        ASSIGN
            ttDatosContacto.IdCliente    = CteEmp.Id-Cliente    
            ttDatosContacto.TipoContacto = CteEmp.TipoContacto
            ttDatosContacto.Nombre       = CteEmp.Nombre
            ttDatosContacto.Puesto       = CteEmp.Puesto
            ttDatosContacto.Telefono     = CteEmp.Tel1
            ttDatosContacto.Correo       = CteEmp.e-mail
            ttDatosContacto.WhatsApp     = CteEmp.WhatsApp
            ttDatosContacto.Categoria    = IF CteEmp.principal = TRUE THEN "Principal" ELSE "Alterno"
            ttDatosContacto.Coment       = CteEmp.comentario.
    END.

    /* Buscar y copiar las referencias de proveedores relacionadas */
    FOR EACH ReferProv WHERE ReferProv.IdCliente  = IdCliente  NO-LOCK:
        CREATE ttReferProv.
        BUFFER-COPY ReferProv TO ttReferProv.
    END.

    /* Buscar y copiar las referencias comerciales relacionadas */
    FOR EACH ReferComercial WHERE ReferComercial.IdCliente  = IdCliente  NO-LOCK:
        CREATE ttReferComercial.
        BUFFER-COPY ReferComercial TO ttReferComercial.
    END.

    /* Buscar y copiar los documentos relacionados */
    FOR EACH DocReq WHERE DocReq.IdCliente  = IdCliente NO-LOCK:
        CREATE ttDocReq.
        BUFFER-COPY DocReq TO ttDocReq
            ASSIGN 
            ttDocReq.PDF = ?.   
    END.
    
    RETURN.
END PROCEDURE.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE EditarSolicitud:
    DEFINE INPUT PARAMETER DATASET FOR dsCliente.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER l-StatusCode AS INTEGER NO-UNDO. /* Parámetro para el código de estado HTTP */
    /* Procesar cada registro en la tabla temporal */
    
    IF CAN-FIND(FIRST ttCteCred) THEN 
    DO:
        FOR EACH ttCteCred:
            /* Validar que la solicitud exista en la tabla Cliente */
            FIND FIRST bfCliente WHERE bfCliente.Id-Cliente  = ttCteCred.IdCliente  EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE bfCliente THEN 
            DO:
                ASSIGN   
                    l-Mensaje = "Cliente con ID " + STRING(ttCteCred.IdCliente ) + " no existe en la base de datos.".
                l-StatusCode = 400. /* Código de error HTTP 400 Bad Request */
                RETURN.
            END.
        
            IF ttCteCred.IdUser = "" THEN
            DO:
                ASSIGN 
                    l-Mensaje = "Ingresar User que realiza la modificacion".
                l-StatusCode = 400. /* Código de error HTTP 400 Bad Request */
                RETURN.
            END.
        
            /* Razon Social - RFC - Correo valida que no existan en BD */ 
            IF ttCteCred.RazonSocial <> "" THEN 
            DO:
                FIND FIRST bfRazonSocial WHERE bfRazonSocial.RazonSocial = ttCteCred.RazonSocial NO-LOCK NO-ERROR.
                IF AVAILABLE bfRazonSocial THEN 
                DO:
                    ASSIGN 
                        l-Mensaje = "Cliente ya registrado con la razón social: " + ttCteCred.RazonSocial.
                    l-StatusCode = 400. /* Código de error HTTP 400 Bad Request */
                    RETURN.
                END.
                ELSE 
                DO:
                    ASSIGN 
                        bfCliente.RazonSocial = ttCteCred.RazonSocial. 
                    
                    CREATE CambioCte.
                    ASSIGN
                        CambioCte.Id-Cliente = bfCliente.Id-Cliente
                        CambioCte.Id-User    = ttCteCred.IdUser
                        CambioCte.Descr      = "RazonSocial"
                        CambioCte.ValorNuevo = ttCteCred.RazonSocial
                        CambioCte.ValorOld   = bfCliente.RazonSocial 
                        CambioCte.FecReg     = TODAY
                        CambioCte.Hora       = TIME
                        CambioCte.Campo      = 1.    
                END.            
            END.
        
            IF ttCteCred.Correo <> "" THEN 
            DO:
                FIND FIRST bfCorreo WHERE bfCorreo.e-mail = ttCteCred.Correo NO-LOCK NO-ERROR.
                IF AVAILABLE bfCorreo THEN 
                DO:
                    ASSIGN 
                        l-Mensaje = "Cliente ya registrado con el email: " + ttCteCred.Correo.
                    l-StatusCode = 400. /* Código de error HTTP 400 Bad Request */
                    RETURN.
                END.
                ELSE 
                DO:
                    ASSIGN 
                        bfCliente.e-mail = ttCteCred.Correo.
                    
                    CREATE CambioCte.
                    ASSIGN
                        CambioCte.Id-Cliente = bfCliente.Id-Cliente
                        CambioCte.Id-User    = ttCteCred.IdUser
                        CambioCte.Descr      = "e-Mail"
                        CambioCte.ValorNuevo = ttCteCred.Correo
                        CambioCte.ValorOld   = bfCliente.e-mail
                        CambioCte.FecReg     = TODAY
                        CambioCte.Hora       = TIME
                        CambioCte.Campo      = 48.        
                END.            
            END.
                
            IF ttCteCred.RFCFiscal <> "" THEN
            DO:
                FIND FIRST bfRFC WHERE bfRFC.RFC = ttCteCred.RFCFiscal NO-LOCK NO-ERROR.
                IF AVAILABLE bfRFC THEN 
                DO:
                    ASSIGN  
                        l-Mensaje = "Cliente ya registrado con el RFC: " + ttCteCred.RFCFiscal.
                    l-StatusCode = 400. /* Código de error HTTP 400 Bad Request */
                    RETURN.
                END.
                ELSE 
                DO:
                    ASSIGN 
                        bfCliente.RFC = ttCteCred.RFCFiscal.
                    
                    CREATE CambioCte.
                    ASSIGN
                        CambioCte.Id-Cliente = bfCliente.Id-Cliente
                        CambioCte.Id-User    = ttCteCred.IdUser
                        CambioCte.Descr      = "RFC"
                        CambioCte.ValorNuevo = ttCteCred.RFCFiscal
                        CambioCte.ValorOld   = bfCliente.RFC
                        CambioCte.FecReg     = TODAY
                        CambioCte.Hora       = TIME
                        CambioCte.Campo      = 4.      
                END.
            END.
        

            /* Actualizar solo los campos con información */

           
            IF ttCteCred.CP <> "" THEN 
            DO:   
                ASSIGN 
                    bfCliente.CP = ttCteCred.CP.
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "CP"
                    CambioCte.ValorNuevo = ttCteCred.CP
                    CambioCte.ValorOld   = bfCliente.CP
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 8.              
            END.  
        
            IF ttCteCred.Colonia <> "" THEN 
            DO:
                ASSIGN 
                    bfCliente.Colonia = ttCteCred.Colonia.
                
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "Colonia"
                    CambioCte.ValorNuevo = ttCteCred.Colonia
                    CambioCte.ValorOld   = bfCliente.Colonia
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 6.     
            END.  
        
            IF ttCteCred.Calle <> "" THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "Calle"
                    CambioCte.ValorNuevo = ttCteCred.Calle
                    CambioCte.ValorOld   = bfCliente.Calle
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 5.   
                ASSIGN
                    bfCliente.Calle   = ttCteCred.Calle
                    bfCliente.CalleNo = TRIM(ttCteCred.Calle + " " + bfCliente.NumInt + " " + bfCliente.NumExt).
               
            END. 
        
            IF ttCteCred.NumExt <> "0" THEN 
            DO:
            
                CREATE CambioCte.     
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "NumExt"
                    CambioCte.ValorNuevo = ttCteCred.NumExt
                    CambioCte.ValorOld   = bfCliente.NumExt
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 100.  
                ASSIGN 
                    bfCliente.NumExt  = ttCteCred.NumExt
                    bfCliente.CalleNo = TRIM(bfCliente.Calle + " " + bfCliente.NumInt + " " + ttCteCred.NumExt).
             
            END.
            IF ttCteCred.NumInt <> "0" THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "NumInt"
                    CambioCte.ValorNuevo = ttCteCred.NumInt
                    CambioCte.ValorOld   = bfCliente.NumInt
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 110. 
                ASSIGN
                    bfCliente.NumInt  = ttCteCred.NumInt
                    bfCliente.CalleNo = TRIM(bfCliente.Calle + " " + ttCteCred.NumInt + " " + bfCliente.NumExt).    
            END.  
        
            IF ttCteCred.SitioWeb <> "" THEN 
            DO:
            
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "SitioWeb"
                    CambioCte.ValorNuevo = ttCteCred.SitioWeb
                    CambioCte.ValorOld   = bfCliente.SitioWeb
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME    
                    CambioCte.Campo      = 120. 
                ASSIGN  
                    bfCliente.SitioWeb = ttCteCred.SitioWeb.         
            END.  
            IF ttCteCred.NumEmpleados <> 0 THEN 
            DO:
           
                
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "NumEmpleados"
                    CambioCte.ValorNuevo = STRING(ttCteCred.NumEmpleados)
                    CambioCte.ValorOld   = STRING(bfCliente.NumEmpleados)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME   
                    CambioCte.Campo      = 130.  
                ASSIGN 
                    bfCliente.NumEmpleados = ttCteCred.NumEmpleados.    
            END.
            IF ttCteCred.NumSucursales <> 0 THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "NumSucursales"
                    CambioCte.ValorNuevo = STRING (ttCteCred.NumSucursales)
                    CambioCte.ValorOld   = STRING (bfCliente.NumSucursales)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 140.  
                ASSIGN
                    bfCliente.NumSucursales = ttCteCred.NumSucursales. 
            END.  
            IF ttCteCred.DondeRealizaraPago <> "" THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "DondeRealizaraPago"
                    CambioCte.ValorNuevo = ttCteCred.DondeRealizaraPago
                    CambioCte.ValorOld   = bfCliente.DondeRealizaraPago
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 150.
                ASSIGN
                    bfCliente.DondeRealizaraPago = ttCteCred.DondeRealizaraPago.     
            END.
        
            IF ttCteCred.LocalPropio <> ? THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "LocalPropio"
                    CambioCte.ValorNuevo = STRING(ttCteCred.LocalPropio)
                    CambioCte.ValorOld   = STRING(bfCliente.LocalPropio)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 160.
                ASSIGN 
                    bfCliente.LocalPropio = ttCteCred.LocalPropio.
            END.    
            IF ttCteCred.EsAsociado <> ? THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "EsAsociado"
                    CambioCte.ValorNuevo = STRING(ttCteCred.EsAsociado)
                    CambioCte.ValorOld   = STRING(bfCliente.EsAsociado)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 170. 
                ASSIGN
                    bfCliente.EsAsociado = ttCteCred.EsAsociado. 
            END.   
            IF ttCteCred.PronCompraMensual <> 0 THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "PronCompraMensual"
                    CambioCte.ValorNuevo = STRING(ttCteCred.PronCompraMensual)
                    CambioCte.ValorOld   = STRING(bfCliente.PronCompraMensual)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 180.      
                ASSIGN          
                    bfCliente.PronCompraMensual = ttCteCred.PronCompraMensual.
            END.  
            IF ttCteCred.Propietario <> "" THEN 
            DO:                                   
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "Propietario"
                    CambioCte.ValorNuevo = ttCteCred.Propietario
                    CambioCte.ValorOld   = bfCliente.Propietario
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 3.               
                ASSIGN
                    bfCliente.Propietario = ttCteCred.Propietario.
            END.   
            IF ttCteCred.CURP <> "" THEN 
            DO:
             
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "Propietario"
                    CambioCte.ValorNuevo = ttCteCred.CURP
                    CambioCte.ValorOld   = bfCliente.curp 
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 54.
                ASSIGN 
                    bfCliente.curp = ttCteCred.CURP.
            END. 
           
            IF ttCteCred.Telefono <> "" THEN 
            DO:
             
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "Tel1"
                    CambioCte.ValorNuevo = ttCteCred.Telefono
                    CambioCte.ValorOld   = bfCliente.Tel1 
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 10. 
                ASSIGN 
                    bfCliente.Tel1 = ttCteCred.Telefono. 
            END.   
            IF ttCteCred.RegimenFiscal <> "" THEN 
            DO:
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "Id-RFiscal"
                    CambioCte.ValorNuevo = ttCteCred.RegimenFiscal
                    CambioCte.ValorOld   = bfCliente.Id-RFiscal
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 14.   
                ASSIGN 
                    bfCliente.Id-RFiscal = ttCteCred.RegimenFiscal.    
            END.   
            IF ttCteCred.FormasPago <> "" THEN 
            DO:
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "FEFormaPago"
                    CambioCte.ValorNuevo = ttCteCred.FormasPago
                    CambioCte.ValorOld   = bfCliente.FEFormaPago
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 190. 
                ASSIGN 
                    bfCliente.FEFormaPago = ttCteCred.FormasPago.
            END.
            IF ttCteCred.UsoCFDI <> "" THEN 
            DO:
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "Id-UsoCFDI"
                    CambioCte.ValorNuevo = ttCteCred.UsoCFDI
                    CambioCte.ValorOld   = bfCliente.Id-UsoCFDI
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 19.  
                ASSIGN 
                    bfCliente.Id-UsoCFDI = ttCteCred.UsoCFDI.  
            END.
            IF ttCteCred.CPFiscal <> "" THEN 
            DO: 
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "CP"
                    CambioCte.ValorNuevo = ttCteCred.CPFiscal
                    CambioCte.ValorOld   = bfCliente.CP
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 8. 
                ASSIGN 
                    bfCliente.CP = ttCteCred.CPFiscal.
            END.   
            IF ttCteCred.CapitalSocial <> 0 THEN 
            DO:
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "CapitalSocial"
                    CambioCte.ValorNuevo = STRING(ttCteCred.CapitalSocial)
                    CambioCte.ValorOld   = STRING(bfCliente.CapitalSocial)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 200. 
                ASSIGN 
                    bfCliente.CapitalSocial = ttCteCred.CapitalSocial.   
            END.
            IF ttCteCred.CapitalContable <> 0 THEN 
            DO:
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "CapitalContable"
                    CambioCte.ValorNuevo = STRING(ttCteCred.CapitalContable) 
                    CambioCte.ValorOld   = STRING(bfCliente.CapitalContable) 
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 210.
                ASSIGN 
                    bfCliente.CapitalContable = ttCteCred.CapitalContable.
            END.   
            IF ttCteCred.RequiereOC <> ? THEN 
            DO:
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "OCyes"
                    CambioCte.ValorNuevo = STRING (ttCteCred.RequiereOC)
                    CambioCte.ValorOld   = STRING (bfCliente.OCyes)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 220.
                ASSIGN 
                    bfCliente.OCyes = ttCteCred.RequiereOC.
             
            END.
            IF ttCteCred.PlataformaFacPago <> "" THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "PlataformaFacPago"
                    CambioCte.ValorNuevo = ttCteCred.PlataformaFacPago
                    CambioCte.ValorOld   = bfCliente.PlataformaFacPago
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 230.
                ASSIGN 
                    bfCliente.PlataformaFacPago = ttCteCred.PlataformaFacPago.
            END.
           
            IF ttCteCred.PortalPlataforma <> "" THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser
                    CambioCte.Descr      = "PortalPlataforma"
                    CambioCte.ValorNuevo = ttCteCred.PortalPlataforma
                    CambioCte.ValorOld   = bfCliente.PortalPlataforma
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME    
                    CambioCte.Campo      = 240. 
                ASSIGN 
                    bfCliente.PortalPlataforma = ttCteCred.PortalPlataforma.          
            END.   
            IF ttCteCred.NombreAval <> "" THEN
                bfCliente.NombreAval = ttCteCred.NombreAval.
           
            IF ttCteCred.ParentescoAval <> "" THEN
                bfCliente.ParentescoAval = ttCteCred.ParentescoAval.
        

        
            IF ttCteCred.IdClase <> 0 THEN    
            DO:
                /* Crear registro de CambioCte */
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser 
                    CambioCte.Descr      = "Id-ClaseCte"
                    CambioCte.ValorNuevo = STRING(ttCteCred.IdClase)
                    CambioCte.ValorOld   = STRING(bfCliente.Id-ClaseCte)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME    
                    CambioCte.Campo      = 250.
         
                /* Asignar Id-ClaseCte */
                ASSIGN
                    bfCliente.Id-ClaseCte = ttCteCred.IdClase.
    
                /* Asignar Id-Resp según la clase */
                CASE ttCteCred.IdClase:
                    WHEN 1 THEN 
                        bfCliente.Id-Resp = 33.  /* Clase 1 -> Responsable 33 */
                    WHEN 2 THEN 
                        bfCliente.Id-Resp = 9.   /* Clase 2 -> Responsable 9 */
                    WHEN 3 THEN 
                        bfCliente.Id-Resp = 30.  /* Clase 3 -> Responsable 30 */
                END CASE.
            END.   
        
           
            IF ttCteCred.CreditoAutorizado <> 0 THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser  
                    CambioCte.Descr      = "limite"
                    CambioCte.ValorNuevo = STRING(ttCteCred.CreditoAutorizado)
                    CambioCte.ValorOld   = STRING(bfCliente.Limite)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME    
                    CambioCte.Campo      = 500.  
                ASSIGN
                    bfCliente.Limite = ttCteCred.CreditoAutorizado.
            END.  
        
            IF ttCteCred.PlazoAutorizado <> 0 THEN 
            DO:
            
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser    
                    CambioCte.Descr      = "Plazo"
                    CambioCte.ValorNuevo = STRING(ttCteCred.PlazoAutorizado)
                    CambioCte.ValorOld   = STRING(bfCliente.Plazo)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME    
                    CambioCte.Campo      = 501. 
                ASSIGN
                    bfCliente.Plazo = ttCteCred.PlazoAutorizado.
            END.     
                          
            IF ttCteCred.Observaciones <> "" THEN 
                bfCliente.Observaciones = ttCteCred.Observaciones.
        
            IF ttCteCred.PagareFirmado <> ? THEN      
                bfCliente.PagareFirmado = ttCteCred.PagareFirmado.
                
            IF ttCteCred.Vendedor <> "" THEN 
            DO:
                FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = ttCteCred.Vendedor NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Vendedor THEN 
                DO:
                    ASSIGN   
                        l-Mensaje = "Vendedor No Valido".
                    l-StatusCode = 400. /* Código de error HTTP 400 Bad Request */
                    RETURN.
                END.
                IF AVAILABLE Vendedor THEN  bfCliente.Id-Vendedor = ttCteCred.Vendedor.    
            END.    
        
            IF ttCteCred.IdCiudad <> 0 THEN 
                bfCliente.Id-Ciudad = ttCteCred.IdCiudad.

            IF ttCteCred.IdRamo <> "" THEN 
                bfCliente.Id-Ramo = ttCteCred.IdRamo.

            IF ttCteCred.IdGiro <> 0 THEN 
                bfCliente.Id-Giro = ttCteCred.IdGiro.

            IF ttCteCred.IdSegmentoCte <> 0 THEN 
                bfCliente.Id-SegmentoCte = ttCteCred.IdSegmentoCte.

            IF ttCteCred.IdCalidad <> 0 THEN 
            DO:
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = bfCliente.Id-Cliente
                    CambioCte.Id-User    = ttCteCred.IdUser       
                    CambioCte.Descr      = "Id-Calidad"
                    CambioCte.ValorNuevo = STRING(ttCteCred.IdCalidad)
                    CambioCte.ValorOld   = STRING(bfCliente.Id-Calidad)
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME    
                    CambioCte.Campo      = 505.  
                ASSIGN 
                    bfCliente.Id-Calidad = ttCteCred.IdCalidad.              
            END.   
            IF ttCteCred.NombreAsociado <> 0 THEN 
                bfCliente.NombreAsociado = ttCteCred.NombreAsociado.

            IF ttCteCred.IdResp <> 0 THEN 
                bfCliente.Id-Resp = ttCteCred.IdResp.

            IF ttCteCred.Activo <> ? THEN 
                bfCliente.Activo = ttCteCred.Activo.

            IF ttCteCred.IdCobrador <> 0 THEN 
                bfCliente.Id-Cobrador = ttCteCred.IdCobrador.

            IF ttCteCred.BuzonFiscal <> "" THEN 
                bfCliente.BuzonFiscal = ttCteCred.BuzonFiscal.

            IF ttCteCred.CPBuzonFiscal <> "" THEN 
                bfCliente.CPBuzonFiscal = ttCteCred.CPBuzonFiscal.

            IF ttCteCred.ECBuzon <> "" THEN 
                bfCliente.ECBuzon = ttCteCred.ECBuzon. 
            
            IF ttCteCred.RegimenCapital <> "" THEN 
                bfCliente.RSocietario = ttCteCred.RegimenCapital. 
            
            /* Liberar buffer */ 
            RELEASE bfCliente.         
        END.
    END.
    IF CAN-FIND(FIRST ttDatosContacto) THEN 
    DO:
        FOR EACH ttDatosContacto:
    
            FIND FIRST bfCteEmp WHERE bfCteEmp.Id-Cliente  = ttDatosContacto.IdCliente
                                  AND bfCteEmp.Nombre       = ttDatosContacto.Nombre
                                  EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE bfCteEmp THEN 
            DO:
                ASSIGN   
                    l-Mensaje = "Cliente con ID " + STRING(ttDatosContacto.IdCliente ) + " no existe en la base de datos.".
                RETURN.
            END.   
        
            IF ttDatosContacto.Categoria <> "" THEN 
            DO: 
            // Principal - Alterno 
                IF ttDatosContacto.Categoria BEGINS "A" THEN
                    ASSIGN   bfCteEmp.principal = FALSE.
                ELSE 
                    ASSIGN   bfCteEmp.principal = TRUE.  
            END.
          
            IF ttDatosContacto.Coment <> "" THEN
                ASSIGN bfCteEmp.comentario = ttDatosContacto.Coment.
        
            IF ttDatosContacto.Correo <> "" THEN
                ASSIGN bfCteEmp.e-mail = ttDatosContacto.Correo.
        
            IF ttDatosContacto.Nombre <> "" THEN
                ASSIGN bfCteEmp.Nombre = ttDatosContacto.Nombre.
           
            IF ttDatosContacto.Puesto <> "" THEN
                ASSIGN bfCteEmp.Puesto = ttDatosContacto.Puesto.
           
            IF ttDatosContacto.Telefono <> "" THEN
                ASSIGN bfCteEmp.Tel1 = ttDatosContacto.Telefono.
              
            IF ttDatosContacto.TipoContacto <> "" THEN
                ASSIGN bfCteEmp.TipoContacto = ttDatosContacto.TipoContacto.
        
            IF ttDatosContacto.WhatsApp <> "" THEN
                ASSIGN bfCteEmp.WhatsApp = ttDatosContacto.WhatsApp.  
      
            RELEASE bfCteEmp.      
        END.
    END.
    
    IF CAN-FIND(FIRST ttReferProv) THEN 
    DO:
        FOR EACH ttReferProv:
        
            FIND FIRST bfReferProv WHERE  bfReferProv.IdCliente  = ttReferProv.IdCliente
                AND  bfReferProv.Num        = ttReferProv.Num EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE bfReferProv THEN 
            DO:
                ASSIGN   
                    l-Mensaje = "Cliente con ID " + STRING(ttReferProv.IdCliente ) + " no existe en Referencias Prov.".
                RETURN.
            END.
        
            IF ttReferProv.TipoProv <> 0 THEN
                bfReferProv.TipoProv = ttReferProv.TipoProv.

            IF ttReferProv.Num <> 0 THEN
                bfReferProv.Num = ttReferProv.Num.

            IF ttReferProv.NombreContacto <> "" THEN
                bfReferProv.NombreContacto = ttReferProv.NombreContacto.

            IF ttReferProv.Telefono <> "" THEN
                bfReferProv.Telefono = ttReferProv.Telefono.

            IF ttReferProv.Celular <> "" THEN
                bfReferProv.Celular = ttReferProv.Celular.

            IF ttReferProv.Correo <> "" THEN
                bfReferProv.Correo = ttReferProv.Correo.

            IF ttReferProv.Revisada <> ? THEN
                bfReferProv.Revisada = ttReferProv.Revisada.

            IF ttReferProv.Observaciones <> "" THEN
                bfReferProv.Observaciones = ttReferProv.Observaciones.

            RELEASE bfReferProv.
        END.
    END.
    
    IF CAN-FIND(FIRST ttReferComercial) THEN 
    DO:
        FOR EACH ttReferComercial:
        
            FIND FIRST bfReferComercial WHERE  bfReferComercial.IdCliente  = ttReferComercial.IdCliente
                AND  bfReferComercial.Num        = ttReferComercial.Num EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE bfReferComercial THEN 
            DO:
                ASSIGN   
                    l-Mensaje = "Cliente con ID " + STRING(ttReferComercial.IdCliente ) + " no existe en Referencia Comer.".
                RETURN.
            END.
        
            IF ttReferComercial.Num <> 0 THEN
                bfReferComercial.Num = ttReferComercial.Num.

            IF ttReferComercial.TiempoTrabajo <> "" THEN
                bfReferComercial.TiempoTrabajo = ttReferComercial.TiempoTrabajo.

            IF ttReferComercial.LimiteCred <> 0 THEN
                bfReferComercial.LimiteCred = ttReferComercial.LimiteCred.

            IF ttReferComercial.CondPago <> "" THEN
                bfReferComercial.CondPago = ttReferComercial.CondPago.

            IF ttReferComercial.DemoraPagos <> "" THEN
                bfReferComercial.DemoraPagos = ttReferComercial.DemoraPagos.

            IF ttReferComercial.DiasAtraso <> 0 THEN
                bfReferComercial.DiasAtraso = ttReferComercial.DiasAtraso.

            IF ttReferComercial.ConsumoPromMensual <> 0 THEN
                bfReferComercial.ConsumoPromMensual = ttReferComercial.ConsumoPromMensual.

            IF ttReferComercial.FormaPago <> "" THEN
                bfReferComercial.FormaPago = ttReferComercial.FormaPago.

            IF ttReferComercial.ChequeDev <> ? THEN
                bfReferComercial.ChequeDev = ttReferComercial.ChequeDev.

            IF ttReferComercial.OrdenCompra <> ? THEN
                bfReferComercial.OrdenCompra = ttReferComercial.OrdenCompra.

            IF ttReferComercial.GarantiaPagare <> ? THEN
                bfReferComercial.GarantiaPagare = ttReferComercial.GarantiaPagare.

            IF ttReferComercial.FecUltCompra <> ? THEN
                bfReferComercial.FecUltCompra = ttReferComercial.FecUltCompra.

            IF ttReferComercial.SaldoActualVig <> 0 THEN
                bfReferComercial.SaldoActualVig = ttReferComercial.SaldoActualVig.

            IF ttReferComercial.ConsideracionCte <> "" THEN
                bfReferComercial.ConsideracionCte = ttReferComercial.ConsideracionCte.
       
            /* Liberar el buffer */
            RELEASE bfReferComercial. 
        END.
    END.
    
    IF CAN-FIND(FIRST ttDocReq) THEN 
    DO:
        FOR EACH ttDocReq:
        
            FIND FIRST bfDocReq WHERE  bfDocReq.IdCliente  = ttDocReq.IdCliente
                AND  bfDocReq.TipoDoc    = ttDocReq.TipoDoc
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE bfDocReq THEN 
            DO:
                ASSIGN   
                    l-Mensaje = "Cliente con ID " + STRING(ttDocReq.IdCliente) + " no existe en Documentos.".
                RETURN.
            END.
        
            IF ttDocReq.PDF <> ? THEN 
            DO:
                COPY-LOB FROM ttDocReq.PDF TO bfDocReq.PDF.
                ASSIGN 
                    bfDocReq.DocCargado = TRUE.
            END.    
        
            /* Liberar el buffer */
            RELEASE bfDocReq.
        END.
    END.
    
    RETURN. 
END PROCEDURE.     


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE AltaRegistro:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER DATASET FOR dsCliente.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER l-StatusCode AS INTEGER NO-UNDO. /* Parámetro para el código de estado HTTP */
     
    DEFINE VARIABLE l-principal AS LOGICAL INITIAL FALSE.
    FOR EACH ttDatosContacto:
    
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente  = ttDatosContacto.IdCliente  EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCliente THEN 
        DO:
            ASSIGN   
                l-Mensaje = "Cliente con ID " + STRING(ttDatosContacto.IdCliente ) + " no existe en la base de datos.".
            l-StatusCode = 400. /* Código de error HTTP 400 Bad Request */
            RETURN.
        END.
        
        IF ttDatosContacto.Categoria <> "" THEN 
        DO: 
            // Principal - Alterno 
            IF ttDatosContacto.Categoria BEGINS "A" THEN
                ASSIGN   l-principal = FALSE.
            ELSE 
                ASSIGN   l-principal = TRUE.  
        END.   
        
        DO TRANSACTION:
            CREATE CteEmp.
            ASSIGN
                CteEmp.Id-Cliente   = ttDatosContacto.IdCliente
                CteEmp.principal    = l-principal   // Principal - Alterno
                CteEmp.comentario   = ttDatosContacto.Coment
                CteEmp.e-mail       = ttDatosContacto.Correo
                CteEmp.Nombre       = ttDatosContacto.Nombre
                CteEmp.Puesto       = ttDatosContacto.Puesto
                CteEmp.Tel1         = ttDatosContacto.Telefono
                CteEmp.TipoContacto = ttDatosContacto.TipoContacto
                CteEmp.WhatsApp     = ttDatosContacto.WhatsApp.
            RELEASE CteEmp.
        END.
        ASSIGN   
            l-Mensaje    = "Registro creado"
            l-StatusCode = 200.
        RETURN .  
            
    END.
    RETURN.        
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE EliminarRegistro:
    /*------------------------------------------------------------------------------
     Purpose: Eliminar un registro de la tabla bfCteEmp.
     Notes: Se eliminan registros por IdCliente y TipoContacto.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER DATASET FOR dsCliente.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER l-StatusCode AS INTEGER NO-UNDO. /* Código de estado HTTP */
     
    FOR EACH ttDatosContacto:
        
        /* Verificar si el cliente existe en la base de datos */
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttDatosContacto.IdCliente EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCliente THEN 
        DO:
            ASSIGN   
                l-Mensaje    = "Cliente con ID " + STRING(ttDatosContacto.IdCliente) + " no existe en la base de datos."
                l-StatusCode = 400. /* Código de error HTTP 400 Bad Request */
            RETURN.
        END.     

        /* Verificar si el contacto existe en la tabla de contactos */
        FIND FIRST bfCteEmp WHERE bfCteEmp.Id-Cliente = ttDatosContacto.IdCliente 
            AND bfCteEmp.TipoContacto = ttDatosContacto.TipoContacto
            AND bfCteEmp.Nombre       = ttDatosContacto.Nombre
            AND bfCteEmp.comentario   = ttDatosContacto.Coment
            AND bfCteEmp.e-mail       = ttDatosContacto.Correo
            AND bfCteEmp.principal    = IF ttDatosContacto.Categoria BEGINS "A" THEN FALSE ELSE TRUE 
            AND bfCteEmp.Puesto       = ttDatosContacto.Puesto
            AND bfCteEmp.Tel1         = ttDatosContacto.Telefono
            AND bfCteEmp.WhatsApp     = ttDatosContacto.WhatsApp
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCteEmp THEN     
        DO:
            ASSIGN   
                l-Mensaje = "Contacto no encontrado para el cliente con ID " + STRING(ttDatosContacto.IdCliente) + " y tipo de contacto: " + ttDatosContacto.TipoContacto.
            l-StatusCode = 404. /* Código de error HTTP 404 Not Found */
            RETURN.
        END.

        /* Eliminar el registro de la tabla bfCteEmp */
        DELETE bfCteEmp.
        
        ASSIGN 
            l-Mensaje    = "Contacto eliminado correctamente para el cliente con ID " + STRING(ttDatosContacto.IdCliente)
            l-StatusCode = 200. /* Código de éxito HTTP 200 OK */
        RETURN. 
    END. 
    RETURN.
END PROCEDURE.




