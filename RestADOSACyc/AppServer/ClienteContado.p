@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Alta Cliente Contado
    Purpose     : Procedimiento para registrar clientes de contado.
    Syntax      : Se usa en Gestión de clientes.
    Author(s)   : sis10
    Created     : (Fecha de creación)
    Notes       : Aplica reglas de asignación de responsables.
------------------------------------------------------------------------*/

/* ***************************  Definitions ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttCliente NO-UNDO   
    FIELD IdCliente       AS INT
    FIELD IdUser          AS CHAR
    FIELD Tipo            AS INTEGER
    FIELD RFC             LIKE Cliente.RFC
    FIELD RazonSocial     AS CHARACTER
    FIELD RegimenCapital  AS CHARACTER
    FIELD NombreComercial AS CHARACTER
    FIELD Propietario     AS CHARACTER
    FIELD Curp            AS CHARACTER
    FIELD Email           AS CHARACTER
    FIELD Telefono        AS CHARACTER
    FIELD Telefono2       AS CHARACTER
    FIELD Telefono3       AS CHARACTER
    FIELD BuzonFiscal     AS CHARACTER
    FIELD IdRFiscal       AS CHARACTER
    FIELD IdUsoCFDI       AS CHARACTER
    FIELD IdRamo          AS CHARACTER
    FIELD IdGiro          AS INTEGER
    FIELD IdSegmentoCte   AS INTEGER
    FIELD IdClase         LIKE Cliente.Id-ClaseCte
    FIELD IdCiudad        AS INTEGER
    FIELD CP              AS CHARACTER
    FIELD Colonia         AS CHARACTER
    FIELD Calle           AS CHARACTER
    FIELD NumExterior     AS CHARACTER 
    FIELD NumInterior     AS CHARACTER
    FIELD FecReg          AS DATE 
    FIELD IdCobrador      AS INT
    FIELD IdResp          AS INT.
    
DEFINE TEMP-TABLE ttClienteVisor NO-UNDO 
    FIELD IdCliente       AS INT
    FIELD IdUser          AS CHAR   
    FIELD Tipo            AS INTEGER
    FIELD RFC             LIKE Cliente.RFC
    FIELD RazonSocial     AS CHARACTER
    FIELD RegimenCapital  AS CHARACTER
    FIELD NombreComercial AS CHARACTER
    FIELD Propietario     AS CHARACTER
    FIELD Curp            AS CHARACTER
    FIELD Email           AS CHARACTER
    FIELD Telefono        AS CHARACTER   
    FIELD Telefono2       AS CHARACTER
    FIELD Telefono3       AS CHARACTER
    FIELD BuzonFiscal     AS CHARACTER
    FIELD RFiscal         AS CHARACTER
    FIELD UsoCFDI         AS CHARACTER
    FIELD Ramo            AS CHARACTER
    FIELD Giro            AS CHARACTER /* */ 
    FIELD SegmentoCte     AS CHARACTER /* */ 
    FIELD Clase           AS CHARACTER /* */ 
    FIELD Pais            AS CHARACTER /* */ 
    FIELD Estado          AS CHARACTER /* */ 
    FIELD Ciudad          AS CHARACTER /* */ 
    FIELD CP              AS CHARACTER
    FIELD Colonia         AS CHARACTER
    FIELD Calle           AS CHARACTER
    FIELD CalleNo         AS CHARACTER
    FIELD NumExterior     AS CHARACTER 
    FIELD NumInterior     AS CHARACTER
    FIELD FecAlta         AS DATE.    
    
DEFINE BUFFER bfCiudad FOR Ciudad. /* Buffer para validar reglas basadas en la ciudad */
DEFINE BUFFER bCliente FOR Cliente.


/* **********************  Internal Procedures  *********************** */


/* **********************  Internal Procedures *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE CreaClientes:

    DEFINE INPUT  PARAMETER TABLE FOR ttCliente.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.

    DEFINE VARIABLE l-RFCExcluido AS CHARACTER NO-UNDO 
        INITIAL "XAXX010101000,XEXX010101000,UIE920518PI5,UAN691126MK2,SEP210905778,GEN620601DTA,CEP981230AP9".
    DEFINE VARIABLE i-ID          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE l-Encontrado  AS LOGICAL   NO-UNDO.

    /* Si no hay registros en la tabla temporal, devolver un mensaje */
    IF NOT CAN-FIND(FIRST ttCliente) THEN 
    DO:
        ASSIGN 
            l-Mensaje = "No se enviaron datos de clientes para procesar.".
        RETURN.
    END.    

    /* Validación y creación del cliente */
    FOR EACH ttCliente EXCLUSIVE-LOCK:
        /* Validar Razon Social duplicada */
        FIND FIRST Cliente WHERE Cliente.RazonSocial = ttCliente.RazonSocial NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Cliente ya registrado con la razón social: " + ttCliente.RazonSocial.
            RETURN.
        END.

        /* Validar Email duplicado */
        FIND FIRST Cliente WHERE Cliente.e-mail = ttCliente.Email NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Cliente ya registrado con el email: " + ttCliente.Email.
            RETURN.
        END.
        
        /* Validar RFC duplicado */
        FIND FIRST Cliente WHERE Cliente.RFC = ttCliente.RFC NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Cliente ya registrado con el RFC: " + ttCliente.RFC.
            RETURN.
        END.
        
        // Valida Ciudad
        IF ttCliente.IdCiudad > 0 THEN 
        DO:
            FIND Ciudad WHERE Ciudad.Id-Ciudad = ttCliente.IdCiudad NO-LOCK NO-ERROR.

    // Validar si el registro de Ciudad fue encontrado
            IF AVAILABLE Ciudad THEN 
            DO:
        
                IF ttCliente.IdGiro = 45 OR ttCliente.IdCobrador = 10 THEN /* En Contado, aquí pide también Cobrador 10 */
                    ASSIGN 
                        ttCliente.IdResp = 30.
                ELSE 
                DO:
                    IF Ciudad.Id-Estado = "019" AND Ciudad.CveZona = 1 THEN 
                        ASSIGN 
                            ttCliente.IdResp = 33.
                    ELSE 
                        ASSIGN 
                            ttCliente.IdResp = 9.
                END.
        
            END.
        END.   
        

        /* Validar RFC en lista excluida */
//        IF LOOKUP(ttCliente.RFC, l-RFCExcluido) > 0 THEN DO:
//            ASSIGN l-Mensaje = "El RFC especificado está en la lista de RFCs excluidos: " + ttCliente.RFC.
//            RETURN.
//        END.

        /* Generar ID único */
        l-Encontrado = FALSE.
        DO i-ID = 10000 TO 90000:
            FIND FIRST Cliente WHERE Cliente.Id-Cliente = i-ID NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Cliente THEN 
            DO:
                ASSIGN 
                    ttCliente.IdCliente = i-ID
                    l-Encontrado        = TRUE.
                LEAVE.
            END.
        END.   

        IF NOT l-Encontrado THEN 
        DO:
            ASSIGN 
                l-Mensaje = "No se pudo generar un ID único para el cliente.".
            RETURN.
        END.

        /* Crear el registro en la tabla Cliente */
        CREATE Cliente.
        ASSIGN
            Cliente.Id-Cliente     = ttCliente.IdCliente
            Cliente.Id-User        = ttCliente.IdUser
            Cliente.Tipo           = ttCliente.Tipo
            Cliente.RFC            = ttCliente.RFC
            Cliente.RazonSocial    = ttCliente.RazonSocial
            Cliente.RSocietario    = ttCliente.RegimenCapital
            Cliente.NomEmpresa     = ttCliente.NombreComercial
            Cliente.Propietario    = ttCliente.Propietario
            Cliente.Curp           = ttCliente.Curp
            Cliente.e-mail         = ttCliente.Email
            Cliente.Tel1           = ttCliente.Telefono
            Cliente.Tel2           = ttCliente.Telefono2
            Cliente.Tel3           = ttCliente.Telefono3
            Cliente.BuzonFiscal    = ttCliente.BuzonFiscal
            Cliente.Id-RFiscal     = ttCliente.IdRFiscal
            Cliente.Id-UsoCFDI     = ttCliente.IdUsoCFDI
            Cliente.Id-Ramo        = ttCliente.IdRamo
            Cliente.Id-Giro        = ttCliente.IdGiro
            Cliente.Id-SegmentoCte = ttCliente.IdSegmentoCte
            Cliente.Id-ClaseCte    = ttCliente.IdClase
            Cliente.Id-ciudad      = ttCliente.IdCiudad
            Cliente.CP             = ttCliente.CP
            Cliente.Colonia        = ttCliente.Colonia
            Cliente.Calle          = ttCliente.Calle
            Cliente.CalleNo        = ttCliente.Calle + " " + ttCliente.NumInterior + " " + ttCliente.NumExterior
            Cliente.NumExt         = ttCliente.NumExterior
            Cliente.NumInt         = ttCliente.NumInterior
            Cliente.FecReg         = TODAY
            Cliente.Id-Cobrador    = ttCliente.IdCobrador
            Cliente.Id-Resp        = ttCliente.IdResp.

        /* Asignar el ID del nuevo cliente a la salida */
        ASSIGN 
            l-Mensaje = "Cliente creado exitosamente con ID: " + STRING(Cliente.Id-Cliente) + " "  + 
                                 STRING(Cliente.RazonSocial) + " " +  STRING(Cliente.FecReg).
    END.   
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetVerCliente:

    DEFINE INPUT  PARAMETER piIdCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttClienteVisor. 

    EMPTY TEMP-TABLE ttClienteVisor.

    /* Buscar cliente por ID */
    FIND FIRST Cliente WHERE Cliente.Id-Cliente = piIdCliente NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE Cliente THEN 
        RETURN. /* Salir si no se encuentra el cliente */
//     ELSE   
    /* Poblar la tabla temporal con los datos del cliente */
    CREATE ttClienteVisor. 
    ASSIGN
        ttClienteVisor.IdCliente       = Cliente.Id-Cliente
        ttClienteVisor.IdUser          = Cliente.Id-User
        ttClienteVisor.RFC             = Cliente.RFC
        ttClienteVisor.RazonSocial     = Cliente.RazonSocial
        ttClienteVisor.RegimenCapital  = Cliente.RSocietario
        ttClienteVisor.NombreComercial = Cliente.NomEmpresa
        ttClienteVisor.Propietario     = Cliente.Propietario
        ttClienteVisor.Curp            = Cliente.Curp
        ttClienteVisor.Email           = Cliente.e-mail
        ttClienteVisor.Telefono        = Cliente.Tel1
        ttClienteVisor.Telefono2       = Cliente.Tel2
        ttClienteVisor.Telefono3       = Cliente.Tel3           
        ttClienteVisor.BuzonFiscal     = Cliente.BuzonFiscal
        ttClienteVisor.CP              = Cliente.CP
        ttClienteVisor.Colonia         = Cliente.Colonia
        ttClienteVisor.Calle           = Cliente.Calle
        ttClienteVisor.CalleNo         = Cliente.CalleNo
        ttClienteVisor.NumExterior     = Cliente.NumExt
        ttClienteVisor.NumInterior     = Cliente.NumInt
        ttClienteVisor.Tipo            = IF Cliente.Limite >0 THEN 1 ELSE 2
        ttClienteVisor.FecAlta         = Cliente.FecReg.

    /* Consultar y asignar datos relacionados */
    FIND RFiscal WHERE RFiscal.Id-RFiscal = Cliente.Id-RFiscal NO-LOCK NO-ERROR.
    IF AVAILABLE RFiscal THEN ASSIGN    ttClienteVisor.RFiscal = RFiscal.Descr.
    
    FIND UsoCFDI WHERE UsoCFDI.Id-UsoCFDI = Cliente.Id-UsoCFDI NO-LOCK NO-ERROR.
    IF AVAILABLE UsoCFDI THEN ASSIGN ttClienteVisor.UsoCFDI = UsoCFDI.Descr.
    
    
    FIND Ramo WHERE Ramo.Id-Ramo = Cliente.Id-Ramo NO-LOCK NO-ERROR.
    IF AVAILABLE Ramo THEN ASSIGN   ttClienteVisor.Ramo = Ramo.Descr.


    FIND GiroCte WHERE GiroCte.Id-Giro = Cliente.Id-Giro NO-LOCK NO-ERROR.
    IF AVAILABLE GiroCte THEN    ttClienteVisor.Giro = GiroCte.Descr.


    FIND SegmentoCte WHERE SegmentoCte.Id-SegmentoCte = Cliente.Id-SegmentoCte NO-LOCK NO-ERROR.
    IF AVAILABLE SegmentoCte THEN ASSIGN   ttClienteVisor.SegmentoCte = SegmentoCte.Descr.


    FIND ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
    IF AVAILABLE ClaseCte THEN ASSIGN   ttClienteVisor.Clase = ClaseCte.Descr. 
        
    FIND Ciudad WHERE Ciudad.Id-Ciudad = Cliente.Id-ciudad NO-LOCK NO-ERROR.
    IF AVAILABLE Ciudad THEN
    DO: 
        ASSIGN 
            ttClienteVisor.Ciudad = Ciudad.Nombre.
        FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
        IF AVAILABLE Estado THEN
        DO:
            ASSIGN 
                ttClienteVisor.Estado = Estado.Nombre.
            FIND Pais WHERE Pais.Id-Pais = Estado.Id-Pais NO-LOCK NO-ERROR.
            IF AVAILABLE Pais THEN ASSIGN ttClienteVisor.Pais = Pais.Nombre.        
        END.
    END.       
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PutModificarCliente:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /* Parámetro de entrada: tabla temporal desde el body */
    DEFINE INPUT PARAMETER TABLE FOR ttCliente.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.
    
        
    /* Si no hay registros en la tabla temporal, devolver un mensaje */
    IF NOT CAN-FIND(FIRST ttCliente) THEN 
    DO:
        ASSIGN 
            l-Mensaje = "No se enviaron datos de clientes para procesar.".
        RETURN.
    END.
    /* Recorrer cada registro de la tabla temporal */
    FOR EACH ttCliente EXCLUSIVE-LOCK:  
        
        /* Validar Razon Social duplicada */
        FIND FIRST Cliente WHERE Cliente.RazonSocial = ttCliente.RazonSocial NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Cliente ya registrado con la razón social: " + ttCliente.RazonSocial.
            RETURN.
        END.

        /* Validar Email duplicado */
        FIND FIRST Cliente WHERE Cliente.e-mail = ttCliente.Email NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Cliente ya registrado con el email: " + ttCliente.Email.
            RETURN.
        END.
        
        /* Validar RFC duplicado */
        FIND FIRST Cliente WHERE Cliente.RFC = ttCliente.RFC NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Cliente ya registrado con el RFC: " + ttCliente.RFC.
            RETURN.
        END.
        
        /* Buscar si el cliente ya existe */
        FIND FIRST bCliente WHERE bCliente.Id-Cliente = ttCliente.IdCliente 
            EXCLUSIVE-LOCK NO-ERROR.

        /* Si el cliente no existe, ignorar el registro */
        IF NOT AVAILABLE bCliente THEN
            NEXT.

        /* Actualizar solo los campos con información */
        IF ttCliente.IdUser <> "" THEN 
            bCliente.Id-User = ttCliente.IdUser.  

        IF ttCliente.Tipo <> 0 THEN 
            bCliente.Tipo = ttCliente.Tipo.

        IF ttCliente.RFC <> "" THEN 
            bCliente.RFC = ttCliente.RFC.

        IF ttCliente.RazonSocial <> "" THEN 
            bCliente.RazonSocial = ttCliente.RazonSocial.

        IF ttCliente.RegimenCapital <> "" THEN 
            bCliente.RSocietario = ttCliente.RegimenCapital.

        IF ttCliente.NombreComercial <> "" THEN 
            bCliente.NomEmpresa = ttCliente.NombreComercial.

        IF ttCliente.Propietario <> "" THEN 
            bCliente.Propietario = ttCliente.Propietario.

        IF ttCliente.Curp <> "" THEN 
            bCliente.Curp = ttCliente.Curp.

        IF ttCliente.Email <> "" THEN 
            bCliente.e-mail = ttCliente.Email.

        IF ttCliente.Telefono <> "" THEN 
            bCliente.Tel1 = ttCliente.Telefono.
            
        IF ttCliente.Telefono2 <> "" THEN 
            bCliente.Tel2 = ttCliente.Telefono2.
                   
        IF ttCliente.Telefono3 <> "" THEN 
            bCliente.Tel3 = ttCliente.Telefono3.

        IF ttCliente.BuzonFiscal <> "" THEN 
            bCliente.BuzonFiscal = ttCliente.BuzonFiscal.

        IF ttCliente.IdRFiscal <> "" THEN 
            bCliente.Id-RFiscal = ttCliente.IdRFiscal.

        IF ttCliente.IdUsoCFDI <> "" THEN 
            bCliente.Id-UsoCFDI = ttCliente.IdUsoCFDI.

        IF ttCliente.IdRamo <> "" THEN 
            bCliente.Id-Ramo = ttCliente.IdRamo.

        IF ttCliente.IdGiro <> 0 THEN 
            bCliente.Id-Giro = ttCliente.IdGiro.

        IF ttCliente.IdSegmentoCte <> 0 THEN 
            bCliente.Id-SegmentoCte = ttCliente.IdSegmentoCte.

        IF ttCliente.IdClase <> 0 THEN 
            bCliente.Id-ClaseCte = ttCliente.IdClase.

        IF ttCliente.IdCiudad <> 0 THEN 
            bCliente.Id-ciudad = ttCliente.IdCiudad.

        IF ttCliente.CP <> "" THEN 
            bCliente.CP = ttCliente.CP.

        IF ttCliente.Colonia <> "" THEN 
            bCliente.Colonia = ttCliente.Colonia.

        IF ttCliente.Calle <> "" THEN 
            bCliente.Calle = ttCliente.Calle.

        IF ttCliente.NumExterior <> "" THEN 
            bCliente.NumExt = ttCliente.NumExterior.

        IF ttCliente.NumInterior <> "" THEN 
            bCliente.NumInt = ttCliente.NumInterior.

        IF ttCliente.FecReg <> ? THEN 
            bCliente.FecReg = ttCliente.FecReg.

        IF ttCliente.IdCobrador <> 0 THEN 
            bCliente.Id-Cobrador = ttCliente.IdCobrador.

        IF ttCliente.IdResp <> 0 THEN 
            bCliente.Id-Resp = ttCliente.IdResp.
            
        /* Asignar el ID del cliente Modificado */
        ASSIGN 
            l-Mensaje = "Cliente modificado exitosamente con ID: " + STRING(bCliente.Id-Cliente) + " "  + 
                                 STRING(bCliente.RazonSocial).       
    END.
END PROCEDURE.

//
//PROCEDURE GuardaBitacora:
///*------------------------------------------------------------------------------
// Purpose:
// Notes:
//------------------------------------------------------------------------------*/
//DEFINE INPUT PARAMETER l-Tipo AS CHARACTER NO-UNDO.
//    
//    DEFINE VARIABLE l-Campo      AS CHARACTER NO-UNDO.
//    DEFINE VARIABLE l-ValorNuevo AS CHARACTER NO-UNDO.
//    DEFINE VARIABLE l-ValorOld   AS CHARACTER NO-UNDO.
//
//    DEFINE VARIABLE l-NumCampo AS INTEGER NO-UNDO.
//    DEFINE VARIABLE l-j        AS INTEGER NO-UNDO.
//    
//    MESSAGE 'GuardaBitacora ' l-Tipo.
//    
//    IF l-Tipo = "NUEVO" THEN DO:
//        DO l-j = 1 TO 27:
//            CASE l-j:
//                WHEN 1 THEN
//                    ASSIGN 
//                        l-Campo = "Tipo"
//                        l-ValorNuevo = STRING(ttCliente.Tipo)
//                        l-ValorOld = ""
//                        l-NumCampo = 2.
//                WHEN 2 THEN
//                    ASSIGN 
//                        l-Campo = "RFC"
//                        l-ValorNuevo = ttCliente.RFC
//                        l-ValorOld = ""
//                        l-NumCampo = 4.
//                WHEN 3 THEN
//                    ASSIGN 
//                        l-Campo = "CURP"
//                        l-ValorNuevo = ttCliente.CURP
//                        l-ValorOld = ""
//                        l-NumCampo = 54.
//                WHEN 4 THEN
//                    ASSIGN 
//                        l-Campo = "RazonSocial"
//                        l-ValorNuevo = ttCliente.RazonSocial
//                        l-ValorOld = ""
//                        l-NumCampo = 1.
//                WHEN 5 THEN
//                    ASSIGN 
//                        l-Campo = "ApP"
//                        l-ValorNuevo = ttCliente.ApP
//                        l-ValorOld = ""
//                        l-NumCampo = 60.
//                WHEN 6 THEN
//                    ASSIGN 
//                        l-Campo = "ApM"
//                        l-ValorNuevo = ttCliente.ApM
//                        l-ValorOld = ""
//                        l-NumCampo = 70.
//                WHEN 7 THEN
//                    ASSIGN 
//                        l-Campo = "Nombre"
//                        l-ValorNuevo = ttCliente.Nombre
//                        l-ValorOld = ""
//                        l-NumCampo = 80.
//                WHEN 8 THEN
//                    ASSIGN 
//                        l-Campo = "Propietario"
//                        l-ValorNuevo = ttCliente.Propietario
//                        l-ValorOld = ""
//                        l-NumCampo = 3.
//                WHEN 9 THEN
//                    ASSIGN 
//                        l-Campo = "CP"
//                        l-ValorNuevo = ttCliente.CP
//                        l-ValorOld = ""
//                        l-NumCampo = 8.
//                WHEN 10 THEN
//                    ASSIGN 
//                        l-Campo = "Calle"
//                        l-ValorNuevo = ttCliente.Calle
//                        l-ValorOld = ""
//                        l-NumCampo = 5.
//                WHEN 11 THEN
//                    ASSIGN 
//                        l-Campo = "NumExt"
//                        l-ValorNuevo = ttCliente.NumExt
//                        l-ValorOld = ""
//                        l-NumCampo = 100.
//                WHEN 12 THEN
//                    ASSIGN 
//                        l-Campo = "NumInt"
//                        l-ValorNuevo = ttCliente.NumInt
//                        l-ValorOld = ""
//                        l-NumCampo = 110.
//                WHEN 13 THEN
//                    ASSIGN 
//                        l-Campo = "TipoCol"
//                        l-ValorNuevo = STRING(ttCliente.TipoCol)
//                        l-ValorOld = ""
//                        l-NumCampo = 90.
//                WHEN 14 THEN
//                    ASSIGN 
//                        l-Campo = "Colonia"
//                        l-ValorNuevo = ttCliente.Colonia
//                        l-ValorOld = ""
//                        l-NumCampo = 6.
//                WHEN 15 THEN
//                    ASSIGN 
//                        l-Campo = "Id-Ciudad"
//                        l-ValorNuevo = STRING(ttCliente.Id-Ciudad)
//                        l-ValorOld = ""
//                        l-NumCampo = 7.
//                WHEN 16 THEN
//                    ASSIGN 
//                        l-Campo = "Tel1"
//                        l-ValorNuevo = ttCliente.Tel1
//                        l-ValorOld = ""
//                        l-NumCampo = 10.
//                WHEN 17 THEN
//                    ASSIGN 
//                        l-Campo = "Tel2"
//                        l-ValorNuevo = ttCliente.Tel2
//                        l-ValorOld = ""
//                        l-NumCampo = 11.
//                WHEN 18 THEN
//                    ASSIGN 
//                        l-Campo = "Tel3"
//                        l-ValorNuevo = ttCliente.Tel3
//                        l-ValorOld = ""
//                        l-NumCampo = 12.
//                WHEN 19 THEN
//                    ASSIGN 
//                        l-Campo = "Id-RFiscal"
//                        l-ValorNuevo = ttCliente.IdRFiscal
//                        l-ValorOld = ""
//                        l-NumCampo = 14.
//                WHEN 20 THEN
//                    ASSIGN 
//                        l-Campo = "Id-Ramo"
//                        l-ValorNuevo = STRING(ttCliente.Id-Ramo)
//                        l-ValorOld = ""
//                        l-NumCampo = 34.
//                WHEN 21 THEN
//                    ASSIGN 
//                        l-Campo = "Id-Giro"
//                        l-ValorNuevo = STRING(ttCliente.Id-Giro)
//                        l-ValorOld = ""
//                        l-NumCampo = 33.
//                WHEN 22 THEN
//                    ASSIGN 
//                        l-Campo = "Id-Vendedor"
//                        l-ValorNuevo = ttCliente.Id-Vendedor
//                        l-ValorOld = ""
//                        l-NumCampo = 36.
//                WHEN 23 THEN
//                    ASSIGN 
//                        l-Campo = "Id-UsoCFDI"
//                        l-ValorNuevo = ttCliente.Id-UsoCFDI
//                        l-ValorOld = ""
//                        l-NumCampo = 19.
//                WHEN 24 THEN
//                    ASSIGN 
//                        l-Campo = "e-Mail"
//                        l-ValorNuevo = ttCliente.e-Mail
//                        l-ValorOld = ""
//                        l-NumCampo = 48.
//                WHEN 25 THEN
//                    ASSIGN 
//                        l-Campo = "BuzonFiscal"
//                        l-ValorNuevo = ttCliente.BuzonFiscal
//                        l-ValorOld = ""
//                        l-NumCampo = 24.
//                WHEN 26 THEN
//                    ASSIGN 
//                        l-Campo = "NomEmpresa"
//                        l-ValorNuevo = ttCliente.NomEmpresa
//                        l-ValorOld = ""
//                        l-NumCampo = 56.
//                WHEN 27 THEN
//                    ASSIGN 
//                        l-Campo = "RSocietario"
//                        l-ValorNuevo = ttCliente.RSocietario
//                        l-ValorOld = ""
//                        l-NumCampo = 57.
//            END CASE.
//            CREATE CambioCte.
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = l-Campo
//                CambioCte.ValorNuevo = l-ValorNuevo
//                CambioCte.ValorOld   = l-ValorOld
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = l-NumCampo.
//        END. 
//    END.
//    ELSE DO:
//        IF Cliente.Tipo <> ttCliente.Tipo THEN DO:
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Tipo"
//                CambioCte.ValorNuevo = STRING(ttCliente.Tipo)
//                CambioCte.ValorOld   = STRING(Cliente.Tipo)
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 2.
//        END.
//        IF Cliente.RFC <> ttCliente.RFC THEN DO:        
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "RFC"
//                CambioCte.ValorNuevo = ttCliente.RFC
//                CambioCte.ValorOld   = Cliente.RFC
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 4.
//        END.
//        IF Cliente.CURP <> ttCliente.CURP THEN DO:        
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "CURP"
//                CambioCte.ValorNuevo = ttCliente.CURP
//                CambioCte.ValorOld   = Cliente.CURP
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 54.
//        END.
//        IF Cliente.RazonSocial <> ttCliente.RazonSocial THEN DO: 
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "RazonSocial"
//                CambioCte.ValorNuevo = ttCliente.RazonSocial
//                CambioCte.ValorOld   = Cliente.RazonSocial
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 1.
//        END.
//        IF Cliente.NomEmpresa <> ttCliente.NomEmpresa THEN DO: 
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "NomEmpresa"
//                CambioCte.ValorNuevo = ttCliente.NomEmpresa
//                CambioCte.ValorOld   = Cliente.NomEmpresa
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 56.
//        END.
//        IF Cliente.RSocietario <> ttCliente.RSocietario THEN DO: 
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "RSocietario"
//                CambioCte.ValorNuevo = ttCliente.RSocietario
//                CambioCte.ValorOld   = Cliente.RSocietario
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 57.
//        END.
//        IF Cliente.ApP <> ttCliente.ApP THEN DO:         
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "ApP"
//                CambioCte.ValorNuevo = ttCliente.ApP
//                CambioCte.ValorOld   = Cliente.ApP
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 60.
//        END.
//        IF Cliente.ApM <> ttCliente.ApM THEN DO:         
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "ApM"
//                CambioCte.ValorNuevo = ttCliente.ApM
//                CambioCte.ValorOld   = Cliente.ApM
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 70.
//        END.
//        IF Cliente.Nombre <> ttCliente.Nombre THEN DO:      
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Nombre"
//                CambioCte.ValorNuevo = ttCliente.Nombre
//                CambioCte.ValorOld   = Cliente.Nombre
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 80.
//        END.
//        IF Cliente.Propietario <> ttCliente.Propietario THEN DO: 
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Propietario"
//                CambioCte.ValorNuevo = ttCliente.Propietario
//                CambioCte.ValorOld   = Cliente.Propietario
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 3.
//        END.
//        IF Cliente.CP <> ttCliente.CP THEN DO:          
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "CP"
//                CambioCte.ValorNuevo = ttCliente.CP
//                CambioCte.ValorOld   = Cliente.CP
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 8.
//        END.
//        IF Cliente.Calle <> ttCliente.Calle THEN DO:       
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Calle"
//                CambioCte.ValorNuevo = ttCliente.Calle
//                CambioCte.ValorOld   = Cliente.Calle
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 5.
//        END.
//        IF Cliente.NumExt <> ttCliente.NumExt THEN DO:      
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "NumExt"
//                CambioCte.ValorNuevo = ttCliente.NumExt
//                CambioCte.ValorOld   = Cliente.NumExt
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 100.
//        END.
//        IF Cliente.NumInt <> ttCliente.NumInt THEN DO:      
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "NumInt"
//                CambioCte.ValorNuevo = ttCliente.NumInt
//                CambioCte.ValorOld   = Cliente.NumInt
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 110.
//        END.
//        IF Cliente.TipoCol <> ttCliente.TipoCol THEN DO:     
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "TipoCol"
//                CambioCte.ValorNuevo = STRING(ttCliente.TipoCol)
//                CambioCte.ValorOld   = STRING(Cliente.TipoCol)
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 90.
//        END.
//        IF Cliente.Colonia <> ttCliente.Colonia THEN DO:      
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Colonia"
//                CambioCte.ValorNuevo = ttCliente.Colonia
//                CambioCte.ValorOld   = Cliente.Colonia
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 6.
//        END.
//        IF Cliente.Id-Ciudad <> ttCliente.Id-Ciudad THEN DO:
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Id-Ciudad"
//                CambioCte.ValorNuevo = STRING(ttCliente.Id-Ciudad)
//                CambioCte.ValorOld   = STRING(Cliente.Id-Ciudad)
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 7.
//        END.
//        IF Cliente.Tel1 <> ttCliente.Tel1 THEN DO:        
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Tel1"
//                CambioCte.ValorNuevo = ttCliente.Tel1
//                CambioCte.ValorOld   = Cliente.Tel1
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 10.
//        END.
//        IF Cliente.Tel2 <> ttCliente.Tel2 THEN DO:        
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Tel2"
//                CambioCte.ValorNuevo = ttCliente.Tel2
//                CambioCte.ValorOld   = Cliente.Tel2
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 11.
//        END.
//        IF Cliente.Tel3 <> ttCliente.Tel3 THEN DO:        
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Tel3"
//                CambioCte.ValorNuevo = ttCliente.Tel3
//                CambioCte.ValorOld   = Cliente.Tel3
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 12.
//        END.
//        IF Cliente.Id-RFiscal <> ttCliente.IdRFiscal THEN DO:        
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Id-RFiscal"
//                CambioCte.ValorNuevo = ttCliente.IdRFiscal
//                CambioCte.ValorOld   = Cliente.Id-RFiscal
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 14.
//        END.
//        IF Cliente.Id-Ramo <> ttCliente.Id-Ramo THEN DO:     
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Id-Ramo"
//                CambioCte.ValorNuevo = STRING(ttCliente.Id-Ramo)
//                CambioCte.ValorOld   = STRING(Cliente.Id-Ramo)
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 34.
//        END.
//        IF Cliente.Id-Giro <> ttCliente.Id-Giro THEN DO:     
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Id-Giro"
//                CambioCte.ValorNuevo = STRING(ttCliente.Id-Giro)
//                CambioCte.ValorOld   = STRING(Cliente.Id-Giro)
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 33.
//        END.
//        IF Cliente.Id-Vendedor <> ttCliente.Id-Vendedor THEN DO:
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Id-Vendedor"
//                CambioCte.ValorNuevo = ttCliente.Id-Vendedor
//                CambioCte.ValorOld   = Cliente.Id-Vendedor
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 36.
//        END.
//        IF Cliente.Id-UsoCFDI <> ttCliente.Id-UsoCFDI THEN DO:          
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "Id-UsoCFDI"
//                CambioCte.ValorNuevo = ttCliente.Id-UsoCFDI
//                CambioCte.ValorOld   = Cliente.Id-UsoCFDI
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 19.
//        END.
//        IF Cliente.e-Mail <> ttCliente.e-Mail THEN DO:      
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "e-Mail"
//                CambioCte.ValorNuevo = ttCliente.e-Mail
//                CambioCte.ValorOld   = Cliente.e-Mail
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 48.
//        END.
//        IF Cliente.BuzonFiscal <> ttCliente.BuzonFiscal THEN DO:
//            CREATE CambioCte.         
//            ASSIGN
//                CambioCte.Id-Cliente = ttCliente.Id-Cliente
//                CambioCte.Id-User    = ttCliente.Id-User
//                CambioCte.Descr      = "BuzonFiscal"
//                CambioCte.ValorNuevo = ttCliente.BuzonFiscal
//                CambioCte.ValorOld   = Cliente.BuzonFiscal
//                CambioCte.FecReg     = TODAY
//                CambioCte.Hora       = TIME
//                CambioCte.Campo      = 24.
//        END.
//    END.
//    RELEASE CambioCte.
//END PROCEDURE. 