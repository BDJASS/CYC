@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : ClienteCreAgenda.p
    Purpose     : Servicios para guardar y consultar información de seguimiento de acuerdos en AgCliente.
    URL         : /ClienteCreditoAgenda
    Module      : Gestión de Seguimiento de Clientes
    Author(s)   : sis10
    Created     : (Fecha actual)
    Notes       :
                  - POST: Guarda nuevos registros en la tabla AgCliente a partir de una tabla temporal.
                  - GET: Consulta registros de AgCliente basados en IdCliente e IdUser.
------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.   

/* Tabla temporal para entrada y salida */
DEFINE TEMP-TABLE ttSeguimiento NO-UNDO
    FIELD Fecha              AS DATE       /* Fecha del registro */
    FIELD SeguimientoAcuerdo AS CHARACTER  /* Comentario del seguimiento */
    FIELD IdCliente          AS INTEGER   /* ID del cliente */
    FIELD IdUser             AS CHARACTER. /* ID del usuario */ 
    
    
DEFINE TEMP-TABLE ttAgenda NO-UNDO
    FIELD Fecha              AS DATE       /* Fecha del registro */
    FIELD SeguimientoAcuerdo AS CHARACTER  /* Comentario del seguimiento */
    FIELD NombreRegistro     AS CHARACTER  /* Nombre del usuario que registró */
    FIELD IdCliente          AS INTEGER   /* ID del cliente */
    FIELD IdUser             AS CHARACTER. /* ID del usuario */     

/* Buffers para tablas persistentes */
DEFINE BUFFER bfAgCliente FOR AgCliente.
DEFINE BUFFER bfUsuario   FOR Usuario.
DEFINE BUFFER bfCliente   FOR Cliente. 



/* **********************  Internal Procedures  *********************** */




/* ***************************  Main Block **************************** */

/* -------------------------------------------------------------------------- */
/* POST: Servicio para guardar nuevos registros en la tabla AgCliente */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GuardarSeguimientos:
    DEFINE INPUT  PARAMETER TABLE FOR ttSeguimiento. /* Tabla temporal de entrada */
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.

    /* Procesar cada registro en la tabla temporal */
    FOR EACH ttSeguimiento:
        
        LOG-MANAGER:WRITE-MESSAGE("[GuardarSeguimientos] Iniciando procesamiento del seguimiento. " +
                                  "IdCliente: " + STRING(ttSeguimiento.IdCliente) + 
                                  ", IdUser: " + ttSeguimiento.IdUser + 
                                  ", Fecha: " + STRING(ttSeguimiento.Fecha)).     
                                   
        /* Validar que el cliente exista en la tabla Cliente */
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttSeguimiento.IdCliente NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCliente THEN 
        DO:
            ASSIGN 
                l-Mensaje = "El cliente con ID " + STRING(ttSeguimiento.IdCliente) + " no existe en la base de datos.".
            RETURN.
        END.
        FIND FIRST bfUsuario WHERE bfUsuario.Id-User = ttSeguimiento.IdUser NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfUsuario THEN 
        DO:
            ASSIGN 
                l-Mensaje = "El Usuario " + STRING(ttSeguimiento.IdUser) + " no existe en la base de datos.".
            RETURN.
        END. 
        
        IF ttSeguimiento.Fecha = ? THEN 
        DO:
            ASSIGN 
                l-Mensaje = "Colocar Fecha al Registro".
            RETURN.
        END.   
        /* Si el cliente existe, proceder a guardar el seguimiento */
        /* Crear un nuevo registro en AgCliente */
        DO TRANSACTION:
            CREATE bfAgCliente.
            ASSIGN
                bfAgCliente.Id-Cliente = ttSeguimiento.IdCliente
                bfAgCliente.Coment1    = ttSeguimiento.SeguimientoAcuerdo
                bfAgCliente.Id-User    = ttSeguimiento.IdUser
                bfAgCliente.FecReg     = ttSeguimiento.Fecha.  
            RELEASE bfAgCliente.  
        END.                             
        ASSIGN 
            l-Mensaje = "Seguimiento Registrado de: " + bfUsuario.Nom-Usuario .
    END.
END PROCEDURE.

/* -------------------------------------------------------------------------- */
/* GET: Servicio para consultar registros de AgCliente */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarSeguimiento:
    DEFINE INPUT  PARAMETER IdCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttAgenda.

    /* Inicializar la tabla temporal */
    EMPTY TEMP-TABLE ttAgenda. 

    /* Buscar los registros en AgCliente basados en IdCliente */
    FOR EACH bfAgCliente WHERE bfAgCliente.Id-Cliente = IdCliente NO-LOCK:
        /* Buscar el nombre del usuario en la tabla Usuario */
        DEFINE VARIABLE nombreUsuario AS CHARACTER NO-UNDO.
        FIND FIRST bfUsuario WHERE bfUsuario.Id-User = bfAgCliente.Id-User NO-LOCK NO-ERROR.
        ASSIGN 
            nombreUsuario = IF AVAILABLE bfUsuario THEN bfUsuario.Nom-Usuario ELSE "Desconocido".

        /* Crear el registro en la tabla temporal de salida */
        CREATE ttAgenda.
        ASSIGN
            ttAgenda.Fecha              = bfAgCliente.FecReg
            ttAgenda.SeguimientoAcuerdo = bfAgCliente.Coment1
            ttAgenda.NombreRegistro     = nombreUsuario
            ttAgenda.IdCliente          = bfAgCliente.Id-Cliente
            ttAgenda.IdUser             = bfAgCliente.Id-User. 
    END.

    /* Liberar buffers al final */
    RELEASE bfAgCliente.
    RELEASE bfUsuario.
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE DeleteAgenda:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER TABLE FOR ttSeguimiento. /* Tabla temporal de entrada */
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE l-Contador AS INTEGER NO-UNDO. /* Contador de registros eliminados */

    ASSIGN l-Mensaje = "". 
    /* Procesar cada registro en la tabla temporal */
    FOR EACH ttSeguimiento:
     
        FIND FIRST bfAgCliente WHERE bfAgCliente.Id-Cliente = ttSeguimiento.IdCliente
            AND bfAgCliente.Coment1    = ttSeguimiento.SeguimientoAcuerdo
            AND bfAgCliente.Id-User    = ttSeguimiento.IdUser
            AND bfAgCliente.FecReg     = ttSeguimiento.Fecha EXCLUSIVE-LOCK NO-ERROR.
      
        /* Si se encuentra el registro, eliminarlo */
        IF AVAILABLE(bfAgCliente) THEN 
        DO:
            DELETE bfAgCliente.
            ASSIGN 
                l-Contador = l-Contador + 1.
        END.
        ELSE 
        DO:
            /* Si no se encuentra el registro, agregar un mensaje de error */
            ASSIGN 
                l-Mensaje = l-Mensaje + "No se encontró el registro para el cliente: " 
                          + STRING(ttSeguimiento.IdCliente) + " - " 
                          + ttSeguimiento.SeguimientoAcuerdo + CHR(10).
        END.
        
    END.
    /* Mensaje final con el número de registros eliminados */
    IF l-Contador > 0 THEN
        ASSIGN l-Mensaje = l-Mensaje + "Se eliminaron " + STRING(l-Contador) + " registros correctamente.".
    ELSE
        ASSIGN l-Mensaje = l-Mensaje + "No se eliminó ningún registro.".
END PROCEDURE.
