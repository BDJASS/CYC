@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*--------------------------------------------------------------------------  
 File        : ActivarClientes.p
 Purpose     : Gestionar clientes inactivos y activarlos si cumplen con las condiciones.
--------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttClienteActivo NO-UNDO
    FIELD NumCliente  AS INTEGER    /* Número de Cliente */
    FIELD FechaDeBaja AS DATE       /* Fecha de Baja que tiene */
    FIELD Motivo      AS CHARACTER. /* Nombre del Usuario que realiza la operación */

/* ***************************  Procedures **************************** */

/* GET: Buscar información del cliente */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetActivarCliente:
    /*--------------------------------------------------------------------------  
     Purpose     : Obtener información del cliente y registrar datos para activarlo.
    --------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pNumCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttClienteActivo.
    DEFINE OUTPUT PARAMETER Respuesta AS CHARACTER NO-UNDO.

    DEFINE BUFFER bCliente FOR Cliente.

    EMPTY TEMP-TABLE ttClienteActivo. 

    /* Buscar el cliente con Activo = FALSE */
    FIND FIRST bCliente WHERE bCliente.Id-Cliente = pNumCliente AND bCliente.Activo = FALSE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bCliente THEN 
    DO:
        ASSIGN 
            Respuesta = "El cliente con ID " + STRING(pNumCliente) + " no está inactivo o no existe.".
        RETURN.
    END.

    /* Crear el registro en la tabla temporal */
    CREATE ttClienteActivo.
    ASSIGN
        ttClienteActivo.NumCliente  = pNumCliente
        ttClienteActivo.FechaDeBaja = bCliente.FecBaja 
        ttClienteActivo.Motivo      = bCliente.Motivo.
END PROCEDURE.

/* POST: Activar cliente */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostActivarCliente:
    /*--------------------------------------------------------------------------  
     Purpose     : Activar un cliente si cumple con las validaciones necesarias.
    --------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pNumCliente AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pUsuario    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER NO-UNDO.

    DEFINE BUFFER bCliente   FOR Cliente.
    DEFINE BUFFER bCambioCte FOR CambioCte.
    DEFINE BUFFER bUsuario   FOR Usuario.
     ASSIGN Respuesta = "".
    /* Iniciar una transacción */
    DO TRANSACTION:
        /* Validar que el cliente exista y esté inactivo */
        FIND FIRST bCliente WHERE bCliente.Id-Cliente = pNumCliente AND bCliente.Activo = FALSE EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE bCliente THEN 
        DO:
            ASSIGN 
                Respuesta = "El cliente con ID " + STRING(pNumCliente) + " no está inactivo o no existe.".
            RETURN.
        END.
        
        /* Buscar el Usuario en la tabla Usuario */
        FIND FIRST bUsuario WHERE bUsuario.Id-User = pUsuario NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bUsuario THEN
        DO:
             ASSIGN 
                Respuesta = "El Usuario " + STRING(pUsuario) + " no existe.". 
             RETURN.
                /* Usar el valor del Usuario si no se encuentra */
        END.   
        /* Cambiar el estatus del cliente a activo */
        ASSIGN 
            bCliente.Activo  = TRUE
            bCliente.Id-User = ""
            bCliente.FecBaja = ?.
        RELEASE bCliente.

        /* Registrar el cambio en la tabla CambioCte */
        CREATE bCambioCte.
        ASSIGN
            bCambioCte.Id-Cliente = pNumCliente
            bCambioCte.Id-User    = pUsuario
            bCambioCte.Descr      = "ACTIVO"
            bCambioCte.valornuevo = "yes"
            bCambioCte.valorold   = "no"
            bCambioCte.fecreg     = TODAY
            bCambioCte.hora       = TIME
            bCambioCte.campo      = 0.
        RELEASE bCambioCte.

        /* Asignar mensaje de éxito */
        ASSIGN 
            Respuesta = "El cliente " + STRING(pNumCliente) + " ha sido activado correctamente.".
    END. /* Fin de la transacción */
END PROCEDURE.
