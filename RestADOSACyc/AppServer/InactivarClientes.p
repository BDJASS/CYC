@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*--------------------------------------------------------------------------  
 File        : InactivarClientes.p
 Purpose     : Gestionar clientes activos e inactivarlos si cumplen con las condiciones.
--------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttClienteInactivo NO-UNDO
    FIELD NumCliente   AS INTEGER    /* Número de Cliente */
    FIELD FechaDeBaja  AS DATE       /* Fecha de Inactivación */
    FIELD Registro     AS CHARACTER. /* Nombre del Usuario que realiza la operación */

/* ***************************  Procedures **************************** */

/* GET: Buscar información del cliente */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetInactivarCliente:
/*--------------------------------------------------------------------------  
 Purpose     : Obtener información del cliente y registrar datos para inactivarlo.
--------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pNumCliente AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pUsuario    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttClienteInactivo.
    DEFINE OUTPUT PARAMETER Respuesta AS CHARACTER NO-UNDO.

    DEFINE BUFFER bCliente FOR Cliente.
    DEFINE BUFFER bUsuario FOR Usuario.

    EMPTY TEMP-TABLE ttClienteInactivo. 

    /* Buscar el cliente con Activo = TRUE */
    FIND FIRST bCliente WHERE bCliente.Id-Cliente = pNumCliente AND bCliente.Activo = TRUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bCliente THEN DO:
        ASSIGN Respuesta = "El cliente con ID " + STRING(pNumCliente) + " no está activo o no existe.".
        RETURN.
    END.

    /* Buscar el Usuario en la tabla Usuario */
    DEFINE VARIABLE nomUsuario AS CHARACTER NO-UNDO.
    FIND FIRST bUsuario WHERE bUsuario.Id-User = pUsuario NO-LOCK NO-ERROR.
    IF AVAILABLE bUsuario THEN
        ASSIGN nomUsuario = bUsuario.Nom-Usuario.
    ELSE
        ASSIGN nomUsuario = pUsuario. /* Usar el valor del Usuario si no se encuentra */

    /* Crear el registro en la tabla temporal */
    CREATE ttClienteInactivo.
    ASSIGN
        ttClienteInactivo.NumCliente  = pNumCliente
        ttClienteInactivo.FechaDeBaja = TODAY
        ttClienteInactivo.Registro    = nomUsuario.
RETURN.        
END PROCEDURE.

/* POST: Inactivar cliente */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostInactivarCliente:
/*--------------------------------------------------------------------------  
 Purpose     : Inactivar un cliente si cumple con las validaciones necesarias.
--------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pNumCliente AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pUsuario    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pMotivo     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER NO-UNDO.

    DEFINE BUFFER bCliente FOR Cliente.
    DEFINE BUFFER bMovCliente FOR MovCliente.
    DEFINE BUFFER bCambioCte FOR CambioCte.

    /* Iniciar una transacción */
    DO TRANSACTION:
        /* Validar que el cliente exista y esté activo */
        FIND FIRST bCliente WHERE bCliente.Id-Cliente = pNumCliente AND bCliente.Activo = TRUE EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE bCliente THEN DO:
            ASSIGN Respuesta = "El cliente con ID " + STRING(pNumCliente) + " no está activo o no existe.".
            RETURN.
        END.

        /* Validar que no tenga saldo pendiente */
        FIND FIRST bMovCliente WHERE bMovCliente.Id-Cliente = bCliente.Id-Cliente
                                AND bMovCliente.Id-Mc <= 3
                                AND bMovCliente.Saldo > 0 NO-LOCK NO-ERROR.
        IF AVAILABLE bMovCliente THEN DO:
            ASSIGN Respuesta = "IMPOSIBLE INACTIVAR UN CLIENTE CON SALDO PENDIENTE.".
            RETURN.
        END.

        /* Cambiar el estatus del cliente a inactivo */
        ASSIGN bCliente.Activo = FALSE
               bCliente.Id-User = pUsuario
               bCliente.FecBaja = TODAY.
        RELEASE bCliente.

        /* Registrar el cambio en la tabla CambioCte */
        CREATE bCambioCte.
        ASSIGN
            bCambioCte.Id-Cliente = pNumCliente
            bCambioCte.Id-User    = pUsuario
            bCambioCte.Descr      = "ACTIVO"
            bCambioCte.valornuevo = "no"
            bCambioCte.valorold   = "yes"
            bCambioCte.fecreg     = TODAY
            bCambioCte.hora       = TIME
            bCambioCte.campo      = 0.
        RELEASE bCambioCte.

        /* Asignar mensaje de éxito */
        ASSIGN Respuesta = "El cliente con ID " + STRING(pNumCliente) + " ha sido inactivado correctamente.".
    END. /* Fin de la transacción */
RETURN.     
END PROCEDURE.
