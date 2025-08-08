@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*--------------------------------------------------------------------------  
 File        : DesbloqueoClientes.p
               /ClienteDesbloqueado
 Purpose     : Desbloquear registros en blkAut basados en el número de cliente.
--------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */



/* Tabla temporal para registrar resultados */
DEFINE TEMP-TABLE ttResultado NO-UNDO
    FIELD NumCliente  AS INTEGER    /* Número de cliente */
    FIELD Accion      AS CHARACTER  /* Acción realizada */
    FIELD Fecha       AS DATE.      /* Fecha de ejecución */

/* Buffer para trabajar con blkAut */
DEFINE BUFFER bfBlkAut FOR blkAut.

/* ***************************  Main Block **************************** */

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostDesbloquear:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER NumCliente AS INTEGER NO-UNDO. /* ID del cliente a desbloquear */
DEFINE OUTPUT PARAMETER TABLE FOR ttResultado. /* Tabla temporal de salida */

EMPTY TEMP-TABLE ttResultado.

/* Validar que el cliente esté bloqueado en blkAut */
FIND FIRST bfBlkAut WHERE bfBlkAut.Id-Cliente = NumCliente EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE bfBlkAut THEN DO:
    /* Registrar en la tabla temporal que no hay bloqueo */
    CREATE ttResultado.
    ASSIGN
        ttResultado.NumCliente = NumCliente
        ttResultado.Accion     = "No hay bloqueo registrado para este cliente"
        ttResultado.Fecha      = TODAY.
    RETURN.
END.

/* Eliminar el registro de blkAut */
DELETE bfBlkAut.

/* Registrar acción de desbloqueo en la tabla temporal */
CREATE ttResultado.
ASSIGN
    ttResultado.NumCliente = NumCliente
    ttResultado.Accion     = "Desbloqueo realizado con éxito"
    ttResultado.Fecha      = TODAY.

/* Liberar buffer */
RELEASE bfBlkAut.
END PROCEDURE.
