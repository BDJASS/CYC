@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : SolCredObservaciones.p
    Purpose     : Servicios para guardar y consultar observaciones y estado del pagaré firmado en solicitudes de crédito.
    URL         : /SolCredObservaciones
    Module      : Gestión de Créditos
------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */

/* Tabla temporal para entrada */
DEFINE TEMP-TABLE ttSolCred NO-UNDO
    FIELD IdSolicitud     AS INTEGER    /* ID de la solicitud */
    FIELD Observaciones   AS CHARACTER  /* Observaciones de la solicitud */
    FIELD PagareFirmado   AS LOGICAL.   /* Indicador de pagaré firmado */

/* Tabla temporal para salida */
DEFINE TEMP-TABLE ttSolCredOutput NO-UNDO
    FIELD IdSolicitud     AS INTEGER    /* ID de la solicitud */
    FIELD Observaciones   AS CHARACTER  /* Observaciones de la solicitud */
    FIELD PagareFirmado   AS LOGICAL.   /* Indicador de pagaré firmado */

/* Buffer para la tabla persistente */
DEFINE BUFFER bfSolCred FOR SolCred.

/* ***************************  Main Procedures **************************** */

/* -------------------------------------------------------------------------- */
/* POST: Servicio para guardar observaciones y pagaré firmado */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GuardarObservacionesPagare:
    DEFINE INPUT  PARAMETER TABLE FOR ttSolCred.  /* Tabla temporal de entrada */

    EMPTY TEMP-TABLE ttSolCredOutput.

    /* Procesar cada registro en la tabla temporal */
    FOR EACH ttSolCred:
        /* Validar que la solicitud exista en la tabla SolCred */
        FIND FIRST bfSolCred WHERE bfSolCred.IdSolicitud = ttSolCred.IdSolicitud EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE bfSolCred THEN DO:
            RETURN ERROR "La solicitud con ID " + STRING(ttSolCred.IdSolicitud) + " no existe en la base de datos.".
        END.

        /* Actualizar los campos en la tabla persistente */
        ASSIGN
            bfSolCred.Observaciones = ttSolCred.Observaciones
            bfSolCred.PagareFirmado = ttSolCred.PagareFirmado.
    END.

    /* Liberar buffer */ 
    RELEASE bfSolCred. 
    RETURN. 
END PROCEDURE.   
