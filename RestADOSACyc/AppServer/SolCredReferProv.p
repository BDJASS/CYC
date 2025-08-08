@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : SolCredReferProv.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Mon Jan 06 16:37:01 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* Tabla temporal para entrada (POST) */
DEFINE TEMP-TABLE ttReferProv NO-UNDO
    FIELD Num             LIKE ReferProv.Num
    FIELD IdSolicitud     LIKE ReferProv.IdSolicitud
    FIELD Revisada        LIKE ReferProv.Revisada
    FIELD Observaciones   LIKE ReferProv.Observaciones.


/* Tabla temporal para salida (POST y GET) */
DEFINE TEMP-TABLE ttReferProvOut NO-UNDO
    FIELD Num             LIKE ReferProv.Num
    FIELD IdSolicitud     LIKE ReferProv.IdSolicitud
    FIELD Revisada        LIKE ReferProv.Revisada 
    FIELD Observaciones   LIKE ReferProv.Observaciones.
/* Buffer para la tabla persistente */
DEFINE BUFFER bfReferProv FOR ReferProv.

/* ***************************  Main Procedures **************************** */

/* -------------------------------------------------------------------------- */
/* POST: Servicio para actualizar los datos de crédito en la tabla Cliente */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ActualizarReferenciasProv:
    DEFINE INPUT  PARAMETER TABLE FOR ttReferProv.  /* Tabla temporal de entrada */
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttReferProvOut.

    /* Procesar cada registro en la tabla temporal */
    FOR EACH ttReferProv:
        /* Validar que el cliente exista en la tabla Cliente */
        FIND FIRST bfReferProv WHERE bfReferProv.IdSolicitud = ttReferProv.IdSolicitud 
                                 AND bfReferProv.Num         = ttReferProv.Num EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE bfReferProv THEN DO:
            ASSIGN l-Mensaje = "La Solicitud " + STRING(ttReferProv.IdSolicitud) + " no existe con Numero de Referencia " +
                          STRING(ttReferProv.Num).
            RETURN.
        END.

        /* Actualizar los campos en la tabla persistente */
        ASSIGN
              bfReferProv.Revisada = ttReferProv.Revisada.

        /* Copiar los datos actualizados a la tabla temporal de salida */
        CREATE ttReferProvOut.
        ASSIGN
            ttReferProvOut.IdSolicitud   = bfReferProv.IdSolicitud
            ttReferProvOut.Num           = bfReferProv.Num
            ttReferProvOut.Revisada      = bfReferProv.Revisada
            ttReferProvOut.Observaciones = bfReferProv.Observaciones.  
    END.

    /* Liberar buffer */
    RELEASE bfReferProv.        
END PROCEDURE.

