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
    LIKE SolCred. /* Incluye todos los campos, incluido FecReg */

/* Tabla temporal para salida */ 
DEFINE TEMP-TABLE ttPrueba NO-UNDO
    LIKE SolCred. /* Incluye todos los campos, incluido FecReg */

/* Buffer para trabajar con la tabla persistente */
DEFINE BUFFER bfSolCred FOR SolCred.

/* Variables */
DEFINE VARIABLE totalProcesado AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE errores        AS INTEGER NO-UNDO INITIAL 0.

/* ***************************  Main Block **************************** */

/* --------------------------------------------------------------------------*/
PROCEDURE CreaSolCreds:
/*--------------------------------------------------------------------------  
 Purpose     : Crear solicitudes de crédito, asignar IDs únicos y registrar resultados.
 Notes       : 
 - Genera primero el folio único y lo graba en la tabla `SolCred`.
 - Posteriormente, actualiza el registro con los datos proporcionados.
--------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER TABLE FOR ttSolCred. /* Tabla temporal de entrada */
    DEFINE OUTPUT PARAMETER TABLE FOR ttPrueba. /* Tabla temporal de salida */

    /* Variables internas */
    DEFINE VARIABLE lEncontrado AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iID AS INTEGER NO-UNDO.

    /* Inicializar la tabla temporal de salida */
    EMPTY TEMP-TABLE ttPrueba.

    /* Iterar sobre la tabla temporal de entrada */
    FOR EACH ttSolCred WHERE ttSolCred.IdSolicitud = 0:
        /* Generar un ID único */
        DO TRANSACTION:
            ASSIGN lEncontrado = FALSE.

            DO iID = 1 TO 90000:
                /* Verificar si el ID ya existe en la tabla SolCred */
                FIND bfSolCred WHERE bfSolCred.IdSolicitud = iID NO-LOCK NO-ERROR.
                IF NOT AVAILABLE bfSolCred THEN DO:
                    /* Crear un registro vacío en SolCred con el folio único */
                    CREATE bfSolCred.
                    ASSIGN 
                        bfSolCred.IdSolicitud = iID
                        bfSolCred.FecReg      = TODAY. /* Fecha de creación */
                    
                    /* Registrar el ID único en la tabla temporal */
                    ASSIGN 
                        ttSolCred.IdSolicitud = iID
                        ttSolCred.FecReg      = TODAY.

                    /* Confirmar que se encontró un ID válido */
                    ASSIGN lEncontrado = TRUE.

                    /* Salir del bucle al encontrar un ID disponible */
                    LEAVE.
                END.
            END.

            /* Validar si no se encontró un ID disponible */
            IF NOT lEncontrado THEN DO:
                RETURN ERROR "No se pudo generar un ID único para la solicitud.".
            END.

            /* Liberar buffer */
            RELEASE bfSolCred.
        END.

        /* Alimentar la tabla `SolCred` con los datos de la solicitud */
        DO TRANSACTION:
            FIND FIRST bfSolCred WHERE bfSolCred.IdSolicitud = ttSolCred.IdSolicitud EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE bfSolCred THEN DO:
                /* Copiar los datos restantes desde la tabla temporal */
                BUFFER-COPY ttSolCred TO bfSolCred.

                /* Registrar el dato procesado en la tabla temporal de salida */
                CREATE ttPrueba.
                BUFFER-COPY bfSolCred TO ttPrueba.
            END.

            /* Liberar buffer */
            RELEASE bfSolCred.
        END.

        /* Liberar el registro procesado */
        RELEASE ttSolCred.
    END.

    /* Liberar buffers generales */
    RELEASE bfSolCred.

END PROCEDURE.

/* Procedimiento GET para consultar una solicitud de crédito */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultaSolCred:
    /*--------------------------------------------------------------------------  
     Purpose     : Consultar una solicitud de crédito por IdSolicitud.
     Notes       : Devuelve todos los campos de la tabla SolCred en una tabla temporal.
    --------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER IdSolicitud AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttSolCred.
    DEFINE OUTPUT PARAMETER Respuesta AS CHARACTER NO-UNDO.
    EMPTY TEMP-TABLE ttSolCred.
    /* Buscar el registro en SolCred */
    FIND FIRST bfSolCred WHERE bfSolCred.IdSolicitud = IdSolicitud NO-LOCK NO-ERROR.

    /* Validar si el registro existe */
    IF NOT AVAILABLE bfSolCred THEN 
    DO:
        ASSIGN 
            Respuesta = "La solicitud con ID " + STRING(IdSolicitud) + " no existe.".
        RETURN.
    END.

    /* Crear un registro en la tabla temporal de salida */
    CREATE ttSolCred.
    BUFFER-COPY bfSolCred TO ttSolCred.

    /* Liberar buffer */
    RELEASE bfSolCred.   
END PROCEDURE.
