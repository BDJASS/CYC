@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : DocCredito.p
    URL         : /DocumentoCredito
    Module      : Gestión Crédito
    Purpose     : Servicio REST para gestionar documentos asociados a créditos.
    Sprint      : 3
    Author(s)   : sis10
    Created     : (Coloca la fecha de creación aquí)
    Notes       : Este servicio permite asignar folios únicos a solicitudes
                  de crédito y gestionar el almacenamiento de documentos PDF
                  relacionados con los clientes de crédito.
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttDocReq NO-UNDO
    FIELD IdSolicitud   AS INTEGER
    FIELD TipoDoc       AS CHAR
    FIELD NombreArchivo AS CHAR
    FIELD PDF           AS BLOB. /* El campo BLOB para el archivo binario */

DEFINE DATASET dsDocReq FOR ttDocReq.

DEFINE TEMP-TABLE ttFinal NO-UNDO
    FIELD IdSolicitud   AS INTEGER  
    FIELD TipoDoc       AS CHAR
    FIELD NombreArchivo AS CHAR 
    FIELD PDF           AS BLOB. /* El campo BLOB para el archivo binario */

DEFINE BUFFER bDocReq FOR DocReq.
DEFINE BUFFER bfSolCred FOR SolCred. /* Buffer para trabajar con SolCred */

DEFINE VARIABLE lNuevoFolio AS INTEGER NO-UNDO. /* Variable para generar nuevo folio */

/* ***************************  Main Block **************************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GuardarDocumentoCredito:

    DEFINE INPUT PARAMETER TABLE FOR ttDocReq.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFinal.

    EMPTY TEMP-TABLE ttFinal.

    /* Procesar cada registro recibido en ttDocReq */
    FOR EACH ttDocReq:

        /* Buscar el IdSolicitud en SolCred */
        FIND FIRST bfSolCred WHERE bfSolCred.IdSolicitud = ttDocReq.IdSolicitud NO-LOCK NO-ERROR.
        IF AVAILABLE bfSolCred THEN DO:
            IF bfSolCred.IdDocumento = 0 THEN DO:
                /* Generar y reservar un nuevo folio único */
                lNuevoFolio = 0. /* Reiniciar el folio temporal */
                DO lNuevoFolio = 1 TO 99999:
                    FIND FIRST DocReq WHERE DocReq.IdSolicitud = lNuevoFolio NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE DocReq THEN DO:
                        /* Reservar el folio creando un registro vacío en DocReq */
                        CREATE bDocReq.
                        ASSIGN bDocReq.IdSolicitud = lNuevoFolio.
                        LEAVE. /* Salir del bucle al encontrar un folio disponible */
                    END.
                END.

                /* Asignar el nuevo folio a SolCred */
                FIND bfSolCred WHERE bfSolCred.IdSolicitud = ttDocReq.IdSolicitud EXCLUSIVE-LOCK.
                ASSIGN bfSolCred.IdDocumento = lNuevoFolio.

                /* Liberar el buffer de bfSolCred */
                RELEASE bfSolCred.
            END.
        END.

        /* Si no se encuentra el IdSolicitud en SolCred, ignorar el registro */
        ELSE NEXT.

        /* Obtener el folio final (existente o generado) */
        DEFINE VARIABLE FolioFinal AS INTEGER NO-UNDO.
        ASSIGN FolioFinal = bfSolCred.IdDocumento.

        /* Verificar si ya existe un registro en DocReq con el mismo IdSolicitud y TipoDoc */
        FIND FIRST DocReq 
            WHERE DocReq.IdSolicitud = FolioFinal
              AND DocReq.TipoDoc = ttDocReq.TipoDoc 
            EXCLUSIVE-LOCK NO-ERROR.

        IF AVAILABLE DocReq THEN DO:
            /* Si el registro ya existe, actualizar los campos */
            ASSIGN
                DocReq.NombreArchivo = ttDocReq.NombreArchivo.

            /* Sobrescribir el contenido del campo PDF */
            COPY-LOB FROM ttDocReq.PDF TO DocReq.PDF.

            /* Liberar el buffer de DocReq */
            RELEASE DocReq.
        END.
        ELSE DO:
            /* Si el registro no existe, crearlo */
            CREATE bDocReq.
            ASSIGN
                bDocReq.IdSolicitud   = FolioFinal
                bDocReq.TipoDoc       = ttDocReq.TipoDoc
                bDocReq.NombreArchivo = ttDocReq.NombreArchivo.

            /* Copiar el contenido del campo PDF */
            COPY-LOB FROM ttDocReq.PDF TO bDocReq.PDF.

            /* Liberar el buffer de bDocReq */
            RELEASE bDocReq.
        END.

        /* Copiar los datos procesados a ttFinal */
        CREATE ttFinal.
        BUFFER-COPY ttDocReq TO ttFinal.
        ASSIGN
            ttFinal.IdSolicitud = FolioFinal.

        /* Liberar el registro procesado de ttDocReq */
        RELEASE ttDocReq.
    END.

    /* Liberar buffers generales al final */
    RELEASE bDocReq.
    RELEASE bfSolCred.

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultaDocumentosCredito:
/*--------------------------------------------------------------------------  
 Purpose     : Consultar los documentos asociados a una solicitud de crédito.
 Notes       : Devuelve los registros correspondientes en una tabla temporal.
--------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER IdSolicitud AS INTEGER NO-UNDO. /* ID de la solicitud */
    DEFINE OUTPUT PARAMETER TABLE FOR ttDocReq.            /* Tabla temporal de salida */

    EMPTY TEMP-TABLE ttDocReq.

    /* Buscar registros en DocReq asociados al IdSolicitud */
    FOR EACH bDocReq WHERE bDocReq.IdSolicitud = IdSolicitud NO-LOCK:
        /* Copiar los datos encontrados a la tabla temporal de salida */
        CREATE ttDocReq.
        BUFFER-COPY bDocReq TO ttDocReq. 
    END.
    /* Liberar buffers */
    RELEASE bDocReq.
END PROCEDURE.
