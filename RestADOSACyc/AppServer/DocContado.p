@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : DocContado.p
    URL         : /DocumentoContado
    Module      : Gestión de Clientes Contado
    Purpose     : Servicio REST para gestionar documentos asociados a clientes de contado.
    Sprint      : 3
    Author(s)   : sis10
    Created     : (Coloca la fecha de creación aquí)
    Notes       : Este servicio permite asignar folios únicos a solicitudes
                  y gestionar el almacenamiento de documentos PDF para los clientes de contado.
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttDocReq NO-UNDO
    FIELD IdCliente     AS INTEGER
    FIELD TipoDoc         LIKE DocReq.TipoDoc
    FIELD NombreArchivo   LIKE DocReq.NombreArchivo
    FIELD PDF             LIKE DocReq.PDF.  


DEFINE BUFFER bDocReq FOR DocReq.
DEFINE BUFFER bfCliente FOR Cliente.

    /* Variables */
    DEFINE VARIABLE lNuevoFolio AS INTEGER NO-UNDO.
    DEFINE VARIABLE FolioFinal AS INTEGER NO-UNDO.

/* ***************************  Main Block **************************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GuardarDocumento:
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttDocReq.

    /* Procesar cada registro recibido en ttDocReq */
    FOR EACH ttDocReq:
        /* Buscar el cliente */
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttDocReq.IdCliente NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCliente THEN   
            NEXT.  /* Saltar si no existe el cliente */

        /* Manejo de folio */
        IF bfCliente.Id-Documento = 0 THEN DO:
            /* Generar nuevo folio */
            DO lNuevoFolio = 1 TO 99999:
                FIND FIRST DocReq WHERE DocReq.IdSolicitud = lNuevoFolio NO-LOCK NO-ERROR.
                IF NOT AVAILABLE DocReq THEN LEAVE.
            END.

            /* Asignar folio al cliente */
            FIND CURRENT bfCliente EXCLUSIVE-LOCK.
            ASSIGN bfCliente.Id-Documento = lNuevoFolio.
            RELEASE bfCliente.  /* Liberar bloqueo pero mantener buffer */

            /* Re-find para actualizar el buffer */
            FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttDocReq.IdCliente NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bfCliente THEN 
                NEXT.
        END.

        /* Obtener folio actualizado */
        ASSIGN FolioFinal = bfCliente.Id-Documento.

        /* Actualizar/crear DocReq */
        FIND FIRST DocReq 
            WHERE DocReq.IdSolicitud = FolioFinal
              AND DocReq.TipoDoc     = ttDocReq.TipoDoc  
            EXCLUSIVE-LOCK NO-ERROR.

        IF AVAILABLE DocReq THEN 
            ASSIGN DocReq.NombreArchivo = ttDocReq.NombreArchivo.
        ELSE 
            CREATE DocReq.
                ASSIGN
                    DocReq.IdSolicitud   = FolioFinal
                    DocReq.TipoDoc       = ttDocReq.TipoDoc
                    DocReq.NombreArchivo = ttDocReq.NombreArchivo.

        /* Copiar PDF */
        COPY-LOB FROM ttDocReq.PDF TO DocReq.PDF.
        RELEASE DocReq.
    END.
END PROCEDURE.  



@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ObtenerDocumentos:

    DEFINE INPUT  PARAMETER IdCliente AS INTEGER NO-UNDO. 
    DEFINE OUTPUT PARAMETER TABLE FOR ttDocReq.
    DEFINE OUTPUT PARAMETER Mensaje AS CHARACTER NO-UNDO.


    EMPTY TEMP-TABLE ttDocReq.
    ASSIGN Mensaje = "".

    /* Buscar el cliente */
    FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = IdCliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfCliente THEN DO:
        ASSIGN Mensaje = "Cliente no encontrado.".
        RETURN.
    END.

    /* Obtener el folio asociado */
    DEFINE VARIABLE FolioCliente AS INTEGER NO-UNDO.
    ASSIGN FolioCliente = bfCliente.Id-Documento.

    /* Si el cliente no tiene folio asignado, no hay documentos */
    IF FolioCliente = 0 THEN DO:
        ASSIGN Mensaje = "El cliente no tiene documentos asociados.".
        RETURN.
    END.

    /* Buscar documentos en la tabla DocReq asociados al folio */
    DEFINE VARIABLE RegistrosEncontrados AS LOGICAL NO-UNDO.
    ASSIGN RegistrosEncontrados = FALSE.

    FOR EACH DocReq WHERE DocReq.IdSolicitud = FolioCliente NO-LOCK:
        CREATE ttDocReq.
        ASSIGN
            ttDocReq.IdCliente     = IdCliente
            ttDocReq.TipoDoc       = DocReq.TipoDoc
            ttDocReq.NombreArchivo = DocReq.NombreArchivo.

        /* Copiar el contenido del campo PDF */
        COPY-LOB FROM DocReq.PDF TO ttDocReq.PDF.

        ASSIGN RegistrosEncontrados = TRUE.  
    END.

    /* Si no se encontraron registros, indicar el mensaje correspondiente */
    IF NOT RegistrosEncontrados THEN
        ASSIGN Mensaje = "No se encontraron documentos asociados al folio del cliente.".

END PROCEDURE.
