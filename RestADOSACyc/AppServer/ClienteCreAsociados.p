@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : ClienteCreAsociados.p
    Purpose     : Servicios para guardar y consultar información de Asociados Credito y Venta.
    URL         : /ClienteCreditoAsociados
    Module      : Gestión de Seguimiento de Clientes
    Author(s)   : sis10
    Created     : (Fecha actual)
    Notes       :
                  - POST: Guarda nuevos registros en la tabla AsocCred o Asociado dependiendo del tipo.
                  - GET: Consulta los asociados de un cliente por su IdCliente.
------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Tabla temporal para entrada */
DEFINE TEMP-TABLE ttSeguimiento NO-UNDO
    FIELD IdCliente     AS INTEGER
    FIELD IdAsociado    AS INTEGER
    FIELD TipoAsociado  AS INTEGER. /* 1 = Crédito, 2 = Venta */

/* Tabla temporal para salida */
DEFINE TEMP-TABLE ttAsociadosPost NO-UNDO
    FIELD IdCliente     AS INTEGER
    FIELD IdAsociado    AS INTEGER
    FIELD TipoAsociado  AS INTEGER
    FIELD TipoAsociadoN AS CHARACTER
    FIELD Asociado      AS CHARACTER.

/* Tabla temporal para el GET */
DEFINE TEMP-TABLE ttAsociados NO-UNDO
    FIELD IdCliente     AS INTEGER
    FIELD IdAsociado    AS INTEGER
    FIELD TipoAsociado  AS CHARACTER
    FIELD Asociado      AS CHARACTER.

/* Buffers para tablas persistentes */
DEFINE BUFFER bfAsocCred FOR AsocCred.
DEFINE BUFFER bfAsociado FOR Asociado.
DEFINE BUFFER bfCliente  FOR Cliente.

/* ***************************  Main Block **************************** */

/* -------------------------------------------------------------------------- */
/* POST: Servicio para guardar nuevos registros en las tablas AsocCred o Asociado */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GuardarSeguimientos:
    DEFINE INPUT  PARAMETER TABLE FOR ttSeguimiento. /* Tabla temporal de entrada */
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE IdClienteBase  AS INTEGER NO-UNDO.

    /* Validar que todos los registros tengan el mismo IdCliente */
    FIND FIRST ttSeguimiento NO-LOCK NO-ERROR.
    IF AVAILABLE ttSeguimiento THEN
        ASSIGN IdClienteBase = ttSeguimiento.IdCliente.
    
    FOR EACH ttSeguimiento:
        IF ttSeguimiento.IdCliente <> IdClienteBase THEN DO:
            ASSIGN l-Mensaje = "Error: Todos los registros deben pertenecer al mismo IdCliente (" + STRING(IdClienteBase) + ").".
            RETURN.
        END.
    END.
    /* Procesar cada registro en la tabla temporal */
    FOR EACH ttSeguimiento:
        /* Validar que el cliente exista en la tabla Cliente */
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttSeguimiento.IdCliente NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCliente THEN DO:
            ASSIGN l-Mensaje =  "El cliente con ID " + STRING(ttSeguimiento.IdCliente) + " no existe en la base de datos.".
            RETURN.
        END.
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttSeguimiento.IdAsociado NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCliente THEN DO:
            ASSIGN l-Mensaje =  "El Asociado con ID " + STRING(ttSeguimiento.IdAsociado) + " no existe en la base de datos.".
            RETURN.
        END.

        /* Procesar según el tipo de asociado */
        CASE ttSeguimiento.TipoAsociado:
            WHEN 2 THEN DO: /* Crédito */
            FIND FIRST bfAsocCred WHERE bfAsocCred.Id-Cliente = ttSeguimiento.IdCliente 
                                    AND bfAsocCred.Id-Asociado = ttSeguimiento.IdAsociado 
                                    NO-LOCK NO-ERROR.
             IF NOT AVAILABLE bfAsocCred THEN 
             DO: 
                CREATE bfAsocCred.
                ASSIGN
                    bfAsocCred.Id-Cliente  = ttSeguimiento.IdCliente
                    bfAsocCred.Id-Asociado = ttSeguimiento.IdAsociado.
                    RELEASE bfAsocCred.
              END.
            END.
            WHEN 1 THEN DO: /* Venta */
                 FIND FIRST bfAsociado WHERE bfAsociado.Id-Cliente = ttSeguimiento.IdCliente 
                                         AND bfAsociado.Id-Asociado = ttSeguimiento.IdAsociado 
                                         NO-LOCK NO-ERROR.
                IF NOT AVAILABLE bfAsociado THEN 
                DO:
                  CREATE bfAsociado.
                  ASSIGN
                    bfAsociado.Id-Cliente  = ttSeguimiento.IdCliente
                    bfAsociado.Id-Asociado = ttSeguimiento.IdAsociado.
                    RELEASE bfAsociado.
                END.
                
            END.
            OTHERWISE DO:
                ASSIGN l-Mensaje =  "Tipo de asociado inválido: " + STRING(ttSeguimiento.TipoAsociado).
                RETURN.
            END.
        END CASE.
        ASSIGN l-Mensaje = "Registro de Asociados Enviado".
    END.
END PROCEDURE.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE EliminarSeguimientos:
    DEFINE INPUT  PARAMETER TABLE FOR ttSeguimiento. /* Tabla temporal de entrada */
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE IdClienteBase  AS INTEGER NO-UNDO.

    /* Validar que todos los registros tengan el mismo IdCliente */
    FIND FIRST ttSeguimiento NO-LOCK NO-ERROR.
    IF AVAILABLE ttSeguimiento THEN
        ASSIGN IdClienteBase = ttSeguimiento.IdCliente.
    
    FOR EACH ttSeguimiento:
        IF ttSeguimiento.IdCliente <> IdClienteBase THEN DO:
            ASSIGN l-Mensaje = "Error: Todos los registros deben pertenecer al mismo IdCliente (" + STRING(IdClienteBase) + ").".
            RETURN.
        END.
    END.
    /* Procesar cada registro en la tabla temporal */
    FOR EACH ttSeguimiento:
        /* Validar que el cliente exista en la tabla Cliente */
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttSeguimiento.IdCliente NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCliente THEN DO:
            ASSIGN l-Mensaje =  "El cliente con ID " + STRING(ttSeguimiento.IdCliente) + " no existe en la base de datos.".
            RETURN.
        END.
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttSeguimiento.IdAsociado NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCliente THEN DO:
            ASSIGN l-Mensaje =  "El Asociado con ID " + STRING(ttSeguimiento.IdAsociado) + " no existe en la base de datos.".
            RETURN.
        END.

        /* Procesar según el tipo de asociado */
        CASE ttSeguimiento.TipoAsociado:
            WHEN 2 THEN DO: /* Crédito */
            FIND FIRST bfAsocCred WHERE bfAsocCred.Id-Cliente = ttSeguimiento.IdCliente 
                                    AND bfAsocCred.Id-Asociado = ttSeguimiento.IdAsociado 
                                    EXCLUSIVE-LOCK NO-ERROR.
             IF AVAILABLE bfAsocCred THEN 
             DO: 
                    DELETE bfAsocCred.
                    RELEASE bfAsocCred.
              END.
            END.
            WHEN 1 THEN DO: /* Venta */
                 FIND FIRST bfAsociado WHERE bfAsociado.Id-Cliente = ttSeguimiento.IdCliente 
                                         AND bfAsociado.Id-Asociado = ttSeguimiento.IdAsociado 
                                         EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE bfAsociado THEN 
                DO:
                  DELETE bfAsociado.
                    RELEASE bfAsociado.
                END.
                
            END.
            OTHERWISE DO:
                ASSIGN l-Mensaje =  "Tipo de asociado inválido: " + STRING(ttSeguimiento.TipoAsociado).
                RETURN.
            END.
        END CASE.
        ASSIGN l-Mensaje = "Registro de Asociados Eliminados".
    END.
END PROCEDURE.

/* -------------------------------------------------------------------------- */
/* GET: Servicio para consultar asociados de un cliente */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarAsociado:
    DEFINE INPUT  PARAMETER IdCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttAsociados.

    /* Inicializar la tabla temporal */
    EMPTY TEMP-TABLE ttAsociados.

    /* Consultar en AsocCred (Asociados de Crédito) */
    FOR EACH bfAsocCred WHERE bfAsocCred.Id-Cliente = IdCliente NO-LOCK:
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = bfAsocCred.Id-Asociado NO-LOCK NO-ERROR.
        CREATE ttAsociados.
        ASSIGN
            ttAsociados.IdCliente    = bfAsocCred.Id-Cliente
            ttAsociados.IdAsociado   = bfAsocCred.Id-Asociado
            ttAsociados.TipoAsociado = "Asociado de Crédito"
            ttAsociados.Asociado     = IF AVAILABLE bfCliente THEN bfCliente.RazonSocial ELSE "".
    END.

    /* Consultar en Asociado (Asociados de Venta) */
    FOR EACH bfAsociado WHERE bfAsociado.Id-Cliente = IdCliente NO-LOCK:
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = bfAsociado.Id-Asociado NO-LOCK NO-ERROR.
        CREATE ttAsociados.
        ASSIGN
            ttAsociados.IdCliente    = bfAsociado.Id-Cliente
            ttAsociados.IdAsociado   = bfAsociado.Id-Asociado
            ttAsociados.TipoAsociado = "Asociado de Venta"
            ttAsociados.Asociado     = IF AVAILABLE bfCliente THEN bfCliente.RazonSocial ELSE "".
    END.
END PROCEDURE.
