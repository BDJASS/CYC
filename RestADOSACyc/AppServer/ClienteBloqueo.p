@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*--------------------------------------------------------------------------  
 File        : ClienteBloqueo.p
 Module      : Gestión de Clientes - Contado y Crédito
 Sprint      : Sprint 3
 URL         : /ClienteBloqueado
 Purpose     : Bloquear un cliente y registrar asociados en las tablas blkAut y blkRFC.

 Description : Este programa es un servicio REST diseñado para gestionar bloqueos de clientes y asociados.
               Recibe parámetros individuales de entrada para procesar un cliente a nivel de:
               - Bloqueo por RFC (blkRFC).
               - Bloqueo por Número de Cliente (blkAut).
               - Registro de asociados bloqueados (blkAut).

               El programa asegura que las acciones se ejecuten de forma dinámica, con una salida
               que detalla las acciones realizadas.

 Notes       : 
               - Este programa no realiza validaciones sobre bloqueos previos.
               - Asume que los datos enviados han sido validados previamente en la capa de consulta.

 Author(s)   : jsegura
 Created     : [Noviembre 2024]
 --------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttClienteBloqueo NO-UNDO
    FIELD NumCliente     AS INTEGER    /* Número de Cliente */
    FIELD Usuario        AS CHARACTER
    FIELD RazonSocial    AS CHARACTER  /*Razon Social */
    FIELD RFC            AS CHARACTER  /*Razon Social */
    FIELD BloqueoRFC     AS LOGICAL    /* Indica si se debe bloquear por RFC */
    FIELD BloqueoCliente AS LOGICAL    /* Indica si se debe bloquear por Número de Cliente */
    FIELD Motivo         AS CHARACTER  /* Motivo del bloqueo (solo aplica para RFC) */
    FIELD Asociados      AS CHARACTER. /* IDs de Asociados separados por comas */
    
    
DEFINE TEMP-TABLE ttClienteBloq NO-UNDO
    FIELD NumCliente     AS INTEGER    /* Número de Cliente */
    FIELD RazonSocial    AS CHARACTER  /*Razon Social */ 
    FIELD RFC            AS CHARACTER  /*Razon Social */
    FIELD BloqueoRFC     AS LOGICAL    /* Indica si se debe bloquear por RFC */
    FIELD BloqueoCliente AS LOGICAL    /* Indica si se debe bloquear por Número de Cliente */
    FIELD Motivo         AS CHARACTER.  /* Motivo del bloqueo (solo aplica para RFC) */
    
    

/* Tabla temporal de salida */
DEFINE TEMP-TABLE ttResultado NO-UNDO
    FIELD Accion       AS CHARACTER FORMAT "x(50)" /* Descripción de la acción realizada */
    FIELD NumCliente   AS INTEGER                  /* Número de cliente */
    FIELD Usuario      AS CHARACTER FORMAT "x(20)" /* Usuario que realizó la acción */
    FIELD FechaBloqueo AS DATE                     /* Fecha del bloqueo */
    FIELD Motivo       AS CHARACTER FORMAT "x(100)". /* Motivo del bloqueo */
    
/* ***************************  Procedures **************************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetClienteBloqueo:
/*--------------------------------------------------------------------------  
 Purpose     : Obtener información del cliente, tipos de bloqueo y motivo.
--------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pNumCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttClienteBloq.

    DEFINE BUFFER bCliente FOR Cliente.
    DEFINE BUFFER bBlkRFC FOR blkRFC.
    DEFINE BUFFER bBlkAut FOR blkAut.

    EMPTY TEMP-TABLE ttClienteBloq.

    /* Buscar el cliente con el número de cliente proporcionado */
    FIND FIRST bCliente WHERE bCliente.Id-Cliente = pNumCliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bCliente THEN DO:
        MESSAGE "El cliente con ID " + STRING(pNumCliente) + " no existe." VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    /* Declarar variables */
    DEFINE VARIABLE bloqueoRFC     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE bloqueoCliente AS LOGICAL NO-UNDO.
    DEFINE VARIABLE motivo         AS CHARACTER NO-UNDO.

    /* Inicializar variables */
    ASSIGN
        bloqueoRFC     = FALSE
        bloqueoCliente = FALSE
        motivo         = "".

    /* Verificar si el cliente está bloqueado por RFC */
    FIND FIRST bBlkRFC WHERE REPLACE(bBlkRFC.RFC, " ", "") = REPLACE(bCliente.RFC, " ", "") NO-LOCK NO-ERROR.
    IF AVAILABLE bBlkRFC THEN DO:
        ASSIGN
            bloqueoRFC = TRUE
            motivo = bBlkRFC.Motivo.
    END.

    /* Verificar si el cliente está bloqueado por número de cliente */
    FIND FIRST bBlkAut WHERE bBlkAut.Id-Cliente = bCliente.Id-Cliente NO-LOCK NO-ERROR.
    IF AVAILABLE bBlkAut THEN
        ASSIGN bloqueoCliente = TRUE.

    /* Crear el registro en la tabla temporal */
    CREATE ttClienteBloq.
    ASSIGN
        ttClienteBloq.NumCliente     = bCliente.Id-Cliente
        ttClienteBloq.RazonSocial    = bCliente.RazonSocial
        ttClienteBloq.RFC            = REPLACE(bCliente.RFC, " ", "")
        ttClienteBloq.BloqueoRFC     = bloqueoRFC
        ttClienteBloq.BloqueoCliente = bloqueoCliente
        ttClienteBloq.Motivo         = motivo.

    RETURN. /* Devolver los datos cargados en la tabla temporal */
END PROCEDURE.



/* PROCEDIMIENTO POST: Bloquear cliente */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostBloqueoCliente.

/* ***************************  Definitions **************************** */

DEFINE INPUT  PARAMETER NumCliente     AS INTEGER NO-UNDO.    /* ID del cliente */
DEFINE INPUT  PARAMETER Usuario        AS CHARACTER NO-UNDO.  /* Usuario que realiza la acción */
DEFINE INPUT  PARAMETER RFC            AS CHARACTER NO-UNDO.  /* RFC del cliente */
DEFINE INPUT  PARAMETER BloqueoRFC     AS LOGICAL NO-UNDO.    /* Indicador de bloqueo por RFC */
DEFINE INPUT  PARAMETER BloqueoCliente AS LOGICAL NO-UNDO.    /* Indicador de bloqueo por número de cliente */
DEFINE INPUT  PARAMETER Motivo         AS CHARACTER NO-UNDO.  /* Motivo del bloqueo */
DEFINE INPUT  PARAMETER Asociados      AS CHARACTER NO-UNDO.  /* Lista de IDs de asociados separados por comas */
DEFINE OUTPUT PARAMETER TABLE FOR ttResultado.                /* Tabla temporal de salida */

 
/* Buffers */
DEFINE BUFFER bfBlkRFC FOR blkRFC.               /* Buffer para tabla blkRFC */
DEFINE BUFFER bfBlkAut FOR blkAut.               /* Buffer para tabla blkAut */
DEFINE BUFFER bfCliente FOR Cliente.             /* Buffer para tabla Cliente */

/* Variables */
DEFINE VARIABLE asociado AS INTEGER NO-UNDO.     /* ID de un asociado en procesamiento */
DEFINE VARIABLE i        AS INTEGER NO-UNDO.     /* Índice para recorrer asociados */

/* ***************************  Main Block **************************** */

 EMPTY TEMP-TABLE ttResultado.

/* Validar que el cliente exista */
FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = NumCliente NO-LOCK NO-ERROR.
IF NOT AVAILABLE bfCliente THEN DO:
    CREATE ttResultado.
    ASSIGN
        ttResultado.Accion = "El cliente no existe"
        ttResultado.NumCliente = NumCliente
        ttResultado.Usuario = Usuario
        ttResultado.FechaBloqueo = TODAY
        ttResultado.Motivo = "Cliente no encontrado.".
    RETURN.
END.

/* Bloqueo por RFC */
IF BloqueoRFC = TRUE THEN DO:
    CREATE bfBlkRFC.
    ASSIGN
        bfBlkRFC.RFC     = REPLACE(RFC, " ", "")
        bfBlkRFC.Id-User = Usuario
        bfBlkRFC.FecReg  = TODAY
        bfBlkRFC.HorReg  = TIME
        bfBlkRFC.Motivo  = Motivo.
    RELEASE bfBlkRFC.

    /* Registrar acción en la tabla temporal */
    CREATE ttResultado.
    ASSIGN
        ttResultado.Accion = "Bloqueo por RFC"
        ttResultado.NumCliente = NumCliente
        ttResultado.Usuario = Usuario
        ttResultado.FechaBloqueo = TODAY
        ttResultado.Motivo = Motivo.
END.

/* Bloqueo por Número de Cliente */
IF BloqueoCliente = TRUE THEN DO:
    CREATE bfBlkAut.
    ASSIGN
        bfBlkAut.Id-Cliente = NumCliente
        bfBlkAut.FecReg     = TODAY.
    RELEASE bfBlkAut.

    /* Registrar acción en la tabla temporal */
    CREATE ttResultado.
    ASSIGN
        ttResultado.Accion = "Bloqueo por número de cliente"
        ttResultado.NumCliente = NumCliente
        ttResultado.Usuario = Usuario
        ttResultado.FechaBloqueo = TODAY
        ttResultado.Motivo = "".
END.

/* Procesar Asociados */
IF Asociados <> "" THEN DO:
    DO i = 1 TO NUM-ENTRIES(Asociados, ","):
        ASSIGN asociado = INTEGER(ENTRY(i, Asociados, ",")). /* Convertir a entero */
        CREATE bfBlkAut.
        ASSIGN
            bfBlkAut.Id-Cliente = asociado
            bfBlkAut.FecReg     = TODAY.
        RELEASE bfBlkAut.

        /* Registrar acción para cada asociado en la tabla temporal */
        CREATE ttResultado.
        ASSIGN
            ttResultado.Accion = "Bloqueo de asociado"
            ttResultado.NumCliente = asociado
            ttResultado.Usuario = Usuario
            ttResultado.FechaBloqueo = TODAY
            ttResultado.Motivo = "".
    END.
END.

/* Si no se realizaron acciones */
IF NOT CAN-FIND(FIRST ttResultado) THEN DO:
    CREATE ttResultado.
    ASSIGN
        ttResultado.Accion = "Sin acciones realizadas"
        ttResultado.NumCliente = NumCliente
        ttResultado.Usuario = Usuario
        ttResultado.FechaBloqueo = TODAY
        ttResultado.Motivo = "No se enviaron parámetros de bloqueo válidos.".
END.

/* Liberar buffers */
RELEASE bfCliente.
RELEASE bfBlkRFC.
RELEASE bfBlkAut.

END PROCEDURE.
