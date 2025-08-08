@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
    Empresa : ADOSA
    Programa: RespuestaClientes.p
              /RespuestaClientes
    Fucnion : Consulta Historial Respuesta / Guarda Respuestas
    Autor   : ALEX
    Fecha   : 3 de Diciembre DEL 2024
*/


DEFINE TEMP-TABLE ttDatos
    FIELD IdCliente LIKE HistRespCte.Id-Cliente
    FIELD IdUser    LIKE HistRespCte.Id-User
    FIELD NomUser   LIKE Usuario.Nom-Usuario
    FIELD Coment    LIKE HistRespCte.Coment
    FIELD FecReg    LIKE HistRespCte.FecReg 
    FIELD Hora      AS CHARACTER.

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetConsultaRespuesta:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
        Empresa : ADOSA
        Programa: leehistrespcte.p
        Fucnion : Lee tabla de historial de respuestas de clientes
        Autor   : ALEX
        Fecha   : 3 de Diciembre DEL 2024
    */


    DEFINE INPUT PARAMETER ipCliente AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttDatos.
    DEFINE OUTPUT PARAMETER opRazonSocial LIKE Cliente.RazonSocial NO-UNDO.
    DEFINE OUTPUT PARAMETER opError AS CHARACTER NO-UNDO.

    FIND Cliente WHERE Cliente.Id-Cliente = ipCliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cliente THEN 
    DO:
        opError = "Cliente inexistente.".
        RETURN.    
    END.
    
    MESSAGE "GET-RespuestaClientes" VIEW-AS ALERT-BOX.
    
    ASSIGN 
        opRazonSocial = Cliente.RazonSocial.

    FOR EACH HistRespCte WHERE HistRespCte.Id-Cliente = ipCliente NO-LOCK:
        
        FIND Usuario WHERE Usuario.Id-User = HistRespCte.Id-User NO-LOCK NO-ERROR.
        CREATE ttDatos.
        ASSIGN 
            ttDatos.IdCliente = HistRespCte.Id-Cliente
            ttDatos.IdUser    = HistRespCte.Id-User
            ttDatos.Coment    = HistRespCte.Coment
            ttDatos.FecReg    = HistRespCte.FecReg
            ttDatos.Hora      = STRING(HistRespCte.HorReg,"HH:MM:SS").
            
        IF AVAILABLE Usuario THEN
            ASSIGN ttDatos.NomUser = Usuario.Nom-Usuario.
        ELSE
            ASSIGN ttDatos.NomUser = HistRespCte.Id-User.
    END.

    RETURN.

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostGuardaRespuesta:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
        Empresa : ADOSA
        Programa: grabahistrespcte.p
        Fucnion : Graba tabla HistRespCte
        Autor   : ALEX
        Fecha   : 3 de Diciembre DEL 2024
    */

    DEFINE INPUT PARAMETER TABLE FOR ttDatos.
    DEFINE OUTPUT PARAMETER opMensaje AS CHARACTER NO-UNDO.
    FIND FIRST ttDatos NO-LOCK NO-ERROR.
    IF AVAILABLE ttDatos THEN
        MESSAGE "POST-RespuestaClientes ejecutado por el usuario: " + ttDatos.IdUser
            VIEW-AS ALERT-BOX INFO TITLE "Usuario en ejecución".
    
    FOR EACH ttDatos NO-LOCK:
        CREATE HistRespCte.
        ASSIGN 
            HistRespCte.Id-Cliente = ttDatos.IdCliente
            HistRespCte.Id-User    = ttDatos.IdUser
            HistRespCte.Coment     = ttDatos.Coment
            HistRespCte.FecReg     = TODAY 
            HistRespCte.HorReg     = TIME.
    END.
 
    ASSIGN 
        opMensaje = "Terminado".
    EMPTY TEMP-TABLE ttDatos. /*  Aquí limpias la tabla temporal */
    RETURN.
END PROCEDURE.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PutActualizaRespuesta:
    /*------------------------------------------------------------------------------
     Purpose : Actualiza comentario en tabla HistRespCte
     Notas   : Utiliza IdCliente e IdUser como criterio para actualizar Comentario
    ------------------------------------------------------------------------------*/
    /*
        Empresa : ADOSA
        Programa: actualizahistrespcte.p
        Función : Actualiza comentario en HistRespCte
        Autor   : JASS11072025
        Fecha   : 11 de Julio DEL 2025
    */

    DEFINE INPUT PARAMETER TABLE FOR ttDatos.
    DEFINE OUTPUT PARAMETER opMensaje AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iHora                AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE cHora                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRegistroActualizado AS LOGICAL   NO-UNDO INIT FALSE.
    
    MESSAGE "PUT-RespuestaClientes ejecutado." VIEW-AS ALERT-BOX INFO TITLE "AUDITORÍA".
        
    FOR EACH ttDatos NO-LOCK:
        
        /* Mensaje para ver el contenido de cada registro en ttDatos */
        MESSAGE 
            ">> Registro recibido:"
            SKIP "IdCliente : " ttDatos.IdCliente
            SKIP "IdUser    : " ttDatos.IdUser
            SKIP "FecReg    : " STRING(ttDatos.FecReg)
            SKIP "Hora      : " ttDatos.Hora
            SKIP "Comentario: " ttDatos.Coment
            VIEW-AS ALERT-BOX INFO TITLE "DEBUG: Datos de Entrada".
        
        ASSIGN
            cHora = ttDatos.Hora
            iHora = (INTEGER(ENTRY(1, cHora, ":")) * 3600)
              + (INTEGER(ENTRY(2, cHora, ":")) * 60)
              + INTEGER(ENTRY(3, cHora, ":")).
    
        FIND FIRST HistRespCte 
            WHERE HistRespCte.Id-Cliente = ttDatos.IdCliente
            AND HistRespCte.Id-User    = ttDatos.IdUser
            AND HistRespCte.FecReg     = ttDatos.FecReg
            AND HistRespCte.HorReg     = iHora
            NO-LOCK NO-ERROR.

        IF AVAILABLE HistRespCte THEN 
        DO:
            FIND CURRENT HistRespCte EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            ASSIGN 
                HistRespCte.Coment = ttDatos.Coment
                HistRespCte.FecReg = TODAY
                HistRespCte.HorReg = TIME.
            RELEASE HistRespCte.
            lRegistroActualizado = TRUE.
            
        END.
        ELSE 
        DO:
            opMensaje = "No se encontró registro para actualizar: " 
                + "Cliente: " + STRING(ttDatos.IdCliente)
                + ", Usuario: " + ttDatos.IdUser
                + ", Fecha: " + STRING(ttDatos.FecReg)
                + ", Hora: " + ttDatos.Hora + ".".
        END.
    END.
    
    IF lRegistroActualizado THEN
        opMensaje = "Actualización completada.".
    
   MESSAGE "PUT-RespuestaClientes ejecutado." opMensaje  VIEW-AS ALERT-BOX INFO TITLE "AUDITORÍA".
    
    EMPTY TEMP-TABLE ttDatos.
    RETURN.
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE DelHistRespCte:
    /*------------------------------------------------------------------------------
     Purpose : Eliminar registros en la tabla HistRespCte usando parámetros simples
     Notas   : Recibe IdCliente, IdUser, FecReg y Hora como parámetros individuales
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipIdCliente AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipIdUser    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipFecReg    AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER ipHora      AS CHARACTER   NO-UNDO. /* formato "HH:MM:SS" */
    DEFINE INPUT  PARAMETER iComentario AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opMensaje   AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE iHora               AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lRegistroEliminado  AS LOGICAL     NO-UNDO INIT FALSE.

    MESSAGE "DELET-RespuestaClientes ejecutado." VIEW-AS ALERT-BOX INFO TITLE "AUDITORÍA".
    
    /* Mensaje inicial para verificar los valores recibidos */
    MESSAGE "Iniciando DelHistRespCte()"
        SKIP "Cliente     : " ipIdCliente
        SKIP "Usuario     : " ipIdUser
        SKIP "Fecha       : " STRING(ipFecReg)
        SKIP "Hora        : " ipHora
        SKIP "Comentario  : " iComentario
        VIEW-AS ALERT-BOX INFO TITLE "DEBUG: Parámetros Recibidos".

    IF ipIdCliente = 0 OR ipIdUser = "" OR ipFecReg = ? OR ipHora = "" THEN DO:
        opMensaje = "Parámetros incompletos. Todos son obligatorios.".
        RETURN.
    END.

    iHora = (INTEGER(ENTRY(1, ipHora, ":")) * 3600)
          + (INTEGER(ENTRY(2, ipHora, ":")) * 60)
          + INTEGER(ENTRY(3, ipHora, ":")).

    FIND FIRST HistRespCte 
        WHERE HistRespCte.Id-Cliente = ipIdCliente
          AND HistRespCte.Id-User    = ipIdUser
          AND HistRespCte.FecReg     = ipFecReg
          AND HistRespCte.HorReg     = iHora
          AND HistRespCte.Coment     = iComentario
        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

    IF AVAILABLE HistRespCte THEN DO:
        DELETE HistRespCte.
        opMensaje = "Eliminación completada.".
        lRegistroEliminado = TRUE.
    END.
    ELSE DO:

        opMensaje = "No se encontró registro para eliminar: "
            + "Cliente: " + STRING(ipIdCliente)
            + ", Usuario: " + ipIdUser
            + ", Fecha: " + STRING(ipFecReg)
            + ", Hora: " + ipHora + "."
            + "Com " + iComentario + " ".
          RETURN ERROR.
    END.

    MESSAGE "DELET-RespuestaClientes ejecutado." opMensaje 
        VIEW-AS ALERT-BOX INFO TITLE "AUDITORÍA".

    RETURN.
END PROCEDURE.

