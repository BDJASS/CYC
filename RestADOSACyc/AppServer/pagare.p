@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : pagare.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Wed Jan 08 02:22:29 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* Tabla temporal para la salida */
DEFINE TEMP-TABLE ttPagare NO-UNDO
    FIELD IdPagare            AS INTEGER
    FIELD IdCliente           AS INTEGER
    FIELD RazonSocial         AS CHARACTER
    FIELD RFC                 AS CHARACTER
    FIELD Telefono            AS CHARACTER
    FIELD CalleNo             AS CHARACTER
    FIELD FecExp              AS DATE
    FIELD Importe             AS DECIMAL  
    FIELD Interes             AS INTEGER
    FIELD Activo              AS LOGICAL
    FIELD FecVenc             AS DATE
    FIELD IdEstatus           AS CHAR /* 1= pendiente 2= que ya trae pagare 3= vencido*/
    FIELD DocPagareCargado    AS LOGICAL
    FIELD DocIneAvalCargado   AS LOGICAL
    FIELD DocIneDeudorCargado AS LOGICAL
    FIELD DocPagare           AS BLOB
    FIELD DocIneAval          AS BLOB
    FIELD DocIneDeudor        AS BLOB
    INDEX idx-pagare IdPagare ASCENDING.

/* Buffer para la tabla persistente */
DEFINE BUFFER bfPagare FOR Pagare.         



/* **********************  Internal Procedures  *********************** */




@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE AltaPdf:
    /*--------------------------------------------------------------------------  
     Purpose     : Cargar documentos PDF en formato BLOB en la tabla Pagare.
    --------------------------------------------------------------------------*/

    /* Parámetros de entrada y salida */
    DEFINE INPUT  PARAMETER TABLE FOR ttPagare.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.

    /* Validar si no hay registros en la tabla temporal de entrada */
    IF NOT CAN-FIND(FIRST ttPagare) THEN 
    DO:
        ASSIGN 
            l-Mensaje = "No se enviaron datos del pagaré para procesar.".
        RETURN.
    END.

    /* Iterar sobre las solicitudes de entrada */
    FOR EACH ttPagare:
        DO TRANSACTION:
            /* Buscar el registro existente en la tabla Pagare */
            FIND FIRST bfPagare WHERE bfPagare.Id-Pagare = ttPagare.IdPagare EXCLUSIVE-LOCK NO-ERROR.

            /* Si el registro existe, actualizar los campos BLOB */
            IF AVAILABLE bfPagare THEN 
            DO:
                
                IF ttPagare.FecExp <> ? THEN 
                DO: 
                    ASSIGN
                        bfPagare.FecExp  = ttPagare.FecExp
                        bfPagare.FecVenc = ADD-INTERVAL (ttPagare.FecExp,3,"YEAR").
                END.
                   
                IF ttPagare.Importe >0 THEN 
                DO:
                    ASSIGN
                        bfPagare.Importe = ttPagare.Importe.
                END.
                 
                /* Copiar los datos de los campos BLOB y manejar el estatus */
                IF ttPagare.DocPagare <> ? THEN 
                DO:
                    COPY-LOB FROM ttPagare.DocPagare TO bfPagare.DocPagare.
                    bfPagare.DocPagareCargado = TRUE.  /* Marcamos que el DocPagare fue cargado */
                END.
                IF ttPagare.DocIneAval <> ? THEN 
                DO:
                    COPY-LOB FROM ttPagare.DocIneAval TO bfPagare.DocIneAval.
                    bfPagare.DocIneAvalCargado = TRUE.   /* Marcamos que el DocIneAval fue cargado */
                END.
                   
                IF ttPagare.DocIneDeudor <> ? THEN
                DO:
                    COPY-LOB FROM ttPagare.DocIneDeudor TO bfPagare.DocIneDeudor.
                    bfPagare.DocIneDeudorCargado = TRUE.
                END.   
                /* Verificamos si ambos documentos están cargados y, si es así, actualizamos el Id-Estatus */
                IF bfPagare.DocPagareCargado AND bfPagare.DocIneDeudorCargado THEN
                DO:
                    bfPagare.Id-Estatus = 2.  /* Actualizamos el estatus a 2 */
                /* Aquí puedes agregar más lógica si deseas realizar otras actualizaciones */
                END.
            END.
        END.   

        /* Liberar buffer */
        RELEASE bfPagare.
    END. /* Cierre del FOR EACH */

    /* Asignar mensaje de éxito */
    ASSIGN 
        l-Mensaje = "Carga de PDF exitoso.".
RETURN.
END PROCEDURE.


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarPagares:
    /*--------------------------------------------------------------------------  
     Purpose     : Consultar registros de la tabla Pagare.
     Notes       : Filtra por Id-Cliente si el parámetro es enviado, de lo contrario, devuelve todos los registros.
    --------------------------------------------------------------------------*/

    /* Parámetro de entrada */
    DEFINE INPUT  PARAMETER  IdCliente AS INTEGER NO-UNDO. /* Opcional */
    DEFINE INPUT  PARAMETER  IdPagare AS INTEGER NO-UNDO. /* Opcional */
    DEFINE INPUT  PARAMETER  ipFecIni AS DATE NO-UNDO.
    DEFINE INPUT  PARAMETER  ipFecFin AS DATE NO-UNDO.
    DEFINE VARIABLE idEstatus AS CHARACTER NO-UNDO.
    /* Parámetro de salida */
    DEFINE OUTPUT PARAMETER TABLE FOR ttPagare.
    

    /* Inicializar la tabla temporal */
    EMPTY TEMP-TABLE ttPagare.
    
    IF IdCliente = ? THEN IdCliente = 0.
    IF IdPagare = ?  THEN IdPagare  = 0.
    /* Recorrer los registros de la tabla Pagare */
    
    IF IdPagare <> 0 THEN 
    DO:
        
        FIND FIRST bfPagare WHERE bfPagare.Activo
            AND  bfPagare.Id-Pagare = IdPagare NO-LOCK NO-ERROR.
        IF AVAILABLE bfPagare THEN 
        DO:
            IF bfPagare.FecVenc >= TODAY AND bfPagare.FecVenc <= TODAY + 15 THEN 
                idEstatus = "POR VENCER".
            ELSE IF bfPagare.FecVenc < TODAY  THEN 
                    idEstatus = "VENCIDO".
                ELSE IF NOT bfPagare.DocPagareCargado OR NOT bfPagare.DocIneDeudorCargado 
                        AND bfPagare.FecVenc >= TODAY THEN 
                        idEstatus = "PENDIENTE".  
                    ELSE IF bfPagare.DocPagareCargado AND bfPagare.DocIneDeudorCargado 
                            AND bfPagare.FecVenc >= TODAY THEN 
                            idEstatus = "VIGENTE".  
                        ELSE 
                            idEstatus = "DESCONOCIDO". /* Opcional para manejar casos no previstos */ 

            /* Crear registro en la tabla temporal */
            CREATE ttPagare.
            ASSIGN   
                ttPagare.IdPagare            = bfPagare.Id-Pagare
                ttPagare.IdCliente           = bfPagare.Id-Cliente
                ttPagare.RazonSocial         = bfPagare.RazonSocial
                ttPagare.RFC                 = bfPagare.RFC
                ttPagare.Telefono            = bfPagare.Tel
                ttPagare.CalleNo             = bfPagare.CalleNo
                ttPagare.FecExp              = bfPagare.FecExp
                ttPagare.Importe             = bfPagare.Importe
                ttPagare.Interes             = bfPagare.Interes
                ttPagare.Activo              = bfPagare.Activo
                ttPagare.FecVenc             = bfPagare.FecVenc
                ttPagare.DocPagareCargado    = bfPagare.DocPagareCargado
                ttPagare.DocIneAvalCargado   = bfPagare.DocIneAvalCargado
                ttPagare.DocIneDeudorCargado = bfPagare.DocIneDeudorCargado
                ttPagare.IdEstatus           = idEstatus
                ttPagare.DocPagare           = bfPagare.DocPagare.
            ttPagare.DocIneAval          = bfPagare.DocIneAval.
            ttPagare.DocIneDeudor        = bfPagare.DocIneDeudor.   
        END.  
    END.
    ELSE 
    DO:
    
        FOR EACH bfPagare NO-LOCK WHERE bfPagare.Activo 
            AND(IdCliente = 0 OR bfPagare.Id-Cliente = IdCliente) 
            AND (ipFecIni = ? OR bfPagare.FecExp >= ipFecIni) 
            AND (ipFecFin = ? OR bfPagare.FecExp <= ipFecFin):   
            /* Filtrar por fecha */
            /* Calcular el valor de IdEstatus basado en las condiciones */

            IF bfPagare.FecVenc >= TODAY AND bfPagare.FecVenc <= TODAY + 15 THEN 
                idEstatus = "POR VENCER".
            ELSE IF bfPagare.FecVenc < TODAY  THEN 
                    idEstatus = "VENCIDO".
                ELSE IF NOT bfPagare.DocPagareCargado OR NOT bfPagare.DocIneDeudorCargado 
                        AND bfPagare.FecVenc >= TODAY THEN 
                        idEstatus = "PENDIENTE".  
                    ELSE IF bfPagare.DocPagareCargado AND bfPagare.DocIneDeudorCargado 
                            AND bfPagare.FecVenc >= TODAY THEN 
                            idEstatus = "VIGENTE".  
                        ELSE 
                            idEstatus = "DESCONOCIDO". /* Opcional para manejar casos no previstos */ 

            /* Crear registro en la tabla temporal */
            CREATE ttPagare.
            ASSIGN   
                ttPagare.IdPagare            = bfPagare.Id-Pagare
                ttPagare.IdCliente           = bfPagare.Id-Cliente
                ttPagare.RazonSocial         = bfPagare.RazonSocial
                ttPagare.RFC                 = bfPagare.RFC
                ttPagare.Telefono            = bfPagare.Tel
                ttPagare.CalleNo             = bfPagare.CalleNo
                ttPagare.FecExp              = bfPagare.FecExp
                ttPagare.Importe             = bfPagare.Importe
                ttPagare.Interes             = bfPagare.Interes
                ttPagare.Activo              = bfPagare.Activo
                ttPagare.FecVenc             = bfPagare.FecVenc
                ttPagare.DocPagareCargado    = bfPagare.DocPagareCargado
                ttPagare.DocIneAvalCargado   = bfPagare.DocIneAvalCargado
                ttPagare.DocIneDeudorCargado = bfPagare.DocIneDeudorCargado
                ttPagare.IdEstatus           = idEstatus.  
        END.
    END.
    /* Liberar buffer */
    RELEASE bfPagare.  
RETURN.
END PROCEDURE.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE AltaPagare:
    /*------------------------------------------------------------------------------  
     Purpose     : Alta de pagarés en la tabla Pagare.  
     Notes       : Valida los datos de entrada y genera un ID único.
    ------------------------------------------------------------------------------*/

    /* Parámetros de entrada y salida */
    DEFINE INPUT  PARAMETER TABLE FOR ttPagare.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.

    /* Variables internas */
    DEFINE VARIABLE lEncontrado AS LOGICAL NO-UNDO INITIAL FALSE.
    DEFINE VARIABLE iID         AS INTEGER NO-UNDO INITIAL 0.

    /* Iterar sobre las solicitudes de entrada */
    FOR EACH ttPagare:  
        /* Generar un ID único */
        DO TRANSACTION:
            DO iID = 1 TO 90000:
                FIND FIRST bfPagare WHERE bfPagare.Id-Pagare = iID NO-LOCK NO-ERROR.
                IF NOT AVAILABLE bfPagare THEN 
                DO:
                    /* Crear el registro en la tabla Pagare */
                    CREATE bfPagare.
                    ASSIGN 
                        bfPagare.Id-Pagare   = iID
                        bfPagare.Id-Cliente  = ttPagare.IdCliente
                        bfPagare.RazonSocial = ttPagare.RazonSocial
                        bfPagare.RFC         = ttPagare.RFC
                        bfPagare.Tel         = ttPagare.Telefono
                        bfPagare.CalleNo     = ttPagare.CalleNo
                        bfPagare.Importe     = ttPagare.Importe
                        bfPagare.Interes     = ttPagare.Interes
                        bfPagare.Activo      = TRUE
                        bfPagare.Id-Estatus  = 1
                        bfPagare.FecExp      = ttPagare.FecExp
                        bfPagare.FecVenc     = ADD-INTERVAL (ttPagare.FecExp,3,"YEAR").
                    ASSIGN 
                        lEncontrado = TRUE.
                    LEAVE. /* Salir del bucle iID */
                END.
            END.
        END .
        /* Asignar mensaje de éxito */
        ASSIGN 
            l-Mensaje = STRING(iID). 
        /* Liberar buffer */
        RELEASE bfPagare.
    END. /* Cierre del FOR EACH */
    RETURN.
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE EliminarPdf:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /* Parámetros de entrada y salida */
    DEFINE INPUT  PARAMETER TABLE FOR ttPagare.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.

    /* Validar si no hay registros en la tabla temporal de entrada */
    IF NOT CAN-FIND(FIRST ttPagare) THEN 
    DO:
        ASSIGN 
            l-Mensaje = "No se enviaron datos del pagaré para procesar.".
        RETURN.
    END.

    /* Iterar sobre las solicitudes de entrada */
    FOR EACH ttPagare:
        /* Generar un ID único */
        DO TRANSACTION:
            FIND FIRST bfPagare WHERE bfPagare.Id-Pagare = ttPagare.IdPagare EXCLUSIVE-LOCK NO-ERROR.
            IF  AVAILABLE bfPagare THEN 
            DO:
                /* Crear el registro en la tabla Pagare */
                ASSIGN 
                    bfPagare.Activo = FALSE.
            END.
        END .
        /* Liberar buffer */
        RELEASE bfPagare.
    END. /* Cierre del FOR EACH */

    /* Asignar mensaje de éxito */
    ASSIGN 
        l-Mensaje = "Pagare eliminado.".
RETURN.
END PROCEDURE.
