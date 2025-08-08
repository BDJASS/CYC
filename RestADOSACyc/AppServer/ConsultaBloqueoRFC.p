@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : ConsultaBloqueoRFC.p
                  /GestionClienteRFC
    Purpose     : Combina las operaciones para consultar y eliminar RFCs bloqueados.
    Author(s)   : sis10
    Created     : (Coloca la fecha de creación aquí)
    Notes       : Este programa contiene dos procedimientos:
                  - ConsultaBloqueoRFC: Consulta el estado del RFC bloqueado (GET).
                  - EliminarBloqueoRFC: Elimina un registro de bloqueo por RFC (POST).
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttBloqueoRFC NO-UNDO
    FIELD RFC                  AS CHARACTER FORMAT "xxxx-xxxxxxx-xxx"
    FIELD FechaDeBloqueo       AS DATE
    FIELD ResponsableDeBloqueo AS CHARACTER FORMAT "x(12)"
    FIELD Motivo               AS CHARACTER FORMAT "x(40)"
    INDEX idxRFC RFC. /* Índice ascendente por RFC */


DEFINE BUFFER bfCliente FOR Cliente. 
DEFINE BUFFER bfBlkRFC FOR blkRFC.

/* ***************************  Main Block **************************** */
/* Consulta el estado de bloqueo de todos los RFCs (GET) */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultaBloqueoRFC:
    DEFINE OUTPUT PARAMETER TABLE FOR ttBloqueoRFC.

    EMPTY TEMP-TABLE ttBloqueoRFC.

    /* Recorrer todos los registros de blkRFC */
    FOR EACH bfBlkRFC NO-LOCK:
        /* Buscar el usuario correspondiente en la tabla Usuario */
        FIND FIRST Usuario 
            WHERE Usuario.Id-User = bfBlkRFC.Id-User 
            NO-LOCK NO-ERROR.

        /* Crear un registro en la tabla temporal */
        CREATE ttBloqueoRFC.
        ASSIGN
            ttBloqueoRFC.RFC                  = REPLACE(bfBlkRFC.RFC, " ", "")
            ttBloqueoRFC.FechaDeBloqueo       = bfBlkRFC.FecReg
            ttBloqueoRFC.ResponsableDeBloqueo = 
                IF AVAILABLE Usuario THEN Usuario.Nom-Usuario
                ELSE bfBlkRFC.Id-User  /* Manejo en caso de que no se encuentre el usuario */
            ttBloqueoRFC.Motivo               = bfBlkRFC.Motivo.
    END.
END PROCEDURE.



/* Elimina un registro de bloqueo por RFC (POST) */ 
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE EliminarBloqueoRFC:
    DEFINE INPUT PARAMETER RFC AS CHARACTER.
    DEFINE OUTPUT PARAMETER Respuesta AS CHARACTER NO-UNDO.

    /* Iniciar una transacción */
    DO TRANSACTION:
        /* Validar si el RFC está en blkRFC */
        FIND FIRST bfBlkRFC 
            WHERE REPLACE(bfBlkRFC.RFC, " ", "") = REPLACE(RFC, " ", "") 
            EXCLUSIVE-LOCK NO-ERROR.

        IF NOT AVAILABLE bfBlkRFC THEN DO:
            /* Manejo controlado de error */
            ASSIGN Respuesta = "El RFC proporcionado no está bloqueado.".
            RETURN.
        END.

        /* Eliminar el registro de blkRFC */
        DELETE bfBlkRFC.

        /* Asignar respuesta de éxito */
        ASSIGN Respuesta = "El RFC ha sido desbloqueado correctamente.".
    END. /* Fin de la transacción */

    /* Liberar el buffer */
    RELEASE bfBlkRFC.
END PROCEDURE.

