@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : GestionCteContado.p
    URL         : /GestionCteContado
    SP          : 3 
    Purpose     : Obtener información estadística de los clientes activos, 
                  clasificándolos según su tipo de límite de crédito.
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Definición de una tabla temporal */
DEFINE TEMP-TABLE ttCliente
    FIELD Totales          AS INTEGER  /* Total de clientes activos */
    FIELD TotalCredito     AS INTEGER  /* Total de clientes con límite de crédito */
    FIELD TotalContado     AS INTEGER  /* Total de clientes sin límite de crédito */
    FIELD TotalLCBloqueado AS INTEGER. /* Total de clientes con límite de crédito bloqueado */

/* Definición de un dataset que encapsula la tabla temporal */
DEFINE DATASET dsCliente FOR ttCliente.

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetClientes:
    
    DEFINE INPUT PARAMETER Clase AS INTEGER  NO-UNDO.
    /* Declarar un parámetro de salida para devolver los datos en ttCliente */
    DEFINE OUTPUT PARAMETER TABLE FOR ttCliente.

    /* Validar si el parámetro Clase fue enviado */
    IF Clase = ? THEN DO:
        RETURN ERROR "El parámetro 'Clase' es obligatorio.".
    END.

    /* Vaciar la tabla temporal antes de llenarla */
    EMPTY TEMP-TABLE ttCliente.

    /* Recorrer la tabla de clientes activos y acumular estadísticas */
    FOR EACH Cliente WHERE Cliente.Activo 
                       AND Cliente.Id-ClaseCte = Clase NO-LOCK:   
        /* Contar todos los clientes activos */
        ACCUMULATE 1 (COUNT).
        
        /* Contar los clientes con límite de crédito positivo */
        IF Cliente.Limite > 0 THEN
            ACCUMULATE 2 (COUNT).
        ELSE 
            /* Contar los clientes sin límite de crédito (contado) */
            ACCUMULATE 3 (COUNT).
    END.

    /* Crear un registro en la tabla temporal ttCliente con los datos acumulados */
    CREATE ttCliente.
    ASSIGN
        ttCliente.Totales       = (ACCUM COUNT 1) /* Total de clientes activos */
        ttCliente.TotalCredito  = (ACCUM COUNT 2) /* Total de clientes con crédito */
        ttCliente.TotalContado  = (ACCUM COUNT 3) /* Total de clientes con contado */
        ttCliente.TotalLCBloqueado = 0.          /* Actualmente no implementado */

    /* Retornar los resultados en el parámetro de salida */
    RETURN.
END PROCEDURE.
