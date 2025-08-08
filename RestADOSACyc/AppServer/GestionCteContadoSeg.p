@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : GestionCteContadoSeg.p
    URL         : /GestionCteConGraSegmento
    SP          :  Numero de Sprint 3
    Purpose     : .
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Definición de una tabla temporal */
DEFINE TEMP-TABLE ttSegmento
    FIELD TipoSegmento  AS CHAR     /* Nombre del segmento */
    FIELD Total         AS INTEGER. /* Cantidad asociada al segmento */

/* Definición de un dataset que encapsula la tabla temporal */
DEFINE DATASET dsSegmento FOR ttSegmento.

/* ********************  Main Block  *************************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetSegmento:
    
    DEFINE INPUT PARAMETER Clase AS INTEGER.
    DEFINE OUTPUT PARAMETER TABLE FOR ttSegmento.

    /* Validar si el parámetro Clase fue enviado */
    IF Clase = ? THEN DO:
        RETURN ERROR "El parámetro 'Clase' es obligatorio.".
    END.

    /* Paso 1: Inicializar todos los segmentos con Total = 0 */
    FOR EACH SegmentoCte NO-LOCK:
        FIND FIRST ttSegmento WHERE ttSegmento.TipoSegmento = SegmentoCte.Descr NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttSegmento THEN DO:
            CREATE ttSegmento.
            ASSIGN
                ttSegmento.TipoSegmento = SegmentoCte.Descr
                ttSegmento.Total        = 0.
        END.
    END.

    /* Paso 2: Recorrer los clientes activos y actualizar las cantidades */
    FOR EACH Cliente WHERE Cliente.Activo
                       AND Cliente.Id-ClaseCte = Clase NO-LOCK:
        FOR EACH SegmentoCte WHERE SegmentoCte.Id-SegmentoCte = Cliente.Id-SegmentoCte NO-LOCK:
            FIND FIRST ttSegmento WHERE ttSegmento.TipoSegmento = SegmentoCte.Descr NO-LOCK NO-ERROR.
            IF AVAILABLE ttSegmento THEN DO:
                /* Incrementar el total de este segmento */
                ttSegmento.Total = ttSegmento.Total + 1.
            END.
        END.
    END.
RETURN.   
END PROCEDURE.
