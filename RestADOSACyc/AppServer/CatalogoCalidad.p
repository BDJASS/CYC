@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*---------------------------------------------------------------------------
    File        : CatalogoCalidad.p
    Purpose     : Servicio GET para consultar datos de la tabla Calidad.
    URL         : /Calidad
    Module      : Gestión de Calidad de Clientes
    Notes       : Devuelve todos los campos de la tabla Calidad.
---------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */

/* Tabla temporal para la salida */
DEFINE TEMP-TABLE ttCalidad NO-UNDO
    FIELD IdCalidad   AS INTEGER       /* Mapeado a Calidad.Id-Calidad */
    FIELD Descripcion AS CHARACTER     /* Mapeado a Calidad.Descr */
    FIELD PostFechado AS LOGICAL       /* Mapeado a Calidad.PostFechado */
    FIELD Credito     AS LOGICAL       /* Mapeado a Calidad.Credito */
    FIELD Cheque      AS LOGICAL       /* Mapeado a Calidad.Cheque */
    FIELD ProntoPago  AS LOGICAL       /* Mapeado a Calidad.ProntoPago */
    FIELD Mayoreo     AS LOGICAL       /* Mapeado a Calidad.Mayoreo */
    FIELD CheDev      AS LOGICAL       /* Mapeado a Calidad.CheDev */
    FIELD PolVenta    AS LOGICAL.      /* Mapeado a Calidad.PolVenta */

/* Buffer para la tabla persistente */
DEFINE BUFFER bfCalidad FOR Calidad.

/* ***************************  Main Procedure **************************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarCalidad:
    DEFINE OUTPUT PARAMETER TABLE FOR ttCalidad. /* Salida con los datos */

    /* Inicializar la tabla temporal */
    EMPTY TEMP-TABLE ttCalidad.

    /* Recorrer la tabla Calidad */
    FOR EACH bfCalidad NO-LOCK:
        /* Crear un registro en la tabla temporal de salida */
        CREATE ttCalidad.
        ASSIGN
            ttCalidad.IdCalidad   = bfCalidad.Id-Calidad
            ttCalidad.Descripcion = bfCalidad.Descr
            ttCalidad.PostFechado = bfCalidad.PostFechado
            ttCalidad.Credito     = bfCalidad.Credito
            ttCalidad.Cheque      = bfCalidad.Cheque
            ttCalidad.ProntoPago  = bfCalidad.ProntoPago
            ttCalidad.Mayoreo     = bfCalidad.Mayoreo
            ttCalidad.CheDev      = bfCalidad.CheDev
            ttCalidad.PolVenta    = bfCalidad.PolVenta.
    END.

    /* Liberar buffer */
    RELEASE bfCalidad.
END PROCEDURE.
