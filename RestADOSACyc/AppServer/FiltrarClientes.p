@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : FiltrarClientes.p
    URL         : /FiltrarClientes
    Module      : Gestión de Clientes
    Purpose     : Servicio REST para filtrar clientes según parámetros de clase y estado.
    Sprint      : 3
    Author(s)   : sis10
    Created     : (Coloca la fecha de creación aquí)
    Notes       : Este servicio permite filtrar clientes activos/inactivos y clasificar
                  los resultados según el tipo de cliente, datos generales y fecha de registro.
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. 

/* Definición de la tabla temporal para los resultados */
DEFINE TEMP-TABLE ttClientes NO-UNDO
    FIELD TipoCliente    AS CHARACTER FORMAT "X(10)" /* Credito o Contado */
    FIELD Segmento       AS CHARACTER
    FIELD NumCliente     AS INTEGER
    FIELD RazonSocial    AS CHARACTER
    FIELD RegimenCapital AS CHARACTER
    FIELD Correo         AS CHARACTER
    FIELD FechaDeAlta    AS DATE
    FIELD Bloqueado      AS LOGICAL
    FIELD Activo         AS LOGICAL
    FIELD RFC            AS CHAR
    INDEX idx-FechaDeAlta IS PRIMARY FechaDeAlta DESCENDING. /* Índice por FechaDeAlta, descendente */
    DEFINE DATASET dsClientes FOR ttClientes.
/* ***************************  Main Block **************************** */


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE FiltrarClientes:
    /*------------------------------------------------------------------------------ 
     Purpose: Filtrar clientes por Clase e indicador Activo.
     Notes: Este procedimiento devuelve una tabla temporal con clientes clasificados
            por tipo (Contado/Crédito), con sus datos básicos y ordenados por FechaDeAlta.
    ------------------------------------------------------------------------------*/

    /* Parámetros de entrada */
    DEFINE INPUT PARAMETER Clase AS INTEGER NO-UNDO. /* Clase del cliente */
    DEFINE INPUT PARAMETER Activo AS LOGICAL NO-UNDO. /* Si el cliente está activo */

    /* Parámetros de salida */
    DEFINE OUTPUT PARAMETER TABLE FOR ttClientes.

    /* Buffers */
    DEFINE BUFFER bfCliente FOR Cliente.
    DEFINE BUFFER bfBlkAut FOR BlkAut.

    /* Validar si no se enviaron los parámetros */
    IF Clase = ? OR Activo = ? THEN DO: 
        RETURN ERROR "No se enviaron los parámetros requeridos: 'Clase' y/o 'Activo'.".
    END.

    /* Limpiar la tabla temporal */
    EMPTY TEMP-TABLE ttClientes. 

    /* Filtrar los clientes según los parámetros recibidos */
    FOR EACH bfCliente WHERE bfCliente.Id-ClaseCte = Clase
                        AND bfCliente.Activo = Activo NO-LOCK
                        BY bfCliente.FecReg DESCENDING:

        /* Buscar en BlkAut por el cliente actual */
        FIND FIRST bfBlkAut WHERE bfBlkAut.Id-Cliente = bfCliente.Id-Cliente NO-LOCK NO-ERROR.
        FIND FIRST SegmentoCte WHERE SegmentoCte.Id-SegmentoCte = bfCliente.Id-SegmentoCte NO-LOCK NO-ERROR.
        /* Crear un registro en la tabla temporal */
        CREATE ttClientes.
        ASSIGN
            /* Determinar el tipo de cliente según su límite */
            ttClientes.TipoCliente = IF bfCliente.Limite > 0 THEN "Credito" ELSE "Contado"
            /* Copiar los demás campos */
            ttClientes.NumCliente     = bfCliente.Id-Cliente
            ttClientes.Segmento       = IF AVAILABLE SegmentoCte THEN SegmentoCte.Descr ELSE "SIN SEGMENTO"
            ttClientes.RazonSocial    = bfCliente.RazonSocial
            ttClientes.RegimenCapital = bfCliente.RSocietario 
            ttClientes.Correo         = bfCliente.e-mail
            ttClientes.FechaDeAlta    = bfCliente.FecReg
            /* Llenar el campo Bloqueado según BlkAut */
            ttClientes.Bloqueado      = IF AVAILABLE bfBlkAut THEN TRUE ELSE FALSE
            ttClientes.Activo         = bfCliente.Activo
            ttClientes.RFC            = bfCliente.RFC.

        /* Liberar el buffer de bfCliente y bfBlkAut */ 
        RELEASE bfCliente.
        RELEASE bfBlkAut.
    END.  

    /* Liberar buffer general */
    RELEASE bfCliente.

END PROCEDURE.

