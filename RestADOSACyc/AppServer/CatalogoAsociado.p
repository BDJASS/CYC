@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*--------------------------------------------------------------------------  
 File        : CatalogoAsociado:p
               /Asociados
 Purpose     : Buscar asociados en la tabla Cliente, validar si están activos,
               y verificar si están registrados en blkAut.
--------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttAsociados NO-UNDO
    FIELD NumCliente      AS INTEGER
    FIELD NombreCliente   AS CHAR
    FIELD NumAsociado     AS INTEGER   /* Número del Asociado */
    FIELD NombreAsociado  AS CHARACTER /* Nombre/Razón Social del asociado */
    FIELD Activo          AS LOGICAL  /* Indicador de si el asociado está activo */
    FIELD BloqAut         AS LOGICAL.  /* Indicador de si el asociado está en blkAut */

    DEFINE BUFFER bCliente FOR Cliente.
    DEFINE BUFFER bAsociado FOR Asociado.
    DEFINE BUFFER bBlkAut FOR blkAut.
    DEFINE BUFFER bBlkRFC FOR blkRFC.
    
    DEFINE VARIABLE nombreCliente AS CHARACTER NO-UNDO. /* Nombre del cliente principal */
    
/* ***************************  Procedures **************************** */

/* PROCEDIMIENTO: Buscar Asociados */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE BuscarAsociados: 
/*--------------------------------------------------------------------------  
 Purpose     : Buscar asociados en la tabla Cliente y validar estado y registro en blkAut.
 Notes       : Devuelve una tabla temporal con los resultados.
--------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pIdCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttAsociados.

    /* Variables locales */
    DEFINE VARIABLE asociadoActivo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE enBlkAut AS LOGICAL NO-UNDO.
    DEFINE VARIABLE nombreAsociado AS CHARACTER NO-UNDO.

    /* Limpiar la tabla temporal */
    EMPTY TEMP-TABLE ttAsociados.
    
        /* Buscar el nombre del cliente principal */
    FIND FIRST bCliente WHERE bCliente.Id-Cliente = pIdCliente NO-LOCK NO-ERROR.
    IF AVAILABLE bCliente THEN
        ASSIGN nombreCliente = bCliente.RazonSocial. /* Almacena el nombre del cliente principal */
    ELSE
        ASSIGN nombreCliente = "". /* Si no se encuentra, asigna vacío */

    /* Buscar todos los asociados ligados al cliente */
    FOR EACH bAsociado WHERE bAsociado.Id-Cliente = pIdCliente NO-LOCK:

        /* Buscar al asociado en la tabla Cliente */
        FIND FIRST bCliente WHERE bCliente.Id-Cliente = bAsociado.Id-Asociado NO-LOCK NO-ERROR.

        /* Verificar si está en blkAut */
        FIND FIRST bBlkAut WHERE bBlkAut.Id-Cliente = bAsociado.Id-Asociado NO-LOCK NO-ERROR.
        enBlkAut = AVAILABLE bBlkAut. /* Será TRUE si hay registro en blkAut */


        /* Registrar los datos en la tabla temporal */ 
        CREATE ttAsociados.
        ASSIGN
            ttAsociados.NumCliente     = bAsociado.Id-Cliente
            ttAsociados.NombreCliente  = nombreCliente
            ttAsociados.NumAsociado    = bAsociado.Id-Asociado
            ttAsociados.Activo         = bCliente.Activo
            ttAsociados.BloqAut        = enBlkAut
            ttAsociados.NombreAsociado = bCliente.RazonSocial.
    END.
END PROCEDURE.
