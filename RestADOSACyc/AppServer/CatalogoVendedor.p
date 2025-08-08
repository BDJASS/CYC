@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Catalogo Vendedor con Nombre de Empleado
    Purpose     : /Vendedor
    Syntax      : Se usa en Reportes

    Description : Servicio para obtener información de vendedores activos con el nombre del empleado relacionado.

    Author(s)   : sis10
    Created     : 
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Tabla temporal para salida */
DEFINE TEMP-TABLE ttVendedor NO-UNDO
    FIELD IdVendedor AS CHARACTER
    FIELD Nombre     AS CHARACTER
    FIELD TipoVen    AS INTEGER
    INDEX idx-vend   IdVendedor ASCENDING.

/* ***************************  Internal Procedures *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetVendedorEmpleado:
/*------------------------------------------------------------------------------
 Purpose     : Retorna los vendedores activos con el nombre del empleado relacionado.
 Notes       : Si se envía TipoVen, filtra por tipo además de los activos.
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER TipoVen AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttVendedor.

    EMPTY TEMP-TABLE ttVendedor.

    /* Siempre tomar vendedores activos */
    FOR EACH Vendedor NO-LOCK WHERE Vendedor.Activo
                                AND (Vendedor.TipoVen = TipoVen OR TipoVen = ?):
        
        /* Buscar el empleado relacionado por Iniciales */
        FIND FIRST Empleado  WHERE Empleado.Iniciales = Vendedor.Iniciales 
        NO-LOCK NO-ERROR.

        /* Crear el registro en la tabla temporal */
        CREATE ttVendedor.
        ASSIGN
            ttVendedor.IdVendedor = Vendedor.Id-Vendedor
            ttVendedor.Nombre     = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE "Sin asignar"
            ttVendedor.TipoVen    = Vendedor.TipoVen.
    END.

END PROCEDURE.
