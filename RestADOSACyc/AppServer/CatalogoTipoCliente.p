@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Catalogo Tipo Cliente
    Purpose     : /TipoCliente

    Syntax      : Se usa en Gestion de clientes

    Description : Solo Muestra los tipos de Cliente

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.   

/* Tabla Temporal */
DEFINE TEMP-TABLE TipoCte NO-UNDO
    FIELD IdTipoCliente  AS INTEGER           
    FIELD Descr    AS CHARACTER    
    INDEX idx-mc IdTipoCliente ASCENDING.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetTipoCliente:
    /*------------------------------------------------------------------------------ 
     Purpose: Devuelve una tabla temporal con los tipos de cliente
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR TipoCte.

    /* Limpiar la tabla temporal antes de llenarla */
    EMPTY TEMP-TABLE TipoCte.

    /* Insertar los valores específicos en la tabla temporal */
    CREATE TipoCte.
    ASSIGN 
        TipoCte.IdTipoCliente = 0
        TipoCte.Descr   = "Todos".

    CREATE TipoCte.
    ASSIGN 
        TipoCte.IdTipoCliente = 1
        TipoCte.Descr   = "Contado".

    CREATE TipoCte.
    ASSIGN 
        TipoCte.IdTipoCliente = 2
        TipoCte.Descr   = "Credito".

    /* Finaliza el procedimiento devolviendo la tabla temporal */
    RETURN.
END PROCEDURE.
