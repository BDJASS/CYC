@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/* Tabla temporal para almacenar el resultado */
DEFINE TEMP-TABLE ttFactura
    FIELD IdFactura       AS CHARACTER
    FIELD FechaReg        AS DATE
    FIELD IdCliente       AS INTEGER
    FIELD NumCliente      AS CHAR
    FIELD PlazoNormal     AS INTEGER  /* Plazo del cliente */
    FIELD PlazoFac        AS INT     /* Plazo de la factura */
    FIELD Total           AS DECIMAL
    FIELD IdVendedor      LIKE Vendedor.Id-Vendedor
    FIELD Vendedor        AS CHAR. 

/* **********************  Internal Procedures  *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetAcuse:
    /*------------------------------------------------------------------------------
     Purpose: Obtener facturas donde el plazo de la factura supera al plazo del cliente.
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER FechaInicio  AS DATE NO-UNDO. 
    DEFINE INPUT  PARAMETER FechaFin     AS DATE NO-UNDO.  
    DEFINE INPUT  PARAMETER iVendedor    AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFactura.
   
    /* Ajuste de parámetros */
    ASSIGN
        iVendedor = IF iVendedor = ? THEN "" ELSE iVendedor. 
    
    /* Buffers */   
    DEFINE BUFFER bffactura    FOR Factura.
    DEFINE BUFFER bfCliente    FOR Cliente.
    DEFINE BUFFER bfVendedor   FOR Vendedor.
    DEFINE BUFFER bfEmpleado   FOR Empleado.
    
    /* Lógica principal */
    FOR EACH bffactura NO-LOCK 
        WHERE (FechaInicio = ? OR bffactura.FecReg >= FechaInicio)
          AND (FechaFin = ? OR bffactura.FecReg <= FechaFin)
          /* Filtro por lista de vendedores */
          AND (iVendedor = "" OR LOOKUP(STRING(bffactura.Id-Vendedor), iVendedor) > 0):
        
        /* Buscar cliente relacionado */  
        FIND FIRST bfCliente 
            WHERE bfCliente.Id-Cliente = bffactura.Id-Cliente
            NO-LOCK NO-ERROR.
        
       FIND FIRST bfVendedor 
            WHERE bfVendedor.Id-Vendedor = bffactura.Id-Vendedor
            NO-LOCK NO-ERROR.
            
        IF AVAILABLE bfVendedor THEN
            FIND FIRST bfEmpleado 
                WHERE bfEmpleado.Iniciales = bfVendedor.Iniciales
                NO-LOCK NO-ERROR.    
        /* Validar plazos si existe el cliente */
        IF AVAILABLE bfCliente AND bffactura.Plazo > bfCliente.Plazo THEN 
        DO:
            CREATE ttFactura.
            ASSIGN
                ttFactura.IdFactura   = bffactura.Id-Factura
                ttFactura.IdCliente   = bffactura.Id-Cliente  
                ttFactura.NumCliente  = bfCliente.RazonSocial 
                ttFactura.FechaReg    = bffactura.FecReg
                ttFactura.Total       = bffactura.Tot
                ttFactura.PlazoNormal = bfCliente.Plazo    /* Plazo del cliente */
                ttFactura.PlazoFac    = bffactura.Plazo    /* Plazo de la factura */
                ttFactura.IdVendedor  = bffactura.Id-Vendedor
                ttFactura.Vendedor    = IF AVAILABLE bfEmpleado THEN bfEmpleado.Nombre ELSE "Vendedor no identificado".
        END.
    END.
    RETURN.     
END PROCEDURE.