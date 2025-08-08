@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : consultafactura.p
    Purpose     : 

    Syntax      :

    Description : 
   
    Author(s)   : sis6
    Created     : Wed Jul 23 16:54:30 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttFactura             
    FIELD IdFactura        AS CHARACTER FORMAT "x(15)"
    FIELD IdCliente        AS INTEGER                  
    FIELD RazonSocial      AS CHARACTER FORMAT "x(40)".
    
     

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetFacturasGral:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pFactura   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pCliente   AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER pVendedor  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pFechaIni  AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER pFechaFin  AS DATE      NO-UNDO. 
DEFINE OUTPUT PARAMETER TABLE FOR ttFactura.

DEFINE VARIABLE lFiltroPorFactura AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFiltroPorCliente AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFiltroPorVendedor AS LOGICAL NO-UNDO.

/* Determinar si es consulta general o por factura espec�fica */
ASSIGN 
    lFiltroPorFactura = (pFactura <> "" AND pFactura <> ? AND pFactura <> "0")
    lFiltroPorCliente = (pCliente <> 0 AND pCliente <> ?)
    lFiltroPorVendedor = (pVendedor <> "" AND pVendedor <> ? AND pVendedor <> "0").

        /* Primero procesar Remisiones */
        IF lFiltroPorFactura THEN DO:
            FOR EACH Remision WHERE Remision.Id-Remision = pFactura NO-LOCK:
                
                CREATE ttFactura.
                ASSIGN 
                    ttFactura.IdFactura = Remision.Id-Remision
                    ttFactura.IdCliente = Remision.Id-Cliente
                    ttFactura.RazonSocial = Remision.RazonSocial.
            END.
        END.
        ELSE DO:
            IF lFiltroPorCliente THEN DO:
                FOR EACH Remision WHERE Remision.Id-Cliente = pCliente 
                                    AND Remision.FecReg >= pFechaIni
                                    AND Remision.FecReg <= pFechaFin NO-LOCK:
                    
                    CREATE ttFactura.
                    ASSIGN 
                        ttFactura.IdFactura = Remision.Id-Remision
                        ttFactura.IdCliente = Remision.Id-Cliente
                        ttFactura.RazonSocial = Remision.RazonSocial.
                END.
            END.
            ELSE DO:
                FOR EACH Remision WHERE Remision.Id-Vendedor = pVendedor 
                                    AND Remision.FecReg >= pFechaIni 
                                    AND Remision.FecReg <= pFechaFin NO-LOCK:
                    
                    CREATE ttFactura.
                    ASSIGN 
                        ttFactura.IdFactura = Remision.Id-Remision
                        ttFactura.IdCliente = Remision.Id-Cliente
                        ttFactura.RazonSocial = Remision.RazonSocial.
                END.
            END.        
        END. 
        
        /* Luego procesar Facturas */
        IF lFiltroPorFactura THEN DO:
            FOR EACH Factura WHERE Factura.Id-Factura = pFactura NO-LOCK:
                
                CREATE ttFactura.
                ASSIGN 
                    ttFactura.IdFactura = Factura.Id-Factura
                    ttFactura.IdCliente = Factura.Id-Cliente
                    ttFactura.RazonSocial = Factura.RazonSocial.
            END.
        END.
        ELSE DO:
            IF lFiltroPorCliente THEN DO:
                FOR EACH Factura WHERE Factura.Id-Cliente = pCliente 
                                    AND Factura.FecReg >= pFechaIni
                                    AND Factura.FecReg <= pFechaFin NO-LOCK:
                    
                    CREATE ttFactura.
                    ASSIGN 
                        ttFactura.IdFactura = Factura.Id-Factura
                        ttFactura.IdCliente = Factura.Id-Cliente
                        ttFactura.RazonSocial = Factura.RazonSocial.
                END.
            END.
            ELSE DO:
                FOR EACH Factura WHERE Factura.Id-Vendedor = pVendedor 
                                    AND Factura.FecReg >= pFechaIni 
                                    AND Factura.FecReg <= pFechaFin NO-LOCK:
                    
                    CREATE ttFactura.
                    ASSIGN 
                        ttFactura.IdFactura = Factura.Id-Factura
                        ttFactura.IdCliente = Factura.Id-Cliente
                        ttFactura.RazonSocial = Factura.RazonSocial.
                END.
            END.        
        END.


/* No es necesario el RELEASE ttFactura ya que es una tabla temporal de salida */
RETURN.  
END PROCEDURE.

