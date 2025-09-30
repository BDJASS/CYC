@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : visorligaspendientes.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Fri Jun 13 10:59:49 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.
   
DEFINE TEMP-TABLE ttDatosLiga
    FIELD FecPago          AS DATE
    FIELD HorPago          AS CHARACTER
    FIELD Referencia       AS CHARACTER
    FIELD IdCliente        AS INTEGER
    FIELD RazonSocial      AS CHARACTER
    FIELD Tipo             AS CHARACTER   
    FIELD ImportePagado    AS DECIMAL   
    FIELD ImporteFacturado AS DECIMAL.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetLigasPendientes:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttDatosLiga.

    FOR EACH MITResp WHERE MITResp.Response = "approved" AND MITResp.Estatus = FALSE AND MITResp.TpOperation = "VENTA" NO-LOCK:
        
    
        IF adosa.MITResp.Id-Pedido <> "" THEN 
        FIND FIRST Pedido WHERE Pedido.Id-Pedido = MITResp.Reference NO-LOCK NO-ERROR.
  
        IF AVAILABLE Pedido THEN 
        DO:
            FIND FIRST Cliente WHERE Cliente.Id-Cliente = Pedido.Id-Cliente NO-LOCK NO-ERROR.
                                  
            FIND Remision WHERE Remision.Id-Remision = Pedido.Id-Factura AND Remision.FecCancel = ? AND CAN-DO(Remision.Pedidos, Pedido.Id-Pedido) NO-LOCK NO-ERROR.
            CREATE ttDatosLiga.
            ASSIGN 
                ttDatosLiga.ImporteFacturado = IF AVAILABLE Remision THEN Remision.Tot ELSE 0                
                ttDatosLiga.FecPago       = MITResp.Fecha
                ttDatosLiga.HorPago       = STRING(MITResp.hora, "HH:MM")
                ttDatosLiga.Referencia    = MITResp.Reference
                ttDatosLiga.IdCliente     = Cliente.Id-Cliente
                ttDatosLiga.RazonSocial   = Cliente.RazonSocial
                ttDatosLiga.ImportePagado = MITResp.Amount
                ttDatosLiga.Tipo = "Contado". 
                
                RELEASE ttDatosLiga.
        END.
        ELSE 
        DO:  
            FIND PreAcuse WHERE PreAcuse.Id-Acuse = MITResp.Reference NO-LOCK NO-ERROR.
            IF AVAILABLE PreAcuse THEN DO:
                FIND FIRST Cliente WHERE Cliente.Id-Cliente = PreAcuse.Id-Cliente NO-LOCK NO-ERROR.
                FIND ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
                CREATE ttDatosLiga.
                ASSIGN ttDatosLiga.Tipo = ClaseCte.Descr
                       ttDatosLiga.ImporteFacturado = 0
                        ttDatosLiga.FecPago       = MITResp.Fecha
                        ttDatosLiga.HorPago       = STRING(MITResp.hora, "HH:MM")
                        ttDatosLiga.Referencia    = MITResp.Reference
                        ttDatosLiga.IdCliente     = Cliente.Id-Cliente
                        ttDatosLiga.RazonSocial   = Cliente.RazonSocial
                        ttDatosLiga.ImportePagado = MITResp.Amount.
                        
                RELEASE ttDatosLiga.
                
            END.  
            ELSE DO:
                
                FIND FIRST Remision WHERE Remision.Id-Remision = MITResp.Reference NO-LOCK NO-ERROR.
                
                CREATE ttDatosLiga.
            ASSIGN 
                ttDatosLiga.ImporteFacturado = IF AVAILABLE Remision THEN Remision.Tot ELSE 0                
                ttDatosLiga.FecPago       = MITResp.Fecha
                ttDatosLiga.HorPago       = STRING(MITResp.hora, "HH:MM")
                ttDatosLiga.Referencia    = MITResp.Reference
                ttDatosLiga.IdCliente     = Remision.Id-Cliente
                ttDatosLiga.RazonSocial   = Remision.RazonSocial 
                ttDatosLiga.ImportePagado = MITResp.Amount
                ttDatosLiga.Tipo = "Contado". 
                /*
                CREATE ttDatosLiga.
                ASSIGN 
                       ttDatosLiga.ImporteFacturado = 0
                        ttDatosLiga.FecPago       = MITResp.Fecha
                        ttDatosLiga.HorPago       = STRING(MITResp.hora, "HH:MM")
                        ttDatosLiga.Referencia    = MITResp.Reference                        
                        ttDatosLiga.ImportePagado = MITResp.Amount.      
                 */                     
                RELEASE ttDatosLiga.
            END.              
        END.   
    END.
RETURN.
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostLigasPendientes:   
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER Referencia AS CHARACTER NO-UNDO.

DO TRANSACTION:
    
    FIND FIRST MitResp WHERE MITResp.Reference = Referencia EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAILABLE MITResp THEN DO:
        
        ASSIGN MITResp.Estatus = TRUE.
    END.    
    
END.
RETURN.
END PROCEDURE.

