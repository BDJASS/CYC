@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : detfactura.p
    Purpose     : 

    Syntax      :

    Description : 
  
    Author(s)   : sis6
    Created     : Mon Jul 28 12:44:13 CST 2025
    Notes       :
  ----------------------------------------------------------------------
    Ajuste 12082025 Cambio enviado CRYB
  
  */

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttFactura NO-UNDO               
    FIELD IdFactura        AS CHARACTER FORMAT "x(15)" /* Número de factura */
    FIELD IdCliente        AS INTEGER                  /* Número de cliente */
    FIELD FecReg           AS DATE                     /* Fecha de registro */
    FIELD RazonSocial      AS CHARACTER FORMAT "x(40)" /* Razón social del cliente */
    FIELD RegimenFiscal    LIKE RFiscal.Id-RFiscal
    FIELD RegimenFiscalDes LIKE RFiscal.Descr
    FIELD UsoCFDI          AS CHARACTER FORMAT "x(3)"  /* Uso CFDI */
    FIELD UsoCFDIDes       AS CHARACTER
    FIELD Requisicion      LIKE Factura.requisicion
    FIELD Plazo            LIKE Factura.Plazo
    FIELD CalleNo          AS CHARACTER FORMAT "x(50)" /* Dirección */
    FIELD Colonia          AS CHARACTER FORMAT "x(30)" /* Colonia */
    FIELD Ciudad           AS CHARACTER FORMAT "x(30)" /* Ciudad */
    FIELD Estado           AS CHARACTER FORMAT "x(30)" /* Estado */
    FIELD CP               AS CHARACTER FORMAT "x(10)" /* Código postal */
    FIELD Subtotal         AS DECIMAL   FORMAT ">>>,>>9.99" /* Subtotal */
    FIELD Descuento        AS DECIMAL   FORMAT ">>>,>>9.99" /* Descuento */
    FIELD ImpFlete         AS DECIMAL   FORMAT ">>>,>>9.99" /* Importe Flete */
    FIELD ImpSeguro        AS DECIMAL   FORMAT ">>>,>>9.99" /* Importe Seguro */
    FIELD IVA              AS DECIMAL   FORMAT ">>>,>>9.99" /* IVA */
    FIELD Total            AS DECIMAL   FORMAT ">>>,>>9.99" /* Total */
    FIELD Pedidos          AS CHARACTER 
    FIELD Bultos           AS INTEGER 
    FIELD Guias            AS CHARACTER
    FIELD Tarimas          AS INTEGER
    FIELD FolioFiscal      AS CHARACTER
    FIELD Vendedor         AS CHARACTER
    FIELD RFC              AS CHARACTER
    FIELD UUID             AS CHARACTER.     

DEFINE TEMP-TABLE ttDetFactura NO-UNDO
    FIELD IdFactura      AS CHARACTER FORMAT "x(15)" /* Número de factura */
    FIELD IdArticulo     AS CHARACTER FORMAT "x(10)" /* Código de artículo */
    FIELD Descripcion    LIKE DetFactura.Descr /* Descripción del artículo */
    FIELD Presentacion   LIKE DetFactura.Descr /* Presentación */
    FIELD ArtColor       AS CHARACTER
    FIELD Cantidad       AS DECIMAL   FORMAT ">>>,>>9" /* Cantidad vendida */
    FIELD PrecioUnitario AS DECIMAL   FORMAT ">>>,>>9.99" /* Precio unitario */
    FIELD Descuento      LIKE detfactura.descto
    FIELD Importe        AS DECIMAL   FORMAT ">>>,>>9.99". /* Importe total */

DEFINE DATASET dsFactura FOR ttFactura, ttDetFactura
    DATA-RELATION RelFacturaDetalle FOR ttFactura, ttDetFactura 
    RELATION-FIELDS (IdFactura, IdFactura).

DEFINE TEMP-TABLE tt-Factura LIKE Factura.
DEFINE TEMP-TABLE tt-DetFactura LIKE DetFactura.
DEFINE TEMP-TABLE tt-DetSerie LIKE DetSerie.

DEFINE VARIABLE l-IdFactura    AS CHARACTER NO-UNDO FORMAT "x(15)". /* Número de factura */
DEFINE VARIABLE l-Nivel        AS INTEGER   NO-UNDO. /* Nivel del usuario */
DEFINE VARIABLE l-saldo        LIKE MovCliente.Saldo.
DEFINE VARIABLE l-NMov         AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-recid        AS RECID     NO-UNDO.
DEFINE VARIABLE l-tipo         AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-UsoCfdi      LIKE Factura.Id-UsoCFDI NO-UNDO.
DEFINE VARIABLE l-UsoCfdiDes   AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Req          LIKE Factura.Requisicion NO-UNDO.
DEFINE VARIABLE l-RFiscal      LIKE Rfiscal.Id-RFiscal NO-UNDO.
DEFINE VARIABLE l-Cliente      LIKE Factura.Id-Cliente NO-UNDO.
DEFINE VARIABLE l-DescrRFiscal AS CHARACTER NO-UNDO FORMAT "X(58)".
DEFINE VARIABLE l-Aceptar      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-Pres         LIKE ArtPres.Descr NO-UNDO FORMAT 'X(8)'.

DEFINE VARIABLE l-titulo       AS CHARACTER.
DEFINE VARIABLE l-Timbrada     AS LOGICAL   NO-UNDO.
DEFINE BUFFER g-Folio     FOR Folio.
DEFINE BUFFER b-movcaja   FOR MovCaja.
DEFINE BUFFER b-Pedido    FOR Pedido.
DEFINE BUFFER b-EstPedido FOR EstPedido.
DEFINE BUFFER b-Movim     FOR Movim.


DEF    VAR      l-fecvence   AS DATE.
DEF    VAR      l-ubic       AS CHAR .
DEF    VAR      l-recmov     AS RECID.

DEF    VAR      l-rec        AS RECID     NO-UNDO.
DEF    VAR      l-usuario    LIKE Password.Usuario.
DEF    VAR      l-acuse      LIKE Acuse.id-Acuse.
DEF    VAR      cp-question  AS CHAR.
DEF    VAR      cp-answer    AS LOGICAL.

DEF    VAR      l-NFactura   LIKE Factura.Id-Factura NO-UNDO.
DEF    VAR      l-NUUID      LIKE Factura.UUID NO-UNDO.
DEF    VAR      l-Anter      AS LOGICAL   NO-UNDO.
DEF    VAR      l-AnoAnt     AS INTEGER   NO-UNDO.
DEF    VAR      l-MesAnt     AS INTEGER   NO-UNDO.

DEFINE VARIABLE l-Asunto     AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Contenido  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-MailDe     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ResponderA AS CHARACTER NO-UNDO.  
DEFINE VARIABLE l-Mail       AS CHARACTER NO-UNDO.  
DEFINE VARIABLE v-Enviado    AS LOGICAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDetFactura:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pIdFactura AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta AS CHAR. 
    DEFINE OUTPUT PARAMETER DATASET FOR dsFactura.
    
    
    /* Buscar la factura */
    FIND Factura WHERE Factura.Id-Factura = pIdFactura NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE Factura THEN 
    DO:
        FIND Remision WHERE Remision.Id-Remision = pIdFactura NO-LOCK NO-ERROR.
        
        IF NOT AVAILABLE Remision THEN 
        DO:
            ASSIGN 
                Respuesta = "El Folio de la factura no esta registrada.".
            RETURN.
        END.
        ELSE 
        DO:
        
            
            l-DescrRFiscal = "".
            l-UsoCfdi      = "". 
            /* pendiente validar que la factura no tenga ningun abono */
            FIND Cliente WHERE Cliente.id-cliente = Remision.id-cliente NO-LOCK NO-ERROR.
            FIND Ciudad WHERE Ciudad.id-ciudad = Remision.id-ciudad NO-LOCK NO-ERROR.
            IF AVAILABLE Ciudad THEN 
            DO:
                FIND FIRST Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.    
            END. 
                
            FIND FIRST RFiscal WHERE RFiscal.Id-RFiscal = Remision.Id-RFiscal NO-LOCK NO-ERROR.
            IF AVAILABLE RFiscal THEN 
                ASSIGN 
                    l-DescrRFiscal = RFiscal.Descr.
            ELSE 
                ASSIGN 
                    l-DescrRFiscal = "".
                                   
            FIND FIRST UsoCFDI WHERE UsoCFDI.Id-UsoCFDI = Remision.Id-UsoCFDI NO-LOCK NO-ERROR.
            IF AVAILABLE UsoCFDI THEN
                ASSIGN 
                    l-UsoCfdi = UsoCFDI.Descr. 
            ELSE          
                ASSIGN
                    l-UsoCfdi = "" .  
                             
            ASSIGN 
                l-recid = RECID(Remision)
                l-tipo  = 3. 
                        
            l-Cliente = Remision.Id-Cliente .
            l-Req     = Remision.Requisicion.
            
            FIND FIRST Guia WHERE Guia.Id-Factura = Remision.Id-Remision NO-LOCK NO-ERROR.
            
            /* Agregar información de la factura a la tabla temporal */
            CREATE ttFactura.
            ASSIGN 
                ttFactura.IdFactura        = Remision.Id-Remision
                ttFactura.FecReg           = Remision.FecReg
                ttFactura.IdCliente        = l-Cliente
                ttFactura.RazonSocial      = Remision.RazonSocial
                ttFactura.RegimenFiscal    = Remision.Id-RFiscal //l-RFiscal 
                ttFactura.RegimenFiscalDes = l-DescrRFiscal
                ttFactura.CalleNo          = Remision.CalleNo
                ttFactura.Colonia          = Remision.Colonia
                ttFactura.CP               = Remision.CP        //Cliente.CP
                ttFactura.Ciudad           = IF AVAILABLE Ciudad THEN Ciudad.Nombre ELSE " "
                ttFactura.Estado           = IF AVAILABLE Estado THEN Estado.Nomcto ELSE " "
                ttFactura.UsoCFDI          = Remision.Id-UsoCFDI
                ttFactura.UsoCFDIDes       = l-UsoCfdi
                ttFactura.Requisicion      = l-Req     
                ttFactura.Plazo            = 0
                ttFactura.Subtotal         = Remision.Subtotal
                ttFactura.Descuento        = Remision.Descuento   
                ttFactura.ImpFlete         = 0
                ttFactura.ImpSeguro        = 0
                ttFactura.IVA              = Remision.Iva      
                ttFactura.Total            = Remision.Tot
                ttFactura.Pedidos          = Remision.Pedidos
                ttFactura.Bultos           = Remision.Bultos
                ttFactura.Guias            = IF AVAILABLE Guia THEN Guia.Id-Guia ELSE ""
                ttFactura.Tarimas          = Remision.Tarimas
                ttFactura.FolioFiscal      = Remision.Folioe
                ttFactura.Vendedor         = Remision.Id-Vendedor
                ttFactura.RFC              = Remision.RFC
                ttFactura.UUID             = Remision.UUID.       
            
            /* Obtener el detalle de los artículos vendidos */  
            FOR EACH DetRemis OF Remision NO-LOCK:
                    
                FIND ArtPres WHERE ArtPres.Id-Articulo = DetRemis.Id-Articulo
                    AND ArtPres.Id-Pres = DetRemis.Id-Pres NO-LOCK NO-ERROR.
                ASSIGN 
                    l-Pres = IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE ".".     
                    
                FIND FIRST ArtCol WHERE ArtCol.Id-Articulo = DetRemis.Id-Articulo AND ArtCol.Id-color = DetRemis.Id-color NO-LOCK NO-ERROR.    
                FIND Kolor OF ArtCol NO-LOCK NO-ERROR.
                CREATE ttDetFactura.
                ASSIGN 
                    ttDetFactura.IdFactura      = Remision.Id-Remision
                    ttDetFactura.IdArticulo     = DetRemis.Id-Articulo
                    ttDetFactura.Descripcion    = DetRemis.Descr
                    ttDetFactura.Presentacion   = l-Pres
                    ttDetFactura.Cantidad       = detRemis.Cant
                    ttDetFactura.PrecioUnitario = DetRemis.PrecUnit
                    ttDetFactura.Importe        = DetRemis.Importe
                    ttDetFactura.ArtColor       = IF AVAILABLE ArtCol THEN Kolor.Descr ELSE "".   
            END.
        END.
        
    END.
    ELSE 
    DO:
        
        l-DescrRFiscal = "".
        l-UsoCfdi      = "". 
        /* pendiente validar que la factura no tenga ningun abono */
        FIND Cliente WHERE Cliente.id-cliente = Factura.id-cliente NO-LOCK NO-ERROR.
        FIND Ciudad WHERE Ciudad.id-ciudad = Factura.id-ciudad NO-LOCK NO-ERROR.
        IF AVAILABLE Ciudad THEN 
        DO:
            FIND FIRST Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.    
        END. 
        
        FIND FIRST RFiscal WHERE RFiscal.Id-RFiscal = Factura.Id-RFiscal NO-LOCK NO-ERROR.
        IF AVAILABLE RFiscal THEN 
            ASSIGN 
                l-DescrRFiscal = RFiscal.Descr.
        ELSE 
            ASSIGN 
                l-DescrRFiscal = "".
                           
        FIND FIRST UsoCFDI WHERE UsoCFDI.Id-UsoCFDI = Factura.Id-UsoCFDI NO-LOCK NO-ERROR.
        IF AVAILABLE UsoCFDI THEN
            ASSIGN 
                l-UsoCfdi = UsoCFDI.Descr. 
        ELSE          
            ASSIGN
                l-UsoCfdi = "" .  
                     
        ASSIGN 
            l-recid = RECID(Factura)
            l-tipo  = 3. 
                
        l-Cliente = Factura.Id-Cliente .
        l-Req     = Factura.Requisicion.
            
        FIND FIRST Guia WHERE Guia.Id-Factura = Factura.Id-Factura NO-LOCK NO-ERROR.    
            
        /* Agregar información de la factura a la tabla temporal */
        CREATE ttFactura.
        ASSIGN 
            ttFactura.IdFactura        = Factura.Id-Factura
            ttFactura.FecReg           = Factura.FecReg
            ttFactura.IdCliente        = l-Cliente
            ttFactura.RazonSocial      = Factura.RazonSocial
            ttFactura.RegimenFiscal    = Factura.Id-RFiscal //l-RFiscal 
            ttFactura.RegimenFiscalDes = l-DescrRFiscal
            ttFactura.CalleNo          = Factura.CalleNo
            ttFactura.Colonia          = Factura.Colonia
            ttFactura.CP               = Factura.CP        //Cliente.CP
            ttFactura.Ciudad           = IF AVAILABLE Ciudad THEN Ciudad.Nombre ELSE " "
            ttFactura.Estado           = IF AVAILABLE Estado THEN Estado.Nomcto ELSE " "
            ttFactura.UsoCFDI          = Factura.Id-UsoCFDI
            ttFactura.UsoCFDIDes       = l-UsoCfdi
            ttFactura.Requisicion      = l-Req     
            ttFactura.Plazo            = Factura.Plazo
            ttFactura.Subtotal         = Factura.Subtotal
            ttFactura.Descuento        = Factura.Descuento   
            ttFactura.ImpFlete         = Factura.ImpFlete
            ttFactura.ImpSeguro        = Factura.ImpSeguro
            ttFactura.IVA              = Factura.Iva      
            ttFactura.Total            = Factura.Tot
            ttFactura.Pedidos          = Factura.Pedidos
            ttFactura.Bultos           = Factura.Bultos
            ttFactura.Guias            = IF AVAILABLE Guia THEN Guia.Id-Guia ELSE ""
            ttFactura.Tarimas          = Factura.Tarimas
            ttFactura.FolioFiscal      = Factura.Folioe
            ttFactura.Vendedor         = Factura.Id-Vendedor  /* 12082025 */
            ttFactura.RFC              = Factura.RFC          /* 12082025 */
            ttFactura.UUID             = Factura.UUID.        /* 12082025 */

        /* Obtener el detalle de los artículos vendidos */  
        FOR EACH DetFactura OF Factura NO-LOCK:
            
            FIND ArtPres WHERE ArtPres.Id-Articulo = DetFactura.Id-Articulo
                AND ArtPres.Id-Pres = DetFactura.Id-Pres NO-LOCK NO-ERROR.
            ASSIGN 
                l-Pres = IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE ".".     
            
            FIND FIRST ArtCol WHERE ArtCol.Id-Articulo = DetFactura.Id-Articulo AND ArtCol.Id-color = DetFactura.Id-color NO-LOCK NO-ERROR.    
            FIND Kolor OF ArtCol NO-LOCK NO-ERROR.
                
            CREATE ttDetFactura.
            ASSIGN 
                ttDetFactura.IdFactura      = Factura.Id-Factura
                ttDetFactura.IdArticulo     = DetFactura.Id-Articulo
                ttDetFactura.Descripcion    = DetFactura.Descr
                ttDetFactura.Presentacion   = l-Pres
                ttDetFactura.Cantidad       = detfactura.Cant
                ttDetFactura.PrecioUnitario = DetFactura.PrecUnit
                ttDetFactura.Importe        = DetFactura.Importe
                ttDetFactura.ArtColor       = IF AVAILABLE ArtCol THEN Kolor.Descr ELSE "".
        END.
        
        
    END.  
    
    

END PROCEDURE.

