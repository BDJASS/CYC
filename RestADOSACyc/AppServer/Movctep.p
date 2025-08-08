@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : carteractevisor.p
    Purpose     : /MovimientoCliente

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Tue Oct 22 14:47:26 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num         AS INT.  
DEFINE VARIABLE l-tot-total   AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig     AS DECIMAL   FORMAT ">>>,>>>,>>9.99". 
DEFINE VARIABLE l-tot-ven     AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv    AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-clase       AS CHARACTER FORMAT "X(20)". 
DEFINE VARIABLE l-segmento    AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE l-estatus     AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-resp        AS CHARACTER FORMAT "X(30)" .
DEFINE VARIABLE l-moneda      AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-tipo-moneda AS INT.
DEFINE VARIABLE l-prompago    AS DECIMAL.
DEFINE VARIABLE l-tot-30      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-31      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-61      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-91      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-dia         AS INT.
DEFINE VARIABLE l-dia-max     AS INT  FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-saldo       AS DEC  FORMAT ">>>,>>>,>>9.99".


DEFINE TEMP-TABLE ttCliente NO-UNDO
    FIELD IdCliente    LIKE Cliente.Id-Cliente
    FIELD RazonSocial  LIKE Cliente.RazonSocial
    FIELD Telefono     LIKE Cliente.Tel1
    FIELD CalleNo      LIKE Cliente.CalleNo
    FIELD NumExterior  LIKE Cliente.NumExt
    FIELD Colonia      LIKE Cliente.Colonia
    FIELD Estatus      LIKE Cliente.Activo
    FIELD LineaCredito LIKE Cliente.Limite
    FIELD Plazo        LIKE Cliente.Plazo
    FIELD IdVendedor   LIKE Cliente.Id-Vendedor
    FIELD Vendedor     AS CHARACTER
    FIELD IdResp       LIKE Cliente.Id-Resp
    FIELD Resp         LIKE Resp.Nombre
    FIELD IdCob        LIKE Cliente.Id-Cobrador
    FIELD Cobrador     AS CHARACTER 
    INDEX idx-clase IdCliente  ASCENDING.

DEFINE TEMP-TABLE ttDetalle NO-UNDO 
    FIELD IdCliente      LIKE MovCliente.Id-Cliente
    FIELD Documento      LIKE MovCliente.RefSaldo
    FIELD Fecha          LIKE MovCliente.FecReg
    FIELD PlazoFactura   AS INT 
    FIELD Descripcion    AS CHAR
    FIELD Cargo          AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Credito        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Saldo          LIKE MovCliente.Saldo
    FIELD Antiguedad     AS CHAR
    FIELD Referencia     AS CHAR
    FIELD EntradaInforme LIKE Pedido.RecCte
    FIELD FolioEstatus   LIKE Factura.CteEstatus
    FIELD Acuse          LIKE Acuse.Id-Acuse
    FIELD Registro       LIKE MovCliente.FecReg
    INDEX idx-clase IdCliente  ASCENDING.
    
DEFINE TEMP-TABLE ttCartera NO-UNDO
    FIELD id           AS INTEGER
    FIELD IdCliente    AS INTEGER
    FIELD saldo        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD vigente      AS DECIMAL   FORMAT ">>>,>>>,>>9.99" 
    FIELD porvencer    AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD montovencido AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD treinta      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD sesenta      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventa      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventamas   AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD diacartera   AS INTEGER
    FIELD promedio     AS DECIMAL
    INDEX idx-clase IdCliente ASCENDING.
/* Definir el DATASET con relaciones */ 
DEFINE DATASET dsMov FOR 
    ttCliente, /* Tabla principal */
    ttDetalle, /* Relación con Cliente */
    ttCartera  /* Relación con Cliente */
    DATA-RELATION ClienteDetalle FOR ttCliente, ttDetalle
        RELATION-FIELDS (IdCliente, IdCliente) /* Relación por IdCliente */
    DATA-RELATION ClienteCartera FOR ttCliente, ttCartera
        RELATION-FIELDS (IdCliente, IdCliente). /* Relación por IdCliente */

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCartera:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER  lCliente  AS INT.
    DEFINE OUTPUT PARAMETER Respuesta AS CHAR. 
    DEFINE OUTPUT PARAMETER DATASET FOR dsMov.

    ASSIGN 
        l-num    = 0
        l-moneda = "".   

    EMPTY TEMP-TABLE ttCartera.
    
    FIND FIRST Cliente WHERE Cliente.Id-Cliente = lCliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cliente OR lCliente = 0 THEN
    DO:
        ASSIGN Respuesta ="El cliente NO Existe".
        RETURN.
    END. 
    IF Cliente.Activo = FALSE THEN DO:
     FIND FIRST Usuario WHERE Usuario.Id-User = Cliente.Id-User NO-LOCK NO-ERROR.
      /* Construir la cadena */
      ASSIGN Respuesta = "Cuenta inactivada por: " + (IF AVAILABLE Usuario THEN CAPS(Usuario.Nom-usuario) ELSE "") + " " 
                          + "Fecha: " + (IF Cliente.FecBaja <> ? 
                                        THEN STRING(Cliente.FecBaja, "99/99/9999") 
                                        ELSE "Sin fecha") + "          ".
      RETURN.
     END.   
    IF AVAILABLE Cliente THEN DO :
    CREATE ttCliente.
    ASSIGN   
     ttCliente.IdCliente    = Cliente.Id-Cliente  
     ttCliente.RazonSocial  = Cliente.RazonSocial
     ttCliente.Telefono     = Cliente.Tel1 + " " + Cliente.Tel2 + " " + Cliente.Tel3
     ttCliente.CalleNo      = Cliente.CalleNo
     ttCliente.NumExterior  = Cliente.NumExt
     ttCliente.Colonia      = Cliente.Colonia
     ttCliente.Estatus      = Cliente.Activo
     ttCliente.LineaCredito = Cliente.Limite
     ttCliente.Plazo        = Cliente.Plazo
     ttCliente.IdVendedor   = Cliente.Id-Vendedor
     ttCliente.IdResp       = Cliente.Id-Resp .
   
     FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
     FIND FIRST Cobrador WHERE Cobrador.Id-Cobrador = Cliente.Id-Cobrador NO-LOCK NO-ERROR.
     FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = Cliente.Id-Vendedor NO-LOCK NO-ERROR.
     IF AVAILABLE Vendedor THEN DO:
        FIND FIRST empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
     END.
     ASSIGN
     ttCliente.Vendedor     = empleado.Nombre WHEN AVAILABLE empleado
     ttCliente.Resp         = Resp.Nombre WHEN AVAILABLE Resp
     ttCliente.IdCob        = Cliente.Id-Cobrador
     ttCliente.Cobrador     = Cobrador.Nombre WHEN AVAILABLE Cobrador. 
        
        
        
        FOR EACH Movcliente WHERE Movcliente.Id-Cliente = Cliente.Id-Cliente 
                              AND Movcliente.FecReg <= TODAY                 
                              AND MovCliente.Id-MC  <= 3                     
                              AND MovCliente.Afectado                       
                             NO-LOCK  BREAK  BY Cliente.Id-Cliente
                                             BY Cliente.RazonSocial 
                                             BY MovCliente.Id-Cliente:
            IF MovCliente.Saldo <= 0 THEN NEXT.                    
            ASSIGN
                l-clase    = "Local" 
                l-segmento = ""
                l-resp     = "". 
            IF FIRST-OF(Cliente.Id-Cliente) THEN  
                ASSIGN
                    l-tot-total = 0
                    l-tot-vig   = 0
                    l-tot-ven   = 0
                    l-tot-porv  = 0
                    l-estatus   = ""
                    l-saldo     = 0
                    l-tot-30    = 0
                    l-tot-31    = 0
                    l-tot-61    = 0
                    l-tot-91    = 0. 
            l-saldo = l-saldo + MovCliente.Saldo.   
            FIND TabMC OF MovCliente NO-LOCK NO-ERROR.
            FIND Acuse WHERE Acuse.Id-Acuse = MovCliente.Documento NO-LOCK NO-ERROR.
            CREATE ttDetalle.
             ASSIGN   
             ttDetalle.IdCliente = MovCliente.Id-Cliente
             ttDetalle.Documento = MovCliente.RefSaldo 
             ttDetalle.Fecha     = MovCliente.FecReg
             ttDetalle.Saldo     = MovCliente.Saldo
             ttDetalle.PlazoFactura = Cliente.Plazo 
             ttDetalle.Descripcion  = (IF MovCliente.Id-NCR <>"" THEN SUBSTRING(TabMC.Descr,1,4)+ "NCR:" + MovCliente.Id-NCR ELSE TabMC.Descr )
             ttDetalle.Cargo        = (IF MovCliente.Importe > 0 THEN MovCliente.Importe ELSE 0)
             ttDetalle.Credito      = (IF MovCliente.Importe <= 0 THEN MovCliente.Importe * -1 ELSE 0)
             ttDetalle.Registro     = MovCliente.FecCap.
            IF MovCliente.fecven <  TODAY THEN  
            DO:
                ASSIGN
                    l-tot-ven = l-tot-ven  +  Movcliente.Saldo.
            END. 
            IF MovCliente.fecven >= TODAY + 16 THEN 
            DO:
                ASSIGN 
                    l-tot-vig = l-tot-vig +  Movcliente.Saldo.
            END.
            IF MovCliente.fecven >= TODAY AND
                MovCliente.fecven <= TODAY + 15 THEN 
            DO:
                ASSIGN 
                    l-tot-porv = l-tot-porv +  Movcliente.Saldo.
            END.  
            l-dia =  TODAY - MovCliente.FecVenc. 
        
            IF l-dia <= 30 THEN
                ASSIGN l-tot-30 = l-tot-30 + Movcliente.Saldo. /* 1-30 */
            ELSE IF l-dia <= 60 THEN
                ASSIGN l-tot-31 = l-tot-31 + Movcliente.Saldo. /* 31-60 */
            ELSE IF l-dia <= 90 THEN
                ASSIGN l-tot-61 = l-tot-61 + Movcliente.Saldo. /* 61-90 */
            ELSE  
                ASSIGN l-tot-91 = l-tot-91 + Movcliente.Saldo. /* 91.. + */
                    
              
            IF LAST-OF(Cliente.id-cliente) THEN   
            DO: 
               
                l-num = l-num + 1.  
                  
                FIND FIRST ttCartera WHERE ttCartera.id = l-num NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttCartera THEN  
                    CREATE ttCartera.
                ASSIGN
                    ttCartera.id           = l-num
                    ttCartera.IdCliente    = Cliente.id-cliente
                    ttCartera.saldo        = l-saldo 
                    ttCartera.montovencido = l-tot-ven
                    ttCartera.vigente      = l-tot-vig
                    ttCartera.porvencer    = l-tot-porv
                    ttCartera.treinta      = l-tot-30 
                    ttCartera.sesenta      = l-tot-31
                    ttCartera.noventa      = l-tot-61
                    ttCartera.noventamas   = l-tot-91
                    ttCartera.diacartera   = l-dia-max
                    ttCartera.promedio     = l-prompago.  
                RELEASE ttCartera.
                ASSIGN
                l-saldo    = 0
                l-tot-vig  = 0
                l-tot-ven  = 0
                l-tot-porv = 0
                l-tot-30   = 0
                l-tot-31   = 0
                l-tot-61   = 0
                l-tot-91   = 0.
            END.
        END.
    END.  
    
  
    
    FOR EACH ttCartera  :
      DO TRANSACTION:                                                                             
        RUN cxcb0270.p(INPUT ttCartera.IdCliente,INPUT TODAY,OUTPUT l-dia-max ,OUTPUT l-prompago).
         IF l-dia-max = ? THEN l-dia-max = 0.
      END.
      ASSIGN  ttCartera.diacartera   = l-dia-max
              ttCartera.promedio     = INTEGER(ROUND(l-prompago, 0)). /* Redondea a un entero */
    END.   
END PROCEDURE. 


