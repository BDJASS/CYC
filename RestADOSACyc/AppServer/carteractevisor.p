@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : carteractevisor.p
    Purpose     : /CarteraVisor
   
    Syntax      :   

    Description : 

    Author(s)   : sis10
    Created     : Tue Oct 22 14:47:26 CST 2024
    Notes       :
  ----------------------------------------------------------------------
  
  Ticket 1266 En facturas extranjeras hacer conversion por tipo de cambio
  JASS18072025
  
  Ticket 1385  Se integra la columna de Monto Pagare [Analisis de Saldos]
  JASS25072025
  
  */

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num              AS INT.  
DEFINE VARIABLE l-tot-total        AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig          AS DECIMAL   FORMAT ">>>,>>>,>>9.99". 
DEFINE VARIABLE l-tot-ven          AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv         AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-clase            AS CHARACTER FORMAT "X(20)". 
DEFINE VARIABLE l-segmento         AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE l-estatus          AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-resp             AS CHARACTER FORMAT "X(30)" .
DEFINE VARIABLE l-moneda           AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-tipo-moneda      AS INT.
DEFINE VARIABLE l-prompago         AS DECIMAL.
DEFINE VARIABLE l-tot-30           AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-31           AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-61           AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-91           AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-dia              AS INT.
DEFINE VARIABLE l-dia-max          AS INT       FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-saldo            AS DEC       FORMAT ">>>,>>>,>>9.99".

DEFINE VARIABLE cClientesExcluidos AS CHARACTER NO-UNDO.

    
DEFINE TEMP-TABLE ttCartera NO-UNDO
    FIELD id           AS INTEGER
    FIELD clasecliente AS CHARACTER FORMAT "X(12)"
    FIELD numcliente   AS INTEGER
    FIELD cliente      AS CHARACTER FORMAT "X(40)"
    FIELD segmento     AS CHARACTER FORMAT "X(12)"
    FIELD tipoMoneda   AS CHARACTER FORMAT "X(15)"
    FIELD saldo        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD montovencido AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD vigente      AS DECIMAL   FORMAT ">>>,>>>,>>9.99" 
    FIELD porvencer    AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD treinta      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD sesenta      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventa      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventamas   AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD lineacredito AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD diacartera   AS INTEGER
    FIELD promedio     AS DECIMAL
    FIELD plazo        AS INTEGER
    FIELD responsable  AS CHARACTER FORMAT "X(40)"
    FIELD montoPagare  AS DECIMAL
    INDEX idx-clase id ASCENDING numcliente ASCENDING cliente ASCENDING.
DEFINE DATASET dscartera FOR ttCartera.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCartera:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttCartera.
    DEFINE INPUT PARAMETER  pClaseCte AS INT.
    DEFINE INPUT PARAMETER  pTipo     AS INT.
    ASSIGN 
        l-num    = 0
        l-moneda = ""
        .
    IF pClaseCte = ? THEN pClaseCte = 0.
    
    ASSIGN 
        cClientesExcluidos = "1,2,4,5,6,7,8,9,10,11".
        
    EMPTY TEMP-TABLE ttCartera.

    /* Procesa todos los clientes */
    FOR EACH Cliente WHERE (pClaseCte = 0 OR Cliente.Id-ClaseCte = pClaseCte)
        AND LOOKUP(STRING(Cliente.Id-Cliente), cClientesExcluidos) = 0
        NO-LOCK BY Cliente.Id-cliente:

      //  MESSAGE "Processing Cliente ID: " + STRING(Cliente.Id-Cliente) VIEW-AS ALERT-BOX.

        /* Inicializa los totales para cada cliente */
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

        /* Procesa todos los registros de MovCliente para cada Cliente */
        FOR EACH Movcliente WHERE Movcliente.Id-Cliente = Cliente.Id-Cliente 
            AND Movcliente.FecReg <= TODAY                   
            AND MovCliente.Id-MC <= 3                     
            AND MovCliente.Afectado 
            NO-LOCK BY Movcliente.FecReg:

            /* Verifica si el saldo es válido */
            IF MovCliente.Saldo <= 0 THEN NEXT.  
            /*
            IF (pTipo = 1 AND MovCliente.FecVenc < TODAY) OR /* Vigente = solo >= TODAY */
                (pTipo = 2 AND (MovCliente.FecVenc < TODAY OR MovCliente.FecVenc > TODAY + 15)) OR /* Por vencer = hoy a 15 días */
                (pTipo = 3 AND MovCliente.FecVenc > TODAY) THEN    
                NEXT. */ 
        
            ASSIGN
                l-clase    = "Local" 
                l-segmento = ""
                l-resp     = "". 
            
            /* Busca información adicional */
            FIND FIRST ClaseCte WHERE ClaseCte.id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK   NO-ERROR.
            IF AVAILABLE ClaseCte THEN l-clase = ClaseCte.Descr. 
            FIND FIRST SegmentoCte WHERE SegmentoCte.id-SegmentoCte = Cliente.Id-SegmentoCte NO-LOCK NO-ERROR.
            IF AVAILABLE SegmentoCte THEN l-segmento = SegmentoCte.Descr.
            FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
            IF AVAILABLE Resp THEN l-resp = Resp.Nombre.   
        
            FIND FIRST Factura WHERE Factura.Id-Factura = MovCliente.RefSaldo
                AND Factura.Id-Cliente = MovCliente.Id-Cliente
                AND Factura.FecReg     = MovCliente.FecReg
                NO-LOCK NO-ERROR.
            IF AVAILABLE Factura THEN l-tipo-moneda = Factura.Id-Moneda.
         
            FIND FIRST Moneda WHERE moneda.id-moneda = l-tipo-moneda NO-LOCK NO-ERROR.
            IF AVAILABLE moneda THEN l-moneda = moneda.nombre.
            
            
            /* JASS18072025 */ 
            /* Acumulación de saldos */
            l-saldo = l-saldo + (IF MovCliente.Id-Moneda > 1 THEN MovCliente.Saldo * MovCliente.TipoCambio ELSE MovCliente.Saldo).
            
            /* Cálculos de antigüedad */
            IF MovCliente.fecven < TODAY THEN 
            DO:
                ASSIGN
                    l-tot-ven = l-tot-ven + (IF MovCliente.Id-Moneda > 1 THEN MovCliente.Saldo * MovCliente.TipoCambio ELSE MovCliente.Saldo)
                    l-dia     = TODAY - MovCliente.FecVenc.

                IF l-dia <= 30 THEN
                    l-tot-30 = l-tot-30 + (IF MovCliente.Id-Moneda > 1 THEN MovCliente.Saldo * MovCliente.TipoCambio ELSE MovCliente.Saldo).
                ELSE IF l-dia <= 60 THEN
                        l-tot-31 = l-tot-31 + (IF MovCliente.Id-Moneda > 1 THEN MovCliente.Saldo * MovCliente.TipoCambio ELSE MovCliente.Saldo).
                    ELSE IF l-dia <= 90 THEN
                            l-tot-61 = l-tot-61 + (IF MovCliente.Id-Moneda > 1 THEN MovCliente.Saldo * MovCliente.TipoCambio ELSE MovCliente.Saldo).
                        ELSE  
                            l-tot-91 = l-tot-91 + (IF MovCliente.Id-Moneda > 1 THEN MovCliente.Saldo * MovCliente.TipoCambio ELSE MovCliente.Saldo).
            END.
   
            
            /* Cuentas por cobrar futuras (más de 15 días) */
            IF MovCliente.fecven >= TODAY + 16 THEN 
            DO:
                l-tot-vig = l-tot-vig + (IF MovCliente.Id-Moneda > 1 THEN MovCliente.Saldo * MovCliente.TipoCambio ELSE MovCliente.Saldo).
            END.

            /* Cuentas por vencer (entre hoy y 15 días) */
            IF MovCliente.fecven >= TODAY AND MovCliente.fecven <= TODAY + 15 THEN 
            DO:
                l-tot-porv = l-tot-porv + (IF MovCliente.Id-Moneda > 1 THEN MovCliente.Saldo * MovCliente.TipoCambio ELSE MovCliente.Saldo).
            END.
             

        END. /* Fin de FOR EACH MovCliente */

        /* Verifica si el cliente tiene movimientos y saldos válidos para insertar en ttCartera */
        IF l-saldo > 0 THEN   
        DO:    
            /*
            /* Filtro anticipado por pTipo */
            IF (pTipo = 1 AND l-tot-vig <= 0) OR
                (pTipo = 2 AND l-tot-porv <= 0) OR
                (pTipo = 3 AND l-tot-ven <= 0) THEN
                NEXT.   
                */ 
            /* Incrementamos el contador de registros */
            l-num = l-num + 1.  
         //   MESSAGE "Graba Cliente ID: " + STRING(Cliente.Id-Cliente) SKIP l-saldo VIEW-AS ALERT-BOX.

            /* Verifica si el registro de cliente ya existe en la tabla temporal */
            FIND FIRST ttCartera WHERE ttCartera.id = l-num NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttCartera THEN  
                CREATE ttCartera.
            
            /* Asigna los valores calculados a la tabla temporal */
            ASSIGN
                ttCartera.id           = l-num
                ttCartera.clasecliente = l-clase
                ttCartera.numcliente   = Cliente.id-cliente
                ttCartera.cliente      = Cliente.RazonSocial
                ttCartera.segmento     = l-segmento
                ttCartera.tipoMoneda   = l-moneda
                ttCartera.saldo        = l-saldo 
                ttCartera.montovencido = l-tot-ven
                ttCartera.vigente      = l-tot-vig
                ttCartera.porvencer    = l-tot-porv
                ttCartera.treinta      = l-tot-30
                ttCartera.sesenta      = l-tot-31
                ttCartera.noventa      = l-tot-61
                ttCartera.noventamas   = l-tot-91
                ttCartera.lineacredito = Cliente.Limite
                ttCartera.diacartera   = l-dia-max
                ttCartera.promedio     = l-prompago
                ttCartera.plazo        = Cliente.Plazo
                ttCartera.responsable  = l-resp
                ttCartera.montoPagare  = Cliente.MontoPagare .  

            RELEASE ttCartera.

        END. /* Fin de bloque para guardar el cliente con saldos válidos */
        
    END. /* Fin de FOR EACH Cliente */
    
    /*  */
    IF pTipo = 1 THEN DO:
        /* vigente */ 
       FOR EACH ttCartera WHERE ttCartera.vigente <= 0 :
           DELETE ttCartera.
       END.     
    END.
    ELSE IF pTipo = 2 THEN DO:
        /* por vencer */ 
       FOR EACH ttCartera WHERE ttCartera.porvencer <= 0 :
           DELETE ttCartera.
       END.      
    END.
    ELSE IF pTipo = 3 THEN DO :
         /* vencido */ 
       FOR EACH ttCartera WHERE ttCartera.montovencido <= 0 :
           DELETE ttCartera.
       END.     
    END. 
    /* */
    
    FOR EACH ttCartera  
        WHERE (pclasecte <> 0)   /* Caso original: clase diferente de 0 */
        OR (pclasecte = 0 AND pTipo <> 0): 
        DO TRANSACTION:                                                                             
            RUN cxcb0270.p(INPUT ttCartera.numcliente,INPUT TODAY,OUTPUT l-dia-max ,OUTPUT l-prompago).
            IF l-dia-max = ? THEN l-dia-max = 0.
        END.
        ASSIGN  
            ttCartera.diacartera = l-dia-max
            ttCartera.promedio   = INTEGER(ROUND(l-prompago, 0)). /* Redondea a un entero */ 
    END.
RETURN.      
END PROCEDURE. 


