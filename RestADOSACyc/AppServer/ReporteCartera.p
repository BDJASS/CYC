@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : ReporteCartera.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Fri May 09 09:52:50 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/*
  Ticket 994 Agregar busqueda por cobrador y Columna 
             JASS20062025
 
 Ticket 1151
 El campo Moneda sea ahora el Simbolo,(antes nombre)
 Cobrador y Responsables que se envien las Iniciales
 ( antes el Id o el Nombre) 
 JASS04072025

TICKET 1200 Parametro Dias para que vean esos de los vencidos
             la columna de fechas vencidas.
             JASS11072025
-----------------------------------------------------------------

Ticket 1215 Ajustes en Reporte Cartera   
            Separar moneda de las facturas  
            Iniciales del Vendedor  

Ticket 1287 Agregar parametro Responsable
            JASS21072025             
*/
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE l-num         AS INTEGER.  
DEFINE VARIABLE l-tot-total   AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig     AS DECIMAL   FORMAT ">>>,>>>,>>9.99". 
DEFINE VARIABLE l-tot-ven     AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv    AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-clase       AS CHARACTER FORMAT "X(20)". 
DEFINE VARIABLE l-segmento    AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE l-estatus     AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-resp        AS CHARACTER FORMAT "X(30)" .
DEFINE VARIABLE l-moneda      AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-tipo-moneda AS INTEGER.
DEFINE VARIABLE l-prompago    AS DECIMAL.
DEFINE VARIABLE l-tot-30      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-31      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-61      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-91      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-dia         AS INTEGER.
DEFINE VARIABLE l-dia-max     AS INTEGER   FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-saldo       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".

DEFINE VARIABLE vIdCobrador   AS INTEGER   NO-UNDO.
DEFINE VARIABLE vNomCobrador  AS CHARACTER NO-UNDO.

    
DEFINE TEMP-TABLE ttCartera
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
    FIELD Fecha        AS DATE
    FIELD IdCobrador   LIKE Cobrador.Id-Cobrador /* JASS20062025 */
    FIELD Cobrador     AS CHAR
    INDEX idx-cliente cliente ASCENDING numcliente ASCENDING.
    //INDEX idx-clase id ASCENDING numcliente ASCENDING cliente ASCENDING.
    
DEFINE VARIABLE l-supera-dias      AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cClientesExcluidos AS CHARACTER NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetReporteCartera:
    /*------------------------------------------------------------------------------
     Purpose:  pTipo = 3 Cartera Clientes Antiguedad Saldos
               ptipo = 2 Cartera Clientes Pronostico
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pClaseCte AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER pMoneda   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER pZona     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pSegmento AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pCalidad  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER pFecha    AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER pCobrador AS INTEGER   NO-UNDO. 
    DEFINE INPUT  PARAMETER pDias     AS INTEGER   NO-UNDO.  
    DEFINE INPUT  PARAMETER pResp LIKE Cliente.Id-Resp  NO-UNDO.   /* JASS21072025 */ 
    DEFINE OUTPUT PARAMETER TABLE FOR ttCartera.   
    
    DEFINE VARIABLE cFechaISO AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dFecha    AS DATE      NO-UNDO.
    
    /* Extraer solo la parte de fecha (primeros 10 caracteres) */
    cFechaISO = SUBSTRING(pFecha, 1, 10).  /* Resultado: "2025-01-15" */

    /* Reorganizar a un formato que DATE() entienda, por ejemplo "01/15/2025" */
    cFechaISO = SUBSTRING(cFechaISO, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISO, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISO, 1, 4).         /* YYYY */

    /* Convertir a tipo DATE */     
    dFecha = DATE(cFechaISO).
    
    ASSIGN 
        l-num    = 0
        l-moneda = "".
    
    ASSIGN 
        cClientesExcluidos = "1,2,4,5,6,7,8,9,10,11".
       
    IF pTipo     = ?   THEN pTipo     = 0   .
    IF pSegmento = ?   THEN pSegmento = "0"  .
    IF pCobrador = ?   THEN pCobrador = 0   .  
    IF pDias     = ?   THEN pDias     = 0   .  /* JASS11072025 */ 
    IF pZona     = ?   THEN pZona     = ""  . // JASS06062025
    IF pClaseCte = ?   THEN pClaseCte = 0   .
    IF pResp     = ?   THEN pResp     = 0   . /* JASS21072025 */
    
    IF ptipo = 2  THEN  pDias = 0. /* EN PRONOSTICO NO SE UTILIZA FILTRO DIAS */  
    EMPTY TEMP-TABLE ttCartera.
    /* Procesa todos los clientes */
    FOR EACH Cliente WHERE (pClaseCte = 0 OR Cliente.Id-ClaseCte = pClaseCte)
        AND (pZona = "" OR pZona = "0" OR CAN-DO(pZona, STRING(Cliente.Id-Zona)))
        AND (pSegmento = "0" OR STRING(Cliente.Id-Segmentocte) = pSegmento)
        AND (pCalidad = 0 OR Cliente.Id-Calidad = pCalidad)
        AND (pCobrador = 0 OR Cliente.Id-Cobrador = pCobrador) 
        AND  (pResp = 0 OR Cliente.Id-Resp = pResp)  
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
        FOR EACH Moneda WHERE(pMoneda = 0 OR pMoneda = ? OR Moneda.Id-Moneda = pMoneda) NO-LOCK :
            /* Procesa todos los registros de MovCliente para cada Cliente */
            FOR EACH Movcliente WHERE Movcliente.Id-Cliente = Cliente.Id-Cliente 
                AND Movcliente.FecReg <= TODAY                   
                AND MovCliente.Id-MC <= 3                     
                AND MovCliente.Afectado 
                AND Movcliente.Id-Moneda = Moneda.Id-Moneda  
                NO-LOCK BY Movcliente.FecReg:

                /* Verifica si el saldo es válido */
                IF MovCliente.Saldo <= 0 THEN NEXT.  
    
                ASSIGN
                    l-clase    = "Local" 
                    l-segmento = ""
                    l-resp     = "". 
            
                /* Busca información adicional */
                FIND FIRST ClaseCte WHERE ClaseCte.id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK   NO-ERROR.
                IF AVAILABLE ClaseCte THEN l-clase = ClaseCte.Descr. 
                FIND FIRST SegmentoCte WHERE SegmentoCte.id-SegmentoCte = Cliente.Id-SegmentoCte NO-LOCK NO-ERROR.
                IF AVAILABLE SegmentoCte THEN l-segmento = SegmentoCte.Descr.
 
        
                FIND FIRST Factura WHERE Factura.Id-Factura = MovCliente.RefSaldo
                    AND Factura.Id-Cliente = MovCliente.Id-Cliente
                    AND Factura.FecReg     = MovCliente.FecReg
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Factura THEN l-tipo-moneda = Factura.Id-Moneda.
           
            
                /* JASS18072025 */ 
                /* Acumulación de saldos */
                l-saldo = l-saldo + MovCliente.Saldo.
               
                /* Cálculos de antigüedad */
                IF MovCliente.fecven < TODAY THEN 
                DO:
                    ASSIGN
                        l-tot-ven = l-tot-ven + MovCliente.Saldo
                        l-dia     = TODAY - MovCliente.FecVenc.

                    /* Aplica filtro si pDias > 0 */  
                    IF pDias = 0 OR l-dia > pDias THEN 
                    DO:
                        /* JASS11072025  */
                        l-supera-dias = TRUE. /* Marca que hay al menos un movimiento vÃ¡lido */
                        
                        /* Solo calcular desglose si pTipo = ANTIGUEDAD SALDOS */
                        IF pTipo = 3 THEN 
                        DO:
                            IF l-dia <= 30 THEN
                                ASSIGN l-tot-30 = l-tot-30 + Movcliente.Saldo. /* 1-30 */
                            ELSE IF l-dia <= 60 THEN
                                    ASSIGN l-tot-31 = l-tot-31 + Movcliente.Saldo. /* 31-60 */
                                ELSE IF l-dia <= 90 THEN
                                        ASSIGN l-tot-61 = l-tot-61 + Movcliente.Saldo. /* 61-90 */
                                    ELSE  
                                        ASSIGN l-tot-91 = l-tot-91 + Movcliente.Saldo. /* 91.. + */  
                        END.   

                    END.  
                  END.
            
                    /* Cuentas por cobrar futuras (más de 15 días) */
                    IF MovCliente.fecven >= TODAY + 16 THEN 
                    DO:
                        l-tot-vig = l-tot-vig + MovCliente.Saldo.
                        l-dia      = TODAY - MovCliente.FecVenc.
                        IF pTipo = 2 THEN 
                        DO:
                            l-dia      = TODAY - MovCliente.FecReg.
                            IF l-dia <= 30 THEN
                                ASSIGN l-tot-30 = l-tot-30 + Movcliente.Saldo. /* 1-30 */
                            ELSE IF l-dia <= 60 THEN
                                    ASSIGN l-tot-31 = l-tot-31 + Movcliente.Saldo. /* 31-60 */
                                ELSE IF l-dia <= 90 THEN
                                        ASSIGN l-tot-61 = l-tot-61 + Movcliente.Saldo. /* 61-90 */
                                    ELSE  
                                        ASSIGN l-tot-91 = l-tot-91 + Movcliente.Saldo. /* 91.. + */  
                        END.   
                        
                    END.

                    /* Cuentas por vencer (entre hoy y 15 días) */
                    IF MovCliente.fecven >= TODAY AND MovCliente.fecven <= TODAY + 15 THEN 
                    DO:
                        l-tot-porv = l-tot-porv + MovCliente.Saldo.
                    END.
             

                END. /* Fin de FOR EACH MovCliente */
            
        
            IF pDias > 0 AND NOT l-supera-dias THEN 
            DO:
                    
                ASSIGN
                    l-saldo       = 0
                    l-tot-vig     = 0
                    l-tot-ven     = 0
                    l-tot-porv    = 0
                    l-tot-30      = 0
                    l-tot-31      = 0
                    l-tot-61      = 0
                    l-tot-91      = 0
                    l-supera-dias = FALSE
                    l-resp        = "".
                NEXT.  
            END.
            
            IF l-saldo > 0 THEN   
            DO:               
              
            
                ASSIGN
                    l-resp = "".
                FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
                IF AVAILABLE Resp THEN 
                DO:
                    DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
                    DEFINE VARIABLE palabras AS INTEGER   NO-UNDO.
                    DEFINE VARIABLE l-resp   AS CHARACTER NO-UNDO.
                    DEFINE VARIABLE entrada  AS CHARACTER NO-UNDO.

                    palabras = NUM-ENTRIES(TRIM(Resp.Nombre), " ").

                    IF palabras = 1 THEN
                        l-resp = TRIM(Resp.Nombre).
                    ELSE 
                    DO:
                        DO i = 1 TO palabras:
                            entrada = ENTRY(i, TRIM(Resp.Nombre), " ").
                            IF entrada <> "" THEN
                                l-resp = l-resp + SUBSTRING(entrada, 1, 1).
                        END.
                    END.
                END.  
            
                
       
                
                /* Inicializa valores por defecto */
                ASSIGN
                    vIdCobrador  = 0
                    vNomCobrador = "".

                /* Busca al cobrador si existe */
                /* Busca al cobrador si existe */
                DEFINE VARIABLE iniciales AS CHARACTER NO-UNDO. /* <-- AQUI LA DEFINES */
                
                /* Busca al cobrador si existe */
                FIND FIRST Cobrador WHERE Cobrador.Id-Cobrador = Cliente.Id-Cobrador NO-LOCK NO-ERROR.
                IF AVAILABLE Cobrador THEN 
                DO:
                    ASSIGN 
                        vIdCobrador = Cobrador.Id-Cobrador.

                    IF TRIM(Cobrador.Iniciales) <> "" THEN
                        vNomCobrador = Cobrador.Iniciales.
                    ELSE IF TRIM(Cobrador.Nombre) <> "" THEN 
                        DO:
                            palabras = NUM-ENTRIES(Cobrador.Nombre, " ").
                            IF palabras = 1 THEN
                                vNomCobrador = TRIM(Cobrador.Nombre).
                            ELSE 
                            DO:
                                ASSIGN 
                                    iniciales = "".
                                DO i = 1 TO palabras:
                                    iniciales = iniciales + SUBSTRING(ENTRY(i, Cobrador.Nombre, " "), 1, 1).
                                END.
                                vNomCobrador = iniciales.
                            END.
                        END.
                        ELSE
                            vNomCobrador = STRING(Cobrador.Id-Cobrador).
                END.  

                /* JASS20062025 */ 
                l-num = l-num + 1.           
                FIND FIRST ttCartera WHERE ttCartera.id = l-num NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttCartera THEN  
                    CREATE ttCartera.
                ASSIGN
                    ttCartera.id           = l-num
                    ttCartera.clasecliente = l-clase
                    ttCartera.numcliente   = Cliente.id-cliente
                    ttCartera.cliente      = Cliente.RazonSocial
                    ttCartera.segmento     = l-segmento
                    ttCartera.tipoMoneda   = Moneda.Simbolo
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
                    ttCartera.Fecha        = today
                    ttCartera.IdCobrador   = vIdCobrador 
                    ttCartera.Cobrador     = vNomCobrador.      
                RELEASE ttCartera.
                ASSIGN
                    l-saldo       = 0
                    l-tot-vig     = 0
                    l-tot-ven     = 0
                    l-tot-porv    = 0
                    l-tot-30      = 0
                    l-tot-31      = 0
                    l-tot-61      = 0
                    l-tot-91      = 0
                    l-resp        = ""
                    l-supera-dias = FALSE .  
            END.
            END. /* FOR EACH MONEDA */
        END. 


        /* ******* */
        /*
        IF pTipo = 1 THEN 
        DO:
            /* vigente */ 
            FOR EACH ttCartera WHERE ttCartera.vigente <= 0 :
                DELETE ttCartera.
            END.     
        END.
        ELSE IF pTipo = 2 THEN 
            DO:
                /* por vencer */ 
                FOR EACH ttCartera WHERE ttCartera.porvencer <= 0 :
                    DELETE ttCartera.
                END.      
            END.
            ELSE IF pTipo = 3 THEN 
                DO :
                    /* vencido */ 
                    FOR EACH ttCartera WHERE ttCartera.montovencido <= 0 :
                        DELETE ttCartera.
                    END.     
                END.
        /* */ 
            
        */
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
