@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").


/*------------------------------------------------------------------------
    File        : carteraresp.p
    Purpose     : 

    Syntax      : /CarteraGraResp

    Description : 

    Author(s)   : sis10
    Created     : Tue Oct 22 07:48:55 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num       AS INT.
DEFINE VARIABLE l-tot-total AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-ven   AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig   AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv  AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-clase     AS CHARACTER FORMAT "X(20)". 
DEFINE VARIABLE l-segmento  AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE l-estatus   AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-resp      AS CHARACTER FORMAT "X(30)" .
DEFINE VARIABLE l-saldo     AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-cte       AS INT.
DEFINE VARIABLE l-cte2      AS INT.
DEFINE VARIABLE l-cte3      AS INT.


DEFINE TEMP-TABLE ttCarCte NO-UNDO
    FIELD id           AS INTEGER
    FIELD clasecliente AS CHARACTER FORMAT "X(12)"
    FIELD numcliente   AS INTEGER
    FIELD cliente      AS CHARACTER FORMAT "X(40)"
    FIELD saldo        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD montovencido AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD vigente      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD responsable  AS CHARACTER FORMAT "X(40)"
    INDEX idx-clase id ASCENDING.
DEFINE DATASET dsttCarCte FOR ttCarCte.

DEFINE TEMP-TABLE ttCartera NO-UNDO
    FIELD Responsable   AS CHARACTER FORMAT "X(40)"
    FIELD TotalClientes AS INTEGER
    FIELD Vigente       AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Vencido       AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Clase         AS CHARACTER FORMAT "X(12)"
    FIELD TotVigente    AS INT
    FIELD TotVencido    AS INT
    INDEX idx-clase Responsable ASCENDING.
DEFINE DATASET dscartera FOR ttCartera.   

DEF BUFFER b-mov        FOR MovCliente.
DEF BUFFER b-movcli1056 FOR MovCliente.
DEFINE VARIABLE cClientesExcluidos AS CHARACTER NO-UNDO.

/* **********************  Internal Procedures  *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCarteraResp:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------ */
    DEFINE OUTPUT PARAMETER TABLE FOR ttCartera.
    EMPTY TEMP-TABLE ttCartera.
    
   ASSIGN cClientesExcluidos = "1,2,4,5,6,7,8,9,10,11".
    
    FOR EACH Cliente WHERE LOOKUP(STRING(Cliente.Id-Cliente), cClientesExcluidos) = 0 
                     NO-LOCK:
    
        ASSIGN
            l-tot-total = 0
            l-tot-vig   = 0
            l-tot-ven   = 0 
            l-saldo     = 0.
            
               
        FOR EACH Movcliente WHERE Movcliente.id-cliente = Cliente.id-cliente AND
            Movcliente.FecReg <= TODAY                 AND
            MovCliente.Id-MC  <= 3                     AND
            MovCliente.Afectado                       
            NO-LOCK  BREAK  BY Cliente.RazonSocial 
            BY Cliente.id-cliente
            BY MovCliente.Id-Cliente:
            IF MovCliente.Saldo <= 0 THEN NEXT.                    
            ASSIGN
                l-clase    = ""
                l-segmento = ""
                l-resp     = "".
            
            
                
            IF MovCliente.Id-MC <= 3 THEN 
            DO:
                FOR EACH b-mov WHERE b-mov.RefSaldo = MovCliente.RefSaldo
                    AND b-mov.Id-MC    > 3
                    AND b-mov.Afectado
                    AND b-mov.FecReg  <= TODAY  NO-LOCK:
                    FIND Acuse WHERE Acuse.Id-Acuse = b-mov.Documento NO-LOCK NO-ERROR.
                    IF AVAILABLE Acuse THEN
                        IF  Acuse.Estatus <> 4 THEN NEXT.
                    ACCUMULATE b-mov.importe (TOTAL).
                END.
                ASSIGN
                    l-saldo = Movcliente.Importe + (ACCUM TOTAL b-mov.Importe).
                IF Movcliente.Id-Moneda > 1 THEN
                    ASSIGN l-saldo = l-saldo * MovCliente.TipoCambio.
      
            END. 
   
            FIND FIRST ClaseCte WHERE ClaseCte.id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK   NO-ERROR.
        IF AVAILABLE ClaseCte THEN l-clase = ClaseCte.Descr. 

        FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
        IF AVAILABLE Resp THEN l-resp = Resp.Nombre.   
            IF l-saldo > 0 THEN 
            DO:
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
            END.  // L-SALDO 
        END.   //MOVC         
      
    IF l-saldo > 0 THEN    
    DO:        
        l-num = l-num + 1.  
        FIND FIRST ttCarCte WHERE ttCarCte.id = l-num NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCarCte THEN  
            CREATE ttCarCte.
        ASSIGN
            ttCarCte.id           = l-num
            ttCarCte.clasecliente = l-clase
            ttCarCte.numcliente   = Cliente.id-cliente
            ttCarCte.cliente      = Cliente.RazonSocial
            ttCarCte.saldo        = l-saldo 
            ttCarCte.montovencido = l-tot-ven
            ttCarCte.vigente      = l-tot-vig
            ttCarCte.responsable  = l-resp .   
        RELEASE ttCarCte.
    END.
    END. // CLIENTE 
    EMPTY TEMP-TABLE ttCartera.
    FOR EACH ttCarCte NO-LOCK:
        FIND FIRST ttCartera WHERE ttCartera.responsable = ttCarCte.responsable NO-ERROR.
    
        IF NOT AVAILABLE ttCartera THEN 
        DO:
            CREATE ttCartera.
            ASSIGN
                ttCartera.Responsable   = ttCarCte.responsable
                ttCartera.Clase         = ttCarCte.clasecliente
                ttCartera.TotalClientes = 1
                ttCartera.Vencido       = ttCarCte.montovencido
                ttCartera.Vigente       = ttCarCte.vigente.
            
        // Inicializar contadores de registros
            ttCartera.TotVencido = 0.
            ttCartera.TotVigente = 0.
        
        // Contar el primer registro
            IF ttCarCte.montovencido > 0 THEN 
                ttCartera.TotVencido = ttCartera.TotVencido + 1.
            IF ttCarCte.vigente > 0 THEN 
                ttCartera.TotVigente = ttCartera.TotVigente + 1.
        END.
        ELSE 
        DO:
        // Actualizar montos y contadores
            ASSIGN
                ttCartera.TotalClientes = ttCartera.TotalClientes + 1
                ttCartera.Vencido       = ttCartera.Vencido + ttCarCte.montovencido
                ttCartera.Vigente       = ttCartera.Vigente + ttCarCte.vigente.
            
        // Actualizar contadores de registros
            IF ttCarCte.montovencido > 0 THEN 
                ttCartera.TotVencido = ttCartera.TotVencido + 1.
            IF ttCarCte.vigente > 0 THEN 
                ttCartera.TotVigente = ttCartera.TotVigente + 1.
        END.
    END. 
    RETURN.  
END PROCEDURE.

