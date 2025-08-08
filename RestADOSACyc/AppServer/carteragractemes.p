@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : carteragractemes.p
    Purpose     : Ya no es por mes es a nivel Clase y trae lo de la grafic
    Syntax      : /CarteraGraCteMes

    Description : 

    Author(s)   : sis10
    Created     : Thu Oct 31 10:48:31 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num       AS INT.
DEFINE VARIABLE l-tot-total AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig   AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-ven   AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv  AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-clase     AS CHARACTER FORMAT "X(20)". 
DEFINE VARIABLE l-segmento  AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE l-estatus   AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-resp      AS CHARACTER FORMAT "X(30)" .
DEFINE VARIABLE l-saldo     AS DEC.
DEFINE VARIABLE l-cte       AS INT.
    
DEFINE TEMP-TABLE ttCarCte NO-UNDO
    FIELD id           AS INTEGER
    FIELD clasecliente AS CHARACTER FORMAT "X(12)"
    FIELD numcliente   AS INTEGER
    FIELD cliente      AS CHARACTER FORMAT "X(40)"
    FIELD saldo        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD montovencido AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD vigente      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD porvencer    AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD responsable  AS CHARACTER FORMAT "X(40)"
    INDEX idx-clase id ASCENDING.
DEFINE DATASET dsttCarCte FOR ttCarCte.

DEFINE TEMP-TABLE ttCartera NO-UNDO
    FIELD Vigente       AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Vencido       AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD PorVencer     AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD TotalClientes AS INTEGER
    FIELD Clase         AS CHARACTER FORMAT "X(12)"
    FIELD Responsable   AS CHARACTER FORMAT "X(40)"
    INDEX idx-clase Responsable ASCENDING.
DEFINE DATASET dscartera FOR ttCartera.   


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCarteraClienteMes:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER TABLE FOR ttCartera. 
DEFINE INPUT PARAMETER iClase AS INT.

ASSIGN 
l-num = 0.

// Se quita rango de fechas en graficas de clientes Responsables
FOR EACH Cliente WHERE Cliente.Id-ClaseCte = iClase NO-LOCK:
    FOR EACH Movcliente WHERE Movcliente.id-cliente = Cliente.id-cliente 
                          AND MovCliente.Id-MC  <= 3                     
                          AND MovCliente.Afectado                       
        NO-LOCK  BREAK  BY Cliente.RazonSocial 
                        BY Cliente.id-cliente
                        BY MovCliente.Id-Cliente:
        IF MovCliente.Saldo <= 0 THEN NEXT.                    
        ASSIGN
            l-clase    = ""
            l-segmento = ""
            l-resp     = "". 
        FIND FIRST ClaseCte WHERE ClaseCte.id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK   NO-ERROR.
        IF AVAILABLE ClaseCte THEN l-clase = ClaseCte.Descr. 

        FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
        IF AVAILABLE Resp THEN l-resp = Resp.Nombre.   
        
        IF FIRST-OF(Cliente.Id-Cliente) THEN  
            ASSIGN
                l-tot-total = 0
                l-tot-vig   = 0
                l-tot-ven   = 0
                l-tot-porv  = 0
                l-saldo     = 0.
        l-saldo = l-saldo + MovCliente.Saldo.
            
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
            MovCliente.fecven <= TODAY + 15  THEN 
        DO:
            ASSIGN 
                l-tot-porv = l-tot-porv +  Movcliente.Saldo.
        END.
            
            
        IF LAST-OF(Cliente.id-cliente) THEN 
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
                ttCarCte.porvencer    = l-tot-porv
                ttCarCte.responsable  = l-resp .  
            RELEASE ttCarCte.
            l-saldo    = 0.
            l-tot-vig  = 0.
            l-tot-ven  = 0.
            l-tot-porv = 0.
        END.
    END.
END.    

EMPTY TEMP-TABLE ttCartera.
FOR EACH ttCarCte  NO-LOCK:

    FIND FIRST ttCartera WHERE ttCartera.Responsable = ttCarCte.responsable NO-LOCK NO-ERROR.

    IF NOT AVAILABLE ttCartera THEN 
    DO: 
        l-cte = l-cte + 1.
        CREATE ttCartera.
        ASSIGN
            ttCartera.Responsable   = ttCarCte.responsable
            ttCartera.Clase         = ttCarCte.clasecliente
            ttCartera.TotalClientes = l-cte
            ttCartera.Vencido       = ttCarCte.montovencido
            ttCartera.Vigente       = ttCarCte.vigente
            ttCartera.PorVencer     = ttCarCte.porvencer
            ttCartera.Clase         = ttCarCte.clasecliente.
        RELEASE ttCartera.  
    END.
    ELSE 
    DO:
        ASSIGN
            ttCartera.TotalClientes = ttCartera.TotalClientes + 1  
            ttCartera.Vencido       = ttCartera.Vencido + ttCarCte.montovencido
            ttCartera.Vigente       = ttCartera.Vigente + ttCarCte.vigente
            ttCartera.PorVencer     = ttCartera.PorVencer + ttCarCte.porvencer.
        RELEASE ttCartera.  
    END.
END.
END PROCEDURE.

