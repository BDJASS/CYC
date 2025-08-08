@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Primer Grafica //CarteraGraCliente
    Purpose     : 

    Syntax      : Se modifica este programa para que sea utilizado.
                  Cards Director/Responsable
    Description : Graficas de Director / Grafica de Responsable

    Author(s)   : sis10
    Created     : Tue Oct 22 07:48:55 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num           AS INT.
DEFINE VARIABLE l-tot-total     AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-ven       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-cartera   AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-micartera AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-clase         AS CHARACTER FORMAT "X(20)". 
DEFINE VARIABLE l-segmento      AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE l-estatus       AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-resp          AS CHARACTER FORMAT "X(30)" .
DEFINE VARIABLE l-saldo         AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-cte           AS INT.
DEFINE VAR      l-total         LIKE l-saldo.
DEF BUFFER bf-cli FOR Cliente .  
DEF BUFFER bf-mov FOR MovCliente .

DEFINE TEMP-TABLE ttCarCte NO-UNDO    
    FIELD id           AS INTEGER    
    FIELD clasecliente AS CHARACTER FORMAT "X(12)"
    FIELD saldo        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD montovencido AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD vigente      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD porvencer    AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    INDEX idx-clase id ASCENDING.
DEFINE DATASET dsttCarCte FOR ttCarCte.

DEFINE TEMP-TABLE ttCartera NO-UNDO
    FIELD Vigente               AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Vencido               AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD PorVencer             AS DECIMAL   FORMAT ">>>,>>>,>>9.99" 
    FIELD Clase                 AS CHARACTER FORMAT "X(12)"
    FIELD SolicitudesNuevas     AS INT
    FIELD SolicitudesValidacion AS INT 
    FIELD MiCartera             AS DECIMAL   FORMAT ">>>,>>>,>>9.99" 
    FIELD TotalCartera          AS DECIMAL   FORMAT ">>>,>>>,>>9.99" 
    INDEX idx-clase Clase ASCENDING.

DEF    BUFFER b-mov        FOR MovCliente.
DEF    BUFFER b-movcli1056 FOR MovCliente.

DEFINE BUFFER bfSolCred    FOR SolCred. 
DEFINE VARIABLE iCountEstatus1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountEstatus2 AS INTEGER NO-UNDO.
DEFINE VARIABLE cClientesExcluidos AS CHARACTER NO-UNDO.
    
/* **********************  Internal Procedures  *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCarteraResp:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iClase  AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCartera.
    EMPTY TEMP-TABLE ttCartera.
    
    ASSIGN cClientesExcluidos = "1,2,4,5,6,7,8,9,10,11".
    
    IF iClase = ? THEN iClase = 0. 
    FOR EACH Cliente WHERE (iClase = 0 OR Cliente.Id-ClaseCte = iClase)
                       AND LOOKUP(STRING(Cliente.Id-Cliente), cClientesExcluidos) = 0 NO-LOCK:
    
        ASSIGN
            l-tot-total     = 0
            l-tot-vig       = 0
            l-tot-porv      = 0
            l-tot-ven       = 0 
            l-saldo         = 0
            l-clase         = ""
            l-segmento      = ""
            l-resp          = ""
            l-tot-micartera = 0
            l-tot-cartera   = 0.
         
        IF iClase <> 0 THEN 
        DO:        
            FIND FIRST ClaseCte WHERE ClaseCte.id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK   NO-ERROR.
            IF AVAILABLE ClaseCte THEN l-clase = ClaseCte.Descr. 
        END.
        ELSE 
        DO:
            ASSIGN 
                l-clase = "TODOS".      
        END.  
        FOR EACH Movcliente WHERE Movcliente.id-cliente = Cliente.id-cliente AND
            Movcliente.FecReg <= TODAY                 AND
            MovCliente.Id-MC  <= 3                     AND
            MovCliente.Afectado                       
            NO-LOCK  BREAK  BY Cliente.RazonSocial 
            BY Cliente.id-cliente
            BY MovCliente.Id-Cliente:
            
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
            
            IF l-saldo > 0 THEN 
            DO:
                  
                IF MovCliente.fecven <  TODAY THEN 
                DO:
                    ASSIGN
                        l-tot-ven = l-tot-ven + l-saldo.  /* Sumar al total vencido */
                END. 
                IF MovCliente.fecven >= TODAY + 16 THEN 
                DO:
                    ASSIGN 
                        l-tot-vig = l-tot-vig + l-saldo.  /* Sumar al total vigente */
                END.
        
                IF MovCliente.fecven >= TODAY AND
                    MovCliente.fecven <= TODAY + 15  THEN 
                DO:
                    ASSIGN 
                        l-tot-porv = l-tot-porv + l-saldo.  /* Sumar al total por vencer */
                END.                      
                            
            END. // l-saldo
            
        END. // movcliente
        FIND FIRST ttCartera WHERE ttCartera.Clase = l-clase NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCartera THEN 
        DO:
            CREATE ttCartera.
            ASSIGN
                ttCartera.Clase     = l-clase
                ttCartera.Vencido   = l-tot-ven
                ttCartera.Vigente   = l-tot-vig
                ttCartera.PorVencer = l-tot-porv
                ttCartera.MiCartera = l-tot-vig + l-tot-porv + l-tot-ven.
        END.
        ELSE 
        DO:
            ASSIGN
                ttCartera.Vencido   = ttCartera.Vencido   + l-tot-ven
                ttCartera.Vigente   = ttCartera.Vigente   + l-tot-vig
                ttCartera.PorVencer = ttCartera.PorVencer + l-tot-porv
                ttCartera.MiCartera = ttCartera.MiCartera + l-tot-vig + l-tot-porv + l-tot-ven.  
        END.
    END.
    /* Realizar conteo directo en la base de datos */
    FOR EACH bfSolCred WHERE bfSolCred.IdEstatus = 1 NO-LOCK:
        ASSIGN 
            iCountEstatus1 = iCountEstatus1 + 1.
    END.

    FOR EACH bfSolCred WHERE bfSolCred.IdEstatus = 2 NO-LOCK:
        ASSIGN 
            iCountEstatus2 = iCountEstatus2 + 1.
    END.

    IF iClase <> 0 THEN 
    DO:
        FOR EACH Cliente NO-LOCK:
            ASSIGN
                l-saldo = 0.
          
            FOR EACH Movcliente WHERE Movcliente.id-cliente = Cliente.id-cliente 
                AND Movcliente.FecReg <= TODAY                 
                AND MovCliente.Id-MC  <= 3                     
                AND MovCliente.Afectado                       
                NO-LOCK  BREAK  BY Cliente.RazonSocial 
                BY Cliente.id-cliente
                BY MovCliente.Id-Cliente:
            
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
            
                IF l-saldo > 0 THEN 
                DO:
                    ASSIGN 
                        l-total = l-total + l-saldo.              
                END. 
            
            END.
        END.
    END.    
  
    FOR EACH ttCartera  :
        ASSIGN 
            ttCartera.SolicitudesNuevas     = iCountEstatus1
            ttCartera.SolicitudesValidacion = iCountEstatus2
            ttCartera.TotalCartera          = l-total.   
    END.        
      
END PROCEDURE. 

