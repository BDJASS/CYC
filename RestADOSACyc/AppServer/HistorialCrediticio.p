@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : HistorialCrediticio.p
    Purpose     : /HistorialCrediticio

    Syntax      : Get- Se utiliza en Gestion de Clientes

    Description : 

    Author(s)   : sis10
    Created     : Tue Oct 22 14:47:26 CST 2024
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
DEFINE VARIABLE l-estatus   AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-prompago  AS DECIMAL.
DEFINE VARIABLE l-tot-30    AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-31    AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-61    AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-91    AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-dia       AS INT.
DEFINE VARIABLE l-dia-max   AS INT       FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-saldo     AS DEC       FORMAT ">>>,>>>,>>9.99".


    
DEFINE TEMP-TABLE HistorialCrediticio NO-UNDO
    FIELD id                   AS INTEGER
    FIELD numcliente           AS INTEGER
    FIELD RazonSocial          AS CHARACTER FORMAT "X(40)"
    FIELD LineaDeCredito       AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD PlazoInicial         AS INTEGER
    FIELD LimiteDeCreditoUsado AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD SaldoActual          AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD SaldoDisponible      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Vigente              AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD PorVencer            AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Vencido              AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD DiasCartera          AS INTEGER
    FIELD PromedioDePago       AS DECIMAL
    FIELD treinta              AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD sesenta              AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventa              AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventamas           AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    INDEX idx-clase id ASCENDING numcliente ASCENDING RazonSocial ASCENDING.
DEFINE DATASET dsHistorialCrediticio FOR HistorialCrediticio.


/* **********************  Internal Procedures  *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetHistorialCrediticio:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR HistorialCrediticio.
    DEFINE INPUT PARAMETER  pCliente AS INT.

    EMPTY TEMP-TABLE HistorialCrediticio.
    FOR EACH Cliente WHERE Cliente.Id-Cliente = pCliente NO-LOCK:
        
        l-num = l-num + 1.  
                  
        FIND FIRST HistorialCrediticio WHERE HistorialCrediticio.id = l-num NO-LOCK NO-ERROR.
        IF NOT AVAILABLE HistorialCrediticio THEN  
            CREATE HistorialCrediticio.
        ASSIGN
            HistorialCrediticio.id              = l-num
            HistorialCrediticio.numcliente      = Cliente.id-cliente
            HistorialCrediticio.RazonSocial     = Cliente.RazonSocial
            HistorialCrediticio.LineaDeCredito  = Cliente.Limite
            HistorialCrediticio.PlazoInicial    = Cliente.Plazo 
            HistorialCrediticio.SaldoActual     = 0
            HistorialCrediticio.SaldoDisponible = Cliente.Limite .  
        RELEASE HistorialCrediticio.
        
        FOR EACH Movcliente WHERE Movcliente.Id-Cliente = Cliente.Id-Cliente 
            AND Movcliente.FecReg <= TODAY                 
            AND MovCliente.Id-MC  <= 3                     
            AND MovCliente.Afectado                       
            NO-LOCK  BREAK  BY Cliente.Id-Cliente
            BY Cliente.RazonSocial 
            BY MovCliente.Id-Cliente:
            IF MovCliente.Saldo <= 0 THEN NEXT.                    
 
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
            
            IF MovCliente.fecven <  TODAY THEN 
            DO:
                ASSIGN
                    l-tot-ven = l-tot-ven  +  Movcliente.Saldo
                    l-dia =  TODAY - MovCliente.FecReg .    
                IF l-dia <= 30 THEN
                    ASSIGN l-tot-30 = l-tot-30 + Movcliente.Saldo. /* 1-30 */
                ELSE IF l-dia <= 60 THEN
                        ASSIGN l-tot-31 = l-tot-31 + Movcliente.Saldo. /* 31-60 */
                    ELSE IF l-dia <= 90 THEN
                            ASSIGN l-tot-61 = l-tot-61 + Movcliente.Saldo. /* 61-90 */
                        ELSE  
                            ASSIGN l-tot-91 = l-tot-91 + Movcliente.Saldo. /* 91.. + */
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

                     
            IF LAST-OF(Cliente.id-cliente) THEN   
            DO:    
                FIND FIRST HistorialCrediticio WHERE HistorialCrediticio.id = l-num NO-LOCK NO-ERROR.
                IF  AVAILABLE HistorialCrediticio THEN  
                    ASSIGN
                        HistorialCrediticio.SaldoActual = l-saldo 
                        HistorialCrediticio.Vencido     = l-tot-ven
                        HistorialCrediticio.PorVencer   = l-tot-porv
                        HistorialCrediticio.Vigente     = l-tot-vig
                        HistorialCrediticio.treinta     = l-tot-30
                        HistorialCrediticio.sesenta     = l-tot-31
                        HistorialCrediticio.noventa     = l-tot-61 
                        HistorialCrediticio.noventamas  = l-tot-91.  
                    
                /* Cálculo del porcentaje del límite de crédito usado */
                IF HistorialCrediticio.LineaDeCredito > 0 THEN
                    ASSIGN
                        HistorialCrediticio.LimiteDeCreditoUsado = (HistorialCrediticio.SaldoActual / HistorialCrediticio.LineaDeCredito) * 100.

                /* Opcional: Cálculo del saldo disponible */
                ASSIGN
                    HistorialCrediticio.SaldoDisponible = HistorialCrediticio.LineaDeCredito - HistorialCrediticio.SaldoActual.     
                    
                    
                RELEASE HistorialCrediticio.  
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
    FOR EACH HistorialCrediticio NO-LOCK:
        DO TRANSACTION:                                                                             
            RUN cxcb0270.p(INPUT HistorialCrediticio.numcliente,INPUT TODAY,OUTPUT l-dia-max ,OUTPUT l-prompago).
            IF l-dia-max = ? THEN l-dia-max = 0.
        END.
        ASSIGN  
            HistorialCrediticio.DiasCartera    = l-dia-max
            HistorialCrediticio.PromedioDePago = l-prompago.
    END. 
RETURN.       
END PROCEDURE.


