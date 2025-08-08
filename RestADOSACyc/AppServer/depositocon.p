@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : 
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Mon Oct 21 15:03:12 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE v-resp            AS CHARACTER.
DEFINE VARIABLE v-clase           AS CHARACTER.
DEFINE VARIABLE v-mes             AS INTEGER.
DEFINE VARIABLE v-dia             AS INTEGER.
DEFINE VARIABLE v-pendiente       AS INTEGER.
DEFINE VARIABLE v-num             AS INTEGER.
DEFINE VARIABLE v-dia-total       AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE v-mes-total       AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE v-pendiente-total AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-lista           AS CHARACTER.



DEFINE TEMP-TABLE ttDeposito NO-UNDO
    FIELD Responsable     AS CHARACTER FORMAT "X(30)"
    FIELD Clase           AS CHARACTER FORMAT "X(30)"
    FIELD DepHoy          AS INTEGER
    FIELD DepHoyTotal     AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD DepMes          AS INTEGER
    FIELD DepMesTotal     AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD DepPorAplicar      AS INTEGER
    FIELD DepPorAplicarTotal AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    INDEX idx-respo Responsable ASCENDING.
    
    DEFINE DATASET dsDeposito FOR ttDeposito.

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDeposito:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER TABLE FOR ttDeposito.
EMPTY TEMP-TABLE ttDeposito.


ASSIGN 
    /* Excluir ctes del 1-11 excepto al 3 en depositos credido-contado */
    l-lista = "1,2,4,5,6,7,8,9,10,11" .
FOR EACH DepBanco WHERE MONTH (DepBanco.FecDep) = MONTH(TODAY)  
                    AND YEAR(DepBanco.FecDep)   = YEAR (TODAY) NO-LOCK :                   
    FOR EACH Cliente WHERE Cliente.Id-Cliente = DepBanco.Id-Cliente 
        NO-LOCK BREAK BY DepBanco.Id-Banco 
                      BY DepBanco.Id-Cliente 
                      BY Cliente.Id-Cliente :  
        IF LOOKUP(STRING (Cliente.Id-Cliente), SUBSTITUTE(l-lista,",",""))> 0 THEN NEXT.    
        IF DepBanco.Tipocte = 4 THEN NEXT. /* cte diferente a contado */     
        FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ClaseCte THEN NEXT.    
        FIND FIRST resp WHERE resp.Id-resp = Cliente.Id-resp NO-LOCK NO-ERROR.
        
        v-num = v-num + 1.
               
        ASSIGN        
        v-dia       = 0        
        v-pendiente = 0
        v-mes       = 0
        v-dia-total = 0
        v-mes-total = 0
        v-pendiente-total = 0. 
         /* POR APLICAR ES IGUAL A FALSE */       
        IF DepBanco.Conciliado = FALSE   THEN 
        DO: 
             ASSIGN 
             v-pendiente = 1
             v-pendiente-total = DepBanco.Importe.
        END.
        ELSE DO:
         IF DepBanco.FecDep     = TODAY THEN
         DO: 
          ASSIGN v-dia = 1
                 v-dia-total = DepBanco.Importe.
         END.
         ELSE DO:
          ASSIGN v-mes = 1
                 v-mes-total = DepBanco.Importe.
         END.
        END.

        FIND FIRST ttDeposito WHERE ttDeposito.Responsable = Resp.Nombre NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttDeposito THEN
        DO:
           CREATE ttDeposito.
           ASSIGN ttDeposito.Responsable         = Resp.Nombre
                  ttDeposito.Clase               = ClaseCte.Descr WHEN AVAILABLE ClaseCte
                  ttDeposito.DepHoy              = v-dia
                  ttDeposito.DepHoyTotal         = v-dia-total
                  ttDeposito.DepMes              = v-mes
                  ttDeposito.DepMesTotal         = v-mes-total
                  ttDeposito.DepPorAplicar       = v-pendiente
                  ttDeposito.DepPorAplicarTotal  = v-pendiente-total.
         END.
         ELSE DO:
         IF AVAILABLE ttDeposito THEN
            ASSIGN
                  ttDeposito.DepHoy              = ttDeposito.DepHoy             + v-dia
                  ttDeposito.DepHoyTotal         = ttDeposito.DepHoyTotal        + v-dia-total
                  ttDeposito.DepMes              = ttDeposito.DepMes             + v-mes
                  ttDeposito.DepMesTotal         = ttDeposito.DepMesTotal        + v-mes-total
                  ttDeposito.DepPorAplicar       = ttDeposito.DepPorAplicar      + v-pendiente
                  ttDeposito.DepPorAplicarTotal  = ttDeposito.DepPorAplicarTotal + v-pendiente-total.
          END.
    END.
END.

END PROCEDURE.
    