@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : depositoR.p
    Purpose     : /DepositoResp

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Thu Oct 24 23:52:29 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE v-num                 AS INTEGER. 
DEFINE VARIABLE v-resp                AS CHARACTER.
DEFINE VARIABLE v-clase               AS CHARACTER. 
DEFINE VARIABLE v-dia                 AS INTEGER.
DEFINE VARIABLE v-mes                 AS INTEGER.
DEFINE VARIABLE v-pendiente           AS INTEGER.
DEFINE VARIABLE v-dia-total           AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE v-mes-total           AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE v-pendiente-total     AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE v-dia-con             AS INTEGER.
DEFINE VARIABLE v-mes-con             AS INTEGER.
DEFINE VARIABLE v-pendiente-con       AS INTEGER.
DEFINE VARIABLE v-dia-total-con       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE v-mes-total-con       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE v-pendiente-total-con AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-lista               AS CHARACTER.

DEFINE TEMP-TABLE ttDepositoResp NO-UNDO
    FIELD Clase                 AS CHARACTER FORMAT "X(30)"
    FIELD DepMes                AS INTEGER
    FIELD DepMesTotal           AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD DepMesCon             AS INTEGER
    FIELD DepMesTotalCon        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD DepHoy                AS INTEGER
    FIELD DepHoyTotal           AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD DepHoyCon             AS INTEGER
    FIELD DepHoyTotalCon        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD DepPorAplicar         AS INTEGER
    FIELD DepPorAplicarTotal    AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD DepPorAplicarCon      AS INTEGER
    FIELD DepPorAplicarTotalCon AS DECIMAL   FORMAT ">>>,>>>,>>9.99" 
    INDEX idx-respo Clase ASCENDING.
DEFINE DATASET dsDeposito FOR ttDepositoResp.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDeposito:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER imes      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ianio     AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iclase    AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttDepositoResp.


DEFINE VARIABLE start-date      AS DATE NO-UNDO.
DEFINE VARIABLE end-date        AS DATE NO-UNDO.
DEFINE VARIABLE v-hoy           AS DATE NO-UNDO.

/* 1. Inicialización de variables clave */
ASSIGN 
    v-hoy = TODAY
    start-date = v-hoy - 90
    end-date = v-hoy.

 
IF imes  = 0 THEN imes   = MONTH(TODAY) .
IF ianio = 0 THEN ianio  = YEAR(TODAY). 
IF iclase = ? THEN iclase = 1.  // si no mandan clase de pone 1  

ASSIGN 
    /* Excluir ctes del 1-11 excepto al 3 en depositos credido-contado */
    l-lista = "1,2,4,5,6,7,8,9,10,11"
    v-num   = 0.

EMPTY TEMP-TABLE ttDepositoResp.     

/* 1. Procesar depósitos de hoy Credito segun clase */

FOR EACH depbanco
    WHERE depbanco.fecdep = v-hoy
      AND depbanco.Conciliado = TRUE
      AND DepBanco.TipoCte = iclase NO-LOCK :
    
    /* Validar clase del cliente */
    FIND FIRST cliente WHERE 
        cliente.id-cliente = depbanco.id-cliente 
        NO-LOCK NO-ERROR.
    IF AVAILABLE Cliente THEN 
    DO:
        IF LOOKUP(STRING(Cliente.Id-Cliente), SUBSTITUTE(l-lista, ",", "")) > 0 THEN NEXT.
    END.    

    /* Acumular valores para hoy */
    ASSIGN 
     v-dia = v-dia + 1
     v-dia-total = v-dia-total + depbanco.Importe.

END. /* PROCESS-DEPOSITOS-HOY CREDITO */


/* 2. Procesar depósitos de hoy Contado (todos ven esto) */

FOR EACH depbanco 
    WHERE depbanco.fecdep     = v-hoy
      AND depbanco.Conciliado = TRUE
      AND DepBanco.TipoCte    = 4 NO-LOCK :
    
    /* Validar  cliente */
    FIND FIRST cliente WHERE 
        cliente.id-cliente = depbanco.id-cliente NO-LOCK NO-ERROR.
    IF AVAILABLE Cliente THEN 
    DO:
        IF LOOKUP(STRING(Cliente.Id-Cliente), SUBSTITUTE(l-lista, ",", "")) > 0 THEN NEXT.
    END.    

    /* Acumular valores para hoy */
    ASSIGN 
     v-dia-con = v-dia-con + 1
     v-dia-total-con = v-dia-total-con + depbanco.Importe.
.
END. /* PROCESS-DEPOSITOS-HOY CONTADO */




/* 3. Procesar depósitos del mes credito */
FOR EACH depbanco NO-LOCK
    WHERE MONTH(depbanco.fecdep) = imes 
      AND YEAR(depbanco.fecdep) = ianio
      AND depbanco.Conciliado = TRUE
      AND DepBanco.TipoCte    = iclase:
    
    /* Excluir depósitos de hoy ya contabilizados */
    IF depbanco.fecdep = v-hoy THEN NEXT.
    
    /* Validar  cliente */
    FIND FIRST cliente WHERE 
        cliente.id-cliente = depbanco.id-cliente NO-LOCK NO-ERROR.
    IF AVAILABLE Cliente THEN 
    DO:
        IF LOOKUP(STRING(Cliente.Id-Cliente), SUBSTITUTE(l-lista, ",", "")) > 0 THEN NEXT.
    END. 
    ASSIGN 
      v-mes = v-mes + 1
      v-mes-total = v-mes-total + depbanco.Importe.
END. /* PROCESS-DEPOSITOS-MES CREDITO */


/* 4. Procesar depósitos del mes contado */
FOR EACH depbanco NO-LOCK
    WHERE MONTH(depbanco.fecdep) = imes 
      AND YEAR(depbanco.fecdep) = ianio
      AND depbanco.Conciliado = TRUE
      AND DepBanco.TipoCte    = 4:
    
    /* Excluir depósitos de hoy ya contabilizados */
    IF depbanco.fecdep = v-hoy THEN NEXT.
    
    /* Validar  cliente */
    FIND FIRST cliente WHERE 
        cliente.id-cliente = depbanco.id-cliente NO-LOCK NO-ERROR.
    IF AVAILABLE Cliente THEN 
    DO:
        IF LOOKUP(STRING(Cliente.Id-Cliente), SUBSTITUTE(l-lista, ",", "")) > 0 THEN NEXT.
    END. 

    ASSIGN 
           v-mes-con = v-mes-con + 1
           v-mes-total-con = v-mes-total-con + depbanco.Importe.
END. /* PROCESS-DEPOSITOS-MES CONTADO */




/* Primero procesar depósitos pendientes de últimos 90 días
   de Credito de la Clase  */
FOR EACH depbanco NO-LOCK 
    WHERE depbanco.fecdep >= start-date
      AND depbanco.fecdep <= end-date
      AND depbanco.Conciliado = FALSE
      AND DepBanco.TipoCte    = iclase
      AND DepBanco.Activo :
    
    /* Validar  cliente */
    FIND FIRST cliente WHERE 
        cliente.id-cliente = depbanco.id-cliente NO-LOCK NO-ERROR.
    IF AVAILABLE Cliente THEN 
    DO:
        IF LOOKUP(STRING(Cliente.Id-Cliente), SUBSTITUTE(l-lista, ",", "")) > 0 THEN NEXT.
    END. 
    
    ASSIGN 
     v-pendiente       = v-pendiente + 1
     v-pendiente-total = v-pendiente-total + depbanco.Importe.
    
END. /* Fin de procesamiento de pendientes credito */

/* Primero procesar depósitos pendientes de últimos 90 días
   de contado  */
FOR EACH depbanco NO-LOCK 
    WHERE depbanco.fecdep >= start-date
      AND depbanco.fecdep <= end-date
      AND depbanco.Conciliado = FALSE
      AND DepBanco.TipoCte    = 4
      AND DepBanco.Activo :
    
    /* Validar  cliente */
    FIND FIRST cliente WHERE 
        cliente.id-cliente = depbanco.id-cliente NO-LOCK NO-ERROR.
    IF AVAILABLE Cliente THEN 
    DO:
        IF LOOKUP(STRING(Cliente.Id-Cliente), SUBSTITUTE(l-lista, ",", "")) > 0 THEN NEXT.
    END. 
    
    ASSIGN 
      v-pendiente-con       = v-pendiente-con + 1
      v-pendiente-total-con = v-pendiente-total-con + depbanco.Importe.
      
END. /* Fin de procesamiento de pendientes contado */



/* 5. Consolidar resultados */
FIND FIRST ClaseCte WHERE ClaseCte.id-ClaseCte = iclase NO-LOCK NO-ERROR.

FIND FIRST ttDepositoResp WHERE ttDepositoResp.Clase = ClaseCte.Descr NO-ERROR.
IF NOT AVAILABLE ttDepositoResp THEN DO:
    CREATE ttDepositoResp.
    ASSIGN ttDepositoResp.Clase = ClaseCte.Descr.
END.

ASSIGN
    /* Valores del mes */
    ttDepositoResp.DepMes                = v-mes           // Depositos credito aplicados Numero
    ttDepositoResp.DepMesTotal           = v-mes-total     // Depositos credito aplicados Importe
    ttDepositoResp.DepMesCon             = v-mes-con       // Depositos contado aplicados Numero
    ttDepositoResp.DepMesTotalCon        = v-mes-total-con // Depositos contado aplicados Importe
    
    /* Valores de hoy */
    ttDepositoResp.DepHoy                = v-dia           // Depositos credito aplicados HOY Numero
    ttDepositoResp.DepHoyTotal           = v-dia-total     // Depositos credito aplicados HOY importe
    ttDepositoResp.DepHoyCon             = v-dia-con       // Depositos contado aplicados HOY Numero
    ttDepositoResp.DepHoyTotalCon        = v-dia-total-con // Depositos contado aplicados HOY importe
    
    /* Valores pendientes */
    ttDepositoResp.DepPorAplicar         = v-pendiente       // Depositos credito pendientes apli Numero
    ttDepositoResp.DepPorAplicarTotal    = v-pendiente-total // Depositos credito pendientes apli Importe
    ttDepositoResp.DepPorAplicarCon      = v-pendiente-con   // Depositos contado pendientes apli Numero
    ttDepositoResp.DepPorAplicarTotalCon = v-pendiente-total-con. // Depositos contado pendientes apli Importe     
END PROCEDURE.    

