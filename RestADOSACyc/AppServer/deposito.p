@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : deposito.p
    Purpose     : /Deposito

    Syntax      : Pantalla Director

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
DEFINE VARIABLE l-lista     AS CHARACTER.
DEF BUFFER bf-dep FOR DepBanco.
DEF BUFFER bf-cli FOR Cliente.

DEFINE TEMP-TABLE ttDepositoPendiente NO-UNDO
    FIELD Responsable        AS CHARACTER FORMAT "X(30)"
    FIELD DepPorAplicar      AS INTEGER
    FIELD DepPorAplicarTotal AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    INDEX idx-respo Responsable ASCENDING.

DEFINE TEMP-TABLE ttDeposito NO-UNDO
    FIELD Responsable        AS CHARACTER FORMAT "X(30)"
    FIELD Clase              AS CHARACTER FORMAT "X(30)"
    FIELD DepHoy             AS INTEGER
    FIELD DepHoyTotal        AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD DepMes             AS INTEGER
    FIELD DepMesTotal        AS DECIMAL FORMAT ">>>,>>>,>>9.99"
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
    l-lista = "1,2,4,5,6,7,8,9,10,11". 
    
DEFINE VARIABLE start-date      AS DATE NO-UNDO.
DEFINE VARIABLE end-date        AS DATE NO-UNDO.
DEFINE VARIABLE v-hoy           AS DATE NO-UNDO.

/* 1. Inicialización de variables clave */
ASSIGN 
    v-hoy = TODAY
    start-date = v-hoy - 90
    end-date = v-hoy.     

EMPTY TEMP-TABLE ttDepositoPendiente.

/* 1. Procesar todos los depósitos en un solo recorrido */
FOR EACH DepBanco NO-LOCK
    WHERE (DepBanco.FecDep >= start-date AND DepBanco.FecDep <= v-hoy):
    
    IF DepBanco.TipoCte = 4 THEN NEXT.  /* Excluir contado */
    
    /* Validar clase del cliente */
    FIND FIRST cliente WHERE cliente.id-cliente = depbanco.id-cliente 
        NO-LOCK NO-ERROR.
    IF AVAILABLE Cliente THEN 
    DO:
        IF LOOKUP(STRING(Cliente.Id-Cliente), SUBSTITUTE(l-lista, ",", "")) > 0 THEN NEXT.
        FIND FIRST Resp WHERE  Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
    END.    

    FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = DepBanco.TipoCte NO-LOCK NO-ERROR.


    /* 2. Determinar categorías del depósito */
    ASSIGN 
        v-dia = 0
        v-mes = 0
        v-pendiente = 0
        v-dia-total = 0
        v-mes-total = 0
        v-pendiente-total = 0.

    CASE TRUE:
        /* Depósitos de hoy conciliados */
        WHEN DepBanco.FecDep = v-hoy AND DepBanco.Conciliado THEN
            ASSIGN 
                v-dia = 1
                v-dia-total = DepBanco.Importe.
        
        /* Depósitos del mes (excluyendo hoy) */
        WHEN MONTH(DepBanco.FecDep) = MONTH(TODAY) 
            AND YEAR(DepBanco.FecDep) = YEAR(TODAY)
            AND DepBanco.FecDep < v-hoy
            AND DepBanco.Conciliado THEN
            ASSIGN 
                v-mes = 1
                v-mes-total = DepBanco.Importe.
        
        /* Pendientes últimos 90 días */
        WHEN DepBanco.FecDep >= start-date 
            AND DepBanco.FecDep <= v-hoy
            AND DepBanco.Activo
            AND NOT DepBanco.Conciliado THEN
            ASSIGN 
                v-pendiente = 1
                v-pendiente-total = DepBanco.Importe.
    END CASE.

    /* 3. Actualizar registros temporales */
    FIND FIRST ttDeposito WHERE ttDeposito.Clase = ClaseCte.Descr
        NO-LOCK NO-ERROR.
        
    IF NOT AVAILABLE ttDeposito THEN DO:
        CREATE ttDeposito.
        ASSIGN 
            ttDeposito.Responsable   = Resp.Nombre
            ttDeposito.Clase         = ClaseCte.Descr
            ttDeposito.DepHoy        = v-dia
            ttDeposito.DepHoyTotal   = v-dia-total
            ttDeposito.DepMes        = v-mes
            ttDeposito.DepMesTotal   = v-mes-total
            ttDeposito.DepPorAplicar = v-pendiente
            ttDeposito.DepPorAplicarTotal = v-pendiente-total.
    END.
    ELSE DO:
        ASSIGN
            ttDeposito.DepHoy         = ttDeposito.DepHoy + v-dia
            ttDeposito.DepHoyTotal    = ttDeposito.DepHoyTotal + v-dia-total
            ttDeposito.DepMes         = ttDeposito.DepMes + v-mes
            ttDeposito.DepMesTotal    = ttDeposito.DepMesTotal + v-mes-total
            ttDeposito.DepPorAplicar  = ttDeposito.DepPorAplicar + v-pendiente
            ttDeposito.DepPorAplicarTotal = ttDeposito.DepPorAplicarTotal + v-pendiente-total.
    END.
END.     

/* 4. Eliminar registros sin movimientos */
FOR EACH ttDeposito:
    IF ttDeposito.DepHoy = 0 
        AND ttDeposito.DepMes = 0 
        AND ttDeposito.DepPorAplicar = 0 THEN
        DELETE ttDeposito.
END.

END PROCEDURE.

    