@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : saldocardresp.p
    Purpose     : /SaldoResponsable

    Syntax      : Se utiliza para Card Resp   

    Description : 

    Author(s)   : sis10
    Created     : Mon Oct 28 15:16:58 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num       AS INT.
DEFINE VARIABLE l-tot-total AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-ven   AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv  AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vigen AS DECIMAL FORMAT ">>>,>>>,>>9.99".

DEFINE VARIABLE l-total AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-ven   AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-porv  AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-vigen AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-total-cartera AS DECIMAL FORMAT ">>>,>>>,>>9.99".

DEF BUFFER bf-mov FOR MovCliente .  
DEF BUFFER bf-cli FOR Cliente .  

DEFINE TEMP-TABLE ttCardResponsable NO-UNDO
    FIELD num          AS INTEGER
    FIELD TotalCartera AS DECIMAL FORMAT ">>>,>>>,>>9.99" 
    FIELD MiCartera      AS DECIMAL FORMAT ">>>,>>>,>>9.99" 
    FIELD Vigente      AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD Vencido      AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD PorVencer    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD SolicitudesNuevas     AS INT
    FIELD SolicitudesValidacion AS INT 

    INDEX idx-num num ASCENDING.
    DEFINE BUFFER bfSolCred FOR SolCred.
    DEFINE VARIABLE iCountEstatus1 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCountEstatus2 AS INTEGER NO-UNDO.
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCardResponsables:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER iClase  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttCardResponsable.

EMPTY TEMP-TABLE ttCardResponsable.

ASSIGN
    l-num   = 0
    l-vigen = 0
    l-ven   = 0
    l-porv  = 0
    l-total = 0.

FOR EACH Cliente  WHERE Cliente.Id-ClaseCte = iClase NO-LOCK:
    FOR EACH MovCliente WHERE MovCliente.id-Cliente = Cliente.id-Cliente 
                          AND MovCliente.FecReg <= TODAY                 
                          AND MovCliente.Id-MC  <= 3                     
                          AND  MovCliente.Afectado NO-LOCK                       
          BREAK BY Cliente.RazonSocial 
                BY Cliente.Id-Cliente
                BY MovCliente.Id-Cliente:
        IF MovCliente.Saldo  <= 0 THEN NEXT.
        
         ASSIGN 
                l-tot-total = 0
                l-tot-vigen = 0   
                l-tot-ven   = 0
                l-tot-porv  = 0.
            IF MovCliente.fecven <  TODAY THEN 
            DO:
                ASSIGN
                    l-tot-ven = l-tot-ven  +  MovCliente.Saldo.
            END. 
            IF MovCliente.fecven >= TODAY + 16 THEN 
            DO:
                ASSIGN 
                    l-tot-vigen = l-tot-vigen +  MovCliente.Saldo.
            END.
            IF MovCliente.fecven >= TODAY AND
                MovCliente.fecven <= TODAY + 15 THEN 
            DO:
                ASSIGN 
                    l-tot-porv = l-tot-porv +  MovCliente.Saldo.
            END.            
            l-vigen = l-vigen +  l-tot-vigen.
            l-ven   = l-ven   +  l-tot-ven.
            l-porv  = l-porv  +  l-tot-porv.
            l-total = l-vigen + l-ven + l-porv.
            
            l-num = l-num + 1. 
    END.
END.

 ASSIGN 
  l-tot-total-cartera = 0.
FOR EACH bf-cli  NO-LOCK:
    FOR EACH bf-mov WHERE bf-mov.id-Cliente = bf-cli.id-Cliente 
                          AND bf-mov.FecReg <= TODAY                 
                          AND bf-mov.Id-MC  <= 3                     
                          AND  bf-mov.Afectado NO-LOCK                       
          BREAK BY bf-cli.RazonSocial 
                BY bf-cli.Id-Cliente
                BY bf-mov.Id-Cliente:
        IF bf-mov.Saldo  <= 0 THEN NEXT.
        
        
         l-tot-total-cartera = l-tot-total-cartera + bf-mov.Saldo.
        
    END. 
END.

/* Realizar conteo directo en la base de datos */
FOR EACH bfSolCred WHERE bfSolCred.IdEstatus = 1 NO-LOCK:
    ASSIGN iCountEstatus1 = iCountEstatus1 + 1.
END. 
FOR EACH bfSolCred WHERE bfSolCred.IdEstatus = 2 NO-LOCK:
    ASSIGN iCountEstatus2 = iCountEstatus2 + 1.
END. 

FIND FIRST ttCardResponsable WHERE ttCardResponsable.num = l-num NO-LOCK NO-ERROR.
IF NOT AVAILABLE ttCardResponsable THEN
    CREATE ttCardResponsable.   
ASSIGN
    ttCardResponsable.num                   = l-num
    ttCardResponsable.TotalCartera          = l-tot-total-cartera
    ttCardResponsable.MiCartera             = l-total
    ttCardResponsable.Vigente               = l-vigen
    ttCardResponsable.Vencido               = l-ven   
    ttCardResponsable.PorVencer             = l-porv  
    ttCardResponsable.SolicitudesNuevas     = iCountEstatus1
    ttCardResponsable.SolicitudesValidacion = iCountEstatus2. 
RELEASE ttCardResponsable.
RETURN.
END PROCEDURE.

