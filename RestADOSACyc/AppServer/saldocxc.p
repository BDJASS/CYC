@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : saldocxc.p    
    Purpose     : /Saldo

    Syntax      : Se utiliza para Card Director

    Description : 

    Author(s)   : sis10
    Created     : Mon Oct 21 08:04:40 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.


/* ********************  Preprocessor Definitions  ******************** */




/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num       AS INT.
DEFINE VARIABLE l-tot-total AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-ven   AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig   AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv  AS DECIMAL FORMAT ">>>,>>>,>>9.99".

DEFINE TEMP-TABLE ttTotal NO-UNDO
    FIELD Total     AS DECIMAL FORMAT ">>>,>>>,>>9.99" 
    FIELD Vigente   AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD Venc      AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD PorVen    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD SolicitudesNuevas AS INT
    FIELD SolicitudesValidacion AS INT 

    INDEX idx-num Total ASCENDING.
    DEFINE DATASET dsTotal FOR ttTotal.
    
    DEFINE BUFFER bfSolCred FOR SolCred. 
    DEFINE VARIABLE iCountEstatus1 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCountEstatus2 AS INTEGER NO-UNDO.
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetSaldos:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER TABLE FOR ttTotal.
    
ASSIGN
    l-tot-total = 0
    l-tot-ven   = 0
    l-tot-vig   = 0
    l-tot-porv  = 0.    
    
 EMPTY TEMP-TABLE ttTotal.   
 FOR EACH Cliente   NO-LOCK :
    FOR EACH Movcliente WHERE Movcliente.id-cliente = Cliente.id-cliente AND
                              Movcliente.FecReg <= TODAY                 AND
                              MovCliente.Id-MC  <= 3                     AND
                              MovCliente.Afectado                       
        NO-LOCK  BREAK  BY Cliente.RazonSocial BY Cliente.id-cliente 
                                               BY MovCliente.Id-Cliente  :
            
        IF MovCliente.Saldo  <= 0 THEN NEXT.
        IF MovCliente.fecven <  TODAY THEN 
        DO:
           l-tot-ven  = l-tot-ven  +  Movcliente.Saldo. 
        END.
        IF MovCliente.fecven >= TODAY AND
           MovCliente.fecven <= TODAY + 15 THEN 
        DO: 
           l-tot-porv = l-tot-porv +  Movcliente.Saldo.  
        END.
        IF MovCliente.fecven >= TODAY + 16 THEN
        DO:
            l-tot-vig = l-tot-vig  + Movcliente.Saldo.
        END.
        l-tot-total =  l-tot-ven + l-tot-porv + l-tot-vig.          
    END.
END.

/* Realizar conteo directo en la base de datos */
FOR EACH bfSolCred WHERE bfSolCred.IdEstatus = 1 NO-LOCK:
    ASSIGN iCountEstatus1 = iCountEstatus1 + 1.
END.

FOR EACH bfSolCred WHERE bfSolCred.IdEstatus = 2 NO-LOCK:
    ASSIGN iCountEstatus2 = iCountEstatus2 + 1.
END.  

FIND FIRST ttTotal WHERE ttTotal.Total = l-tot-total NO-LOCK NO-ERROR.
IF NOT AVAILABLE ttTotal THEN 
    CREATE ttTotal.
ASSIGN
    ttTotal.Total   = l-tot-total
    ttTotal.Vigente = l-tot-vig
    ttTotal.Venc    = l-tot-ven
    ttTotal.PorVen  = l-tot-porv
    ttTotal.SolicitudesNuevas = iCountEstatus1
    ttTotal.SolicitudesValidacion = iCountEstatus2.
RELEASE ttTotal.  




RETURN.  
END PROCEDURE.


    
    
    