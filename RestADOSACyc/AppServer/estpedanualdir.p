@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : estpedanualdir.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Tue Oct 29 07:29:23 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF VAR l-ClaseCte AS CHAR NO-UNDO.
DEF VAR l-NomResp AS CHAR NO-UNDO.
DEF VAR l-NP AS INTEGER NO-UNDO.
DEF VAR l-Imp AS DECIMAL NO-UNDO.
DEF VAR l-NomClase AS CHARACTER.


DEF TEMP-TABLE ttEstPed
    FIELD Clase   AS CHAR
    FIELD NMes    AS INTEGER
    FIELD NPed    AS INTEGER
    FIELD ImpPed  AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD NFac    AS INTEGER
    FIELD ImpFac  AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD NCanc   AS INTEGER
    FIELD ImpCanc AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    INDEX Idx-Def NMes. 
 DEFINE DATASET dsEstPed FOR ttEstPed. 

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetPedidoAnualDir:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pClase  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pNano  AS INTEGER NO-UNDO.
DEF OUTPUT PARAMETER TABLE FOR ttEstPed.

IF pClase = ? THEN pClase = 1.
IF pNano  = ? THEN pNano  = YEAR(TODAY).
         
EMPTY TEMP-TABLE ttEstPed.

ASSIGN l-ClaseCte = "".
FOR EACH Cliente WHERE Cliente.Id-ClaseCte = pClase NO-LOCK,
    EACH Pedido WHERE Pedido.Id-Cliente = Cliente.Id-Cliente
                  AND Pedido.FecReg >= DATE(1,1,pNano)
                  AND Pedido.FecReg <= DATE(12,31,pNano)
                  AND Pedido.Id-Cond > 0 NO-LOCK:
    FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
    IF AVAILABLE ClaseCte THEN l-ClaseCte = ClaseCte.Descr.
    FIND FIRST ttEstPed WHERE ttEstPed.NMes = MONTH(Pedido.FecReg) EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ttEstPed THEN DO:
       CREATE ttEstPed.
       ASSIGN ttEstPed.NMes = MONTH(Pedido.FecReg)
              ttEstPed.Clase = l-ClaseCte.
    END. 
    IF Pedido.EnFirme = FALSE OR Pedido.SolCancPed = TRUE THEN
       ASSIGN ttEstPed.NCanc   = ttEstPed.NCanc + 1
              ttEstPed.ImpCanc = ttEstPed.ImpCanc + Pedido.Tot.
    ELSE IF Pedido.AutPor <> "" THEN
       ASSIGN ttEstPed.NPed   = ttEstPed.NPed + 1
              ttEstPed.ImpPed = ttEstPed.ImpPed + Pedido.Tot.
    RELEASE ttEstPed.
END.


ASSIGN l-ClaseCte = "".
FOR EACH Cliente WHERE Cliente.Id-ClaseCte = pClase NO-LOCK,
    EACH CancPed WHERE CancPed.Id-Cliente = Cliente.Id-Cliente
                  AND CancPed.FecReg >= DATE(1,1,pNano)
                  AND CancPed.FecReg <= DATE(12,31,pNano)
                  AND CancPed.Id-Cond > 0 NO-LOCK:
    FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
    IF AVAILABLE ClaseCte THEN l-ClaseCte = ClaseCte.Descr.                  
  
    FIND FIRST ttEstPed WHERE ttEstPed.NMes = MONTH(CancPed.FecReg) EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ttEstPed THEN DO:
       CREATE ttEstPed.
       ASSIGN ttEstPed.NMes = MONTH(CancPed.FecReg)
              ttEstPed.Clase = l-ClaseCte.
    END.
    IF NOT CancPed.SolCancPed THEN
       ASSIGN ttEstPed.NCanc   = ttEstPed.NCanc + 1
              ttEstPed.ImpCanc = ttEstPed.ImpCanc + CancPed.Tot.
    RELEASE ttEstPed.
END.
ASSIGN l-ClaseCte = "".
FOR EACH Cliente WHERE Cliente.Id-ClaseCte = pClase  NO-LOCK,
    EACH Factura WHERE Factura.Id-Cliente = Cliente.Id-Cliente
                  AND Factura.FecReg >= DATE(1,1,pNano)
                  AND Factura.FecReg <= DATE(12,31,pNano)
                  AND Factura.Id-Cond > 0 NO-LOCK:
    FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
    IF AVAILABLE ClaseCte THEN l-ClaseCte = ClaseCte.Descr.                  
  
    IF Factura.FecCanc <> ? THEN NEXT.
    FIND FIRST ttEstPed WHERE ttEstPed.NMes = MONTH(Factura.FecReg) EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ttEstPed THEN DO:
       CREATE ttEstPed.
       ASSIGN ttEstPed.NMes = MONTH(Factura.FecReg)
              ttEstPed.Clase = l-ClaseCte.
    END.
    ASSIGN ttEstPed.NFac   = ttEstPed.NFac + 1
           ttEstPed.ImpFac = ttEstPed.ImpFac + Factura.Tot.
    RELEASE ttEstPed.
END.

END PROCEDURE.

