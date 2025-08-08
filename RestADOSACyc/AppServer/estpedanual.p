@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
    Programa:   cxcs0010.p
    Funcion :   Rellenar tabla de dashboard de pedidos por autorizar
    Autor   :   FLC
    Fecha   :   24 OCT 2024
*/

DEF VAR l-ClaseCte AS INTEGER NO-UNDO.
DEF VAR l-NomResp AS CHAR NO-UNDO.

DEF TEMP-TABLE ttEstPed
    FIELD NMes   AS INTEGER
    FIELD NPed AS INTEGER
    FIELD ImpPed AS DECIMAL
    FIELD NFac AS INTEGER
    FIELD ImpFac AS DECIMAL
    FIELD NCanc AS INTEGER
    FIELD ImpCanc AS DECIMAL
    INDEX Idx-Def NMes. 
    




/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetPedidoAnual:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pUser LIKE Usuario.Id-User NO-UNDO.
DEF INPUT PARAMETER pNano AS INTEGER NO-UNDO.
DEF OUTPUT PARAMETER TABLE FOR ttEstPed.
    
EMPTY TEMP-TABLE ttEstPed.

l-ClaseCte = 0.
l-NomResp = "".
FIND Usuario WHERE Usuario.Id-User = pUser NO-LOCK NO-ERROR.
IF AVAILABLE Usuario THEN
   ASSIGN l-ClaseCte = Usuario.Id-ClaseCte
          l-NomResp = Usuario.Nom-Usuario.
          
FOR EACH Cliente WHERE (IF l-ClaseCte = 0 THEN TRUE ELSE Cliente.Id-ClaseCte = l-ClaseCte) NO-LOCK,
    EACH Pedido WHERE Pedido.Id-Cliente = Cliente.Id-Cliente
                  AND Pedido.FecReg >= DATE(1,1,pNano)
                  AND Pedido.FecReg <= DATE(12,31,pNano)
                  AND Pedido.Id-Cond > 0 NO-LOCK:
  
    FIND FIRST ttEstPed WHERE ttEstPed.NMes = MONTH(Pedido.FecReg) EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ttEstPed THEN DO:
       CREATE ttEstPed.
       ASSIGN ttEstPed.NMes = MONTH(Pedido.FecReg).
    END.
    IF Pedido.EnFirme = FALSE OR Pedido.SolCancPed = TRUE THEN
       ASSIGN ttEstPed.NCanc   = ttEstPed.NCanc + 1
              ttEstPed.ImpCanc = ttEstPed.ImpCanc + Pedido.Tot.
    ELSE IF Pedido.AutPor <> "" THEN
       ASSIGN ttEstPed.NPed   = ttEstPed.NPed + 1
              ttEstPed.ImpPed = ttEstPed.ImpPed + Pedido.Tot.
    RELEASE ttEstPed.
END.

FOR EACH Cliente WHERE (IF l-ClaseCte = 0 THEN TRUE ELSE Cliente.Id-ClaseCte = l-ClaseCte) NO-LOCK,
    EACH CancPed WHERE CancPed.Id-Cliente = Cliente.Id-Cliente
                  AND CancPed.FecReg >= DATE(1,1,pNano)
                  AND CancPed.FecReg <= DATE(12,31,pNano)
                  AND CancPed.Id-Cond > 0 NO-LOCK:
  
    FIND FIRST ttEstPed WHERE ttEstPed.NMes = MONTH(CancPed.FecReg) EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ttEstPed THEN DO:
       CREATE ttEstPed.
       ASSIGN ttEstPed.NMes = MONTH(CancPed.FecReg).
    END.
    IF NOT CancPed.SolCancPed THEN
       ASSIGN ttEstPed.NCanc   = ttEstPed.NCanc + 1
              ttEstPed.ImpCanc = ttEstPed.ImpCanc + CancPed.Tot.
    RELEASE ttEstPed.
END.

FOR EACH Cliente WHERE (IF l-ClaseCte = 0 THEN TRUE ELSE Cliente.Id-ClaseCte = l-ClaseCte) NO-LOCK,
    EACH Factura WHERE Factura.Id-Cliente = Cliente.Id-Cliente
                  AND Factura.FecReg >= DATE(1,1,pNano)
                  AND Factura.FecReg <= DATE(12,31,pNano)
                  AND Factura.Id-Cond > 0 NO-LOCK:
  
    IF Factura.FecCanc <> ? THEN NEXT.
    FIND FIRST ttEstPed WHERE ttEstPed.NMes = MONTH(Factura.FecReg) EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ttEstPed THEN DO:
       CREATE ttEstPed.
       ASSIGN ttEstPed.NMes = MONTH(Factura.FecReg).
    END.
    ASSIGN ttEstPed.NFac   = ttEstPed.NFac + 1
           ttEstPed.ImpFac = ttEstPed.ImpFac + Factura.Tot.
    RELEASE ttEstPed.
END.

END PROCEDURE.
