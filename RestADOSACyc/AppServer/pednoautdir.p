@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : pednoautdir.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Mon Oct 28 17:11:56 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF VAR l-ClaseCte AS INTEGER NO-UNDO.
DEF VAR l-NomResp AS CHAR NO-UNDO.
DEF VAR l-NP AS INTEGER NO-UNDO.
DEF VAR l-Imp AS DECIMAL NO-UNDO.
DEF VAR l-NomClase AS CHARACTER.

DEF TEMP-TABLE ttPedXAut
    FIELD NomResp AS CHAR
    FIELD Clase   AS CHAR
    FIELD NPBloq AS INTEGER
    FIELD ImpBloq AS DECIMAL 
    FIELD NCBloq AS INTEGER
    FIELD NPNoBloq AS INTEGER
    FIELD ImpNoBloq AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD NCNoBloq AS INTEGER
    INDEX Idx-Def NomResp.
 DEFINE DATASET dsPedXAut FOR ttPedXAut.   




/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetPedidoDirector:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER TABLE FOR ttPedXAut.

EMPTY TEMP-TABLE ttPedXAut.

l-ClaseCte = 0.
l-NomResp = "".         
FOR EACH Pedido WHERE Pedido.Id-Pedido BEGINS '0'
                  AND Pedido.EnFirme = FALSE 
                  AND Pedido.Id-Vendedor <> "0100" NO-LOCK,
    FIRST Cliente WHERE Cliente.Id-Cliente = Pedido.Id-Cliente 
    NO-LOCK BREAK BY Pedido.Id-Cliente:
        
    FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
    IF AVAILABLE Resp THEN l-NomResp = Resp.Nombre.
    IF FIRST-OF(Pedido.Id-Cliente) THEN
       ASSIGN l-NP = 0
              l-Imp = 0
              l-ClaseCte = 0.
    l-ClaseCte = Cliente.Id-ClaseCte. 
    FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
    IF AVAILABLE ClaseCte THEN l-NomClase = ClaseCte.Descr.       
    l-NP = l-NP + 1.
    IF Pedido.Id-Moneda > 1 THEN
       l-Imp = l-Imp + (Pedido.Tot * Pedido.TipoCambio).
    ELSE l-Imp = l-Imp + Pedido.Tot.
       
    IF LAST-OF(Pedido.Id-Cliente) THEN DO:
       IF l-ClaseCte = 0 OR (l-ClaseCte <> 0 AND l-ClaseCte = Cliente.Id-ClaseCte) THEN DO:
          FIND FIRST ttPedXAut WHERE ttPedXAut.NomResp = l-NomResp EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAILABLE ttPedXAut THEN DO:
             CREATE ttPedXAut.
             ASSIGN ttPedXAut.NomResp = l-NomResp
                    ttPedXAut.Clase   =  l-NomClase.
          END.
          FIND FIRST BlkAut WHERE BlkAut.Id-Cliente = Pedido.Id-Cliente NO-LOCK NO-ERROR.
          FIND FIRST blkRFC WHERE REPLACE(blkRFC.RFC," ","") = REPLACE(Pedido.RFC," ","") NO-LOCK NO-ERROR.
          IF AVAILABLE BlkAut OR AVAILABLE blkRFC THEN DO:
             ASSIGN ttPedXAut.NPBloq  = ttPedXAut.NPBloq + l-NP
                    ttPedXAut.ImpBloq = ttPedXAut.ImpBloq + l-Imp
                    ttPedXAut.NCBloq  = ttPedXAut.NCBloq + 1.
          END.
          ELSE DO:
             ASSIGN ttPedXAut.NPNoBloq  = ttPedXAut.NPNoBloq + l-NP
                    ttPedXAut.ImpNoBloq = ttPedXAut.ImpNoBloq + l-Imp
                    ttPedXAut.NCNoBloq  = ttPedXAut.NCNoBloq + 1.
          END.
          RELEASE ttPedXAut.
       END.
    END.
END.
RETURN.
END PROCEDURE.

