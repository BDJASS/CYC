@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : custom2.p
    Purpose     : 

    Syntax      :

    Description : releasecte

    Author(s)   : sis10
    Created     : Wed Mar 05 07:33:54 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */



DEF VAR l-teclas      AS   CHAR                 NO-UNDO INITIAL "ENTER,GO,RETURN,TAB".
DEF VAR l-nomprov     AS   CHAR                 NO-UNDO INITIAL "ABASTECEDORA DE OFICINAS S.A. DE C.V.".
DEF VAR l-lista       AS   CHAR                 NO-UNDO.
DEF VAR l-FrameValue  AS   CHAR                 NO-UNDO.
DEF VAR l-Archivo     AS   CHARACTER            NO-UNDO.
DEF VAR l-Print   AS CHAR NO-UNDO EXTENT 3 INITIAL ['Si','No','Todos'].
DEF VAR l-iPrint  AS INTE NO-UNDO.
DEF VAR l-lPrint  AS LOGI NO-UNDO INITIAL TRUE.
DEF VAR l-Print2   AS CHAR NO-UNDO EXTENT 3 INITIAL ['No','Si','Todos'].
DEF VAR l-iPrint2  AS INTE NO-UNDO.
DEF VAR l-lPrint2  AS LOGI NO-UNDO INITIAL TRUE.


DEFINE TEMP-TABLE ttRel
    FIELD IdCliente LIKE Pedido.Id-Cliente
    FIELD Cliente    LIKE Cliente.RazonSocial
    FIELD Pedidos    AS CHAR FORMAT 'X(29)'
    FIELD Importe        AS DEC FORMAT '-ZZZ,ZZZ,ZZ9.99'
    FIELD IdFactura LIKE Factura.Id-Factura
    FIELD Fecfac     LIKE Factura.FecReg
    FIELD Fecemb     LIKE EstPedido.FecEmb
    FIELD Guia       LIKE EstPedido.Guia
    FIELD NumIR     LIKE Pedido.RecCte 
    FIELD Rel     LIKE Pedido.ReqCte  
    INDEX Idx-Req IdCliente Rel.     
    

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE RepRelease:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER v-fecini  AS DATE NO-UNDO.    
DEFINE INPUT PARAMETER v-fecfin  AS DATE NO-UNDO.
DEFINE INPUT PARAMETER l-iPrint AS INTEGER NO-UNDO. //  Pedidos Embarcados SI/NO/TODOS
DEFINE INPUT PARAMETER l-iPrint2 AS INTEGER NO-UNDO. // Pedidos Facturados SI/NO/TODOS
DEFINE INPUT PARAMETER l-Id-cliente  LIKE Cliente.Id-Cliente.
DEFINE INPUT PARAMETER l-Asociado AS LOGICAL.
DEFINE OUTPUT PARAMETER TABLE FOR ttRel.

DEFINE BUFFER bfCliente FOR Cliente.
IF v-fecini = ? THEN v-fecini = TODAY - 1.
IF v-fecfin = ? THEN v-fecfin = TODAY. 
IF l-iPrint = ? THEN l-iPrint = 1.
IF l-iPrint2 = ? THEN l-iPrint2 = 1.
IF l-Asociado = ? THEN l-Asociado = FALSE.


/* Datos que manda el FRONT 

  1 = TODOS
  2 = SI
  3 = NO

*/
    /* Construir la lista de clientes */
    IF l-Asociado THEN DO:
        /* Si l-Asociado es TRUE, buscar todos los asociados */
        ASSIGN l-lista = STRING(l-Id-cliente).
        FOR EACH Asociado WHERE Asociado.Id-Cliente = l-Id-Cliente NO-LOCK:
            ASSIGN l-lista = l-lista + "," + STRING(Asociado.Id-Asociado).
        END.
    END.
    ELSE DO:
        /* Si l-Asociado es FALSE, solo buscar por el cliente */
        ASSIGN l-lista = STRING(l-Id-cliente).
    END.   
 

    /*Inicia reporte*/
    FOR EACH Pedido WHERE CAN-DO(l-lista, STRING(Pedido.Id-Cliente)) AND Pedido.EnFirme
                      AND Pedido.FecReg >= v-fecini AND Pedido.FecReg <= v-fecfin NO-LOCK.        
        FIND EstPedido WHERE EstPedido.Id-Pedido = Pedido.Id-Pedido AND EstPedido.Id-Seq = Pedido.Resto NO-LOCK NO-ERROR.
        
        IF NOT AVAILABLE EstPedido THEN NEXT.
        IF Pedido.ReqCte = '' THEN NEXT.
        /*FILTRO DE EMBARQUE*/
        IF l-iPrint = 2 AND EstPedido.Id-Embarque = '' THEN NEXT.  // Embarcados
        IF l-iPrint = 3 AND EstPedido.Id-Embarque <> '' THEN NEXT. // No Embarcados
        
        /*FILTRO DE FACTURADOS*/    
        IF l-iPrint2 = 2 AND Pedido.Id-Factura = '' THEN NEXT.    // Facturados    
        IF l-iPrint2 = 3 AND Pedido.Id-Factura <> '' THEN NEXT.   // No Facturados  

        STATUS DEFAULT "Revisando Pedido " + STRING(Pedido.Id-Pedido) + "...".

        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = Pedido.Id-Cliente NO-LOCK NO-ERROR.
        FIND ttRel WHERE ttRel.Rel = Pedido.ReqCte NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttRel THEN DO:
            CREATE ttRel.
            ASSIGN 
                   ttRel.IdCliente = Pedido.Id-Cliente
                   ttRel.Cliente    = IF AVAILABLE bfCliente THEN bfCliente.RazonSocial ELSE "" 
                   ttRel.Rel     = Pedido.ReqCte
                   ttRel.NumIR     = Pedido.RecCte
                   ttRel.IdFactura = Pedido.Id-Factura 
                   ttRel.fecfac     = Pedido.FecFac
                   ttRel.fecemb     = EstPedido.FecEmb WHEN AVAILABLE EstPedido
                   ttRel.Guia       = EstPedido.Guia   WHEN AVAILABLE EstPedido.                    
        END.
        FOR EACH DetPedido WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido AND
                                 DetPedido.Resto     = Pedido.Resto NO-LOCK.
            ASSIGN 
                ttRel.Importe = ttRel.Importe + (DetPedido.Importe + DetPedido.Iva).
        END.
        ASSIGN 
            ttRel.pedidos = ttRel.pedidos + (IF ttRel.pedidos <> '' THEN ',' ELSE '') + (Pedido.Id-Pedido + '-' + STRING(Pedido.Resto,"9")).                                
    END. /*FOR EACH */   
  
END PROCEDURE.  
