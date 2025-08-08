@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : pedaut.p
    Purpose     : Programa que regresa la cantidad de pedidos autorizados, cancelados y 
                   rechazados por empleado en el mes actual
    Syntax      :

    Description : /PedidoAutCanRec      [Modulo Pedidos Carlos] 

    Author(s)   : Alex
    Created     : Tue Nov 19 14:07:29 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE l-FecIni AS DATE NO-UNDO. 
DEFINE VARIABLE l-FecFin AS DATE NO-UNDO.

DEFINE TEMP-TABLE ttPedAut
    FIELD Totales AS INTEGER
    FIELD Autorizados AS INTEGER 
    FIELD Rechazados AS INTEGER 
    FIELD Cancelados AS INTEGER.
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetPedidos:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   
DEFINE INPUT PARAMETER ipClaseCte LIKE Cliente.Id-ClaseCte NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttPedAut.
/*
ASSIGN l-FecIni = DATE(STRING(MONTH(TODAY)) + "/01/" + STRING(YEAR(TODAY))).
IF MONTH(TODAY) = 12 THEN 
    ASSIGN l-FecFin = DATE("12/31/" + STRING(YEAR(TODAY))).
ELSE 
    ASSIGN l-FecFin = DATE(MONTH(TODAY) + 1, 1, YEAR(TODAY)) - 1.
 */
         
FOR EACH Pedido WHERE Pedido.AutPor <> ""
                  AND MONTH(Pedido.FecReg)= MONTH(TODAY)
                  AND YEAR(Pedido.FecReg)= YEAR(TODAY) 
                  AND NOT Pedido.Filler-1 BEGINS "*** GRAFICO" USE-INDEX Idx-FecReg NO-LOCK,
   FIRST Cliente OF PEdido WHERE Cliente.Id-ClaseCte = ipClaseCte NO-LOCK:
    
    ACCUMULATE 1 (COUNT).
    
END.

FOR EACH CancPed WHERE MONTH(CancPed.FecCancel)= MONTH(TODAY)
                   AND YEAR(CancPed.FecCancel)= YEAR(TODAY)
                   AND NOT CancPed.Filler-1 BEGINS "*** GRAFICO" 
                   USE-INDEX Idx-FecReg NO-LOCK,
    FIRST Cliente OF CancPed WHERE Cliente.Id-ClaseCte = ipClaseCte NO-LOCK:
    IF CancPed.Id-Pedido BEGINS "0" THEN 
        ACCUMULATE 2 (COUNT).
    ELSE 
        ACCUMULATE 3 (COUNT).
END.

CREATE ttPedAut.
ASSIGN ttPedAut.Autorizados = (ACCUM COUNT 1)
       ttPedAut.Rechazados  = (ACCUM COUNT 2)
       ttPedAut.Cancelados  = (ACCUM COUNT 3)
       ttPedAut.Totales = ttPedAut.Autorizados + ttPedAut.Rechazados + ttPedAut.Cancelados.
       
RETURN.

END PROCEDURE.

