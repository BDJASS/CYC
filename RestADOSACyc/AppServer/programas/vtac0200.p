/*
  Empresa  : Consultoria en Informatica Ejecutiva
  Programa : vtac0200.p
  Funcion  : Imprime Pedido de Mostrador
  Autor    : FLC
  Fecha    : 22-NOV-96
  Anotaciones:
   Mandar llamar a el programa vtac0200.p como sigue:

    vtac0200.p(INPUT <Pedido>, INPUT        <Resto>,
               INPUT FALSE,    INPUT-OUTPUT <Variable-Logica-Tmp>, FALSE).
*/

//{/usr2/adosa/includes/sia00000.var}

DEFINE INPUT        PARAMETER p-Pedido             LIKE Pedido.Id-Pedido NO-UNDO.
DEFINE INPUT        PARAMETER p-Resto              LIKE Pedido.Resto     NO-UNDO.
DEFINE INPUT        PARAMETER p-ImprimeElCompleto  AS   LOGICAL          NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-SiLoImprimi        AS   LOGICAL          NO-UNDO.
DEFINE INPUT        PARAMETER p-PedirImpresora     AS   LOGICAL          NO-UNDO.
DEFINE BUFFER b-AP FOR ArtPres.  


FIND Pedido WHERE Pedido.Id-Pedido = p-Pedido
              AND Pedido.Resto = p-Resto NO-LOCK NO-ERROR.

IF Pedido.Id-Alm = "02B" THEN               
    FIND CteAG WHERE CteAG.Id-Cliente = Pedido.Id-Cliente NO-LOCK NO-ERROR.

IF NOT AVAILABLE CteAG THEN DO:
    RUN programas/vtac0220.p(INPUT        p-Pedido,
                                     INPUT        p-Resto,
                                     INPUT        p-ImprimeElCompleto,
                                     INPUT-OUTPUT p-SiLoImprimi,
                                     INPUT        "",
                                     INPUT        p-PedirImpresora).
END.
ELSE DO: /* Clientes de Artes Greficas */
    FOR EACH DetPedido WHERE DetPedido.Id-Pedido = p-Pedido
                         AND DetPedido.Resto = p-Resto
                         AND DetPedido.Id-Articulo <> ''                                 
                         AND DetPedido.Tipo = 1
                         AND DetPedido.TpoCorte = 0 NO-LOCK,
        FIRST ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
                        AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK:
            FIND FIRST DetTarea WHERE DetTarea.Id-Pedido = DetPedido.Id-Pedido
                                  AND DetTarea.Resto = DetPedido.Resto
                                  AND DetTarea.Id-Articulo = DetPedido.Id-Articulo
                                  AND DetTarea.Id-Color = DetPedido.Id-color NO-LOCK NO-ERROR.
            IF NOT AVAILABLE DetTarea THEN DO TRANSACTION:
                RUN /usr2/adosa/procs/vtac0116.p(INPUT p-Pedido,
                                                 INPUT p-Resto,
                                                 INPUT TRUE,
                                                 INPUT "CEL20").  
                LEAVE.
            END.
            ELSE DO TRANSACTION:
                FOR EACH DetTarea WHERE DetTarea.Id-Pedido = DetPedido.Id-Pedido
                                    AND DetTarea.Resto = DetPedido.Resto
                                    AND DetTarea.Id-Articulo = DetPedido.Id-Articulo
                                    AND DetTarea.Id-Color = DetPedido.Id-Color NO-LOCK,
                        FIRST B-AP WHERE b-AP.Id-Articulo = DetTarea.Id-Articulo
                                     AND b-AP.Id-Pres = DetTarea.Id-Pres NO-LOCK:
                        ACCUMULATE (DetTarea.Cant * b-AP.Equiv) (TOTAL).
                END.
                IF (ACCUM TOTAL (DetTarea.Cant * b-AP.Equiv)) <> DetPedido.CantPed * ArtPres.Equiv THEN DO:
                    RUN /usr2/adosa/procs/vtac0116.p(INPUT p-Pedido,
                                                     INPUT p-Resto,
                                                     INPUT TRUE,
                                                     INPUT "CEL20").
                    LEAVE.
                END.
           END.
    END.      
END.