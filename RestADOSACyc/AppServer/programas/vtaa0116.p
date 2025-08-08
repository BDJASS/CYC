/*
  Empresa  : Consultoria en Informatica Ejecutiva
  Programa : vtaa0116.p
  Funcion  : Iniciliza los cortes
  Autor    : 
  Fecha    : 
*/

DEF INPUT PARAMETER l-Recid AS RECID NO-UNDO.
DEF INPUT PARAMETER l-Consul AS LOGICAL NO-UNDO.
DEF  VAR l-Tipo-3 AS LOGICAL NO-UNDO.
DEF  VAR l-Tipo-4 AS LOGICAL NO-UNDO.
DEF  VAR l-Alta AS LOGICAL NO-UNDO.
DEF  VAR l-Cancela AS LOGICAL NO-UNDO.
DEF BUFFER b-DetPedido FOR DetPedido.
FIND Pedido WHERE RECID(Pedido) = l-Recid NO-LOCK.

ASSIGN l-tipo-3 = FALSE l-Tipo-4 = FALSE.
FOR EACH DetPedido WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido
		     AND DetPedido.Resto = Pedido.Resto
		     EXCLUSIVE-LOCK BREAK BY DetPedido.Id-Pedido
					  BY DetPedido.Reng
		     TRANSACTION:
    ASSIGN l-Tipo-3 = IF DetPedido.Tipo = 3 THEN TRUE ELSE l-Tipo-3.
	   l-Tipo-4 = IF DetPedido.Tipo = 4 THEN TRUE ELSE l-Tipo-4.
    IF DetPedido.Tipo = 3 OR DetPedido.Tipo = 4 THEN
       ASSIGN DetPedido.Corte = TRUE
	      DetPedido.TpoCorte = DetPedido.Tipo.
    FIND b-DetPedido WHERE RECID(b-DetPedido)
       = RECID(DetPedido) NO-LOCK.
    FIND NEXT b-DetPedido WHERE b-DetPedido.Id-Pedido
			      = DetPedido.Id-Pedido
			    AND b-DetPedido.Resto = DetPedido.Resto
			    USE-INDEX idx-reng
			    NO-LOCK NO-ERROR.
    IF AVAILABLE b-DetPedido THEN DO:
       IF b-DetPedido.Tipo = 3 OR b-DetPedido.Tipo = 4 THEN DO:
	  ASSIGN DetPedido.Corte = TRUE
		 DetPedido.TpoCorte = b-DetPedido.Tipo.
       END.
    END.
END.
IF l-Alta AND NOT l-Cancela
   OR (NOT l-Alta AND NOT l-Consul AND
       NOT l-Cancela AND INTEGER(SUBSTRING(Pedido.Id-Pedido,1,1)) >= 5)
THEN DO TRANSACTION:
    IF l-Tipo-3 OR l-Tipo-4 THEN
       FIND Pedido WHERE RECID(Pedido) = l-Recid EXCLUSIVE-LOCK.

    FIND FIRST AlmPrior WHERE AlmPrior.Id-UbiVta = Pedido.Id-UbiVta
                          AND AlmPrior.Tipo = 1 NO-LOCK NO-ERROR.
    IF l-Tipo-3 THEN DO:
        IF Pedido.Id-Alm <> '02B' AND Pedido.Id-Alm <> 'FUG' THEN DO:
            FIND Folio WHERE Folio.Id-Doc = "CORTES" 
                         AND Folio.Id-Alm= Pedido.Id-Alm EXCLUSIVE-LOCK.
            ASSIGN
                Pedido.Id-Corte3 = STRING(Folio.Folio,"9999999")
                Folio.Folio = Folio.Folio + 1. 
            RELEASE Folio.
        END.
        ELSE DO:
            IF Pedido.Id-Pedido BEGINS '2' THEN DO:
                FIND Folio WHERE Folio.Id-Doc = "CORTE02B"
                             AND Folio.Id-Alm= "" EXCLUSIVE-LOCK.
                ASSIGN
                    Pedido.Id-Corte3 = STRING(Folio.Folio,"9999999")
                    Folio.Folio = Folio.Folio + 1. 
                RELEASE Folio.
            END.
            ELSE DO:
                FIND Folio WHERE Folio.Id-Doc = "CORTEFUG"
                             AND Folio.Id-Alm= "" EXCLUSIVE-LOCK.
                ASSIGN
                    Pedido.Id-Corte3 = STRING(Folio.Folio,"9999999")
                    Folio.Folio = Folio.Folio + 1. 
                RELEASE Folio.
            END.
        END.
    END. 
    IF l-Tipo-4 THEN DO:
        IF Pedido.Id-Alm <> '02B' AND Pedido.Id-Alm <> 'FUG' THEN DO:
            FIND Folio WHERE Folio.Id-Doc = "CORTES" 
                         AND Folio.Id-Alm= Pedido.Id-Alm EXCLUSIVE-LOCK.
            ASSIGN
                Pedido.Id-Corte4 = STRING(Folio.Folio,"9999999")
                Folio.Folio = Folio.Folio + 1. 
            RELEASE Folio.
        END.
        ELSE DO:
            IF Pedido.Id-Pedido BEGINS '2' THEN DO:
                FIND Folio WHERE Folio.Id-Doc = "CORTE02B"
                             AND Folio.Id-Alm= "" EXCLUSIVE-LOCK.
                ASSIGN
                    Pedido.Id-Corte4 = STRING(Folio.Folio,"9999999")
                    Folio.Folio = Folio.Folio + 1. 
                RELEASE Folio.
            END.
            ELSE DO:
                FIND Folio WHERE Folio.Id-Doc = "CORTEFUG"
                             AND Folio.Id-Alm= "" EXCLUSIVE-LOCK.
                ASSIGN
                    Pedido.Id-Corte4 = STRING(Folio.Folio,"9999999")
                    Folio.Folio = Folio.Folio + 1. 
                RELEASE Folio.
            END.
        END.
    END.

END.
