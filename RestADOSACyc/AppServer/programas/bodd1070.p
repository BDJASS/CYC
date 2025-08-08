/*
  Empresa : ADOSA
  Programa: bodd1070.p
  Funcion : Examina las areas de surtido
  Autor   : ALEX
  Fecha   : 1 de Diciembre del 2001
*/

//{/usr2/adosa/includes/sia00000.var}
DEF INPUT PARAMETER l-Pedido LIKE Pedido.Id-Pedido NO-UNDO.
DEF INPUT PARAMETER l-Resto  LIKE Pedido.Resto     NO-UNDO.
DEF BUFFER b-AP FOR ArtPres.   

FIND EstPedido WHERE EstPedido.Id-Pedido = l-Pedido
		 AND EstPedido.Id-Seq = l-Resto EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE EstPedido THEN ASSIGN EstPedido.Areas = ''
                                   EstPedido.Dobleimpr = FALSE.

FOR EACH DetPedido WHERE DetPedido.Id-Pedido = l-Pedido
                     AND DetPedido.Resto = l-Resto
                     AND DetPedido.Id-Articulo <> ''
                     AND DetPedido.Id-Loc BEGINS '2C'
                     AND (SUBSTRING(DetPedido.Id-Loc,1,4) < '2C16' OR SUBSTRING(DetPedido.Id-Loc,1,4) > '2C26')
                     AND DetPedido.Tipo = 1
                     AND DetPedido.TpoCorte = 0 NO-LOCK,
    FIRST ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
                    AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK:
  FIND FIRST DetTarea WHERE DetTarea.Id-Pedido = DetPedido.Id-Pedido
                        AND DetTarea.Resto = DetPedido.Resto
                        AND DetTarea.Id-Articulo = DetPedido.Id-Articulo
                        AND DetTarea.Id-Color = DetPedido.Id-Color
                      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE DetTarea THEN DO TRANSACTION: 
    FIND EstPedido WHERE EstPedido.Id-Pedido = DetPedido.Id-Pedido
                     AND EstPedido.Id-Seq = DetPedido.Resto
                   EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE EstPedido THEN ASSIGN EstPedido.Areas = 'C'.
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
    IF (ACCUM TOTAL (DetTarea.Cant * b-AP.Equiv)) <>
       DetPedido.CantPed * ArtPres.Equiv THEN DO:
      ASSIGN EstPedido.Areas = 'C'.
      LEAVE.
    END.
  END.
END.
/*AREA C-ALTA*/
FOR EACH DetPedido WHERE DetPedido.Id-Pedido = l-Pedido
                     AND DetPedido.Resto = l-Resto
                     AND DetPedido.Id-Articulo <> ''
                     AND DetPedido.Id-Loc BEGINS '2C'
                     AND SUBSTRING(DetPedido.Id-Loc,1,4) >= '2C16'
                     AND SUBSTRING(DetPedido.Id-Loc,1,4) <= '2C26'
                     AND DetPedido.Tipo = 1
                     AND DetPedido.TpoCorte = 0 NO-LOCK,
    FIRST ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
                    AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK:
  FIND FIRST DetTarea WHERE DetTarea.Id-Pedido = DetPedido.Id-Pedido
                        AND DetTarea.Resto = DetPedido.Resto
                        AND DetTarea.Id-Articulo = DetPedido.Id-Articulo
                        AND DetTarea.Id-Color = DetPedido.Id-Color
                      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE DetTarea THEN DO TRANSACTION: 
    FIND EstPedido WHERE EstPedido.Id-Pedido = DetPedido.Id-Pedido
                     AND EstPedido.Id-Seq = DetPedido.Resto
                   EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE EstPedido THEN ASSIGN EstPedido.Dobleimpr = (IF EstPedido.Areas MATCHES '*C*' THEN TRUE ELSE FALSE) 
                                       EstPedido.Areas = EstPedido.Areas + (IF EstPedido.Areas = "" THEN "" ELSE ",") + 'P'.
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
    IF (ACCUM TOTAL (DetTarea.Cant * b-AP.Equiv)) <>
       DetPedido.CantPed * ArtPres.Equiv THEN DO:
      ASSIGN EstPedido.Dobleimpr = (IF EstPedido.Areas MATCHES '*C*' THEN TRUE ELSE FALSE) 
             EstPedido.Areas     = EstPedido.Areas + (IF EstPedido.Areas = "" THEN "" ELSE ",") + 'P'.
      LEAVE.
    END.
  END.
END.

FOR EACH DetPedido WHERE DetPedido.Id-Pedido = l-Pedido
                     AND DetPedido.Resto = l-Resto
                     AND DetPedido.Id-Articulo <> ''
                     AND (DetPedido.Id-Loc BEGINS '2D' OR
                          DetPedido.Id-Loc BEGINS 'S' OR
                          DetPedido.Id-Loc BEGINS '5' OR
                          DetPedido.Id-Loc = '')
                     AND DetPedido.Tipo = 1
                     AND DetPedido.TpoCorte = 0 NO-LOCK,
    FIRST ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
                    AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK:
  FIND FIRST DetTarea WHERE DetTarea.Id-Pedido = DetPedido.Id-Pedido
                        AND DetTarea.Resto = DetPedido.Resto
                        AND DetTarea.Id-Articulo = DetPedido.Id-Articulo
                        AND DetTarea.Id-Color = DetPedido.Id-Color
                      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE DetTarea THEN DO TRANSACTION:
    FIND EstPedido WHERE EstPedido.Id-Pedido = DetPedido.Id-Pedido
                     AND EstPedido.Id-Seq = DetPedido.Resto
                   EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE EstPedido THEN
      ASSIGN EstPedido.Areas = EstPedido.Areas + (IF EstPedido.Areas = ''
                                                  THEN 'D' ELSE ',D').
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
    IF (ACCUM TOTAL (DetTarea.Cant * b-AP.Equiv)) <>
       DetPedido.CantPed * ArtPres.Equiv THEN DO:
      ASSIGN EstPedido.Areas = EstPedido.Areas + (IF EstPedido.Areas = ''
                                                  THEN 'D' ELSE ',D').
      LEAVE.
    END.
  END.
END.
RELEASE EstPedido.
