/*
  Empresa : ADOSA
  Programa: bodd1071.p
  Funcion : Examina si el pedido tiene cortes
  Autor   : ALEX
  Fecha   : 20 de Mayo del 2003
*/

//{/usr2/adosa/includes/sia00000.var}
DEF INPUT PARAMETER l-Pedido LIKE Pedido.Id-Pedido NO-UNDO.
DEF INPUT PARAMETER l-Resto  LIKE Pedido.Resto     NO-UNDO.
DEF BUFFER b-AP FOR ArtPres.

FIND EstPedido WHERE EstPedido.Id-Pedido = l-Pedido
                 AND EstPedido.Id-Seq = l-Resto EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE EstPedido THEN DO:
  FOR EACH DetPedido WHERE DetPedido.Id-Pedido = l-Pedido
                       AND DetPedido.Resto = l-Resto
                       AND DetPedido.Tipo >= 3
                       AND DetPedido.Tipo <= 4 NO-LOCK:
    IF EstPedido.Cortes = '' THEN
      ASSIGN EstPedido.Cortes = STRING(DetPedido.Tipo,'9').
    ELSE
      IF NOT EstPedido.Cortes MATCHES '*3*' AND DetPedido.Tipo = 3 THEN
        ASSIGN EstPedido.Cortes = EstPedido.Cortes + ',' +
                                  STRING(DetPedido.Tipo,'9').
      ELSE
        IF NOT EstPedido.Cortes MATCHES '*4*' AND DetPedido.Tipo = 4 THEN
          ASSIGN EstPedido.Cortes = EstPedido.Cortes + ',' +
                                    STRING(DetPedido.Tipo,'9').
    IF EstPedido.Cortes MATCHES '*3*' AND EstPedido.Cortes MATCHES '*4*' THEN
      LEAVE.
  END.
END.

RELEASE EstPedido.
