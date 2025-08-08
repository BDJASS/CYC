/*
  Empresa : ADOSA
  Programa: vtac0220.i
  Funcion : Impresion del Pedido
  Autor   : ALEX
  Fecha   : 2 de Noviembre del 2000
*/

IF (Pedido.Enviar = TRUE AND AlmPrior.Id-Alm <> '11') OR Pedido.Id-Alm = "02B"
THEN DO:
  IF DetPedido.TpoCorte = 3 OR DetPedido.TpoCorte = 4 THEN NEXT.
  IF DetPedido.Id-Loc = '' THEN NEXT.
  IF NOT DetPedido.Id-Loc BEGINS '2C' AND NOT DetPedido.Id-Loc BEGINS '2B'
  THEN NEXT.
  IF DetPedido.Tipo = 5 THEN NEXT.
END.

FIND Articulo OF DetPedido NO-LOCK NO-ERROR.
FIND Kolor WHERE kolor.id-color = DetPedido.id-color NO-LOCK NO-ERROR.
FIND MovPedido WHERE MovPedido.Id-Pedido = DetPedido.Id-Pedido
		 AND MovPedido.Id-Articulo = DetPedido.Id-Articulo
		 AND MovPedido.Id-Color = DetPedido.Id-Color
		 AND MovPedido.Id-Pres = DetPedido.Id-Pres
		 AND MovPedido.Id-Seq = DetPedido.Id-Seq
	       NO-LOCK NO-ERROR.
ASSIGN l-nombre = IF AVAILABLE kolor THEN Kolor.Descr ELSE ''
       l-Renglon = l-Renglon + 1
       l-IVA     = (IF DetPedido.PorcIVA = 0 THEN 'X' ELSE ' ').
ASSIGN l-Descr = STRING(DetPedido.Descr,"x(59)") + " " + l-Nombre.
/**/
FIND ArtPres WHERE ArtPres.id-pres = DetPedido.id-pres
	       AND ArtPres.id-art = DetPedido.id-art NO-LOCK NO-ERROR.
IF AVAILABLE ArtPres THEN DO:
    ASSIGN l-present = ArtPres.Descr
           l-ExistAlta = DetPedido.ExistAlta / ArtPres.Equiv.
END.
ELSE DO:
    ASSIGN l-present = ''
           l-ExistAlta = 0.
END.

/**/
ASSIGN l-Signo = ''.
IF DetPedido.CantPed * (IF AVAILABLE ArtPres THEN ArtPres.Equiv ELSE 1) <= DetPedido.ExistAlta THEN DO:
    FIND ArtUbic WHERE ArtUbic.Id-Articulo = DetPedido.Id-Articulo
		   AND ArtUbic.Id-Color = DetPedido.Id-Color
		   AND ArtUbic.Id-Alm = DetPedido.Id-Alm NO-LOCK NO-ERROR.
    IF AVAILABLE ArtUbic THEN DO:
      IF ArtUbic.Exist >= 0 THEN
          ASSIGN l-Signo = '+'.
      ELSE DO:
          FIND ArtUbic WHERE ArtUbic.Id-Articulo = DetPedido.Id-Articulo
                 AND ArtUbic.Id-Color = DetPedido.Id-Color
                 AND ArtUbic.Id-Alm = '02B' NO-LOCK NO-ERROR.
          IF AVAILABLE ArtUbic THEN DO:
            IF ArtUbic.Exist >= 0 THEN
                ASSIGN l-Signo = 'B'.
            ELSE
                ASSIGN l-Signo = 'b'.
          END.
      END.
    END.
END.
ELSE DO:
  IF DetPedido.ExistAlta > 0 THEN DO:
    FIND ArtUbic WHERE ArtUbic.Id-Articulo = DetPedido.Id-Articulo
		   AND ArtUbic.Id-Color = DetPedido.Id-Color
		   AND ArtUbic.Id-Alm = 'FUG' NO-LOCK NO-ERROR.
    IF AVAILABLE ArtUbic THEN DO:
      IF ArtUbic.Exist > DetPedido.CantCom THEN ASSIGN l-Signo = 'F'.
      ELSE '*'.
    END.
    ELSE ASSIGN l-Signo = '-'.
  END.
  ELSE DO:
    FIND ArtUbic WHERE ArtUbic.Id-Articulo = DetPedido.Id-Articulo
		   AND ArtUbic.Id-Color = DetPedido.Id-Color
		   AND ArtUbic.Id-Alm = 'FUG' NO-LOCK NO-ERROR.
    IF AVAILABLE ArtUbic THEN DO:
      IF ArtUbic.Exist > DetPedido.CantCom THEN ASSIGN l-Signo = 'F'.
      ELSE 'f'.
    END.
    ELSE DO:
        ASSIGN l-Signo = ''.
        FOR EACH DetRecibo WHERE DetRecibo.Id-Articulo = DetPedido.Id-Articulo
                             AND DetRecibo.Id-Color = DetPedido.Id-Color NO-LOCK,
            FIRST Tarima WHERE Tarima.Id-Tarima = DetRecibo.Id-Tarima
                           AND Tarima.Estatus < 3 NO-LOCK,
            FIRST Recibo WHERE Recibo.Id-Recibo = DetRecibo.Id-Recibo
                           AND Recibo.Especial = FALSE NO-LOCK:
            ACCUMULATE DetRecibo.CantUmi (TOTAL).
        END.
        IF (ACCUM TOTAL DetRecibo.cantUMI) >=
           (DetPedido.CantPed * (IF AVAILABLE ArtPres THEN ArtPres.Equiv ELSE 1)) THEN
               ASSIGN l-Signo = 'T'.
    END.
  END.
END.
/**/

IF TRUNCATE(DetPedido.CantPed,0) = DetPedido.CantPed
THEN ASSIGN l-CantPed = STRING(DetPedido.CantPed,"zzzzzzzzz").
ELSE ASSIGN l-CantPed = STRING(DetPedido.CantPed,">>>>9.9<<").
IF LENGTH(l-CantPed) < 9 THEN 
  ASSIGN l-CantPed = FILL(' ',9 - LENGTH(l-CantPed)) + l-CantPed.
/**/

FIND Almacen WHERE almacen.Id-Alm = DetPedido.Id-Alm NO-LOCK NO-ERROR.
IF DetPedido.Id-Alm = "02B" OR DetPedido.Id-Alm = "11" OR (AVAILABLE Almacen AND almacen.TipoCedis)
   OR Pedido.Enviar = TRUE THEN DO:
  IF DetPedido.Id-Loc <> ''
  THEN ASSIGN l-alm = DetPedido.Id-Loc.
  ELSE DO:
      ASSIGN l-Alm = ''.
      FOR EACH DetLoc3 WHERE DetLoc3.Id-Articulo = DetPedido.Id-Articulo
		                 AND DetLoc3.Id-Color = DetPedido.Id-Color NO-LOCK,
          FIRST Localizacion WHERE Localizacion.Clave = DetLoc3.Localizacion NO-LOCK,
          FIRST Almacen WHERE Almacen.Clave = Localizacion.Almacen
                          AND Almacen.Id-Alm = g-origen NO-LOCK:
          ASSIGN l-Alm = Localizacion.Codigo.
          LEAVE.
      END.
  END.
END.
ELSE ASSIGN l-alm = DetPedido.Id-Alm.

ASSIGN l-CantRacks = 0.
IF (Pedido.Enviar = TRUE AND AlmPrior.Id-Alm <> '11') OR Pedido.Id-Alm = "02B"
THEN DO:
  {vtac0221.i}
END.

FIND b-DetPedido WHERE RECID(b-detPedido) = RECID(DetPedido) EXCLUSIVE-LOCK NO-ERROR.
ASSIGN b-DetPedido.RengImp = l-Renglon.
RELEASE b-DetPedido.

IF l-CantPed <> '' OR Pedido.Enviar = FALSE THEN DO:
  FOR EACH Tarea WHERE Tarea.Refer = Pedido.Id-Pedido NO-LOCK:
    FIND FIRST DetTarea WHERE DetTarea.Id-Tarea = Tarea.Id-Tarea AND
                              DetTarea.Id-Articulo = DetPedido.Id-Art AND
                              DetTarea.Id-Color = DetPedido.Id-Color AND 
                              DetTarea.Id-Loc <> '' NO-LOCK NO-ERROR. 
    IF AVAILABLE DetTarea THEN LEAVE.
  END.
  DISPLAY STREAM s-Salida1
    l-Renglon
    (l-Alm + (IF AVAILABLE DetTarea OR DetPedido.Tipo <> 1 OR DetPedido.Id-Alm = '02B' THEN '' ELSE 'TDA')) @ l-Alm
    l-CantPed
    l-Signo WHEN DetPedido.Tipo = 1
    l-Guiones
    l-present
    DetPedido.id-art
    l-Descr
    l-IVA
    l-Existalta
  WITH FRAME f-det.
  DOWN STREAM s-Salida1 WITH FRAME f-det.
END.

IF Pedido.Id-Cliente = 3 THEN HIDE STREAM s-Salida1 FRAME f-enc3-1.
			 ELSE HIDE STREAM s-Salida1 FRAME f-enc3-2.
