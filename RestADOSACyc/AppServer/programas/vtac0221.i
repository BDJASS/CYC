/*
  Empresa : ADOSA
  Programa: vtac0221.i
  Funcion : Genera ranglon de surtido de RACKS
  Autor   : ALEX
  Fecha   : 8 de Septiembre del 2001
*/

FIND ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
               AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK NO-ERROR.
FIND FIRST DetTarea WHERE DetTarea.Id-Pedido = DetPedido.Id-Pedido
                      AND DetTarea.Resto = DetPedido.Resto
                      AND DetTarea.Reng = DetPedido.Reng NO-LOCK NO-ERROR.
IF AVAILABLE DetTarea THEN
  ASSIGN l-CantRacks = DetPedido.CantPed * ArtPres.Equiv /* umi */
         l-CantPed   = ''.
ELSE
  ASSIGN l-CantRacks = 0.

FOR EACH DetTarea WHERE DetTarea.Id-Pedido = DetPedido.Id-Pedido
                    AND DetTarea.Resto = DetPedido.Resto
                    AND DetTarea.Reng = DetPedido.Reng
                    AND DetTarea.Original = TRUE NO-LOCK,
    FIRST b-AP WHERE b-AP.Id-Articulo = DetTarea.Id-Articulo
		 AND b-AP.Id-Pres = DetTarea.Id-Pres NO-LOCK
               BREAK BY DetTarea.Id-Pres:
  ACCUMULATE DetTarea.Cant (TOTAL BY DetTarea.Id-Pres).
  IF LAST-OF(DetTarea.Id-Pres) THEN DO:    
    DISPLAY STREAM s-Salida1
      l-Renglon
      'RACKS'                           @ l-alm
      STRING((ACCUM TOTAL BY DetTarea.Id-Pres DetTarea.Cant),'ZZZZZZZZZ')
                                        @ l-CantPed
      l-Signo WHEN DetPedido.Tipo = 1
      l-Guiones
      b-Ap.Descr                        @ l-present
      DetPedido.id-art
      l-Descr
      l-IVA
      l-ExistAlta
    WITH FRAME f-det.
    DOWN STREAM s-Salida1 WITH FRAME f-det.
  END.
  ASSIGN l-CantRacks = l-CantRacks - (DetTarea.Cant * b-AP.Equiv).
END.

IF l-CantRacks > 0 THEN DO:
  IF (l-CantRacks / IF AVAILABLE ArtPres THEN ArtPres.Equiv ELSE 1) -
     TRUNCATE((l-CantRacks / IF AVAILABLE ArtPres
			     THEN ArtPres.Equiv ELSE 1),0) <> 0 THEN DO:
    FOR EACH ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
		       AND ArtPres.Activo = TRUE
		     NO-LOCK BY ArtPres.Equiv DESCENDING:
      IF (l-CantRacks / ArtPres.Equiv) -
	 TRUNCATE((l-CantRacks / ArtPres.Equiv),0) = 0 THEN DO:
	ASSIGN l-CantRacks = l-CantRacks / ArtPres.Equiv
	       l-Present = ArtPres.Descr.
	LEAVE.
      END.
    END.
  END.
  ELSE ASSIGN l-CantRacks = l-CantRacks / IF AVAILABLE ArtPres 
					  THEN ArtPres.Equiv ELSE 1.

  IF TRUNCATE(l-CantRacks,0) = l-CantRacks
  THEN ASSIGN l-CantPed = STRING(l-CantRacks,"zzzzzzzzz").
  ELSE ASSIGN l-CantPed = STRING(l-CantRacks,">>>>9.9<<").
  IF LENGTH(l-CantPed) < 9 THEN 
    ASSIGN l-CantPed = FILL(' ',9 - LENGTH(l-CantPed)) + l-CantPed.

END.