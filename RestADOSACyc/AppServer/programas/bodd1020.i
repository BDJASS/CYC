/*
  Empresa : ADOSA
  Programa: bodd1020.i
  Funcion : Genera detalles de tareas.
  Autor   : ALEX
  Fecha   : 22 de Septiembre del 2001
*/

/*
IF (DetPedido.CantPed * ArtPres.Equiv) > (DetLoc3.Maximo * .4) THEN DO: /* Si se pide mas del 60% de Picking */
*/
  ASSIGN l-Completo = FALSE.
  
  /*
  FOR EACH Existencia WHERE Existencia.Id-Articulo = DetPedido.Id-Articulo
                        AND Existencia.Id-Color = DetPedido.Id-Color
                        AND Existencia.Id-Alm = DetPedido.Id-Alm
                        AND (IF g-Origen <> "03A" THEN NOT Existencia.Id-Loc MATCHES "*RECIBO*" ELSE TRUE) USE-INDEX Idx-Art EXCLUSIVE-LOCK,
      FIRST b-AP WHERE b-AP.Id-Articulo = Existencia.Id-Articulo
                   AND b-AP.Id-Pres = Existencia.Id-Pres NO-LOCK BY b-AP.Equiv DESCENDING:

    IF TRUNCATE(((DetPedido.CantPed  * ArtPres.Equiv) / b-AP.Equiv),0) = 0 THEN NEXT.

    IF (Existencia.Disponible * b-AP.Equiv) >= (DetPedido.CantPed * ArtPres.Equiv) THEN DO:
      ASSIGN l-Completo = TRUE.
      CREATE DetTarea.
      ASSIGN DetTarea.Id-Tarea     = l-FolTar
             DetTarea.Id-Pedido    = DetPedido.Id-Pedido
             DetTarea.Resto        = DetPedido.Resto
             DetTarea.Reng         = DetPedido.Reng
                 DetTarea.Id-Articulo  = DetPedido.Id-Articulo
                 DetTarea.Id-Color     = DetPedido.Id-Color
                 DetTarea.Id-Loc       = Existencia.Id-Loc
                 DetTarea.Secuencia    = Existencia.Secuencia
                 DetTarea.Id-Pres      = Existencia.Id-Pres
                 DetTarea.Cant         = TRUNCATE(((DetPedido.CantPed *
                                                    ArtPres.Equiv) / b-AP.Equiv),0)
             Existencia.FecMod     = TODAY
             Existencia.HorMod     = TIME
             Existencia.Disponible = Existencia.Disponible - DetTarea.Cant
             Existencia.Reservada  = Existencia.Reservada + DetTarea.Cant.
      LEAVE.
    END. /* disp > cantidadumi */
  END. /* for each existencia */
  */
  
  IF l-Completo = FALSE THEN DO:
    ASSIGN l-CantUMI = DetPedido.CantPed * ArtPres.Equiv
           l-Cant1   = 0.
    FOR EACH Existencia WHERE Existencia.Id-Articulo = DetPedido.Id-Articulo
                          AND Existencia.Id-Color = DetPedido.Id-Color
                          AND Existencia.Id-Alm = DetPedido.Id-Alm
                          AND Existencia.Disponible > 0
                          AND (IF g-Origen <> "03A" THEN NOT Existencia.Id-Loc MATCHES "*RECIBO*" ELSE TRUE) USE-INDEX Idx-Art EXCLUSIVE-LOCK,
        FIRST b-AP WHERE b-AP.Id-Articulo = Existencia.Id-Articulo
                     AND b-AP.Id-Pres = Existencia.Id-Pres NO-LOCK BY (b-AP.Equiv * existencia.disponible):
      IF TRUNCATE((l-CantUMI / b-AP.Equiv),0) = 0 THEN NEXT.
      CREATE DetTarea.
      ASSIGN DetTarea.Id-Tarea     = l-FolTar
             DetTarea.Id-Pedido    = DetPedido.Id-Pedido
             DetTarea.Resto        = DetPedido.Resto
             DetTarea.Reng         = DetPedido.Reng
             DetTarea.Id-Articulo  = DetPedido.Id-Articulo
             DetTarea.Id-Color     = DetPedido.Id-Color
             DetTarea.Id-Loc       = Existencia.Id-Loc
             DetTarea.Id-Pres      = Existencia.Id-Pres
             DetTarea.Secuencia    = Existencia.Secuencia
             l-Cant1               = IF Existencia.Disponible < (l-CantUMI / b-AP.Equiv)
                                     THEN Existencia.Disponible
                                     ELSE TRUNCATE((l-CantUMI / b-AP.Equiv),0)
             DetTarea.Cant         = l-Cant1
             Existencia.Reservada  = Existencia.Reservada + l-Cant1
             Existencia.Disponible = Existencia.Disponible - l-Cant1
             Existencia.FecMod     = TODAY
             Existencia.HorMod     = TIME
             l-CantUMI             = l-CantUMI - (l-Cant1 * b-AP.Equiv).
    END.
  END. /* si no se completo con una sola existencia */
/*
END.
*/
