/*
  Empresa : ADOSA
  Programa: bodd1020.p
  Funcion : Genera tareas de pedidos
  Autor   : ALEX
  Fecha   : 20 de Septiembre del 2001
*/

//{/usr2/adosa/includes/sia00000.var}
DEFINE VAR l-FolTar AS CHAR NO-UNDO.
DEFINE VAR l-FolTar2 AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER l-Pedido LIKE Pedido.Id-PEdido NO-UNDO.
DEFINE INPUT PARAMETER l-Resto  LIKE Pedido.Resto     NO-UNDO.
DEFINE INPUT  PARAMETER p-User    AS CHARACTER NO-UNDO.

DEFINE VAR l-PedPres   LIKE ArtPres.Equiv NO-UNDO.
DEFINE VAR l-ExistPres LIKE ArtPres.Equiv NO-UNDO.
DEFINE VAR l-GenTar    AS LOGI NO-UNDO.
DEFINE VAR l-Completo  AS LOGI NO-UNDO.
DEFINE VAR l-Destinos  AS CHAR NO-UNDO.
DEFINE VAR l-CantUMI   AS DECI NO-UNDO.
DEFINE VAR l-Cant1     AS DECI NO-UNDO.

DEFINE BUFFER b-AP FOR ArtPres.
DEFINE BUFFER b-Tarea FOR Tarea.

  
DEF VAR g-Origen AS CHAR NO-UNDO.

FIND FIRST Usuario WHERE Usuario.Id-User = p-User NO-LOCK NO-ERROR.
IF AVAILABLE Usuario THEN g-Origen = Usuario.id-ubicacion.  

FIND FIRST Almacen WHERE Almacen.Id-Alm = '02B' NO-LOCK NO-ERROR.

RELEASE CteAG.
FIND Pedido WHERE Pedido.Id-Pedido = l-Pedido
              AND Pedido.Resto = l-Resto NO-LOCK NO-ERROR.
IF AVAILABLE Pedido AND Pedido.Id-Alm = "02B" THEN DO:
    FIND FIRST CteAG WHERE CteAG.Id-Cliente = Pedido.Id-Cliente NO-LOCK NO-ERROR.
END.

FOR EACH DetPedido WHERE DetPedido.Id-Pedido = l-Pedido
                     AND DetPedido.Resto = l-Resto
                     AND DetPedido.Tipo = 1 
                     AND DetPedido.TpoCorte = 0
                     AND DetPedido.Id-Articulo <> '' NO-LOCK,
    /*
    EACH DetLoc3 WHERE DetLoc3.Id-Articulo = DetPedido.Id-Articulo
                    AND DetLoc3.Id-Color = DetPedido.Id-Color NO-LOCK,
    FIRST Localizacion WHERE Localizacion.Clave = DetLoc3.Localizacion
                         AND Localizacion.Almacen = Almacen.Clave
                         AND NOT Localizacion.Codigo MATCHES "2B0*CONT"
                       NO-LOCK,
    */
    FIRST ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
                    AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK:
    /*
    IF (DetPedido.CantPed * ArtPres.Equiv) > (DetLoc3.Maximo * .4) THEN DO:
    */
      ASSIGN l-GenTar = TRUE
             l-Destinos = (IF l-Destinos = '' THEN '' ELSE ',') +
                          (IF DetPedido.TpoCorte = 0 THEN 'E' ELSE 'C').

    /*
    END.
    */
END.

IF l-GenTar = TRUE THEN DO:
  IF l-Destinos MATCHES '*E*' THEN DO: /* destino de empaque */
    FIND Folio WHERE Folio.Id-Doc = 'Tarea'
                 AND Folio.Id-Alm = '02B' EXCLUSIVE-LOCK NO-ERROR.

    IF Folio.Folio >= 999999 THEN 
        ASSIGN 
            Folio.Folio = 0.

    ASSIGN l-FolTar = Folio.Prefijo + STRING(Folio.Folio,'999999')
           Folio.Folio = Folio.Folio + 1.
    RELEASE Folio.

    FIND Pedido WHERE Pedido.Id-Pedido = l-pedido AND
		      Pedido.Resto     = l-resto NO-LOCK NO-ERROR.
    CREATE Tarea.
    ASSIGN 
       Tarea.Id-Tarea  = l-FolTar
	   Tarea.Area      = 'E'
       Tarea.Id-Alm    = '02B'
	   Tarea.Estatus   = 0
	   Tarea.FecReg    = TODAY
	   Tarea.HReg      = TIME
	   Tarea.Pasillo   = ''
	   Tarea.Prioridad = (IF AVAILABLE Pedido THEN
                                (IF Pedido.Id-Entrega = 16 THEN 0
                                 ELSE IF Pedido.Id-UbiVta = 'MLA' THEN 1
                                      ELSE 2)
                               ELSE 2)
	   Tarea.Refer     = l-Pedido
	   Tarea.Resto     = l-Resto
	   Tarea.Tipo      = 2
       Tarea.Id-TD     = IF AVAILABLE CteAG THEN 6 ELSE 1 /* para surtido de pedidos 6=ARTES GRAFICAS 1=PEDIDOS NORMALES */.

    FOR EACH DetPedido WHERE DetPedido.Id-Pedido = l-Pedido
                         AND DetPedido.Resto = l-Resto
			             AND DetPedido.Tipo = 1
                         AND DetPedido.TpoCorte = 0
			             AND DetPedido.Id-Articulo <> '' NO-LOCK,
	    /*
        EACH DetLoc3 WHERE DetLoc3.Id-Articulo = DetPedido.Id-Articulo
		  	           AND DetLoc3.Id-Color = DetPedido.Id-Color NO-LOCK,
        FIRST Localizacion WHERE Localizacion.Clave = DetLoc3.Localizacion
			                 AND Localizacion.Almacen = Almacen.Clave
			                 AND NOT Localizacion.Codigo MATCHES "2B0*CONT"
                           NO-LOCK,
        */
        FIRST ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
                        AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK:
        {bodd1020.i}
    END.
  END. /* destino de empaque */
  
  /*
  IF l-Destinos MATCHES '*C*' THEN DO: /* destino de cortes */
    FIND Folio WHERE Folio.Id-Doc = 'Tarea'
                 AND Folio.Id-Alm = '02B' EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN l-FolTar = Folio.Prefijo + STRING(Folio.Folio,'999999')
    Folio.Folio = Folio.Folio + 1.
    RELEASE Folio.

    CREATE Tarea.
    ASSIGN Tarea.Id-Tarea  = l-FolTar
	   Tarea.Area      = 'C'
           Tarea.Id-Alm    = '02B'
	   Tarea.Estatus   = 0
	   Tarea.FecReg    = TODAY
	   Tarea.HReg      = TIME
	   Tarea.Pasillo   = ''
	   Tarea.Prioridad = (IF AVAILABLE Pedido AND Pedido.Id-Entrega = 16 
                              THEN 1 ELSE 2)
	   Tarea.Refer     = l-Pedido
	   Tarea.Resto     = l-Resto
	   Tarea.Tipo      = 2.
    FOR EACH DetPedido WHERE DetPedido.Id-Pedido = l-Pedido
			 AND DetPedido.Resto = l-Resto
			 AND DetPedido.Tipo = 3
			 AND DetPedido.Id-Articulo <> '' NO-LOCK,
	FIRST Almacen WHERE Almacen.Id-Alm = DetPedido.Id-Alm NO-LOCK,
	FIRST DetLoc3 WHERE DetLoc3.Id-Articulo = DetPedido.Id-Articulo
			AND DetLoc3.Id-Color = DetPedido.Id-Color NO-LOCK,
	FIRST Localizacion WHERE Localizacion.Clave = DetLoc3.Localizacion
			     AND Localizacion.Almacen = Almacen.Clave NO-LOCK,
	FIRST ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
			AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK:
      {/usr2/adosa/includes/bodd1020.i}
    END.
  END. /* destino de cortes */
  */

  /*
  {/usr2/adosa/includes/bodd1021.i}
  */

  FIND FIRST DetTarea WHERE DetTarea.Id-Tarea = l-FolTar NO-LOCK NO-ERROR.
  IF NOT AVAILABLE DetTarea THEN DO:
    FIND FIRST Tarea WHERE Tarea.Id-Tarea = l-FolTar EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Tarea THEN DELETE Tarea.
  END.  

END. /* l-gentar */
