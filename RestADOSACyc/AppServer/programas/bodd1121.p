/*
    Empresa:      ADOSA
    Programa:     bodd1120.p
    Funcion:      Generador de tareas desde pedidos de telemarketing SUCURSALES (Articulos con corte)
    Autor:        ALEX
    Fecha:        4 de Septiembre del 2015
    
    Modificacion: 
*/

DEFINE INPUT PARAMETER l-Pedido LIKE Pedido.id-Pedido NO-UNDO.
DEFINE INPUT PARAMETER l-Resto LIKE Pedido.Resto NO-UNDO.
DEFINE INPUT  PARAMETER p-User    AS CHARACTER NO-UNDO. 

DEFINE VARIABLE l-Art LIKE ArtBarra.CodBarras NO-UNDO.
DEFINE VARIABLE l-Col LIKE ArtBarra.Id-Color NO-UNDO.
DEFINE VARIABLE l-FolTar LIKE Tarea.Id-Tarea NO-UNDO.
DEFINE VARIABLE l-FolTar2 LIKE Tarea.Id-Tarea NO-UNDO.

DEFINE VARIABLE l-folio AS CHARACTER NO-UNDO.

DEFINE VARIABLE l-CantUMI AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Cant1 AS DECIMAL NO-UNDO.

DEFINE VARIABLE l-Cant AS INTEGER FORMAT '-Z,ZZZ,ZZ9' NO-UNDO.
DEFINE VARIABLE l-sec AS INTEGER NO-UNDO.
DEF VARIABLE g-Origen AS CHAR NO-UNDO.

FIND FIRST Usuario WHERE Usuario.Id-User = p-User NO-LOCK NO-ERROR.
IF AVAILABLE Usuario THEN g-Origen = Usuario.id-ubicacion.
//{sia00000.var}

{bodb0120.v}  

DEFINE VARIABLE l-Origen AS CHAR NO-UNDO.

FIND FIRST Pedido WHERE Pedido.Id-Pedido = l-Pedido AND Pedido.Resto = l-resto
                  NO-LOCK NO-ERROR.
l-Origen = g-Origen.

IF Pedido.Id-Alm <> "" AND Pedido.Id-Alm <> g-Origen THEN
   ASSIGN l-Origen = g-Origen
          g-Origen = Pedido.Id-Alm.
          
FOR EACH DetPedido WHERE DetPedido.id-pedido = l-pedido
                     AND DetPedido.resto = l-resto
                     AND DetPedido.tipo = 1
                     AND tpoCorte <> 0 NO-LOCK:
    ASSIGN l-Art = DetPedido.Id-Articulo
           l-Col = DetPedido.Id-color.
    
    FIND ArtPres WHERE ArtPres.Id-Articulo = l-Art
                   AND ArtPres.Id-PRes = DetPedido.Id-pres NO-LOCK NO-ERROR.
    FIND FIRST t-Rep WHERE t-Rep.id-articulo = l-art
                       AND t-Rep.id-color = l-col
                       AND t-Rep.id-pres = ArtPres.Id-Pres EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE t-Rep THEN
        ASSIGN l-Cant = t-Rep.Cant + DetPedido.CantPed.
    ELSE 
        ASSIGN l-cant = DetPedido.CantPed.               

    {bodb0120.i}
     
END.

FOR EACH t-Rep /*WHERE t-Rep.existalta > 0*/ NO-LOCK,
    FIRST artpres WHERE artpres.id-articulo = t-Rep.id-articulo
                    AND artpres.id-pres = t-Rep.id-pres NO-LOCK:

    FIND FIRST Existencia WHERE Existencia.Id-Articulo = t-Rep.id-articulo
                            AND Existencia.Id-Color = t-Rep.id-color
                            AND Existencia.Disponible > 0
                            AND Existencia.id-alm = g-origen NO-LOCK NO-ERROR.
                            
    IF AVAILABLE existencia THEN DO:
        FIND FIRST t-DetRAU WHERE t-DetRAU.Id-Articulo = t-Rep.id-articulo
                              AND t-DetRAU.Id-Color = t-Rep.id-color
                              AND t-detrau.id-pres = t-Rep.id-pres
                            EXCLUSIVE-LOCK NO-ERROR.
                            
        IF NOT AVAILABLE t-DetRAU THEN DO:            
            CREATE t-DetRAU.
            ASSIGN
                l-Sec = l-Sec + 1
                t-DetRAU.Seq = l-sec.
            ASSIGN
                t-DetRAU.CantPres = t-Rep.cant
                t-DetRAU.Id-Articulo = t-Rep.id-articulo
                t-DetRAU.Id-Color = t-Rep.id-color
                t-DetRAU.Id-Pres = t-Rep.id-pres
                t-DetRAU.CantUMI = t-Rep.cant * ArtPres.equiv.
        END.
        ELSE DO:
            ASSIGN
                t-DetRAU.CantPres = t-DetRAU.CantPres + t-Rep.cant
                t-DetRAU.CantUMI = t-DetRAU.CantUMI + (t-Rep.cant * ArtPres.equiv).
        END.
    END.
    ELSE DO: /*SI NO ESTA LOCALIZADO EL ARTICULO SAUL 14-ABR-2011*/
        FIND FIRST t-DetRAU WHERE t-DetRAU.Id-Articulo = t-Rep.id-articulo
                              AND t-DetRAU.Id-Color = t-Rep.id-color
                              AND t-detrau.id-pres = t-Rep.id-pres
                              AND t-detrau.CantPres = 0
                            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE t-DetRAU THEN DO:
            CREATE t-DetRAU.
            ASSIGN
                l-Sec = l-Sec + 1
                t-DetRAU.Seq = l-sec
                t-DetRAU.Id-Articulo = t-Rep.id-articulo
                t-DetRAU.Id-Color = t-Rep.id-color
                t-DetRAU.Id-Pres = t-Rep.id-pres
                t-DetRAU.CantPres = 0
                t-DetRAU.CantUMI = t-Rep.cant * ArtPres.equiv.
        END.
        ELSE DO:
            ASSIGN                
                t-DetRAU.CantUMI = t-DetRAU.CantUMI + (t-Rep.cant * ArtPres.equiv).
        END.
    END.
END.

FIND FIRST t-detrau NO-LOCK NO-ERROR. /* Si encuentra requisiciones, genera e imprime tareas */

IF AVAILABLE t-detrau THEN DO TRANSACTION:
    FIND Folio WHERE Folio.Id-Doc = 'REQAU'
                 AND Folio.Id-Alm = g-origen EXCLUSIVE-LOCK.
    ASSIGN l-Folio = Folio.Prefijo + STRING(Folio.Folio,'999999')
           Folio.Folio = Folio.Folio + 1.
    RELEASE Folio.

    CREATE ReqAU.
    ASSIGN ReqAU.Id-RAU = l-folio 
           ReqAU.FecReg = TODAY
           ReqAU.id-Alm = g-origen
           ReqAU.Id-Per = p-User
           ReqAU.Pasillo = STRING(l-pedido) + "-" + STRING(l-Resto,'99').

    FOR EACH t-DetRAU NO-LOCK,
        FIRST ArtPres WHERE ArtPres.Id-Articulo = t-DetRAU.Id-Articulo
                        AND ArtPres.Id-Pres = t-DetRAU.Id-Pres NO-LOCK:
      CREATE DetRAU.
      BUFFER-COPY t-DetRAU TO DetRAU.
      ASSIGN DetRAU.Id-RAU = l-Folio.
      IF DetRAU.CantPres = 0 THEN
        ASSIGN DetRAU.CantPres = DetRAU.CantUMI / ArtPres.Equiv.      
    END.

    IF g-origen <> '02B' AND g-Origen <> "11" THEN DO: 
        {bodb0122.i " "}
    END.
    ELSE DO:
        {bodb0122.i "DESCENDING"}
    END.        
    
    RELEASE Existencia.
    RELEASE ReqAU.
    RELEASE DetRAU.
    /* DETENDRE ESTA RUTINA YA QUE MANDA A IMPRESION LOCAL EL TXT COMO EN PUTTY */
    IF g-origen <> '02B' THEN DO:
        FIND FIRST DetTarea WHERE DetTarea.Id-Tarea = l-FolTar NO-LOCK NO-ERROR.
        IF AVAILABLE DetTarea THEN DO:
            RUN bodc1160.p(l-FolTar).
        END.
        FIND FIRST DetTarea WHERE DetTarea.Id-Tarea = l-FolTar2 NO-LOCK NO-ERROR.
        IF AVAILABLE DetTarea THEN DO:
            RUN bodc1160.p(l-FolTar2).
        END.
    END.
  /*  */
    FIND FIRST DetTarea WHERE DetTarea.Id-Tarea = l-FolTar NO-LOCK NO-ERROR.
    IF NOT AVAILABLE DetTarea THEN DO:
        FIND FIRST Tarea WHERE Tarea.Id-Tarea = l-FolTar EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Tarea THEN DELETE Tarea.
    END.
    
    FIND FIRST DetTarea WHERE DetTarea.Id-Tarea = l-FolTar2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE DetTarea THEN DO:
        FIND FIRST Tarea WHERE Tarea.Id-Tarea = l-FolTar2 EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Tarea THEN DELETE Tarea.
    END.
    RELEASE tarea.      
END.
RELEASE folio.
ASSIGN g-Origen = l-Origen.

