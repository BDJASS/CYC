/*
    Empresa : ADOSA
    Programa: bodc1160.p (.Net)
    Funcion : Imprime la tarea 
    Autor   : ALEX
    Fecha   : 3 de Marzo del 2025  
*/

// {/usr2/adosa/includes/sia00000.var}

DEFINE INPUT PARAMETER l-Tarea AS CHARACTER NO-UNDO.
/*
define input parameter NombreImpresoraa AS CHARACTER NO-UNDO. /* Agregar esto para acoplar esta impresion al proyecto del CEDIS en .NET */
*/

DEFINE VARIABLE l-Archivo      AS CHARACTER NO-UNDO.  
DEFINE VARIABLE l-Nombre       AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Present      AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Localizacion LIKE Localizacion.Codigo NO-UNDO.
DEFINE VARIABLE l-Secuencia    LIKE DetLoc3.Secuencia NO-UNDO.
DEFINE VARIABLE l-DescAlm      AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Depto        LIKE Depto.nombre NO-UNDO.
DEFINE VARIABLE l-sec          AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-Ctr15        AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Ctr18        AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-lista        AS CHARACTER.
DEFINE VARIABLE l-consigna     AS LOGI.
DEFINE VARIABLE l-cliente      LIKE Cliente.Id-Cliente.
DEFINE VARIABLE l-CodBar2      AS CHARACTER NO-UNDO FORMAT 'X(20)'.
DEFINE VARIABLE l-Picking      AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-aster        AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-cantdife     LIKE DetSUI.CantUMI NO-UNDO.
DEFINE VARIABLE l-NomUsuario   LIKE usuario.nom-usuario NO-UNDO.
DEFINE VARIABLE l-Trasp        LIKE STrasp.Id-STrasp NO-UNDO.
DEFINE VARIABLE l-i            AS INTEGER NO-UNDO.

DEFINE STREAM s-salida.

DEFINE TEMP-TABLE w-rep 
    FIELD Id-Art    LIKE DetSUI.id-art
    FIELD Descr     LIKE Articulo.Descr 
    FIELD Nombre    LIKE l-Nombre
    FIELD Id-Color  LIKE Kolor.Id-color
    FIELD Sec       AS INTEGER
    FIELD Present   LIKE l-Present
    FIELD CantPres  LIKE DetSUI.Cantpres
    FIELD Loc       LIKE Localizacion.Codigo
    FIELD Secuencia LIKE DetLoc3.Secuencia
    FIELD CantUMI   LIKE DetSUI.CantUMI.

DEFINE TEMP-TABLE tt
    FIELD Id-Art     LIKE DetSUI.Id-art
    FIELD Id-Color   LIKE Kolor.Id-color
    FIELD CantUMIPed LIKE DetSUI.CantUMI
    FIELD CantUMITar LIKE DetSUI.CantUMI
    FIELD CantDife   LIKE DetSUI.CantUMI
    FIELD refer      AS LOGICAL INITIAL TRUE.   

ASSIGN 
    l-Ctr15 = CHR(27) + CHR(15)   
    l-Ctr18 = CHR(27) + CHR(18).
    
ASSIGN 
    l-Archivo = '/usr3/tmp/' + l-Tarea + STRING(TIME) + ".lst".

FIND Tarea WHERE Tarea.id-Tarea = l-Tarea NO-LOCK.

ASSIGN 
    l-CodBar2 = '!R! BARCODE 1, "Code 39", "' + Tarea.Refer + '",  140, 0, 60, 3, 0, 56; EXIT;'.

FIND FIRST ReqAlm WHERE ReqAlm.Id-Req = Tarea.Refer
                    AND ReqAlm.Resto = Tarea.Resto NO-LOCK NO-ERROR.
ASSIGN 
    l-Trasp = ''.
    
IF AVAILABLE ReqAlm THEN DO:
    FIND Almacen WHERE Almacen.Id-Alm = Reqalm.Id-Ubic NO-LOCK.
    FIND LAST STrasp WHERE STrasp.Id-Req = ReqAlm.Id-Req
                       AND STrasp.ReqRest = ReqAlm.Resto NO-LOCK NO-ERROR.
    IF AVAILABLE STrasp THEN ASSIGN l-Trasp = STrasp.Id-STrasp.
END.

IF AVAILABLE Almacen THEN 
    ASSIGN
        l-DescAlm = "Almacen: " + Almacen.Id-Alm + "   " + Almacen.Refer
        l-Depto   = "".

FIND FIRST Empleado WHERE Empleado.Iniciales = Tarea.Id-User
                      AND Empleado.Iniciales <> "" NO-LOCK NO-ERROR.
IF AVAILABLE Empleado THEN
    ASSIGN l-nomUsuario = Empleado.Nombre.
ELSE 
    ASSIGN l-NomUsuario = Tarea.Id-User.

FORM HEADER
    l-CodBar2 FORMAT 'x(78)' TO 120
    SKIP
    "Requisicion    :"       Tarea.Refer   AT 18  FORMAT "XXXXXXX"
    Tarea.Resto
    "  Traspaso :"           l-Trasp
    "Fecha    :"       AT 55 Tarea.FecReg  AT 66                   SKIP(0)
    "Realizo        :"       l-NomUsuario  AT 18                   
    "Entregar a:"      AT 54 Tarea.Pasillo FORMAT 'x(15)' AT 66    SKIP

    /* "Autoriza       :"       sUsoInt.id-autor AT 18 PersAlm.Nombre 
    "Bultos   :"       AT 55 sUsoInt.Bultos   AT 66                   SKIP */
    "Comentarios    :"       Tarea.Observacion  AT 18                   SKIP 
    FILL("-",80) FORMAT "x(80)"                                       SKIP
    l-Ctr15 + "Codigo Descripcion                                            Color     Pres.       CantPres    CantEnt    Racks  Picking   Dife(UMI)" FORMAT "x(135)" SKIP
    FILL("-",133) FORMAT "x(133)"
WITH FRAME f-sal OVERLAY SIDE-LABEL CENTERED WIDTH 150 NO-BOX PAGE-TOP.

FORM
    DetTarea.id-articulo
    Articulo.Descr     FORMAT "x(54)"
    l-Nombre           FORMAT "x(9)"
    l-Present          FORMAT "x(9)"
    DetTarea.Cant
    DetTarea.CantEnt
    l-localizacion     FORMAT 'X(8)'
    l-Picking
    l-aster            FORMAT 'X(1)'
    l-cantdife
WITH  OVERLAY CENTERED WIDTH 161 FRAME f-rep DOWN NO-BOX NO-LABELS.

FORM HEADER
    CHR(27) + CHR(18) + FILL("=",80) FORMAT "x(82)" SKIP
    "__________   __________   __________ ______ _______  __________   __________"
    " CREDITO       SURTIO       EMPACO   BULTOS TARIMAS     CORTO      CLIENTE  "
WITH FRAME f-pie2 OVERLAY SIDE-LABEL  CENTERED  WIDTH 100 NO-BOX PAGE-BOTTOM.

EMPTY TEMP-TABLE w-rep.
EMPTY TEMP-TABLE tt.

OUTPUT STREAM s-salida TO VALUE(l-Archivo) PAGED PAGE-SIZE 30.
PUT STREAM s-salida CONTROL CHR(27) + CHR(18) + CHR(27) + "C" + CHR(33).

FIND Pedido WHERE Pedido.Id-Pedido = Tarea.Refer
              AND Pedido.Resto = Tarea.Resto NO-LOCK NO-ERROR.

IF NOT AVAILABLE Pedido AND Tarea.Resto = ? THEN  
   FIND Pedido WHERE Pedido.Id-Pedido = Tarea.Refer
              AND Pedido.Resto = 0 NO-LOCK NO-ERROR.            
              
/*            
{cieheadr.i  
    &Ancho     = 81
    &AnchoM    = 75
    &titulo    = " 'SURTIDO DE RACKS' "
    &Subtitulo = " ' Tarea: ' + Tarea.Id-Tarea "
    &Aveces    = "(IF AVAILABLE Pedido THEN (IF Pedido.Id-Vendedor <> '0100' THEN 'PEDIDO DE TELEMARKETING' ELSE 'PEDIDO DE INTERNET     *** ENTREGAR A MESAS 7 Y 8 ***') ELSE '')"
    &Stream    = s-salida
}
*/   
      
IF (Pedido.Id-Alm = "03A" OR Pedido.Id-Alm = "11") AND 
    SUBSTRING(Tarea.Pasillo,9,2) >= "00" AND 
    SUBSTRING(Tarea.Pasillo,9,2) <= "99" THEN DO: /*para agregar * a renglones no localizados e incompletos*/
    FOR EACH DetPedido WHERE DetPedido.Id-Pedido = Tarea.Refer AND
        DetPedido.Resto = INT(SUBSTRING(Tarea.Pasillo,9,2)) AND    
        DetPedido.Tipo = 1 NO-LOCK.
        FIND ArtPres WHERE ArtPres.Id-articulo = DetPedido.Id-Articulo AND
            ArtPres.Id-pres     = DetPedido.Id-pres NO-LOCK NO-ERROR.
        FIND tt WHERE tt.id-art      = DetPedido.Id-Articulo AND
            tt.id-color    = DetPedido.Id-color NO-LOCK NO-ERROR.        
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            ASSIGN 
                tt.id-art     = DetPedido.Id-Articulo
                tt.id-color   = DetPedido.Id-color
                tt.CantUMIPed = DetPedido.CantPed * ArtPres.Equiv.
        END.
        ELSE DO:
            ASSIGN
                tt.CantUMIPEd = tt.CantUMIPed  + (DetPedido.CantPed * ArtPres.Equiv).
        END.
    END.                             
END. 


VIEW STREAM s-salida FRAME f-Sal.
VIEW STREAM s-Salida FRAME f-pie2.
ASSIGN 
    l-sec = 0.

FOR EACH DetTarea WHERE DetTarea.id-Tarea = l-Tarea NO-LOCK USE-INDEX idx-sec:
    FIND Articulo WHERE Articulo.Id-Articulo = DetTarea.Id-Articulo NO-LOCK NO-ERROR.
    FIND Kolor    WHERE kolor.id-color       = DetTarea.id-color NO-LOCK NO-ERROR.
    FIND ArtPres  WHERE ArtPres.id-pres      = DetTarea.id-pres
                    AND ArtPres.id-art       = DetTarea.id-art   NO-LOCK NO-ERROR.
    ASSIGN
        l-Nombre  = IF AVAILABLE kolor   THEN Kolor.Descr   ELSE ''
        l-Present = IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE ''.
    CREATE w-Rep.
    ASSIGN
        w-Rep.Id-Art    = DetTarea.Id-Art
        w-Rep.Descr     = Articulo.Descr
        w-Rep.Nombre    = l-nombre
        w-Rep.Id-Color  = DetTarea.Id-Color
        l-sec           = l-sec + 1
        w-Rep.sec       = l-sec
        w-Rep.Present   = l-Present
        w-Rep.CantPres  = DetTarea.Cant
        w-Rep.CantUMI   = DetTarea.CantEnt
        w-Rep.Secuencia = DetTarea.Secuencia
        w-Rep.Loc       = DetTarea.Id-Loc.
        
    IF Tarea.Refer BEGINS '8' OR Tarea.Refer BEGINS '7' OR Tarea.Refer BEGINS '4' THEN DO:    
        FIND tt WHERE tt.id-art      = DetTarea.Id-Articulo AND
            tt.id-color    = DetTarea.Id-color NO-LOCK NO-ERROR.        
        IF AVAILABLE tt THEN 
            ASSIGN 
                tt.CantUMITar = tt.CantUMITar + (w-Rep.CantPres * ArtPres.Equiv).                           
    END.
END. /* del for each detsui */

FOR EACH w-Rep NO-LOCK BY w-Rep.Secuencia:
    ASSIGN 
        l-Picking = ''
        l-aster   = ''.
    FOR EACH DetLoc3 WHERE DetLoc3.Id-Articulo = w-Rep.Id-Art
        AND DetLoc3.Id-Color = w-Rep.Id-Color NO-LOCK,
        FIRST Localizacion WHERE Localizacion.Clave = DetLoc3.Localizacion NO-LOCK,
        FIRST Almacen WHERE Almacen.Clave = Localizacion.Almacen
        AND Almacen.Id-Alm = Pedido.Id-Alm NO-LOCK:
        ASSIGN 
            l-Picking = Localizacion.Codigo.
        LEAVE.
    END.

    FIND tt WHERE tt.id-art = w-Rep.Id-Art
        AND tt.id-color = w-Rep.Id-color EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE tt AND tt.CantUMIPed <> tt.CantUMITar AND tt.refer = TRUE THEN 
        ASSIGN 
            tt.CantDife = (tt.CantUMIPed - tt.CantUMITar)         
            l-aster     = '*'.

    DISPLAY STREAM s-salida
        w-Rep.Id-Art   @ DetTarea.id-art
        w-Rep.Descr    @ Articulo.Descr
        w-Rep.Nombre   @ l-nombre
        w-Rep.Pres     @ l-present
        w-Rep.CantPres @ DetTarea.Cant
        w-Rep.CantUMI  @ DetTarea.CantEnt
        w-Rep.Loc      @ l-Localizacion
        l-Picking
        l-aster  
        tt.CantDife  
        WHEN AVAILABLE tt AND tt.CantDife <> 0 AND w-Rep.Loc <> '' AND tt.refer @ l-cantdife        
        WITH FRAME f-rep.
    DOWN STREAM s-Salida WITH FRAME f-Rep.
    
    IF (Tarea.Refer BEGINS '8' OR Tarea.Refer BEGINS '7' OR Tarea.Refer BEGINS '4') AND AVAILABLE tt THEN ASSIGN tt.refer = FALSE.  
    RELEASE tt.    
END. /* del for each */
PUT STREAM s-salida CONTROL CHR(18).
OUTPUT STREAM s-salida CLOSE.

CASE Pedido.Id-Alm:
    WHEN "03A" THEN DO:
       RUN /usr2/adosa/procs/cieimpr11.p(INPUT l-Archivo, 'M3', 1, '', 0, '', 0).  /* Agregar impresora "N" para altos */
    END.
    WHEN "11" THEN DO:
        RUN /usr2/adosa/procs/cieimpr11.p(INPUT l-Archivo, 'SAEMP', 1, '', 0, '', 0). /* Agregar "SACOR" para cortes y "SAEMP" para rellenos en Saltillo */
    END.
    WHEN "12" THEN DO:
        RUN /usr2/adosa/procs/cieimpr11.p(INPUT l-Archivo, 'CHALM', 1, '', 0, '', 0). /* Agregar "CHCOR" para cortes en Chihuahua */
    END.
    WHEN "7" THEN DO:
        RUN /usr2/adosa/procs/cieimpr11.p(INPUT l-Archivo, 'RCRES', 1, '', 0, '', 0).
    END.  
END CASE.

