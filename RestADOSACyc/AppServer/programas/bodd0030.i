/*
    Empresa : ADOSA
    Programa: bodd0030.i
    Funcion : Guarda la localizacion y la secuencia del DetLoc3 al DetPedido
    Autor   : ALEX
    Fecha   : 1 de Noviembre del 2000
    ********* IGUALAR AL bodd0031.i *********
    ********* IGUALAR AL bodd0032.i *********
*/

ASSIGN l-Secuencia = 0
       l-Loc = ''.

IF DetPedido.Id-Alm = '02B' OR DetPedido.Id-Alm = '11' OR Pedido.Enviar THEN DO:
    FOR EACH DetLoc3 WHERE DetLoc3.Id-Articulo = DetPedido.Id-Articulo
                       AND DetLoc3.Id-Color = DetPedido.Id-Color NO-LOCK,
        FIRST Localizacion WHERE Localizacion.Clave = DetLoc3.Localizacion NO-LOCK,
        FIRST Almacen WHERE Almacen.Clave = Localizacion.Almacen
                        AND Almacen.Id-Alm = DetPedido.Id-Alm NO-LOCK:
        ASSIGN DetPedido.Secuencia = DetLoc3.Secuencia
               l-secuencia = DetLoc3.Secuencia.
        ASSIGN DetPedido.Id-Loc = Localizacion.Codigo
               l-Loc            = Localizacion.Codigo.
    END.
END.
