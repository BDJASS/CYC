/*
  Empresa : ADOSA
  Programa: bodb0120.i
  Funcion : Rellenos de estantes por pasillo por articulo, creacion de renglones para tareas
  Autor   : ALEX
  Fecha   : 24 de Octubre del 2007
  ***********************************************************************
  *** Modificar el /usr2/sea/incluidos/bodb0120.p si se modifica este ***
  ***********************************************************************
*/


FIND articulo WHERE articulo.id-articulo = l-art NO-LOCK NO-ERROR.
/*
FOR EACH artpres WHERE artpres.id-articulo = l-Art
                   AND artpres.tipo = 4
                   AND artpres.activo = TRUE NO-LOCK BY artpres.equiv:
    ASSIGN l-recid = RECID(artpres).
    LEAVE.
END.
FIND artpres WHERE RECID(artpres) = l-RecId NO-LOCK NO-ERROR.
*/

FIND Kolor WHERE Kolor.Id-Color = l-Col NO-LOCK NO-ERROR.

IF NOT AVAILABLE t-rep THEN DO:
    CREATE t-rep.
    ASSIGN
        t-rep.id-articulo = l-art
        t-rep.descr = articulo.descr
        t-rep.id-color = l-col
        t-rep.nomcol = IF AVAILABLE kolor THEN kolor.Abrev ELSE ''
        t-rep.id-pres = ArtPres.id-pres
        t-rep.nompres = artpres.descr.
END.
ASSIGN t-rep.cant = l-cant.
FIND artubic WHERE artubic.id-articulo = l-art
               AND artubic.id-color = l-col
               AND artubic.id-alm = g-origen NO-LOCK NO-ERROR.
ASSIGN t-rep.existalta = artubic.exist / artpres.equiv.
