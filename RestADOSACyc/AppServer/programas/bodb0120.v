/*
    Empresa:    ADOSA
    Programa:   bodb0120.v
    Funcion:    Definicion del Temp Table global
    Autor:      Alex
    Fecha:      24 de Oactubre del 2007
*/
    
DEFINE NEW SHARED TEMP-TABLE t-Rep
    FIELD Id-Articulo LIKE adosa.articulo.id-articulo
    FIELD Descr LIKE adosa.articulo.descr
    FIELD id-color LIKE adosa.kolor.id-color
    FIELD nomcol LIKE adosa.kolor.Abrev
    FIELD cant AS INTEGER
    FIELD id-pres LIKE adosa.artpres.id-pres
    FIELD nompres AS CHARACTER FORMAT 'x(10)'
    FIELD existalta LIKE adosa.artubic.exist
    INDEX idx-art id-articulo id-color id-pres.

DEFINE TEMP-TABLE t-detrau LIKE detrau.

DEFINE BUFFER b-ap FOR artpres.
    