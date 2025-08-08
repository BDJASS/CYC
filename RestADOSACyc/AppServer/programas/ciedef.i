/*
   Empresa : Consultoria en Informatica Ejecutiva
   Sistema : Ambiente General para el Desarrollador
   Modulo  :
   Programa: ciedef.i
   Usado por: cieheadr.i
   Funcion : Declaracion de variables utilizadas para generar el encabezado.

   Autor   : J.A.Z.V
   Fecha   : 21/Sept/1994

   Bitacora:  Iniciales  Descripcion General del Cambio
	      ---------  ------------------------------
		 INI     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
*/
/* Declaracion de las Variables*/
DEF VAR l-espenc     AS INT.
DEF VAR l-subenc     AS INT.
DEF VAR l-libres1    AS INT.
DEF VAR l-libres2    AS INT.
DEF VAR l-libres3    AS INT.
DEF VAR l-long       AS INT.
DEF VAR l-fechanva   AS CHAR FORMAT "x(8)".
DEF VAR l-hora       AS CHAR FORMAT "x(8)".
DEF VAR l-fech       AS CHAR FORMAT "x(6)" INITIAL "Fecha: ".
DEF VAR l-hor        AS CHAR FORMAT "x(5)" INITIAL "Hora : ".
DEF VAR l-dist       AS CHAR FORMAT "x(6)" INITIAL "#Dist: ".
DEF VAR l-pag        AS CHAR FORMAT "x(4)" INITIAL "Pag:".
DEF VAR l-esp        AS CHAR FORMAT "x(5)" INITIAL "     ".
