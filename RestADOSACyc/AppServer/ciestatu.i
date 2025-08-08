/*
  Programa: ciestatu.i
  Funcion : Impresion de Algun campo durante el proceso de un reporte
  Autor   : LUIS
  Fecha   : 15/12/1997
*/

STATUS DEFAULT "Generando Reporte ...   " + "{&Mensaje} " + STRING({&Campo}) 
{ifdef {&Stream}}
   + "   Pagina: " + STRING(PAGE-NUMBER(s-salida))
{endif} */ .
  