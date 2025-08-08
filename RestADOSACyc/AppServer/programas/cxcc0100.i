/*
  Empresa : ADOSA
  Programa: cxcc0100.i
  Funcion : Calculo del numero de meses en limites de credito sugeridos
  Autor   : ALEX
  Fecha   : 8 de Diciembre de 1999
*/

ASSIGN l-Ventas = 0 /* l-VtaMay = 0 */.

IF YEAR(l-FecIni) <> YEAR(l-FecFin) THEN DO:

  DO l-i = YEAR(l-FecIni) TO YEAR(l-FecFin):
    IF l-i = YEAR(l-FecIni) THEN 
      DO l-j = MONTH(l-FecIni) TO 12:
        ASSIGN l-Ventas = l-Ventas + EstCte.VentasCr[l-j].
/*
        IF l-VtaMay < EstCte.VentasCr[l-j] THEN
          ASSIGN l-VtaMay = EstCte.VentasCr[l-j].
*/
      END.
    IF l-i > YEAR(l-FecIni) AND l-i < YEAR(l-FecFin) THEN
      DO l-j = 1 TO 12:
        ASSIGN l-Ventas = l-Ventas + EstCte.VentasCr[l-j].
/*
        IF l-VtaMay < EstCte.VentasCr[l-j] THEN
          ASSIGN l-VtaMay = EstCte.VentasCr[l-j].
*/
      END.
    IF l-i = YEAR(l-FecFin) THEN
      DO l-j = 1 TO MONTH(l-FecFin):
        ASSIGN l-Ventas = l-Ventas + EstCte.VentasCr[l-j].
/*
        IF l-VtaMay < EstCte.VentasCr[l-j] THEN
          ASSIGN l-VtaMay = EstCte.VentasCr[l-j].
*/
      END.
  END.

END.
ELSE DO:

  DO l-j = MONTH(l-FecIni) TO MONTH(l-FecFin):
    ASSIGN l-Ventas = l-Ventas + EstCte.VentasCr[l-j].
/*
    IF l-VtaMay < EstCte.VentasCr[l-j] THEN
      ASSIGN l-VtaMay = EstCte.VentasCr[l-j].
*/
  END.

END.
