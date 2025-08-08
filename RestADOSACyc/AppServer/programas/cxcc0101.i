/*
  Empresa : ADOSA
  Programa: cxcc0101.i
  Funcion : Calculo de las ventas de credito de los limites de credito sugerido
  Autor   : ALEX
  Fecha   : 8 de Diciembre de 1999
*/

IF YEAR(l-FecIni) <> YEAR(l-FecFin) THEN DO:

  DO l-i = YEAR(l-FecIni) TO YEAR(l-FecFin):
    IF l-i = YEAR(l-FecIni)
      THEN ASSIGN l-NumMes = 13 - MONTH(l-FecIni).
    IF l-i > YEAR(l-FecIni) AND l-i < YEAR(l-FecFin)
      THEN ASSIGN l-NumMes = l-NumMes + 12.
    IF l-i = YEAR(l-FecFin)
      THEN ASSIGN l-NumMes = l-NumMes + MONTH(l-FecFin).
  END.

END.
ELSE DO:

  ASSIGN l-NumMes = (MONTH(l-FecFin) - MONTH(l-FecIni)) + 1.

END.

