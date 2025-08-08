/*
  Programa  : vtad1000.p
  Funcion   : Proceso Para Calcular Digito Verificador p/Refer Banamex
  Autor     : FLC
  Fecha     : 8 MAY 2013
*/

DEF INPUT PARAMETER p-Valor AS INTEGER NO-UNDO.
DEF OUTPUT PARAMETER p-Verif AS INTEGER NO-UNDO.

DEF VAR l-Valor AS CHAR NO-UNDO.
DEF VAR l-Suma AS INTEGER NO-UNDO.
DEF VAR l-i AS INTEGER NO-UNDO.
DEF VAR l-PFij AS INTEGER NO-UNDO.
DEF VAR l-Op AS INTEGER NO-UNDO.
DEF VAR l-Fijos AS INTEGER EXTENT 7 NO-UNDO 
    INITIAL [13, 17, 19, 23, 29, 31, 37].
    
l-Suma = 449 + 728.    
l-Valor = TRIM(STRING(p-Valor)).
l-PFij = 7.
DO l-i = LENGTH(l-Valor) TO 1 BY -1:
   l-Op = (INTEGER(SUBSTRING(l-Valor,l-i,1)) * l-Fijos[l-PFij]).
   l-Suma = l-Suma + l-Op.
   l-PFij = l-PFij - 1.
END.
p-Verif = 99 - (l-Suma MODULO 97).


