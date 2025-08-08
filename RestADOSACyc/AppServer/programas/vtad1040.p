/*
  Programa  : vtad1040.i
  Funcion   : Proceso Para Asignar Folio de Pedido a sucursales
  Autor     : Alex
  Fecha     : 31 de Marzo del 2007
  Descripcion:
*/



DEF OUTPUT PARAMETER p-Pedido  LIKE Pedido.Id-Pedido   NO-UNDO INITIAL "".
DEF INPUT  PARAMETER p-Client  LIKE Pedido.Id-Cliente  NO-UNDO.
DEF INPUT  PARAMETER p-UbiVta  LIKE Pedido.Id-UbiVta   NO-UNDO.
DEF INPUT  PARAMETER p-Vended  LIKE Pedido.Id-Vendedor NO-UNDO.
DEF INPUT  PARAMETER p-EnFirme LIKE Pedido.EnFirme     NO-UNDO.
DEF INPUT  PARAMETER p-Enviar  LIKE Pedido.Enviar      NO-UNDO.
DEF INPUT  PARAMETER p-Folio   LIKE Pedido.Id-Pedido   NO-UNDO.
DEF VAR              l-UbiBod  LIKE UbiBod.Id-UbiBod   NO-UNDO INITIAL "SLOC".


DO TRANSACTION:
    FIND FIRST Pedido WHERE Pedido.Id-Pedido = p-Folio NO-LOCK NO-ERROR.
    FIND vendedor WHERE vendedor.id-vendedor = pedido.id-vendedor NO-LOCK NO-ERROR.
    FIND usuario WHERE usuario.id-user = vendedor.iniciales NO-LOCK NO-ERROR.
    IF p-Enviar = TRUE THEN DO:
        ASSIGN l-UbiBod = IF p-Enviar = TRUE THEN "SLAG" ELSE (IF Pedido.Id-Alm = 'FUG' THEN "SLOC" ELSE "SMOS").
        FIND Folio WHERE Folio.Id-Doc = "PED" + l-UbiBod AND Folio.Id-Alm = "" EXCLUSIVE-LOCK NO-ERROR.
    END.
    ELSE DO:
        FIND folio WHERE folio.id-doc = "PEDTMK"
                     AND folio.id-alm = Pedido.Id-Alm EXCLUSIVE-LOCK NO-ERROR.
    END.
    IF AVAILABLE Folio THEN
        ASSIGN p-Pedido    = STRING(Folio.Prefijo,"x") + STRING(Folio.Folio,"999999")
               Folio.Folio = Folio.Folio + 1.
               
    IF Pedido.Id-Alm = "6" OR Pedido.Id-Alm = "11" THEN DO:
        IF Folio.Folio >= 500001 THEN
            ASSIGN Folio.Folio = 1.
    END.
    ELSE DO:
        IF Folio.Folio >= 1000000 THEN
            ASSIGN Folio.Folio = (IF Pedido.Id-Alm = "7" THEN 500001 ELSE 1).
    END.
               
END.    
RELEASE Folio.   
