
/*------------------------------------------------------------------------
    File        : custom.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Mon Jan 06 12:49:16 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/*
  Programa  : vtad0040.p
  Funcion   : Proceso Para Asignar Folio de Pedido
  Autor     : AARON
  Fecha     : 16/Feb/1998
  Descripcion:
 
**********          I   M   P   O   R   T   A   N   T   E          **********
**********                                                         **********
********** 1_ SI MODIFICAS ALGO TAMBIEN MODIFICALO EN "embd0010.p" **********
********** 2_ NO USAR EL Folio.Prefijo 5 PORQUE ES TEMPORAL        **********
 
*/
 
//{/usr2/adosa/includes/sia00000.var}
 
DEFINE OUTPUT PARAMETER p-Pedido  LIKE Pedido.Id-Pedido   NO-UNDO INITIAL "".
DEFINE INPUT  PARAMETER p-Client  LIKE Pedido.Id-Cliente  NO-UNDO.
DEFINE INPUT  PARAMETER p-UbiVta  LIKE Pedido.Id-UbiVta   NO-UNDO.
DEFINE INPUT  PARAMETER p-Vended  LIKE Pedido.Id-Vendedor NO-UNDO.
DEFINE INPUT  PARAMETER p-EnFirme LIKE Pedido.EnFirme     NO-UNDO.
DEFINE INPUT  PARAMETER p-Enviar  LIKE Pedido.Enviar      NO-UNDO.
DEFINE INPUT  PARAMETER p-Folio   LIKE Pedido.Id-Pedido   NO-UNDO.
DEFINE VARIABLE         l-UbiBod  LIKE UbiBod.Id-UbiBod   NO-UNDO INITIAL "SLOC".
 
IF p-Client <> 3 THEN DO:
    FIND Cliente WHERE Cliente.Id-Cliente = p-Client NO-LOCK NO-ERROR.
    IF AVAILABLE Cliente THEN DO:
        FIND Zona WHERE Zona.Id-Zona = Cliente.Id-Zona NO-LOCK NO-ERROR.
        IF AVAILABLE Zona THEN IF Zona.Ubic >= 2 THEN ASSIGN l-UbiBod = 'SFOR'.
    END.
END.
 
RELEASE UbiBod.
 
FIND UbiVta WHERE UbiVta.Id-UbiVta = p-UbiVta NO-LOCK NO-ERROR.
IF AVAILABLE UbiVta THEN DO:
    IF (p-Vended = "0004") THEN
        FIND UbiBod WHERE UbiBod.Id-UbiBod = "SLAG" NO-LOCK NO-ERROR.
    ELSE
        IF (p-UbiVta = "MLA" AND p-Client = 4672) OR
           (p-UbiVta = "MLA" AND p-Client = 1943) THEN
            FIND UbiBod WHERE UbiBod.Id-UbiBod = "SFOR" NO-LOCK NO-ERROR.
        ELSE
            IF ((p-UbiVta = "MFR" OR p-UbiVta = "COM") AND (p-Vended = "0056" OR p-Vended = "0038" OR p-Vended = "0045")) THEN
                FIND UbiBod WHERE UbiBod.Id-UbiBod = "SLOC" NO-LOCK NO-ERROR.
            ELSE
                IF p-UbiVta = "GER" AND l-UbiBod = "SFOR" THEN
                    FIND UbiBod WHERE UbiBod.Id-UbiBod = "SFOR" NO-LOCK NO-ERROR.
                ELSE
                    FIND UbiBod WHERE UbiBod.Id-UbiBod = UbiVta.Id-UbiBod NO-LOCK NO-ERROR.
END.
 
ASSIGN l-UbiBod = (IF AVAILABLE UbiBod THEN UbiBod.Id-UbiBod ELSE "SLOC").
 
FIND FIRST Pedido WHERE Pedido.Id-Pedido = p-Folio NO-LOCK NO-ERROR.
 
IF p-Enfirme THEN DO:
    CASE Pedido.Id-Alm:
        WHEN "02B" THEN DO:
            ASSIGN
                l-UbiBod = IF (p-Enviar = TRUE OR Pedido.Id-Alm = "02B") THEN "SLAG" ELSE (IF Pedido.Id-Alm = 'FUG' THEN "SLOC" ELSE "SMOS").
            FIND Folio WHERE Folio.Id-Doc = "PED" + l-UbiBod AND Folio.Id-Alm = "" EXCLUSIVE-LOCK NO-ERROR.
        END.
        WHEN "FUG" THEN DO:
            ASSIGN
                l-UbiBod = IF (p-Enviar = TRUE OR Pedido.Id-Alm = "02B") THEN "SLAG" ELSE (IF Pedido.Id-Alm = 'FUG' THEN "SLOC" ELSE "SMOS").
            FIND Folio WHERE Folio.Id-Doc = "PED" + l-UbiBod AND Folio.Id-Alm = "" EXCLUSIVE-LOCK NO-ERROR.
        END.
        WHEN "11" THEN DO:
            FIND Folio WHERE Folio.Id-Doc = "PEDTMK" AND Folio.Id-Alm = Pedido.Id-Alm EXCLUSIVE-LOCK NO-ERROR.
        END.
        WHEN "12" THEN DO:
            FIND Folio WHERE Folio.Id-Doc = "PEDTMK" AND Folio.Id-Alm = Pedido.Id-Alm EXCLUSIVE-LOCK NO-ERROR.
        END.
        OTHERWISE DO:
            FIND Folio WHERE Folio.Id-Doc = "PEDTMK" AND Folio.Id-Alm = Pedido.Id-Alm EXCLUSIVE-LOCK NO-ERROR.
        END.
    END CASE.
END.
ELSE DO:
    FIND Folio WHERE Folio.Id-Doc = "PEDAUTO" AND Folio.Id-Alm = "" EXCLUSIVE-LOCK NO-ERROR.
END.
 
IF AVAILABLE Folio THEN DO:
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