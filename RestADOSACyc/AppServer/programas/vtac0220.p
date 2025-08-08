/*
    Empresa  : Consultoria en Informatica Ejecutiva
    Programa : vtac0220.p
    Funcion  : Imprime Pedido de Mostrador
    Autor    : FLC
    Fecha    : 22-NOV-96
    Anotaciones: Mandar llamar a el programa vtac0220.p como sigue:
                 vtac0220.p(INPUT <Pedido>,
                            INPUT <Resto>,
                            INPUT FALSE,
                            INPUT-OUTPUT <Variable-Logica-Tmp>,
                            INPUT-OUTPUT <Variable-Logica-Tmp>,
                            INPUT FALSE).
     
    IMPORTANTE: Mantener Ligado este Programa con el vtac0221.p y el vtac0223.p
                "ln -f vtac0220.p vtac0221.p"   
*/

//{/usr2/adosa/includes/sia00000.var}

DEFINE VARIABLE l-FolTar LIKE Tarea.Id-Tarea NO-UNDO.  
DEFINE INPUT        PARAMETER p-Pedido LIKE Pedido.Id-Pedido NO-UNDO.
DEFINE INPUT        PARAMETER p-Resto LIKE Pedido.Resto NO-UNDO.
DEFINE INPUT        PARAMETER p-ImprimeElCompleto AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-SiLoImprimi AS LOGICAL NO-UNDO.
DEFINE INPUT        PARAMETER p-EnElAlmacen AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER p-PedirImpresora AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-Corte33 AS LOGI NO-UNDO.
DEFINE VARIABLE l-Mens1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-AreaC AS LOGI NO-UNDO.
DEFINE VARIABLE l-AreaCup AS LOGI NO-UNDO.
DEFINE VARIABLE l-AreaD AS LOGI NO-UNDO.
DEFINE VARIABLE l-Corte34 AS LOGI NO-UNDO.
DEFINE VARIABLE l-archivo AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-envia AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-nombre AS CHARACTER NO-UNDO FORMAT 'x(10)'.
DEFINE VARIABLE l-present AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-present2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-descAlm AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Renglon AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE l-i AS INTEGER NO-UNDO.
DEFINE VARIABLE l-nomsol LIKE PersAlm.nombre NO-UNDO.
DEFINE VARIABLE l-Guiones AS CHARACTER NO-UNDO INITIAL "____".
DEFINE VARIABLE l-Fecha_ AS DATE NO-UNDO.
DEFINE VARIABLE l-Hora_ AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Limite AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE l-NC AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE l-CD1 AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE l-CD2 AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE l-SalTot AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE l-Venc AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE l-Completo LIKE AlmImp.Completo NO-UNDO INITIAL TRUE.
DEFINE VARIABLE l-IVA AS CHARACTER NO-UNDO INITIAL " ".
DEFINE VARIABLE l-ExistAlta LIKE DetPedido.ExistAlta NO-UNDO FORMAT '->>>>>9.<<'.
DEFINE VARIABLE l-Descr AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE l-Corte3 AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-Corte4 AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-LocEsp AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-Control AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-PregImPeCo LIKE UbiVta.PregImPeCo NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-Impresiones AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE l-SoloCorte AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-CantPed AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE l-CantRacks LIKE DetPedido.CantPed NO-UNDO.
DEFINE VARIABLE l-YaImprimiElCompleto AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-ExisteUnCompleto AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-Pedido AS CHARACTER NO-UNDO INITIAL "".
DEFINE BUFFER b-Ped             FOR  Pedido.
DEFINE BUFFER b-DPTmp           FOR  DetPedido.
DEFINE BUFFER B-Vendedor        FOR  Vendedor.
DEFINE BUFFER B-DetPedido       FOR  DetPedido.
DEFINE BUFFER b-AP FOR ArtPres.
DEFINE BUFFER b-Exi FOR Existencia.
DEFINE VARIABLE l-RazonSocial LIKE Cliente.RazonSocial NO-UNDO INITIAL ''.
DEFINE VARIABLE l-LinEm0 AS CHARACTER NO-UNDO INITIAL '********************************************************************************************************************'.
DEFINE VARIABLE l-LinEm1 AS CHARACTER NO-UNDO INITIAL ''.
DEFINE VARIABLE l-LinEm2 AS CHARACTER NO-UNDO INITIAL ''.
DEFINE VARIABLE l-LinEm3 AS CHARACTER NO-UNDO INITIAL ''.
DEFINE VARIABLE l-ConEmb AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE l-localizacion AS CHARACTER FORMAT "x(8)".  
DEFINE VARIABLE l-alm AS CHARACTER FORMAT "x(10)".
DEFINE VARIABLE l-Signo AS CHARACTER FORMAT "x".
DEFINE STREAM s-Salida1.     
DEFINE  VARIABLE g-Origen   AS CHARACTER  NO-UNDO INITIAL "02B".
DEFINE NEW SHARED VARIABLE g-nomcia   AS CHARACTER  NO-UNDO.  
DEFINE NEW SHARED VARIABLE g-dist AS INTE FORMAT "9999" NO-UNDO.
FIND FIRST SysGeneral NO-LOCK NO-ERROR.
IF AVAILABLE SysGeneral THEN ASSIGN g-nomcia = SysGeneral.Empresa.
ASSIGN
    l-archivo =  '/usr3/tmp/' + "P" +  STRING(TIME,"HH:MM:SS") + ".lst"
    l-Fecha_  = TODAY
    l-Hora_   = STRING(TIME,"HH:MM:SS").
    
FIND Pedido WHERE Pedido.Id-Pedido = p-Pedido AND Pedido.Resto = p-Resto NO-LOCK NO-ERROR.
IF AVAILABLE Pedido AND Pedido.Enfirme = FALSE THEN RETURN.

ASSIGN g-Origen = Pedido.Id-Alm.

FIND Cliente OF Pedido NO-LOCK NO-ERROR.

IF Pedido.Id-Cliente <> 3 THEN
    FIND Ciudad OF Cliente NO-LOCK NO-ERROR.
ELSE
    FIND Ciudad OF Pedido NO-LOCK NO-ERROR.
    
FIND Estado OF Ciudad NO-LOCK NO-ERROR.
FIND Vendedor OF Pedido NO-LOCK NO-ERROR.
FIND UbiVta OF Pedido NO-LOCK NO-ERROR.
FIND CondVta OF Pedido NO-LOCK NO-ERROR.
FIND Entrega OF Pedido NO-LOCK NO-ERROR.
FIND Flete OF Pedido NO-LOCK NO-ERROR.
FIND Transporte OF Pedido NO-LOCK NO-ERROR.
FIND B-Vendedor WHERE B-Vendedor.Id-Vendedor=Pedido.Id-Captura NO-LOCK NO-ERROR.

IF Pedido.Resto = 0 THEN
    ASSIGN l-Pedido = "  " + STRING(Pedido.Id-Pedido,"x(7)").
ELSE
    ASSIGN l-Pedido = STRING(Pedido.Id-Pedido,"x(7)") + "-" + STRING(Pedido.Resto,"9").

IF AVAILABLE Pedido THEN DO:
    ASSIGN
        l-Impresiones = Pedido.Impresiones
        l-RazonSocial = (IF Pedido.Id-Cliente >= 101 AND AVAILABLE Cliente AND Cliente.Tipo = 1 THEN Cliente.Propietario ELSE Cliente.RazonSocial).
END.

FIND FIRST DetPedido WHERE DetPedido.Id-Pedido = p-Pedido
                       AND DetPedido.Resto = p-Resto
                       AND (DetPedido.Id-Loc BEGINS '2D' OR DetPedido.Id-Loc BEGINS 'S' OR DetPedido.Id-Loc = '')
                       AND DetPedido.TpoCorte <> 3
                       AND DetPedido.TpoCorte <> 4
                       AND DetPedido.Tipo = 1 NO-LOCK NO-ERROR.
IF AVAILABLE DetPedido THEN
    ASSIGN l-AreaD = TRUE.
ELSE
    ASSIGN l-AreaD = FALSE.
    
FIND FIRST DetPedido WHERE DetPedido.Id-Pedido = p-Pedido
                       AND DetPedido.Resto = p-Resto
                       AND (DetPedido.Tipo = 4) NO-LOCK NO-ERROR.
IF AVAILABLE DetPedido THEN
    ASSIGN l-Corte34 = TRUE.
ELSE
    ASSIGN l-Corte34 = FALSE.
FIND FIRST DetPedido WHERE DetPedido.Id-Pedido = p-Pedido
                       AND DetPedido.Resto = p-Resto
                       AND (DetPedido.Tipo = 3) NO-LOCK NO-ERROR.
IF AVAILABLE DetPedido THEN
    ASSIGN l-Corte33 = TRUE.
ELSE
    ASSIGN l-Corte33 = FALSE.

DO:
    FIND FIRST DetPedido WHERE DetPedido.Id-Pedido = p-Pedido
                           AND DetPedido.Resto = p-Resto
                           AND (DetPedido.Id-Loc BEGINS '2C' OR DetPedido.Id-Loc BEGINS '2B')
                           AND DetPedido.TpoCorte <> 3
                           AND DetPedido.TpoCorte <> 4
                           AND DetPedido.Tipo = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE DetPedido THEN
        ASSIGN l-Mens1 = "**** Pedido AreaC"
               l-AreaC = TRUE.
    ELSE
        ASSIGN l-Mens1 = '**** Pedido'
               l-AreaC = FALSE.

    FIND FIRST DetPedido WHERE DetPedido.Id-Pedido = p-Pedido
                           AND DetPedido.Resto = p-Resto                        
                           AND DetPedido.TpoCorte <> 3
                           AND DetPedido.TpoCorte <> 4
                           AND DetPedido.Tipo = 1
                           AND SUBSTRING(DetPedido.Id-Loc,1,4) >= '2C16'
                           AND SUBSTRING(DetPedido.Id-Loc,1,4) <= '2C26' NO-LOCK NO-ERROR.
    IF AVAILABLE DetPedido THEN DO:    
        /*
        FIND EstPedido WHERE EstPedido.Id-Pedido = p-Pedido
                         AND EstPedido.Id-seq = p-Resto
                         AND (EstPedido.dobleimpr = TRUE OR EstPedido.Areas MATCHES '*C*') NO-LOCK NO-ERROR.
        IF AVAILABLE EstPedido THEN
            l-AreaCup = TRUE.
        ELSE
            l-AreaCup = FALSE.
        */
        ASSIGN l-AreaCup = TRUE.        
    END.    
    IF l-Corte34 OR l-AreaD OR l-corte33 OR l-AreaCup THEN 
        ASSIGN
            l-Mens1 = l-Mens1 +
                      (IF l-AreaCup THEN ", AreaC-2" ELSE "") +
                      (IF l-AreaD THEN ", AreaD" ELSE "") +
                      (IF l-Corte33 THEN ", Corte3" ELSE "") +
                      (IF l-Corte34 THEN ", Corte4" ELSE "") +
                      ", Pasar a Facturarlos ****".
                      
    IF l-Corte34 = FALSE AND l-AreaD = FALSE AND l-corte33 = FALSE AND l-AreaCup = FALSE THEN 
        ASSIGN
            l-Mens1 = "**** Pedido solo de AreaC, Pasar a Facturarlos ****".
        
    ASSIGN
        l-Mens1 = '**** AREAS ' + l-Mens1.
END.

IF AVAILABLE Pedido THEN 
    l-Mens1 = l-Mens1 + (IF Pedido.Id-Vendedor = "0100" THEN " ENTREGAR EN MESAS 7 Y 8 ****" ELSE "").

IF AVAILABLE UbiVta THEN
    ASSIGN l-PregImPeCo = IF CAN-DO("02B,FUG", Pedido.Id-Alm) THEN FALSE ELSE UbiVta.PregImPeCo.
    
IF LENGTH(TRIM(p-EnElAlmacen)) = 0 THEN DO:   
    IF p-ImprimeElCompleto = FALSE THEN DO:
        IF l-PregImPeCo = TRUE AND NOT PROGRAM-NAME(3) MATCHES '*vtab0020*' 
                               AND NOT PROGRAM-NAME(3) MATCHES '*vtac0222*' THEN DO:
            RUN /usr2/adosa/procs/vtac0221.p(INPUT p-Pedido,
                                             INPUT p-Resto,
                                             INPUT TRUE,
                                             INPUT-OUTPUT p-SiLoImprimi,
                                             INPUT "",
                                             INPUT TRUE).
            IF p-SiLoImprimi THEN
                ASSIGN l-YaImprimiElCompleto = TRUE.
            ELSE DO:
                RUN /usr2/adosa/procs/vtac0203.p(INPUT p-Pedido,
                                                 INPUT p-Resto,
                                                 OUTPUT l-ExisteUnCompleto).
                IF NOT l-ExisteUnCompleto THEN DO:
                    RUN /usr2/adosa/procs/vtac0221.p(INPUT Pedido.Id-Pedido,
                                                     INPUT Pedido.Resto,
                                                     INPUT TRUE,
                                                     INPUT-OUTPUT p-SiLoImprimi,
                                                     INPUT "",
                                                     INPUT FALSE).
                    IF p-SiLoImprimi THEN
                        ASSIGN l-YaImprimiElCompleto = TRUE.
                    ELSE
                        ASSIGN l-YaImprimiElCompleto = FALSE.
                END.
                ELSE
                    ASSIGN l-YaImprimiElCompleto = FALSE.
            END.
        END.    
        ELSE DO:
            RUN /usr2/adosa/procs/vtac0203.p(INPUT p-Pedido, INPUT p-Resto, OUTPUT l-ExisteUnCompleto).
            IF NOT l-ExisteUnCompleto THEN DO:
                    RUN /usr2/adosa/procs/vtac0221.p(INPUT Pedido.Id-Pedido, INPUT Pedido.Resto, INPUT TRUE,
                                                 INPUT-OUTPUT p-SiLoImprimi, INPUT "", INPUT FALSE).
                    IF p-SiLoImprimi THEN ASSIGN l-YaImprimiElCompleto = TRUE.
                                             ELSE ASSIGN l-YaImprimiElCompleto = FALSE.
            END.
        END.
    END.
END.
/*OK*/
FIND Pedido WHERE Pedido.Id-Pedido = p-Pedido AND Pedido.Resto = p-Resto NO-LOCK NO-ERROR.
IF AVAILABLE Pedido THEN
    ASSIGN l-Impresiones = Pedido.Impresiones.

FORM HEADER
    '!R! BARCODE 1, " ", "' + Pedido.Id-Pedido + '",  140, 0, 60, 3, 0, 0; EXIT;' FORMAT 'x(78)'
WITH FRAME f-enc1 OVERLAY SIDE-LABEL CENTERED WIDTH 134 NO-BOX PAGE-TOP.

FORM HEADER
    '!R! BARCODE 1, "Code 39", "' + Pedido.Id-Pedido + '",  140, 0, 60, 3, 0, 56; EXIT;' FORMAT 'x(78)'
WITH FRAME f-enc1-1 OVERLAY SIDE-LABEL CENTERED WIDTH 134 NO-BOX PAGE-TOP.


FORM HEADER
  "FECHA:"           AT     1 Pedido.FecCap
  "VENDEDOR:"        AT    20 Pedido.Id-Vendedor
  "No. PEDIDO:"      AT   100 "(" + Pedido.Id-UbiVta + ") " + l-Pedido FORMAT "x(17)"
WITH FRAME f-enc2 OVERLAY SIDE-LABEL  CENTERED  WIDTH 134 NO-BOX PAGE-TOP.

FORM HEADER
  FILL("=",132)                                     FORMAT "x(132)"
  "CLIENTE.....:"      AT     1 Pedido.RazonSocial
  "No. CUENTA:"        AT    47 Cliente.Id-Cliente  FORMAT "zzzzz9"
  "TEL:"                        Pedido.Tel          FORMAT "x(10)"
  "SUSTITUCION (S/N):" AT     1 Pedido.Sust
  "CONDICIONES:"                CondVta.Descr
  "DESTINO:"                    Entrega.Descr
  WITH FRAME f-enc3-1 OVERLAY SIDE-LABEL  CENTERED  WIDTH 134 NO-BOX PAGE-TOP.

FORM HEADER
  FILL("=",132)                                     FORMAT "x(132)"
  "CLIENTE.....:"      AT     1 l-RazonSocial
  "No. CUENTA:"                 Cliente.Id-Cliente  FORMAT "zzzzz9"
  "TEL:"                        Cliente.Tel1        FORMAT "x(10)"
  "SUSTITUCION (S/N):" AT     1 Pedido.Sust
  "CONDICIONES:"                CondVta.Descr       FORMAT "x(14)"
  "DESTINO:"                    Entrega.Descr
  WITH FRAME f-enc3-2 OVERLAY SIDE-LABEL  CENTERED  WIDTH 134 NO-BOX PAGE-TOP.

FORM HEADER
  FILL("=",132)                                  FORMAT "x(132)"
  SKIP
  "No  LOCALIZA    PEDIDA   SURTIDO  UNIDAD  CLAVE  DESCRIPCION                                                     COLOR   EXIST-ALTA" 
  SKIP
  FILL("=",132)                                  FORMAT "x(132)"
WITH FRAME f-enc4 OVERLAY SIDE-LABEL CENTERED WIDTH 134 NO-BOX PAGE-TOP.

FORM
  l-Renglon             FORMAT ">>9"
  l-alm                 FORMAT "x(8)"
  l-CantPed             FORMAT "x(9)"
  l-Signo
  l-Guiones             FORMAT "x(8)"
  l-present             FORMAT "x(7)"
  DetPedido.id-articulo FORMAT "x(6)"
  l-Descr               FORMAT "x(71)"
  l-IVA                 FORMAT "x"
  l-ExistAlta
WITH OVERLAY CENTERED WIDTH 134 FRAME f-det DOWN NO-BOX NO-LABEL.

FORM
  l-Renglon FORMAT ">>9"
  l-LinEm0  FORMAT 'x(120)'
  WITH FRAME f-Emb0 OVERLAY NO-BOX DOWN NO-LABELS WIDTH 134.

FORM
  l-Renglon FORMAT ">>9"
  l-LinEm1  FORMAT 'x(120)'
  WITH FRAME f-Emb1 OVERLAY NO-BOX DOWN NO-LABELS WIDTH 134.

FORM
  l-Renglon FORMAT ">>9"
  l-LinEm2  FORMAT 'x(120)'
  WITH FRAME f-Emb2 OVERLAY NO-BOX DOWN NO-LABELS WIDTH 134.

FORM
  l-Renglon FORMAT ">>9"
  l-LinEm3  FORMAT 'x(120)'
  WITH FRAME f-Emb3 OVERLAY NO-BOX DOWN NO-LABELS WIDTH 134.

FORM HEADER
  FILL("=",132)                                     FORMAT "x(132)"
  "OBSERVACIONES:" + Pedido.Notas[1] FORMAT "x(96)"
  "              " + Pedido.Notas[2] FORMAT "x(96)" SKIP
  "Continua en Siguiente Pagina..." TO 132
  SKIP(1)
  l-Impresiones               AT 1 FORMAT "z9"
  Pedido.Id-Captura           AT 4 FORMAT "x(4)"
  l-Fecha_                      TO 123
  l-Hora_                       TO 132
  WITH FRAME f-pie1 OVERLAY SIDE-LABEL  CENTERED  WIDTH 134 NO-BOX PAGE-BOTTOM.

FORM HEADER
  CHR(15) FORMAT 'x' AT 1
  l-Mens1                                          FORMAT "X(100)"  TO 132
  FILL("=",132)                                    FORMAT "x(132)"
  "OBSERVACIONES:" Pedido.Notas[1]
  "              " Pedido.Notas[2] SKIP
  "________________  ________________  ________________  ________________  ________________  ________________  ________________"
  "    CREDITO             SURTIO            EMPACO           BULTOS            TARIMAS           REVISO            VENTAS"
  SKIP
  l-Impresiones                                    FORMAT "z9"
  Pedido.Id-Captura                                FORMAT "x(4)"
  l-Fecha_                      TO 123
  l-Hora_                       TO 132
  WITH FRAME f-pie2 OVERLAY SIDE-LABEL  CENTERED  WIDTH 134 NO-BOX PAGE-BOTTOM.

ASSIGN l-Corte3    = FALSE
       l-Corte4    = FALSE
       l-LocEsp    = FALSE
       l-Control   = FALSE
       l-SoloCorte = FALSE.
IF (NOT p-ImprimeElCompleto) OR (Pedido.Id-Alm BEGINS '1' AND
                                 Pedido.Id-Alm <> '11') THEN DO:
  RUN /usr2/adosa/procs/vtac0226.p(INPUT p-Pedido,
                                   INPUT p-Resto,
                                   INPUT-OUTPUT l-Corte3,
                                   INPUT-OUTPUT l-Corte4,
                                   INPUT-OUTPUT l-LocEsp).
  RUN /usr2/adosa/procs/vtac0227.p(INPUT p-Pedido,
                                   INPUT p-Resto,
                                   INPUT-OUTPUT l-SoloCorte).
  RUN /usr2/adosa/procs/vtac0117.p(INPUT p-Pedido,
                                   INPUT p-Resto,
                                   INPUT-OUTPUT l-Control).
END.

IF Pedido.Enviar = TRUE THEN
  FIND FIRST AlmPrior WHERE AlmPrior.Id-UbiVta = Pedido.Id-UbiVta
                        AND AlmPrior.Tipo = 1 NO-LOCK NO-ERROR.
ELSE
  FIND LAST AlmPrior WHERE AlmPrior.Id-UbiVta = Pedido.Id-UbiVta
                       AND AlmPrior.Tipo = 1 NO-LOCK NO-ERROR.

IF /*AlmPrior.Id-Alm <> '02B' OR*/ Pedido.Enviar = FALSE OR
   (Pedido.Enviar AND l-AreaC) THEN DO:
  FOR EACH B-DetPedido WHERE B-DetPedido.Id-Pedido = Pedido.Id-Pedido
                         AND B-DetPedido.Resto = Pedido.Resto
      USE-INDEX Idx-Loc NO-LOCK BREAK BY b-DetPedido.Id-Alm BY b-DetPedido.Reng:
    IF FIRST-OF(B-DetPedido.Id-Alm) AND
       LENGTH(TRIM(B-DetPedido.Id-Alm)) > 0 AND
       (l-SoloCorte = FALSE OR
        (l-SoloCorte = TRUE AND B-DetPedido.Id-Alm = "01E"))
    THEN DO:
      FIND FIRST AlmImp WHERE AlmImp.Documento = "Pedido    "       AND
                              AlmImp.Id-Alm    = B-DetPedido.Id-Alm AND
                              AlmImp.Id-UbiVta = Pedido.Id-UbiVta NO-LOCK NO-ERROR.
      IF NOT AVAILABLE AlmImp THEN DO:
        FIND FIRST AlmImp WHERE AlmImp.Documento = "Pedido    "
                            AND AlmImp.Id-Alm = B-DetPedido.Id-Alm NO-LOCK NO-ERROR.
      END.
      IF NOT p-ImprimeElCompleto THEN DO:
        IF AVAILABLE AlmImp THEN ASSIGN l-Completo = AlmImp.Completo.
                            ELSE ASSIGN l-Completo = TRUE.
        IF l-YaImprimiElCompleto AND l-Completo THEN NEXT.
      END.
      ELSE IF l-YaImprimiElCompleto THEN NEXT.

      OUTPUT STREAM s-Salida1 TO VALUE(l-archivo) PAGED PAGE-SIZE 32.
      PUT STREAM s-Salida1 CONTROL CHR(27) + CHR(15).

      IF Pedido.Id-Pedido BEGINS '2' AND (USERID('dictdb') <> 'MLLR' AND 
                                          USERID('dictdb') <> 'OLDS' AND 
                                          USERID('dictdb') <> 'DLGR') THEN
          VIEW STREAM s-Salida1 FRAME f-enc1.
      ELSE
          VIEW STREAM s-Salida1 FRAME f-enc1-1.

      {/usr2/adosa/includes/cieheadr.i  
        &AnchoM    = 128
        &Ancho     = 134
        &titulo    = "IF Pedido.Id-Vendedor <> '0100' THEN 'PEDIDO DE TELEMARKETING' ELSE 'PEDIDO DE INTERNET     *** ENTREGAR A MESAS 7 Y 8 ***'"
        &Subtitulo = "(IF Pedido.BckOrd = 1 THEN 'SOLO EXISTENCIAS'
                       ELSE IF Pedido.BckOrd = 2 THEN 'EXISTENCIAS + BO'
                       ELSE IF Pedido.BckOrd = 3 THEN 'SOLO COMPLETO'
                       ELSE 'COMPLETO O CANCELAR') + 
                      IF Pedido.Adelantado THEN '; FAC X ADEL' ELSE ''"
        &Aveces    = "IF Pedido.Enviar = FALSE THEN
                       ((IF p-ImprimeElCompleto
                         THEN (IF NOT AVAILABLE UbiVta
                               THEN 'TODOS LOS ALMACENES'
                               ELSE UbiVta.Descr)
                         ELSE 'ALMACEN ' + STRING(B-DetPedido.Id-Alm,'xxx')) +
                        (IF l-Completo THEN ' (COMPLETO)' ELSE '') + 
                        (IF Pedido.Especial THEN ' ' + 'ESPECIAL' ELSE ''))
                     ELSE ('AREA C') + ' ' + (IF Pedido.Id-Vendedor <> '0100'
                                              THEN (IF Pedido.Id-Entrega = 11
                                                    THEN '**** NUESTRA CAMIONETA ****'
                                                    ELSE IF Pedido.Id-Entrega = 16
                                                         THEN '**** PASA A CEDIS ****'
                                                         ELSE '**** PAQUETERIA ****')
                                              ELSE '')"
        &Stream    = s-Salida1
      }

      ASSIGN l-Renglon = 0.
      VIEW STREAM s-Salida1 FRAME f-enc2.
      IF Pedido.Id-Cliente = 3 THEN VIEW STREAM s-Salida1 FRAME f-enc3-1.
                               ELSE VIEW STREAM s-Salida1 FRAME f-enc3-2.
      VIEW STREAM s-Salida1 FRAME f-enc4.
      VIEW STREAM s-Salida1 FRAME f-pie1.

      IF Pedido.Enviar = FALSE OR AlmPrior.Id-Alm = '11' THEN DO:
        RUN vtac0220.p1.
      END.
      ELSE DO:
        RUN vtac0220.p2.
      END.

      HIDE STREAM s-Salida1 FRAME f-pie1.
      VIEW STREAM s-Salida1 FRAME f-pie2.

      IF l-Completo THEN DO:
            IF l-ConEmb > 0 THEN DO:
              IF l-ConEmb <> IF l-Renglon <= 11
              THEN (11 - l-Renglon)
              ELSE ((14 * (TRUNCATE((l-Renglon - 11) / 14,0) +
                   (IF (l-Renglon - 11) MODULO 14 = 0 THEN 0 ELSE 1))) -
                   (l-Renglon - 11)) THEN DO:
                ASSIGN l-Renglon = l-Renglon + 1.
                DISPLAY STREAM s-Salida1 l-Renglon l-LinEm0
                WITH FRAME f-Emb0.
              END.
              IF LENGTH(TRIM(l-LinEm1)) > 0 THEN DO:
                ASSIGN l-Renglon = l-Renglon + 1.
                DISPLAY STREAM s-Salida1 l-Renglon l-LinEm1
                WITH FRAME f-Emb1.
              END.
              IF LENGTH(TRIM(l-LinEm2)) > 0 THEN DO:
                ASSIGN l-Renglon = l-Renglon + 1.
                DISPLAY STREAM s-Salida1 l-Renglon l-LinEm2
                WITH FRAME f-Emb2.
              END.
              IF LENGTH(TRIM(l-LinEm3)) > 0 THEN DO:
                ASSIGN l-Renglon = l-Renglon + 1.
                DISPLAY STREAM s-Salida1 l-Renglon l-LinEm3
                WITH FRAME f-Emb3.
              END.
            END.
      END.

      DO l-Renglon = l-Renglon + 1 TO 11:
            DISPLAY STREAM s-Salida1
              l-Renglon
            WITH FRAME f-det.
            DOWN WITH FRAME f-det.
      END.

      IF AVAILABLE Cliente THEN DO:
            ASSIGN l-Limite = Cliente.Limite / 1000.
            FOR EACH MovCliente OF Cliente NO-LOCK:
              IF MovCliente.Id-MC = 2 THEN ASSIGN l-NC  = l-NC  + 1.
              IF MovCliente.Id-MC = 3 THEN DO:
                ASSIGN l-CD1 = l-CD1 + 1.
                IF MovCliente.Saldo > 0 THEN ASSIGN l-CD2 = l-CD2 + 1.
              END.
              IF MovCliente.Saldo > 0 THEN DO:
                ASSIGN l-SalTot= l-SalTot + MovCliente.Saldo.
                IF (MovCliente.Id-MC = 1 OR MovCLiente.Id-MC =2 OR
                    MovCLiente.Id-MC = 3) AND l-Fecha_ - MovCliente.FecVenc > 0
                THEN ASSIGN l-Venc = l-Venc + MovCliente.Saldo.
              END.
            END.
      END.

      PUT STREAM s-Salida1 CONTROL CHR(27) + CHR(15).
      OUTPUT STREAM s-Salida1 CLOSE.

      IF AVAILABLE AlmImp THEN DO:
        IF p-ImprimeElCompleto THEN DO:
          IF p-PedirImpresora THEN DO:
            DISPLAY 'Todos los Almacenes' FORMAT 'x(19)'
            WITH FRAME f-Impres1 CENTERED ROW 12 OVERLAY.
            IF Pedido.Id-Alm <> "11" THEN
              RUN /usr2/adosa/procs/vtac0202.p(INPUT l-archivo, 20, OUTPUT p-SiLoImprimi).
            ELSE
              RUN /usr2/adosa/procs/vtac0204.p(INPUT l-archivo, 20, OUTPUT p-SiLoImprimi).
            HIDE FRAME f-Impres1 NO-PAUSE.
            IF p-SiLoImprimi THEN RUN /usr2/adosa/procs/vtac0228.p(INPUT p-Pedido, INPUT p-Resto).
          END.
          ELSE DO:
            DISPLAY
              'Imprimiendo pedido...' FORMAT 'x(55)'
            WITH FRAME f-Impresora CENTERED ROW 12 OVERLAY.

            IF Pedido.Enviar = TRUE THEN
                FIND FIRST AlmPrior WHERE AlmPrior.Id-UbiVta = Pedido.Id-UbiVta
                                    AND AlmPrior.Tipo = 1 NO-LOCK NO-ERROR.
            ELSE
                FIND LAST AlmPrior WHERE AlmPrior.Id-UbiVta = Pedido.Id-UbiVta
                    AND AlmPrior.Tipo = 1 NO-LOCK NO-ERROR.

            IF Pedido.Enviar = FALSE THEN DO:
                IF Pedido.Id-Alm = "11" THEN
                    ASSIGN l-envia = 'lpsaemp ' + ' ' + l-archivo.
                ELSE
                    IF Pedido.Id-Pedido BEGINS '4' THEN
                        ASSIGN l-envia = 'lpr17 ' + ' ' + l-archivo.
                    ELSE
                        IF Pedido.Id-Pedido BEGINS '3' THEN
                            ASSIGN l-envia = 'lpr25 ' + ' ' + l-archivo.
                        ELSE
                            ASSIGN l-envia = 'lpchmos2 ' + ' ' + l-archivo.                  
            END.
            ELSE DO:
                IF Pedido.Id-Alm = "11" THEN
                    ASSIGN l-envia = 'lpsaemp ' + ' ' + l-archivo.
                ELSE 
                    IF l-AreaD = FALSE THEN
                        ASSIGN l-envia = 'lpe05 ' + ' ' + l-archivo.
                    ELSE
                        ASSIGN l-envia = 'lpe06 ' + ' ' + l-archivo.
            END.

            RUN /usr2/adosa/procs/vtaa0001.p(INPUT l-Archivo). /* Quita line-Feed */
            MESSAGE "Imprimiendo archivo.....".
            UNIX SILENT VALUE(l-envia).
            PAUSE 2.
            HIDE MESSAGE NO-PAUSE.
            HIDE FRAME f-Impresora NO-PAUSE.
            RUN /usr2/adosa/procs/vtac0228.p(INPUT p-Pedido, INPUT p-Resto).
          END.
          ASSIGN l-YaImprimiElCompleto = TRUE.
        END.   
        ELSE DO:
          IF LENGTH(TRIM(p-EnElAlmacen)) > 0 THEN DO:
            IF TRIM(AlmImp.Id-Alm) = TRIM(p-EnElAlmacen) THEN DO:
              IF p-PedirImpresora THEN DO:
                    DISPLAY
                      'Destino de la Impresion del Almacen ' +
                      AlmImp.Id-Alm FORMAT 'x(45)'
                    WITH FRAME f-Impres3 CENTERED ROW 12 OVERLAY.
                    IF Pedido.Id-Alm <> "11" THEN
                      RUN /usr2/adosa/procs/vtac0202.p(INPUT l-archivo, 20, OUTPUT p-SiLoImprimi).
                    ELSE
                      RUN /usr2/adosa/procs/vtac0204.p(INPUT l-archivo, 20, OUTPUT p-SiLoImprimi).
                    HIDE FRAME f-Impres3 NO-PAUSE.
                    IF p-SiLoImprimi THEN
                      RUN /usr2/adosa/procs/vtac0228.p(INPUT p-Pedido, INPUT p-Resto).
              END.
              ELSE DO:
                  DISPLAY
                     'Destino de la Impresion del Almacen ' +
                     AlmImp.Id-Alm + ': ' + AlmImp.Id-Imp FORMAT 'x(45)'
                  WITH FRAME f-Impres4 CENTERED ROW 12 OVERLAY.
                  FIND Impresoras OF AlmImp NO-LOCK NO-ERROR.
                  IF AVAILABLE Impresoras THEN DO:
                      IF Pedido.Enviar = FALSE THEN DO:
                          IF pedido.id-pedido BEGINS '3' THEN DO:
                              ASSIGN l-envia = 'lpr25 ' + ' ' + l-archivo.
                          END.
                          ELSE DO:
                              ASSIGN l-envia = TRIM(Impresoras.Comando) + ' ' + l-archivo.
                          END.
                      END.
                      ELSE DO:
                        IF l-AreaD = FALSE THEN
                          ASSIGN l-envia = TRIM(Impresoras.Comando) + ' ' + l-archivo.
                        ELSE
                          ASSIGN l-envia = 'lpe06 ' + ' ' + l-archivo.
                      END.
    
                      RUN /usr2/adosa/procs/vtaa0001.p(INPUT l-Archivo). /* Quita line-Feed */
                      MESSAGE "Imprimiendo archivo.....".
                      UNIX SILENT VALUE(l-envia).
                      PAUSE 2.
                      HIDE MESSAGE NO-PAUSE.
                      RUN /usr2/adosa/procs/vtac0228.p(INPUT p-Pedido, INPUT p-Resto).
                    END.
                    HIDE FRAME f-Impres4 NO-PAUSE.
              END.
            END.
          END.
          ELSE DO:
            DISPLAY
              'Destino de la Impresion del Almacen ' +
              AlmImp.Id-Alm + ': ' + AlmImp.Id-Imp FORMAT 'x(45)'
            WITH FRAME f-Impres2 CENTERED ROW 12 OVERLAY.
            FIND Impresoras OF AlmImp NO-LOCK NO-ERROR.
            IF AVAILABLE Impresoras THEN DO:

              IF Pedido.Enviar = FALSE THEN
                    ASSIGN l-envia = TRIM(Impresoras.Comando) + ' ' + l-archivo.
              ELSE DO:
                    IF l-AreaD = FALSE THEN
                ASSIGN l-envia = TRIM(Impresoras.Comando) + ' ' + l-archivo.
                    ELSE
                ASSIGN l-envia = 'lpe06 ' + ' ' + l-archivo.
              END.

              RUN /usr2/adosa/procs/vtaa0001.p(INPUT l-Archivo). /* Quita line-Feed */
              MESSAGE "Imprimiendo archivo.....".
              UNIX SILENT VALUE(l-envia).
              PAUSE 2.
              HIDE MESSAGE NO-PAUSE.
              RUN /usr2/adosa/procs/vtac0228.p(INPUT p-Pedido, INPUT p-Resto).
            END.
            HIDE FRAME f-Impres2 NO-PAUSE.
          END.
        END.
      END.
    END. /* IF FIRST-OF */
  END. /* FOR EACH b-DetPedido */
END.

IF Pedido.Id-Alm <> "11" THEN DO:
    /* IMPRIME TODOS LOS TIPOS DE PEDIDO */
  IF l-Corte3 THEN DO:
    RUN /usr2/adosa/procs/vtac0210.p(INPUT p-Pedido, INPUT p-Resto, INPUT 3, INPUT l-Impresiones,
                   INPUT p-EnElAlmacen,INPUT "").
  END.
  IF l-Corte4 THEN DO:
    RUN /usr2/adosa/procs/vtac0210.p(INPUT p-Pedido, INPUT p-Resto, INPUT 4, INPUT l-Impresiones,
                   INPUT p-EnElAlmacen, INPUT "").
  END.
  IF l-LocEsp AND Pedido.Enviar = TRUE THEN DO:
    RUN /usr2/adosa/procs/vtac0225.p(INPUT p-Pedido, INPUT p-Resto, INPUT l-Impresiones,
                   INPUT p-EnElAlmacen,"").
  END.
  IF l-Control AND Pedido.Enviar = TRUE THEN DO:
    RUN /usr2/adosa/procs/vtac0224.p(INPUT p-Pedido, INPUT p-Resto, INPUT l-Impresiones,
                   INPUT p-EnElAlmacen,"").
  END.
  IF l-AreaCup /*AND Pedido.Enviar = TRUE*/ THEN DO:
    FOR EACH DetPedido WHERE DetPedido.Id-Pedido = p-Pedido
                         AND DetPedido.Resto = p-Resto
                         AND DetPedido.Id-Articulo <> ''                                 
                         AND SUBSTRING(DetPedido.Id-Loc,1,4) >= '2C16'
                         AND SUBSTRING(DetPedido.Id-Loc,1,4) <= '2C26'
                         AND DetPedido.Tipo = 1
                         AND DetPedido.TpoCorte = 0 NO-LOCK,
        FIRST ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
                        AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK:
        FIND FIRST DetTarea WHERE DetTarea.Id-Pedido = DetPedido.Id-Pedido
                            AND DetTarea.Resto = DetPedido.Resto
                            AND DetTarea.Id-Articulo = DetPedido.Id-Articulo
                            AND DetTarea.Id-Color = DetPedido.Id-color NO-LOCK NO-ERROR.
        IF NOT AVAILABLE DetTarea THEN DO TRANSACTION:
            RUN /usr2/adosa/procs/vtac0116.p(INPUT p-Pedido, INPUT p-Resto, INPUT TRUE, INPUT p-EnElAlmacen).
            LEAVE.
        END.
        ELSE DO TRANSACTION:
            FOR EACH DetTarea WHERE DetTarea.Id-Pedido = DetPedido.Id-Pedido
                                            AND DetTarea.Resto = DetPedido.Resto
                                            AND DetTarea.Id-Articulo = DetPedido.Id-Articulo
                                            AND DetTarea.Id-Color = DetPedido.Id-Color NO-LOCK,
                    FIRST B-AP WHERE b-AP.Id-Articulo = DetTarea.Id-Articulo
                                     AND b-AP.Id-Pres = DetTarea.Id-Pres NO-LOCK:
                    ACCUMULATE (DetTarea.Cant * b-AP.Equiv) (TOTAL).
            END.
            IF (ACCUM TOTAL (DetTarea.Cant * b-AP.Equiv)) <>
               DetPedido.CantPed * ArtPres.Equiv THEN DO:
                RUN /usr2/adosa/procs/vtac0116.p(INPUT p-Pedido, INPUT p-Resto, INPUT TRUE, INPUT p-EnElAlmacen).
                LEAVE.
            END.
       END.
    END.    
  END.
END.  


PROCEDURE vtac0220.p1.
  FOR EACH DetPedido WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido
                               AND DetPedido.Resto = Pedido.Resto
                               AND ((l-Completo = TRUE) OR (l-Completo = FALSE AND  DetPedido.Id-Alm = B-DetPedido.Id-Alm))
                     USE-INDEX Idx-Reng NO-LOCK:
    {/usr2/adosa/includes/vtac0220.i} 
  END.                           
END.
PROCEDURE vtac0220.p2.
  FOR EACH DetPedido WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido
                               AND DetPedido.Resto = Pedido.Resto
                               AND ((l-Completo = TRUE) OR (l-Completo = FALSE AND DetPedido.Id-Alm = B-DetPedido.Id-Alm))
                             NO-LOCK BY DetPEdido.Secuencia BY DetPedido.Reng:
    {/usr2/adosa/includes/vtac0220.i}    
  END.
END.
