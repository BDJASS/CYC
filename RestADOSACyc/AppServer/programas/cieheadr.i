/*
   Empresa : Consultoria en Informatica Ejecutiva
   Sistema : Ambiente General para el Desarrollador
   Modulo  : General
   Programa: cieheadr.i
   Usado por: ejemplo2.t
   Funcion :  Centrar el encabezado.

   Autor   : J.A.Z.V
   Fecha   : 21/Sept/1994

   Bitacora:  Iniciales  Descripcion General del Cambio
              ---------  ------------------------------
                 INI     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
*/
  /****************************************************************************/
  /*   &anchom =  Se refiera al ancho de la primera linea del encabezado el   */
  /*              cual debe ser menor a {&ancho} por 6 unidades.              */
  /*   &Ancho =   Numero de columnas que ocupara el Reporte.                  */
  /*   &Titulo =  Titulo de la tercera linea del encabezado.                  */
  /*   &Stream =  Nombre del Stream.                                          */
  /*   &NoStream = Se refiere a cuando no se quiere que ponga las opciones de */
  /*               a pantalla, a disco, a impresora.                          */
 /*Declaracion de las Variables del Encabezado*/

{ifndef {&NoVars}}  
DEF VAR l-enc1       AS CHAR .
DEF VAR l-enc2       AS CHAR .
DEF VAR l-enc3       AS CHAR .
DEF VAR l-enc4       AS CHAR .
DEF VAR l-enc5       AS CHAR .
DEF VAR l AS INT.
{endif} */
/*Declaracion de las Variables que seran utilizadas para centrar el encabezado*/
{ifndef {&NoVars}} 
     {ciedef.i} {endif} */
  /*   
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
DEF VAR l-esp        AS CHAR FORMAT "x(5)" INITIAL "     ".  */ 
     
DEFINE  VARIABLE g-nomcia   AS CHARACTER  NO-UNDO.   
DEFINE  VARIABLE g-dist   LIKE sysgeneral.Dist NO-UNDO.
/* **********************  Internal Procedures  *********************** */
FIND FIRST SysGeneral NO-LOCK NO-ERROR.
IF AVAILABLE SysGeneral THEN ASSIGN g-nomcia = SysGeneral.Empresa .

FIND FIRST SysGeneral NO-LOCK NO-ERROR.
IF AVAILABLE SysGeneral THEN ASSIGN g-dist = sysgeneral.Dist .
 
/* Checa que no se pueda mandar un ancho de reporte menor a 80 */
IF {&ancho} < 80 THEN   
DO:
  BELL.
  RETURN.
END.

l-hora = STRING(TIME,"HH:MM:SS").
l-fechanva = STRING(TODAY,"99/99/99").


/* Centra La Primera Linea del Encabezado*/
l-libres1 = {&ancho} - 27.
l-espenc = (l-libres1 - LENGTH(g-nomcia)) / 2.
l-enc1 = l-fech + l-fechanva + FILL(' ',INTEGER(l-espenc)) + g-nomcia.
l-long = {&anchom} - (LENGTH(l-enc1)).
l-enc1 = l-enc1 + FILL(' ',l-long - 4) + l-pag.

/*Centra la Segunda Linea del Encabezado*/
l-libres2 = {&ancho} - 27.
IF LENGTH({&titulo}) MODULO 2 <> 0 THEN
  l-long = 1.
l-espenc = (l-libres2 - LENGTH({&titulo})) / 2.

l-enc2 = l-hor + l-hora + FILL(' ',INTEGER(l-espenc)) + {&titulo}.
l-long = ({&ancho} - 2 ) - LENGTH(l-enc2).
l-enc2 = l-enc2 + FILL(' ',l-long - 8)
        + IF PROGRAM-NAME(1) = ? THEN "        "
          ELSE IF R-INDEX(PROGRAM-NAME(1),"/") = 0 THEN
                     SUBSTRING(PROGRAM-NAME(1),1,8)
               ELSE  SUBSTRING(PROGRAM-NAME(1),
                     R-INDEX(PROGRAM-NAME(1),"/") + 1,8).

/*Centra la Tercera Linea del Encabezado*/
l-libres3 = {&ancho} - 27.
l-subenc = (l-libres3 - LENGTH({&Subtitulo})) / 2.
l-long = LENGTH({&Subtitulo}) MOD 2.
IF l-long = 0 THEN l-long = 6. ELSE l-long = 7.

IF LENGTH(STRING(g-dist)) > 1 THEN
   l-long = l-long - (LENGTH(STRING(g-dist)) - 1).

l-enc3 = /*l-dist + STRING(g-dist)*/
         "ADOSA   " + FILL(' ',l-long)
                                 + FILL(' ',INTEGER(l-subenc)) + {&Subtitulo}.
l-long = {&Ancho} - length(l-enc3).
l-long = l-long - LENGTH(USERID("DictDB")).
l-enc3 = l-enc3 + FILL(' ',(l-long - 2)) + USERID("DictDB").

   l = 3.
/*Cuarta linea del encabezado*/
 {ifdef {&Aveces}}
   IF LENGTH({&Aveces}) MODULO 2 <> 0 THEN
    l = l - 1.
   ELSE l = l - 2.
   IF LENGTH({&Aveces}) < ({&ancho} - 27) THEN DO: /***/
      l-libres3 = INTEGER(({&ancho} - LENGTH({&Aveces})) / 2).
      l-enc4 = FILL(' ',l-libres3 + l) + {&Aveces} + FILL(' ',l-libres3 + l).
   END.
    ELSE DO:                            /****/
      l-libres3 = INTEGER(({&anchom} - 2 - LENGTH({&Aveces})) / 2).
      l-enc4 = FILL(' ',l-libres3) + {&Aveces} + FILL(' ',l-libres3).
    END.
  {else} */
    l-libres3 = {&ancho} - 27.
    l-subenc  = (l-libres3 - LENGTH({&Subtitulo})) / 2.
    l-enc3    =
    /*l-dist + STRING(g-dist)*/
    "ADOSA   " + l-esp + FILL(' ',INTEGER(l-subenc)) +
                 FILL(' ',LENGTH({&Subtitulo})).
    l-long = {&Ancho} - length(l-enc3).
    l-long = l-long - LENGTH(USERID("DictDB")).
    l-enc3 = l-enc3 + FILL(' ',(l-long - 2)) + USERID("DictDB").

   IF LENGTH({&Subtitulo}) MODULO 2 = 0 THEN
      l-long = 0.
      ELSE l-long = 1.
      IF LENGTH({&Subtitulo}) < ({&ancho} - 27) THEN DO: /***/
         l-libres3 = {&ancho} - 27.
         l-espenc = (l-libres3 - LENGTH({&Subtitulo})) / 2.
         l-enc4 = FILL(' ',15) +
                  FILL(' ',l-espenc + l-long) + {&Subtitulo}
                 + FILL(' ',l-espenc + l).
      END.
       ELSE DO:                                                  /****/
         l-espenc = ({&ancho} - LENGTH({&Subtitulo})) / 2.
         l-enc4 = FILL(' ',l-espenc) + {&Subtitulo} + FILL(' ',l-espenc + l).
       END.                                                       /****/
  {endif} */

   l-enc5 = FILL('-',({&ancho} - 2)).


FORM HEADER
  l-enc1 FORMAT "x({&anchom})" 
    {ifndef {&noStream}}
          PAGE-NUMBER ({&stream})
          {else} */  
          PAGE-NUMBER
          {endif} */
         FORMAT ">>>9"        SKIP
  l-enc2 FORMAT "x({&ancho})" SKIP
  l-enc3 FORMAT "x({&ancho})" SKIP
  l-enc4 FORMAT "x({&ancho})" SKIP
  l-enc5 FORMAT "x({&ancho})"
  WITH PAGE-TOP FRAME f-encrep{&Seq} NO-BOX WIDTH {&ancho} SIDE-LABELS OVERLAY.
 VIEW  {ifndef {&NoStream}} STREAM
   {&stream} {endif}*/ FRAME f-encrep{&seq}.
