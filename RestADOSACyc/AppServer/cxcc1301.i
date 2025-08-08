/*
  Empresa : Consultoria en Informatica Ejecutiva
  Sistema : ADOSA
  Modulo  : Cuentas por Pagar
  Programa: Cxcc0320.i
  Funcion : Variables del programa Cxcc0320.p
  Autor   : LUIS
  Fecha   : 05/12/96
  Modificacion: RNPC - 209-07-25 - Ajustes para convertir montos de dolares a pesos
*/

DEF STREAM s-salida.
DEF NEW SHARED VAR s-lista AS CHAR                                      NO-UNDO.
DEF BUFFER b-mov   FOR MovCliente.
DEF BUFFER bb-mov  FOR MovCliente.
DEF BUFFER bf-mov  FOR MovCliente.
DEF BUFFER bf-hist FOR HistMovCte.
DEF BUFFER b-hist  FOR HistMovCte.
DEF TEMP-TABLE w-saldo LIKE Saldo
    FIELD Acomodo  AS CHAR
    FIELD Acomodo2 AS CHAR
    FIELD Acomodo3 AS DECIMAL.
DEF VAR l-diascartera AS INTE FORMAT "zzz9.9"                           NO-UNDO.
DEF VAR l-dc          AS INTE FORMAT "zzz9.9"                           NO-UNDO.
DEF VAR l-salxant  AS DECI				NO-UNDO.
DEF VAR l-saltotal AS DECI				NO-UNDO.
DEF VAR l-tsalxant    AS DECIMAL     NO-UNDO.    /*para opcion solo +dias*/
DEF VAR l-tsaltotal   AS DECIMAL     NO-UNDO.
DEF VAR l-tsalxant2    AS DECIMAL     NO-UNDO. /*para opcion todos*/
DEF VAR l-tsaltotal2   AS DECIMAL     NO-UNDO.
DEF VAR l-casilla AS INTE                                                NO-UNDO.
DEF VAR l-dias    AS INTE                                               NO-UNDO.
DEF VAR l-diascap AS INTE FORMAT "zz9"             LABEL "Dias"         NO-UNDO.
DEF VAR l-diascap2 AS INTE FORMAT "zz9"             LABEL "Dias"         NO-UNDO. /*para guardar el valor original de l-diascap*/
DEF VAR l-totcli  AS INTE FORMAT "zzz9"                                        NO-UNDO.
DEF VAR l-totclih AS INTE FORMAT "z9"                                        NO-UNDO.
DEF VAR l-nc      AS INTE                                               NO-UNDO.
DEF VAR l-i       AS INTE                                               NO-UNDO.
DEF VAR l-indice  AS INTE FORMAT "9" INITIAL 1                          NO-UNDO.
DEF VAR l-largo   AS INTE                             INITIAL 20        NO-UNDO.
DEF VAR l-agregado AS CHAR                                  NO-UNDO.
DEF VAR l-prompag AS INTE                                                NO-UNDO.
DEF VAR l-pp      AS INTE                                                NO-UNDO.
DEF VAR l-impxant  AS DECI				NO-UNDO.
DEF VAR l-imptotal AS DECI				NO-UNDO.
DEF VAR l-timpxant    AS DECIMAL     NO-UNDO.    /*para opcion solo +dias*/
DEF VAR l-timptotal   AS DECIMAL     NO-UNDO.
DEF VAR l-timpxant2    AS DECIMAL     NO-UNDO. /*para opcion todos*/
DEF VAR l-timptotal2   AS DECIMAL     NO-UNDO.
DEF VAR l-pagina  AS INTE                                                NO-UNDO.
DEF VAR l-chedev  AS INTE FORMAT "99"                                   NO-UNDO.
DEF VAR l-saldo   AS DECI FORMAT "-zzz,zzz,zz9.99"  LABEL "Saldo"              NO-UNDO.
DEF VAR l-movsal  AS DECI                                                NO-UNDO.
DEF VAR l-simbolo       LIKE Moneda.simbolo                INITIAL ""  NO-UNDO. // RNPC - 2019-07-23

/* variables para totales por hoja */
DEF VAR l-diascarterah AS INTE FORMAT "zzz9.99"                                NO-UNDO.
DEF VAR l-totcorteh AS INTE                                                 NO-UNDO.
DEF VAR l-enca      AS CHAR FORMAT "x(11)"                     NO-UNDO.
DEF VAR l-saldocalh AS DECI FORMAT "-zzz,zzz,zz9.99" LABEL "Saldo"     NO-UNDO.
DEF VAR l-90diash  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Dias"     NO-UNDO.
DEF VAR l-vtamesh  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Vta. Mes" NO-UNDO.
DEF VAR l-pagmesh  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Pag. Mes" NO-UNDO.
DEF VAR l-vtasemh  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Vta. Sem" NO-UNDO.
DEF VAR l-pagsemh  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Pag. Sem" NO-UNDO.
DEF VAR l-vtaanoh  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Vta. Ano" NO-UNDO.
DEF VAR l-paganoh  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Pag. Ano" NO-UNDO.
DEF VAR l-cargosh  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Cargos" NO-UNDO.


DEF VAR l-saldocal AS DECI FORMAT "-zzz,zzz,zz9.99" LABEL "Saldo"  NO-UNDO.
DEF VAR l-90dias  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Dias"      NO-UNDO.
DEF VAR l-vtames  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Vta. Mes"   NO-UNDO.
DEF VAR l-pagmes  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Pag. Mes"   NO-UNDO.
DEF VAR l-vtasem  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Vta. Sem"   NO-UNDO.
DEF VAR l-pagsem  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Pag. Sem"   NO-UNDO.
DEF VAR l-vtaano  AS DECI FORMAT "-Zzz,zzz,zz9"   LABEL "Vta. Ano"   NO-UNDO.
DEF VAR l-pagano  AS DECI FORMAT "-Zzz,zzz,zz9"   LABEL "Pag. Ano"   NO-UNDO.
DEF VAR l-cargos  AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Cargos  "   NO-UNDO.

DEF VAR l-EXsaldo  AS DECI FORMAT "-zzz,zzz,zz9.99" LABEL "Saldo" NO-UNDO.
DEF VAR l-EX90dias AS DECI FORMAT "-zzz,zzz,zz9" LABEL "Dias"     NO-UNDO.
DEF VAR l-EXvtames AS DECI FORMAT "-zzz,zzz,zz9" LABEL "Vta. Mes" NO-UNDO.
DEF VAR l-EXcargos AS DECI FORMAT "-zzz,zzz,zz9" LABEL "Cargos  " NO-UNDO.
DEF VAR l-EXpagmes AS DECI FORMAT "-zzz,zzz,zz9" LABEL "Pag. Mes" NO-UNDO.
DEF VAR l-EXvtasem AS DECI FORMAT "-zzz,zzz,zz9" LABEL "Vta. Sem" NO-UNDO.
DEF VAR l-EXpagsem AS DECI FORMAT "-zzz,zzz,zz9" LABEL "Pag. Sem" NO-UNDO.
DEF VAR l-EXtsalxant   AS DECIMAL     NO-UNDO.    /*para opcion solo +dias*/
DEF VAR l-EXtsaltotal  AS DECIMAL     NO-UNDO.
DEF VAR l-EXtimpxant   AS DECIMAL     NO-UNDO.    /*para opcion solo +dias*/
DEF VAR l-EXtimptotal  AS DECIMAL     NO-UNDO.
DEF VAR l-EXtsalxant2  AS DECIMAL     NO-UNDO. /*para opcion todos*/
DEF VAR l-EXtsaltotal2 AS DECIMAL     NO-UNDO.
DEF VAR l-EXtimpxant2  AS DECIMAL     NO-UNDO. /*para opcion todos*/
DEF VAR l-EXtimptotal2 AS DECIMAL     NO-UNDO.
DEF VAR l-EXCount      AS INTEGER     NO-UNDO.
DEF VAR l-EXtotcli     AS INTEGER FORMAT "zzz9" NO-UNDO.



DEF VAR l-saldot  AS DECI FORMAT "-zzz,zzz,zz9.99"  LABEL "Saldo"      NO-UNDO.
DEF VAR l-90diast AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Dias"      NO-UNDO.
DEF VAR l-vtamest AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Vta. Mes"  NO-UNDO.
DEF VAR l-pagmest AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Pag. Mes"  NO-UNDO.
DEF VAR l-vtasemt AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Vta. Sem"  NO-UNDO.
DEF VAR l-pagsemt AS DECI FORMAT "-zzz,zzz,zz9"   LABEL "Pag. Sem"  NO-UNDO.
DEF VAR l-vtaanot AS DECI FORMAT "-zzz,zzz,zzz9"   LABEL "Vta. Ano" NO-UNDO.
DEF VAR l-paganot AS DECI FORMAT "-zzz,zzz,zzz9"   LABEL "Pag. Ano"  NO-UNDO.
DEF VAR l-cargost AS DECI FORMAT "-zzz,zzz,zzz9"   LABEL "Cargos"  NO-UNDO.

DEFINE VARIABLE v-anticipo AS INTEGER NO-UNDO.                               

DEF VAR l-entro   AS LOGI                                       NO-UNDO.
DEF VAR l-cheques AS CHAR FORMAT "x(5)"                         NO-UNDO.
DEF VAR l-reporte AS CHAR                                       NO-UNDO.
DEF VAR l-fecha          AS DATE FORMAT "99/99/9999"            NO-UNDO.
DEF VAR l-fechaini AS DATE FORMAT "99/99/9999"                  NO-UNDO.
DEF VAR l-meses   AS CHAR FORMAT "x(12)"    EXTENT 12       INITIAL
    ["Enero",        "Febrero",      "Marzo",        "Abril",
     "Mayo",         "Junio",        "Julio",        "Agosto",
     "Septiembre",   "Octubre",      "Noviembre",    "Diciembre"]   NO-UNDO.
DEF VAR l-SumaNoDep AS INT FORMAT '-zzz9' NO-UNDO.
DEF VAR l-Importe LIKE Pagoacuse.Importe NO-UNDO.     
ASSIGN l-pagina = 9999.

FORM
   MovCliente.Id-Cliente
   Cliente.RazonSocial      FORMAT "x(23)"
   l-saldo
   l-90dias
   l-vtames
   l-Cargos
   l-pagmes
   l-vtasem
   l-pagsem
   /* l-vtaano
   l-pagano  */      SKIP
   Cliente.Id-Zona          FORMAT "999"
   Cliente.Id-Resp          FORMAT "99"
   Cliente.Plazo            FORMAT "z99"
   Cliente.Limite           FORMAT "zzzzzz9"
   l-cheques
   l-diascartera            FORMAT "zzz9"
   l-prompag                FORMAT ">>9"
   Cliente.FactorDesc
   l-SumaNoDep
   l-simbolo FORMAT "x(2)"  AT 55
   "AP:"         AT 87
   v-anticipo    FORMAT "ZZZZ,ZZ9" AT  91
   Cliente.MontoPagare TO 126              
WITH FRAME f-x OVERLAY CENTERED WIDTH 148 DOWN NO-LABEL.

FORM HEADER
  "     "
WITH FRAME f-pie OVERLAY CENTERED PAGE-BOTTOM.

FORM
    /*"D.C.  " AT 4 l-diascarterah*/ SKIP(1)
    "TOTAL de la Hoja" 
    l-saldocalh AT 32
    l-90diash
    l-vtamesh
    l-cargosh
    l-pagmesh
    l-vtasemh
    l-pagsemh
WITH FRAME f-tothoja OVERLAY CENTERED WIDTH 148 NO-LABELS.

FORM HEADER
   "  Cte  Nombre                        Saldo Ini" AT 1
   l-enca FORMAT "x(14)" AT 47   
   "Ventas       Cargos        Pagos     Creditos " AT 67
   "Saldo Final"  AT 114
   SKIP
   " ZO  RE  PZ   LC-D CH.D    DC DPP FacDsc   ChND   TM                                                              MONTO PAGARE"
   SKIP
   "------------------------------------------------------------------------------------------------------------------------------"

WITH FRAME f-enca OVERLAY CENTERED WIDTH 148 NO-LABEL PAGE-TOP. // RNPC - 2019-07-22
