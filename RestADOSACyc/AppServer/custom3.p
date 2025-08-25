@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : custom3.p
    Purpose     : 

    Syntax      : Analisis de Saldos

    Description : /RepAnalisisSaldos

    Author(s)   : sis10
    Created     : Wed Mar 05 23:37:48 CST 2025
    Notes       :
  ----------------------------------------------------------------------
 Mod: JASS03062025
 El reporte se realizo igual al que utilizan en putty, pero se solicito
 modificar la columna numero 4 que dice Plazo90 ahi registra el saldo
 segun la fecha registro, pero ahora sera segun fecha vencimiento
 Ticket 825 AdosaERP_Mantenimiento_CyC
 ----------------------------------------------------------------------
 Mod: JASS06062025
  Se requiere que se puedan enviar mas de 1 Zona en el reporte
  en caso de que aplique.
  se cambia el tipo de parametro a char para realizar la busqueda
  con las zonas que envien.
 Ticket 886 AdosaERP_Mantenimiento_CyC  
  ----------------------------------------------------------------------
 Mod: JASS27062025
  Se requiere agregar el Input para que busque x Responsable
  Se requiere que se quite el parametro Clase 
  Se dejara el parametro Clase se ocultara por la vista en el Front
  el servicio si no mandan parametro busca todos sin filtrar clase
  o si manda el front 0
 Ticket 1090 AdosaERP_Mantenimiento_CyC  
 
  Mod: JASS04072025
  Analisis de Saldos Agregar Margen Promedio del Cliente
 Ticket 1146 AdosaERP_Mantenimiento_CyC 
 
 --------------------------------------------------------------------
 
 Ticket 1212  Agregar el Filtro Cobrador
 JASS14072025
 
 TICKET 76  Integrar una columna con los Importes
            De los depositos santander (creditos) de los
            clientes que esten en el Reporte.
            JASS22082025
  */

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */



DEF STREAM s-salida.
DEF NEW SHARED VAR      s-lista        AS CHAR    NO-UNDO.

DEF            VAR      l-diascartera  AS INTE    FORMAT "zzz9.9" NO-UNDO.
DEF            VAR      l-dc           AS INTE    FORMAT "zzz9.9" NO-UNDO.
DEF            VAR      l-salxant      AS DECI    NO-UNDO.
DEF            VAR      l-saltotal     AS DECI    NO-UNDO.
DEF            VAR      l-tsalxant     AS DECIMAL NO-UNDO.    /*para opcion solo +dias*/
DEF            VAR      l-tsaltotal    AS DECIMAL NO-UNDO.
DEF            VAR      l-tsalxant2    AS DECIMAL NO-UNDO. /*para opcion todos*/
DEF            VAR      l-tsaltotal2   AS DECIMAL NO-UNDO.
DEF            VAR      l-casilla      AS INTE    NO-UNDO.
DEF            VAR      l-dias         AS INTE    NO-UNDO.
DEF            VAR      l-diascap      AS INTE    FORMAT "zz9" LABEL "Dias" NO-UNDO.
DEF            VAR      l-diascap2     AS INTE    FORMAT "zz9" LABEL "Dias" NO-UNDO. /*para guardar el valor original de l-diascap*/
DEF            VAR      l-totcli       AS INTE    FORMAT "zzz9" NO-UNDO.
DEF            VAR      l-totclih      AS INTE    FORMAT "z9" NO-UNDO.
DEF            VAR      l-nc           AS INTE    NO-UNDO.
DEF            VAR      l-i            AS INTE    NO-UNDO.
DEF            VAR      l-indice       AS INTE    FORMAT "9" INITIAL 1 NO-UNDO.
DEF            VAR      l-largo        AS INTE    INITIAL 20 NO-UNDO.
DEF            VAR      l-agregado     AS CHAR    NO-UNDO.
DEF            VAR      l-prompag      AS DECI    FORMAT "-zzz,zzz,zz9.99" NO-UNDO.
DEF            VAR      l-pp           AS INTE    NO-UNDO.
DEF            VAR      l-impxant      AS DECI    NO-UNDO.
DEF            VAR      l-imptotal     AS DECI    NO-UNDO.
DEF            VAR      l-timpxant     AS DECIMAL NO-UNDO.    /*para opcion solo +dias*/
DEF            VAR      l-timptotal    AS DECIMAL NO-UNDO.
DEF            VAR      l-timpxant2    AS DECIMAL NO-UNDO. /*para opcion todos*/
DEF            VAR      l-timptotal2   AS DECIMAL NO-UNDO.
DEF            VAR      l-pagina       AS INTE    NO-UNDO.
DEF            VAR      l-chedev       AS INTE    FORMAT "99" NO-UNDO.
DEF            VAR      l-saldo        AS DECI    FORMAT "-zzz,zzz,zz9.99" LABEL "Saldo" NO-UNDO.
DEF            VAR      l-movsal       AS DECI    NO-UNDO.
DEF            VAR      l-simbolo      LIKE Moneda.simbolo INITIAL "" NO-UNDO. // RNPC - 2019-07-23

/* variables para totales por hoja */      
DEF            VAR      l-diascarterah AS INTE    FORMAT "zzz9.99" NO-UNDO.
DEF            VAR      l-totcorteh    AS INTE    NO-UNDO.
DEF            VAR      l-enca         AS CHAR    FORMAT "x(11)" NO-UNDO.
DEF            VAR      l-saldocalh    AS DECI    FORMAT "-zzz,zzz,zz9.99" LABEL "Saldo" NO-UNDO.
DEF            VAR      l-90diash      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Dias" NO-UNDO.
DEF            VAR      l-vtamesh      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Vta. Mes" NO-UNDO.
DEF            VAR      l-pagmesh      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Pag. Mes" NO-UNDO.
DEF            VAR      l-vtasemh      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Vta. Sem" NO-UNDO.
DEF            VAR      l-pagsemh      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Pag. Sem" NO-UNDO.
DEF            VAR      l-vtaanoh      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Vta. Ano" NO-UNDO.
DEF            VAR      l-paganoh      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Pag. Ano" NO-UNDO.
DEF            VAR      l-cargosh      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Cargos" NO-UNDO.


DEF            VAR      l-saldocal     AS DECI    FORMAT "-zzz,zzz,zz9.99" LABEL "Saldo" NO-UNDO.
DEF            VAR      l-90dias       AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Dias" NO-UNDO.
DEF            VAR      l-vtames       AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Vta. Mes" NO-UNDO.
DEF            VAR      l-pagmes       AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Pag. Mes" NO-UNDO.
DEF            VAR      l-vtasem       AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Vta. Sem" NO-UNDO.
DEF            VAR      l-pagsem       AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Pag. Sem" NO-UNDO.
DEF            VAR      l-vtaano       AS DECI    FORMAT "-Zzz,zzz,zz9" LABEL "Vta. Ano" NO-UNDO.
DEF            VAR      l-pagano       AS DECI    FORMAT "-Zzz,zzz,zz9" LABEL "Pag. Ano" NO-UNDO.
DEF            VAR      l-cargos       AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Cargos  " NO-UNDO.

DEF            VAR      l-EXsaldo      AS DECI    FORMAT "-zzz,zzz,zz9.99" LABEL "Saldo" NO-UNDO.
DEF            VAR      l-EX90dias     AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Dias" NO-UNDO.
DEF            VAR      l-EXvtames     AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Vta. Mes" NO-UNDO.
DEF            VAR      l-EXcargos     AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Cargos  " NO-UNDO.
DEF            VAR      l-EXpagmes     AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Pag. Mes" NO-UNDO.
DEF            VAR      l-EXvtasem     AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Vta. Sem" NO-UNDO.
DEF            VAR      l-EXpagsem     AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Pag. Sem" NO-UNDO.
DEF            VAR      l-EXtsalxant   AS DECIMAL NO-UNDO.    /*para opcion solo +dias*/
DEF            VAR      l-EXtsaltotal  AS DECIMAL NO-UNDO.
DEF            VAR      l-EXtimpxant   AS DECIMAL NO-UNDO.    /*para opcion solo +dias*/
DEF            VAR      l-EXtimptotal  AS DECIMAL NO-UNDO.
DEF            VAR      l-EXtsalxant2  AS DECIMAL NO-UNDO. /*para opcion todos*/
DEF            VAR      l-EXtsaltotal2 AS DECIMAL NO-UNDO.
DEF            VAR      l-EXtimpxant2  AS DECIMAL NO-UNDO. /*para opcion todos*/
DEF            VAR      l-EXtimptotal2 AS DECIMAL NO-UNDO.
DEF            VAR      l-EXCount      AS INTEGER NO-UNDO.
DEF            VAR      l-EXtotcli     AS INTEGER FORMAT "zzz9" NO-UNDO.



DEF            VAR      l-saldot       AS DECI    FORMAT "-zzz,zzz,zz9.99" LABEL "Saldo" NO-UNDO.
DEF            VAR      l-90diast      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Dias" NO-UNDO.
DEF            VAR      l-vtamest      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Vta. Mes" NO-UNDO.
DEF            VAR      l-pagmest      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Pag. Mes" NO-UNDO.
DEF            VAR      l-vtasemt      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Vta. Sem" NO-UNDO.
DEF            VAR      l-pagsemt      AS DECI    FORMAT "-zzz,zzz,zz9" LABEL "Pag. Sem" NO-UNDO.
DEF            VAR      l-vtaanot      AS DECI    FORMAT "-zzz,zzz,zzz9" LABEL "Vta. Ano" NO-UNDO.
DEF            VAR      l-paganot      AS DECI    FORMAT "-zzz,zzz,zzz9" LABEL "Pag. Ano" NO-UNDO.
DEF            VAR      l-cargost      AS DECI    FORMAT "-zzz,zzz,zzz9" LABEL "Cargos" NO-UNDO.

DEFINE         VARIABLE v-anticipo     AS INTEGER NO-UNDO.                               

DEF            VAR      l-entro        AS LOGI    NO-UNDO.
DEF            VAR      l-cheques      AS CHAR    FORMAT "x(5)" NO-UNDO.
DEF            VAR      l-reporte      AS CHAR    NO-UNDO.

DEF            VAR      l-meses        AS CHAR    FORMAT "x(12)" EXTENT 12 INITIAL
    ["Enero", "Febrero", "Marzo", "Abril",
    "Mayo", "Junio", "Julio", "Agosto",
    "Septiembre", "Octubre", "Noviembre", "Diciembre"] NO-UNDO.
DEF            VAR      l-SumaNoDep    AS INT     FORMAT '-zzz9' NO-UNDO.
DEF            VAR      l-Importe      LIKE Pagoacuse.Importe NO-UNDO.  
DEFINE         VARIABLE l-dia-max      AS INT     FORMAT "zz9" NO-UNDO.
DEFINE         VARIABLE v-margen       AS DECIMAL NO-UNDO FORMAT "-zzz9.99%".
   
ASSIGN 
    l-pagina = 9999.


DEF BUFFER b-mov   FOR MovCliente.
DEF BUFFER bb-mov  FOR MovCliente.
DEF BUFFER bf-mov  FOR MovCliente.
DEF BUFFER bf-hist FOR HistMovCte.
DEF BUFFER b-hist  FOR HistMovCte.
DEF TEMP-TABLE w-saldo LIKE Saldo
    FIELD Acomodo  AS CHAR
    FIELD Acomodo2 AS CHAR
    FIELD Acomodo3 AS DECIMAL.
    
       
    

DEF TEMP-TABLE ttAnalisis NO-UNDO
    FIELD IdPrincipal    AS INT
    FIELD ClienteId      LIKE Saldo.Sec
    FIELD Clase          LIKE ClaseCte.Descr
    FIELD RazonSocial    AS CHARACTER
    FIELD Saldo          AS DECIMAL
    FIELD Noventa        AS DECI      FORMAT "-zzz,zzz,zz9"
    FIELD Venta          AS DECIMAL
    FIELD Cargo          AS DECIMAL
    FIELD Pago           AS DECIMAL   
    FIELD Credito        AS DECIMAL
    FIELD SaldoFin       AS DECIMAL
    FIELD Resp           LIKE Resp.Nombre
    FIELD Plazo          LIKE Cliente.Plazo
    FIELD LineaCredito   LIKE Cliente.Limite   
    FIELD TipoMon        AS CHAR
    FIELD Anticipo       AS INT
    FIELD MontoPag       AS DECIMAL
    FIELD DiasCartera    AS INTEGER  
    FIELD PromedioPago   AS DEC       FORMAT "-zzz,zzz,zz9.99"
    FIELD MargenPromedio  AS DEC /* JASS0407202 */ 
    FIELD ImporteDeposito AS DEC. /* JASS22082025 */
 

 
 
    
DEF TEMP-TABLE ttDatos NO-UNDO
    FIELD IdPrincipal    AS INT
    FIELD TotalCteDia    LIKE l-totcli
    FIELD TotalCteDiaDC  AS DECIMAL 
    FIELD TotalCteDiaDPP AS DECIMAL
    FIELD TotalCte       LIKE Saldo.Sec
    FIELD TotalCteDC     AS DECIMAL
    FIELD TotalCteDPP    AS DECIMAL FORMAT ">>9.9".

DEFINE DATASET dsAnalisis FOR 
    ttDatos,
    ttAnalisis /* Tabla principal */
    
    DATA-RELATION Analisis FOR ttDatos,ttAnalisis 
    RELATION-FIELDS (IdPrincipal, IdPrincipal).                   
    
DEFINE NEW SHARED VARIABLE g-tty AS CHARACTER NO-UNDO.
ASSIGN 
    g-tty = STRING(TIME).

DEFINE VARIABLE vTotal  AS DECIMAL NO-UNDO.  /* JASS22082025 */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ReporteAnalisisSaldos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER l-fecha    AS DATE FORMAT "99/99/9999"  NO-UNDO.
    DEF INPUT PARAMETER l-fechaini AS DATE FORMAT "99/99/9999"  NO-UNDO.
    DEF INPUT PARAMETER l-diascap  AS INTE FORMAT ">>9"         NO-UNDO.
    DEF INPUT PARAMETER l-clase    AS INT NO-UNDO.
 //   DEF INPUT PARAMETER l-zona     AS INT NO-UNDO. 
    DEF INPUT PARAMETER l-zona     AS CHAR NO-UNDO. // JASS06062025
    DEF INPUT PARAMETER l-Resp LIKE Cliente.Id-Resp  NO-UNDO. /* JASS27062025 */
    DEFINE INPUT PARAMETER pCobrador AS INTEGER NO-UNDO. /* JASS14072025 */ 
    
    DEFINE OUTPUT  PARAMETER DATASET FOR dsAnalisis.    

    DEF VAR l-diasmas   AS LOGICAL FORMAT 'SI/NO' NO-UNDO. 
    DEF VAR l-indice    AS INTE    FORMAT "9" NO-UNDO.
    DEF VAR l-opciones  AS CHAR    EXTENT 2 INITIAL ["Importe","Alfabetico"].
    DEF VAR l-opciones2 AS CHAR    EXTENT 2 INITIAL ["Solo + dias", "Todos"].
    DEF VAR l-aparece   AS CHAR    EXTENT 4 
        INITIAL["Todos","Facturas","NCO","Cheques Devueltos"] NO-UNDO.
    DEF VAR l-index     AS INTE. 
    DEF VAR l-index2    AS INTE.
    DEF VAR l-index3    AS INTE.
    DEF VAR l-mcini     AS INTE.
    DEF VAR l-mcfin     AS INTE.  

    ASSIGN 
        l-diasmas      = FALSE
        l-aparece[1]   = "Todos"
        l-opciones[1]  = "Importe"
        l-opciones2[1] = "Solo + dias"
        l-indice       = 1
        l-index3       = 1   // Documentos  - Todos-facturas-nco-cheques 
        l-index        = 1   // Acomodado por Importe-Alfabetico
        l-index2       = 1 .  // Seleccionar - Solo + dias - Todos 
    
    MESSAGE "Parámetros recibidos: "
        SKIP 
        "Fecha corte     : " STRING(l-fecha)
        SKIP 
        "Fecha inicial   : " STRING(l-fechaini)
        SKIP 
        "Días de cartera : " STRING(l-diascap)
        SKIP 
        "Clase cliente   : " STRING(l-clase)
        SKIP 
        "Zona            : " l-zona
        SKIP 
        "Responsable     : " STRING(l-Resp)
        SKIP 
        "Cobrador        : " STRING(pCobrador)
        VIEW-AS ALERT-BOX INFO TITLE "Filtros aplicados".
        
    IF l-index3 = 1 THEN ASSIGN l-mcini = 1 l-mcfin = 3.
    IF l-fechaini = ? THEN l-fechaini = TODAY - 1 .
    IF l-fecha    = ? THEN l-fecha    = TODAY     .
    IF l-diascap  = ? THEN l-diascap  = 90        . 
    IF l-clase    = ? THEN l-clase    = 0         . 
  //  IF l-zona     = ? THEN l-zona     = 0       . 
    IF l-zona     = ? THEN l-zona     = ""        . // JASS06062025
    IF l-Resp     = ? THEN l-Resp = 0   . /* JASS27062025 */
    IF pCobrador   = ? THEN pCobrador = 0. /* JASS14072025 */ 

    ASSIGN 
        l-nc         = 0     
        l-saldo      = 0    
        l-90dias     = 0     
        l-vtames     = 0
        l-pagmes     = 0     
        l-vtasem     = 0    
        l-pagsem     = 0     
        l-vtaano     = 0       
        l-pagano     = 0     
        l-sumanodep  = 0  
        l-salxant    = 0    
        l-saltotal   = 0 
        l-tsalxant   = 0   
        l-tsaltotal  = 0  
        l-tsalxant2  = 0  
        l-tsaltotal2 = 0  
        l-impxant    = 0    
        l-imptotal   = 0   
        l-timpxant   = 0   
        l-timptotal  = 0
        l-timpxant2  = 0  
        l-timptotal2 = 0. 
       


       
    FOR EACH Cliente WHERE (l-clase = 0 OR Cliente.Id-ClaseCte = l-clase) 
        AND  (l-zona = "" OR l-zona = "0" OR CAN-DO(l-zona, STRING(Cliente.Id-Zona)))
        AND  (l-resp = 0 OR Cliente.Id-Resp = l-resp) 
        AND   (pCobrador = 0 OR Cliente.Id-Cobrador = pCobrador )NO-LOCK:    
        IF Cliente.Id-Cliente < 12 AND Cliente.Id-Cliente <> 3 THEN NEXT.  
        
        /* Solo para cliente 343 */
  //  IF Cliente.Id-Cliente <> 343 THEN NEXT.
    
        IF l-diasmas = TRUE THEN 
            l-diascap = Cliente.Plazo + l-diascap2.

        CREATE w-saldo.
        ASSIGN 
            w-saldo.Sec = Cliente.Id-Cliente.       
        FOR EACH MovCliente WHERE MovCliente.FecReg <= l-fecha        AND
            (MovCliente.Id-MC >= l-mcini AND
            MovCliente.Id-MC <= l-mcfin) AND 
            MovCliente.Id-Cliente = Cliente.Id-Cliente 
            NO-LOCK
            BREAK BY MovCliente.Id-Cliente:    
             
            IF l-index2 = 1 THEN 
            DO:       
                ASSIGN 
                    w-saldo.tty = g-tty.
                IF l-diascap > 0 THEN 
                DO:
                   
                    IF l-diascap = 0 OR (l-fecha - MovCliente.FecVenc) >= l-diascap THEN 
                    DO:
                        ACCUMULATE MovCliente.Saldo (TOTAL BY MovCliente.Id-Cliente).
                    END.
                END.
            END. /* si solo los que cumplen con los diascap */

            FOR EACH b-mov WHERE b-mov.RefSaldo = MovCliente.RefSaldo         AND
                b-mov.Id-MC > 3                         AND
                b-mov.FecReg <= l-fecha NO-LOCK:
                IF b-mov.Id-Mc <> 65 THEN 
                DO:
                    FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento NO-LOCK NO-ERROR.
                    IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
                END.
                ACCUMULATE b-mov.Importe (TOTAL).
            END. /* del for each b-mov */
 
            ASSIGN 
                l-saldo = MovCliente.Importe + (ACCUM TOTAL b-mov.Importe).
    
            IF Movcliente.Id-Moneda > 1 THEN        // RNPC - 2019-07-22 
                ASSIGN l-saldo = l-saldo * MovCliente.TipoCambio.
            
            /*
            IF l-diascap <> 0 THEN 
                IF (l-fecha - MovCliente.FecReg) > l-diascap THEN 
                    ACCUMULATE l-saldo (TOTAL BY MovCliente.ID-cliente).
             
            IF l-diascap = 0 THEN 
                IF (l-fecha - MovCliente.FecVenc) > 0 THEN 
                    ACCUMULATE l-saldo (TOTAL BY MovCliente.Id-Cliente). */
              
            /*      
          IF l-diascap <> 0 THEN 
              IF (l-fecha - MovCliente.FecVenc) > l-diascap THEN 
                  ACCUMULATE l-saldo (TOTAL BY MovCliente.ID-cliente).  */
                    
            IF l-diascap = 0 OR (l-fecha - MovCliente.FecVenc) >= l-diascap THEN 
                ACCUMULATE l-saldo (TOTAL BY MovCliente.ID-cliente).  
            
    
            IF LAST-OF (MovCliente.Id-Cliente) AND 
                ((ACCUM TOTAL BY MovCliente.Id-Cliente l-saldo) > 0 OR
                l-index2 = 2) THEN 
            DO:
                FIND FIRST w-saldo WHERE w-saldo.Sec = MovCliente.Id-Cliente EXCLUSIVE.
                IF NOT AVAILABLE w-saldo THEN CREATE w-Saldo.
                ASSIGN 
                    w-Saldo.Saldo = (ACCUM TOTAL BY MovCliente.Id-cliente l-saldo)
                    w-Saldo.TTy   = g-tty
                    w-Saldo.Sec   = MovCliente.Id-Cliente
                    w-Saldo.Doc   = "0".
               

                IF l-index = 1 THEN 
                DO: /* por importe */
                    ASSIGN 
                        w-saldo.Acomodo2 = STRING(w-saldo.Saldo,"-999,999,999.99")                  
                        w-saldo.Acomodo  = w-saldo.Acomodo2
                        w-saldo.Acomodo3 = w-saldo.Saldo.
                END. /* por importe */  
            END. /* del last-of Movcliente */
        END. /* del for each Movcliente */.

        ASSIGN 
            l-totcli = 0.
        
    END.   
 
    IF l-index = 1 THEN 
    DO:
        ASSIGN 
            l-i = 0.
        FOR EACH w-saldo EXCLUSIVE-LOCK 
            BY w-saldo.Acomodo3 DESCENDING :
            ASSIGN 
                l-i             = l-i + 1
                w-saldo.Acomodo = STRING(l-i,"999999").
        END.
    END. /* si es por importe */

    FOR EACH w-Saldo WHERE w-Saldo.Tty = g-tty EXCLUSIVE-LOCK,
        EACH Cliente WHERE Cliente.Id-Cliente = w-saldo.Sec NO-LOCK
        BREAK BY w-Saldo.Acomodo: 
        ASSIGN 
            l-saldo  = 0
            l-pagsem = 0.
        FOR EACH bb-mov WHERE bb-mov.id-cliente = w-saldo.sec AND
            bb-mov.id-mc >= l-mcini AND
            bb-mov.id-mc <= l-mcfin AND
            bb-mov.fecreg <= l-fecha NO-LOCK BREAK BY bb-mov.id-cliente:
            FOR EACH b-mov WHERE b-mov.refsaldo = bb-mov.RefSaldo    AND
                b-mov.Id-MC    > 3                  AND
                b-mov.FecReg  <= l-fecha            AND
                b-mov.afectado NO-LOCK:
                IF b-mov.Id-Mc <> 65 THEN 
                DO:
                    FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento NO-LOCK NO-ERROR.
                    IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
                END.
                ACCUMULATE b-mov.importe (TOTAL).
            END. /* del b-mov */
            
            ASSIGN 
                l-saldo = bb-mov.Importe + (ACCUM TOTAL b-mov.Importe).
           
            IF bb-mov.Id-Moneda > 1 THEN        // RNPC - 2019-07-23 
                ASSIGN l-saldo = (l-saldo) * bb-mov.TipoCambio.
    
            ASSIGN 
                l-pagsem = l-pagsem + l-saldo.
    
        END. /* del bb-mov */
        IF l-pagsem = 0 THEN 
        DO:         
            DELETE w-saldo.
            NEXT.
        END.  
        ASSIGN 
            l-pagsem = 0 
            l-saldo  = 0.
        IF l-index2 = 1 THEN 
        DO:  
            /* saca DIAS CARTERA */         
            RUN "cxcd0016.p"(INPUT w-saldo.Sec, INPUT l-Fecha,OUTPUT l-salxant, OUTPUT l-saltotal).
            /* saca el promedio de pago */
            RUN "cxcd0012.p" (INPUT w-saldo.sec, OUTPUT l-impxant, OUTPUT l-imptotal).         
            ASSIGN 
                l-tsalxant2  = l-tsalxant2 + l-salxant 
                l-tsaltotal2 = l-tsaltotal2 + l-saltotal
                l-timpxant2  = l-timpxant2 + l-impxant
                l-timptotal2 = l-timptotal2 + l-imptotal.  

            ACCUMULATE w-Saldo.sec (COUNT).
            IF w-Saldo.saldo = 0 THEN 
            DO:
                DELETE w-Saldo.
                NEXT.
            END.
        END.
        IF l-diasmas THEN 
            l-diascap = Cliente.Plazo + l-diascap2.
        /* PARA SACAR UN SALDO INICIAL */
        FOR EACH MovCliente WHERE MovCliente.Id-Cliente = w-Saldo.Sec AND
            MovCliente.FecReg < l-fechaini      AND
            Movcliente.id-mc >= l-mcini         AND
            MovCliente.Id-Mc <= l-mcfin NO-LOCK
            USE-INDEX idx-mov:
            FOR EACH b-mov WHERE b-mov.refsaldo = MovCliente.RefSaldo AND
                b-mov.Id-MC    > 3                   AND
                b-mov.FecReg  < l-fechaini           AND
                b-mov.afectado        NO-LOCK:
                IF b-mov.Id-Mc <> 65 THEN 
                DO:
                    FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento NO-LOCK NO-ERROR.
                    IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
                END.
                ACCUMULATE b-mov.importe (TOTAL).
            END.
            ASSIGN 
                l-saldo = MovCliente.Importe + (ACCUM TOTAL b-mov.Importe).
        
            IF Movcliente.Id-Moneda > 1 THEN        // RNPC - 2019-07-23 
                ASSIGN l-saldo   = l-saldo * MovCliente.TipoCambio
                    l-simbolo = "US".
        
            ACCUMULATE l-saldo (TOTAL).  
            ASSIGN 
                l-saldocal = l-saldocal + l-saldo.
        END. /* del movcliente */    
        FOR EACH HistMovCte WHERE HistMovCte.Id-Cliente = w-Saldo.Sec AND
            HistMovCte.FecReg < l-fechaini      AND
            HistMovCte.Id-MC >= l-mcini         AND
            HistMovCte.Id-MC <= l-mcfin NO-LOCK
            USE-INDEX idx-mov:
            FOR EACH b-hist WHERE b-hist.refsaldo = HistMovCte.RefSaldo AND
                b-hist.Id-MC    > 3                   AND
                b-hist.FecReg   < l-fechaini          AND
                b-hist.afectado  NO-LOCK:
                IF b-hist.Id-Mc <> 65 THEN 
                DO:
                    FIND Acuse WHERE Acuse.Id-Acuse = b-hist.documento 
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
                END.
                ACCUMULATE b-hist.importe (TOTAL).
            END.
            ASSIGN 
                l-saldo = HistMovcte.Importe + (ACCUM TOTAL b-hist.Importe).
        
            IF HistMovCte.Id-Moneda > 1 THEN        // RNPC - 2019-07-23 
                ASSIGN l-saldo   = l-saldo * HistMovCte.TipoCambio
                    l-simbolo = "US".
        
            ACCUMULATE l-saldo (TOTAL).  
            ASSIGN 
                l-saldocal = l-saldocal + l-saldo.
        END. /* del HistMovCte */

        IF /* LAST-OF(STRING(MovCliente.Id-Cliente)) */ TRUE THEN 
        DO:
            FOR EACH CheDev WHERE CheDev.Id-Cliente = w-saldo.sec NO-LOCK:
                ACCUMULATE CheDev.FecCargo (COUNT).
            END.
            /* para sacar las compras */
            FOR EACH Factura WHERE Factura.Id-Cliente = w-saldo.sec AND
                Factura.FecReg >= l-fechaini     AND
                Factura.FecReg <= l-fecha        AND
                Factura.FecCancel = ? NO-LOCK.        
                IF Factura.Id-Moneda > 1 THEN        // RNPC - 2019-07-25 
                    ASSIGN l-vtames  = l-vtames + (Factura.Tot * Factura.TipoCambio)
                        l-simbolo = "US".
                ELSE
                    ASSIGN l-vtames = l-vtames + Factura.Tot.        
            END. /* del for each factura */

            /* para sacar los cargos */
            FOR EACH MovCliente WHERE MovCliente.Id-Cliente = w-saldo.sec AND
                MovCliente.FecReg >= l-fechaini     AND
                MovCliente.FecReg <= l-fecha        AND
                MovCliente.Id-MC <= l-mcfin         AND
                MovCliente.Id-MC >= l-mcini         NO-LOCK:
                IF MovCliente.Id-MC = 1 THEN NEXT.
                ASSIGN 
                    l-cargos = l-cargos + MovCliente.Importe.
            END. /* del for each factura */

            /* para sacar los pagos y creditos */
            FOR EACH b-mov WHERE b-mov.Id-Cliente = w-saldo.sec AND
                b-mov.Id-Mc > 3                AND
                b-mov.fecreg >= l-fechaini     AND
                b-mov.fecreg <= l-fecha 
                NO-LOCK:
                FIND TabMC WHERE TabMc.Id-MC = b-mov.Id-MC 
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE TabMC THEN NEXT.
                IF TabMC.Sentido THEN NEXT.
        
                IF b-mov.Id-Mc <> 65 THEN 
                DO:
                    FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento NO-LOCK NO-ERROR.
                    IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
                END.
                ELSE RELEASE Acuse.
        
                IF AVAILABLE Acuse AND b-mov.id-mc <> 90 THEN 
                DO:
                    FIND FIRST DocAcuse WHERE DocAcuse.Id-Acuse = Acuse.Id-Acuse AND
                        DocAcuse.Id-MC >= l-mcini AND
                        DocAcuse.Id-Mc <= l-mcfin 
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE Docacuse THEN NEXT.
                END.
        
                IF b-mov.id-moneda > 1 THEN
                    FIND FIRST MovCliente WHERE MovCliente.RefSaldo = b-mov.RefSaldo AND
                        MovCliente.Id-MC    <= 3              AND
                        MovCliente.Afectado                  AND
                        MovCliente.FecReg  <= l-fecha  NO-LOCK NO-ERROR. 
            
                IF b-mov.id-mc <= 62 OR b-mov.id-mc = 90 THEN 
                DO:
                    IF b-mov.id-moneda > 1 AND AVAILABLE MovCliente THEN
                        ASSIGN l-pagmes = l-pagmes + ((b-mov.importe * MovCliente.TipoCambio) * -1). 
                    ELSE 
                        ASSIGN l-pagmes = l-pagmes + (b-mov.importe * -1).
                END.
                ELSE 
                DO:
                    IF b-mov.id-moneda > 1 AND AVAILABLE MovCliente THEN
                        ASSIGN l-vtasem = l-vtasem + ((b-mov.importe * MovCliente.TipoCambio) * -1).
                    ELSE
                        ASSIGN l-vtasem = l-vtasem + (b-mov.importe * -1).
                END.
            END. /* del b-mov */

            FOR EACH HistMovCte WHERE HistMovCte.Id-Cliente = w-saldo.sec AND
                HistMovCte.Id-Mc  > 3               AND
                HistMovCte.fecreg >= l-fechaini     AND
                HistMovCte.fecreg <= l-fecha 
                NO-LOCK:
                FIND TabMC WHERE TabMc.Id-MC = HistMovCte.Id-MC NO-LOCK NO-ERROR.
                IF NOT AVAILABLE TabMC THEN NEXT.
                IF TabMC.Sentido THEN NEXT.

                IF HistMovCte.Id-Mc <> 65 THEN 
                DO:
                    FIND Acuse WHERE Acuse.Id-Acuse = HistMovcte.documento 
                        NO-LOCK NO-ERROR.
       
                    IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
                END.
                ELSE RELEASE Acuse.
        
                IF AVAILABLE Acuse THEN 
                DO:
                    FIND FIRST DocAcuse WHERE DocAcuse.Id-Acuse = Acuse.Id-Acuse AND
                        DocAcuse.Id-MC >= l-mcini AND
                        DocAcuse.Id-Mc <= l-mcfin 
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE Docacuse THEN NEXT.
                END.
        
            // RNPC - 2019-07-26
                IF HistMovCte.id-moneda > 1 THEN
                    FIND FIRST b-hist WHERE b-hist.RefSaldo = HistMovCte.RefSaldo AND
                        b-hist.Id-MC    <= 3              AND
                        b-hist.Afectado                  AND
                        b-hist.FecReg  <= l-fecha  NO-LOCK NO-ERROR.
    
                IF HistMovCte.id-mc <= 62 OR HistMovCte.Id-Mc = 90 THEN 
                DO: 
                    IF HistMovCte.id-moneda > 1 AND AVAILABLE b-hist THEN
                        ASSIGN l-pagmes = l-pagmes + ((HistMovCte.importe * b-hist.TipoCambio) * -1).
                    ELSE
                        ASSIGN l-pagmes = l-pagmes + (HistMovCte.importe * -1).
                END.
                ELSE 
                DO:
                    IF HistMovCte.id-moneda > 1 AND AVAILABLE b-hist THEN
                        ASSIGN l-vtasem = l-vtasem + ((HistMovCte.importe * b-hist.TipoCambio) * -1). 
                    ELSE
                        ASSIGN l-vtasem = l-vtasem + (HistMovCte.importe * -1).
                END.
            END. /* del HistMovCte */

            /*--------------------------------------------------------------------------------- */
            v-anticipo = 0.     
            FOR EACH anticipo WHERE Anticipo.ImpAnticipo - Anticipo.ImpAplicado - Anticipo.ImpContado - Anticipo.ImpDevuelto > 0 AND 
                NOT Anticipo.Canc AND 
                Anticipo.Id-Cliente = w-saldo.sec
                NO-LOCK:
                v-anticipo = v-anticipo + 
                    (anticipo.impanticipo - anticipo.impaplicado - anticipo.impdevuelto - anticipo.impcontado).                         
            END.

            /* para sacar el saldo final y > 90 dias */
            FOR EACH bb-mov WHERE bb-mov.id-cliente = w-saldo.sec AND
                bb-mov.id-mc >= l-mcini AND
                bb-mov.id-mc <= l-mcfin AND
                bb-mov.fecreg <= l-fecha NO-LOCK 
                BREAK BY bb-mov.id-cliente:
                FOR EACH b-mov WHERE b-mov.refsaldo = bb-mov.RefSaldo    AND
                    b-mov.Id-MC    > 3                  AND
                    b-mov.FecReg  <= l-fecha            AND
                    b-mov.afectado NO-LOCK:
                    IF b-mov.Id-Mc <> 65 THEN 
                    DO:
                        FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento NO-LOCK NO-ERROR.
                        IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
                    END.
                    ACCUMULATE b-mov.importe (TOTAL).
                END. /* del b-mov */
                ASSIGN 
                    l-saldo = bb-mov.Importe + (ACCUM TOTAL b-mov.Importe).
             
                IF bb-mov.Id-Moneda > 1 THEN        // RNPC - 2019-07-22 
                    ASSIGN l-saldo = l-saldo * bb-mov.TipoCambio.
             
                IF bb-mov.Id-MC = 2 AND l-saldo > 0 THEN
                    ASSIGN l-nc = l-nc + 1.
                IF bb-mov.Id-MC = 3 AND l-saldo > 0 THEN
                    ASSIGN l-chedev = l-chedev + 1.
                IF l-diascap <> 0 THEN 
                    ASSIGN 
                        // l-dias   = l-fecha - bb-mov.FecReg   
                        l-dias   = l-fecha - bb-mov.FecVenc   /* JASS03062025 */
                        l-90dias = l-90dias + (IF l-dias > l-diascap THEN l-saldo 
                                                                    ELSE 0)
                        l-pagsem = l-pagsem + l-saldo.
                ELSE ASSIGN l-dias   = l-fecha - bb-mov.fecvenc
                        l-90dias = l-90dias + (IF l-dias > 0 THEN l-saldo ELSE 0)
                        l-pagsem = l-pagsem + l-saldo.
                ASSIGN 
                    l-dias = l-fecha - bb-mov.fecreg.
                IF l-dias > 0 THEN
                    ACCUMULATE l-dias * l-saldo (TOTAL).
            END. /* del bb-mov */
            
           
            ASSIGN 
                l-sumanodep = 0.
            FOR EACH Acuse WHERE Acuse.Id-cliente = w-Saldo.Sec
                AND Acuse.Estatus < 3 NO-LOCK:
                ASSIGN 
                    l-Importe = 0.
                FOR EACH Pagoacuse OF acuse NO-LOCK:
                    ASSIGN 
                        l-importe = l-importe + pagoacuse.importe.
                END.
                IF l-Importe > 0 THEN 
                DO:
                    ASSIGN 
                        l-SumaNoDep = l-SumaNoDep + l-Importe.
                END.
            END. /**/
            IF l-pagsem <> 0 THEN 
            DO:  
                ASSIGN 
                    l-diascartera = (ACCUM TOTAL l-dias * l-saldo) / l-pagsem
                    l-tsalxant    = l-tsalxant + (ACCUM TOTAL l-dias * l-saldo)
                    l-tsaltotal   = l-tsaltotal + l-pagsem.
            END.
            ASSIGN      
                l-entro    = TRUE
                l-totcli   = l-totcli + 1
                l-saldo    = l-saldocal
                l-saldot   = l-saldot + l-saldo
                l-saldocal = 0
                l-90diast  = l-90diast + l-90dias
                l-vtamest  = l-vtamest + l-vtames
                l-pagmest  = l-pagmest + l-pagmes
                l-cargost  = l-cargost + l-Cargos
                l-vtasemt  = l-vtasemt + l-vtasem
                l-pagsemt  = l-pagsemt + l-pagsem
                l-vtaanot  = l-vtaanot + l-vtaano
                l-paganot  = l-paganot + l-pagano.
            ACCUMULATE l-diascartera (AVERAGE).
            IF l-diascartera < 0 THEN ASSIGN l-diascartera = 0.
            /* para los totales del saldo POR hoja */      
            ASSIGN 
                l-saldocalh    = l-saldocalh + l-saldo
                l-90diash      = l-90diash + l-90dias
                l-vtamesh      = l-vtamesh + l-vtames
                l-pagmesh      = l-pagmesh + l-pagmes
                l-cargosh      = l-cargosh + l-cargos
                l-vtasemh      = l-vtasemh + l-vtasem
                l-pagsemh      = l-pagsemh + l-pagsem
                l-vtaanoh      = l-vtaanoh + l-vtaano
                l-paganoh      = l-paganoh + l-pagano
                l-totclih      = l-totclih + 1
                l-diascarterah = l-diascarterah + l-diascartera.
    
            /* saca el promedio de pago */
            RUN "cxcd0010.p" (INPUT w-saldo.sec, OUTPUT l-prompag). /*para un cliente*/
            RUN "cxcd0012.p" (INPUT w-saldo.sec, OUTPUT l-impxant, OUTPUT l-imptotal).  /*para ponderados*/
            ASSIGN 
                l-timpxant  = l-timpxant + l-impxant
                l-timptotal = l-timptotal + l-imptotal.
         
            ACCUMULATE l-prompag (AVERAGE).
            FIND FIRST Clase WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
            FIND FIRST Resp  WHERE Resp.Id-Resp         = Cliente.Id-Resp    NO-LOCK NO-ERROR.
            IF l-indice = 1 THEN        
            DO:
                CREATE ttAnalisis.
                ASSIGN
                    ttAnalisis.IdPrincipal  = 1
                    ttAnalisis.ClienteId    = w-saldo.sec
                    ttAnalisis.Clase        = ClaseCte.Descr 
                    WHEN AVAILABLE Clase
                    ttAnalisis.RazonSocial  = Cliente.RazonSocial 
                    WHEN AVAILABLE Cliente
                    ttAnalisis.Saldo        = l-saldo
                    ttAnalisis.Noventa      = ROUND(l-90dias, 0) // l-90dias 
                    ttAnalisis.Venta        = l-vtames
                    ttAnalisis.Cargo        = l-Cargos
                    ttAnalisis.Pago         = l-pagmes
                    ttAnalisis.Credito      = l-vtasem
                    ttAnalisis.SaldoFin     = l-pagsem
                    ttAnalisis.Resp         = Resp.Nombre 
                    WHEN AVAILABLE Resp
                    ttAnalisis.Plazo        = Cliente.Plazo   
                    WHEN AVAILABLE Cliente 
                    ttAnalisis.LineaCredito = Cliente.Limite  
                    WHEN AVAILABLE Cliente 
                    ttAnalisis.TipoMon      = ""
                    ttAnalisis.Anticipo     = v-anticipo 
                    ttAnalisis.MontoPag     = Cliente.MontoPagare  
                    WHEN AVAILABLE Cliente
                    ttAnalisis.DiasCartera  = l-diascartera 
                    ttAnalisis.PromedioPago = ROUND(l-prompag, 1).
                    
                ACCUMULATE l-vtames (TOTAL).
                ACCUMULATE l-pagmes (TOTAL).
                ACCUMULATE l-cargos (TOTAL).
                ACCUMULATE l-vtasem (TOTAL).
                ACCUMULATE l-pagsem (TOTAL).
                ACCUMULATE l-vtaano (TOTAL).
                ACCUMULATE l-pagano (TOTAL).
                ACCUMULATE l-90dias (TOTAL).
                

             
                IF Cliente.Id-Resp <> 99 THEN 
                DO:
                    l-ExSaldo  = l-ExSaldo  + l-saldo.
                    l-Ex90Dias = l-Ex90Dias + l-90dias.
                    l-ExVtames = l-ExVtames + l-vtames.
                    l-Excargos = l-ExCargos + l-cargos.
                    l-ExPagmes = l-Expagmes + l-pagmes.
                    l-ExVtaSem = l-ExVtaSem + l-vtasem.
                    l-ExPagSem = l-ExPagSem + l-pagsem.
                    l-EXtotcli = l-EXtotcli + 1.
                    IF l-pagsem <> 0 THEN 
                    DO:  
                        ASSIGN 
                            l-EXtsalxant  = l-EXtsalxant + (ACCUM TOTAL l-dias * l-saldo)
                            l-EXtsaltotal = l-EXtsaltotal + l-pagsem.
                    END.
                    ASSIGN 
                        l-EXtimpxant  = l-EXtimpxant + l-impxant
                        l-EXtimptotal = l-EXtimptotal + l-imptotal.
                END.
                ASSIGN 
                    l-90dias      = 0 
                    l-vtasem      = 0 
                    l-pagsem      = 0 
                    l-nc          = 0
                    l-vtaano      = 0 
                    l-pagano      = 0 
                    l-vtames      = 0 
                    l-pagmes      = 0 
                    l-saldo       = 0 
                    l-cargos      = 0 
                    l-diascartera = 0 
                    l-simbolo     = "".   // RNPC -2019-07-23
            END. /* si el l-indice = 1 entonces es detallado  (del if indice = 1) */
        END. /* del LAST-OF(MovCliente) */
    
        IF ((l-totclih = 25 OR LAST(w-saldo.Acomodo )) AND l-indice <> 2) OR
            ((l-totcorteh = 25 OR LAST( w-saldo.Acomodo )) AND l-indice = 2) THEN 
        DO:
            ASSIGN 
                l-diascarterah = l-diascarterah / l-totclih.

            ASSIGN 
                l-pagina       = 0
                l-diascarterah = 0 
                l-saldocalh    = 0
                l-totclih      = 0 
                l-90diash      = 0
                l-vtamesh      = 0 
                l-pagmesh      = 0
                l-vtasemh      = 0 
                l-pagsemh      = 0
                l-vtaanoh      = 0 
                l-paganoh      = 0
                l-totcorteh    = 0 
                l-cargosh      = 0.
    
        END. /* si hubo un salto de pagina */ 
        ASSIGN 
            l-saldocal = 0 
            l-saldo    = 0.
    END.  /* del for each saldo */ 
   
    CREATE ttDatos.   
    ASSIGN  
        ttDatos.IdPrincipal    = 1
        ttDatos.TotalCteDia    = l-totcli    
        ttDatos.TotalCteDiaDC  = ROUND((l-tsalxant / l-tsaltotal),1)
        ttDatos.TotalCteDiaDPP = ROUND((l-timpxant / l-timptotal),1)
        ttDatos.TotalCte       = ACCUM COUNT w-Saldo.Sec
        ttDatos.TotalCteDC     = ROUND((l-tsalxant2 / l-tsaltotal2),1)
        ttDatos.TotalCteDPP    = ROUND((l-timpxant2 / l-timptotal2),1).
    
    FOR EACH ttAnalisis WHERE  ttAnalisis.ClienteId > 0 :        
        RUN programas/margencte.p(INPUT ttAnalisis.ClienteId, OUTPUT v-margen).
        RUN DepositoSantander(INPUT ttAnalisis.ClienteId, OUTPUT vTotal).
        ASSIGN 
            ttAnalisis.MargenPromedio = ROUND(v-margen, 2)
            ttAnalisis.ImporteDeposito = vTotal.
    END.                   
/*        
FOR EACH ttAnalisis Where ttAnalisis.Noventa = 0 :
    Delete ttAnalisis.
END.   */     
RETURN. 
END PROCEDURE.     

PROCEDURE "cxcd0016.p":
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF BUFFER b-Mov FOR MovCliente.
    DEF INPUT PARAMETER l-cliente LIKE Cliente.Id-Cliente           NO-UNDO.
    DEF INPUT PARAMETER l-fecha AS DATE FORMAT "99/99/9999"     NO-UNDO.
    DEF OUTPUT PARAMETER l-salxant AS DECI              NO-UNDO.
    DEF OUTPUT PARAMETER l-saltotal AS DECI             NO-UNDO.
    DEF VAR l-saldo AS DECI NO-UNDO.
    DEF VAR l-ant   AS INTE NO-UNDO.

    FOR EACH MovCliente WHERE MovCliente.Id-Cliente = l-cliente AND
        MovCliente.FecReg    <= l-fecha AND
        MovCliente.Id-MC     <= 3 AND
        MovCliente.Afectado NO-LOCK:
        FOR EACH b-mov WHERE b-mov.refsaldo = movcliente.refsaldo AND
            b-mov.id-mc > 3 AND
            b-mov.afectado AND
            b-mov.fecreg <= l-fecha NO-LOCK:
            IF b-mov.Id-Mc <> 65 THEN 
            DO:
                FIND Acuse WHERE Acuse.Id-Acuse = b-mov.documento NO-LOCK NO-ERROR.
                IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
            END.
            ACCUMULATE b-mov.importe (TOTAL).
        END. /* del for each b-mov */
        ASSIGN 
            l-saldo = MovCliente.Importe + (ACCUM TOTAL b-mov.importe).
    
        IF Movcliente.Id-Moneda > 1 THEN        // RNPC - 2019-07-23 
            ASSIGN l-saldo = l-saldo * MovCliente.TipoCambio.
    
        ASSIGN 
            l-ant = l-fecha - movcliente.fecreg.
        ACCUMULATE l-saldo * l-ant (TOTAL).
        ACCUMULATE l-saldo (TOTAL).
    END. /* for each movcliente */

    ASSIGN 
        l-salxant  = (ACCUM TOTAL l-saldo * l-ant)
        l-saltotal = (ACCUM TOTAL l-saldo).  

END PROCEDURE.    

PROCEDURE "cxcd0012.p":
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
       Empresa : Consultoria en Informatica Ejecutiva
       Modulo  : Cuentas por Cobrar
       Sistema : ADOSA
       Programa: cxcd0012.p
       Funcion : Saca el promedio de pago de VARIOS cliente PONDERADO
       Autor   : SAUL
       Fecha   : 21-ENE-2010
    */

    DEF BUFFER b-MovCte     FOR MovCliente.
    DEF BUFFER b-MovHistCte FOR HistMovCte.
    DEF INPUT PARAMETER l-cliente LIKE Cliente.Id-Cliente NO-UNDO.
    DEF OUTPUT PARAMETER l-impxant  AS DECI               NO-UNDO.
    DEF OUTPUT PARAMETER l-imptotal AS DECI               NO-UNDO.
    DEF VAR l-fecha AS DATE NO-UNDO.
    DEF VAR l-dias  AS INTE NO-UNDO.
    DEF VAR l-mes   AS INTE NO-UNDO.
    DEF VAR l-mes2  AS INTE NO-UNDO.

    /*
    ASSIGN l-mes   = MONTH(g-today) - 6
           l-mes2  = (IF l-mes <= 0 THEN 12 + l-mes ELSE 0)
           l-fecha = (IF l-mes2 <> 0
                                  THEN DATE(l-mes2,DAY(g-today),YEAR(g-today) - 1)
                                  ELSE DATE(l-mes,1,YEAR(g-today)) ).
    */
    ASSIGN 
        l-Fecha = TODAY - 180.
    FOR EACH HistMovCte WHERE HistMovCte.Id-Cliente = l-cliente AND
        HistMovCte.FecReg     >= l-fecha  AND
        HistMovCte.Importe    < 0 NO-LOCK:
        FIND TipoPago WHERE TipoPago.Id-Tp = HistMovCte.Id-MC NO-LOCK NO-ERROR.
        IF NOT AVAILABLE TipoPago THEN NEXT.
        FIND FIRST b-MovHistCte WHERE b-MovHistCte.RefSaldo = HistMovCte.RefSaldo AND
            b-MovHistCte.Id-MC   <= 3 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-MovHistCte THEN NEXT.
        ASSIGN 
            l-dias = HistMovCte.FecReg - b-MovHistCte.FecReg.
        ACCUMULATE (HistMovCte.Importe * l-dias) (TOTAL).
        ACCUMULATE HistMovCte.Importe (TOTAL).
    END.

    FOR EACH MovCliente WHERE MovCliente.Id-Cliente = l-cliente AND
        MovCliente.FecReg    >= l-fecha   AND
        MovCliente.Importe    < 0 NO-LOCK:
        FIND TipoPago WHERE TipoPago.Id-Tp = MovCliente.Id-MC NO-LOCK NO-ERROR.
        IF NOT AVAILABLE TipoPago THEN NEXT.
        FIND FIRST b-MovCte WHERE b-MovCte.RefSaldo = MovCliente.RefSaldo AND
            b-MovCte.Id-MC   <= 3 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-MovCte THEN NEXT.
        ASSIGN 
            l-dias = MovCliente.FecReg - b-MovCte.FecReg.
        ACCUMULATE (MovCliente.Importe * l-dias) (TOTAL).
        ACCUMULATE MovCliente.Importe (TOTAL).
    END.

    ASSIGN 
        l-impxant  = ((ACCUM TOTAL HistMovCte.Importe * l-dias) +
                    (ACCUM TOTAL Movcliente.Importe * l-dias))
        l-imptotal = ((ACCUM TOTAL HistMovCte.Importe) +
                    (ACCUM TOTAL MovCliente.Importe)).                    
/*ASSIGN l-prompag = ((ACCUM TOTAL HistMovCte.Importe * l-dias) +
                    (ACCUM TOTAL Movcliente.Importe * l-dias)) /
                    ((ACCUM TOTAL HistMovCte.Importe) +
                    (ACCUM TOTAL MovCliente.Importe)) .
IF l-prompag = ? OR l-prompag < 0 THEN l-prompag = 0.*/

END PROCEDURE.   

PROCEDURE "cxcd0010.p":
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
       Empresa : Consultoria en Informatica Ejecutiva
       Modulo  : Cuentas por Cobrar
       Sistema : ADOSA
       Programa: cxcd0010.p
       Funcion : Saca el promedio de pago de un cliente
       Autor   : LUIS
       Fecha   : 11/04/97
    */

    DEF BUFFER b-MovCte     FOR MovCliente.
    DEF BUFFER b-MovHistCte FOR HistMovCte.
    DEF INPUT PARAMETER l-cliente LIKE Cliente.Id-Cliente NO-UNDO.
    DEF OUTPUT PARAMETER l-prompag  AS DECI    FORMAT "-zzz,zzz,zz9.99"            NO-UNDO.
    DEF VAR l-fecha AS DATE NO-UNDO.
    DEF VAR l-dias  AS INTE NO-UNDO.
    DEF VAR l-mes   AS INTE NO-UNDO.
    DEF VAR l-mes2  AS INTE NO-UNDO.  

    /*
    ASSIGN l-mes   = MONTH(g-today) - 6
           l-mes2  = (IF l-mes <= 0 THEN 12 + l-mes ELSE 0)
           l-fecha = (IF l-mes2 <> 0
                                  THEN DATE(l-mes2,DAY(g-today),YEAR(g-today) - 1)
                                  ELSE DATE(l-mes,1,YEAR(g-today)) ).
    */
    ASSIGN 
        l-Fecha = TODAY - 180.
    FOR EACH HistMovCte WHERE HistMovCte.Id-Cliente = l-cliente AND
        HistMovCte.FecReg     >= l-fecha  AND
        HistMovCte.Importe    < 0 NO-LOCK:
        FIND TipoPago WHERE TipoPago.Id-Tp = HistMovCte.Id-MC NO-LOCK NO-ERROR.
        IF NOT AVAILABLE TipoPago THEN NEXT.
        FIND FIRST b-MovHistCte WHERE b-MovHistCte.RefSaldo = HistMovCte.RefSaldo AND
            b-MovHistCte.Id-MC   <= 3 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-MovHistCte THEN NEXT.
        ASSIGN 
            l-dias = HistMovCte.FecReg - b-MovHistCte.FecReg.
        ACCUMULATE (HistMovCte.Importe * l-dias) (TOTAL).
        ACCUMULATE HistMovCte.Importe (TOTAL).
    END.

    FOR EACH MovCliente WHERE MovCliente.Id-Cliente = l-cliente AND
        MovCliente.FecReg    >= l-fecha   AND
        MovCliente.Importe    < 0 NO-LOCK:
        FIND TipoPago WHERE TipoPago.Id-Tp = MovCliente.Id-MC NO-LOCK NO-ERROR.
        IF NOT AVAILABLE TipoPago THEN NEXT.
        FIND FIRST b-MovCte WHERE b-MovCte.RefSaldo = MovCliente.RefSaldo AND
            b-MovCte.Id-MC   <= 3 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-MovCte THEN NEXT.
        FIND Acuse WHERE Acuse.Id-Acuse = MovCliente.Documento NO-LOCK NO-ERROR.
        IF (AVAILABLE Acuse) AND Acuse.Estatus <> 4 THEN NEXT.
        ASSIGN 
            l-dias = MovCliente.FecReg - b-MovCte.FecReg.
        ACCUMULATE (MovCliente.Importe * l-dias) (TOTAL).
        ACCUMULATE MovCliente.Importe (TOTAL).
    END.

    ASSIGN 
        l-prompag = ((ACCUM TOTAL HistMovCte.Importe * l-dias) +
                    (ACCUM TOTAL Movcliente.Importe * l-dias)) /
                    ((ACCUM TOTAL HistMovCte.Importe) +
                    (ACCUM TOTAL MovCliente.Importe)) .
    IF l-prompag = ? OR l-prompag < 0 THEN l-prompag = 0.


END PROCEDURE.
 
PROCEDURE DepositoSantander:
/* =========================================================================
    File : sumaDepBanco.p
    Purpose : Regresa la suma de importes de los depósitos por cliente
    Author : Angel & ChatGPT
   ========================================================================= */

DEFINE INPUT  PARAMETER ipIdCliente AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opTotal     AS DECIMAL NO-UNDO.

DEFINE VARIABLE dSuma   AS DECIMAL NO-UNDO INITIAL 0.

/* -----------------------------------------------------------
   Busca los depósitos válidos de un cliente en los últimos 90 días
   ----------------------------------------------------------- */
FOR EACH DepBanco
    WHERE DepBanco.Id-Cliente = ipIdCliente
      AND DepBanco.FecDep     >= TODAY - 90
      AND DepBanco.Conciliado = FALSE
      AND DepBanco.Activo     = TRUE
      AND DepBanco.Tipo       <> 4
    NO-LOCK:

    dSuma   = dSuma + DepBanco.Importe.
END.   

/* Retorna los resultados */
opTotal  = dSuma.

END PROCEDURE.
   
