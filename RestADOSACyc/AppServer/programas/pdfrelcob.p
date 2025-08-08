/*
    Empresa : ADOSA
    Programa: cxcc2025.p /cambio de nombre para validar rutas
    Funcion : Genera una impresion en PDF de una relacion de cobranza
    Autor   : ALEX
    Fecha   : 5 de Diciembre DEL 2024 
*/

{/usr2/adosa/includes/sia00000.var "NEW GLOBAL"}  
IF USERID("dictdb") = "" THEN
    RUN /usr2/adosa/procs/propath.p.
FIND FIRST SysGeneral NO-LOCK NO-ERROR.
ASSIGN g-nomcia   = sysgeneral.Empresa. 

//OUTPUT TO /usr2/adosa/akubica/jjjj.txt APPEND.
//PUT UNFORMATTED SKIP "cxcc2025.p -->  g-origen = " g-origen + " " + USERID('DICTDB') + " " + STRING(TIME, "HH:MM:SS") + " PROGRAM-NAME(2) = " + PROGRAM-NAME(2) SKIP.
//OUTPUT CLOSE.
//  
///* Cambiar los permisos del archivo a 777 */
//UNIX SILENT VALUE("chmod 777 /usr2/adosa/akubica/jjjj.txt").


DEFINE NEW SHARED VARIABLE v-Login AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE l-Arch2 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-Arch1 AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER ipFolio AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opArchivo AS CHARACTER NO-UNDO.

DEFINE VARIABLE l-todo      LIKE MovCliente.Saldo NO-UNDO.
DEFINE VARIABLE l-todo2     LIKE MovCliente.Saldo NO-UNDO.
DEFINE VARIABLE l-tot1      LIKE l-todo NO-UNDO.
DEFINE VARIABLE l-tot2      LIKE l-todo NO-UNDO.
DEFINE VARIABLE l-tot3      LIKE l-todo NO-UNDO.
DEFINE VARIABLE l-tot4      LIKE l-todo NO-UNDO.
DEFINE VARIABLE l-tot5      LIKE l-todo NO-UNDO.
DEFINE VARIABLE l-tot6      LIKE l-todo NO-UNDO.

DEFINE VARIABLE l-reporte   AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-CobNom    AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-mes       AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-fecstr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-aste      AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE l-DigVer    AS CHARACTER FORMAT 'X(3)'.
DEFINE VARIABLE l-CteStr    AS CHARACTER FORMAT 'X(8)'.

DEFINE VARIABLE l-todocomp  AS DECIMAL FORMAT 'zzzz,zz9.99' NO-UNDO.
DEFINE VARIABLE l-todocomp2 AS DECIMAL FORMAT 'zzzz,zz9.99' NO-UNDO.
DEFINE VARIABLE l-Imp       AS DECIMAL FORMAT 'zzzz,zz9.99' NO-UNDO.
DEFINE VARIABLE l-CompIVA   AS DECIMAL FORMAT 'zzzz,zz9.99' NO-UNDO.
DEFINE VARIABLE l-Saldo     AS DECIMAL FORMAT 'zzzz,zz9.99' NO-UNDO.

DEFINE VARIABLE l-Dias      AS INTEGER NO-UNDO.
DEFINE VARIABLE l-totdoc    AS INTEGER FORMAT 'zz9' NO-UNDO INITIAL 0.
DEFINE VARIABLE l-totdoc2   AS INTEGER FORMAT 'zz9' NO-UNDO INITIAL 0.
DEFINE VARIABLE l-I         AS INTEGER.
DEFINE VARIABLE l-Suma      AS INTEGER.
DEFINE VARIABLE l-largo     AS INTEGER INITIAL 20 NO-UNDO.

DEFINE BUFFER b-DetRCob FOR DetRCob.
DEFINE STREAM s-salida.

DEFINE BUFFER Mov  FOR MovCliente.
DEFINE BUFFER b-Mov FOR MovCliente.

/* Forma del reporte */

FORM
    '=============== =========== =============      ==' SKIP
    '='
    l-todo
    l-todocomp  
    l-todo2 SPACE(5)            
    l-totdoc  
    ''    SKIP
    '=============== =========== =============      =='
WITH FRAME f-totall OVERLAY NO-BOX NO-LABELS COL 22 DOWN.

FORM           
    MovCliente.RefSaldo   FORMAT "x(8)" AT 1
    MovCliente.FecReg     AT 10
    l-imp                 AT 24 FORMAT '-Z,ZZZ,ZZ9.99'
    l-compiva             AT 39 FORMAT 'zzz,zz9.99'
    l-saldo               AT 50 FORMAT '-Z,ZZZ,ZZ9.99'  
    l-aste FORMAT "x"
    l-dias                AT 65 FORMAT "zz9"
    DetRCob.Flag     FORMAT "x(2)"  
WITH FRAME f-x OVERLAY NO-BOX DOWN NO-LABELS.

FORM
    l-CteStr
    Cliente.RazonSocial   AT 10
WITH FRAME f-Cli1 OVERLAY NO-BOX NO-LABELS.

FORM HEADER
    "CTA" AT 2
    "    N  O  M  B  R  E                                      R E P O R T E    "
    SKIP
    "FACTURA  FECHA               IMPORTE COMPLEMENT0         SALDO DIAS"
    '------------------------------------------------------------------------------'
WITH FRAME f-cobr OVERLAY PAGE-TOP WIDTH 80 NO-BOX.

ASSIGN 
    l-reporte = "/tmp/RELCOB" + STRING(ipFolio) + ".txt".
   
OUTPUT STREAM s-salida TO VALUE(l-reporte) PAGED PAGE-SIZE 60.
 
FIND FIRST RCob WHERE RCob.Id-RelCob = ipFolio NO-LOCK NO-ERROR.
IF NOT AVAILABLE RCob THEN RETURN.

FIND Cobrador WHERE Cobrador.Id-Cobrador = RCob.Id-cobrador NO-LOCK  NO-ERROR.
FIND Empleado WHERE Empleado.Iniciales = Cobrador.Iniciales
                AND Empleado.Activo NO-LOCK NO-ERROR.

ASSIGN 
    l-mes    = ENTRY(MONTH(TODAY),'ENERO,FEBRERO,MARZO,ABRIL,MAYO,JUNIO,JULIO,AGOSTO,SEPTIEMBRE,OCTUBRE,NOVIEMBRE,DICIEMBRE')
    l-fecstr = ' ' + STRING(DAY(TODAY)) + ' DE ' + l-mes + ' DE ' + STRING(YEAR(TODAY),'9999')
    l-CobNom = STRING(Cobrador.Id-Cobrador,'Z9') + '  ' + (IF AVAILABLE Empleado THEN CAPS(Empleado.Nombre) ELSE "") + '                                                  ' .

{/usr2/adosa/includes/cieheadr.i
   &Titulo   =  "'RELACION DE COBRANZA #' + ' ' + string(RCob.Id-RelCob,'999999')" 
   &subtitulo = " l-fecstr "
   &Aveces    = l-CobNom + 
   &ancho     = 80
   &Anchom    = 74
   &Stream    = s-salida
}

FOR EACH b-DetRCob WHERE b-DetRCob.Id-RelCob = RCob.Id-RelCob NO-LOCK BREAK BY b-DetRCob.Id-Cliente:
    IF LAST-OF(b-DetRCob.Id-Cliente) THEN 
        FOR EACH MovCliente WHERE MovCliente.Id-Cliente = b-DetRCob.Id-Cliente
                              AND MovCliente.Id-MC <= 3
                              AND MovCliente.Saldo > 0 NO-LOCK:
            ASSIGN 
                l-saldo = MovCliente.Saldo.
            FOR EACH Mov WHERE Mov.Refsaldo = MovCliente.Refsaldo
                           AND Mov.Afectado = FALSE NO-LOCK:
                ASSIGN 
                    l-saldo = l-saldo + Mov.Importe.
            END.
            IF l-saldo > 10 THEN DO:
                FIND FIRST DetRCob WHERE DetRCob.Documento  = MovCliente.Documento AND DetRCob.Id-RelCob = RCob.Id-RelCob NO-LOCK NO-ERROR.
                IF AVAILABLE DetRCob THEN NEXT. 
                FIND Cliente WHERE Cliente.Id-Cliente = MovCliente.Id-Cliente NO-LOCK NO-ERROR.
                CREATE DetRCob.
                ACCUMULATE MovCliente.Id-Cliente (COUNT).
                ASSIGN 
                    DetRCob.Id-Relcob  = RCob.Id-RelCob
                    DetRCob.Id-Cliente = MovCliente.Id-Cliente
                    DetRCob.Documento  = MovCliente.Documento
                    DetRCob.FecMov     = MovCliente.FecReg
                    DetRCob.Descr      = Cliente.RazonSoc
                    DetRCob.Id-MC      = MovCliente.Id-MC                                
                    DetRCob.Flag       = 'X'
                    DetRCob.Importe    = MovCliente.Importe
                    DetRCob.Saldo      = l-saldo.
            END.
        END. 
END.          

l-saldo = 0.

VIEW STREAM s-salida FRAME f-cobr.

FOR EACH DetRCob WHERE DetRCob.Id-RelCob = RCob.Id-RelCob
                 NO-LOCK BREAK BY DetRCob.Descr
                               BY DetRCob.Id-Cliente
                               BY DetRCob.FecMov
                               BY DetRCob.Documento WITH FRAME f-x:
    FIND FIRST Cliente WHERE Cliente.Id-Cliente = DetRCob.Id-cliente NO-LOCK NO-ERROR.
    IF FIRST-OF(DetRCob.id-cliente) THEN DO:
        RUN /usr2/adosa/procs/vtad1000.p(INPUT Cliente.Id-Cliente, OUTPUT l-Suma).
        ASSIGN 
            l-digver = "-" + STRING(l-Suma,'99')
            l-CteStr = STRING(Cliente.Id-Cliente) + l-DigVer.

        DISPLAY STREAM s-salida
            l-CteStr
            (Cliente.RazonSocial +
            (IF Cliente.Curp <> "" THEN "" ELSE "*")) @ Cliente.RazonSocial
        WITH FRAME f-Cli1.
    END.
    {/usr2/adosa/includes/cxca0005.i &Factura = DetRCob.Documento}
    ASSIGN 
        l-saldo = MovCliente.Importe.
    FOR EACH Mov WHERE Mov.Refsaldo = DetRCob.Documento
        AND Mov.Afectado = FALSE
        AND Mov.Id-Mc > 3 NO-LOCK:
        ASSIGN 
            l-saldo = l-saldo + Mov.Importe.
    END.

    FIND FIRST b-Mov WHERE b-Mov.RefSaldo = MovCliente.RefSaldo AND
        NOT b-Mov.afectado NO-LOCK NO-ERROR.
    IF AVAILABLE b-Mov THEN
        ASSIGN l-aste = "*".
    ELSE ASSIGN l-aste = "".
    
    FIND FIRST Factura WHERE Factura.id-factura = Movcliente.RefSaldo NO-LOCK NO-ERROR.
    IF AVAILABLE Factura AND MovCliente.id-Mc = 1 THEN l-compiva = Factura.complmonto. 
    ELSE l-compiva = 0.
    
    DISPLAY STREAM s-salida                
        MovCliente.RefSaldo + (IF (DetRCob.Importe - DetRCob.Saldo) <> 0 THEN "*" ELSE '') @ MovCliente.RefSaldo
        MovCliente.FecReg WHEN AVAILABLE MovCliente
        DetRCob.Importe @ l-imp
        l-compiva WHEN l-compiva > 0 @ l-compiva 
        (DetRCob.Saldo + l-compiva) @ l-saldo      
        l-aste
        IF (TODAY - MovCliente.FecReg > 0) THEN (TODAY - MovCliente.FecReg) ELSE 0 @ l-dias
        DetRCob.Flag             
    WITH FRAME f-x DOWN.
    DOWN STREAM s-salida WITH FRAME f-x.
    ASSIGN 
        l-Tot1   = l-Tot1 + DetRCob.Importe
        l-Tot2   = l-Tot2 + DetRCob.Importe
        l-Tot3   = l-Tot3 + DetRCob.Saldo + l-compiva
        l-Tot4   = l-Tot4 + DetRCob.Saldo + l-compiva
        l-Tot5   = l-Tot5 + l-compiva
        l-Tot6   = l-Tot6 + l-compiva
        l-totdoc = l-totdoc + (IF DetRCob.Flag = 'F' OR DetRCob.Flag = 'CR' THEN 1 ELSE 0).           

    IF LAST-OF(DetRCob.Id-Cliente) THEN DO:
        DISPLAY STREAM s-salida
            l-Tot1 @ l-Todo
            l-Tot3 @ l-Todo2
            l-Tot5 @ l-TodoComp         
            l-totdoc
            SKIP(1)
        WITH FRAME f-totall.
        ASSIGN 
            l-Tot1    = 0
            l-Tot3    = 0
            l-Tot5    = 0
            l-totdoc2 = l-totdoc2 + l-totdoc
            l-TotDoc  = 0.
    END.

    IF LAST (DetRCob.Id-Cliente) THEN 
    DO:
        DOWN STREAM s-salida WITH FRAME f-totall.
        DISPLAY STREAM s-salida
            l-Tot2 @ l-todo
            l-Tot4 @ l-todo2
            l-Tot6 @ l-todocomp
            l-totdoc2 @ l-totdoc
        WITH FRAME f-totall.
        ASSIGN 
            l-Tot2    = 0
            l-Tot4    = 0
            l-Tot6    = 0
            l-totdoc2 = 0.
    END.
END.

HIDE FRAME f-totall   NO-PAUSE.
HIDE FRAME f-menu-opc NO-PAUSE.
HIDE FRAME f-cie-blk1 NO-PAUSE.
HIDE FRAME f-cob      NO-PAUSE.
HIDE FRAME f-x        NO-PAUSE.
HIDE FRAME f-Cli1     NO-PAUSE.

OUTPUT STREAM s-salida CLOSE.

UNIX SILENT VALUE("chmod 777 " + l-Reporte).

/* Convierte a PDF */
RUN /usr2/adosa/procs/rptpdf01.p (INPUT l-Reporte,
                                  INPUT "V",
                                  OUTPUT l-Arch2).

DO l-i = 1 TO 100:
    IF SEARCH("/tmp/" + STRING(l-i) + SUBSTRING(TRIM(l-Arch2),R-INDEX(TRIM(l-Arch2),"/") + 1,LENGTH(TRIM(l-Arch2)))) = ? THEN LEAVE. 
END.
UNIX SILENT VALUE('cp ' + TRIM(l-Arch2) + ' /tmp/' + STRING(l-i) + SUBSTRING(TRIM(l-Arch2),R-INDEX(TRIM(l-Arch2),"/") + 1,LENGTH(TRIM(l-Arch2)))).
 
ASSIGN
    v-Arch1 = SUBSTRING(TRIM(l-Arch2),R-INDEX(TRIM(l-Arch2),'/') + 1, R-INDEX(TRIM(l-Arch2),".")) + ".pdf:type=application/pdf:filetype=B64ENCODED".

IF v-Arch1 MATCHES "*.txt*" THEN
    v-Arch1 = REPLACE(v-Arch1,".txt","").

ASSIGN
    l-Arch2 = '/tmp/' + STRING(l-i) + SUBSTRING(TRIM(l-Arch2),R-INDEX(TRIM(l-Arch2),'/') + 1,LENGTH(TRIM(l-Arch2))).

UNIX SILENT VALUE("chmod 777 " + l-Arch2).
    
ASSIGN opArchivo = REPLACE(l-Arch2, ".txt", ".pdf").

UNIX SILENT VALUE("cp " + l-Arch2 + " " + opArchivo).                                  

ASSIGN opArchivo = REPLACE(opArchivo, "/tmp/", "").
                                  
RETURN.
