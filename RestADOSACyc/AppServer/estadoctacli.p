@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : estadoctacli.p        
    Purpose     : 

    Syntax      :
v
    Description : 

    Author(s)   : sis6
    Created     : Wed Apr 09 10:53:50 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/
  
/* Ticket 744 Ajustar rangos de vencido en Informacion de Saldos-Estado de Cuenta
   Información de saldos en estado de cuenta debe ser igual que visor de cartera de 
    saldos, solo es necesario modificar los rangos de vencido
    JASS29052025                                               
      
      
   Ticket  772
   Se agrega en la temporal el campo Adelantado esto para identificar las facturas
   con Factura.Adelantado = true; en caso de que existan se modificia la fecha
   la toma de la fecha embarque de estpedid.fecemb; y sumando el plazo de la factura
   recalculca ahora la fecha de vencimiento y segun al dia coloca al final los dias vencidos
   que tiene  JASS23062025
     
   Ticket 1552 Parametro para indicar en el Proceso de Adelantado 
                cuando una factura tiene entrega Parcial 
                ( Si entrego pero hay productos pendientes)
                JASS14082025  
                                                              
  
*/
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttDatos
    FIELD IdCliente      AS INTEGER  
    FIELD DigVerif       AS CHARACTER
    FIELD FecSol         AS CHARACTER 
    FIELD RazonSocial    AS CHARACTER
    FIELD RFC            AS CHARACTER
    FIELD CalleNo        AS CHARACTER
    FIELD Colonia        AS CHARACTER
    FIELD Ciudad         AS CHARACTER
    FIELD Estado         AS CHARACTER
    FIELD CP             AS CHARACTER
    FIELD Telefono       AS CHARACTER
    FIELD TotChequesDev  AS DECIMAL
    FIELD TotFac         AS DECIMAL
    FIELD TotFacME       AS DECIMAL
    FIELD TotPagar       AS DECIMAL
    FIELD TotPagarME     AS DECIMAL
    FIELD SaldoActual    AS DECIMAL
    FIELD SaldoActualME  AS DECIMAL
    FIELD TotPorVencer   AS DECIMAL
    FIELD TotPorVencerME AS DECIMAL
    FIELD TotVencido     AS DECIMAL
    FIELD TotVencidoME   AS DECIMAL
    FIELD Tot30          AS DECIMAL
    FIELD Tot30ME        AS DECIMAL
    FIELD Tot60          AS DECIMAL
    FIELD Tot60ME        AS DECIMAL
    FIELD Tot90          AS DECIMAL
    FIELD Tot90ME        AS DECIMAL
    FIELD Tot91          AS DECIMAL
    FIELD Tot91ME        AS DECIMAL
    FIELD TotDevNCR      AS DECIMAL
    FIELD TotDevNCRME    AS DECIMAL
    FIELD DiasCartera    AS INTEGER
    FIELD PromedioPago   AS INTEGER
    FIELD PorcInteres    AS DECIMAL
    FIELD TasaInt        AS DECIMAL
    FIELD IntMoratorios  AS DECIMAL
    FIELD IdUser         AS CHARACTER
    FIELD NomUser        AS CHARACTER.
    
DEFINE TEMP-TABLE ttOrdenCompra
    FIELD IdCliente    AS INTEGER 
    FIELD Folio        AS CHARACTER
    FIELD OrdenCompra  AS CHARACTER
    FIELD FecReg       AS DATE
    FIELD FecVenc      AS DATE 
    FIELD Descr        AS CHARACTER 
    FIELD MontoOrig    AS DECIMAL 
    FIELD PorVencer    AS DECIMAL 
    FIELD Vencido      AS DECIMAL
    FIELD TM           AS CHARACTER 
    FIELD Plazo        AS INTEGER
    FIELD DiasVencido  AS CHARACTER
    FIELD PorPagar     AS DECIMAL
    FIELD DevNCR       AS DECIMAL
    FIELD Adelantado   AS LOGICAL  /* JASS23062025 */
    FIELD FecReg2      AS DATE
    FIELD FecVenc2     AS DATE 
    FIELD DiasVencido2 AS INT
    FIELD Parcial      AS LOGICAL /*JASS14082025 */.


DEFINE DATASET dsEstadoCuenta FOR 
    ttDatos,
    ttOrdenCompra
    DATA-RELATION EstoadoCtaCli FOR ttDatos, ttOrdenCompra
    RELATION-FIELDS (IdCliente, IdCliente) NESTED.
        
DEFINE VARIABLE l-cheques      AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE l-meses        AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-fechastring  AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE l-aste         AS CHARACTER NO-UNDO FORMAT "X".
DEFINE VARIABLE l-temporada    AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE l-dias         AS INTEGER   LABEL "Dias" FORMAT "zzz9" NO-UNDO.
DEFINE VARIABLE l-dias2        AS INTEGER   LABEL "Dias" FORMAT "zzz9" NO-UNDO. /* JASS29052025 */
DEFINE VARIABLE l-diasnorm     AS INTEGER   FORMAT "zzz9" NO-UNDO.
DEFINE VARIABLE l-diasmax      AS INTEGER   FORMAT "zzz9" NO-UNDO.
DEFINE VARIABLE l-diasint      AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-chedev       AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-chedev2      AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-prompago     AS INTEGER   FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE l-saldo        AS DECIMAL   FORMAT "z,zzz,zz9.99" NO-UNDO.
DEFINE VARIABLE l-interes      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-totncargo    AS DECIMAL   FORMAT "-zzzzzz,zz9.99" NO-UNDO.
DEFINE VARIABLE l-totintereses AS DECIMAL   FORMAT "-zzzzzz,zz9.99" NO-UNDO.
DEFINE VARIABLE l-totcargo     AS DECIMAL   FORMAT "-zzzzzz,zz9.99" NO-UNDO.
DEFINE VARIABLE l-totDevPend   AS DECIMAL   FORMAT "-zzzzzz,zz9.99" NO-UNDO.
DEFINE VARIABLE l-total        AS DECIMAL   FORMAT "-z,zzz,zz9.99" NO-UNDO.
DEFINE VARIABLE l-tasa         AS DECIMAL   FORMAT "zz9.9999" NO-UNDO.
DEFINE VARIABLE l-vencido      AS DECIMAL   LABEL "Vencido" FORMAT "->>>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE l-porvencido   AS DECIMAL   LABEL "Por Vencer" FORMAT "->>>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE l-apagar       AS DECIMAL   LABEL "A Pagar" FORMAT "->>>>>,>>9.99" NO-UNDO. 
DEFINE VARIABLE l-hubo         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-negativos    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-limite       LIKE Cliente.Limite NO-UNDO.
DEFINE VARIABLE l-plazo        LIKE Cliente.Plazo NO-UNDO.
DEFINE VARIABLE l-calidad      LIKE Cliente.Id-Calidad NO-UNDO.
DEFINE VARIABLE l-vendedor     LIKE Cliente.Id-Vendedor NO-UNDO.
DEFINE VARIABLE v-clave        AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-estatus      LIKE Factura.CteEstatus NO-UNDO.
DEFINE VARIABLE l-plazo1       AS INTEGER   FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-totalME      AS DECIMAL   FORMAT "-z,zzz,zz9.99" NO-UNDO.
DEFINE VARIABLE l-HayMonedaEX  AS LOGICAL   INITIAL FALSE NO-UNDO.
DEFINE VARIABLE l-vencidoME    AS DECIMAL   FORMAT "->>>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE l-DevNCRME     AS DECIMAL   FORMAT "->>>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE l-apagarME     AS DECIMAL   FORMAT "->>>>>,>>9.99" NO-UNDO. 
DEFINE VARIABLE l-porvencidoME AS DECIMAL   FORMAT "->>>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE l-DevNCR       LIKE MovCliente.Importe NO-UNDO.
DEFINE VARIABLE l-CompIVA      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-hayPendiente AS LOGICAL   NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetEstadoCtaCli:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipCliente AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipFecha AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipTasaInt AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipUser AS CHARACTER NO-UNDO.
    //DEFINE OUTPUT PARAMETER opError AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJson AS LONGCHAR NO-UNDO.
    
    ASSIGN 
        l-meses = "ENERO,FEBRERO,MARZO,ABRIL,MAYO,JUNIO,JULIO,AGOSTO,SEPTIEMBRE,OCTUBRE,NOVIEMBRE,DICIEMBRE".
    //OUTPUT TO /usr2/sis6/reporte.txt.
    DEFINE VARIABLE l-DV AS INTEGER NO-UNDO.
    EMPTY TEMP-TABLE ttDatos.
    EMPTY TEMP-TABLE ttOrdenCompra.
    
    DEFINE BUFFER bf-mov FOR MovCliente.
    
    ASSIGN
        l-totintereses = 0  
        l-totncargo    = 0
        l-totcargo     = 0 
        l-total        = 0        
        l-chedev       = 0
        l-chedev2      = 0 
        l-totDevPend   = 0
        l-interes      = 0  
        l-totalME      = 0.
    
    LOG-MANAGER:WRITE-MESSAGE("/EstadoCuentaCli >>> Ejecutando: Reporte Fecha " + STRING(ipFecha, "99/99/9999") + 
        " Usuario " + STRING(ipUser)).
    
    FOR EACH Cliente WHERE Cliente.Id-Cliente = ipCliente NO-LOCK:
        FIND ciudad OF Cliente NO-LOCK NO-ERROR.
        FIND Estado OF Ciudad  NO-LOCK NO-ERROR.
        
        CREATE ttDatos.  
        
        RUN /usr2/adosa/procs/vtad1000.p(INPUT Cliente.Id-Cliente, OUTPUT l-DV).
                    //l-DigVer = "-" + STRING(l-DV,"99").  
        
        ASSIGN
            ttDatos.IdCliente   = Cliente.Id-Cliente 
            ttDatos.DigVerif    = FILL("0", 8 - LENGTH(STRING(Cliente.Id-Cliente) + STRING(l-DV,"99"))) 
                   + STRING(Cliente.Id-Cliente) + STRING(l-DV, "99")
            ttDatos.FecSol      = ("AL " + STRING(DAY(ipFecha)) + " DE " + ENTRY(MONTH(ipFecha),l-meses) + " DE " + STRING(YEAR(ipFecha)))
            ttDatos.RazonSocial = Cliente.RazonSocial 
            ttDatos.RFC         = Cliente.RFC
            ttDatos.CalleNo     = Cliente.CalleNo
            ttDatos.Colonia     = Cliente.Colonia
            ttDatos.Ciudad      = Ciudad.Nombre
            ttDatos.Estado      = Estado.Nombre
            ttDatos.CP          = Cliente.CP            
            ttDatos.Telefono    = Cliente.Tel1.        
                                          
        FIND Usuario WHERE Usuario.Id-User = ipUser NO-LOCK NO-ERROR.
        ASSIGN 
            ttDatos.IdUser  = Usuario.Id-User
            ttDatos.NomUser = Usuario.Nom-Usuario.
          
        FOR EACH CheDev WHERE CheDev.Id-Cliente = Cliente.Id-Cliente
            AND CheDev.FecCargo <= ipFecha NO-LOCK:
            ACCUMULATE CheDev.FecCargo (COUNT).
        END.
        
        ASSIGN 
            l-chedev = ACCUM COUNT CheDev.FecCargo.
        
        FOR EACH MovCliente WHERE MovCliente.Id-Cliente = Cliente.Id-Cliente
            AND MovCliente.Id-MC <= 3 
            AND MovCliente.FecReg <= ipFecha NO-LOCK BREAK BY MovCliente.Id-Cliente
            BY MovCliente.Fecreg
            BY MovCliente.RefSaldo:
            
            CREATE ttOrdenCompra.
            ASSIGN 
                ttOrdenCompra.DevNCR = 0
                ttDatos.TotDeVNCR    = 0.
                //l-DevNCR = 0.
//                FOR EACH bf-Mov WHERE bf-Mov.RefSaldo = MovCliente.RefSaldo
//                    AND bf-Mov.Afectado = TRUE
//                    AND bf-Mov.FecReg <= ipFecha
//                    AND bf-Mov.Id-MC > 3 NO-LOCK:
//                    
//
//                    IF bf-Mov.Id-Mc > 63 THEN DO:
//                        ASSIGN 
//                            ttDatos.TotDesc = ttDatos.TotDesc + (bf-Mov.Importe)
//                            l-DevNCR = l-DevNCR + (bf-Mov.Importe)
//                            ttOrdenCompra.DevNCR = l-DevNCR.
//                            
//                        DISPLAY bf-Mov.Importe.
//                    END.
//                
//                    ACCUMULATE bf-Mov.Importe (TOTAL).
//                END.
            
            IF MovCliente.Id-MC = 1 THEN 
                ASSIGN 
                    ttOrdenCompra.MontoOrig = MovCliente.Importe
                    ttOrdenCompra.Descr     = "FACT.".
            
            FOR EACH bf-Mov WHERE bf-Mov.RefSaldo = MovCliente.RefSaldo
                AND bf-Mov.Afectado = TRUE
                AND bf-Mov.FecReg <= ipFecha
                AND bf-Mov.Id-MC > 3 NO-LOCK:
                
                IF bf-Mov.Id-Mc <> 65 THEN
                    FIND Acuse WHERE Acuse.Id-Acuse = bf-mov.Documento NO-LOCK NO-ERROR.
                ELSE
                    RELEASE Acuse.

                IF bf-Mov.Id-Mc > 63 THEN 
                DO:
                    ASSIGN 
                        //ttDatos.TotDevNCR = ttDatos.TotDevNCR + (bf-Mov.Importe)
                        l-DevNCR             = l-DevNCR + (bf-Mov.Importe)
                        ttOrdenCompra.DevNCR = bf-Mov.Importe.
                    
                    //DISPLAY bf-Mov.Id-Mc bf-Mov.Importe l-DevNCR.
                END.
                
                IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN NEXT.
                
                ACCUMULATE bf-Mov.Importe (TOTAL).
            END.
            
            
            ASSIGN
                l-saldo = MovCliente.Importe + ACCUM TOTAL bf-Mov.Importe.
                
                
            IF (l-saldo = 0.01 OR l-saldo = 0.01 ) AND NOT LAST-OF(MovCliente.Id-Cliente) THEN NEXT.

            IF l-saldo <= 0 THEN
                ASSIGN l-negativos = FALSE.
            ELSE
                ASSIGN l-negativos = TRUE.
                    
            IF l-Saldo <> 0.01 AND l-saldo <> -0.01 AND l-negativos THEN 
            DO:
                ASSIGN
                    ttOrdenCompra.IdCliente = Cliente.Id-Cliente
                    ttOrdenCompra.FecReg    = MovCliente.FecReg
                    ttOrdenCompra.FecVenc   = MovCliente.FecVenc.
                
                ASSIGN 
                    l-hubo = TRUE.
                
                FIND TabMC WHERE TabMC.Id-MC = MovCliente.Id-MC NO-LOCK NO-ERROR.
                IF TabMC.Id-MC = 1 THEN
                    FIND Factura WHERE Factura.Id-Factura = MovCliente.RefSaldo NO-LOCK NO-ERROR.
                    
                /*_ RNPC - Obtengo el simbolo de la moneda en caso de aplicar _*/
                IF MovCliente.Id-Moneda > 1 THEN 
                DO:
                    FIND FIRST Moneda WHERE Moneda.Id-moneda = MovCliente.Id-Moneda NO-LOCK NO-ERROR.
                    IF AVAILABLE Moneda THEN ttOrdenCompra.TM = Moneda.Simbolo. //  l-simbolo = Moneda.Simbolo.
                    l-HayMonedaEX = TRUE.
                END.
                ELSE ttOrdenCompra.TM = "". //l-simbolo = "".
                
                ASSIGN          
                    l-dias                  = (ipFecha - MovCliente.FecReg)   //duda con este
                    l-dias2                 = (ipFecha - MovCliente.FecVenc)   /* JASS29052025 */
                    l-diasnorm              = ipFecha - MovCliente.FecVenc
                    l-porvencido            = (IF MovCliente.FecVenc >= ipFecha THEN l-saldo ELSE 0)
                    l-vencido               = (IF MovCliente.FecVenc < ipFecha THEN l-saldo ELSE 0)                    
                    l-cheques               = STRING(l-chedev) + "/" + STRING(l-chedev - l-chedev2)
                    l-apagar                = (IF l-vencido <> 0 THEN l-vencido ELSE l-porvencido) + l-compiva
                    l-estatus               = IF AVAILABLE Factura THEN Factura.CteEstatus ELSE ""
                    l-plazo1                = IF AVAILABLE Factura THEN Factura.Plazo ELSE 0      
                    //ttOrdenCompra.DiasVencido = STRING(ipFecha - MovCliente.FecVenc)
                    ttOrdenCompra.PorVencer = (IF MovCliente.FecVenc >= ipFecha THEN l-saldo ELSE 0)
                    ttOrdenCompra.Vencido   = (IF MovCliente.FecVenc < ipFecha THEN l-saldo ELSE 0)
                    ttOrdenCompra.PorPagar  = (IF l-vencido <> 0 THEN l-vencido ELSE l-porvencido) + l-compiva
                    ttOrdenCompra.Plazo     = IF AVAILABLE Factura THEN Factura.Plazo ELSE 0.
                    
                IF l-diasnorm > 0 THEN ttOrdenCompra.DiasVencido = STRING(l-diasnorm).
                    
                IF AVAILABLE Factura THEN 
                DO:
                    FIND FIRST EstPedido WHERE EstPedido.Id-Factura = Factura.Id-Factura NO-LOCK NO-ERROR.
                    IF AVAILABLE EstPedido AND EstPedido.Estatus < 6 THEN
                        ASSIGN l-Estatus = SUBSTRING(l-Estatus,1,7) + "(NE)".
                END.
                
                /*_ RNPC - Acumulo valores en variables por tipo de moneda _*/
                IF MovCliente.Id-Moneda > 1 THEN 
                DO:
                    ASSIGN
                        ttDatos.SaldoActualME  = ttDatos.SaldoActualME + l-saldo
                        ttDatos.TotVencidoME   = ttDatos.TotVencidoME + l-vencido
                        ttDatos.TotPagarME     = ttDatos.TotPagarME + l-apagar
                        ttDatos.TotPorVencerME = ttDatos.TotPorVencerME + l-porvencido
                        ttDatos.TotDevNCRME    = ttDatos.TotDevNCRME + l-DevNCR
                        l-vencidoME            = l-vencidoME + l-vencido 
                        l-DevNCRME             = l-DevNCRME + l-DevNCR 
                        l-apagarME             = l-apagarME + l-apagar                                                                                                                                               
                        l-porvencidoME         = l-porvencidoME + l-porvencido.
                    ACCUMULATE (l-saldo * MovCliente.TipoCambio) * l-dias (TOTAL).       // RNPC - 2019-08-17
                    ACCUMULATE (l-saldo * MovCliente.TipoCambio) (TOTAL).                // RNPC - 2019-08-17 
                    
                    IF l-dias <= 30 THEN
                        ASSIGN ttDatos.Tot30ME = ttDatos.Tot30ME + l-vencido.
                        //ASSIGN l-tot30ME = l-tot30ME + l-saldo.
                    IF l-dias >= 31 AND l-dias <= 60 THEN
                        ASSIGN ttDatos.Tot60ME = ttDatos.Tot60ME + l-vencido.
                        //ASSIGN l-tot60ME = l-tot60ME + l-saldo.
                    IF l-dias >= 61 AND l-dias <= 90 THEN
                        ASSIGN ttDatos.Tot90ME = ttDatos.Tot90ME + l-vencido.
                        //ASSIGN l-tot75ME = l-tot75ME + l-saldo.
                    //IF l-dias >= 76 AND l-dias <= 90 THEN
                        //ASSIGN l-tot90ME = l-tot90ME + l-saldo.
                    IF l-dias > 90 THEN
                        ASSIGN ttDatos.Tot91ME = ttDatos.Tot91ME + l-vencido.
                        //ASSIGN l-tot91ME = l-tot91ME + l-saldo.
                END.
                ELSE 
                DO:
                    ttDatos.SaldoActual = ttDatos.SaldoActual + l-saldo.
                    ttDatos.TotVencido = ttDatos.TotVencido +  l-vencido.
                    ttDatos.TotPorVencer = ttDatos.TotPorVencer + l-porvencido.
                    ttDatos.TotPagar = ttDatos.TotPagar + l-apagar.
                    //ttDatos.TotDevNCR = ttDatos.TotDevNCR + l-DevNCR.
                    //l-total = l-total + l-saldo.                    
                    ACCUMULATE l-vencido (TOTAL).
                    ACCUMULATE l-DevNCR (TOTAL).
                    ACCUMULATE l-porvencido (TOTAL).
                    ACCUMULATE l-saldo * l-dias (TOTAL).
                    /*
                    ACCUMULATE l-compiva (TOTAL).
                    */
                    ACCUMULATE l-apagar (TOTAL).
                    
                    /* 
                    IF l-dias <= 30 THEN
                        ASSIGN ttDatos.Tot30 = ttDatos.Tot30 + l-vencido.
                        //ASSIGN l-tot30 = l-tot30 + l-saldo.
                    IF l-dias >= 31 AND l-dias <= 60 THEN
                        ASSIGN ttDatos.Tot60 = ttDatos.Tot60 + l-vencido.
                        //ASSIGN l-tot60 = l-tot60 + l-saldo.
                    IF l-dias >= 61 AND l-dias <= 90 THEN
                        ASSIGN ttDatos.Tot90 = ttDatos.Tot90 + l-vencido.
                        //ASSIGN l-tot75 = l-tot75 + l-saldo.
                    //IF l-dias >= 76 AND l-dias <= 90 THEN
                        //ASSIGN l-tot90 = l-tot90 + l-saldo.
                    IF l-dias > 90 THEN
                        ASSIGN ttDatos.Tot91 = ttDatos.Tot91 + l-vencido.
                        //ASSIGN l-tot91 = l-tot91 + l-saldo.   */
                        
                    /* JASS29052025 */    
                    IF l-dias2 <= 30 THEN
                        ASSIGN ttDatos.Tot30 = ttDatos.Tot30 + l-vencido.
                    IF l-dias2 >= 31 AND l-dias2 <= 60 THEN
                        ASSIGN ttDatos.Tot60 = ttDatos.Tot60 + l-vencido.
                    IF l-dias2 >= 61 AND l-dias2 <= 90 THEN
                        ASSIGN ttDatos.Tot90 = ttDatos.Tot90 + l-vencido.
                    IF l-dias2 > 90 THEN
                        ASSIGN ttDatos.Tot91 = ttDatos.Tot91 + l-vencido.
                        
                    IF MovCliente.Id-MC = 3 THEN
                        ASSIGN ttDatos.TotChequesDev = ttDatos.TotChequesDev + l-saldo
                            l-chedev2             = l-chedev2 + (IF l-saldo > 0 THEN 1 ELSE 0).
                    IF MovCliente.Id-MC = 2 THEN
                        ASSIGN ttDatos.TotDevNCR = ttDatos.TotDevNCR + l-saldo. 
                END.
                    
                FIND FIRST bf-Mov WHERE bf-mov.RefSaldo = MovCliente.REfSaldo
                    AND NOT bf-mov.Afectado NO-LOCK NO-ERROR.
                IF AVAILABLE bf-mov THEN
                    ASSIGN
                        //ttOrdenCompra.TM = "*" 
                        l-aste = "*".
                ELSE ASSIGN 
                        //ttOrdenCompra.TM = "*"
                        l-aste = "".
                
                FOR EACH bf-Mov WHERE bf-mov.RefSaldo = MovCliente.REfSaldo
                    AND bf-mov.Afectado = TRUE NO-LOCK:
                    IF bf-Mov.Id-Mc <> 65 THEN
                        FIND Acuse WHERE Acuse.Id-Acuse = bf-mov.Documento NO-LOCK NO-ERROR.
                    ELSE
                        RELEASE Acuse.
                        
                    IF AVAILABLE Acuse AND Acuse.Estatus <> 4 
                        THEN 
                        ASSIGN 
                                //ttOrdenCompra.TM = "&"
                            l-aste = "&".
                END.
                
                    //ttOrdenCompra.Folio = "".
                    
                IF Movcliente.Id-MC = 1 THEN 
                DO:
                    FIND Factura WHERE Factura.Id-Factura = Movcliente.RefSaldo NO-LOCK NO-ERROR.
                    IF AVAILABLE Factura THEN
                    DO:
                        ASSIGN
                            l-temporada         = IF Factura.Id-Cond = 1 THEN "S"
                                  ELSE IF Factura.Id-Cond = 4 THEN "E"
                                  ELSE IF Factura.Id-Cond = 5 THEN "N"
                                  ELSE ""
                            ttOrdenCompra.Folio = IF Factura.Id-Fiscal <> "" 
                                  THEN Factura.Id-Fiscal 
                                  ELSE Factura.Id-Factura.

                        IF Factura.Adelantado THEN  /* JASS23062025 */
                        DO:
                            ASSIGN 
                                ttOrdenCompra.Adelantado = TRUE.
                            FIND LAST Pedido 
                                WHERE Pedido.Id-Pedido = Factura.Pedidos
                                NO-LOCK NO-ERROR.

                            IF AVAILABLE Pedido THEN 
                            DO:
                                
                                /* Inicializar bandera */
                                l-hayPendiente = FALSE.

                                /* Revisar si hay artículos pendientes */
                                FOR EACH DetPedido 
                                    WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido
                                    AND (DetPedido.CantPed - DetPedido.CantEnt) > 0
                                    NO-LOCK:

                                    l-hayPendiente = TRUE.
                                    LEAVE. /* Con encontrar uno es suficiente */
                                END.
                                FIND EstPedido 
                                    WHERE EstPedido.Id-Pedido = Pedido.Id-Pedido 
                                    AND EstPedido.Id-Seq = Pedido.Resto 
                                    NO-LOCK NO-ERROR.

                                IF AVAILABLE EstPedido THEN 
                                DO:
                                    ASSIGN  
                                        ttOrdenCompra.Parcial  = l-hayPendiente /* JASS14082025 */ 
                                        ttOrdenCompra.FecReg2  = EstPedido.FecEmb
                                        ttOrdenCompra.FecVenc2 = EstPedido.FecEmb + ttOrdenCompra.Plazo.

                                    /* Aquí calculamos días vencidos con base en la fecha de input */
                                    ttOrdenCompra.DiasVencido2 = INTEGER(ipFecha - ttOrdenCompra.FecVenc2).
                                END.
                            END. /* JASS23062025 */
                        END.
                     END.     /* IF AVAILABLE FACTURA */
                    END.
                    ELSE 
                    DO:    
                        ASSIGN
                            l-temporada         = ""
                            ttOrdenCompra.Folio = MovCliente.RefSaldo.
                    END.
                    /*
                    IF Movcliente.Id-MC = 1 THEN 
                    DO:
                        FIND Factura WHERE Factura.Id-Factura = Movcliente.RefSaldo NO-LOCK NO-ERROR.
                        IF AVAILABLE Factura THEN
                            ASSIGN
                                l-temporada         = IF Factura.Id-Cond = 1
                                              THEN "S"
                                              ELSE IF Factura.Id-Cond = 4
                                                   THEN "E"
                                                   ELSE IF Factura.Id-Cond = 5
                                                        THEN "N"
                                                        ELSE ""
                                ttOrdenCompra.Folio = IF Factura.Id-Fiscal <> ""
                                           THEN Factura.Id-Fiscal
                                           ELSE Factura.Id-Factura.
                    END.
                    ELSE
                        ASSIGN
                            l-temporada         = ""
                            ttOrdenCompra.Folio = MovCliente.RefSaldo.
                    */    
                    ASSIGN                           
                        ttOrdenCompra.OrdenCompra = (IF AVAILABLE factura THEN STRING(Factura.requisicion) ELSE "") 
                        v-clave                   = (IF AVAILABLE factura THEN STRING(Factura.requisicion) ELSE "").
                                  
                
                    IF MovCliente.Id-MC = 1 THEN 
                    DO:
                        FIND FIRST SysGeneral NO-LOCK NO-ERROR.
                        ASSIGN
                            ttDatos.TasaInt = ipTasaInt / 100 / 30.42
                            l-tasa          = ipTasaInt / 100 / 30.42.
                   
                        IF MovCliente.Id-Moneda > 1 THEN ASSIGN l-tasa = 0.
                    
                        ASSIGN
                            l-diasint             = ipFecha - (MovCliente.FecVenc)
                            ttDatos.IntMoratorios = ttDatos.IntMoratorios + (IF l-diasint > 0
                                                           THEN l-saldo * l-tasa * l-diasint
                                                           ELSE 0)                           
                            l-totintereses        = l-totintereses + (IF l-diasint > 0
                                                           THEN l-saldo * l-tasa * l-diasint
                                                           ELSE 0)
                            l-interes             = l-interes + (IF l-diasint > 0
                                                 THEN (l-saldo * l-tasa * l-diasint)
                                                 ELSE 0).
                    
                        IF MovCliente.Id-Moneda > 1 THEN ttDatos.TotFacME = ttDatos.TotFacME + l-saldo.
                        ELSE ttDatos.TotFac = ttDatos.TotFac + l-saldo.
                     
                    END.
               
                END.
             
                IF LAST-OF(MovCliente.Id-Cliente) THEN 
                DO:
                                    
                    FOR EACH Devolucion WHERE Devolucion.Id-Cliente = Cliente.Id-Cliente
                        AND Devolucion.FecApl = ?
                        AND Devolucion.FecCanc = ?
                        AND Devolucion.TipoVenta = 3
                        AND Devolucion.FecReg <= ipFecha NO-LOCK:
                        CREATE ttOrdenCompra. 
                        ASSIGN                            
                            l-dias       = ipFecha - Devolucion.FecReg
                            l-totDevPend = l-totDevPend + devolucion.Tot.
                            
                        ASSIGN
                            ttOrdenCompra.Folio       = Devolucion.Id-Factura
                            ttOrdenCompra.OrdenCompra = "DEV. PEND. # " + STRING(Devolucion.Id-Dev)
                            ttOrdenCompra.FecReg      = Devolucion.FecReg
                            ttOrdenCompra.DiasVencido = STRING(l-dias) + "d"
                            ttOrdenCompra.DevNCR      = Devolucion.Tot.

                        
                        IF l-dias <= 30 THEN
                            ASSIGN ttDatos.Tot30 = ttDatos.Tot30 - Devolucion.Tot.
                        IF l-dias >= 31 AND l-dias <= 60 THEN
                            ASSIGN ttDatos.Tot60 = ttDatos.Tot60 - Devolucion.Tot.
                        IF l-dias >= 61 AND l-dias <= 90 THEN
                            ASSIGN ttDatos.Tot90 = ttDatos.Tot90 - Devolucion.Tot.                        
                        IF l-dias > 90 THEN
                            ASSIGN ttDatos.Tot91 = ttDatos.Tot91 - Devolucion.Tot.
                        
                    END.
                
                    IF l-hubo THEN 
                    DO:
                    
                        ASSIGN
                            ttDatos.TotVencido   = ((ACCUM TOTAL l-vencido) + l-interes)
                     //   ttDatos.TotDevNCR    = ((ACCUM TOTAL l-DevNCR) + l-TotDevPend)
                            ttDatos.TotPorVencer = ((ACCUM TOTAL l-porvencido) + l-TotCargo)
                            ttDatos.TotPagar     = ACCUM TOTAL l-apagar.
                        
                     
                        RUN /usr2/adosa/procs/cxcd0010.p (INPUT MovCliente.Id-Cliente, OUTPUT l-prompago).
                     
                        ASSIGN 
                            ttDatos.SaldoActual  = ttDatos.SaldoActual + l-totCargo - l-totDevPend
                            ttDatos.PromedioPago = l-prompago
                            ttDatos.DiasCartera  = (((ACCUM TOTAL l-saldo * l-dias) + (ACCUM TOTAL (l-saldo * MovCliente.TipoCambio) * l-dias))) 
                                        / (ttDatos.SaldoActual + (ACCUM TOTAL l-saldo * MovCliente.TipoCambio)).
                        l-diasmax           = (((ACCUM TOTAL l-saldo * l-dias) + (ACCUM TOTAL (l-saldo * MovCliente.TipoCambio) * l-dias))) 
                            / (ttDatos.SaldoActual + (ACCUM TOTAL l-saldo * MovCliente.TipoCambio)).    // RNPC - 2019-08-17
                            
                    
                        ASSIGN 
                            ttDatos.Tot30       = ttDatos.Tot30 + l-interes
                            ttDatos.SaldoActual = ttDatos.SaldoActual + l-interes.
                                
                        IF l-diasmax < 0 THEN
                            l-diasmax = 0.
                     
                        ASSIGN 
                            l-hubo = FALSE.
                    END. /* si hubo registros */
                 
                    ASSIGN
                        l-totintereses = 0  
                        l-totncargo    = 0
                        l-totcargo     = 0  
                        l-total        = 0
                        l-chedev       = 0
                        l-chedev2      = 0  
                        l-totDevPend   = 0  
                        l-interes      = 0
                        l-totalME      = 0.          
                END.        
            END.
               
        END.         
    
    
    //OUTPUT CLOSE.
        DATASET dsEstadoCuenta:WRITE-JSON("LONGCHAR", opcJson, TRUE).
        RETURN.   
    

    END PROCEDURE.

