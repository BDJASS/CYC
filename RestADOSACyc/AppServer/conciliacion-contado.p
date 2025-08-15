@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
DEFINE TEMP-TABLE ttDocto
    FIELD IdUser      AS CHARACTER
    FIELD IdCliente   LIKE Acuse.Id-Cliente
    FIELD IdDoc       LIKE DocAcuse.Documento
    FIELD FecReg      LIKE Factura.FecReg
    FIELD RazonSocial LIKE Factura.RazonSocial
    FIELD Total       AS DECIMAL
    FIELD SubTotal    LIKE Factura.SubTotal      
    FIELD TDoc        AS CHARACTER
    FIELD ImpPago     AS DECIMAL
    FIELD ImpAnt      AS DECIMAL    
    FIELD Desc1       AS DECIMAL
    FIELD Desc2       AS DECIMAL
    FIELD Desc3       AS DECIMAL
    FIELD Desc4       AS DECIMAL
    FIELD Desc5       AS DECIMAL
    FIELD Desc6       AS DECIMAL    
    FIELD Desc7       AS DECIMAL
    FIELD Desc8       AS DECIMAL
    FIELD Desc9       AS DECIMAL
    FIELD Desc10      AS DECIMAL
    FIELD Desc11      AS DECIMAL
    FIELD Desc12      AS DECIMAL
    FIELD IdCobrador  LIKE Cobrador.Id-Cobrador  
    FIELD Comentarios AS CHARACTER
    INDEX Idx-Def TDoc IdDoc.     
    
DEFINE TEMP-TABLE ttAnticipos
    FIELD IdAnticipo  LIKE Anticipo.Id-Anticipo
    FIELD ImpAplicado LIKE DetAnticipo.Importe
    FIELD IdDoc       LIKE DocAcuse.Documento
    FIELD RelationId  AS CHARACTER /* Relaci�n con ttDocto */
    INDEX Idx-Def IdDoc.
    
DEFINE TEMP-TABLE ttPago     
    FIELD FormaPago     AS INTEGER 
    FIELD Importe       AS DECIMAL
    FIELD Rec           AS RECID
    FIELD FecDep        AS DATE
    FIELD IdBanco       AS INTEGER
    FIELD Cuenta        AS CHARACTER  
    FIELD FolioCheque   AS CHARACTER
    FIELD FechaCheque   AS DATE 
    FIELD Observaciones AS CHARACTER
    FIELD RelationId    AS CHARACTER /* Relación con ttDocto */
    INDEX Idx-Def Importe DESCENDING.  

DEFINE DATASET dsConciliacion FOR 
    ttDocto,
    ttAnticipos,
    ttPago
    DATA-RELATION drDoctoAnticipos FOR ttDocto, ttAnticipos
    RELATION-FIELDS (IdDoc, IdDoc)
    NESTED
    DATA-RELATION drDoctoPago FOR ttDocto, ttPago
    RELATION-FIELDS (IdDoc, RelationId)
    NESTED.

DEFINE VARIABLE v-Status    AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-TieneDesc AS LOGICAL NO-UNDO.
DEFINE BUFFER b-CtlCaja  FOR CtlCaja.
DEFINE BUFFER b-Pedido   FOR Pedido.
DEFINE BUFFER b-Remision FOR Remision.
DEFINE BUFFER b-DepBanco FOR DepBanco.
DEFINE BUFFER b-ttDocto  FOR ttDocto.


DEFINE NEW SHARED VARIABLE g-Origen AS CHARACTER NO-UNDO INITIAL "02B".
DEFINE NEW SHARED VARIABLE g-nomcia AS CHARACTER NO-UNDO.  
DEFINE NEW SHARED VARIABLE g-dist   AS INTEGER   FORMAT "9999" NO-UNDO.
DEFINE NEW SHARED VARIABLE g-tty    AS CHARACTER NO-UNDO.  
DEFINE NEW SHARED VARIABLE g-iva    AS INTEGER   FORMAT "z9%" NO-UNDO.
/* **********************  Internal Procedures  *********************** */
FIND FIRST SysGeneral NO-LOCK NO-ERROR.
IF AVAILABLE SysGeneral THEN DO : 
     ASSIGN g-nomcia = SysGeneral.Empresa
            g-iva    = sysgeneral.Porc-IVA.       
END.  

ASSIGN 
    g-tty = STRING(TIME).



/* **********************  Internal Procedures  *********************** */



@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostGeneraPago:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER DATASET FOR dsConciliacion.
    DEFINE OUTPUT PARAMETER p-Aplica AS CHARACTER NO-UNDO.
    

    DEFINE VARIABLE ip-formapago   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-Veces        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE l-NPagos       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE l-Nda          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE l-TP           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE l-recmov       AS RECID     NO-UNDO.
    DEFINE VARIABLE l-FecVence     AS DATE      NO-UNDO.
    DEFINE VARIABLE l-Ubic         LIKE MovCliente.Id-Ubic NO-UNDO.

    DEFINE VARIABLE l-folAcuse     LIKE Acuse.Id-Acuse NO-UNDO INITIAL "".
    DEFINE VARIABLE l-folAntAcuse  LIKE Acuse.Id-Acuse NO-UNDO INITIAL "".
    DEFINE VARIABLE l-ImpPagado    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-ImpDeposito  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-depsantander AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-RestoAnt     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-AntApl       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-FecDep       AS DATE      NO-UNDO.
    DEFINE VARIABLE l-UsuApl       AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE meses          AS CHARACTER EXTENT 12 NO-UNDO INITIAL 
        ["ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC"].

    DEFINE VARIABLE l-reccaja      AS RECID     NO-UNDO.  
    DEFINE VARIABLE l-PagInfo      LIKE Pedido.PagInfo NO-UNDO.

       
    p-Aplica = "NO APLICADO".
    /* VALIDACION PARA REVISAR SI YA SE CONCILIO UN PAGO SANTANDER */

    FIND FIRST ttPago WHERE ttPago.Rec <> ? NO-LOCK NO-ERROR.
    IF AVAILABLE ttPago THEN 
    DO:
        FIND FIRST DepBanco WHERE RECID(DepBanco) = ttPago.Rec EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE DepBanco AND DepBanco.Conciliado = TRUE THEN
        DO:
            ASSIGN 
                p-Aplica = "NO APLICADO".
            RETURN.
        END.   
    END.          
    FIND FIRST ttDocto WHERE ttDocto.IdCliente > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE ttDocto THEN 
    DO:
        l-UsuApl = ttDocto.IdUser.
        FIND Usuario WHERE Usuario.Id-User = l-UsuApl NO-LOCK NO-ERROR.
        FIND Caja WHERE Caja.Id-Caja = INTEGER(Usuario.Id-Caja) NO-LOCK NO-ERROR. 
        FIND FIRST Empleado WHERE Empleado.Iniciales = Usuario.Id-User NO-LOCK NO-ERROR.
    END.
    ELSE RETURN.   


    l-FecDep       = TODAY.
    l-PagInfo      = "App".   
    l-depsantander = 0.
    FIND FIRST ttPago WHERE ttPago.Rec <> ? NO-LOCK NO-ERROR.
    IF AVAILABLE ttPago THEN 
    DO:
        FIND FIRST DepBanco WHERE RECID(DepBanco) = ttPago.Rec EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE DepBanco THEN 
        DO:
            l-FecDep = DepBanco.FecDep.
            ASSIGN 
                l-PagInfo      = "DEP SANTANDER, " + STRING(DepBanco.FecDep,"99/99/99") + ", " +
                          (IF LENGTH(DepBanco.HoraDep) = 4 
                           THEN SUBSTRING(STRING(DepBanco.HoraDep),1,2) + ':' + 
                                SUBSTRING(STRING(DepBanco.HoraDep),3,2)
                           ELSE '0' + SUBSTRING(STRING(DepBanco.HoraDep),1,1) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),2,2)) +
                          ", " + TRIM(STRING(DepBanco.Importe,"$>>>>,>>>,>>9.99"))
                l-depsantander = DepBanco.Importe.
        END.
        ELSE 
        DO :
            l-FecDep  = ttPago.FecDep.
            IF ttPago.Observaciones <> "" THEN 
            DO:
                ASSIGN
                    l-PagInfo = l-PagInfo + " " + STRING(ttPago.FecDep, "99/99/99") + " " + TRIM(ttPago.Observaciones).  
            END.
            ELSE 
            DO:
                l-PagInfo = l-PagInfo + " " + STRING(ttPago.FecDep, "99/99/99").
            END.  
        END.                            
    END.
    ELSE 
    DO:
        FIND FIRST ttPago WHERE ttPago.FecDep <> ? NO-LOCK NO-ERROR. 
        IF AVAILABLE ttPago THEN 
        DO:
            ASSIGN 
                l-FecDep = ttPago.FecDep.
        END.
    END.

          
    l-veces = 1.
    l-NPagos = 1.
    l-ImpPagado = 0.


    FOR EACH ttDocto WHERE ttDocto.IdCliente > 0 AND ttDocto.IdDoc > "" AND ttDocto.TDoc = "CONTADO" 
        NO-LOCK BY ttDocto.IdDoc:  
        
        /* Inicializas como falso */
        ASSIGN 
            v-TieneDesc = FALSE.

        /* Verificas si alguno de los descuentos es mayor a 0 */
        IF ttDocto.desc1  > 0 OR 
            ttDocto.desc2 > 0 OR
            ttDocto.desc3 > 0 OR
            ttDocto.desc4 > 0 OR
            ttDocto.desc5 > 0 OR 
            ttDocto.desc6 > 0 OR 
            ttDocto.desc7 > 0 OR 
            ttDocto.desc8 > 0 OR 
            ttDocto.desc9 > 0 OR 
            ttDocto.desc10 > 0 OR 
            ttDocto.desc11 > 0 OR 
            ttDocto.desc12 > 0 THEN
            ASSIGN v-TieneDesc = TRUE.   
            
        FIND LAST CtlCaja WHERE CtlCaja.Id-Caja   = Caja.Id-Caja AND
            CtlCaja.FecCierre = ? NO-LOCK NO-ERROR.
        IF NOT AVAILABLE CtlCaja THEN 
        DO:
            FIND LAST b-CtlCaja WHERE b-CtlCaja.Id-Caja   = Caja.Id-Caja AND
                b-CtlCaja.FecCierre <> ? NO-LOCK NO-ERROR.

            CREATE CtlCaja.
            ASSIGN 
                CtlCaja.Id-Caja      = Caja.Id-Caja 
                CtlCaja.Turno        = 1
                CtlCaja.FecOper      = TODAY
                CtlCaja.FecApertura  = TODAY
                CtlCaja.HoraApertura = TIME
                CtlCaja.FolioIni     = (IF AVAILABLE b-CtlCaja THEN b-CtlCaja.FolioFin ELSE 0)
                CtlCaja.FolioFin     = CtlCaja.FolioIni
                CtlCaja.Dotacion     = 0.  
        END.
    
        ASSIGN 
            l-reccaja = RECID(CtlCaja).

        FIND CtlCaja WHERE RECID(CtlCaja) = l-reccaja EXCLUSIVE-LOCK.

        FIND Remision WHERE Remision.Id-Remision = ttDocto.IdDoc NO-LOCK NO-ERROR.
        /**********************************/
        /* REGISTRA EL MOVIMIENTO EN CAJA */
        /**********************************/    
        CREATE MovCaja.
        ASSIGN 
            MovCaja.Id-Caja    = CtlCaja.Id-Caja
            MovCaja.Turno      = CtlCaja.Turno
            MovCaja.Id-Cliente = Remision.Id-Cliente
            MovCaja.FecReg     = TODAY
            MovCaja.Folio      = (IF CtlCaja.FolioFin + 1 >= 999999 THEN 0 ELSE CtlCaja.FolioFin + 1 )
            MovCaja.Referencia = Remision.Id-Remision
            MovCaja.TipoVenta  = Remision.TipoVenta
            MovCaja.TotVenta   = Remision.Tot
            MovCaja.Estatus    = 1  
            MovCaja.FolioAut   = 0  
            MovCaja.FecOper    = CtlCaja.FecOper
            MovCaja.FecDep     = CtlCaja.Fecoper
            MovCaja.Iniciales  = Empleado.Iniciales
            MovCaja.Id-Cajero  = Usuario.Id-Cajero
            CtlCaja.FolioFin   = (IF CtlCaja.FolioFin + 1 >= 999999 THEN 0 ELSE CtlCaja.FolioFin + 1 ).
           
        FIND b-Remision WHERE RECID(b-Remision) = RECID(Remision) EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE b-Remision THEN 
            ASSIGN b-Remision.Pagada = TRUE.
        FIND b-DepBanco WHERE RECID(b-DepBanco) = RECID(DepBanco) EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE b-DepBanco THEN 
        DO:
            ASSIGN 
                b-DepBanco.Id-Remision = Remision.Id-Remision
                b-DepBanco.Id-Acuse    = ''
                b-DepBanco.Id-User     = l-UsuApl
                b-DepBanco.FecAplica   = DATETIME(TODAY, MTIME)
                b-DepBanco.Conciliado  = TRUE.
        END.
        FOR EACH b-Pedido WHERE b-Pedido.Id-Factura = Remision.Id-Remision
            AND CAN-DO(Remision.Pedidos, b-Pedido.Id-Pedido) NO-LOCK:
            FIND Pedido WHERE RECID(Pedido) = RECID(b-Pedido) EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE Pedido THEN 
                ASSIGN Pedido.Pagado  = TRUE
                    Pedido.UsrPag  = l-UsuApl
                    Pedido.HorPag  = TIME
                    Pedido.FecPag  = TODAY 
                    Pedido.PagInfo = l-PagInfo.
            RELEASE Pedido.
        END.


        l-AntApl = 0.
        FOR EACH ttAnticipos WHERE ttAnticipos.IdDoc = ttDocto.IdDoc NO-LOCK:
            l-AntApl = ttAnticipos.ImpAplicado.
            FIND Anticipo WHERE Anticipo.Id-Anticipo = ttAnticipos.IdAnticipo   
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE Anticipo THEN 
            DO:
                l-Nda = 0.
                FIND LAST DetAnticipo WHERE DetAnticipo.Id-Anticipo = Anticipo.Id-Anticipo
                    NO-LOCK NO-ERROR.
                IF AVAILABLE DetAnticipo THEN
                    ASSIGN l-Nda = DetAnticipo.Sec.
                CREATE DetAnticipo.
                ASSIGN 
                    DetAnticipo.Id-Anticipo = Anticipo.Id-Anticipo
                    DetAnticipo.FecReg      = TODAY
                    DetAnticipo.Documento   = ttDocto.IdDoc
                    DetAnticipo.Id-MC       = 1
                    DetAnticipo.Importe     = ttAnticipos.ImpAplicado
                    DetAnticipo.Sec         = l-Nda + 1
                    DetAnticipo.UsuarioApl  = l-UsuApl.
                ASSIGN 
                    Anticipo.ImpAplicado = Anticipo.ImpAplicado + DetAnticipo.Importe
                    Anticipo.ImpContado  = Anticipo.ImpContado + DetAnticipo.Importe.
                  
                CREATE DetMovC.
                ASSIGN 
                    DetMovC.Id-Caja     = MovCaja.Id-Caja
                    DetMovC.Folio       = MovCaja.Folio
                    DetMovC.Id-tp       = 0                      
                    DetMovC.Id-Banco    = 0                      
                    DetMovC.Mov         = 'A'                     
                    DetMovC.Sec         = l-NPagos
                    DetMovC.MontoRec    = DetAnticipo.Importe   
                    DetMovC.MontoCambio = 0                       
                    DetMovC.TC          = 1
                    DetMovC.MontoPago   = DetAnticipo.Importe
                    DetMovC.Id-Dev      = INTEGER(Anticipo.Id-Anticipo)
                    DetMovC.PagInfo     = l-PagInfo
                    l-NPagos            = l-NPagos + 1.
            END.
            RELEASE Anticipo.   
        END.

        FOR EACH b-ttDocto WHERE b-ttDocto.IdCliente > 0 AND b-ttDocto.IdDoc > "" AND b-ttDocto.TDoc = "DEV" 
            NO-LOCK BY b-ttDocto.IdDoc:
            FIND Devolucion WHERE Devolucion.Id-Dev = INTEGER(b-ttDocto.IdDoc) 
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Devolucion THEN NEXT.
    
            ASSIGN 
                Devolucion.Documento  = Remision.Id-Remision
                Devolucion.FecApl     = TODAY
                Devolucion.UsuarioApl = l-UsuApl.
        
            CREATE DetMovC.   
            ASSIGN 
                DetMovC.Id-Caja     = MovCaja.Id-Caja
                DetMovC.Folio       = MovCaja.Folio
                DetMovC.Id-tp       = 0                      
                DetMovC.Id-Banco    = IF ttPago.IdBanco <> 0 THEN ttPago.IdBanco 
                                     ELSE IF ttPago.FormaPago = 57 THEN 25
                                     ELSE IF ttPago.FormaPago = 58 THEN 1
                                     ELSE 0 
                DetMovC.CtaCheq     = IF ttPago.Cuenta <> "0" THEN ttPago.Cuenta  ELSE "" 
                DetMovC.Cheque      = IF ttPago.FolioCheque <> "0" THEN ttPago.FolioCheque ELSE ""
                DetMovC.FecCheque   = IF ttPago.FechaCheque <> ? THEN ttPago.FechaCheque ELSE ?                     
                DetMovC.Mov         = 'D'                        
                DetMovC.Sec         = l-NPagos
                DetMovC.MontoRec    = b-ttDocto.ImpPago   
                DetMovC.MontoCambio = 0                       
                DetMovC.TC          = 1
                DetMovC.MontoPago   = b-ttDocto.ImpPago
                DetMovC.Id-Dev      = Devolucion.Id-Dev
                DetMovC.PagInfo     = l-PagInfo
                l-NPagos            = l-NPagos + 1.
            RELEASE Devolucion.
            
            
            RUN GeneraNCR(INPUT Remision.Id-Remision, INPUT l-UsuApl,OUTPUT v-status).
            IF NOT v-Status THEN 
            DO:
                /* Solo logueas o sigues con la siguiente */
                LOG-MANAGER:WRITE-MESSAGE("Saltando remisión EN DEV " + Remision.Id-Remision + " porque no aplica NCR").
            END.  
        END.

        l-TP = 0.
        l-ImpDeposito = 0.
        l-ImpPagado   = 0.
        FOR EACH ttPago WHERE ttPago.Importe > 0 BY ttPago.Importe DESCENDING:
            IF l-TP = 0 THEN
                l-TP = ttPago.FormaPago.
           
            ASSIGN 
                ip-formapago = "03".
            IF l-TP = 52 THEN
                ASSIGN ip-formapago = "28".
            ELSE IF l-TP = 57 OR l-TP = 58 THEN
                    ASSIGN ip-formapago = "03".
                ELSE IF l-TP = 60 THEN
                        ASSIGN ip-formapago = "01".
                    ELSE IF l-TP = 61 THEN
                            ASSIGN ip-formapago = "02".
                        ELSE IF l-TP = 62 THEN
                                ASSIGN ip-formapago = "04".
           
            CREATE DetMovC.
            ASSIGN 
                DetMovC.Id-Caja     = MovCaja.Id-Caja
                DetMovC.Folio       = MovCaja.Folio
                DetMovC.Id-tp       = l-TP                      
                DetMovC.Id-Banco    = IF ttPago.IdBanco <> 0 THEN ttPago.IdBanco 
                                     ELSE IF l-TP = 57 THEN 25
                                     ELSE IF l-TP = 58 THEN 1
                                     ELSE 0 
                DetMovC.CtaCheq     = IF ttPago.Cuenta <> "0" THEN ttPago.Cuenta  ELSE "" 
                DetMovC.Cheque      = IF ttPago.FolioCheque <> "0" THEN ttPago.FolioCheque ELSE ""
                DetMovC.FecCheque   = IF ttPago.FechaCheque <> ? THEN ttPago.FechaCheque ELSE ?                       
                DetMovC.Mov         = 'P'                     
                DetMovC.Sec         = l-NPagos
                DetMovC.MontoRec    = ttPago.Importe   
                DetMovC.MontoCambio = 0                       
                DetMovC.TC          = 1
                DetMovC.MontoPago   = ttPago.Importe
                DetMovC.PagInfo     = l-PagInfo
                l-NPagos            = l-NPagos + 1.
            l-ImpDeposito = l-ImpDeposito + ttPago.Importe.
            l-ImpPagado = l-ImpPagado + ttPago.Importe.    
        END.
    
        RELEASE b-DepBanco.       
        RELEASE b-Remision.   
        RELEASE CtlCaja.    
        p-Aplica = "APLICADO".  
        
        /* 
        IF v-TieneDesc = TRUE THEN 
        DO: 
            RUN GeneraNCR(INPUT Remision.Id-Remision, INPUT l-UsuApl,OUTPUT v-status).
        END.
        IF NOT v-Status THEN 
        DO:
            /* Solo logueas o sigues con la siguiente */
            LOG-MANAGER:WRITE-MESSAGE("Saltando remisión " + Remision.Id-Remision + " porque no aplica NCR").
        END.  
        
        */
    END.




    l-RestoAnt = l-depsantander - l-ImpPagado.   
    IF l-RestoAnt > 10 THEN 
    DO:
        FIND Folio WHERE Folio.Id-Doc = "ACUSE" AND Folio.Id-Alm = "NA" 
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Folio THEN 
        DO:
            CREATE Folio.
            ASSIGN 
                Folio.Id-Doc  = "ACUSE"
                Folio.Id-Alm  = "NA"
                Folio.Prefijo = "NA"   
                Folio.Folio   = 1.
        END.
    
        ASSIGN 
            l-folAntacuse = STRING(Folio.Folio,"9999999") + TRIM(Folio.PreFijo)
            l-folAntacuse = SUBSTRING(l-folAntacuse,LENGTH(l-folAntacuse) - 6,7)
            Folio.Folio   = Folio.Folio + 1.
        RELEASE Folio.
    
        FIND FIRST ttDocto WHERE ttDocto.IdCliente > 0 NO-LOCK NO-ERROR.
        CREATE Acuse.
        ASSIGN 
            Acuse.Id-Acuse      = l-FolAntAcuse
            Acuse.Id-Caja       = INTEGER(Usuario.Id-Caja)
            Acuse.Turno         = 1
            Acuse.Fecoper       = TODAY
            Acuse.FecReg        = TODAY
            Acuse.FecDep        = l-FecDep
            Acuse.UsuarioReg    = l-UsuApl
            Acuse.Tipo          = "A"
            Acuse.Estatus       = 4
            Acuse.Iniciales     = l-UsuApl
            Acuse.Id-Cajero     = Usuario.Id-Cajero
            Acuse.Id-Cliente    = IF AVAILABLE ttDocto THEN ttDocto.IdCliente ELSE 3
            Acuse.Id-Cobrador   = 25   // Numero Fijo (Validado en tabla Cobrador) 
            Acuse.AcuseCobrador = "0000000"
            Acuse.Comen[3]      = "".
        IF AVAILABLE DepBanco THEN 
        DO:
            ASSIGN 
                Acuse.Comen[1]  = "Acuse generado AUTOMATICAMENTE "
                Acuse.Comen[2]  = STRING(DAY(DepBanco.FecDep)) + '/' +
                                    STRING(meses[MONTH(DepBanco.FecDep)]) + '/' +
                                    STRING(YEAR(DepBanco.FecDep)) + ' ' +
                                    IF LENGTH(DepBanco.HoraDep) = 4 THEN SUBSTRING(STRING(DepBanco.HoraDep),1,2) + ':' + 
                                                                         SUBSTRING(STRING(DepBanco.HoraDep),3,2)
                                    ELSE '0' + SUBSTRING(STRING(DepBanco.HoraDep),1,1) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),2,2)
                Acuse.Comen[3]  = "Sobrante de pago FAC " + DepBanco.Id-Remision
                Acuse.Id-Origen = 'ST'.
        END.
        ELSE 
        DO:
            ASSIGN 
                Acuse.Comen[1]  = "Acuse generado MANUALMENTE "
                Acuse.Comen[2]  = ""
                Acuse.Id-Origen = 'SN'.    
        END.
        CREATE PagoAcuse.
        ASSIGN
            PagoAcuse.Id-Acuse    = l-FolAntAcuse
            PagoAcuse.Sec         = 1 
            PagoAcuse.Id-Tp       = l-TP 
            PagoAcuse.ImpRecibido = l-RestoAnt
            PagoAcuse.Importe     = l-RestoAnt
            PagoAcuse.Id-Banco    = IF l-TP = 57 THEN 25
                                ELSE IF l-TP = 58 THEN 1 
                                ELSE 25
            PagoAcuse.Cuenta      = ""
            PagoAcuse.Cheque      = ""
            PagoAcuse.FecCheque   = ?    
            PagoAcuse.CPFormaPago = ip-formapago       // "01=Efectivo,02=Cheque,03=Transferencia"
            PagoAcuse.Id-Moneda   = 1
            PagoAcuse.TC          = 1     
            PagoAcuse.TipoCambio  = 1.

        FIND FIRST ttPago WHERE ttPago.Rec <> ? NO-LOCK NO-ERROR.
        IF AVAILABLE ttPago AND AVAILABLE DepBanco THEN 
        DO:
            ASSIGN 
                DepBanco.Id-AcuseAnt = l-folAntAcuse.
        END.    
        
        FIND Folio WHERE Folio.Id-Doc = "ANT" AND Folio.Id-Alm = ""
            EXCLUSIVE-LOCK NO-ERROR.
        CREATE Anticipo.
        ASSIGN
            Anticipo.Id-Anticipo = STRING(Folio.Folio,'9999999')
            Anticipo.Id-Cliente  = Acuse.Id-Cliente
            Anticipo.Id-Acuse    = Acuse.Id-Acuse
            Anticipo.FecReg      = TODAY
            Anticipo.ImpAnticipo = l-RestoAnt
            Anticipo.ImpAplicado = 0
            Anticipo.ImpDevuelto = 0
            Anticipo.ImpContado  = 0
            Anticipo.Canc        = FALSE   
            Anticipo.Concepto    = Acuse.Comen
            Folio.Folio          = Folio.Folio + 1.
        RELEASE Folio.
    END.       
 
    RETURN.    

END PROCEDURE.


PROCEDURE GeneraNCR:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
      Empresa : Consultoria en Informatica Ejecutiva S.A de C.V
      Sistema : ADOSA
      Programa: ausa0028.p
      Funcion : Crea NCR de una factura de Contado
      Llamador: ausa0020.p
      Autor   : IOC
      Fecha   : 26/06/1997
    */

    DEFINE INPUT PARAMETER l-Remision      LIKE Remision.Id-Remision   NO-UNDO.
    DEFINE INPUT PARAMETER l-UsuApl        AS CHARACTER                NO-UNDO.
    DEFINE OUTPUT PARAMETER l-Status       AS LOGICAL   NO-UNDO. /* TRUE=Generado, FALSE=No aplica */
    DEFINE VARIABLE l-NCR    LIKE NCR.Id-NCR NO-UNDO INITIAL "100N".
    DEFINE VARIABLE l-TotNCR LIKE NCR.Tot NO-UNDO.
    DEFINE VARIABLE l-TotD   LIKE NCR.Tot NO-UNDO.
    DEFINE VARIABLE l-TotR   LIKE NCR.Tot NO-UNDO.
    DEFINE VARIABLE l-TotF   LIKE NCR.Tot NO-UNDO.
    DEFINE VARIABLE l-secNCR AS INTEGER   NO-UNDO.
    DEFINE VARIABLE l-titulo AS CHARACTER FORMAT "x(50)" NO-UNDO.
    DEFINE VARIABLE l-nncr   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-monto  AS DECIMAL   DECIMALS 2 .
    DEFINE VARIABLE l-sub    AS DECIMAL   DECIMALS 2.
    DEFINE VARIABLE l-recmov AS RECID     NO-UNDO.

     ASSIGN l-Status = TRUE. 
    /* --- INICIO LOG --- */
    LOG-MANAGER:WRITE-MESSAGE("=== INICIO GeneraNCR ===").
    LOG-MANAGER:WRITE-MESSAGE("Parámetros: Remision=" + STRING(l-Remision) + " Usuario=" + l-UsuApl).


    FIND Usuario WHERE Usuario.Id-User = l-UsuApl NO-LOCK NO-ERROR.
    IF AVAILABLE Usuario THEN g-Origen = Usuario.id-ubicacion.

    LOG-MANAGER:WRITE-MESSAGE("Origen detectado: " + g-Origen).

    FIND Cajero OF Usuario NO-LOCK NO-ERROR.
    FIND FIRST Empleado WHERE Empleado.Iniciales = Cajero.Iniciales
        AND Empleado.Activo NO-LOCK NO-ERROR.
                      
    FIND Remision WHERE Remision.Id-Remision = l-Remision NO-LOCK NO-ERROR.
    FIND FIRST MovCaja WHERE
        MovCaja.Referencia = Remision.Id-Remision NO-LOCK NO-ERROR.
    FOR EACH DetMovC WHERE
        DetMovC.Id-Caja = MovCaja.Id-Caja AND
        DetMovC.Folio   = MovCaja.Folio NO-LOCK:
        IF CAPS(DetMovC.Mov) = "P" THEN NEXT.
        IF CAPS(DetMovC.Mov) = "A" THEN NEXT.
        IF CAPS(DetMovC.Mov) = "F" THEN ASSIGN l-TotF = l-TotF + DetMovC.MontoPago.
        IF CAPS(DetMovC.Mov) = "R" THEN ASSIGN l-TotR = l-TotR + DetMovC.MontoPago.
        ACCUMULATE DetMovC.MontoPago (TOTAL).
    END.

    ASSIGN 
        l-TotNCR = ACCUM TOTAL DetMovC.MontoPago
        l-recmov = RECID(MovCaja).
       
    LOG-MANAGER:WRITE-MESSAGE("Totales: l-TotNCR=" + STRING(l-TotNCR) + 
        " l-TotF=" + STRING(l-TotF) +
        " l-TotR=" + STRING(l-TotR)).
        
    IF l-TotNCR = 0 THEN 
    DO:
        LOG-MANAGER:WRITE-MESSAGE("No se generó NCR para la remisión " + l-Remision + " (Total=0)").
        ASSIGN 
            l-Status = FALSE.
        RETURN. /* ← ahora ya no rompe al llamador, solo sale del internal procedure */
    END.
                          
    DO TRANSACTION ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE:
        CASE g-Origen:
            WHEN "11" THEN
                FIND Folio WHERE Folio.Id-Doc = "NCR5"
                    AND Folio.Id-Alm = "" EXCLUSIVE-LOCK NO-ERROR.
            WHEN "12" THEN
                FIND Folio WHERE Folio.Id-Doc = "NCR6"
                    AND Folio.Id-Alm = g-Origen EXCLUSIVE-LOCK NO-ERROR.
            OTHERWISE
            FIND Folio WHERE Folio.Id-Doc = "NCR6"
                AND Folio.Id-Alm = "" EXCLUSIVE-LOCK NO-ERROR.
        END CASE.

        FOR EACH DistIVA WHERE DistIVA.Id-Factura = Remision.Id-Remision AND
            DistIVA.TipoVenta  = Remision.TipoVenta NO-LOCK :
            ASSIGN 
                l-monto = (l-totNcr * (DistIVA.Participacion / 100)) /
                             (1 + (DistIVA.PorcIVA / 100))
                l-sub   = l-sub + l-monto.
        END.

        CREATE NCR.
        ASSIGN 
            NCR.Id-NCR      = IF g-Origen <> "11"
                            THEN STRING(Folio.Folio,'999999') + Folio.Prefijo
                            ELSE Folio.Prefijo + STRING(Folio.Folio,'999999')
            l-nncr          = NCR.Id-NCR
            NCR.Subtotal    = l-sub
            Ncr.Iva         = l-totNcr - l-sub
            NCR.Tot         = l-TotNCR
            NCR.Id-Cliente  = Remision.Id-Cliente
            NCR.RazonSocial = Remision.RazonSocial
            NCR.NomEmpresa  = Remision.NomEmpresa
            NCR.RFC         = Remision.RFC
            NCR.Id-RFiscal  = Remision.Id-RFiscal
            NCR.CalleNo     = Remision.CalleNo
            NCR.Colonia     = Remision.Colonia
            NCR.Id-Ciudad   = Remision.Id-Ciudad
            NCR.BuzonFiscal = Remision.BuzonFiscal
            NCR.CP          = Remision.CP
            NCR.FecCorte    = TODAY
            Ncr.FecReg      = TODAY 
            /*IF WEEKDAY(TODAY) = 7 THEN
              TODAY + 2 ELSE TODAY */
            NCR.HorReg      = TIME
            NCR.Tipo        = 2
            Folio.Folio     = Folio.Folio + 1
            l-secNCR        = 1.
    
        RELEASE folio.

        FIND MovCaja WHERE RECID(MovCaja) = l-recmov EXCLUSIVE-LOCK NO-ERROR.
        IF MovCaja.TipoVenta <> 1 THEN  
            ASSIGN MovCaja.Id-ncr = Ncr.Id-Ncr.

        FOR EACH DetMovC WHERE
            DetMovC.Id-Caja = MovCaja.Id-Caja AND
            DetMovC.Folio   = MovCaja.Folio   NO-LOCK:
            IF CAPS(DetMovC.Mov) = "P" THEN NEXT.
            IF CAPS(DetMovC.Mov) = "A" THEN NEXT.
            CREATE DetNCR.
            IF CAPS(DetMovC.Mov) = 'D' THEN
                ASSIGN DetNCR.Referencia = STRING(DetMovC.Id-Dev,"9999999").
            ASSIGN 
                DetNCR.Documento = Remision.Id-Remision
                DetNCR.Id-MC     = 1
                DetNCR.Id-NCR    = NCR.Id-NCR
                DetNCR.FecReg    = Remision.FecReg
                DetNCR.TipoVenta = Remision.TipoVenta
                DetNCR.Sec       = l-secNCR
                l-secNCR         = l-secNCR + 1
                DetNCR.Importe   = (IF CAPS(DetMovC.Mov) = 'F' THEN l-TotF
                 ELSE (IF CAPS(DetMovC.Mov) = 'R' THEN l-totR
                       ELSE (IF CAPS(DetMovC.Mov) = 'D' THEN DetMovC.MontoPago
                             ELSE 0)))
                DetNCR.TipoCR    = (IF CAPS(DetMovC.Mov) = 'F' THEN 68
                 ELSE (IF CAPS(DetMovC.Mov) = 'R' THEN 63
                       ELSE (IF CAPS(DetMovC.Mov) = 'D' THEN 65 ELSE 0))).
        END.

        {ausa0004.i
     &Cliente      = Remision.Id-Cliente
     &Referencia   = l-nncr
     &TipoVenta    = 9
     &TotVenta     = l-TotNCR
     &Estatus      = 1
     &FolioAut     = 0
     &EmpIniciales = Empleado.Iniciales
     &Cajero       = Usuario.Id-Cajero  }

    END. /* END de la transaccion */

    MESSAGE 'Se genero Nota Credito No.:' + l-nncr +
        '. Presione cualquier tecla para continuar...'.
    PAUSE 1.

    /* Genera e imprime la nota de credito electronica */
    RUN /usr2/adosa/procs/ausc0070.p(INPUT l-nncr).  /* generacion */ 

    /* IMPRESION COMENTARON QUE NO APLICA 
    RUN ausc0071.p(INPUT l-nncr).  
    */

    /* Antes de finalizar */
    LOG-MANAGER:WRITE-MESSAGE("NCR generado: " + l-nncr).
    LOG-MANAGER:WRITE-MESSAGE("=== FIN GeneraNCR ==="). 
 
  
END PROCEDURE.

