@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : custom5.p
    Purpose     : 

    Syntax      : /RepRelacionPago

    Description : REPORTE RELACION DE PAGOS 
    Author(s)   : sis10
    Created     : Wed Mar 05 23:37:48 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */



DEFINE VARIABLE l-Archivo AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Fecini AS DATE NO-UNDO LABEL 'Fec Ini' FORMAT '99/99/9999' INITIAL TODAY.
DEFINE VARIABLE l-Fecfin AS DATE NO-UNDO LABEL 'Fec Fin' FORMAT '99/99/9999' INITIAL TODAY.
DEFINE VARIABLE l-Estatus AS CHARACTER NO-UNDO FORMAT 'x(10)'.
DEFINE VARIABLE l-Importe LIKE Pagoacuse.Importe NO-UNDO.
DEFINE VARIABLE l-Descto  LIKE Docacuse.ImpDescPP NO-UNDO. /* arl */
DEFINE VARIABLE l-SumaDesctoAcuse AS DECIMAL FORMAT '->>>,>>>,>>9.99' NO-UNDO.
DEFINE VARIABLE l-SumaMontoAcuse AS DECIMAL FORMAT '->>>,>>>,>>9.99' NO-UNDO.
DEFINE VARIABLE l-SumaMontoFac AS DECIMAL FORMAT '->>>,>>>,>>9.99' NO-UNDO.
DEFINE VARIABLE l-SumaNoDep AS DECIMAL FORMAT '->>>,>>>,>>9.99' NO-UNDO.

DEF BUFFER b-MovCliente FOR MovCliente.
DEF BUFFER b-HistMovCte FOR HistMovCte.

DEF TEMP-TABLE wCheque
    FIELD IdCliente LIKE Acuse.Id-Cliente
    FIELD IdAcuse LIKE Acuse.Id-Acuse
    FIELD FecOper LIKE Acuse.FecOper
    FIELD FecDep LIKE Acuse.FecDep
    FIELD Importe LIKE PagoAcuse.Importe
    FIELD Descto  AS DECIMAL  /* arl */
    FIELD Estatus AS CHAR
    FIELD Observa AS CHAR
    FIELD IdCPago LIKE CPago.Id-CPago
    FIELD FecCPago LIKE CPago.FecReg
    INDEX idx-fec FecDep
    INDEX idx-acu IdAcuse.

DEF TEMP-TABLE wFactura
    FIELD IdCliente LIKE Factura.Id-Cliente
    FIELD IdFactura LIKE Factura.Id-Factura
    FIELD FecReg    LIKE Factura.FecReg
    FIELD Monto     LIKE Factura.Tot
    INDEX idx-fac FecReg DESC.
    
DEF TEMP-TABLE wResumen
    FIELD IdCliente         LIKE Factura.Id-Cliente
    FIELD RazonSocial       LIKE Cliente.RazonSocial
    FIELD TotalChequesDep   LIKE Factura.Tot
    FIELD TotalDesc         LIKE Factura.Tot
    FIELD TotalFact         LIKE Factura.Tot
    FIELD EfectoNetoCart    LIKE Factura.Tot
    FIELD TotalChequesNoDep LIKE Factura.Tot.
    
DEFINE DATASET dsRelacion FOR 
    wCheque,
    wFactura,
    wResumen
    DATA-RELATION RelacionPago FOR wCheque, wFactura
    RELATION-FIELDS (IdCliente, IdCliente)
    DATA-RELATION RelacionPago2 FOR wCheque, wResumen
    RELATION-FIELDS (IdCliente, IdCliente). 

    
    

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ReporteRelPago:
/*
    Empresa:    ADOSA
    Programa:   cxcc0460.p
    Funcion:    Consulta de pagos y facturas autorizadass a clientes
    Autor:      Alex
    Fecha:      19 de Enero del 2007
*/
    DEF INPUT PARAMETER l-FecIni AS DATE FORMAT "99/99/9999"  NO-UNDO.
    DEF INPUT PARAMETER l-FecFin    AS DATE FORMAT "99/99/9999"  NO-UNDO.
    DEF INPUT PARAMETER l-cliente  AS INT  NO-UNDO.
    DEFINE OUTPUT PARAMETER DATASET FOR dsRelacion.

    
    
    IF l-FecIni = ? THEN l-FecIni = TODAY - 1.
    IF l-FecFin    = ? THEN l-FecFin    = TODAY.
    IF l-cliente = ? THEN l-cliente = 0.
    
    ASSIGN l-SumaMontoAcuse = 0 l-SumaNoDep = 0. 
    ASSIGN l-SumaDesctoAcuse = 0.   
     
    EMPTY TEMP-TABLE wCheque.
    EMPTY TEMP-TABLE wFactura.
    EMPTY TEMP-TABLE wResumen.
    
    FOR EACH Acuse WHERE Acuse.Id-cliente = l-cliente
                     AND Acuse.FecDep >= l-FecIni
                     AND Acuse.FecDep <= l-FecFin
                     AND Acuse.Estatus = 4 NO-LOCK:
        ASSIGN l-Importe = 0.
        FOR EACH Pagoacuse OF acuse NO-LOCK:
            ASSIGN l-importe = l-importe + pagoacuse.importe.
        END.
        l-Descto = 0.

        IF l-Importe > 0 THEN DO:
            ASSIGN l-SumaMontoAcuse = l-SumaMontoAcuse + l-Importe.
            ASSIGN l-SumaDesctoAcuse = l-SumaDesctoAcuse + l-Descto.  /* arl */
            ASSIGN l-Estatus = "DEPOSITADO".
            CREATE wCheque.
            ASSIGN wCheque.IdCliente = Acuse.Id-Cliente
                   wCheque.IdAcuse  = Acuse.Id-Acuse
                   wCheque.FecOper   = Acuse.FecOper
                   wCheque.FecDep    = Acuse.FecDep
                   wCheque.Importe   = l-Importe
                   wCheque.Descto    = l-Descto  /* arl */
                   wCheque.Estatus   = l-Estatus.
            FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
                               AND CPago.FecCanc = ?
                               NO-LOCK NO-ERROR.
            IF AVAILABLE CPago THEN
               ASSIGN wCheque.IdCPago = CPago.Id-CPago
                      wCheque.FecCPago = CPago.FecReg.
        END.
    END.
    FOR EACH Acuse WHERE Acuse.Id-cliente = l-cliente
                     AND Acuse.Estatus < 3 NO-LOCK:
        ASSIGN l-Importe = 0.
        FOR EACH Pagoacuse OF acuse NO-LOCK:
            ASSIGN l-importe = l-importe + pagoacuse.importe.
        END.

        ASSIGN l-Descto = 0.
 
        IF l-Importe > 0 THEN DO:
            ASSIGN l-SumaNoDep = l-SumaNoDep + l-Importe.
    
            IF Acuse.Estatus = 1 THEN
                ASSIGN l-Estatus = "REGISTRADO".
            ELSE
                IF Acuse.Estatus = 2 THEN
                    ASSIGN l-Estatus = "AFECTADO".
    
            CREATE wCheque.
            ASSIGN wCheque.IdCliente = Acuse.Id-Cliente
                   wCheque.IdAcuse  = Acuse.Id-Acuse
                   wCheque.FecOper   = Acuse.FecOper
                   wCheque.FecDep    = Acuse.FecDep
                   wCheque.Descto    = l-Descto    /* arl */
                   wCheque.Importe   = l-Importe
                   wCheque.Estatus   = l-Estatus.
        END.
    END.
    
  /* Cargar en pagos las aplicaciones de anticipos registrados con cliente 3 */
    FOR EACH MovCliente WHERE MovCliente.Id-cliente = 3
                         AND MovCliente.Id-Mc = 90
                         AND MovCliente.FecReg >= l-FecIni
                         AND MovCliente.FecReg <= l-FecFin
                         NO-LOCK:
      FIND FIRST b-MovCliente WHERE b-MovCliente.RefSaldo = MovCliente.RefSaldo
                                  AND b-MovCliente.Id-Mc <= 3
                                  AND b-MovCliente.Id-Cliente = l-cliente
                                  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-MovCliente THEN NEXT.
        
        l-Importe = MovCliente.Importe * -1.
        l-Descto  = 0.  /* arl */
        FIND wCheque WHERE wCheque.IdAcuse = MovCliente.Documento
               EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE wCheque THEN DO:
            FIND Acuse WHERE Acuse.Id-Acuse = MovCliente.Documento 
                 NO-LOCK NO-ERROR.
            IF AVAILABLE Acuse THEN DO:
                ASSIGN l-Estatus = "DEPOSITADO".
                CREATE wCheque.
                ASSIGN wCheque.IdCliente = Acuse.Id-Cliente
                       wCheque.IdAcuse  = Acuse.Id-Acuse
                       wCheque.FecOper   = Acuse.FecOper
                       wCheque.FecDep    = Acuse.FecDep
                       wCheque.Importe   = 0
                       wCheque.Descto    = 0 /* ARL */ 
                       wCheque.Estatus   = l-Estatus
                       wCheque.Observa   = "ANT. CTE 3".
            END.
        END.
        IF AVAILABLE wCheque THEN DO:
           ASSIGN wCheque.Importe = wCheque.Importe + l-Importe.
           ASSIGN wCheque.Descto  = wCheque.Descto  + l-Descto.  
           ASSIGN l-SumaMontoAcuse = l-SumaMontoAcuse + l-Importe.
           ASSIGN l-SumaDesctoAcuse = l-SumaDesctoAcuse + l-Descto. 
        END.
        RELEASE wCheque.
    END.
    FOR EACH HistMovCte WHERE HistMovCte.Id-cliente = 3
                         AND HistMovCte.Id-Mc = 90
                         AND HistMovCte.FecReg >= l-FecIni
                         AND HistMovCte.FecReg <= l-FecFin
                         NO-LOCK:
      FIND FIRST b-HistMovCte WHERE b-HistMovCte.RefSaldo = HistMovCte.RefSaldo
                                  AND b-HistMovCte.Id-Mc <= 3
                                  AND b-HistMovCte.Id-Cliente = l-cliente
                                  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-HistMovCte THEN NEXT.
        
        l-Importe = HistMovCte.Importe * -1.
        l-Descto  = 0. /* arl */
        FIND wCheque WHERE wCheque.IdAcuse = HistMovCte.Documento
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE wCheque THEN DO:
            FIND Acuse WHERE Acuse.Id-Acuse = HistMovCte.Documento 
            NO-LOCK NO-ERROR.
            IF AVAILABLE Acuse THEN DO:
                ASSIGN l-Estatus = "DEPOSITADO".
                CREATE wCheque.
                ASSIGN wCheque.IdCliente = Acuse.Id-Cliente
                       wCheque.IdAcuse  = Acuse.Id-Acuse
                       wCheque.FecOper   = Acuse.FecOper
                       wCheque.FecDep    = Acuse.FecDep
                       wCheque.Importe   = 0
                       wCheque.Estatus   = l-Estatus
                       wCheque.Descto    = 0   
                       wCheque.Observa   = "ANT. CTE 3".
                FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
                                   AND CPago.FecCanc = ?
                                   NO-LOCK NO-ERROR.
                IF AVAILABLE CPago THEN
                   ASSIGN wCheque.IdCPago = CPago.Id-CPago
                          wCheque.FecCPago = CPago.FecReg.
            END.
        END.
        IF AVAILABLE wCheque THEN DO:
           ASSIGN wCheque.Importe = wCheque.Importe + l-Importe.
           ASSIGN wCheque.Descto  = wCheque.Descto  + l-Descto.  /* arl */
           ASSIGN l-SumaMontoAcuse = l-SumaMontoAcuse + l-Importe.
           ASSIGN l-SumaDesctoAcuse = l-SumaDesctoAcuse + l-Descto.  /* arl */
        END.
        RELEASE wCheque.
    END.
  /* Cargar descuentos aplicados en el periodo */
    l-SumaDesctoAcuse  = 0.
    FOR EACH MovCliente WHERE MovCliente.Id-cliente = l-cliente
                         AND MovCliente.Id-Mc > 3
                         AND MovCliente.FecReg >= l-FecIni
                         AND MovCliente.FecReg <= l-FecFin
                         NO-LOCK:
        
        FIND TipoPago WHERE TipoPago.Id-TP = MovCliente.Id-Mc NO-LOCK NO-ERROR.
        IF AVAILABLE TipoPago OR MovCliente.Id-MC = 67
                              OR MovCliente.Id-MC = 90 THEN NEXT.
        l-SumaDesctoAcuse = l-SumaDesctoAcuse + (MovCliente.Importe * -1).
    END.
    FOR EACH HistMovCte WHERE HistMovCte.Id-cliente = l-cliente
                          AND HistMovCte.Id-Mc > 3
                          AND HistMovCte.FecReg >= l-FecIni
                          AND HistMovCte.FecReg <= l-FecFin
                          NO-LOCK:
        
        FIND TipoPago WHERE TipoPago.Id-TP = HistMovCte.Id-Mc NO-LOCK NO-ERROR.
        IF AVAILABLE TipoPago OR HistMovCte.Id-MC = 67
                              OR HIstMovCte.Id-MC = 90  THEN NEXT.
        l-SumaDesctoAcuse = l-SumaDesctoAcuse + (HistMovCte.Importe * -1).
    END.
    
    FOR EACH CheDev WHERE CheDev.Id-Cliente = l-Cliente
                      AND CheDev.FecCargo >= l-FecIni
                      AND CheDev.FecCargo <= l-FecFin
                      AND CheDev.FecCanc = ?
        NO-LOCK BY CheDev.FecCargo:
        ASSIGN l-SumaMontoAcuse = l-SumaMontoAcuse - CheDev.ImpCheque.
        CREATE wCheque.
        ASSIGN wCheque.IdCliente = CheDev.Id-Cliente
               wCheque.IdAcuse = CheDev.Id-CheDev
               wCheque.FecOper = ?
               wCheque.FecDep = CheDev.FecCargo
               wCheque.Importe = CheDev.ImpCheque * -1
               wCheque.Estatus = "DEVUELTO".
    END.
    
    ASSIGN l-SumaMontoFac = 0.

    FOR EACH factura WHERE factura.id-cliente = l-Cliente
                       AND factura.fecreg >= l-fecini
                       AND factura.fecreg <= l-fecfin
                       AND factura.feccanc = ? NO-LOCK:
        
      CREATE wFactura.
      ASSIGN wFactura.IdCliente = Factura.Id-Cliente
             wFactura.FecReg    = Factura.FecReg
             wFactura.IdFactura = Factura.Id-Factura
             wFactura.Monto     = Factura.Tot .

        ASSIGN l-SumaMontoFac = l-SumaMontoFac + Factura.Tot.
    END.
    
    IF l-SumaMontoAcuse <> 0 OR l-SumaNoDep <> 0 OR l-SumaDesctoAcuse <> 0 OR l-SumaMontoFac <> 0
    THEN DO:
             FIND FIRST Cliente WHERE Cliente.Id-Cliente = l-cliente NO-LOCK NO-ERROR.
             CREATE wResumen.
             ASSIGN wResumen.IdCliente         = l-cliente
                    WResumen.RazonSocial       = IF AVAILABLE Cliente AND Cliente.RazonSocial <> "" THEN Cliente.RazonSocial ELSE ""
                    wResumen.TotalChequesDep   = l-SumaMontoAcuse
                    wResumen.TotalDesc         = l-SumaDesctoAcuse
                    wResumen.TotalFact         = l-SumaMontoFac
                    wResumen.EfectoNetoCart    = l-SumaMontoFac - l-SumaMontoAcuse - l-SumaDesctoAcuse
                    wResumen.TotalChequesNoDep = l-SumaNoDep.

    END.
    
    
    

END PROCEDURE.   

     
