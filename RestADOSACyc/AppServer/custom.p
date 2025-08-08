@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : custom.p
    Purpose     : 

    Syntax      :

    Description : Reporte Comision

    Author(s)   : sis10
    Created     : Wed Mar 05 01:25:06 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEF BUFFER bf-MovCliente FOR MovCliente.

DEF TEMP-TABLE tt-totagente NO-UNDO
    FIELD id-vendedor           LIKE vendedor.id-vend
    FIELD nombre                LIKE usuario.nom
    FIELD cobrado               AS   DECIMAL
    FIELD importe               AS   DECIMAL
    FIELD comision1             AS   DECIMAL
    FIELD pagotardio            AS   DECIMAL
    FIELD comision2             AS   DECIMAL
    FIELD CargoAnt              AS   DECIMAL
    FIELD comision3             AS   DECIMAL
    FIELD porc-com              LIKE vendedor.porc-com
    INDEX ip-1 AS PRIMARY UNIQUE id-vendedor.

DEFINE TEMP-TABLE ttFacturas NO-UNDO
    FIELD IdVendedor LIKE adosa.Vendedor.Id-Vendedor
    FIELD NomAgente   AS CHAR
    FIELD idFactura  LIKE adosa.Remision.id-Remision
    FIELD fecreg      AS   DATE
    FIELD FecPago     AS   DATE
    FIELD FecVence    AS   DATE
    FIELD DiasPago    AS   INTEGER
    FIELD DiasVenc    AS   INTEGER
    FIELD IdCliente  LIKE adosa.Cliente.Id-Cliente
    FIELD RazSoc      AS   CHARACTER /*FORMAT 'X(5)'*/
    FIELD Neto        AS   DECIMAL FORMAT '->>>,>>>,>>9.99'
    FIELD Margen      AS   DECIMAL FORMAT '->>9.99'
    FIELD ComBru      AS   DECIMAL
    FIELD ComNet      AS   DECIMAL
    FIELD subtot      AS   DECIMAL
    FIELD Costo       AS   DECIMAL FORMAT '->>>,>>>,>>9.99'
    FIELD Importe     AS   DECIMAL FORMAT '->>>,>>>,>>9.99'
    FIELD PagAnt      AS   DECIMAL FORMAT '->>>,>>>,>>9.99'
    FIELD DifOri      AS   DECIMAL FORMAT '->,>>>,>>9.99'
    FIELD Dif         AS   DECIMAL FORMAT '->,>>>,>>9.99'
    FIELD impFlete    AS   DECIMAL FORMAT '>>>,>>9'
    FIELD Descto      AS   DECIMAL FORMAT '->,>>>,>>9.99'
    INDEX Idx-Fac AS UNIQUE idFactura
    INDEX Idx-Def AS PRIMARY IdVendedor RazSoc idFactura.

DEF VAR l-Importe   LIKE DocAcus.ImpPago                            NO-UNDO.
DEF VAR l-DVen      AS   INT                                        NO-UNDO FORMAT 'zzz9'.
DEF VAR l-DPla      AS   INT                                        NO-UNDO FORMAT 'zzz9'.
DEF VAR l-i         AS   INT                                        NO-UNDO.
DEF VAR l-Neto      AS   DEC                                        NO-UNDO.
DEF VAR l-Comision  AS   DEC FORMAT '->>>,>>9.99'                   NO-UNDO.
DEF VAR l-porc-com  LIKE vendedor.porc-com                          NO-UNDO.
DEF VAR l-Ped       LIKE Factura.Pedidos                            NO-UNDO.
DEF VAR l-Acum      AS   DECIMAL FORMAT '-zz,zz9.99'                NO-UNDO.
DEF VAR l-Reporte   AS   CHAR                                       NO-UNDO.
DEF VAR l-Aster     AS   CHAR FORMAT 'X'                            NO-UNDO.
DEF VAR l-ContP     AS   INT FORMAT 'z9'                            NO-UNDO.
DEF VAR l-DP        AS   INT  FORMAT 'zz9'                          NO-UNDO.
DEF VAR l-FecPag    AS   DATE                                       NO-UNDO.
DEF VAR v-difer     AS   DECIMAL                                    NO-UNDO.
DEF VAR l-Pago      AS DECIMAL NO-UNDO.
DEF VAR l-AntMas90  AS DECIMAL NO-UNDO.
DEF VAR l-CargoAnt  AS DECIMAL NO-UNDO.
DEF VAR l-Saldo  AS DECIMAL NO-UNDO.
DEF VAR l-SaldoCte  AS DECIMAL NO-UNDO.
DEF VAR l-PagAnt    AS DECIMAL NO-UNDO.
DEF VAR l-PagAntSin AS DECIMAL NO-UNDO.

DEFINE VARIABLE l-SubTot1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-SubTot2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Costo3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Descto1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Descto2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-FactorPP AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-MovImp1 AS DECIMAL NO-UNDO.







/* **********************  Internal Procedures  *********************** */


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE RepComision:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER v-fecini  AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER v-fecfin  AS DATE NO-UNDO.
DEFINE INPUT PARAMETER v-agente  LIKE Vendedor.Id-Vendedor NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttFacturas.

IF v-fecini = ? THEN v-fecini = TODAY - 1.
IF v-fecfin = ? THEN v-fecfin = TODAY. 
IF v-agente = ? THEN v-agente = "".

RUN SelFacturas(INPUT v-fecini, INPUT v-fecfin, INPUT v-agente).


FOR EACH ttFacturas NO-LOCK
      BREAK BY ttFacturas.IdVendedor
            BY ttFacturas.RazSoc
            BY ttFacturas.FecPag:

     l-porc-com = 15.
     IF ttFacturas.IdCliente = 35947 THEN /* Exclusivo para IMAPCOLOR por el costo del factoraje */
        l-porc-com = 11.
        
     ASSIGN l-Importe = ttFacturas.DifOri * l-porc-com / 100
            l-DVen = MAXIMUM(ttFacturas.FecPag - ttFacturas.FecVence,0)
            l-DPla = MAXIMUM(ttFacturas.FecVence - ttFacturas.FecReg,0)
            l-Acum      = 0.
     IF l-dVen > 30 AND l-dVen <= 60 THEN
       ASSIGN l-Neto = l-porc-com * 0.5.
     ELSE IF l-DVen < 31 THEN ASSIGN l-Neto = l-porc-com.
     ELSE IF l-DVen > 60 THEN ASSIGN l-Neto = 0
                                     l-Acum = ttFacturas.Neto * 
                                              0.013043 * -1.
     ASSIGN 
         l-Comision = ttFacturas.Dif * l-Neto / 100.

    IF l-Importe < 0 THEN l-Importe = 0.
    IF l-Comision < 0 THEN l-Comision = 0.
    
    ASSIGN
          ttFacturas.ComBru = l-Importe
          ttFacturas.ComNet = l-Comision.
    

        FIND Vendedor WHERE Vendedor.Id-Vendedor = ttFacturas.IdVendedor
             NO-LOCK NO-ERROR.
        FIND Empleado WHERE Empleado.Iniciales = Vendedor.Iniciales
             NO-LOCK NO-ERROR.
             
        ASSIGN
            ttFacturas.NomAgente  = empleado.nombre WHEN AVAILABLE empleado.
    

     
END.    



END PROCEDURE.

PROCEDURE SelFacturas.
    DEFINE INPUT PARAMETER v-fecini  AS DATE NO-UNDO. 
    DEFINE INPUT PARAMETER v-fecfin  AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER v-agente  AS CHAR NO-UNDO.
    
    FOR EACH Vendedor WHERE Vendedor.Activo AND Vendedor.TipoVen = 3
                        AND NOT CAN-DO("0100",Vendedor.Id-Vendedor)
                        AND (v-agente = "" OR Vendedor.Id-Vendedor = v-agente)  
        NO-LOCK BY Vendedor.Id-Vendedor:
    
        FOR EACH MovCaja WHERE MovCaja.FecOper >= v-fecini
                           AND MovCaja.FecOper <= v-fecfin
                           AND MovCaja.TipoVenta = 2 NO-LOCK,
            EACH Remision WHERE Remision.Id-Remision = MovCaja.Refer
                            AND Remision.TipoVenta = MovCaja.TipoVenta
                            AND Remision.Id-Vendedor = Vendedor.Id-Vendedor
                            AND Remision.FecCancel = ?
                            AND Remision.Pagada
                            NO-LOCK:
        
            ASSIGN l-SubTot1 = 0
                   l-SubTot2 = 0
                   l-Costo3  = 0
                   l-Descto1 = 0
                   l-Descto2 = 0.
        
            FOR EACH DetRemis OF Remision WHERE DetRemis.Tipo <= 2 NO-LOCK:
                IF DetRemis.Tipo = 1 OR
                   (DetRemis.Tipo = 2 AND DetRemis.Importe < 0) THEN DO:                                
                    ASSIGN
                        l-Costo3 = l-Costo3 + (DetRemis.Costo * (DetRemis.Cant - DetRemis.CantDev)).
                    ASSIGN
                        l-SubTot2 = l-SubTot2 + (DetRemis.PrecUnit * ((100 - DetRemis.PorcDesc) / 100) * (DetRemis.Cant)).
                    ASSIGN
                        l-SubTot1 = l-SubTot1 + (DetRemis.PrecUnit * ((100 - DetRemis.PorcDesc) / 100) * (DetRemis.Cant - DetRemis.CantDev)).
                END.
            END.
            
            FOR EACH DetMovC WHERE DetMovC.Id-Caja = MovCaja.Id-Caja
                               AND DetMovC.Folio = MovCaja.Folio
                               AND DetMovC.Mov = "R" NO-LOCK:
                l-Descto2 = l-Descto2 + DetMovC.MontoPago.
            END.
            
            ASSIGN
                l-Descto1 = Remision.Descuento.
            
            IF l-Descto1 > 0 OR l-Descto2 > 0 THEN
                l-FactorPP = (1 - (l-Descto1 / Remision.Subtotal)) * (1 - (l-Descto2 / Remision.Tot)).
            ELSE
                l-FactorPP = 1.
                               
            IF l-Subtot1 <= 0 THEN NEXT.
            
            IF l-Subtot1 = ? THEN
                ASSIGN l-Subtot1  = 0.
                
            IF l-Costo3 = ? THEN
                ASSIGN l-Costo3    = 0.
    
    
            FIND ttFacturas WHERE ttFacturas.idFactura = Remision.id-Remision NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttFacturas THEN DO:
                CREATE ttFacturas.
                ASSIGN
                    ttFacturas.idFactura  = Remision.id-Remision
                    ttFacturas.fecreg      = Remision.fecReg
                    ttFacturas.IdCliente  = Remision.id-Cliente
                    ttFacturas.IdVendedor = Remision.Id-Vendedor
                    ttFacturas.RazSoc      = Remision.RazonSocial
                    ttFacturas.FecPago     = MovCaja.FecOper
                    ttFacturas.FecVence    = Remision.FecReg
                    ttFacturas.DiasPago    = 0
                    ttFacturas.DiasVenc    = 0.
    
    
            END.
    
            ASSIGN
                ttFacturas.importe = ttFacturas.importe + l-subtot1
                ttFacturas.costo   = ttFacturas.costo  + l-costo3
                ttFacturas.descto  = ttFacturas.descto + (l-Subtot2 - (l-SubTot2 * l-FactorPP))
    
                ttFacturas.Neto    = ttFacturas.Importe - ttFacturas.Descto
                ttFacturas.PagAnt  = 0
                ttFacturas.Dif     = ttFacturas.Neto    - ttFacturas.Costo
                ttFacturas.DifOri  = ttFacturas.Neto    - ttFacturas.Costo
                
                ttFacturas.Margen  = (1 - ((ttFacturas.costo - ttFacturas.impFlete) / (ttFacturas.Importe - ttFacturas.Descto))) * 100.
                
        END. /* for each MovCaja, Remision */
    
        FOR EACH bf-MovCliente WHERE bf-MovCliente.afectado
                                 AND bf-MovCliente.fecreg >= v-fecini
                                 AND bf-MovCliente.fecreg <= v-fecfin
                                 AND bf-MovCliente.id-mc > 3
                               NO-LOCK,
            FIRST Factura WHERE Factura.Id-Factura = bf-MovCliente.RefSaldo 
                            AND Factura.Id-Vendedor = Vendedor.Id-Vendedor
                          NO-LOCK 
                          BREAK BY bf-MovCliente.RefSaldo BY bf-MovCliente.fecreg:
            
            IF NOT FIRST-OF(bf-MovCliente.RefSaldo) THEN NEXT.
            IF Factura.Id-Cliente <= 10 AND Factura.Id-Cliente <> 3 THEN NEXT.
            FIND FIRST MovCliente WHERE MovCliente.RefSaldo = bf-MovCliente.RefSaldo
                                    AND MovCliente.Id-MC = 67 NO-LOCK NO-ERROR.
                                    
            IF AVAILABLE MovCliente THEN NEXT.
        
            l-FecPag = ?.
            l-PagAnt = 0.
            FOR EACH MovCliente WHERE MovCliente.fecreg <= v-fecfin
                                  AND MovCliente.refsaldo = bf-MovCliente.refsaldo NO-LOCK:
                FIND Acuse WHERE Acuse.Id-Acuse = MovCliente.Documento NO-LOCK NO-ERROR.
                IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN
                    NEXT.
                ACCUMULATE MovCliente.importe(TOTAL).
                IF l-FecPag = ? OR (l-FecPag <> ? AND MovCliente.FecReg > l-FecPag) THEN
                   ASSIGN l-FecPag = MovCliente.FecReg.
                IF AVAILABLE Acuse AND MovCliente.FecReg <= 09/20/2022 AND
                   CAN-DO("52,57,58,60,61,62",STRING(MovCliente.Id-MC,"99")) THEN
                   l-PagAnt = l-PagAnt + (MovCliente.Importe * -1).
            END.
    
            IF (ACCUM TOTAL MovCliente.importe) < 1 THEN DO:
                l-Descto2 = 0.
                l-MovImp1 = 0.
                l-PagAntSin = 0.
                FOR EACH MovCliente WHERE MovCliente.Afectado
                                      AND MovCliente.refsaldo = bf-MovCliente.refsaldo
                                      AND MovCliente.Id-MC > 3 NO-LOCK:
                    FIND FIRST TipoPago WHERE TipoPago.Id-TP = MovCliente.Id-MC NO-LOCK NO-ERROR.
                    IF AVAILABLE TipoPago OR MovCliente.Id-MC = 65 OR MovCliente.Id-MC = 90 THEN
                        NEXT.
                    l-MovImp1 = l-MovImp1 + (MovCliente.Importe * -1).
                END.
                IF l-MovImp1 <> 0 THEN DO:
                    FOR EACH DistIva WHERE DistIVA.Id-Factura = bf-MovCliente.RefSaldo
                                       AND DistIVA.TipoVenta = 3 NO-LOCK:
                        l-Descto2 = l-Descto2 + (l-MovImp1 * (DistIVA.Participacion / 100) / (1 + (DistIVA.PorcIVA / 100))).
                        l-PagAntSin  = l-PagAntSin + (l-PagAnt * (DistIVA.Participacion / 100) / (1 + (DistIVA.PorcIVA / 100))).
                    END.
                END.    
                    
                ASSIGN l-Subtot1 = 0
                       l-Costo3  = 0
                       l-Descto1 = 0.
                FOR EACH DetFactura OF Factura WHERE DetFactura.Tipo <= 2 NO-LOCK:
                    IF DetFactura.Tipo = 1 OR
                       (DetFactura.Tipo = 2 AND DetFactura.Descr BEGINS 'Remision') THEN DO:                                
                        ASSIGN l-Costo3 = l-Costo3 + (DetFactura.Costo * (DetFactura.Cant - DetFactura.CantDev)).
                        ASSIGN l-SubTot1 = l-SubTot1 + (DetFactura.PrecUnit * 
                                  ((100 - DetFactura.Descto) / 100) *
                                  (DetFactura.Cant - DetFactura.CantDev)).
                    END.
                END.
                
                ASSIGN l-Descto1 = Factura.Descuento.
                IF l-Descto1 > 0 /*OR l-Descto2 > 0*/ THEN
                   l-FactorPP = (1 - (l-Descto1 / Factura.Subtotal)). /* *
                                (1 - (l-Descto2 / Factura.Tot)).*/
                ELSE l-FactorPP = 1.
                                   
                IF l-Subtot1 <= 0 THEN NEXT.
                IF l-Subtot1 = ? THEN ASSIGN l-Subtot1  = 0.
                IF l-Costo3 = ? THEN ASSIGN l-Costo3    = 0.
            
            
                FIND FIRST ttFacturas WHERE ttFacturas.idFactura = Factura.id-Factura
                                       NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttFacturas THEN DO:
            
                    CREATE ttFacturas.
                    ASSIGN
                        ttFacturas.idFactura  = Factura.id-Factura
                        ttFacturas.fecreg      = Factura.fecreg
                        ttFacturas.IdCliente  = Factura.id-Cliente
                        ttFacturas.IdVendedor = Factura.Id-Vendedor
                        ttFacturas.RazSoc      = Factura.RazonSocial
                        ttFacturas.FecPago     = l-FecPag
                        ttFacturas.FecVence    = Factura.FecVenc
                        ttFacturas.DiasPago    = l-FecPag - Factura.FecReg
                        ttFacturas.DiasVenc    = IF l-FecPag > Factura.FecVenc 
                                                  THEN l-FecPag - Factura.FecVenc ELSE 0.
  
           
                END.
            
                IF Factura.Id-Moneda > 1 THEN
                   ASSIGN
                        ttFacturas.importe = ttFacturas.importe + (l-subtot1 * Factura.TipoCambio)
                        ttFacturas.costo   = ttFacturas.costo  + (l-costo3 * Factura.TipoCambio)
                        ttFacturas.descto  = ttFacturas.descto + (((l-Subtot1 - (l-SubTot1 * l-FactorPP)) + l-Descto2) * Factura.TipoCambio)
                        ttFacturas.Margen  = (1 - ( (ttFacturas.costo + ttFacturas.impFlete) / (ttFacturas.Importe - ttFacturas.Descto) )) * 100
                        ttFacturas.PagAnt  = ttFacturas.PagAnt  + (l-PagAntSin * Factura.TipoCambio)
                        ttFacturas.Neto    = ttFacturas.Importe - ttFacturas.Descto
                        ttFacturas.Dif     = ttFacturas.Neto    - ttFacturas.Costo - (ttFacturas.PagAnt * (ttFacturas.Margen / 100))
                        ttFacturas.DifOri  = ttFacturas.Neto    - ttFacturas.Costo.
                ELSE    
                    ASSIGN
                        ttFacturas.importe = ttFacturas.importe + l-subtot1
                        ttFacturas.costo   = ttFacturas.costo  + l-costo3
                        ttFacturas.descto  = ttFacturas.descto + ((l-Subtot1 - (l-SubTot1 * l-FactorPP)) + l-Descto2)
                        ttFacturas.Margen  = (1 - ( (ttFacturas.costo + ttFacturas.impFlete) / (ttFacturas.Importe - ttFacturas.Descto) )) * 100
                        ttFacturas.PagAnt  = ttFacturas.PagAnt  + l-PagAntSin
                        ttFacturas.Neto    = ttFacturas.Importe - ttFacturas.Descto
                        ttFacturas.Dif     = ttFacturas.Neto    - ttFacturas.Costo - (ttFacturas.PagAnt * (ttFacturas.Margen / 100))
                        ttFacturas.DifOri  = ttFacturas.Neto    - ttFacturas.Costo.
                    
               IF ttFacturas.PagAnt > ttFacturas.Neto THEN
                  ASSIGN ttFacturas.PagAnt = ttFacturas.Neto.   
                    
            END. /* si esta totalmente pagada */
        END. /* for each bf-MovCliente */        
    END.
END.          