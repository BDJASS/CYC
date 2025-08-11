@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : poraut.p
    Purpose     : Programa que regresa la cantidad de pedidos autorizados, 
                  cancelados y rechazados por empleado en el mes actual
    Syntax      : /PedidoEmpMes   */
 
DEFINE VARIABLE vFecIni AS DATE NO-UNDO.
DEFINE VARIABLE vFecFin AS DATE NO-UNDO.
DEFINE VARIABLE vCont AS INTEGER NO-UNDO.   


DEFINE VARIABLE vLimite LIKE Cliente.Limite NO-UNDO.  
DEFINE VARIABLE vLimiteMaximo LIKE Cliente.Limite NO-UNDO.
DEFINE VARIABLE vFolio AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDisponible AS DECIMAL NO-UNDO.           

DEFINE VARIABLE l-Loc AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Para AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Estatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-limite2 AS DECIMAL NO-UNDO.        
DEFINE VARIABLE v-limitemaximo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-disponible AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Secuencia AS INTEGER NO-UNDO.
DEFINE VARIABLE l-Truco AS LOGICAL NO-UNDO.
DEFINE VARIABLE vSaldoV30 AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldoV40 AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldoVenc AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-NomVend LIKE Usuario.Nom-Usuario NO-UNDO.
DEFINE VARIABLE v-SaldoPed AS DECIMAL NO-UNDO.
DEFINE VARIABLE vPedSaldo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-DC AS INTEGER NO-UNDO.
DEFINE VARIABLE v-PromPago AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-FecDep AS DATE NO-UNDO.
DEFINE VARIABLE l-Acuse AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ImpPago AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-SumaDoc AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotChDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotFac AS DECIMAL NO-UNDO.
DEFINE VARIABLE vEfecto AS DECIMAL NO-UNDO.
DEFINE VARIABLE vChNoDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotPed AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Importe AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-Asunto AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Contenido AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Pedido AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Hora AS INTEGER NO-UNDO.

DEFINE BUFFER bfPedido FOR Pedido.
DEFINE BUFFER bf2Pedido FOR Pedido.
DEFINE BUFFER b-MovCliente FOR MovCliente.
DEFINE BUFFER b-HistMovCte FOR HistMovCte.

DEFINE TEMP-TABLE ttFactura  
    FIELD Id-Cliente LIKE Cliente.Id-Cliente 
    FIELD Id-Vendedor LIKE Vendedor.Id-Vendedor
    FIELD Tipo AS CHARACTER 
    FIELD RecAuto AS RECID   
    FIELD Importe AS DECIMAL 
    FIELD Id-Ubic AS CHARACTER   
    FIELD Id-Banco LIKE Banco.Id-Banco
    FIELD NumCheque LIKE Cheque.NumCheque
    FIELD CtaCheq AS CHARACTER 
    FIELD FecCheque AS DATE
    FIELD Saldo AS DECIMAL
    FIELD SaldoVenc AS DECIMAL
    FIELD SaldoV30 AS DECIMAL 
    FIELD SaldoV40 AS DECIMAL
    FIELD PedSaldo AS DECIMAL
    FIELD Orden AS INTEGER.

DEFINE TEMP-TABLE ttPedido
    FIELD Id-Cliente LIKE cliente.Id-cliente COLUMN-LABEL "Cte"
    FIELD Id-Vendedor LIKE Vendedor.Id-Vendedor
    FIELD Nombre LIKE Empleado.Nombre
    FIELD RazonSocial LIKE Cliente.RazonSocial
    FIELD Saldo AS DECIMAL    /* Saldo en MovCliente */
    FIELD SaldoTot AS DECIMAL /* Saldo total incluyendo pedidos en transito */ 
    FIELD pedsaldo AS DECIMAL /* Saldo en pedidos por facturar */
    FIELD SaldoVenc AS DECIMAL 
    FIELD SaldoV30 AS DECIMAL /* Saldo vencido a mas de 30 dias del plazo */
    FIELD SaldoV40 AS DECIMAL /* Saldo vencido a mas de 40 dias del plazo */
    FIELD FecReg AS DATE FORMAT "99/99/99"
    FIELD HorReg AS CHARACTER FORMAT 'x(5)'
    FIELD Importe AS DECIMAL FORMAT "zzz,zz9.99"
    FIELD Id-Pedido LIKE Pedido.Id-Pedido
    FIELD CHPF-Blk AS CHARACTER FORMAT 'X(4)'
    FIELD Autorizado AS LOGICAL INITIAL FALSE FORMAT 'AUT/NO'
    FIELD Cond AS CHARACTER FORMAT 'x(5)'
    FIELD TotChDep AS DECIMAL
    FIELD TotFac AS DECIMAL
    FIELD Efecto AS DECIMAL
    FIELD ChNoDep AS DECIMAL
    FIELD TotPed AS DECIMAL
    FIELD Simbolo LIKE Moneda.simbolo
    FIELD Id-Alm LIKE Almacen.Id-Alm     
    FIELD Id-UbiVta LIKE Pedido.Id-UbiVta
    FIELD Tot LIKE Pedido.Tot
    FIELD Plazo LIKE Pedido.Plazo  // Ajuste ticket 225 Azure
    INDEX IdxPrinc FecReg HorReg Id-Pedido. 
    
DEFINE TEMP-TABLE ttPorAut
    FIELD Estatus AS CHARACTER /* AUTORIZADO, PENDIENTE, RECHAZADO, CANCELADO */ 
    FIELD Orden AS INTEGER 
    FIELD IdClaseCte LIKE Cliente.Id-ClaseCte
    FIELD NombreClaseCte LIKE ClaseCte.Descr
    FIELD IdCliente LIKE Cliente.Id-Cliente
    FIELD RazonSocial LIKE Cliente.RazonSocial
    FIELD Bloqueado AS LOGICAL FORMAT "SI/NO"
    FIELD TipoBloqueo AS CHARACTER 
    FIELD Documento AS CHARACTER 
    FIELD TipoDocto AS CHARACTER /* FACTURA, PEDIDO */
    FIELD Cond AS CHARACTER /* CONTADO, CREDITO */
    FIELD FecReg AS DATE 
    FIELD HorReg AS CHARACTER 
    FIELD Importe LIKE Pedido.Tot
    FIELD Plazo AS INTEGER FORMAT "ZZ9"
    FIELD Sucursal AS CHARACTER 
    FIELD NomVendedor AS CHARACTER 
    FIELD NomResponsable AS CHARACTER
    FIELD FecAutorizado AS DATE
    FIELD Motivo AS CHARACTER  
    FIELD Desactivado AS LOGICAL
    FIELD SaldoVenc AS DECIMAL /* nuevos campos */
    FIELD Saldo AS DECIMAL
    FIELD IdUbic AS CHARACTER   
    FIELD RecAuto AS RECID
    FIELD IdVendedor LIKE Vendedor.Id-Vendedor.

DEFINE NEW SHARED VARIABLE g-Origen   AS CHARACTER  NO-UNDO INITIAL "02B".
DEFINE NEW SHARED VARIABLE g-nomcia   AS CHARACTER  NO-UNDO.  
DEFINE NEW SHARED VARIABLE g-dist AS INTE FORMAT "9999" NO-UNDO.
DEFINE NEW SHARED VARIABLE g-tty   AS CHARACTER  NO-UNDO.  

/* **********************  Internal Procedures  *********************** */
FIND FIRST SysGeneral NO-LOCK NO-ERROR.
IF AVAILABLE SysGeneral THEN ASSIGN g-nomcia = SysGeneral.Empresa.       

ASSIGN 
      g-tty = STRING(TIME).
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetPorAut: 
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipClaseCte LIKE Cliente.Id-ClaseCte NO-UNDO.
DEFINE INPUT PARAMETER ipNivel AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttPorAut.

DEFINE BUFFER bfttFactura FOR ttFactura.
DEFINE BUFFER bfPedido FOR Pedido.
ASSIGN vCont = 0.

/* Log de entrada del procedimiento */
    LOG-MANAGER:WRITE-MESSAGE(
        SUBSTITUTE("/PedidosEmpMes [PEDIDOS] /GetPorAut >>> Ejecutando con Clase = &1, ipNivel = &2 a las &3",
                   STRING(ipClaseCte), STRING(ipNivel), STRING(NOW))
    ).  
    
/* Carga de facturas de tiendas */
EMPTY TEMP-TABLE ttFactura NO-ERROR.

FOR EACH AutPend WHERE NOT CAN-FIND(FIRST Autorizacion WHERE Autorizacion.Id-Cliente = AutPend.Id-Cliente
                                                         AND Autorizacion.RecTipo = AutPend.RecAuto) NO-LOCK:
    CREATE ttFactura.
    BUFFER-COPY AutPend EXCEPT RecAuto TO ttFactura.
    ASSIGN ttFactura.RecAuto = AutPend.RecAuto.

    FOR EACH bfttFactura WHERE bfttFactura.recauto = ttFactura.recauto NO-LOCK.
        ACCUMULATE 11 (COUNT).
    END.
    IF (ACCUM COUNT 11) > 1 THEN DO:
        DELETE ttFactura.
        NEXT.
    END.   
    
    IF ttFactura.Id-Cliente = 0 THEN
        NEXT.

    FOR EACH MovCliente WHERE MovCliente.Id-Cliente = ttFactura.Id-Cliente
                          AND MovCliente.Id-mc <= 3 NO-LOCK:

        IF MovCliente.FecVenc < TODAY THEN DO:
            ASSIGN 
                ttFactura.SaldoVenc = ttFactura.SaldoVenc + MovCliente.Saldo.

            IF MovCliente.fecvenc < (TODAY - 30) THEN            
                ASSIGN
                    ttFactura.SaldoV30 = ttFactura.SaldoV30 + MovCliente.Saldo.
        END.
        
        IF MovCliente.importe = MovCliente.Saldo AND TODAY - MovCliente.FecReg > 40 THEN            
            ASSIGN
                ttFactura.SaldoV40 = ttFactura.SaldoV40 + MovCliente.Saldo.

        ASSIGN
            ttFactura.Saldo = ttFactura.Saldo + MovCliente.Saldo.

    END. /* del for each MovCliente */

    FOR EACH Pedido WHERE Pedido.Id-Cliente = ttFactura.Id-Cliente
                      AND Pedido.Id-Estatus >= -1
                      AND Pedido.Id-Estatus < 5
                      AND Pedido.EnFirme = TRUE NO-LOCK:
        ASSIGN 
            ttFactura.Saldo = ttFactura.Saldo + Pedido.Subtotal
            ttFactura.PedSaldo = ttFactura.PedSaldo + Pedido.Subtotal.       

    END. /* del pedido */  

END.
INPUT CLOSE.

FOR EACH ttFactura EXCLUSIVE-LOCK,
    FIRST Cliente OF ttFactura NO-LOCK,
  //  FIRST Cliente OF ttFactura WHERE Cliente.Id-ClaseCte = ipClaseCte NO-LOCK,
    FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK:
    
    FIND FIRST BlkAut WHERE BlkAut.Id-Cliente = Cliente.Id-Cliente NO-LOCK NO-ERROR.
    FIND FIRST blkRFC WHERE REPLACE(blkRFC.RFC," ","") = REPLACE(Cliente.RFC," ","") NO-LOCK NO-ERROR.
    FIND UbiVta WHERE UbiVta.Id-UbiVta = ttFactura.Id-Ubic NO-LOCK NO-ERROR.
    FIND Vendedor WHERE Vendedor.Id-Vendedor = ttFactura.Id-Vendedor NO-LOCK NO-ERROR.
    IF AVAILABLE Vendedor THEN 
        FIND Empleado WHERE Empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
    FIND LAST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.

    ASSIGN vCont = vCont + 1.
    CREATE ttPorAut.
    ASSIGN 
        ttPorAut.Estatus = "PENDIENTE"
        ttPorAut.Orden = vCont
        ttFactura.Orden = vCont 
        ttPorAut.IdClaseCte = Cliente.Id-claseCte
        ttPorAut.NombreClaseCte = ClaseCte.Descr
        ttPorAut.IdCliente = Cliente.Id-Cliente
        ttPorAut.RazonSocial = Cliente.RazonSocial
        ttPorAut.Bloqueado = IF AVAILABLE BlkAut OR AVAILABLE blkRFC THEN TRUE ELSE FALSE
        ttPorAut.TipoBloqueo = IF AVAILABLE BlkAut THEN "BLOQUEO AUTORIZACION"
                               ELSE IF AVAILABLE blkRFC
                                    THEN "BLOQUEO RFC"
                                    ELSE "" 
        ttPorAut.Documento = "" 
        ttPorAut.TipoDocto = ttFactura.Tipo
        ttPorAut.Cond = (IF ttFactura.Tipo = "REMISION" THEN "CONTADO" ELSE "CREDITO")
        ttPorAut.FecReg = TODAY  
        ttPorAut.HorReg = STRING(TIME, "HH:MM:SS")  
        ttPorAut.Importe = ttFactura.Importe
        ttPorAut.Plazo = Cliente.Plazo
        ttPorAut.Sucursal = IF AVAILABLE UbiVta THEN UbiVta.Descr ELSE ttFactura.Id-Ubic 
        ttPorAut.NomVendedor = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE ttFactura.Id-Vendedor  
        ttPorAut.NomResponsable = IF AVAILABLE Resp THEN Resp.Nombre ELSE STRING(Cliente.Id-Resp)
        ttPorAut.RecAuto = ttFactura.RecAuto
        ttPorAut.SaldoVenc = ttFactura.SaldoVenc
        ttPorAut.Saldo = ttFactura.Saldo
        ttPorAut.IdVendedor = ttFactura.Id-Vendedor
        ttPorAut.IdUbic = ttFactura.Id-Ubic.
        /*
        ttPorAut.FecAutorizado = TODAY.
        */

    IF ipNivel > 3 THEN DO:    
        IF ttFactura.saldov40 > 0 OR
           ttFactura.Importe > 8000 OR
           (ttFactura.importe + ttFactura.Saldo) >= (Cliente.Limite * 1.5) THEN  
            ASSIGN ttPorAut.Desactivado = TRUE.
    END.
        
END.
/**/

/* Carga de pedidos pendientes de autorizar */
FOR EACH Pedido WHERE Pedido.Id-Pedido BEGINS '0'
                  AND Pedido.EnFirme = FALSE NO-LOCK,
    FIRST Cliente OF Pedido WHERE Cliente.Id-ClaseCte = ipClaseCte NO-LOCK:

    IF Pedido.Id-Vendedor = "0100" AND Pedido.Id-Cond = 0 THEN NEXT.

    FIND FIRST Moneda WHERE Moneda.id-moneda = Pedido.Id-Moneda NO-LOCK NO-ERROR.

    CREATE ttPedido.
    ASSIGN ttPedido.id-cliente  = Pedido.Id-Cliente
           ttPedido.id-vendedor = Pedido.Id-Vendedor
           ttPedido.Id-Pedido   = Pedido.Id-Pedido
           ttPedido.FecReg      = Pedido.FecReg
           ttPedido.RazonSocial = Pedido.RazonSocial
           ttPedido.HorReg      = STRING(Pedido.HrSta,'HH:MM:SS')
           ttPedido.importe     = Pedido.Subtotal
           ttPedido.CHPF-Blk    = (IF Pedido.Filler-1 MATCHES '*CHPF*' THEN "CHPF" ELSE "")
           ttPedido.simbolo     = (IF AVAILABLE Moneda AND Moneda.Id-Moneda > 1 THEN Moneda.simbolo ELSE "")   /*_ RNPC _*/
           ttPedido.Id-Alm      = Pedido.Id-Alm
           ttPedido.Id-UbiVta   = Pedido.Id-UbiVta
           ttPedido.Tot         = Pedido.Tot
           ttPedido.Plazo       = Pedido.Plazo.

    FIND FIRST BlkAut WHERE BlkAut.Id-Cliente = Pedido.Id-Cliente NO-LOCK NO-ERROR.
    FIND FIRST blkRFC WHERE REPLACE(blkRFC.RFC," ","") = REPLACE(Pedido.RFC," ","") NO-LOCK NO-ERROR.
    IF AVAILABLE BlkAut OR AVAILABLE blkRFC THEN
        ASSIGN ttPedido.CHPF-Blk = "BLOQ".

    FIND LAST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.

    FIND FIRST CondVta WHERE CondVta.Id-Cond = Pedido.Id-Cond NO-LOCK NO-ERROR.
    IF AVAILABLE CondVta THEN
        ASSIGN ttPedido.Cond = CondVta.Descr.

    vFecIni = ?.

    FOR EACH MovCliente WHERE MovCliente.id-cliente = ttPedido.id-cliente AND movcliente.id-mc <= 3 NO-LOCK:
        IF MovCliente.FecVenc < TODAY THEN DO:
            ASSIGN 
                ttPedido.saldoVenc = ttPedido.saldovenc + movcliente.saldo.

            IF movcliente.fecvenc < (TODAY - 30) THEN            
                ASSIGN
                    ttPedido.saldov30 = ttPedido.saldov30 + movcliente.saldo.

        END.
    
        IF movcliente.importe = movcliente.saldo AND TODAY - movcliente.fecreg > 40 THEN            
            ASSIGN
                ttPedido.saldov40 = ttPedido.saldov40 + movcliente.saldo.

        IF vFecIni = ? THEN
            vFecIni = movcliente.fecreg.

        ASSIGN 
            ttPedido.saldo = ttPedido.saldo + movcliente.saldo.
    END. /* del for each movcliente */
  
    ASSIGN ttPedido.SaldoTot = ttPedido.Saldo
           ttPedido.PedSaldo = 0.
    
    FOR EACH bfPedido WHERE bfPedido.Id-Cliente = ttPedido.id-cliente
                        AND bfPedido.Id-Estatus >= -1
                        AND bfPedido.Id-Estatus < 5
                        AND bfPedido.EnFirme = TRUE NO-LOCK:
        ASSIGN 
            ttPedido.saldoTot = ttPedido.saldoTot + (bfPedido.Tot)
            ttPedido.pedsaldo = ttPedido.pedsaldo + (bfPedido.Tot).
    END. /* del bfPedido */

END.

FOR EACH ttPedido,
    FIRST Cliente OF ttPedido NO-LOCK,
    FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte
    NO-LOCK BY ttPedido.FecReg BY ttPedido.HorReg BY ttPedido.Id-Pedido:
        
    IF ttPedido.id-cliente = 0 THEN DO:
        DELETE ttPedido.
        NEXT.
    END.

    FIND FIRST UbiVta WHERE UbiVta.Id-UbiVta = ttPedido.Id-UbiVta NO-LOCK NO-ERROR.
    
    FIND Vendedor WHERE Vendedor.Id-Vendedor = ttPedido.Id-Vendedor NO-LOCK NO-ERROR.
    IF AVAILABLE Vendedor THEN 
        FIND Empleado WHERE Empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
    IF AVAILABLE Empleado THEN 
        ASSIGN ttPedido.Nombre = Empleado.Nombre.           
    FIND FIRST BlkAut WHERE BlkAut.Id-Cliente = Cliente.Id-Cliente NO-LOCK NO-ERROR.
    FIND FIRST blkRFC WHERE REPLACE(blkRFC.RFC," ","") = REPLACE(Cliente.RFC," ","") NO-LOCK NO-ERROR.
    
    ASSIGN vCont = vCont + 1.
    CREATE ttPorAut.
    ASSIGN 
        ttPorAut.Estatus = "PENDIENTE"
        ttPorAut.Orden = vCont 
        ttPorAut.IdClaseCte = Cliente.Id-ClaseCte
        ttPorAut.NombreClaseCte = ClaseCte.Descr
        ttPorAut.IdCliente = Cliente.Id-Cliente
        ttPorAut.RazonSocial = ttPedido.RazonSocial
        ttPorAut.Bloqueado = IF AVAILABLE BlkAut OR AVAILABLE blkRFC THEN TRUE ELSE FALSE
        ttPorAut.TipoBloqueo = IF AVAILABLE BlkAut THEN "BLOQUEO AUTORIZACION"
                               ELSE IF AVAILABLE blkRFC
                                    THEN "BLOQUEO RFC"
                                    ELSE "" 
        ttPorAut.Documento = ttPedido.Id-Pedido 
        ttPorAut.TipoDocto = "PEDIDO"
        ttPorAut.Cond = ttPedido.Cond
        ttPorAut.FecReg = ttPedido.FecReg
      //  ttPorAut.FecReg = TODAY  
        ttPorAut.HorReg = ttPedido.HorReg      
        ttPorAut.Importe = ttPedido.Tot
        ttPorAut.Plazo = ttPedido.Plazo  // Cliente.Plazo
        ttPorAut.Sucursal = IF AVAILABLE UbiVta THEN UbiVta.Descr ELSE ttPedido.Id-UbiVta 
        ttPorAut.NomVendedor = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE ttPedido.Id-Vendedor  
        ttPorAut.NomResponsable = IF AVAILABLE Resp THEN Resp.Nombre ELSE STRING(Cliente.Id-Resp)
        ttPorAut.RecAuto = 0.
        /*
        ttPorAut.FecAutorizado = TODAY.
        */
    IF ipNivel > 3  AND ttPedido.Cond <> "CONTADO" THEN DO:    
        IF ttPedido.saldov40 > 0 OR
           ttPedido.Importe > 8000 OR
           (ttPedido.importe + ttPedido.Saldo) >= (Cliente.Limite * 1.5) THEN  
            ASSIGN ttPorAut.Desactivado = TRUE.
    END.
    
    RELEASE Vendedor.
    RELEASE Empleado.
END.   
/**/

/* Carga de pedidos autorizados, rechazados y cancelados */

/*
ASSIGN vFecIni = DATE(STRING(MONTH(TODAY)) + "/01/" + STRING(YEAR(TODAY))).
IF MONTH(TODAY) = 12 THEN 
    ASSIGN vFecFin = DATE("12/31/" + STRING(YEAR(TODAY))).
ELSE 
    ASSIGN vFecFin = DATE(MONTH(TODAY) + 1, 1, YEAR(TODAY)) - 1.
*/

FOR EACH Pedido WHERE Pedido.AutPor <> ""
                  AND MONTH(Pedido.FecReg)= MONTH(TODAY)
                  AND YEAR(Pedido.FecReg)= YEAR(TODAY) 
                  AND NOT Pedido.Filler-1 BEGINS "*** GRAFICO" USE-INDEX Idx-FecReg NO-LOCK,
    FIRST Cliente OF Pedido WHERE Cliente.Id-ClaseCte = ipClaseCte NO-LOCK,
    FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK:
    
    FIND FIRST BlkAut WHERE BlkAut.Id-Cliente = Pedido.Id-Cliente NO-LOCK NO-ERROR.
    FIND FIRST blkRFC WHERE REPLACE(blkRFC.RFC," ","") = REPLACE(Pedido.RFC," ","") NO-LOCK NO-ERROR.
    
    FIND LAST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
    
    FIND FIRST CondVta WHERE CondVta.Id-Cond = Pedido.Id-Cond NO-LOCK NO-ERROR.
    FIND FIRST UbiVta WHERE UbiVta.Id-UbiVta = Pedido.Id-UbiVta NO-LOCK NO-ERROR.

    ASSIGN vCont = vCont + 1.
    CREATE ttPorAut.
    ASSIGN 
        ttPorAut.Estatus = "AUTORIZADO"
        ttPorAut.Orden = vCont 
        ttPorAut.IdClaseCte = Cliente.Id-ClaseCte
        ttPorAut.NombreClaseCte = ClaseCte.Descr
        ttPorAut.IdCliente = Cliente.Id-Cliente
        ttPorAut.RazonSocial = Pedido.RazonSocial
        ttPorAut.Bloqueado = IF AVAILABLE BlkAut OR AVAILABLE blkRFC THEN TRUE ELSE FALSE
        ttPorAut.TipoBloqueo = IF AVAILABLE BlkAut THEN "BLOQUEO AUTORIZACION"
                               ELSE IF AVAILABLE blkRFC
                                    THEN "BLOQUEO RFC"
                                    ELSE "" 
        ttPorAut.Documento = Pedido.Id-Pedido 
        ttPorAut.TipoDocto = "PEDIDO"
        ttPorAut.Cond = IF AVAILABLE CondVta THEN CondVta.Descr ELSE STRING(Pedido.Id-Cond)
        ttPorAut.FecReg = Pedido.FecReg  
    //    ttPorAut.FecReg = TODAY  
        ttPorAut.HorReg = STRING(Pedido.HrSta, "HH:MM:SS")
        ttPorAut.Importe = Pedido.Tot
        ttPorAut.Plazo = Pedido.Plazo //Cliente.Plazo
        ttPorAut.Sucursal = IF AVAILABLE UbiVta THEN UbiVta.Descr ELSE Pedido.Id-UbiVta 
        ttPorAut.NomVendedor = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE Pedido.Id-Vendedor  
        ttPorAut.NomResponsable = IF AVAILABLE Resp THEN Resp.Nombre ELSE STRING(Cliente.Id-Resp)
        ttPorAut.FecAutorizado = Pedido.FecReg
        ttPorAut.RecAuto = 0.
    
END.   

FOR EACH CancPed WHERE MONTH(CancPed.FecCancel)= MONTH(TODAY)
                   AND YEAR(CancPed.FecCancel)= YEAR(TODAY)
                   AND NOT CancPed.Filler-1 BEGINS "*** GRAFICO"  USE-INDEX Idx-FecReg NO-LOCK,
    FIRST Cliente OF CancPed WHERE Cliente.Id-ClaseCte = ipClaseCte NO-LOCK,
    FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK:
    
    FIND FIRST BlkAut WHERE BlkAut.Id-Cliente = CancPed.Id-Cliente NO-LOCK NO-ERROR.
    FIND FIRST blkRFC WHERE REPLACE(blkRFC.RFC," ","") = REPLACE(CancPed.RFC," ","") NO-LOCK NO-ERROR.
    
    FIND LAST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
    
    FIND FIRST CondVta WHERE CondVta.Id-Cond = CancPed.Id-Cond NO-LOCK NO-ERROR.
    FIND FIRST UbiVta WHERE UbiVta.Id-UbiVta = CancPed.Id-UbiVta NO-LOCK NO-ERROR.

    ASSIGN vCont = vCont + 1.   
    CREATE ttPorAut.
    ASSIGN 
        ttPorAut.Estatus = IF CancPed.EnFirme = TRUE THEN "CANCELADO" ELSE "RECHAZADO"
        ttPorAut.Orden = vCont 
        ttPorAut.IdClaseCte = Cliente.Id-ClaseCte
        ttPorAut.NombreClaseCte = ClaseCte.Descr
        ttPorAut.IdCliente = Cliente.Id-Cliente
        ttPorAut.RazonSocial = CancPed.RazonSocial
        ttPorAut.Bloqueado = IF AVAILABLE BlkAut OR AVAILABLE blkRFC THEN TRUE ELSE FALSE
        ttPorAut.TipoBloqueo = IF AVAILABLE BlkAut THEN "BLOQUEO AUTORIZACION"
                               ELSE IF AVAILABLE blkRFC
                                    THEN "BLOQUEO RFC"
                                    ELSE "" 
        ttPorAut.Documento = CancPed.Id-Pedido 
        ttPorAut.TipoDocto = "PEDIDO"
        ttPorAut.Cond = IF AVAILABLE CondVta THEN CondVta.Descr ELSE STRING(CancPed.Id-Cond)
        ttPorAut.FecReg = CancPed.FecReg  
      //  ttPorAut.FecReg = TODAY  
        ttPorAut.HorReg = STRING(CancPed.HrSta, "HH:MM:SS")
        ttPorAut.Importe = CancPed.Tot
        ttPorAut.Plazo = Cliente.Plazo
        ttPorAut.Sucursal = IF AVAILABLE UbiVta THEN UbiVta.Descr ELSE CancPed.Id-UbiVta 
        ttPorAut.NomVendedor = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE CancPed.Id-Vendedor  
        ttPorAut.NomResponsable = IF AVAILABLE Resp THEN Resp.Nombre ELSE STRING(Cliente.Id-Resp)
        ttPorAut.FecAutorizado = CancPed.FecReg
        ttPorAut.Motivo = CancPed.MotivoSolCanc
        ttPorAut.RecAuto = 0. 
    
END.   
/**/

RETURN.
END PROCEDURE.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostAutorizador:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:  Autorizacion de Facturas en cajas desde .Net
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipAutoriza AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipUser AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ttPorAut. 

/* Log simple   */
LOG-MANAGER:WRITE-MESSAGE(
    SUBSTITUTE("/PedidoEmpMEs [Post] Autorizacion de Facturas >>> Inicia ejecución | Usuario: &1 | Autoriza: &2 | FechaHora: &3",
               ipUser,
               IF ipAutoriza THEN "TRUE" ELSE "FALSE",
               STRING(NOW))
).  

RUN programas/facautorizacion.p(INPUT ipAutoriza,INPUT ipUser,INPUT TABLE ttPorAut).  
RETURN.
END PROCEDURE.    

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PutAutorizarPedido:       
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*
    Autoriza pedidos desde .Net
*/

DEFINE INPUT PARAMETER ipPedido LIKE Pedido.Id-Pedido NO-UNDO.
DEFINE INPUT PARAMETER ipSegunda AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipTodos AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipUsuario AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipImporte AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opError AS CHARACTER NO-UNDO.  

DEFINE VARIABLE vLimite LIKE Cliente.Limite NO-UNDO.
DEFINE VARIABLE vLimiteMaximo LIKE Cliente.Limite NO-UNDO.
DEFINE VARIABLE vFolio AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDisponible AS DECIMAL NO-UNDO.

DEFINE VARIABLE l-Loc AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Para AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Estatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-limite2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-limitemaximo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-disponible AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Secuencia AS INTEGER NO-UNDO.
DEFINE VARIABLE l-Truco AS LOGICAL NO-UNDO.
DEFINE VARIABLE vSaldoV30 AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldoV40 AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldoVenc AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-NomVend LIKE Usuario.Nom-Usuario NO-UNDO.
DEFINE VARIABLE v-SaldoPed AS DECIMAL NO-UNDO.
DEFINE VARIABLE vPedSaldo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-DC AS INTEGER NO-UNDO.
DEFINE VARIABLE v-PromPago AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-FecDep AS DATE NO-UNDO.
DEFINE VARIABLE l-Acuse AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ImpPago AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-SumaDoc AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotChDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotFac AS DECIMAL NO-UNDO.
DEFINE VARIABLE vEfecto AS DECIMAL NO-UNDO.
DEFINE VARIABLE vChNoDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotPed AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Importe AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-Asunto AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Contenido AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Pedido AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Hora AS INTEGER NO-UNDO.

DEFINE BUFFER bfPedido FOR Pedido.
DEFINE BUFFER bf2Pedido FOR Pedido.
DEFINE BUFFER b-MovCliente FOR MovCliente.
DEFINE BUFFER b-HistMovCte FOR HistMovCte.

/* Log de inicio de ejecución */
LOG-MANAGER:WRITE-MESSAGE(
    SUBSTITUTE("/PedidosEmpMes [PUT] >>> Inicia autorización de pedido | Pedido: &1 | Usuario: &2 | Segunda: &3 | Todos: &4 | Importe: &5 | FechaHora: &6",
        STRING(ipPedido),
        ipUsuario,
        IF ipSegunda THEN "TRUE" ELSE "FALSE",
        IF ipTodos THEN "TRUE" ELSE "FALSE",
        STRING(ipImporte),
        STRING(NOW))
).


FIND FIRST Pedido WHERE Pedido.Id-Pedido = ipPedido NO-LOCK NO-ERROR NO-WAIT.
IF NOT AVAILABLE Pedido THEN DO:
    ASSIGN opError = "Folio de pedido inexistente".
    RETURN.
END.

IF Pedido.SolCancPed THEN DO:
    ASSIGN opError = "Folio de pedido con solicitud de cancelacion".
    RETURN.
END.     

FIND FIRST BlkAut WHERE BlkAut.Id-Cliente = Pedido.Id-Cliente NO-LOCK NO-ERROR.
IF AVAILABLE BlkAut THEN DO:
    ASSIGN opError = "Actualmente el Cliente " +
                     STRING(Pedido.Id-Cliente) +
                     " " +
                     Pedido.RazonSocial + 
                     " se encuentra bloqueado para autorizaciones. " +
                     "No es posible realizar la autorizacion de su pedido".
    RETURN.
END. 

FIND FIRST blkRFC WHERE REPLACE(blkRFC.RFC," ","") = REPLACE(Pedido.RFC," ","") NO-LOCK NO-ERROR.
IF AVAILABLE blkRFC THEN DO:
    ASSIGN opError = "El RFC " +
                    Pedido.RFC +
                    " del Cliente " +
                    STRING(Pedido.Id-Cliente) + 
                    " " + 
                    Pedido.RazonSocial +
                    " se encuentra bloqueado. " +
                    "No es posible realizar la autorizacion del pedido".
    RETURN.
END.

FIND Cliente OF Pedido NO-LOCK NO-ERROR.

IF Pedido.EnFirme = FALSE AND
   Pedido.Cancelado = FALSE AND
   INTEGER(SUBSTRING(Pedido.Id-Pedido,1,1)) < 5 THEN DO:
    
    IF ipSegunda = FALSE THEN DO:
        FOR EACH bfPedido WHERE bfPedido.Id-Pedido <> Pedido.Id-Pedido
                            AND INTEGER(SUBSTRING(Pedido.Id-Pedido,1,1)) < 5
                            AND bfPedido.Id-Cliente = Pedido.Id-Cliente
                            AND bfPedido.Id-Cliente <> 3
                            AND bfPedido.EnFirme = FALSE
                            AND bfPEdido.Cancelado = FALSE NO-LOCK:
            ACCUMULATE bfPedido.Tot (TOTAL).
            ACCUMULATE 1 (COUNT).
        END.
        IF (ACCUM COUNT 1) > 1 THEN DO:
            ASSIGN opError = "001.- Existen" + 
                             STRING((ACCUM COUNT 1)) + 
                             " pedidos del Cliente " + 
                             STRING(Pedido.Id-Cliente) + 
                             " pendientes de autorizar por un monto total de $" +
                             STRING((ACCUM TOTAL bfPedido.Tot)) + 
                             ". Desea autorizar todos los pedidos pendientes de este Cliente?".
            RETURN.
        END.
    END.
    
    RUN Autoriza(INPUT ipPedido,INPUT ipSegunda,INPUT ipTodos,INPUT ipUsuario, INPUT ipImporte,OUTPUT opError).
END.    
RELEASE Pedido.     
        
LOG-MANAGER:WRITE-MESSAGE(
    SUBSTITUTE("PedidosEmpMes [PUT] >>> Finaliza autorización de pedido | Usuario: &1", ipUsuario)
).
 
RETURN.
END PROCEDURE.     

PROCEDURE Autoriza.
DEFINE INPUT PARAMETER ipPedido LIKE Pedido.Id-Pedido NO-UNDO.
DEFINE INPUT PARAMETER ipSegunda AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipTodos AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipUsuario AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipImporte AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opError AS CHARACTER NO-UNDO.

    DO TRANSACTION:
        FOR EACH bfPedido WHERE (IF ipTodos = TRUE 
                                 THEN bfPedido.Id-Cliente = Pedido.Id-Cliente
                                 ELSE bfPedido.Id-Pedido = Pedido.Id-Pedido)
                            AND bfPedido.EnFirme = FALSE NO-LOCK:
                                    
            ASSIGN 
               vSaldoV30 = 0
               vSaldoV40 = 0
               vSaldoVenc = 0
               vSaldo = 0.
            FOR EACH MovCliente WHERE MovCliente.id-cliente = bfPedido.id-cliente
                                  AND movcliente.id-mc <= 3 NO-LOCK:
                IF MovCliente.FecVenc < TODAY THEN DO:
                    ASSIGN vSaldoVenc = vSaldovenc + movcliente.saldo.
                    IF movcliente.fecvenc < (TODAY - 30) THEN            
                        ASSIGN vSaldov30 = vSaldov30 + movcliente.saldo.
                END.
                IF movcliente.importe = movcliente.saldo AND
                   TODAY - movcliente.fecreg > 40 THEN            
                    ASSIGN vSaldov40 = vSaldov40 + movcliente.saldo.
                ASSIGN vSaldo = vSaldo + movcliente.saldo.
            END. /* del for each movcliente */
            
            RELEASE Ciudad.
            RELEASE Estado.
            FIND Cliente WHERE Cliente.Id-Cliente = bfPedido.Id-Cliente NO-LOCK NO-ERROR.
            FIND Ciudad WHERE Ciudad.Id-Ciudad = Cliente.Id-Ciudad NO-LOCK NO-ERROR.
            IF AVAILABLE Ciudad THEN
                FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.

            ASSIGN
                v-limite2      = Cliente.limite
                v-limitemaximo = (Cliente.limite * 1.5)
                v-disponible   = v-limite2 - vSaldo.
            
            ASSIGN vFolio = ''.

            FIND bf2Pedido WHERE ROWID(bf2Pedido) = ROWID(bfPedido) EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN bf2Pedido.EnFirme = TRUE
                   bf2Pedido.AutPor  = ipUsuario
                   bf2Pedido.CveAut  = 9999999
                   bf2Pedido.FecReg  = TODAY
                   bf2Pedido.FFinAut = TODAY
                   bf2Pedido.HFinAut = TIME.
            RELEASE bf2Pedido.
     
            IF SUBSTRING(bfPedido.Id-Pedido,1,1) = '0' THEN DO:
                IF bfPedido.Id-Alm <> '11' THEN DO:
                    FIND Vendedor WHERE Vendedor.Id-vend = bfPedido.Id-vend NO-LOCK NO-ERROR.
                    IF AVAILABLE Vendedor THEN DO:
                        IF Vendedor.Id-Vendedor <> "0104" THEN
                            FIND FIRST Usuario WHERE Usuario.Id-user = Vendedor.iniciales NO-LOCK NO-ERROR.
                        ELSE
                            FIND FIRST Usuario WHERE Usuario.Id-user = Vendedor.iniciales NO-LOCK NO-ERROR.
                    END.  
                    
                    IF AVAILABLE Usuario THEN DO:    
                        IF bfPedido.Id-Alm BEGINS '1' THEN DO:
                            RUN programas/vtad1040.p(OUTPUT vFolio,
                                                     INPUT bfPedido.Id-Cliente,
                                                     INPUT bfPedido.Id-UbiVta,
                                                     INPUT bfPedido.Id-Vendedor,
                                                     INPUT TRUE,
                                                     INPUT bfPedido.Enviar,
                                                     INPUT bfPedido.Id-Pedido).  
                        END.
                        ELSE DO:
                            RUN programas/vtad0040.p(OUTPUT vFolio,
                                                     INPUT bfPedido.Id-Cliente,
                                                     INPUT bfPedido.Id-UbiVta,
                                                     INPUT bfPedido.Id-Vendedor,
                                                     INPUT TRUE,
                                                     INPUT bfPedido.Enviar,
                                                     INPUT bfPedido.Id-Pedido).       
                        END.  
                    END.
                END.
                ELSE DO:
                    RUN programas/vtad0040.p(OUTPUT vFolio,
                                                     INPUT bfPedido.Id-Cliente,
                                                     INPUT bfPedido.Id-UbiVta,
                                                     INPUT bfPedido.Id-Vendedor,
                                                     INPUT TRUE,
                                                     INPUT bfPedido.Enviar,
                                                     INPUT bfPedido.Id-Pedido).   
                END.
     
                FOR EACH BtcBajDetPed WHERE BtcBajDetPed.Id-Pedido = bfPedido.Id-Pedido EXCLUSIVE-LOCK:
                    ASSIGN BtcBajDetPed.Id-Pedido = vFolio.
                END.
                FOR EACH DetPedido WHERE DetPedido.Id-Pedido = bfPedido.Id-Pedido
                                     AND DetPedido.Resto = bfPedido.resto EXCLUSIVE-LOCK:
                    ASSIGN DetPedido.Id-Pedido = vFolio.
             
                    IF DetPedido.Tipo = 1 AND DetPedido.CantCom = 0 THEN DO:
                        {vtaa0001.new
                            &Articulo = DetPedido.Id-Articulo
                            &Color    = DetPedido.Id-Color
                            &Pres     = DetPedido.Id-Pres
                            &Cant     = DetPedido.CantPed
                            &bfPedido = bfPedido.Id-Pedido
                            &UbiVta   = bfPedido.Id-UbiVta
                            &Seq      = DetPedido.Id-Seq
                            &Costo    = DetPedido.Costo
                            &Prov     = DetPedido.Id-Prov
                        }
                        IF DetPedido.Id-Loc = '' THEN DO:
                            {bodd0030.i}   
                        END.   
                    END.
                END.
                RELEASE DetPedido.
                FIND EstPedido WHERE EstPedido.Id-Pedido = bfPedido.Id-Pedido
                                 AND EstPedido.Id-Seq = bfPedido.Resto EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE EstPedido THEN ASSIGN EstPedido.Id-Pedido = vFolio.
                RELEASE EstPedido.
                FOR EACH btcautprec WHERE btcautprec.Id-factura = bfPedido.Id-Pedido EXCLUSIVE-LOCK: /*actualiza folio en la bitacora*/
                    ASSIGN btcautprec.Id-factura = vFolio.
                END.     
                FIND FIRST PedFolio WHERE PedFolio.Id-PreFolio = bfPedido.Id-Pedido EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE PedFolio THEN DO:
                    CREATE PedFolio.
                    ASSIGN PedFolio.Id-PreFolio = bfPedido.Id-Pedido.
                END.
                ASSIGN PedFolio.Id-Pedido = vFolio
                       PedFolio.Resto     = bfPedido.Resto
                       PedFolio.FecReg    = TODAY
                       PedFolio.HorReg    = TIME
                       PedFolio.Id-User   = ipUsuario.
                RELEASE PedFolio.
             
                FIND bf2Pedido WHERE ROWID(bf2Pedido) = ROWID(bfPedido) EXCLUSIVE-LOCK NO-ERROR.
                ASSIGN bf2Pedido.Id-Pedido = vFolio.  /* Aqui agrega el folio como id-Pedido */
                RELEASE bf2Pedido.
                 
                IF bfPedido.Filler-1 MATCHES '*CHPF*' THEN DO:
                    SUBSTRING(bfPedido.Filler-1,INDEX(bfPedido.Filler-1,"*CHPF*"),6) = "".
                    FOR EACH ChequePF WHERE ChequePF.Id-Cliente = bfPedido.Id-Cliente
                                        AND ChequePF.Dep = FALSE NO-LOCK:
                        ACCUMULATE 1 (COUNT).
                        ACCUMULATE ChequePF.Importe (TOTAL).
                    END.
                    IF ((ACCUM COUNT 1) + 1 > Cliente.CantCheque) OR
                       ((ACCUMU TOTAL ChequePF.Importe) + (bfPedido.SubTot * (1 + (bfPedido.PorcIVA / 100))) > Cliente.LimCheque) THEN DO:
                        MESSAGE
                            "NO se movieron los limites de importe o cantidad en los cheques posfechados del Cliente,"
                            "por lo tanto, se aceptara pago unicamente con cheque normal para este Pedido"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    END.
                END.
            END.
            
            IF bfPedido.EnFirme THEN DO:
                RUN programas/vtaa0116.p(RECID(bfPedido),FALSE). 
                RUN /usr2/adosa/procs/embd0010.p (INPUT 1,
                                                  INPUT bfPedido.Id-Pedido,
                                                  INPUT bfPedido.Resto).
                IF bfPedido.Enfirme = TRUE AND
                   (bfPedido.Id-Alm = '02B' OR bfPedido.Id-Alm = '11' OR bfPedido.Id-Alm = '12' OR bfPedido.Envia = TRUE) AND
                   bfPedido.Adelantado = FALSE AND bfPedido.Id-Tran <> 98 THEN DO:
                    IF CAN-DO("02B",bfPedido.Id-Alm) THEN /*QUITAR ESTA CONDICION CUANDO ESTE LISTO 11*/
                     //   RUN /usr2/adosa/objpacific/bodd1020 (bfPedido.Id-Pedido,bfPedido.Resto). /* genera tareas */ 
                          RUN programas/bodd1020.p(bfPedido.Id-Pedido,bfPedido.Resto,ipUsuario). /* genera tareas */
                    ELSE
                          RUN programas/bodd1120.p(bfPedido.Id-Pedido,bfPedido.Resto,ipUsuario). /* genera tareas sucursales*/
                    RUN /usr2/adosa/objpacific/bodd1070 (bfPedido.Id-Pedido,bfPedido.Resto). /* examina areas */ 
                  //  RUN programas/bodd1070.p(bfPedido.Id-Pedido,bfPedido.Resto). /* examina areas */
                END.
                RUN /usr2/adosa/objpacific/bodd1071 (bfPedido.Id-Pedido,bfPedido.Resto). /* examina cortes */
            //    RUN programas/bodd1071.p(bfPedido.Id-Pedido,bfPedido.Resto). /* examina cortes */
            END.          
                  
            RUN programas/vtac0800.p(INPUT bfPedido.Id-Pedido,    
                                     INPUT bfPedido.Resto) NO-ERROR.                                     
            
            IF bfPedido.Id-Alm = "02B" THEN                                           
                RUN /usr2/adosa/akubica/vtac0200cc.p(INPUT bfPedido.Id-Pedido,  
                                                     INPUT bfPedido.Resto,
                                                     INPUT FALSE,
                                                     INPUT-OUTPUT l-Truco,
                                                     INPUT FALSE).   
            ELSE
                RUN /usr2/adosa/akubica/vtac0200cc.p(INPUT bfPedido.Id-Pedido,     
                                                     INPUT bfPedido.Resto,  
                                                     INPUT TRUE,
                                                     INPUT-OUTPUT l-Truco,
                                                     INPUT FALSE).    
                   
            // 2021-10-07 - Genera Requisiciones Automaticas
            IF bfPedido.BckOrd = 3 AND bfPedido.Id-Alm = "02B" THEN 
                RUN /usr2/adosa/procs/inva0156.p (INPUT bfPedido.Id-Pedido, 
                                                  INPUT bfPedido.Resto,
                                                  INPUT 1,
                                                  OUTPUT l-estatus).     
            
            /* Envia correos */
            FIND vendedor WHERE vendedor.id-vend = bfPedido.id-vend NO-LOCK NO-ERROR.
            IF AVAILABLE vendedor THEN
                FIND FIRST usuario WHERE usuario.id-user = vendedor.iniciales NO-LOCK NO-ERROR.
            IF AVAILABLE usuario THEN
                v-nomvend = usuario.nom-usuario.
            
            ASSIGN vPedSaldo = 0.   
            FOR EACH bf2Pedido WHERE bf2Pedido.Id-Cliente = bfPedido.Id-Cliente
                                 AND bf2Pedido.Id-Estatus >= -1
                                 AND bf2Pedido.Id-Estatus < 5
                                 AND Pedido.EnFirme = TRUE NO-LOCK:
                ASSIGN 
                     vSaldo = vSaldo + Pedido.Subtotal
                     vPedSaldo = vPedSaldo + Pedido.Subtotal.       
            
            END. /* del pedido */  
            
            
            ASSIGN
                v-saldoped = IF vSaldo > 0 THEN vSaldo - vPedSaldo ELSE 0.
            
            FIND FIRST usuario WHERE usuario.id-user = ipUsuario NO-LOCK NO-ERROR.
            
            RUN cxcb0270.p(INPUT bfPedido.Id-Cliente,
                                             INPUT TODAY,
                                             OUTPUT v-dc,
                                             OUTPUT v-prompago).
            IF v-dc = ? THEN
                v-dc = 0.
            IF v-prompago = ? THEN  
                v-prompago = 0.
            
            l-fecdep = ?.
            l-acuse = "".
            l-ImpPago = 0.
            FOR EACH acuse WHERE acuse.id-cliente = cliente.id-cliente NO-LOCK:
               IF (l-fecdep = ? OR acuse.fecdep > l-fecdep) AND 
                  acuse.estatus <> 3 THEN DO:
                  l-SumaDoc = 0.
                  IF Acuse.Tipo <> "A" THEN DO:
                     FOR EACH docacuse WHERE docacuse.id-acuse = acuse.id-acuse NO-LOCK:
                         l-SumaDoc = l-SumaDoc + docacuse.imppago.
                     END.
                  END.
                  ELSE DO:
                     FOR EACH PagoAcuse WHERE PagoAcuse.Id-acuse = Acuse.id-acuse 
                             NO-LOCK:
                         l-SumaDoc = l-SumaDoc + PagoAcuse.Importe.
                     END.
                  END.
                  IF l-SumaDoc > 0 THEN  
                     ASSIGN l-acuse = acuse.id-acuse        
                            l-fecdep = acuse.fecdep
                            l-ImpPago = l-SumaDoc.
               END.
            END.
            FOR EACH DetAnticipo WHERE (DetAnticipo.FecReg >= l-fecdep OR l-fecdep = ?) NO-LOCK,
                EACH Anticipo WHERE Anticipo.Id-Anticipo = DetAnticipo.Id-Anticipo
                                AND Anticipo.Id-Cliente = 3 NO-LOCK,
                EACH Acuse WHERE Acuse.Id-Acuse = Anticipo.Id-Acuse NO-LOCK,
                EACH b-MovCliente WHERE b-MovCliente.RefSaldo = DetAnticipo.Documento
                                    AND b-MovCliente.Id-Mc = DetAnticipo.Id-MC
                                    AND b-MovCliente.Id-Cliente = Cliente.Id-Cliente 
                                    NO-LOCK BREAK BY Acuse.Id-Acuse:
                IF FIRST-OF(Acuse.Id-Acuse) THEN 
                      l-SumaDoc = 0.
                l-SumaDoc = l-SumaDoc + DetAnticipo.Importe.
                IF LAST-OF(Acuse.Id-Acuse) THEN DO:
                  IF l-SumaDoc > 0 AND (l-fecdep = ? OR acuse.fecdep > l-fecdep) THEN 
                     ASSIGN l-acuse = acuse.id-acuse
                            l-fecdep = acuse.fecdep
                            l-ImpPago = l-SumaDoc.
                END.
                 
            END.
            
            FOR EACH DetAnticipo WHERE (DetAnticipo.FecReg >= l-fecdep OR l-fecdep = ?) NO-LOCK,
                EACH Anticipo WHERE Anticipo.Id-Anticipo = DetAnticipo.Id-Anticipo
                                AND Anticipo.Id-Cliente = 3 NO-LOCK,
                EACH Acuse WHERE Acuse.Id-Acuse = Anticipo.Id-Acuse NO-LOCK,
                EACH b-HistMovCte WHERE b-HistMovCte.RefSaldo = DetAnticipo.Documento
                                    AND b-HistMovCte.Id-Mc = DetAnticipo.Id-MC
                                    AND b-HistMovCte.Id-Cliente = Cliente.Id-Cliente 
                                    NO-LOCK BREAK BY Acuse.Id-Acuse:
                IF FIRST-OF(Acuse.Id-Acuse) THEN 
                      l-SumaDoc = 0.
                l-SumaDoc = l-SumaDoc + DetAnticipo.Importe.
                IF LAST-OF(Acuse.Id-Acuse) THEN DO:
                  IF l-SumaDoc > 0 AND (l-fecdep = ? OR acuse.fecdep > l-fecdep) THEN 
                     ASSIGN l-acuse = acuse.id-acuse
                            l-fecdep = acuse.fecdep
                            l-ImpPago = l-SumaDoc.
                END.
                 
            END.
            
            /* Calcula resumen de cheques */
            FIND CURRENT bfPedido EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            ASSIGN vTotChDep = 0
                   vtotfac = 0
                   vefecto = 0
                   vchnodep = 0
                   vtotped = 0.
            FOR EACH Acuse WHERE Acuse.Id-cliente = bfPedido.id-cliente
                             AND Acuse.FecDep >= (TODAY - 30)
                             AND Acuse.FecDep <= TODAY
                             AND Acuse.Estatus = 4 NO-LOCK:
                ASSIGN l-Importe = 0.
                FOR EACH Pagoacuse OF acuse NO-LOCK:
                    ASSIGN l-importe = l-importe + pagoacuse.importe.
                END.
                ASSIGN vTotChDep = vTotChDep + l-Importe.
            END.
            FOR EACH DetAnticipo WHERE DetAnticipo.FecReg >= (TODAY - 30) NO-LOCK,
                EACH Anticipo WHERE Anticipo.Id-Anticipo = DetAnticipo.Id-Anticipo
                                AND Anticipo.Id-Cliente = 3 NO-LOCK,
                EACH b-MovCliente WHERE b-MovCliente.RefSaldo = DetAnticipo.Documento
                                    AND b-MovCliente.Id-Mc = DetAnticipo.Id-MC
                                    AND b-MovCliente.Id-Cliente = Cliente.Id-Cliente 
                                    NO-LOCK:
                ASSIGN vTotChDep = vTotChDep + DetAnticipo.Importe.
            END.
            
            FOR EACH Acuse WHERE Acuse.Id-cliente = bfPedido.id-cliente
                             AND Acuse.Estatus < 3 NO-LOCK:
                ASSIGN l-Importe = 0.
                FOR EACH Pagoacuse OF acuse NO-LOCK:
                    ASSIGN l-importe = l-importe + pagoacuse.importe.
                END.
                ASSIGN vChNoDep = vChNoDep + l-Importe.
            END.
            FOR EACH CheDev WHERE CheDev.Id-Cliente = bfPedido.id-cliente
                              AND CheDev.FecCargo >= (TODAY - 30)
                              AND CheDev.FecCargo <= TODAY
                              AND CheDev.FecCanc = ? NO-LOCK BY CheDev.FecCargo:
                ASSIGN vTotChDep = vTotChDep - CheDev.ImpCheque.
            END.
            FOR EACH factura WHERE factura.id-cliente = bfPedido.id-cliente
                               AND factura.fecreg >= (TODAY - 30)
                               AND factura.fecreg <= TODAY
                               AND factura.feccanc = ? NO-LOCK:
                ASSIGN vTotFac = vTotFac + Factura.Tot.
            END.
            
            FOR EACH estpedido WHERE estpedido.id-cliente = bfPedido.id-cliente
                                 AND estpedido.estatus < 5 NO-LOCK,
                EACH Detpedido WHERE detpedido.id-pedido = estpedido.id-pedido
                                 AND detpedido.resto = estpedido.id-seq NO-LOCK:
                ACCUMULATE vTotPed = vTotPed + DetPedido.Importe + DetPedido.Iva.
            END.
            
            ASSIGN vEfecto = (vTotFac + vTotPed) - vTotChDep.
            /**/
            
            ASSIGN
                 v-asunto    = "AVISO AUTOMATICO DE AUTORIZACION DE CREDITO (cxcd1662-M)"
                 v-contenido = "<html><head>" + CHR(13) +
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) +
                               "style='font-size:10.0pt;font-family:Verdana'>" + 'Se realizo una autorizacion del pedido Num: ' + v-pedido + ', por ' + usuario.nom-usuario + '.' + "<o:p></o:p></span></p>" + CHR(13) +
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + 'Fecha y hora.....:   ' + STRING(TODAY,"99/99/9999") + ' ' + STRING(v-hora,"hh:mm:ss")  + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +     
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Cliente..........:   "  + STRING(bfPedido.id-cliente) + " " + (IF AVAILABLE cliente THEN cliente.razonsocial ELSE "No disponible") + "<o:p></o:p></span></p>" + CHR(13) +
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Poblacion........:   " + (IF AVAILABLE Ciudad THEN TRIM(ciudad.nombre) ELSE "") + ", " + (IF AVAILABLE estado THEN TRIM(estado.nombre) ELSE "") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + 
                               "Motivo...........:   " + (IF ipImporte > 50000
                                                          THEN "COMPRA MAYOR A $50,000 "
                                                          ELSE "") +
                                                         (IF ((ipImporte + vSaldo) >= v-limitemaximo)
                                                          THEN "CLIENTE EXCEDE MAS DE UN 50% SU LIMITE DE CREDITO "
                                                          ELSE "") +
                                                         (IF vSaldov30 > 0 THEN "CLIENTE CON MAS DE 30 DIAS VENCIDO"
                                                          ELSE "") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" +                                                    
                               "Plazo............:   " + STRING(cliente.plazo) + " dias"                               + CHR(13) + 
                               "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + 
                               "Solicita.........:  $" + TRIM(STRING(ipImporte,"-ZZZ,ZZZ,ZZ9.99"))                + CHR(13) +
                               "<o:p></o:p></span></p>" + CHR(13) + 
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" +                                              
                               "Ultimo Pago......:  $" + TRIM(STRING(l-ImpPago,"-ZZZ,ZZZ,ZZ9.99"))                     + "  " + 
                                                         (IF l-FecDep <> ? THEN STRING(l-FecDep,"99/99/9999") ELSE "") + "  " +
                                                         l-Acuse                                                       + CHR(13) + CHR(13) + 
                               "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" +
                               "Resumen 30 dias"                                                                       + CHR(13) +
                               "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Total Ch. Depos..:  $" + STRING(vTotChDep,"-ZZZ,ZZZ,ZZ9.99") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Total de Factura.:  $" + STRING(vTotFac,"-ZZZ,ZZZ,ZZ9.99")   + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                        
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Pedidos Pend.....:  $" + STRING(vTotPed,"-ZZZ,ZZZ,ZZ9.99")   + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                        
            
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Efecto en cartera:  $" + STRING(vEfecto,"-ZZZ,ZZZ,ZZ9.99")   + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                       
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Ch NO Depositados:  $" + STRING(vChNoDep,"-ZZZ,ZZZ,ZZ9.99")  + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Saldo Act........:  $" + STRING(vSaldo,"-ZZZ,ZZZ,ZZ9.99")    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) + 
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + " * Con Factura...:  $" + STRING(v-saldoped,"-ZZZ,ZZZ,ZZ9.99")      + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "       * Venc -30:  $" + STRING(vSaldovenc - vSaldov30,"-ZZZ,ZZZ,ZZ9.99") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                        
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "       * Venc +30:  $" + STRING(vSaldov30,"-ZZZ,ZZZ,ZZ9.99")                    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "       * P/Ven...:  $" + STRING(v-saldoped - vSaldovenc,"-ZZZ,ZZZ,ZZ9.99")      + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + " * Por Facturar..:  $" + STRING(vPedSaldo,"-ZZZ,ZZZ,ZZ9.99")                    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Limite Credito...:  $" + STRING(v-limite2,"-ZZZ,ZZZ,ZZ9.99")                          + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                        
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Cred. Disponible.:  $" + STRING(v-disponible,"-ZZZ,ZZZ,ZZ9.99")                       + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Dias Cartera.....:   " + STRING(v-dc,"-ZZZ,ZZZ,ZZ9")                                  + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Promedio Pago....:   " + STRING(v-prompago,"-ZZZ,ZZZ,ZZ9")                            + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Factor Descto....:   " + STRING(cliente.FactorDes,"-ZZZ,ZZZ,ZZ9.99 %")                + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Vendedor.........:   " + STRING(v-nomvend,"X(20)")                                    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) + CHR(13) +
                                                       
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "(cxcd1662.i)"          + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                                                                             
                               
                               '<meta http-equiv="Content-Type" content="text/html; charset=windows-1252">' + CHR(13) +
                               "</head>".  
                               
            IF {sist0001.i} = "DESARROLLO" THEN 
                ASSIGN v-Para = "desarrollo10@adosa.com.mx".
            ELSE DO:
                v-Para = (IF ipImporte >= 5000 AND vEfecto >= 3000 THEN 'agomez@adosa.com.mx; ogomez@adosa.com.mx; ' ELSE '') +
                         'dgarza@adosa.com.mx; jgonzalez@adosa.com.mx' +
                         (IF Pedido.Id-Alm = '11' THEN '; jlopez@adosa.com.mx' ELSE '') +
                         (IF Pedido.Id-Alm = '12' THEN '; hterrazas@adosa.com.mx; jge@adosa.com.mx' ELSE '').
            END.

                                 
            {inva0007.i
                &Asunto     = "v-asunto"
                &contenido  = "v-contenido"
                &Iniciales  = "'SIS10'"
                &Direccion  = "v-Para"
                &Refer      = "'DIRECTO'"
                &Attachment = ""
            }
                
              
        END.
         
    END.
    RELEASE Folio.     
    RELEASE Pedido.
    RETURN.
END PROCEDURE.      



@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE RechazaPedido:   
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*
    Rechaza pedidos desde .Net   
*/   

DEFINE INPUT PARAMETER ipPedido AS CHARACTER NO-UNDO.   
DEFINE INPUT PARAMETER ipUser AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opError AS CHARACTER NO-UNDO.
DEFINE BUFFER b-Usuario FOR Usuario.

/* Log de inicio */
LOG-MANAGER:WRITE-MESSAGE(
    SUBSTITUTE("/PedidosEmpMes [DELETE] >>> Inicia rechazo de pedido | Pedido: &1 | Usuario: &2 | FechaHora: &3",
        ipPedido,
        ipUser,
        STRING(NOW))
).


FIND FIRST Pedido WHERE Pedido.Id-Pedido = ipPedido EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE Pedido THEN DO:
    IF Pedido.Id-Liga BEGINS "F" THEN DO:
        ASSIGN opError = "El pedido " +
                         Pedido.Id-Pedido +
                         " se encuentra ligado en facturacion a otro(s) pedido(s), favor de eliminar la liga antes de cancelarlo".
        RETURN.
    END.
    IF Pedido.EnFirme = FALSE AND Pedido.Cancelado = FALSE AND INTEGER(SUBSTRING(Pedido.Id-Pedido,1,1)) < 5 THEN DO:
        DO TRANSACTION:
            FOR EACH BtcBajDetPed WHERE BtcBajDetPed.Id-Pedido = Pedido.Id-Pedido EXCLUSIVE-LOCK:
                DELETE BtcBajDetPed.
            END.

            FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = Pedido.Id-Vendedor NO-LOCK NO-ERROR.
            IF AVAILABLE Vendedor THEN DO:
                FIND FIRST Usuario WHERE Usuario.Id-User = Vendedor.Iniciales NO-LOCK NO-ERROR.
                IF AVAILABLE Usuario THEN DO:
                    FIND Cliente OF Pedido NO-LOCK NO-ERROR.
                    FIND b-Usuario WHERE b-usuario.Id-User = ipUser NO-LOCK NO-ERROR.
                    {inva0007.i
                        &Asunto     = "'Se le ha cancelado un pedido en creditos'"
                        &Contenido  = "'Pedido.......: ' + Pedido.Id-Pedido + CHR(10) +
                                       'Vendedor.....: ' + Usuario.Nom-Usuario + CHR(10) +
                                       'No. de Cuenta: ' + STRING(Pedido.Id-Cliente) + CHR(10) +
                                       'Razon Social.: ' + (IF AVAILABLE Cliente THEN Cliente.RazonSocial ELSE '') + CHR(10) +
                                       'Subtotal.....: ' + STRING(Pedido.SubTotal,'Z,ZZZ,ZZ9.99') + CHR(10) +
                                       'Fecha Pedido.: ' + STRING(Pedido.FecReg,'99/99/9999') + CHR(10) +
                                       'Hora Pedido..: ' + STRING(Pedido.HrSta,'HH:MM') + CHR(10) + CHR(10) +
                                       'Cancelado por: ' + b-Usuario.Nom-Usuario + CHR(10) +
                                       'Fecha Canc...: ' + STRING(TODAY,'99/99/9999') + CHR(10) +
                                       'Hora Canc....: ' + STRING(TIME,'HH:MM')"
                        &Iniciales  = ipUser
                        &Direccion  = "Usuario.e-Mail + ',mluna@adosa.com.mx'" 
                    }
                END.
            END.

            ASSIGN Pedido.Cancelado   = TRUE
                   Pedido.FecCancel   = TODAY
                   Pedido.UsuarioCanc = ipUser.
            IF Pedido.Id-Fuente <> 0 THEN DO:
                ASSIGN Pedido.FuenteSta = 1
                       Pedido.FuenteFec = TODAY
                       Pedido.FuenteHor = TIME.
            END.

            FOR EACH DetPedido WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido
                                 AND DetPedido.Resto = Pedido.Resto EXCLUSIVE-LOCK:
                IF Pedido.Id-Fuente = 1 THEN DO:
                    OUTPUT TO VALUE("/usr2/compartido/PA_" + TRIM(Pedido.OrdComCte) +
                                    "_" + TRIM(ENTRY(1,DetPedido.Filler-1)) + ".txt").
                    DISPLAY TRIM(Pedido.OrdComCte) + "|" + 
                            TRIM(ENTRY(1,DetPedido.Filler-1)) + "|1|" + 
                            STRING(YEAR(Pedido.FuenteFec),"9999") +
                            STRING(MONTH(Pedido.FuenteFec),"99") +
                            STRING(DAY(Pedido.FuenteFec),"99") + "|" +
                            "NO ACEPTADO POR DEPARTAMENTO DE CREDITOS" + "|" FORMAT "x(100)"
                    WITH WIDTH 120.
                    OUTPUT CLOSE.
                END.

                CREATE CancDetPed.
                BUFFER-COPY DetPedido TO CancDetPed.
                IF DetPedido.Tipo = 1 AND DetPedido.CantCom <> 0 THEN DO:
                    {vtaa0006.i}.
                END.
                DELETE DetPedido.
            END.
            RELEASE DetPedido.

            CREATE CancPed.
            BUFFER-COPY Pedido TO CancPed.
            ASSIGN CancPed.FecCancel = TODAY.
            
            CREATE LogBaja.
            ASSIGN LogBaja.Id-Pedido = Pedido.Id-Pedido
                   LogBaja.FecBaja = TODAY
                   LogBaja.Hora = TIME
                   LogBaja.Id-User = ipUser
                   LogBaja.Donde = ".Net".
            
            RUN /usr2/adosa/procs/embd0010.p(INPUT 2,
                                             INPUT Pedido.Id-Pedido,
                                             INPUT Pedido.Resto).
            
            DELETE Pedido.
            RELEASE Pedido.
        END.
    END.
    ELSE DO:
        ASSIGN opError = "El Pedido ya ha sido Cancelado".
        RETURN.
    END.
END.
ELSE DO:
    BELL.
    IF LOCKED(Pedido) THEN DO:
        ASSIGN opError = "Pedido Bloqueado por Otro Usuario".
        RETURN.
    END.
    ELSE DO:
        ASSIGN opError = "Pedido a sido borrado de la base de datos".
        RETURN.
    END.               
END. 
/* Log de inicio */
LOG-MANAGER:WRITE-MESSAGE(
    SUBSTITUTE("/PedidosEmpMes [DELETE] >>> Finaliza rechazo de pedido | Pedido: &1 | Usuario: &2 | FechaHora: &3",
        ipPedido,
        ipUser,  
        STRING(NOW))
).  
RETURN.             
END PROCEDURE.