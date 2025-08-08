@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : detallepedido.p
    Purpose     : 

    Syntax      : En el menu de Movimientos de Clientes
                  se colocara el Inf.Saldos que esta en pantalla N
                  se envia el Numero de Cliente y muestra saldos en transito.

    Description : 

    Author(s)   : sis10
    Created     : Tue Jul 29 08:44:19 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/
/*
    Empresa:    ADOSa
    Programa:   cxcd1660.p
    Funcion:    Autorizacion de pedidos
    Autor:      Alex
    Fecha:      9 de Julio del 2004
    Modificaci�n: RNPC - 2019-03-04 - Agrego columna de moneda en el listado de pedidos.
    
    USAREMOS EL DE INFO-SALDOS
    &MenuOpc8   = "Inf. Saldos"
    &HelpOpc8   = "'Saldo actual y vencido del cliente'"
    &AccionOpc8 = cxcd1660.i
    
    
*/
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttCliente NO-UNDO    
    FIELD IdCliente        LIKE Cliente.Id-Cliente
    FIELD RazonSocial      LIKE Cliente.RazonSocial
    FIELD LimiteCredito    LIKE Cliente.Limite
    FIELD PedidoPendiente  AS INTEGER 
    FIELD Saldo            LIKE MovCliente.Saldo
    FIELD SaldoPedTransito LIKE MovCliente.Saldo
    FIELD SaldoTotal       LIKE MovCliente.Saldo
    FIELD SaldoVencido     LIKE MovCliente.Saldo
    FIELD VencSaldo        AS DECIMAL
    INDEX idx-clase IdCliente ASCENDING.

DEFINE TEMP-TABLE ttDetalle NO-UNDO 
    FIELD IdCliente LIKE MovCliente.Id-Cliente
    FIELD Pedido    LIKE Pedido.Id-Pedido
    FIELD Resto     LIKE Pedido.Resto
    FIELD FechaReg  LIKE Pedido.FecReg 
    FIELD Vendedor  AS CHARACTER
    FIELD Almacen   LIKE Pedido.Id-Alm
    FIELD Estatus   AS CHARACTER
    FIELD Total     LIKE Pedido.Tot
    INDEX Idx-Ped Pedido . 

DEFINE DATASET dsDetPedido FOR 
    ttCliente, /* Tabla principal */
    ttDetalle /* Relación con Cliente */
    DATA-RELATION ClienteDetalle FOR ttCliente, ttDetalle
    RELATION-FIELDS (IdCliente, IdCliente). /* Relación por IdCliente */

DEFINE VARIABLE l-Estatus   AS CHARACTER NO-UNDO FORMAT "X(12)".
DEFINE VARIABLE l-PedPend   AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-saldoVenc AS DECIMAL. 
DEFINE VARIABLE l-saldo     AS DECIMAL.
DEFINE VARIABLE l-saldoTot  AS DECIMAL.
DEFINE VARIABLE l-pedsaldo  AS DECIMAL.
    
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE DetallePedidosCliente:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-Cliente LIKE Cliente.Id-Cliente NO-UNDO.
    DEFINE OUTPUT PARAMETER DATASET FOR dsDetPedido.



    FIND FIRST Cliente WHERE Cliente.Id-Cliente = l-Cliente NO-LOCK NO-ERROR.

    

    ASSIGN
        l-saldoVenc = 0
        l-saldo     = 0
        l-saldoTot  = 0.
    FOR EACH MovCliente WHERE MovCliente.id-cliente = l-Cliente 
        AND movcliente.id-mc <= 3
        AND MovCliente.Saldo > 0 NO-LOCK:
        IF MovCliente.FecVenc < TODAY THEN 
        DO:
            ASSIGN 
                l-saldoVenc = l-saldoVenc + movcliente.saldo.
        END.
        ASSIGN 
            l-saldo = l-saldo + movcliente.saldo.
    END. /* del for each movcliente */
  
    
    
    
    
    
    
    ASSIGN 
        l-PedPend  = 0
        l-saldoTot = l-saldo
        l-pedsaldo = 0.
    FOR EACH Pedido WHERE Pedido.Id-Cliente = l-Cliente
        AND Pedido.Id-Estatus >= -1
        AND Pedido.Id-Estatus < 5 
        AND Pedido.EnFirme = TRUE 
        USE-INDEX idx-cteped NO-LOCK:
        ASSIGN 
            l-PedPend  = l-PedPend + 1
            l-saldoTot = l-saldoTot + (Pedido.Tot)
            l-pedsaldo = l-pedsaldo + (Pedido.Tot).
            
        /* Determinar el Estatus */
        IF Pedido.FecRem <> ? THEN 
        DO:
            ASSIGN 
                l-Estatus = "REMISIONADO".
            FIND EstPedido WHERE EstPedido.Id-Pedido = Pedido.Id-Pedido
                AND EstPedido.Id-Seq = Pedido.Resto NO-LOCK NO-ERROR.
            IF AVAILABLE EstPedido AND EstPedido.FecEmb <> ? THEN
                ASSIGN l-Estatus = "REMIS Y EMB".
        END.
        ELSE 
        DO:
            CASE Pedido.Id-Estatus:
                WHEN -1 THEN 
                    ASSIGN 
                        l-Estatus = "PAUSA SURT".
                WHEN 0  THEN 
                    ASSIGN 
                        l-Estatus = "ORIGINAL".
                WHEN 1  THEN 
                    ASSIGN 
                        l-Estatus = "SURTIENDOSE".
                WHEN 2  THEN 
                    ASSIGN 
                        l-Estatus = "POR EMPACAR".
                WHEN 3  THEN 
                    ASSIGN 
                        l-Estatus = "EMPACANDOSE".
                OTHERWISE 
                ASSIGN 
                    l-Estatus = "POR FACTURAR".
            END CASE.
        END.

        /* Obtener nombre del vendedor */
        FIND Vendedor WHERE Vendedor.Id-Vendedor = Pedido.Id-Vendedor NO-LOCK NO-ERROR.
        FIND Empleado WHERE Empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.

        CREATE ttDetalle.
        ASSIGN
            ttDetalle.IdCliente = Pedido.Id-Cliente
            ttDetalle.Pedido    = Pedido.Id-Pedido
            ttDetalle.Resto     = Pedido.Resto
            ttDetalle.FechaReg  = Pedido.FecReg
            ttDetalle.Vendedor  = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE ""
            ttDetalle.Almacen   = Pedido.Id-Alm
            ttDetalle.Estatus   = l-Estatus
            ttDetalle.Total     = Pedido.Tot.
    END.
    
    FIND FIRST ttCliente WHERE ttCliente.IdCliente = l-Cliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttCliente THEN 
    DO:
      
        CREATE ttCliente.
        ASSIGN 
            ttCliente.IdCliente        = l-cliente
            ttCliente.RazonSocial      = Cliente.RazonSocial
            ttCliente.LimiteCredito    = Cliente.Limite
            ttCliente.PedidoPendiente  = l-PedPend
            ttCliente.Saldo            = l-saldo
            ttCliente.SaldoPedTransito = l-pedsaldo
            ttCliente.SaldoTotal       = l-saldoTot
            ttCliente.SaldoVencido     = l-saldoVenc
            ttCliente.VencSaldo        = ROUND((l-saldoVenc / l-saldo) * 100, 2) .
        
    END. 

    


END PROCEDURE.

