@openapi.openedge.export (type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : notificacion.p   
    Purpose     : 

    Syntax      :/Notificacion

    Description : Servicio para Enviar
                  Si hay pagares Vencidos.
                  Solicitudes de Credito Nuevas.
                  Pedidos / Facturas por autorizar.

    Author(s)   : sis10
    Created     : Mon Aug 11 16:54:01 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
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
    
 
   
DEFINE TEMP-TABLE ttNotificacion NO-UNDO
    FIELD NumPagare     AS INTEGER
    FIELD NumSolicitud  AS INTEGER
    FIELD NumPedido     AS INTEGER
    FIELD NumFactura    AS INTEGER.

/* **********************  Internal Procedures  *********************** */
DEFINE BUFFER bfPagare FOR Pagare.
DEFINE BUFFER bfttFactura FOR ttFactura.
DEFINE BUFFER bfAutPend FOR AutPend.
         
DEFINE VARIABLE vTotalFacturas AS INTEGER NO-UNDO.
DEFINE VARIABLE vTotalPedidos AS INTEGER NO-UNDO.
DEFINE VARIABLE vTotalPagares AS INTEGER NO-UNDO.
DEFINE VARIABLE vTotalSolicitudes AS INTEGER NO-UNDO.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE BuscaNoti:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipClaseCte      AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttNotificacion.
    
EMPTY TEMP-TABLE ttNotificacion NO-ERROR.
EMPTY TEMP-TABLE ttFactura      NO-ERROR.


/* Validación: ipClaseCte obligatorio */
    IF ipClaseCte = 0 OR ipClaseCte = ? THEN DO:
        MESSAGE "El parámetro Clase es obligatorio." VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

/* ---------------- FACTURAS ---------------- */
    LOG-MANAGER:WRITE-MESSAGE("Cargando facturas pendientes...").
 

FOR EACH AutPend WHERE NOT CAN-FIND(FIRST Autorizacion WHERE Autorizacion.Id-Cliente = AutPend.Id-Cliente
                                                         AND Autorizacion.RecTipo = AutPend.RecAuto) NO-LOCK:   
    CREATE ttFactura.     
    BUFFER-COPY AutPend EXCEPT RecAuto TO ttFactura.
    ASSIGN ttFactura.RecAuto = AutPend.RecAuto.

    FOR EACH bfttFactura WHERE bfttFactura.recauto = ttFactura.recauto NO-LOCK.
        ACCUMULATE 1 (COUNT).      
    END.   
    IF (ACCUM COUNT 1) > 1 THEN DO:
        DELETE ttFactura.
        NEXT.
    END.      
    
    IF ttFactura.Id-Cliente = 0 THEN
        NEXT.
END.     

/* Aquí obtenemos el total de facturas cargadas */
ASSIGN vTotalFacturas = 0.

FOR EACH ttFactura NO-LOCK:
    ASSIGN vTotalFacturas = vTotalFacturas + 1.
END.

/* Si no hay facturas, que sea 0 */
IF vTotalFacturas = 0 THEN
    ASSIGN vTotalFacturas = 0.

/* Creamos notificación con el total */
CREATE ttNotificacion.
ASSIGN ttNotificacion.NumFactura = vTotalFacturas.
LOG-MANAGER:WRITE-MESSAGE("Facturas encontradas: " + STRING(vTotalFacturas)).

/* Carga de pedidos pendientes de autorizar */
ASSIGN vTotalPedidos = 0.

/* Carga de pedidos pendientes de autorizar */
FOR EACH Pedido WHERE Pedido.Id-Pedido BEGINS '0'
                  AND Pedido.EnFirme = FALSE NO-LOCK,
    FIRST Cliente OF Pedido WHERE Cliente.Id-ClaseCte = ipClaseCte NO-LOCK:
    
    IF Pedido.Id-Vendedor = "0100" AND Pedido.Id-Cond = 0 THEN NEXT.
    
     /* Solo incrementamos el contador */
    ASSIGN vTotalPedidos = vTotalPedidos + 1.
END.

/* Si ya existe notificación creada arriba, solo actualizamos */
FIND FIRST ttNotificacion NO-LOCK NO-ERROR.
IF AVAILABLE ttNotificacion THEN
    ASSIGN ttNotificacion.NumPedido = vTotalPedidos.
ELSE DO:
    CREATE ttNotificacion.
    ASSIGN ttNotificacion.NumPedido = vTotalPedidos.
END.
LOG-MANAGER:WRITE-MESSAGE("Pedidos encontrados: " + STRING(vTotalPedidos)).
/* PROCESO PAGARE VENCIDOS */

/* Total pagares vencidos */
/* pagare vencido de la clase del usuario loggeado */ 


ASSIGN vTotalPagares = 0.

FOR EACH Pagare
    WHERE Pagare.FecVenc < TODAY
      AND Pagare.Id-Cliente <> 0
    NO-LOCK,
    FIRST Cliente OF Pagare
    WHERE Cliente.Id-ClaseCte = ipClaseCte
    NO-LOCK:
    vTotalPagares = vTotalPagares + 1.
END.  


/* Actualizar notificación */
FIND FIRST ttNotificacion NO-ERROR.
IF AVAILABLE ttNotificacion THEN
    ttNotificacion.NumPagare = vTotalPagares.
ELSE DO:
    CREATE ttNotificacion.
    ttNotificacion.NumPagare = vTotalPagares.
END.

LOG-MANAGER:WRITE-MESSAGE("Pagares vencidos encontrados: " + STRING(vTotalPagares)).
/* Total solicitudes de crédito pendientes de autorización */
ASSIGN vTotalSolicitudes = 0.
  
FOR EACH SolCred
    WHERE SolCred.IdEstatus = 1
    NO-LOCK:
    vTotalSolicitudes = vTotalSolicitudes + 1.
END.

/* Actualizar notificación */
FIND FIRST ttNotificacion NO-ERROR.
IF AVAILABLE ttNotificacion THEN
    ttNotificacion.NumSolicitud = vTotalSolicitudes.
ELSE DO:
    CREATE ttNotificacion.
    ttNotificacion.NumSolicitud = vTotalSolicitudes.
END.

LOG-MANAGER:WRITE-MESSAGE("Solicitudes de credito pendientes: " + STRING(vTotalSolicitudes)).

  /* Cierre */
    LOG-MANAGER:WRITE-MESSAGE("=== Fin BuscaNoti ===").
RETURN.      
END PROCEDURE.

