@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : PedidoPorAutVisor.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Tue Oct 29 12:21:45 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF    VAR l-ClaseCte     AS INTEGER   NO-UNDO.
DEF    VAR l-NomResp      AS CHAR      NO-UNDO.
DEF    VAR l-NP           AS INTEGER   NO-UNDO.
DEF    VAR l-Imp          AS DECIMAL   NO-UNDO.
DEF    VAR l-NomClase     AS CHARACTER.

DEF    VAR l-num          AS INTEGER   NO-UNDO.
DEF    VAR l-tipo         AS INT.
DEFINE VAR l-pedido       AS CHAR.
DEFINE VAR l-empleado     AS CHAR.
DEFINE VAR l-Vendedor     AS CHAR.
DEFINE VAR l-sucursal     AS CHAR.
DEFINE VAR l-tipo-cliente AS CHAR.

DEF TEMP-TABLE ttPedXAut
    FIELD Num           AS INT
    FIELD ClaseCliente  AS CHAR
    FIELD Cliente       AS INT
    FIELD TipoCliente   AS CHAR
    FIELD RazonSocial   AS CHAR
    FIELD Bloqueo       AS CHAR
    FIELD TipoBloqueo   AS CHAR
    FIELD NumPedido     AS CHAR
    FIELD FechaRegistro AS DATE
    FIELD Hora          AS CHAR
    FIELD ImportePedido AS DECIMAL FORMAT ">>>,>>>,>>9.99" 
    FIELD Sucursal      AS CHAR
    FIELD NomVendedor   AS CHAR
    FIELD NomResp       AS CHAR
    FIELD NomBloqueo    AS CHAR
    FIELD FecBloqueo    AS DATE
    INDEX Idx-Def Num.
DEFINE DATASET dsPedXAut FOR ttPedXAut.   


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetPedPorAutVisor:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttPedXAut.
    l-NomResp = "".
    l-num     = 0.   
    EMPTY TEMP-TABLE ttPedXAut.      
    FOR EACH Pedido WHERE Pedido.Id-Pedido BEGINS '0'
        AND Pedido.EnFirme = FALSE 
        AND Pedido.Id-Vendedor <> "0100" NO-LOCK,
        FIRST Cliente WHERE Cliente.Id-Cliente = Pedido.Id-Cliente 
        NO-LOCK BREAK BY Pedido.Id-Cliente:
        FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
        IF AVAILABLE Resp THEN l-NomResp = Resp.Nombre.
        ASSIGN 
            l-NP       = 0
            l-Imp      = 0
            l-ClaseCte = 1
            l-num      = l-num + 1
            l-ClaseCte = Cliente.Id-ClaseCte. 
        FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
        IF AVAILABLE ClaseCte THEN l-NomClase = ClaseCte.Descr.       
        l-NP = l-NP + 1.
        IF Pedido.Id-Moneda > 1 THEN
            l-Imp = l-Imp + (Pedido.Tot * Pedido.TipoCambio).
        ELSE l-Imp = l-Imp + Pedido.Tot.
        FIND FIRST CondVta WHERE CondVta.Id-Cond = Pedido.Id-Cond NO-LOCK NO-ERROR.   
        FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = Pedido.Id-Vendedor NO-LOCK NO-ERROR.
        IF AVAILABLE Vendedor THEN 
        DO:
            FIND FIRST empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
            IF AVAILABLE empleado THEN l-empleado = empleado.Nombre.
        END.
        FIND FIRST Almacen WHERE Almacen.Id-Alm = Pedido.Id-Alm NO-LOCK NO-ERROR.
        IF AVAILABLE Almacen THEN l-sucursal = Almacen.Refer. 
        IF Pedido.Id-Cond >0 THEN l-tipo-cliente = "Credito". 
        ELSE l-tipo-cliente = "Contado". 
        IF l-ClaseCte = 0 OR (l-ClaseCte <> 0 AND l-ClaseCte = Cliente.Id-ClaseCte) THEN 
        DO:
            l-pedido= Pedido.Id-Pedido.
            FIND FIRST ttPedXAut WHERE ttPedXAut.Num = l-Num EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE ttPedXAut THEN 
            DO:
                CREATE ttPedXAut.
                ASSIGN 
                    ttPedXAut.Num           = l-num
                    ttPedXAut.Cliente       = Cliente.Id-Cliente
                    ttPedXAut.TipoCliente   = l-tipo-cliente
                    ttPedXAut.NomResp       = l-NomResp
                    ttPedXAut.ClaseCliente  = l-NomClase
                    ttPedXAut.RazonSocial   = Cliente.RazonSocial
                    ttPedXAut.NumPedido     = l-pedido
                    ttPedXAut.FechaRegistro = Pedido.FecReg
                    ttPedXAut.ImportePedido = l-Imp
                    ttPedXAut.NomVendedor   = l-empleado
                    ttPedXAut.Hora          = STRING(Pedido.HrSta,"HH:MM")
                    ttPedXAut.Sucursal      = l-sucursal.    
            END.
            FIND FIRST BlkAut WHERE BlkAut.Id-Cliente = Pedido.Id-Cliente NO-LOCK NO-ERROR.
            FIND FIRST blkRFC WHERE REPLACE(blkRFC.RFC," ","") = REPLACE(Pedido.RFC," ","") NO-LOCK NO-ERROR.
            IF AVAILABLE BlkAut OR AVAILABLE blkRFC THEN 
            DO:
                /* --- Siempre prioriza RFC si está disponible --- */
                IF AVAILABLE blkRFC THEN 
                DO:
                    
                    /* Busca el nombre del empleado en la tabla Empleado */
                    FIND FIRST Empleado WHERE empleado.Iniciales = blkRFC.Id-User NO-LOCK NO-ERROR.
                    ASSIGN 
                        ttPedXAut.Bloqueo     = "SI"
                        ttPedXAut.TipoBloqueo = "RFC"
                        ttPedXAut.NomBloqueo  = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE blkRFC.Id-User  
                        ttPedXAut.FecBloqueo  = blkRFC.FecReg.  /* Fecha de RFC */
                END.    
                /* --- Solo usa BlkAut si no hay bloqueo por RFC --- */
                ELSE IF AVAILABLE BlkAut THEN 
                        ASSIGN 
                            ttPedXAut.Bloqueo     = "SI"  
                            ttPedXAut.TipoBloqueo = "CLIENTE"
                            ttPedXAut.NomBloqueo  = "SIN USUARIO"  /* Valor por defecto */
                            ttPedXAut.FecBloqueo  = BlkAut.FecReg. /* Fecha de BlkAut */
            END.
            ELSE 
            DO:
                ASSIGN 
                    ttPedXAut.Bloqueo = "NO".
            END.
            RELEASE ttPedXAut.   
        END.
    END. 
    RETURN.         
END PROCEDURE.

