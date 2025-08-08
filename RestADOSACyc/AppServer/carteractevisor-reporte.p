

/*------------------------------------------------------------------------
    File        : carteractevisor.p
    Purpose     : /CarteraVisor

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Tue Oct 22 14:47:26 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num         AS INT.
DEFINE VARIABLE l-tot-total   AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig     AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-ven     AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv    AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-clase       AS CHARACTER FORMAT "X(20)". 
DEFINE VARIABLE l-segmento    AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE l-estatus     AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-resp        AS CHARACTER FORMAT "X(30)" .
DEFINE VARIABLE l-moneda      AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-tipo-moneda AS INT.
DEFINE VARIABLE l-prompago    AS DECIMAL.
DEFINE VARIABLE l-dia-max     AS INT.
DEFINE VARIABLE l-tot-30      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-31      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-61      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-dia         AS INT.

DEFINE VAR      l-saldo       AS DEC FORMAT ">>>,>>>,>>9.99".


    
DEFINE TEMP-TABLE ttCartera NO-UNDO
    FIELD id           AS INTEGER
    FIELD clasecliente AS CHARACTER FORMAT "X(12)"
    FIELD numcliente   AS INTEGER
    FIELD cliente      AS CHARACTER FORMAT "X(40)"
    FIELD segmento     AS CHARACTER FORMAT "X(12)"
    FIELD tipoMoneda   AS CHARACTER FORMAT "X(15)"
    FIELD saldo        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD montovencido AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD vigente      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD porvencer    AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD fecha        AS DATE      FORMAT 99/99/9999
    FIELD treinta      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD sesenta      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventa      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD lineacredito AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD diacartera   AS INTEGER
    FIELD promedio     AS DECIMAL
    FIELD plazo        AS INTEGER
    FIELD responsable  AS CHARACTER FORMAT "X(40)"
    INDEX idx-clase id ASCENDING.
DEFINE DATASET dscartera FOR ttCartera.


    ASSIGN 
        l-num    = 0
        l-moneda = "".
    EMPTY TEMP-TABLE ttCartera.
    
OUTPUT to "/home/sis10/cartera-reporte-cte-3.txt". 
    FOR EACH Cliente WHERE Cliente.Id-Resp = 33 NO-LOCK:
        FOR EACH Movcliente WHERE Movcliente.id-cliente = Cliente.id-cliente AND
            Movcliente.FecReg <= TODAY                 AND
            MovCliente.Id-MC  <= 3                     AND
            MovCliente.Afectado                       
            NO-LOCK  BREAK  BY Cliente.RazonSocial BY Cliente.id-cliente
            BY MovCliente.Id-Cliente  :
            IF MovCliente.Saldo <= 0 THEN NEXT.                    
            ASSIGN
                l-clase    = ""
                l-segmento = ""
                l-resp     = "". 
            FIND FIRST ClaseCte WHERE ClaseCte.id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK   NO-ERROR.
            IF AVAILABLE ClaseCte THEN l-clase = ClaseCte.Descr. 
            FIND FIRST SegmentoCte WHERE SegmentoCte.id-SegmentoCte = Cliente.Id-SegmentoCte NO-LOCK NO-ERROR.
            IF AVAILABLE SegmentoCte THEN l-segmento = SegmentoCte.Descr.
            FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
            IF AVAILABLE Resp THEN l-resp = Resp.Nombre.   
        
            FIND FIRST Factura WHERE Factura.Id-Factura = MovCliente.RefSaldo
                                 AND Factura.Id-Cliente = Cliente.Id-Cliente
                                 AND Factura.FecReg     = MovCliente.FecReg
                NO-LOCK NO-ERROR.
            IF AVAILABLE Factura THEN l-tipo-moneda = Factura.Id-Moneda.
         
            FIND FIRST Moneda WHERE moneda.id-moneda = l-tipo-moneda NO-LOCK NO-ERROR.
            IF AVAILABLE moneda THEN l-moneda = moneda.nombre.
            IF FIRST-OF(Cliente.Id-Cliente) THEN  
                ASSIGN
                    l-tot-total = 0
                    l-tot-vig   = 0
                    l-tot-ven   = 0
                    l-tot-porv  = 0
                    l-estatus   = ""
                    l-saldo     = 0
                    l-tot-30    = 0
                    l-tot-31    = 0
                    l-tot-61    = 0.
            l-saldo = l-saldo + MovCliente.Saldo.
            
            IF MovCliente.fecven <  TODAY THEN 
            DO:
                ASSIGN
                    l-tot-ven = l-tot-ven  +  Movcliente.Saldo.
            END. 
            IF MovCliente.fecven >= TODAY + 16 THEN 
            DO:
                ASSIGN 
                    l-tot-vig = l-tot-vig +  Movcliente.Saldo.
            END.
            IF MovCliente.fecven >= TODAY AND
                MovCliente.fecven <= TODAY + 15 THEN 
            DO:
                ASSIGN 
                    l-tot-porv = l-tot-porv +  Movcliente.Saldo.
            END.  
            l-dia =  TODAY - MovCliente.FecReg .
        
            IF l-dia <= 30 THEN
                ASSIGN l-tot-30 = l-tot-30 + Movcliente.Saldo.
            ELSE IF l-dia <= 61 THEN
                    ASSIGN l-tot-31 = l-tot-31 + Movcliente.Saldo.
                ELSE 
                    ASSIGN l-tot-61 = l-tot-61 + Movcliente.Saldo.
            IF LAST-OF(Cliente.id-cliente) THEN
            DO:
/*                message "INICIO" MovCliente.Id-Cliente l-dia-max SKIP l-prompago VIEW-AS ALERT-BOX.*/
               RUN cxcb0270.p (INPUT MovCliente.Id-Cliente,INPUT TODAY,OUTPUT l-dia-max ,OUTPUT l-prompago)  
                
                l-num = l-num + 1.  
                
/*                DISPLAY                */
/*                 l-num                 */
/*                 l-clase               */
/*                 Cliente.id-cliente    */
/*                 Cliente.RazonSocial   */
/*                 l-segmento            */
/*                 l-saldo               */
/*                 MovCliente.Importe    */
/*                 MovCliente.Saldo      */
/*                 l-tot-ven             */
/*                 l-tot-vig             */
/*                 l-tot-porv            */
/*                 l-tot-30              */
/*                 l-tot-31              */
/*                 l-tot-61              */
/*                 l-resp                */
/*                 l-dia-max             */
/*                 l-prompago            */
/*                       with frame a    */
/*                        down width 500.*/

                FIND FIRST ttCartera WHERE ttCartera.id = l-num NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttCartera THEN
                    CREATE ttCartera.
                ASSIGN
                    ttCartera.id           = l-num
                    ttCartera.clasecliente = l-clase
                    ttCartera.numcliente   = Cliente.id-cliente
                    ttCartera.cliente      = Cliente.RazonSocial
                    ttCartera.segmento     = l-segmento
                    ttCartera.tipoMoneda   = l-moneda
                    ttCartera.saldo        = l-saldo
                    ttCartera.montovencido = l-tot-ven
                    ttCartera.vigente      = l-tot-vig
                    ttCartera.porvencer    = l-tot-porv
                    ttCartera.treinta      = l-tot-30
                    ttCartera.sesenta      = l-tot-31
                    ttCartera.noventa      = l-tot-61
                    ttCartera.lineacredito = Cliente.Limite
                    ttCartera.diacartera   = l-dia-max
                    ttCartera.promedio     = l-prompago
                    ttCartera.plazo        = Cliente.Plazo
                    ttCartera.responsable  = l-resp .
                RELEASE ttCartera.
                
                ASSIGN
                l-saldo    = 0
                l-tot-vig  = 0
                l-tot-ven  = 0
                l-tot-porv = 0
                l-tot-30   = 0
                l-tot-31   = 0
                l-tot-61   = 0.
            END.
        END.
    END.  
    
FOR EACH ttCartera:
     Display ttCartera
             with frame a
             down width 500.
END.    
      
output close.

