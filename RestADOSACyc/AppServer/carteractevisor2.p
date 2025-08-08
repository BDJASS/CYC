@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
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
    FIELD treinta      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD sesenta      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventa      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD lineacredito AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD diacartera   AS INTEGER
    FIELD promedio     AS DECIMAL
    FIELD plazo        AS INTEGER 
    FIELD responsable  AS CHARACTER FORMAT "X(40)"
    INDEX idx-clase id ASCENDING numcliente ASCENDING cliente ASCENDING.
DEFINE DATASET dscartera FOR ttCartera.

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
DEFINE VARIABLE l-tot-30      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-31      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-61      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-dia         AS INT.
DEFINE VARIABLE l-dia-max     AS INT       FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-saldo       AS DEC       FORMAT ">>>,>>>,>>9.99".




@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCartera:
    DEFINE OUTPUT PARAMETER TABLE FOR ttCartera.
    DEFINE INPUT PARAMETER pClaseCte AS INT.
    DEFINE INPUT PARAMETER pTipo     AS INT.

    /* Variables */
    DEFINE VARIABLE l-num      AS INT       NO-UNDO.
    DEFINE VARIABLE l-clase    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-segmento AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-resp     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-moneda   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-saldo    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-tot-ven  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-tot-vig  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-tot-porv AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-tot-30   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-tot-31   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-tot-61   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-dia-max  AS INT       NO-UNDO.
    DEFINE VARIABLE l-prompago AS DECIMAL   NO-UNDO.

    EMPTY TEMP-TABLE ttCartera.
    l-num = 0.

    /* Parámetros nulos o no definidos */
    IF pClaseCte = ? THEN pClaseCte = 0.

    /* Búsqueda optimizada por Cliente */
    FOR EACH Cliente WHERE (pClaseCte = 0 OR Cliente.Id-ClaseCte = pClaseCte) AND Cliente.Activo = TRUE 
                     NO-LOCK BY Cliente.Id-Cliente:
                
        /* Inicializa acumuladores */
        ASSIGN
            l-saldo    = 0
            l-tot-ven  = 0
            l-tot-vig  = 0 
            l-tot-porv = 0
            l-tot-30   = 0
            l-tot-31   = 0
            l-tot-61   = 0
            l-dia-max  = 0
            l-prompago = 0.

        /* Procesa los movimientos del cliente */
        FOR EACH MovCliente WHERE MovCliente.Id-Cliente = Cliente.Id-Cliente 
            AND MovCliente.FecReg <= TODAY 
            AND MovCliente.Afectado = TRUE 
            AND MovCliente.Saldo > 0
            NO-LOCK BY MovCliente.FecReg:

            /* Cálculos de saldos */
            l-saldo = l-saldo + MovCliente.Saldo.

            /* Clasificación por fechas */
            IF MovCliente.FecVen < TODAY THEN
                l-tot-ven = l-tot-ven + MovCliente.Saldo.
            ELSE IF MovCliente.FecVen >= TODAY + 16 THEN
                    l-tot-vig = l-tot-vig + MovCliente.Saldo.
                ELSE
                    l-tot-porv = l-tot-porv + MovCliente.Saldo.

            /* Acumulación por días */
            DEFINE VARIABLE l-dia AS INT NO-UNDO.
            l-dia = TODAY - MovCliente.FecReg.

            IF l-dia <= 30 THEN
                l-tot-30 = l-tot-30 + MovCliente.Saldo.
            ELSE IF l-dia <= 61 THEN
                    l-tot-31 = l-tot-31 + MovCliente.Saldo.
                ELSE
                    l-tot-61 = l-tot-61 + MovCliente.Saldo.
        END.
        /* Obtiene información del cliente */
        FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
        IF AVAILABLE ClaseCte THEN 
            ASSIGN l-clase = ClaseCte.Descr.
        ELSE 
            ASSIGN l-clase = "Sin Clase".

        FIND FIRST SegmentoCte WHERE SegmentoCte.Id-SegmentoCte = Cliente.Id-SegmentoCte NO-LOCK NO-ERROR.
        IF AVAILABLE SegmentoCte THEN 
            ASSIGN l-segmento = SegmentoCte.Descr.
        ELSE 
            ASSIGN l-segmento = "Sin Segmento".

        FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
        IF AVAILABLE Resp THEN 
            ASSIGN l-resp = Resp.Nombre.
        ELSE 
            ASSIGN l-resp = "Sin Responsable".


        /* Llamada a cálculo externo */
        RUN cxcb0270.p(INPUT Cliente.Id-Cliente, INPUT TODAY, OUTPUT l-dia-max, OUTPUT l-prompago).

        /* Inserta los datos en la tabla temporal */
        l-num = l-num + 1.
        CREATE ttCartera.
        ASSIGN
            ttCartera.id           = l-num
            ttCartera.clasecliente = l-clase
            ttCartera.numcliente   = Cliente.Id-Cliente
            ttCartera.cliente      = Cliente.RazonSocial
            ttCartera.segmento     = l-segmento
            ttCartera.tipoMoneda   = ""
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
            ttCartera.responsable  = l-resp.
    END.
    /* Filtrado según el tipo */
    IF pTipo = 1 THEN 
    DO:
        FOR EACH ttCartera WHERE ttCartera.vigente <= 0:
            DELETE ttCartera.
        END.
    END.
    ELSE IF pTipo = 2 THEN 
        DO:
            FOR EACH ttCartera WHERE ttCartera.porvencer <= 0:
                DELETE ttCartera.
            END.
        END.
        ELSE IF pTipo = 3 THEN 
            DO:
                FOR EACH ttCartera WHERE ttCartera.montovencido <= 0:
                    DELETE ttCartera.
                END.
            END.
RETURN.             
END PROCEDURE.
