@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : devoluciones.p
    Purpose     : Consulta de devoluciones con número de NCR, Factura Aplicada,
                  Número de Entrada, y Fecha de Expiración.
    Author(s)   : /Devoluciones sis10
    Created     : Sun Dec 08 18:52:13 CST 2024
    Updated     : (Fecha de actualización)

    Description : Este programa realiza una consulta sobre devoluciones de facturas.
                  Filtra devoluciones pendientes o aplicadas según parámetros enviados
                  por el usuario y genera una salida estructurada en una tabla temporal.

    Notes       : Incluye validaciones para los parámetros de entrada y lógica 
                  para buscar información adicional como NCR, Factura Aplicada y Número de Entrada.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores en nivel de bloque */

/* ********************  Preprocessor Definitions  ******************** */
DEFINE TEMP-TABLE ttDevoluciones NO-UNDO
    FIELD IdDev         AS CHARACTER FORMAT "x(6)"     /* ID de la devolución */
    FIELD FecReg        AS DATE                        /* Fecha de registro */
    FIELD IdCliente     AS INTEGER                     /* ID del cliente */
    FIELD RazonSocial   AS CHARACTER FORMAT "x(30)"    /* Razón social del cliente */
    FIELD IdFactura     AS CHARACTER FORMAT "x(10)"    /* ID de la factura */
    FIELD FactApl       AS CHARACTER FORMAT "x(10)"    /* Factura aplicada */
    FIELD NoEntrada     AS CHARACTER FORMAT "x(10)"    /* Número de entrada */
    FIELD FecApl        AS DATE                        /* Fecha de aplicación */
    FIELD Total         AS DECIMAL FORMAT "ZZZ,ZZ9.99" /* Total de la devolución */
    FIELD IdNCR         AS CHARACTER FORMAT "x(10)"    /* Número de NCR */
    FIELD FecExp        AS DATE.                       /* Fecha de expiración */

DEFINE VARIABLE l-Total     AS DECIMAL NO-UNDO INITIAL 0. /* Total acumulado */
DEFINE VARIABLE l-ncr       AS CHARACTER NO-UNDO FORMAT "x(10)". /* NCR relacionado */
DEFINE VARIABLE l-FactApl   AS CHARACTER NO-UNDO FORMAT "x(10)". /* Factura aplicada */
DEFINE VARIABLE l-NoEntrada AS CHARACTER NO-UNDO FORMAT "x(10)". /* Número de entrada */

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDevoluciones:
    /* -------------------------------------------------------------------
       Nombre      : GetDevoluciones
       Propósito   : Consulta devoluciones de un cliente, con opción de
                     filtrar por devoluciones aplicadas o pendientes desde
                     una fecha específica.
       Parámetros  :
           - INPUT  l-Todo   : Lógico. TRUE para incluir pendientes y aplicadas.
           - INPUT  l-fec    : Fecha. Fecha inicial del filtro (opcional).
           - INPUT  l-Cliente: ID del cliente para filtrar las devoluciones.
           - OUTPUT ttDevoluciones: Tabla temporal con los resultados.

       ------------------------------------------------------------------- */

    DEFINE INPUT PARAMETER l-Todo AS LOGICAL NO-UNDO.      /* TRUE para incluir pendientes y aplicadas */
    DEFINE INPUT PARAMETER l-fec   AS DATE NO-UNDO.        /* Fecha inicial para filtrar */
    DEFINE INPUT PARAMETER l-Cliente LIKE Cliente.Id-Cliente NO-UNDO. /* Cliente */
    DEFINE OUTPUT PARAMETER TABLE FOR ttDevoluciones.      /* Salida de resultados */

    /* Inicialización */
    ASSIGN l-Total = 0.
    
    /* Validación: Si l-Todo es TRUE y l-fec es NULL, asignar la fecha actual */
    IF l-Todo AND l-fec = ? THEN
        ASSIGN l-fec = TODAY. 
    

    /* Transacción para la consulta */
    DO TRANSACTION:
        /* Si se requiere filtrar por fecha */
        IF l-Todo THEN DO:
            /* Trae devoluciones pendientes y aplicadas a partir de la fecha */
            FOR EACH Devolucion WHERE
                ((Devolucion.Id-Cliente = l-Cliente AND Devolucion.FecApl = ? AND
                  Devolucion.FecCanc = ?) OR
                 (Devolucion.FecReg >= l-fec AND Devolucion.Id-Cliente = l-Cliente))
                NO-LOCK:
                
                /* Buscar el NCR relacionado */
                ASSIGN l-ncr = "". /* Inicializar */
                IF Devolucion.Id-NCR <> '' THEN
                    ASSIGN l-ncr = Devolucion.Id-NCR.
                ELSE DO:
                    FIND FIRST NCR WHERE NCR.Id-Cliente = Devolucion.Id-Cliente NO-LOCK NO-ERROR.
                    IF AVAILABLE NCR THEN DO:
                        FIND FIRST DetNCR WHERE DetNCR.Id-NCR = NCR.Id-NCR AND
                            DetNCR.Refer = STRING(Devolucion.Id-Dev, "999999") NO-LOCK NO-ERROR.
                        IF AVAILABLE DetNCR THEN
                            ASSIGN l-ncr = NCR.Id-NCR.
                    END.
                END.

                /* Buscar Factura Aplicada y Número de Entrada */
                ASSIGN l-FactApl = Devolucion.Documento. /* Factura Aplicada */
                FIND FIRST EDev WHERE EDev.Id-Dev = Devolucion.Id-Dev NO-LOCK NO-ERROR.
                IF AVAILABLE EDev THEN
                    ASSIGN l-NoEntrada = EDev.Id-EDev.
                ELSE
                    ASSIGN l-NoEntrada = "".

                ASSIGN l-Total = l-Total + Devolucion.Tot.

                /* Crear un registro en la tabla temporal */
                CREATE ttDevoluciones.
                ASSIGN ttDevoluciones.IdDev = STRING(Devolucion.Id-Dev, "999999")
                       ttDevoluciones.FecReg = Devolucion.FecReg
                       ttDevoluciones.IdCliente = Devolucion.Id-Cliente
                       ttDevoluciones.RazonSocial = Devolucion.RazonSocial
                       ttDevoluciones.IdFactura = Devolucion.Id-Factura
                       ttDevoluciones.FactApl = l-FactApl /* Factura Aplicada */
                       ttDevoluciones.NoEntrada = l-NoEntrada /* Número de Entrada */
                       ttDevoluciones.FecApl = Devolucion.FecApl
                       ttDevoluciones.Total = Devolucion.Tot
                       ttDevoluciones.IdNCR = l-ncr /* Número de NCR */
                       ttDevoluciones.FecExp = Devolucion.FecExpira. /* Fecha de Expiración */
            END.
        END.
        ELSE DO:
            /* Sin filtro de fecha (solo devoluciones pendientes) */
            FOR EACH Devolucion WHERE
                Devolucion.Id-Cliente = l-Cliente AND Devolucion.FecApl = ? AND
                Devolucion.FecCanc = ?
                NO-LOCK:

                /* Buscar el NCR relacionado */
                ASSIGN l-ncr = "". /* Inicializar */
                IF Devolucion.Id-NCR <> '' THEN
                    ASSIGN l-ncr = Devolucion.Id-NCR.
                ELSE DO:
                    FIND FIRST NCR WHERE NCR.Id-Cliente = Devolucion.Id-Cliente NO-LOCK NO-ERROR.
                    IF AVAILABLE NCR THEN DO:
                        FIND FIRST DetNCR WHERE DetNCR.Id-NCR = NCR.Id-NCR AND
                            DetNCR.Refer = STRING(Devolucion.Id-Dev, "999999") NO-LOCK NO-ERROR.
                        IF AVAILABLE DetNCR THEN
                            ASSIGN l-ncr = NCR.Id-NCR.
                    END.
                END.

                /* Buscar Factura Aplicada y Número de Entrada */
                ASSIGN l-FactApl = Devolucion.Documento. /* Factura Aplicada */
                FIND FIRST EDev WHERE EDev.Id-Dev = Devolucion.Id-Dev NO-LOCK NO-ERROR.
                IF AVAILABLE EDev THEN
                    ASSIGN l-NoEntrada = EDev.Id-EDev.
                ELSE
                    ASSIGN l-NoEntrada = "".

                ASSIGN l-Total = l-Total + Devolucion.Tot.

                /* Crear un registro en la tabla temporal */
                CREATE ttDevoluciones.
                ASSIGN ttDevoluciones.IdDev = STRING(Devolucion.Id-Dev, "999999")
                       ttDevoluciones.FecReg = Devolucion.FecReg
                       ttDevoluciones.IdCliente = Devolucion.Id-Cliente
                       ttDevoluciones.RazonSocial = Devolucion.RazonSocial
                       ttDevoluciones.IdFactura = Devolucion.Id-Factura
                       ttDevoluciones.FactApl = l-FactApl /* Factura Aplicada */
                       ttDevoluciones.NoEntrada = l-NoEntrada /* Número de Entrada */
                       ttDevoluciones.FecApl = Devolucion.FecApl
                       ttDevoluciones.Total = Devolucion.Tot
                       ttDevoluciones.IdNCR = l-ncr /* Número de NCR */
                       ttDevoluciones.FecExp = Devolucion.FecExpira. /* Fecha de Expiración */
            END.
        END.
    END. /* Fin de la transacción */
END PROCEDURE.
