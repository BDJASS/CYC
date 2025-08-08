@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").


/* ***************************************************************************


  /MovimientosClienteEstadistica




 ****************************************************************************** */





DEFINE TEMP-TABLE ttEstadistica NO-UNDO 
    FIELD idCliente     AS INTEGER
    FIELD RazonSocial   AS CHARACTER
    FIELD anio          AS INTEGER
    FIELD mes           AS CHARACTER
    FIELD ventas        AS INT FORMAT "zzzzzzz9-"  
    FIELD acumulado     AS INT FORMAT "zzzzzzz9-"  
    FIELD pagos         AS INT FORMAT "zzzzzzz9-" 
    FIELD pagostot      AS INT FORMAT "zzzzzzz9-" 
    FIELD crecimiento   AS DECI FORMAT "ZZ,ZZ9.99-".

DEFINE VARIABLE l-anio        AS INTEGER NO-UNDO.
DEFINE VARIABLE l-anio2       AS INTEGER NO-UNDO.
DEFINE VARIABLE l-anio3       AS INTEGER NO-UNDO.
DEFINE VARIABLE l-meses       AS CHARACTER EXTENT 12 NO-UNDO
    INITIAL ["ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC"].
DEFINE VARIABLE i             AS INTEGER NO-UNDO.
DEFINE VARIABLE l-Cliente     AS INTEGER NO-UNDO.
DEFINE VARIABLE l-todo        AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-todopago    AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-acumuladoAnt1 AS DECIMAL EXTENT 12 NO-UNDO. /* Acumulados del año actual */
DEFINE VARIABLE l-acumuladoAnt2 AS DECIMAL EXTENT 12 NO-UNDO. /* Acumulados del año anterior */
DEFINE VARIABLE l-acumuladoAnt3 AS DECIMAL EXTENT 12 NO-UNDO. /* Acumulados del año más antiguo */
DEFINE VARIABLE l-venta       AS INT FORMAT "zzzzzzz9-"   EXTENT 12 NO-UNDO.
DEFINE VARIABLE l-ventaccum   AS INT FORMAT "zzzzzzz9-"   EXTENT 12 NO-UNDO.
DEFINE VARIABLE l-pagoaccum   AS INT FORMAT "zzzzzzz9-"   EXTENT 12 NO-UNDO.
DEFINE VARIABLE l-pagos       AS INT FORMAT "zzzzzzz9-"   EXTENT 12 NO-UNDO.
DEFINE VARIABLE l-crecimiento AS DECI FORMAT "ZZ,ZZ9.99-"  EXTENT 12 NO-UNDO.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").



PROCEDURE GetEstadistica:
    DEFINE INPUT PARAMETER anio       AS INTEGER.
    DEFINE INPUT PARAMETER idCliente AS INTEGER.
    DEFINE OUTPUT PARAMETER TABLE FOR ttEstadistica.

    /* Inicialización */
    ASSIGN l-anio = anio
           l-Cliente = idCliente
           l-anio2 = l-anio - 1
           l-anio3 = l-anio - 2.

    DO i = 1 TO 12:
        ASSIGN l-venta[i] = 0
               l-ventaccum[i] = 0
               l-pagos[i] = 0
               l-acumuladoAnt1[i] = 0
               l-acumuladoAnt2[i] = 0
               l-acumuladoAnt3[i] = 0.
    END.

    /* Procesar año actual (l-anio) primero */
    FIND FIRST Cliente WHERE Cliente.Id-Cliente = l-Cliente NO-LOCK NO-ERROR. 
    FIND FIRST EstCte WHERE EstCte.Anio = l-anio AND EstCte.Id-Cliente = l-Cliente NO-LOCK NO-ERROR.
    IF AVAILABLE EstCte THEN DO: 
        ASSIGN l-todo     = 0
               l-todopago = 0.
        DO i = 1 TO 12:
            ASSIGN l-venta[i]     = EstCte.VentasCO[i] + EstCte.VentasCR[i]
                   l-pagos[i]     = EstCte.AbonoCO[i]  + EstCte.AbonoCR[i]
                   l-todo         = l-todo + l-venta[i]
                   l-todopago     = l-todopago + l-pagos[i]
                   l-ventaccum[i] = l-todo
                   l-pagoaccum[i] = l-todopago.

            CREATE ttEstadistica.
            ASSIGN ttEstadistica.idCliente = l-Cliente
                   ttEstadistica.RazonSocial = Cliente.RazonSocial WHEN AVAILABLE Cliente
                   ttEstadistica.anio        = l-anio
                   ttEstadistica.mes         = l-meses[i]
                   ttEstadistica.ventas      = l-venta[i]
                   ttEstadistica.acumulado   = l-ventaccum[i]
                   ttEstadistica.pagos       = l-pagos[i]
                   ttEstadistica.pagostot    = l-pagoaccum[i]
                   ttEstadistica.crecimiento = 0. 
        END.
    END.
    ELSE DO:
        /* Inserta datos en blanco si no hay registros */
        DO i = 1 TO 12:
            CREATE ttEstadistica.
            ASSIGN ttEstadistica.idCliente = l-Cliente
                   ttEstadistica.RazonSocial = Cliente.RazonSocial WHEN AVAILABLE Cliente
                   ttEstadistica.anio = l-anio
                   ttEstadistica.mes = l-meses[i]
                   ttEstadistica.ventas = 0
                   ttEstadistica.acumulado = 0
                   ttEstadistica.pagos = 0
                   ttEstadistica.pagostot = 0
                   ttEstadistica.crecimiento = 0.
        END.
    END.

    /* Procesar año anterior (l-anio2) después */
    FIND FIRST EstCte WHERE EstCte.Anio = l-anio2 AND EstCte.Id-Cliente = l-Cliente NO-LOCK NO-ERROR.
    IF AVAILABLE EstCte THEN DO:
        ASSIGN l-todo = 0. 
        DO i = 1 TO 12:
            ASSIGN l-todo = l-todo + (EstCte.VentasCO[i] + EstCte.VentasCR[i])
                   l-acumuladoAnt2[i] = l-todo.

            CREATE ttEstadistica.
            ASSIGN ttEstadistica.idCliente = l-Cliente
                   ttEstadistica.RazonSocial = Cliente.RazonSocial WHEN AVAILABLE Cliente
                   ttEstadistica.anio = l-anio2
                   ttEstadistica.mes = l-meses[i]
                   ttEstadistica.ventas = EstCte.VentasCO[i] + EstCte.VentasCR[i]
                   ttEstadistica.acumulado = l-todo
                   ttEstadistica.pagos =  0 /* Sin detalle de pagos */
                   ttEstadistica.crecimiento = 0.
        END.
    END.
    ELSE DO:
        /* Inserta datos en blanco si no hay registros */
        DO i = 1 TO 12:
            CREATE ttEstadistica.
            ASSIGN ttEstadistica.idCliente = l-Cliente
                   ttEstadistica.RazonSocial = Cliente.RazonSocial WHEN AVAILABLE Cliente
                   ttEstadistica.anio = l-anio2
                   ttEstadistica.mes = l-meses[i]
                   ttEstadistica.ventas = 0
                   ttEstadistica.acumulado = 0
                   ttEstadistica.pagos = 0
                   ttEstadistica.crecimiento = 0.
        END.
    END.
    
    

/* Procesar año más antiguo (l-anio3) con crecimiento */
FIND FIRST EstCte WHERE EstCte.Anio = l-anio3 AND EstCte.Id-Cliente = l-Cliente NO-LOCK NO-ERROR.
IF AVAILABLE EstCte THEN DO:
    ASSIGN l-todo = 0. /* Inicializar el acumulado total */
    
    /* Iterar sobre los meses del año */
    DO i = 1 TO 12:
        /* Acumular las ventas del año más antiguo */
        ASSIGN l-todo = l-todo + INT(EstCte.VentasCO[i] + EstCte.VentasCR[i])
               l-acumuladoAnt3[i] = l-todo. /* Guardar acumulado para cada mes */

        /* Calcular crecimiento basado en los acumulados de los años 2 y 3 */
        ASSIGN l-crecimiento[i] = IF l-acumuladoAnt2[i] > 0 THEN
                                 ROUND(((l-ventaccum[i] - l-acumuladoAnt2[i]) / l-acumuladoAnt2[i]) * 100,2)   
                                ELSE 0. /* Evitar división por cero */

        /* Crear un registro en la tabla temporal con los datos del mes */
        CREATE ttEstadistica.
        ASSIGN ttEstadistica.idCliente = l-Cliente
               ttEstadistica.RazonSocial = Cliente.RazonSocial WHEN AVAILABLE Cliente
               ttEstadistica.anio = l-anio3
               ttEstadistica.mes = l-meses[i]
               ttEstadistica.ventas = 0 /* Sin detalle de ventas para este año */
               ttEstadistica.acumulado = l-acumuladoAnt3[i]
               ttEstadistica.pagos = 0 /* Sin detalle de pagos para este año */
               ttEstadistica.crecimiento = l-crecimiento[i]. /* Guardar el crecimiento */
    END.
END.
 ELSE DO:
        /* Inserta datos en blanco si no hay registros */
        DO i = 1 TO 12:
            CREATE ttEstadistica.
            ASSIGN ttEstadistica.idCliente = l-Cliente
                   ttEstadistica.RazonSocial = Cliente.RazonSocial WHEN AVAILABLE Cliente
                   ttEstadistica.anio = l-anio3
                   ttEstadistica.mes = l-meses[i]
                   ttEstadistica.ventas = 0
                   ttEstadistica.acumulado = 0
                   ttEstadistica.pagos = 0
                   ttEstadistica.crecimiento = 0.
        END.
 END. 


END PROCEDURE.
