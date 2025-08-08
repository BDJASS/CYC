@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : anticipocons.p
    Purpose     : 

    Syntax      :/AnticipoCte

    Description : Consulta de anticipos por cliente

    Author(s)   : sis10
    Created     : Fri Jan 17 11:00:31 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/
  /*
  
  Mod ticket 904 Se requiere que consulte todos los anticipos,
  actualmente solo mostraba los pendientes x aplicar,
  se agregara la columna para saber cuando esta pendiente x aplicar
  
  JASS10062025
  
  
  */
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttAnticipos NO-UNDO
    FIELD IdAnticipo       AS CHARACTER
    FIELD Fecha            AS DATE
    FIELD Acuse            AS CHARACTER
    FIELD ImporteAnticipo  AS DECIMAL
    FIELD ImporteAplicado  AS DECIMAL
    FIELD ImportePendiente AS DECIMAL.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetConsultaAnticipo:
/*------------------------------------------------------------------------------
 Purpose: Consulta de anticipos mostrando siempre los pendientes y últimos 20
 Notes: Filtra por cliente y opcionalmente por fechas
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCliente AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER EsMov    AS LOGICAL INITIAL FALSE. // SE USA EN MOVIMIENTOS DE CLIENTES PARA QUE MUESTRE TODO
DEFINE INPUT PARAMETER pFecIni  AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pFecFin  AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttAnticipos.

DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.
DEFINE BUFFER bAnticipo FOR Anticipo.   

/* Limpiar la tabla temporal */
EMPTY TEMP-TABLE ttAnticipos.

// IF EsMov = ? THEN  EsMov = FALSE.

IF EsMov = TRUE THEN DO :
/* 1. Primero agregamos TODOS los anticipos pendientes (sin importar fechas) */
FOR EACH Anticipo NO-LOCK
    WHERE Anticipo.Id-Cliente = pCliente
      AND Anticipo.ImpAnticipo > Anticipo.ImpAplicado
      AND Anticipo.Canc = FALSE:
    
    CREATE ttAnticipos.
    ASSIGN
        ttAnticipos.IdAnticipo      = Anticipo.Id-Anticipo
        ttAnticipos.Fecha           = Anticipo.FecReg
        ttAnticipos.Acuse           = Anticipo.Id-Acuse
        ttAnticipos.ImporteAnticipo = Anticipo.ImpAnticipo
        ttAnticipos.ImporteAplicado = Anticipo.ImpAplicado
        ttAnticipos.ImportePendiente = Anticipo.ImpAnticipo - Anticipo.ImpAplicado.
END.

/* 2. Luego agregamos los últimos 20 anticipos (incluyendo los ya aplicados) */
IF pFecIni = ? OR pFecFin = ? THEN DO:
    /* Sin parámetros de fecha - tomamos los últimos 20 */
    FOR EACH bAnticipo NO-LOCK
        WHERE bAnticipo.Id-Cliente = pCliente
          AND bAnticipo.Canc = FALSE
        BY bAnticipo.FecReg DESCENDING
        BY bAnticipo.Id-Anticipo DESCENDING:
        
        /* Verificamos si ya existe en la tabla temporal (por ser pendiente) */
        FIND FIRST ttAnticipos WHERE ttAnticipos.IdAnticipo = bAnticipo.Id-Anticipo NO-ERROR.
        IF AVAILABLE ttAnticipos THEN NEXT.
        
        CREATE ttAnticipos.
        ASSIGN
            ttAnticipos.IdAnticipo      = bAnticipo.Id-Anticipo
            ttAnticipos.Fecha           = bAnticipo.FecReg
            ttAnticipos.Acuse           = bAnticipo.Id-Acuse
            ttAnticipos.ImporteAnticipo = bAnticipo.ImpAnticipo
            ttAnticipos.ImporteAplicado = bAnticipo.ImpAplicado
            ttAnticipos.ImportePendiente = bAnticipo.ImpAnticipo - bAnticipo.ImpAplicado.
            
        iCounter = iCounter + 1.
        IF iCounter >= 20 THEN LEAVE.
    END.
END.
ELSE DO:
    /* Con parámetros de fecha - tomamos los últimos 20 del rango */
    FOR EACH bAnticipo NO-LOCK
        WHERE bAnticipo.Id-Cliente = pCliente
          AND bAnticipo.FecReg >= pFecIni
          AND bAnticipo.FecReg <= pFecFin
          AND bAnticipo.Canc = FALSE
        BY bAnticipo.FecReg DESCENDING
        BY bAnticipo.Id-Anticipo DESCENDING:
        
        /* Verificamos si ya existe en la tabla temporal (por ser pendiente) */
        FIND FIRST ttAnticipos WHERE ttAnticipos.IdAnticipo = bAnticipo.Id-Anticipo NO-ERROR.
        IF AVAILABLE ttAnticipos THEN NEXT.
        
        CREATE ttAnticipos.
        ASSIGN
            ttAnticipos.IdAnticipo      = bAnticipo.Id-Anticipo
            ttAnticipos.Fecha           = bAnticipo.FecReg
            ttAnticipos.Acuse           = bAnticipo.Id-Acuse
            ttAnticipos.ImporteAnticipo = bAnticipo.ImpAnticipo
            ttAnticipos.ImporteAplicado = bAnticipo.ImpAplicado
            ttAnticipos.ImportePendiente = bAnticipo.ImpAnticipo - bAnticipo.ImpAplicado.
    END.
END.
END.
ELSE DO:
    /* SOLO MUESTRA EN CASO DE HABER PENDIENTES
        PROCESO PARA APLICAR PAGOS- SANTANDER- LIGAS DE PAGO */
FOR EACH Anticipo NO-LOCK
    WHERE Anticipo.Id-Cliente = pCliente
      AND Anticipo.ImpAnticipo > Anticipo.ImpAplicado
      AND Anticipo.Canc = FALSE:
    
    CREATE ttAnticipos.
    ASSIGN
        ttAnticipos.IdAnticipo      = Anticipo.Id-Anticipo
        ttAnticipos.Fecha           = Anticipo.FecReg
        ttAnticipos.Acuse           = Anticipo.Id-Acuse
        ttAnticipos.ImporteAnticipo = Anticipo.ImpAnticipo
        ttAnticipos.ImporteAplicado = Anticipo.ImpAplicado
        ttAnticipos.ImportePendiente = Anticipo.ImpAnticipo - Anticipo.ImpAplicado.
END.
    
    
END.    
END PROCEDURE.
