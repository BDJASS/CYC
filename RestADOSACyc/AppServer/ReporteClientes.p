@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Clientes.p
    Purpose     : Consultar clientes con filtros opcionales.

    Syntax      :

    Description : Devuelve información de clientes filtrada según los parámetros opcionales.

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------



Ticket 1215 Quitar el Nombre del Vendedor y enviar solo Iniciales
            JASS16072015     
            
            
*/
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.  

/* Tabla temporal para la salida */
DEFINE TEMP-TABLE ttRepClientes NO-UNDO 
    FIELD TipoCliente    AS CHARACTER   /* Contado o Crédito */
    FIELD IdCliente      AS INTEGER
    FIELD RazonSocial    AS CHARACTER
    FIELD RegimenCapital AS CHARACTER
    FIELD ClaseCliente   AS CHARACTER
    FIELD Segmento       AS CHARACTER
    FIELD FechaAlta      AS DATE
    FIELD Correo         AS CHARACTER
    FIELD Estado         AS CHARACTER
    FIELD Ciudad         AS CHARACTER
    FIELD Colonia        AS CHARACTER
    FIELD CalleYNumero   AS CHARACTER
    FIELD Estatus        AS CHARACTER
    FIELD IdGiro         LIKE Cliente.Id-Giro
    FIELD Giro           AS CHARACTER
    FIELD IdVendedor     LIKE Cliente.Id-Vendedor
    FIELD NomVend        AS CHARACTER
    
    INDEX idx-cliente IdCliente ASCENDING.

/* Buffers para las tablas relacionadas */
DEFINE BUFFER bfCliente     FOR Cliente.  
DEFINE BUFFER bfSegmentoCte FOR SegmentoCte.
DEFINE BUFFER bfClaseCte    FOR ClaseCte.
DEFINE BUFFER bfCiudad      FOR Ciudad.
DEFINE BUFFER bfEstado      FOR Estado.

DEFINE VARIABLE l-resp   AS CHARACTER NO-UNDO.
DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
DEFINE VARIABLE palabras AS INTEGER   NO-UNDO.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarClientes:
    /*--------------------------------------------------------------------------  
     Purpose     : Consultar clientes con filtros opcionales.
     Notes       : Estatus es obligatorio.
    --------------------------------------------------------------------------*/

    /* Parámetros de entrada */
    DEFINE INPUT  PARAMETER TipoCliente  AS INTEGER NO-UNDO .  /* "Credito" o "Contado" */
    DEFINE INPUT  PARAMETER ClaseCliente AS INTEGER NO-UNDO .
    DEFINE INPUT  PARAMETER Segmento     AS INTEGER NO-UNDO .
    DEFINE INPUT  PARAMETER Ciudad       AS INTEGER NO-UNDO .
    DEFINE INPUT  PARAMETER FechaInicio  AS DATE NO-UNDO. /* Nueva fecha de inicio */
    DEFINE INPUT  PARAMETER FechaFin     AS DATE NO-UNDO.    /* Nueva fecha de fin */
    DEFINE INPUT  PARAMETER Estatus      AS LOGICAL NO-UNDO. /* Obligatorio */
    DEFINE INPUT  PARAMETER iEstado      AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER iGiro        AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER iVendedor    AS CHAR NO-UNDO.
    
    DEFINE VARIABLE l-Ciudades AS CHAR .
    
    /* Parámetro de salida */
    DEFINE OUTPUT PARAMETER TABLE FOR ttRepClientes.    

    /* Inicializar la tabla temporal */
    EMPTY TEMP-TABLE ttRepClientes.

    /* Ajuste de parámetros opcionales */
    ASSIGN
        TipoCliente  = IF TipoCliente  = ? THEN 0 ELSE TipoCliente
        ClaseCliente = IF ClaseCliente = ? THEN 0 ELSE ClaseCliente
        Segmento     = IF Segmento     = ? THEN 0 ELSE Segmento
        Ciudad       = IF Ciudad       = ? THEN 0 ELSE Ciudad
        Estatus      = IF Estatus      = ? THEN TRUE ELSE Estatus
        iGiro        = IF iGiro        = ? THEN 0 ELSE iGiro
        iVendedor    = IF iVendedor = ? OR iVendedor = "0" THEN "" ELSE iVendedor
        iEstado      = IF iEstado      = ? THEN "" ELSE iEstado.
    
    /* Si se envía Estado, obtener todas sus Ciudades */
    IF iEstado > "" AND iEstado <> "0" THEN 
    DO:
        l-Ciudades = "".  /* Inicializar cadena vacía */
    
        FOR EACH Ciudad WHERE Ciudad.Id-Estado = iEstado NO-LOCK:
            /* Concatenar IDs separados por coma */
            ASSIGN 
                l-Ciudades = l-Ciudades + STRING(Ciudad.Id-Ciudad) + ",".
        END.
    
        /* Eliminar la última coma */
        IF l-Ciudades <> "" THEN 
            l-Ciudades = SUBSTRING(l-Ciudades, 1, LENGTH(l-Ciudades) - 1).
    END. 
    
    IF iEstado > "" AND iEstado <> "0" AND iEstado <> ? AND Ciudad > 0 THEN
        iEstado = "".
    
   //  IF iEstado > "" AND Ciudad > 0 THEN iEstado = "0".         
    /* Construir la consulta con los filtros opcionales */
    FOR EACH bfCliente NO-LOCK WHERE 
        bfCliente.Activo = Estatus AND
        (TipoCliente = ? OR TipoCliente = 0 OR 
        (TipoCliente = 2 AND bfCliente.Plazo > 0) OR 
        (TipoCliente = 1 AND bfCliente.Plazo = 0)) AND
        (ClaseCliente = 0 OR bfCliente.Id-ClaseCte = ClaseCliente) AND
        (Segmento = 0 OR bfCliente.Id-SegmentoCte = Segmento) 
        AND 
        (iGiro = 0 OR bfCliente.Id-Giro = iGiro) AND
        (iVendedor = "" OR bfCliente.Id-Vendedor = iVendedor) AND
        (
          /* Prioridad 1: Filtrar por Ciudad específica */
        (Ciudad > 0 AND bfCliente.Id-Ciudad = Ciudad) 
        OR
        /* Prioridad 2: Filtrar por Ciudades del Estado (si no hay Ciudad) */
        (Ciudad = 0 AND iEstado > "" AND LOOKUP(STRING(bfCliente.Id-Ciudad), l-Ciudades) > 0)
        OR
        /* Sin filtro de ubicación */
        (Ciudad = 0 AND iEstado = "")
        )
        AND (FechaInicio = ? OR bfCliente.FecReg >= FechaInicio)  /* Condiciones ajustadas para fechas */
        AND (FechaFin = ? OR bfCliente.FecReg <= FechaFin):          
            
        /* Buscar datos relacionados */
        FIND FIRST bfSegmentoCte WHERE bfSegmentoCte.Id-SegmentoCte = bfCliente.Id-SegmentoCte NO-LOCK NO-ERROR.
        FIND FIRST bfClaseCte    WHERE bfClaseCte.Id-ClaseCte = bfCliente.Id-ClaseCte NO-LOCK NO-ERROR.
        FIND FIRST bfCiudad      WHERE bfCiudad.Id-Ciudad = bfCliente.Id-Ciudad NO-LOCK NO-ERROR.
        FIND FIRST bfEstado      WHERE bfEstado.Id-Estado = bfCiudad.Id-Estado NO-LOCK NO-ERROR.
        FIND FIRST GiroCte       WHERE GiroCte.Id-Giro = bfCliente.Id-Giro NO-LOCK NO-ERROR.
        FIND FIRST Vendedor      WHERE Vendedor.Id-Vendedor = bfCliente.Id-Vendedor NO-LOCK NO-ERROR.
        IF AVAILABLE Vendedor THEN 
        DO:
            FIND FIRST Empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
        END.
        

        /* Crear registro en la tabla temporal */
        CREATE ttRepClientes.
        ASSIGN
            ttRepClientes.TipoCliente    = IF bfCliente.Plazo > 0 THEN "Credito" ELSE "Contado"
            ttRepClientes.IdCliente      = bfCliente.Id-Cliente
            ttRepClientes.RazonSocial    = bfCliente.RazonSocial
            ttRepClientes.RegimenCapital = bfCliente.RSocietario
            ttRepClientes.ClaseCliente   = IF AVAILABLE bfClaseCte THEN bfClaseCte.Descr ELSE "Sin asignar"
            ttRepClientes.Segmento       = IF AVAILABLE bfSegmentoCte THEN bfSegmentoCte.Descr ELSE "Sin asignar"
            ttRepClientes.FechaAlta      = bfCliente.FecReg
            ttRepClientes.Correo         = bfCliente.E-Mail
            ttRepClientes.Estado         = IF AVAILABLE bfEstado THEN bfEstado.Nombre ELSE " "
            ttRepClientes.Ciudad         = IF AVAILABLE bfCiudad THEN bfCiudad.Nombre ELSE " "
            ttRepClientes.Colonia        = bfcliente.Colonia
            ttRepClientes.CalleYNumero   = bfCliente.CalleNo
            ttRepClientes.Estatus        = IF bfCliente.Activo THEN "Activo" ELSE "Inactivo"
            ttRepClientes.IdVendedor     = bfCliente.Id-Vendedor
            ttRepClientes.NomVend        = ""
            ttRepClientes.IdGiro         = bfCliente.Id-Giro
            ttRepClientes.Giro           = IF AVAILABLE GiroCte THEN GiroCte.Descr ELSE "" .
            
        /* Asignar NomVend solo si existe el Vendedor */
        FIND FIRST Vendedor 
            WHERE Vendedor.Id-Vendedor = bfCliente.Id-Vendedor 
            NO-LOCK NO-ERROR.

        IF AVAILABLE Vendedor THEN 
        DO:
            IF Vendedor.Iniciales <> "" THEN
                ttRepClientes.NomVend = Vendedor.Iniciales.
            ELSE DO:
                ttRepClientes.NomVend = "".
            END.
        END.
            
            
    END.
    /* Liberar buffers al final */
    RELEASE bfCliente.        
RETURN.
END PROCEDURE.  
