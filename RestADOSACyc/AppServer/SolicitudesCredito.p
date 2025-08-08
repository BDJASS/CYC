@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : SolicitudesCredito.p   
    Purpose     : 

    Syntax      :

    Description : Lo usa Manuel en su Grid y Carlos en Reporte Solicitud Credito

    Author(s)   : sis10
    Created     : Tue Nov 26 04:42:53 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.  

/* ********************  Preprocessor Definitions  ******************** */
    /* Tabla temporal para la salida */
    DEFINE TEMP-TABLE ttSolicitudes NO-UNDO 
        FIELD IdEstatus          AS INTEGER    
        FIELD Estatus            LIKE EstSolCred.Descr
        FIELD IdSolicitud        AS INTEGER    
        FIELD FechaRegistro      AS DATE       
        FIELD RazonSocial        AS CHARACTER  
        FIELD Clase              AS CHAR
        FIELD SegmentoCliente    LIKE SegmentoCte.Descr  
        FIELD CreditoAutorizado  LIKE Cliente.Limite
        FIELD PlazoAutorizado    LIKE Cliente.Plazo
        FIELD FechaAlta          AS DATE 
        FIELD Ramo               LIKE SolCred.IdRamo 
        FIELD CreditoSolicitado  AS DECIMAL    
        FIELD PlazoSolicitado    AS INTEGER    
        FIELD Vendedor           AS CHARACTER  
        FIELD Observaciones      AS CHARACTER  
        FIELD PagareFirmado      AS LOGICAL  
        FIELD IdCliente          AS INT.       

/* ***************************  Main Block  *************************** */

    /* Buffer para la tabla persistente */
    DEFINE BUFFER bfSolCred FOR SolCred.
    
    
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarTodasSolicitudes:
/*--------------------------------------------------------------------------  
 Purpose     : Consultar todos los registros de la tabla SolCred.
 Notes       : Devuelve una tabla temporal con los campos especificados.
--------------------------------------------------------------------------*/
   /* Parámetros de entrada */
    DEFINE INPUT  PARAMETER FechaInicio AS DATE NO-UNDO .
    DEFINE INPUT  PARAMETER FechaFin    AS DATE NO-UNDO .
    DEFINE INPUT  PARAMETER IdEstatus   AS INTEGER NO-UNDO .
    DEFINE INPUT  PARAMETER Segmento    AS INTEGER NO-UNDO .
    DEFINE INPUT  PARAMETER iClase       AS INTEGER NO-UNDO .
   
    /* Parámetro de salida */
    DEFINE OUTPUT PARAMETER TABLE FOR ttSolicitudes.

    /* Inicializar la tabla temporal */
    EMPTY TEMP-TABLE ttSolicitudes.

    /* Recorrer todos los registros de la tabla SolCred */
     /* Recorrer registros de la tabla SolCred con filtros */
    FOR EACH bfSolCred NO-LOCK WHERE 
        (FechaInicio = ? OR bfSolCred.FecReg >= FechaInicio) AND
        (FechaFin = ? OR bfSolCred.FecReg <= FechaFin) AND
          (IdEstatus = ? OR IdEstatus = 0 OR bfSolCred.IdEstatus = IdEstatus) AND
    (Segmento = ? OR Segmento = 0 OR bfSolCred.IdSegmentoCte = Segmento)
     AND (iClase = ? OR iClase = 0 OR bfSolCred.IdClase = iClase):

       
      FIND FIRST EstSolCred WHERE EstSolCred.Id-EstSolCred = bfSolCred.IdEstatus NO-LOCK NO-ERROR.
      FIND FIRST Cliente WHERE Cliente.Id-Cliente = bfSolCred.IdCliente NO-LOCK NO-ERROR.
      FIND FIRST SegmentoCte WHERE SegmentoCte.Id-SegmentoCte = bfSolCred.IdSegmentoCte NO-LOCK NO-ERROR.
      FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = bfSolCred.Vendedor NO-LOCK NO-ERROR.
      FIND FIRST Empleado  WHERE Empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
      FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = bfSolCred.IdClase NO-LOCK NO-ERROR.
        CREATE ttSolicitudes.
        ASSIGN
            ttSolicitudes.IdSolicitud        = bfSolCred.IdSolicitud
            ttSolicitudes.FechaRegistro      = bfSolCred.FecReg
            ttSolicitudes.IdEstatus          = bfSolCred.IdEstatus
            ttSolicitudes.Estatus            = IF AVAILABLE EstSolCred THEN EstSolCred.Descr ELSE "Sin asignar"
            ttSolicitudes.RazonSocial        = bfSolCred.RazonSocial
            ttSolicitudes.Clase              = IF AVAILABLE ClaseCte THEN ClaseCte.Descr ELSE " "
            ttSolicitudes.Ramo               = bfSolCred.IdRamo
            ttSolicitudes.SegmentoCliente    = IF AVAILABLE SegmentoCte THEN SegmentoCte.Descr ELSE " "
            ttSolicitudes.CreditoSolicitado  = bfSolCred.CreditoSol
            ttSolicitudes.PlazoSolicitado    = bfSolCred.PlazoSol
            ttSolicitudes.Vendedor           = IF AVAILABLE Empleado THEN empleado.Nombre ELSE " "
            ttSolicitudes.Observaciones      = bfSolCred.Observaciones
            ttSolicitudes.PagareFirmado      = bfSolCred.PagareFirmado
            ttSolicitudes.CreditoAutorizado  = IF AVAILABLE Cliente THEN Cliente.Limite ELSE 0
            ttSolicitudes.PlazoAutorizado    = IF AVAILABLE Cliente THEN Cliente.Plazo  ELSE 0
            ttSolicitudes.FechaAlta          = bfSolCred.FecAlta
            ttSolicitudes.IdCliente          = bfSolCred.IdCliente.  
    END.

    /* Liberar buffers al final */
    RELEASE bfSolCred.
RETURN.
END PROCEDURE.
