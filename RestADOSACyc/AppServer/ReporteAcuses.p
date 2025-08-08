@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : reporteacuses.p
    Purpose     : 

    Syntax      : /Acuse aqui muestra acuses de tipo N que traen DocAcuse/PagoAcuse
    se omite lo de tipo A ; ya que ahi directo esta a nivel Acuse en /AcuseCte /Acuse 
    Description : 

    Author(s)   : sis10
    Created     : Mon Dec 16 02:11:54 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/* Tabla temporal para almacenar el resultado */
DEFINE TEMP-TABLE ttAcuse
    FIELD IdAcuse         AS CHARACTER
    FIELD IdCliente       AS INTEGER
    FIELD NumCliente      AS CHAR
    FIELD FecDep          AS DATE
    FIELD MontoPago       AS DECIMAL
    FIELD DescrTp         AS CHAR
    FIELD IdTp            AS INTEGER.
    
/* **********************  Internal Procedures  *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetAcuse:
    /*------------------------------------------------------------------------------
     Purpose: Obtener la información de un Acuse, junto con sus documentos y pagos.
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER v-fecini  AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER v-fecfin  AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER v-forma   AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttAcuse.
   
    /* Ajuste de parámetros */
    ASSIGN
        v-fecini = (IF v-fecini = ? THEN TODAY - 1 ELSE v-fecini)
        v-fecfin = (IF v-fecfin = ? THEN TODAY     ELSE v-fecfin)
        v-forma  = (IF v-forma  = ? THEN 0         ELSE v-forma). 
    
    /* Buffers */
    DEFINE BUFFER bfAcuse      FOR Acuse.
    DEFINE BUFFER bfPagoAcuse  FOR PagoAcuse.
    DEFINE BUFFER bfTipoPago   FOR TipoPago.
    DEFINE BUFFER bfCliente    FOR Cliente.
    
    FOR EACH bfAcuse 
        WHERE bfAcuse.FecDep >= v-fecini 
          AND bfAcuse.FecDep <= v-fecfin
          AND bfAcuse.Estatus = 4 
        NO-LOCK :

        /* Recorrer cada PagoAcuse relacionado */
        FOR EACH bfPagoAcuse 
            WHERE bfPagoAcuse.Id-Acuse = bfAcuse.Id-Acuse
            NO-LOCK:

            /* Aplicar filtro por forma de pago */
            IF v-forma <> 0 AND bfPagoAcuse.Id-Tp <> v-forma THEN 
                NEXT.  

            /* Obtener datos adicionales */
            FIND FIRST bfCliente NO-LOCK
                WHERE bfCliente.Id-Cliente = bfAcuse.Id-Cliente NO-ERROR.
            FIND FIRST bfTipoPago NO-LOCK
                WHERE bfTipoPago.Id-Tp = bfPagoAcuse.Id-Tp NO-ERROR.

            /* Crear registro en la tabla temporal */
            CREATE ttAcuse.
            ASSIGN
                ttAcuse.IdAcuse         = bfAcuse.Id-Acuse
                ttAcuse.IdCliente       = bfAcuse.Id-Cliente  
                ttAcuse.NumCliente      = IF AVAILABLE bfCliente THEN bfCliente.RazonSocial ELSE ""
                ttAcuse.FecDep          = bfAcuse.FecDep
                ttAcuse.MontoPago       = bfPagoAcuse.ImpRecibido
                ttAcuse.IdTp            = bfPagoAcuse.Id-Tp
                ttAcuse.DescrTp         = IF AVAILABLE bfTipoPago THEN bfTipoPago.Descr ELSE "".
        END.  
    END.
    RETURN.     
END PROCEDURE.    