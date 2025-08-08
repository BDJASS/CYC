@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Catalogo UsoCFDI
    Purpose     : /UsoCFDI

    Syntax      : Se usa en Registro Clientes Credito

    Description : 

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttUsoCFDI NO-UNDO
    FIELD IdUsoCFDI      AS CHARACTER          
    FIELD Descr          AS CHARACTER 
    FIELD ListaRFiscal   AS CHARACTER
    FIELD Activo         AS LOGICAL
    INDEX idx-mc IdUsoCFDI ASCENDING.

DEFINE DATASET dsUsoCFDI FOR ttUsoCFDI.


/* **********************  Internal Procedures  *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetUsoCFDI:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttUsoCFDI.
    
    EMPTY TEMP-TABLE ttUsoCFDI.
    FOR EACH UsoCFDI  NO-LOCK :
      BUFFER-COPY UsoCFDI TO ttUsoCFDI.    
       ASSIGN 
         ttUsoCFDI.IdUsoCFDI    = UsoCFDI.Id-UsoCFDI
         ttUsoCFDI.Descr        = UsoCFDI.Descr
         ttUsoCFDI.ListaRFiscal = UsoCFDI.ListaRFiscal
         ttUsoCFDI.Activo       = UsoCFDI.Activo.  
         RELEASE ttUsoCFDI. 
    END.
END PROCEDURE.

