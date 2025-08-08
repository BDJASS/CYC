@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").


/*------------------------------------------------------------------------
    File        : Catalogo RFiscal
    Purpose     : /RFiscal

    Syntax      : Se usa en Gestion de clientes

    Description : 

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttRFiscal NO-UNDO
    FIELD IdRFiscal AS CHARACTER           
    FIELD Descr     AS CHARACTER 
    INDEX idx-mc  IdRFiscal ASCENDING  .

DEFINE DATASET dsRFiscal FOR ttRFiscal.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRFiscal:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttRFiscal.
    
    EMPTY TEMP-TABLE ttRFiscal.
    FOR EACH RFiscal NO-LOCK :
        BUFFER-COPY RFiscal TO ttRFiscal.    
        ASSIGN 
            ttRFiscal.IdRFiscal = RFiscal.Id-RFiscal
            ttRFiscal.Descr     = RFiscal.Descr. 
        RELEASE ttRFiscal.
    END.

END PROCEDURE.

