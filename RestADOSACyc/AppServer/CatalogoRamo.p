@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Catalogo Ramo
    Purpose     : /Ramo
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
DEFINE TEMP-TABLE ttRamo NO-UNDO
    FIELD IdRamo AS CHARACTER           
    FIELD Descr  AS CHARACTER 
    INDEX idx-mc  IdRamo ASCENDING  .

DEFINE DATASET dsRamo FOR ttRamo.


/* **********************  Internal Procedures  *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRamo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttRamo.
    
    EMPTY TEMP-TABLE ttRamo.
    FOR EACH Ramo NO-LOCK :
        BUFFER-COPY Ramo TO ttRamo.    
        ASSIGN 
            ttRamo.IdRamo = Ramo.Id-Ramo
            ttRamo.Descr  = Ramo.Descr. 
        RELEASE ttRamo.
    END.

END PROCEDURE.

