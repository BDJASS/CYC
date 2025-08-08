@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Catalogo GiroCte
    Purpose     : /GiroCte

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
DEFINE TEMP-TABLE ttGiroCte NO-UNDO
    FIELD IdGiro        AS INTEGER           
    FIELD Descr         AS CHARACTER 
    FIELD IdRamo        AS CHARACTER 
    FIELD IdSegmentoCte AS INTEGER    
    INDEX idx-mc  IdGiro ASCENDING  .

DEFINE DATASET dsGiroCte FOR ttGiroCte.


/* **********************  Internal Procedures  *********************** */


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetGiroCte:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttGiroCte.
    
    EMPTY TEMP-TABLE ttGiroCte.
    FOR EACH GiroCte NO-LOCK :
        BUFFER-COPY GiroCte TO ttGiroCte.    
        ASSIGN 
            ttGiroCte.IdGiro        = GiroCte.Id-Giro
            ttGiroCte.Descr         = GiroCte.Descr
            ttGiroCte.IdRamo        = GiroCte.Id-Ramo
            ttGiroCte.IdSegmentoCte = GiroCte.Id-SegmentoCte. 
        RELEASE ttGiroCte.
    END.

END PROCEDURE.

