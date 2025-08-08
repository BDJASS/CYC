@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Tablas Ramo y GiroCte. Ramogirocte.p
    Purpose     : /RamoGiroCte

    Syntax      : Se usa en Gestion de clientes

    Description : Al elegir un Ramo, envia los tipos de GiroCte que hay.
                  Input recibe el IdRamo.
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
    INDEX idx-mc IdGiro ASCENDING.
DEFINE DATASET dsGiroCte FOR ttGiroCte.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetGiroCte:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER PIdRamo AS CHARACTER.
    DEFINE OUTPUT PARAMETER TABLE FOR ttGiroCte.
    
    EMPTY TEMP-TABLE ttGiroCte.
    FOR EACH GiroCte WHERE GiroCte.Id-Ramo = PIdRamo NO-LOCK :
      BUFFER-COPY GiroCte TO ttGiroCte.    
       ASSIGN 
         ttGiroCte.IdGiro        = GiroCte.Id-Giro
         ttGiroCte.Descr         = GiroCte.Descr
         ttGiroCte.IdRamo        = GiroCte.Id-Ramo
         ttGiroCte.IdSegmentoCte = GiroCte.Id-SegmentoCte.  
         RELEASE ttGiroCte.   
    END. 
    RETURN.
END PROCEDURE. 