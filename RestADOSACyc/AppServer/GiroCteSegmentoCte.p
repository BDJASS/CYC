@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Tablas SegmentoCte y SegmentoCte. SegmentoCteSegmentoCte.p
    Purpose     : /GiroCteSegmentoCte

    Syntax      : Se usa en Gestion de clientes

    Description : Al elegir un SegmentoCte, envia los tipos de SegmentoCte que hay.
                  Input recibe el IdSegmentoCteCte.
    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttSegmentoCte NO-UNDO
    FIELD IdSegmentoCte AS INTEGER           
    FIELD Descr         AS CHARACTER 
    INDEX idx-mc IdSegmentoCte ASCENDING.
DEFINE DATASET dsSegmentoCte FOR ttSegmentoCte.


/* **********************  Internal Procedures  *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetSegmentoCte:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER PIdSegmentoCte AS INTEGER.
    DEFINE OUTPUT PARAMETER TABLE FOR ttSegmentoCte.
    
    EMPTY TEMP-TABLE ttSegmentoCte.
    FOR EACH SegmentoCte WHERE SegmentoCte.Id-SegmentoCte = PIdSegmentoCte NO-LOCK :
      BUFFER-COPY SegmentoCte TO ttSegmentoCte.    
       ASSIGN 
         ttSegmentoCte.IdSegmentoCte = SegmentoCte.Id-SegmentoCte
         ttSegmentoCte.Descr         = SegmentoCte.Descr.  
         RELEASE ttSegmentoCte.   
    END. 
    RETURN. 
END PROCEDURE. 