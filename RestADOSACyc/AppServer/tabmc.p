@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : tabmc.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Wed Oct 16 15:36:44 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttTabmc NO-UNDO
    FIELD IdMC    AS INTEGER           
    FIELD Descr   AS CHARACTER 
    FIELD Sentido AS LOGICAL
    FIELD Tran    AS INTEGER      
    FIELD TipoMC  AS INTEGER 
    INDEX idx-mc IS PRIMARY UNIQUE IdMC ASCENDING  .

DEFINE DATASET dsTabmc FOR ttTabmc.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetTabMc:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttTabmc.
    
    EMPTY TEMP-TABLE ttTabmc.
    FOR EACH TabMc NO-LOCK :
        BUFFER-COPY TabMc TO ttTabmc.    
        ASSIGN 
            ttTabmc.IdMC    = TabMC.Id-MC
            ttTabmc.Descr   = TabMC.Descr
            ttTabmc.Sentido = TabMC.Sentido
            ttTabmc.Tran    = TabMC.Tran
            ttTabmc.TipoMC  = TabMC.TipoMC. 
        RELEASE ttTabmc.
    END.
RETURN.
END PROCEDURE.

