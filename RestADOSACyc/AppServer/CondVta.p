@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : CondVta.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Tue Oct 15 16:44:55 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE TEMP-TABLE ttCondvta NO-UNDO
    FIELD IdCond      AS INTEGER   LABEL "Cond"          
    FIELD Descripcion AS CHARACTER LABEL "Descripcion" FORMAT "X(20)"
    FIELD Activo      AS LOGICAL   LABEL "Activo" FORMAT "S/N"
    FIELD FecVen      AS DATE      LABEL "FecVen" FORMAT 99/99/9999
    FIELD Usuario     AS CHARACTER LABEL "Usuario" FORMAT "X(10)"
    INDEX idx-act  Activo  ASCENDING IdCond ASCENDING 
    INDEX idx-cond IS PRIMARY UNIQUE IdCond ASCENDING .

DEFINE DATASET dsCondvta FOR ttCondvta.






/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCondVta:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE OUTPUT PARAMETER TABLE FOR ttCondvta.
    
    EMPTY TEMP-TABLE ttCondvta.
    FOR EACH Condvta NO-LOCK :
        BUFFER-COPY Condvta TO ttCondvta.    
        ASSIGN 
            ttCondvta.IdCond     = CondVta.Id-Cond
            ttCondvta.Descripcion = CondVta.Descr
            ttCondvta.Activo      = CondVta.Activo 
            ttCondvta.FecVen      = CondVta.FecVen
            ttCondvta.Usuario     = CondVta.Usuario. 
        RELEASE ttCondvta.
    END.
END PROCEDURE.

