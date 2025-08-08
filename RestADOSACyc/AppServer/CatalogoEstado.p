@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Catalogo Estado
    Purpose     : /Estado

    Syntax      : Se usa en Gestion de clientes

    Description : Solo Muestra Estados cuando se recibe el Id-Pais.

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttEstado NO-UNDO
    FIELD IdEstado AS CHARACTER           
    FIELD Nombre   AS CHARACTER 
    INDEX idx-mc IdEstado ASCENDING.

DEFINE DATASET dsEstado FOR ttEstado.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetEstado:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER PIdPais AS CHARACTER.
    DEFINE OUTPUT PARAMETER TABLE FOR ttEstado.
    
    EMPTY TEMP-TABLE ttEstado.
    FOR EACH Estado WHERE Estado.Id-Pais = PIdPais NO-LOCK :
      BUFFER-COPY Estado TO ttEstado.    
       ASSIGN 
         ttEstado.IdEstado = Estado.Id-Estado
         ttEstado.Nombre = Estado.Nombre. 
         RELEASE ttEstado. 
    END.
END PROCEDURE.

