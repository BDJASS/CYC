@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Catalogo Pais
    Purpose     : /Pais

    Syntax      : Se usa en Gestion de clientes

    Description : Solo Muestra Paises que tienen Estado y Municipio configurado

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttPais NO-UNDO
    FIELD IdPais AS CHARACTER           
    FIELD Nombre AS CHARACTER 
    INDEX idx-mc IdPais ASCENDING  .

DEFINE DATASET dsPais FOR ttPais.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetPais:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttPais.
    
    EMPTY TEMP-TABLE ttPais.
    FOR EACH Pais NO-LOCK :
        
     FIND FIRST Estado WHERE Estado.Id-Pais = Pais.Id-Pais NO-LOCK NO-ERROR.
     IF AVAILABLE Estado THEN 
     DO:
        FIND FIRST Ciudad WHERE Ciudad.Id-Estado = Estado.Id-Estado NO-LOCK NO-ERROR.
        IF AVAILABLE Ciudad THEN 
        DO:
          BUFFER-COPY Pais TO ttPais.    
          ASSIGN 
            ttPais.IdPais = Pais.Id-Pais
            ttPais.Nombre = Pais.Nombre. 
            RELEASE ttPais.    
        END.    
      END.
    END.
END PROCEDURE.

