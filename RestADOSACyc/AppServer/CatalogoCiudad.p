@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Catalogo Ciudad
    Purpose     : /Ciudad

    Syntax      : Se usa en Gestion de clientes

    Description : Solo Muestra Ciudad cuando se recibe el Id-Estado.

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttCiudad NO-UNDO
    FIELD IdCiudad AS INTEGER           
    FIELD Nombre   AS CHARACTER 
    INDEX idx-mc IdCiudad ASCENDING.

DEFINE DATASET dsCiudad FOR ttCiudad.


/* **********************  Internal Procedures  *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCiudad:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER PIdEstado AS CHARACTER.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCiudad.
    
    EMPTY TEMP-TABLE ttCiudad.
    FOR EACH Ciudad WHERE  (PIdEstado = ? OR Ciudad.Id-Estado = PIdEstado) NO-LOCK :
      BUFFER-COPY Ciudad TO ttCiudad.    
       ASSIGN 
         ttCiudad.IdCiudad = Ciudad.Id-Ciudad
         ttCiudad.Nombre   = Ciudad.Nombre. 
         RELEASE ttCiudad. 
    END.
END PROCEDURE.

