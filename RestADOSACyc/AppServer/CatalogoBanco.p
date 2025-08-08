@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Catalogo Banco
    Purpose     : /Banco
    Syntax      : Se usa en Gestion de clientes Credito

    Nombreiption : 

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.      

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttBanco NO-UNDO
    FIELD IdBanco AS INTEGER           
    FIELD Nombre  AS CHARACTER 
    FIELD NomCto  AS CHARACTER 
    INDEX idx-mc  IdBanco ASCENDING  .

DEFINE DATASET dsBanco FOR ttBanco.


/* **********************  Internal Procedures  *********************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetBanco:
/*------------------------------------------------------------------------------
 Purpose:  Obtener información de bancos, con soporte para múltiples IDs
 Notes:    IdBanco es cadena con IDs separados por comas (ej: "1,2,3")
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttBanco.
    DEFINE INPUT PARAMETER IdBanco AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttBanco.
    
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBancoId AS INTEGER NO-UNDO.
    
    IF IdBanco = ? THEN IdBanco = "".
    
    /* Si IdBanco está vacío, devolver todos */
    IF IdBanco = "" OR IdBanco = ? THEN DO:
        FOR EACH Banco NO-LOCK:
            CREATE ttBanco.
            ASSIGN 
                ttBanco.IdBanco = Banco.Id-Banco
                ttBanco.Nombre  = Banco.Nombre
                ttBanco.NomCto  = Banco.NomCto.
        END.
    END.
    ELSE DO:
        /* Procesar múltiples IDs */
        DO i = 1 TO NUM-ENTRIES(IdBanco):
            iBancoId = INTEGER(ENTRY(i, IdBanco)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN NEXT.
            
            FIND FIRST Banco NO-LOCK 
                 WHERE Banco.Id-Banco = iBancoId NO-ERROR.
            
            IF AVAILABLE Banco THEN DO:
                CREATE ttBanco.
                ASSIGN 
                    ttBanco.IdBanco = Banco.Id-Banco
                    ttBanco.Nombre  = Banco.Nombre
                    ttBanco.NomCto  = Banco.NomCto.
            END.
        END.
    END.
END PROCEDURE.