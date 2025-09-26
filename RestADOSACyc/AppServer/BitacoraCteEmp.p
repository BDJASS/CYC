@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : BitacoraCambioCte.p
    Purpose     : 

    Syntax      :/BitacoraCambioCte

    Description : 

    Author(s)   : sis10
    Created     : Sat Sep 20 18:38:17 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/
/*
  Empresa : Consutoria en Informatica Ejecutiva S.A. de C.V.
  Modulo  : Cuentas por Pagar
  Sistema : ADOSA
  Programa: cxcb0045.p
  Llamador:
  Funcion : Consulta de cambios en el Archivo de Clientes
  Autor   : LUIS
  Fecha   : 01/04/97
*/
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttCambioCte NO-UNDO
    FIELD IdCliente    AS INTEGER  
    FIELD FecReg       AS DATE 
    FIELD Hora         AS CHARACTER 
    FIELD Campo        AS CHARACTER        
    FIELD IdUser       AS CHARACTER
    FIELD ValorNuevo   AS CHARACTER
    FIELD ValorAntiguo AS CHARACTER
    INDEX idx-mc FecReg DESCENDING  .

DEFINE VARIABLE l-cliente  LIKE MovCliente.Id-Cliente NO-UNDO.
DEFINE VARIABLE l-hora     AS CHARACTER.
DEFINE VARIABLE l-importa  AS INTEGER.
DEFINE VARIABLE l-User     LIKE Usuario.Id-User NO-UNDO.
DEFINE VARIABLE l-opciones AS CHARACTER EXTENT 3 FORMAT "x(30)" INITIAL 
    ["Cambios a Clientes","Autorizaciones a Clientes","Cambios a Clientes Por Usuario"].
        
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetBitacora:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER IdCliente AS INTEGER.
    DEFINE INPUT PARAMETER l-todos AS LOGICAL.  
    DEFINE OUTPUT PARAMETER TABLE FOR ttCambioCte.
    
    EMPTY TEMP-TABLE ttCambioCte.
    IF l-todos = ? THEN l-todos = TRUE.
    
    IF l-todos THEN ASSIGN l-importa = 0. 
    ELSE ASSIGN l-importa = 500.
    
    FOR EACH CambioCte WHERE CambioCte.Id-Cliente = IdCliente AND
        CambioCte.Campo >= l-importa NO-LOCK
        USE-INDEX Idx-FecReg:
        CREATE ttCambioCte.
        ASSIGN 
            ttCambioCte.IdCliente    = CambioCte.Id-Cliente
            ttCambioCte.FecReg       = CambioCte.FecReg
            ttCambioCte.Hora         = STRING(CambioCte.Hora,"HH:MM:SS") 
            ttCambioCte.Campo        = CambioCte.Descr
            ttCambioCte.ValorNuevo   = CambioCte.ValorNuevo
            ttCambioCte.ValorAntiguo = CambioCte.ValorOld
            ttCambioCte.IdUser       = adosa.CambioCte.Id-User.                                              
                                                     
    END.
    RETURN.      
END PROCEDURE.     
    