@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
    Empresa : ADOSA
    Programa: conscliente.p
    Fucnion : Consulta de clientes
    Autor   : ALEX
    Fecha   : 6 de Diciembre DEL 2024
*/





/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetConsultaCte:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipCliente LIKE CLiente.Id-Cliente NO-UNDO.
DEFINE OUTPUT PARAMETER opRazonSocial LIKE Cliente.RazonSocial NO-UNDO.
DEFINE OUTPUT PARAMETER opDireccion AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opCobrador LIKE Cobrador.Id-Cobrador NO-UNDO.
DEFINE OUTPUT PARAMETER opInicialesCob AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opNombreCob AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opError AS CHARACTER NO-UNDO.

FIND Cliente WHERE Cliente.Id-Cliente = ipCliente 
               AND Cliente.Activo = TRUE NO-LOCK NO-ERROR.
IF NOT AVAILABLE Cliente THEN DO:
    ASSIGN opError = "Cliente inexistente o inactivo".
    RETURN.
END.

FIND Ciudad WHERE Ciudad.Id-Ciudad = Cliente.Id-Ciudad NO-LOCK NO-ERROR.
IF AVAILABLE Ciudad THEN 
    FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
    
ASSIGN opRazonSocial = Cliente.RazonSocial
       opCobrador = Cliente.Id-Cobrador
       opDireccion = TRIM(Cliente.CalleNo) + ", " +
                     TRIM(Cliente.Colonia) + ", " +
                     (IF AVAILABLE Ciudad THEN TRIM(Ciudad.Nombre) ELSE "") + ", " +
                     (IF AVAILABLE Estado THEN TRIM(Estado.Nombre) ELSE "") + ", " +
                     Cliente.CP.
       
FIND Cobrador WHERE Cobrador.Id-Cobrador = Cliente.Id-Cobrador NO-LOCK NO-ERROR.
IF AVAILABLE Cobrador THEN 
    ASSIGN opInicialesCob = Cobrador.Iniciales
           opNombreCob = Cobrador.Nombre.
    
RETURN.
END PROCEDURE.
