@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
// Se utiliza para validar si existe un registo en la base de Datos
//    /ValidarCliente

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ValidarClienteExistente:
    /* Parámetros de entrada para validar */
    DEFINE INPUT  PARAMETER pcRazonSocial AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pcEmail       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pcRFC         AS CHARACTER NO-UNDO.
    
    /* Parámetro de salida con el mensaje */
    DEFINE OUTPUT PARAMETER pcMensaje     AS LONGCHAR  NO-UNDO.

    
    /* Validar Razon Social */
    IF pcRazonSocial > "" THEN DO:
        FIND FIRST Cliente WHERE Cliente.RazonSocial = pcRazonSocial NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN
            ASSIGN pcMensaje = "Cliente ya registrado con la razón social: " + pcRazonSocial.
    END.

    /* Validar Email */
    IF pcEmail > "" AND pcMensaje = "" THEN DO:
        FIND FIRST Cliente WHERE Cliente.e-mail = pcEmail NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN
            ASSIGN pcMensaje = "Cliente ya registrado con el email: " + pcEmail.
    END.

    /* Validar RFC */
    IF pcRFC > "" AND pcMensaje = "" THEN DO:
        FIND FIRST Cliente WHERE Cliente.RFC = pcRFC NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN
            ASSIGN pcMensaje = "Cliente ya registrado con el RFC: " + pcRFC.
    END.   
RETURN.
END PROCEDURE.  