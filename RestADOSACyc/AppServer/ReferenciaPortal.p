@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
    Empresa : ADOSA
    Programa: ReferenciaPortal.p
    Fucnion : Consulta registro de referencia / Guarda registro de referencia
    Autor   : ALEX
    Fecha   : 2 de Diciembre DEL 2024
*/

DEFINE TEMP-TABLE ttDatos
    FIELD IdCliente LIKE RefPorta.Id-Cliente
    FIELD IdUser LIKE RefPortal.Id-User 
    FIELD IdFactura LIKE RefPorta.Id-Factura
    FIELD FecFac LIKE Factura.FecReg
    FIELD Cargo AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD Estatus LIKE RefPorta.Estatus
    FIELD Acuse LIKE RefPortal.Acuse
    FIELD FecReg LIKE RefPorta.FecReg.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetConsultaReferencia:
/*
    Empresa : ADOSA
    Programa: buscarelportal.p
    Fucnion : Busca relaciones de portales de clientes con facturas con adeudo
    Autor   : ALEX
    Fecha   : 2 de Diciembre DEL 2024
*/

DEFINE INPUT PARAMETER IpCliente LIKE Cliente.Id-Cliente NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttDatos.
DEFINE OUTPUT PARAMETER opRazonSocial LIKE Cliente.RazonSocial NO-UNDO.
DEFINE OUTPUT PARAMETER opError AS CHARACTER NO-UNDO.

FIND Cliente WHERE Cliente.Id-Cliente = ipCliente NO-LOCK NO-ERROR.
IF NOT AVAILABLE Cliente THEN DO:
    ASSIGN opError = "Cliente inexistente".
    RETURN.    
END.

ASSIGN opRazonSocial = Cliente.RazonSocial.

FOR EACH MovCliente WHERE MovCliente.Id-Cliente = ipCliente
                      AND MovCliente.Id-MC <= 3
                      AND MovCliente.Saldo <> 0 NO-LOCK,
    LAST Factura WHERE Factura.Id-Factura = MovCliente.RefSaldo NO-LOCK:
    
    FIND RefPortal WHERE RefPortal.Id-Cliente = MovCliente.Id-Cliente
                     AND RefPortal.Id-Factura = MovCliente.RefSaldo NO-LOCK NO-ERROR.
        
    CREATE ttDatos.
    ASSIGN ttDatos.IdCliente = MovCliente.Id-Cliente
           ttDatos.IdUser    = (IF AVAILABLE RefPortal THEN RefPortal.Id-User ELSE "")
           ttDatos.IdFactura = MovCliente.RefSaldo
           ttDatos.FecFac    = Factura.FecReg
           ttDatos.Cargo     = MovCliente.Saldo
           ttDatos.Estatus   = (IF AVAILABLE RefPortal THEN RefPortal.Estatus ELSE "")
           ttDatos.Acuse     = (IF AVAILABLE RefPortal THEN RefPortal.Acuse ELSE "")
           ttDatos.FecReg    = (IF AVAILABLE RefPortal THEN RefPortal.FecReg ELSE ?).
END.

RETURN.

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostGrabaReferencia:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*
    Empresa : ADOSA
    Programa: grabaportal.p
    Fucnion : Graba tabla RefPortal
    Autor   : ALEX
    Fecha   : 2 de Diciembre DEL 2024
*/

DEFINE INPUT PARAMETER TABLE FOR ttDatos.
DEFINE OUTPUT PARAMETER opMensaje AS CHARACTER NO-UNDO.

FOR EACH ttDatos NO-LOCK:
    FIND RefPortal WHERE RefPortal.Id-Cliente = ttDatos.IdCliente
                     AND RefPortal.Id-Factura = ttDatos.IdFactura EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE RefPortal THEN DO:
        CREATE RefPortal.
        ASSIGN RefPortal.Id-Cliente = ttDatos.IdCliente
               RefPortal.Id-Factura = ttDatos.IdFactura
               RefPorta.Id-User = ttDatos.IdUser
               RefPortal.FecReg = ttDatos.FecReg. 
    END.
    ASSIGN 
       RefPortal.Estatus = ttDatos.Estatus
       RefPortal.Acuse = ttDatos.Acuse.
END.

ASSIGN opMensaje = "Terminado".
RETURN. 
END PROCEDURE.


