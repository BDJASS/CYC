/*------------------------------------------------------------------------
    File        : ClienteVer.p
    Purpose     : Obtener datos de un cliente por ID.
    Syntax      :
    Description : Devuelve los datos de un cliente en la tabla temporal ttCliente.
    Author(s)   : sis10
    Created     : Tue Nov 26 08:01:55 CST 2024
    Notes       : Diseñado para ser expuesto como servicio REST.
------------------------------------------------------------------------*/

/* ***************************  Definitions ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttCliente NO-UNDO
    FIELD IdCliente       AS INTEGER
    FIELD RFC             AS CHARACTER
    FIELD RazonSocial     AS CHARACTER
    FIELD RegimenCapital  AS CHARACTER
    FIELD NombreComercial AS CHARACTER
    FIELD Propietario     AS CHARACTER
    FIELD Curp            AS CHARACTER
    FIELD Email           AS CHARACTER
    FIELD Telefono        AS CHARACTER
    FIELD BuzonFiscal     AS CHARACTER
    FIELD IdRFiscal       AS CHARACTER
    FIELD IdUsoCFDI       AS CHARACTER
    FIELD IdRamo          AS CHARACTER
    FIELD IdGiro          AS INTEGER
    FIELD IdSegmentoCte   AS INTEGER
    FIELD IdClase         AS INTEGER
    FIELD IdCiudad        AS INTEGER
    FIELD CP              AS CHARACTER
    FIELD Colonia         AS CHARACTER
    FIELD Calle           AS CHARACTER
    FIELD NumExterior     AS CHARACTER
    FIELD NumInterior     AS CHARACTER.

/* ***************************  Main Block *************************** */

PROCEDURE GetClienteById:

    DEFINE INPUT  PARAMETER piIdCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCliente.

    EMPTY TEMP-TABLE ttCliente.

    /* Buscar cliente por ID */
    FIND FIRST Cliente WHERE Cliente.Id-Cliente = piIdCliente NO-LOCK NO-ERROR.
    
    IF AVAILABLE Cliente THEN DO:
        /* Poblar la tabla temporal con los datos del cliente */
        CREATE ttCliente.
        ASSIGN
            ttCliente.IdCliente       = Cliente.Id-Cliente
            ttCliente.RFC             = Cliente.RFC
            ttCliente.RazonSocial     = Cliente.RazonSocial
            ttCliente.RegimenCapital  = Cliente.RSocietario
            ttCliente.NombreComercial = Cliente.NomEmpresa
            ttCliente.Propietario     = Cliente.Propietario
            ttCliente.Curp            = Cliente.Curp
            ttCliente.Email           = Cliente.e-mail
            ttCliente.Telefono        = Cliente.Tel1
            ttCliente.BuzonFiscal     = Cliente.BuzonFiscal
            ttCliente.IdRFiscal       = Cliente.Id-RFiscal
            ttCliente.IdUsoCFDI       = Cliente.Id-UsoCFDI
            ttCliente.IdRamo          = Cliente.Id-Ramo
            ttCliente.IdGiro          = Cliente.Id-Giro
            ttCliente.IdSegmentoCte   = Cliente.Id-SegmentoCte
            ttCliente.IdClase         = Cliente.Id-ClaseCte
            ttCliente.IdCiudad        = Cliente.Id-Ciudad
            ttCliente.CP              = Cliente.CP
            ttCliente.Colonia         = Cliente.Colonia
            ttCliente.Calle           = Cliente.Calle
            ttCliente.NumExterior     = Cliente.NumExt
            ttCliente.NumInterior     = Cliente.NumInt.   
    END.
END PROCEDURE.


