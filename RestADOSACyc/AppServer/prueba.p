@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : prueba.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Fri Oct 25 16:41:53 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttUsuario BEFORE-TABLE bttUsuario
    FIELD IdUser AS CHARACTER
    FIELD NomUsuario AS CHARACTER 
    FIELD IdCaja AS CHARACTER 
    FIELD IdUbicacion AS CHARACTER 
    FIELD TipoUsuario AS CHARACTER 
    FIELD hisPassword AS CHARACTER 
    FIELD IdFuncion AS CHARACTER 
    FIELD Sistemas AS LOGICAL INITIAL "no" 
    FIELD Facultado AS LOGICAL INITIAL "no" 
    FIELD Foraneo AS LOGICAL INITIAL "no" 
    FIELD Clave AS INTEGER INITIAL "0" 
    FIELD Nivel AS INTEGER INITIAL "0" 
    FIELD Firma AS CHARACTER 
    FIELD Depto AS CHARACTER 
    FIELD Telefono AS CHARACTER 
    FIELD NumMenu AS INTEGER INITIAL "0" 
    FIELD IdSup AS INTEGER INITIAL "0" 
    FIELD IdCajero AS INTEGER INITIAL "0" 
    FIELD IdUbivta AS CHARACTER 
    FIELD Email AS CHARACTER 
    FIELD Email2 AS CHARACTER 
    FIELD IdSeguro AS CHARACTER 
    FIELD IdVendedor AS CHARACTER 
    FIELD FUG AS LOGICAL INITIAL "YES"
    FIELD IdClase AS INTEGER
    FIELD IdModulo AS INTEGER
    FIELD IdRol AS INTEGER
    INDEX Idx-Clave IS  UNIQUE  Clave  ASCENDING. 

DEFINE DATASET dsUsuario FOR ttUsuario.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetPrueba:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER Id-User      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttUsuario.
    
    
    IF Id-User = "" OR Id-User = ? THEN UNDO, THROW NEW Progress.Lang.AppError("El ID de usuario no se mando o esta en blanco.").
  
    IF SETUSERID(Id-User,"sistema","DICTDB") <> TRUE THEN DO:
        UNDO, THROW NEW Progress.Lang.AppError("El Userid no esta registrado en el sistema.").
    END.

    
    EMPTY TEMP-TABLE ttUsuario.

    
     EMPTY TEMP-TABLE ttUsuario.
     FOR EACH Usuario WHERE Usuario.Id-User = Id-User NO-LOCK:
         BUFFER-COPY Usuario TO ttUsuario.    
         ASSIGN ttUsuario.IdUser      = Usuario.Id-User
               ttUsuario.NomUsuario  = Usuario.Nom-Usuario
               ttUsuario.IdCaja      = Usuario.Id-Caja
               ttUsuario.IdUbicacion = Usuario.Id-Ubicacion
               ttUsuario.TipoUsuario = Usuario.Tipo-Usuario
               ttUsuario.hisPassword = Usuario.his-password
               ttUsuario.IdSup       = Usuario.Id-sup
               ttUsuario.IdCajero    = Usuario.Id-Cajero
               ttUsuario.IdUbivta    = Usuario.id-ubivta
               ttUsuario.Email       = Usuario.e-mail
               ttUsuario.Email2      = Usuario.e-mail2
               ttUsuario.IdSeguro    = Usuario.id-seguro
               ttUsuario.IdClase     = Usuario.Id-ClaseCte
               ttUsuario.IdModulo    = Usuario.Id-Modulo
               ttUsuario.IdRol       = Usuario.Id-Rol.  
               RELEASE ttUsuario.    
     END.
END PROCEDURE.

