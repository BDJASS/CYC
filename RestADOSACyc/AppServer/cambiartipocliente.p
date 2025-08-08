@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cambiartipocliente.p
    Purpose     : 

    Syntax      : /TipoCliente 

    Description : GET y PUT para HU02 Depositos Pendientes por Aplicar

    Author(s)   : sis10
    Created     : Thu Dec 05 07:45:12 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/
/*
  Ticket 1115 No permite cambiar depositos a algun tipo;
              en pantalla N tiene un mensaje Undo,Next y se paso
              aca como Return entonces no avanzaba, es solo de aviso.
              JASS02072025


*/
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    DEFINE BUFFER bf-Cliente  FOR Cliente.
    DEFINE BUFFER bf-DepBanco FOR DepBanco.
    DEFINE VARIABLE l-tpCte AS INTEGER NO-UNDO.
    DEFINE VARIABLE l-ClienteNuevo LIKE Cliente.Id-Cliente NO-UNDO.
    DEFINE VARIABLE l-hora  AS INTEGER NO-UNDO.   
     
    DEFINE TEMP-TABLE tt-DepBanco LIKE DepBanco
    FIELD RazonSocial LIKE Cliente.RazonSocial.

DEFINE TEMP-TABLE ttdatos NO-UNDO
    FIELD id       AS INTEGER
    FIELD nombre   AS CHARACTER.

DEFINE TEMP-TABLE TipoCliente NO-UNDO
    FIELD IdTipoCliente       AS INTEGER
    FIELD NombreTipoCliente   AS CHARACTER.

/* **********************  Internal Procedures  *********************** */


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE CambiaTipoCliente:    
   DEFINE INPUT PARAMETER IdTipoCliente AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER Rec           AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER Nivel         AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER Respuesta    AS CHARACTER.  
   
    EMPTY TEMP-TABLE tt-DepBanco.
   
    /* Validar el Nivel */
    IF Nivel = ? THEN 
        ASSIGN Nivel = 0.


    /* Validar si el nivel es permitido (2 o 3) */ 
    IF Nivel <> 2 AND Nivel <> 3 THEN DO:
        ASSIGN Respuesta = "Usuario No Permitido para Cambiar Tipo Cliente.".
        RETURN.
    END.      
    
    IF IdTipoCliente >= 1 AND  IdTipoCliente <= 4 THEN DO:
       /* Continúa con la lógica del cambio */
      //  ASSIGN Respuesta = "Cambio de Tipo de cliente permitido.". 
    END.
    ELSE DO:
       ASSIGN Respuesta = "Tipo de Cliente no Valido.".
       RETURN.  
    END. 
    
    FIND FIRST DepBanco WHERE RECID(DepBanco) = Rec EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE DepBanco THEN DO:
        ASSIGN Respuesta = "El registro no está disponible para actualización.".
        RETURN.
    END.
     
    FIND FIRST Depbanco WHERE RECID(DepBanco)= Rec NO-LOCK NO-ERROR.
    IF AVAILABLE DepBanco THEN DO:
        CREATE tt-DepBanco.
        ASSIGN tt-Depbanco.Id-Banco   = DepBanco.Id-Banco
               tt-DepBanco.Id-Cliente = DepBanco.Id-Cliente
               tt-DepBanco.FecDep     = DepBanco.FecDep
               tt-DepBanco.Importe    = DepBanco.Importe 
               tt-DepBanco.Referencia = DepBanco.Referencia
               tt-DepBanco.HoraDep    = DepBanco.HoraDep
               tt-DepBanco.TipoCte    = DepBanco.TipoCte.
     END.
    
    FIND FIRST tt-DepBanco NO-LOCK NO-ERROR.       
    /********* Inicial la actualizacion *******/ 
    
    IF AVAILABLE tt-DepBanco THEN DO:
        FIND FIRST bf-Cliente WHERE bf-Cliente.Id-Cliente = tt-DepBanco.Id-Cliente NO-LOCK NO-ERROR.    // Mismo Cliente que tengo
        IF AVAILABLE bf-Cliente THEN DO:
            // Valido si quiero cambiar a cliente corporativo y si, pero el responsable NO es 30 marca error.
            IF IdTipoCliente = 3 AND bf-Cliente.Id-Resp <> 30 THEN DO:
                ASSIGN Respuesta = "El cliente NO califica para ser corporativo,revise la informacion del responsable asignado".
               // RETURN.   JASS02072025
            END.
            // Valido si cliente tiene movimientos de credito
            IF  IdTipoCliente < 3 THEN DO:
                // 2020-01-13 - Update de Reviso si el cliente es de cr�dito para reasignar su TipoCliente
                FIND FIRST MovCliente WHERE MovCliente.id-cliente = bf-Cliente.Id-Cliente NO-LOCK NO-ERROR.
                IF (NOT AVAILABLE MovCliente) THEN l-tpCte = 4.                
                ELSE DO:
                    FIND FIRST MovCliente WHERE MovCliente.id-cliente = bf-Cliente.Id-Cliente
                                            AND MovCliente.id-mc = 1
                                            AND MovCliente.saldo > 0
                                            NO-LOCK NO-ERROR.
                    IF (NOT AVAILABLE MovCliente AND bf-Cliente.limite = 0) THEN l-tpCte = 4.                
                END.
                IF l-tpCte = 4 THEN DO:
                    ASSIGN Respuesta ="El cliente NO tiene movimientos de CREDITO,revise la informacion".
                   // RETURN.    JASS02072025
                END.
            END.
        END.
            
        l-hora =(tt-DepBanco.HoraDep). 
        // Busco el registro de la tabla temporal para actualizar el cliente
        FIND FIRST bf-DepBanco WHERE bf-DepBanco.Id-Banco   = tt-DepBanco.Id-Banco AND
                                     bf-DepBanco.Id-Cliente = tt-DepBanco.Id-Cliente AND  
                                     bf-DepBanco.FecDep     = tt-DepBanco.FecDep AND 
                                     bf-DepBanco.Importe    = tt-DepBanco.Importe AND
                                     bf-DepBanco.Referencia = tt-DepBanco.Referencia AND
                                     NOT bf-DepBanco.Conciliado
                                     EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bf-DepBanco AND bf-DepBanco.HoraDep = l-hora THEN DO:
            ASSIGN 
                bf-DepBanco.TipoCte     = IdTipoCliente.
        
          RELEASE bf-DepBanco.
          ASSIGN Respuesta ='Cliente modificado.Se actualizara la informacion. '.
          DELETE tt-DepBanco.
        END.
        ELSE DO:
            ASSIGN Respuesta = "El registro ya ha sido modificado o la hora del depósito ha cambiado.".
            RETURN.  
        END.    
    END.   
END PROCEDURE. 

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetMostrarTipoCliente:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER IdTipoCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR TipoCliente.

    /* Cargar los datos iniciales */
    CREATE ttdatos.
    ASSIGN ttdatos.id = 1 ttdatos.nombre = "Local".

    CREATE ttdatos.
    ASSIGN ttdatos.id = 2 ttdatos.nombre = "Foraneo".

    CREATE ttdatos.
    ASSIGN ttdatos.id = 3 ttdatos.nombre = "Corporativo".

    CREATE ttdatos.
    ASSIGN ttdatos.id = 4 ttdatos.nombre = "Contado".

    /* Filtrar y llenar la tabla de salida */
    FOR EACH ttdatos WHERE ttdatos.id <> IdTipoCliente BY ttdatos.id:
        CREATE TipoCliente.
        ASSIGN TipoCliente.IdTipoCliente     = ttdatos.id
               TipoCliente.NombreTipoCliente = ttdatos.nombre.
    END.
END PROCEDURE.
