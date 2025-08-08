@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : depositoestatus.p
    Purpose     : 

    Syntax      : /DepositoEstatus

    Description : Programa que recibe un RECID; 
                 Si al recibir y validar en Depbanco si viene Activo; se Inactiva.
                                                     si viene Inactivo; Se Activa.
                 Todo esto siempre y cuando sea un deposito pendiente por aplicar Conciliado = False.
                 La vista en donde se tienen los Depositos Activos/Inactivos 
                 la administra el front; 

    Author(s)   : sis10
    Created     : Wed Dec 11 20:56:19 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */
DEFINE TEMP-TABLE tt-DepBanco NO-UNDO
    FIELD Id-Banco    LIKE DepBanco.Id-Banco
    FIELD Id-Cliente  LIKE DepBanco.Id-Cliente
    FIELD Concepto    LIKE DepBanco.Concepto
    FIELD Importe     LIKE DepBanco.Importe
    FIELD Referencia  LIKE DepBanco.Referencia
    FIELD FechaDep    LIKE DepBanco.FecDep
    FIELD HoraDep     LIKE DepBanco.HoraDep
    FIELD Signo       LIKE DepBanco.Signo
    FIELD Saldo       LIKE DepBanco.Saldo
    FIELD Automatico  AS CHARACTER FORMAT "x(1)"
    FIELD TipoCte     LIKE DepBanco.TipoCte  
    FIELD Activo      LIKE DepBanco.Activo
    INDEX Idx-Dep Id-Cliente FechaDep DESC.
    
DEFINE BUFFER bf_DepBanco FOR DepBanco.
DEFINE VARIABLE l-recid AS RECID NO-UNDO.    
DEFINE VARIABLE l-hora  AS INTEGER NO-UNDO.

/* ***************************  Main Block  *************************** */


/*_ -----------------------------------------
    Procedure Borra Registro
----------------------------------------- _*/ 


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE p-Borra:
    DEFINE INPUT PARAMETER  Rec        AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER  IdUser     AS CHARACTER.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR.
    
    
    EMPTY TEMP-TABLE tt-DepBanco.
    
    FIND Usuario WHERE Usuario.Id-User = IdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN DO:
         ASSIGN Respuesta = "El usuario especificado no existe.".
         RETURN.
    END.
    
    FIND FIRST DepBanco WHERE RECID(DepBanco) = Rec EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE DepBanco THEN DO:
        ASSIGN Respuesta = "El registro no está disponible para actualización.".
        RETURN.
    END.
    IF DepBanco.Conciliado = TRUE THEN
    DO:
       ASSIGN Respuesta = "El deposito no es Pendiente por Aplicar".
        RETURN. 
    END.
    ELSE DO:
        CREATE tt-DepBanco.
        ASSIGN tt-Depbanco.Id-Banco   = DepBanco.Id-Banco
               tt-DepBanco.Id-Cliente = DepBanco.Id-Cliente
               tt-DepBanco.FechaDep     = DepBanco.FecDep
               tt-DepBanco.Importe    = DepBanco.Importe 
               tt-DepBanco.Referencia = DepBanco.Referencia
               tt-DepBanco.HoraDep    = DepBanco.HoraDep
               tt-DepBanco.TipoCte    = DepBanco.TipoCte
               tt-Depbanco.Activo     = DepBanco.Activo.      
    END.      

    
    FIND FIRST tt-DepBanco NO-LOCK NO-ERROR.             
    IF tt-DepBanco.Activo = TRUE THEN DO: /* Inactivar un Deposito */
        FIND FIRST bf_DepBanco WHERE bf_DepBanco.Id-Banco   = tt-DepBanco.Id-Banco AND
                                     bf_DepBanco.Id-Cliente = tt-DepBanco.Id-Cliente AND  
                                     bf_DepBanco.FecDep     = tt-DepBanco.FechaDep AND 
                                     bf_DepBanco.Importe    = tt-DepBanco.Importe AND
                                     bf_DepBanco.Referencia = tt-DepBanco.Referencia AND
                                     NOT bf_DepBanco.Conciliado
                                     EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bf_DepBanco AND bf_DepBanco.HoraDep = tt-DepBanco.HoraDep THEN 
        DO:
            ASSIGN 
                bf_DepBanco.Activo      = FALSE
                bf_DepBanco.Id-User     = IdUser
                bf_DepBanco.FecAplica   = DATETIME(TODAY, MTIME).
        END.        
        RELEASE bf_DepBanco.  
        ASSIGN Respuesta = "El deposito del Cliente " + STRING(tt-DepBanco.Id-Cliente) + 
                   " con fecha Deposito " + STRING(tt-DepBanco.FechaDep, "99/99/9999") + 
                   " por un importe de " + STRING(tt-DepBanco.Importe, "->,>>>,>>9.99") +
                   " fue eliminado exitosamente.".
                 
        DELETE tt-DepBanco.
       
        RETURN. 
    END.
    ELSE DO:  /* Activar un Deposito */
         FIND FIRST bf_DepBanco WHERE bf_DepBanco.Id-Banco   = tt-DepBanco.Id-Banco AND
                                     bf_DepBanco.Id-Cliente = tt-DepBanco.Id-Cliente AND  
                                     bf_DepBanco.FecDep     = tt-DepBanco.FechaDep AND 
                                     bf_DepBanco.Importe    = tt-DepBanco.Importe AND
                                     bf_DepBanco.Referencia = tt-DepBanco.Referencia AND
                                     NOT bf_DepBanco.Conciliado
                                     EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bf_DepBanco AND bf_DepBanco.HoraDep = tt-DepBanco.HoraDep THEN 
        DO:
            ASSIGN 
                bf_DepBanco.Activo      = TRUE
                bf_DepBanco.Id-User     = ""
                bf_DepBanco.FecAplica   = ?.
        END.        
        RELEASE bf_DepBanco. 
       ASSIGN Respuesta = "El deposito del Cliente " + STRING(tt-DepBanco.Id-Cliente) + 
                   " con fecha Deposito " + STRING(tt-DepBanco.FechaDep, "99/99/9999") + 
                   " por un importe de " + STRING(tt-DepBanco.Importe, "->,>>>,>>9.99") +
                   " fue restaurado exitosamente.".            
        DELETE tt-DepBanco.
    END.    
END PROCEDURE.