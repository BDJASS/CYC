@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cambiarasociado.p
    Purpose     : 

    Syntax      : /CambiarAsociado

    Description : Cambiar Asociado Modulo HU02 Depositos Pendientes por Aplicar

    Author(s)   : sis10
    Created     : Thu Dec 05 00:13:01 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */
DEFINE BUFFER bf-Cliente  FOR Cliente. 
DEFINE BUFFER bf-DepBanco FOR DepBanco.
DEFINE VARIABLE l-tpCte         AS INTEGER NO-UNDO.
DEFINE VARIABLE l-AsociadoNuevo LIKE Cliente.Id-Cliente NO-UNDO.
DEFINE VARIABLE l-hora          AS INTEGER NO-UNDO.
    
DEFINE TEMP-TABLE tt-DepBanco LIKE DepBanco
    FIELD RazonSocial LIKE Cliente.RazonSocial. 

/* ***************************  Main Block  *************************** */
/*_ -----------------------------------------
    Procedure Cambia Cliente
----------------------------------------- _*/ 
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").  
PROCEDURE p-CambiaAsociado: 
   
    DEFINE INPUT  PARAMETER AsociadoNuevo LIKE Cliente.Id-Cliente NO-UNDO.
    DEFINE INPUT  PARAMETER Rec           AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta     AS CHARACTER.  

    MESSAGE "Inicia procedimiento CambioAsociado AsociadoNuevo: " AsociadoNuevo SKIP 
        "Rec " Rec VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FIND FIRST DepBanco WHERE RECID(DepBanco) = Rec EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE DepBanco THEN 
    DO:
        ASSIGN 
            Respuesta = "El registro no está disponible para actualización.".
        MESSAGE Respuesta VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.  

    IF DepBanco.Id-Cliente = AsociadoNuevo THEN 
    DO:
        ASSIGN 
            Respuesta = "El Asociado ya está actualizado con el nuevo valor.".
        MESSAGE Respuesta VIEW-AS ALERT-BOX INFO.
        RETURN.
    END.

    MESSAGE "Creando temporal" VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FIND FIRST DepBanco WHERE RECID(DepBanco)= Rec NO-LOCK NO-ERROR.
    IF AVAILABLE DepBanco THEN 
    DO:
        CREATE tt-DepBanco.
        ASSIGN 
            tt-DepBanco.Id-Banco   = DepBanco.Id-Banco
            tt-DepBanco.Id-Cliente = DepBanco.Id-Cliente
            tt-DepBanco.FecDep     = DepBanco.FecDep
            tt-DepBanco.Importe    = DepBanco.Importe
            tt-DepBanco.Referencia = DepBanco.Referencia
            tt-DepBanco.HoraDep    = DepBanco.HoraDep
            tt-DepBanco.TipoCte    = DepBanco.TipoCte.
    END.

    MESSAGE "Temporal creada, buscando Asociado" VIEW-AS ALERT-BOX INFO.

    FIND FIRST Asociado WHERE 
        Asociado.Id-Cliente = tt-DepBanco.Id-Cliente AND 
        Asociado.Id-Asociado = AsociadoNuevo NO-LOCK NO-ERROR.

    IF NOT AVAILABLE Asociado THEN 
    DO:
       
        FIND FIRST AsocCred WHERE 
            AsocCred.Id-Cliente = tt-DepBanco.Id-Cliente AND 
            AsocCred.Id-Asociado = AsociadoNuevo NO-LOCK NO-ERROR.
        IF NOT AVAILABLE AsocCred THEN 
        DO:
            ASSIGN 
                Respuesta = "Asociado No Válido.".
            MESSAGE Respuesta VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END. 

    MESSAGE "Asociado válido. Buscando Cliente..." VIEW-AS ALERT-BOX INFO.

    FIND FIRST bf-Cliente WHERE bf-Cliente.Id-Cliente = AsociadoNuevo NO-LOCK NO-ERROR.
    IF AVAILABLE bf-Cliente THEN 
    DO:
        IF bf-Cliente.Id-Cliente = 3 THEN 
            l-tpCte = 4.
        ELSE 
        DO:
            FIND FIRST Zona OF bf-Cliente NO-LOCK NO-ERROR.
            IF AVAILABLE Zona THEN 
            DO:
                IF Zona.Ubic = 1 THEN l-tpCte = 1.
                ELSE IF Zona.Ubic <> 1 THEN l-tpCte = 2.
                IF bf-Cliente.Id-Resp = 30 THEN l-tpCte = 3.
            END.
        END.
    END.

    MESSAGE "Tipo de cliente calculado: " + STRING(l-tpCte) VIEW-AS ALERT-BOX INFO.

    FIND FIRST MovCliente WHERE MovCliente.id-cliente = AsociadoNuevo NO-LOCK NO-ERROR.
    IF NOT AVAILABLE MovCliente THEN
        l-tpCte = 4.
    ELSE 
    DO:
        FIND FIRST MovCliente WHERE 
            MovCliente.id-cliente = bf-Cliente.Id-Cliente AND
            MovCliente.id-mc = 1 AND
            MovCliente.saldo > 0 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE MovCliente AND bf-Cliente.limite = 0 THEN   
            l-tpCte = 4.
    END.

    MESSAGE "Validando tipo de cliente del temporal" VIEW-AS ALERT-BOX INFO.

    FIND FIRST tt-DepBanco NO-LOCK NO-ERROR.
    IF tt-DepBanco.TipoCte <> l-tpCte THEN 
    DO:
        ASSIGN 
            Respuesta = "El Asociado seleccionado tiene un tipo de cliente diferente.".
        DELETE tt-DepBanco.
        MESSAGE Respuesta VIEW-AS ALERT-BOX WARNING.
        RETURN.
    END.

    MESSAGE "Buscando bf-DepBanco para actualizar" VIEW-AS ALERT-BOX INFO.

    FIND FIRST bf-DepBanco WHERE 
        bf-DepBanco.Id-Banco   = tt-DepBanco.Id-Banco AND
        bf-DepBanco.Id-Cliente = tt-DepBanco.Id-Cliente AND  
        bf-DepBanco.FecDep     = tt-DepBanco.FecDep AND 
        bf-DepBanco.Importe    = tt-DepBanco.Importe AND
        bf-DepBanco.Referencia = tt-DepBanco.Referencia AND
        NOT bf-DepBanco.Conciliado
        EXCLUSIVE-LOCK NO-ERROR.

    IF AVAILABLE bf-DepBanco AND bf-DepBanco.HoraDep = tt-DepBanco.HoraDep THEN 
    DO:
        FIND FIRST DepBanco WHERE 
            DepBanco.Id-Banco   = tt-DepBanco.Id-Banco AND
            DepBanco.Id-Cliente = AsociadoNuevo AND  
            DepBanco.FecDep     = tt-DepBanco.FecDep AND 
            DepBanco.Importe    = tt-DepBanco.Importe AND
            DepBanco.Referencia = tt-DepBanco.Referencia AND
            NOT DepBanco.Conciliado
            NO-LOCK NO-ERROR.
        IF AVAILABLE DepBanco AND DepBanco.HoraDep = tt-DepBanco.HoraDep THEN 
        DO:
            ASSIGN 
                Respuesta = 'Error, NO se puede continuar. Ya existe un registro con este Asociado.'.
            MESSAGE Respuesta VIEW-AS ALERT-BOX ERROR.
            UNDO, NEXT.
        END.
        ELSE 
        DO:
            ASSIGN 
                bf-DepBanco.Activo    = FALSE
                bf-DepBanco.FecAplica = DATETIME(TODAY, MTIME).

            BUFFER-COPY bf-DepBanco EXCEPT HoraDep TO DepBanco NO-ERROR.

            ASSIGN 
                DepBanco.Id-Cliente     = AsociadoNuevo
                DepBanco.HoraDep        = tt-DepBanco.HoraDep
                DepBanco.Activo         = TRUE
                DepBanco.Id-User        = ''
                DepBanco.FecAplica      = ?
                DepBanco.TipoCte        = l-tpCte
                tt-DepBanco.Id-Cliente  = AsociadoNuevo
                tt-DepBanco.RazonSocial = bf-Cliente.RazonSocial.

            MESSAGE "Actualización realizada exitosamente." VIEW-AS ALERT-BOX INFO.
        END.
    END.

    RELEASE bf-DepBanco.
    RELEASE DepBanco.

    IF tt-DepBanco.TipoCte <> l-tpCte THEN 
    DO:
        ASSIGN 
            Respuesta = 'El Asociado seleccionado tiene un tipo de cliente diferente'.
        DELETE tt-DepBanco.
        MESSAGE Respuesta VIEW-AS ALERT-BOX WARNING.
    END.
    ELSE   
    DO:
        ASSIGN 
            Respuesta = 'Asociado modificado. Se actualizará la información.'.
        MESSAGE Respuesta VIEW-AS ALERT-BOX INFO.
    END.
END PROCEDURE.
    