@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

BLOCK-LEVEL ON ERROR UNDO, THROW.


/*  Nos basamos en este programa cxca0806.p 
    para cambiar cliente no debe estar Aplicado el Deposito Conciliado = false  */ 


/* Define buffers, variables y tablas temporales */
DEFINE BUFFER bf-Cliente  FOR Cliente.
DEFINE BUFFER bf-DepBanco FOR DepBanco.

DEFINE VARIABLE l-tpCte        AS INTEGER NO-UNDO.
DEFINE VARIABLE l-ClienteNuevo LIKE Cliente.Id-Cliente NO-UNDO.
DEFINE VARIABLE l-hora         AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tt-DepBanco LIKE DepBanco 
    FIELD RazonSocial LIKE Cliente.RazonSocial.

/* Procedimiento principal */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE p-CambiaCliente:

    /* Parámetros de entrada y salida */
    DEFINE INPUT PARAMETER ClienteNuevo LIKE Cliente.Id-Cliente NO-UNDO.
    DEFINE INPUT PARAMETER IdUsuario    LIKE Usuario.Id-User NO-UNDO.
    DEFINE INPUT PARAMETER Rec          AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER Nivel        AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta   AS CHARACTER.
     
    EMPTY TEMP-TABLE tt-DepBanco. 
    /* Validar Nivel */
    IF Nivel = ? THEN  
        ASSIGN Nivel = 0.

    /* Validar si el nivel es permitido (2 o 3) */ 
    IF Nivel <> 2 AND Nivel <> 3 THEN DO:
        ASSIGN Respuesta = "Usuario No Permitido para Cambiar Cliente.".
        RETURN.
    END.

    /* Validar existencia del cliente */
    FIND FIRST Cliente WHERE Cliente.Id-cliente = ClienteNuevo NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cliente THEN DO:
        ASSIGN Respuesta = "Cliente No Válido.".
        RETURN.
    END.

    /* Validar existencia del usuario */
    FIND FIRST Usuario WHERE Usuario.Id-User = IdUsuario NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN DO:
        ASSIGN Respuesta = "Usuario No Válido.".
        RETURN.
    END.

    /* Validar existencia del depósito */
    FIND FIRST DepBanco WHERE RECID(DepBanco) = Rec EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE DepBanco THEN DO:
        ASSIGN Respuesta = "El registro no está disponible para actualización.".
        RETURN.
    END.

    /* Verificar si el cliente ya está actualizado */
    IF DepBanco.Id-Cliente = ClienteNuevo THEN DO:
        ASSIGN Respuesta = "El cliente ya está actualizado con el nuevo valor.".
        RETURN.
    END.

    /* Crear un registro temporal del depósito */
    FIND FIRST DepBanco WHERE RECID(DepBanco) = Rec NO-LOCK NO-ERROR.
    IF AVAILABLE DepBanco THEN DO:
        CREATE tt-DepBanco.
        ASSIGN tt-DepBanco.Id-Banco   = DepBanco.Id-Banco
               tt-DepBanco.Id-Cliente = DepBanco.Id-Cliente
               tt-DepBanco.FecDep     = DepBanco.FecDep
               tt-DepBanco.Importe    = DepBanco.Importe
               tt-DepBanco.Referencia = DepBanco.Referencia
               tt-DepBanco.HoraDep    = DepBanco.HoraDep
               tt-DepBanco.TipoCte    = DepBanco.TipoCte.
    END.
    FIND FIRST tt-DepBanco NO-LOCK NO-ERROR.
    /********** Inicia la Actualización **********/
    IF AVAILABLE tt-DepBanco THEN DO:
        /* Determinar tipo de cliente */
        FIND FIRST bf-Cliente WHERE bf-Cliente.Id-Cliente = ClienteNuevo NO-LOCK NO-ERROR.
        IF AVAILABLE bf-Cliente THEN DO:
            IF bf-Cliente.Id-Cliente = 3 THEN 
                ASSIGN l-tpCte = 4. /* CONTADO */
            ELSE DO:
                FIND FIRST Zona OF Cliente NO-LOCK NO-ERROR.
                IF AVAILABLE Zona THEN DO:
                    IF Zona.Ubic = 1 THEN 
                        ASSIGN l-tpCte = 1. /* LOCAL */
                    ELSE 
                        ASSIGN l-tpCte = 2. /* FORÁNEO */
                    IF bf-Cliente.Id-Resp = 30 THEN 
                        ASSIGN l-tpCte = 3. /* CORPORATIVO */
                END.
            END.
        END. 

        /* Validar si el cliente es de crédito */
        FIND FIRST MovCliente WHERE MovCliente.id-cliente = ClienteNuevo NO-LOCK NO-ERROR.
        IF NOT AVAILABLE MovCliente THEN 
            ASSIGN l-tpCte = 4.
        ELSE DO:
            FIND FIRST MovCliente WHERE MovCliente.id-cliente = Cliente.Id-Cliente
                                    AND MovCliente.id-mc = 1
                                    AND MovCliente.saldo > 0
                                    NO-LOCK NO-ERROR.
            IF NOT AVAILABLE MovCliente AND Cliente.limite = 0 THEN 
                ASSIGN l-tpCte = 4.
        END.
        

        /* Validar duplicados antes de actualizar */
        FIND FIRST bf-DepBanco WHERE bf-DepBanco.Id-Banco   = tt-DepBanco.Id-Banco AND
                                     bf-DepBanco.Id-Cliente = tt-DepBanco.Id-Cliente AND
                                     bf-DepBanco.FecDep     = tt-DepBanco.FecDep AND
                                     bf-DepBanco.Importe    = tt-DepBanco.Importe AND
                                     bf-DepBanco.Referencia = tt-DepBanco.Referencia AND
                                     NOT bf-DepBanco.Conciliado
                                     EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bf-DepBanco AND bf-DepBanco.HoraDep = tt-DepBanco.HoraDep THEN DO:
            /* Validar si ya existe un registro con este cliente */
            FIND FIRST DepBanco WHERE DepBanco.Id-Banco   = tt-DepBanco.Id-Banco AND
                                      DepBanco.Id-Cliente = ClienteNuevo AND
                                      DepBanco.FecDep     = tt-DepBanco.FecDep AND
                                      DepBanco.Importe    = tt-DepBanco.Importe AND
                                      DepBanco.Referencia = tt-DepBanco.Referencia AND
                                      NOT DepBanco.Conciliado
                                      NO-LOCK NO-ERROR.
            IF AVAILABLE DepBanco THEN DO:
                ASSIGN Respuesta = "Error: Ya existe un registro previo con este cliente.".
                RETURN.
            END.

            /* Actualizar el cliente del depósito */
            ASSIGN bf-DepBanco.Activo = FALSE
                   bf-DepBanco.Id-User = CAPS(IdUsuario)
                   bf-DepBanco.FecAplica = DATETIME(TODAY, MTIME).

            BUFFER-COPY bf-DepBanco EXCEPT HoraDep TO DepBanco NO-ERROR.

            ASSIGN DepBanco.Id-Cliente = ClienteNuevo
                   DepBanco.HoraDep    = tt-DepBanco.HoraDep
                   DepBanco.Activo     = TRUE
                   DepBanco.Id-User    = ""
                   DepBanco.FecAplica  = ?
                   DepBanco.TipoCte    = l-tpCte
                   tt-DepBanco.Id-Cliente = ClienteNuevo
                   tt-DepBanco.RazonSocial = bf-Cliente.RazonSocial.
        END.
        RELEASE bf-DepBanco.
        RELEASE DepBanco.   

        /* Este mensaje es solo aviso, realiza el cambio */
        IF tt-DepBanco.TipoCte <> l-tpCte THEN DO:
            ASSIGN Respuesta = "Cliente modificado,se actualizara la informacion. El cliente asociado tiene un tipo de cliente diferente.".
            DELETE tt-DepBanco.
            RETURN.
        END.  
        ELSE DO:  
            ASSIGN Respuesta = "Cliente modificado,se actualizara la informacion.".
            RETURN.
        END.
    END.    

END PROCEDURE. 
