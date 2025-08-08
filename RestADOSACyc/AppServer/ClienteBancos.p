@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*--------------------------------------------------------------------------  
 File        : ClienteBancos.p
 Purpose     : 
    - POST: Asignar hasta tres bancos a un cliente existente en la tabla Cliente.
    - GET: Consultar los bancos registrados para un cliente específico.
 Notes       : 
    - Valida la existencia del cliente en base a IdCliente.
    - Registra hasta tres bancos con información adicional.
    - Devuelve los bancos asignados para un cliente específico.
 Author(s)   : sis10
 Created     : (Coloca la fecha de creación aquí)
--------------------------------------------------------------------------*/

/* ***************************  Definitions ************************** */

/* Tabla temporal para entrada (POST) */
DEFINE TEMP-TABLE ttBancoInfo NO-UNDO
    FIELD IdCliente      AS INTEGER
    FIELD IdBanco        AS INTEGER
    FIELD CuentaBancaria AS CHARACTER
    FIELD Bloqueado      AS LOGICAL. /* True o False */ 


/* Tabla temporal para salida en GET */
DEFINE TEMP-TABLE ttBancoCliente NO-UNDO
    FIELD IdBanco        AS INTEGER
    FIELD CuentaBancaria AS CHARACTER
    FIELD Bloqueado      AS LOGICAL /* True o False */
    FIELD IdCliente      LIKE Cliente.Id-Cliente. 

/* Buffers */
DEFINE BUFFER bfCliente FOR Cliente.
DEFINE BUFFER bfBanco   FOR Banco.

/* ***************************  Main Block **************************** */

/* Procedimiento POST: Asignar Bancos */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE AsignarBancosCliente:
    /*------------------------------------------------------------------------------ 
     Purpose: Asignar hasta tres bancos a un cliente existente en la tabla Cliente.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER TABLE FOR ttBancoInfo.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.

    /* Validar que se envíen registros */
    IF NOT CAN-FIND(FIRST ttBancoInfo) THEN DO:
           ASSIGN l-Mensaje = "Error: No se enviaron registros para procesar.".
        RETURN.
    END.
    
    FIND FIRST ttBancoInfo NO-LOCK NO-ERROR.
    /* Buscar el cliente */
    FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttBancoInfo.IdCliente EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bfCliente THEN DO:
            ASSIGN l-Mensaje = "Error: Cliente no encontrado.".
        RETURN.
    END.

    /* Procesar cada banco recibido */
    DEFINE VARIABLE BancoAsignado AS INTEGER NO-UNDO.
    ASSIGN BancoAsignado = 1. /* Inicia con el primer banco */

    FOR EACH ttBancoInfo WHERE ttBancoInfo.IdCliente = bfCliente.Id-Cliente:
        /* Validar que el banco exista */
        FIND FIRST bfBanco WHERE bfBanco.Id-Banco = ttBancoInfo.IdBanco NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfBanco THEN DO:
                ASSIGN l-Mensaje = "Error: El banco " + STRING(ttBancoInfo.IdBanco) + " no existe.".
            RETURN.
        END.

        /* Asignar el banco al cliente */
        CASE BancoAsignado:
            WHEN 1 THEN 
                ASSIGN bfCliente.Id-Banco1  = ttBancoInfo.IdBanco
                       bfCliente.Ctcheq1    = ttBancoInfo.CuentaBancaria
                       bfCliente.Blk1       = ttBancoInfo.Bloqueado.
            WHEN 2 THEN 
                ASSIGN bfCliente.Id-Banco2 = ttBancoInfo.IdBanco
                       bfCliente.Ctacheq2  = ttBancoInfo.CuentaBancaria
                       bfCliente.Blk2      = ttBancoInfo.Bloqueado.
            WHEN 3 THEN 
                ASSIGN bfCliente.Id-Banco3 = ttBancoInfo.IdBanco
                       bfCliente.Ctacheq3  = ttBancoInfo.CuentaBancaria
                       bfCliente.Blk3      = ttBancoInfo.Bloqueado.  
            OTHERWISE DO:
                ASSIGN 
                    l-Mensaje = "Advertencia: Solo se pueden asignar hasta 3 bancos por cliente.".
                RETURN.
            END.
        END CASE.

        BancoAsignado = BancoAsignado + 1.
    END.

         ASSIGN l-Mensaje  = "Bancos asignados correctamente al cliente.".

    RELEASE bfCliente.
    RELEASE bfBanco.
END PROCEDURE.

/* Procedimiento GET: Consultar Bancos Registrados */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarBancosCliente:
    /*------------------------------------------------------------------------------ 
     Purpose: Consultar los bancos registrados para un cliente específico.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER IdCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttBancoCliente.

    EMPTY TEMP-TABLE ttBancoCliente. 

    /* Validar existencia del cliente */
    FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = IdCliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfCliente THEN RETURN ERROR "El cliente no existe.".

    /* Agregar bancos registrados a la tabla temporal */
    IF bfCliente.Id-Banco1 > 0 THEN DO:
        CREATE ttBancoCliente.
        ASSIGN
            ttBancoCliente.IdBanco        = bfCliente.Id-Banco1
            ttBancoCliente.CuentaBancaria = bfCliente.Ctcheq1
            ttBancoCliente.Bloqueado      = bfCliente.Blk1
            ttBancoCliente.IdCliente      = bfCliente.Id-Cliente.
    END.

    IF bfCliente.Id-Banco2 > 0 THEN DO:
        CREATE ttBancoCliente. 
        ASSIGN
            ttBancoCliente.IdBanco        = bfCliente.Id-Banco2
            ttBancoCliente.CuentaBancaria = bfCliente.Ctacheq2
            ttBancoCliente.Bloqueado      = bfCliente.Blk2
            ttBancoCliente.IdCliente      = bfCliente.Id-Cliente.
    END.

    IF bfCliente.Id-Banco3 > 0 THEN DO:
        CREATE ttBancoCliente.
        ASSIGN
            ttBancoCliente.IdBanco        = bfCliente.Id-Banco3
            ttBancoCliente.CuentaBancaria = bfCliente.Ctacheq3
            ttBancoCliente.Bloqueado      = bfCliente.Blk3
            ttBancoCliente.IdCliente      = bfCliente.Id-Cliente.
    END.

    RELEASE bfCliente.
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE DeleteBancos:
    /*------------------------------------------------------------------------------ 
     Purpose: borrar registros .
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER TABLE FOR ttBancoInfo.
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.
     
     
    DEFINE VARIABLE l-Contador AS INTEGER NO-UNDO. /* Contador de registros eliminados */

    ASSIGN l-Mensaje = "".  
     
    /* Validar que se envíen registros */
    IF NOT CAN-FIND(FIRST ttBancoInfo) THEN DO:
           ASSIGN l-Mensaje = "Error: No se enviaron registros para procesar.".
        RETURN.
    END.
    
    FIND FIRST ttBancoInfo NO-LOCK NO-ERROR.
    /* Buscar el cliente */
    FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttBancoInfo.IdCliente EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bfCliente THEN DO:
            ASSIGN l-Mensaje = "Error: Cliente no encontrado.".
        RETURN.
    END.

    /* Procesar cada banco recibido */
    DEFINE VARIABLE BancoAsignado AS INTEGER NO-UNDO.
    ASSIGN BancoAsignado = 1. /* Inicia con el primer banco */

    FOR EACH ttBancoInfo:
        
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttBancoInfo.IdCliente
                               AND bfCliente.Id-Banco1  = ttBancoInfo.IdBanco
                               AND bfCliente.Ctcheq1    = ttBancoInfo.CuentaBancaria
                               AND bfCliente.Blk1       = ttBancoInfo.Bloqueado EXCLUSIVE-LOCK NO-ERROR.
      
        /* Si se encuentra el registro, eliminarlo */
        IF AVAILABLE(bfCliente) THEN 
        DO:
            ASSIGN 
                  bfCliente.Id-Banco1 = 0
                  bfCliente.Ctcheq1   = ""
                  bfCliente.Blk1      =  FALSE      
                l-Contador = l-Contador + 1.
        END.
         FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttBancoInfo.IdCliente
                               AND bfCliente.Id-Banco2  = ttBancoInfo.IdBanco
                               AND bfCliente.Ctacheq2    = ttBancoInfo.CuentaBancaria
                               AND bfCliente.Blk2       = ttBancoInfo.Bloqueado EXCLUSIVE-LOCK NO-ERROR.
      
        /* Si se encuentra el registro, eliminarlo */
        IF AVAILABLE(bfCliente) THEN 
        DO:
            ASSIGN 
                  bfCliente.Id-Banco2  = 0
                  bfCliente.Ctacheq2   = ""
                  bfCliente.Blk2       =  FALSE      
                l-Contador = l-Contador + 1.
        END.
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttBancoInfo.IdCliente
                               AND bfCliente.Id-Banco3  = ttBancoInfo.IdBanco
                               AND bfCliente.Ctacheq3    = ttBancoInfo.CuentaBancaria
                               AND bfCliente.Blk3       = ttBancoInfo.Bloqueado EXCLUSIVE-LOCK NO-ERROR.
      
        /* Si se encuentra el registro, eliminarlo */
        IF AVAILABLE(bfCliente) THEN 
        DO:
            ASSIGN 
                  bfCliente.Id-Banco3  = 0
                  bfCliente.Ctacheq3   = ""
                  bfCliente.Blk3       =  FALSE      
                l-Contador = l-Contador + 1.
        END.
        
    END. 
    /* Mensaje final con el número de registros eliminados */
    IF l-Contador > 0 THEN
        ASSIGN l-Mensaje = l-Mensaje + "Se elimino " + STRING(l-Contador) + " registros correctamente.".
    ELSE
        ASSIGN l-Mensaje = l-Mensaje + "No se eliminó ningún registro.".
END PROCEDURE.