@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
DEFINE TEMP-TABLE ttCliEmb NO-UNDO
    FIELD IdCliente          AS INTEGER     /* Mapea con CliEmb.Id-Cliente */
    FIELD Calle              AS CHARACTER   /* Mapea con CliEmb.CalleNo */
    FIELD Colonia            AS CHARACTER   /* Mapea con CliEmb.Colonia */
    FIELD Poblacion          AS CHARACTER   /* Mapea con CliEmb.Ciudad */
    FIELD Estado             AS CHARACTER   /* Mapea con CliEmb.Estado */
    FIELD Delegacion         AS CHARACTER   /* Mapea con CliEmb.delegacion */
    FIELD CP                 AS CHARACTER   /* Mapea con CliEmb.CP */
    FIELD ReferenciaEmbarque AS CHARACTER   /* Mapea con CliEmb.Referencia */
    FIELD Telefono           AS CHARACTER   /* Mapea con CliEmb.Tel */
    FIELD Correo             AS CHARACTER   /* Mapea con CliEmb.e-mail */
    FIELD Atencion           AS CHARACTER   /* Mapea con CliEmb.Attn */
    FIELD RazonSocial        AS CHARACTER   /* Mapea con CliEmb.RazonSocial */
    FIELD RutaEmbarque       AS INTEGER     /* Mapea con CliEmb.Id-RutaEmb */.
    
    

/* Buffer para la tabla persistente */    
DEFINE BUFFER bfCliEmb FOR CliEmb.
DEFINE BUFFER bfCliente  FOR Cliente.
/* -------------------------------------------------------------------------- */
/* POST: Servicio para guardar nuevos registros en la tabla CliEmb */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GuardarDatosCliEmb:
    DEFINE INPUT  PARAMETER TABLE FOR ttCliEmb. /* Tabla temporal de entrada */
    DEFINE OUTPUT PARAMETER l-Mensaje AS CHARACTER NO-UNDO.

    /* Procesar cada registro en la tabla temporal */
    FOR EACH ttCliEmb:
        
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttCliEmb.IdCliente NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCliente THEN DO:
            ASSIGN l-Mensaje =  "El cliente con ID " + STRING(ttCliEmb.IdCliente) + " no existe en la base de datos.".
            RETURN.
        END.
        DO TRANSACTION:
            
            /* Buscar si el cliente ya existe en bfCliEmb */
            FIND FIRST bfCliEmb WHERE bfCliEmb.Id-Cliente = ttCliEmb.IdCliente EXCLUSIVE-LOCK NO-ERROR.

            IF NOT AVAILABLE bfCliEmb THEN DO:
                /* Crear un nuevo registro si no existe */
                CREATE bfCliEmb.
            END.
               
            ASSIGN
                bfCliEmb.Id-Cliente  = ttCliEmb.IdCliente
                bfCliEmb.CalleNo     = ttCliEmb.Calle
                bfCliEmb.Colonia     = ttCliEmb.Colonia
                bfCliEmb.Ciudad      = ttCliEmb.Poblacion
                bfCliEmb.Estado      = ttCliEmb.Estado
                bfCliEmb.delegacion  = ttCliEmb.Delegacion
                bfCliEmb.Tel         = ttCliEmb.Telefono
                bfCliEmb.CP          = ttCliEmb.CP
                bfCliEmb.e-mail      = ttCliEmb.Correo
                bfCliEmb.Attn        = ttCliEmb.Atencion
                bfCliEmb.RazonSocial = ttCliEmb.RazonSocial
                bfCliEmb.Referencia  = ttCliEmb.ReferenciaEmbarque
                bfCliEmb.Id-RutaEmb  = ttCliEmb.RutaEmbarque .

        END.
    END.

    /* Liberar buffer */
    RELEASE bfCliEmb.
END PROCEDURE.


/* -------------------------------------------------------------------------- */
/* GET: Servicio para consultar registros de CliEmb */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarDatosCliEmb:
    DEFINE INPUT  PARAMETER IdCliente AS INTEGER NO-UNDO. /* Parámetro de entrada */
    DEFINE OUTPUT PARAMETER TABLE FOR ttCliEmb.           /* Tabla temporal de salida */

    /* Inicializar la tabla temporal de salida */
    EMPTY TEMP-TABLE ttCliEmb.

    /* Buscar registros en CliEmb asociados al cliente */
    FOR EACH bfCliEmb WHERE bfCliEmb.Id-Cliente = IdCliente NO-LOCK:
        /* Copiar los datos encontrados a la tabla temporal de salida */
        CREATE ttCliEmb.
        ASSIGN
            ttCliEmb.IdCliente          = bfCliEmb.Id-Cliente
            ttCliEmb.Calle              = bfCliEmb.CalleNo
            ttCliEmb.Colonia            = bfCliEmb.Colonia
            ttCliEmb.Poblacion          = bfCliEmb.Ciudad
            ttCliEmb.Estado             = bfCliEmb.Estado
            ttCliEmb.Delegacion         = bfCliEmb.delegacion
            ttCliEmb.CP                 = bfCliEmb.CP
            ttCliEmb.ReferenciaEmbarque = bfCliEmb.Referencia
            ttCliEmb.Telefono           = bfCliEmb.Tel
            ttCliEmb.Correo             = bfCliEmb.e-mail
            ttCliEmb.Atencion           = bfCliEmb.Attn
            ttCliEmb.RazonSocial        = bfCliEmb.RazonSocial
            ttCliEmb.RutaEmbarque       = bfCliEmb.Id-RutaEmb.
    END.

    /* Liberar buffer */
    RELEASE bfCliEmb.
END PROCEDURE.

    