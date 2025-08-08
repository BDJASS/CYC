@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*************************************************************************************

   /ClientesActivos 
    Se utiliza para saber que Cliente usar como Asociados , puede ser usado en otros 
    programas.


***************************************************************************************/

/* Tabla temporal para la salida */
DEFINE TEMP-TABLE ttClientesActivos NO-UNDO
    FIELD IdCliente   AS INTEGER      /* Mapea con Cliente.Id-Cliente */
    FIELD RazonSocial AS CHARACTER  /* Mapea con Cliente.RazonSocial */
     INDEX idx_IdCliente IS PRIMARY UNIQUE IdCliente.

/* Buffer para la tabla persistente */
DEFINE BUFFER bfCliente FOR Cliente.


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarClientesActivos:
    /*--------------------------------------------------------------------------  
     Purpose     : Consultar clientes activos mostrando solo dos columnas.
     Notes       : Devuelve únicamente `Id-Cliente` y `RazonSocial` para clientes activos.
    --------------------------------------------------------------------------*/
    /* Parámetro de salida */
    DEFINE OUTPUT PARAMETER TABLE FOR ttClientesActivos.

    /* Inicializar la tabla temporal */
    EMPTY TEMP-TABLE ttClientesActivos.

    /* Recorrer la tabla Cliente para obtener los clientes activos */
    FOR EACH bfCliente WHERE bfCliente.Activo = TRUE NO-LOCK
        BY   bfCliente.Id-Cliente :     
        CREATE ttClientesActivos.  
        ASSIGN
            ttClientesActivos.IdCliente   = bfCliente.Id-Cliente
            ttClientesActivos.RazonSocial = bfCliente.RazonSocial.
        
    END.
END PROCEDURE.
