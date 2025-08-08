@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*************************************************************************************

   /Responsables

   TICKET 1129
   El catalogo de Responsables solo debe mostrar responsables
   que tengan clientes Activos y se puso para que no salgan de
   clientes globales
   JASS03072025
***************************************************************************************/

/* Tabla temporal para la salida */
DEFINE TEMP-TABLE ttResponsables NO-UNDO
    FIELD IdResp   AS INTEGER      /* Mapea con Resp.Id-Resp */
    FIELD Nombre AS CHARACTER  /* Mapea con Resp.Nombre */
     INDEX idx_IdResp IS PRIMARY UNIQUE IdResp.

/* Buffer para la tabla persistente */  
DEFINE BUFFER bfResp FOR Resp.   
 

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarResp:
    /*--------------------------------------------------------------------------  
     Purpose     :
    --------------------------------------------------------------------------*/
    /* Parámetro de salida */
    DEFINE OUTPUT PARAMETER TABLE FOR ttResponsables.

    /* Inicializar la tabla temporal */
    EMPTY TEMP-TABLE ttResponsables.

    /* Recorrer la tabla Resp para obtener los Resps activos */
    FOR EACH bfResp NO-LOCK: 
        
        /* JASS03072025 */
        FIND FIRST Cliente WHERE Cliente.Id-Resp = bfResp.Id-Resp
                             AND Cliente.Activo  = TRUE 
                             AND Cliente.Id-Cliente > 11  
                             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Cliente THEN NEXT.   
        CREATE ttResponsables.  
        ASSIGN 
            ttResponsables.IdResp   = bfResp.Id-Resp
            ttResponsables.Nombre = bfResp.Nombre.
    END.
END PROCEDURE.
