@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : acuse.p
    Purpose     : 

    Syntax      : /Acuse aqui muestra acuses de tipo N que traen DocAcuse/PagoAcuse
    se omite lo de tipo A ; ya que ahi directo esta a nivel Acuse en /AcuseCte /Acuse 
    Description : 

    Author(s)   : sis10
    Created     : Mon Dec 16 02:11:54 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/* Tabla temporal para almacenar el resultado */
DEFINE TEMP-TABLE ttAcuse
    FIELD IdAcuse        AS CHARACTER    
    FIELD IdCliente      AS INTEGER
    FIELD FecReg         AS DATE
    FIELD FecDep         AS DATE
    FIELD UsuarioReg     AS CHARACTER
    FIELD NomUsuarioReg  AS CHARACTER
    FIELD Estatus        LIKE Acuse.Estatus
    FIELD UsuarioCanc    AS CHAR
    FIELD NomUsuarioCanc AS CHAR
    FIELD Coment1        AS CHAR
    FIELD Coment2        AS CHAR.

DEFINE TEMP-TABLE ttDocAcuse
    FIELD IdAcuse    AS CHARACTER
    FIELD Sec        AS INTEGER
    FIELD FecDoc     AS DATE
    FIELD Documento  AS CHARACTER
    FIELD ImpPago    AS DECIMAL
    FIELD ImpDescPP  AS DECIMAL
    FIELD ImpDescEsp AS DECIMAL
    FIELD ImpDevol   AS DECIMAL.

DEFINE TEMP-TABLE ttPagoAcuse
    FIELD IdAcuse         AS CHARACTER
    FIELD Sec             AS INTEGER
    FIELD IdTp            AS INTEGER
    FIELD DescrTp         AS CHAR
    FIELD Importe         AS DECIMAL
    FIELD ImporteRecibido AS DECIMAL
    FIELD FecCheque       AS DATE
    FIELD IdBanco         AS INTEGER
    FIELD DescrBanco      AS CHARACTER
    FIELD Cheque          AS CHARACTER
    FIELD CpFormaPago     AS CHAR
    FIELD DesCpFormaPago  AS CHAR.
    
DEFINE TEMP-TABLE ttAnticipo
    FIELD IdAnticipo AS CHARACTER
    FIELD IdAcuse    AS CHARACTER
    FIELD Documento  AS CHARACTER
    FIELD Importe    AS DECIMAL.

DEFINE DATASET dsAcuse
    FOR ttAcuse, ttDocAcuse, ttPagoAcuse,ttAnticipo  
    DATA-RELATION RelAcuseDoc FOR ttAcuse, ttDocAcuse
    RELATION-FIELDS (IdAcuse, IdAcuse) NESTED
    DATA-RELATION RelAcusePago  FOR ttAcuse, ttPagoAcuse
    RELATION-FIELDS (IdAcuse, IdAcuse) NESTED
    DATA-RELATION RelDocAnt FOR ttDocAcuse, ttAnticipo
    RELATION-FIELDS (Documento, Documento) NESTED.    
  


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetAcuse:
    /*------------------------------------------------------------------------------
     Purpose: Obtener la información de un Acuse, junto con sus documentos y pagos.
    ------------------------------------------------------------------------------*/

    /* Parámetro de entrada: ID de Acuse */
    DEFINE INPUT PARAMETER l-acuse LIKE DocAcuse.Id-Acuse NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJson AS LONGCHAR NO-UNDO.
    //DEFINE OUTPUT PARAMETER DATASET FOR dsAcuse.

    /* Buffers */
    DEFINE BUFFER bfAcuse     FOR Acuse.
    DEFINE BUFFER bfDocAcuse  FOR DocAcuse.
    DEFINE BUFFER bfPagoAcuse FOR PagoAcuse.
    DEFINE BUFFER bfTipoPago  FOR TipoPago.
    DEFINE BUFFER bfBanco     FOR Banco.
    DEFINE BUFFER bfUsuario   FOR Usuario.

    /* Si el Acuse existe, llenamos la tabla ttAcuse */
    FIND FIRST bfAcuse WHERE bfAcuse.Id-Acuse = l-acuse AND bfAcuse.Tipo = "N" 
        NO-LOCK NO-ERROR.

    IF AVAILABLE(bfAcuse) THEN 
    DO:
        /* Crear registro en ttAcuse */
        CREATE ttAcuse.
        ASSIGN
            ttAcuse.IdAcuse     = bfAcuse.Id-Acuse
            ttAcuse.IdCliente   = bfAcuse.Id-Cliente  
            ttAcuse.FecReg      = bfAcuse.FecReg
            ttAcuse.FecDep      = bfAcuse.FecDep
            ttAcuse.UsuarioReg  = bfAcuse.UsuarioReg
            ttAcuse.Coment1     = bfAcuse.Comen[1]
            ttAcuse.Coment2     = bfAcuse.Comen[2]
            ttAcuse.Estatus     = bfAcuse.Estatus
            ttAcuse.UsuarioCanc = bfAcuse.UsuarioCanc.
        
        /* Buscar el nombre del usuario si existe */
        FIND FIRST bfUsuario WHERE bfUsuario.Id-User = bfAcuse.UsuarioReg NO-LOCK NO-ERROR.
        ttAcuse.NomUsuarioReg = IF AVAILABLE(bfUsuario) THEN bfUsuario.Nom-Usuario ELSE "".
        
        /* Buscar el nombre del usuario si existe */
        FIND FIRST bfUsuario WHERE bfUsuario.Id-User = bfAcuse.UsuarioCanc NO-LOCK NO-ERROR.
        ttAcuse.NomUsuarioCanc = IF AVAILABLE(bfUsuario) THEN bfUsuario.Nom-Usuario ELSE "".
        /* Buscar los DocAcuses relacionados */
        FOR EACH bfDocAcuse WHERE bfDocAcuse.Id-Acuse = bfAcuse.Id-Acuse NO-LOCK:
            CREATE ttDocAcuse.
            ASSIGN
                ttDocAcuse.IdAcuse    = bfDocAcuse.Id-Acuse
                ttDocAcuse.Sec        = bfDocAcuse.Sec
                ttDocAcuse.Documento  = bfDocAcuse.Documento
                ttDocAcuse.ImpPago    = bfDocAcuse.ImpPago
                ttDocAcuse.ImpDescPP  = bfDocAcuse.ImpDescPP
                ttDocAcuse.ImpDescEsp = bfDocAcuse.ImpDescEsp
                ttDocAcuse.FecDoc     = bfDocAcuse.FecDoc
                ttDocAcuse.ImpDevol   = bfDocAcuse.ImpDevol.
                
            IF bfDocAcuse.AntApl > 0 THEN 
            DO:

                FOR EACH DetAnticipo
                    WHERE DetAnticipo.Documento = bfDocAcuse.Documento
                    NO-LOCK:

                    FIND FIRST Anticipo 
                        WHERE Anticipo.Id-Anticipo = DetAnticipo.Id-Anticipo
                        NO-LOCK NO-ERROR.

                    CREATE ttAnticipo.
                    ASSIGN
                        ttAnticipo.IdAnticipo = DetAnticipo.Id-Anticipo
                        ttAnticipo.Documento  = DetAnticipo.Documento
                        ttAnticipo.Importe    = DetAnticipo.Importe
                        ttAnticipo.IdAcuse    = IF AVAILABLE Anticipo THEN Anticipo.Id-Acuse ELSE "".
                END.   
            END.             
        END.      

        /* Buscar los PagoAcuses relacionados */
        FOR EACH bfPagoAcuse WHERE bfPagoAcuse.Id-Acuse = bfAcuse.Id-Acuse NO-LOCK:
            /* Buscar el tipo de pago */
            FIND FIRST bfTipoPago WHERE bfTipoPago.Id-Tp = bfPagoAcuse.Id-Tp NO-LOCK NO-ERROR.

            /* Buscar el banco relacionado */
            FIND FIRST bfBanco WHERE bfBanco.Id-Banco = bfPagoAcuse.Id-Banco NO-LOCK NO-ERROR.

            CREATE ttPagoAcuse.
            ASSIGN
                ttPagoAcuse.IdAcuse         = bfPagoAcuse.Id-Acuse
                ttPagoAcuse.Sec             = bfPagoAcuse.Sec
                ttPagoAcuse.IdTp            = bfPagoAcuse.Id-Tp
                ttPagoAcuse.DescrTp         = IF AVAILABLE bfTipoPago AND bfTipoPago.Descr <> ""
                          THEN bfTipoPago.Descr
                          ELSE IF bfPagoAcuse.Id-Tp = 65 THEN "DEVOLUCION"
                          ELSE "" 
                ttPagoAcuse.Importe         = bfPagoAcuse.Importe
                ttPagoAcuse.ImporteRecibido = bfPagoAcuse.ImpRecibido
                ttPagoAcuse.IdBanco         = bfPagoAcuse.Id-Banco 
                ttPagoAcuse.DescrBanco      = IF AVAILABLE(bfBanco) THEN bfBanco.Nombre ELSE ""
                ttPagoAcuse.Cheque          = bfPagoAcuse.Cheque
                ttPagoAcuse.FecCheque       = bfPagoAcuse.FecCheque
                ttPagoAcuse.CpFormaPago     = bfPagoAcuse.CPFormaPago.

            /* Asignar descripción de la forma de pago */
            CASE bfPagoAcuse.CPFormaPago:
                WHEN "01" THEN 
                    ttPagoAcuse.DesCpFormaPago = "Efectivo".   
                WHEN "02" THEN 
                    ttPagoAcuse.DesCpFormaPago = "Cheque".
                WHEN "03" THEN 
                    ttPagoAcuse.DesCpFormaPago = "Transferencia".
                OTHERWISE   
                ttPagoAcuse.DesCpFormaPago = "Desconocido".
            END.
        END.
    END.
    ELSE 
    DO:
        /* Si el Acuse existe, llenamos la tabla ttAcuse */
        FIND FIRST bfAcuse WHERE bfAcuse.Id-Acuse = l-acuse AND bfAcuse.Tipo = "P" 
            NO-LOCK NO-ERROR.

        IF AVAILABLE(bfAcuse) THEN 
        DO:
            /* Crear registro en ttAcuse */
            CREATE ttAcuse.
            ASSIGN
                ttAcuse.IdAcuse     = bfAcuse.Id-Acuse
                ttAcuse.IdCliente   = bfAcuse.Id-Cliente  
                ttAcuse.FecReg      = bfAcuse.FecReg
                ttAcuse.FecDep      = bfAcuse.FecDep
                ttAcuse.UsuarioReg  = bfAcuse.UsuarioReg
                ttAcuse.Coment1     = bfAcuse.Comen[1]
                ttAcuse.Coment2     = bfAcuse.Comen[2]
                ttAcuse.Estatus     = bfAcuse.Estatus
                ttAcuse.UsuarioCanc = bfAcuse.UsuarioCanc.
        
            /* Buscar el nombre del usuario si existe */
            FIND FIRST bfUsuario WHERE bfUsuario.Id-User = bfAcuse.UsuarioReg NO-LOCK NO-ERROR.
            ttAcuse.NomUsuarioReg = IF AVAILABLE(bfUsuario) THEN bfUsuario.Nom-Usuario ELSE "".
        
            /* Buscar el nombre del usuario si existe */
            FIND FIRST bfUsuario WHERE bfUsuario.Id-User = bfAcuse.UsuarioCanc NO-LOCK NO-ERROR.
            ttAcuse.NomUsuarioCanc = IF AVAILABLE(bfUsuario) THEN bfUsuario.Nom-Usuario ELSE "".
        
            /* Buscar los DocAcuses relacionados */
            FOR EACH bfDocAcuse WHERE bfDocAcuse.Id-Acuse = bfAcuse.Id-Acuse NO-LOCK:
                CREATE ttDocAcuse.
                ASSIGN
                    ttDocAcuse.IdAcuse    = bfDocAcuse.Id-Acuse
                    ttDocAcuse.Sec        = bfDocAcuse.Sec
                    ttDocAcuse.Documento  = bfDocAcuse.Documento
                    ttDocAcuse.ImpPago    = bfDocAcuse.ImpPago
                    ttDocAcuse.ImpDescPP  = bfDocAcuse.ImpDescPP
                    ttDocAcuse.ImpDescEsp = bfDocAcuse.ImpDescEsp
                    ttDocAcuse.FecDoc     = bfDocAcuse.FecDoc
                    ttDocAcuse.ImpDevol   = bfDocAcuse.ImpDevol.
            END.   

            /* Buscar los PagoAcuses relacionados */
            FOR EACH bfPagoAcuse WHERE bfPagoAcuse.Id-Acuse = bfAcuse.Id-Acuse NO-LOCK:
                /* Buscar el tipo de pago */
                FIND FIRST bfTipoPago WHERE bfTipoPago.Id-Tp = bfPagoAcuse.Id-Tp NO-LOCK NO-ERROR.

                /* Buscar el banco relacionado */
                FIND FIRST bfBanco WHERE bfBanco.Id-Banco = bfPagoAcuse.Id-Banco NO-LOCK NO-ERROR.

                CREATE ttPagoAcuse.
                ASSIGN
                    ttPagoAcuse.IdAcuse         = bfPagoAcuse.Id-Acuse
                    ttPagoAcuse.Sec             = bfPagoAcuse.Sec
                    ttPagoAcuse.IdTp            = bfPagoAcuse.Id-Tp
                    ttPagoAcuse.DescrTp         = IF AVAILABLE bfTipoPago AND bfTipoPago.Descr <> ""
                          THEN bfTipoPago.Descr
                          ELSE IF bfPagoAcuse.Id-Tp = 65 THEN "DEVOLUCION"
                          ELSE ""   
                    ttPagoAcuse.Importe         = bfPagoAcuse.Importe
                    ttPagoAcuse.ImporteRecibido = bfPagoAcuse.ImpRecibido
                    ttPagoAcuse.IdBanco         = bfPagoAcuse.Id-Banco 
                    ttPagoAcuse.DescrBanco      = IF AVAILABLE(bfBanco) THEN bfBanco.Nombre ELSE ""
                    ttPagoAcuse.Cheque          = bfPagoAcuse.Cheque
                    ttPagoAcuse.FecCheque       = bfPagoAcuse.FecCheque
                    ttPagoAcuse.CpFormaPago     = bfPagoAcuse.CPFormaPago.

                /* Asignar descripción de la forma de pago */
                CASE bfPagoAcuse.CPFormaPago:
                    WHEN "01" THEN 
                        ttPagoAcuse.DesCpFormaPago = "Efectivo".   
                    WHEN "02" THEN 
                        ttPagoAcuse.DesCpFormaPago = "Cheque".
                    WHEN "03" THEN 
                        ttPagoAcuse.DesCpFormaPago = "Transferencia".
                    OTHERWISE   
                    ttPagoAcuse.DesCpFormaPago = "Desconocido".
                END.
            END.
        END.    
        
    END.
      
    DATASET dsAcuse:WRITE-JSON("LONGCHAR", opcJson, TRUE).
END PROCEDURE.

