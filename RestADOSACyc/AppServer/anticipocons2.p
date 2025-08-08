@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : anticipocons2.p
    Purpose     : 

    Syntax      :/Anticipo

    Description : Consulta de anticipos por nivel Anticipo y detalle

    Author(s)   : sis10
    Created     : Fri Jan 17 11:00:31 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttAnticipo NO-UNDO
    FIELD IdAnticipo            AS CHARACTER
    FIELD Fecha                 AS DATE
    FIELD Acuse                 AS CHARACTER
    FIELD ImporteAnticipo       AS DECIMAL
    FIELD ImporteAplicado       AS DECIMAL
    FIELD SaldoPendienteAplicar AS DECIMAL
    FIELD FecDep                AS DATE  /* NUEVO */ 
    FIELD UsuarioReg            AS CHAR
    FIELD NomUsuarioReg         AS CHAR
    FIELD Coment1               AS CHAR
    FIELD Coment2               AS CHAR
    FIELD Coment3               AS CHAR
    FIELD TipoPago              AS CHAR.
    
DEFINE TEMP-TABLE ttDetAnticipo NO-UNDO
    FIELD IdAnticipo  AS CHARACTER
    FIELD Sec         AS INT
    FIELD Documento   AS CHARACTER
    FIELD AplicadoPor AS CHAR
    FIELD Importe     AS DECIMAL
    FIELD Fecha       AS DATE.   

DEFINE DATASET dsAnticipo FOR 
    ttAnticipo, /* Tabla principal */
    ttDetAnticipo /* Relación con IdAnticipo */
    DATA-RELATION AnticipoDetalle FOR ttAnticipo, ttDetAnticipo
    RELATION-FIELDS (IdAnticipo, IdAnticipo). /* Relación por IdAnticipo */ 
        
DEFINE BUFFER bfUsuario FOR Usuario.        
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetConsultaAnticipo:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pAnticipo AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER DATASET FOR dsAnticipo.


    /* Limpiar las tablas temporales */
    EMPTY TEMP-TABLE ttAnticipo.
    EMPTY TEMP-TABLE ttDetAnticipo.

    /* Buscar el anticipo */
    FIND FIRST Anticipo WHERE Anticipo.Id-Acuse = pAnticipo NO-LOCK NO-ERROR.

    /* Si el anticipo existe, llenamos la tabla ttAnticipo */
    IF AVAILABLE(Anticipo) THEN 
    DO:
        CREATE ttAnticipo.
        ASSIGN
            ttAnticipo.IdAnticipo            = Anticipo.Id-Anticipo
            ttAnticipo.Fecha                 = Anticipo.FecReg
            ttAnticipo.Acuse                 = Anticipo.Id-Acuse    
            ttAnticipo.ImporteAnticipo       = Anticipo.ImpAnticipo
            ttAnticipo.ImporteAplicado       = Anticipo.ImpAplicado
            ttAnticipo.SaldoPendienteAplicar = Anticipo.ImpAnticipo - Anticipo.ImpAplicado.
         
        FIND FIRST Acuse WHERE Acuse.Id-Acuse =  Anticipo.Id-Acuse 
            AND Acuse.Tipo     = "A" NO-LOCK NO-ERROR.
        IF AVAILABLE Acuse THEN  
        DO:
             
            ASSIGN
                ttAnticipo.Coment1    = Acuse.Comen[1]
                ttAnticipo.Coment2    = Acuse.Comen[2]
                ttAnticipo.Coment3    = Acuse.Comen[3] 
                ttAnticipo.FecDep     = Acuse.FecDep
                ttAnticipo.UsuarioReg = Acuse.UsuarioReg.
                  
            /* Buscar el nombre del usuario si existe */
            FIND FIRST bfUsuario WHERE bfUsuario.Id-User = Acuse.UsuarioReg NO-LOCK NO-ERROR.
            ttAnticipo.NomUsuarioReg = IF AVAILABLE(bfUsuario) THEN bfUsuario.Nom-Usuario ELSE "".
            
            FIND FIRST PagoAcuse WHERE PagoAcuse.Id-Acuse = Anticipo.Id-Acuse
                                   AND PagoAcuse.Sec      = 1 NO-LOCK NO-ERROR.
            IF AVAILABLE PagoAcuse THEN DO:
                
                FIND FIRST tipoPago WHERE TipoPago.Id-Tp = PagoAcuse.Id-tp NO-LOCK NO-ERROR.
                IF AVAILABLE tipopago THEN DO:
                ASSIGN 
                  ttAnticipo.TipoPago = TipoPago.Descr.                 
               END.
            END.  
           
        END.    
         
        /* Buscar los detalles del anticipo */
        FOR EACH DetAnticipo WHERE DetAnticipo.Id-Anticipo = Anticipo.Id-Anticipo NO-LOCK:
            CREATE ttDetAnticipo.
            ASSIGN
                ttDetAnticipo.IdAnticipo  = DetAnticipo.Id-Anticipo
                ttDetAnticipo.Sec         = DetAnticipo.Sec
                ttDetAnticipo.Documento   = DetAnticipo.Documento
                ttDetAnticipo.Importe     = DetAnticipo.Importe
                ttDetAnticipo.Fecha       = DetAnticipo.FecReg
                ttDetAnticipo.AplicadoPor = DetAnticipo.UsuarioApl.
        END.
    END.

END PROCEDURE.


