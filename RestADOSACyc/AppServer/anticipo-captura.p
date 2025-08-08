@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : anticipo-captura.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Mon Jul 07 15:50:09 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */
/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : Cuentas por Cobrar
  Programa : Cxca0190.p
  Funcion  : Recibos de Anticipos
  Autor    : LEAS
  Fecha    : 27-09-96
*/

DEF SHARED     VAR g-today  AS DATE NO-UNDO.
DEF NEW SHARED VAR s-valpas AS CHAR EXTENT 3 NO-UNDO.
DEF BUFFER bf_PagoAcuse FOR PagoAcuse.
DEF BUFFER b-PagoAcuse  FOR PagoAcuse.
DEF VAR l-titulo      AS CHAR.
DEF VAR l-cob         AS INTEGER   NO-UNDO.
DEF VAR l-cf          AS INTE      NO-UNDO.
DEF VAR l-folioaut    AS INTE      NO-UNDO.
DEF VAR l-menu2       AS CHAR      EXTENT 2 FORMAT "x(12)"
    INITIAL ["Con Cobrador","Sin Cobrador"] NO-UNDO.
DEF VAR l-prueba      AS INT       NO-UNDO.
DEF VAR l-sec1        AS RECID     NO-UNDO.
DEF VAR l-consecutivo AS INT       NO-UNDO.
DEF VAR l-vez1        AS INTEGER   NO-UNDO.
DEF VAR l-teclas      AS CHAR      INITIAL "RETURN,ENTER,GO" NO-UNDO.
DEF VAR l-mo1         LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-mo2         LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-mo3         LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-folio       LIKE folio.folio LABEL 'Folio' NO-UNDO.
DEF VAR l-folstr      LIKE Acuse.Id-Acuse NO-UNDO.
DEF VAR l-nombre      LIKE Banco.NomCto NO-UNDO.

DEF VAR v-mensaje     AS CHARACTER NO-UNDO.
DEF VAR v-cuenta      LIKE cliente.ctcheq1 NO-UNDO.
DEF VAR v-banco       LIKE banco.id-banco NO-UNDO.

DEF VAR v-dest        AS CHARACTER NO-UNDO.
DEF VAR v-cli         LIKE Cliente.id-cliente NO-UNDO.
DEF VAR v-asun        AS CHARACTER NO-UNDO.
DEF VAR v-orig        AS CHARACTER NO-UNDO.
DEF VAR v-conten      AS CHARACTER NO-UNDO.
DEF VAR v-envia       AS LOGICAL   NO-UNDO.
DEF VAR v-brin        AS CHARACTER NO-UNDO.
DEF VAR v-time        AS INTEGER   NO-UNDO.
DEF VAR v-mens        AS CHARACTER NO-UNDO.

DEF TEMP-TABLE ttDocto                      
    FIELD IdUser      AS CHAR
    FIELD IdCliente   LIKE Acuse.Id-Cliente
    FIELD IdDoc       LIKE DocAcuse.Documento
    FIELD FecReg      LIKE Factura.FecReg
    FIELD RazonSocial LIKE Factura.RazonSocial
    FIELD Total       AS DECIMAL
    FIELD SubTotal    LIKE Factura.SubTotal
    FIELD TDoc        AS CHAR
    FIELD ImpPago     AS DECIMAL
    FIELD ImpAnt      AS DECIMAL
    FIELD Desc1       AS DECIMAL
    FIELD Desc2       AS DECIMAL           
    FIELD Desc3       AS DECIMAL
    FIELD Desc4       AS DECIMAL
    FIELD Desc5       AS DECIMAL
    FIELD Desc6       AS DECIMAL    
    FIELD Desc7       AS DECIMAL  
    FIELD Desc8       AS DECIMAL
    FIELD Desc9       AS DECIMAL
    FIELD Desc10      AS DECIMAL
    FIELD Desc11      AS DECIMAL
    FIELD Desc12      AS DECIMAL 
    FIELD IdCobrador  LIKE Cobrador.Id-Cobrador  
    FIELD Comentarios AS CHAR.

    
DEF TEMP-TABLE ttPago     
    FIELD FormaPago     AS INTEGER 
    FIELD Importe       AS DECIMAL
    FIELD Rec           AS RECID
    FIELD FecDep        AS DATE
    FIELD IdBanco       AS INT
    FIELD Cuenta        AS CHAR  
    FIELD FolioCheque   AS CHAR
    FIELD FechaCheque   AS DATE 
    FIELD Observaciones AS CHAR
    FIELD RelationId    AS CHAR /* Relación con ttDocto */
    INDEX Idx-Def Importe DESCENDING.     

DEF TEMP-TABLE ttAnticipos
    FIELD IdAnticipo  LIKE Anticipo.Id-Anticipo
    FIELD ImpAplicado LIKE DetAnticipo.Importe
    FIELD IdDoc       LIKE DocAcuse.Documento
    FIELD RelationId  AS CHAR /* Relación con ttDocto */
    INDEX Idx-Def IdDoc.
    
DEFINE DATASET dsConciliacion FOR 
    ttDocto,
    ttPago
    DATA-RELATION drDoctoAnticipos FOR ttDocto, ttPago
    RELATION-FIELDS (IdUser, RelationId)
    NESTED.  

/* ***************************  Main Block  *************************** */

DEF VAR l-folAcuse    LIKE Acuse.Id-Acuse NO-UNDO INITIAL "".
DEF VAR l-folAntAcuse LIKE Acuse.Id-Acuse NO-UNDO INITIAL "".
DEF VAR l-RestoAnt    AS DECIMAL NO-UNDO.
DEF VAR l-AntApl      AS DECIMAL NO-UNDO.
DEF VAR l-FecDep      AS DATE    NO-UNDO.
DEF VAR l-Comen2      AS CHAR    NO-UNDO.
DEF VAR l-Comen3      AS CHAR    NO-UNDO.
DEF VAR l-UsuApl      AS CHAR    NO-UNDO INITIAL "".
DEF VAR l-TP          AS INTEGER NO-UNDO.
DEF VAR ip-formapago  AS CHAR    NO-UNDO.

DEFINE VAR l-cliente  LIKE ttdocto.IdCliente.
DEFINE VAR l-cobrador LIKE ttdocto.IdCobrador.
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE AltaAnticipo:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER DATASET FOR dsConciliacion.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    
    FOR EACH ttDocto:
        IF ttDocto.IdCliente = 0 OR ttDocto.IdCliente = ? 
            OR ttDocto.IdCobrador = 0 OR ttDocto.IdCobrador = ? THEN 
        DO:
       
            IdError   = TRUE.
            Respuesta = "Error: El Cliente o Cobrador no puede estar vacío o en 0.".
            RETURN.
        END.
        
        ASSIGN
        l-UsuApl = ttDocto.IdUser
        l-cliente = ttDocto.IdCliente
        l-Comen2  = ttDocto.Comentarios.
        
    END.
    
    FOR EACH ttPago:
        
        IF ttPago.FormaPago = 0 OR ttpago.FormaPago = ? THEN DO:
        
        IdError   = TRUE.
            Respuesta = "Error: Forma de Pago.".
            RETURN.
        
        END.
        
        IF ttPago.Importe = 0 OR ttPago.Importe = ? THEN DO:
            
            IdError   = TRUE.
            Respuesta = "Error: Importe.".
            RETURN.
            
        END.
        
        ASSIGN 
        l-TP   = ttpago.FormaPago.
       // l-Importe = ttPago.Importe.
        
    END.
    
    
    FIND Usuario WHERE Usuario.Id-User = l-UsuApl NO-LOCK NO-ERROR.
    
    FIND FIRST ttPago NO-LOCK NO-ERROR.
    l-RestoAnt = 11.
    IF l-RestoAnt > 10 THEN 
    DO:
        FIND Folio WHERE Folio.Id-Doc = "ACUSE" AND Folio.Id-Alm = "NA" 
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Folio THEN 
        DO:
            CREATE Folio.
            ASSIGN 
                Folio.Id-Doc  = "ACUSE"
                Folio.Id-Alm  = "NA"
                Folio.Prefijo = "NA"
                Folio.Folio   = 1.
        END.
    
        ASSIGN 
            l-folAntacuse = STRING(Folio.Folio,"9999999") + TRIM(Folio.PreFijo)
            l-folAntacuse = SUBSTRING(l-folAntacuse,LENGTH(l-folAntacuse) - 6,7)
            Folio.Folio   = Folio.Folio + 1.
        RELEASE Folio.  
    
        FIND FIRST ttDocto WHERE ttDocto.IdCliente > 0 NO-LOCK NO-ERROR.
        CREATE Acuse.
        ASSIGN 
            Acuse.Id-Acuse      = l-FolAntAcuse
            Acuse.Id-Caja       = INTEGER(Usuario.Id-Caja)
            Acuse.Turno         = 1
            Acuse.Fecoper       = TODAY
            Acuse.FecReg        = TODAY
            Acuse.FecDep        = ttPago.FecDep  //l-FecDep
            Acuse.UsuarioReg    = l-UsuApl
            Acuse.Tipo          = "A"
            Acuse.Estatus       = 4
            Acuse.Iniciales     = l-UsuApl
            Acuse.Id-Cajero     = Usuario.Id-Cajero
            Acuse.Id-Cliente    = l-cliente
            Acuse.Id-Cobrador   = l-cobrador // 25   // Numero Fijo (Validado en tabla Cobrador) 
            Acuse.AcuseCobrador = "0000000"
            Acuse.Comen[3]      = "Generacion de Anticipo App Pagos " .
        IF AVAILABLE DepBanco THEN 
        DO:
            ASSIGN 
                Acuse.Comen[1]  = "Acuse generado AUTOMATICAMENTE "
                Acuse.Comen[2]  = l-Comen2
                Acuse.Id-Origen = 'ST'.
        END.
        ELSE 
        DO:
            ASSIGN 
                Acuse.Comen[1]  = "Acuse generado MANUALMENTE "
                Acuse.Comen[2]  = ""
                Acuse.Id-Origen = 'SN' .  
        END. 
              
        CREATE PagoAcuse.
        ASSIGN
            PagoAcuse.Id-Acuse    = l-FolAntAcuse
            PagoAcuse.Sec         = 1 
            PagoAcuse.Id-Tp       = ttpago.FormaPago   
            PagoAcuse.ImpRecibido = ttPago.Importe
            PagoAcuse.Importe     = ttPago.Importe
         // PagoAcuse.Id-Banco    = 25  
            PagoAcuse.Id-Banco    = IF ttPago.IdBanco = 57 THEN 25
                                    ELSE IF ttPago.IdBanco = 58 THEN 1
                                    ELSE 25   
            PagoAcuse.Cuenta      = ""
            PagoAcuse.Cheque      = ""
            PagoAcuse.FecCheque   = ?    
            PagoAcuse.CPFormaPago = ip-formapago       // "01=Efectivo,02=Cheque,03=Transferencia"
            PagoAcuse.Id-Moneda   = 1
            PagoAcuse.TC          = 1     
            PagoAcuse.TipoCambio  = /*IF AVAILABLE TCReal THEN TCReal.Importe ELSE*/ 1.
   
        
        FIND Folio WHERE Folio.Id-Doc = "ANT" AND Folio.Id-Alm = ""
            EXCLUSIVE-LOCK NO-ERROR.
        CREATE Anticipo.
        ASSIGN
            Anticipo.Id-Anticipo = STRING(Folio.Folio,'9999999')
            Anticipo.Id-Cliente  = Acuse.Id-Cliente
            Anticipo.Id-Acuse    = Acuse.Id-Acuse
            Anticipo.FecReg      = TODAY
            Anticipo.ImpAnticipo = ttPago.Importe
            Anticipo.ImpAplicado = 0
            Anticipo.ImpDevuelto = 0
            Anticipo.ImpContado  = 0
            Anticipo.Canc        = FALSE   
            Anticipo.Concepto    = Acuse.Comen
            Folio.Folio          = Folio.Folio + 1.
        RELEASE Folio.      
    END.
    


    /* Llamar al procedimiento antes de retornar */
    FIND CURRENT Folio EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Folio THEN RELEASE Folio.
    
    /* Tablas específicas de procesos */
    
    IF AVAILABLE Anticipo   THEN RELEASE Anticipo.
    IF AVAILABLE Acuse      THEN RELEASE Acuse.
    IF AVAILABLE DepBanco   THEN RELEASE DepBanco.  
    IF AVAILABLE DocAcuse   THEN RELEASE DocAcuse.
    IF AVAILABLE PagoAcuse  THEN RELEASE PagoAcuse. 
    
    
    ASSIGN 
          Respuesta = "Folio Anticipo Generado " + string(l-FolAntAcuse).  
    RETURN.    

END PROCEDURE.



