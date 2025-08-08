@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : reportecheques.p
    Purpose     : 

    Syntax      : /RepChequeSeguricheq

    Description : Reporte Cheques Protegidos HU12

    Author(s)   : sis10
    Created     : Tue Feb 04 01:39:57 CST 2025  
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttCheques NO-UNDO 
    FIELD ttFolioCheque      LIKE DetMovC.autcheque    
    FIELD ttFechaOperacion   LIKE MovCaja.FecOper     
    FIELD ttAutorizado       AS CHARACTER
    FIELD ttAutorizadoNom    AS CHARACTER
    FIELD ttCaja             LIKE MovCaja.Id-caja
    FIELD ttFactura          LIKE MovCaja.Referencia
    FIELD ttCliente          LIKE Cliente.Id-Cliente
    FIELD ttNombre           LIKE Cliente.RazonSocial
    FIELD ttImporteFactura   LIKE Remision.Tot
    FIELD ttBanco            LIKE Banco.NomCto
    FIELD ttCuenta           LIKE DetMovC.CtaCheq .

DEF VAR v-importe      AS   DECIMAL               NO-UNDO.
DEFINE BUFFER b-detmovc FOR detmovc.
  
/* ********************  Preprocessor Definitions  ******************** */
   


/* ***************************  Main Block  *************************** */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRepCheqProtegido:
/* ----------------------------------------------------------------------------- */
    DEFINE INPUT  PARAMETER v-fecini  AS DATE NO-UNDO. 
    DEFINE INPUT  PARAMETER v-fecfin  AS DATE NO-UNDO. 
    DEFINE INPUT  PARAMETER v-autorizador AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER v-cliente  AS INT.     

    /* Parámetro de salida */
    DEFINE OUTPUT PARAMETER TABLE FOR ttCheques.  
    

  /* Manejar fechas nulas */
    IF v-fecini = ? THEN v-fecini = TODAY.
    IF v-fecfin = ? THEN v-fecfin = TODAY.


    FOR EACH b-detmovc NO-LOCK 
        WHERE b-detmovc.autcheque > ""
          AND (IF v-autorizador > "" THEN b-detmovc.usuautchq = v-autorizador ELSE TRUE),
        FIRST movcaja WHERE movcaja.folio = b-detmovc.folio     
                        AND movcaja.id-caja = b-detmovc.id-caja 
                        AND movcaja.fecoper >= v-fecini       
                        AND movcaja.fecoper <= v-fecfin
                        AND (IF v-cliente > 0 THEN movcaja.id-cliente = v-cliente ELSE TRUE)
                        NO-LOCK,
        FIRST cliente  WHERE cliente.id-cliente = movcaja.id-cliente NO-LOCK,
        FIRST remision WHERE remision.id-remision = movcaja.refer 
                         AND remision.feccanc = ? NO-LOCK,
        FIRST banco WHERE banco.id-banco = b-detmovc.id-banco NO-LOCK
        BY movcaja.fecoper:
        FIND FIRST Usuario  WHERE Usuario.Id-User    = b-detmovc.usuautchq NO-LOCK NO-ERROR.   
        v-importe = remision.tot. 

        CREATE ttCheques.
        ASSIGN 
              ttFolioCheque     = b-detmovc.autcheque   
              ttFechaOperacion  = movcaja.fecoper 
              ttAutorizado      = b-detmovc.usuautchq        
              ttAutorizadoNom   = IF AVAILABLE Usuario THEN Usuario.Nom-Usuario ELSE ""
              ttCaja            = movcaja.id-caja
              ttFactura         = movcaja.refer
              ttCliente         = movcaja.id-cliente 
              ttNombre          = Cliente.RazonSocial 
              ttImporteFactura  = v-importe
              ttBanco           = banco.nomcto 
              ttCuenta          = b-detmovc.ctacheq  .  
    END.
    RETURN.  
END PROCEDURE.