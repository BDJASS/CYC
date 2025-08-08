@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
    Empresa : ADOSA
    Programa: consrelcob.p
              /RelacionCobranzaConsulta
    Fucnion : Consulta todas las relaciones de cobranza
    Autor   : ALEX
    Fecha   : 16 de Diciembre DEL 2024
*/

DEFINE VARIABLE vSuma AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttRCob
    FIELD IdRelCob   AS INTEGER  
    FIELD FecReg     AS DATE 
    FIELD HorReg     AS INTEGER 
    FIELD UsrReg     AS CHARACTER 
    FIELD FecRec     AS DATE 
    FIELD HorRec     AS INTEGER
    FIELD Hora       AS CHARACTER  
    FIELD UsrRec     AS CHARACTER 
    FIELD IdCobrador AS INTEGER.
    
DEFINE TEMP-TABLE ttRelCob
    FIELD IdRelCob    AS INTEGER 
    FIELD IdCliente   AS INTEGER
    FIELD Cuenta      AS CHARACTER
    FIELD RazonSocial AS CHARACTER 
    FIELD IdSuplente  AS INTEGER 
    FIELD IdMC        AS INTEGER 
    FIELD Documento   AS CHARACTER 
    FIELD IdIndCob    AS INTEGER
    FIELD DescrIndCob AS CHARACTER 
    FIELD Observ      AS CHARACTER
    FIELD Direccion   AS CHARACTER.
    
DEFINE TEMP-TABLE ttDetRCob
    FIELD IdRelCob    AS INTEGER 
    FIELD IdCliente   AS INTEGER 
    FIELD Documento   AS CHARACTER 
    FIELD Importe     AS DECIMAL 
    FIELD Saldo       AS DECIMAL 
    FIELD Estatus     AS CHARACTER 
    FIELD Flag        AS CHARACTER 
    FIELD IdMC        AS INTEGER 
    FIELD Descr       AS CHARACTER 
    FIELD FecMov      AS DATE 
    FIELD FecFac      AS DATE 
    FIELD Complemento AS DECIMAL
    FIELD Dias        AS INTEGER.
/*
DEFINE DATASET dsRelacion FOR 
    ttRCob, /* Tabla principal */
    ttRelCob, /* Relación con RelCob */
    ttDetRCob  /* Relación con DetRCob */
    DATA-RELATION RCobRelCob FOR ttRCob, ttRelCob
        RELATION-FIELDS (IdRelCob, IdRelCob) /* Relación por IdRelCob */
    DATA-RELATION RelCobDetRCob FOR ttRelCob, ttDetRCob
        RELATION-FIELDS (IdRelCob, IdRelCob). /* Relación por IdRelCob */
 */
    
DEFINE DATASET dsRelacion FOR 
    ttRCob,
    ttRelCob, 
    ttDetRCob 
    DATA-RELATION RCobRelCob FOR ttRCob, ttRelCob
    RELATION-FIELDS (IdRelCob, IdRelCob) NESTED
    DATA-RELATION RelCobDetRCob FOR ttRelCob, ttDetRCob
    RELATION-FIELDS (IdCliente, IdCliente) NESTED.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultaRelacion:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipCliente LIKE Cliente.Id-Cliente NO-UNDO.
    DEFINE INPUT PARAMETER ipFecIni AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipFecFin AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipRelCob LIKE RCob.Id-RelCob NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJson AS LONGCHAR NO-UNDO.


    FOR EACH RCob WHERE (IF ipFecIni = ? THEN TRUE ELSE RCob.FecReg >= ipFecIni)
        AND (IF ipFecFin = ? THEN TRUE ELSE RCob.FecReg <= ipFecFin)
        AND (IF ipRelCob = 0 OR ipRelCob = ? THEN TRUE ELSE RCob.Id-RelCob = ipRelCob)
        AND (IF ipCliente <> 0 AND ipCliente <> ?
        THEN CAN-FIND(FIRST RelCob WHERE RelCob.Id-RelCob = RCob.Id-RelCob AND RelCob.Id-Cliente = ipCliente)
        ELSE TRUE) NO-LOCK: 
                       
        CREATE ttRCob.                
        ASSIGN 
            ttRCob.IdRelCob   = RCob.Id-RelCob
            ttRCob.FecReg     = RCob.FecREg
            ttRCob.FecRec     = RCob.FecRec  // Se agrego no estaba tomando este dato JASS  
            ttRCob.HorReg     = RCob.HorReg
            ttRCob.UsrReg     = RCob.UsrReg   
            ttRCob.Hora       = STRING(RCob.HorReg,"HH:MM:SS")
            ttRCob.UsrRec     = RCob.UsrRec
            ttRCob.IdCobrador = RCob.Id-Cobrador.
           
        FOR EACH RelCob WHERE RelCob.Id-RelCob = RCob.Id-RelCob
            AND (IF ipCliente = 0 OR ipCliente = ?
            THEN TRUE
            ELSE RelCob.Id-Cliente = ipCliente) NO-LOCK:
            CREATE ttRelCob.
            ASSIGN 
                ttRelCob.IdRelCob   = RelCob.Id-RelCob
                ttRelCob.IdCliente  = RelCob.Id-Cliente
                ttRelCob.IdSuplente = RelCob.Id-Suplente
                ttRelCob.IdMC       = RelCob.Id-MC
                ttRelCob.Documento  = RelCob.Documento
                ttRelCob.IdIndCob   = RelCob.Id-IndCob
                ttRelCob.Observ     = RelCob.Observ.
            FIND Cliente WHERE Cliente.Id-Cliente = RelCob.Id-Cliente NO-LOCK NO-ERROR.
            IF AVAILABLE Cliente THEN 
            DO:
                FIND Ciudad WHERE Ciudad.Id-Ciudad = Cliente.Id-Ciudad NO-LOCK NO-ERROR.
                IF AVAILABLE Ciudad THEN
                    FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
                ASSIGN 
                    ttRelCob.RazonSocial = Cliente.RazonSocial
                    ttRelCob.Direccion   = TRIM(Cliente.CalleNo) + ", " + TRIM(Cliente.Colonia) + ", " + (IF AVAILABLE Ciudad THEN TRIM(Ciudad.Nombre) + ", " ELSE "") + (IF AVAILABLE Estado THEN TRIM(Estado.Nombre) + ", " ELSE "") + Cliente.CP.
            END.
        
            FIND IndCob WHERE IndCob.Id-IndCob = RelCob.Id-IndCob NO-LOCK NO-ERROR.
            IF AVAILABLE IndCob THEN ASSIGN ttRelCob.DescrIndCob = IndCob.Descr.  
           
            RUN /usr2/adosa/procs/vtad1000.p(INPUT RelCob.Id-Cliente,
                OUTPUT vSuma).
            ASSIGN 
                ttRelCob.Cuenta = TRIM(STRING(RelCob.Id-Cliente)) + "-" + STRING(vSuma,'99').
        END.  
    
        FOR EACH DetRCob WHERE DetRCob.Id-RelCob = RCob.Id-RelCob
            AND (ipCliente = 0 OR ipCliente = ? OR DetRCob.Id-Cliente = ipCliente) NO-LOCK: 
            CREATE ttDetRCob.
            ASSIGN 
                ttDetRCob.IdRelCob  = DetRCob.Id-RelCob
                ttDetRCob.IdCliente = DetRCob.Id-Cliente
                ttDetRCob.Documento = DetRCob.Documento
                ttDetRCob.Importe   = DetRCob.Importe
                ttDetRCob.Saldo     = DetRCob.Saldo
                ttDetRCob.Estatus   = DetRCob.Estatus
                ttDetRCob.Flag      = DetRCob.Flag
                ttDetRCob.IdMC      = DetRCob.Id-MC
                ttDetRCob.Descr     = DetRCob.Descr
                ttDetRCob.FecMov    = DetRCob.FecMov.
         
            FIND FIRST MovCliente WHERE MovCliente.RefSaldo = DetRCob.Documento
                AND MovCliente.Documento = DetRCob.Documento
                AND MovCliente.Id-MC <= 3 NO-LOCK NO-ERROR.      
            FIND FIRST Factura WHERE Factura.id-factura = DetRCob.Documento NO-LOCK NO-ERROR.
         
            IF AVAILABLE MovCliente THEN 
            DO:
                ASSIGN 
                    ttDetRCob.Dias   = IF (TODAY - MovCliente.FecReg > 0) THEN (TODAY - MovCliente.FecReg) ELSE 0
                    ttDetRCob.FecFac = MovCliente.FecReg.

                IF AVAILABLE Factura AND MovCliente.id-MC = 1 THEN
                    ttDetRCob.Complemento = Factura.complmonto.
                ELSE 
                    ttDetRCob.Complemento = 0.
            END.
            ELSE 
            DO:
                ASSIGN 
                    ttDetRCob.Dias        = 0
                    ttDetRCob.FecFac      = DetRCob.FecMov
                    ttDetRCob.Complemento = 0.  /* ← evita que quede sin valor */
            END.
            
            
        END.
    END.
    DATASET dsRelacion:WRITE-JSON("LONGCHAR", opcJson, TRUE).

    RETURN.

END PROCEDURE.
