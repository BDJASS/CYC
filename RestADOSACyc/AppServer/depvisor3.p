@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : depvisor.p
    Purpose     : /DepositoVisor3   

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Mon Oct 28 07:15:45 CST 2024   
    Notes       :
  ----------------------------------------------------------------------*/

/*
   Ticket 839 Agregar Validacion a Facturas con Descuento tipo 65 (devolucion)
   Se mostrara en una columna el importe que traen en esos tipos ID-MC 65
   JASS020625
   
   Ticket 987 Aplicacion Manual: Agregar una columna con la cantidad de dias de antiguedad 
              que tiene cada factura o documento. (TODAY - MovCliente.FecReg) 
              En una columna pequeña con el label "Dias".
   JAS17062025
   
   Ticket 1041 Las devoluciones de Id-Mc 65 enviarselas al front en Positivo,
               por tema de validaciones; se hace ajuste y ya el descuento
               lo tiene por parte del front.
   /* JASS23062025 */
   
   Ticket 1026 Agregar los descuentos tipo 66,69,70,71 sumarlos y enviarlos
               al front 
     JASS23062025_2
   Ticket 936  Consulta de Devoluciones x Folio para el Proceso de tesoria
               En la pantalla de Aplicacion x pagos
     JASS27062025
   
   Ticket 1102   Conciliaciones - Quitar validación .- Se debe permitir hacer 
                 conciliación aun con Factura con plazo vencido y DPP
   JASS01072025
   
    Ticket 1104   Agregar columna de factura plazo en servicio GetConciliacion
   JASS01072025_2
   
   
   Ticket  1244 Enviar un Logico si el Documento tiene un pago en Transito
                Afectacion Pendiente [Cuando pagan con Cheque y posterior
                                      deposita Tesoreria ] 
                JASS17072025    
   
  Ticket 1497   En Santander se quito las facturas de Cotiza - Pedido
                para que no le salgan a cliente 3 cuando esta en contado
                ya que no se utilizan y son muchas.
                JASS08082025
  
  Ticket 184    En santander se quita las facturas de contado para el cliente
                3 que tengan fecha de registro mayor a 60 dias
                JASS01092025              
                
*/
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num         AS INTEGER.
DEFINE VARIABLE l-lista       AS CHARACTER.
DEFINE VARIABLE v-tipo-cte    AS CHARACTER.
DEFINE VARIABLE v-documento   AS CHARACTER FORMAT "X(9)" .
DEFINE VARIABLE l-desactivado AS LOGIC.
DEFINE VARIABLE l-conciliado  AS CHAR.
DEFINE VARIABLE l-FormaPago   AS CHAR.

DEFINE VARIABLE l-pendientes  AS LOGICAL   NO-UNDO INITIAL FALSE.



DEFINE TEMP-TABLE ttDeposito NO-UNDO   
    FIELD NumList         AS INTEGER
    FIELD Clase           AS CHARACTER FORMAT "X(30)"  
    FIELD Zona            AS CHARACTER FORMAT "X(30)"
    FIELD TipoDeCliente   AS CHARACTER FORMAT "X(30)"
    FIELD NumCliente      AS INTEGER
    FIELD Cliente         AS CHARACTER FORMAT "X(30)"  
    FIELD FechaDeposito   AS DATE      FORMAT 99/99/9999
    FIELD Hora            AS INTEGER
    FIELD Banco           AS CHARACTER FORMAT "X(30)"
    FIELD Descripcion     AS CHARACTER FORMAT "X(30)"
    FIELD Importe         AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD FechaAplicacion AS DATE      FORMAT 99/99/9999
    FIELD Documento       AS CHARACTER FORMAT "X(30)"
    FIELD Anticipo        AS CHARACTER FORMAT "X(10)"
    FIELD Aplicado        AS LOGICAL   FORMAT "SI/NO"
    FIELD Responsable     AS CHARACTER FORMAT "X(30)"
    FIELD Automatico      AS LOGICAL   /* Si esta true debe mostrar el Boton de Conciliacion Automatica */
    FIELD Desactivado     AS LOGICAL  /* Si esta true debe mostrar el Boton de Cambio-Cliente */
    FIELD Rec             AS RECID 
    FIELD Signo           LIKE DepBanco.Signo
    FIELD Saldo           LIKE DepBanco.Saldo
    FIELD Revisado        AS LOGICAL
    FIELD Aut             AS CHARACTER 
    FIELD TipoCte         LIKE DepBanco.TipoCte
    FIELD Asociado        LIKE Asociado.Id-Cliente /* Si esta <> 0 debe mostrar el Boton de Cambio-Asociado */
    FIELD Activo          LIKE DepBanco.Activo
    FIELD Factura         AS LOGICAL
    FIELD IdUser          LIKE DepBanco.Id-User
    FIELD FormaDePago     AS CHARACTER
    FIELD Descuento1      LIKE Cliente.DescPP1   
    FIELD Descuento2      LIKE Cliente.DescPP2
    INDEX idx-respo FechaDeposito DESCENDING    Hora DESCENDING
    INDEX Idx-Dep   NumCliente    FechaDeposito DESC. 
DEFINE DATASET dsDeposito FOR ttDeposito.  



DEFINE TEMP-TABLE ttDocumentos NO-UNDO
    FIELD IdCliente         LIKE MovCliente.Id-Cliente
    FIELD RazonRemision     LIKE Remision.RazonSocial
    FIELD Refsaldo          LIKE MovCliente.Refsaldo
    FIELD FecReg            LIKE MovCliente.FecReg
    FIELD FecVenc           LIKE MovCliente.FecVenc
    FIELD IdMC              LIKE MovCliente.Id-MC
    FIELD Importe           LIKE MovCliente.Importe
    FIELD ImpDescto         LIKE MovCliente.Importe
    FIELD IdMoneda          LIKE MovCliente.Id-Moneda
    FIELD TipoCambio        LIKE MovCliente.TipoCambio
    FIELD Saldo             LIKE MovCliente.Saldo
    FIELD SaldoSinDsc       LIKE MovCliente.Saldo       // 2019-09-10
    FIELD TipoDoc           AS CHAR 
    FIELD GenAcuse          AS LOGICAL
    FIELD Aplica            AS LOGICAL
    FIELD DescNoAplicado    AS LOGICAL                 // 2019-09-10
    FIELD DescVigente       AS LOGICAL 
    FIELD Descuento1        LIKE Cliente.DescPP1   
    FIELD Descuento2        LIKE Cliente.DescPP2 
    FIELD Mov65             LIKE MovCliente.Importe   /* JASS020625 */     
    FIELD Dias              AS INT                      /* JAS17062025 */
    FIELD MovDescuentos     LIKE MovCliente.Importe    /* JASS23062025_2 */
    FIELD FacturaPlazo      LIKE Factura.Plazo  /* JASS01072025_2 */
    FIELD FacturaEnTransito AS LOGICAL        /* JASS17072025 */
    
    /*  INDEX Idx-Doc IdMC IdCliente Saldo Importe   
      INDEX Idx-Docto IdMC IdCliente Saldo GenAcuse 
      INDEX Idx-Saldo Saldo ASC */
    INDEX Idx-Tipo  TipoDoc
    INDEX Idx-Fecha FecReg  DESC.  /* PARA QUE SALGA LA FACTURA MAS VIEJA */ 
    
DEFINE TEMP-TABLE tt-Cliente NO-UNDO
    FIELD Id-Cliente LIKE Cliente.Id-Cliente
    FIELD Descto1    LIKE Cliente.descPP1 
    FIELD Descto2    LIKE Cliente.descPP2
    FIELD plazopp    LIKE Cliente.PlazoPP
    INDEX Idx-Cte Id-Cliente.    

DEFINE VARIABLE l-RecID        AS RECID     NO-UNDO.
DEFINE VARIABLE l-impMov       LIKE MovCliente.Importe NO-UNDO.
DEFINE VARIABLE l-impDevMov    LIKE MovCliente.Importe NO-UNDO.
DEFINE VARIABLE l-coincide     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-impPaso      AS DECIMAL   NO-UNDO INITIAL 0.
DEFINE VARIABLE l-impAntesDesc LIKE MovCliente.Importe NO-UNDO. 
DEFINE VARIABLE l-impDesc      LIKE MovCliente.Importe NO-UNDO.   /* JASS23062025_2 */
DEFINE VARIABLE l-listaDocs    AS CHARACTER NO-UNDO.  
DEFINE VARIABLE l-transito     AS LOGICAL   NO-UNDO INITIAL FALSE.  /* JASS17072025 */ 
DEFINE BUFFER bf-ttDocumentos FOR ttDocumentos.    
DEFINE BUFFER bf_DepBanco     FOR DepBanco.
DEFINE BUFFER bfMov           FOR MovCliente.  /* JASS23062025_2 */
DEFINE BUFFER bf-MovCliente   FOR MovCliente.  

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDepositos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER iRec       AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER iCliente  AS INTEGER NO-UNDO.  
 
    DEFINE OUTPUT PARAMETER TABLE FOR ttDeposito.
    DEFINE OUTPUT PARAMETER TABLE FOR ttDocumentos.     

    EMPTY TEMP-TABLE tt-Cliente.
    EMPTY TEMP-TABLE ttDocumentos.
    EMPTY TEMP-TABLE ttDeposito.


   

    /* Caso 1: Validación por cliente */
   
    /* 1. Lógica para nuevo parámetro iCliente */
    IF iCliente <> 0 THEN 
    DO:
        FIND FIRST Cliente 
            WHERE Cliente.Id-Cliente = iCliente
            NO-LOCK NO-ERROR.
        
        IF AVAILABLE Cliente THEN 
        DO:
            /* Guardar en tt-Cliente */
            CREATE tt-Cliente.
            ASSIGN 
                tt-Cliente.Id-Cliente = Cliente.Id-Cliente  
                tt-Cliente.Descto1    = Cliente.descPP1
                tt-Cliente.Descto2    = Cliente.descPP2
                tt-Cliente.plazopp    = Cliente.PlazoPP + 30.
            
            /* Guardar también en ttDeposito */
            CREATE ttDeposito.
            ASSIGN
                ttDeposito.NumCliente    = Cliente.Id-Cliente  /* Cliente en Deposito */
                ttDeposito.Importe       = 0                   /* Valores por defecto */
                ttDeposito.FechaDeposito = TODAY - 4  
                ttDeposito.Aplicado      = FALSE  
                ttDeposito.Saldo         = 0
                ttDeposito.TipoCte       = 1500    // valor para que envie facturas a nivel cliente
                ttDeposito.Activo        = FALSE.  // valor para que envie facturas a nivel cliente
 
        END.  
        RUN Movimientos2.
    END.
    /*
    OUTPUT TO VALUE("/home/sis10/deposito-manuel-2.txt") APPEND.
     FOR EACH ttDeposito:
         /* Escribir todos los campos relevantes */
         PUT UNFORMATTED
             "Registro ttDeposito:" SKIP
             "NumCliente: " ttDeposito.NumCliente SKIP
             "Importe: " ttDeposito.Importe SKIP
             "FechaDeposito: " ttDeposito.FechaDeposito SKIP
             "TipoCte: " ttDeposito.TipoCte SKIP
             "Activo: " ttDeposito.Activo SKIP(2).
     END.
     OUTPUT CLOSE.
     UNIX SILENT VALUE("chmod 777 /home/sis10/deposito-manuel-2.txt").
        
     OUTPUT TO VALUE("/home/sis10/deposito-manuel-3.txt") APPEND.
     FOR EACH tt-Cliente:
         /* Escribir todos los campos relevantes */
         PUT UNFORMATTED
             "Registro Cliente:" SKIP
             "NumCliente: " tt-Cliente.Id-Cliente SKIP
             "des1: " tt-Cliente.Descto1 SKIP
             "Des2: " tt-Cliente.Descto2 SKIP
             "Plazo: " tt-Cliente.plazopp SKIP.
     END.
     OUTPUT CLOSE.
     UNIX SILENT VALUE("chmod 777 /home/sis10/deposito-manuel-3.txt").   */
    
    IF iRec <> 0 THEN 
    DO:  
     
        /* Crear un registro temporal del depósito */
        FIND FIRST DepBanco WHERE RECID(DepBanco) = iRec NO-LOCK NO-ERROR.
        IF AVAILABLE DepBanco THEN 
        DO:
            ASSIGN 
                l-RecID = RECID(DepBanco). /* Capturamos el ROWID aquí */
            CREATE ttDeposito.
            ASSIGN
                ttDeposito.NumCliente      = DepBanco.Id-Cliente
                ttDeposito.Importe         = DepBanco.Importe
                ttDeposito.FechaAplicacion = DepBanco.FecAplica
                ttDeposito.Anticipo        = DepBanco.Id-AcuseAnt
                ttDeposito.Aplicado        = DepBanco.Conciliado
                ttDeposito.Rec             = l-RecID
                ttDeposito.Saldo           = DepBanco.Saldo
                ttDeposito.Signo           = DepBanco.Signo
                ttDeposito.Revisado        = FALSE
                ttDeposito.TipoCte         = DepBanco.TipoCte
                ttDeposito.Activo          = DepBanco.Activo
                ttDeposito.Factura         = FALSE   
                ttDeposito.IdUser          = DepBanco.Id-User
                ttDeposito.FechaDeposito   = DepBanco.FecDep.
        END.   

        /* Validación y búsqueda del cliente */
        FIND FIRST Cliente 
            WHERE Cliente.Id-Cliente = ttDeposito.NumCliente
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Cliente THEN NEXT. /* Si no encuentra el cliente, pasa al siguiente DepBanco */

        /* Validar descuentos y marcar conciliación automática */
        IF (Cliente.descPP1 > 0 OR Cliente.descPP2 > 0) AND ttDeposito.TipoCte <> 4 THEN 
        DO:
            ASSIGN 
                l-conciliado = "D".
        END.
        ELSE 
        DO: 
            ASSIGN 
                l-conciliado = "".                                                     
        END.
    
        FIND FIRST ttDeposito NO-LOCK NO-ERROR.
        ASSIGN
            ttDeposito.Aut        = l-conciliado 
            ttDeposito.Automatico = NOT(ttDeposito.Aut = "" OR ttDeposito.Aut = "FC"). 
    
    
    
     // Lleno tabla de clientes con movimientos
        FIND FIRST tt-Cliente WHERE tt-Cliente.Id-Cliente = DepBanco.Id-Cliente NO-LOCK NO-ERROR.
        IF NOT AVAILABLE tt-Cliente THEN 
        DO:
            IF AVAILABLE Cliente THEN 
            DO: 
                CREATE tt-Cliente.
                ASSIGN 
                    tt-Cliente.Id-Cliente = Cliente.Id-Cliente  
                    tt-Cliente.Descto1    = Cliente.descPP1
                    tt-Cliente.Descto2    = Cliente.descPP2
                    tt-Cliente.plazopp    = Cliente.PlazoPP + 30.
            END.  
        END.
    END. // FIN DE RUTINA CUANDO SE ENVIA iREC
  
    /* 7. Ejecutar procesos según tipo de cliente */
    IF ttDeposito.Aplicado = FALSE 
        AND ttDeposito.Activo = TRUE THEN 
    DO:   
   
        CASE ttDeposito.TipoCte:
            WHEN 4 THEN RUN MovimientosContado.
            OTHERWISE RUN Movimientos.
        END CASE.      
    END.  

        
           
END PROCEDURE.   



PROCEDURE Movimientos:
    
    /*
      Empresa  : ADOSA
      Modulo   : Cuentas por Cobrar
      Programa : cxcd0800.i
      Llamador : cxca0800.p
      Funcion  : Proceso que llena tabla temporal que se muestra en el listado
      Autor    : RNPC
      Fecha    : 24 de Junio del 2019
    */
    EMPTY TEMP-TABLE ttDocumentos.

    /*_ ******************************************************************** 
        II. Busco movimientos de credito (MovCliente) y contado (Remision) 
        de los clientes encontrados con depositos 
        (Creo ttDocumentos) excluyendo dolares
        ******************************************************************* * _*/
    FOR EACH ttDeposito NO-LOCK :    
        FOR EACH tt-Cliente NO-LOCK BY Id-Cliente:
            FOR EACH MovCliente WHERE
                MovCliente.Id-MC <= 3 AND
                MovCliente.Saldo > 0  AND
                Movcliente.id-cliente = tt-Cliente.Id-Cliente NO-LOCK
                USE-INDEX idx-mov:
                /*BY MovCliente.FecVenc BY MovCliente.RefSaldo:*/
            
                IF MovCliente.Id-Moneda > 1 THEN NEXT.
        
        // Busco devoluciones
                ASSIGN 
                    l-impDevMov = 0.
                FOR EACH bf-MovCliente WHERE
                    bf-MovCliente.RefSaldo = MovCliente.RefSaldo AND
                    bf-MovCliente.Id-MC = 65 AND
                    bf-Movcliente.id-cliente = MovCliente.id-Cliente NO-LOCK:
                    ASSIGN 
                        l-impDevMov = l-impDevMov + bf-MovCliente.Importe. 
                END.
        
        // Busco descuentos 66-69-70-71
                ASSIGN 
                    l-impDesc = 0.
                FOR EACH bfMov WHERE
                    bfMov.RefSaldo = MovCliente.RefSaldo AND
                    (bfMov.Id-MC = 66 OR
                    bfMov.Id-MC = 69 OR
                    bfMov.Id-MC = 70 OR
                    bfMov.Id-MC = 71) AND
                    bfMov.id-cliente = MovCliente.id-Cliente NO-LOCK:
                    ASSIGN 
                        l-impDesc = l-impDesc + bfMov.Importe. 
                END.
                
                 // Revisa si el documento tiene un pago en transito
                ASSIGN 
                    l-transito = FALSE.
                FOR EACH bfMov WHERE bfMov.RefSaldo = MovCliente.RefSaldo 
                                 AND bfMov.id-cliente = MovCliente.id-Cliente 
                                 AND bfMov.Afectado = FALSE
                                 NO-LOCK:
                    ASSIGN 
                        l-transito = TRUE. 
                END.
                  
                Find first Factura WHERE Factura.Id-Factura = MovCliente.RefSaldo 
                    AND Factura.Id-Cliente = MovCliente.Id-Cliente NO-LOCK NO-ERROR.
                IF MovCliente.Saldo = MovCliente.Importe THEN ASSIGN l-impMov = MovCliente.Importe - l-impDevMov.   // 2019-09-19
                ELSE l-impMov = MovCliente.Saldo.
        //ASSIGN l-impMov = MovCliente.Importe - l-impDevMov
               
                ASSIGN 
                    l-impAntesDesc = l-impMov.     // 2019-09-06
        
                IF tt-Cliente.Descto1 > 0 /* AND ((MovCliente.FecVenc + tt-cliente.plazopp) >= ttDeposito.FechaDeposito) */ THEN
                    ASSIGN 
                        l-impMov = l-impMov - ROUND(l-impMov * (tt-Cliente.Descto1 / 100),2).
                // 2019-09-11 - l-impMov = ROUND(l-impMov * ((100 - tt-Cliente.Descto1) / 100),2).
                IF tt-Cliente.Descto2 > 0 /* AND ((MovCliente.FecVenc + tt-cliente.plazopp) >= ttDeposito.FechaDeposito) */ THEN
                    ASSIGN 
                        l-impMov = l-impMov - ROUND(l-impMov * (tt-Cliente.Descto2 / 100),2).
                // 2019-09-11 - l-impMov = ROUND(l-impMov * ((100 - tt-Cliente.Descto2) / 100),2).
                
     // JASS   IF (MovCliente.Saldo = MovCliente.Importe) AND l-impMov > (ttdeposito.Importe + 0.01) THEN NEXT. // 2019-09-04 / 2019-09-18
         
        // Creo registro en tabla temporal
                CREATE ttDocumentos.
                ASSIGN 
                    ttDocumentos.IdCliente      = MovCliente.Id-Cliente
                    ttDocumentos.Refsaldo       = MovCliente.Refsaldo
                    ttDocumentos.FecReg         = MovCliente.FecReg
                    ttDocumentos.FecVenc        = MovCliente.FecVenc
                    ttDocumentos.IdMC           = MovCliente.Id-MC
                    ttDocumentos.Importe        = MovCliente.Importe   // l-impMov
                    ttDocumentos.ImpDescto      = IF l-impAntesDesc > l-impMov THEN (l-impAntesDesc - l-impMov) ELSE 0   
                    ttDocumentos.IdMoneda       = IF MovCliente.Id-Moneda < 3 THEN 1 ELSE MovCliente.Id-Moneda
                    ttDocumentos.TipoCambio     = IF MovCliente.TipoCambio < 1 THEN 1 ELSE MovCliente.TipoCambio
                    ttDocumentos.Saldo          = l-impMov // IF MovCliente.Saldo = MovCliente.Importe THEN l-impMov ELSE MovCliente.Saldo
                    ttDocumentos.SaldoSinDsc    = l-impAntesDesc // IF MovCliente.Saldo = MovCliente.Importe THEN (MovCliente.Importe - l-impDevMov) ELSE MovCliente.Saldo   // 2019-09-10
                    ttDocumentos.GenAcuse       = TRUE
                    ttDocumentos.Aplica         = FALSE
                    ttDocumentos.DescNoAplicado = FALSE    // 2019-09-10
                    ttDocumentos.DescVigente    = IF (tt-Cliente.Descto1 > 0 OR tt-Cliente.Descto2 > 0) THEN TRUE ELSE FALSE 
             // ttDocumentos.DescVigente    = IF ((MovCliente.FecVenc + tt-cliente.plazopp) > ttDeposito.FechaDeposito) THEN TRUE ELSE FALSE
                    ttDocumentos.Descuento1     = tt-Cliente.Descto1
                    ttDocumentos.Descuento2     = tt-Cliente.Descto2
                    l-impMov                    = 0
                    ttDocumentos.TipoDoc        = IF MovCliente.Id-MC = 3 THEN "CHEDEV" ELSE "FACT"
                    ttDocumentos.Mov65          = ABS(l-impDevMov)   /* JASS23062025 */  
                    ttDocumentos.Dias           = (TODAY - MovCliente.FecReg)  /* JAS17062025 */  
                    ttDocumentos.MovDescuentos  = ABS(l-impDesc)   /* JASS23062025_2 */  
                    ttDocumentos.FacturaPlazo   = IF AVAILABLE Factura AND Factura.Plazo <> ? THEN Factura.Plazo ELSE 0          /* JASS01072025_2 */
                    ttDocumentos.FacturaEnTransito = l-transito.
            END.  
    
            /*_ Creo registros de Tabla temporal con posibles opciones de Remisiones _*/
            FOR EACH Remision WHERE         
                Remision.TipoVenta = 2 AND
                Remision.Id-Cliente = tt-Cliente.Id-Cliente AND
                Remision.Pagada = FALSE 
                AND Remision.FecCancel = ? NO-LOCK:           
            

                ASSIGN
                    l-impMov = Remision.Tot.           
            
     // JASS       IF l-impMov > ttDeposito.Importe  THEN NEXT.
            // Creo registro en tabla temporal
                CREATE ttDocumentos.
                ASSIGN 
                    ttDocumentos.IdCliente      = Remision.Id-Cliente
                    ttDocumentos.RazonRemision  = Remision.RazonSocial
                    ttDocumentos.Refsaldo       = Remision.Id-Remision
                    ttDocumentos.FecReg         = Remision.FecReg
                    ttDocumentos.IdMC           = Remision.TipoVenta 
                    ttDocumentos.Importe        = l-impMov
                    ttDocumentos.Saldo          = l-impMov 
                    ttDocumentos.GenAcuse       = FALSE
                    ttDocumentos.Aplica         = FALSE
                    ttDocumentos.DescNoAplicado = FALSE    // 2019-09-10
                    ttDocumentos.DescVigente    = FALSE
                    l-impMov                    = 0
                    ttDocumentos.TipoDoc        = "CONTADO"
                    ttDocumentos.Descuento1     = tt-Cliente.Descto1
                    ttDocumentos.Descuento2     = tt-Cliente.Descto2.         
            END.
        END. /* End de la tabla tt-Cliente */  
    END. /* TT DEPOSITOS */ 
    /*_ ******************************************************************
        III. Reviso por cada movimiento de deposito si hay movimientos 
        que apliquen directamente o bien que apliquen en combinacion
        ****************************************************************** _*/

    FOR EACH tt-Cliente NO-LOCK :
        FOR EACH ttDeposito NO-LOCK USE-INDEX Idx-Dep: 
            IF ttDeposito.Revisado THEN NEXT.
            l-coincide = FALSE.    
    
            /* ********************************************************* 
                 Busco movimiento que coincida con el mismo importe _*/
            FIND FIRST ttDocumentos WHERE
                ttDocumentos.IdMC <= 3 AND
                ttDocumentos.IdCliente = ttDeposito.NumCliente AND
                ttDocumentos.Saldo > 0 AND
                (ttDocumentos.Saldo = ttDeposito.Importe OR
                ABS(ttDocumentos.Saldo - ttDeposito.Importe) <= 0.01 AND ttDocumentos.DescVigente)
                EXCLUSIVE-LOCK NO-ERROR. // 2019-09-17 - Busco - 1 centavo
            IF AVAILABLE ttDocumentos THEN 
            DO:
        
        //MESSAGE '*0* ' + STRING(ip-cliente) + ' - ' + STRING(ttDocumentos.Saldo) + ' - ' + STRING(ttDocumentos.DescVigente) VIEW-AS ALERT-BOX.
        
                IF (ttDocumentos.Saldo - 0.01 = ttDeposito.Importe) THEN // 2019-09-18   - Ajuste en saldos
                    ASSIGN
                        ttDocumentos.Importe   = ttDocumentos.Importe - 0.01
                        ttDocumentos.Saldo     = ttDocumentos.Saldo - 0.01
                        ttDocumentos.ImpDescto = IF ttDocumentos.ImpDescto > 0 THEN (ttDocumentos.ImpDescto + 0.01) ELSE 0.01.
                ELSE IF (ttDocumentos.Saldo + 0.01 = ttDeposito.Importe) THEN 
                        ASSIGN
                            ttDocumentos.Importe   = ttDocumentos.Importe + 0.01
                            ttDocumentos.Saldo     = ttDocumentos.Saldo + 0.01
                            ttDocumentos.ImpDescto = IF ttDocumentos.ImpDescto > 0 THEN (ttDocumentos.ImpDescto - 0.01) ELSE 0.
                        
                ASSIGN 
                    l-coincide          = TRUE
                    ttDocumentos.Aplica = TRUE.
            END.
    
            /* ********************************************************* 
                 2019-09-10 - Busco movimiento sin descuento que coincida con el mismo importe _*/
            IF l-coincide = TRUE THEN NEXT.         
            FIND FIRST ttDocumentos WHERE
                ttDocumentos.IdMC <= 3 AND
                ttDocumentos.IdCliente = ttDeposito.NumCliente AND 
                ttDocumentos.Saldo > 0 AND
                ttDocumentos.Saldo <> ttDeposito.Importe AND 
                (ttDocumentos.SaldoSinDsc = ttDeposito.Importe OR 
                ABS(ttDocumentos.SaldoSinDsc - ttDeposito.Importe) <= 0.01 AND ttDocumentos.DescVigente)      // 2019-09-17 - Busco - 1 centavo
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE ttDocumentos THEN 
            DO:
        //MESSAGE '*1* ' + STRING(ip-cliente) + ' - ' + STRING(ttDocumentos.SaldoSinDsc) + ' - ' + STRING(ttDocumentos.DescVigente) VIEW-AS ALERT-BOX.
            
                IF ttDocumentos.SaldoSinDsc = ttDeposito.Importe THEN ASSIGN ttDocumentos.ImpDescto = 0.
            
                ASSIGN 
                    ttDocumentos.Saldo = ttDocumentos.SaldoSinDsc. // 2019-09-25 - Para quitar del saldo los descuentos, anticipos, etc.
            
            // DEBUG ***********
                /*IF ttDocumentos.IdCliente = 33518 THEN
                PUT UNFORMATTED 'Entro a sin descuento ' + STRING(ttDocumentos.ImpDescto)  SKIP.*/
            
                IF (ttDocumentos.SaldoSinDsc - 0.01 = ttDeposito.Importe) THEN  // 2019-09-18   - Ajuste en saldos
                    ASSIGN
                        ttDocumentos.Importe     = ttDocumentos.Importe - 0.01
                        ttDocumentos.Saldo       = ttDocumentos.Saldo - 0.01
                        ttDocumentos.SaldoSinDsc = ttDocumentos.SaldoSinDsc - 0.01
                        ttDocumentos.ImpDescto   = 0.01.
                ELSE IF (ttDocumentos.SaldoSinDsc + 0.01 = ttDeposito.Importe) THEN
                        ASSIGN
                            ttDocumentos.Importe     = ttDocumentos.Importe + 0.01
                            ttDocumentos.Saldo       = ttDocumentos.Saldo + 0.01
                            ttDocumentos.SaldoSinDsc = ttDocumentos.SaldoSinDsc + 0.01
                            ttDocumentos.ImpDescto   = IF ttDocumentos.ImpDescto > 0 THEN (- 0.01) ELSE 0.

            // DEBUG ***********
                /*IF ttDocumentos.IdCliente = 3050 THEN
                PUT UNFORMATTED 'Entro a sin descuento ' + STRING(ttDocumentos.ImpDescto)  SKIP.*/
                        
                ASSIGN 
                    ttDocumentos.Aplica         = TRUE
                    ttDocumentos.DescNoAplicado = IF (ttDocumentos.SaldoSinDsc = ttDeposito.Importe) AND ttDocumentos.DescVigente THEN TRUE ELSE FALSE
                    l-coincide                  = TRUE.
            END.        
    
            /* ********************************************************* */
            /*_ Busco combinaciones de movimientos que puedan coincidir _*/
            IF l-coincide = TRUE THEN NEXT.
        
            FOR EACH ttDocumentos WHERE   
                ttDocumentos.IdMC <= 3 AND
                ttDocumentos.Saldo > 0 AND  
                ttDocumentos.IdCliente = ttDeposito.NumCliente AND
                ttDocumentos.GenAcuse 
                EXCLUSIVE-LOCK 
                BY ttDocumentos.FecReg
                BY ttDocumentos.RefSaldo: 
                         
            
                IF ttDocumentos.Saldo > (ttDeposito.Importe + 0.01) OR ttDocumentos.Aplica OR l-coincide THEN NEXT.  // 2019-09-17 - Sumo 1 centavo
        
                ASSIGN
                    l-impPaso   = ttDocumentos.Saldo
                    l-coincide  = FALSE
                    l-listaDocs = '0'.
        
                FOR EACH bf-ttDocumentos WHERE
                    bf-ttDocumentos.IdMC <= 3 AND 
                    bf-ttDocumentos.IdCliente = ttDocumentos.IdCliente AND 
                    bf-ttDocumentos.Saldo <> ttDocumentos.Saldo AND 
                    bf-ttDocumentos.GenAcuse
                    EXCLUSIVE-LOCK 
                    BY bf-ttDocumentos.FecReg
                    BY bf-ttDocumentos.RefSaldo:
            
                    IF bf-ttDocumentos.FecReg < ttDocumentos.FecReg THEN NEXT.
            
                    /* 2019-09-10 */
                    IF (l-impPaso + bf-ttDocumentos.Saldo) > (ttDeposito.Importe + 0.01) THEN NEXT.    // 2019-09-17 - Sumo 1 centavo
                    ELSE 
                    DO: 
                        ASSIGN 
                            l-impPaso   = l-impPaso + bf-ttDocumentos.Saldo
                            l-listaDocs = l-listaDocs + ',' + STRING(RECID(bf-ttDocumentos)).
                    END.
            
            
            // DEBUG ***********
                    /*IF ttDocumentos.IdCliente = 32636 THEN DO:
                        PUT UNFORMATTED ' 1.7) Movimiento= ' + STRING(bf-ttDocumentos.RefSaldo) + ' $ ' + STRING(bf-ttDocumentos.Saldo) + 
                                        ' Fecha BF: ' + STRING(bf-ttDocumentos.FecReg) skip.
                    END.*/
            
            
            // DEBUG ***********
                    /*IF ttDocumentos.IdCliente = 1728 THEN DO:
                        PUT UNFORMATTED ' 2) Si l-impPaso es igual al Importe Det ' + STRING(ttDeposito.Importe) + ' o ImpDet es igual a (' + STRING(ttDocumentos.Saldo) + '+' + STRING(bf-ttDocumentos.Saldo) + ') ' +
                                STRING(ttDocumentos.Saldo + bf-ttDocumentos.Saldo) + ' / l-impPaso=' + STRING(l-impPaso) + ' Fecha BF: ' + STRING(bf-ttDocumentos.FecReg) skip.
                    END. */ 
            
                    /*IF (l-impPaso = ttDeposito.Importe) OR ABS(ttDocumentos.Saldo + bf-ttDocumentos.Saldo - ttDeposito.Importe) <= 0.01 THEN DO:*/
                    IF (l-impPaso = ttDeposito.Importe) OR (ttDocumentos.Saldo + bf-ttDocumentos.Saldo = ttDeposito.Importe) 
                        THEN 
                    DO:
                        ASSIGN 
                            l-coincide          = TRUE
                            ttDocumentos.Aplica = TRUE.
                        
                        IF (ttDocumentos.Saldo + bf-ttDocumentos.Saldo = ttDeposito.Importe) THEN 
                            l-listaDocs = l-listaDocs + ',' + STRING(RECID(bf-ttDocumentos)).
                        // 2019-09-05 - bf-ttDocumentos.Aplica = TRUE.
                        LEAVE.
                    END.
            
            // 2019-09-18 - Validaciones por si hay diferencias en centavos al sumarizar
                    IF (ABS(l-impPaso - ttDeposito.Importe) <= 0.01 OR ABS(ttDocumentos.Saldo + bf-ttDocumentos.Saldo - ttDeposito.Importe) <= 0.01)  // 2019-09-18 - Validacion para dif. de 1 centavo
                        AND (tt-cliente.Descto1 > 0 OR tt-cliente.Descto2 > 0) THEN 
                    DO:
                        IF (l-impPaso > ttDeposito.Importe) OR (ttDocumentos.Saldo + bf-ttDocumentos.Saldo > ttDeposito.Importe) THEN
                            ASSIGN
                                ttDocumentos.Importe     = ttDocumentos.Importe - 0.01
                                ttDocumentos.Saldo       = ttDocumentos.Saldo - 0.01
                                ttDocumentos.SaldoSinDsc = ttDocumentos.SaldoSinDsc - 0.01
                                ttDocumentos.ImpDescto   = IF ttDocumentos.ImpDescto > 0 THEN (ttDocumentos.ImpDescto + 0.01) ELSE 0.01.
                        ELSE IF (l-impPaso < ttDeposito.Importe) OR (ttDocumentos.Saldo + bf-ttDocumentos.Saldo < ttDeposito.Importe) THEN
                                ASSIGN
                                    ttDocumentos.Importe     = ttDocumentos.Importe + 0.01
                                    ttDocumentos.Saldo       = ttDocumentos.Saldo + 0.01
                                    ttDocumentos.SaldoSinDsc = ttDocumentos.SaldoSinDsc + 0.01
                                    ttDocumentos.ImpDescto   = IF ttDocumentos.ImpDescto > 0 THEN (ttDocumentos.ImpDescto - 0.01) ELSE 0.                
        
                        ASSIGN 
                            l-coincide          = TRUE
                            ttDocumentos.Aplica = TRUE.
                        
                        IF ABS(ttDocumentos.Saldo + bf-ttDocumentos.Saldo - ttDeposito.Importe) <= 0.01 THEN     // 2019-09-18 
                            l-listaDocs = l-listaDocs + ',' + STRING(RECID(bf-ttDocumentos)).
                        // 2019-09-05 - bf-ttDocumentos.Aplica = TRUE.
                        LEAVE.
                    END.
                        
                /*_ 2019-09-10- Subo validacion
                IF (l-impPaso + bf-ttDocumentos.Saldo) > ttDeposito.Importe THEN NEXT.
                ELSE l-impPaso = l-impPaso + bf-ttDocumentos.Saldo. */
                END. /* End del bf-ttDocumentos */
            END.    /* End del ttDocumentos */
        END.  /* End del ttDeposito */
    END. /* TTCLIENTE */             

  // Marco los Registros encontrados
    FOR EACH bf-ttDocumentos WHERE
        LOOKUP(STRING(RECID(bf-ttDocumentos)), l-listaDocs) > 0
        EXCLUSIVE-LOCK:
        ASSIGN 
            bf-ttDocumentos.Aplica = TRUE.
    END.
    
    // En tipo de credito pondremos las devoluciones a nivel cliente
    // Solo se deben mostrar en la Manual,pero aqui se genera la temporal y Manuel filtra
    // con lo de aplica false
    FIND FIRST ttDocumentos WHERE ttDocumentos.IdCliente <> 0 NO-LOCK NO-ERROR.
    IF  AVAILABLE ttDocumentos 
        THEN 
    DO :
        FOR EACH Devolucion WHERE Devolucion.Id-Cliente = ttDocumentos.IdCliente
            AND Devolucion.FecApl = ?
            AND Devolucion.FecCanc = ?
            AND Devolucion.Documento = ""
            AND Devolucion.VtaCanc   = FALSE 
            NO-LOCK BY Devolucion.FecReg:                      
          // Creo registro en tabla temporal
            CREATE ttDocumentos.
            ASSIGN 
                ttDocumentos.IdCliente   = Devolucion.Id-Cliente
                ttDocumentos.Refsaldo    = STRING (Devolucion.Id-Dev)
                ttDocumentos.FecReg      = Devolucion.FecReg
                ttDocumentos.FecVenc     = Devolucion.FecExpira  // null proximamente se llenara
                ttDocumentos.Importe     = Devolucion.Tot * -1  
                ttDocumentos.ImpDescto   = Devolucion.Tot //0   
                ttDocumentos.Saldo       = Devolucion.Tot * -1
                ttDocumentos.SaldoSinDsc = Devolucion.Tot * -1
                ttDocumentos.TipoDoc     = "DEV".                                              
        END.   
    END.
    
END PROCEDURE.             

PROCEDURE MovimientosContado:
    /*
      Empresa  : ADOSA   
      Modulo   : Cuentas por Cobrar
      Programa : cxcd0801.i
      Llamador : cxca0806.p
      Funcion  : Proceso que llena tabla temporal que se muestra en el listado para Clientes de Contado
      Autor    : RNPC
      Fecha    : 2019-12-04
    */

    /*_ ******************************************************************** 
        II. Busco movimientos de pedidos (Pedido), Cotizaciones (Cot) 
        y facturas (Remision) de los clientes encontrados con depositos 
        (Creo ttDocumentos) excluyendo dolares
        ******************************************************************* * _*/
    EMPTY TEMP-TABLE ttDocumentos.    
    FOR EACH tt-Cliente NO-LOCK BY Id-Cliente:
    // Busco Pedidos NO facturados (Pedido.Id-Estatus < 5) del cliente
        FOR EACH Pedido WHERE Pedido.Id-Cliente = tt-Cliente.Id-Cliente AND
            Pedido.Id-Estatus < 5 AND 
            Pedido.Id-Cond = 0 AND 
            Pedido.FecReg > (TODAY - 90) NO-LOCK:
                              
            IF Pedido.Id-Moneda > 1 THEN NEXT.  
            IF Pedido.Cancelado THEN NEXT.
            IF tt-Cliente.Id-Cliente = 3 THEN NEXT. /* JASS08082025 */
        
        // RNPC 2020-01-15 - Quito pedidos con monto 0 y pedidos de tiendas
            IF Pedido.Tot = 0 THEN NEXT.
            IF NOT (Pedido.Id-Pedido BEGINS "2" OR Pedido.Id-Pedido BEGINS "3") THEN NEXT.
        
    //  Rutina para agregar descuentos en tabla de facturas 
            FIND FIRST Cliente WHERE Cliente.Id-Cliente  = tt-Cliente.Id-Cliente NO-LOCK NO-ERROR. 
        // Creo registro en tabla temporal
            CREATE ttDocumentos.
            ASSIGN 
                ttDocumentos.IdCliente      = Pedido.Id-Cliente
                ttDocumentos.Refsaldo       = Pedido.Id-Pedido
                ttDocumentos.FecReg         = Pedido.FecReg
                ttDocumentos.Importe        = Pedido.Tot   // Incluye descuentos e iva
                ttDocumentos.Saldo          = Pedido.Tot
                ttDocumentos.RazonRemision  = Pedido.RazonSocial
                ttDocumentos.GenAcuse       = TRUE
                ttDocumentos.Aplica         = FALSE
                ttDocumentos.DescNoAplicado = FALSE
                ttDocumentos.DescVigente    = IF (tt-Cliente.Descto1 > 0 OR tt-Cliente.Descto2 > 0) THEN TRUE ELSE FALSE
                ttDocumentos.TipoDoc        = 'PEDIDO'
                ttDocumentos.Descuento1     = tt-Cliente.Descto1
                ttDocumentos.Descuento2     = tt-Cliente.Descto2.
        END.    
         
    
        /*_ Busco facturas (Remisiones) no pagadas _*/
  //  STATUS DEFAULT 'Buscando facturas del cliente... ' + STRING(tt-Cliente.Id-Cliente).
        FOR EACH Remision WHERE
            Remision.TipoVenta = 2 AND
            Remision.Id-Cliente = tt-Cliente.Id-Cliente AND
            Remision.Pagada = FALSE NO-LOCK:
                 
            IF Remision.FecCanc <> ? THEN NEXT.
            IF Remision.Pagada THEN NEXT.
        
            FIND FIRST Devolucion WHERE Devolucion.Id-Factura = Remision.Id-Remision AND
                Devolucion.TipoVenta = 2 NO-LOCK NO-ERROR.
            IF AVAILABLE Devolucion AND Devolucion.VtaCanc = TRUE AND
                Devolucion.FecCanc = ? THEN NEXT.
            
            /* JASS01092025 - Excluir cliente 3 con más de 60 días */
            IF Remision.Id-Cliente = 3 
            AND Remision.FecReg <> ? 
            AND Remision.FecReg <= TODAY - 60 THEN NEXT.    
            
            FIND FIRST ttDeposito WHERE ttDeposito.NumCliente = tt-Cliente.Id-Cliente NO-LOCK NO-ERROR.
        // Creo registro en tabla temporal
            CREATE ttDocumentos.
            ASSIGN 
                ttDocumentos.IdCliente      = Remision.Id-Cliente
                ttDocumentos.Refsaldo       = Remision.Id-Remision
                ttDocumentos.FecReg         = Remision.FecReg
                ttDocumentos.Importe        = Remision.Tot
                ttDocumentos.Saldo          = Remision.Tot
                ttDocumentos.RazonRemision  = Remision.RazonSocial
                ttDocumentos.GenAcuse       = TRUE
                ttDocumentos.Aplica         = FALSE
                ttDocumentos.DescNoAplicado = FALSE
                ttDocumentos.DescVigente    = IF (tt-Cliente.Descto1 > 0 OR tt-Cliente.Descto2 > 0) THEN TRUE ELSE FALSE
                ttDocumentos.TipoDoc        = "CONTADO"
                ttDocumentos.Descuento1     = tt-Cliente.Descto1
                ttDocumentos.Descuento2     = tt-Cliente.Descto2.
                
            /* Ahora en ttDeposito, si el TipoDoc es "FACT", entonces asignamos TRUE a ttDeposito.Factura */
            IF ttDocumentos.TipoDoc = "CONTADO" THEN
                ASSIGN ttDeposito.Factura = TRUE.
        END.
    
        /*_ Busco cotizaciones (Cot) vigentes _*/
  //  STATUS DEFAULT 'Buscando cotizaciones del cliente... ' + STRING(tt-Cliente.Id-Cliente).
        FOR EACH Cot WHERE
            Cot.Id-Cliente = tt-Cliente.Id-Cliente AND
            Cot.FecReg > (TODAY - 90) NO-LOCK:
            
            IF tt-Cliente.Id-Cliente = 3 THEN NEXT.     /* JASS08082025 */ 
            IF (Cot.FecReg + Cot.Vigencia) < TODAY THEN NEXT.
             
            FIND FIRST Pedido WHERE Pedido.Id-Cot = Cot.Id-Cot NO-LOCK NO-ERROR.
            IF AVAILABLE Pedido THEN NEXT. 
             
            // Creo registro en tabla temporal
            CREATE ttDocumentos.
            ASSIGN 
                ttDocumentos.IdCliente      = Cot.Id-Cliente
                ttDocumentos.Refsaldo       = Cot.Id-Cot
                ttDocumentos.FecReg         = Cot.FecReg
                ttDocumentos.Importe        = Cot.Tot
                ttDocumentos.Saldo          = Cot.Tot
                ttDocumentos.RazonRemision  = Cot.RazonSocial 
                ttDocumentos.GenAcuse       = FALSE
                ttDocumentos.Aplica         = FALSE
                ttDocumentos.DescNoAplicado = FALSE
                ttDocumentos.DescVigente    = IF (tt-Cliente.Descto1 > 0 OR tt-Cliente.Descto2 > 0) THEN TRUE ELSE FALSE
                ttDocumentos.TipoDoc        = 'COTIZA'
                ttDocumentos.Descuento1     = tt-Cliente.Descto1
                ttDocumentos.Descuento2     = tt-Cliente.Descto2.
        END.
    END.

    /*_ ******************************************************************
        III. Reviso por cada movimiento de deposito si hay movimientos 
        que apliquen directamente o bien que apliquen en combinacion
        ****************************************************************** _*/
    /*_ DEBUG _*/
// OUTPUT TO /usr2/sis6/logSantander_contado.dat.
    FOR EACH ttDeposito NO-LOCK USE-INDEX Idx-Dep:
  //  STATUS DEFAULT 'Buscando coincidencias de depositos... '.
        IF ttDeposito.Revisado THEN NEXT.
        l-coincide = FALSE.    
    
        /* ********************************************************* 
             Busco movimiento que coincida con el mismo importe _*/
        FIND FIRST ttDocumentos WHERE
            ttDocumentos.IdCliente = ttDeposito.NumCliente AND
            ttDocumentos.Importe > 0 AND
            ttDocumentos.Importe = ttDeposito.Importe 
            /* RNPC 2020-02-27 Solo montos iguales
            OR (ABS(ttDocumentos.Importe - ttDeposito.Importe) <= 0.01 AND ttDocumentos.DescVigente) */
            EXCLUSIVE-LOCK NO-ERROR. // 2019-09-17 - Busco - 1 centavo
        IF AVAILABLE ttDocumentos THEN 
        DO:
            ASSIGN 
                ttDeposito.Revisado   = TRUE
                ttDeposito.Aut        = ttDeposito.Aut + IF ttDocumentos.GenAcuse THEN '*' ELSE 'CT'
                ttDeposito.Automatico = NOT(ttDeposito.Aut = "" OR ttDeposito.Aut = "CT")
                ttDocumentos.Aplica   = TRUE
                l-coincide            = TRUE.
        END.
    
    /* ********************************************************* */
    /*_ Busco combinaciones de movimientos que puedan coincidir _*/
    /*
    IF l-coincide = TRUE THEN NEXT.
    FOR EACH ttDocumentos WHERE
             ttDocumentos.IdCliente = ttDeposito.Id-Cliente AND
             ttDocumentos.Importe > 0 AND 
             ttDocumentos.GenAcuse 
             EXCLUSIVE-LOCK 
             BY ttDocumentos.FecReg:
            
        IF ttDocumentos.Importe > (ttDeposito.Importe + 0.01) OR ttDocumentos.Aplica OR l-coincide THEN NEXT.  // Sumo 1 centavo
                    
        ASSIGN
            l-impPaso = ttDocumentos.Importe
            l-coincide = FALSE.
        
        FOR EACH bf-ttDocumentos WHERE
                 bf-ttDocumentos.IdCliente = ttDocumentos.IdCliente AND 
                 bf-ttDocumentos.Importe <> ttDocumentos.Importe AND 
                 bf-ttDocumentos.GenAcuse
                 EXCLUSIVE-LOCK 
                 BY bf-ttDocumentos.FecReg:
            
            IF bf-ttDocumentos.FecReg < ttDocumentos.FecReg THEN NEXT.
            
            /* 2019-09-10 */
            IF (l-impPaso + bf-ttDocumentos.Importe) > (ttDeposito.Importe + 0.01) THEN NEXT.    // 2019-09-17 - Sumo 1 centavo
            ELSE l-impPaso = l-impPaso + bf-ttDocumentos.Importe.
            
            /*IF (l-impPaso = ttDeposito.Importe) OR ABS(ttDocumentos.Saldo + bf-ttDocumentos.Saldo - ttDeposito.Importe) <= 0.01 THEN DO:*/
            IF (l-impPaso = ttDeposito.Importe) OR (ttDocumentos.Importe + bf-ttDocumentos.Importe = ttDeposito.Importe) 
            THEN DO:
                ASSIGN 
                    l-coincide = TRUE
                    ttDocumentos.Aplica = TRUE
                    bf-ttDocumentos.Aplica = TRUE
                    ttDeposito.Automatico = ttDeposito.Automatico + IF ttDocumentos.GenAcuse THEN '*' ELSE 'CT'.                    
               LEAVE. 
            END.
            
            // 2019-09-18 - Validaciones por si hay diferencias en centavos al sumarizar
            IF (ABS(l-impPaso - ttDeposito.Importe) <= 0.01 OR ABS(ttDocumentos.Importe + bf-ttDocumentos.Importe - ttDeposito.Importe) <= 0.01)  // 2019-09-18 - Validacion para dif. de 1 centavo
            AND ttDocumentos.DescVigente THEN DO:                    
                ASSIGN 
                    l-coincide = TRUE
                    ttDocumentos.Aplica = TRUE
                    bf-ttDocumentos.Aplica = TRUE
                    ttDeposito.Aut = ttDeposito.Aut + IF ttDocumentos.GenAcuse THEN '*' ELSE 'FC'
                    ttDeposito.Automatico = NOT(ttDeposito.Aut = "" OR ttDeposito.Aut = "FC").

                LEAVE.
            END.
        END.
    END.    
    */
    END.

  // En tipo de contado pondremos las devoluciones a nivel cliente
    // Solo se deben mostrar en la Manual,pero aqui se genera la temporal y Manuel filtra
    // con lo de aplica false
    FIND FIRST ttDocumentos WHERE ttDocumentos.IdCliente <> 0 NO-LOCK NO-ERROR.
    IF  AVAILABLE ttDocumentos 
        THEN 
    DO :
        FOR EACH Devolucion WHERE Devolucion.Id-Cliente = ttDocumentos.IdCliente
            AND Devolucion.FecApl = ?
            AND Devolucion.FecCanc = ?
            AND Devolucion.Documento = ""
            AND Devolucion.VtaCanc   = FALSE 
            NO-LOCK 
            BY Devolucion.FecReg:                         
          // Creo registro en tabla temporal
            CREATE ttDocumentos.
            ASSIGN 
                ttDocumentos.IdCliente   = Devolucion.Id-Cliente
                ttDocumentos.Refsaldo    = STRING (Devolucion.Id-Dev)
                ttDocumentos.FecReg      = Devolucion.FecReg
                ttDocumentos.FecVenc     = Devolucion.FecExpira  // null proximamente se llenara
                ttDocumentos.Importe     = Devolucion.Tot * -1  
                ttDocumentos.ImpDescto   = 0   
                ttDocumentos.Saldo       = Devolucion.Tot * -1
                ttDocumentos.SaldoSinDsc = Devolucion.Tot * -1
                ttDocumentos.TipoDoc     = "DEV".                                              
        END.      
    END.      
    
/* ***********DEBUG **********
FOR EACH ttDocumentos NO-LOCK BY ttDocumentos.FecReg:
    PUT UNFORMATTED ttDocumentos.RefSaldo + ' | ' +
            STRING(ttDocumentos.FecReg) + ' | ' + 
            STRING(ttDocumentos.Importe) + ' | ' +
            STRING(ttDocumentos.Saldo) + ' | ' +
            STRING(ttDocumentos.GenAcuse) + ' | ' +
            STRING(ttDocumentos.Aplica) 
            STRING(ttDocumentos.TipoDoc) SKIP.
END.*/
// DEBUG - OUTPUT CLOSE.
  
END PROCEDURE.    

PROCEDURE Movimientos2:   
    
    /*
      Empresa  : ADOSA
      Modulo   : Cuentas por Cobrar
      Programa : cxcd0800.i
      Llamador : cxca0800.p
      Funcion  : Proceso que llena tabla temporal que se muestra en el listado
      Autor    : RNPC
      Fecha    : 24 de Junio del 2019
    */
    EMPTY TEMP-TABLE ttDocumentos.

    /*_ ******************************************************************** 
        II. Busco movimientos de credito (MovCliente) y contado (Remision) 
        de los clientes encontrados con depositos 
        (Creo ttDocumentos) excluyendo dolares
        ******************************************************************* * _*/
    FOR EACH ttDeposito NO-LOCK :    
        FOR EACH tt-Cliente NO-LOCK BY Id-Cliente:
            FOR EACH MovCliente WHERE
                MovCliente.Id-MC <= 3 AND
                MovCliente.Saldo > 0  AND
                Movcliente.id-cliente = tt-Cliente.Id-Cliente NO-LOCK
                USE-INDEX idx-mov:
                /*BY MovCliente.FecVenc BY MovCliente.RefSaldo:*/
            
    //    IF MovCliente.Id-Moneda > 1 THEN NEXT.
        
        // Busco devoluciones
                ASSIGN 
                    l-impDevMov = 0.
                FOR EACH bf-MovCliente WHERE
                    bf-MovCliente.RefSaldo = MovCliente.RefSaldo AND
                    bf-MovCliente.Id-MC = 65 AND
                    bf-Movcliente.id-cliente = MovCliente.id-Cliente NO-LOCK:
                    ASSIGN 
                        l-impDevMov = l-impDevMov + bf-MovCliente.Importe. 
                END.
        
         // Busco descuentos 66-69-70-71
                ASSIGN 
                    l-impDesc = 0.
                FOR EACH bfMov WHERE
                    bfMov.RefSaldo = MovCliente.RefSaldo AND
                    (bfMov.Id-MC = 66 OR
                    bfMov.Id-MC = 69 OR
                    bfMov.Id-MC = 70 OR
                    bfMov.Id-MC = 71) AND
                    bfMov.id-cliente = MovCliente.id-Cliente NO-LOCK:
                    ASSIGN 
                        l-impDesc = l-impDesc + bfMov.Importe. 
                END.
                
                // Revisa si el documento tiene un pago en transito
                ASSIGN 
                    l-transito = FALSE.
                FOR EACH bfMov WHERE bfMov.RefSaldo = MovCliente.RefSaldo 
                                 AND bfMov.id-cliente = MovCliente.id-Cliente 
                                 AND bfMov.Afectado = FALSE
                                 NO-LOCK:
                    ASSIGN 
                        l-transito = TRUE. 
                END.
                  
        
                IF MovCliente.Saldo = MovCliente.Importe THEN ASSIGN l-impMov = MovCliente.Importe - l-impDevMov.   // 2019-09-19
                ELSE l-impMov = MovCliente.Saldo.
        //ASSIGN l-impMov = MovCliente.Importe - l-impDevMov
               
                ASSIGN 
                    l-impAntesDesc = l-impMov.     // 2019-09-06
        
     // JASS   IF (MovCliente.Saldo = MovCliente.Importe) AND l-impMov > (ttdeposito.Importe + 0.01) THEN NEXT. // 2019-09-04 / 2019-09-18
                    
        // Creo registro en tabla temporal
                CREATE ttDocumentos.
                ASSIGN 
                    ttDocumentos.IdCliente         = MovCliente.Id-Cliente
                    ttDocumentos.Refsaldo          = MovCliente.Refsaldo
                    ttDocumentos.FecReg            = MovCliente.FecReg
                    ttDocumentos.FecVenc           = MovCliente.FecVenc
                    ttDocumentos.IdMC              = MovCliente.Id-MC
                    ttDocumentos.Importe           = MovCliente.Importe   // l-impMov
                    ttDocumentos.ImpDescto         = IF l-impAntesDesc > l-impMov THEN (l-impAntesDesc - l-impMov) ELSE 0   
                    ttDocumentos.IdMoneda          = IF MovCliente.Id-Moneda < 3 THEN 1 ELSE MovCliente.Id-Moneda
                    ttDocumentos.TipoCambio        = IF MovCliente.TipoCambio < 1 THEN 1 ELSE MovCliente.TipoCambio
                    ttDocumentos.Saldo             = l-impMov // IF MovCliente.Saldo = MovCliente.Importe THEN l-impMov ELSE MovCliente.Saldo
                    ttDocumentos.SaldoSinDsc       = l-impAntesDesc // IF MovCliente.Saldo = MovCliente.Importe THEN (MovCliente.Importe - l-impDevMov) ELSE MovCliente.Saldo   // 2019-09-10
                    ttDocumentos.GenAcuse          = TRUE
                    ttDocumentos.Aplica            = FALSE
                    ttDocumentos.DescNoAplicado    = FALSE    // 2019-09-10
                    ttDocumentos.DescVigente       = IF ((MovCliente.FecVenc + tt-cliente.plazopp) > ttDeposito.FechaDeposito) THEN TRUE ELSE FALSE
                    l-impMov                       = 0
                    ttDocumentos.TipoDoc           = IF MovCliente.Id-MC = 3 THEN "CHEDEV" ELSE "FACT"
                    ttDocumentos.Descuento1        = tt-Cliente.Descto1
                    ttDocumentos.Descuento2        = tt-Cliente.Descto2
                    ttDocumentos.Mov65             = ABS(l-impDevMov) /* JASS23062025 */
                    ttDocumentos.Dias              = (TODAY - MovCliente.FecReg)  /* JAS17062025 */
                    ttDocumentos.MovDescuentos     = ABS(l-impDesc)   /* JASS23062025_2 */ 
                    ttDocumentos.FacturaEnTransito = l-transito. /* JASS17072025 */  
            END.  
    
            /*_ Creo registros de Tabla temporal con posibles opciones de Remisiones _*/
            FOR EACH Remision WHERE
                Remision.TipoVenta = 2 AND
                Remision.Id-Cliente = tt-Cliente.Id-Cliente AND
                Remision.Pagada = FALSE 
                AND Remision.FecCancel = ? NO-LOCK:
            
                ASSIGN
                    l-impMov = Remision.Tot.   
            
     // JASS       IF l-impMov > ttDeposito.Importe  THEN NEXT.
            // Creo registro en tabla temporal
            
                CREATE ttDocumentos.
                ASSIGN 
                    ttDocumentos.IdCliente      = Remision.Id-Cliente
                    ttDocumentos.RazonRemision  = Remision.RazonSocial
                    ttDocumentos.Refsaldo       = Remision.Id-Remision
                    ttDocumentos.FecReg         = Remision.FecReg
                    ttDocumentos.IdMC           = Remision.TipoVenta
                    ttDocumentos.Importe        = l-impMov
                    ttDocumentos.Saldo          = l-impMov 
                    ttDocumentos.GenAcuse       = FALSE
                    ttDocumentos.Aplica         = FALSE
                    ttDocumentos.DescNoAplicado = FALSE    // 2019-09-10
                    ttDocumentos.DescVigente    = FALSE
                    l-impMov                    = 0
                    ttDocumentos.TipoDoc        = "CONTADO"
                    ttDocumentos.Descuento1     = tt-Cliente.Descto1
                    ttDocumentos.Descuento2     = tt-Cliente.Descto2.         
            END. 
        END. /* End de la tabla tt-Cliente */  
    END. /* TT DEPOSITOS */ 

    // En tipo de credito pondremos las devoluciones a nivel cliente
    // Solo se deben mostrar en la Manual,pero aqui se genera la temporal y Manuel filtra
    // con lo de aplica false
    FOR EACH tt-Cliente WHERE tt-Cliente.Id-Cliente <> 0 NO-LOCK BY Id-Cliente:
        FOR EACH Devolucion WHERE Devolucion.Id-Cliente = tt-Cliente.Id-Cliente
            AND Devolucion.FecApl = ?
            AND Devolucion.FecCanc = ?
            AND Devolucion.Documento = ""
            AND Devolucion.VtaCanc   = FALSE 
            NO-LOCK BY Devolucion.FecReg:                      
          // Creo registro en tabla temporal
            CREATE ttDocumentos.
            ASSIGN 
                ttDocumentos.IdCliente   = Devolucion.Id-Cliente
                ttDocumentos.Refsaldo    = STRING (Devolucion.Id-Dev)
                ttDocumentos.FecReg      = Devolucion.FecReg
                ttDocumentos.FecVenc     = Devolucion.FecExpira  // null proximamente se llenara
                ttDocumentos.Importe     = Devolucion.Tot * -1  
                ttDocumentos.ImpDescto   = Devolucion.Tot //0   
                ttDocumentos.Saldo       = Devolucion.Tot * -1
                ttDocumentos.SaldoSinDsc = Devolucion.Tot * -1
                ttDocumentos.TipoDoc     = "DEV".                                              
        END. 
    END.               
END PROCEDURE.         

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDevolucionPorId:  
    /*------------------------------------------------------------------------------
     Purpose:  Ticket 936 AdosaERP_Mantenimiento  JASS27062025
     Notes:    En tesoreria van a consultar por el Folio de la Devolucion
               En la pantalla de Aplicacion x Pagos
               Este servicio sera para una consulta x folio 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iDevolucion LIKE Devolucion.Id-Dev NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttDocumentos.   
    EMPTY TEMP-TABLE ttDocumentos.
    FOR EACH Devolucion WHERE Devolucion.Id-Dev = iDevolucion
        AND Devolucion.FecApl = ?
        AND Devolucion.FecCanc = ?
        AND Devolucion.Documento = ""
        AND Devolucion.VtaCanc   = FALSE 
        NO-LOCK BY Devolucion.FecReg:                      
          // Creo registro en tabla temporal
        CREATE ttDocumentos.
        ASSIGN 
            ttDocumentos.IdCliente   = Devolucion.Id-Cliente
            ttDocumentos.Refsaldo    = STRING (Devolucion.Id-Dev)
            ttDocumentos.FecReg      = Devolucion.FecReg
            ttDocumentos.FecVenc     = Devolucion.FecExpira  // null proximamente se llenara
            ttDocumentos.Importe     = Devolucion.Tot * -1  
            ttDocumentos.ImpDescto   = Devolucion.Tot //0   
            ttDocumentos.Saldo       = Devolucion.Tot * -1
            ttDocumentos.SaldoSinDsc = Devolucion.Tot * -1
            ttDocumentos.TipoDoc     = "DEV".                                              
    END.    

END PROCEDURE.
