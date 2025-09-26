@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : carteractevisor.p
    Purpose     : /MovimientoCliente

    Syntax      :

    Description : basado en cxcb0050.p

    Author(s)   : sis10
    Created     : Tue Oct 22 14:47:26 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/
/*  --------------------------------------------------------------------- 
 Mod: Ticket 900  /* JASS09062025 */
      Agregar Campo para Identificar NC en Movimientos de Clientes     
      Se colocara Un Logico en la tabla Detalle para que se identifique
      y se mande el webservices.
  --------------------------------------------------------------------- */
/*  --------------------------------------------------------------------- 
 Mod: Ticket 982  /* JASS17062025 */
      Movimientos de clientes debe mostrar facturas con dolares-
      Si las muestra en el area de Movimientos de credito; pero
      en Saldos no las toma para calcular su vencimiento
      se agrego .
      
 Mod: Ticket 989  /* JASS18062025 */
      En Movimientos de clientes cuando tiene Moneda Extranjera
      coloca unos renglones Extras.
      Se enviaran tambien, del lado del front se colocara de forma dinamica.
  
 Mod: Ticket 292 - 293 /* JASS11092025 */
      Integrar Proceso de Depositos Santander
      Mostrar Promedio,Margen,DiasC incluso sin saldo.    
  --------------------------------------------------------------------- */  
  
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num          AS INTEGER.  
DEFINE VARIABLE l-tot-total    AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig      AS DECIMAL   FORMAT ">>>,>>>,>>9.99". 
DEFINE VARIABLE l-tot-ven      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv     AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-clase        AS CHARACTER FORMAT "X(20)". 
DEFINE VARIABLE l-segmento     AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE l-estatus      AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-moneda       AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-tipo-moneda  AS INTEGER.
DEFINE VARIABLE l-tot-30       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-31       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-61       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-91       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-dia          AS INTEGER.
DEFINE VARIABLE l-dia-max      AS INTEGER   FORMAT "zz9" NO-UNDO.


DEFINE VARIABLE l-cargo        AS DECIMAL   FORMAT ">,>>>,>>9.99" LABEL "Cargo" NO-UNDO.
DEFINE VARIABLE l-credito      AS DECIMAL   FORMAT ">,>>>,>>9.99" LABEL "Credito" NO-UNDO.
DEFINE VARIABLE l-totcargo     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-totcredito   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-cargoME      AS DECIMAL   FORMAT ">,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE l-creditoME    AS DECIMAL   FORMAT ">,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE l-vencidoME    AS DECIMAL   FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08 
DEFINE VARIABLE l-pvencerME    AS DECIMAL   FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEFINE VARIABLE l-saldoME      AS DECIMAL   FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEFINE VARIABLE l-tot30ME      AS DECIMAL   FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEFINE VARIABLE l-tot60ME      AS DECIMAL   FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEFINE VARIABLE l-tot90ME      AS DECIMAL   FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEFINE VARIABLE l-tot91ME      AS DECIMAL   FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEFINE VARIABLE l-totcargoME   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-totcreditoME AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-RefValor     AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-vencido      AS DECIMAL   FORMAT "-zzzzz9.99" LABEL "Vencido" NO-UNDO.
DEFINE VARIABLE l-pvencer      AS DECIMAL   FORMAT "-zzzzz9.99" LABEL "P/Vencer" NO-UNDO.
DEFINE VARIABLE l-saldo        AS DECIMAL   FORMAT "-zzzzz9.99" LABEL "Saldo" NO-UNDO.
DEFINE VARIABLE l-tot30        AS DECIMAL   FORMAT "-zzzzz9.99" LABEL " 1-30" NO-UNDO.
DEFINE VARIABLE l-tot60        AS DECIMAL   FORMAT "-zzzzz9.99" LABEL "31-60" NO-UNDO.
DEFINE VARIABLE l-tot90        AS DECIMAL   FORMAT "-zzzzz9.99" LABEL "61+" NO-UNDO.
DEFINE VARIABLE l-totMas90     AS DECIMAL   FORMAT "-zzzzz9.99" LABEL "90+" NO-UNDO.
DEFINE VARIABLE l-chequepag    AS INTEGER   FORMAT ">9" NO-UNDO.
DEFINE VARIABLE l-prompago     AS DECIMAL.
DEFINE VARIABLE l-pagina       AS INTEGER   FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-numcheque    AS INTEGER   FORMAT "z9" NO-UNDO.
DEFINE VARIABLE l-consecutivo  AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-HayMonedaEX  AS LOGICAL   INITIAL FALSE NO-UNDO.   // RNPC - 2019-07-08
DEFINE VARIABLE l-largo        AS INTEGER   INITIAL 20 NO-UNDO.
DEFINE VARIABLE l-tam          AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-ctrl         AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-cont         AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-dias         AS INTEGER   FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-linea2       AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE VARIABLE l-ant          AS INTEGER   FORMAT "zz9" LABEL "Ant" NO-UNDO.
DEFINE VARIABLE l-resp         AS LOGI      FORMAT "Si/No" NO-UNDO.
DEFINE VARIABLE l-hubo         AS LOGI      NO-UNDO.
DEFINE VARIABLE l-acomodo      AS INTEGER   NO-UNDO.
DEFINE TEMP-TABLE ttCliente NO-UNDO    
    FIELD IdCliente    LIKE Cliente.Id-Cliente
    FIELD RazonSocial  LIKE Cliente.RazonSocial
    FIELD Telefono     LIKE Cliente.Tel1
    FIELD CalleNo      LIKE Cliente.CalleNo
    FIELD NumExterior  LIKE Cliente.NumExt
    FIELD Colonia      LIKE Cliente.Colonia
    FIELD Estatus      LIKE Cliente.Activo
    FIELD LineaCredito LIKE Cliente.Limite
    FIELD Plazo        LIKE Cliente.Plazo
    FIELD IdVendedor   LIKE Cliente.Id-Vendedor
    FIELD Vendedor     AS CHARACTER
    FIELD IdResp       LIKE Cliente.Id-Resp
    FIELD Resp         LIKE Resp.Nombre
    FIELD IdCob        LIKE Cliente.Id-Cobrador           
    FIELD Cobrador     AS CHARACTER 
    FIELD CteDig       AS CHARACTER
    FIELD UsrModLimite AS CHARACTER
    FIELD FecModLimite AS DATE
    FIELD UsrModPlazo  AS CHARACTER   
    FIELD FecModPlazo  AS DATE
    FIELD MontoPagare  LIKE Cliente.MontoPagare   
    INDEX idx-clase IdCliente ASCENDING.

DEFINE TEMP-TABLE ttDetalle NO-UNDO 
    FIELD IdCliente        LIKE MovCliente.Id-Cliente
    FIELD Documento        LIKE MovCliente.RefSaldo
    FIELD Fecha            LIKE MovCliente.FecReg
    FIELD PlazoFactura     AS INTEGER 
    FIELD Descripcion      AS CHARACTER
    FIELD Cargo            AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Credito          AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Saldo            LIKE MovCliente.Saldo
    FIELD Antiguedad       AS INTEGER
    FIELD Referencia       AS CHARACTER
    FIELD NotaCredito      AS LOGICAL     /* JASS09062025 */
    FIELD TipoAcuse        LIKE Acuse.Tipo   
    FIELD FolioEstatus     LIKE Factura.CteEstatus
    FIELD Acuse            LIKE Acuse.Id-Acuse
    FIELD Registro         LIKE MovCliente.FecReg
    FIELD Margen           AS DECIMAL   FORMAT "-zzz9.99%"      
    FIELD Acomodo          AS INTEGER 
    FIELD Rec              AS RECID    
    FIELD Id-MC            LIKE MovCliente.Id-MC
    FIELD FacAutorizadoPor LIKE Factura.autorizado-por
    INDEX Idx-Acomodo Acomodo .
    
DEFINE TEMP-TABLE ttCartera NO-UNDO
    FIELD id             AS INTEGER
    FIELD Moneda         AS CHARACTER
    FIELD IdCliente      AS INTEGER
    FIELD saldo          AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD porvencer      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD montovencido   AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD treinta        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD sesenta        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventa        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventamas     AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD diacartera     AS INTEGER
    FIELD promedio       AS DECIMAL  
    FIELD margenpromedio AS DECIMAL
    FIELD deposito       AS DECIMAL 
    INDEX idx-clase IdCliente ASCENDING.
/* Definir el DATASET con relaciones */ 
DEFINE DATASET dsMov FOR 
    ttCliente, /* Tabla principal */
    ttDetalle, /* Relación con Cliente */
    ttCartera  /* Relación con Cliente */
    DATA-RELATION ClienteDetalle FOR ttCliente, ttDetalle
    RELATION-FIELDS (IdCliente, IdCliente) /* Relación por IdCliente */
    DATA-RELATION ClienteCartera FOR ttCliente, ttCartera
    RELATION-FIELDS (IdCliente, IdCliente). /* Relación por IdCliente */


DEFINE BUFFER b-mov      FOR MovCliente.
DEFINE BUFFER bf-mov     FOR MovCliente.
DEFINE BUFFER bCambioCte FOR CambioCte.

DEFINE BUFFER bk-saldo   FOR ttDetalle.
DEFINE BUFFER bbk-saldo  FOR ttDetalle.
DEFINE VARIABLE l-fecha         AS DATE      NO-UNDO.
DEFINE VARIABLE v-marneto       AS DECIMAL   NO-UNDO FORMAT "-zzz9.99%".
DEFINE VARIABLE v-margen        AS DECIMAL   NO-UNDO FORMAT "-zzz9.99%".

DEFINE VARIABLE l-digver        AS CHARACTER .
DEFINE VARIABLE l-CteDig        AS INTEGER   NO-UNDO.

DEFINE VARIABLE l-RefValor2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-RefValor3     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-id-a-procesar AS INTEGER   NO-UNDO.
DEFINE VARIABLE vTotal          AS DECIMAL   NO-UNDO.   
DEFINE VARIABLE l-prompago2     AS DECIMAL.

/* **********************  Internal Procedures  *********************** */


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCartera:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER  lCliente  AS INTEGER.
    DEFINE OUTPUT PARAMETER Respuesta AS CHARACTER. 
    DEFINE OUTPUT PARAMETER DATASET FOR dsMov.

    ASSIGN 
        l-num    = 0
        l-moneda = "".   

    EMPTY TEMP-TABLE ttCartera.
    /* Log de inicio del recurso */
    LOG-MANAGER:WRITE-MESSAGE("/MovimientoCliente [GET] >>> Iniciando consulta de movimientos | Cliente: "
        + STRING(lCliente) + " | FechaHora: " + STRING(NOW)).   
    FIND FIRST Cliente WHERE Cliente.Id-Cliente = lCliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cliente OR lCliente = 0 THEN
    DO:
        ASSIGN 
            Respuesta = "El cliente NO Existe".
        RETURN.
    END. 
    IF Cliente.Activo = FALSE THEN 
    DO:
        FIND FIRST Usuario WHERE Usuario.Id-User = Cliente.Id-User NO-LOCK NO-ERROR.
        /* Construir la cadena */
        ASSIGN 
            Respuesta = "Cuenta inactivada por: " + (IF AVAILABLE Usuario THEN CAPS(Usuario.Nom-usuario) ELSE "") + " " 
                          + "Fecha: " + (IF Cliente.FecBaja <> ? 
                                        THEN STRING(Cliente.FecBaja, "99/99/9999") 
                                        ELSE "Sin fecha") + "          ".
        RETURN.
    END.   
    IF AVAILABLE Cliente THEN 
    DO :
        
        
        
        
        RUN programas/vtad1000.p(INPUT Cliente.Id-Cliente, OUTPUT l-CteDig).
        ASSIGN 
            l-digver = STRING(Cliente.Id-Cliente) + "-" + STRING(l-CteDig,"99").  
        CREATE ttCliente.
        ASSIGN   
            ttCliente.IdCliente    = Cliente.Id-Cliente  
            ttCliente.RazonSocial  = Cliente.RazonSocial
            ttCliente.Telefono     = Cliente.Tel1 + " " + Cliente.Tel2 + " " + Cliente.Tel3
            ttCliente.CalleNo      = Cliente.CalleNo
            ttCliente.NumExterior  = Cliente.NumExt
            ttCliente.Colonia      = Cliente.Colonia
            ttCliente.Estatus      = Cliente.Activo
            ttCliente.LineaCredito = Cliente.Limite             
            ttCliente.Plazo        = Cliente.Plazo
            ttCliente.IdVendedor   = Cliente.Id-Vendedor  
            ttCliente.IdResp       = Cliente.Id-Resp   
            ttCliente.CteDig       = l-digver 
            ttCliente.MontoPagare  = Cliente.MontoPagare.
   
        FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
        FIND FIRST Cobrador WHERE Cobrador.Id-Cobrador = Cliente.Id-Cobrador NO-LOCK NO-ERROR.
        FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = Cliente.Id-Vendedor NO-LOCK NO-ERROR.
        IF AVAILABLE Vendedor THEN 
        DO:
            FIND FIRST empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
        END.
        ASSIGN
            ttCliente.Vendedor = empleado.Nombre 
            WHEN AVAILABLE empleado
            ttCliente.Resp     = Resp.Nombre 
            WHEN AVAILABLE Resp
            ttCliente.IdCob    = Cliente.Id-Cobrador
            ttCliente.Cobrador = Cobrador.Nombre 
            WHEN AVAILABLE Cobrador. . 
            
        FOR EACH bCambioCte WHERE 
            bCambioCte.Id-Cliente = Cliente.Id-Cliente AND
            bCambioCte.Campo = 500
            NO-LOCK
            BY bCambioCte.FecReg DESCENDING
            BY bCambioCte.Hora   DESCENDING:

            /* Este es el más reciente */
            FIND FIRST Empleado WHERE Empleado.Iniciales = bCambioCte.Id-User NO-LOCK NO-ERROR.

            ASSIGN 
                ttCliente.UsrModLimite = IF AVAILABLE Empleado 
                                 THEN Empleado.Nombre 
                                 ELSE bCambioCte.Id-User
                ttCliente.FecModLimite = bCambioCte.FecReg.

            LEAVE. /* Ya obtuviste el último registro */
        END.
    
        FOR EACH bCambioCte WHERE 
            bCambioCte.Id-Cliente = Cliente.Id-Cliente AND
            bCambioCte.Campo = 501
            NO-LOCK
            BY bCambioCte.FecReg DESCENDING
            BY bCambioCte.Hora   DESCENDING:

            /* Este es el más reciente */
            FIND FIRST Empleado WHERE Empleado.Iniciales = bCambioCte.Id-User NO-LOCK NO-ERROR.

            ASSIGN 
                ttCliente.UsrModPlazo = IF AVAILABLE Empleado 
                                 THEN Empleado.Nombre 
                                 ELSE bCambioCte.Id-User
                ttCliente.FecModPlazo = bCambioCte.FecReg.

            LEAVE. /* Ya obtuviste el último registro */
        END.
    END.  
    
    /**************************************************************************************************** */
    FOR EACH b-mov WHERE b-mov.Id-Cliente = lCliente  AND
        b-mov.FecReg    <= TODAY AND
        b-mov.Id-MC     <= 3 AND
        b-mov.Afectado  NO-LOCK
        BREAK BY b-mov.FecReg
        BY b-mov.RefSaldo
        BY b-mov.Id-MC:
    
        IF b-mov.Id-MC <= 3 THEN 
        DO:
            FOR EACH bf-mov WHERE bf-mov.RefSaldo = b-mov.RefSaldo  AND
                bf-mov.FecReg  <= TODAY      AND
                bf-mov.Id-MC    > 3               AND
                bf-mov.Afectado NO-LOCK:
                ACCUMULATE bf-mov.Importe (TOTAL).
            END.
            ASSIGN 
                l-saldo = b-mov.Importe + ACCUM TOTAL bf-mov.Importe.
        
        END. /* del MovCliente.Id-MC <= 3 */
        FOR EACH MovCliente WHERE MovCliente.RefSaldo = b-Mov.refsaldo AND
            MovCliente.FecReg  <= TODAY     AND
            MovCliente.Afectado NO-LOCK
            BREAK BY MovCliente.REfSaldo   
            BY MovCliente.Id-MC:
                   
            FIND TabMC OF MovCliente NO-LOCK NO-ERROR.
            
            FIND Cliente WHERE Cliente.Id-Cliente = MovCliente.Id-Cliente
                NO-LOCK NO-ERROR.
            IF MovCliente.Id-Mc <> 65 THEN
                FIND Acuse WHERE Acuse.Id-Acuse = MovCliente.Documento NO-LOCK NO-ERROR.
            ELSE RELEASE Acuse.

            IF FIRST-OF(MovCliente.RefSaldo) THEN 
            DO:
                ASSIGN 
                    l-fecha = MovCliente.FecReg.
            END.
        
            ASSIGN 
                l-cargo   = (IF MovCliente.Importe > 0 THEN MovCliente.Importe ELSE 0)
                l-credito = (IF MovCliente.Importe < 0 THEN MovCliente.Importe ELSE 0).
                   
            ACCUMULATE l-cargo (TOTAL BY MovCliente.RefSaldo).
            ACCUMULATE l-credito (TOTAL BY MovCliente.RefSaldo).
        
            /*_ RNPC - Sumatoria de totales seg�n la moneda _*/
            IF MovCliente.Id-Moneda > 1 THEN 
            DO:
                ASSIGN 
                    l-cargoME      = (IF MovCliente.Importe > 0 THEN MovCliente.Importe ELSE 0)
                    l-creditoME    = (IF MovCliente.Importe < 0 THEN MovCliente.Importe ELSE 0)
                    l-totcargoME   = l-totcargoME + (l-cargo)
                    l-totcreditoME = l-totcreditoME + (l-credito)  .
                    
                FIND FIRST Moneda WHERE Moneda.Id-Moneda = MovCliente.Id-Moneda NO-LOCK NO-ERROR.
                IF AVAILABLE Moneda THEN ASSIGN l-RefValor2 = Moneda.Simbolo.   
                /*
                IF MovCliente.FecVenc >= TODAY AND MovCliente.Id-MC <= 3 THEN
                    ASSIGN l-pvencerME = l-pvencerME + l-saldo.
                IF MovCliente.Id-MC <= 3 AND MovCliente.FecVenc <= TODAY THEN 
                DO:
                    IF (TODAY - MovCliente.FecVenc) <= 30 AND
                        (TODAY - MovCliente.FecVenc) >= 1 THEN
                        ASSIGN l-tot30ME = l-tot30ME + l-saldo.
                    IF (TODAY - MovCliente.FecVenc) <= 60 AND
                        (TODAY - MovCliente.FecVenc) >= 31 THEN
                        ASSIGN l-tot90ME = l-tot90ME + l-saldo.
                    IF (TODAY - MovCliente.FecVenc) >= 61 THEN
                        ASSIGN l-tot91ME = l-tot91ME + l-saldo.
                END. */
                /* JASS17062025 */
                IF MovCliente.FecVenc >= TODAY AND MovCliente.Id-MC <= 3 THEN
                    ASSIGN l-pvencerME = l-pvencerME + l-saldo.
                IF MovCliente.Id-MC <= 3 AND MovCliente.FecVenc <= TODAY THEN 
                DO:
                    IF (TODAY - MovCliente.FecVenc) >= 1 AND (TODAY - MovCliente.FecVenc) <= 30 THEN 
                    DO:
                        ASSIGN 
                            l-tot30ME = l-tot30ME + l-saldo.      /* 1-30 días */
                    END.
                    ELSE IF (TODAY - MovCliente.FecVenc) >= 31 AND (TODAY - MovCliente.FecVenc) <= 60 THEN 
                        DO:
                            ASSIGN 
                                l-tot60ME = l-tot60ME + l-saldo.      /* 31-60 días */
                        END.
                        ELSE IF (TODAY - MovCliente.FecVenc) >= 61 AND (TODAY - MovCliente.FecVenc) <= 90 THEN 
                            DO:
                                ASSIGN 
                                    l-tot90ME = l-tot90ME + l-saldo.      /* 61-90 días */
                            END.
                            ELSE IF (TODAY - MovCliente.FecVenc) > 90 THEN 
                                DO:
                                    ASSIGN 
                                        l-tot91ME = l-tot91ME + l-saldo. /* Más de 90 días */
                                END.
                END.  
            END.
            ELSE 
            DO:
                ASSIGN 
                    l-totcargo   = l-totcargo + l-cargo
                    l-totcredito = l-totcredito + l-credito.
                
                FIND FIRST Moneda WHERE Moneda.Id-Moneda = MovCliente.Id-Moneda NO-LOCK NO-ERROR.
                IF AVAILABLE Moneda THEN ASSIGN l-RefValor3 = Moneda.Simbolo. 
                    
                IF MovCliente.FecVenc >= TODAY AND MovCliente.Id-MC <= 3 THEN
                    ASSIGN l-pvencer = l-pvencer + l-saldo.
                IF MovCliente.Id-MC <= 3 AND MovCliente.FecVenc <= TODAY THEN 
                DO:
                    IF (TODAY - MovCliente.FecVenc) >= 1 AND (TODAY - MovCliente.FecVenc) <= 30 THEN 
                    DO:
                        ASSIGN 
                            l-tot30 = l-tot30 + l-saldo.      /* 1-30 días */
                    END.
                    ELSE IF (TODAY - MovCliente.FecVenc) >= 31 AND (TODAY - MovCliente.FecVenc) <= 60 THEN 
                        DO:
                            ASSIGN 
                                l-tot60 = l-tot60 + l-saldo.      /* 31-60 días */
                        END.
                        ELSE IF (TODAY - MovCliente.FecVenc) >= 61 AND (TODAY - MovCliente.FecVenc) <= 90 THEN 
                            DO:
                                ASSIGN 
                                    l-tot90 = l-tot90 + l-saldo.      /* 61-90 días */
                            END.
                            ELSE IF (TODAY - MovCliente.FecVenc) > 90 THEN 
                                DO:
                                    ASSIGN 
                                        l-totMas90 = l-totMas90 + l-saldo. /* Más de 90 días */
                                END.
                END.  
            END.    /*_ RNPC _*/
           
            IF MovCliente.Id-MC = 3 THEN 
            DO:
                ASSIGN 
                    l-numcheque = l-numcheque + 1.
                IF l-saldo = 0 THEN ASSIGN l-chequepag = l-chequepag + 1.
            END.
    
            /*_ RNPC - Obtengo el s�mbolo de la moneda _*/
            IF l-credito <> 0 THEN 
                l-RefValor = (MovCliente.Documento + (IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN "&" ELSE '')).
            ELSE 
            DO:
                IF MovCliente.Id-Moneda > 1 THEN 
                DO:
                    FIND FIRST Moneda WHERE Moneda.Id-Moneda = MovCliente.Id-Moneda NO-LOCK NO-ERROR.
                    IF AVAILABLE Moneda THEN ASSIGN l-RefValor = Moneda.Simbolo.
                END.
                ELSE ASSIGN l-RefValor = "". 
            END.    
              
        
            FIND TabMC OF MovCliente NO-LOCK NO-ERROR.
            CREATE ttDetalle.  
            ASSIGN 
                ttDetalle.IdCliente   = MovCliente.Id-Cliente
                ttDetalle.Documento   = MovCliente.RefSaldo
                ttDetalle.Fecha       = MovCliente.FecReg
                ttDetalle.Descripcion = IF AVAILABLE TabMc THEN TabMC.Descr ELSE ""
                ttDetalle.Cargo       = MovCliente.Importe 
                WHEN MovCliente.Importe > 0
                ttDetalle.Credito     = (MovCliente.Importe * -1) 
                WHEN MovCliente.Importe <= 0
                ttDetalle.Saldo       = 0.01
                ttDetalle.Margen      = ?
                ttDetalle.id-mc       = IF AVAILABLE MovCliente THEN MovCliente.Id-MC ELSE 0  // . movcliente.id-mc
                ttDetalle.Antiguedad  = TODAY - l-fecha 
                ttDetalle.Referencia  = IF MovCliente.Id-NCR <> "" THEN  
                                 MovCliente.Id-NCR ELSE (MovCliente.Documento + 
                                (IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN "&" ELSE '')) 
                ttDetalle.Rec         = RECID(TabMC)    
                ttDetalle.NotaCredito = TRIM(MovCliente.Id-NCR) <> "" .   
             
            /* Buscar Acuse con base en la referencia que acabamos de construir */
            FIND FIRST Acuse 
                WHERE Acuse.Id-Acuse = ttDetalle.Referencia
                NO-LOCK NO-ERROR.

            /* Asignar TipoAcuse si se encontró el Acuse */
            IF AVAILABLE Acuse THEN
                ttDetalle.TipoAcuse = Acuse.Tipo.
            ELSE
                ttDetalle.TipoAcuse = "".      
                
            FIND Factura WHERE Factura.Id-Factura = MovCliente.RefSaldo NO-LOCK NO-ERROR.
            IF AVAILABLE Factura THEN 
            DO:
                IF MovCliente.Id-MC = 1 THEN 
                DO:
                    ASSIGN 
                        ttDetalle.PlazoFactura     = Factura.Plazo
                        ttDetalle.Referencia       = Factura.Id-Fiscal   
                        ttDetalle.FacAutorizadoPor = Factura.autorizado-por.   
                END.
                ELSE
                    ASSIGN ttDetalle.PlazoFactura = (MovCliente.FecReg - Factura.FecReg).
            END.
            
            IF MovCliente.Id-MC = 1 THEN 
            DO:
                FOR EACH Devolucion WHERE Devolucion.Id-Factura = MovCliente.RefSaldo AND
                    Devolucion.TipoVenta = 3 AND
                    Devolucion.VtaCanc    = TRUE NO-LOCK.
                    ASSIGN 
                        ttDetalle.Referencia = "D" + STRING(Devolucion.Id-Dev).
                END. /* del end de devolucion */
            END. /* del if movcliente.id-mc */
    
            /*_ RNPC - Obtengo Referencia _*/
            IF MovCliente.Id-MC = 1 AND (ttDetalle.Referencia = "" OR ttDetalle.Referencia = ttDetalle.Documento) THEN 
            DO:
                IF MovCliente.Id-Moneda > 1 THEN 
                DO:
                    FIND FIRST Moneda WHERE Moneda.Id-Moneda = MovCliente.Id-Moneda NO-LOCK NO-ERROR.
                    IF AVAILABLE Moneda THEN ASSIGN ttDetalle.Referencia = Moneda.simbolo.
                    l-HayMonedaEX = TRUE.
                END.
            END.
   
            /*_ RNPC - Por cada movimiento diferente _*/
            IF LAST-OF(MovCliente.RefSaldo) THEN 
            DO:            
                ASSIGN 
                    l-ant           = TODAY - l-fecha
                    l-saldo         = (ACCUM TOTAL BY MovCliente.RefSaldo l-cargo) +
                                      (ACCUM TOTAL BY MovCliente.RefSaldo l-credito)
                    l-saldoME       = l-cargoME + l-creditoME
                    ttDetalle.Saldo = l-saldo  
                    l-hubo          = TRUE.      

            
                IF l-saldoME > 0 THEN l-saldo = l-saldo + l-saldoME.    /*_ RNPC _*/
                ACCUMULATE l-saldo * l-ant (TOTAL).
            END.
            /*_ RNPC - Suma los totales de cr�dito y cargo al terminar de barrer los registros _*/
            IF LAST(b-mov.fecreg) /* AND LAST(MovCliente.REfSaldo) */   THEN 
            DO:
                ASSIGN 
                    l-saldo   = l-totcargo + l-totcredito
                    l-saldoME = l-totcargoME + l-totcreditoME.
            END.
            ASSIGN 
                l-linea2 = l-linea2 + 1.       
          
        END. /* DEL for each MovCliente */
    END. /* del for each a b-mov*/
    FOR EACH ttDetalle :
        FIND RefPortal WHERE RefPortal.Id-Cliente = ttDetalle.IdCliente 
            AND RefPortal.Id-Factura = ttDetalle.Documento  NO-LOCK NO-ERROR.
       
        ASSIGN 
            ttDetalle.FolioEstatus = (IF AVAILABLE RefPortal THEN RefPortal.Estatus ELSE "")
            ttDetalle.Acuse        = (IF AVAILABLE RefPortal THEN RefPortal.Acuse ELSE "")
           
            ttDetalle.Registro     = (IF AVAILABLE RefPortal THEN RefPortal.FecReg ELSE ?).   
           
    END.   
    
    CREATE ttCartera.
    ASSIGN
        ttCartera.id        = 1
        ttCartera.IdCliente = lCliente.
    IF l-saldo > 0 THEN 
    DO:
        ASSIGN
            ttCartera.saldo        = l-saldo
            ttCartera.Moneda       = "$"
            ttCartera.porvencer    = l-pvencer
            ttCartera.montovencido = l-saldo - l-pvencer.
    END.
    /* ELSE IF l-saldoME > 0 THEN 
         DO:
             ASSIGN
                 ttCartera.saldo        = l-saldoME
                 ttCartera.porvencer    = l-pvencerME
                 ttCartera.montovencido = l-saldoME - l-pvencerME.
         END. */
    ELSE 
    DO:
        ASSIGN 
            ttCartera.Moneda       = "$"
            ttCartera.saldo        = 0
            ttCartera.porvencer    = 0
            ttCartera.montovencido = 0.
    END.  
    
    ASSIGN     
        ttcartera.treinta        = l-tot30 
        ttcartera.sesenta        = l-tot60
        ttcartera.noventa        = l-tot90 
        ttcartera.noventamas     = l-totMas90 
        ttcartera.margenpromedio = ?.  
        
    IF l-saldoME > 0 THEN 
    DO:   /* JASS18062025 */
        CREATE ttCartera.
        ASSIGN
            ttCartera.id             = 2
            ttCartera.IdCliente      = lCliente
            ttCartera.Moneda         = "US"
            ttCartera.saldo          = l-saldoME
            ttCartera.porvencer      = l-pvencerME
            ttCartera.montovencido   = l-saldoME - l-pvencerME
            ttCartera.treinta        = l-tot30ME
            ttCartera.sesenta        = l-tot60ME
            ttCartera.noventa        = l-tot90ME
            ttCartera.noventamas     = l-tot91ME
            ttCartera.margenpromedio = ?.
    END.

    ASSIGN 
        l-acomodo = 10.
    FOR EACH bk-saldo WHERE bk-saldo.id-mc <= 3 NO-LOCK BY bk-saldo.Fecha :
        FIND ttDetalle WHERE RECID(ttDetalle) = RECID(bk-saldo) EXCLUSIVE NO-ERROR.
        ASSIGN 
            ttDetalle.Acomodo = l-acomodo 
            l-acomodo         = l-acomodo + 10.
        FOR EACH bbk-saldo WHERE bbk-saldo.Id-MC > 3 AND
            bbk-saldo.Documento = bk-saldo.Documento NO-LOCK
            BY bbk-saldo.Id-MC.
            FIND ttDetalle WHERE RECID(ttDetalle) = RECID(bbk-saldo) EXCLUSIVE NO-ERROR.
            ASSIGN 
                ttDetalle.acomodo = l-acomodo 
                l-acomodo         = l-acomodo + 10. 
        END. /* del bbk-saldo */
    END. /* del bk-saldo */   
    
    FOR EACH ttDetalle NO-LOCK:
        
        /* Si PlazoFactura es 0, lo ajustamos */
        IF ttDetalle.PlazoFactura = 0 THEN 
            ASSIGN ttDetalle.PlazoFactura = ?.

        /* Si Cargo es 0, lo ajustamos */
        IF ttDetalle.Cargo = 0 THEN 
            ASSIGN ttDetalle.Cargo = ?.

        /* Si Credito es 0, lo ajustamos */
        IF ttDetalle.Credito = 0 THEN 
            ASSIGN ttDetalle.Credito = ?.

        /* Si Saldo es menor o igual a 0.01, lo ajustamos */
        IF ttDetalle.Saldo = 0.01 THEN 
            ASSIGN ttDetalle.Saldo = ?.

        /* Si la Referencia es igual al Documento, ajustamos la Referencia */
        IF ttDetalle.Referencia = ttDetalle.Documento THEN 
            ASSIGN ttDetalle.Referencia = "".
        
        IF ttDetalle.Saldo <= 0.01 THEN
            ASSIGN ttDetalle.Antiguedad = ?.
            
        /* Solo mantener Antiguedad si Descripción inicia con "fact" (no distingue mayúsculas/minúsculas) */
        IF NOT (ttDetalle.Descripcion BEGINS "fact") THEN
            ASSIGN ttDetalle.Antiguedad = ?.  
    END.
    
    /* JASS11092025 */  
    /* Si existe saldo en id=2 pero el id=1 está en 0, eliminar el id=1 */
    IF CAN-FIND(FIRST ttCartera WHERE ttCartera.id = 2 AND ttCartera.saldo > 0) 
        AND CAN-FIND(FIRST ttCartera WHERE ttCartera.id = 1 AND ttCartera.saldo = 0) THEN 
    DO:
    
        FIND FIRST ttCartera WHERE ttCartera.id = 1 AND ttCartera.saldo = 0 NO-ERROR.
        IF AVAILABLE ttCartera THEN DELETE ttCartera.
    END.  
      
      
    /* regla de prioridad */
    IF CAN-FIND(FIRST ttCartera WHERE ttCartera.id = 1 AND ttCartera.saldo > 0) THEN 
        v-id-a-procesar = 1.
    ELSE IF CAN-FIND(FIRST ttCartera WHERE ttCartera.id = 2 AND ttCartera.saldo > 0) THEN 
            v-id-a-procesar = 2.
        ELSE 
            v-id-a-procesar = 1. /* ninguno trae saldo, forzar id=1 */

    FOR EACH ttCartera WHERE ttCartera.id = v-id-a-procesar:
        DO TRANSACTION:                                                                             

            RUN cxcb0270.p(INPUT ttCartera.IdCliente, INPUT TODAY, OUTPUT l-dia-max , OUTPUT l-prompago).
            RUN cxcd0010.p(INPUT ttCartera.IdCliente,OUTPUT l-prompago2).
            RUN programas/margencte.p(INPUT ttCartera.IdCliente, OUTPUT v-margen).
            IF l-dia-max = ? THEN l-dia-max = 0.
            RUN DepositoSantander(INPUT ttCartera.IdCliente, OUTPUT vTotal).
            ASSIGN  
                ttCartera.diacartera     = l-dia-max
                ttCartera.promedio       = INTEGER(ROUND(l-prompago2, 0))
                ttCartera.margenpromedio = v-margen
                ttCartera.deposito       = vTotal.   
        END.
    END.
    
    /*
    IF CAN-FIND(FIRST ttCartera WHERE ttCartera.id = 1 AND ttCartera.saldo >0) THEN 
    DO:
        FOR EACH ttCartera WHERE ttCartera.id = 1:
            DO TRANSACTION:                                                                             
                RUN cxcb0270.p(INPUT ttCartera.IdCliente, INPUT TODAY, OUTPUT l-dia-max , OUTPUT l-prompago).
                RUN programas/margencte.p(INPUT ttCartera.IdCliente, OUTPUT v-margen).
                IF l-dia-max = ? THEN l-dia-max = 0.
            END.
            ASSIGN  
                ttCartera.diacartera     = l-dia-max
                ttCartera.promedio       = INTEGER(ROUND(l-prompago, 0))
                ttCartera.margenpromedio = v-margen.
        END.
    END.
    ELSE IF CAN-FIND(FIRST ttCartera WHERE ttCartera.id = 2) THEN 
        DO:
            FOR EACH ttCartera WHERE ttCartera.id = 2:
                DO TRANSACTION:                                                                             
                    RUN cxcb0270.p(INPUT ttCartera.IdCliente, INPUT TODAY, OUTPUT l-dia-max , OUTPUT l-prompago).
                    RUN programas/margencte.p(INPUT ttCartera.IdCliente, OUTPUT v-margen).
                    IF l-dia-max = ? THEN l-dia-max = 0.
                END.
                ASSIGN  
                    ttCartera.diacartera     = l-dia-max
                    ttCartera.promedio       = INTEGER(ROUND(l-prompago, 0))
                    ttCartera.margenpromedio = v-margen.
            END.
        END.
    */   
    
    /*
   FOR EACH ttCartera  :
       DO TRANSACTION:                                                                             
           RUN cxcb0270.p(INPUT ttCartera.IdCliente,INPUT TODAY,OUTPUT l-dia-max ,OUTPUT l-prompago).
           RUN programas/margencte.p(INPUT ttCartera.IdCliente,OUTPUT v-margen).
           IF l-dia-max = ? THEN l-dia-max = 0.
       END.
       ASSIGN  
           ttCartera.diacartera     = l-dia-max
           ttCartera.promedio       = INTEGER(ROUND(l-prompago, 0)) /* Redondea a un entero */
           ttCartera.margenpromedio = v-margen .    
   END. 
    */
    
    /* Si una factura ya esta pagada debe mostrar el margen NETO */ 
    DEFINE BUFFER bfFact  FOR ttDetalle.
    DEFINE BUFFER bf-Fact FOR ttDetalle.
    FOR EACH ttDetalle WHERE ttDetalle.Saldo = 0.0:

        DO TRANSACTION:
            RUN GetMargenFactura(INPUT ttDetalle.Documento, OUTPUT v-margen,OUTPUT v-marneto).
        END.

        FIND FIRST bfFact WHERE 
            bfFact.Documento = ttDetalle.Documento AND
            bfFact.Descripcion BEGINS "Fact"
            NO-ERROR.

        IF AVAILABLE bfFact THEN
            ASSIGN bfFact.Margen = v-marneto.

    END. 
    FOR EACH ttDetalle WHERE ttDetalle.Saldo > 0.0:

        DO TRANSACTION:   
            RUN GetMargenFactura(INPUT ttDetalle.Documento, OUTPUT v-margen,OUTPUT v-marneto).
        END.

        FIND FIRST bf-Fact WHERE 
            bf-Fact.Documento = ttDetalle.Documento AND
            bf-Fact.Descripcion BEGINS "Fact"
            NO-ERROR.

        IF AVAILABLE bf-Fact THEN
            ASSIGN bf-Fact.Margen = v-margen.

    END.   
    /*
    FOR EACH ttDetalle WHERE ttDetalle.Descripcion BEGINS "Fact":
        DO TRANSACTION:                                                                             
            RUN calcularMargenNeto(INPUT ttDetalle.Documento,OUTPUT  v-marneto).
        END.
        ASSIGN  
            ttDetalle.Margen = v-marneto. 
    END. */
    RETURN.              
END PROCEDURE.    


PROCEDURE calcularMargenNeto:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
       Programa: calcularMargenNeto.p
       Función : Calcular el margen bruto de una factura
       Autor   : [Tu nombre]
       Fecha   : [Fecha]
    */     


    DEFINE INPUT  PARAMETER pi-factura AS CHARACTER NO-UNDO.  /* Número de factura */
    DEFINE OUTPUT PARAMETER po-marventa AS DECIMAL   NO-UNDO  FORMAT "-zzz9.99%".  /* Margen bruto calculado */

    DEFINE VARIABLE l-tcosto   LIKE factura.subtotal NO-UNDO.
    DEFINE VARIABLE l-tprecio  LIKE factura.subtotal NO-UNDO.
    DEFINE VARIABLE l-pagos    LIKE factura.subtotal NO-UNDO.  
    DEFINE VARIABLE l-desc     LIKE factura.subtotal NO-UNDO.
    DEFINE VARIABLE l-dev      LIKE factura.subtotal NO-UNDO.
    DEFINE VARIABLE l-desinc   LIKE factura.subtotal NO-UNDO.
    DEFINE VARIABLE l-porcdesc AS DECIMAL NO-UNDO.
    DEFINE VARIABLE l-marventa AS DECIMAL NO-UNDO FORMAT "-zzz9.99%".
    DEFINE VARIABLE l-marneto  AS DECIMAL NO-UNDO.  

    /* Buscar la factura */
    FIND factura WHERE factura.id-factura = pi-factura NO-LOCK NO-ERROR.

    IF AVAILABLE factura THEN 
    DO:
        IF factura.feccanc = ? THEN 
        DO:
            /* Calcular costos y precios */
            l-tcosto = 0.
            l-tprecio = 0.

            FOR EACH detfactura OF factura NO-LOCK:
                l-tcosto = l-tcosto + (detfactura.costo * detfactura.cant).
                l-tprecio = l-tprecio + (detfactura.precunit * detfactura.cant * (1 - (detfactura.descto / 100))).
            END.

            /* Calcular pagos, descuentos y devoluciones */
            l-pagos = 0.
            l-desc = 0.
            l-dev = 0.

            FOR EACH movcliente WHERE movcliente.refsaldo = pi-factura NO-LOCK:
                IF (movcliente.id-mc >= 57 AND movcliente.id-mc <= 62) OR
                    (movcliente.id-mc = 90 OR movcliente.id-mc = 96) THEN
                    l-pagos = l-pagos + (movcliente.importe * -1).
                IF movcliente.id-mc = 63 OR movcliente.id-mc = 64 OR
                    movcliente.id-mc = 66 OR movcliente.id-mc = 68 THEN
                    l-desc = l-desc + (movcliente.importe * -1).
                IF movcliente.id-mc = 65 THEN
                    l-dev = l-dev + (movcliente.importe * -1).
            END.

            FOR EACH HistMovCte WHERE HistMovCte.refsaldo = pi-factura NO-LOCK:
                IF (HistMovCte.id-mc >= 57 AND HistMovCte.id-mc <= 62) OR
                    (HistMovCte.id-mc = 90 OR HistMovCte.id-mc = 96) THEN
                    l-pagos = l-pagos + (HistMovCte.importe * -1).
                IF HistMovCte.id-mc = 63 OR HistMovCte.id-mc = 64 OR
                    HistMovCte.id-mc = 66 OR HistMovCte.id-mc = 68 THEN
                    l-desc = l-desc + (HistMovCte.importe * -1).
                IF HistMovCte.id-mc = 65 THEN
                    l-dev = l-dev + (HistMovCte.importe * -1).
            END.

            /* Calcular margen neto */
            IF l-tcosto > 0 THEN
                l-marventa = (l-tprecio / l-tcosto - 1) * 100.
            ELSE
                l-marventa = 100.

            l-porcdesc = l-desc / (l-pagos + l-desc) * 100.
            l-marneto = ((l-tprecio * (1 - (l-porcdesc / 100))) / l-tcosto - 1) * 100.

            /* Asignar el valor de salida */
            po-marventa = l-marventa.
        END.  
        ELSE 
        DO:       
            MESSAGE "Factura Cancelada..." VIEW-AS ALERT-BOX TITLE "Error!".
            po-marventa = ?.  /* Retornar valor nulo si la factura está cancelada */
        END.
    END.  
    ELSE 
    DO:
        MESSAGE "Factura Inexistente" VIEW-AS ALERT-BOX TITLE "Error!".
        po-marventa = ?.  /* Retornar valor nulo si la factura no existe */
    END.      
     
  
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetMargenFactura:
    DEFINE INPUT  PARAMETER lFactura   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER lMarVenta AS DECIMAL   NO-UNDO FORMAT "-zzz9.99%".
    DEFINE OUTPUT PARAMETER lMarNeto  AS DECIMAL   NO-UNDO FORMAT "-zzz9.99%".

    DEFINE VARIABLE lTCosto   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lTPrecio  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lPagos    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lDesc     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lDev      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lPUnit    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lPorcDesc AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lDesInc   AS DECIMAL NO-UNDO.

    ASSIGN 
        lTCosto   = 0
        lTPrecio  = 0
        lPagos    = 0
        lDesc     = 0
        lDev      = 0
        lPorcDesc = 0
        lDesInc   = 0
        lMarVenta = 0
        lMarNeto  = 0.

    FIND Factura WHERE Factura.Id-Factura = lFactura NO-LOCK NO-ERROR.
    IF AVAILABLE Factura AND Factura.FecCanc = ? THEN 
    DO:
        FOR EACH DetFactura OF Factura NO-LOCK:
            lPUnit = DetFactura.PrecUnit * (1 - (DetFactura.Descto / 100)).
            lTCosto = lTCosto + (DetFactura.Costo * DetFactura.Cant).
            lTPrecio = lTPrecio + (lPUnit * DetFactura.Cant).
        END.

        FOR EACH MovCliente WHERE MovCliente.RefSaldo = lFactura NO-LOCK:
            CASE MovCliente.Id-MC:
                WHEN 57 THEN 
                    lPagos = lPagos + (MovCliente.Importe * -1).
                WHEN 58 THEN 
                    lPagos = lPagos + (MovCliente.Importe * -1).
                WHEN 59 THEN 
                    lPagos = lPagos + (MovCliente.Importe * -1).
                WHEN 60 THEN 
                    lPagos = lPagos + (MovCliente.Importe * -1).
                WHEN 61 THEN 
                    lPagos = lPagos + (MovCliente.Importe * -1).
                WHEN 62 THEN 
                    lPagos = lPagos + (MovCliente.Importe * -1).
                WHEN 90 THEN 
                    lPagos = lPagos + (MovCliente.Importe * -1).
                WHEN 96 THEN 
                    lPagos = lPagos + (MovCliente.Importe * -1).
                WHEN 63 THEN 
                    lDesc = lDesc + (MovCliente.Importe * -1).
                WHEN 64 THEN 
                    lDesc = lDesc + (MovCliente.Importe * -1).
                WHEN 66 THEN 
                    lDesc = lDesc + (MovCliente.Importe * -1).
                WHEN 68 THEN 
                    lDesc = lDesc + (MovCliente.Importe * -1).
                WHEN 65 THEN 
                    lDev = lDev + (MovCliente.Importe * -1).
            END CASE.

        END.

        FOR EACH HistMovCte WHERE HistMovCte.RefSaldo = lFactura NO-LOCK:
            CASE HistMovCte.Id-MC:
                WHEN 57 THEN 
                    lPagos = lPagos + (HistMovCte.Importe * -1).
                WHEN 58 THEN 
                    lPagos = lPagos + (HistMovCte.Importe * -1).
                WHEN 59 THEN 
                    lPagos = lPagos + (HistMovCte.Importe * -1).
                WHEN 60 THEN 
                    lPagos = lPagos + (HistMovCte.Importe * -1).
                WHEN 61 THEN 
                    lPagos = lPagos + (HistMovCte.Importe * -1).
                WHEN 62 THEN 
                    lPagos = lPagos + (HistMovCte.Importe * -1).
                WHEN 90 THEN 
                    lPagos = lPagos + (HistMovCte.Importe * -1).
                WHEN 96 THEN 
                    lPagos = lPagos + (HistMovCte.Importe * -1).
                WHEN 63 THEN 
                    lDesc = lDesc + (HistMovCte.Importe * -1).
                WHEN 64 THEN 
                    lDesc = lDesc + (HistMovCte.Importe * -1).
                WHEN 66 THEN 
                    lDesc = lDesc + (HistMovCte.Importe * -1).
                WHEN 68 THEN 
                    lDesc = lDesc + (HistMovCte.Importe * -1).
                WHEN 65 THEN 
                    lDev = lDev + (HistMovCte.Importe * -1).
            END CASE.
        END.
    END.
    ELSE     
    DO: /* Buscar en REMISION */
        FIND Remision WHERE Remision.Id-Remision = lFactura NO-LOCK NO-ERROR.
        IF AVAILABLE Remision AND Remision.FecCanc = ? THEN 
        DO:
            lDesInc = Remision.Descuento.

            FOR EACH DetRemis OF Remision NO-LOCK:
                lPUnit = DetRemis.PrecUnit * (1 - (DetRemis.PorcDesc / 100)).
                lTCosto = lTCosto + (DetRemis.Costo * DetRemis.Cant).
                lTPrecio = lTPrecio + (lPUnit * DetRemis.Cant).
            END.

            FOR EACH MovCaja WHERE MovCaja.Refer = Remision.Id-Remision
                AND MovCaja.Fecreg = Remision.Fecreg NO-LOCK,
                EACH DetMovC WHERE DetMovC.Id-Caja = MovCaja.Id-Caja
                AND DetMovC.Folio = MovCaja.Folio NO-LOCK:
                CASE DetMovC.Id-Tp:
                    WHEN 57 THEN 
                        lPagos = lPagos + DetMovC.MontoPago.
                    WHEN 58 THEN 
                        lPagos = lPagos + DetMovC.MontoPago.
                    WHEN 59 THEN 
                        lPagos = lPagos + DetMovC.MontoPago.
                    WHEN 60 THEN 
                        lPagos = lPagos + DetMovC.MontoPago.
                    WHEN 61 THEN 
                        lPagos = lPagos + DetMovC.MontoPago.
                    WHEN 62 THEN 
                        lPagos = lPagos + DetMovC.MontoPago.
                    WHEN 63 THEN 
                        lDesc = lDesc + DetMovC.MontoPago.
                    WHEN 64 THEN 
                        lDesc = lDesc + DetMovC.MontoPago.
                    WHEN 66 THEN 
                        lDesc = lDesc + DetMovC.MontoPago.
                    WHEN 68 THEN 
                        lDesc = lDesc + DetMovC.MontoPago.
                    WHEN 65 THEN 
                        lDev = lDev + DetMovC.MontoPago.
                END CASE.
            END.
        END.
        ELSE 
        DO:
           // RETURN "Factura o remisión no encontrada o cancelada.".
        END.
    END.

    /* Cálculos finales (válido para ambas rutas) */
    IF lTCosto > 0 THEN
        lMarVenta = (1 - (lTCosto / (lTPrecio - lDesInc))) * 100.
    ELSE
        lMarVenta = 100.

    IF (lPagos + lDesc) > 0 THEN
        lPorcDesc = lDesc / (lPagos + lDesc) * 100.

    IF lTCosto > 0 THEN
        lMarNeto = (1 - (lTCosto / ((lTPrecio - lDesInc) * (1 - (lPorcDesc / 100))))) * 100.
    ELSE
        lMarNeto = 100.  

END PROCEDURE.
PROCEDURE DepositoSantander:
    /* =========================================================================
        File : 
        Purpose : Regresa la suma de importes de los depósitos por cliente
        Author : 
       ========================================================================= */

    DEFINE INPUT  PARAMETER ipIdCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opTotal     AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dSuma AS DECIMAL NO-UNDO INITIAL 0.

    DEFINE BUFFER b-DepBanco FOR DepBanco. 
    /* -----------------------------------------------------------
       Busca los depósitos válidos de un cliente en los últimos 30 días
       ----------------------------------------------------------- */
   
  
    FOR EACH b-DepBanco
        WHERE b-DepBanco.Id-Cliente = ipIdCliente
        AND b-DepBanco.FecDep     >= TODAY - 180
        AND b-DepBanco.Conciliado = FALSE
        AND b-DepBanco.Activo     = TRUE
        AND b-DepBanco.Tipo       <> 4  
        NO-LOCK:

        dSuma   = dSuma + b-DepBanco.Importe.
    END.    
    /* Retorna los resultados */
    opTotal  = dSuma.
  
END PROCEDURE.
