@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : depvisor.p
    Purpose     : /DepositoVisor

    Syntax      :

    Description :   

    Author(s)   : sis10
    Created     : Mon Oct 28 07:15:45 CST 2024
    Notes       :
  ----------------------------------------------------------------------
  
  Ticket 1096 Enviar datos de la ultima Carga de Datos
              GetCargaDatos
    JASS27062025   
     
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
    INDEX idx-cliente NumCliente ASCENDING FechaDeposito DESCENDING Hora DESCENDING .
   // INDEX idx-respo FechaDeposito DESCENDING Hora DESCENDING
   // INDEX Idx-Dep NumCliente FechaDeposito DESC. 
DEFINE DATASET dsDeposito FOR ttDeposito.


DEFINE TEMP-TABLE tt-Documentos NO-UNDO
    FIELD Id-Cliente     LIKE MovCliente.Id-Cliente
    FIELD RazonRemision  LIKE Remision.RazonSocial
    FIELD Refsaldo       LIKE MovCliente.Refsaldo
    FIELD FecReg         LIKE MovCliente.FecReg
    FIELD FecVenc        LIKE MovCliente.FecVenc
    FIELD Id-MC          LIKE MovCliente.Id-MC
    FIELD Importe        LIKE MovCliente.Importe
    FIELD Id-Moneda      LIKE MovCliente.Id-Moneda
    FIELD TipoCambio     LIKE MovCliente.TipoCambio
    FIELD Saldo          LIKE MovCliente.Saldo
    FIELD SaldoSinDsc    LIKE MovCliente.Saldo       // 2019-09-10
    FIELD TipoDoc        AS CHAR 
    FIELD GenAcuse       AS LOGICAL
    FIELD Aplica         AS LOGICAL
    FIELD DescNoAplicado AS LOGICAL                 // 2019-09-10
    FIELD DescVigente    AS LOGICAL 
    INDEX Idx-Doc   Id-MC  Id-Cliente Saldo Importe
    INDEX Idx-Docto Id-MC  Id-Cliente Saldo GenAcuse
    INDEX Idx-Saldo Saldo  ASC
    INDEX Idx-Fecha FecReg ASC.  
    
DEFINE TEMP-TABLE tt-Cliente NO-UNDO
    FIELD Id-Cliente LIKE Cliente.Id-Cliente
    FIELD Descto1    LIKE Cliente.descPP1 
    FIELD Descto2    LIKE Cliente.descPP2
    INDEX Idx-Cte Id-Cliente.    

DEFINE VARIABLE l-RecID        AS RECID     NO-UNDO.
DEFINE VARIABLE l-impMov       LIKE MovCliente.Importe NO-UNDO.
DEFINE VARIABLE l-impDevMov    LIKE MovCliente.Importe NO-UNDO.
DEFINE VARIABLE l-coincide     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-impPaso      AS DECIMAL   NO-UNDO INITIAL 0.
DEFINE VARIABLE l-impAntesDesc LIKE MovCliente.Importe NO-UNDO. 
DEFINE VARIABLE l-descr        AS CHARACTER.
DEFINE BUFFER bf-tt-Documentos FOR tt-Documentos.
DEFINE BUFFER bf_DepBanco      FOR DepBanco.
DEFINE BUFFER bf-MovCliente    FOR MovCliente.  

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCargaDatos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:  JASS27062025
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER cMensaje AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFechaCarga AS CHARACTER NO-UNDO.
    
    /* Log de inicio */
    LOG-MANAGER:WRITE-MESSAGE("/DepositoCargaDatos [GET] >>> Iniciando obtención de última fecha de carga | FechaHora: " + STRING(NOW)).
    FOR EACH DepBanco NO-LOCK BY FecCarga DESCENDING:

            ASSIGN
            cFechaCarga = STRING(DepBanco.FecCarga).
            cMensaje = cFechaCarga.
    /* Log de primer resultado */
    LOG-MANAGER:WRITE-MESSAGE("/DepositoCargaDatos [GET] >>> Fecha de carga encontrada: " + cFechaCarga).
    LEAVE.
    END.
    RETURN.       
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDepositos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER iMes     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER Year     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER Aplicado AS LOGICAL NO-UNDO. 
    DEFINE INPUT PARAMETER Nivel    AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER Tipo     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER Activo   AS LOGICAL NO-UNDO. 
    DEFINE OUTPUT PARAMETER TABLE FOR ttDeposito.  

    EMPTY TEMP-TABLE tt-Cliente.
    EMPTY TEMP-TABLE tt-Documentos.
    EMPTY TEMP-TABLE ttDeposito.
   
     /* Log de inicio */
    LOG-MANAGER:WRITE-MESSAGE(
        SUBSTITUTE("/DepositoVisor [GET] >>> Inicia consulta | Mes: &1 | Año: &2 | Aplicado: &3 | Nivel: &4 | Tipo: &5 | Activo: &6 | FechaHora: &7",
            STRING(iMes),
            STRING(Year),
            STRING(Aplicado),
            STRING(Nivel),
            STRING(Tipo),
            STRING(Activo),
            STRING(NOW))
    ).

    DEFINE VAR iFechaInicio AS DATE.
    ASSIGN  
        l-lista = "1,2,4,5,6,7,8,9,10,11"
        l-num   = 0.
    
    /* Para el deposito de Aplicaciones lo Pendiente por Aplicar debe traer de todas las fechas */  
    IF iMes     = ? THEN iMes      = MONTH(TODAY) .
    IF Year     = ? THEN Year     = YEAR(TODAY). 
    IF Aplicado = ? THEN Aplicado = FALSE. /* Si no manda parametro envia Pendiente Por Aplicar */
    IF Nivel    = ? THEN Nivel    = 0. 
    IF Tipo = ? THEN 
        Tipo = 1. /* Si no mandan parametro Envia Tipo Credito*/
    ELSE IF Tipo <> 1 AND Tipo <> 2 AND Tipo <> 3 AND Tipo <> 4 THEN 
        DO:
            RETURN. /* Salir del procedimiento */
        END.


    /* Si Aplicado = FALSE, solo considerar los últimos 30 días */
    IF Aplicado = FALSE THEN 
    DO:
        ASSIGN
            iFechaInicio = TODAY - 90.
    END.


    /* Nivel 2 es JAGR; Nivel 3 DJGL,NMRA,RGP */
    IF Nivel = 2 OR Nivel = 3 THEN 
    DO: 
        ASSIGN 
            l-desactivado = TRUE.        
    END.
    ELSE 
    DO:
        ASSIGN 
            l-desactivado = FALSE.       
    END.

    FOR EACH DepBanco
        WHERE DepBanco.Id-Cliente >= 0   
        AND ((Aplicado = FALSE AND DepBanco.FecDep >= iFechaInicio) /* Últimos 30 días */
        OR (Aplicado = TRUE AND (iMes = 0 OR MONTH(DepBanco.FecDep) = iMes) 
        AND (Year = 0 OR YEAR(DepBanco.FecDep) = Year))) /* Si es aplicado usa iMes y Year */
        AND (Activo = ? OR DepBanco.Activo = Activo) /* Filtra por Activo si no es ? */
        AND DepBanco.Conciliado = Aplicado 
        USE-INDEX idx-DepBcoActivo NO-LOCK:

          
        /* Variables por asignar */
        ASSIGN 
            v-documento  = ""
            v-tipo-cte   = " "
            l-num        = l-num + 1
            l-conciliado = ""
            l-descr      = "".
        
        IF Tipo = 1 THEN 
        DO: 
            IF DepBanco.TipoCte <> 1 THEN NEXT. /* Solo registros con TipoCte = 1 */
            ASSIGN 
                v-tipo-cte  = "Credito" 
                v-documento = DepBanco.Id-Acuse.
        END.
        IF Tipo = 2 THEN 
        DO: 
            IF DepBanco.TipoCte <> 2 THEN NEXT. /* Solo registros con TipoCte = 2 */
            ASSIGN 
                v-tipo-cte  = "Credito" 
                v-documento = DepBanco.Id-Acuse.
        END.
        IF Tipo = 3 THEN 
        DO: 
            IF DepBanco.TipoCte <> 3 THEN NEXT. /* Solo registros con TipoCte = 3 */
            ASSIGN 
                v-tipo-cte  = "Credito" 
                v-documento = DepBanco.Id-Acuse.
        END.  
        IF Tipo = 4 THEN 
        DO: /* Quita los tipo credito envia tipo Contado */
            IF DepBanco.TipoCte <> 4 THEN NEXT.
            ASSIGN 
                v-tipo-cte  = "Contado"     
                v-documento = DepBanco.Id-Remision. 
        END.

        ASSIGN 
            l-RecID = RECID(DepBanco). /* Capturamos el ROWID aquí */

        /* Validación y búsqueda del cliente */
        FIND FIRST Cliente 
            WHERE Cliente.Id-Cliente = DepBanco.Id-Cliente
            NO-LOCK NO-ERROR.        

        /* Verificar si el cliente está en l-lista */
        IF AVAILABLE Cliente THEN 
        DO:
            IF LOOKUP(STRING(Cliente.Id-Cliente), SUBSTITUTE(l-lista, ",", "")) > 0 THEN NEXT.

            /* Validar descuentos y marcar conciliación automática */
            IF (Cliente.descPP1 > 0 OR Cliente.descPP2 > 0) AND DepBanco.TipoCte <> 4 THEN 
            DO:
                ASSIGN 
                    l-conciliado = "D".
            END.
            ELSE 
            DO: 
                ASSIGN 
                    l-conciliado = "".                                                     
            END.
        END.
        /* Búsqueda de otras entidades relacionadas */
        FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = DepBanco.TipoCte NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ClaseCte THEN 
        DO:
            FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
        END.
        FIND FIRST Resp     WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
        FIND FIRST Banco    WHERE Banco.Id-Banco = DepBanco.Id-Banco NO-LOCK NO-ERROR.
        FIND FIRST Asociado WHERE Asociado.Id-Cliente = DepBanco.Id-Cliente NO-LOCK NO-ERROR.
    
        /* Marcar pendientes si Aplicado es FALSE */
        IF Aplicado = FALSE THEN ASSIGN l-pendientes = TRUE.    
    
    // Valido la forma de pago del deposito rutina de cxca0804.p
        ASSIGN 
            l-FormaPago = "Tranferencia". // '03'.
        IF DepBanco.Descripcion MATCHES '*TRAN*' THEN l-FormaPago = "Tranferencia" . //'03'   //TRANSFERENCIA
        ELSE IF DepBanco.Descripcion MATCHES '*EFEC*' THEN l-FormaPago = "Efectivo". //'01' //EFECTIVO
            ELSE IF DepBanco.Descripcion MATCHES '*CHE*' OR 
                    DepBanco.Descripcion MATCHES '*S B COBRO*' THEN l-FormaPago = "Cheque".  //'02'   // CHEQUE
    
        ASSIGN 
            l-descr = DepBanco.Descripcion.
        /* Solo cambio en vista de pendientes por aplicar de credito para quitar bug de boton */
        IF l-descr = "DEP S B COBRO" AND DepBanco.TipoCte <> 4 AND Aplicado = FALSE THEN l-descr = "DEP S B COBR".
        /* Procesar los datos en ttDeposito */
        FIND FIRST ttDeposito 
            WHERE ttDeposito.NumList = l-num 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttDeposito THEN
        DO:
            CREATE ttDeposito.
            ASSIGN
                ttDeposito.NumList         = l-num
                ttDeposito.Clase           = ClaseCte.Descr 
                WHEN AVAILABLE ClaseCte
                ttDeposito.Zona            = " " /* Pendiente definir */
                ttDeposito.TipoDeCliente   = v-tipo-cte
                ttDeposito.NumCliente      = DepBanco.Id-Cliente
                ttDeposito.Cliente         = Cliente.RazonSocial 
                WHEN AVAILABLE Cliente
                ttDeposito.FechaDeposito   = DepBanco.FecDep
                ttDeposito.Hora            = DepBanco.HoraDep
                ttDeposito.Banco           = Banco.Nombre
                ttDeposito.Descripcion     = l-descr // DepBanco.Descripcion
                ttDeposito.Importe         = DepBanco.Importe
                ttDeposito.FechaAplicacion = DepBanco.FecAplica
                ttDeposito.Documento       = v-documento
                ttDeposito.Anticipo        = DepBanco.Id-AcuseAnt
                ttDeposito.Aplicado        = DepBanco.Conciliado
                ttDeposito.Responsable     = Resp.Nombre 
                WHEN AVAILABLE Resp
                ttDeposito.Desactivado     = l-desactivado
                ttDeposito.Rec             = l-RecID
                ttDeposito.Saldo           = DepBanco.Saldo
                ttDeposito.Signo           = DepBanco.Signo
                ttDeposito.Revisado        = FALSE
                ttDeposito.Aut             = l-conciliado 
                ttDeposito.Automatico      = INDEX(ttDeposito.Aut, "*") > 0 //NOT(ttDeposito.Aut = "" OR ttDeposito.Aut = "FC")
                ttDeposito.TipoCte         = DepBanco.TipoCte
                ttDeposito.Asociado        = Asociado.Id-Asociado 
                WHEN AVAILABLE Asociado
                ttDeposito.Activo          = DepBanco.Activo
                ttDeposito.Factura         = Aplicado
                ttDeposito.IdUser          = DepBanco.Id-User
                ttDeposito.FormaDePago     = l-FormaPago
                ttDeposito.Descuento1      = Cliente.DescPP1  
                WHEN AVAILABLE Cliente
                ttDeposito.Descuento2      = Cliente.DescPP2  
                WHEN AVAILABLE Cliente .  
            RELEASE ttDeposito.
        END.  
    
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
                    tt-Cliente.Descto2    = Cliente.descPP2.
            END.  
        END.
    END. /* End del For Each DepBanco */ 

    /* Ejecutar procesos según tipo */
    IF l-pendientes = TRUE AND Activo = TRUE AND (Tipo = 1 OR Tipo = 2 OR Tipo = 3) THEN 
        RUN Movimientos.

    IF l-pendientes = TRUE AND Activo = TRUE AND Tipo = 4 THEN 
        RUN MovimientosContado.  
        RETURN.     
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
    EMPTY TEMP-TABLE tt-Documentos.

    /*_ ******************************************************************** 
        II. Busco movimientos de credito (MovCliente) y contado (Remision) 
        de los clientes encontrados con depositos 
        (Creo tt-Documentos) excluyendo dolares
        ******************************************************************* * _*/
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
               
        
            IF MovCliente.Saldo = MovCliente.Importe THEN ASSIGN l-impMov = MovCliente.Importe - l-impDevMov.   // 2019-09-19
            ELSE l-impMov = MovCliente.Saldo.
        //ASSIGN l-impMov = MovCliente.Importe - l-impDevMov
               
            ASSIGN 
                l-impAntesDesc = l-impMov.     // 2019-09-06
        
            IF tt-Cliente.Descto1 > 0 THEN
                ASSIGN 
                    l-impMov = l-impMov - ROUND(l-impMov * (tt-Cliente.Descto1 / 100),2).
                // 2019-09-11 - l-impMov = ROUND(l-impMov * ((100 - tt-Cliente.Descto1) / 100),2).
            IF tt-Cliente.Descto2 > 0 THEN
                ASSIGN 
                    l-impMov = l-impMov - ROUND(l-impMov * (tt-Cliente.Descto2 / 100),2).
                // 2019-09-11 - l-impMov = ROUND(l-impMov * ((100 - tt-Cliente.Descto2) / 100),2).
                    
        // Creo registro en tabla temporal
            CREATE tt-Documentos.
            ASSIGN 
                tt-Documentos.Id-Cliente     = MovCliente.Id-Cliente
                tt-Documentos.Refsaldo       = MovCliente.Refsaldo
                tt-Documentos.FecReg         = MovCliente.FecReg
                tt-Documentos.Id-MC          = MovCliente.Id-MC
                tt-Documentos.Importe        = MovCliente.Importe   // l-impMov
                tt-Documentos.Saldo          = l-impMov // IF MovCliente.Saldo = MovCliente.Importe THEN l-impMov ELSE MovCliente.Saldo
                tt-Documentos.SaldoSinDsc    = l-impAntesDesc // IF MovCliente.Saldo = MovCliente.Importe THEN (MovCliente.Importe - l-impDevMov) ELSE MovCliente.Saldo   // 2019-09-10
                tt-Documentos.GenAcuse       = TRUE
                tt-Documentos.Aplica         = FALSE
                tt-Documentos.DescNoAplicado = FALSE    // 2019-09-10
                tt-Documentos.DescVigente    = IF (tt-Cliente.Descto1 > 0 OR tt-Cliente.Descto2 > 0) THEN TRUE ELSE FALSE   // 2019-09-10
                l-impMov                     = 0.
        END.
    
        /*_ Creo registros de Tabla temporal con posibles opciones de Remisiones _*/
        FOR EACH Remision WHERE
            Remision.TipoVenta = 2 AND
            Remision.Id-Cliente = tt-Cliente.Id-Cliente AND
            Remision.Pagada = FALSE NO-LOCK:
            
            ASSIGN
                l-impMov = Remision.Tot.
            
            // Creo registro en tabla temporal
            CREATE tt-Documentos.
            ASSIGN 
                tt-Documentos.Id-Cliente     = Remision.Id-Cliente
                tt-Documentos.Refsaldo       = Remision.Id-Remision
                tt-Documentos.FecReg         = Remision.FecReg
                tt-Documentos.Id-MC          = Remision.TipoVenta
                tt-Documentos.Importe        = l-impMov
                tt-Documentos.Saldo          = l-impMov 
                tt-Documentos.GenAcuse       = FALSE
                tt-Documentos.Aplica         = FALSE
                tt-Documentos.DescNoAplicado = FALSE    // 2019-09-10
                tt-Documentos.DescVigente    = FALSE
                l-impMov                     = 0.         
        END.
    END. /* End de la tabla tt-Cliente */  

    /*_ ******************************************************************
        III. Reviso por cada movimiento de deposito si hay movimientos 
        que apliquen directamente o bien que apliquen en combinacion
        ****************************************************************** _*/


    FOR EACH ttDeposito NO-LOCK USE-INDEX idx-cliente: 
        IF ttDeposito.Revisado THEN NEXT.
        l-coincide = FALSE.    
    
        /* ********************************************************* 
             Busco movimiento que coincida con el mismo importe _*/
        FIND FIRST tt-Documentos WHERE
            tt-Documentos.Id-MC <= 3 AND
            tt-Documentos.id-cliente = ttDeposito.NumCliente AND
            tt-Documentos.Saldo > 0 AND
            (tt-Documentos.Saldo = ttDeposito.Importe OR
            ABS(tt-Documentos.Saldo - ttDeposito.Importe) <= 0.01 AND tt-Documentos.DescVigente)
            EXCLUSIVE-LOCK NO-ERROR. // 2019-09-17 - Busco - 1 centavo
        IF AVAILABLE tt-Documentos THEN 
        DO:
            ASSIGN 
                ttDeposito.Revisado   = TRUE 
                ttDeposito.Aut        = ttDeposito.Aut + IF tt-Documentos.GenAcuse THEN '*' ELSE 'FC'
                ttDeposito.Automatico = INDEX(ttDeposito.Aut, "*") > 0 //NOT(ttDeposito.Aut = "" OR ttDeposito.Aut = "FC")
                tt-Documentos.Aplica  = TRUE 
                l-coincide            = TRUE. 
        END.
    
        /* ********************************************************* 
             2019-09-10 - Busco movimiento sin descuento que coincida con el mismo importe _*/
        IF l-coincide = TRUE THEN NEXT.         
        FIND FIRST tt-Documentos WHERE
            tt-Documentos.Id-MC <= 3 AND
            tt-Documentos.id-cliente = ttDeposito.NumCliente AND 
            tt-Documentos.Saldo > 0 AND
            tt-Documentos.Saldo <> ttDeposito.Importe AND 
            (tt-Documentos.SaldoSinDsc = ttDeposito.Importe OR 
            ABS(tt-Documentos.SaldoSinDsc - ttDeposito.Importe) <= 0.01 AND tt-Documentos.DescVigente)      // 2019-09-17 - Busco - 1 centavo
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE tt-Documentos AND tt-Documentos.DescVigente THEN 
        DO:
            ASSIGN 
                ttDeposito.Revisado          = TRUE
                ttDeposito.Aut               = ttDeposito.Aut + IF tt-Documentos.GenAcuse THEN '!*' ELSE 'FC'
                ttDeposito.Automatico        = INDEX(ttDeposito.Aut, "*") > 0 //NOT(ttDeposito.Aut = "" OR ttDeposito.Aut = "FC")
                tt-Documentos.Aplica         = TRUE
                tt-Documentos.DescNoAplicado = IF tt-Documentos.DescVigente THEN TRUE ELSE FALSE
                l-coincide                   = TRUE.
        END.        
    
        /* ********************************************************* */
        /*_ Busco combinaciones de movimientos que puedan coincidir _*/
        IF l-coincide = TRUE THEN NEXT.
        
        FOR EACH tt-Documentos WHERE
            tt-Documentos.Id-MC <= 3 AND
            tt-Documentos.Saldo > 0 AND  
            tt-Documentos.id-cliente = ttDeposito.NumCliente AND
            tt-Documentos.GenAcuse 
            EXCLUSIVE-LOCK 
            BY tt-Documentos.FecReg
            BY tt-Documentos.RefSaldo: 
                         
            
            IF tt-Documentos.Saldo > (ttDeposito.Importe + 0.01) OR tt-Documentos.Aplica OR l-coincide THEN NEXT.  // 2019-09-17 - Sumo 1 centavo
        
            ASSIGN
                l-impPaso  = tt-Documentos.Saldo
                l-coincide = FALSE.
        
            FOR EACH bf-tt-Documentos WHERE
                bf-tt-Documentos.Id-MC <= 3 AND 
                bf-tt-Documentos.id-cliente = tt-Documentos.id-cliente AND 
                bf-tt-Documentos.Saldo <> tt-Documentos.Saldo AND 
                bf-tt-Documentos.GenAcuse
                EXCLUSIVE-LOCK 
                BY bf-tt-Documentos.FecReg
                BY bf-tt-Documentos.RefSaldo:
            
                IF bf-tt-Documentos.FecReg < tt-Documentos.FecReg THEN NEXT.
            
                /* 2019-09-10 */
                IF (l-impPaso + bf-tt-Documentos.Saldo) > (ttDeposito.Importe + 0.01) THEN NEXT.    // 2019-09-17 - Sumo 1 centavo
                ELSE l-impPaso = l-impPaso + bf-tt-Documentos.Saldo.
            
            
            // DEBUG ***********
                /*IF tt-Documentos.id-cliente = 32636 THEN DO:
                    PUT UNFORMATTED ' 1.7) Movimiento= ' + STRING(bf-tt-Documentos.RefSaldo) + ' $ ' + STRING(bf-tt-Documentos.Saldo) + 
                                    ' Fecha BF: ' + STRING(bf-tt-Documentos.FecReg) skip.
                END.*/
            
            
            // DEBUG ***********
                /*IF tt-Documentos.id-cliente = 1728 THEN DO:
                    PUT UNFORMATTED ' 2) Si l-impPaso es igual al Importe Det ' + STRING(ttDeposito.Importe) + ' o ImpDet es igual a (' + STRING(tt-Documentos.Saldo) + '+' + STRING(bf-tt-Documentos.Saldo) + ') ' +
                            STRING(tt-Documentos.Saldo + bf-tt-Documentos.Saldo) + ' / l-impPaso=' + STRING(l-impPaso) + ' Fecha BF: ' + STRING(bf-tt-Documentos.FecReg) skip.
                END. */ 
            
                /*IF (l-impPaso = ttDeposito.Importe) OR ABS(tt-Documentos.Saldo + bf-tt-Documentos.Saldo - ttDeposito.Importe) <= 0.01 THEN DO:*/
                IF (l-impPaso = ttDeposito.Importe) OR (tt-Documentos.Saldo + bf-tt-Documentos.Saldo = ttDeposito.Importe) 
                    THEN 
                DO:
                    ASSIGN 
                        l-coincide              = TRUE
                        tt-Documentos.Aplica    = TRUE
                        bf-tt-Documentos.Aplica = TRUE
                        ttDeposito.Aut          = ttDeposito.Aut + IF tt-Documentos.GenAcuse THEN '*' ELSE 'FC'
                        ttDeposito.Automatico   = INDEX(ttDeposito.Aut, "*") > 0 . //NOT(ttDeposito.Aut = "" OR ttDeposito.Aut = "FC")
                // DEBUG ***********
                    /*IF tt-Documentos.id-cliente = 961 THEN DO:
                    PUT UNFORMATTED ' Cliente=' + STRING(tt-Documentos.id-cliente) + ' IF (l-impPaso = ttDeposito.Importe) ' + 
                                    STRING(l-impPaso) + '=' + STRING(ttDeposito.Importe) + ' o (tt-Documentos.Saldo + bf-tt-Documentos.Saldo = ttDeposito.Importe) ' +
                                    STRING(tt-Documentos.Saldo) + ' + ' + STRING(bf-tt-Documentos.Saldo) + '=' + STRING(ttDeposito.Importe) 
                                    SKIP.
                    END.*/
                    LEAVE. 
                END.
            
            // 2019-09-18 - Validaciones por si hay diferencias en centavos al sumarizar
                IF (ABS(l-impPaso - ttDeposito.Importe) <= 0.01 OR ABS(tt-Documentos.Saldo + bf-tt-Documentos.Saldo - ttDeposito.Importe) <= 0.01)  // 2019-09-18 - Validacion para dif. de 1 centavo
                    AND tt-Documentos.DescVigente THEN 
                DO:                    
                    ASSIGN 
                        l-coincide              = TRUE
                        tt-Documentos.Aplica    = TRUE
                        bf-tt-Documentos.Aplica = TRUE
                        ttDeposito.Aut          = ttDeposito.Aut + IF tt-Documentos.GenAcuse THEN '*' ELSE 'FC'
                        ttDeposito.Automatico   = INDEX(ttDeposito.Aut, "*") > 0 . //NOT(ttDeposito.Aut = "" OR ttDeposito.Aut = "FC").
                    LEAVE.
                END.
                        
            /*_ 2019-09-10- Subo validacion
            IF (l-impPaso + bf-tt-Documentos.Saldo) > ttDeposito.Importe THEN NEXT.
            ELSE l-impPaso = l-impPaso + bf-tt-Documentos.Saldo. */
            END. /* End del bf-tt-Documentos */
        END.    /* End del tt-Documentos */
    END.  /* End del ttDeposito */


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
        (Creo tt-Documentos) excluyendo dolares
        ******************************************************************* * _*/
    EMPTY TEMP-TABLE tt-Documentos.    
    FOR EACH tt-Cliente NO-LOCK BY Id-Cliente:
    // Busco Pedidos NO facturados (Pedido.Id-Estatus < 5) del cliente
        FOR EACH Pedido WHERE Pedido.Id-Cliente = tt-Cliente.Id-Cliente AND
            Pedido.Id-Estatus < 5 AND 
            Pedido.Id-Cond = 0 AND 
            Pedido.FecReg > (TODAY - 90) NO-LOCK:
                              
            IF Pedido.Id-Moneda > 1 THEN NEXT.
            IF Pedido.Cancelado THEN NEXT.
        
        // RNPC 2020-01-15 - Quito pedidos con monto 0 y pedidos de tiendas
            IF Pedido.Tot = 0 THEN NEXT.
            IF NOT (Pedido.Id-Pedido BEGINS "2" OR Pedido.Id-Pedido BEGINS "3") THEN NEXT.
        
        // Creo registro en tabla temporal
            CREATE tt-Documentos.
            ASSIGN 
                tt-Documentos.Id-Cliente     = Pedido.Id-Cliente
                tt-Documentos.Refsaldo       = Pedido.Id-Pedido
                tt-Documentos.FecReg         = Pedido.FecReg
                tt-Documentos.Importe        = Pedido.Tot   // Incluye descuentos e iva
                tt-Documentos.Saldo          = Pedido.Tot
                tt-Documentos.RazonRemision  = Pedido.RazonSocial
                tt-Documentos.GenAcuse       = TRUE
                tt-Documentos.Aplica         = FALSE
                tt-Documentos.DescNoAplicado = FALSE
                tt-Documentos.DescVigente    = IF (tt-Cliente.Descto1 > 0 OR tt-Cliente.Descto2 > 0) THEN TRUE ELSE FALSE
                tt-Documentos.TipoDoc        = 'PEDIDO'.
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
        
            FIND FIRST ttDeposito WHERE ttDeposito.NumCliente = tt-Cliente.Id-Cliente NO-LOCK NO-ERROR.
        // Creo registro en tabla temporal
            CREATE tt-Documentos.
            ASSIGN 
                tt-Documentos.Id-Cliente     = Remision.Id-Cliente
                tt-Documentos.Refsaldo       = Remision.Id-Remision
                tt-Documentos.FecReg         = Remision.FecReg
                tt-Documentos.Importe        = Remision.Tot
                tt-Documentos.Saldo          = Remision.Tot
                tt-Documentos.RazonRemision  = Remision.RazonSocial
                tt-Documentos.GenAcuse       = TRUE
                tt-Documentos.Aplica         = FALSE
                tt-Documentos.DescNoAplicado = FALSE
                tt-Documentos.DescVigente    = IF (tt-Cliente.Descto1 > 0 OR tt-Cliente.Descto2 > 0) THEN TRUE ELSE FALSE
                tt-Documentos.TipoDoc        = "FACTURA".
                
            /* Ahora en ttDeposito, si el TipoDoc es "FACTURA", entonces asignamos TRUE a ttDeposito.Factura */
            IF tt-Documentos.TipoDoc = "FACTURA" OR tt-Documentos.Aplica = TRUE  THEN  
                ASSIGN ttDeposito.Factura = TRUE.
        END.
    
        /*_ Busco cotizaciones (Cot) vigentes _*/
  //  STATUS DEFAULT 'Buscando cotizaciones del cliente... ' + STRING(tt-Cliente.Id-Cliente).
        FOR EACH Cot WHERE
            Cot.Id-Cliente = tt-Cliente.Id-Cliente AND
            Cot.FecReg > (TODAY - 90) NO-LOCK:
                 
            IF (Cot.FecReg + Cot.Vigencia) < TODAY THEN NEXT.
             
            FIND FIRST Pedido WHERE Pedido.Id-Cot = Cot.Id-Cot NO-LOCK NO-ERROR.
            IF AVAILABLE Pedido THEN NEXT. 
             
            // Creo registro en tabla temporal
            CREATE tt-Documentos.
            ASSIGN 
                tt-Documentos.Id-Cliente     = Cot.Id-Cliente
                tt-Documentos.Refsaldo       = Cot.Id-Cot
                tt-Documentos.FecReg         = Cot.FecReg
                tt-Documentos.Importe        = Cot.Tot
                tt-Documentos.Saldo          = Cot.Tot
                tt-Documentos.RazonRemision  = Cot.RazonSocial 
                tt-Documentos.GenAcuse       = FALSE
                tt-Documentos.Aplica         = FALSE
                tt-Documentos.DescNoAplicado = FALSE
                tt-Documentos.DescVigente    = IF (tt-Cliente.Descto1 > 0 OR tt-Cliente.Descto2 > 0) THEN TRUE ELSE FALSE
                tt-Documentos.TipoDoc        = 'COTIZA'.
        END.
    END.

    /*_ ******************************************************************
        III. Reviso por cada movimiento de deposito si hay movimientos 
        que apliquen directamente o bien que apliquen en combinacion
        ****************************************************************** _*/
    /*_ DEBUG _*/
// OUTPUT TO /usr2/sis6/logSantander_contado.dat.
    FOR EACH ttDeposito NO-LOCK USE-INDEX idx-cliente:
  //  STATUS DEFAULT 'Buscando coincidencias de depositos... '.
        IF ttDeposito.Revisado THEN NEXT.
        l-coincide = FALSE.    
    
        /* ********************************************************* 
             Busco movimiento que coincida con el mismo importe _*/
        FIND FIRST tt-Documentos WHERE
            tt-Documentos.id-cliente = ttDeposito.NumCliente AND
            tt-Documentos.Importe > 0 AND
            tt-Documentos.Importe = ttDeposito.Importe 
            /* RNPC 2020-02-27 Solo montos iguales
            OR (ABS(tt-Documentos.Importe - ttDeposito.Importe) <= 0.01 AND tt-Documentos.DescVigente) */
            EXCLUSIVE-LOCK NO-ERROR. // 2019-09-17 - Busco - 1 centavo
        IF AVAILABLE tt-Documentos THEN 
        DO:
            ASSIGN 
                ttDeposito.Revisado   = TRUE
                ttDeposito.Aut        = ttDeposito.Aut + IF tt-Documentos.GenAcuse THEN '*' ELSE 'CT'
                ttDeposito.Automatico = INDEX(ttDeposito.Aut, "*") > 0 //NOT(ttDeposito.Aut = "" OR ttDeposito.Aut = "FC")
                tt-Documentos.Aplica  = TRUE
                l-coincide            = TRUE.
        END.
    
    /* ********************************************************* */
    /*_ Busco combinaciones de movimientos que puedan coincidir _*/
    /*
    IF l-coincide = TRUE THEN NEXT.
    FOR EACH tt-Documentos WHERE
             tt-Documentos.id-cliente = ttDeposito.Id-Cliente AND
             tt-Documentos.Importe > 0 AND 
             tt-Documentos.GenAcuse 
             EXCLUSIVE-LOCK 
             BY tt-Documentos.FecReg:
            
        IF tt-Documentos.Importe > (ttDeposito.Importe + 0.01) OR tt-Documentos.Aplica OR l-coincide THEN NEXT.  // Sumo 1 centavo
                    
        ASSIGN
            l-impPaso = tt-Documentos.Importe
            l-coincide = FALSE.
        
        FOR EACH bf-tt-Documentos WHERE
                 bf-tt-Documentos.id-cliente = tt-Documentos.id-cliente AND 
                 bf-tt-Documentos.Importe <> tt-Documentos.Importe AND 
                 bf-tt-Documentos.GenAcuse
                 EXCLUSIVE-LOCK 
                 BY bf-tt-Documentos.FecReg:
            
            IF bf-tt-Documentos.FecReg < tt-Documentos.FecReg THEN NEXT.
            
            /* 2019-09-10 */
            IF (l-impPaso + bf-tt-Documentos.Importe) > (ttDeposito.Importe + 0.01) THEN NEXT.    // 2019-09-17 - Sumo 1 centavo
            ELSE l-impPaso = l-impPaso + bf-tt-Documentos.Importe.
            
            /*IF (l-impPaso = ttDeposito.Importe) OR ABS(tt-Documentos.Saldo + bf-tt-Documentos.Saldo - ttDeposito.Importe) <= 0.01 THEN DO:*/
            IF (l-impPaso = ttDeposito.Importe) OR (tt-Documentos.Importe + bf-tt-Documentos.Importe = ttDeposito.Importe) 
            THEN DO:
                ASSIGN 
                    l-coincide = TRUE
                    tt-Documentos.Aplica = TRUE
                    bf-tt-Documentos.Aplica = TRUE
                    ttDeposito.Automatico = ttDeposito.Automatico + IF tt-Documentos.GenAcuse THEN '*' ELSE 'CT'.                    
               LEAVE. 
            END.
            
            // 2019-09-18 - Validaciones por si hay diferencias en centavos al sumarizar
            IF (ABS(l-impPaso - ttDeposito.Importe) <= 0.01 OR ABS(tt-Documentos.Importe + bf-tt-Documentos.Importe - ttDeposito.Importe) <= 0.01)  // 2019-09-18 - Validacion para dif. de 1 centavo
            AND tt-Documentos.DescVigente THEN DO:                    
                ASSIGN 
                    l-coincide = TRUE
                    tt-Documentos.Aplica = TRUE
                    bf-tt-Documentos.Aplica = TRUE
                    ttDeposito.Aut = ttDeposito.Aut + IF tt-Documentos.GenAcuse THEN '*' ELSE 'FC'
                    ttDeposito.Automatico = NOT(ttDeposito.Aut = "" OR ttDeposito.Aut = "FC").

                LEAVE.
            END.
        END.
    END.    
    */
    END.   
/* ***********DEBUG **********
FOR EACH tt-Documentos NO-LOCK BY tt-Documentos.FecReg:
    PUT UNFORMATTED tt-Documentos.RefSaldo + ' | ' +
            STRING(tt-Documentos.FecReg) + ' | ' + 
            STRING(tt-Documentos.Importe) + ' | ' +
            STRING(tt-Documentos.Saldo) + ' | ' +
            STRING(tt-Documentos.GenAcuse) + ' | ' +
            STRING(tt-Documentos.Aplica) 
            STRING(tt-Documentos.TipoDoc) SKIP.
END.*/
// DEBUG - OUTPUT CLOSE.

END PROCEDURE. 


