@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : repchequesdev.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Fri May 09 15:17:21 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/
/*

    Ticket 1052  
    Que pueda consultar todos los clientes, por Clase y campos nuevos
    en la tabla clase, comision,fechas credito
   /* JASS25062025 */

*/
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEF TEMP-TABLE wDiario  
    FIELD RefCargo    LIKE CheDev.Id-CheDev   
    FIELD IdCliente   LIKE Chedev.Id-Cliente
    FIELD RazonSocial LIKE Cliente.RazonSocial
    FIELD Tel         LIKE Cliente.Tel1
    FIELD BancoCO     LIKE Chedev.BancoCO
    FIELD BancoNom    LIKE Banco.Nombre
    FIELD Cheque      LIKE Chedev.Cheque 
    FIELD ValorCheque LIKE CheDev.ImpCheque 
    FIELD FecCargo    LIKE CheDev.FecCargo
    FIELD FechaDep    LIKE MovCliente.FecReg 
    FIELD IdVendedor  LIKE Cliente.Id-Vendedor
    FIELD Vendedor    LIKE empleado.Nombre
    FIELD Responsable LIKE Resp.Nombre 
    FIELD Estatus     AS CHAR  
    FIELD Saldo       LIKE MovCliente.Saldo
    FIELD Clase       AS CHAR                  /* JASS25062025 */
    FIELD Comision    LIKE CheDev.ImpComision 
    FIELD FecCredito  AS DATE
    FIELD TotCredito  LIKE MovCliente.Importe
    INDEX Idx-Def RefCargo. 
 
DEF VAR l-fecha        LIKE MovCliente.FecReg NO-UNDO.
DEF VAR l-ref          AS CHAR      NO-UNDO.
DEF VAR l-next         AS LOGICAL. 
DEF VAR l-saldo        LIKE movcliente.Saldo NO-UNDO.
DEF VAR l-totcredito   LIKE MovCliente.Saldo NO-UNDO.
DEF VAR l-totinteres   LIKE MovCliente.Saldo NO-UNDO.
DEF VAR l-recargo      LIKE MovCliente.saldo NO-UNDO.
DEF VAR l-credcheq     LIKE MovCliente.Saldo NO-UNDO.
DEF VAR l-descr        LIKE Banco.nomcto NO-UNDO.
DEF VAR v-impcheque    AS DECIMAL   NO-UNDO.
DEF VAR v-impcomision  AS DECIMAL   NO-UNDO.
DEF VAR v-impinteres   AS DECIMAL   NO-UNDO.
DEF VAR v-razonsocial  AS CHARACTER NO-UNDO.
DEF VAR v-avisoimporte AS DECIMAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ReporteCheques:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER l-fecha    AS DATE    NO-UNDO.
    DEF INPUT PARAMETER l-fechaini AS DATE    NO-UNDO.
    DEF INPUT PARAMETER l-cliente  AS INTEGER  NO-UNDO.
    DEF INPUT PARAMETER l-estatus  AS INTEGER NO-UNDO.
    DEF INPUT PARAMETER ipClase    AS INTEGER NO-UNDO.  /* JASS25062025 */
    
    DEFINE OUTPUT PARAMETER TABLE FOR wDiario.

    IF l-fechaini = ? THEN l-fechaini = TODAY - 1.
    IF l-fecha    = ? THEN l-fecha    = TODAY.
    IF l-cliente  = ? THEN l-cliente  = 0.
    IF l-estatus  = ? THEN l-estatus  = 0.
    IF ipClase    = ? THEN ipClase    = 0.

    IF l-estatus = 0 THEN RUN ChequeTodo(INPUT l-fecha,INPUT l-fechaini,INPUT l-cliente,INPUT ipClase).
    IF l-estatus = 1 THEN RUN ChequePagado(INPUT l-fecha,INPUT l-fechaini,INPUT l-cliente,INPUT ipClase).
    IF l-estatus = 2 THEN RUN ChequeNoPagado(INPUT ?,INPUT ?,INPUT l-cliente,INPUT ipClase).

END PROCEDURE.


PROCEDURE ChequeTodo:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER l-fecha    AS DATE NO-UNDO.
    DEF INPUT PARAMETER l-fechaini AS DATE NO-UNDO.
    DEF INPUT PARAMETER l-cliente AS INTEGER  NO-UNDO.
    DEF INPUT PARAMETER ipClase AS INTEGER NO-UNDO.
    
    FOR EACH CheDev WHERE   CheDev.FecCargo >= l-fechaini AND 
        CheDev.FecCargo <= l-fecha  
        NO-LOCK:

        ASSIGN 
            l-next = FALSE.
  
        IF NOT l-next THEN 
        DO:

            FIND Cliente OF CheDev    
                NO-LOCK NO-ERROR.
     
            IF NOT AVAILABLE Cliente THEN 
                NEXT.
            
        
            /* Filtrar por Cliente */
            IF l-cliente <> 0 AND Cliente.Id-Cliente <> l-cliente THEN 
                NEXT. /* Si el cliente no coincide, continuar con el siguiente registro */    
            
            /* Filtrar por Clase de Cliente */
            IF ipClase <> 0 AND Cliente.Id-ClaseCte <> ipClase THEN 
                NEXT. 
                
            ASSIGN 
                l-fecha = ?
                l-ref   = ''.
     
            FOR EACH MovCliente WHERE MovCliente.Refsaldo = Chedev.Id-Chedev
                NO-LOCK 
                BY MovCliente.FecReg 
                BY MovCliente.Documento:
         
                ACCUMULATE 98 (COUNT).
                IF MovCliente.Importe > 0 THEN 
                    NEXT.
            
                ACCUMULATE MovCliente.Importe (TOTAL).
            
                ASSIGN 
                    l-fecha = MovCliente.FecReg
                    l-ref   = MovCliente.Documento.
            END.

            FOR EACH HistMovCte WHERE HistMovCte.Refsaldo = Chedev.Id-Chedev
                NO-LOCK 
                BY HistMovCte.FecReg 
                BY HistMovCte.Documento:
            
                ACCUMULATE 99 (COUNT).
            
                IF HistMovCte.Importe > 0 THEN 
                    NEXT.

                ACCUMULATE HistMovCte.Importe (TOTAL).
         
                ASSIGN 
                    l-fecha = HistMovCte.FecReg
                    l-ref   = HistMovCte.Documento.
            END.
      
            ASSIGN 
                l-totcredito = 0.

            IF (ACCUM COUNT 98) = 0 AND (ACCUM COUNT 99) = 0 THEN 
            DO:
            /*
            {resa0004.i 
                &Run = "RUN resc0801.p (INPUT Chedev.Id-CheDev, 
                                        INPUT l-indice_opcion,
                                        INPUT l-fecini,
                                        INPUT l-feccorte, 
                                        OUTPUT l-restahist,
                                        OUTPUT l-fecha, OUTPUT l-ref)." 
                &Salte = "IF l-fecha <> ? THEN DO:
                            ASSIGN 
                                l-totcredito = l-totcredito - l-restahist.
                            LEAVE.
                          END." 
                &AnoMax = "YEAR(CheDev.FecCargo) - 1" } */
            END.
            ELSE 
                ASSIGN l-totcredito = l-totcredito + 
               ((ACCUM TOTAL MovCliente.Importe) * -1) + ((ACCUM TOTAL HistMovCte.Importe) * -1).
     
        
            FOR EACH MovAviso WHERE MovAviso.Id-Chedev = chedev.Id-Chedev 
                NO-LOCK:

                ASSIGN 
                    l-totcredito = l-totcredito + MovAviso.ImpCanc +
                               MovAviso.ImpPago + MovAviso.ImpDesc .
            END.
            /*
            FIND FIRST AvisoCargo WHERE AvisoCargo.Id-Chedev = Chedev.Id-Chedev
                NO-LOCK NO-ERROR.

            ASSIGN 
                l-recargo = AvisoCargo.Importe + AvisoCargo.iva
                l-saldo   = Chedev.ImpCheque  + Chedev.ImpComision +
                            Chedev.ImpInteres + l-recargo - l-totcredito.  */
                            
            FIND FIRST AvisoCargo WHERE AvisoCargo.Id-Chedev = Chedev.Id-Chedev
                NO-LOCK NO-ERROR.

            IF AVAILABLE AvisoCargo THEN
                ASSIGN 
                    l-recargo = AvisoCargo.Importe + AvisoCargo.iva.
            ELSE
                ASSIGN   
                    l-recargo = 0.

            ASSIGN 
                l-saldo = Chedev.ImpCheque  + Chedev.ImpComision +
              Chedev.ImpInteres + l-recargo - l-totcredito.
        END.
    
        ASSIGN
            v-RazonSocial = (IF CheDev.FecCanc <> ? THEN 
                                     "C A N C E L A D O" 
                                 ELSE 
                                     (IF Cliente.Id-Cliente = 3 THEN 
                                         Chedev.RazonSocial
                                     ELSE 
                                         Cliente.RazonSocial ))
            v-ImpCheque   = (IF CheDev.FecCanc <> ? THEN 0 ELSE CheDev.ImpCheque)
            v-ImpComision = (IF CheDev.FecCanc <> ? THEN 0 ELSE CheDev.ImpComision)
            v-ImpInteres  = (IF CheDev.FecCanc <> ? THEN 0 ELSE Chedev.ImpInteres).
        
        FIND FIRST Banco    WHERE Banco.Id-Banco       = CheDev.BancoCO      NO-LOCK NO-ERROR.
        FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
        FIND FIRST Resp     WHERE Resp.Id-Resp         = Cliente.Id-Resp     NO-LOCK NO-ERROR.
        FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = Cliente.Id-Vendedor NO-LOCK NO-ERROR.
        IF AVAILABLE Vendedor THEN 
        DO:
            FIND FIRST Empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
        END.    
         
        CREATE wDiario.   
        ASSIGN 
            wDiario.BancoCo     = Chedev.BancoCo
            wDiario.BancoNom    = IF AVAILABLE Banco THEN Banco.Nombre ELSE ""
            wDiario.Cheque      = Chedev.Cheque
            wDiario.IdCliente   = Chedev.Id-Cliente
            wDiario.RazonSocial = v-razonsocial
            wDiario.Tel         = IF Cliente.Id-Cliente = 3 THEN "" ELSE Cliente.Tel1
            wDiario.FecCargo    = CheDev.FecCargo
            wDiario.RefCargo    = CheDev.Id-CheDev
            wDiario.ValorCheque = v-impcheque
            wDiario.FechaDep    = l-fecha
            wDiario.IdVendedor  = IF AVAILABLE Cliente THEN Cliente.Id-Vendedor ELSE ""
            wDiario.Vendedor    = IF AVAILABLE Empleado THEN empleado.Nombre ELSE ""
            wDiario.Responsable = IF AVAILABLE Cliente THEN STRING(Cliente.Id-Resp) ELSE "0"
            wDiario.Estatus     = IF l-saldo = 0 THEN "PAGADO" ELSE "DEVUELTO" /* Establece el Estatus */
            wDiario.Saldo       = l-saldo
            wDiario.Clase       = IF AVAILABLE ClaseCte THEN ClaseCte.Descr ELSE ""
            wDiario.Comision    = v-ImpComision
            wDiario.FecCredito  = l-fecha
            wDiario.TotCredito  = l-totcredito.   
  
    END.   

END PROCEDURE.

PROCEDURE ChequePagado:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER l-fecha    AS DATE  NO-UNDO.
    DEF INPUT PARAMETER l-fechaini AS DATE   NO-UNDO.
    DEF INPUT PARAMETER l-cliente AS INTEGER  NO-UNDO.
    DEF INPUT PARAMETER ipClase AS INTEGER NO-UNDO.
    
    FOR EACH CheDev WHERE (l-fechaini = ? OR CheDev.FecCargo >= l-fechaini) AND
        (l-fecha = ? OR CheDev.FecCargo <= l-fecha) 
        NO-LOCK:

        ASSIGN 
            l-next = FALSE.
  
        IF NOT l-next THEN 
        DO:

            FIND Cliente OF CheDev 
                NO-LOCK NO-ERROR.
     
            IF NOT AVAILABLE Cliente THEN 
                NEXT.
            
        
            /* Filtrar por Cliente */
            IF l-cliente <> 0 AND Cliente.Id-Cliente <> l-cliente THEN 
                NEXT. /* Si el cliente no coincide, continuar con el siguiente registro */  
                
            /* Filtrar por Clase de Cliente */
            IF ipClase <> 0 AND Cliente.Id-ClaseCte <> ipClase THEN 
                NEXT.      
     
            ASSIGN 
                l-fecha = ?
                l-ref   = ''.
     
            FOR EACH MovCliente WHERE MovCliente.Refsaldo = Chedev.Id-Chedev
                NO-LOCK 
                BY MovCliente.FecReg 
                BY MovCliente.Documento:
         
                ACCUMULATE 98 (COUNT).
                IF MovCliente.Importe > 0 THEN 
                    NEXT.
            
                ACCUMULATE MovCliente.Importe (TOTAL).
            
                ASSIGN 
                    l-fecha = MovCliente.FecReg
                    l-ref   = MovCliente.Documento.
            END.

            FOR EACH HistMovCte WHERE HistMovCte.Refsaldo = Chedev.Id-Chedev
                NO-LOCK 
                BY HistMovCte.FecReg 
                BY HistMovCte.Documento:
            
                ACCUMULATE 99 (COUNT).
            
                IF HistMovCte.Importe > 0 THEN 
                    NEXT.

                ACCUMULATE HistMovCte.Importe (TOTAL).
         
                ASSIGN 
                    l-fecha = HistMovCte.FecReg
                    l-ref   = HistMovCte.Documento.
            END.
      
            ASSIGN 
                l-totcredito = 0.

            IF (ACCUM COUNT 98) = 0 AND (ACCUM COUNT 99) = 0 THEN 
            DO:
            /*
            {resa0004.i 
                &Run = "RUN resc0801.p (INPUT Chedev.Id-CheDev, 
                                        INPUT l-indice_opcion,
                                        INPUT l-fecini,
                                        INPUT l-feccorte, 
                                        OUTPUT l-restahist,
                                        OUTPUT l-fecha, OUTPUT l-ref)." 
                &Salte = "IF l-fecha <> ? THEN DO:
                            ASSIGN 
                                l-totcredito = l-totcredito - l-restahist.
                            LEAVE.
                          END." 
                &AnoMax = "YEAR(CheDev.FecCargo) - 1" } */
            END.
            ELSE 
                ASSIGN l-totcredito = l-totcredito + 
               ((ACCUM TOTAL MovCliente.Importe) * -1) + ((ACCUM TOTAL HistMovCte.Importe) * -1).
     
        
            FOR EACH MovAviso WHERE MovAviso.Id-Chedev = chedev.Id-Chedev 
                NO-LOCK:

                ASSIGN 
                    l-totcredito = l-totcredito + MovAviso.ImpCanc +
                               MovAviso.ImpPago + MovAviso.ImpDesc .
            END.
            /*
            FIND FIRST AvisoCargo WHERE AvisoCargo.Id-Chedev = Chedev.Id-Chedev
                NO-LOCK NO-ERROR.

            ASSIGN 
                l-recargo = AvisoCargo.Importe + AvisoCargo.iva
                l-saldo   = Chedev.ImpCheque  + Chedev.ImpComision +
                            Chedev.ImpInteres + l-recargo - l-totcredito. */
                            
                            
            FIND FIRST AvisoCargo WHERE AvisoCargo.Id-Chedev = Chedev.Id-Chedev
                NO-LOCK NO-ERROR.

            IF AVAILABLE AvisoCargo THEN
                ASSIGN 
                    l-recargo = AvisoCargo.Importe + AvisoCargo.iva.
            ELSE
                ASSIGN
                    l-recargo = 0.

            ASSIGN 
                l-saldo = Chedev.ImpCheque  + Chedev.ImpComision +
              Chedev.ImpInteres + l-recargo - l-totcredito.
        END.
        
        IF l-saldo <> 0 THEN NEXT.
        ASSIGN
            v-RazonSocial = (IF CheDev.FecCanc <> ? THEN 
                                     "C A N C E L A D O" 
                                 ELSE 
                                     (IF Cliente.Id-Cliente = 3 THEN 
                                         Chedev.RazonSocial
                                     ELSE 
                                         Cliente.RazonSocial ))
            v-ImpCheque   = (IF CheDev.FecCanc <> ? THEN 0 ELSE CheDev.ImpCheque)
            v-ImpComision = (IF CheDev.FecCanc <> ? THEN 0 ELSE CheDev.ImpComision)
            v-ImpInteres  = (IF CheDev.FecCanc <> ? THEN 0 ELSE Chedev.ImpInteres).
        
        FIND FIRST Banco    WHERE Banco.Id-Banco       = CheDev.BancoCO      NO-LOCK NO-ERROR.
        FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
        FIND FIRST Resp     WHERE Resp.Id-Resp         = Cliente.Id-Resp     NO-LOCK NO-ERROR.
        FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = Cliente.Id-Vendedor NO-LOCK NO-ERROR.
        IF AVAILABLE Vendedor THEN 
        DO:
            FIND FIRST Empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
        END.    
         
        CREATE wDiario.   
        ASSIGN 
            wDiario.BancoCo     = Chedev.BancoCo
            wDiario.BancoNom    = IF AVAILABLE Banco THEN Banco.Nombre ELSE ""
            wDiario.Cheque      = Chedev.Cheque
            wDiario.IdCliente   = Chedev.Id-Cliente
            wDiario.RazonSocial = v-razonsocial
            wDiario.Tel         = IF Cliente.Id-Cliente = 3 THEN "" ELSE Cliente.Tel1
            wDiario.FecCargo    = CheDev.FecCargo
            wDiario.RefCargo    = CheDev.Id-CheDev
            wDiario.ValorCheque = v-impcheque
            wDiario.FechaDep    = l-fecha
            wDiario.IdVendedor  = IF AVAILABLE Cliente THEN Cliente.Id-Vendedor ELSE ""
            wDiario.Vendedor    = IF AVAILABLE Empleado THEN empleado.Nombre ELSE ""
            wDiario.Responsable = IF AVAILABLE Cliente THEN STRING(Cliente.Id-Resp) ELSE "0"
            wDiario.Estatus     = IF l-saldo = 0 THEN "PAGADO" ELSE "DEVUELTO" /* Establece el Estatus */
            wDiario.Saldo       = l-saldo
            wDiario.Clase       = IF AVAILABLE ClaseCte THEN ClaseCte.Descr ELSE ""
            wDiario.Comision    = v-ImpComision
            wDiario.FecCredito  = l-fecha
            wDiario.TotCredito  = l-totcredito.     
  
    END. 

END PROCEDURE.


PROCEDURE ChequeNoPagado:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER l-fecha    AS DATE   NO-UNDO.
    DEF INPUT PARAMETER l-fechaini AS DATE   NO-UNDO.
    DEF INPUT PARAMETER l-cliente AS INTEGER  NO-UNDO.
    DEF INPUT PARAMETER ipClase    AS INTEGER NO-UNDO.
       
    FOR EACH CheDev WHERE (l-fechaini = ? OR CheDev.FecCargo >= l-fechaini) AND
        (l-fecha = ? OR CheDev.FecCargo <= l-fecha) 
        NO-LOCK:

        ASSIGN 
            l-next = FALSE.
  
        IF NOT l-next THEN 
        DO:

            FIND Cliente OF CheDev 
                NO-LOCK NO-ERROR.
     
            IF NOT AVAILABLE Cliente THEN 
                NEXT.
            
        
            /* Filtrar por Cliente */
            IF l-cliente <> 0 AND Cliente.Id-Cliente <> l-cliente THEN 
                NEXT. /* Si el cliente no coincide, continuar con el siguiente registro */   
                
            /* Filtrar por Clase de Cliente */
            IF ipClase <> 0 AND Cliente.Id-ClaseCte <> ipClase THEN 
                NEXT.  
     
            ASSIGN 
                l-fecha = ?
                l-ref   = ''.
     
            FOR EACH MovCliente WHERE MovCliente.Refsaldo = Chedev.Id-Chedev
                NO-LOCK 
                BY MovCliente.FecReg 
                BY MovCliente.Documento:
         
                ACCUMULATE 98 (COUNT).
                IF MovCliente.Importe > 0 THEN 
                    NEXT.
            
                ACCUMULATE MovCliente.Importe (TOTAL).
            
                ASSIGN 
                    l-fecha = MovCliente.FecReg
                    l-ref   = MovCliente.Documento.
            END.

            FOR EACH HistMovCte WHERE HistMovCte.Refsaldo = Chedev.Id-Chedev
                NO-LOCK 
                BY HistMovCte.FecReg 
                BY HistMovCte.Documento:
            
                ACCUMULATE 99 (COUNT).
            
                IF HistMovCte.Importe > 0 THEN 
                    NEXT.

                ACCUMULATE HistMovCte.Importe (TOTAL).
         
                ASSIGN 
                    l-fecha = HistMovCte.FecReg
                    l-ref   = HistMovCte.Documento.
            END.
      
            ASSIGN 
                l-totcredito = 0.

            IF (ACCUM COUNT 98) = 0 AND (ACCUM COUNT 99) = 0 THEN 
            DO:
            /*
            {resa0004.i 
                &Run = "RUN resc0801.p (INPUT Chedev.Id-CheDev, 
                                        INPUT l-indice_opcion,
                                        INPUT l-fecini,
                                        INPUT l-feccorte, 
                                        OUTPUT l-restahist,
                                        OUTPUT l-fecha, OUTPUT l-ref)." 
                &Salte = "IF l-fecha <> ? THEN DO:
                            ASSIGN 
                                l-totcredito = l-totcredito - l-restahist.
                            LEAVE.
                          END." 
                &AnoMax = "YEAR(CheDev.FecCargo) - 1" } */
            END.
            ELSE 
                ASSIGN l-totcredito = l-totcredito + 
               ((ACCUM TOTAL MovCliente.Importe) * -1) + ((ACCUM TOTAL HistMovCte.Importe) * -1).
     
        
            FOR EACH MovAviso WHERE MovAviso.Id-Chedev = chedev.Id-Chedev 
                NO-LOCK:

                ASSIGN 
                    l-totcredito = l-totcredito + MovAviso.ImpCanc +
                               MovAviso.ImpPago + MovAviso.ImpDesc .
            END.

            FIND FIRST AvisoCargo WHERE AvisoCargo.Id-Chedev = Chedev.Id-Chedev
                NO-LOCK NO-ERROR.
            IF AVAILABLE AvisoCargo THEN
                ASSIGN 
                    l-recargo = AvisoCargo.Importe + AvisoCargo.iva.    
            ELSE
                ASSIGN
                    l-recargo = 0.
              
            ASSIGN 
                l-saldo = Chedev.ImpCheque  + Chedev.ImpComision +
                            Chedev.ImpInteres + l-recargo - l-totcredito.
        END.
        
        IF l-saldo = 0 THEN NEXT.  // SOLO CHEQUES CON SALDO PENDIENTE
        ASSIGN
            v-RazonSocial = (IF CheDev.FecCanc <> ? THEN 
                                     "C A N C E L A D O" 
                                 ELSE 
                                     (IF Cliente.Id-Cliente = 3 THEN 
                                         Chedev.RazonSocial
                                     ELSE 
                                         Cliente.RazonSocial ))
            v-ImpCheque   = (IF CheDev.FecCanc <> ? THEN 0 ELSE CheDev.ImpCheque)
            v-ImpComision = (IF CheDev.FecCanc <> ? THEN 0 ELSE CheDev.ImpComision)
            v-ImpInteres  = (IF CheDev.FecCanc <> ? THEN 0 ELSE Chedev.ImpInteres).
        
        FIND FIRST Banco    WHERE Banco.Id-Banco       = CheDev.BancoCO      NO-LOCK NO-ERROR.
        FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR. 
        FIND FIRST Resp     WHERE Resp.Id-Resp         = Cliente.Id-Resp     NO-LOCK NO-ERROR.
        FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = Cliente.Id-Vendedor NO-LOCK NO-ERROR.
        IF AVAILABLE Vendedor THEN 
        DO:
            FIND FIRST Empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
        END.    
         
        CREATE wDiario.   
        ASSIGN 
            wDiario.BancoCo     = Chedev.BancoCo
            wDiario.BancoNom    = IF AVAILABLE Banco THEN Banco.Nombre ELSE ""
            wDiario.Cheque      = Chedev.Cheque
            wDiario.IdCliente   = Chedev.Id-Cliente
            wDiario.RazonSocial = v-razonsocial
            wDiario.Tel         = IF Cliente.Id-Cliente = 3 THEN "" ELSE Cliente.Tel1
            wDiario.FecCargo    = CheDev.FecCargo
            wDiario.RefCargo    = CheDev.Id-CheDev
            wDiario.ValorCheque = v-impcheque
            wDiario.FechaDep    = l-fecha
            wDiario.IdVendedor  = IF AVAILABLE Cliente THEN Cliente.Id-Vendedor ELSE ""
            wDiario.Vendedor    = IF AVAILABLE Empleado THEN empleado.Nombre ELSE ""
            wDiario.Responsable = IF AVAILABLE Cliente THEN STRING(Cliente.Id-Resp) ELSE "0"
            wDiario.Estatus     = IF l-saldo = 0 THEN "PAGADO" ELSE "DEVUELTO" /* Establece el Estatus */
            wDiario.Saldo       = l-saldo
            wDiario.Clase       = IF AVAILABLE ClaseCte THEN ClaseCte.Descr ELSE ""
            wDiario.Comision    = v-ImpComision
            wDiario.FecCredito  = l-fecha
            wDiario.TotCredito  = l-totcredito.   
     
    END. 

END PROCEDURE.



