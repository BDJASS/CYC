@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : GetFacturaInfo.p
    Purpose     : Servicio GET para consultar la información de una factura
                  y los artículos vendidos, validando el acceso a la caja
                  y el nivel del usuario.
    Author(s)   : sis10
    Created     : Fecha actual
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

DEFINE TEMP-TABLE ttFactura NO-UNDO               
    FIELD IdFactura        AS CHARACTER FORMAT "x(15)" /* Número de factura */
    FIELD IdCliente        AS INTEGER                  /* Número de cliente */
    FIELD FecReg           AS DATE                     /* Fecha de registro */
    FIELD RazonSocial      AS CHARACTER FORMAT "x(40)" /* Razón social del cliente */
    FIELD RegimenFiscal    LIKE RFiscal.Id-RFiscal
    FIELD RegimenFiscalDes LIKE RFiscal.Descr
    FIELD UsoCFDI          AS CHARACTER FORMAT "x(3)"  /* Uso CFDI */
    FIELD UsoCFDIDes       AS CHARACTER
    FIELD Requisicion      LIKE Factura.requisicion
    FIELD Plazo            LIKE Factura.Plazo
    FIELD CalleNo          AS CHARACTER FORMAT "x(50)" /* Dirección */
    FIELD Colonia          AS CHARACTER FORMAT "x(30)" /* Colonia */
    FIELD Ciudad           AS CHARACTER FORMAT "x(30)" /* Ciudad */
    FIELD Estado           AS CHARACTER FORMAT "x(30)" /* Estado */
    FIELD CP               AS CHARACTER FORMAT "x(10)" /* Código postal */
    FIELD Subtotal         AS DECIMAL   FORMAT ">>>,>>9.99" /* Subtotal */
    FIELD Descuento        AS DECIMAL   FORMAT ">>>,>>9.99" /* Descuento */
    FIELD ImpFlete         AS DECIMAL   FORMAT ">>>,>>9.99" /* Importe Flete */
    FIELD ImpSeguro        AS DECIMAL   FORMAT ">>>,>>9.99" /* Importe Seguro */
    FIELD IVA              AS DECIMAL   FORMAT ">>>,>>9.99" /* IVA */
    FIELD Total            AS DECIMAL   FORMAT ">>>,>>9.99". /* Total */     

DEFINE TEMP-TABLE ttDetFactura NO-UNDO
    FIELD IdFactura      AS CHARACTER FORMAT "x(15)" /* Número de factura */
    FIELD IdArticulo     AS CHARACTER FORMAT "x(10)" /* Código de artículo */
    FIELD Descripcion    LIKE DetFactura.Descr /* Descripción del artículo */
    FIELD Presentacion   LIKE DetFactura.Descr /* Presentación */
    FIELD Cantidad       AS DECIMAL   FORMAT ">>>,>>9" /* Cantidad vendida */
    FIELD PrecioUnitario AS DECIMAL   FORMAT ">>>,>>9.99" /* Precio unitario */
    FIELD Descuento      LIKE detfactura.descto
    FIELD Importe        AS DECIMAL   FORMAT ">>>,>>9.99". /* Importe total */

DEFINE DATASET dsFactura FOR ttFactura, ttDetFactura
    DATA-RELATION RelFacturaDetalle FOR ttFactura, ttDetFactura 
    RELATION-FIELDS (IdFactura, IdFactura).

DEFINE TEMP-TABLE tt-Factura LIKE Factura.
DEFINE TEMP-TABLE tt-DetFactura LIKE DetFactura.
DEFINE TEMP-TABLE tt-DetSerie LIKE DetSerie.

DEFINE VARIABLE l-IdFactura    AS CHARACTER NO-UNDO FORMAT "x(15)". /* Número de factura */
DEFINE VARIABLE l-Nivel        AS INTEGER   NO-UNDO. /* Nivel del usuario */
DEFINE VARIABLE l-saldo        LIKE MovCliente.Saldo.
DEFINE VARIABLE l-NMov         AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-recid        AS RECID     NO-UNDO.
DEFINE VARIABLE l-tipo         AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-UsoCfdi      LIKE Factura.Id-UsoCFDI NO-UNDO.
DEFINE VARIABLE l-UsoCfdiDes   AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Req          LIKE Factura.Requisicion NO-UNDO.
DEFINE VARIABLE l-RFiscal      LIKE Rfiscal.Id-RFiscal NO-UNDO.
DEFINE VARIABLE l-Cliente      LIKE Factura.Id-Cliente NO-UNDO.
DEFINE VARIABLE l-DescrRFiscal AS CHARACTER NO-UNDO FORMAT "X(58)".
DEFINE VARIABLE l-Aceptar      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-Pres         LIKE ArtPres.Descr NO-UNDO FORMAT 'X(8)'.

DEFINE VARIABLE l-titulo       AS CHARACTER.
DEFINE VARIABLE l-Timbrada     AS LOGICAL   NO-UNDO.
DEFINE BUFFER g-Folio     FOR Folio.
DEFINE BUFFER b-movcaja   FOR MovCaja.
DEFINE BUFFER b-Pedido    FOR Pedido.
DEFINE BUFFER b-EstPedido FOR EstPedido.
DEFINE BUFFER b-Movim     FOR Movim.


DEF    VAR      l-fecvence   AS DATE.
DEF    VAR      l-ubic       AS CHAR .
DEF    VAR      l-recmov     AS RECID.

DEF    VAR      l-rec        AS RECID     NO-UNDO.
DEF    VAR      l-usuario    LIKE Password.Usuario.
DEF    VAR      l-acuse      LIKE Acuse.id-Acuse.
DEF    VAR      cp-question  AS CHAR.
DEF    VAR      cp-answer    AS LOGICAL.

DEF    VAR      l-NFactura   LIKE Factura.Id-Factura NO-UNDO.
DEF    VAR      l-NUUID      LIKE Factura.UUID NO-UNDO.
DEF    VAR      l-Anter      AS LOGICAL   NO-UNDO.
DEF    VAR      l-AnoAnt     AS INTEGER   NO-UNDO.
DEF    VAR      l-MesAnt     AS INTEGER   NO-UNDO.

DEFINE VARIABLE l-Asunto     AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Contenido  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-MailDe     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ResponderA AS CHARACTER NO-UNDO.  
DEFINE VARIABLE l-Mail       AS CHARACTER NO-UNDO.  
DEFINE VARIABLE v-Enviado    AS LOGICAL   NO-UNDO.

/* **********************  Internal Procedures  *********************** */


/* ***************************  Main Procedure *************************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRefactura:
    DEFINE INPUT  PARAMETER pIdFactura AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUser AS CHARACTER NO-UNDO. /* Usuario a validar */
    DEFINE INPUT  PARAMETER pConfirmar AS LOGICAL NO-UNDO INITIAL FALSE. /* Confirmación del usuario */
    DEFINE OUTPUT PARAMETER Respuesta AS CHAR. 
    DEFINE OUTPUT PARAMETER DATASET FOR dsFactura.
    
    
    IF pConfirmar = ? THEN pConfirmar = FALSE.
    /* Validación del Usuario */
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado no existe.".
        RETURN.
    END.

    /* Obtener el nivel del usuario */
    ASSIGN 
        l-Nivel = Usuario.Nivel.

    /* Validar que el nivel del usuario sea 2 o 3 */
    IF l-Nivel <> 2 AND l-Nivel <> 3 THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario no tiene permiso para realizar esta operación.".
        RETURN.
    END.

    /* Validar acceso a la caja */
    FIND FIRST Caja WHERE Caja.Id-Caja = INTEGER(Usuario.Id-Caja) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Caja THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario no tiene acceso a ninguna caja.".
        RETURN. 
    END.

    FIND LAST CtlCaja WHERE CtlCaja.Id-Caja = Caja.Id-Caja AND 
        CtlCaja.FecCierre = ? NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CtlCaja THEN 
    DO:
        ASSIGN 
            Respuesta = "La caja " + CAPS(Caja.Descr) + " está cerrada.".
        RETURN.
    END.

    FIND FIRST CorteCaja WHERE CorteCaja.Id-Caja = CtlCaja.Id-Caja AND
        CorteCaja.Turno = CtlCaja.Turno AND
        CorteCaja.FecOper = CtlCaja.FecOper AND
        CorteCaja.Declaracion > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE CorteCaja THEN 
    DO:
        ASSIGN 
            Respuesta = "El cajero ya hizo su declaración.".
        RETURN.
    END.

    /* Buscar la factura */
    FIND Factura WHERE Factura.Id-Factura = pIdFactura NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Factura THEN 
    DO:
        ASSIGN 
            Respuesta = "El Folio de la factura no esta registrada.".
        RETURN.
    END.
    
    IF Factura.FecCanc <> ? THEN 
    DO:
        ASSIGN 
            Respuesta = "La factura fue cancelada.".
        RETURN.   
    END.    
    /* Validar si la factura tiene UUID */
    IF Factura.UUID <> "" THEN 
    DO:
        IF NOT pConfirmar THEN 
        DO:
            ASSIGN 
                Respuesta = "(1)La factura " + pIdFactura + " ya cuenta con folio electrónico,al realizar una modificacion se generara un nuevo folio electronico.¿Desea Continuar?".
            RETURN.
        END.
        
    END. 
    
    l-Saldo = 0.  
    l-NMov = 0.
    FOR EACH MovCliente WHERE MovCliente.RefSaldo = Factura.Id-Factura NO-LOCK:
        l-Saldo = l-Saldo + MovCliente.Importe.
        l-NMov = l-NMov + 1.
    END.
    IF l-NMov <> 1 OR l-Saldo <> Factura.Tot THEN 
    DO:
        ASSIGN 
            Respuesta = "La factura tiene pagos o descuentos, imposible refacturar.".
        RETURN.
    END.
        
    FIND FIRST Devolucion WHERE Devolucion.Id-Factura = Factura.id-Factura
        AND Devolucion.TipoVenta = 3 NO-LOCK NO-ERROR.
    IF AVAILABLE Devolucion AND Devolucion.FecCanc = ? THEN 
    DO:
        ASSIGN 
            Respuesta = "La Factura tiene articulos registrados por devolucion por lo tanto no se permite refacturar.".
        RETURN.
    END.     

    l-DescrRFiscal = "".
    l-UsoCfdi      = "". 
    /* pendiente validar que la factura no tenga ningun abono */
    FIND Cliente WHERE Cliente.id-cliente = Factura.id-cliente NO-LOCK NO-ERROR.
    FIND Ciudad WHERE Ciudad.id-ciudad = Factura.id-ciudad NO-LOCK NO-ERROR.
    IF AVAILABLE Ciudad THEN 
    DO:
        FIND FIRST Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.    
    END. 
    
    FIND FIRST RFiscal WHERE RFiscal.Id-RFiscal = Factura.Id-RFiscal NO-LOCK NO-ERROR.
    IF AVAILABLE RFiscal THEN 
        ASSIGN 
            l-DescrRFiscal = RFiscal.Descr.
    ELSE 
        ASSIGN 
            l-DescrRFiscal = "".
                       
    FIND FIRST UsoCFDI WHERE UsoCFDI.Id-UsoCFDI = Factura.Id-UsoCFDI NO-LOCK NO-ERROR.
    IF AVAILABLE UsoCFDI THEN
        ASSIGN 
            l-UsoCfdi = UsoCFDI.Descr. 
    ELSE          
        ASSIGN
            l-UsoCfdi = "" .  
                 
    ASSIGN 
        l-recid = RECID(Factura)
        l-tipo  = 3. 
            
    l-Cliente = Factura.Id-Cliente .
    l-Req     = Factura.Requisicion.
        
    /* Agregar información de la factura a la tabla temporal */
    CREATE ttFactura.
    ASSIGN 
        ttFactura.IdFactura        = Factura.Id-Factura
        ttFactura.FecReg           = Factura.FecReg
        ttFactura.IdCliente        = l-Cliente
        ttFactura.RazonSocial      = Factura.RazonSocial
        ttFactura.RegimenFiscal    = Factura.Id-RFiscal //l-RFiscal 
        ttFactura.RegimenFiscalDes = l-DescrRFiscal
        ttFactura.CalleNo          = Factura.CalleNo
        ttFactura.Colonia          = Factura.Colonia
        ttFactura.CP               = Factura.CP        //Cliente.CP
        ttFactura.Ciudad           = IF AVAILABLE Ciudad THEN Ciudad.Nombre ELSE " "
        ttFactura.Estado           = IF AVAILABLE Estado THEN Estado.Nomcto ELSE " "
        ttFactura.UsoCFDI          = Factura.Id-UsoCFDI
        ttFactura.UsoCFDIDes       = l-UsoCfdi
        ttFactura.Requisicion      = l-Req     
        ttFactura.Plazo            = Factura.Plazo
        ttFactura.Subtotal         = Factura.Subtotal
        ttFactura.Descuento        = Factura.Descuento   
        ttFactura.ImpFlete         = Factura.ImpFlete
        ttFactura.ImpSeguro        = Factura.ImpSeguro
        ttFactura.IVA              = Factura.Iva      
        ttFactura.Total            = Factura.Tot.       

    /* Obtener el detalle de los artículos vendidos */  
    FOR EACH DetFactura OF Factura NO-LOCK:
        
        FIND ArtPres WHERE ArtPres.Id-Articulo = DetFactura.Id-Articulo
            AND ArtPres.Id-Pres = DetFactura.Id-Pres NO-LOCK NO-ERROR.
        ASSIGN 
            l-Pres = IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE ".".     
        
        
        CREATE ttDetFactura.
        ASSIGN 
            ttDetFactura.IdFactura      = Factura.Id-Factura
            ttDetFactura.IdArticulo     = DetFactura.Id-Articulo
            ttDetFactura.Descripcion    = DetFactura.Descr
            ttDetFactura.Presentacion   = l-Pres
            ttDetFactura.Cantidad       = detfactura.Cant
            ttDetFactura.PrecioUnitario = DetFactura.PrecUnit
            ttDetFactura.Importe        = DetFactura.Importe.   
    END.
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostRefacturacion:   
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pIdFactura    AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER l-UsoCfdi      LIKE Factura.Id-UsoCFDI NO-UNDO.
    DEFINE INPUT PARAMETER l-Req          LIKE Factura.Requisicion NO-UNDO.
    DEFINE INPUT PARAMETER l-RFiscal      LIKE Rfiscal.Id-RFiscal NO-UNDO.
    DEFINE INPUT PARAMETER l-Cliente      LIKE Factura.Id-Cliente NO-UNDO.
    DEFINE INPUT PARAMETER l-Plazo        LIKE Factura.Plazo NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUser   AS CHARACTER NO-UNDO. /* Usuario a validar */
    DEFINE OUTPUT PARAMETER Respuesta AS CHAR. 



    FIND Cliente WHERE Cliente.Id-Cliente = l-Cliente    
        AND Cliente.Activo = TRUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cliente THEN 
    DO:
        ASSIGN 
            Respuesta = "Numero de cliente inexistente, favor de verificar...".
        RETURN.
    END.
            
    IF Cliente.Plazo = 0 OR Cliente.Limite = 0 THEN 
    DO:
        ASSIGN 
            Respuesta = "El cliente " + STRING(Cliente.Id-Cliente) + " no tiene crédito, no es posible realizar el cambio.".
        RETURN.
    END.
            
    FIND FIRST BlkAut WHERE BlkAut.Id-Cliente = Cliente.Id-Cliente NO-LOCK NO-ERROR.
    IF AVAILABLE BlkAut THEN 
    DO:
        ASSIGN 
            Respuesta = "El cliente " + STRING(Cliente.Id-Cliente) + 
                    " se encuentra bloqueado por autorizaciones, no es posible realizar el cambio.".
        RETURN.
    END.
            
    IF Cliente.Id-Calidad = 37 THEN 
    DO:
        FOR EACH ChequePF WHERE ChequePF.Id-Cliente = Cliente.Id-Cliente
            AND ChequePF.Dep = FALSE NO-LOCK:
            ACCUMULATE 1 (COUNT).
            ACCUMULATE ChequePF.Importe (TOTAL).
        END.
        IF ((ACCUM COUNT 1) + 1 > Cliente.CantCheque) OR
            ((ACCUMU TOTAL ChequePF.Importe) + (Factura.Subtotal + Factura.IVA)) > Cliente.LimCheque THEN 
        DO:
            ASSIGN 
                Respuesta = "El cliente "
                        + STRING(Cliente.Id-Cliente) + 
                        " sobrepasara su limite de credito, no es posible realizar el cambio." .
            RETURN.
        END.
    END.

    FIND FIRST blkRFC WHERE REPLACE(blkRFC.RFC," ","") = REPLACE(Cliente.RFC," ","") NO-LOCK NO-ERROR.
    IF AVAILABLE blkRFC THEN 
    DO:
        ASSIGN 
            Respuesta = "El cliente "
                    + STRING(Cliente.Id-Cliente) + 
                    " tiene bloquedo su RFC, no es posible realizar el cambio.".
        RETURN.
    END.
            
    IF l-Plazo <= 0 THEN 
    DO:
        ASSIGN 
            Respuesta = "Se debe Asignar un Plazo mayor a 0".
        RETURN.
    END.
              
    /* Buscar el UsoCFDI con el ID proporcionado */
    FIND UsoCfdi WHERE UsoCfdi.Id-UsoCfdi = l-UsoCfdi NO-LOCK NO-ERROR.
    IF NOT AVAILABLE UsoCfdi THEN 
    DO:
        ASSIGN 
            Respuesta = "UsoCFDI Inexistente....".
        RETURN.
    END.

    /* Comprobar si el UsoCFDI es correcto */
    IF NOT CAN-DO(UsoCFDI.ListaRFiscal, l-RFiscal) THEN 
    DO:
        ASSIGN 
            Respuesta = "El regimen fiscal del cliente no es valido para el uso del CFDI ingresado, " +
        "los valores aceptados para el uso del CFDI " + STRING(l-UsoCfdi) +
        " son " + UsoCFDI.ListaRFiscal + ". Favor de verificar...".
        RETURN.
    END.

    /* Validar el régimen fiscal */
    FIND FIRST RFiscal WHERE RFiscal.Id-RFiscal = l-RFiscal NO-LOCK NO-ERROR.
    IF NOT AVAILABLE RFiscal THEN 
    DO:
        ASSIGN 
            Respuesta = "Folio de regimen fiscal inexistente, favor de verificar.".
        RETURN.
    END.

    /* Validar UsoCFDI si tiene un valor */
    IF l-UsoCFDI <> "" THEN 
    DO:
        FIND FIRST UsoCFDI WHERE UsoCFDI.Id-UsoCFDI = l-UsoCFDI NO-LOCK NO-ERROR.
        IF NOT AVAILABLE UsoCFDI THEN 
        DO:
            ASSIGN 
                Respuesta = "UsoCFDI no encontrado para el valor proporcionado.".
            RETURN.
        END.

        /* Validar si el régimen fiscal es válido */
        IF NOT CAN-DO(UsoCFDI.ListaRFiscal, l-RFiscal) THEN 
        DO:
            ASSIGN 
                Respuesta = "El regimen fiscal del cliente no es valido para el uso del CFDI ingresado, " +
            "los valores aceptados para el uso del CFDI " + STRING(l-UsoCfdi) +
            " son " + UsoCFDI.ListaRFiscal + ". Favor de verificar...".
            RETURN.
        END.
    END.
        

    /*
Programa: tesa201.p
Funcion : Rutina de refacturacion de credito
Autor   : FLC
Fecha   : 5 MAY 2018
*/

    /* Validación del Usuario */
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.  
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado no existe.".
        RETURN.
    END.

    /* Obtener el nivel del usuario */
    ASSIGN 
        l-Nivel = Usuario.Nivel.  

    /* Validar que el nivel del usuario sea 2 o 3 */
    IF l-Nivel <> 2 AND l-Nivel <> 3 THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario no tiene permiso para realizar esta operación.".
        RETURN.
    END.

    /* Validar acceso a la caja */
    FIND FIRST Caja WHERE Caja.Id-Caja = INTEGER(Usuario.Id-Caja) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Caja THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario no tiene acceso a ninguna caja.".
        RETURN. 
    END.

    FIND LAST CtlCaja WHERE CtlCaja.Id-Caja = Caja.Id-Caja AND 
        CtlCaja.FecCierre = ? NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CtlCaja THEN 
    DO:
        ASSIGN 
            Respuesta = "La caja " + CAPS(Caja.Descr) + " está cerrada.".
        RETURN.
    END.

    FIND FIRST CorteCaja WHERE CorteCaja.Id-Caja = CtlCaja.Id-Caja AND
        CorteCaja.Turno = CtlCaja.Turno AND
        CorteCaja.FecOper = CtlCaja.FecOper AND
        CorteCaja.Declaracion > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE CorteCaja THEN 
    DO:
        ASSIGN 
            Respuesta = "El cajero ya hizo su declaración.".
        RETURN.
    END.

    FIND Factura WHERE Factura.Id-Factura = pIdFactura NO-LOCK NO-ERROR.
    IF Factura.UUID = "" THEN 
        ASSIGN l-Timbrada = FALSE.
    ELSE 
        ASSIGN l-Timbrada = TRUE.
    
    l-Anter = FALSE.

    IF AVAILABLE Factura AND l-Timbrada = TRUE THEN 
    DO: 
        FIND LAST DetFactura WHERE DetFactura.Id-Factura = Factura.Id-Factura
            AND DetFactura.Descr BEGINS 'ESTA FACTURA SUSTITUYE A' NO-LOCK NO-ERROR.
        IF AVAILABLE DetFactura THEN 
        DO:
            IF SUBSTRING(DetFactura.Descr,43,1) = '0' THEN
                MESSAGE DetFactura.Descr + '\n * Factura puede tener refacturas anteriores *' VIEW-AS ALERT-BOX. 
            ELSE
                MESSAGE DetFactura.Descr VIEW-AS ALERT-BOX.
        END.
    END.

    RUN CreaFac(INPUT pIdFactura,INPUT pIdUser,INPUT l-Cliente,INPUT l-Plazo,INPUT l-RFiscal,
        INPUT l-Req,INPUT l-UsoCfdi,OUTPUT l-NFactura,OUTPUT Respuesta).
        
    IF Respuesta <> "" THEN DO:
        ASSIGN 
            Respuesta = Respuesta.
        RETURN. 
    END.
    ASSIGN       
        Respuesta = l-NFactura. 
    
    IF l-Timbrada = TRUE THEN 
    DO:       
        IF l-Anter = TRUE THEN
            RUN GAcuseFac.
        ELSE
            RUN CancelaFac(INPUT pIdFactura, INPUT pIdUser).
    END.

    RUN EnviaCorreo(INPUT pIdFactura,INPUT pIdUser).
   
    OUTPUT TO "/usr2/adosa/logs/refacturacion.txt" APPEND.
    EXPORT pIdFactura l-NFactura pIdUser l-cliente TODAY STRING(TIME,"hh:mm:ss") .     
    OUTPUT CLOSE.   
            
            
    IF AVAILABLE Factura    THEN RELEASE Factura.   
    IF AVAILABLE MovCliente THEN RELEASE MovCliente.
    IF AVAILABLE VFolio     THEN RELEASE VFolio.      
    
    /* Mensaje de log para depuración */
    MESSAGE "Tablas liberadas para Nueva Fac:" l-NFactura + " " + pIdUser VIEW-AS ALERT-BOX.
    RETURN.
END PROCEDURE.


PROCEDURE CancelaFac.
    DEFINE INPUT  PARAMETER pIdFactura AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUser AS CHARACTER NO-UNDO. /* Usuario a validar */
    STATUS DEFAULT "Cancelando ...".
    Proc:
    DO TRANSACTION ON ENDKEY UNDO,LEAVE ON ERROR UNDO,LEAVE:
        FIND Factura WHERE Factura.Id-Factura = pIdFactura EXCLUSIVE-LOCK NO-ERROR.
        FIND FIRST MovCliente WHERE MovCliente.RefSaldo = Factura.Id-Factura
            AND MovCliente.Id-Cliente = Factura.Id-Cliente
            AND MovCliente.Id-MC = 1
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE MovCliente THEN           
            DELETE MovCliente.
        
        ASSIGN 
            Factura.CteEstatus   = "RF." + l-NFactura
            Factura.FecCancel    = TODAY
            Factura.UsuarioCanc  = CAPS(pIdUser)
            Factura.PorIdFactura = l-NFactura
            Factura.PorUUID      = l-NUUID.

        CREATE DetFactura.
        ASSIGN 
            DetFactura.Id-Factura  = Factura.Id-Factura
            DetFactura.Id-Articulo = ""
            DetFactura.Id-Color    = 0
            DetFactura.Id-Pres     = 0
            DetFactura.Descr       = "REFACTURADA EN EL FOLIO " + l-NFactura
            DetFactura.Tipo        = 5
            DetFactura.Reng        = 1.
    END.  /* Fin de Transaccion */
    
    RELEASE Factura.
    RELEASE DetFactura.   
    RELEASE MovCliente.

    /* Cancelar la factura electronica */
    FIND Factura WHERE Factura.Id-Factura = pIdFactura NO-LOCK NO-ERROR.
    IF AVAILABLE Factura AND Factura.FecCancel <> ? AND Factura.Folioe <> '' THEN 
    DO:
        /* */ RUN /usr2/adosa/procs/vtac2062.p(INPUT pIdFactura). /* */                
    END.

    STATUS DEFAULT ''.  
    BELL.
    
END.     


PROCEDURE CreaFac.
    DEFINE INPUT  PARAMETER pIdFactura AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUser    AS CHARACTER NO-UNDO. /* Usuario a validar */   
    DEFINE INPUT PARAMETER l-Cliente      LIKE Factura.Id-Cliente NO-UNDO.
    DEFINE INPUT PARAMETER l-Plazo        LIKE Factura.Plazo NO-UNDO. 
    DEFINE INPUT PARAMETER l-RFiscal      LIKE Rfiscal.Id-RFiscal NO-UNDO.
    DEFINE INPUT PARAMETER l-Req          LIKE Factura.requisicion NO-UNDO.
    DEFINE INPUT PARAMETER l-UsoCfdi      LIKE Factura.Id-UsoCFDI NO-UNDO.
    DEFINE OUTPUT PARAMETER l-NFactura AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER v-Resp  AS CHARACTER . 
    
    DO ON ERROR UNDO, LEAVE:
        FIND Factura WHERE Factura.Id-Factura = pIdFactura NO-LOCK NO-ERROR.
        IF Factura.UUID = "" THEN 
            ASSIGN l-Timbrada = FALSE.
        ELSE 
            ASSIGN l-Timbrada = TRUE.   
    
        FIND FIRST Cliente WHERE Cliente.Id-Cliente = l-Cliente NO-LOCK NO-ERROR.    
        CREATE tt-Factura.
        BUFFER-COPY Factura TO tt-Factura.            
        ASSIGN 
            tt-Factura.Id-Cliente      = l-Cliente  
            tt-Factura.RazonSocial     = Cliente.RazonSocial
            tt-Factura.CalleNo         = Cliente.CalleNo
            tt-Factura.Colonia         = Cliente.Colonia
            tt-Factura.CP              = Cliente.CP
            tt-Factura.Id-Ciudad       = Cliente.Id-Ciudad
            tt-Factura.RFC             = Cliente.RFC
            tt-Factura.Tel             = Cliente.Tel1
            tt-Factura.Fax1            = Cliente.Fax
            tt-Factura.Id-RutaEmb      = Cliente.Id-RutaEmb
            tt-Factura.FEFormaPago     = Cliente.FEFormaPago
            tt-Factura.FEDigitosCuenta = Cliente.FEDigitosCuenta
            tt-Factura.BuzonFiscal     = Cliente.BuzonFiscal
            tt-Factura.NomEmpresa      = Cliente.NomEmpresa
            tt-Factura.RSocietario     = Cliente.RSocietario
            tt-Factura.FecReg          = TODAY
            tt-Factura.Plazo           = l-Plazo
            tt-Factura.FecVence        = tt-Factura.FecReg + tt-Factura.Plazo
            tt-Factura.requisicion     = l-Req
            tt-Factura.Id-RFiscal      = l-RFiscal 
            tt-Factura.Id-UsoCFDI      = l-UsoCfdi
            tt-Factura.CteEstatus      = "RF." + pIdFactura
            tt-Factura.SustUUID        = IF l-Timbrada = TRUE THEN Factura.UUID ELSE ""
            tt-Factura.SustIdFactura   = IF l-Timbrada = TRUE THEN 
            (IF Factura.SustIdFactura <> "" THEN Factura.SustIdFactura ELSE Factura.Id-Factura) ELSE "".
            
        FOR EACH DetFactura OF Factura NO-LOCK:
            CREATE tt-DetFactura.
            BUFFER-COPY DetFactura TO tt-DetFactura.
            ASSIGN
                tt-DetFactura.Id-Factura = Factura.Id-Factura.
        END.   
        
 
        IF l-Timbrada = TRUE THEN 
        DO:
            CREATE tt-DetFactura.
            ASSIGN 
                tt-DetFactura.Id-Factura  = pIdFactura //tt-Factura.Id-Factura
                tt-DetFactura.Id-Articulo = ""
                tt-DetFactura.Id-Color    = 0
                tt-DetFactura.Id-Pres     = 0
                tt-DetFactura.Descr       = "ESTA FACTURA SUSTITUYE A LA FACTURA FOLIO " + pIdFactura
                tt-DetFactura.Tipo        = 5
                tt-DetFactura.Reng        = 1.
        
            EMPTY TEMP-TABLE tt-DetSerie.
            FOR EACH DetSerie WHERE DetSerie.Tipo      = 'Factura' AND
                DetSerie.Documento = pIdFactura
                NO-LOCK:
                CREATE tt-DetSerie.
                BUFFER-COPY DetSerie TO tt-DetSerie.
            END.

            FIND VFolio WHERE VFolio.Id-Doc = "FAC" AND VFolio.Id-Alm = "" EXCLUSIVE-LOCK NO-ERROR.
        
            ASSIGN 
                l-NFactura            = STRING(VFolio.Folio,"9999999")
                VFolio.Folio          = VFolio.Folio + 1
                tt-Factura.Id-Factura = l-NFactura.

            ASSIGN 
                tt-Factura.FecReg  = TODAY 
                tt-Factura.FecVenc = tt-Factura.FecReg + tt-Factura.Plazo.
            
            CREATE Factura.
            BUFFER-COPY tt-Factura TO Factura.
        END.
        ELSE 
        DO:
            FIND CURRENT Factura EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN 
                Factura.Id-Cliente      = tt-Factura.Id-Cliente
                Factura.RazonSocial     = tt-Factura.RazonSocial
                Factura.CalleNo         = tt-Factura.CalleNo
                Factura.Colonia         = tt-Factura.Colonia
                Factura.CP              = tt-Factura.CP
                Factura.Id-Ciudad       = tt-Factura.Id-Ciudad
                Factura.RFC             = tt-Factura.RFC
                Factura.Tel             = tt-Factura.Tel
                Factura.Fax1            = tt-Factura.Fax1
                Factura.Id-RutaEmb      = tt-Factura.Id-RutaEmb
                Factura.Plazo           = tt-Factura.Plazo
                Factura.FEFormaPago     = tt-Factura.FEFormaPago
                Factura.FEDigitosCuenta = tt-Factura.FEDigitosCuenta
                Factura.BuzonFiscal     = tt-Factura.BuzonFiscal
                Factura.Id-UsoCFDI      = tt-Factura.Id-UsoCFDI
                Factura.Id-RFiscal      = tt-Factura.Id-RFiscal
                Factura.NomEmpresa      = tt-Factura.NomEmpresa
                Factura.RSocietario     = tt-Factura.RSocietario   
                Factura.FecVence        = tt-Factura.FecReg + tt-Factura.Plazo.
            
            FIND FIRST MovCliente WHERE MovCliente.RefSaldo = Factura.Id-Factura
                AND MovCliente.Id-MC = 1 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE MovCliente THEN 
                ASSIGN MovCliente.Id-Cliente = Factura.Id-Cliente
                    MovCliente.FecVenc    = Factura.FecVence.
        END.

        IF l-Timbrada = TRUE THEN 
        DO:  
            ASSIGN 
                Factura.FecReg     = TODAY 
                Factura.Especial   = TRUE
                Factura.Id-Fiscal  = ""
                Factura.Folioe     = ""
                Factura.UUID       = ""
                Factura.VersionSAT = "".
           
            FOR EACH tt-DetFactura NO-LOCK:
                DO:
                    CREATE DetFactura.
                    BUFFER-COPY tt-DetFactura TO DetFactura.
                    ASSIGN 
                        DetFactura.Id-Factura = l-NFactura.
                END.
            END.

            FOR EACH tt-DetSerie NO-LOCK:
                DO:
                    CREATE DetSerie.
                    BUFFER-COPY tt-DetSerie TO DetSerie.
                    ASSIGN 
                        DetSerie.Documento = l-NFactura.
                END.
            END.  

      
        /* Distribuye el IVA de la Factura (Participacion) */
            {cxca0007.i 
            &Factura = Factura.Id-Factura
            &TipoVenta = 3
        }    

        /* afectar la cartera de clientes */
            {cxca0001.i
            &TipoMov      = 1
            &TipoPadre    = 1
            &FecReg       = Factura.FecReg
            &FecVence     = Factura.FecVence
            &Documento    = Factura.Id-Factura
            &RefSaldo     = Factura.Id-Factura
            &Importe      = "(Factura.Tot)"
            &Cliente      = Factura.Id-Cliente
            &Afectar      = " TRUE "
        }

/* Registra el Movimiento en Cajas */
FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
IF Usuario.Id-Caja <> '' THEN 
DO:
    FIND Caja WHERE Caja.Id-Caja = INT(Usuario.Id-Caja) NO-LOCK NO-ERROR.
    FIND Cajero OF Usuario NO-LOCK NO-ERROR.
    FIND FIRST Empleado WHERE Empleado.Iniciales = Cajero.Iniciales
        AND Empleado.Activo NO-LOCK NO-ERROR.
        {ausa0002.i
                &Cliente      = Factura.Id-Cliente
                &Referencia   = Factura.Id-factura
                &TipoVenta    = 3
                &TotVenta     = Factura.Tot
                &Estatus      = 1
                &FolioAut     = 0
                &EmpIniciales = Empleado.Iniciales
                &Cajero       = Usuario.Id-Cajero
               }
END.    
        
RELEASE Folio.  
RELEASE MovCliente.  
RELEASE MovCaja.
RELEASE DistIva.
MESSAGE 'Se genero el folio ' + Factura.Id-Factura .
END.        
    
/* Generacion de la factura Electronica  */

RUN programas/vtac2060.p(INPUT Factura.Id-Factura,
    INPUT Factura.Id-Factura,
    INPUT 6,
    INPUT pIdUser,
    OUTPUT v-Resp).  
    
IF v-Resp <> "" THEN 
DO:
    MESSAGE "Error al generar CFDI: " v-Resp VIEW-AS ALERT-BOX ERROR.
    LEAVE.
END.   
    
IF l-Timbrada = TRUE THEN    
DO:
    /*RUN vtac2063.p(INPUT Factura.Id-Factura).*/ 
    ASSIGN 
        l-NUUID = Factura.UUID.
    FOR EACH Pedido WHERE Pedido.Id-Factura = pIdFactura NO-LOCK:
        FIND b-Pedido WHERE RECID(b-Pedido) = RECID(Pedido) EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE b-Pedido THEN
            ASSIGN b-Pedido.Id-Factura = l-NFactura
                b-Pedido.FecFac     = Factura.FecReg.
        RELEASE b-Pedido.
    END.
    FOR EACH EstPedido WHERE EstPedido.Id-Factura = pIdFactura NO-LOCK:
        FIND b-EstPedido WHERE RECID(b-EstPedido) = RECID(EstPedido) EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE b-EstPedido THEN
            ASSIGN b-EstPedido.Id-Factura = l-NFactura
                b-EstPedido.FecFac     = Factura.FecReg.
        RELEASE b-EstPedido.
    END.
    FOR EACH Movim WHERE Movim.Refer = pIdFactura
        AND Movim.Tipo = "sVtaF" NO-LOCK:
        FIND b-Movim WHERE RECID(b-Movim) = RECID(Movim) EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE b-Movim THEN
            ASSIGN b-Movim.Refer = l-NFactura.
        RELEASE b-Movim.
    END.    
END.
RELEASE Factura. 
END.        
END PROCEDURE.  

PROCEDURE EnviaCorreo.
    DEFINE INPUT  PARAMETER pIdFactura AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUser AS CHARACTER NO-UNDO. /* Usuario a validar */
    FIND Factura WHERE Factura.Id-Factura = pIdFactura NO-LOCK NO-ERROR. 
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    ASSIGN
        l-Asunto    = "REFACTURACION"
        l-Contenido = "<html><head>" +
                      "<p class=MsoNormal align=center style='text-align:left'><span style='font-size:14.0pt;font-family:Verdana'>" +
                      "Se le informa que fue realizada la siguiente refacturacion:<br/><br/>" +
                      "FACTURA: <b>" + Factura.Id-Factura +  "</b><br/>" +
                      "FECHA: <b>" + STRING(Factura.FecReg,"99/99/9999") + "</b><br/>" +
                      "CLIENTE: <b>" + STRING(Factura.Id-Cliente,"99999") + " " + Factura.RazonSocial + "</b><br/>" +
                      "TOTAL: <b>" + STRING(Factura.Tot) + "</b><br/><br/>" +

                      "NUEVA FACTURA: <b>" + l-NFactura +  "</b><br/>" +
                      "FECHA: <b>" + STRING(TODAY,"99/99/9999") + "</b><br/>" +
                      "HORA: <b>" + STRING(TIME,"hh:mm:ss") + "</b><br/>" +
                      "USUARIO: <b>" + pIdUser + " " + Usuario.Nom-Usuario + "</b><br/><br/>" +
                      "<o:p></o:p></span></p>" +
                      '<meta http-equiv="Content-Type" content="text/html; charset=windows-1252">' +
                      "</head>".
                      
    IF AVAILABLE Usuario THEN
        ASSIGN
            l-Mail       = Usuario.e-Mail
            v-MailDe     = Usuario.e-mail + ";" + Usuario.Nom-Usuario
            v-ResponderA = Usuario.e-mail + ";" + Usuario.Nom-Usuario.
            
    IF {sist0001.i} = "DESARROLLO" THEN 
        ASSIGN  l-Mail = "desarrollo10@adosa.com.mx".                   
    ELSE 
    DO:        
        ASSIGN 
            l-Mail = "flucio@adosa.com.mx;desarrollo10@adosa.com.mx".
    END.           
    /* Asigna el mail de respuesta */
    IF v-ResponderA <> "" THEN
        ASSIGN
            v-MailDe = v-MailDe + "^" + v-ResponderA.
            
    /* Activa confirmacion de lectura */
    ASSIGN
        v-MailDe = v-MailDe + CHR(1) + "No,No".
    
      {programas/inva0007.i
                &Asunto     = "l-Asunto"
                &contenido  = "l-Contenido"
                &Iniciales  = "'JAGR'"
                &Direccion  = "l-Mail"
                &Refer      = "'DIRECTO'"
                &Attachment = ""
      }      
    
    /*       
    RUN /usr2/adosa/procs/correo01.p (INPUT l-Mail,
        INPUT v-MailDe,
        INPUT "",
        INPUT "", /*nombre del archivo solamente */
        INPUT "", /*ruta completa del archivo */
        INPUT l-Asunto,
        INPUT l-Contenido,
        OUTPUT v-Enviado).
        
        */
/*  message l-mail skip v-mailde skip l-asunto skip v-enviado view-as alert-box.  */                                      
END PROCEDURE. 
    

