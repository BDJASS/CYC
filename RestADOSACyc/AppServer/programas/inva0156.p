/*------------------------------------------------------------------------
    File        : inva0156.p
    Purpose     : 

    Syntax      :

    Description : Recibe parametros de pedido autorizado para generar requisiciones.

    Author(s)   : sis6
    Created     : Sep 03, 2021
    Notes       :
        
    Ticket: 
           Se modifico para cambiar el programa de envio de correos
           JASS11092025    
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttArtAlmacen NO-UNDO
    FIELD id-Almacen  LIKE Almacen.Id-Alm
    FIELD id          AS INTEGER
    FIELD existencia  AS INTEGER
    FIELD cantPed     LIKE DetPedido.CantPed
    FIELD orden       AS INTEGER
    FIELD genreq      AS LOGICAL
    INDEX idxArtalm id-almacen DESC id DESC.

DEFINE TEMP-TABLE ttArticulos NO-UNDO
    FIELD id                  AS INTEGER
    FIELD id-Articulo LIKE DetPedido.Id-Articulo
    FIELD id-Pres     LIKE DetPedido.Id-Pres
    FIELD id-Color    LIKE DetPedido.Id-Color
    FIELD Kolor       LIKE kolor.abrev
    FIELD Pres        LIKE ArtPres.Descr
    FIELD equiv       LIKE Artpres.equiv
    FIELD cantPed     LIKE DetPedido.CantPed LABEL "CantPed"
    FIELD cantPdte    LIKE DetPedido.CantPed LABEL "CantPdte"
    FIELD completo    AS LOGICAL
    FIELD completofor AS LOGICAL        // Completo con foraneo
    FIELD descripcion AS CHARACTER
    FIELD cambiaPres AS LOGICAL
    INDEX idxArticulos id DESC id-articulo DESC.

/* ******************************************************************** */

DEF INPUT PARAMETER iIdPedido LIKE Pedido.Id-Pedido.
DEF INPUT PARAMETER iResto LIKE Pedido.Resto.
DEF INPUT PARAMETER iOpcion AS INTEGER NO-UNDO.
DEF OUTPUT PARAMETER oEstatus AS LOGICAL NO-UNDO.

DEF VAR lstOpciones AS CHARACTER NO-UNDO EXTENT 4 INITIAL ["AUTORIZADO", "VTA. INTERNET", "EMPAQUE","VTA. EXIROS"].

DEF VAR y AS INTEGER NO-UNDO.
DEF VAR x AS INTEGER NO-UNDO.
DEF VAR lstOrden AS CHARACTER NO-UNDO INITIAL "6,7,8,9,10,03A".
DEF VAR lstOrdenCompleto AS CHARACTER NO-UNDO INITIAL "6,7,8,9,10,03A,11".

DEF VAR l-equiv LIKE ArtPres.Equiv.
DEF VAR HayReq AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR l-foliotmp AS CHAR NO-UNDO.

DEF VAR l-mensaje AS CHAR NO-UNDO.
DEF VAR l-mensaje2 AS CHAR NO-UNDO.
DEF VAR l-mensaje3 AS CHAR NO-UNDO.
DEF VAR l-asunto AS CHAR NO-UNDO.

DEF VAR v-MailDe     AS CHARACTER NO-UNDO.
DEF VAR v-ResponderA AS CHARACTER NO-UNDO.
DEF VAR v-login      AS CHARACTER NO-UNDO.
DEF VAR v-Enviado    AS LOGICAL   NO-UNDO.
DEF VAR l-eMail      AS CHARACTER  NO-UNDO.
DEF VAR l-eMail2      AS CHARACTER  NO-UNDO.


DEF VAR oSuccessful    AS LOGICAL      NO-UNDO INIT YES.
DEF VAR vMessage       AS CHAR                      NO-UNDO.

/*_ ***************************************************
    0. LLENO ttArticulos DONDE FALTE EXISTENCIA  
*************************************************** _*/
FIND FIRST Pedido WHERE Pedido.Id-Pedido = iIdPedido AND Pedido.Resto = iResto 
     AND Pedido.Id-Estatus < 5
     NO-LOCK NO-ERROR.
IF AVAILABLE Pedido THEN DO:
    FIND Vendedor WHERE Vendedor.Id-Vendedor = Pedido.Id-Vendedor NO-LOCK NO-ERROR.
    FIND FIRST Empleado WHERE Empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
    FIND FIRST Usuario WHERE Usuario.Id-User = Vendedor.Iniciales NO-LOCK NO-ERROR.
    ASSIGN l-eMail = (IF AVAILABLE Usuario THEN Usuario.e-mail ELSE "").
    
    y = 1.
    FOR EACH DetPedido OF Pedido NO-LOCK:
        IF DetPedido.Tipo > 1 THEN NEXT.
        IF DetPedido.CantEnt >= DetPedido.CantPed THEN NEXT.
        
        /*_ Hago conversion a UMI's de cantidad pedida y busco la cantidad en 
        almacen origen para restarlo a la busqueda de existencias  _*/
        FIND FIRST ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo 
                             AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK NO-ERROR.
        IF AVAILABLE ArtPres THEN l-equiv = ArtPres.Equiv.
        ELSE l-equiv = 1. 
        
        IF DetPedido.ExistAlta >= (DetPedido.CantPed * l-equiv) THEN NEXT. /* DetPedido.ExistAlta: Toma la existencia antes de bajar el inventario en vtaa0001.new */
        ELSE DO:
            IF Pedido.Id-Alm = "02B" THEN DO:
                FOR EACH ArtUbic WHERE ArtUbic.Id-Articulo = DetPedido.Id-Articulo
                                   AND ArtUbic.Id-Color = DetPedido.Id-Color
                                   AND CAN-DO("02B,02A,02C", ArtUbic.Id-Alm)
                                   AND ArtUbic.Exist > 0 NO-LOCK:
                    ACCUMULATE ArtUbic.Exist (TOTAL).
                END.
                /*IF ((ACCUM TOTAL ArtUbic.Exist) + DetPedido.CantCom) >= (DetPedido.CantPed * l-Equiv) THEN NEXT.*/
                IF (ACCUM TOTAL ArtUbic.Exist) >= (DetPedido.CantPed * l-Equiv) THEN NEXT.
            END.
        END.
        
        FIND FIRST ttArticulos WHERE ttArticulos.Id-Articulo = DetPedido.Id-Articulo
                                 AND ttArticulos.Id-Color    = DetPedido.Id-Color NO-LOCK NO-ERROR.
        IF AVAILABLE ttArticulos THEN DO:
                ASSIGN ttArticulos.cantPed      = ttArticulos.cantPed + (DetPedido.CantPed * l-equiv)
                       ttArticulos.cantPdte     = ttArticulos.cantPdte + (DetPedido.CantPed * l-equiv)
                       ttArticulos.cambiaPres   = (IF ttArticulos.cambiaPres OR ttArticulos.equiv <> ArtPres.Equiv THEN TRUE ELSE FALSE).
        END.
        ELSE DO:
            FIND FIRST Kolor WHERE Kolor.Id-Color = DetPedido.Id-Color NO-LOCK NO-ERROR.
                CREATE ttArticulos.
                ASSIGN ttArticulos.Id = y
                       ttArticulos.Id-Articulo  = DetPedido.Id-Articulo
                       ttArticulos.Id-Pres      = IF AVAILABLE ArtPres THEN ArtPres.Id-Pres ELSE 1
                       ttArticulos.Pres         = IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE ""
                       ttArticulos.Kolor        = IF AVAILABLE Kolor THEN kolor.abrev ELSE ""
                       ttArticulos.equiv        = l-equiv
                       ttArticulos.Id-Color     = DetPedido.Id-Color
                       ttArticulos.cantPed      = (DetPedido.CantPed * l-equiv) - (IF DetPedido.ExistAlta > 0 THEN DetPedido.ExistAlta ELSE 0) 
                       ttArticulos.cantPdte     = (DetPedido.CantPed * l-equiv) - (IF DetPedido.ExistAlta > 0 THEN DetPedido.ExistAlta ELSE 0) 
                       ttArticulos.Completo     = FALSE
                       ttArticulos.Completofor  = FALSE
                       ttArticulos.descripcion  = DetPedido.Descr
                       ttArticulos.cambiaPres    = FALSE.
                       y = y + 1.
        END.
        RELEASE ttArticulos.
    END. 
        
    // 1. LLENO TEMPORAL ttArtAlmacen -----------------------------------------------    
    FOR EACH ttArticulos EXCLUSIVE-LOCK:
        // Busco la UMI menor de venta SOLO si tengo mas de 1 linea del mismo articulo faltante
        IF ttArticulos.cambiaPres THEN DO:
            FIND FIRST Artpres WHERE ArtPres.Id-articulo = ttArticulos.id-Articulo AND
                                     ArtPres.Tipo = 3 AND
                                     ArtPres.Activo AND
                                     ArtPres.Equiv >= 1 NO-LOCK NO-ERROR.
            IF AVAILABLE Artpres THEN DO:
                ASSIGN ttArticulos.equiv    = ArtPres.Equiv
                       ttArticulos.Pres     = ArtPres.Descr
                       ttArticulos.cantPed  = IF (ttArticulos.cantPed MODULO ttArticulos.Equiv) = 0 THEN ttArticulos.cantPed ELSE ttArticulos.cantPed + (ttArticulos.Equiv - (ttArticulos.cantPed MODULO ttArticulos.Equiv))
                       ttArticulos.cantPdte = ttArticulos.cantPed.
            END.
        END.
        ELSE DO:
            // Busco ajustar UMIs si tengo menos de las solicitadas
            IF (ttArticulos.cantPed MODULO ttArticulos.Equiv) > 0 THEN DO:
                ASSIGN ttArticulos.cantPed  = ttArticulos.cantPed + (ttArticulos.Equiv - (ttArticulos.cantPed MODULO ttArticulos.Equiv))
                       ttArticulos.cantPdte = ttArticulos.cantPed.
            END.            
        END.
        
            REPEAT x = 1 TO NUM-ENTRIES(lstOrdenCompleto):
                    FIND FIRST ArtUbic WHERE ArtUbic.id-art   = ttArticulos.id-Articulo
                                                             AND ArtUbic.id-Color = ttArticulos.id-Color
                                                             AND Artubic.id-alm   = ENTRY(x,lstOrdenCompleto)
                    NO-LOCK NO-ERROR.
                    IF AVAILABLE ArtUbic AND ArtUbic.Exist > 0 THEN DO:
                            CREATE ttArtAlmacen.
                            ASSIGN ttArtAlmacen.id-Almacen  = ENTRY(x,lstOrdenCompleto)
                                   ttArtAlmacen.id          = ttArticulos.id
                                   ttArtAlmacen.existencia  = ArtUbic.Exist
                                   ttArtAlmacen.cantPed     = 0
                                   ttArtAlmacen.orden       = LOOKUP(ENTRY(x,lstOrdenCompleto),lstOrdenCompleto).
                       RELEASE ttArtAlmacen.
                    END.
            END.
    END.
    
    // 3. VALIDO CANTIDADES SOLICITADAS EN CADA ALMACEN, EXCEPTO ALM 11 -------------
    FOR EACH ttArticulos WHERE NOT ttArticulos.completo EXCLUSIVE-LOCK:
            FOR EACH ttArtAlmacen WHERE ttArtAlmacen.id = ttArticulos.id AND LOOKUP(ttArtAlmacen.id-Almacen,lstOrden) > 0 EXCLUSIVE-LOCK 
            BY ttArtAlmacen.existencia DESCENDING BY ttArtAlmacen.orden:
                    
                    IF ttArticulos.cantPdte <= 0 THEN NEXT.
                    IF (ttArtAlmacen.existencia - ttArticulos.cantPdte) >= 0 THEN DO:
                            ASSIGN ttArtAlmacen.genreq  = TRUE
                                   ttArtAlmacen.cantPed = IF (ttArtAlmacen.existencia - ttArticulos.cantPdte) >= 0 THEN ttArticulos.cantPdte ELSE ttArtAlmacen.existencia
                                   ttArticulos.completo = TRUE
                                   ttArticulos.cantPdte = 0.
                    END.
                    ELSE DO:
                            ASSIGN ttArticulos.CantPdte = ttArticulos.CantPdte - (ttArtAlmacen.existencia - (ttArtAlmacen.existencia MODULO ttArticulos.Equiv))
                                   ttArtAlmacen.cantPed = (ttArtAlmacen.existencia - (ttArtAlmacen.existencia MODULO ttArticulos.Equiv))    
                                   ttArtAlmacen.genreq = TRUE.
                    END.
            END.
    END.
    RELEASE ttArticulos.
    RELEASE ttArtAlmacen.
    
    // 4. VALIDO CANTIDADES SOLICITADAS O PENDIENTES EN ALM 11 ----------------------
    FOR EACH ttArticulos WHERE NOT ttArticulos.completo EXCLUSIVE-LOCK,
            EACH ttArtAlmacen WHERE ttArtAlmacen.id = ttArticulos.id AND ttArtAlmacen.id-Almacen = "11" EXCLUSIVE-LOCK 
            BY ttArtAlmacen.existencia DESC 
            BY ttArtAlmacen.orden:
            
            IF ttArticulos.cantPdte <= 0 THEN NEXT.
            IF (ttArtAlmacen.existencia - ttArticulos.cantPdte) >= 0 THEN DO:
                    ASSIGN ttArtAlmacen.cantPed    = IF (ttArtAlmacen.existencia - ttArticulos.cantPdte) >= 0 THEN ttArticulos.cantPdte ELSE ttArtAlmacen.existencia
                           ttArticulos.completofor = TRUE
                           ttArticulos.cantPdte    = 0.
            END.
            ELSE DO:
                    ASSIGN ttArticulos.CantPdte = ttArticulos.CantPdte - (ttArtAlmacen.existencia - (ttArtAlmacen.existencia MODULO ttArticulos.Equiv))   
                           ttArtAlmacen.cantPed = (ttArtAlmacen.existencia - (ttArtAlmacen.existencia MODULO ttArticulos.Equiv)). 
            END.
    END.
    RELEASE ttArticulos.
    RELEASE ttArtAlmacen.
    
    // 5. REVISO ARTICULOS PENDIENTES PARA OBTENER DETALLES DEL CORREO --------------
    FOR EACH ttArticulos WHERE NOT ttArticulos.completo NO-LOCK:
        IF ttArticulos.completofor THEN DO:
            FIND FIRST ttArtAlmacen WHERE ttArtAlmacen.Id = ttArticulos.Id AND ttArtAlmacen.Id-Almacen = "11" NO-LOCK NO-ERROR.
            l-mensaje2 = l-mensaje2 + '<tr><td> Art. ' + ttArticulos.id-Articulo + '</td><td>' + ttArticulos.descripcion + ' ' + ttArticulos.Kolor + '</td><td>Pres: ' + STRING(ttArtAlmacen.cantPed / ttArticulos.equiv) + ' ' + ttArticulos.Pres + '</td><td>' +
                         "Cant: " + (IF AVAILABLE ttArtAlmacen THEN STRING(ttArtAlmacen.cantPed) ELSE '') + " de " + STRING(ttArticulos.cantPed) + " (UMI)" +
                         '</td></tr>'.
        END.
        ELSE
            l-mensaje3 = l-mensaje3 + '<tr><td> Art. ' + ttArticulos.id-Articulo + '</td><td>' + ttArticulos.descripcion + ' ' + ttArticulos.Kolor + '</td><td>Pres: ' + STRING(ttArticulos.cantPdte / ttArticulos.equiv) + ' ' + ttArticulos.Pres + '</td><td>' +
                        (IF ttArticulos.cantPdte <= ttArticulos.cantPed THEN "<span style='color:red'>&#9888;</span> Faltantes: " + STRING(ttArticulos.cantPdte) + " de " + STRING(ttArticulos.cantPed) + " (UMI)" ELSE " ") +
                        '</td></tr>'.
    END.          
    
    // 6. GENERO LAS REQUISICIONES AUTOMATICAS DE LOS ARTICULOS COMPLETOS -----------
    REPEAT x = 1 TO NUM-ENTRIES(lstOrden):
        ASSIGN y = 1
               HayReq = FALSE.
        FOR EACH ttArtAlmacen WHERE ttArtAlmacen.Id-Alm = ENTRY(x,lstOrden) AND ttArtAlmacen.genreq NO-LOCK,
            EACH ttArticulos WHERE ttArticulos.id = ttArtAlmacen.id AND ttArticulos.completo NO-LOCK:
            IF y EQ 1 THEN DO:
                IF ttArtAlmacen.Id-Alm = '02B' OR ttArtAlmacen.Id-Alm = '03A' OR ttArtAlmacen.Id-Alm = 'FUG' OR ttArtAlmacen.Id-Alm = '11' THEN DO:
                    FIND Folio WHERE Folio.id-Doc = "REQRTP" AND folio.id-alm = ttArtAlmacen.Id-Alm EXCLUSIVE-LOCK NO-ERROR.
                END.
                ELSE DO:
                    FIND Folio WHERE Folio.id-Doc = "REQRTP" AND folio.id-alm = "" EXCLUSIVE-LOCK NO-ERROR.
                END.
                
                ASSIGN
                    l-foliotmp = folio.prefijo + STRING(Folio.folio,"999999").
                    Folio.folio = Folio.folio + 1.
                RELEASE Folio.
                
                CREATE ReqAlm.
                ASSIGN ReqAlm.Id-Req        = l-foliotmp
                       ReqAlm.FecReg        = TODAY                  
                       ReqAlm.Id-Dest       = ttArtAlmacen.Id-Alm
                       ReqAlm.Id-Solic      = STRING(INTEGER(Pedido.Id-Vendedor)) // Por detalle con info. en tabla PersAlm
                       ReqAlm.Id-ubic       = Pedido.Id-Alm
                       ReqAlm.Id-Per        = Pedido.Iniciales
                       ReqAlm.Coment1       = "REQ AUTOMATICA - PEDIDO " + lstOpciones[iOpcion]
                       ReqAlm.Cancelado     = FALSE
                       ReqAlm.Resto         = 0 
                       ReqAlm.Autorizada    = TRUE 
                       ReqAlm.AutPor        = "AUT"
                       ReqAlm.Id-Pedido     = Pedido.Id-Pedido
                       ReqAlm.PedResto      = Pedido.Resto.
               RELEASE ReqAlm.
               
               CREATE EstPedido.
               ASSIGN 
                    EstPedido.Id-Pedido  = l-foliotmp
                    EstPedido.Id-Seq     = 0
                    EstPedido.FecReg     = TODAY
                    EstPedido.Tipo       = 1
                    EstPedido.HorReg     = TIME
                    EstPedido.FecPed     = TODAY
                    EstPedido.FleteYes   = TRUE
                    EstPedido.Id-UbiVta  = Pedido.Id-Alm
                    EstPedido.Id-cliente = 0.
                RELEASE EstPedido.
               
               ASSIGN HayReq = TRUE
                      l-mensaje = l-mensaje + '<tr><td colspan=3></br> Se genera la requisicion <b>' + l-foliotmp + '</b> Alm: ' + ttArtAlmacen.Id-Alm + '</td></tr>'.
            END.
           
            // Crea el detalle de la Requisicion
            CREATE DetReq.
            ASSIGN DetReq.Id-Req        = l-foliotmp
                   DetReq.Id-Articulo   = ttArticulos.Id-Articulo
                   DetReq.Id-Color      = ttArticulos.Id-Color
                   DetReq.Id-Pres       = ttArticulos.Id-Pres
                   DetReq.CantPres      = ttArtAlmacen.cantPed / ttArticulos.equiv
                   DetReq.CantUMI       = ttArtAlmacen.CantPed
                   DetReq.CantCom       = ttArtAlmacen.CantPed
                   DetReq.Seq           = y
                   DetReq.Reng          = y
                   DetReq.ExistAlta     = ttArtAlmacen.Existencia.
                   
           l-mensaje = l-mensaje + '<tr><td> Art. ' + ttArticulos.Id-Articulo + '</td><td>' + ttArticulos.descripcion + ' ' + ttArticulos.Kolor + '</td><td>Pres: ' + STRING(ttArtAlmacen.cantPed / ttArticulos.equiv) + ' ' + ttArticulos.Pres + '</td><td>Cant: ' + STRING(ttArtAlmacen.CantPed) + ' (UMI)</td><td></tr>'.
           
           RELEASE DetReq.
           
           // Compromete Inventarios
           FIND FIRST ArtUbic WHERE ArtUbic.id-art   = ttArticulos.Id-Articulo
                                AND ArtUbic.id-Color = ttArticulos.Id-Color
                                AND Artubic.id-alm   = ttArtAlmacen.Id-Alm
                                EXCLUSIVE-LOCK NO-ERROR.
           ASSIGN ArtUbic.Exist  = ArtUbic.Exist  - ttArtAlmacen.CantPed
                  ArtUbic.Compro = ArtUbic.Compro + ttArtAlmacen.CantPed.
           RELEASE ArtUbic.
            
           y = y + 1.
        END.
        
        IF HayReq THEN
            RUN /usr2/adosa/procs/invc0150.p (INPUT l-foliotmp,INPUT FALSE,'').     // Impresion
    END.

    IF (l-mensaje <> "" OR l-mensaje2 <> "" OR l-mensaje3 <> "") AND HayReq = TRUE THEN DO:
        ASSIGN l-mensaje = '<table>' + l-mensaje.
               l-asunto = 'Creacion de REQ de Pedido ' + Pedido.Id-Pedido + ' (' + lstOpciones[iOpcion] + ')'.
        IF l-mensaje2 <> "" THEN 
            ASSIGN l-mensaje = l-mensaje + '<tr><td colspan=4></br></br><span style="color:blue"><b><u>IMPORTANTE (EXISTENCIAS FORANEAS) &#9951</u></b></span></br> Para los siguientes articulos se completa la cantidad solicitada con existencias foraneas de almacen 11, revisar tiempo de espera de 3-4 dias con el cliente y posteriormente generar la requisici�n manual. </br></br></td></tr>' + l-mensaje2
                   l-asunto = l-asunto + " !".
        IF l-mensaje3 <> "" THEN              
            ASSIGN l-mensaje = l-mensaje + '<tr><td colspan=4></br></br><span style="color:red"><b><u>ATENCION</u></b></span></br> Para los siguientes articulos NO se genero ninguna requisicion por no completar el total de unidades, favor de revisar: </br></br></td></tr>' + l-mensaje3
                   l-asunto = l-asunto + " �". 
        
        l-eMail2  = "desarrollo10@adosa.com.mx;flucio@adosa.com.mx;flopez@adosa.com.mx;eguerra@adosa.com.mx".
        IF l-eMail = "" THEN l-eMail = "desarrollo10@adosa.com.mx;".
        
         ASSIGN      
        l-eMail = l-eMail + ";"+  l-eMail2.  
        /* JASS11092025 */
         {inva0007.i
            &Asunto     = "l-asunto"
            &contenido  = "l-mensaje"
            &Iniciales  = "'SIS10'"
            &Direccion  = "l-eMail"
            &Refer      = "'DIRECTO'"  
            &Attachment = ""    
      } 
        IF l-mensaje <> "" THEN oEstatus = TRUE.
    END.    
END.