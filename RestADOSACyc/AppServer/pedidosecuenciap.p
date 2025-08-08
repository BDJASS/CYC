@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : pedidosecuenciap
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Tue Jul 29 12:51:03 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE NEW SHARED VARIABLE g-Origen  AS CHARACTER NO-UNDO INITIAL "02B".
DEFINE NEW SHARED VARIABLE g-nomcia  AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g-dist    AS INTEGER   NO-UNDO INITIAL 0 FORMAT "9999".
DEFINE NEW SHARED VARIABLE g-tty     AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE l-Tipo-3  AS LOGICAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE l-Tipo-4  AS LOGICAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE l-Alta    AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE NEW SHARED VARIABLE l-Cancela AS LOGICAL   NO-UNDO INITIAL FALSE.
 
/* ********************  Preprocessor Definitions  ******************** */
/*ATENCION, cualquier cambio en la esta definicion de tablas hay que ponerla en vtac0800c0.p y vtac0800cc.p*/
DEFINE TEMP-TABLE ttPedido BEFORE-TABLE bttPedido
    FIELD Id-Pedido        AS CHARACTER LABEL "Pedido"
    FIELD Id-Cot           AS CHARACTER LABEL "Coti."
    FIELD Id-Vendedor      AS CHARACTER LABEL "Vendedor"
    FIELD Id-Captura       AS CHARACTER LABEL "Capt."
    FIELD id-ubivta        AS CHARACTER LABEL "Ubicacion"
    FIELD Id-Cliente       AS INTEGER   INITIAL "0" LABEL "Cuenta"
    FIELD Compra           AS CHARACTER LABEL "Comprador"
    FIELD Attn             AS CHARACTER LABEL "Attn"
    FIELD CalleNo1         AS CHARACTER LABEL "CalleNo"
    FIELD Id-Ciudad1       AS INTEGER   INITIAL "0" LABEL "Cd Emb"
    FIELD Colonia1         AS CHARACTER LABEL "Colonia"
    FIELD Ciudad1          AS CHARACTER LABEL "Ciudad"
    FIELD Estado1          AS CHARACTER LABEL "Estado"
    FIELD CP1              AS CHARACTER LABEL "CP"
    FIELD Pais1            AS CHARACTER LABEL "Pais"
    FIELD ReqCte           AS CHARACTER LABEL "Requisicion"
    FIELD Especial         AS LOGICAL   INITIAL "no" LABEL "Especial"
    FIELD FecReg           AS DATE      INITIAL "?" LABEL "FecReg"
    FIELD FecCap           AS DATE      INITIAL "?" LABEL "Fec.Cap"
    FIELD FecEst           AS DATE      INITIAL "?" LABEL "?"
    FIELD Notas            AS CHARACTER EXTENT 2 LABEL "Notas"
    FIELD Id-Cond          AS INTEGER   INITIAL "0" LABEL "Condicion"
    FIELD Enviar           AS LOGICAL   INITIAL "no" LABEL "?"
    FIELD Id-Flete         AS INTEGER   INITIAL "0" LABEL "Flete"
    FIELD Id-Tran          AS INTEGER   INITIAL "0" LABEL "Transporte"
    FIELD Id-Entrega       AS INTEGER   INITIAL "0" LABEL "Entrega"
    FIELD Seguro           AS LOGICAL   INITIAL "no" LABEL "?"
    FIELD PorcSeg          AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD PorcIVA          AS DECIMAL   INITIAL "0" LABEL "IVA"
    FIELD Sust             AS LOGICAL   INITIAL "no" LABEL "Sustituir"
    FIELD TipoPrecio       AS CHARACTER LABEL "Tipo Pecio"
    FIELD Vigentes         AS LOGICAL   INITIAL "no" LABEL "?"
    FIELD FecBck           AS DATE      INITIAL "?" LABEL "?"
    FIELD EnFirme          AS LOGICAL   INITIAL "no" LABEL "?"
    FIELD FecSta           AS DATE      INITIAL "?" LABEL "Fec.Estatus"
    FIELD hrSta            AS INTEGER   INITIAL "0" LABEL "?"
    FIELD CalleNo          AS CHARACTER LABEL "CalleNo"
    FIELD Colonia          AS CHARACTER LABEL "Colonia"
    FIELD CP               AS CHARACTER LABEL "CP"
    FIELD Id-Ciudad        AS INTEGER   INITIAL "0" LABEL "?"
    FIELD RazonSocial      AS CHARACTER LABEL "Razon Social"
    FIELD RFC              AS CHARACTER LABEL "RFC"
    FIELD Tel1             AS CHARACTER LABEL "Tel"
    FIELD Plazo            AS INTEGER   INITIAL "0" LABEL "Plazo"
    FIELD Tel              AS CHARACTER LABEL "Tel"
    FIELD CveAut           AS INTEGER   INITIAL "0" LABEL "CveAut"
    FIELD Id-Factura       AS CHARACTER LABEL "?"
    FIELD FecFac           AS DATE      INITIAL "?" LABEL "Fec.Fac"
    FIELD FecFac2          AS DATE      INITIAL "?" LABEL "Fec.Fac"
    FIELD Id-Factura2      AS CHARACTER LABEL "?"
    FIELD FecFac3          AS DATE      INITIAL "?" LABEL "Fec.Fac"
    FIELD Id-Factura3      AS CHARACTER LABEL "?"
    FIELD Cancelado        AS LOGICAL   INITIAL "no" LABEL "?"
    FIELD FecCancel        AS DATE      INITIAL "?" LABEL "?"
    FIELD UsuarioCanc      AS CHARACTER LABEL "Usuario Canc"
    FIELD Id-ForPed        AS CHARACTER LABEL "FormPed"
    FIELD Impresiones      AS INTEGER   INITIAL "0" LABEL "Impresiones"
    FIELD PorcCom          AS DECIMAL   INITIAL "0" LABEL "Porc Com"
    FIELD ComEsp           AS LOGICAL   INITIAL "no" LABEL "?"
    FIELD Iniciales        AS CHARACTER LABEL "Ini."
    FIELD DescPP1          AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD DescPP2          AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD Dpp              AS LOGICAL   INITIAL "yes" LABEL "?"
    FIELD PlazoPP          AS INTEGER   INITIAL "0" LABEL "?"
    FIELD EspDPP           AS LOGICAL   INITIAL "?" LABEL "?"
    FIELD Resto            AS INTEGER   INITIAL "?" LABEL "?"
    FIELD Id-Estatus       AS INTEGER   INITIAL "0" LABEL "Estatus"
    FIELD Alm              AS CHARACTER LABEL "?"
    FIELD Id-Corte3        AS CHARACTER LABEL "FolCor3"
    FIELD Filler-1         AS CHARACTER LABEL "?"
    FIELD Id-Corte4        AS CHARACTER LABEL "FolCor4"
    FIELD UsuarioAutD      AS CHARACTER LABEL "?"
    FIELD PPuntual         AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD TipoFactor       AS CHARACTER LABEL "Tipo de Factor"
    FIELD Observaciones    AS CHARACTER EXTENT 2 LABEL "Observaciones"
    FIELD OrdComCte        AS CHARACTER LABEL "OrdenCompra"
    FIELD SubTotal         AS DECIMAL   INITIAL "0" LABEL "SubTotal"
    FIELD AutPor           AS CHARACTER LABEL "Autorizado-por"
    FIELD Adelantado       AS LOGICAL   INITIAL "no" LABEL "Adelantado"
    FIELD Terminado        AS LOGICAL   INITIAL "no" LABEL "Terminado"
    FIELD BckOrd           AS INTEGER   INITIAL "1" LABEL "BackOrder"
    FIELD Bultos           AS INTEGER   INITIAL "0" LABEL "Bultos"
    FIELD Tarimas          AS INTEGER   INITIAL "0" LABEL "Tarimas"
    FIELD Id-Fuente        AS INTEGER   INITIAL "0" LABEL "?"
    FIELD FuenteSta        AS INTEGER   INITIAL "0" LABEL "?"
    FIELD FuenteHor        AS INTEGER   INITIAL "0" LABEL "?"
    FIELD FuenteFec        AS DATE      INITIAL "?" LABEL "?"
    FIELD Id-RutaEmb       AS INTEGER   INITIAL "0" LABEL "Ruta Embarque"
    FIELD e-mail           AS CHARACTER LABEL "e-mail"
    FIELD Id-Remision      AS CHARACTER LABEL "Remision"
    FIELD FecRemision      AS DATE      INITIAL "?" LABEL "Fecha Remision"
    FIELD RazonSocial1     AS CHARACTER LABEL "RazonSocial"
    FIELD Id-Liga          AS CHARACTER LABEL "Ligas"
    FIELD Id-Alm           AS CHARACTER LABEL "Almacen"
    FIELD delegacion1      AS CHARACTER LABEL "Delegacion"
    FIELD fax1             AS CHARACTER LABEL "Fax"
    FIELD vitrina          AS LOGICAL   INITIAL "No" LABEL "Vitrina"
    FIELD tarjetacr        AS CHARACTER LABEL "Tarjeta Crd"
    FIELD auttarcr         AS CHARACTER LABEL "Aut Trj Crd"
    FIELD Id-CteInt        AS INTEGER   INITIAL "0" LABEL "Cliente"
    FIELD FIniAut          AS DATE      INITIAL "?" LABEL "Fecha Inicio Aut"
    FIELD HIniAut          AS INTEGER   INITIAL "0" LABEL "Hora Inicio Aut"
    FIELD FFinAut          AS DATE      INITIAL "?" LABEL "Fecha Fin Aut"
    FIELD HFinAut          AS INTEGER   INITIAL "0" LABEL "Hora Fin Aut"
    FIELD RecCte           AS CHARACTER LABEL "?"
    FIELD Recibe           AS CHARACTER LABEL "?"
    FIELD e-mail2          AS CHARACTER LABEL "e-mail2"
    FIELD BuzonFiscal      AS CHARACTER LABEL "?"
    FIELD PagAnt           AS LOGICAL   INITIAL "No" LABEL "PagAnt"
    FIELD Pagado           AS LOGICAL   INITIAL "No" LABEL "Pagado"
    FIELD UsrPag           AS CHARACTER LABEL "?"
    FIELD FecPag           AS DATE      INITIAL "?" LABEL "FecPag"
    FIELD HorPag           AS INTEGER   INITIAL "0" LABEL "Hora-Pag"
    FIELD PartDetPed       AS INTEGER   LABEL "Partidas"
    FIELD IdPedido         AS CHARACTER LABEL "Pedido."
    FIELD IdCliente        AS INTEGER   INITIAL "0" LABEL "Cuenta"
    FIELD IdVendedor       AS CHARACTER LABEL "Vendedor"
    FIELD Total            AS DECIMAL   INITIAL "0" LABEL "Total"
    FIELD IVA              AS DECIMAL   INITIAL "0" LABEL "IVA"
    FIELD RutaEmb          AS CHARACTER LABEL "Ruta Embarque"
    FIELD Condicion        AS CHARACTER LABEL "Condicion"
    FIELD Entrega          AS CHARACTER LABEL "Entrega"
    FIELD Transporte       AS CHARACTER LABEL "Transporte"
    FIELD VendNombre       AS CHARACTER LABEL "Nombre Vendedor"
    FIELD sHFinAut         AS CHARACTER LABEL "Hora Autorizacion"
    FIELD sHorPag          AS CHARACTER LABEL "Hora Pago"
    FIELD FleteYes         AS LOGICAL   INITIAL "no" LABEL "FleteSN"
    FIELD IdFactura        AS CHARACTER LABEL "?"
    FIELD IdCot            AS CHARACTER LABEL "Coti."
    FIELD IdRemision       AS CHARACTER LABEL "Remision"
    FIELD IdEstatus        AS INTEGER   INITIAL "0" LABEL "Estatus"
    FIELD Estatus          AS CHARACTER INITIAL "" LABEL "ESTATUS"
    FIELD IdCond           AS INTEGER   INITIAL "0" LABEL "Condicion"
    FIELD IdCiudad         AS INTEGER   INITIAL "0" LABEL "?"
    FIELD Ciudad           AS CHARACTER LABEL "Ciudad"
    FIELD Estado           AS CHARACTER LABEL "Estado"
    FIELD Pais             AS CHARACTER LABEL "PAIS"
    FIELD AutPorNom        AS CHARACTER LABEL "AutPorNom"
    FIELD UsrPagNom        AS CHARACTER LABEL "UsrPagNom"
    FIELD IdCaptura        AS CHARACTER LABEL "Capt."
    FIELD idUbivta         AS CHARACTER LABEL "Ubicacion"
    FIELD IdCiudad1        AS INTEGER   INITIAL "0" LABEL "Cd Emb"
    FIELD IdFlete          AS INTEGER   INITIAL "0" LABEL "Flete"
    FIELD IdTran           AS INTEGER   INITIAL "0" LABEL "Transporte"
    FIELD IdEntrega        AS INTEGER   INITIAL "0" LABEL "Entrega"
    FIELD IdForPed         AS CHARACTER LABEL "FormPed"
    FIELD IdFuente         AS INTEGER   INITIAL "0" LABEL "?"
    FIELD IdRutaEmb        AS INTEGER   INITIAL "0" LABEL "Ruta Embarque"
    FIELD IdLiga           AS CHARACTER LABEL "Ligas"
    FIELD IdAlm            AS CHARACTER LABEL "Almacen"
    FIELD IdCteInt         AS INTEGER   INITIAL "0" LABEL "Cliente"
    FIELD email            AS CHARACTER LABEL "e-mail"
    FIELD email2           AS CHARACTER LABEL "e-mail2"
    FIELD Filler1          AS CHARACTER LABEL "?"
    FIELD IdsLigados       AS CHARACTER LABEL "IdsLigados"
    FIELD OpLiga           AS INTEGER   INITIAL "0" LABEL "OpLiga"
    FIELD PusoPassw        AS LOGICAL   INITIAL FALSE LABEL "Puso Password"
    FIELD DescError        AS CHARACTER INITIAL "" LABEL "DescError"
    FIELD FEFormaPago      AS CHARACTER INITIAL "" LABEL "MetodoDePago"
    FIELD FEDigitosCuenta  AS CHARACTER INITIAL "" LABEL "Digitos"
    FIELD Id-UsoCFDI       AS CHARACTER LABEL "Uso CFDI" SERIALIZE-NAME "IdUsoCFDI"                               /*jbs 180115*/
    FIELD Pago             AS CHARACTER INITIAL "" LABEL "Pago"                                                         /*RNPC 20200403*/
    FIELD CreaReq          AS LOGICAL   INITIAL FALSE LABEL "Crea Req"                                                 /*RNPC 20210716*/
    FIELD SurtParcial      AS LOGICAL   INITIAL FALSE LABEL "Surt Parcial"                                         /*RNPC 20210924*/
    FIELD TC               AS LOGICAL   INITIAL "No" LABEL "TC"                                                             /*RNPC 20211103*/
    FIELD PagInfo          AS CHARACTER INITIAL "" LABEL "Pag Info"                                                  /*RNPC 20211103*/
    FIELD SolCancela       AS LOGICAL   INITIAL "yes" LABEL "Sol Cancelacion"                                       /*RNPC 20211112 Bandera para ver si se puede o no cancelar*/ 
    FIELD Id-BancoChP      AS INTEGER   INITIAL "0" LABEL "Id Banco"                                               /*RNPC 20211117*/
    FIELD IdBancoChP       AS INTEGER   INITIAL "0" LABEL "Id Banco1"                                               /*RNPC 20211117*/
    FIELD NumChP           AS CHARACTER LABEL "Num Cheque"                                                            /*RNPC 20211117*/
    FIELD CuentaChP        AS CHARACTER LABEL "Cta Bancaria"                                                       /*RNPC 20211117*/
    FIELD AutChP           AS INTEGER   LABEL "Autorizacion"                                                            /*RNPC 20211117*/
    FIELD UsuarioChP       AS CHARACTER LABEL "Usuario Ch"                                                        /*RNPC 20211117*/
    FIELD FecChP           AS DATE      LABEL "Fecha Ch"                                                                   /*RNPC 20211117*/
    FIELD HorChP           AS INTEGER   LABEL "Hora Ch"                                                                 /*RNPC 20211117*/
    FIELD sHorChP          AS CHARACTER LABEL "Hora Ch1"
    FIELD Referencia       AS CHARACTER LABEL "Referencia"                                                        /*RNPC 20211117*/
    FIELD SolCancPed       AS LOGICAL   INITIAL "No" LABEL "SolCanc"                                                /*RNPC 20211117*/
    FIELD MotivoSolCancPed AS CHARACTER LABEL "Motivo SolCanc"                                              /*RNPC 20211117*/
    FIELD UsuSolCancPed    AS CHARACTER LABEL "Usuario SolCanc"                                                /*RNPC 20211117*/
    FIELD FecSolCancPed    AS DATE      LABEL "Fecha SolCanc"                                                       /*RNPC 20211117*/
    FIELD HorSolCancPed    AS INTEGER   LABEL "Hora SolCanc"                                                     /*RNPC 20211117*/
    FIELD sHorSolCancPed   AS CHARACTER LABEL "Hora SolCanc1"                                                 /*RNPC 20211117*/
    FIELD IdRFiscal        AS CHARACTER LABEL "Regimien Fiscal"                                                    /*RNPC 20220422*/
    FIELD DescrRFiscal     AS CHARACTER LABEL "Descr. R. Fiscal"                                                /*RNPC 20220510*/
    FIELD NomEmpresa       AS CHARACTER LABEL "Nom. Empresa"   
    FIELD Simbolo          AS CHARACTER   /*RNPC 20220801*/
    FIELD IdMoneda         AS CHARACTER                                               
    INDEX Idx-Adelantado            Adelantado  ASCENDING FecReg     ASCENDING 
    INDEX Idx-Alm                   Id-Alm      ASCENDING Id-Pedido  ASCENDING  Resto      ASCENDING 
    INDEX Idx-BckOrd                BckOrd      ASCENDING 
    INDEX idx-cancvitped            FecCancel   ASCENDING vitrina    ASCENDING  FecReg     ASCENDING  Id-Pedido  ASCENDING 
    INDEX Idx-Corte3                Id-Corte3   ASCENDING 
    INDEX Idx-Corte4                Id-Corte4   ASCENDING 
    INDEX Idx-CteFec                Id-Cliente  ASCENDING FecReg     ASCENDING  Id-Pedido  ASCENDING  Resto      ASCENDING 
    INDEX idx-cteint                Id-CteInt   ASCENDING FecReg     ASCENDING 
    INDEX idx-cteped                Id-Cliente  ASCENDING Id-Pedido  ASCENDING  Resto      ASCENDING 
    INDEX Idx-EstFec                Id-Estatus  ASCENDING FecReg     ASCENDING 
    INDEX Idx-FecCte                FecReg      ASCENDING Id-Cliente ASCENDING  Id-Pedido  ASCENDING  Resto      ASCENDING 
    INDEX Idx-FecPed                FecReg      ASCENDING Id-Pedido  ASCENDING  Resto      ASCENDING  Id-Cliente ASCENDING 
    INDEX Idx-FecREg                FecReg      ASCENDING 
    INDEX Idx-FiCaClPeRe            EnFirme     ASCENDING Cancelado  ASCENDING  Id-Cliente ASCENDING  Id-Pedido  ASCENDING  Resto     ASCENDING 
    INDEX Idx-FiCaFePeRe            EnFirme     ASCENDING Cancelado  ASCENDING  FecReg     ASCENDING  Id-Pedido  ASCENDING  Resto     ASCENDING 
    INDEX Idx-FiCaPeRe              EnFirme     ASCENDING Cancelado  ASCENDING  Id-Pedido  ASCENDING  Resto      ASCENDING 
    INDEX idx-firme                 EnFirme     ASCENDING id-ubivta  ASCENDING  Id-Pedido  ASCENDING  Resto      ASCENDING 
    INDEX Idx-ForPed                Id-ForPed   ASCENDING 
    INDEX Idx-Fuente                Id-Fuente   ASCENDING FuenteSta  ASCENDING  Id-Pedido  ASCENDING  Resto      ASCENDING 
    INDEX Idx-Liga                  Id-Liga     ASCENDING 
    INDEX Idx-OrdComCte             Id-Vendedor ASCENDING OrdComCte  DESCENDING 
    INDEX idx-ped IS PRIMARY UNIQUE Id-Pedido   ASCENDING Resto      ASCENDING 
    INDEX Idx-PedDes                Id-Cliente  ASCENDING FecReg     DESCENDING Id-Pedido  DESCENDING Resto      DESCENDING 
    INDEX Idx-RecCte                RecCte      ASCENDING Id-Cliente ASCENDING 
    INDEX Idx-ReqCte                ReqCte      ASCENDING Id-Cliente ASCENDING 
    INDEX idx-vcp IS UNIQUE         Id-Vendedor ASCENDING Id-Cliente ASCENDING  Id-Pedido  ASCENDING  Resto      ASCENDING 
    INDEX Idx-VeFiCaClPeRe          Id-Vendedor ASCENDING EnFirme    ASCENDING  Cancelado  ASCENDING  Id-Cliente ASCENDING  Id-Pedido ASCENDING Resto ASCENDING 
    INDEX Idx-VeFiCaPeRe            Id-Vendedor ASCENDING EnFirme    ASCENDING  Cancelado  ASCENDING  Id-Pedido  ASCENDING  Resto     ASCENDING 
    INDEX idx-vencte                Id-Vendedor ASCENDING Id-Cliente ASCENDING 
    INDEX Idx-VenFec                Id-Vendedor ASCENDING FecReg     ASCENDING  Id-Pedido  ASCENDING  Resto      ASCENDING 
    INDEX idx-venped                Id-Vendedor ASCENDING Id-Pedido  ASCENDING  Resto      ASCENDING 
    INDEX idx-vitfecped             vitrina     ASCENDING FecReg     ASCENDING  Id-Pedido  ASCENDING . 

DEFINE TEMP-TABLE ttDetPedido BEFORE-TABLE bttDetPedido
    FIELD Id-Pedido           AS CHARACTER LABEL "Pedido"
    FIELD Id-Articulo         AS CHARACTER LABEL "Codigo"
    FIELD Id-color            AS INTEGER   INITIAL "0" LABEL "Color"
    FIELD Id-pres             AS INTEGER   INITIAL "0" LABEL "Id.Pres"
    FIELD Descr               AS CHARACTER LABEL "Descr. Art"
    FIELD CantPed             AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD CantSur             AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD CantEnt             AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD CantFac             AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD Tipo                AS INTEGER   INITIAL "0" LABEL "?"
    FIELD PUnit               AS DECIMAL   INITIAL "0" LABEL "Precio"
    FIELD Descto              AS INTEGER   INITIAL "0" LABEL "?"
    FIELD PorcIVA             AS DECIMAL   INITIAL "0" LABEL "IVA"
    FIELD Iva                 AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD FecReg              AS DATE      INITIAL "?" LABEL "FecReg"
    FIELD Id-Seq              AS INTEGER   INITIAL "0" LABEL "Seq."
    FIELD Reng                AS INTEGER   INITIAL "0" LABEL "?"
    FIELD CantCom             AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD Id-Alm              AS CHARACTER LABEL "Almacen"
    FIELD FecAct              AS DATE      INITIAL "?" LABEL "?"
    FIELD hora                AS INTEGER   INITIAL "0" LABEL "?"
    FIELD Impreso             AS LOGICAL   INITIAL "no" LABEL "Impreso"
    FIELD Equiv               AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD Id-Loc              AS CHARACTER LABEL "Loc"
    FIELD Origen              AS INTEGER   INITIAL "0" LABEL "?"
    FIELD IndicaPre           AS CHARACTER LABEL "?"
    FIELD Importe             AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD Costo               AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD IdMoneda            AS INTEGER   INITIAL "0" LABEL "?"
    FIELD Iniciales           AS CHARACTER LABEL "Ini."
    FIELD PCosto              AS DECIMAL   INITIAL "0" LABEL "?"  /*  lsl 13/Jun/19 */
    FIELD Resto               AS INTEGER   INITIAL "0" LABEL "?"
    FIELD Evento              AS INTEGER   INITIAL "0" LABEL "?"
    FIELD Corte               AS LOGICAL   INITIAL "no" LABEL "Co"
    FIELD Filler-1            AS CHARACTER LABEL "?"
    FIELD TpoCorte            AS INTEGER   INITIAL "0" LABEL "?"
    FIELD NoPP                AS LOGICAL   INITIAL "no" LABEL "?"
    FIELD Id-Prov             AS INTEGER   INITIAL "0" LABEL "Prov"
    FIELD Id-Cot              AS CHARACTER LABEL "Coti."
    FIELD CantRes             AS DECIMAL   INITIAL "0" LABEL "Cant Reservada"
    FIELD CantBO              AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD secuencia           AS INTEGER   INITIAL "0" LABEL "?"
    FIELD FecCanc             AS DATE      INITIAL "?" LABEL "Fecha Canc"
    FIELD UsuarioCanc         AS CHARACTER LABEL "Usuario Canc"
    FIELD MotivoCanc          AS CHARACTER LABEL "Motivo"
    FIELD ExistAlta           AS DECIMAL   INITIAL "0" LABEL "?"
    FIELD Cajas               AS CHARACTER LABEL "?"
    FIELD RengImp             AS INTEGER   INITIAL "0" LABEL "?"
    FIELD IdPedido            AS CHARACTER LABEL "Coti."
    FIELD IdArticulo          AS CHARACTER LABEL "Codigo"
    FIELD IdColor             AS INTEGER   INITIAL "0" LABEL "Color"
    FIELD IdPres              AS INTEGER   INITIAL "0" LABEL "Id.Pres"
    FIELD IdSeq               AS INTEGER   INITIAL "0" LABEL "Seq."
    FIELD Pres                AS CHARACTER LABEL "Pres."
    FIELD sColor              AS CHARACTER LABEL "COLOR"
    FIELD IdAlm               AS CHARACTER LABEL "Almacen"
    FIELD IdLoc               AS CHARACTER LABEL "Loc"
    FIELD Filler1             AS CHARACTER LABEL "?"
    FIELD IdProv              AS INTEGER   INITIAL "0" LABEL "Prov"
    FIELD IdCot               AS CHARACTER LABEL "Coti."
    FIELD SobrepasaExistencia AS LOGICAL   LABEL "SobrepasaExistencia"          /* RNPC 20210716 */
    FIELD HayInventario       AS LOGICAL   LABEL "HayInventario"                    /* RNPC 20210804 */
    FIELD Mensaje             AS CHARACTER INITIAL "" LABEL "Mensaje"

    INDEX Idx-Articulo       Id-Articulo ASCENDING Id-color  ASCENDING Id-Alm      ASCENDING Id-Pedido ASCENDING Resto   ASCENDING 
    INDEX Idx-CantPed        CantPed     ASCENDING Id-Pedido ASCENDING Resto       ASCENDING 
    INDEX idx-det IS PRIMARY Id-Pedido   ASCENDING Resto     ASCENDING Id-Articulo ASCENDING Id-color  ASCENDING Id-pres ASCENDING Id-Seq ASCENDING 
    INDEX idx-det2 IS UNIQUE IdPedido    ASCENDING Resto     ASCENDING IdArticulo  ASCENDING IdColor   ASCENDING IdPres  ASCENDING IdSeq  ASCENDING
    INDEX idx-estvta         Id-Articulo ASCENDING Id-Pedido ASCENDING Resto       ASCENDING 
    INDEX Idx-FecProv        FecReg      ASCENDING Id-Prov   ASCENDING Id-Articulo ASCENDING Id-color  ASCENDING 
    INDEX idx-loc            Id-Pedido   ASCENDING Resto     ASCENDING Id-Alm      ASCENDING Id-Loc    ASCENDING Reng    ASCENDING 
    INDEX idx-loctiprestped  Id-Loc      ASCENDING Tipo      ASCENDING Resto       ASCENDING Id-Pedido ASCENDING 
    INDEX Idx-PedLoc         Id-Pedido   ASCENDING Resto     ASCENDING Id-Loc      ASCENDING 
    INDEX Idx-PedSec         Id-Pedido   ASCENDING Resto     ASCENDING secuencia   ASCENDING Reng      ASCENDING 
    INDEX Idx-Pres           Id-Articulo ASCENDING Id-pres   ASCENDING 
    INDEX idx-reng           Id-Pedido   ASCENDING Resto     ASCENDING Reng        ASCENDING .

DEFINE DATASET dsPedido2 FOR ttPedido, ttDetPedido DATA-RELATION Pedidos FOR ttPedido, ttDetPedido RELATION-FIELDS (id-pedido, id-pedido).

DEFINE TEMP-TABLE ttArtAlmacen NO-UNDO
    FIELD id-Almacen LIKE Almacen.Id-Alm
    FIELD id         AS INTEGER
    FIELD existencia AS INTEGER
    FIELD cantPed    LIKE DetPedido.CantPed
    FIELD orden      AS INTEGER
    FIELD genreq     AS LOGICAL
    INDEX idxArtalm id-almacen DESC id DESC.

DEFINE TEMP-TABLE ttArticulos NO-UNDO
    FIELD id          AS INTEGER
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
    FIELD cambiaPres  AS LOGICAL
    INDEX idxArticulos id DESC id-articulo DESC.

DEFINE BUFFER bfCliente2 FOR Cliente.
DEFINE VARIABLE l-Cliente   LIKE Pedido.Id-Cliente NO-UNDO.
DEFINE VARIABLE l-Contenido AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-OK        AS LOGICAL   NO-UNDO.
/* ***************************  Main Block  *************************** */

FIND FIRST SysGeneral NO-LOCK NO-ERROR.
IF AVAILABLE SysGeneral THEN ASSIGN g-nomcia = SysGeneral.Empresa.

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE leePedidos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER Id-Pedido   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER Resto       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER Id-Vendedor AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER Id-Cliente  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER RFC         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ctFecRegIni AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ctFecRegFin AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER Id-Estatus  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ReqCte      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER Direccion   AS CHARACTER NO-UNDO. /* "+" o "-" */
    DEFINE OUTPUT PARAMETER TABLE FOR ttPedido.  

    DEFINE VARIABLE iNumPedido   AS INTEGER       NO-UNDO.
    DEFINE VARIABLE iNumResto    AS INTEGER       NO-UNDO.
    DEFINE VARIABLE iNumVendedor AS INTEGER       NO-UNDO.
    DEFINE VARIABLE iNumCliente  AS INTEGER       NO-UNDO.
    DEFINE VARIABLE dtFecRegIni  AS DATE          NO-UNDO.
    DEFINE VARIABLE dtFecRegFin  AS DATE          NO-UNDO.
    DEFINE VARIABLE iNumEstatus  AS INTEGER       NO-UNDO.
    DEFINE VARIABLE sReqCte      AS CHARACTER     NO-UNDO.    
    DEFINE VARIABLE qh           AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE sQuery       AS CHARACTER     NO-UNDO.
  
    iNumPedido = INTEGER(Id-Pedido) NO-ERROR.
    MESSAGE "iNumPedido: " iNumPedido.
    IF ERROR-STATUS:ERROR THEN UNDO, THROW NEW Progress.Lang.AppError("N�mero de pedido invalido.").  
  
    iNumResto = INTEGER(Resto) NO-ERROR.
    MESSAGE "DEBUG: " iNumResto.
    IF ERROR-STATUS:ERROR THEN UNDO, THROW NEW Progress.Lang.AppError("N�mero de resto invalido.").
  
    iNumVendedor = INTEGER(Id-Vendedor) NO-ERROR.
    MESSAGE "iNumVendedor: " iNumVendedor.
    IF ERROR-STATUS:ERROR THEN UNDO, THROW NEW Progress.Lang.AppError("N�mero de vendedor invalido.").
    
    iNumCliente = INTEGER(Id-Cliente) NO-ERROR.
    MESSAGE "iNumCliente: " iNumCliente.
    IF ERROR-STATUS:ERROR THEN UNDO, THROW NEW Progress.Lang.AppError("N�mero de cliente invalido.").
  
    dtFecRegIni = DATE(ctFecRegIni) NO-ERROR.
    MESSAGE "dtFecRegIni: " dtFecRegIni.
    IF ERROR-STATUS:ERROR THEN UNDO, THROW NEW Progress.Lang.AppError("Fecha inicial invalida.").
  
    dtFecRegFin = DATE(ctFecRegFin) NO-ERROR.
    MESSAGE "dtFecRegFin: " dtFecRegFin.
    IF ERROR-STATUS:ERROR THEN UNDO, THROW NEW Progress.Lang.AppError("Fecha final invalida.").

    iNumEstatus = INTEGER(Id-Estatus) NO-ERROR.
    MESSAGE "iNumEstatus: " iNumEstatus.
    IF ERROR-STATUS:ERROR THEN UNDO, THROW NEW Progress.Lang.AppError("N�mero de estatus invalido.").
  
    sReqCte = TRIM(ReqCte).
    MESSAGE "sReqCte: " sReqCte.
    
    MESSAGE "RFC: " RFC.
    
    IF Direccion = ? THEN Direccion = "".
    /* Definiciones previas aquí... */
    DEFINE VARIABLE cDireccion AS CHARACTER NO-UNDO.
   
    cDireccion    = TRIM(Direccion).
    MESSAGE ">> Dirección recibida directamente: [" + Direccion + "]" VIEW-AS ALERT-BOX INFO.
   
    MESSAGE "Direccion recibida: " cDireccion VIEW-AS ALERT-BOX INFO.
   
    ASSIGN 
        sQuery = "FOR EACH Pedido WHERE ".

    IF iNumPedido > 0 THEN 
    DO:
        IF cDireccion = "mas" THEN 
        DO:
            /* Para 'más' (mayor que), orden ascendente */
            sQuery = "FOR EACH pedido NO-LOCK " +
                "WHERE pedido.id-pedido > '" + Id-Pedido + "' " +
                "BY pedido.id-pedido: ".
        END.
        ELSE IF cDireccion = "menos" THEN 
            DO:
                /* Para 'menos' (menor que), orden descendente */
                sQuery = "FOR EACH pedido NO-LOCK " +
                    "WHERE pedido.id-pedido < '" + Id-Pedido + "' " +
                    "BY pedido.id-pedido DESCENDING: ".
            END.
            ELSE 
            DO:
                /* Igual, sólo el pedido exacto */
                sQuery = "FOR EACH pedido NO-LOCK " +
                    "WHERE pedido.id-pedido = '" + Id-Pedido + "': ".
            END.
    END.  
     
    /* JASS29072025  
   IF iNumResto   <> ? THEN ASSIGN sQuery = sQuery + (IF iNumPedido > 0 THEN ' AND ' ELSE ' ') + 'Pedido.Resto = "' + Resto  + '"'.
   IF iNumVendedor > 0 THEN ASSIGN sQuery = sQuery + (IF iNumPedido > 0 OR iNumResto    > 0 THEN ' AND ' ELSE ' ') + 'Pedido.Id-Vendedor = "' + Id-Vendedor + '"'.
   IF iNumCliente  > 0 THEN ASSIGN sQuery = sQuery + (IF iNumPedido > 0 OR iNumResto    > 0 OR iNumVendedor > 0 THEN ' AND ' ELSE ' ') + 'Pedido.Id-Cliente = ' + Id-Cliente.
   IF dtFecRegIni <> ? THEN ASSIGN sQuery = sQuery + (IF iNumPedido > 0 OR iNumResto    > 0 OR iNumVendedor > 0 OR iNumCliente  > 0 THEN ' AND ' ELSE ' ') + 'Pedido.FecReg >= DATE(' + ctFecRegIni + ')'.  
   IF dtFecRegFin <> ? THEN ASSIGN sQuery = sQuery + (IF iNumPedido > 0 OR iNumResto    > 0 OR iNumVendedor > 0 OR iNumCliente  > 0 OR dtFecRegIni <> ? THEN ' AND ' ELSE ' ') + 'Pedido.FecReg <= DATE(' + ctFecRegFin + ')'.
   */
    /*IF Id-Estatus  <> ? THEN DO:  // RNPC 2020-03-31
      IF iNumEstatus = -99 THEN 
          ASSIGN sQuery = sQuery + (IF iNumPedido > 0 OR iNumResto    > 0 OR iNumVendedor > 0 OR iNumCliente  > 0 OR dtFecRegIni <> ? OR dtFecRegFin <> ? THEN ' AND ' ELSE ' ') + 'LOOKUP(STRING(Pedido.Id-Estatus),"-2,-1") > 0 '.
      ELSE IF iNumEstatus = 99 THEN
          ASSIGN sQuery = sQuery + (IF iNumPedido > 0 OR iNumResto    > 0 OR iNumVendedor > 0 OR iNumCliente  > 0 OR dtFecRegIni <> ? OR dtFecRegFin <> ? THEN ' AND ' ELSE ' ') + 'LOOKUP(STRING(Pedido.Id-Estatus),"-2,-1") = 0 '.
      ELSE 
          ASSIGN sQuery = sQuery + (IF iNumPedido > 0 OR iNumResto    > 0 OR iNumVendedor > 0 OR iNumCliente  > 0 OR dtFecRegIni <> ? OR dtFecRegFin <> ? THEN ' AND ' ELSE ' ') + 'Pedido.Id-Estatus = ' + Id-Estatus.
    END.*/
    /*
     IF RFC   <> ? THEN ASSIGN sQuery = sQuery + (IF iNumPedido > 0 OR iNumResto    > 0 OR iNumVendedor > 0 OR iNumCliente  > 0 OR dtFecRegIni <> ? OR dtFecRegFin <> ? THEN ' AND ' ELSE ' ') + 'Pedido.RFC MATCHES "*' + RFC + '*"'.
     IF LENGTH(sReqCte) > 0 THEN ASSIGN sQuery = sQuery + (IF iNumPedido > 0 OR iNumResto    > 0 OR iNumVendedor > 0 OR iNumCliente  > 0 OR dtFecRegIni <> ? OR dtFecRegFin <> ? OR RFC <> ? THEN ' AND ' ELSE ' ') + 'Pedido.ReqCte = "' + sReqCte + '"'.    
     */ 
    ASSIGN 
        sQuery = sQuery + ' NO-LOCK:'.     
    
    MESSAGE "sQuery construido:" SKIP sQuery VIEW-AS ALERT-BOX INFO BUTTONS OK.
                      
    EMPTY TEMP-TABLE ttPedido.
  
    CREATE QUERY qh.
    qh:SET-BUFFERS(BUFFER Pedido:HANDLE).
    qh:QUERY-PREPARE(sQuery).
    qh:QUERY-OPEN.
    REPEAT:
        qh:GET-NEXT().
        IF qh:QUERY-OFF-END THEN 
        DO:
            IF LENGTH(Id-Pedido) = 7 THEN 
            DO:
                FIND FIRST ttPedido NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttPedido THEN 
                DO:
                    FIND LAST LogBaja WHERE LogBaja.Id-Pedido = Id-Pedido NO-LOCK NO-ERROR.
                    IF AVAILABLE LogBaja THEN 
                    DO:
                        qh:QUERY-CLOSE().
                        DELETE OBJECT qh.
                      
                        MESSAGE "Pedido Cancelado: FECHA: " STRING(LogBaja.FecBaja) " HORA: " STRING(LogBaja.Hora,"HH:MM:SS") " AUT: " LogBaja.Id-User.
                        UNDO, THROW NEW Progress.Lang.AppError("Pedido Cancelado: FECHA: " + STRING(LogBaja.FecBaja) + " HORA: " + STRING(LogBaja.Hora,"HH:MM:SS") + " AUT: " +  LogBaja.Id-User).
                    END.
                    ELSE 
                    DO:
                        qh:QUERY-CLOSE().
                        DELETE OBJECT qh.
                      
                        MESSAGE "No existe pedido con ese folio: " Id-Pedido.
                        UNDO, THROW NEW Progress.Lang.AppError("No existe pedido con ese folio").
                    END.          
                END.
            END.
            ELSE 
            DO:
                IF LENGTH(Id-Pedido) > 0 THEN 
                DO:            
                    MESSAGE "No existe pedido con ese folio: " Id-Pedido.
                    UNDO, THROW NEW Progress.Lang.AppError("No existe pedido con ese folio").
                END.          
            END.            
            LEAVE.
        END.    
        IF LENGTH(Pedido.Id-Pedido) = 7 THEN RUN Llenado.
        LEAVE. /* <- Esto rompe el REPEAT después de llenar el primero */ 
    END.

    qh:QUERY-CLOSE().
    DELETE OBJECT qh.  
  
    IF Id-Estatus  <> ? AND iNumEstatus = -99 THEN 
    DO:    // RNPC 2020-04-03
        FOR EACH ttPedido WHERE ttPedido.IdEstatus <> -99 EXCLUSIVE-LOCK:
            DELETE ttPedido.
        END.
        RELEASE ttPedido.
    END.
END PROCEDURE.

PROCEDURE Llenado:
    /*------------------------------------------------------------------------------
     Purpose: Se llama al leer pedidos
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iPartDetPed AS INTEGER NO-UNDO.
    DEFINE VARIABLE dImporte    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dIva        AS DECIMAL NO-UNDO.
    FIND Moneda WHERE Moneda.Id-Moneda = Pedido.Id-Moneda NO-LOCK NO-ERROR.
    BUFFER-COPY Pedido TO ttPedido.
  
    IF SUBSTRING(Pedido.Id-Liga,1,1) = "F" THEN ttPedido.SolCancela = FALSE.
    
    FIND FIRST RutaEmb WHERE RutaEmb.Id-RutaEmb = Pedido.Id-RutaEmb NO-LOCK NO-ERROR.
    IF AVAILABLE RutaEmb THEN ASSIGN ttPedido.RutaEmb = RutaEmb.Descr.
    
    FIND CondVta WHERE CondVta.Id-Cond = Pedido.Id-Cond NO-LOCK NO-ERROR.
    IF AVAILABLE CondVta THEN ASSIGN ttPedido.Condicion = CondVta.Descr.                      
    
    FIND Entrega OF Pedido NO-LOCK NO-ERROR.
    IF AVAILABLE Entrega THEN ASSIGN ttPedido.Entrega = Entrega.Descr.
    
    FIND Transporte OF Pedido NO-LOCK NO-ERROR.
    IF AVAILABLE Transporte THEN ASSIGN ttPedido.Transporte = Transporte.Nombre.
        
    FIND Vendedor OF Pedido NO-LOCK NO-ERROR.
    IF AVAILABLE Vendedor THEN 
    DO:
        FIND Empleado WHERE Empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
        IF AVAILABLE Empleado THEN ASSIGN ttPedido.VendNombre = Empleado.Nombre.
    END.
    
    FIND FIRST EstPedido WHERE EstPedido.Id-Factura = Pedido.Id-Factura NO-LOCK NO-ERROR.
    IF AVAILABLE EstPedido THEN ASSIGN ttPedido.FleteYes = EstPedido.FleteYes.
  
    FIND FIRST RFiscal WHERE RFiscal.Id-RFiscal = Pedido.Id-RFiscal NO-LOCK NO-ERROR. 
    IF AVAILABLE RFiscal THEN ASSIGN ttPedido.DescrRFiscal = RFiscal.Descr.
        
    ASSIGN 
        iPartDetPed = 0  
        dImporte    = 0  
        dIva        = 0.
    FOR EACH DetPedido WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido NO-LOCK:
        IF DetPedido.Tipo >= 1 AND DetPedido.Tipo <= 2 THEN 
            ASSIGN iPartDetPed = iPartDetPed + 1
                dImporte    = dImporte    + DetPedido.Importe
                dIva        = dIva        + DetPedido.Iva.
        IF DetPedido.Tipo = 3 OR DetPedido.Tipo = 4 THEN ttPedido.SolCancela = FALSE.
    END.
  
    IF Pedido.Enfirme = FALSE OR Pedido.Id-Estatus > 5 THEN ttPedido.SolCancela = FALSE.  // "Pedido NO autorizado o ya embarcado."
    IF Pedido.SolCancPed THEN ttPedido.SolCancela = FALSE.
    IF AVAILABLE Moneda AND Moneda.Id-Moneda <> 1 THEN
        ASSIGN ttPedido.Simbolo = Moneda.Simbolo.
    ASSIGN 
        ttPedido.PartDetPed     = iPartDetPed
        ttPedido.IdPedido       = Pedido.Id-Pedido
        ttPedido.IdCot          = Pedido.Id-Cot
        ttPedido.IdVendedor     = Pedido.Id-Vendedor
        ttPedido.IdCliente      = Pedido.Id-Cliente
        ttPedido.IdFactura      = Pedido.Id-Factura
        ttPedido.IdRemision     = Pedido.Id-Remision
        ttPedido.IdEstatus      = Pedido.Id-Estatus
        ttPedido.sHFinAut       = STRING(Pedido.HFinAut,"HH:MM:SS")
        ttPedido.sHorPag        = STRING(Pedido.HorPag,"HH:MM:SS")
        ttPedido.sHorChP        = STRING(Pedido.HorChP,"HH:MM:SS")
        ttPedido.sHorSolCancPed = STRING(Pedido.HorSolCancPed,"HH:MM:SS")
        ttPedido.Iva            = dIva
        ttPedido.Total          = Pedido.SubTotal + dIva
        ttPedido.IdBancoChP     = Pedido.Id-BancoChP.         
         
    ASSIGN 
        ttPedido.IdCond    = Pedido.Id-Cond             
        ttPedido.IdCaptura = Pedido.Id-Captura 
        ttPedido.idUbivta  = Pedido.id-Ubivta 
        ttPedido.IdCiudad  = Pedido.Id-Ciudad 
        ttPedido.IdCiudad1 = Pedido.Id-Ciudad1 
        ttPedido.IdFlete   = Pedido.Id-Flete 
        ttPedido.IdTran    = Pedido.Id-Tran 
        ttPedido.IdEntrega = Pedido.Id-Entrega 
        ttPedido.IdForPed  = Pedido.Id-ForPed 
        ttPedido.IdFuente  = Pedido.Id-Fuente 
        ttPedido.IdRutaEmb = Pedido.Id-RutaEmb 
        ttPedido.IdLiga    = Pedido.Id-Liga 
        ttPedido.IdAlm     = Pedido.Id-Alm 
        ttPedido.IdCteInt  = Pedido.Id-CteInt 
        ttPedido.email     = Pedido.e-mail 
        ttPedido.email2    = Pedido.e-mail2
        ttPedido.Filler1   = Pedido.Filler-1
        ttPedido.IdRFiscal = Pedido.Id-RFiscal.
                      
    FIND EstVta WHERE EstVta.Id-Estatus = Pedido.Id-Estatus NO-LOCK NO-ERROR.
    IF AVAILABLE EstVta THEN ASSIGN ttPedido.Estatus = EstVta.Descr.
  
  // RNPC - 2020-04-02 - Ajustes para homologar info. de pedido seg�n vtac1300.p
    FOR EACH DetPedido  WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido 
        AND DetPedido.Resto = pedido.resto
        NO-LOCK BREAK BY reng:
        IF DetPedido.Tipo = 1 AND DetPedido.CantEnt < DetPedido.CantPed THEN 
            ACCUMULATE DetPedido.Id-Articulo (COUNT).
        ELSE NEXT.
    END.
  
    IF Pedido.Id-Cond = 0 THEN 
    DO:
        IF Pedido.Pagado THEN ASSIGN ttPedido.pago = "PAGADO".
        ELSE 
        DO:
            IF Pedido.PagAnt THEN ASSIGN ttPedido.pago = "PDTE. PAGO".
            ELSE ASSIGN ttPedido.pago = "CONTRA ENTREGA".
            IF Pedido.FEFormaPago = '02' AND Pedido.NumChP = "" THEN ASSIGN ttPedido.pago = "PDTE. CHEQUE".  
        END. 
    END.
    ELSE 
    DO:
        ASSIGN 
            ttPedido.pago = "CREDITO".
    
    // RNPC 2020-10-19 - Ajuste para obtener el DescPP1 y DescPP2 si es CONTADO obtener dato de la cotizaci�n, si es CREDITO del cliente
        FIND FIRST adosa.Cliente WHERE adosa.Cliente.Id-Cliente = ttPedido.IdCliente NO-LOCK NO-ERROR.
        IF AVAILABLE adosa.Cliente THEN 
            ASSIGN ttPedido.DescPP1 = adosa.Cliente.DescPP1
                ttPedido.DescPP2 = adosa.Cliente.DescPP2.
    END.
  
    /* ---------------- Separaci�n de Estatus vtac1300.p (Pedidos pendientes) ---------------- */
    IF NOT Pedido.enFirme THEN ASSIGN ttPedido.Estatus   = "No-Aut"
            ttPedido.IdEstatus = -99. // 1) Pedido NO Autorizado
    ELSE 
    DO:
        IF ((Pedido.Id-Estatus = -1 OR (Pedido.Id-Estatus = 4 AND ((ACCUM COUNT DetPedido.Id-Articulo) > 0) AND Pedido.bckOrd >= 3))
            OR (Pedido.Id-Estatus = 5 AND Pedido.adelantado AND (ACCUM COUNT DetPedido.Id-Articulo) > 0)) THEN 
            ASSIGN ttPedido.Estatus   = "Mcia-Pdte"
                ttPedido.IdEstatus = -99. // 2) Pedidos con Mercanc�a Pendiente
        ELSE IF (Pedido.Id-Estatus >= 0 AND Pedido.Id-Estatus < 4) THEN 
                ASSIGN ttPedido.Estatus   = "Mcia-P/Surt"
                    ttPedido.IdEstatus = -99. // 3) Pedidos con Mercanc�a Por Surtir
            ELSE IF (Pedido.Id-Estatus = 4 AND ((Pedido.bckOrd < 3) OR ((ACCUM COUNT DetPedido.Id-Articulo) = 0 AND Pedido.bckOrd >= 3))) THEN 
                    ASSIGN ttPedido.Estatus   = "P/Facturar"
                        ttPedido.IdEstatus = -99. // 4) Pedidos por Facturar
                ELSE IF (Pedido.Id-Estatus = 5 OR Pedido.Id-Estatus = -2) AND (Pedido.Id-Entrega = 16 OR Pedido.Id-Entrega = 3) THEN 
                        ASSIGN ttPedido.Estatus   = "Cte-Recoge"
                            ttPedido.IdEstatus = -99. // 5) Pedidos cliente pasa a recoger
                    ELSE IF (Pedido.Id-Estatus = 5 OR Pedido.Id-Estatus = -2) AND Pedido.Id-Entrega = 11 THEN 
                            ASSIGN ttPedido.Estatus   = "Emb-Local"
                                ttPedido.IdEstatus = -99. // 6) Pedidos por Embarcar Local
                        ELSE IF (Pedido.Id-Estatus = 5 OR Pedido.Id-Estatus = -2) AND Pedido.Id-Entrega = 17 AND Pedido.id-tran = 38 THEN
                                ASSIGN ttPedido.Estatus   = "Emb-Estafeta"
                                    ttPedido.IdEstatus = -99. // 7) Pedidos por Embarcar Estafeta
                            ELSE IF ((Pedido.Id-Estatus = 5 OR Pedido.Id-Estatus = -2) AND LOOKUP(STRING(Pedido.Id-Entrega), "16,11,3") = 0)   
                                    AND (Pedido.Id-Entrega <> 17 OR (Pedido.Id-Entrega = 17 AND Pedido.id-tran <> 38)) THEN 
                                    ASSIGN ttPedido.Estatus   = "Emb-Foraneo"
                                        ttPedido.IdEstatus = -99. // 8) Pedidos por Embarcar Foraneo
    END.
  // -------------------------------------------------------------------------
         
    FIND Ciudad WHERE Ciudad.Id-Ciudad = Pedido.Id-ciudad NO-LOCK NO-ERROR.
    IF AVAILABLE Ciudad THEN 
    DO: 
        ASSIGN 
            ttPedido.IdCiudad = ttPedido.Id-Ciudad. 
        ttPedido.Ciudad   = Ciudad.Nombre.
           
        FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
        IF AVAILABLE Estado THEN 
        DO:
            ASSIGN 
                ttPedido.Estado = Estado.Nombre.
            FIND Pais WHERE Pais.Id-Pais = Estado.Id-Pais NO-LOCK NO-ERROR.
            IF AVAILABLE Pais THEN ASSIGN ttPedido.Pais = Pais.Nombre.        
        END.
    END.
  
    FIND Empleado WHERE Empleado.Iniciales = Pedido.AutPor NO-LOCK NO-ERROR.
    IF AVAILABLE Empleado THEN ASSIGN ttPedido.AutPorNom = Empleado.Nombre.             
   
    FIND Empleado WHERE Empleado.Iniciales = Pedido.UsrPag NO-LOCK NO-ERROR.
    IF AVAILABLE Empleado THEN ASSIGN ttPedido.UsrPagNom = Empleado.Nombre.             

    RELEASE ttPedido.
    RETURN.
END PROCEDURE.

