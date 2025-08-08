
/*------------------------------------------------------------------------
    File        : intereses.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Tue Jan 28 11:50:25 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

   DEFINE  TEMP-TABLE ttInteres
           FIELD Cliente AS INT
           FIELD IdUser  AS CHAR
           FIELD Factura LIKE Factura.Id-Factura LABEL "Factura"
           FIELD Importe  AS DECI FORMAT "zz,zzz,zz9.99"          
     INDEX Idx-Sec IS PRIMARY Factura.   
     

  DEF TEMP-TABLE w-cliente NO-UNDO LIKE Cliente.
  

  DEFINE VAR l-reccaja AS RECID.
  DEFINE VAR l-remision LIKE Remision.Id-Remision  NO-UNDO.
  DEFINE VAR l-con     AS INT.
  DEFINE VAR l-oknco AS LOGICAL NO-UNDO.

  DEFINE BUFFER bfVFolio FOR VFolio.
  DEFINE BUFFER bfRemision FOR Remision.
/* **********************  Internal Procedures  *********************** */

PROCEDURE PostRemision:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : Cuentas por Cobrar
  Programa : cxca1051.p
  Funcion  : Genera Remision por Intereses cobrados por adelantado
  Autor    : LUIS
 */


  //{sia00000.var}
  {cxca0002.i}  

 
  DEFINE INPUT PARAMETER TABLE FOR ttInteres. 
  DEFINE OUTPUT PARAMETER Remision AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE lcUsuario     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE liCliente     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcCaja        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcEmpleado    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE liCajero      AS INTEGER   NO-UNDO.
    
    
   /* 1. Validar que todos los registros tengan el mismo cliente y usuario */
    FOR EACH ttInteres BREAK BY ttInteres.Cliente BY ttInteres.IdUser:
        IF FIRST(ttInteres.Cliente) THEN 
            liCliente = ttInteres.Cliente.
        ELSE IF ttInteres.Cliente <> liCliente THEN DO:
            MESSAGE "Error: Múltiples clientes en los registros" VIEW-AS ALERT-BOX.
            RETURN ERROR.
        END.

        IF FIRST(ttInteres.IdUser) THEN 
            lcUsuario = ttInteres.IdUser.
        ELSE IF ttInteres.IdUser <> lcUsuario THEN DO:
            MESSAGE "Error: Múltiples usuarios en los registros" VIEW-AS ALERT-BOX.
            RETURN ERROR.
        END.
    END.
    
   /* 2. Obtener datos del usuario y cajero */
    FIND Usuario WHERE Usuario.Id-User = lcUsuario NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN DO:
        MESSAGE "Usuario no encontrado: " + lcUsuario VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.

    FIND Cajero WHERE Cajero.Id-Cajero = Usuario.Id-Cajero NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cajero THEN DO:
        MESSAGE "Cajero no encontrado para usuario: " + lcUsuario VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.

    /* 3. Asignar valores a variables */
    ASSIGN 
        lcCaja     = Usuario.Id-Caja  
        liCajero   = Cajero.Id-Cajero
        lcEmpleado = Cajero.Iniciales.  
      
            FIND FIRST Cliente WHERE cliente.Id-cliente = liCliente NO-LOCK NO-ERROR.
            IF AVAILABLE cliente THEN 
            DO:
                 CREATE w-cliente. 
                 ASSIGN      
                 w-cliente.id-cliente   = cliente.id-cliente
                 w-cliente.razonsocial = cliente.razonsocial
                 w-cliente.rfc         = cliente.rfc
                 w-cliente.calleno     = cliente.calleno
                 w-cliente.colonia     = cliente.colonia
                 w-cliente.Tel1        = cliente.tel1
                 w-cliente.propietario = cliente.propietario
                 w-cliente.cp          = cliente.cp
                 w-cliente.id-ciudad   = cliente.id-ciudad
                 w-cliente.Id-RFiscal  = Cliente.Id-RFiscal. 
             END.
     
  
  FIND FIRST SysGeneral NO-LOCK NO-ERROR.
  ASSIGN l-oknco = FALSE.
  
  Ciclo:
  DO TRANSACTION :
   FOR EACH ttInteres NO-LOCK:
      ACCUMULATE (ttInteres.Importe) (TOTAL).
   END. /* del for each a ttInteres */

  FIND FIRST w-cliente NO-LOCK .
  IF (ACCUM TOTAL ttInteres.Importe) > 0 THEN DO:

     FIND Usuario WHERE Usuario.Id-User = ttInteres.IdUser NO-LOCK NO-ERROR.
     IF AVAILABLE Usuario AND Usuario.Id-Ubicacion BEGINS "12" THEN DO:
            FIND FIRST VFolio WHERE VFolio.Id-Doc = "Remision" 
                                AND VFolio.Id-alm = Usuario.Id-Ubicacion EXCLUSIVE-LOCK NO-ERROR.
     END.
     ELSE DO:
            FIND FIRST VFolio WHERE VFolio.Id-Doc = "Remision" 
                                AND VFolio.id-alm = '' EXCLUSIVE-LOCK NO-ERROR.
     END.
     FIND FIRST Vendedor WHERE Vendedor.Iniciales = lcEmpleado NO-LOCK NO-ERROR.
     CREATE Remision.
     ASSIGN Remision.Id-Remision = STRING(VFolio.Folio, '99999') + VFolio.Prefijo
            VFolio.Folio         = VFolio.Folio + 1
            Remision.Subtotal    = (ACCUM TOTAL (ttInteres.Importe))
            Remision.Tot         = (ACCUM TOTAL (ttInteres.Importe)) * 
                                            (1 + (SysGeneral.Porc-Iva / 100))
            Remision.Iva         = Remision.Tot - Remision.Subtotal
            Remision.RazonSocial = w-Cliente.RazonSocial
            Remision.CalleNo     = w-Cliente.CalleNo
            Remision.Id-Cliente  = w-Cliente.Id-Cliente
            Remision.Colonia     = w-Cliente.Colonia
            Remision.Tel1        = w-Cliente.Tel1
            Remision.FecReg      = TODAY
            Remision.Id-Ciudad   = w-Cliente.Id-Ciudad
            Remision.Propietario = w-Cliente.Propietario
            Remision.RFC         = w-Cliente.RFC
            Remision.Id-RFiscal  = w-Cliente.Id-RFiscal
            Remision.TipoVenta   = 2
            Remision.NCO         = TRUE
            Remision.UsuarioReg  = lcUsuario 
            Remision.Iniciales   = lcEmpleado
            Remision.Id-Vendedor = IF AVAILABLE Vendedor THEN
                                      Vendedor.Id-Vendedor ELSE ''
            Remision.TipoPrecio  = "M"
            Remision.Id-Ubic     = 'CYC'
            l-remision           = Remision.Id-Remision
            Remision.Id-Entrega  = 4
            Remision.Pagada      = TRUE
            Remision.VersionSAT  = "4.0".

     IF VFolio.Folio >= 99999 THEN DO:
         ASSIGN VFolio.Folio = 1.
                   
         IF SUBSTRING(VFolio.Prefijo,2,1) <> "Y" THEN DO: /* Se deja la letra "Z" para contingencias */
             ASSIGN VFolio.Prefijo = SUBSTRING(VFolio.Prefijo,1,1) + CHR(ASC(SUBSTRING(VFolio.Prefijo,2,1)) + 1).
         END.
         ELSE DO:
             BlkPrefijo:
             REPEAT:
                 ASSIGN VFolio.Prefijo = CHR(ASC(SUBSTRING(VFolio.Prefijo,1,1)) + 1) + "A".
                 FIND FIRST bfVFolio WHERE (bfVFolio.Id-Doc = "REMISION")
                                       AND SUBSTRING(bfVFolio.Prefijo,1,1) = SUBSTRING(VFolio.Prefijo,1,1) NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE bfVFolio THEN DO:
                     FIND FIRST bfRemision WHERE bfRemision.Id-Remis MATCHES ("*" + VFolio.Prefijo) NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE bfRemision THEN  
                         LEAVE BlkPrefijo.
                 END.
             END.
         END.                   
     END.
     
     {cxca0006.i   
          &Cliente = Remision.Id-Cliente
          &Importe = Remision.Tot
          &renglon = 5
          &fecha   = Remision.FecReg}
  END.   
  
  FIND FIRST sysgeneral NO-LOCK NO-ERROR.
  FOR EACH ttInteres ON ERROR UNDO ciclo, LEAVE ciclo
                       ON ENDKEY UNDO ciclo, LEAVE ciclo :

            CREATE DetRemis.
            ASSIGN DetRemis.Id-Remis = Remision.Id-Remis
                   DetRemis.Cant     = 1
                   DetRemis.Tipo     = 2
                   DetRemis.PrecUnit = ttInteres.Importe  
                   DetRemis.Importe  = DetRemis.PrecUnit
                   DetRemis.Descr    = "INTERESES DEL DOC. # " + ttInteres.Factura
                   DetRemis.PorcIva  = SysGeneral.Porc-Iva
                   DetRemis.Iva      = ttInteres.importe *
                                            (DetRemis.PorcIVA / 100)
                   DetRemis.Sec      = l-con
                   l-con = l-con + 1.
  END. /* del for each a chedev */
  
  /* crea renglon en la remision con el concepto de la NCO */
  CREATE DetRemis.
  ASSIGN DetRemis.Id-Remis = Remision.Id-Remis
         DetRemis.Cant     = 1
         DetRemis.Tipo     = 2
         DetRemis.PrecUnit = 0
         DetRemis.Importe  = 0
         DetRemis.Descr    = "COBRO DE INTERES"
         DetRemis.PorcIva  = 0
         DetRemis.Iva      = 0
         DetRemis.Sec      = l-con
         l-con = l-con + 1.


  ASSIGN l-con = 0.

  IF AVAILABLE Remision THEN DO:
     {cxca0007.i
          &Factura   = Remision.Id-Remision
          &TipoVenta = 2 }
  END.
  ASSIGN l-oknco = TRUE.
  END. /* Fin de DO TRANSACTION */ 
  RELEASE Ctlcaja.
  RELEASE Remision.
  RELEASE DetRemis.
  RELEASE Folio.
  RELEASE VFolio.
  RELEASE EstCte.
  RELEASE DistIVA.
  RELEASE MovCaja.
  IF l-oknco AND l-remision <> "" THEN DO:
      MESSAGE "La nota de cargo se registro con el folio : " + l-remision.
      PAUSE 2 NO-MESSAGE.
      
      /* Genera Remision Electronica */
      RUN vtac2070.p(INPUT l-remision, INPUT lcUsuario). /* Generacion de Factura 
                                                         Electronica de Contado */
      
      
    /*  RUN vtac2073.p(INPUT l-remision). */  /* Este proceso imprime la factura electronica se quita ya 
                                                que ya existe el proceso donde se visualiza desde .net */
      /* RUN vtac0310.p (INPUT l-remision). */  /* Este programa ya estaba asi comentado */
  END. 
END PROCEDURE.