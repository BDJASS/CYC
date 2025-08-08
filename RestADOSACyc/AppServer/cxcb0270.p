/*
    Programa    : cxcb0270.p
    Funcion     : Extrae los dias de cartera y promedio de pago de un cliente.
    Autor       : Ing. Fco. Helio Gutierrez Garza
    Fecha       : 12 de Marzo de 2005
*/


/*{sia00000.var}*/

DEF INPUT PARAMETER  l-cliini   LIKE Cliente.id-Cliente                NO-UNDO.
DEF INPUT PARAMETER  l-fecharep AS DATE FORMAT "99/99/9999"            NO-UNDO.

DEF OUTPUT PARAMETER l-diasmax  AS INTE             FORMAT "zz9"       NO-UNDO.
DEF OUTPUT PARAMETER l-prompago AS INTE FORMAT ">>9"                   NO-UNDO. 


DEF BUFFER bf-mov FOR MovCliente .

DEF VAR l-TasaNCO LIKE SysGeneral.TasaNCO INITIAL 0.
DEF VAR l-cheques       AS CHAR FORMAT "x(5)"                          NO-UNDO.

DEF VAR l-fechastring   AS CHAR FORMAT "x(30)"                         NO-UNDO.
DEF VAR l-aste          AS CHAR                                        NO-UNDO.
DEF VAR l-temporada        AS CHAR FORMAT "x"                          NO-UNDO.
DEF VAR l-indice        AS INTE                                        NO-UNDO.
DEF VAR l-dias          AS INTE LABEL "Dias"        FORMAT "zz9"       NO-UNDO.
DEF VAR l-diasnorm      AS INTE                     FORMAT "zz9"       NO-UNDO.
DEF VAR l-diasint       AS INTE                                        NO-UNDO.
DEF VAR l-largo         AS INTE                           INITIAL 20   NO-UNDO.
DEF VAR l-chedev        AS INTE                                        NO-UNDO.
DEF VAR l-chedev2       AS INTE                                        NO-UNDO.
DEF VAR l-pagina        AS INTE                                        NO-UNDO.
DEF VAR l-saldo         AS DECI FORMAT "z,zzz,zz9.99"                  NO-UNDO.
DEF VAR l-interes        AS DECI                                       NO-UNDO.
DEF VAR l-totchedev     AS DECI FORMAT "-zzzzzz,zz9.99"                NO-UNDO.
DEF VAR l-totncargo     AS DECI FORMAT "-zzzzzz,zz9.99"                NO-UNDO.
DEF VAR l-totfacturas   AS DECI FORMAT "-zzzzzz,zz9.99"                NO-UNDO.
DEF VAR l-totintereses  AS DECI FORMAT "-zzzzzz,zz9.99"                NO-UNDO.
DEF VAR l-totcargo      AS DECI FORMAT "-zzzzzz,zz9.99"                NO-UNDO.
DEF VAR l-totDevPend    AS DECI FORMAT "-zzzzzz,zz9.99"                NO-UNDO.
DEF VAR l-tot30         AS DECI FORMAT "-zzzzz9.99"                    NO-UNDO.
DEF VAR l-tot60         AS DECI FORMAT "-zzzzz9.99"                    NO-UNDO.
DEF VAR l-total         AS DECI FORMAT "-zzzzz9.99"                    NO-UNDO.
DEF VAR l-tot75         AS DECI FORMAT "-zzzzz9.99"                    NO-UNDO.
DEF VAR l-tot90         AS DECI FORMAT "-zzzzz9.99"                    NO-UNDO.
DEF VAR l-tot91         AS DECI FORMAT "-zzzz9.99"                     NO-UNDO.
DEF VAR l-tasa          AS DECI FORMAT "zz9.9999"                      NO-UNDO.
DEF VAR l-vencido       AS DECI LABEL  "Vencido"    FORMAT "->>>>>,>>9.99"
                                                                       NO-UNDO.
DEF VAR l-porvencido    AS DECI LABEL "Por Vencer"  FORMAT "->>>>>,>>9.99"
                                                                       NO-UNDO.
DEF VAR l-bytes         AS CHAR                                        NO-UNDO.
DEF VAR l-ctrl          AS INTE                                        NO-UNDO.
DEF VAR l-tam           AS INTE                                        NO-UNDO.
DEF VAR l-hubo          AS LOGI                                        NO-UNDO.
DEF VAR l-negativos     AS LOGI                                        NO-UNDO.
DEF VAR l-checausuario  AS LOGI                                        NO-UNDO.
DEF VAR l-aviso         AS DECI FORMAT "->>>,>>>,>>9.99"               NO-UNDO.
DEF VAR l-cliente     LIKE Cliente.Id-Cliente                          NO-UNDO.
/*DEF VAR l-sistema     LIKE g-sistema                                   NO-UNDO.*/
DEF VAR l-razonsocial LIKE Cliente.RazonSocial                         NO-UNDO.
DEF VAR l-calleNo     LIKE Cliente.CalleNo                             NO-UNDO.
DEF VAR l-colonia     LIKE Cliente.Colonia                             NO-UNDO.
DEF VAR l-cp          LIKE Cliente.CP                                  NO-UNDO.
DEF VAR l-tel1        LIKE Cliente.Tel1                                NO-UNDO.
DEF VAR l-Correo      LIKE Cliente.e-Mail                              NO-UNDO. 
DEF VAR l-limite      LIKE Cliente.Limite                              NO-UNDO.
DEF VAR l-plazo       LIKE Cliente.Plazo                               NO-UNDO.
DEF VAR l-calidad     LIKE Cliente.Id-Calidad                          NO-UNDO.
DEF VAR l-vendedor    LIKE Cliente.Id-Vendedor                         NO-UNDO.
DEF VAR l-resp        LIKE Cliente.Id-Resp                             NO-UNDO.
DEF VAR l-zona        LIKE Cliente.Id-Zona                             NO-UNDO.
DEF VAR l-giro        LIKE Cliente.Id-Giro                             NO-UNDO.
DEF VAR l-ramo        LIKE Cliente.Id-ramo                             NO-UNDO.
DEF VAR l-cobrador    LIKE Cliente.Id-Cobrador                         NO-UNDO.
DEF VAR l-propietario LIKE Cliente.Propietario                         NO-UNDO.
DEF VAR l-observaciones LIKE Cliente.Observaciones                     NO-UNDO.
DEF VAR l-otrasobserva   AS CHAR FORMAT "X(83)"                        NO-UNDO.
DEF VAR l-cobradorstring AS CHAR FORMAT "x(2)"                         NO-UNDO.
DEF VAR l-encacob       AS CHAR INITIAL "CO" FORMAT "X(2)"             NO-UNDO.
DEF VAR v-clave         AS CHARACTER                                   NO-UNDO.


/*{cxcc0023.i}                                                 */
/*{cxcc0023.i &Colonia = TRUE &Propietario = TRUE &Sec = ctodo}*/
/*{cxcc0023.i &Colonia = TRUE &Sec = ccol}                     */
/*{cxcc0023.i &Propietario = TRUE &Sec = scol}                 */


IF USERID("dictdb") = "RMGN" OR USERID("dictdb") = "RETH" OR USERID("dictdb") = "AVGO" THEN 
    ASSIGN l-checausuario = TRUE.
ELSE 
    ASSIGN l-checausuario = FALSE.  

/*
UPDATE 
    l-tasaNCO LABEL "Tasa de Interes"  go-on(f4 f1)
    WITH FRAME f-tasaNCO OVERLAY SIDE-LABEL ROW 3 CENTERED.
    
  */  

    
FOR EACH Cliente WHERE Cliente.Id-Cliente = l-cliini 
                       NO-LOCK:

    FIND ciudad OF Cliente NO-LOCK NO-ERROR.
   
    IF AVAILABLE ciudad THEN DO:
        FIND Estado OF Ciudad  NO-LOCK NO-ERROR.
        ASSIGN 
            l-cp = (Cliente.CP + "-" + Ciudad.Nombre + "," + Estado.NomCto).
    END.
       
    ASSIGN  
        l-cliente     = Cliente.Id-Cliente  
/*        l-sistema     = g-sistema*/
        l-razonsocial = Cliente.RazonSocial 
        l-calleNo     = Cliente.CalleNo
        l-colonia     = Cliente.Colonia
        l-tel1        = Cliente.Tel1        
        l-limite      = Cliente.Limite
        l-Correo      = (IF Cliente.e-Mail <> "" THEN Cliente.e-Mail ELSE Cliente.BuzonFiscal)
        l-plazo       = {cxcc0024.i} 
        l-calidad     = Cliente.Id-Calidad
        l-vendedor    = Cliente.Id-Vendedor 
        l-resp        = Cliente.Id-Resp
        l-zona        = Cliente.Id-Zona     
        l-giro        = Cliente.Id-Giro
        l-ramo        = Cliente.Id-ramo
        l-propietario = Cliente.Propietario
        l-observaciones = Cliente.Observaciones.
    
    FIND FIRST CteEmp WHERE CteEmp.Id-Cliente = Cliente.Id-Cliente AND
			                CteEmp.Principal = TRUE 
                            NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CteEmp THEN
	    FIND FIRST CteEmp WHERE CteEmp.Id-Cliente = Cliente.Id-Cliente 
                                NO-LOCK NO-ERROR.
        ASSIGN 
            l-otrasobserva = (IF AVAILABLE CteEmp THEN
				                (CteEmp.Nombre + '  Tel: ' + CteEmp.Tel1 + 
                                     ' e-mail: ' + CteEmp.e-mail) 
                              ELSE '').

    FIND Usuario WHERE Usuario.Id-User = USERID("dictdb") 
                       NO-LOCK NO-ERROR.
    IF l-checausuario THEN 
        ASSIGN 
            l-cobradorstring = ''
            l-encacob = ''.
    ELSE
        ASSIGN 
            l-cobrador    = Cliente.Id-Cobrador
            l-cobradorstring = STRING(Cliente.Id-Cobrador,"99")
            l-encacob = "CO".

    FOR EACH CheDev WHERE CheDev.Id-Cliente = Cliente.Id-Cliente AND
                          CheDev.FecCargo <= l-fecharep 
                          NO-LOCK:
        ACCUMULATE CheDev.FecCargo (COUNT).
    END.
   
    ASSIGN l-chedev = ACCUM COUNT CheDev.FecCargo.
   
    FOR EACH MovCliente WHERE MovCliente.Id-Cliente = Cliente.Id-Cliente AND
                              MovCliente.Id-MC     <= 3                  AND
                              MovCliente.FecReg <= l-Fecharep 
                              NO-LOCK
                              BREAK BY MovCliente.Id-Cliente
                                    BY MovCliente.Fecreg
                                    BY MovCliente.RefSaldo:

        FOR EACH bf-Mov WHERE bf-Mov.RefSaldo = MovCliente.RefSaldo AND
                              bf-Mov.Afectado = TRUE                AND
                              bf-Mov.FecReg  <= l-fecharep          AND
                              bf-Mov.Id-MC    > 3 NO-LOCK:
            
            FIND Acuse WHERE Acuse.Id-Acuse = bf-mov.Documento 
                             NO-LOCK NO-ERROR.
            IF {salt0005.i} = "MATRIZ" AND AVAILABLE Acuse THEN 
                IF Acuse.Estatus <> 4 THEN 
                    NEXT.
       
            ACCUMULATE bf-Mov.Importe (TOTAL).
        END.
     
        ASSIGN 
            l-saldo = MovCliente.Importe + ACCUM TOTAL bf-Mov.Importe.
        
        IF (l-saldo = 0.01 OR l-saldo = 0.01 ) AND 
           NOT LAST-OF(MovCliente.Id-Cliente) THEN 
            NEXT.

        IF NOT l-checausuario THEN
            IF l-saldo = 0 THEN 
                ASSIGN l-negativos = FALSE.
            ELSE 
                ASSIGN 
                    l-negativos = TRUE.
        ELSE 
            IF l-saldo <= 0 THEN 
                ASSIGN l-negativos = FALSE.
            ELSE 
                ASSIGN l-negativos = TRUE.

        IF l-Saldo <> 0.01 AND l-saldo <> -0.01 AND l-negativos THEN DO:
       
            ASSIGN 
                l-hubo = TRUE.
        
            FIND TabMC WHERE TabMC.Id-MC = MovCliente.Id-MC 
                             NO-LOCK NO-ERROR.
            ASSIGN 
                l-dias       = (l-fecharep - MovCliente.FecReg)
                l-diasnorm   = l-fecharep - MovCliente.FecVenc
                l-porvencido = (IF MovCliente.FecVenc >= l-fecharep THEN 
                                    l-saldo ELSE 0)
                l-vencido    = (IF MovCliente.FecVenc < l-fecharep THEN 
                                    l-saldo ELSE 0)
                l-total      = l-total + l-saldo
                l-cheques    = STRING(l-chedev) + "/" + STRING(l-chedev - l-chedev2).
            
            ACCUMULATE l-vencido (TOTAL).
            ACCUMULATE l-porvencido (TOTAL).
            ACCUMULATE l-saldo * l-dias (TOTAL).
       
            IF l-dias <= 30 THEN 
                ASSIGN l-tot30 = l-tot30 + l-saldo.
       
            IF l-dias >= 31 AND l-dias <= 60 THEN
                ASSIGN l-tot60 = l-tot60 + l-saldo.
       
            IF l-dias >= 61 AND l-dias <= 75 THEN
                ASSIGN l-tot75 = l-tot75 + l-saldo.
       
            IF l-dias >= 76 AND l-dias <= 90 THEN
                ASSIGN l-tot90 = l-tot90 + l-saldo.
       
            IF l-dias > 90 THEN 
                ASSIGN l-tot91 = l-tot91 + l-saldo.
       
            IF MovCliente.Id-MC = 3 THEN
                ASSIGN 
                    l-totchedev   = l-totchedev + l-saldo
                    l-chedev2     = l-chedev2 + (IF l-saldo > 0 THEN 1 ELSE 0).
            
            IF MovCliente.Id-MC = 2 THEN
                ASSIGN 
                    l-totncargo   = l-totncargo + l-saldo.
       
            FIND FIRST bf-Mov WHERE bf-mov.RefSaldo = MovCliente.REfSaldo AND 
                                    NOT bf-mov.Afectado 
                                    NO-LOCK NO-ERROR.
            IF AVAILABLE bf-mov THEN
                ASSIGN 
                    l-aste = "*".
            ELSE 
                ASSIGN 
                    l-aste = "".
       
            FOR EACH bf-Mov WHERE bf-mov.RefSaldo = MovCliente.REfSaldo AND
                                  bf-mov.Afectado = TRUE 
                                  NO-LOCK:
    	        FIND Acuse WHERE Acuse.Id-Acuse = bf-mov.Documento 
                                 NO-LOCK NO-ERROR.
    	  
                IF {salt0005.i} = "MATRIZ" AND AVAILABLE Acuse THEN 
    	            IF Acuse.Estatus <> 4 THEN 
                        ASSIGN 
                            l-aste = "&".
            END.
           
            IF Movcliente.Id-MC = 1 THEN DO:
                FIND Factura WHERE Factura.Id-Factura = Movcliente.RefSaldo 
                                   NO-LOCK NO-ERROR.
                
                IF AVAILABLE Factura THEN 
                    ASSIGN 
                        l-temporada = IF Factura.Id-Cond = 1 THEN "S" ELSE 
                                      IF Factura.Id-Cond = 4 THEN "E" ELSE 
                                      IF Factura.Id-Cond = 5 THEN "N" ELSE "".
            END.
            ELSE 
                l-temporada = "".
    
            v-clave = STRING(MovCliente.RefSaldo) + (IF AVAILABLE factura THEN
                                                        "/" + STRING(Factura.requisicion)
                                                     ELSE "").                        
    
    
            IF MovCliente.Id-MC = 1 THEN DO:
                
                FIND FIRST SysGeneral NO-LOCK NO-ERROR.
                ASSIGN 
                    l-totfacturas  = l-totfacturas + l-saldo
                    l-tasa         = l-TasaNCO / 100 / 30.42
                    l-diasint      = l-fecharep - (MovCliente.FecVenc)
                    l-totintereses = l-totintereses + (IF l-diasint > 0 THEN
                                                        l-saldo * l-tasa * l-diasint
                                                       ELSE 0)
                    l-interes      = l-interes + (IF l-diasint > 0 THEN 
                                                    (l-saldo * l-tasa * l-diasint) ELSE 0).
                
            END.
           
        END. /* si el saldo <> 0 e impresion de negativos */
    
        IF LAST-OF(MovCliente.Id-Cliente) THEN DO:
            
            FOR EACH AvisoCargo WHERE AvisoCargo.Id-Cliente = Cliente.Id-Cliente AND 
                                      AvisoCargo.FecCanc    = ?                  AND 
                        (AvisoCargo.Importe + AvisoCargo.IVA - AvisoCargo.Descuento - AvisoCargo.Pago) > 0
                                 AND  AvisoCargo.Cancelacion = 0
                                 AND  AvisoCargo.FecReg     <= l-fecharep 
                                      NO-LOCK :
                ASSIGN 
                    l-totcargo = l-totCargo + ((AvisoCargo.Importe + AvisoCargo.IVA)
                                     - AvisoCargo.descuento - AvisoCargo.Pago)
                    l-aviso = (AvisoCargo.Importe + AvisoCargo.IVA - AvisoCargo.Descuento - AvisoCargo.Pago)
                    l-dias  = l-fecharep - AvisoCargo.FecReg.
                  
                  
                IF (l-dias) > 0 THEN
                    ACCUMULATE l-aviso (TOTAL).
                
                  
                IF (l-dias <= 30) THEN
                    ASSIGN 
                        l-tot30 = l-tot30 + l-aviso.
                IF (l-dias >= 31) AND (l-dias <= 60) THEN
                    ASSIGN 
                        l-tot60 = l-tot60 + l-aviso.
                  
                IF l-dias >= 61 AND l-dias <= 75 THEN
                    ASSIGN 
                        l-tot75 = l-tot75 + l-aviso.
                
                IF l-dias >= 76 AND l-dias <= 90 THEN
                    ASSIGN 
                        l-tot90 = l-tot90 + l-aviso.
                  
                IF l-dias > 90 THEN
                    ASSIGN 
                        l-tot91 = l-tot91 + l-aviso.
                                  
                ACCUMULATE l-aviso * l-dias (TOTAL).
            END. /* for each AvisoCargo */
    
    
            IF NOT l-checausuario THEN DO:
                
                FOR EACH Devolucion WHERE Devolucion.Id-Cliente = Cliente.Id-Cliente AND 
                                          Devolucion.FecApl     = ? AND 
                                          Devolucion.FecCanc    = ? AND 
                                          Devolucion.TipoVenta  = 3 AND 
                                          Devolucion.FecReg    <= l-fecharep 
                                          NO-LOCK:
                    ASSIGN 
                        l-dias       = l-fecharep - Devolucion.FecReg
                        l-totDevPend = l-totDevPend + devolucion.Tot.
                  
                  
                    IF l-dias > 0 THEN 
                        ACCUMULATE Devolucion.Tot (TOTAL).
                    
                  
                    IF l-dias <= 30 THEN
                        ASSIGN 
                            l-tot30 = l-tot30 - Devolucion.Tot.
                  
                    IF l-dias >= 31 AND l-dias <= 60 THEN
                        ASSIGN 
                            l-tot60 = l-tot60 - Devolucion.Tot.
                  
                    IF l-dias >= 61 AND l-dias <= 75 THEN
                        ASSIGN 
                            l-tot75 = l-tot75 - Devolucion.Tot.
                  
                    IF l-dias >= 76 AND l-dias <= 90 THEN
                        ASSIGN 
                            l-tot90 = l-tot90 - Devolucion.Tot.
                  
                    IF l-dias > 90 THEN
                        ASSIGN 
                            l-tot91 = l-tot91 - Devolucion.Tot.
                  
                END.
    
            END. /* si es para enviar no se imprimen devoluciones */
    
            IF l-hubo THEN DO:
                 
                RUN cxcd0010.p (INPUT MovCliente.Id-Cliente, OUTPUT l-prompago).
                
                ASSIGN 
                    l-total = l-total + l-totCargo - l-totDevPend
                    l-diasmax  = ((ACCUM TOTAL l-saldo * l-dias) + (ACCUM TOTAL l-aviso * l-dias)) / l-total.
                
                IF NOT l-checausuario THEN
                    ASSIGN 
                        l-tot30 = l-tot30 + l-interes 
                        l-total = l-total + l-interes.
    
             
                IF l-diasmax < 0 THEN 
                    l-diasmax = 0.
                
    
                ASSIGN 
                    l-hubo = FALSE.

            END. /* si hubo registros */
             
            ASSIGN 
                l-totchedev = 0  l-totintereses = 0  l-totncargo = 0
                l-totcargo  = 0  l-totfacturas  = 0  l-total     = 0
                l-tot30     = 0  l-tot60        = 0  l-tot75     = 0
                l-tot90     = 0  l-tot91        = 0  l-chedev    = 0
                l-chedev2   = 0  l-totDevPend   = 0  l-interes   = 0.
        END.
    END.
END.
