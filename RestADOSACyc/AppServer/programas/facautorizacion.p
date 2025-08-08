/* 
    Autorizacion de Facturas en cajas desde .Net
*/

DEFINE VARIABLE v-NomVend AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-SaldoPed AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldo AS DECIMAL NO-UNDO.
DEFINE VARIABLE vPedSaldo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-DC AS INTEGER NO-UNDO.
DEFINE VARIABLE v-PromPago AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-FecDep AS DATE NO-UNDO.
DEFINE VARIABLE l-Acuse LIKE Acuse.Id-Acuse NO-UNDO.
DEFINE VARIABLE l-ImpPago AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-SumaDoc AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotChDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotFac AS DECIMAL NO-UNDO.
DEFINE VARIABLE vEfevo AS DECIMAL NO-UNDO.
DEFINE VARIABLE vChNoDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotPed AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Importe AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-Asunto AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Contenido AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Limite2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-LimiteMaximo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-Disponible AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldoV30 AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldoV40 AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldoVenc AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-Para AS CHARACTER NO-UNDO.
DEFINE BUFFER b-MovCliente FOR MovCliente.
DEFINE BUFFER b-HistMovCte FOR HistMovCte.
DEFINE BUFFER bfPedido FOR Pedido.


DEFINE TEMP-TABLE ttPorAut
    FIELD Estatus AS CHARACTER /* AUTORIZADO, PENDIENTE, RECHAZADO, CANCELADO */ 
    FIELD Orden AS INTEGER 
    FIELD IdClaseCte LIKE Cliente.Id-ClaseCte
    FIELD NombreClaseCte LIKE ClaseCte.Descr
    FIELD IdCliente LIKE Cliente.Id-Cliente
    FIELD RazonSocial LIKE Cliente.RazonSocial
    FIELD Bloqueado AS LOGICAL FORMAT "SI/NO"
    FIELD TipoBloqueo AS CHARACTER 
    FIELD Documento AS CHARACTER 
    FIELD TipoDocto AS CHARACTER /* Factura, PEDIDO */
    FIELD Cond AS CHARACTER /* CONTADO, CREDITO */
    FIELD FecReg AS DATE 
    FIELD HorReg AS CHARACTER 
    FIELD Importe LIKE Pedido.Tot
    FIELD Plazo AS INTEGER FORMAT "ZZ9"
    FIELD Sucursal AS CHARACTER 
    FIELD NomVendedor AS CHARACTER 
    FIELD NomResponsable AS CHARACTER
    FIELD FecAutorizado AS DATE
    FIELD Motivo AS CHARACTER
    FIELD Desactivado AS LOGICAL
    FIELD SaldoVenc AS DECIMAL
    FIELD Saldo AS DECIMAL
    FIELD IdUbic AS CHARACTER 
    FIELD RecAuto AS RECID
    FIELD IdVendedor LIKE Vendedor.Id-Vendedor. 



/* **********************  Internal Procedures  *********************** */
DEFINE INPUT PARAMETER ipAutoriza AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipUser AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ttPorAut.


FIND FIRST ttPorAut NO-LOCK NO-ERROR.

CREATE Autorizacion.
ASSIGN 
    Autorizacion.Id-Cliente  = ttPorAut.IdCliente
    Autorizacion.FecReg      = TODAY
    Autorizacion.HorReg      = TIME
    Autorizacion.Tipo        = ttPorAut.TipoDocto
    Autorizacion.Rectipo     = ttPorAut.RecAuto
    Autorizacion.SaldoVenc   = ttPorAut.SaldoVenc
    Autorizacion.Saldo       = vSaldo
    Autorizacion.Importe     = ttPorAut.Importe
    Autorizacion.Id-Vendedor = ttPorAut.IdVendedor
    Autorizacion.Id-Autoriza = ipUser
    Autorizacion.Id-Ubic     = ttPorAut.IdUbic
    Autorizacion.Campo2      = (IF ipAutoriza = TRUE THEN "" ELSE "FALSE").

IF ipAutoriza = TRUE THEN DO:
    FIND Cliente WHERE Cliente.Id-Cliente = ttPorAut.idCliente NO-LOCK NO-ERROR.
    FIND Ciudad WHERE Ciudad.id-Ciudad = cliente.id-Ciudad NO-LOCK NO-ERROR.
    IF AVAILABLE Ciudad THEN
        FIND estado WHERE estado.id-estado = Ciudad.id-estado NO-LOCK NO-ERROR.                       
    FIND Vendedor WHERE Vendedor.Id-vend = ttPorAut.idVendedor NO-LOCK NO-ERROR.
    IF AVAILABLE Vendedor THEN
        FIND FIRST usuario WHERE usuario.Id-user = Vendedor.iniciales NO-LOCK NO-ERROR.    
    IF AVAILABLE usuario THEN
        v-nomvend = usuario.nom.

    vSaldo = 0.
    vPedSaldo = 0.
    FOR EACH Pedido WHERE Pedido.Id-Cliente = Cliente.id-cliente
                      AND Pedido.Id-Estatus >= -1
                      AND Pedido.Id-Estatus < 5
                      AND Pedido.EnFirme = TRUE NO-LOCK:
        ASSIGN 
            vSaldo = vSaldo + Pedido.Subtotal
            vPedSaldo = vPedSaldo + Pedido.Subtotal.       
    END. /* del pedido */  

    ASSIGN
        v-limite2       = cliente.limite
        v-limitemaximo  = (cliente.limite * 1.5 )
        v-disponible    = v-limite2 - vSaldo.

    ASSIGN
        v-saldoped = IF vSaldo > 0 THEN vSaldo - vPedSaldo ELSE 0.
    
    FIND FIRST usuario WHERE usuario.Id-user = ipUser NO-LOCK NO-ERROR.

    RUN cxcb0270.p (INPUT Cliente.Id-Cliente, 
                    INPUT TODAY,
                    OUTPUT v-dc,
                    OUTPUT v-prompago).
    
    IF v-dc = ? THEN
        v-dc = 0.
    IF v-prompago = ? THEN
        v-prompago = 0.

    l-fecdep = ?.
    l-Acuse = "".
    l-ImpPago = 0.
    FOR EACH Acuse WHERE Acuse.Id-Cliente = Cliente.Id-Cliente NO-LOCK:
       IF (l-fecdep = ? OR Acuse.fecdep > l-fecdep) AND 
          Acuse.estatus <> 3 THEN DO:
          l-SumaDoc = 0.
          IF Acuse.Tipo <> "A" THEN DO:
             FOR EACH DocAcuse WHERE DocAcuse.Id-Acuse = Acuse.Id-Acuse NO-LOCK:
                 l-SumaDoc = l-SumaDoc + DocAcuse.imppago.
             END.
          END.
          ELSE DO:
             FOR EACH PagoAcuse WHERE PagoAcuse.Id-Acuse = Acuse.Id-Acuse NO-LOCK:
                 l-SumaDoc = l-SumaDoc + PagoAcuse.Importe.
             END.
          END.
          IF l-SumaDoc > 0 THEN
             ASSIGN l-Acuse = Acuse.Id-Acuse
                    l-fecdep = Acuse.fecdep
                    l-ImpPago = l-SumaDoc.
       END.
    END.
    FOR EACH DetAnticipo WHERE (DetAnticipo.FecReg >= l-fecdep OR l-fecdep = ?) NO-LOCK,
        EACH Anticipo WHERE Anticipo.Id-Anticipo = DetAnticipo.Id-Anticipo
                        AND Anticipo.Id-Cliente = 3 NO-LOCK,
        EACH Acuse WHERE Acuse.Id-Acuse = Anticipo.Id-Acuse NO-LOCK,
        EACH b-MovCliente WHERE b-MovCliente.RefSaldo = DetAnticipo.Documento
                            AND b-MovCliente.Id-Mc = DetAnticipo.Id-MC
                            AND b-MovCliente.Id-Cliente = Cliente.Id-Cliente 
                          NO-LOCK BREAK BY Acuse.Id-Acuse:
                              
        IF FIRST-OF(Acuse.Id-Acuse) THEN 
            l-SumaDoc = 0.
              
        l-SumaDoc = l-SumaDoc + DetAnticipo.Importe.
        
        IF LAST-OF(Acuse.Id-Acuse) THEN DO:
            IF l-SumaDoc > 0 AND (l-fecdep = ? OR Acuse.fecdep > l-fecdep) THEN 
                ASSIGN l-Acuse = Acuse.Id-Acuse
                       l-fecdep = Acuse.fecdep
                       l-ImpPago = l-SumaDoc.
        END.
    END.

    FOR EACH DetAnticipo WHERE (DetAnticipo.FecReg >= l-fecdep OR l-fecdep = ?) NO-LOCK,
        EACH Anticipo WHERE Anticipo.Id-Anticipo = DetAnticipo.Id-Anticipo
                        AND Anticipo.Id-Cliente = 3 NO-LOCK,
        EACH Acuse WHERE Acuse.Id-Acuse = Anticipo.Id-Acuse NO-LOCK,
        EACH b-HistMovCte WHERE b-HistMovCte.RefSaldo = DetAnticipo.Documento
                            AND b-HistMovCte.Id-Mc = DetAnticipo.Id-MC
                            AND b-HistMovCte.Id-Cliente = Cliente.Id-Cliente 
                            NO-LOCK BREAK BY Acuse.Id-Acuse:
        IF FIRST-OF(Acuse.Id-Acuse) THEN 
              l-SumaDoc = 0.
        l-SumaDoc = l-SumaDoc + DetAnticipo.Importe.
        IF LAST-OF(Acuse.Id-Acuse) THEN DO:
          IF l-SumaDoc > 0 AND (l-fecdep = ? OR Acuse.fecdep > l-fecdep) THEN 
             ASSIGN l-Acuse = Acuse.Id-Acuse
                    l-fecdep = Acuse.fecdep
                    l-ImpPago = l-SumaDoc.
        END.
         
    END.
    
    FIND CURRENT ttPorAut EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    ASSIGN vTotChDep = 0
           vTotFac = 0
           vEfevo = 0
           vChNoDep = 0
           vTotPed = 0.
           
    FOR EACH Acuse WHERE Acuse.Id-Cliente = ttPorAut.IdCliente
                     AND Acuse.FecDep >= (TODAY - 30)
                     AND Acuse.FecDep <= TODAY
                     AND Acuse.Estatus = 4 NO-LOCK:
        ASSIGN l-Importe = 0.
        FOR EACH PagoAcuse OF Acuse NO-LOCK:
            ASSIGN l-Importe = l-Importe + pagoAcuse.Importe.
        END.
        ASSIGN vTotChDep = vTotChDep + l-Importe.
    END.
    
    FOR EACH DetAnticipo WHERE DetAnticipo.FecReg >= (TODAY - 30) NO-LOCK,
        EACH Anticipo WHERE Anticipo.Id-Anticipo = DetAnticipo.Id-Anticipo
                        AND Anticipo.Id-Cliente = 3 NO-LOCK,
        EACH b-MovCliente WHERE b-MovCliente.RefSaldo = DetAnticipo.Documento
                            AND b-MovCliente.Id-Mc = DetAnticipo.Id-MC
                            AND b-MovCliente.Id-Cliente = Cliente.Id-Cliente 
                            NO-LOCK:
        ASSIGN vTotChDep = vTotChDep + DetAnticipo.Importe.
    END. 
    
    FOR EACH Acuse WHERE Acuse.Id-Cliente = ttPorAut.IdCliente
                     AND Acuse.Estatus < 3 NO-LOCK:
        ASSIGN l-Importe = 0.
        FOR EACH PagoAcuse OF Acuse NO-LOCK:
            ASSIGN l-Importe = l-Importe + pagoAcuse.Importe.
        END.
        ASSIGN vChNoDep = vChNoDep + l-Importe.
    END.
    FOR EACH CheDev WHERE CheDev.Id-Cliente = ttPorAut.IdCliente
                      AND CheDev.FecCargo >= (TODAY - 30)
                      AND CheDev.FecCargo <= TODAY
                      AND CheDev.FecCanc = ? NO-LOCK BY CheDev.FecCargo:
        ASSIGN vTotChDep = vTotChDep - CheDev.ImpCheque.
    END.
    FOR EACH Factura WHERE Factura.Id-Cliente = ttPorAut.IdCliente
                       AND Factura.FecReg >= (TODAY - 30)
                       AND Factura.FecReg <= TODAY
                       AND Factura.FecCanc = ? NO-LOCK:
        ASSIGN vTotFac = vTotFac + Factura.Tot.
    END.
    
    FOR EACH estpedido WHERE estpedido.Id-Cliente = ttPorAut.IdCliente
                         AND estpedido.estatus < 5 NO-LOCK,
        EACH Detpedido WHERE detpedido.Id-pedido = estpedido.Id-pedido
                         AND detpedido.resto = estpedido.Id-seq NO-LOCK:
        ACCUMULATE vTotPed = vTotPed + DetPedido.Importe + DetPedido.Iva.
    END.
    
    ASSIGN vEfevo = (vTotFac + vTotPed) - vTotChDep.
    
    ASSIGN 
       vSaldoV30 = 0
       vSaldoV40 = 0
       vSaldoVenc = 0
       vSaldo = 0.
       
    FOR EACH MovCliente WHERE MovCliente.id-cliente = Cliente.Id-Cliente
                          AND movcliente.id-mc <= 3 NO-LOCK:
        IF MovCliente.FecVenc < TODAY THEN DO:
            ASSIGN vSaldoVenc = vSaldovenc + movcliente.saldo.
            IF movcliente.fecvenc < (TODAY - 30) THEN            
                ASSIGN vSaldov30 = vSaldov30 + movcliente.saldo.
        END.
        IF movcliente.importe = movcliente.saldo AND
           TODAY - movcliente.fecreg > 40 THEN            
            ASSIGN vSaldov40 = vSaldov40 + movcliente.saldo.
        ASSIGN vSaldo = vSaldo + movcliente.saldo.
    END. /* del for each movcliente */
    
    ASSIGN
         v-asunto    = "AVISO AUTOMATICO DE AUTORIZACION DE CREDITO (cxcd1613-M)"             
         v-contenido = "<html><head>" + CHR(13) +
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) +
                       "style='font-size:10.0pt;font-family:Verdana'>" + 'Se realizo una autorizacion de ' + ttPorAut.TipoDocto + ', por ' + usuario.nom-usuario + '.' + "<o:p></o:p></span></p>" + CHR(13) +
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + 'Fecha y hora.....:   ' + STRING(TODAY,"99/99/9999") + ' ' + STRING(TIME,"hh:mm:ss")  + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +     
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Cliente..........:   "  + STRING(ttPorAut.IdCliente) + " " + (IF AVAILABLE Cliente THEN Cliente.razonsocial ELSE "No disponible") + "<o:p></o:p></span></p>" + CHR(13) +
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Poblacion........:   " + (IF AVAILABLE Ciudad THEN TRIM(Ciudad.nombre) ELSE "") + ", " + (IF AVAILABLE estado THEN TRIM(estado.nombre) ELSE "") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + 
                       "Motivo...........:   " + (IF ttPorAut.Importe > 50000
                                                  THEN "COMPRA MAYOR A $50,000 "
                                                  ELSE "") +
                                                 (IF ((ttPorAut.Importe + vSaldo) >= v-limitemaximo)
                                                  THEN "Cliente EXCEDE MAS DE UN 50% SU LIMITE DE CREDITO "
                                                  ELSE "") +
                                                 (IF vSaldov30 > 0 THEN "Cliente CON MAS DE 30 DIAS VENCIDO"
                                                  ELSE "") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +   
                                                                      
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" +                                                    
                       "Plazo............:   " + STRING(Cliente.plazo) + " dias"                               + CHR(13) + 
                       "<o:p></o:p></span></p>" + CHR(13) +     
                                         
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + 
                       "Solicita.........:  $" + TRIM(STRING(ttPorAut.Importe,"-ZZZ,ZZZ,ZZ9.99"))                + CHR(13) +
                       "<o:p></o:p></span></p>" + CHR(13) +    
                                          
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" +                                              
                       "Ultimo Pago......:  $" + TRIM(STRING(l-ImpPago,"-ZZZ,ZZZ,ZZ9.99"))                     + "  " + 
                                                 (IF l-FecDep <> ? THEN STRING(l-FecDep,"99/99/9999") ELSE "") + "  " +
                                                 l-Acuse                                                       + CHR(13) + CHR(13) + 
                       "<o:p></o:p></span></p>" + CHR(13) +         
                                     
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" +
                       "Resumen 30 dias"                                                                       + CHR(13) +
                       "<o:p></o:p></span></p>" + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Total Ch. Depos..:  $" + STRING(vTotChDep,"-ZZZ,ZZZ,ZZ9.99") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Total de Factura.:  $" + STRING(vTotFac,"-ZZZ,ZZZ,ZZ9.99")   + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                        
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Pedidos Pend.....:  $" + STRING(vTotPed,"-ZZZ,ZZZ,ZZ9.99")   + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                        

                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Efecto en cartera:  $" + STRING(vEfevo,"-ZZZ,ZZZ,ZZ9.99")   + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                       
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Ch NO Depositados:  $" + STRING(vChNoDep,"-ZZZ,ZZZ,ZZ9.99")  + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Saldo Act........:  $" + STRING(vSaldo,"-ZZZ,ZZZ,ZZ9.99")    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) + 
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + " * Con Factura...:  $" + STRING(v-saldoped,"-ZZZ,ZZZ,ZZ9.99")      + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "       * Venc -30:  $" + STRING(ttPorAut.SaldoVenc - vSaldov30,"-ZZZ,ZZZ,ZZ9.99") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                        
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "       * Venc +30:  $" + STRING(vSaldov30,"-ZZZ,ZZZ,ZZ9.99")                    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "       * P/Ven...:  $" + STRING(v-saldoped - ttPorAut.SaldoVenc,"-ZZZ,ZZZ,ZZ9.99")      + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + " * Por Facturar..:  $" + STRING(vPedSaldo,"-ZZZ,ZZZ,ZZ9.99")                    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Limite Credito...:  $" + STRING(v-limite2,"-ZZZ,ZZZ,ZZ9.99")                          + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                        
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Cred. Disponible.:  $" + STRING(v-disponible,"-ZZZ,ZZZ,ZZ9.99")                       + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Dias Cartera.....:   " + STRING(v-dc,"-ZZZ,ZZZ,ZZ9")                                  + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Promedio Pago....:   " + STRING(v-prompago,"-ZZZ,ZZZ,ZZ9")                            + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Factor Descto....:   " + STRING(Cliente.FactorDes,"-ZZZ,ZZZ,ZZ9.99 %")                + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + 
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Vendedor.........:   " + STRING(v-nomvend,"X(20)")                                    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) + CHR(13) +
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Ubic Cte.........:   " + STRING(ttPorAut.Sucursal)                                       + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) + CHR(13) +                                                
                       
                       "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "(cxcd1613.i)"          + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                       
                       '<meta http-equiv="Content-Type" content="text/html; charset=windows-1252">' + CHR(13) +
                       "</head>".             
                   
                   
       IF {sist0001.i} = "DESARROLLO" THEN 
                ASSIGN v-Para = "crivera@adosa.com.mx".
            ELSE DO:
                v-Para = (IF ttPorAut.Importe >= 5000 AND vEfevo >= 3000 THEN 'agomez@adosa.com.mx; ogomez@adosa.com.mx; ' ELSE '') +
                         'dgarza@adosa.com.mx; jgonzalez@adosa.com.mx'.
            END.             
                   
                   
    {inva0007.i 
        &Asunto     = "v-asunto"   
        &contenido  = "v-contenido"
        &Iniciales  = "'JAGR'"
        &Direccion  = "v-Para"
        &Refer      = "'DIRECTO'"
        &Attachment = "" 
    }    
                      
END.   

RETURN.
