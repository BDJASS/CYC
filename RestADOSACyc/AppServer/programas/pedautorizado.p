/*
    Autoriza pedidos desde .Net
*/

DEFINE INPUT PARAMETER ipPedido LIKE Pedido.Id-Pedido NO-UNDO.
DEFINE INPUT PARAMETER ipSegunda AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipTodos AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipUsuario AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipImporte AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opError AS CHARACTER NO-UNDO.

DEFINE NEW SHARED VARIABLE l-Tipo-3 AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE l-Tipo-4 AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE l-Alta AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE l-Cancela AS LOGICAL NO-UNDO.

DEFINE VARIABLE vLimite LIKE Cliente.Limite NO-UNDO.
DEFINE VARIABLE vLimiteMaximo LIKE Cliente.Limite NO-UNDO.
DEFINE VARIABLE vFolio AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDisponible AS DECIMAL NO-UNDO.

DEFINE VARIABLE l-Loc AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Para AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Estatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-limite2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-limitemaximo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-disponible AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Secuencia AS INTEGER NO-UNDO.
DEFINE VARIABLE l-Truco AS LOGICAL NO-UNDO.
DEFINE VARIABLE vSaldoV30 AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldoV40 AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldoVenc AS DECIMAL NO-UNDO.
DEFINE VARIABLE vSaldo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-NomVend LIKE Usuario.Nom-Usuario NO-UNDO.
DEFINE VARIABLE v-SaldoPed AS DECIMAL NO-UNDO.
DEFINE VARIABLE vPedSaldo AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-DC AS INTEGER NO-UNDO.
DEFINE VARIABLE v-PromPago AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-FecDep AS DATE NO-UNDO.
DEFINE VARIABLE l-Acuse AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ImpPago AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-SumaDoc AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotChDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotFac AS DECIMAL NO-UNDO.
DEFINE VARIABLE vEfecto AS DECIMAL NO-UNDO.
DEFINE VARIABLE vChNoDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotDep AS DECIMAL NO-UNDO.
DEFINE VARIABLE vTotPed AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Importe AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-Asunto AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Contenido AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Pedido AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Hora AS INTEGER NO-UNDO.

DEFINE BUFFER bfPedido FOR Pedido.
DEFINE BUFFER bf2Pedido FOR Pedido.
DEFINE BUFFER b-MovCliente FOR MovCliente.
DEFINE BUFFER b-HistMovCte FOR HistMovCte.

FIND FIRST Pedido WHERE Pedido.Id-Pedido = ipPedido NO-LOCK NO-ERROR NO-WAIT.
IF NOT AVAILABLE Pedido THEN DO:
    ASSIGN opError = "Folio de pedido inexistente".
    RETURN.
END.

IF Pedido.SolCancPed THEN DO:
    ASSIGN opError = "Folio de pedido con solicitud de cancelacion".
    RETURN.
END.

FIND FIRST BlkAut WHERE BlkAut.Id-Cliente = Pedido.Id-Cliente NO-LOCK NO-ERROR.
IF AVAILABLE BlkAut THEN DO:
    ASSIGN opError = "Actualmente el Cliente " +
                     STRING(Pedido.Id-Cliente) +
                     " " +
                     Pedido.RazonSocial + 
                     " se encuentra bloqueado para autorizaciones. " +
                     "No es posible realizar la autorizacion de su pedido".
    RETURN.
END. 

FIND FIRST blkRFC WHERE REPLACE(blkRFC.RFC," ","") = REPLACE(Pedido.RFC," ","") NO-LOCK NO-ERROR.
IF AVAILABLE blkRFC THEN DO:
    ASSIGN opError = "El RFC " +
                    Pedido.RFC +
                    " del Cliente " +
                    STRING(Pedido.Id-Cliente) + 
                    " " + 
                    Pedido.RazonSocial +
                    " se encuentra bloqueado. " +
                    "No es posible realizar la autorizacion del pedido".
    RETURN.
END.

FIND Cliente OF Pedido NO-LOCK NO-ERROR.

IF Pedido.EnFirme = FALSE AND
   Pedido.Cancelado = FALSE AND
   INTEGER(SUBSTRING(Pedido.Id-Pedido,1,1)) < 5 THEN DO:
    
    IF ipSegunda = FALSE THEN DO:
        FOR EACH bfPedido WHERE bfPedido.Id-Pedido <> Pedido.Id-Pedido
                            AND INTEGER(SUBSTRING(Pedido.Id-Pedido,1,1)) < 5
                            AND bfPedido.Id-Cliente = Pedido.Id-Cliente
                            AND bfPedido.Id-Cliente <> 3
                            AND bfPedido.EnFirme = FALSE
                            AND bfPEdido.Cancelado = FALSE
                            AND bfPedido.Id-Alm = Pedido.Id-Alm NO-LOCK:
            ACCUMULATE bfPedido.Tot (TOTAL).
            ACCUMULATE 1 (COUNT).
        END.
        IF (ACCUM COUNT 1) > 1 THEN DO:
            ASSIGN opError = "001.- Existen" + 
                             STRING((ACCUM COUNT 1)) + 
                             " pedidos del Cliente " + 
                             STRING(Pedido.Id-Cliente) + 
                             " pendientes de autorizar por un monto total de $" +
                             STRING((ACCUM TOTAL bfPedido.Tot)) + 
                             ". Desea autorizar todos los pedidos pendientes de este Cliente?".
            RETURN.
        END.
    END.
    
    RUN Autoriza.
END.    
RELEASE Pedido.

RETURN.


PROCEDURE Autoriza.
    DO TRANSACTION:
        FOR EACH bfPedido WHERE (IF ipTodos = TRUE 
                                THEN bfPedido.Id-Cliente = Pedido.Id-Cliente
                                ELSE bfPedido.Id-Pedido = Pedido.Id-Pedido)
                            AND bfPedido.EnFirme = FALSE EXCLUSIVE-LOCK:
                                    
            ASSIGN 
               vSaldoV30 = 0
               vSaldoV40 = 0
               vSaldoVenc = 0
               vSaldo = 0.
            FOR EACH MovCliente WHERE MovCliente.id-cliente = bfPedido.id-cliente
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
            
            RELEASE Ciudad.
            RELEASE Estado.
            FIND Cliente WHERE Cliente.Id-Cliente = bfPedido.Id-Cliente NO-LOCK NO-ERROR.
            FIND Ciudad WHERE Ciudad.Id-Ciudad = Cliente.Id-Ciudad NO-LOCK NO-ERROR.
            IF AVAILABLE Ciudad THEN
                FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.

            ASSIGN
                v-limite2      = Cliente.limite
                v-limitemaximo = (Cliente.limite * 1.5)
                v-disponible   = v-limite2 - vSaldo.
            
            ASSIGN vFolio = ''.

            ASSIGN bfPedido.EnFirme = TRUE
                   bfPedido.AutPor  = ipUsuario
                   bfPedido.CveAut  = 9999999
                   bfPedido.FecReg  = TODAY.
     
            ASSIGN bfPedido.FFinAut = TODAY
                   bfPedido.HFinAut = TIME.
     
            IF SUBSTRING(bfPedido.Id-Pedido,1,1) = '0' THEN DO:
                IF bfPedido.Id-Alm <> '11' THEN DO:
                    FIND Vendedor WHERE Vendedor.Id-vend = bfPedido.Id-vend NO-LOCK NO-ERROR.
                    IF AVAILABLE Vendedor THEN DO:
                        IF Vendedor.Id-Vendedor <> "0104" THEN
                            FIND FIRST Usuario WHERE Usuario.Id-user = Vendedor.iniciales NO-LOCK NO-ERROR.
                        ELSE
                            FIND FIRST Usuario WHERE Usuario.Id-user = Vendedor.iniciales NO-LOCK NO-ERROR.
                    END.
                    
MESSAGE AVAILABLE Usuario bfPEdido.Id-Vendedor
VIEW-AS ALERT-BOX.                    
                    
                    IF AVAILABLE Usuario THEN DO:
                        IF bfPedido.Id-Alm BEGINS '1' THEN DO:
                            RUN /usr2/adosa/procs/vtad1040.p(OUTPUT vFolio,
                                                             INPUT bfPedido.Id-Cliente,
                                                             INPUT bfPedido.Id-UbiVta,
                                                             INPUT bfPedido.Id-Vendedor,
                                                             INPUT TRUE,
                                                             INPUT bfPedido.Enviar,
                                                             INPUT bfPedido.Id-Pedido).
                        END.
                        ELSE DO:
                            RUN /usr2/adosa/procs/vtad0040.p(OUTPUT vFolio,
                                                             INPUT bfPedido.Id-Cliente,
                                                             INPUT bfPedido.Id-UbiVta,
                                                             INPUT bfPedido.Id-Vendedor,
                                                             INPUT TRUE,
                                                             INPUT bfPedido.Enviar,
                                                             INPUT bfPedido.Id-Pedido).
                        END.
                    END.
                END.
                ELSE DO:
                    RUN /usr2/adosa/procs/vtad0040.p(OUTPUT vFolio,
                                                     INPUT bfPedido.Id-Cliente,
                                                     INPUT bfPedido.Id-UbiVta,
                                                     INPUT bfPedido.Id-Vendedor,
                                                     INPUT TRUE,
                                                     INPUT bfPedido.Enviar,
                                                     INPUT bfPedido.Id-Pedido).
                END.
     
                FOR EACH BtcBajDetPed WHERE BtcBajDetPed.Id-Pedido = bfPedido.Id-Pedido EXCLUSIVE-LOCK:
                    ASSIGN BtcBajDetPed.Id-Pedido = vFolio.
                END.
                FOR EACH DetPedido WHERE DetPedido.Id-Pedido = bfPedido.Id-Pedido
                                     AND DetPedido.Resto = bfPedido.resto EXCLUSIVE-LOCK:
                    ASSIGN DetPedido.Id-Pedido = vFolio.
             
                    IF DetPedido.Tipo = 1 AND DetPedido.CantCom = 0 THEN DO:
                        {/usr2/adosa/includes/vtaa0001.new
                            &Articulo = DetPedido.Id-Articulo
                            &Color    = DetPedido.Id-Color
                            &Pres     = DetPedido.Id-Pres
                            &Cant     = DetPedido.CantPed
                            &bfPedido   = bfPedido.Id-Pedido
                            &UbiVta   = bfPedido.Id-UbiVta
                            &Seq      = DetPedido.Id-Seq
                            &Costo    = DetPedido.Costo
                            &Prov     = DetPedido.Id-Prov
                        }
                        IF DetPedido.Id-Loc = '' THEN DO:
                            {/usr2/adosa/includes/bodd0030.i}
                        END.
                    END.
                END.
                RELEASE DetPedido.
                
                FIND EstPedido WHERE EstPedido.Id-Pedido = bfPedido.Id-Pedido
                                 AND EstPedido.Id-Seq = bfPedido.Resto EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE EstPedido THEN ASSIGN EstPedido.Id-Pedido = vFolio.
                RELEASE EstPedido.
                FOR EACH btcautprec WHERE btcautprec.Id-factura = bfPedido.Id-Pedido EXCLUSIVE-LOCK: /*actualiza folio en la bitacora*/
                    ASSIGN btcautprec.Id-factura = vFolio.
                END.
                
                FIND FIRST PedFolio WHERE PedFolio.Id-PreFolio = bfPedido.Id-Pedido EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE PedFolio THEN DO:
                    CREATE PedFolio.
                    ASSIGN PedFolio.Id-PreFolio = bfPedido.Id-Pedido.
                END.
                
                ASSIGN PedFolio.Id-Pedido = vFolio
                       PedFolio.Resto     = bfPedido.Resto
                       PedFolio.FecReg    = TODAY
                       PedFolio.HorReg    = TIME
                       PedFolio.Id-User   = USERID('dictdb').
                RELEASE PedFolio.

                ASSIGN bfPedido.Id-Pedido = vFolio.
                
                IF bfPedido.Filler-1 MATCHES '*CHPF*' THEN DO:
                    SUBSTRING(bfPedido.Filler-1,INDEX(bfPedido.Filler-1,"*CHPF*"),6) = "".
                    FOR EACH ChequePF WHERE ChequePF.Id-Cliente = bfPedido.Id-Cliente
                                        AND ChequePF.Dep = FALSE NO-LOCK:
                        ACCUMULATE 1 (COUNT).
                        ACCUMULATE ChequePF.Importe (TOTAL).
                    END.
                    IF ((ACCUM COUNT 1) + 1 > Cliente.CantCheque) OR
                       ((ACCUMULATE TOTAL ChequePF.Importe) + (bfPedido.SubTot * (1 + (bfPedido.PorcIVA / 100))) > Cliente.LimCheque) THEN DO:
                        MESSAGE
                            "NO se movieron los limites de importe o cantidad en los cheques posfechados del Cliente,"
                            "por lo tanto, se aceptara pago unicamente con cheque normal para este bfPedido"
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                    END.
                END.
            END.
            
            IF bfPedido.EnFirme THEN DO:
                RUN /usr2/adosa/procs/vtaa0116.p(RECID(bfPedido),FALSE).
                RUN /usr2/adosa/procs/embd0010.p (INPUT 1,
                                                  INPUT bfPedido.Id-Pedido,
                                                  INPUT bfPedido.Resto).
                IF bfPedido.Enfirme = TRUE AND
                   (bfPedido.Id-Alm = '02B' OR bfPedido.Id-Alm = '11' OR bfPedido.Id-Alm = '12' OR bfPedido.Envia = TRUE) AND
                   bfPedido.Adelantado = FALSE AND bfPedido.Id-Tran <> 98 THEN DO:
                    IF CAN-DO("02B",bfPedido.Id-Alm) THEN /*QUITAR ESTA CONDICION CUANDO ESTE LISTO 11*/ 
                        RUN /usr2/adosa/procs/bodd1020.p(bfPedido.Id-Pedido,bfPedido.Resto). /* genera tareas */
                    ELSE
                        RUN /usr2/adosa/procs/bodd1120.p(bfPedido.Id-Pedido,bfPedido.Resto). /* genera tareas sucursales*/
                    RUN /usr2/adosa/procs/bodd1070.p(bfPedido.Id-Pedido,bfPedido.Resto). /* examina areas */
                END.
                RUN /usr2/adosa/procs/bodd1071.p(bfPedido.Id-Pedido,bfPedido.Resto). /* examina cortes */
            END.
            
            RUN /usr2/adosa/procs/vtac0800.p(INPUT bfPedido.Id-Pedido,
                                             INPUT bfPedido.Resto) NO-ERROR.
            
            IF bfPedido.Id-Alm <> "11" THEN
                RUN /usr2/adosa/procs/vtac0200.p(INPUT bfPedido.Id-Pedido,
                                                 INPUT bfPedido.Resto,
                                                 INPUT FALSE,
                                                 INPUT-OUTPUT l-Truco,
                                                 INPUT FALSE).
            ELSE
                RUN /usr2/adosa/procs/vtac0200.p(INPUT bfPedido.Id-Pedido,
                                                 INPUT bfPedido.Resto,
                                                 INPUT TRUE,
                                                 INPUT-OUTPUT l-Truco,
                                                 INPUT FALSE).
        
            // 2021-10-07 - Genera Requisiciones Automaticas
            IF bfPedido.BckOrd = 3 AND bfPedido.Id-Alm = "02B" THEN 
                RUN /usr2/adosa/procs/inva0156.p (INPUT bfPedido.Id-Pedido, 
                                                  INPUT bfPedido.Resto,
                                                  INPUT 1,
                                                  OUTPUT l-estatus).
            
            /* Envia correos */
            FIND vendedor WHERE vendedor.id-vend = bfPedido.id-vend NO-LOCK NO-ERROR.
            IF AVAILABLE vendedor THEN
                FIND FIRST usuario WHERE usuario.id-user = vendedor.iniciales NO-LOCK NO-ERROR.
            IF AVAILABLE usuario THEN
                v-nomvend = usuario.nom-usuario.
            
            ASSIGN vPedSaldo = 0.
            FOR EACH bf2Pedido WHERE bf2Pedido.Id-Cliente = bfPedido.Id-Cliente
                                 AND bf2Pedido.Id-Estatus >= -1
                                 AND bf2Pedido.Id-Estatus < 5
                                 AND Pedido.EnFirme = TRUE NO-LOCK:
                ASSIGN 
                     vSaldo = vSaldo + Pedido.Subtotal
                     vPedSaldo = vPedSaldo + Pedido.Subtotal.       
            
            END. /* del pedido */  
            
            
            ASSIGN
                v-saldoped = IF vSaldo > 0 THEN vSaldo - vPedSaldo ELSE 0.
            
            FIND FIRST usuario WHERE usuario.id-user = ipUsuario NO-LOCK NO-ERROR.
            
            RUN /usr2/adosa/procs/cxcb0270.p(INPUT bfPedido.Id-Cliente,
                                             INPUT TODAY,
                                             OUTPUT v-dc,
                                             OUTPUT v-prompago).
            IF v-dc = ? THEN
                v-dc = 0.
            IF v-prompago = ? THEN
                v-prompago = 0.
            
            l-fecdep = ?.
            l-acuse = "".
            l-ImpPago = 0.
            FOR EACH acuse WHERE acuse.id-cliente = cliente.id-cliente NO-LOCK:
               IF (l-fecdep = ? OR acuse.fecdep > l-fecdep) AND 
                  acuse.estatus <> 3 THEN DO:
                  l-SumaDoc = 0.
                  IF Acuse.Tipo <> "A" THEN DO:
                     FOR EACH docacuse WHERE docacuse.id-acuse = acuse.id-acuse NO-LOCK:
                         l-SumaDoc = l-SumaDoc + docacuse.imppago.
                     END.
                  END.
                  ELSE DO:
                     FOR EACH PagoAcuse WHERE PagoAcuse.Id-acuse = Acuse.id-acuse 
                             NO-LOCK:
                         l-SumaDoc = l-SumaDoc + PagoAcuse.Importe.
                     END.
                  END.
                  IF l-SumaDoc > 0 THEN
                     ASSIGN l-acuse = acuse.id-acuse
                            l-fecdep = acuse.fecdep
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
                  IF l-SumaDoc > 0 AND (l-fecdep = ? OR acuse.fecdep > l-fecdep) THEN 
                     ASSIGN l-acuse = acuse.id-acuse
                            l-fecdep = acuse.fecdep
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
                  IF l-SumaDoc > 0 AND (l-fecdep = ? OR acuse.fecdep > l-fecdep) THEN 
                     ASSIGN l-acuse = acuse.id-acuse
                            l-fecdep = acuse.fecdep
                            l-ImpPago = l-SumaDoc.
                END.
                 
            END.
            
            /* Calcula resumen de cheques */
            FIND CURRENT bfPedido EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            ASSIGN vTotChDep = 0
                   vtotfac = 0
                   vefecto = 0
                   vchnodep = 0
                   vtotped = 0.
            FOR EACH Acuse WHERE Acuse.Id-cliente = bfPedido.id-cliente
                             AND Acuse.FecDep >= (TODAY - 30)
                             AND Acuse.FecDep <= TODAY
                             AND Acuse.Estatus = 4 NO-LOCK:
                ASSIGN l-Importe = 0.
                FOR EACH Pagoacuse OF acuse NO-LOCK:
                    ASSIGN l-importe = l-importe + pagoacuse.importe.
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
            
            FOR EACH Acuse WHERE Acuse.Id-cliente = bfPedido.id-cliente
                             AND Acuse.Estatus < 3 NO-LOCK:
                ASSIGN l-Importe = 0.
                FOR EACH Pagoacuse OF acuse NO-LOCK:
                    ASSIGN l-importe = l-importe + pagoacuse.importe.
                END.
                ASSIGN vChNoDep = vChNoDep + l-Importe.
            END.
            FOR EACH CheDev WHERE CheDev.Id-Cliente = bfPedido.id-cliente
                              AND CheDev.FecCargo >= (TODAY - 30)
                              AND CheDev.FecCargo <= TODAY
                              AND CheDev.FecCanc = ? NO-LOCK BY CheDev.FecCargo:
                ASSIGN vTotChDep = vTotChDep - CheDev.ImpCheque.
            END.
            FOR EACH factura WHERE factura.id-cliente = bfPedido.id-cliente
                               AND factura.fecreg >= (TODAY - 30)
                               AND factura.fecreg <= TODAY
                               AND factura.feccanc = ? NO-LOCK:
                ASSIGN vTotFac = vTotFac + Factura.Tot.
            END.
            
            FOR EACH estpedido WHERE estpedido.id-cliente = bfPedido.id-cliente
                                 AND estpedido.estatus < 5 NO-LOCK,
                EACH Detpedido WHERE detpedido.id-pedido = estpedido.id-pedido
                                 AND detpedido.resto = estpedido.id-seq NO-LOCK:
                ACCUMULATE vTotPed = vTotPed + DetPedido.Importe + DetPedido.Iva.
            END.
            
            ASSIGN vEfecto = (vTotFac + vTotPed) - vTotChDep.
            /**/
            
            ASSIGN
                 v-asunto    = "AVISO AUTOMATICO DE AUTORIZACION DE CREDITO (cxcd1662-M)"
                 v-contenido = "<html><head>" + CHR(13) +
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) +
                               "style='font-size:10.0pt;font-family:Verdana'>" + 'Se realizo una autorizacion del pedido Num: ' + v-pedido + ', por ' + usuario.nom-usuario + '.' + "<o:p></o:p></span></p>" + CHR(13) +
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + 'Fecha y hora.....:   ' + STRING(TODAY,"99/99/9999") + ' ' + STRING(v-hora,"hh:mm:ss")  + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +     
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Cliente..........:   "  + STRING(bfPedido.id-cliente) + " " + (IF AVAILABLE cliente THEN cliente.razonsocial ELSE "No disponible") + "<o:p></o:p></span></p>" + CHR(13) +
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Poblacion........:   " + (IF AVAILABLE Ciudad THEN TRIM(ciudad.nombre) ELSE "") + ", " + (IF AVAILABLE estado THEN TRIM(estado.nombre) ELSE "") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + 
                               "Motivo...........:   " + (IF ipImporte > 50000
                                                          THEN "COMPRA MAYOR A $50,000 "
                                                          ELSE "") +
                                                         (IF ((ipImporte + vSaldo) >= v-limitemaximo)
                                                          THEN "CLIENTE EXCEDE MAS DE UN 50% SU LIMITE DE CREDITO "
                                                          ELSE "") +
                                                         (IF vSaldov30 > 0 THEN "CLIENTE CON MAS DE 30 DIAS VENCIDO"
                                                          ELSE "") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" +                                                    
                               "Plazo............:   " + STRING(cliente.plazo) + " dias"                               + CHR(13) + 
                               "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + 
                               "Solicita.........:  $" + TRIM(STRING(ipImporte,"-ZZZ,ZZZ,ZZ9.99"))                + CHR(13) +
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
            
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Efecto en cartera:  $" + STRING(vEfecto,"-ZZZ,ZZZ,ZZ9.99")   + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                       
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Ch NO Depositados:  $" + STRING(vChNoDep,"-ZZZ,ZZZ,ZZ9.99")  + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Saldo Act........:  $" + STRING(vSaldo,"-ZZZ,ZZZ,ZZ9.99")    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) + 
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + " * Con Factura...:  $" + STRING(v-saldoped,"-ZZZ,ZZZ,ZZ9.99")      + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "       * Venc -30:  $" + STRING(vSaldovenc - vSaldov30,"-ZZZ,ZZZ,ZZ9.99") + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                        
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "       * Venc +30:  $" + STRING(vSaldov30,"-ZZZ,ZZZ,ZZ9.99")                    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "       * P/Ven...:  $" + STRING(v-saldoped - vSaldovenc,"-ZZZ,ZZZ,ZZ9.99")      + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + " * Por Facturar..:  $" + STRING(vPedSaldo,"-ZZZ,ZZZ,ZZ9.99")                    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Limite Credito...:  $" + STRING(v-limite2,"-ZZZ,ZZZ,ZZ9.99")                          + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +                        
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Cred. Disponible.:  $" + STRING(v-disponible,"-ZZZ,ZZZ,ZZ9.99")                       + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Dias Cartera.....:   " + STRING(v-dc,"-ZZZ,ZZZ,ZZ9")                                  + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Promedio Pago....:   " + STRING(v-prompago,"-ZZZ,ZZZ,ZZ9")                            + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Factor Descto....:   " + STRING(cliente.FactorDes,"-ZZZ,ZZZ,ZZ9.99 %")                + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) + CHR(13) +
                               
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "Vendedor.........:   " + STRING(v-nomvend,"X(20)")                                    + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) + CHR(13) + CHR(13) +
                                                       
                               "<p class=MsoNormal align=center style='text-align:left'><span" + CHR(13) + "style='font-size:10.0pt;font-family:Verdana'>" + "(cxcd1662.i)"          + CHR(13) + "<o:p></o:p></span></p>" + CHR(13) +
                                                                             
                               
                               '<meta http-equiv="Content-Type" content="text/html; charset=windows-1252">' + CHR(13) +
                               "</head>".
                               
            IF {/usr2/adosa/includes/sist0001.i} = "DESARROLLO" THEN 
                ASSIGN v-Para = "crivera@adosa.com.mx".
            ELSE DO:
                v-Para = (IF ipImporte >= 5000 AND vEfecto >= 3000 THEN 'agomez@adosa.com.mx; ogomez@adosa.com.mx; ' ELSE '') +
                         'dgarza@adosa.com.mx; jgonzalez@adosa.com.mx' +
                         (IF Pedido.Id-Alm = '11' THEN '; jlopez@adosa.com.mx' ELSE '') +
                         (IF Pedido.Id-Alm = '12' THEN '; hterrazas@adosa.com.mx; jge@adosa.com.mx' ELSE '').
            END.
                               
            {/usr2/adosa/includes/inva0007.i
                &Asunto     = "v-asunto"
                &contenido  = "v-contenido"
                &Iniciales  = "'JAGR'"
                &Direccion  = "v-Para"
                &Refer      = "'DIRECTO'"
                &Attachment = ""
            }
            
            
        END.
         
    END.
    RELEASE Folio.
    RELEASE Pedido.
END PROCEDURE.
