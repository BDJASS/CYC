/*
    Rechaza pedidos desde .Net
*/

DEFINE INPUT PARAMETER ipPedido AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipUser AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opError AS CHARACTER NO-UNDO.

DEFINE BUFFER b-Usuario FOR Usuario.

FIND FIRST Pedido WHERE Pedido.Id-Pedido = ipPedido EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE Pedido THEN DO:
    IF Pedido.Id-Liga BEGINS "F" THEN DO:
        ASSIGN opError = "El pedido " +
                         Pedido.Id-Pedido +
                         " se encuentra ligado en facturacion a otro(s) pedido(s), favor de eliminar la liga antes de cancelarlo".
        RETURN.
    END.
    IF Pedido.EnFirme = FALSE AND Pedido.Cancelado = FALSE AND INTEGER(SUBSTRING(Pedido.Id-Pedido,1,1)) < 5 THEN DO:
        DO TRANSACTION:
            FOR EACH BtcBajDetPed WHERE BtcBajDetPed.Id-Pedido = Pedido.Id-Pedido EXCLUSIVE-LOCK:
                DELETE BtcBajDetPed.
            END.

            FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = Pedido.Id-Vendedor NO-LOCK NO-ERROR.
            IF AVAILABLE Vendedor THEN DO:
                FIND FIRST Usuario WHERE Usuario.Id-User = Vendedor.Iniciales NO-LOCK NO-ERROR.
                IF AVAILABLE Usuario THEN DO:
                    FIND Cliente OF Pedido NO-LOCK NO-ERROR.
                    FIND b-Usuario WHERE b-usuario.Id-User = ipUser NO-LOCK NO-ERROR.
                    {/usr2/adosa/includes/inva0007.i
                        &Asunto     = "'Se le ha cancelado un pedido en creditos'"
                        &Contenido  = "'Pedido.......: ' + Pedido.Id-Pedido + CHR(10) +
                                       'Vendedor.....: ' + Usuario.Nom-Usuario + CHR(10) +
                                       'No. de Cuenta: ' + STRING(Pedido.Id-Cliente) + CHR(10) +
                                       'Razon Social.: ' + (IF AVAILABLE Cliente THEN Cliente.RazonSocial ELSE '') + CHR(10) +
                                       'Subtotal.....: ' + STRING(Pedido.SubTotal,'Z,ZZZ,ZZ9.99') + CHR(10) +
                                       'Fecha Pedido.: ' + STRING(Pedido.FecReg,'99/99/9999') + CHR(10) +
                                       'Hora Pedido..: ' + STRING(Pedido.HrSta,'HH:MM') + CHR(10) + CHR(10) +
                                       'Cancelado por: ' + b-Usuario.Nom-Usuario + CHR(10) +
                                       'Fecha Canc...: ' + STRING(TODAY,'99/99/9999') + CHR(10) +
                                       'Hora Canc....: ' + STRING(TIME,'HH:MM')"
                        &Iniciales  = ipUser
                        &Direccion  = "Usuario.e-Mail + ',mluna@adosa.com.mx'" 
                    }
                END.
            END.

            ASSIGN Pedido.Cancelado   = TRUE
                   Pedido.FecCancel   = TODAY
                   Pedido.UsuarioCanc = ipUser.
            IF Pedido.Id-Fuente <> 0 THEN DO:
                ASSIGN Pedido.FuenteSta = 1
                       Pedido.FuenteFec = TODAY
                       Pedido.FuenteHor = TIME.
            END.

            FOR EACH DetPedido WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido
                                 AND DetPedido.Resto = Pedido.Resto EXCLUSIVE-LOCK:
                IF Pedido.Id-Fuente = 1 THEN DO:
                    OUTPUT TO VALUE("/usr2/compartido/PA_" + TRIM(Pedido.OrdComCte) +
                                    "_" + TRIM(ENTRY(1,DetPedido.Filler-1)) + ".txt").
                    DISPLAY TRIM(Pedido.OrdComCte) + "|" + 
                            TRIM(ENTRY(1,DetPedido.Filler-1)) + "|1|" + 
                            STRING(YEAR(Pedido.FuenteFec),"9999") +
                            STRING(MONTH(Pedido.FuenteFec),"99") +
                            STRING(DAY(Pedido.FuenteFec),"99") + "|" +
                            "NO ACEPTADO POR DEPARTAMENTO DE CREDITOS" + "|" FORMAT "x(100)"
                    WITH WIDTH 120.
                    OUTPUT CLOSE.
                END.

                CREATE CancDetPed.
                BUFFER-COPY DetPedido TO CancDetPed.
                IF DetPedido.Tipo = 1 AND DetPedido.CantCom <> 0 THEN DO:
                    {/usr2/adosa/includes/vtaa0006.i}.
                END.
                DELETE DetPedido.
            END.
            RELEASE DetPedido.

            CREATE CancPed.
            BUFFER-COPY Pedido TO CancPed.
            ASSIGN CancPed.FecCancel = TODAY.
            
            CREATE LogBaja.
            ASSIGN LogBaja.Id-Pedido = Pedido.Id-Pedido
                   LogBaja.FecBaja = TODAY
                   LogBaja.Hora = TIME
                   LogBaja.Id-User = ipUser
                   LogBaja.Donde = ".Net".
            
            RUN /usr2/adosa/procs/embd0010.p(INPUT 2,
                                             INPUT Pedido.Id-Pedido,
                                             INPUT Pedido.Resto).
            
            DELETE Pedido.
            RELEASE Pedido.
        END.
    END.
    ELSE DO:
        ASSIGN opError = "El Pedido ya ha sido Cancelado".
        RETURN.
    END.
END.
ELSE DO:
    BELL.
    IF LOCKED(Pedido) THEN DO:
        ASSIGN opError = "Pedido Bloqueado por Otro Usuario".
        RETURN.
    END.
    ELSE DO:
        ASSIGN opError = "Pedido a sido borrado de la base de datos".
        RETURN.
    END.
END.
