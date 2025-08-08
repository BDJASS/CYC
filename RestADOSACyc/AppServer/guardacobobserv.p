/*
    Empresa : ADOSA
    Programa: guardacobobserv.p
    Fucnion : Graba Observaciones y fecha de alta de observaciones de las relaciones de cobranza
    Autor   : ALEX
    Fecha   : 4 de Diciembre DEL 2024
*/

DEFINE TEMP-TABLE ttDatos
    FIELD IdCliente LIKE RelCob.Id-Cliente
    FIELD IdRelCob LIKE RCob.Id-RelCob
    FIELD IdIndCob LIKE IndCob.Id-IndCob
    FIELD Observ LIKE RelCob.Observ
    FIELD IdUser AS CHARACTER.
    
DEFINE INPUT PARAMETER TABLE FOR ttDatos.

DO TRANSACTION:
    FOR EACH ttDatos NO-LOCK:
        FIND FIRST RCob WHERE RCob.Id-RelCob = ttDatos.IdRelCob EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE RCob THEN DO:
            ASSIGN RCob.FecREg = TODAY 
                   RCob.HorReg = TIME 
                   RCob.UsrRec = ttDatos.IdUser.
        END. 
        
        FIND FIRST RelCob WHERE RelCob.Id-RelCob = ttDatos.IdRelCob
                            AND RelCob.Id-Cliente = ttDatos.IdCliente EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE RelCob THEN DO:
            ASSIGN RelCob.Observ = ttDatos.Observ.
        END. 
    END.
END.

RETURN.