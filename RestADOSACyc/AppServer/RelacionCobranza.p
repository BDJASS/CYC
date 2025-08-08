@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
    Empresa : ADOSA   [POST]
    Programa: cobranza.p
              /RelacionCobranza
    D_Adosa_EspecificReq_CxC_HU04_RelaciondeCobranza_V1.0
    Fucnion : Graba tablas de relaciones de cobranza
    Autor   : ALEX
    Fecha   : 4 de Diciembre DEL 2024
*/

/*
    Empresa : ADOSA  [GET]
    Programa: reimpcobranza.p
    Fucnion : Reimprime relaciones de cobranza
    Autor   : ALEX 
    Fecha   : 4 de Diciembre DEL 2024
*/
/*
    Empresa : ADOSA [PUT]
    Programa: guardacobobserv.p
    Fucnion : Graba Observaciones y fecha de alta de observaciones de las relaciones de cobranza
    Autor   : ALEX
    Fecha   : 4 de Diciembre DEL 2024
    
    
    -----------------------------------------------------------------------
     /RelacionCobranza
     
     
     ----------------------------------------------------------------------
*/


DEFINE TEMP-TABLE ttDatos
    FIELD IdCliente  LIKE RelCob.Id-Cliente 
    FIELD IdCobrador LIKE Cobrador.Id-Cobrador
    FIELD IdIndCob   LIKE IndCob.Id-IndCob
    FIELD IdRelCob   LIKE RCob.Id-RelCob   
    FIELD Observ     LIKE RelCob.Observ.   
    
DEFINE BUFFER bfMovCliente FOR MovCliente.

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE AltaObsRelacionCobranza:
    /*------------------------------------------------------------------------------
     Purpose: Graba Observaciones y fecha de alta de observaciones de las relaciones de cobranza
     Notes: Servicio tipo PUT
    ------------------------------------------------------------------------------*/
 
    DEFINE INPUT PARAMETER TABLE FOR ttDatos.
    DEFINE INPUT PARAMETER ipUser LIKE Usuario.Id-User NO-UNDO.

    DO TRANSACTION:   
        FOR EACH ttDatos NO-LOCK:
            FIND FIRST RCob WHERE RCob.Id-RelCob = ttDatos.IdRelCob EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE RCob THEN 
            DO:
                ASSIGN 
                    RCob.HorRec = TIME 
                    RCob.UsrRec = ipUser
                    RCob.FecRec = TODAY.     
            END. 
        
            FIND FIRST RelCob WHERE RelCob.Id-RelCob = ttDatos.IdRelCob
                AND RelCob.Id-Cliente = ttDatos.IdCliente EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE RelCob THEN 
            DO:
                ASSIGN 
                    RelCob.Observ = ttDatos.Observ.
            END. 
        END.
    END.

    RETURN.

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostCobranza:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Servicio tipo POST
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttDatos.
    DEFINE INPUT PARAMETER ipUser LIKE Usuario.Id-User NO-UNDO.
    DEFINE OUTPUT PARAMETER opMensaje AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opFolio AS INTEGER NO-UNDO.
    //DEFINE OUTPUT PARAMETER opArchivo AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE vSaldo AS DECIMAL NO-UNDO.
    DEFINE VARIABLE l-i AS INTEGER NO-UNDO.
    
    DO TRANSACTION:
        FIND Folio WHERE Folio.Id-Doc = 'RelCob' EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Folio THEN DO:
            opMensaje = 'No existe el Folio para el tipo de documento'. 
            RETURN.                 
        END.

        ASSIGN
            opFolio     = Folio.Folio
            Folio.Folio = Folio.Folio + 1
            l-i         = 1.
               
        FOR EACH ttDatos NO-LOCK BREAK BY ttDatos.IdCliente:
            FIND Cliente WHERE Cliente.Id-Cliente = ttDatos.IdCliente NO-LOCK NO-ERROR.
            IF l-i = 1 THEN DO:
                CREATE RCob.
                ASSIGN 
                    RCob.Id-RelCob   = opFolio
                    RCob.UsrReg      = ipUser // ttDatos.IdUser  
                    RCob.FecReg      = TODAY
                    RCob.horReg      = TIME
                    Rcob.Id-Cobrador = ttDatos.IdCobrador
                    l-i              = l-i + 1.
            END.
            CREATE RelCob.
            ASSIGN 
                RelCob.Id-RelCob  = opFolio
                RelCob.Id-Cliente = ttDatos.IdCliente
                RelCob.Id-IndCob  = ttDatos.IdIndCob.
        
            /* FACTURAS Y CONTRARECIBOS */       
            FOR EACH MovCliente WHERE MovCliente.Id-Cliente = ttDatos.IdCliente
                AND MovCliente.Id-MC <= 3
                AND MovCliente.Saldo > 0 NO-LOCK BY MovCliente.RefSaldo:
                ASSIGN 
                    vSaldo = MovCliente.Importe.
                FOR EACH bfMovCliente WHERE bfMovCliente.Refsaldo = MovCliente.Refsaldo
                    AND bfMovCliente.Afectado = FALSE 
                    AND bfMovCliente.Id-MC > 3 NO-LOCK:
                    ASSIGN 
                        vSaldo = vSaldo + bfMovCliente.Importe.
                END.
                IF vSaldo >= 0 THEN DO:                  
                    FIND Cliente OF MovCliente NO-LOCK NO-ERROR.
                    CREATE DetRCob.
                    ASSIGN 
                        DetRCob.Id-RelCob  = opFolio
                        DetRCob.Id-Cliente = MovCliente.Id-Cliente
                        DetRCob.Documento  = MovCliente.Documento
                        DetRCob.Descr      = Cliente.RazonSoc
                        DetRCob.Id-MC      = MovCliente.Id-MC
                        DetRCob.FecMov     = MovCliente.FecReg                       
                        DetRCob.Flag       = 'X'
                        DetRCob.Importe    = MovCliente.Importe
                        DetRCob.Saldo      = vSaldo.
                END.
            END.
              
        END.
    END.

    RELEASE Folio. 

    //RUN programas/pdfrelcob.p(INPUT opFolio,OUTPUT opArchivo). 
    
    //UNIX SILENT VALUE("chmod 777 /tmp/" + opArchivo).

    ASSIGN 
        opMensaje = "Folio generado: " + STRING(opFolio).

    RETURN.     

END PROCEDURE.    

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ReImpresionCobranza:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Servicio Post porque genera PDF
    ------------------------------------------------------------------------------*/
    /*
        Empresa : ADOSA
        Programa: reimpcobranza.p
        Fucnion : Reimprime relaciones de cobranza
        Autor   : ALEX
        Fecha   : 4 de Diciembre DEL 2024 
    */ 
    DEFINE INPUT PARAMETER ipRelCob LIKE RCob.Id-RelCob NO-UNDO. 
    DEFINE OUTPUT PARAMETER opArchivo AS CHARACTER NO-UNDO.      

    RUN programas/pdfrelcob.p(INPUT ipRelCob,OUTPUT opArchivo).             
    UNIX SILENT VALUE("chmod 777 /tmp/" + opArchivo). 
    RETURN.    
END PROCEDURE.      

