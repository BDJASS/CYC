@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : Se usa para las facturas de aplicacion de Pagos
    Purpose     : facturas de credito.

    Syntax      : 

    Description : Nos basamos en el programa de ayuda de documentos credito 
                  que nos dio el Ing cxcf0171.p

    Author(s)   : sis10
    Created     : Tue Jan 28 11:50:25 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

  

DEFINE TEMP-TABLE ttFactura NO-UNDO
    FIELD Documento        AS CHARACTER
    FIELD Descripcion      AS CHARACTER
    FIELD Registro         AS DATE
    FIELD Vencimiento      AS DATE
    FIELD Importe          AS DECIMAL
    FIELD PorcIVA          LIKE sysgeneral.Porc-IVA
    FIELD Dias             AS INT
    FIELD Saldo            AS DECIMAL .   


/* **********************  Internal Procedures  *********************** */


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetFacturas:
    DEFINE INPUT  PARAMETER pCliente AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER piRango  AS INTEGER NO-UNDO INITIAL ?. /* 30,60,90,91 */
    DEFINE OUTPUT PARAMETER TABLE FOR ttFactura.

    EMPTY TEMP-TABLE ttFactura.
    DEF VAR l-saldo               LIKE MovCliente.Saldo     NO-UNDO.
    DEF VAR l-dias                AS INTEGER                NO-UNDO.
    DEF VAR i                     AS INTEGER.
    DEFINE VARIABLE vDias AS INTEGER NO-UNDO.
    DEF BUFFER MovCliente_bf FOR MovCliente.
    
    FOR EACH MovCliente WHERE
        MovCliente.Id-MC <= 3 AND
        MovCliente.Saldo > 0  AND Movcliente.id-cliente = pCliente NO-LOCK
        USE-INDEX idx-mov :

        ASSIGN l-saldo = MovCliente.Saldo.
        FOR EACH MovCliente_bf
             WHERE MovCliente_bf.RefSaldo = MovCliente.Refsaldo AND
               MovCliente_bf.Afectado  = FALSE NO-LOCK :
         ASSIGN l-Saldo = l-Saldo + MovCliente_bf.importe.
        END.
        FOR EACH DocAcuse WHERE DocAcuse.Documento = MovCliente.Documento NO-LOCK :
        ASSIGN l-saldo = l-saldo - DocAcuse.ImpPago -
                   DocAcuse.ImpDevol - DocAcuse.ImpDescPP
                  - DocAcuse.ImpDescAdc - DocAcuse.ImpDescEsp.
        END.
        
        IF l-saldo > 0 THEN DO: 
             FIND TabMC OF MovCliente NO-LOCK NO-ERROR.
             vDias = TODAY - MovCliente.FecVenc.     
            CREATE ttFactura.   
            ASSIGN
                ttFactura.Documento   = MovCliente.RefSaldo  
                ttFactura.Descripcion = IF AVAILABLE TabMC THEN TabMC.Descr ELSE ""
                ttFactura.Registro    = MovCliente.FecReg
                ttFactura.Vencimiento = MovCliente.FecVen
                ttFactura.Importe     = MovCliente.Importe
                ttFactura.Dias        = vDias
                ttFactura.Saldo       = MovCliente.Saldo. 
       END.     
    END.    
END PROCEDURE.  