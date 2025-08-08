@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").


/*------------------------------------------------------------------------
    File        : depvisor2.p
    Purpose     : 

    Syntax      : /DepositoVisorPendiente

    Description : Se usa para Grafica Director Depositos; al dar clic en los Pdte
                  Por Aplicar, manda ese visor.

    Author(s)   : sis10
    Created     : Mon Oct 28 07:15:45 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num       AS INTEGER.
DEFINE VARIABLE v-resp      AS CHARACTER.
DEFINE VARIABLE v-clase     AS CHARACTER.
DEFINE VARIABLE v-dia       AS INTEGER.
DEFINE VARIABLE v-mes       AS INTEGER.
DEFINE VARIABLE imes       AS INTEGER.
DEFINE VARIABLE ianio      AS INTEGER.
DEFINE VARIABLE i-clase     AS INTEGER.
DEFINE VARIABLE l-lista     AS CHARACTER.
DEFINE VARIABLE v-tipo-cte  AS CHARACTER.
DEFINE VARIABLE v-documento   AS CHARACTER FORMAT "X(9)" .
DEFINE VARIABLE v-time      AS CHARACTER NO-UNDO.
DEFINE VARIABLE numStr      AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttDeposito NO-UNDO
    FIELD NumList         AS INTEGER
    FIELD Clase           AS CHARACTER FORMAT "X(30)" 
    FIELD Zona            AS CHARACTER FORMAT "X(30)"
    FIELD TipoDeCliente   AS CHARACTER FORMAT "X(30)"
    FIELD NumCliente      AS INTEGER
    FIELD Cliente         AS CHARACTER FORMAT "X(30)"
    FIELD FechaDeposito   AS DATE      FORMAT 99/99/9999
    FIELD Hora            AS INTEGER
    FIELD Banco           AS CHARACTER FORMAT "X(30)"
    FIELD Descripcion     AS CHARACTER FORMAT "X(30)"
    FIELD Importe         AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD FechaAplicacion AS DATE      FORMAT 99/99/9999
    FIELD Documento       AS CHARACTER FORMAT "X(30)"
    FIELD Anticipo        AS CHARACTER FORMAT "X(10)"
    FIELD Aplicado        AS LOGICAL   FORMAT "SI/NO"
    FIELD Responsable     AS CHARACTER FORMAT "X(30)"
    INDEX idx-respo FechaDeposito ASCENDING Hora ASCENDING.
DEFINE DATASET dsDeposito FOR ttDeposito.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDepositos:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


DEFINE OUTPUT PARAMETER TABLE FOR ttDeposito.

EMPTY TEMP-TABLE ttDeposito.

ASSIGN 
    l-lista = "1,2,4,5,6,7,8,9,10,11"
    l-num   = 0.

FOR EACH Depbanco WHERE Depbanco.Conciliado = FALSE 
                    AND DepBanco.Activo  NO-LOCK :                   
    FOR EACH Cliente WHERE Cliente.Id-Cliente = Depbanco.Id-Cliente 
        NO-LOCK BREAK BY DepBanco.Id-Cliente 
                      BY DepBanco.FecDep
                      BY DepBanco.HoraDep 
                      BY DepBanco.Conciliado
                      BY Cliente.Id-Cliente :
        IF LOOKUP(STRING (Cliente.Id-Cliente), SUBSTITUTE(l-lista,",",""))> 0 THEN NEXT.               
        FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.   
        FIND FIRST Resp     WHERE Resp.Id-Resp = Cliente.Id-Resp     NO-LOCK NO-ERROR.
        FIND FIRST Banco    WHERE Banco.Id-Banco = Depbanco.Id-Banco NO-LOCK NO-ERROR.
        
        IF DepBanco.TipoCte = 4  THEN NEXT.
        /* El director solo visualiza depositos de credito */ 
        ASSIGN 
            v-tipo-cte  = "Credito" 
            v-documento = DepBanco.Id-Acuse
            l-num       = l-num + 1.
        
               FIND FIRST ttDeposito WHERE ttDeposito.NumList = l-num NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttDeposito THEN                                        
                CREATE ttDeposito.                                                      
                ASSIGN                                                                  
                       ttDeposito.NumList         = l-num                               
                       ttDeposito.Clase           = ClaseCte.Descr WHEN AVAILABLE ClaseCte                    
                       ttDeposito.Zona            = " "                              
                       ttDeposito.TipoDeCliente   = v-tipo-cte                          
                       ttDeposito.NumCliente      = Cliente.Id-Cliente                  
                       ttDeposito.Cliente         = Cliente.RazonSocial                 
                       ttDeposito.FechaDeposito   = Depbanco.FecDep                     
                       ttDeposito.Hora            = DepBanco.HoraDep                   
                       ttDeposito.Banco           = Banco.Nombre                        
                       ttDeposito.Descripcion     = Depbanco.Descripcion                
                       ttDeposito.Importe         = Depbanco.Importe                    
                       ttDeposito.FechaAplicacion = Depbanco.FecAplica                  
                       ttDeposito.Documento       = v-documento                            
                       ttDeposito.Anticipo        = Depbanco.Id-AcuseAnt                                 
                       ttDeposito.Aplicado        = Depbanco.Conciliado                 
                       ttDeposito.Responsable     = Resp.Nombre WHEN AVAILABLE Resp.                        
                       RELEASE ttDeposito.                                              
    END. 
END.
END PROCEDURE.

