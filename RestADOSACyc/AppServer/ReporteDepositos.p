@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : ReporteDepositos.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Mon Feb 17 12:16:38 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num         AS INTEGER.   
DEFINE VARIABLE l-lista       AS CHARACTER.
DEFINE VARIABLE v-tipo-cte    AS CHARACTER.
DEFINE VARIABLE v-documento   AS CHARACTER FORMAT "X(9)" .
DEFINE VARIABLE v-documento2   AS CHARACTER FORMAT "X(9)" .
DEFINE VARIABLE l-desactivado AS LOGIC.
DEFINE VARIABLE l-conciliado  AS CHAR.
DEFINE VARIABLE l-FormaPago   AS CHAR.

DEFINE VARIABLE l-pendientes  AS LOGICAL   NO-UNDO INITIAL FALSE.



DEFINE TEMP-TABLE ttDeposito NO-UNDO
    FIELD Clase           AS CHARACTER FORMAT "X(30)"  
    FIELD TipoDeCliente   AS CHARACTER FORMAT "X(30)"
    FIELD NumCliente      AS INTEGER
    FIELD Cliente         AS CHARACTER FORMAT "X(30)"
    FIELD FechaDeposito   AS DATE      FORMAT 99/99/9999
    FIELD Hora            AS CHAR
    FIELD Banco           AS CHARACTER FORMAT "X(30)"
    FIELD Descripcion     AS CHARACTER FORMAT "X(30)"
    FIELD Importe         AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD FechaAplicacion AS DATE      FORMAT 99/99/9999
    FIELD Documento       AS CHARACTER FORMAT "X(30)"
    FIELD FechaAF         AS DATE      FORMAT 99/99/9999
    FIELD AF              AS CHARACTER FORMAT "X(10)"
    FIELD Aplicado        AS LOGICAL   FORMAT "SI/NO"
    FIELD Anticipo        LIKE DepBanco.Id-AcuseAnt
    INDEX idx-respo FechaDeposito DESCENDING    Hora DESCENDING
    INDEX Idx-Dep   NumCliente    FechaDeposito DESC. 


DEFINE VARIABLE l-RecID        AS RECID   NO-UNDO.
DEFINE VARIABLE l-impMov       LIKE MovCliente.Importe NO-UNDO.
DEFINE VARIABLE l-impDevMov    LIKE MovCliente.Importe NO-UNDO.
DEFINE VARIABLE l-coincide     AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-impPaso      AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE l-impAntesDesc LIKE MovCliente.Importe NO-UNDO. 
DEF VAR v-hora      AS CHAR NO-UNDO.
DEFINE BUFFER bf_DepBanco      FOR DepBanco.
DEFINE BUFFER bf-MovCliente    FOR MovCliente.  

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDepositos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER iClase     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER v-fecini  AS DATE NO-UNDO. 
    DEFINE INPUT PARAMETER v-fecfin  AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER Tipo     AS INTEGER NO-UNDO.

    DEFINE OUTPUT PARAMETER TABLE FOR ttDeposito.  


    EMPTY TEMP-TABLE ttDeposito.


    ASSIGN  
        l-lista = "1,2,4,5,6,7,8,9,10,11"  // el Ing Francisco comento Omitir Estos clientes
        l-num   = 0.
    
    /* Manejar fechas nulas */
    IF v-fecini = ? THEN v-fecini = TODAY.
    IF v-fecfin = ? THEN v-fecfin = TODAY.   

    IF Tipo = ? THEN Tipo = 0.
    IF iClase = ? THEN iClase = 0.


FOR EACH DepBanco
    WHERE DepBanco.Id-Cliente >= 0   
      AND DepBanco.FecDep >= v-fecini 
      AND DepBanco.FecDep <= v-fecfin
      AND DepBanco.Activo  /* Filtra por Activo si no es ? */
      AND DepBanco.Conciliado  // SOLO APLICADO 
      USE-INDEX idx-DepBcoActivo NO-LOCK:

          
    /* Variables por asignar */
    ASSIGN 
        v-documento  = ""
        v-documento2 = ""
        v-tipo-cte   = " "
        l-num        = l-num + 1
        l-conciliado = ""
        v-hora       = "".
        
        
    
    /* Validación y búsqueda del cliente */
    FIND FIRST Cliente 
        WHERE Cliente.Id-Cliente = DepBanco.Id-Cliente
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cliente THEN NEXT. /* Si no encuentra el cliente, pasa al siguiente DepBanco */
    
    
    /* Verificar si el cliente está en l-lista */
    IF LOOKUP(STRING(Cliente.Id-Cliente), SUBSTITUTE(l-lista, ",", "")) > 0 THEN NEXT.
    
    /* Si iClase es 0, no se filtra. Si es 1, 2 o 3, solo clientes de esa clase */
    IF iClase <> 0 AND Cliente.Id-ClaseCte <> iClase THEN 
        NEXT.  /* Salta clientes que no coinciden con la clase */    
        
        
    /* Condición para Tipo = 0 (ambos tipos) */
    IF Tipo = 0 THEN 
    DO:
        /* Asigna según el TipoCte del registro actual */
        IF DepBanco.TipoCte <> 4 THEN 
            ASSIGN 
                v-tipo-cte  = "Credito"
                v-documento = DepBanco.Id-Acuse
                v-documento2 = DepBanco.Id-AcuseAnt.
        ELSE 
            ASSIGN 
                v-tipo-cte  = "Contado"
                v-documento = DepBanco.Id-Remision
                v-documento2 = DepBanco.Id-AcuseAnt.
    END.
    /* Condición original para Tipo = 1 (solo crédito) */
    ELSE IF Tipo = 2 THEN         
        DO: 
            IF DepBanco.TipoCte = 4 THEN NEXT. /* Salta si es crédito */
            ASSIGN 
                v-tipo-cte  = "Credito"
                v-documento = DepBanco.Id-Acuse
                v-documento2 = DepBanco.Id-AcuseAnt.
        END.
        /* Condición original para Tipo = 2 (solo contado) */
        ELSE IF Tipo = 1 THEN    
            DO: 
                IF DepBanco.TipoCte <> 4 THEN NEXT. /* Salta si no es crédito */
                ASSIGN 
                    v-tipo-cte  = "Contado"
                    v-documento = DepBanco.Id-Remision
                    v-documento2 = DepBanco.Id-AcuseAnt.
            END.
         
         v-hora = IF LENGTH(DepBanco.HoraDep) = 4 THEN SUBSTRING(STRING(DepBanco.HoraDep),1,2) + ':' + 
                           SUBSTRING(STRING(DepBanco.HoraDep),3,2)
                       ELSE '0' + SUBSTRING(STRING(DepBanco.HoraDep),1,1) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),2,2).
    /* Búsqueda de otras entidades relacionadas */
    FIND FIRST ClaseCte WHERE ClaseCte.Id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK NO-ERROR.
    FIND FIRST Banco    WHERE Banco.Id-Banco = DepBanco.Id-Banco NO-LOCK NO-ERROR.
    

        CREATE ttDeposito.
        ASSIGN
            ttDeposito.Clase           = IF AVAILABLE ClaseCte THEN ClaseCte.Descr ELSE "Sin Clase"
            ttDeposito.TipoDeCliente   = v-tipo-cte
            ttDeposito.NumCliente      = DepBanco.Id-Cliente
            ttDeposito.Cliente         = IF AVAILABLE Cliente THEN Cliente.RazonSocial ELSE ""
            ttDeposito.FechaDeposito   = DepBanco.FecDep
            ttDeposito.Hora            = v-hora     
            ttDeposito.Banco           = IF AVAILABLE Banco THEN Banco.Nombre ELSE ""
            ttDeposito.Descripcion     = DepBanco.Descripcion
            ttDeposito.Importe         = DepBanco.Importe
            ttDeposito.FechaAplicacion = DepBanco.FecAplica
            ttDeposito.Documento       = v-documento
            ttDeposito.FechaAF         = TODAY
            ttDeposito.AF              = ""
            ttDeposito.Aplicado        = DepBanco.Conciliado  
            ttDeposito.Anticipo        = v-documento2 .       
             
END. /* End del For Each DepBanco */ 
   
RETURN. 
END PROCEDURE.
 


