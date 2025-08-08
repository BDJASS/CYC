@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : reporteanticipos.p
    Purpose     : 

    Syntax      :/ReporteAnticipos

    Description : 

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttAnticipo NO-UNDO
    FIELD IdAnticipo  AS CHARACTER
    FIELD Fecha          AS DATE
    FIELD Acuse          AS CHARACTER
    FIELD Cliente        AS INT
    FIELD ClienteNombre  AS CHAR
    FIELD ImporteAnticipo AS DECIMAL
    FIELD ImporteAplicado AS DECIMAL
    FIELD ImportePendiente AS DECIMAL.
    
DEF VAR l-aplicado  AS DECI FORMAT "zz,zzz,zz9.99"                NO-UNDO.
DEF VAR l-anticipo  AS DECI FORMAT "-z,zzz,zz9.99"                NO-UNDO.
DEF VAR l-pendiente AS DECI FORMAT "-z,zzz,zz9.99"                NO-UNDO.
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetConsultaAnticipo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iCliente  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER v-fecini  AS DATE NO-UNDO. 
    DEFINE INPUT PARAMETER v-fecfin  AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER Anticipo  AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttAnticipo.


    IF v-fecini = ? THEN v-fecini = TODAY - 1.
    IF v-fecfin = ? THEN v-fecfin = TODAY.
    IF iCliente = ? THEN iCliente = 0.
    IF Anticipo = ? THEN Anticipo = 0.  // 0 se trae todos anticipos y 1 solo Pendientes

/* Limpiar las tablas temporales */     
    EMPTY TEMP-TABLE ttAnticipo.


   FOR EACH Anticipo WHERE  NOT Anticipo.Canc           AND
                           Anticipo.FecReg >= v-fecini  AND
                           Anticipo.FecReg <= v-fecfin 
                           AND (iCliente = 0 OR Anticipo.Id-Cliente = iCliente) /* Filtro por cliente */ 
                           NO-LOCK BY Anticipo.Id-Anticipo:
                           
      l-Aplicado = 0.
      FOR EACH DetAnticipo WHERE DetAnticipo.Id-Anticipo = Anticipo.Id-Anticipo
                             AND DetAnticipo.FecReg <= v-fecfin
                             NO-LOCK:
          l-Aplicado = l-Aplicado + DetAnticipo.Importe.
      END.
      ASSIGN l-Pendiente = Anticipo.ImpAnticipo - l-Aplicado.
      
       /* Filtra solo si el parámetro Anticipo = 1 */
        IF Anticipo = 1 AND l-Pendiente <= 0 THEN 
            NEXT.  /* Omite anticipos sin saldo pendiente */

     FIND Cliente OF Anticipo NO-LOCK NO-ERROR. 
     
     CREATE ttAnticipo.
      ASSIGN
           ttAnticipo.IdAnticipo       = Anticipo.Id-Anticipo 
           ttAnticipo.Fecha            = Anticipo.FecReg 
           ttAnticipo.Acuse            = Anticipo.Id-Acuse
           ttAnticipo.ImporteAnticipo  = Anticipo.ImpAnticipo
           ttAnticipo.ClienteNombre    = IF AVAILABLE Cliente THEN Cliente.RazonSocial ELSE ""
           ttAnticipo.Cliente          = anticipo.Id-Cliente
           ttAnticipo.ImporteAplicado  = l-Aplicado          
           ttAnticipo.ImportePendiente = l-Pendiente .       
    END.
    RETURN.
END PROCEDURE.


