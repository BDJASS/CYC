/*
  Funcion  : Genera Movimiento de Cliente desde backend
  Autor    : IOC
  Fecha    : 11 DIC 2024 
*/

IF {&TipoPadre} <> {&TipoMov} THEN DO:
  FIND FIRST adosa.MovCliente WHERE
             adosa.MovCliente.RefSaldo    = {&RefSaldo}  AND
             adosa.MovCliente.Id-MC       = {&TipoPadre} AND
             adosa.MovCliente.Documento   = {&Refsaldo}
             NO-LOCK NO-ERROR.

  IF AVAILABLE Adosa.MovCliente THEN DO:
    ASSIGN l-recmov = RECID(adosa.MovCliente).
    FIND adosa.MovCliente WHERE RECID(adosa.MovCliente) = l-recmov 
				EXCLUSIVE-LOCK  NO-WAIT  NO-ERROR.    
    IF NOT AVAILABLE adosa.MovCliente THEN
        DO:
            ASSIGN   
                p-Acuse = "Existe un bloqueo por otra aplicacion en la BD".  
                
                   /* Log manual para saber quién es */
    LOG-MANAGER:WRITE-MESSAGE(
        SUBSTITUTE(
            "Bloqueo detectado en MovCliente RefSaldo:&1 TipoPadre:&2 Cliente:&3",
            {&RefSaldo}, {&TipoPadre}, {&Cliente}  
        )
    ).     
            RETURN.
        END.     
				
    ASSIGN 
      adosa.MovCliente.Saldo     = adosa.MovCliente.Saldo + 
				   ( IF {&afectar} THEN
				      {&Importe} 
				     ELSE 0 )
	   l-fecvence           = adosa.MovCliente.FecVenc
	   l-Ubic               = adosa.MovCliente.Id-Ubic.
  END.
END.

CREATE adosa.MovCliente.

ASSIGN adosa.MovCliente.FecReg      = {&FecReg}
       adosa.MovCliente.Documento   = {&Documento}
       adosa.MovCliente.RefSaldo    = {&RefSaldo}
       adosa.MovCliente.Id-MC       = {&TipoMov}
       adosa.MovCliente.FecVenc     = l-fecvence
       adosa.MovCliente.RefSec      = ''
       adosa.MovCliente.Importe     = {&Importe}
       adosa.MovCliente.Id-Cliente  = {&Cliente}
       adosa.MovCliente.Afectado    = {&Afectar}
       adosa.MovCliente.Id-Ubic     = l-Ubic
       adosa.MovCliente.Id-Moneda   = {&Moneda}
       adosa.MovCliente.TipoCambio  = {&Cambio}.   
IF {&TipoPadre} = {&TipoMov} THEN
  ASSIGN adosa.MovCliente.Saldo     = {&Importe}.

