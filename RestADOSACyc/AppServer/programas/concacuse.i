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
				EXCLUSIVE-LOCK NO-ERROR.
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
       adosa.MovCliente.Id-Moneda   = 1
       adosa.MovCliente.TipoCambio  = 1.
IF {&TipoPadre} = {&TipoMov} THEN
  ASSIGN adosa.MovCliente.Saldo     = {&Importe}.

