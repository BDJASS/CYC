/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : Tesoreria
  Programa : Cxca0001.i
  Funcion  : Genera Movimiento de Cliente
  Autor    : IOC
  Fecha    : 20-08-96
  Modificacion: RNPC - Se agrega la asignación de Moneda y TipoCambio a la tabla de MovCliente.
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
       adosa.MovCliente.FecVenc     =
            {ifndef {&FecVence}} l-fecvence
            {else} */ {&FecVence}
            {endif}*/
       adosa.MovCliente.RefSec      =
            {ifndef {&RefId}} ''
            {else} */ {&RefId}
            {endif}*/
       adosa.MovCliente.Importe     = {&Importe}
       adosa.MovCliente.Id-Cliente  = {&Cliente}
       adosa.MovCliente.Afectado    = {&Afectar}
       adosa.MovCliente.Id-Ubic     =
            {ifndef {&Ubic}} l-Ubic
            {else} */ {&Ubic}
            {endif}*/
       adosa.MovCliente.Id-Moneda   =
            {ifndef {&Moneda}} 1
            {else} */ {&Moneda}
            {endif}*/
       adosa.MovCliente.TipoCambio   =
            {ifndef {&TipoCambio}} 1
            {else} */ {&TipoCambio}
            {endif}*/.
IF {&TipoPadre} = {&TipoMov} THEN
  ASSIGN adosa.MovCliente.Saldo     = {&Importe}.   

