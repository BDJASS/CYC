/*
  Programa: inva0007.i
  Funcion : Da de Alta un e-mail para un destinatario
*/

DO TRANSACTION:
    CREATE Adosa.e-mail.
    ASSIGN Adosa.e-mail.Iniciales  = {ifdef {&Iniciales}} {&Iniciales} {else} */ userid("dictdb") {endif} */
           Adosa.e-mail.Direccion  = {&Direccion}
           Adosa.e-mail.Asunto     = {ifdef {&Asunto}} {&Asunto} {else} */ "" {endif} */
           Adosa.e-mail.Contenido  = {ifdef {&Contenido}} {&Contenido} {else} */ "" {endif} */
           Adosa.e-mail.Attachment = {ifdef {&Attachment}} {&Attachment} {else} */ "" {endif} */   
           Adosa.e-mail.copia      = {ifdef {&copia}} {&copia} {else} */ FALSE {endif} */   
           Adosa.e-mail.refer      = {ifdef {&refer}} {&refer} {else} */ "" {endif} */   
           Adosa.e-mail.Id-Refer   = {ifdef {&idrefer}} {&idrefer} {else} */ 0 {endif} */ 
           Adosa.e-mail.fecreg     = TODAY
           Adosa.e-mail.hora       = TIME.
END.
RELEASE Adosa.e-mail.
