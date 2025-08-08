@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
  Programa : ligapago.p
  Funcion  : es para ver las respuestas de santander 
  Autor    : 
  Fecha    : 
*/

DEF TEMP-TABLE ttMitResp
    FIELD IdPedido        LIKE MITResp.Id-Pedido        
    FIELD Resto           LIKE MITResp.Resto         
    FIELD monto           LIKE MITResp.monto         
    FIELD NumOperacion    LIKE MITResp.NumOperacion      
    FIELD Auth            LIKE MITResp.Auth      
    FIELD UserTrans       LIKE MITResp.UserTrans      
    FIELD emmail          LIKE MITResp.e-mail     
    FIELD Reference       LIKE MITResp.Reference      
    FIELD Response        LIKE MITResp.Response      
    FIELD FolioCPagos     LIKE MITResp.FolioCPagos      
    FIELD RAuth           LIKE MITResp.RAuth      
    FIELD CdResponse      LIKE MITResp.CdResponse      
    FIELD CdError         LIKE MITResp.CdError      
    FIELD NbError         LIKE MITResp.NbError      
    FIELD hora            LIKE MITResp.hora      
    FIELD Fecha           LIKE MITResp.Fecha      
    FIELD NbCompany       LIKE MITResp.NbCompany      
    FIELD NbMerchant      LIKE MITResp.NbMerchant      
    FIELD NbStreet        LIKE MITResp.NbStreet      
    FIELD CcType          LIKE MITResp.CcType      
    FIELD TpOperation     LIKE MITResp.TpOperation      
    FIELD CcName          LIKE MITResp.CcName      
    FIELD CcNumber        LIKE MITResp.CcNumber      
    FIELD CcExpMonth      LIKE MITResp.CcExpMonth      
    FIELD CcExpYear       LIKE MITResp.CcExpYear      
    FIELD Amount          LIKE MITResp.Amount      
    FIELD Voucher         LIKE MITResp.Voucher      
    FIELD VoucherComercio LIKE MITResp.VoucherComercio   
    FIELD VoucherCliente  LIKE MITResp.VoucherCliente   
    FIELD EmbKeyDate      LIKE MITResp.EmbKeyDate   
    FIELD NumberTkn       LIKE MITResp.NumberTkn   
    FIELD IdURL           LIKE MITResp.IdURL   
    FIELD eMail           LIKE MITResp.eMail   
    FIELD CCMask          LIKE MITResp.CCMask   
    FIELD Estatus         LIKE MITResp.Estatus .   
                         
    

    
/* **********************  Internal Procedures  *********************** */

/* Procedimiento REST */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetLigaPagoResp:

    DEFINE INPUT PARAMETER ip-preAcuse LIKE PreAcuse.Id-Acuse.
    DEFINE OUTPUT PARAMETER TABLE FOR ttMitResp.


    FOR EACH MITResp WHERE MITResp.Reference = ip-PreAcuse
        AND MITResp.Id-Pedido = "" NO-LOCK:
                   
        CREATE  ttMitResp.
        ASSIGN    
            ttMitResp.IdPedido        = MITResp.Id-Pedido          
            ttMitResp.Resto           = MITResp.Resto              
            ttMitResp.monto           = MITResp.monto              
            ttMitResp.NumOperacion    = MITResp.NumOperacion       
            ttMitResp.Auth            = MITResp.Auth               
            ttMitResp.UserTrans       = MITResp.UserTrans          
            ttMitResp.emmail          = MITResp.e-mail             
            ttMitResp.Reference       = MITResp.Reference          
            ttMitResp.Response        = MITResp.Response           
            ttMitResp.FolioCPagos     = MITResp.FolioCPagos        
            ttMitResp.RAuth           = MITResp.RAuth              
            ttMitResp.CdResponse      = MITResp.CdResponse         
            ttMitResp.CdError         = MITResp.CdError            
            ttMitResp.NbError         = MITResp.NbError            
            ttMitResp.hora            = MITResp.hora               
            ttMitResp.Fecha           = MITResp.Fecha              
            ttMitResp.NbCompany       = MITResp.NbCompany          
            ttMitResp.NbMerchant      = MITResp.NbMerchant         
            ttMitResp.NbStreet        = MITResp.NbStreet           
            ttMitResp.CcType          = MITResp.CcType             
            ttMitResp.TpOperation     = MITResp.TpOperation        
            ttMitResp.CcName          = MITResp.CcName             
            ttMitResp.CcNumber        = MITResp.CcNumber           
            ttMitResp.CcExpMonth      = MITResp.CcExpMonth         
            ttMitResp.CcExpYear       = MITResp.CcExpYear          
            ttMitResp.Amount          = MITResp.Amount             
            ttMitResp.Voucher         = MITResp.Voucher            
            ttMitResp.VoucherComercio = MITResp.VoucherComercio    
            ttMitResp.VoucherCliente  = MITResp.VoucherCliente     
            ttMitResp.EmbKeyDate      = MITResp.EmbKeyDate         
            ttMitResp.NumberTkn       = MITResp.NumberTkn          
            ttMitResp.IdURL           = MITResp.IdURL              
            ttMitResp.eMail           = MITResp.eMail              
            ttMitResp.CCMask          = MITResp.CCMask             
            ttMitResp.Estatus         = MITResp.Estatus.                
                       
    END.   
RETURN.
END PROCEDURE.

