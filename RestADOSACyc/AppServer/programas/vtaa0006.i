/*
  Empresa: Abastecedora de Oficinas
  Sistema: Adosa
  Modulo : Ventas
  Programa: vtaa0006.i
  Funcion: Descompromete Inventario dandolo de Baja
  Usado por: vtaa0120.p (entre otros)
  Autor: DCH
  Fecha: 
*/

blk-Des:
DO :
  FIND Artubic WHERE ArtUbic.Id-Articulo = DetPedido.Id-Articulo AND
                     ArtUbic.Id-Color = DetPedido.Id-Color AND
                     ArtUbic.Id-Alm = DetPedido.Id-Alm EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE ArtUbic THEN DO:
     ASSIGN ArtUbic.Exist  = ArtUbic.Exist + DetPedido.CantCom
            ArtUbic.BO     = ArtUbic.BO - DetPedido.CantBO
            ArtUbic.Compro = ArtUbic.Compro - DetPedido.CantCom.
  END.
  RELEASE ArtUbic.
  ASSIGN DetPedido.CantCom = 0.
END.
