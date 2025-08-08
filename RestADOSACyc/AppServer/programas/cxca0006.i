/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : General
  Programa : cxca0006.i
  Funcion  : Rutina para afectar los Clientes
  Autor    : IOC
  Fecha    : 04-12-1996 */

FIND EstCte WHERE
     EstCte.Id-Cliente = {&Cliente} AND
     EstCte.Anio       = YEAR({&Fecha}) EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAILABLE (EstCte) THEN DO:
   CREATE EstCte.
   ASSIGN EstCte.Id-Cliente = {&Cliente}
          EstCte.Anio       = YEAR({&Fecha}).
END.


IF {&renglon} = 1 THEN
   ASSIGN EstCte.VentasCo[MONTH({&fecha})] =
          EstCte.VentasCo[MONTH({&fecha})] + {&Importe}.

   ELSE IF {&renglon} = 2 THEN
       ASSIGN EstCte.VentasCr[MONTH({&fecha})] =
              EstCte.VentasCr[MONTH({&fecha})] + {&Importe}.

     ELSE IF {&renglon} = 3 THEN
         ASSIGN EstCte.AbonoCo[MONTH({&fecha})] =
                EstCte.AbonoCo[MONTH({&fecha})] + {&Importe}.

       ELSE IF {&renglon} = 4 THEN
            ASSIGN EstCte.AbonoCr[MONTH({&fecha})] =
                   EstCte.AbonoCr[MONTH({&fecha})] + {&Importe}.

          ELSE IF {&renglon} = 5 THEN
              ASSIGN EstCte.Cargos[MONTH({&fecha})] =
                     EstCte.Cargos[MONTH({&fecha})] + {&Importe}.

             ELSE IF {&renglon} = 6 THEN
                    ASSIGN EstCte.CheDev[MONTH({&fecha})] =
                           EstCte.CheDev[MONTH({&fecha})] + {&Importe}.

                 ELSE IF {&renglon} = 7 THEN
                      ASSIGN EstCte.AbonoNC[MONTH({&fecha})] =
                             EstCte.AbonoNC[MONTH({&fecha})] + {&Importe}.

                    ELSE IF {&renglon} = 8 THEN
                         ASSIGN EstCte.AbonoCh[MONTH({&fecha})] =
                                EstCte.AbonoCh[MONTH({&fecha})] + {&Importe}.

{ifdef {&subtotal}}
  IF {&renglon} = 1 THEN
      ASSIGN EstCte.VentaBrCO[MONTH({&fecha})] =
             EstCte.VentaBrCO[MONTH({&fecha})] + {&subtotal}.
  ELSE
      ASSIGN EstCte.VentaBrCR[MONTH({&fecha})] =
             EstCte.VentaBrCR[MONTH({&fecha})] + {&subtotal}.
{endif} */    
