/*
    Empresa:    ADOSA
    Programa:   bodb0122.i
    Funcion:    Genera Tareas de racks bajos
    Autor:      Alex
    Fecha:      5 de Mayo del 2008
*/
        
FIND Folio WHERE Folio.Id-Doc = 'Tarea'
             AND Folio.Id-Alm = g-Origen EXCLUSIVE-LOCK.

IF Folio.Folio >= 999999 THEN 
    ASSIGN 
        Folio.Folio = 0.

ASSIGN l-FolTar = Folio.Prefijo + STRING(Folio.Folio,'999999')
       Folio.Folio = Folio.Folio + 1.
       
RELEASE Folio.
CREATE Tarea.
ASSIGN Tarea.Id-Tarea  = l-FolTar
       Tarea.Area      = "CD"
       Tarea.Id-Alm    = g-Origen
       Tarea.Estatus   = IF g-origen = '02B' THEN 0 ELSE 1
       Tarea.FecReg    = TODAY
       Tarea.HReg      = TIME
       Tarea.Pasillo   = ReqAU.Pasillo
       Tarea.Prioridad = IF PROGRAM-NAME(1) MATCHES '*bodd112*' THEN 0 ELSE 5 /*5MANUALES SE AGREGO 0 PARA TLMKT*/
       Tarea.Refer     = IF PROGRAM-NAME(1) MATCHES '*bodd112*' THEN SUBSTRING(ReqAU.Pasillo,1,7) ELSE l-Folio
       Tarea.Tipo      = IF PROGRAM-NAME(1) MATCHES '*bodd112*' THEN 2 ELSE 3 /*3MANUALES SE AGREGO 2 PARA TLMKT*/
       Tarea.Id-TD     = IF PROGRAM-NAME(1) MATCHES '*bodd112*' THEN 0 ELSE 4 /*4MANUALES SE AGREGO 0 PARA TLMKT*/
       Tarea.Id-User   = IF g-origen = '02B' THEN "" ELSE "MANUAL".
 
FOR EACH t-DetRAU WHERE t-DetRAU.CantPres <> 0 EXCLUSIVE-LOCK,
    FIRST ArtPres WHERE ArtPres.Id-Articulo = t-DetRAU.Id-Articulo
                    AND ArtPres.Id-Pres = t-DetRAU.Id-Pres NO-LOCK,
    EACH Existencia WHERE (IF g-origen = '03A'
                           THEN SUBSTRING(Existencia.id-loc,1,4) < '3A50'
                           ELSE IF g-Origen = "02B"
                                THEN SUBSTRING(Existencia.Id-Loc,7,1) <= "M"
                                ELSE TRUE)
                      AND Existencia.Id-Articulo = t-DetRau.Id-Articulo
                      AND Existencia.Id-Color = t-DetRau.Id-Color
                      AND Existencia.Id-Alm = g-Origen
                      AND Existencia.Disponible > 0
                      AND (IF g-Origen <> "03A" THEN NOT Existencia.Id-Loc MATCHES "*RECIBO*" ELSE TRUE) USE-INDEX Idx-Art EXCLUSIVE-LOCK,
    FIRST b-AP WHERE b-AP.Id-Articulo = Existencia.Id-Articulo
                 AND b-AP.Id-Pres = Existencia.Id-Pres
               NO-LOCK BREAK BY t-detrau.id-articulo
                             BY t-detrau.id-color
                             BY t-detrau.id-pres
                             BY SUBSTRING(Existencia.Id-Loc,2,1) {1}
                             BY (b-AP.Equiv * Existencia.disponible):
                       
    IF FIRST-OF(t-detrau.id-pres) THEN DO:
        ASSIGN l-CantUMI = t-DetRau.CantPres * ArtPres.Equiv
               l-Cant1   = 0.
    END.
                       
    IF TRUNCATE((l-CantUMI / b-AP.Equiv),0) > 0 THEN DO:        
        CREATE DetTarea.
        ASSIGN DetTarea.Id-Tarea     = l-FolTar
               DetTarea.Id-Articulo  = t-DetRau.Id-Articulo
               DetTarea.Id-Color     = t-DetRau.Id-Color
               DetTarea.Id-Loc       = Existencia.Id-Loc
               DetTarea.Id-Pres      = Existencia.Id-Pres
               DetTarea.Secuencia    = Existencia.Secuencia
               l-Cant1               = IF Existencia.Disponible < (l-CantUMI / b-AP.Equiv)
                                       THEN Existencia.Disponible
                                       ELSE TRUNCATE((l-CantUMI / b-AP.Equiv),0)
               DetTarea.Cant         = l-Cant1
               Existencia.Reservada  = Existencia.Reservada + l-Cant1
               Existencia.Disponible = Existencia.Disponible - l-Cant1
               Existencia.FecMod     = TODAY
               Existencia.HorMod     = TIME
               l-CantUMI             = l-CantUMI - (l-Cant1 * b-AP.Equiv)
               t-DetRAU.CantUMI      = l-CantUMI.
    END.
    ELSE DO:
        IF g-origen = '11' OR g-origen = '12' AND LAST-OF(t-detrau.id-pres) THEN DO:  
            ASSIGN t-DetRAU.CantPres = 0.
        END.
    END.
           
    IF LAST-OF(t-detrau.id-pres) AND t-DetRAU.CantPres <> 0 THEN DO: /*Borra solo los que no son almacen 11 ni 12 */
        DELETE t-detrau.
    END.
END.

IF g-origen = '11' OR g-origen = '12' THEN DO:  
    FOR EACH t-DetRAU WHERE t-DetRAU.CantPres = 0
                        AND t-DetRAU.CantUMI > 0 NO-LOCK,
        FIRST ArtPres WHERE ArtPres.Id-Articulo = t-DetRAU.Id-Articulo
                        AND ArtPres.Id-Pres = t-DetRAU.Id-Pres
                      NO-LOCK BREAK BY t-detrau.id-articulo
                                    BY t-detrau.id-color
                                    BY t-detrau.id-pres:  
        CREATE DetTarea.
        ASSIGN DetTarea.Id-Tarea    = l-FolTar
               DetTarea.Id-Articulo = t-DetRau.Id-Articulo
               DetTarea.Id-Color    = t-DetRau.Id-Color
               DetTarea.Id-Loc      = ''
               DetTarea.Id-Pres     = t-DetRAU.Id-Pres
               DetTarea.Secuencia   = 9999999  /*para que queden al final*/
               DetTarea.Cant        = t-DetRAU.CantUMI / ArtPres.Equiv.                   
        IF LAST-OF(t-detrau.id-pres) THEN DO:
            DELETE t-detrau.
        END.
    END.
END.
RELEASE t-DetRau.
RELEASE Existencia.
