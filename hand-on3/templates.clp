(deftemplate MAIN::smartphone
   (slot marca)
   (slot modelo)
   (slot color)
   (slot precio)
   (slot stock))

(deftemplate MAIN::compu
   (slot marca)
   (slot modelo)
   (slot color)
   (slot precio)
   (slot stock))

(deftemplate MAIN::accesorio
   (slot tipo)
   (slot precio)
   (slot stock))

(deftemplate MAIN::tarjetacred
   (slot cliente)
   (slot banco)
   (slot grupo)
   (slot exp-mes)  
   (slot exp-anio))

(deftemplate MAIN::estado-tarjeta
   (slot cliente)
   (slot estado))  ; vencida, proxima-a-vencer, vigente


(deftemplate MAIN::orden
   (slot cliente)
   (slot forma-pago)
   (slot id-orden)) ;; identificador único de orden

(deftemplate MAIN::orden-producto
   (slot id-orden)
   (slot tipo) ;; smartphone, compu, accesorio
   (slot marca (default none))
   (slot modelo (default none))
   (slot tipo-accesorio (default none)) ;; solo si es accesorio
   (slot qty))


(deftemplate subtotal
   (slot id-orden)
   (slot valor))

(deftemplate total
   (slot id-orden)
   (slot valor))

(deftemplate MAIN::vale
   (slot cliente)
   (slot monto))


(deftemplate MAIN::cliente
   (slot nombre)
   (slot id))



(deftemplate MAIN::compra
  (slot cliente)
  (slot total))




(deftemplate cuenta-productos
   (slot id-orden)
   (slot total))




(deftemplate MAIN::tipo-cliente
   (slot id-orden)
   (slot tipo))    ; valor: menudista o mayorista
