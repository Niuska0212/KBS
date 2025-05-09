; Archivo editado para saber para qué sirve cada una de las reglas del sistema de promociones y recomendaciones

; Regla que ofrece 24 MSI con Banamex Oro por comprar un iPhone16
(defrule MAIN::promo-iphone-banamex
   (orden (id-orden ?id) (cliente ?cli) (forma-pago tarjeta))
   (orden-producto (id-orden ?id) (tipo smartphone) (marca apple) (modelo iphone16))
   (tarjetacred (cliente ?cli) (banco banamex) (grupo oro))
   =>
   (printout t "Oferta para cliente ID " ?cli ": 24 MSI con Banamex Oro por comprar iPhone16." crlf))


; Regla que ofrece 12 MSI con tarjeta Liverpool VISA al comprar Samsung Note21
(defrule MAIN::promo-note21-liverpool
   ?orden <- (orden (forma-pago tarjeta) (cliente ?cli) (id-orden ?id))
   (orden-producto (id-orden ?id) (tipo smartphone) (marca samsung) (modelo note21))
   (tarjetacred (cliente ?cli) (banco liverpool) (grupo visa))
   =>
   (printout t "Oferta para " ?cli ": 12 MSI con tarjeta Liverpool VISA por comprar Samsung Note21." crlf))


; Regla para ofrecer vales por compra al contado de MacBook Air e iPhone16


(defrule MAIN::calcular-total-smartphones
   ?o <- (orden (id-orden ?id))
   ?p <- (orden-producto (id-orden ?id) (tipo smartphone) (marca ?m) (modelo ?mo) (qty ?q))
   ?s <- (smartphone (marca ?m) (modelo ?mo) (precio ?precio))
   =>
   (bind ?subtotal (* ?q ?precio))
   (assert (subtotal (id-orden ?id) (valor ?subtotal))))


(defrule MAIN::acumular-totales
   (declare (salience -10)) ; Se ejecuta después de los subtotales
   ?o <- (orden (id-orden ?id))
   (not (total (id-orden ?id))) ; Evita recalcular si ya existe un total
   =>
   (bind ?total 0)
   (do-for-all-facts ((?s subtotal)) 
       (eq ?s:id-orden ?id)
       (bind ?total (+ ?total ?s:valor)))
   (assert (total (id-orden ?id) (valor ?total))))



; Regla que da descuento especial si un iPhone cuesta mas de 25 mil
(defrule MAIN::descuento-apple-premium
   (smartphone (marca apple) (modelo ?m) (precio ?p&:(> ?p 25000)))
   =>
   (printout t "Descuento especial en " ?m ": precio superior a 25 mil." crlf))

; Regla que promueve productos con stock alto (mas de 80 unidades)
(defrule MAIN::promocion-stock-alto
   (smartphone (marca ?m) (modelo ?mod) (stock ?s&:(> ?s 80)))
   =>
   (printout t "Promocion por stock alto: " ?m " " ?mod " disponible en grandes cantidades." crlf))

; Regla que alerta si hay stock bajo de computadoras (menos de 10)
(defrule MAIN::alerta-stock-bajo
   (compu (modelo ?mod) (stock ?s&:(< ?s 10)))
   =>
   (printout t "Alerta! Stock bajo de computadoras modelo " ?mod ", queda "?s"." crlf))

; Regla que anuncia accesorios baratos (precio menor a $500)
(defrule MAIN::accesorio-barato
   (accesorio (tipo ?t) (precio ?p&:(< ?p 500)))
   =>
   (printout t "Accesorio economico disponible: " ?t " por $" ?p "." crlf))


; Regla que agradece el pago en efectivo
(defrule MAIN::pago-efectivo
   (orden (id-orden ?id) (cliente ?cli) (forma-pago efectivo))
   (orden-producto (id-orden ?id) (tipo ?tipo) (marca ?marca) (modelo ?modelo))
   =>
   (printout t "Gracias por pagar en efectivo tu compra de " ?tipo " " ?marca " " ?modelo ", cliente " ?cli "." crlf))


; Regla que recomienda MacBook Air por su precio
(defrule MAIN::recomienda-macbook
   (compu (marca apple) (modelo macbookair) (precio ?p))
   =>
   (printout t "Recomendacipn: MacBook Air por $" ?p ". Ideal para estudiantes." crlf))



;Regla para contar productos por orden
(defrule contar-productos-por-orden
   ?orden <- (orden (id-orden ?id) (cliente ?cli))
   (not (cuenta-productos (id-orden ?id))) ;; solo una vez por orden
   =>
   (bind ?suma 0)
   (do-for-all-facts ((?p orden-producto)) 
      (eq ?p:id-orden ?id)
      (bind ?suma (+ ?suma ?p:qty))
   )
   (assert (cuenta-productos (id-orden ?id) (total ?suma))))


; Regla que otorga vale de $500 por comprar mas de 2 productos
(defrule otorgar-vale-por-multiples-productos
   ?orden <- (orden (id-orden ?id) (cliente ?cli))
   (cuenta-productos (id-orden ?id) (total ?t&:(> ?t 2)))
   (not (vale (cliente ?cli) (monto 500))) ;; evita duplicados
   =>
   (assert (vale (cliente ?cli) (monto 500)))
   (printout t "Se otorgo vale de $500 al cliente " ?cli " por comprar mas de 2 productos en la orden " ?id crlf))




; Regla que alerta sobre tarjeta de crédito proxima a expirar

;reglas para evaluar el estado de la tarjeta
(defglobal ?*mes-actual* = 5)
(defglobal ?*anio-actual* = 25)


; Tarjeta vencida
(defrule tarjeta-vencida
  (tarjetacred (cliente ?c) (exp-mes ?m) (exp-anio ?a))
  (test (or (< ?a ?*anio-actual*)
            (and (= ?a ?*anio-actual*) (< ?m ?*mes-actual*))))
  =>
  (assert (estado-tarjeta (cliente ?c) (estado vencida)))
  (printout t "La tarjeta del cliente " ?c " esta vencida." crlf))

; Tarjeta proxima a vencer
(defrule tarjeta-proxima-a-vencer
  (tarjetacred (cliente ?c) (exp-mes ?m) (exp-anio ?a))
  (test (or 
         (and (= ?a ?*anio-actual*) 
              (>= ?m ?*mes-actual*) 
              (<= ?m (+ ?*mes-actual* 2)))
         (and (= ?a (+ ?*anio-actual* 1)) 
              (<= ?m (- (+ ?*mes-actual* 2) 12)))))
  =>
  (assert (estado-tarjeta (cliente ?c) (estado proxima-a-vencer)))
  (printout t "La tarjeta del cliente " ?c " esta PROXIMA A VENCER." crlf))

;tarjeta vigente
(defrule tarjeta-vigente
  (tarjetacred (cliente ?c))
  (not (estado-tarjeta (cliente ?c)))
  =>
  (assert (estado-tarjeta (cliente ?c) (estado vigente)))
  (printout t "La tarjeta del cliente " ?c " esta VIGENTE." crlf))



; Regla mejorada para generar vales por uso de tarjeta vigente
(defrule generar-vale-por-tarjeta-vigente
   (orden (cliente ?cli) (forma-pago tarjeta)) ; El cliente pago con tarjeta
   (estado-tarjeta (cliente ?cli) (estado vigente)) ; Su tarjeta está vigente
   (not (vale (cliente ?cli))) ; Verifica que no tenga ya un vale
   =>
   (assert (vale (cliente ?cli) (monto 300))) ; Asigna vale de $300
   (printout t "** Cliente " ?cli " ha recibido un vale de $300 por usar tarjeta vigente **" crlf))

; Regla mejorada para ofertas especiales
(defrule oferta-por-tarjeta
   (orden (cliente ?cli) (forma-pago tarjeta))
   (estado-tarjeta (cliente ?cli) (estado ?estado&proxima-a-vencer|vigente))
   =>
   (printout t "Oferta disponible: Cliente " ?cli " (Tarjeta " ?estado ") puede obtener descuentos especiales" crlf))

; Regla para mostrar vales asignados
(defrule mostrar-vales-asignados
   (declare (salience -10)) ; Se ejecuta al final
   ?v <- (vale (cliente ?cli) (monto ?m))
   =>
   (printout t "-> Cliente " ?cli " tiene un vale activo de $" ?m crlf))


; Regla que identifica computadoras premium (precio mayor a 20 mil)
(defrule MAIN::compu-premium
   (compu (precio ?p&:(> ?p 20000)) (marca ?m) (modelo ?mod))
   =>
   (printout t "Computadora premium: " ?m " " ?mod " cuesta $" ?p "." crlf))

; Regla que identifica computadoras de precio promedio (hasta 20 mil)
(defrule MAIN::compu-promedio
   (compu (precio ?p&:(<= ?p 20000)) (marca ?m) (modelo ?mod))
   =>
   (printout t "Buena opcion: " ?m " " ?mod " cuesta $" ?p "." crlf))

; Regla que resalta smartphones en edicion exclusiva (color rojo)
(defrule MAIN::smartphone-exclusivo
   (smartphone (color rojo) (marca ?m) (modelo ?mod))
   =>
   (printout t "Edicion exclusiva en color rojo: " ?m " " ?mod "." crlf))


; Regla que detecta compras mayoristas (10 o mas productos) y sugiere descuento
(defrule MAIN::orden-mayorista
   (orden-producto (id-orden ?id) (qty ?q&:(>= ?q 10)) (tipo ?prod) (marca ?m))
   =>
   (printout t "Compra mayorista de " ?prod " marca " ?m ". Aplicar descuento por volumen." crlf))



; Regla que ofrece descuento en accesorios por compra de smartphone
(defrule MAIN::smartphone-desc-accesorios
   (orden (id-orden ?id) (cliente ?cli))
   (orden-producto (id-orden ?id) (tipo smartphone))
   =>
   (printout t "Cliente " ?cli ": Descuento del 15% en funda y mica por comprar un smartphone." crlf))


; Regla que recuerda al cliente solicitar su factura
(defrule MAIN::recordatorio-factura
   (orden (id-orden ?id) (cliente ?cli))
   (orden-producto (id-orden ?id) (tipo ?prod))
   =>
   (printout t "Recordatorio para " ?cli ": Solicita tu factura por la compra de " ?prod "." crlf))


; Regla para guardar o imprimier el historial de compras o que permita recuperarlo
(defrule MAIN::mostrar-compras
   (orden (id-orden ?id) (cliente ?c))
   (orden-producto (id-orden ?id) (tipo ?prod) (marca ?m) (modelo ?mod) (qty ?q))
   =>
   (printout t "Cliente " ?c " compro " ?q " unidad(es) de " ?prod " " ?m " " ?mod "." crlf))



; Regla que recomienda smartphones accesibles (menores a $8,000)
(defrule MAIN::recomienda-smartphone-barato
   (smartphone (precio ?p&:(< ?p 8000)) (marca ?m) (modelo ?mod))
   =>
   (printout t "Opcion accesible: " ?m " " ?mod " por solo $" ?p "." crlf))



; Regla de funda y mica al comprar smartphone con 15% de descuento
(defrule MAIN::promo-accesorios-smartphone
   (orden (id-orden ?id))
   (orden-producto (id-orden ?id) (tipo smartphone))
   (accesorio (tipo funda) (precio ?pf))
   (accesorio (tipo mica) (precio ?pm))
   =>
   (bind ?df (* ?pf 0.15))
   (bind ?dm (* ?pm 0.15))
   (printout t "Oferta: 15% de descuento en funda y mica por comprar smartphone. "
              "Descuento funda: $" ?df ", Descuento mica: $" ?dm crlf))



; Regla que evalúa si el cliente es menudista o mayorista
(defrule MAIN::evaluar-tipo-cliente
   ?orden <- (orden (id-orden ?id) (cliente ?cli))
   (cuenta-productos (id-orden ?id) (total ?cantidad))
   =>
   (if (>= ?cantidad 10) then
      (assert (tipo-cliente (id-orden ?id) (tipo mayorista)))
      (printout t "Cliente " ?cli " clasificado como MAYORISTA en la orden " ?id "." crlf)
   else
      (assert (tipo-cliente (id-orden ?id) (tipo menudista)))
      (printout t "Cliente " ?cli " clasificado como MENUDISTA en la orden " ?id "." crlf)))



(defrule MAIN::actualizar-stock-smartphone
   ?orden <- (orden (id-orden ?id))
   ?prod <- (orden-producto (id-orden ?id) (tipo smartphone) (marca ?m) (modelo ?mod) (qty ?q))
   ?stock-fact <- (smartphone (marca ?m) (modelo ?mod) (stock ?s&:(>= ?s ?q)) (precio ?p) (color ?c))
   =>
   (bind ?nuevo-stock (- ?s ?q))
   (retract ?stock-fact)
   (assert (smartphone (marca ?m) (modelo ?mod) (stock ?nuevo-stock) (precio ?p) (color ?c)))
   (printout t "Stock actualizado: " ?m " " ?mod " ahora tiene " ?nuevo-stock " unidades disponibles." crlf))

(defrule actualizar-stock-compu
   ?op <- (orden-producto (id-orden ?id) (tipo compu) (marca ?marca) (modelo ?modelo) (qty ?q))
   ?prod <- (compu (marca ?marca) (modelo ?modelo) (stock ?s&:(>= ?s ?q)) (precio ?p) (color ?c))
   =>
   (bind ?nuevo-stock (- ?s ?q))
   (retract ?prod)
   (assert (compu (marca ?marca) (modelo ?modelo) (stock ?nuevo-stock) (precio ?p) (color ?c)))
   (printout t "Stock actualizado para computadora " ?marca " " ?modelo ": nuevo stock = " ?nuevo-stock crlf))

(defrule actualizar-stock-accesorio
   ?op <- (orden-producto (id-orden ?id) (tipo accesorio) (tipo-accesorio ?tipo) (qty ?q))
   ?prod <- (accesorio (tipo ?tipo) (stock ?s&:(>= ?s ?q)) (precio ?p))
   =>
   (bind ?nuevo-stock (- ?s ?q))
   (retract ?prod)
   (assert (accesorio (tipo ?tipo) (stock ?nuevo-stock) (precio ?p)))
   (printout t "Stock actualizado para accesorio " ?tipo ": nuevo stock = " ?nuevo-stock crlf))



