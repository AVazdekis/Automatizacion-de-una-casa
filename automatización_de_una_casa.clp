;---DEFINICIÓN DE LA PLANTILLA-----------------------------------
(deftemplate person
    (slot name (type STRING)) 
    (slot rol (type STRING))
    (slot location (type STRING))
)


(deftemplate location    
    (slot name (type STRING)) 
	(slot light (type STRING))
	(slot alarm (type STRING))
	(slot distance (type INTEGER))
	(slot door (type STRING))
)


(deftemplate time
    (slot hour (type INTEGER))
)


(deftemplate smoke_sensor
	(slot location)
	(slot status)
)


(deftemplate water_ejector
	(slot location)
	(slot status)
	(slot temperature (type INTEGER))
)


(deftemplate real_time_notification
   (slot status (type STRING)) 
   (slot message (type STRING))
   (slot location)
)


(deftemplate camera
   (slot status (type STRING)) 
   (slot location)
)


(deftemplate police
   (slot call (type STRING)) 
   (slot message (type STRING))
)

(deftemplate battery
    (slot status (type STRING))
    (slot charge (type INTEGER)) ; Nivel de carga de la batería
)

(deftemplate inverter
    (slot status (type STRING)) 
)

(deftemplate radiation_sensor
    (slot radiation (type INTEGER)) ; Valor del sensor de radiación
)

;---DECLARACIÓN DE HECHOS-------------------------------------

(deffacts main_facts

	(time 
        (hour 2400)
    )

    ; ****
    ; PEOPLE
    ; ****
    (person        
		(name "Manolo")
		(rol "Father")
		(location "Bathroom 1")
	)

	(person        
		(name "Laura") 
		(rol "Mother")
		(location "Out of Home")
	)

	(person        
		(name "Juan") 
		(rol "Son")
		(location "Out of Home")
	)

	(person        
		(name "Andrea")
		(rol "Daughter")
		(location "Out of Home")
	)

	
	(person        
        (name "Not Known")
        (rol "Intruder")
        (location "Andrea Room")
    )
	

    
    ; ****
    ; ROOMS
    ; ****
    (location
        (name "Bathroom 1")
		(alarm "Off")
		(distance 3)
		(door "Open")
    )

    (location
        (name "Bathroom 2")
		(alarm "Off")
		(distance 2)
		(door "Close")

    )

    (location
        (name "Kitchen")
		(alarm "Off")
		(distance 1)
		(door "Open")

    )

    (location
        (name "Dining room")
		(alarm "Off")
   		(distance 1)
		(door "Open")

	)


    (location
        (name "Parent Room")
		(alarm "Off")
		(distance 2)
		(door "Open")

    )

    (location
        (name "Juan Room")
		(alarm "Off")
		(distance 2)
    	(door "Open")

	)

    (location
        (name "Andrea Room")
		(alarm "Off")
		(distance 2)
		(door "Open")

    )


    (location
        (name "Garage")
		(alarm "Off")
		(distance 3)
		(door "Open")

    )


    (location
        (name "Out of Home")
    )
	

	; ****
    ; SMOKE SENSOR Y WATER EJECTORS
    ; ****
    (smoke_sensor
		(location "Bathroom 2")
		(status "Detection")
	)
	
    (water_ejector
        (location "Bathroom 2")
		(status "Inactive")
		(temperature 121)
    )
    
	
	; ****
    ; SOLAR PANEL
    ; ****
    (battery
        (status "Off")
		(charge 100)
    )
	
	(inverter
		(status "Off")
	)
	
	(radiation_sensor
		(radiation 30)
	)
	
	
	; ************
    ; CONDITIONS
    ; ************
    (blinds "Raise") ;Estado de las persinas
	(temperature 14) ;Temperatura de la calle
	(water_heater "On") ;Calentador de agua
	(door "Open") ;Puerta principal
	(general_alarm "Off") ;Alarma general de la casa
	(if_else "False") ;Para hacer if y else en las reglas
)


;---REGLAS------------------------------------------------------------------------------------------------------------------


; -----------------------------------------------------------1--------------------------------------------------------------
; *** TURNING LIGHTS ON ***
(defrule turn_light_on
    (location (name ?l&~"Out of Home")) ; Considera solo ubicaciones que no son "Out of Home"
    (exists (person (location ?l)))
    =>
    (printout t "Encender la luz de: " ?l crlf)
)
	
; *** TURNING LIGHTS OF ***
(defrule turn_light_on_or_off
    (location (name ?l&~"Out of Home")) ; Considera solo ubicaciones que no son "Out of Home"
    (not (exists (person (location ?l))))
    =>
	(printout t "Apagar la luz de: " ?l crlf)
)



; -----------------------------------------------------------2--------------------------------------------------------------
; *** CLOSE THE DOORS ***
(defrule close_the_doors
	(time (hour ?hour&:(< ?hour 2200)))
	?id_d <- (door ?d)  
	?id_i <- (if_else "False")

	(exists (person (rol "Father") (location "Out of Home")))
    (exists (person (rol "Mother") (location "Out of Home")))
    =>
	(if (eq ?d "Open") then
		(retract ?id_d)
		(assert (door "Close")) 
		
		;Ver si se ejeuta el else
		(retract ?id_i)  
        (assert (if_else "True"))  
        
		(printout t "Se bloquean las puertas de la casa." crlf)
	else
	    (if (not(eq ?id_i "True")) then
			(printout t "Las puertas ya estan bloqueadas" crlf)
		)	
	)
)


; *** OPEN THE DOORS ***
(defrule open_the_doors
	(time (hour ?hour&:(< ?hour 2200)))
	?id_d <- (door ?d)  
	?id_i <- (if_else "False")
	(or (and (person (rol "Father") (location ~"Out of Home")) ;Si la madre no esta en casa pero el padre si
             (person (rol "Mother") (location "Out of Home")))
        (and (person (rol "Mother") (location ~"Out of Home")) ;Si el padre no esta en casa pero la madre si
             (person (rol "Father") (location "Out of Home")))
		(and (person (rol "Mother") (location ~"Out of Home")) ;Si los dos padres estan 
             (person (rol "Father") (location ~"Out of Home"))))
    =>
	(if (eq ?d "Close") then
		(retract ?id_d)
		(assert (door "Open")) ; Cambiar el estado de la puerta
		
		;Ver si se ejeuta el else
		(retract ?id_i)  
        (assert (if_else "True"))  
       
		(printout t "Se desbloquean las puertas de la casa." crlf)
		else 
		    (if (not(eq ?id_i "True")) then
				(printout t "Las puertas ya estan desbloqueadas" crlf)
			)		
	)
)


; *** CLOSE THE DOORS LATE EVENING ***
(defrule close_doors_late_evening
    (time (hour ?hour&:(>= ?hour 2200)))
    ?id_d <- (door ?d)  
	?id_i <- (if_else "False")

    (person (location ~"Out of Home") (rol "Son"))
    (person (location ~"Out of Home") (rol "Daughter"))
    =>
    (if (eq ?d "Open")
        then
        (retract ?id_d)
        (assert (door "Close")) ;Cambiar el estado de la puerta
		
		;Ver si se ejeuta el else
		(retract ?id_i)  
        (assert (if_else "True")) 
       
        (printout t "Es tarde, se cierran las puertas de la casa." crlf)
    else
		(if (not(eq ?id_i "True")) then
			(printout t "Las puertas ya están bloqueadas" crlf)
		)
	)
)



; -----------------------------------------------------------3--------------------------------------------------------------
; *** UPPER BLINDS ***
(defrule upper_blinds
	?id_b <- (blinds ?b)  
	?id_i <- (if_else "False")
	(time (hour ?hour&:(= ?hour 0800)))
	
    =>
	(if (eq ?b "Lower") then
		(retract ?id_b) ;Retracta el hecho actual de las persianas
		(assert (blinds "Raise")) ; Cambiar el estado de las persinas a "Raise"
		
		;Ver si se ejeuta el else
		(retract ?id_i)  
        (assert (if_else "True"))  
        
		(printout t "Subir persianas" crlf)
	else
        (if (not(eq ?id_i "True")) then
              (printout t "Las persianas ya están subidas" crlf)
        )
    )
)

; *** LOWER BLINDS ***
(defrule lower_blinds
    ?id_b <- (blinds ?b)  
    ?id_i <- (if_else "False")
    (time (hour ?hour&:(= ?hour 2000)))
    =>
    (if (eq ?b "Raise") then
        (retract ?id_b)  ; Retracta el hecho actual de las persianas
        (assert (blinds "Lower"))  ; Cambia el estado de las persianas a "Lower"
        
		;Ver si se ejeuta el else
        (retract ?id_i)  ; Retracta el hecho actual de las persianas
        (assert (if_else "True"))  ; Cambia el estado de las persianas a "Lower"
        
        (printout t "Bajar persianas" crlf)
    else
        (if (not(eq ?id_i "True")) then
            (printout t "Las persianas ya están bajadas" crlf)
        
        )
    )
)



; -----------------------------------------------------------4--------------------------------------------------------------
; *** TURNING ON AIR CONDITIONER ***
(defrule turn_air_conditioner_on
    (location (name ?l&~"Out of Home")) ; Considera solo ubicaciones que no son "Out of Home"
    (temperature ?t) ; Obtener la temperatura ambiente
    (test (> ?t 25)) ; Comprobar si la temperatura es mayor a 25 grados
    =>
    (bind ?counter 0) ; Inicializar contador de personas en la habitación
    ; Contar el número de personas en la habitación
    (do-for-all-facts ((?p person)) ; Loop a través de los hechos de personas
        (eq ?p:location ?l) ; Condición: Persona está en la habitación
		(bind ?counter (+ ?counter 1)) ; Acción: Incrementar el contador
    )
    ; Si hay al menos una persona en la habitación, encender el aire acondicionado
    (if (>= ?counter 1)
        then
        (printout t "Encender aire acondicionado de: " ?l crlf)
    )
)


; *** TURNING OFF AIR CONDITIONER ***
(defrule turn_air_conditioner_off1
    (location (name ?l&~"Out of Home")) ; Considera solo ubicaciones que no son "Out of Home"
    (temperature ?t) ; Obtener la temperatura de la habitación
    (test (< ?t 25)) ; Comprobar si la temperatura es menor a 25 grados
    =>
        (printout t "Apagar aire acondicionado de: " ?l crlf)
)

; *** TURNING OFF AIR CONDITIONER ***
(defrule turn_air_conditioner_off2
    (location (name ?l&~"Out of Home")) ; Considera solo ubicaciones que no son "Out of Home"
    (temperature ?t) ; Obtener la temperatura de la habitación
    (test (> ?t 25)) ; Comprobar si la temperatura es menor a 25 grados
    =>
    (bind ?counter 0) ; Inicializar contador de personas en la habitación
    ; Contar el número de personas en la habitación
    (do-for-all-facts ((?p person)) ; Loop a través de los hechos de personas
        (eq ?p:location ?l) ; Condición: Persona está en la habitación
		(bind ?counter (+ ?counter 1)) ; Acción: Incrementar el contador
    )
    ; Si no hay ninguna persona en la habitación,apagar el aire
    (if (= ?counter 0)
        then
        (printout t "Apagar aire acondicionado de: " ?l crlf)
    )
)

; -----------------------------------------------------------5--------------------------------------------------------------
; *** TURNING WATER HEATER OFF ***
(defrule turn_water_heater_off
    ?id_w <- (water_heater ?w)
    ?id_i <- (if_else "False")

    (time (hour ?hour&:(= ?hour 2200)))    
    =>
    (if (eq ?w "On")
        then
		(retract ?id_w)
		(assert (water_heater "Off"))
		
		;Ver si se ejeuta el else
		(retract ?id_i)  ; Retracta el hecho actual de las persianas
        (assert (if_else "True"))  ; Cambia el estado de las persianas a "Lower"
        
        (printout t "Se apaga el calentador de agua." crlf)
    else
		(if (not(eq ?id_i "True")) then
			(printout t "El calentador de agua ya está apagado." crlf)
		)
	)
)


; *** TURNING WATER HEATER ON ***
(defrule turn_water_heater_on
    ?id_w <- (water_heater ?w)
	?id_i <- (if_else "False")

    (time (hour ?hour&:(= ?hour 0500)))   
    =>
	(if (eq ?w "Off") 
		then
		(retract ?id_w)
		(assert (water_heater "On"))
		
		;Ver si se ejeuta el else
		(retract ?id_i) 
        (assert (if_else "True"))  
        
		(printout t "Se enciende el calentador de agua." crlf)
	else
	    (if (not(eq ?id_i "True")) then
			(printout t "El calentador de agua ya esta encendido." crlf)
		)
	)
)

; -----------------------------------------------------------6--------------------------------------------------------------
; *** FIRE PROTOCOL ***
(defrule fire_protocol
	?id_d <- (door ?d)  
	?location <- (location (name ?fire-room) (door "Close"))
    ?smoke <- (smoke_sensor (status "Detection") (location ?fire-room))
    ?water <- (water_ejector (location ?fire-room) (status "Inactive") (temperature ?temp))
    =>
    (do-for-all-facts ((?p person)) (neq ?p:location "Out of Home")
        (bind ?person-location (fact-slot-value ?p location))
        (assert (real_time_notification (location ?person-location)
                                        (status "Active")
                                        (message (str-cat "Se ha detectado un posible fuego en la habitación " ?fire-room ". Atención a la ruta de evacuación."))))
	)
    
    (if (>= ?temp 70) then
        (modify ?water (status "Active"))
		(printout t "Se ha activado el water ejector en la habitación " ?fire-room " debido al aumento de temperatura." crlf)
	)
	

    
	; En la implementacion dada, cada localizacion cuenta con una distancia. Dicha distancia se interpreta con valores que representan cuan lejanos estan de
	; las puertas de emergencia. La logica explica que a una distancia de 1 = mas cercana "Entrance". A una distancia de dos mejor la salida de emergencia de
	; "Bathroom 2" y a una distancia de 3 mejor la salida de emergencia de "Garage"
    (if (>= ?temp 120) then
		(switch ?fire-room
			(case "Entrance" then
				(printout t "Abrir puertas hacia la salida de Garage." crlf)
				(if (eq ?d "Close") then
					(retract ?id_d)
					(assert (door "Open"))))
					
			(case "Bathroom 2" then
				(printout t "Abrir puertas hacia la salida de Entrance." crlf)
				(modify ?location (door "Open")))


			(case "Garage" then
				(printout t "Abrir puertas hacia la salida de Entrance." crlf)
				(modify ?location (door "Open")))
		)


		(do-for-all-facts ((?p person)) (neq ?p:location "Out of Home")
			(bind ?person-location (fact-slot-value ?p location))
            (do-for-all-facts ((?l location))
                (if (eq ?l:name ?person-location) then
                    (switch ?l:distance
                        (case 1 then
                            (if (eq ?fire-room "Entrance") then
                                (assert (real_time_notification (location ?person-location)
                                                                (status "Active")
                                                                (message "¡Alerta! ¡El fuego a inhabilitado su salida de emergencia mas cercana! Diríjase hacia la salida de Garage.")))
                                else
                                (assert (real_time_notification (location ?person-location)
                                                                (status "Active")
                                                                (message "Diríjase hacia la salida de Entrance.")))))
                        (case 2 then
                            (if (eq ?fire-room "Bathroom 2") then
                                (assert (real_time_notification (location ?person-location)
                                                                (status "Active")
                                                                (message "¡Alerta! ¡El fuego a inhabilitado su salida de emergencia mas cercana! Diríjase hacia la salida de Entrance.")))
                                else
                                (assert (real_time_notification (location ?person-location)
                                                                (status "Active")
                                                                (message "Diríjase hacia la salida de Bathroom 2.")))))
                        (case 3 then
                            (if (eq ?fire-room "Garage") then
                                (assert (real_time_notification (location ?person-location)
                                                                (status "Active")
                                                                (message "¡Alerta! ¡El fuego a inhabilitado su salida de emergencia mas cercana! Diríjase hacia la salida de Entrance.")))
                                else
                                (assert (real_time_notification (location ?person-location)
                                                                (status "Active")
                                                                (message "Diríjase hacia la salida de Garage.")))))
					)
				)
			)
		)
		(printout t "Se ha notificado correctamente a todos los habitantes de la casa." crlf)
	)
)



; -----------------------------------------------------------7--------------------------------------------------------------
; ********* SECURITY IN CASE OF THEFT *********
; Definición de reglas para detectar intrusos y enviar notificaciones en tiempo real
(defrule detect_intruder1
    ?id_a <- (location (name ?l_intruder&~"Out of Home")
                       (alarm "Off"))
	?id_ag <- (general_alarm "Off")
    (location (name ?l_intruder&~"Out of Home"))
    (exists (person (rol "Intruder") (location ?l_intruder)))
    (exists (person (rol ~"Intruder") (location ?l&~"Out of Home"))) ; Al menos una persona que no sea intrusa no está "Out of Home"
	
	=>
	;Repriducir por los altavoces
		(assert (real_time_notification (status "Intruder Detected") 
										 (message "There is an intruder detected!") 
										 (location ?l_intruder)))
	;Encender las camaras y grabar en el lugar donde se encuentre el intruso
		(assert (camera (status "Camera recording intruder") 
						 (location ?l_intruder)))
						 
	;Disparar alarma solo en la localización donde este el intruso
		(modify ?id_a (alarm "On"))
						 
	; Encender todas las luces de todas las habitaciones
		(foreach ?room 
		  (find-all-facts ((?f location)) 
			(neq (fact-slot-value ?f name) "Out of Home"))
		  (modify ?room (light "On"))
		)

	
	; Preguntar a la familia si desean llamar a la policía 
    (printout t "¿Desea llamar a la policía? (Si/No): ")
    ;(bind ?response (lowcase(read)))
	(bind ?respuesta (read)) ;cadena o simbolo 
    
	(if (eq ?respuesta Si)
        then
		;Llamar a la policia
        (assert (police (call "Yes") (message "Calling the police")))
		
		;Disparar alarma general
		(retract ?id_ag)  
        (assert (general_alarm "On"))
		
        (printout t "Se esta llamando a la policía y la alarma general de la casa esta sonando." crlf)
    	
	else
		(if (eq ?respuesta No) then 
				(printout t "No se llamará a la policía." crlf)
			
		)
    )
	
)



(defrule detect_intruder2
	?id_ag <- (general_alarm "Off")
	?id_d <- (door ?d)
    (location (name ?l_intruder&~"Out of Home"))
    (exists (person (rol "Intruder") (location ?l_intruder)))
    (exists (person (rol ~"Intruder") (location ?l&"Out of Home"))) ; Todas las personas que no son intrusos están "Out of Home"
    =>
	;Disparar alarma general
	(retract ?id_ag)  
    (assert (general_alarm "On"))

	;Cerrar puertas
	(retract ?id_d)
	(assert (door "Close"))


	;Encender las camaras y grabar en el lugar donde se encuentre el intruso
    (assert (camera (status "Camera recording intruder") 
                     (location ?l_intruder)))
					 
	;Llamar a la policia 
	(assert (police (call "Yes") (message "Calling the police")))
        
	(printout t "Se esta llamando a la policía, la alarma general de la casa esta sonando, y la camara esta grabando." crlf)

)



; -----------------------------------------------------------8--------------------------------------------------------------
; *** SOLAR PANELS ***
(defrule solar_panel
	(time (hour ?hour))
	?batt <- (battery (status ?status_b)(charge ?charge))
	?inv <- (inverter (status ?status_i))
	?rad <- (radiation_sensor (radiation ?radiation))
	=>
	(if (>= ?hour 2100) then
		(modify ?batt (status "On"))
		(modify ?inv (status "Off"))
		(printout t "Se encendió la batería y se apagó el inversor pues es de noche y no hay luz solar." crlf)
	
	else
		(if	(= ?charge 100) then
			(modify ?inv (status "Off"))
			(printout t "La batería está cargada al 100%, se apagó el inversor." crlf)
		)
			
		(if (and (>= ?radiation 250) ;a partir de 250 watts/m2 se activa
				 (< ?charge 100)) then
			(modify ?inv (status "On"))
			(printout t "El valor del sensor de radiación es alto, se encendió el inversor." crlf)
		else
			(modify ?inv (status "Off"))
			(printout t "El valor del sensor de radiación es bajo, se apagó el inversor." crlf)


		)
	)
)