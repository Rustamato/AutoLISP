;;;Функция разбивки оси скважины на характерные точки
(defun c:axpoint ( / skv prof begin plist pnt dist)
  (setq prof (vlax-ename->vla-object (car (entsel "\nУкажите 2D-линию характерных точек В ТАБЛИЦЕ : ")))
	skv (vlax-ename->vla-object (car (entsel "\nУкажите 2D-линию ОСИ СЕЧЕНИЯ на плане: ")))
	begin (osnap (cadr (entsel "\nУкажите точку НАЧАЛА линии оси сечения на плане: ")) "_END")
	);setq
  (if (equal (vlax-curve-getstartpoint skv) begin)
    (progn
      (setq plist (vlax-safearray->list (vlax-variant-value (vlax-get-property prof 'coordinates)))
	    begin (list (car plist) (cadr plist))
	    plist (cddr plist)
	    );setq
      (while plist
	(setq dist (distance begin (list (car plist) (cadr plist))))
	(if (> dist (vlax-get-property skv 'length)) (setq pnt (vlax-curve-getendpoint skv))
	  (setq pnt (vlax-curve-getPointAtDist skv dist))
	  );if
      (vla-AddPoint model_space (vlax-3D-Point pnt))
	(setq plist (cddr plist))
	      );while
      
      (if (= (distof (rtos (vlax-get-property prof 'length) 2 3)) (distof (rtos (vlax-get-property skv 'length) 2 3)))
	      (progn
		  (princ "\n\"Axpoint\" is OK")
		  (princ)
	       );progn
	      (if (> (vlax-get-property prof 'length) (vlax-get-property skv 'length))
		(progn
		  (princ "\nВнимание!!! Длина линии НА ПРОФИЛЕ больше чем на ПЛАНЕ!")
		  (princ)
		  );progn
		(progn
		  (princ "\nВнимание!!! Длина линии на ПЛАНЕ больше чем на ПРОФИЛЕ!")
		  (princ)
		  );progn
		);if
	);if
      );progn
    (progn
      (princ "\nТочка начала линии на плане не совпадает с указанной! Обратите полилинию и повторите функцию.")
    (princ));progn
    );if
   
  );defun