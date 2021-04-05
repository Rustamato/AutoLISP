;Построение линии водопровода
;Возвращаемое значение v-listing - список координат точек 3d-полилинии
;в формате метода vla-Add3Dpoly

(defun truba ( / listing pnt1 pnt z1 z2 z12 dist tmp_listing)
  (initget 1)
  (setq pnt1 (trans (getpoint "\nУкажите первую вершину полилинии: ") 1 0)
	z1 (getreal "\nУкажите координату Z введенной точки:")
	listing (list (list (car pnt1) (cadr pnt1) z1))
	z2 nil
	dist 0
	pnt T
	);setq
;Цикл ввода вершин полилинии
  (while (/= pnt "end")
	;цикл проверки условий ввода
    (while (not (or (listp pnt) (= pnt "end") (not pnt)))
    	(initget 128)
    	(setq pnt (getpoint (trans (car listing) 0 1) "\nУкажите вершину полилинии или END, чтобы завершить ввод точек <промежуточная>: "))
        (if (vl-consp pnt) (setq pnt (trans pnt 1 0)))
      );while цикл проверки условий ввода
    (if (/= pnt "end")
    
;Выбор сценария в зависимости от предыдущего ввода
    (if (not pnt)
      
        ;ввод промежуточных точек
      (progn
	(setq tmp_listing (list (car listing)));setq
	(while (/= pnt "no")
	  (setq pnt (getpoint (trans (car tmp_listing) 0 1) "\nУкажите промежуточную точку <завершить ввод промежуточных точек>: "))
	  (if (vl-consp pnt) (setq pnt (trans pnt 1 0)))	  
	  (if (not pnt) (setq pnt "no"))

	  (if (/= pnt "no")
	    ;выбор сценария в зависимости от предыдущего ввода
	    (progn
	      (grdraw (trans (car tmp_listing) 0 1) (trans pnt 0 1) 5 1)
	      (setq pnt (list (car pnt) (cadr pnt))
		    
		    dist (+ dist (distance (car tmp_listing) pnt))
		    tmp_listing (cons (append pnt (list dist)) tmp_listing)		     
		);setq
	    );progn в if
	  );if
	 );while цикл формирования списка (X Y) промежуточных точек
	  (initget 1)
	  (setq z2 (getreal "\nВведите координату Z последней промежуточной точки: ")
		z1 (last (last tmp_listing))
		z12 (/ (- z2 z1) dist)
		tmp_listing (reverse (mapcar '(lambda (x)
				       (subst  (+ z1 (* z12 (last x))) (last x) x))
				    (cdr (reverse tmp_listing))))
		listing (append tmp_listing listing)
		dist 0
		
		);setq
       );progn цикл ввода промежуточных точек

      ;цикл ввода основных точек
      (progn
	(grdraw (trans (car listing) 0 1) (trans pnt 0 1) 5 1)
	(initget 1)
	(setq z1 (getreal "\nВведите координату Z указанной точки: ")
	      pnt (list (car pnt) (cadr pnt) z1)
	      listing (cons pnt listing)
	      pnt T
	   );setq
	);progn
     );if выбора типа точек
    
   );if end

);while цикл ввода вершин полилинии

  ;преобразуем полученный список в формат vla-add3dpoly
 (repeat (length listing)
  (setq v-listing (append v-listing (car listing))
	listing (cdr listing)
	);setq
  );repeat преобразование в формат vla-add3dpoly
  
(princ "\n\"Truba\" is OK")
  (princ)
);defun
			
			
    
    

		    
		    
		    
  