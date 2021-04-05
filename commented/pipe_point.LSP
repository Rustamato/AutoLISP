;;;ѕоиск абсолютной отметки точки пересечени€ коммуникации координат точки
;;;построени€ на профиле p_point (dist zed)
;; глобальные переменные: intlst
;; возвращает:	p_list       
(defun pipepoint (lst / int dist a2 z_a2 b2 z_b2 l1 zed ch p_point)
	(setq ch -1)
  (if (vl-consp (caddr lst))
  	(repeat (/ (length lst) 3)
		(progn
		    	(setq 	dist (nth (+ 1 ch) lst)
				int (nth (+ 2 ch) lst)
				a2 (car (nth (+ 3 ch) lst))
				z_a2 (last a2)
				b2 (car (cdr (nth (+ 3 ch) lst)))
				z_b2 (last b2)
		   	);setq
		  	(if (< z_a2 z_b2)
		    		(setq l1 (/ (* (distance a2 int) (abs (- z_a2 z_b2))) (distance a2 (reverse (cdr (reverse b2)))))
					zed (+ z_a2 l1)
				 );setq
			  	(setq l1 (/ (* (distance b2 int) (abs (- z_a2 z_b2))) (distance a2 (reverse (cdr (reverse b2)))))
				      zed (+ z_b2 l1)
				  );setq
			  );if
			  (if (< ch 2) (setq p_list (list pipe dist zed)) (setq p_list (append (list pipe dist zed) p_list)))
			  (setq ch (+ 3 ch));setq
		  );progn
	  );repeat
       (2dpipepoint surfline)    
    );if
  (princ)
);defun
;;;конец pipe_point