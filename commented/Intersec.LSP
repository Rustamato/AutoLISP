;;;Поиск координат точек пересечения int (x y) линии оси и выбранной коммуникации
;;;и дистанции dist точки пересечения от начала оси и вывод их в виде списка intlst
;;;в формате (<dist (intx inty) ((x_a2 y_a2 z_a2) (x_b2 y_b2 z_b2))>...) 
;;axlst pipelst - списки координат точек линий оси и ком-ии соответственно
;;возвращает intlst - искомый сиписок
(defun intersec (list1 list2 / a1 b1 a2 b2 int dist1 dist2 n1 n2 ch1 ch2 intlst_tmp)
(setq n1 (1- (length list1))
      n2 (1- (length list2))
      dist1 0
      dist2 0
      ch1 0
      );setq
  ;;цикл перебора вершин линии оси
  (repeat n1
	  (progn
	    (setq a1 (nth ch1 list1)
		  b1 (list (car (nth (1+ ch1) list1)) (car (cdr (nth (1+ ch1) list1))))
		  ch1 (1+ ch1)
		  ch2 0
		);setq

	    ;;цикл перебора вершин линии коммуникации
	    (repeat n2
	      (progn
		(setq a2 (nth ch2 list2)
		      b2 (nth (1+ ch2) list2)
		      int (inters a1 b1 a2 b2)
		      ch2 (1+ ch2)
		 );setq
		      	(if int
			  (if (not (minusp (last (car list2))))
			      (setq dist2 (+ dist1 (distance a1 int))
				  intlst_tmp (append intlst_tmp (list dist2 int (list a2 b2)))
			     );setq
			      (setq dist2 (+ dist1 (distance a1 int))
				    intlst_tmp (append intlst_tmp (list dist2 int (last (car list2))))
				    );setq
			    );if
			);if
		   
		);progn
	      );repeat
	    (setq dist1 (+ dist1 (distance a1 b1)));setq
	    );progn
    );repeat
(setq intlst intlst_tmp)
  );defun
;;;конец intersec