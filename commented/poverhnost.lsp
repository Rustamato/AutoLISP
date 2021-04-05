;;;;;;Определим функцию построения профиля поверхности по выбранной линии сечения
;;;;;;и набору 3D-полилиний, определяющих строение рельефа на черетеже

(defun c:poverhnost ( / obj axlst pipe pipelst intlst poverh_set num1 n_rep listall p_list)
(if (/= (type model_space) vla-object) (begin_activex))
  
;;;Построение найденных точек пересечения оси скважины и линий
;;;поверхности на профиле 
(defun construct (list_tmp / st hor st_ucs ucs_list ch point_list p_list)
  (setq ch -1
	point_list (mapcar '- startpoint (list 0 horizon 0))
   );setq
  ;рисуем точки
  (repeat (/ (length listall) 3) 
      (setq p_list (mapcar '+ point_list (list (nth (+ ch 2) list_tmp) (nth (+ ch 3) list_tmp) 0))
	    ch (+ ch 3));setq
      (vla-AddPoint model_space (vlax-3D-Point p_list))
   );repeat
);defun construct

	       
;;;Зададим тело функции poverhnost
(setq obj (car (entsel "\nВыберите линию оси скважины")))
(axis_list obj)
(setq 
      poverh_set (ssget "X" '((8 . "gnb_poverhnost") (0 . "POLYLINE")))
      n_rep (sslength poverh_set)
      num1 0
      listall (list 2 1)
 );setq
(repeat n_rep
	    (setq pipe (ssname poverh_set num1) num1 (1+ num1));setq
	    (pipe_list pipe)
	    (intersec axlst pipelst)
  (if intlst
    (progn
	    (pipepoint intlst)
	  (setq listall (append listall p_list))
      );progn
    );if
	 );repeat
  (setq listall (cddr listall))
(construct listall)

(princ "\n\"Poverhnost\" is OK")
(princ)
);defun poverhnost
;;;конец всего
      