;;;‘ункци€ составлени€ списка точек выбранной коммуникации (3DPoly или LWPolyline) pipe_list
;;глобальные переменные: pipelst
;;возвращает : pipelst	       
(defun pipe_list (p / contr vert_st vert_end lst_temp z)
  (setq z (vlax-ldata-get (vlax-ename->vla-object p) "otmetka"))  
  (if (= "LWPOLYLINE" (cdr (assoc '0 (entget p))))
    (setq lst_tmp (entget p)
          pipelst (mapcar 'cdr (vl-remove-if-not (function (lambda (x) (= '10 (car x)))) lst_tmp))
	  pipelst (mapcar '(lambda (x) (append x (list z))) pipelst)
	  );setq
    (progn
    	  (setq vert_st (entnext p)
		vert_end (entnext vert_st)
		lst_temp (list (cdr (assoc '10 (entget vert_st))) (cdr (assoc '10 (entget vert_end))))	;создаем список из двух первых вершин
		contr (cdr (assoc '0 (entget (entnext vert_end))))
	  );setq
	  (while (/= contr "SEQEND")
	    (setq vert_end (entnext vert_end)
		  lst_temp (append lst_temp (list (cdr (assoc '10 (entget vert_end)))))
		  contr (cdr (assoc '0 (entget (entnext vert_end))))
	    );setq
	  );while
	  (setq pipelst lst_temp)
      );progn
    );if
  ;(princ pipelst)
  (princ)
);defun	       
;;;конец pipe_list