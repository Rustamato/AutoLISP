;;;Функция поиска координаты Z для коммуникаций без
;;;абсолютной отметки глубины залегания
;;;par2 - имя 2d-полилинии, являющейся линией поверхности
;;;на профиле
;;;p_list - список возвращаемый функцией intersec для 2D-полилиний (прописан в pipe_point)

(defun 2dpipepoint (par2 / v-name a b v-list v-surf plist z point_list)
  (setq point_list (mapcar '- startpoint (list 0 horizon 0)))
  (repeat (/ (length lst) 3)
	   (setq a (mapcar '+ point_list (list (nth (1+ ch) lst) horizon))
		  b (mapcar '+ a '(0 100))
		  z (nth (+ 3 ch) lst)
		  v-list (append a b)
		  v-name (vla-addLightWeightPolyline model_space (vlax-make-variant
			 (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble (cons 1 (length v-list))) v-list)))
		  v-surf (vlax-ename->vla-object par2)
		  dist (nth (+ 1 ch) lst)
		  );setq
	    ;Вычисление пересечения
	  (setq plist (vla-IntersectWith v-surf v-name acExtendNone)
		plist (vlax-variant-value plist));setq
	  (mapcar '(lambda (x)
		      (if (>= (vlax-safearray-get-u-bound x 1)
			     (vlax-safearray-get-l-bound x 1))
		          (setq pl (vlax-safearray->list x)));if
		    ) (list plist));mapcar
	  (vla-delete v-name)
	  (setq ch (+ 3 ch)
		  zed (- (+ (cadr pl) z) (cadr point_list))
		  plist (list pipe dist zed)
	   );setq
    	  (if (> ch 2) (setq p_list (append plist p_list)) (setq p_list plist))
    );repeat
  ;(princ p_list)
  (princ)
);defun
