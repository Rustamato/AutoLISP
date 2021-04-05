;;;Функция расчета глубины заложения характерных точек коммуникации на профиле

(defun c:tube ( / pov skv obj plist1 plist2 tekst tekstpoint a b ch)
  (while (not (and pov skv)) 
	  (setq pov (vlax-ename->vla-object (car (entsel "\nУкажите линию ПОВЕРХНОСТИ земли на профиле: ")))
		skv (vlax-ename->vla-object (car (entsel "\nУкажите на профиле линию, глубину ХАРАКТЕРНЫХ ТОЧЕК которой надо рассчитать: ")))
		ch 0
		);setq
    (if (not (and pov skv)) (princ "\nУказаных линий не достаточно для функции. Повторите ввод.\n"));if
    );while
 (setq skv (vlax-safearray->list (vlax-variant-value (vlax-get-property skv 'coordinates))))
(repeat (/ (length skv) 2)
  (setq a (list (nth ch skv) (nth (+ ch 1) skv))
	b (mapcar '- a '(0 50))
	plist1 (append a b))
  (setq obj  (vla-addLightWeightPolyline model_space (vlax-make-variant (vlax-safearray-fill
									  (vlax-make-safearray vlax-vbDouble (cons 1 (length plist1))) plist1))))
    ;;Вычисление пересечения
    (setq plist2 (vla-IntersectWith pov obj acExtendOtherEntity)
    	  plist2 (vlax-variant-value plist2));setq
    (if (>= (vlax-safearray-get-u-bound plist2 1) (vlax-safearray-get-l-bound plist2 1)) (setq plist2 (vlax-safearray->list plist2)));if
    (vla-delete obj)
    
    ;;Задаем текст
    (setq tekst (rtos (- (cadr plist2) (cadr plist1)) 2 2))

;;;определяем точку вставки текста
    (setq tekstpoint (mapcar '- plist1 (list 0.33 (* 0.8 mashtab))))
    (vla-addtext model_space tekst (vlax-3D-point tekstpoint) (* 0.25 mashtab))
    (setq ch (+ ch 2))
);repeat  
  (princ "\n\"Tube\" is OK")
  (princ)
);defun
	  

    