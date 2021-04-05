;;;Функция заполнения глубин точек в таблице

(defun c:glubina ( / pov skv obj linedown plist1 plist2 tekst tekstpoint a b)
  (while (not (and pov obj linedown)) 
	  (setq pov (vlax-ename->vla-object (car (entsel "\nУкажите линию ПОВЕРХНОСТИ земли на профиле: ")))
		skv (vlax-ename->vla-object (car (entsel "\nУкажите линию, глубину которой надо измерить на профиле: ")))
		linedown (vlax-ename->vla-object (car (entsel "\nУкажите ОПОРНУЮ линию для размещения текста в таблице: ")))
		obj (entsel "\nУкажите ВЕРТИКАЛЬНУЮ линию в таблице: ")
		);setq
    (if (not (and pov obj linedowm)) (princ "\nУказаных линий не достаточно для функции. Повторите ввод.\n"));if
    );while
  (while obj
    (setq obj (vlax-ename->vla-object (car obj)))

;;;определяем содержание текста
    ;;Вычисление пересечения
    (setq plist1 (vla-IntersectWith pov obj acExtendOtherEntity)
    	  plist1 (vlax-variant-value plist1));setq
    (if (>= (vlax-safearray-get-u-bound plist1 1) (vlax-safearray-get-l-bound plist1 1)) (setq plist1 (vlax-safearray->list plist1)));if
    
    (setq plist2 (vla-IntersectWith skv obj acExtendOtherEntity)
    	  plist2 (vlax-variant-value plist2));setq
    (if (>= (vlax-safearray-get-u-bound plist2 1) (vlax-safearray-get-l-bound plist2 1)) (setq plist2 (vlax-safearray->list plist2)));if
    ;;Задаем текст
    (setq tekst (rtos (- (cadr plist1) (cadr plist2)) 2 2))

;;;определяем точку вставки текста
    (setq tekstpoint (vla-IntersectWith linedown obj acExtendOtherEntity)
	  tekstpoint (vlax-safearray->list (vlax-variant-value tekstpoint))
	  a (- (car tekstpoint) (* mashtab 0.125))
	  b (+ (cadr tekstpoint) (* mashtab 0.05))
	  tekstpoint (list a b 0))
    (vla-rotate (vla-addtext model_space tekst (vlax-3D-point tekstpoint) (* 0.25 mashtab)) (vlax-3D-point tekstpoint) (/ pi 2))

    (setq obj (entsel "\nУкажите СЛЕДУЮЩУЮ вертикальную линию в таблице <завершить>: "))
    );while
  (princ "\n\"Glubina\" is OK")
  (princ)
  );defun
	  

    