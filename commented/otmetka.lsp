;;;Функция заполнения отметок точек в таблице

(defun c:otmetka ( / pov obj linedown plist tekst tekstpoint a b)
  (while (not (and pov obj linedown)) 
	  (setq pov (vlax-ename->vla-object (car (entsel "\nУкажите ИЗМЕРЯЕМУЮ ЛИНИЮ на профиле: ")))
		linedown (vlax-ename->vla-object (car (entsel "\nУкажите ОПОРНУЮ линию в графе таблицы: ")))
		obj (entsel "\nУкажите ВЕРТИКАЛЬНУЮ линию построения в таблице: ")
		);setq
    (if (not (and pov obj linedowm)) (princ "\nУказаных линий не достаточно для функции. Повторите ввод."));if
    );while
  (while obj
    (setq obj (vlax-ename->vla-object (car obj)))

;;;определяем содержание текста
    ;;Вычисление пересечения
    (setq plist (vla-IntersectWith pov obj acExtendOtherEntity)
    	  plist (vlax-variant-value plist));setq
    (if (>= (vlax-safearray-get-u-bound plist 1) (vlax-safearray-get-l-bound plist 1)) (setq plist (vlax-safearray->list plist)));if
    ;;Задаем текст
    (setq tekst (rtos (- (cadr plist) (cadr point_list)) 2 2))

;;;определяем точку вставки текста
    (setq tekstpoint (vla-IntersectWith linedown obj acExtendOtherEntity)
	  tekstpoint (vlax-safearray->list (vlax-variant-value tekstpoint))
	  a (- (car tekstpoint) (* mashtab 0.125))
	  b (+ (cadr tekstpoint) (* mashtab 0.2))
	  tekstpoint (list a b 0))
    (vla-rotate (vla-addtext model_space tekst (vlax-3D-point tekstpoint) (* 0.25 mashtab)) (vlax-3D-point tekstpoint) (/ pi 2))

    (setq obj (entsel "\nУкажите СЛЕДУЮЩУЮ вертикальную линию в таблице <завершить>: "))
    );while
  (princ "\n\"Otmetka\" is OK")
  (princ)
  );defun
	  

    