;;;Функция построения линий коммуникаций на геоподоснове

(defun c:pipeline ( / lastobj v-listing pask ask)
  (if (/= (type model_space) "vla-object") (begin_activex))
   (while (/= ask "No")
     (initget "оТметки Кабель _Truba Kabel")
     (setq pask (getkword "\nВыберите тип коммуникации [оТметки/Кабель] <отметки>: "))
	(if (not pask) (setq pask "Truba"))
     (if (= pask "Truba")
       (progn
	;находим списки координат линий поверхности в формате vla-add3DPoly
	(truba)
	;создаем полилинию
        (vodoprovod v-listing)
	);progn
       (cable)
       );if
	(initget "Да Нет _Yes No")
	(setq ask (getkword "\nПродолжить построение коммуникаций [Да/Нет] <продолжить>?: ")
	      v-listing nil)
	(if (not ask) (setq ask "Yes"))
    );while
  (princ "\n\"Pipeline\" is OK")
  (princ)
);defun