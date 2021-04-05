;;;;Построение линии коммуникации с присвоением характеристик, введенных пользователем
;;;;В качестве параметра par1 передается список координат точек в формате add3DPoly


(defun vodoprovod (par1 / pipetype txt diam size sizetxt material cent otmetka otmtxt color lastobj vid)
;создаем полилинию и сохраняем имя в lastobj для присвоения значений свойств
 (setq lastobj (vla-add3dpoly model_space (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble (cons 1 (length par1))) par1)))
;Выбираем тип коммуникации
  (initget "Водопровод щиТ водоСток Дренаж Канализация Газопровод телеФон _Vodoprovod Tonnel vodoStok Drenazh Kanalizacia Gaz teleFon")
  (setq pipetype (getkword "\nУкажите тип подземной коммуникации [Водопровод/щиТ/водоСток/Дренаж/Канализация/Газопровод/телеФон] <вручную>: "))
  (if (not pipetype) (setq pipetype "hand"))
  (cond
    ((= pipetype "Vodoprovod") (initget 3) (setq txt "водопровод" cent "top" otmtxt "в.тр." color 5 sizetxt "d=" diam (getreal "\nЗадайте величину диаметра коммуникации в метрах: "))
			      (setq material (getstring "\nУкажите материал трубы <сталь>: ")) (if (= (strlen material) 0) (setq material "ст.")))
    ((= pipetype "Tonnel") (initget 3) (setq txt "щит" sizetxt "d=" diam (getreal "\nЗадайте величину диаметра коммуникации в метрах: ") material (getstring T "\nУкажите материал трубы: ") color 7) (initget 1 "Верх Низ _top bot")
			 (setq cent (getkword "\nУкажите к чему привязана отметка коммуникации [Верх/Низ]: "))
			 (if (= cent "top") (setq otmtxt "верх") (setq otmtxt "низ")))
    ((= pipetype "vodoStok") (initget 3) (setq txt "водосток" cent "bot" otmtxt "лот." sizetxt "d=" diam (getreal "\nЗадайте величину диаметра коммуникации в метрах: ") color 30)
     				(setq material (getstring T "\nУкажите материал трубы <ж.б.>: ")) (if (= (strlen material) 0) (setq material "ж.б.")))
    ((= pipetype "Drenazh") (initget 3) (setq txt "дренаж" cent "bot" otmtxt "лот." sizetxt "d=" diam (getreal "\nЗадайте величину диаметра коммуникации в метрах: ") color 30)
     				(setq material (getstring T "\nУкажите материал трубы <ж.б.>: ")) (if (= (strlen material) 0) (setq material "ж.б.")))
    ((= pipetype "Kanalizacia") (initget 3) (setq txt "канализация" cent "bot" otmtxt "лот." sizetxt "d=" diam (getreal "\nЗадайте величину диаметра коммуникации в метрах: ") color 16)
     				(setq material (getstring "\nУкажите материал трубы <ж.б.>: ")) (if (= (strlen material) 0) (setq material "ж.б.")))
    ((= pipetype "teleFon") (initget 3) (setq txt "телефон" cent "top" otmtxt "в.тр." color 3) (setq sizetxt (strcat (getstring T "\nУкажите количество отверстий: ") "отв.") material " ")
     				(progn (initget 7)
			      	(setq a (getreal "\nЗадайте ширину коммуникации в метрах: "))
			      	(initget 7)
			      	(setq b (getreal "\nЗадайте высоту коммуникации в метрах: "))
			      	(setq size (list a b))
			      	);progn
     )
     				
    ((= pipetype "Gaz") (initget 3) (setq txt "газопровод" cent "top" otmtxt "в.тр." sizetxt "d=" diam (getreal "\nЗадайте величину диаметра коммуникации в метрах: ") material (getstring T "\nУкажите давление и материал газопровода: ") color 3))
    ((= pipetype "hand") (initget 1)
			 (setq txt (getstring T "\nВведите название коммуникации: "))
			       
			  (initget 1 "пРямоугольная Круглая _squaRe Krug")
			  (if (= "Krug" (getkword "\nУкажите форму сечения коммуникации [пРямоугольная/Круглая]: "))
			    (progn (initget 7)
			      (setq diam (getreal "\nЗадайте величину диаметра коммуникации в метрах: ")));progn
			    (progn (initget 7)
			      (setq a (getreal "\nЗадайте ширину коммуникации в метрах: "))
			      (initget 7)
			      (setq b (getreal "\nЗадайте высоту коммуникации в метрах: "))
			      (setq size (list a b))
			      );progn
			    );if

			   (if diam (setq sizetxt "d=")
			     (setq sizetxt (strcat (rtos (* 1000 (car size)) 2 0) "x" (rtos (* 1000 (cadr size)) 2 0)))
			     );if
     			 (setq material (getstring T "\nУкажите материал коммуникации <пропустить>: "))
			 (initget 1 "Верх Низ _top bot")
			 (setq cent (getkword "\nУкажите к чему привязана отметка коммуникации [Верх/Низ]: "))
			 (if (= cent "top") (setq otmtxt "верх") (setq otmtxt "низ"))

     			 (initget 1 "чеРный Зеленый Красный Оранжевый коричНевый сИний _cheRn Zelen Krasn Oranzh korichN sIniy")
			 (setq color (getkword "\nУкажите цвет коммуникации [чеРный/Зеленый/Красный/Оранжевый/коричНевый/сИний]: "))
			 (cond
			   ((= color "cheRn") (setq color 7))
			   ((= color "Zelen") (setq color 3))
			   ((= color "Krasn") (setq color 1))
			   ((= color "Oranzh") (setq color 30))
			   ((= color "korichN") (setq color 16))
			   ((= color "sIniy") (setq color 5))
			   );cond
			)
    );cond
  (if (or (= pipetype "teleFon") (= pipetype "hand"))
    (if diam
	    (setq sizetxt (strcat txt " " sizetxt (rtos (* diam 1000) 2 0) material))
	    (setq sizetxt (strcat txt " " sizetxt " " material))
    );if
   );if
  (initget "Бездействующая Проектируемая _Bd Pr")
  (setq vid (getkword "\nУкажите вид коммуникации [Бездействующая/Проектируемая] <действующая>: "))

;Присваиваем свойства к построенной коммуникации
(vla-put-layer lastobj "gnb_pipes")
(vla-put-color lastobj color)
(vlax-ldata-put lastobj "pipetype" pipetype)
(vlax-ldata-put lastobj "txt" txt)
(vlax-ldata-put lastobj "diam" diam)
(vlax-ldata-put lastobj "size" size)
(vlax-ldata-put lastobj "sizetxt" sizetxt)
(vlax-ldata-put lastobj "material" material)
(vlax-ldata-put lastobj "cent" cent)
(vlax-ldata-put lastobj "otmtxt" otmtxt)
(vlax-ldata-put lastobj "vid" vid)
(princ "\n\"Vodoprovod\" is OK")
(princ)
  );defun
  