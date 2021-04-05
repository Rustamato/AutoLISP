;;;Функция построения коммуникаций круглого сечения
;;;par1 - список возвращаемый функцией pipe_point

(defun krugpipe (par1 / obj dist zed v-obj znak rad lstobj otmetka txt sizetxt diam material otmtxt zed tekst tekstpoint stpoint endpoint points)
     (setq obj (car  par1)
	  dist (cadr par1)
	  zed (caddr par1)
	  v-obj (vlax-ename->vla-object obj)
	  znak (vlax-ldata-get v-obj "cent")
	  
      );setq
    (if (= znak "top") (setq znak -1) (setq znak 1))
    (setq center (mapcar '+ (list dist zed 0) point_list)
	  rad (* (vlax-ldata-get v-obj "diam") 0.5)
	  center (mapcar '+ center (list 0 (* znak rad) 0))
	  );setq
  
   (setq lstobj (vla-addcircle model_space (vlax-3D-point center) rad))
  ;строим выносную линию и текст
(setq stpoint (mapcar '- center (list 0 rad 0))
      endpoint (mapcar '+ (list dist horizon 0) point_list)
      points (append (list (car stpoint) (cadr stpoint)) (list (car endpoint) (cadr endpoint)))
      );setq
   (vla-addLightWeightPolyline model_space (vlax-make-variant (vlax-safearray-fill
									  (vlax-make-safearray vlax-vbDouble (cons 1 (length points))) points)))
  ;задаем содержание текста
   (setq txt (vlax-ldata-get v-obj "txt")
	 sizetxt (vlax-ldata-get v-obj "sizetxt")
	 diam (rtos (* (vlax-ldata-get v-obj "diam") 1000) 2 0)
	 material (vlax-ldata-get v-obj "material")
	 otmtxt (vlax-ldata-get v-obj "otmtxt")
	 zed (rtos zed 2 2)
	 )
 (if (= (vlax-ldata-get v-obj "pipetype") "hand")
  (setq tekst (strcat (vlax-ldata-get v-obj "sizetxt") " " zed otmtxt)) ;(rtos (- (cadr (vlax-curve-getstartpoint lstobj)) (cadr point_list)) 2 2) (vlax-ldata-get v-obj "otmtxt"))) 
  (setq tekst (strcat txt " " sizetxt diam material " " zed otmtxt))
   );if
	 
  (if (vlax-ldata-get v-obj "vid")
    (cond
      ((= (vlax-ldata-get v-obj "vid") "Bd") (setq tekst (strcat tekst " " "БД")))
      ((= (vlax-ldata-get v-obj "vid") "Pr") (setq tekst (strcat tekst " " "пр.")))
      );cond
    );if
    
    ;ищем точку вставки текста
    (setq tekstpoint (mapcar '+ endpoint (list (* 0.1 -1 mashtab) (* 0.1 mashtab) 0)))
	
  (vla-rotate (vla-addtext model_space tekst (vlax-3D-point tekstpoint) (* 0.25 mashtab)) (vlax-3D-point tekstpoint) (/ pi 2))
  );defun
