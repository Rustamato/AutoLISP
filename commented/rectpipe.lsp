;;;Функция построения коммуникации прямоугольного сечения
;;;par1 - список в формате выдаваемом функцией pipe_point
(defun rectpipe (par1 / obj v-obj v-listing dist zed znak ch2 listing lstobj stpoint endpoint points lstobj tekst tekstpoint point_lst)
    (setq obj (car par1)
	  dist (cadr par1)
	  zed (caddr par1)
	  v-obj (vlax-ename->vla-object obj)
	  znak (vlax-ldata-get v-obj "cent")
	  ch2 -1
	  point_lst (mapcar '- startpoint (list 0 horizon 0))
	  );setq
    (if (= znak "top") (setq znak -1) (setq znak 1))
    (setq v-listing (mapcar '+ (list dist zed 0) point_lst)
	  v-listing (append (mapcar '- v-listing (list (/ (car (vlax-ldata-get v-obj "size")) 2) 0 0)) v-listing)
	  v-listing (append (mapcar '+ (list (car v-listing) (cadr v-listing) (caddr v-listing)) (list 0 (/ (* znak (cadr (vlax-ldata-get v-obj "size"))) 1) 0)) v-listing)
	  v-listing (append (mapcar '+ (list (car v-listing) (cadr v-listing) (caddr v-listing)) (list (/ (car (vlax-ldata-get v-obj "size")) 1) 0 0)) v-listing)
	  v-listing (append (mapcar '- (list (car v-listing) (cadr v-listing) (caddr v-listing)) (list 0 (/ (* znak (cadr (vlax-ldata-get v-obj "size"))) 1) 0)) v-listing)
	  v-listing (append (mapcar '- (list (car v-listing) (cadr v-listing) (caddr v-listing)) (list (/ (car (vlax-ldata-get v-obj "size")) 2) 0 0)) v-listing)
	  listing (list (nth (+ 1 ch2) v-listing) (nth (+ 2 ch2) v-listing))
	  v-listing (cdddr v-listing)
	  );setq
    (repeat (/ (length v-listing) 3)
      (setq listing (append (list (nth (+ 1 ch2) v-listing) (nth (+ 2 ch2) v-listing)) listing)
	    ch2 (+ 3 ch2));setq
     );repeat
	    
;строим коммуникацию на профиле
(setq lstobj (vla-addLightWeightPolyline model_space (vlax-make-variant (vlax-safearray-fill
									  (vlax-make-safearray vlax-vbDouble (cons 1 (length listing))) listing))))
      

;строим выносную линию и текст
(if (minusp znak) (setq stpoint (mapcar '+ (vlax-curve-getstartpoint lstobj) (list 0 (* znak (cadr (vlax-ldata-get v-obj "size"))) 0)))
  (setq stpoint (vlax-curve-getstartpoint lstobj))
  );if
(setq endpoint (mapcar '+ (list dist horizon 0) point_lst)
      points (append (list (car stpoint) (cadr stpoint)) (list (car endpoint) (cadr endpoint)))
      );setq
  (vla-addLightWeightPolyline model_space (vlax-make-variant (vlax-safearray-fill
									  (vlax-make-safearray vlax-vbDouble (cons 1 (length points))) points)))
  ;задаем содержание текста
  (if (or (= (vlax-ldata-get v-obj "pipetype") "hand") (= (vlax-ldata-get v-obj "pipetype") "teleFon"))
  (setq tekst (strcat (vlax-ldata-get v-obj "sizetxt") " " (rtos (- (cadr (vlax-curve-getstartpoint lstobj)) (cadr point_list)) 2 2) (vlax-ldata-get v-obj "otmtxt"))) 
  (progn  
  (if (vlax-ldata-get v-obj "txt")
   (setq tekst (strcat (vlax-ldata-get v-obj "txt") " " (rtos (vlax-ldata-get v-obj "diam") 2 0) (vlax-ldata-get v-obj "sizetxt") " " (rtos (- (cadr (vlax-curve-getstartpoint lstobj)) (cadr point_list)) 2 2) (vlax-ldata-get v-obj "otmtxt"))) 
   (setq tekst (strcat (rtos (vlax-ldata-get v-obj "diam") 2 0) (vlax-ldata-get v-obj "sizetxt") " " (rtos (- (cadr (vlax-curve-getstartpoint lstobj)) (cadr point_list)) 2 2) (vlax-ldata-get v-obj "otmtxt")))
    );if
  );progn
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
  (princ)
);defun