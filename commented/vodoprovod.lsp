;;;;���������� ����� ������������ � ����������� �������������, ��������� �������������
;;;;� �������� ��������� par1 ���������� ������ ��������� ����� � ������� add3DPoly


(defun vodoprovod (par1 / pipetype txt diam size sizetxt material cent otmetka otmtxt color lastobj vid)
;������� ��������� � ��������� ��� � lastobj ��� ���������� �������� �������
 (setq lastobj (vla-add3dpoly model_space (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble (cons 1 (length par1))) par1)))
;�������� ��� ������������
  (initget "���������� ��� �������� ������ ����������� ���������� ������� _Vodoprovod Tonnel vodoStok Drenazh Kanalizacia Gaz teleFon")
  (setq pipetype (getkword "\n������� ��� ��������� ������������ [����������/���/��������/������/�����������/����������/�������] <�������>: "))
  (if (not pipetype) (setq pipetype "hand"))
  (cond
    ((= pipetype "Vodoprovod") (initget 3) (setq txt "����������" cent "top" otmtxt "�.��." color 5 sizetxt "d=" diam (getreal "\n������� �������� �������� ������������ � ������: "))
			      (setq material (getstring "\n������� �������� ����� <�����>: ")) (if (= (strlen material) 0) (setq material "��.")))
    ((= pipetype "Tonnel") (initget 3) (setq txt "���" sizetxt "d=" diam (getreal "\n������� �������� �������� ������������ � ������: ") material (getstring T "\n������� �������� �����: ") color 7) (initget 1 "���� ��� _top bot")
			 (setq cent (getkword "\n������� � ���� ��������� ������� ������������ [����/���]: "))
			 (if (= cent "top") (setq otmtxt "����") (setq otmtxt "���")))
    ((= pipetype "vodoStok") (initget 3) (setq txt "��������" cent "bot" otmtxt "���." sizetxt "d=" diam (getreal "\n������� �������� �������� ������������ � ������: ") color 30)
     				(setq material (getstring T "\n������� �������� ����� <�.�.>: ")) (if (= (strlen material) 0) (setq material "�.�.")))
    ((= pipetype "Drenazh") (initget 3) (setq txt "������" cent "bot" otmtxt "���." sizetxt "d=" diam (getreal "\n������� �������� �������� ������������ � ������: ") color 30)
     				(setq material (getstring T "\n������� �������� ����� <�.�.>: ")) (if (= (strlen material) 0) (setq material "�.�.")))
    ((= pipetype "Kanalizacia") (initget 3) (setq txt "�����������" cent "bot" otmtxt "���." sizetxt "d=" diam (getreal "\n������� �������� �������� ������������ � ������: ") color 16)
     				(setq material (getstring "\n������� �������� ����� <�.�.>: ")) (if (= (strlen material) 0) (setq material "�.�.")))
    ((= pipetype "teleFon") (initget 3) (setq txt "�������" cent "top" otmtxt "�.��." color 3) (setq sizetxt (strcat (getstring T "\n������� ���������� ���������: ") "���.") material " ")
     				(progn (initget 7)
			      	(setq a (getreal "\n������� ������ ������������ � ������: "))
			      	(initget 7)
			      	(setq b (getreal "\n������� ������ ������������ � ������: "))
			      	(setq size (list a b))
			      	);progn
     )
     				
    ((= pipetype "Gaz") (initget 3) (setq txt "����������" cent "top" otmtxt "�.��." sizetxt "d=" diam (getreal "\n������� �������� �������� ������������ � ������: ") material (getstring T "\n������� �������� � �������� �����������: ") color 3))
    ((= pipetype "hand") (initget 1)
			 (setq txt (getstring T "\n������� �������� ������������: "))
			       
			  (initget 1 "������������� ������� _squaRe Krug")
			  (if (= "Krug" (getkword "\n������� ����� ������� ������������ [�������������/�������]: "))
			    (progn (initget 7)
			      (setq diam (getreal "\n������� �������� �������� ������������ � ������: ")));progn
			    (progn (initget 7)
			      (setq a (getreal "\n������� ������ ������������ � ������: "))
			      (initget 7)
			      (setq b (getreal "\n������� ������ ������������ � ������: "))
			      (setq size (list a b))
			      );progn
			    );if

			   (if diam (setq sizetxt "d=")
			     (setq sizetxt (strcat (rtos (* 1000 (car size)) 2 0) "x" (rtos (* 1000 (cadr size)) 2 0)))
			     );if
     			 (setq material (getstring T "\n������� �������� ������������ <����������>: "))
			 (initget 1 "���� ��� _top bot")
			 (setq cent (getkword "\n������� � ���� ��������� ������� ������������ [����/���]: "))
			 (if (= cent "top") (setq otmtxt "����") (setq otmtxt "���"))

     			 (initget 1 "������ ������� ������� ��������� ���������� ����� _cheRn Zelen Krasn Oranzh korichN sIniy")
			 (setq color (getkword "\n������� ���� ������������ [������/�������/�������/���������/����������/�����]: "))
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
  (initget "�������������� ������������� _Bd Pr")
  (setq vid (getkword "\n������� ��� ������������ [��������������/�������������] <�����������>: "))

;����������� �������� � ����������� ������������
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
  