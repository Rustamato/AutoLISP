;;;;���������� ����� ������������ �� ����� �� ������� ��������� � ����������� �������������, ��������� �������������

(defun cable ( / v-listing listing pnt pipetype diam size sizetxt cent otmtxt otmetka lastobj txt vid)

  (initget 1)
  (setq pnt (list (trans (getpoint "\n������� ��������� ����� ���������: ") 1 0)))
  (initget 1)
  (setq listing (append (list (trans (getpoint (trans (car pnt) 0 1) "\n������� ��������� ����� ���������: ") 1 0)) pnt))
  (grdraw (trans (car listing) 0 1) (trans (cadr listing) 0 1) 1 1)
  (while (vl-consp (car pnt))
    (setq pnt (list (getpoint (trans (car listing) 0 1) "\n������� ��������� ����� ��������� <���������>: ")))
    (if (vl-consp (car pnt))
      (progn
	(grdraw (car pnt) (trans (car listing) 0 1) 1 1)
      (setq pnt (list (trans (car pnt) 1 0))
	listing (append pnt listing))
	);progn		
      );if
    );while
  (setq v-listing (apply 'append (mapcar 'list (mapcar 'car listing) (mapcar 'cadr listing))))
  ;������� 2d-��������� � ��������� ��� � lastobj ��� ���������� �������� �������
  (setq lastobj (vla-addLightWeightPolyline model_space (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble (cons 1 (length v-listing))) v-listing))))
  ;���������� ���� ������� ������������
  (initget "��� ����� ���������� �������� ������� ���� _Kabel kabelsvaZy kabsVet koRozia teleFon oruD")
  (setq pipetype (getkword "\n������� ��� ������ [���/�����/����������/��������/�������/����] <�������>: "))
  (if (not pipetype) (setq pipetype "hand"))
  (initget 7)
  (setq diam (getint "\n������� ���������� �������: ")
	cent "top"
   );setq
  
  (if (= diam 1)
  	(setq size (list 0.1 0.1))
  	(progn
	  	(initget 7)
		(setq a (getreal "\n������� ������ ������������ � ������: "))
		(initget 6)
		(setq b (getreal "\n������� ������ ������������ � ������ <0.10�>: "))
	  	(if b
		(setq size (list a b))
		 (setq size (list a 0.1)));if
        );progn
  );if
  (cond
    ((= pipetype "hand") (initget 1) (setq sizetxt (getstring T "\n������� ������������ ������������: "))
     			 (initget 7) (setq otmetka (* -1 (getreal "\n������� ������� ��������� �� ����������� � ������: ")))
     			 (setq otmtxt "�.�.")
     			 (initget 1 "������ ������� ������� _cheRn Zelen Krasn")
			 (setq color (getkword "\n������� ���� ������������ [������/�������/�������]: "))
			 (cond
			   ((= color "cheRn") (setq color 7))
			   ((= color "Zelen") (setq color 3))
			   ((= color "Krasn") (setq color 1))
			   );cond
     )
    ((= pipetype "Kabel") (setq sizetxt "���." otmtxt "�.�." otmetka -0.9 color 1))
    ((= pipetype "oruD") (setq sizetxt "��.����" otmtxt "�.��." otmetka -0.6 color 7))
    ((= pipetype "kabelsvaZy") (setq sizetxt "��.�." otmtxt "�.�." otmetka -0.9 color 3))
    ((= pipetype "kabsVet") (setq sizetxt "�." otmtxt "�.�." otmetka -0.6 color 1))
    ((= pipetype "koRozia") (setq sizetxt "�. ����������" otmtxt "�.�." otmetka -0.9 color 1))
    ((= pipetype "teleFon") (setq txt "�������" sizetxt "���." otmtxt "�.��." color 3)
     (initget 7) (setq otmetka (* -1 (getreal "\n������� ������� ��������� �� ����������� � ������: "))))
    );cond
  (if (or (= pipetype "teleFon") (= pipetype "hand"))
    (if (= pipetype "teleFon")
    (setq sizetxt (strcat txt " " (itoa diam) sizetxt))
      (setq sizetxt (strcat (itoa diam) sizetxt))
      );if
   );if
  (initget "�������������� ������������� _Bd Pr")
  (setq vid (getkword "\n������� ��� ������������ [��������������/�������������] <�����������>: "))
  ;����������� �������� ������������ ����������� �����
  (vla-put-layer lastobj "gnb_pipes")
(vla-put-color lastobj color)
(vlax-ldata-put lastobj "diam" diam)
(vlax-ldata-put lastobj "pipetype" pipetype)
(vlax-ldata-put lastobj "size" size)
(vlax-ldata-put lastobj "sizetxt" sizetxt)
(vlax-ldata-put lastobj "cent" cent)
(vlax-ldata-put lastobj "otmtxt" otmtxt)
(vlax-ldata-put lastobj "otmetka" otmetka)
  (if txt (vlax-ldata-put lastobj "txt" txt))
(vlax-ldata-put lastobj "vid" vid)
(princ "\n\"Cable\" is OK")
(princ)
);defun