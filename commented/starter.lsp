;;;��������� ������� ��� ������� ��������� ������
;���������� ����������
;mashtab - ������� �������
;horizon - ������� ��������� ���������
;startpoint - ���������� ������ ������� � ���

(defun c:starter ( / ucs_list ucsname)
  (if (/= (type model_space) "vla-object") (begin_activex))
  (initget 7 "100 200")
  (setq mashtab (/ (atoi (getkword "\n� ����� �������� ����� �������� ������� 1 � [100/200]? ")) 100))
  (initget 1)
  (setq startpoint (trans (getpoint "\n������� ����� ������ ������� �������: ") 1 0))
  (initget 1)
  (setq horizon (getreal "\n������� �������� ��������� ���������: ")
	point_list (mapcar '- startpoint (list 0 horizon 0)))

  ;������� ���� ��� ����� ������ ����������� � ������������
  (if (not (tblsearch "LAYER" "gnb_pipes"))
  (entmakex '((0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord") (2 . "gnb_pipes") (70 . 0) (62 . 7) (6 . "Continuous") (290 . 1) (370 . -3))));if
  (if (not (tblsearch "LAYER" "gnb_poverhnost"))
  (entmakex '((0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord") (2 . "gnb_poverhnost") (70 . 0) (62 . 7) (6 . "Continuous") (290 . 1) (370 . -3))));if
  
  ;������� ��������� ����� ��� �������� ������� ����������� �������
  (entmake '((0 . "STYLE") (100 . "AcDbSymbolTableRecord") (100 . "AcDbTextStyleTableRecord") (2 . "GOST M1_500 GNB") (70 . 0) (40 . 1.25) (41 . 1.0) (50 . 0.0) (71 . 0) (42 . 0.3) (3 . "CS_Gost2304.shx") (4 . "")))
  (entmake '((0 . "STYLE") (100 . "AcDbSymbolTableRecord") (100 . "AcDbTextStyleTableRecord") (2 . "GOST M1_100 GNB") (70 . 0) (40 . 0.25) (41 . 1.0) (50 . 0.0) (71 . 0) (42 . 5.0) (3 . "CS_Gost2304.shx") (4 . "")))
  (entmake '((0 . "STYLE") (100 . "AcDbSymbolTableRecord") (100 . "AcDbTextStyleTableRecord") (2 . "GOST M1_200 GNB") (70 . 0) (40 . 0.5) (41 . 1.0) (50 . 0.0) (71 . 0) (42 . 5.0) (3 . "CS_Gost2304.shx") (4 . "")))
(princ "\n\"Starter\" is OK")
(princ)
);defun

