;;;������� ���������� ����� ������������ �� ������������

(defun c:pipeline ( / lastobj v-listing pask ask)
  (if (/= (type model_space) "vla-object") (begin_activex))
   (while (/= ask "No")
     (initget "������� ������ _Truba Kabel")
     (setq pask (getkword "\n�������� ��� ������������ [�������/������] <�������>: "))
	(if (not pask) (setq pask "Truba"))
     (if (= pask "Truba")
       (progn
	;������� ������ ��������� ����� ����������� � ������� vla-add3DPoly
	(truba)
	;������� ���������
        (vodoprovod v-listing)
	);progn
       (cable)
       );if
	(initget "�� ��� _Yes No")
	(setq ask (getkword "\n���������� ���������� ������������ [��/���] <����������>?: ")
	      v-listing nil)
	(if (not ask) (setq ask "Yes"))
    );while
  (princ "\n\"Pipeline\" is OK")
  (princ)
);defun