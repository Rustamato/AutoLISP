;;;������� �������� ��� �������� �� ����������� �����
(defun c:axpoint ( / skv prof begin plist pnt dist)
  (setq prof (vlax-ename->vla-object (car (entsel "\n������� 2D-����� ����������� ����� � ������� : ")))
	skv (vlax-ename->vla-object (car (entsel "\n������� 2D-����� ��� ������� �� �����: ")))
	begin (osnap (cadr (entsel "\n������� ����� ������ ����� ��� ������� �� �����: ")) "_END")
	);setq
  (if (equal (vlax-curve-getstartpoint skv) begin)
    (progn
      (setq plist (vlax-safearray->list (vlax-variant-value (vlax-get-property prof 'coordinates)))
	    begin (list (car plist) (cadr plist))
	    plist (cddr plist)
	    );setq
      (while plist
	(setq dist (distance begin (list (car plist) (cadr plist))))
	(if (> dist (vlax-get-property skv 'length)) (setq pnt (vlax-curve-getendpoint skv))
	  (setq pnt (vlax-curve-getPointAtDist skv dist))
	  );if
      (vla-AddPoint model_space (vlax-3D-Point pnt))
	(setq plist (cddr plist))
	      );while
      
      (if (= (distof (rtos (vlax-get-property prof 'length) 2 3)) (distof (rtos (vlax-get-property skv 'length) 2 3)))
	      (progn
		  (princ "\n\"Axpoint\" is OK")
		  (princ)
	       );progn
	      (if (> (vlax-get-property prof 'length) (vlax-get-property skv 'length))
		(progn
		  (princ "\n��������!!! ����� ����� �� ������� ������ ��� �� �����!")
		  (princ)
		  );progn
		(progn
		  (princ "\n��������!!! ����� ����� �� ����� ������ ��� �� �������!")
		  (princ)
		  );progn
		);if
	);if
      );progn
    (progn
      (princ "\n����� ������ ����� �� ����� �� ��������� � ���������! �������� ��������� � ��������� �������.")
    (princ));progn
    );if
   
  );defun