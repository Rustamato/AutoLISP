;���������� ����� �����������
;������������ �������� v-listing - ������ ��������� ����� 3d-���������
;� ������� ������ vla-Add3Dpoly

(defun truba ( / listing pnt1 pnt z1 z2 z12 dist tmp_listing)
  (initget 1)
  (setq pnt1 (trans (getpoint "\n������� ������ ������� ���������: ") 1 0)
	z1 (getreal "\n������� ���������� Z ��������� �����:")
	listing (list (list (car pnt1) (cadr pnt1) z1))
	z2 nil
	dist 0
	pnt T
	);setq
;���� ����� ������ ���������
  (while (/= pnt "end")
	;���� �������� ������� �����
    (while (not (or (listp pnt) (= pnt "end") (not pnt)))
    	(initget 128)
    	(setq pnt (getpoint (trans (car listing) 0 1) "\n������� ������� ��������� ��� END, ����� ��������� ���� ����� <�������������>: "))
        (if (vl-consp pnt) (setq pnt (trans pnt 1 0)))
      );while ���� �������� ������� �����
    (if (/= pnt "end")
    
;����� �������� � ����������� �� ����������� �����
    (if (not pnt)
      
        ;���� ������������� �����
      (progn
	(setq tmp_listing (list (car listing)));setq
	(while (/= pnt "no")
	  (setq pnt (getpoint (trans (car tmp_listing) 0 1) "\n������� ������������� ����� <��������� ���� ������������� �����>: "))
	  (if (vl-consp pnt) (setq pnt (trans pnt 1 0)))	  
	  (if (not pnt) (setq pnt "no"))

	  (if (/= pnt "no")
	    ;����� �������� � ����������� �� ����������� �����
	    (progn
	      (grdraw (trans (car tmp_listing) 0 1) (trans pnt 0 1) 5 1)
	      (setq pnt (list (car pnt) (cadr pnt))
		    
		    dist (+ dist (distance (car tmp_listing) pnt))
		    tmp_listing (cons (append pnt (list dist)) tmp_listing)		     
		);setq
	    );progn � if
	  );if
	 );while ���� ������������ ������ (X Y) ������������� �����
	  (initget 1)
	  (setq z2 (getreal "\n������� ���������� Z ��������� ������������� �����: ")
		z1 (last (last tmp_listing))
		z12 (/ (- z2 z1) dist)
		tmp_listing (reverse (mapcar '(lambda (x)
				       (subst  (+ z1 (* z12 (last x))) (last x) x))
				    (cdr (reverse tmp_listing))))
		listing (append tmp_listing listing)
		dist 0
		
		);setq
       );progn ���� ����� ������������� �����

      ;���� ����� �������� �����
      (progn
	(grdraw (trans (car listing) 0 1) (trans pnt 0 1) 5 1)
	(initget 1)
	(setq z1 (getreal "\n������� ���������� Z ��������� �����: ")
	      pnt (list (car pnt) (cadr pnt) z1)
	      listing (cons pnt listing)
	      pnt T
	   );setq
	);progn
     );if ������ ���� �����
    
   );if end

);while ���� ����� ������ ���������

  ;����������� ���������� ������ � ������ vla-add3dpoly
 (repeat (length listing)
  (setq v-listing (append v-listing (car listing))
	listing (cdr listing)
	);setq
  );repeat �������������� � ������ vla-add3dpoly
  
(princ "\n\"Truba\" is OK")
  (princ)
);defun
			
			
    
    

		    
		    
		    
  