;;;������� ����������� ������ ����� ����� ������� (3DPoly) axis_list
;;���������� ����������: obj
;;���������� axlst	       
(defun axis_list (axis / contr vert_st vert_end)
  ;(setq axlst nil)
    (if (/= (cdr (assoc '0 (entget axis))) "POLYLINE")	;��������� ��� ���������� �������
  	(princ "\n�������� ��� ������� ��� ����� �������.")
    	(progn
	  (setq vert_st (entnext axis)
		vert_end (entnext vert_st)
		axlst (list (reverse (cdr (reverse (cdr (assoc '10 (entget vert_st))))))
			    (reverse (cdr (reverse (cdr (assoc '10 (entget vert_end)))))))	;������� ������ �� ���� ������ ������
		contr (cdr (assoc '0 (entget (entnext vert_end))))
	  );setq
	  (while (/= contr "SEQEND")
	    (setq vert_end (entnext vert_end)
		  axlst (append axlst (list (reverse (cdr (reverse (cdr (assoc '10 (entget vert_end))))))))
		  contr (cdr (assoc '0 (entget (entnext vert_end))))
	    );setq
	  );while
	 );progn
    );if
);defun
;;;����� axis_list