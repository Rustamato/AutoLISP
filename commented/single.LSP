;;;;;;��������� ������� ���������� ������� ������������ �� ��������� ����� �������
;;;;;;� ������ 3D-���������, ������������ �������� ������������ �� ��������

(defun c:single ( / ax axlst pipe pipelst intlst pipe_set num1 num2 listall tmp_list p_list surfline)
  (if (/= (type model_space) "vla-object") (begin_activex))
  (while (not ax)
	(setq ax (car (entsel "\n�������� ����� ��� ��������: ")))
    (if (not ax) (princ "\n������� ������� ��� ��������! ��������� ����."))
    );while
	(axis_list ax)
  (while (not surfline)
  	(setq surfline (car (entsel "\n������� 2D-��������� ����������� �� �������: ")))
    (if (not surfline) (princ "\n������� ������� ����� �����������! ��������� ����."))
    );while
	(setq 
	      num1 0
	      num2 -1
	      listall (list 2 1)
	 );setq
	
	    (setq pipe (car (entsel "\n������� ������������ �� �����:")));setq
	    (pipe_list pipe)
	    (intersec axlst pipelst)
	  (if (vl-consp intlst)
	    (progn
	    (pipepoint intlst)
	  (setq listall (append listall p_list))
	    );progn
	    );if
	 
  (setq listall (cddr listall))		

  	(repeat (/ (length listall) 3)
	  (setq pipe (nth (1+ num2) listall)
		tmp_list (list (nth (1+ num2) listall) (nth (+ 2 num2) listall) (nth (+ 3 num2) listall))
		num2 (+ 3 num2));setq
	  (if (vl-consp (vlax-ldata-get (vlax-ename->vla-object pipe) "size"))
	    (rectpipe tmp_list)
	    (krugpipe tmp_list)
	    );if
	  );repeat
	    
	  
	
	(princ "\n\"Pipes\" is OK")
	  (princ)
);defun pipes
;;;����� �����
      