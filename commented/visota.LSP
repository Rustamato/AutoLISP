;;;������� ���������� ������� ����� �� �������

(defun c:visota ( / num ch tekst tekstpoint)
  (initget 7)
  (setq num (fix (/ (getreal "\n������� �������� ��� ���������� ����� � ������: ") mashtab)))
  (setq ch (* 1 mashtab))
  (repeat num
;;    ���������� ������
    (setq tekst (rtos (+ horizon ch) 2 2))
;;;    ����� �������
    (setq tekstpoint (mapcar '+ startpoint (list 0 ch 0))
	  tekstpoint (mapcar '- (mapcar '+ startpoint (list 0 ch 0)) (list (* 1.35 mashtab) (* 0.125 mashtab) 0))
	  ch (+ ch mashtab));setq
    (vla-addtext model_space tekst (vlax-3D-point tekstpoint) (* 0.25 mashtab))
    );repeat
  (princ "\n\"Visota\" is OK")
  (princ)
  );defun
			