;;;������� ���������� ������ ����� � �������

(defun c:glubina ( / pov skv obj linedown plist1 plist2 tekst tekstpoint a b)
  (while (not (and pov obj linedown)) 
	  (setq pov (vlax-ename->vla-object (car (entsel "\n������� ����� ����������� ����� �� �������: ")))
		skv (vlax-ename->vla-object (car (entsel "\n������� �����, ������� ������� ���� �������� �� �������: ")))
		linedown (vlax-ename->vla-object (car (entsel "\n������� ������� ����� ��� ���������� ������ � �������: ")))
		obj (entsel "\n������� ������������ ����� � �������: ")
		);setq
    (if (not (and pov obj linedowm)) (princ "\n�������� ����� �� ���������� ��� �������. ��������� ����.\n"));if
    );while
  (while obj
    (setq obj (vlax-ename->vla-object (car obj)))

;;;���������� ���������� ������
    ;;���������� �����������
    (setq plist1 (vla-IntersectWith pov obj acExtendOtherEntity)
    	  plist1 (vlax-variant-value plist1));setq
    (if (>= (vlax-safearray-get-u-bound plist1 1) (vlax-safearray-get-l-bound plist1 1)) (setq plist1 (vlax-safearray->list plist1)));if
    
    (setq plist2 (vla-IntersectWith skv obj acExtendOtherEntity)
    	  plist2 (vlax-variant-value plist2));setq
    (if (>= (vlax-safearray-get-u-bound plist2 1) (vlax-safearray-get-l-bound plist2 1)) (setq plist2 (vlax-safearray->list plist2)));if
    ;;������ �����
    (setq tekst (rtos (- (cadr plist1) (cadr plist2)) 2 2))

;;;���������� ����� ������� ������
    (setq tekstpoint (vla-IntersectWith linedown obj acExtendOtherEntity)
	  tekstpoint (vlax-safearray->list (vlax-variant-value tekstpoint))
	  a (- (car tekstpoint) (* mashtab 0.125))
	  b (+ (cadr tekstpoint) (* mashtab 0.05))
	  tekstpoint (list a b 0))
    (vla-rotate (vla-addtext model_space tekst (vlax-3D-point tekstpoint) (* 0.25 mashtab)) (vlax-3D-point tekstpoint) (/ pi 2))

    (setq obj (entsel "\n������� ��������� ������������ ����� � ������� <���������>: "))
    );while
  (princ "\n\"Glubina\" is OK")
  (princ)
  );defun
	  

    