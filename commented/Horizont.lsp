;;;������� ���������� ����� �����������

(defun c:horizont ( / lastobj v-listing ask)
  (if (/= (type model_space) "vla-object") (begin_activex))
  (while (/= ask "No")
;������� ������ ��������� ����� ����������� � ������� vla-add3DPoly
(truba)
;������� ��������� � ��������� ��� � lastobj ��� ���������� �������� �������
 (setq lastobj (vla-add3dpoly model_space (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble (cons 1 (length v-listing))) v-listing)))
;����������� �������� � ����������� ������������
(vla-put-layer lastobj "gnb_poverhnost")
(vla-put-color lastobj 252)
(initget "Yes No")
(setq ask (getkword "\n���������� ���������� ������������ [Yes/No] <����������>?: ")
      v-listing nil)
(if (not ask) (setq ask "Yes"))
  );while
(princ "\n\"Horizont\" is OK")
(princ)
);defun