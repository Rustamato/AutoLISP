;;;‘ункци€ построени€ линий поверхности

(defun c:horizont ( / lastobj v-listing ask)
  (if (/= (type model_space) "vla-object") (begin_activex))
  (while (/= ask "No")
;находим списки координат линий поверхности в формате vla-add3DPoly
(truba)
;создаем полилинию и сохран€ем им€ в lastobj дл€ присвоени€ значений свойств
 (setq lastobj (vla-add3dpoly model_space (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble (cons 1 (length v-listing))) v-listing)))
;ѕрисваиваем свойства к построенной коммуникации
(vla-put-layer lastobj "gnb_poverhnost")
(vla-put-color lastobj 252)
(initget "Yes No")
(setq ask (getkword "\nѕродолжить построение горизонталей [Yes/No] <продолжить>?: ")
      v-listing nil)
(if (not ask) (setq ask "Yes"))
  );while
(princ "\n\"Horizont\" is OK")
(princ)
);defun