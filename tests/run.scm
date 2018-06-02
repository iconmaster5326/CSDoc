(use csdoc)

; test basic documentation
(printf "TEST 1: ")
(document procedure (+ . numbers) "Adds numbers.")
(printf "~s\n" (docs-for +))
(assert (docs-for +))

; test that "@(...)" is getting expanded properly
(printf "TEST 2: ")
(document procedure (add . numbers) "A synonym for the proc @(ref add), which does addition.")
(let ((desc (cdr (assv 'desc (cdr (docs-for add))))))
  (printf "~s\n" desc)
  (assert (= (length desc) 3))
  (assert (string? (first desc)))
  (assert (equal? (second desc) '(ref add)))
  (assert (string? (third desc)))
)
