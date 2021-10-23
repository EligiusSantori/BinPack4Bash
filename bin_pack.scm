#!/usr/bin/guile
!#; Usage: `du -abd0 * | bin_pack.scm 23.7G`.

(use-modules
	(srfi srfi-1)
	(srfi srfi-11)
	(ice-9 format)
	(ice-9 textual-ports)
)

(define (parse-human-size s)
	(inexact->exact (truncate (or (cond
		((string-suffix? "G" s) (* (string->number (string-drop-right s 1)) (expt 2 30)))
		((string-suffix? "M" s) (* (string->number (string-drop-right s 1)) (expt 2 20)))
		((string-suffix? "K" s) (* (string->number (string-drop-right s 1)) (expt 2 10)))
		((string-suffix? "B" s) (string->number (string-drop-right s 1)))
		(else (string->number s))
	) (throw 'wrong-size s))))
)

(define (format-human-size n)
	(or (fold (lambda (p r)
		(or r (if (> n (car p))
			(let ((s (/ (round (/ (* n 100) (car p))) 100)))
				(format #f (if (= (truncate s) s) "~d~c" "~f~c") s (cdr p))
			)
			r
		))
	) #f (list
		(cons (expt 2 30) #\G)
		(cons (expt 2 20) #\M)
		(cons (expt 2 10) #\K)
	)) (format #f "~dB" n))
)

(define (parse-capacity a)
	(if (not (null? (cdr a)))
		(parse-human-size (cadr a))
		(throw 'capacity-missed)
	)
)

(define (parse-table p r)
	(let ((l (get-line p)))
		(if (not (eof-object? l))
			(let* ((t (string-split l (char-set #\space #\tab))) (s (parse-human-size (car t))))
				(parse-table p (cons (cons s (string-join (cdr t) " ")) r))
			)
			r
		)
	)
)

(define (summarize data)
	(fold (lambda (p r)
		(let-values (((s l) (car+cdr r) ))
			(cons (+ (car p) s) (cons (cdr p) l))
		)
	) (cons 0 (list)) data)
)

; Выбираем комбинацию с меньшим количеством элементов при равном размере.
(define (best-of s1 s2)
	(cond
		((> (car s1) (car s2)) s1)
		((< (car s1) (car s2)) s2)
		((< (length s1) (length s2)) s1)
		(else s2)
	)
)

; Рекурсивно, при помощи стека вызовов, перебираем все вариации. На лету отбрасываем ветви с превышением размера.
	; Если передавать (append aside l) в вертикальный вызов r, то будет происходить перебор сочитаний во всех порядках.
(define (algo-recursive data capacity)
	(define (r bag aside left capacity sum best)
		(cond
			((> (car sum) capacity) best) ; Capacity overflow.
			((or (= (car sum) capacity) (null? left)) (best-of best sum)) ; Stop propagnation if enough or last step.
			(else (let ((best (best-of best sum)) (p (car left)) (l (cdr left)))
				(let ((best (r (cons p bag) (list) l capacity (summarize (cons p bag)) best))) ; Go deep.
					(if (= (car best) capacity) best (r bag (cons p aside) l capacity sum best)) ; Go next.
				)
			))
		)
	)
	(r (list) (list) data capacity (list 0) (list 0))
)

(define (print-combination c)
	(let ((sc (sort (cdr c) string-ci<?)))
		(fold (lambda (s i) (format #t "~d. ~a\n" i s) (+ i 1)) 1 sc)
		(format #t "Total: ~a\n" (format-human-size (car c)))
	)
)

(let ((capacity (parse-capacity (command-line))) (data (parse-table (current-input-port) (list))))
	(print-combination (algo-recursive data capacity))
)
