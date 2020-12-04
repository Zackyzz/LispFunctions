;; reversing a list
;; practic, ia pe rand elementele din lista
;; (folosind car) si il face o lista (cons (car lista) nil)
;; pt a putea da append cu restul listei fara ultimul element

(defun reversing (lista)
	(if (null lista)
		nil
		(append (reversing (cdr lista)) (cons (car lista) nil))
	)
)

;; suma pana la n recursiv

(defun q (n)
		(if (= n 0)
			0
			(+ n (sum (- n 1)))
		)
	)
)

;; suma pana la n iterativ

(defun sumq (n)
(setq a 0)
	(if (< n 0)
		"Baga ceva pozitiv!"
		(loop for i from 0 to n  do
		(setq a (+ a i)))
	)
(return-from sumq a)
)

;; minimum recursiv

(defun minidad (lista)
	(if (null lista)
		(return-from minidad "E goala lista..")
		(if (not (integerp (car lista)))
		    (return-from minidad (/ 1 3))
    		(if (eql nil (cdr lista)) 
    			(car lista)
    			(if (< (car lista) (minidad (cdr lista)))
    				(car lista)
    				(minidad (cdr lista))
    			)
    		)
    	)
	)
)


(setq q (minidad '(2 3 4 "abcd" xaxa)))

(if (= q (/ 1 3))
    (print "Lista nu e formata doar din intregi..")
    (print q)
)


;; lungimea listei recursiv

(defun lenq (lista)
    (if (null lista)
        0
        (+ 1 (lenq (cdr lista)))
    )
)

;; iterativ

(defun lenq (lista)
    (setq i 0)
    (loop for k in lista do
        (setq i (+ 1 i)))
        (return-from lenq i)
)

;; suma cifrelor recursiv 


(defun sumqq (n)
    (cond 
    ((not (integerp n)) "Baga un nr intreg..")
    ((< n 0) "Baga ceva pozitiv")
    ((= n 0) 0)
        ((+ (mod n 10) (sumqq (truncate n 10))))
    )
)

;; iterativ

(defun sumqq (n)
    (setq i 0)
    (loop
        (if  (= n 0)
            (return-from sumqq i)
            (setq i(+ i (mod n 10)))
        )
        (setq n (truncate n 10))
    )
)

;;removeaza duplicatele..

(defun reversing (lista)
	(if (null lista)
		nil
		(union (cons (car lista) nil) (reversing (cdr lista)))
	)
)

;;palindrom

(defun palindrom (lista)
    (setq clona (reverse lista))
    (loop for i in lista do
        (if (not (= (nth i lista) (nth i clona)))
            (return-from palindrom "Nu e palindrom")
        )

    )
    (return-from palindrom "E palindrom")
)

(print (palindrom '(1 2 3 4 3 2 1)))


;; loto


(defun loto (n limita)
    (setq lista nil)
    (loop
        (if (= n 0)
            (return-from loto lista)
            (setq lista(append lista (cons (random limita) nil)))
        )
        (setq n (- n 1))
    )
)

(print (loto 7 49))

;;flatten
(defun flatten (lista)
    (loop
        (setq tester 1)
        (setq clona nil)
        (loop for i in lista do
            (if (listp i)
                (setq clona (append clona i))
                (setq clona (append clona (list i)))
            )
        )
        (setq lista clona)
        (loop for i in lista do
            (if (listp i)
                (setq tester 0)
                (setq tester 1)
            )
        )
        (if (= 1 tester)
             (return-from flatten lista)
        )
    )
)

(print (flatten '(1 (2 3) (3 (4 (5 ((((((((((((6))))))))))))))))))

;; union + intersect


(defun uni (lista1 lista2)
    (if (null lista1)
        lista2
        (if (eql nil (find (car lista1) lista2))
            (append (list (car lista1)) (uni (cdr lista1) lista2))
            (uni (cdr lista1) lista2)
        )
    )
)

(print (uni '(1 2 3 4) '(2 4 6 8)))

(defun inter (lista1 lista2)
    (if (null lista1)
        nil
        (if (not (eql nil (find (car lista1) lista2)))
            (append (list (car lista1)) (inter (cdr lista1) lista2))
            (inter (cdr lista1) lista2)
        )
    )
)

(print (inter '(1 2 3 4) '(2 4 6 8)))


;; palindrom

(defun fun1 (lista)
    (cond 
        ((null lista) "Lista e palindrom")
        ((not ( = (car lista) (car (reverse lista)))) "Lista nu e palindrom")
        ((fun1 (reverse (cdr (reverse (cdr lista))))))
    )
)

(print (fun1 '(1 2 5 6 3 4 2 1)))
(print (fun1 '(1 2 3 2 1)))


;;remove n

(defun removeqq (lista n)
    (setq i 0)
    (setq clona nil)
    (loop 
    (if (= i (length lista)) (return-from removeqq clona))
        (if (not (= (+ n 1) (nth i lista)))
            (setq clona (append clona (list (nth i lista))))
        )
        (setq i (+ 1 i))
    )
)

(print (removeqq '(1 2 3 4) 2))


;;reverse numberr

(defun number-of-digits (n)
    (setq ct 0)
    (loop
        (if (= n 0)
            (return-from number-of-digits (- ct 1))
        )
        (setq ct (+ 1 ct))
        (setq n (truncate n 10))
    )
)


(defun reverse-number (n)
    (if (< n 9)
        n
        (+  (* (expt 10 (number-of-digits n)) (mod n 10)) (reverse-number (truncate n 10)))
    )
)
