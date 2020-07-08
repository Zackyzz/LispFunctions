(defun check (lista)
    (loop for element in lista do
        (if (listp element) (return-from check 0))
    )
    (return-from check 1)
)

(defun flatten (lista)
    (loop
        (setq clona nil)
        (loop for element in lista do
            (if (listp element)
                (setq clona (append clona element))
                (setq clona (append clona (list element)))
            )
        )
        (setq lista clona)
        (if (= 1 (check lista))
            (return-from flatten lista)
        )
    )
)

(print (flatten '(1 (2 3) (4 (5 (6 (((((((((((((7)))))))))))))))))))


(defun count-element (lista element)
    (setq contor 0)
    (loop for i in lista do
        (if (eq element i)
            (setq contor (+ 1 contor))
        )
    )
    (return-from count-element contor)
)

(defun encode (lista)
    (if (null lista)
        nil
        (cons (list (car lista) (count-element lista (car lista))) (encode (remove (car lista) lista)))
    )
)

(print (encode '(a q q q q q q q q q q t6 t6 a a a q q t6)))