;generate all of the possible operations
(setq operation nil)
(do
 ((i 0 (+ i 1)))
 ((> i 6) 'done)
  (do ((j 0 (+ j 1)))
      ((> j 6) 'done)
    (setq operation (cons (cons i j) operation))
  )
)
(setq operation (remove-if #'(lambda (x) (or (= (+ (car x) (cdr x)) 0) (> (+ (car x) (cdr x)) 6))) operation))
(setq operation (remove-if #'(lambda (x) (and (> (cdr x) 0) (> (car x) (cdr x)))) operation))

;state m c number in left right bank and state of boat(left or right, specified by 01 value)
(defstruct state cl ml cr mr boat)
(setq initstate (make-state :cl 24 :ml 24 :cr 0 :mr 0 :boat 0))
(setq goalstate (make-state :cl 0 :ml 0 :cr 24 :mr 24 :boat 1))

;utility functions
(defun myequ (s1 s2)
  (and (= (state-cl s1) (state-cl s2))
       (= (state-ml s1) (state-ml s2))
       (= (state-cr s1) (state-cr s2))
       (= (state-mr s1) (state-mr s2))
       (= (state-boat s1) (state-boat s2))))
(defun mymember (s l)
  (if(null l)
     nil
     (if (myequ s (car l))
         l
         (mymember s (cdr l)))))
;expand
(defun nextstate (now action)
  (let ( (cc (car action)) (mm (cdr action)) )
    (if (equal (state-boat now) 0)
      (make-state :cl (- (state-cl now) cc) :ml (- (state-ml now) mm) :cr (+ (state-cr now) cc) :mr (+ (state-mr now) mm) :boat (- 1 (state-boat now)))
      (make-state :cl (+ (state-cl now) cc) :ml (+ (state-ml now) mm) :cr (- (state-cr now) cc) :mr (- (state-mr now) mm) :boat (- 1 (state-boat now))))
    ))
(defun invalid (new)
  (or (> (state-cl new) (state-ml new) 0)
      (> (state-cr new) (state-mr new) 0)
      (< (state-cl new) 0)
      (< (state-ml new) 0)
      (< (state-cr new) 0)
      (< (state-mr new) 0)))

(defun toexpand (now)
  #'(lambda(x) (nextstate now x)))

(defun shortest-path(start end)
  (bfs end (list (list start)) nil
       ))

;i use bfs here
(defun bfs (end queue close)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if(myequ node end)
             (reverse path)
             (progn
               (if (mymember node close)
                   (bfs end (cdr queue) close)
                   (bfs end (append (cdr queue) (new-paths path node (cons node close))) (cons node close)))
                  ))))))

(defun new-paths(path node close)
  (mapcar #'(lambda (n) (cons n path))
  (remove-if #'(lambda (n) (mymember n close)) (remove-if #'invalid (mapcar (toexpand node) operation)))))

;output
(shortest-path initstate goalstate)

