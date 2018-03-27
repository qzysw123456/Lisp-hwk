;state structure in search tree, contains g h function value and encode of puzzle
(defstruct searchstate node g h)

;utility functions for encode and decode
(defun Div (x y)
  (/ (- x (mod x y)) y))
(defun whereis (a x ret)
  (if(= (mod x 10) a)
     ret
     (whereis a (Div x 10) (+ ret 1))))
(defun itol (x l)
  (if (= x 0)
      l
      (itol (Div x 10) (cons (mod x 10) l))))
(defun ltoi (l x)
  (if (null l)
      x
      (ltoi (cdr l) (+ (* x 10) (car l)))))
(defun take (x l)
  (if(= x 0)
     (car l)
     (take (- x 1) (cdr l))))
(defun swap (a b l1 l2)
  (if(null l1)
     l2
     (if(= (car l1) a)
        (swap a b (cdr l1) (cons b l2))
        (if(= (car l1) b)
           (swap a b (cdr l1) (cons a l2))
           (swap a b (cdr l1) (cons (car l1) l2))))))
;manually input of up down left right operations
(setq operation nil)
(setq operation (cons (cons -1 0) operation))
(setq operation (cons (cons 1 0) operation))
(setq operation (cons (cons 0 1) operation))
(setq operation (cons (cons 0 -1) operation))

;initial state, no solution state and goal
(setq start 913425786)
(setq infi 123456879)
(setq goal 123456789)
(setq counter 0)
;expand
(defun nextstate (state action)
  (let((fst (car action)) (scd (cdr action)) (pos (whereis 9 state 0)))
    (let((px (Div pos 3)) (py (mod pos 3)))
      (let((nx (+ px fst)) (ny (+ py scd)))
        (if( and (> nx -1) (> ny -1) (< nx 3) (< ny 3))
           (let((np (+ (* nx 3) ny)))
             (let((vs (take (- 8 np) (itol state nil))))
               (ltoi (reverse (swap vs 9 (itol state nil) nil)) 0)
             )))))))
(defun toexpand (state)
  #'(lambda(x) (nextstate state x)))
(defun dis (a b)
  (if (= a b) 0 1))

;h function
(defun heuristic (state)
  (let((p1 (whereis 1 state 0))
       (p2 (whereis 2 state 0))
       (p3 (whereis 3 state 0))
       (p4 (whereis 4 state 0))
       (p5 (whereis 5 state 0))
       (p6 (whereis 6 state 0))
       (p7 (whereis 7 state 0))
       (p8 (whereis 8 state 0))
       (p9 (whereis 9 state 0)))
    (+ (dis p1 8) (dis p2 7) (dis p3 6)
       (dis p4 5) (dis p5 4) (dis p6 3)
       (dis p7 2) (dis p8 1) (dis p9 0))))

(setq initstate (make-searchstate :node start :g 0 :h (heuristic start)))

(defun next-search-state(path serstate close)
  (let((state (searchstate-node serstate)) (gstate (searchstate-g serstate)))
    (mapcar #'(lambda(x) (cons x path))
     (mapcar #'(lambda(x) (make-searchstate :node x :g (+ gstate 1) :h (heuristic x)))
        (remove-if #'(lambda(x) (member x close))
          (remove-if #'not (mapcar (toexpand state) operation)))))))
(defun fx (searstate)
  (let((g (searchstate-g searstate)) (h (searchstate-h searstate)))
    (+ g h)))
;pick the node with minimum f value
(defun pick-min-f-path (l ret)
  (if(null l)
     ret
     (if(< (fx (car ret)) (fx (car (car l))))
        (pick-min-f-path (cdr l) ret)
        (pick-min-f-path (cdr l) (car l)))))
(defun checkgoal (serstate)
  (= (searchstate-node serstate) goal))


(defun shortest-path(start)
  (astar (list (list start)) nil
       ))

(defun astar (queue close)
  (if (null queue)
      nil
      (let ((path (pick-min-f-path queue (car queue))))
        (let ((node (car path)))
          (if(checkgoal node)
             (reverse path)
             (progn
               (setq counter (+ counter 1))
               (if (member (searchstate-node node) close)
                   (astar (cdr queue) close)
                   (astar (append (cdr queue)
                                    (next-search-state path node (cons (searchstate-node node) close)))
                        (cons (searchstate-node node) close)))
               ))))))
;output
(shortest-path initstate)
