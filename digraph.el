;; -*- lexical-binding: t; -*-
;; digraphel.el — A directed graph package in Emacs-Lisp
;;
;; Author: Erwann Rogard
;; License: GPL 3.0 (https://www.gnu.org/licenses/gpl-3.0.en.html)
;;
;; Terminology:
;; A DIGRAPH consists of VERTICES and ARROWS, each of the form (TAIL, HEAD), where
;; HEAD and TAIL are members of VERTICES. In symbolic terms, G = (V, A). The number
;; of ARROWS pointing to a vertex is called its INDEGREE. The LEVEL of a vertex is
;; the length of the longest sequence of ARROWS from any vertex with indegree 0
;; (a source) to that vertex.
;; Reference: https://en.wikipedia.org/wiki/Directed_graph

;; <backend>
;; G is represented as a hash-table, where KEY=TAIL and VALUE='(HEAD...). The
;; special case VALUE=nil corresponds to an isolated VERTEX.
;;
;; PROPAGATE is a plist of keyword arguments passed downstream.
(cl-defmacro digraphel-backend-adapt
    (&key
     adaptee-suffix
     target-infix
     (target-suffix adaptee-suffix)
     key-map
     &allow-other-keys
     &aux
     (__adaptee (intern (string-join (list "digraphel-backend" adaptee-suffix) "-")))
     (__target (intern
                (string-join
                 (delq nil (list "digraphel" target-infix target-suffix))
                 "-")))
     (__example `(digraphel-backend-adapt
		   :adaptee-suffix ,adaptee-suffix
		   :target-suffix ,target-suffix
		   :key-map ',key-map))
     (__docstring
      (format
       "Adapt %s to a target function using key-map %S.\n\nExample:\n  %S"
       __adaptee
       key-map
       __example)))
     (message "DEBUG: %S\n"
	      __adaptee
	      __target)
     `(cl-defun ,__target
	  (&rest rest
		 &allow-other-keys
		 &aux (__key-map ,key-map)
		 (__args (cl-loop for (k v) on rest by #'cddr
				  append (list (or (plist-get __key-map k) k) v))))
	,__docstring
	(message "DEBUG: %S %S" __key-map __args)
	(apply #',__adaptee __args)))
;;
(cl-defun digraphel-backend-vertex-as-string
    (&key vertex (string "%s") &allow-other-keys)
  "Convert VERTEX into a string using STRING as format.

  Example:
    (digraphel-backend-vertex-as-string
   :vertex \"X\")"
  (format string vertex))
;; 
(cl-defun digraphel-backend-pair-p (&key object &allow-other-keys)
  "Return t if OBJECT is a dotted pair (a cons whose cdr is not a proper list)."
  (and (consp object)
       (not (proper-list-p object))))
;;
(cl-defun digraphel-backend-arrow-as-string
    (&key pair head tail (string "%s->%s") propagate &allow-other-keys)
  "Return a string representing a ARROW relationship.

Alternatively:
- Read HEAD and TAILS from PAIR

  Example:
    (digraphel-backend-arrow-as-string
   :head \"X\"
   :tail \"Y\")"
  (cond
   (pair (if (digraphel-backend-pair-p :object pair)
	      (digraphel-backend-arrow-as-string :head (car pair) :tails (cdr pair) :propagate propagate)
	    (error "%s not an pair" pair)))
   (t
    (format string head tail))))
;;
(cl-defun digraphel-backend-arrows-as-string
    (&key tail heads (sep "; ") propagate &allow-other-keys)
  "Return a string representing arrows from TAIL to each HEAD in HEADS.
  Handles empty HEADS by printing the standalone vertex.

  Example:
  (digraphel-backend-arrows-as-string
   :tail \"X\"
   :heads '(\"Y\" \"Z\"))"
  (if (and heads (listp heads))
      ;; Normal case: arrows
      (string-join
       (cl-loop for head in heads
                collect (apply #'digraphel-backend-arrow-as-string
                               (append (list :tail tail :head head)
                                       propagate)))
       sep)
    ;; Degenerate case: single vertex
    (apply #'digraphel-backend-vertex-as-string
           (append (list :vertex tail)
                   propagate))))
;;
(cl-defun digraphel-backend-graph-as-string
    (&key hash-table (sep "\n") propagate &allow-other-keys)
  "Return a string representation of all arrows in HASH-TABLE.
  Each key in HASH-TABLE represents a TAIL whose value is a list of HEADS.

  Arrows for each TAIL are joined by SEP between tails.

  Example:
  (digraphel-backend-graph-as-string
   :hash-table digraphel-backend-demo)"
  (string-join
   (cl-loop for tail being the hash-keys of hash-table
            using (hash-values heads)
            collect (apply #'digraphel-backend-arrows-as-string
                           (append (list :tail tail :heads heads)
                                   propagate)))
   sep))
;;
(cl-defun digraphel-backend-arglist
    (&key
     hash-table
     (test (if hash-table (hash-table-test hash-table) 'equal))
     (size (when hash-table (hash-table-size hash-table)))
     (weakness (when hash-table (hash-table-weakness hash-table)))
     purecopy
     &allow-other-keys
     &aux
     (__keys '(:test :size :weakness :purecopy))
     (__values (list test size weakness purecopy)))
  "Return a hash-table keyword arglist suitable for `make-hash-table`. Unless explicitly provided,
  TEST SIZE and WEAKNESS are derived from HASH-TABLE."
  (cl-loop for k in __keys
           for v in __values
           when v append (list k v)))
;;
(cl-defun digraphel-backend-make
    (&key propagate &allow-other-keys
          &aux (__arglist (apply #'digraphel-backend-arglist propagate)))
  "Create a new hash table.

    Requirement:
    (apply #'digraphel-backend-arglist PROPAGATE) is valid.

  Example:
  (digraphel-backend-make
  :propagate '(:test eq))
    "
  (apply #'make-hash-table __arglist))
;;
(cl-defmacro digraphel-backend-demo (&rest body)
  "Bind a demo for tests."
  `(let ((ht (digraphel-backend-make))
	 (tail-1 "X") (heads-1 '("Y" "Z"))
	 (tail-2 "W") (heads-2 '("X" "Y")))
     (digraphel-backend-put
      :hash-table ht
      :adjacency-list (list tail-1 heads-1 tail-2 heads-2))
     ,@body))
;;
(cl-defun digraphel-backend-put
    (&key hash-table adjacency-list
  	  &allow-other-keys
          &aux (__ (unless (hash-table-p hash-table)
                     (error "❌%S is not a hash-table" hash-table))))
  "Put ADJACENCY-LIST into HASH-TABLE.

  Requirement:
  ADJACENCY-LIST A flat list where each tail is followed by a list of heads

  Example:
  (digraphel-backend-put
  :hash-table hash-table
  :adjacency-list  (\"X\" '(\"Y\" \"Z\") \"W\" '(\"X\" \"Y\")))"
  (cl-loop for (k v) on adjacency-list by #'cddr
           do (puthash k v hash-table))
  hash-table)
;;
(cl-defun digraphel-backend-vertices
    (&key
     hash-table
     &allow-other-keys
     &aux
     (__result
      (cl-loop for tail being the hash-keys of hash-table
  	       using (hash-values heads)
  	       append (cons tail heads))))
  "Extracts vertices (whether tail or heads) from HASH-TABLE"
  (delete-dups __result))
;;
(cl-defun digraphel-backend-indegree
    (&key
     hash-table
     (vertices (digraphel-backend-vertices :hash-table hash-table))
     &allow-other-keys
     &aux
     (__arglist (digraphel-backend-arglist hash-table))
     (__result (apply #'make-hash-table __arglist)))
  "Compute the indegree of each tail in HASH-TABLE.

  Keys:
  - HASH-TABLE represents an adjacency list: keys are vertices, values are lists of heads.
  - VERTICES contains all vertices (keys and heads)."
  (cl-loop for tail in vertices
           do (puthash tail 0 __result))
  (cl-loop for tail being the hash-keys of hash-table
           using (hash-values neighbor-list)
           do (cl-loop for neighbor in neighbor-list
                       do (puthash neighbor (1+ (gethash neighbor __result)) __result)))
  __result)
;;
(cl-defun digraphel-backend-levels
    (&key hash-table
  	  &allow-other-keys
          &aux
          (__arglist (digraphel-backend-arglist :hash-table hash-table))
          (__result (apply #'make-hash-table __arglist)))
  "Return a hash-table mapping each node to its topological level in HASH-TABLE.
  Nodes with lower levels appear earlier in any valid topological order."
  (let* ((indegree (digraphel-backend-indegree :hash-table hash-table))
         (queue nil)
         (levels (make-hash-table :test 'equal))
         tail heads new-indeg)

    ;; initialize queue and levels
    (maphash (lambda (k v)
               (puthash k 0 levels)
               (when (zerop v)
                 (push k queue)))
             indegree)

    ;; process queue
    (while queue
      (setq tail (pop queue))
      (setq heads (or (gethash tail hash-table) nil))
      (dolist (n heads)
        ;; update level of neighbor
        (puthash n (max (gethash n levels 0)
                        (1+ (gethash tail levels 0)))
                 levels)
        ;; decrement indegree
        (setq new-indeg (1- (gethash n indegree 0)))
        (puthash n new-indeg indegree)
        (when (zerop new-indeg)
          (push n queue))))

    ;; detect cycles (properly)
    (let ((vertices (digraphel-backend-vertices :hash-table hash-table)))
      (when (/= (hash-table-count levels) (length vertices))
        (error "❌Graph contains a cycle")))

    levels))
;;
(cl-defmacro digraphel-backend-frontend
    (&key string &allow-other-keys
          &aux
          (__backend (intern (concat "digraphel-backend-" string)))
          (__frontend (intern (concat "digraphel-" string)))
          (__docstring (format "Forward to `%s' using the struct's hash-table." __backend)))
  "Define a frontend DIGRAPHEL-<STRING> forwarding to DIGRAPHEL-BACKEND-<STRING>.

    The generated function accepts keyword arguments, including `:struct'.
    It extracts the hash-table from STRUCT and forwards all other arguments to the backend."
  `(cl-defun ,__frontend (&rest rest
				&aux (struct (plist-get rest :struct))
				(__ (cl-check-type struct digraphel-struct))
				(__ht (digraphel-struct-hash-table struct))
				(__args (cl-loop for (k v) on rest by #'cddr
						 unless (eq k :struct)
						 append (list k v))))
     ,__docstring
     (apply #',__backend
            (append (list :hash-table __ht) __args))))
;; </backend>

;; <frontend>
;; The frontend consists of a struct that wraps a hash table,
;; along with functions defined on that struct which delegate to the backend.
;;
(cl-defstruct digraphel-struct
  "The graph structure"
  hash-table)
;;
(cl-defun digraphel-new
    (&rest rest
           &aux
           (__arglist (apply #'digraphel-backend-arglist rest))
           (__ht (apply #'make-hash-table __arglist))
           (__struct (make-digraphel-struct)))
  "Create a DIGRAPHEL-STRUCT with a hash table initialized from REST arguments.

    Requirement:
    - (apply #'digraphel-backend-arglist REST) must return valid args for `make-hash-table`.

    Example:
      (digraphel-new :test 'eq)"
  ;; initialize struct with the new hash-table
  (setf (digraphel-struct-hash-table __struct) __ht)
  __struct)
(digraphel-backend-frontend :string "graph-as-string")
(digraphel-backend-frontend :string "put")
(digraphel-backend-frontend :string "vertices")
(digraphel-backend-frontend :string "indegree")
(digraphel-backend-frontend :string "levels")
;; </frontend>
