;; showcase.el — A showcase of package digraphel 
;;
;; Author: Erwann Rogard
;; License: GPL 3.0 (https://www.gnu.org/licenses/gpl-3.0.en.html)

(load-file "../el/digraph.el")

;; Create a struct and store in variable DEMO-STRUCT
(defvar demo-struct
  (digraphel-new))
;; Populate DEMO-STRUCT with an adjacency list
(let ((adjacency-list '("V" nil "X" ("Y" "Z") "W" ("X" "Y"))))
   (digraphel-put
    :struct demo-struct
    :adjacency-list adjacency-list))
;; Convert the graph to a string
(message "DEMO-STRUCT as a string:\n%s\n"
	 (digraphel-graph-as-string
	  :struct demo-struct))
;; Extract vertices
(message "DEMO-STRUCT's vertices:\n %s\n"
	 (digraphel-vertices
	  :struct demo-struct))
;; Compute indegree
(message "DEMO-STRUCT's indegree:\n %s\n"
	 (digraphel-indegree
	  :struct demo-struct))
;; Compute levels
(message "DEMO-STRUCT's levels:\n %s\n"
	 (digraphel-levels
	  :struct demo-struct))
;; *Messages*
;;
;; DEMO-STRUCT as a string:
;; V
;; Y->X; Z->X
;; X->W; Y->W
;; 
;; DEMO-STRUCT’s vertices:
;;  (V X Y Z W)
;; 
;; DEMO-STRUCT’s indegree:
;;  #s(hash-table test equal data (V 0 X 1 Y 2 Z 1 W 0))
;; 
;; DEMO-STRUCT’s levels:
;;  #s(hash-table test equal data (V 0 X 1 Y 2 Z 2 W 0))
