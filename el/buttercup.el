;; -*- lexical-binding: t; -*-
;; buttercup.el — An buttercup suite for package digraphel 
;;
;; Author: Erwann Rogard
;; License: GPL 3.0 (https://www.gnu.org/licenses/gpl-3.0.en.html)

(load-file "./digraph.el")

(use-package buttercup
  :ensure t)

;;
(describe "digraphel-backend-arglist"
  (it ":test 'eq"
    (let ((arglist (digraphel-backend-arglist
                     :test 'eq)))
      (expect (plist-get arglist :test)
              :to-equal 'eq))))
;;
(describe "digraphel-backend-vertices"
  (digraphel-backend-demo
   (let ((vertices
	  (digraphel-backend-vertices
	   :hash-table ht))
	 (bench '("W" "X" "Y" "Z")))
     (it (digraphel-backend-graph-as-string :hash-table ht)
       (expect (sort vertices) :to-equal (sort bench))))))
;;
(describe "digraphel-backend-indegree"
  (digraphel-backend-demo
   (let ((in-deg (digraphel-backend-indegree
		  :hash-table ht)))
     (it "W has indegree 0"
       (expect (gethash "W" in-deg) :to-equal 0))
     (it "X has indegree 1"
       (expect (gethash "X" in-deg) :to-equal 1))
     (it "Y has indegree 2"
       (expect (gethash "Y" in-deg) :to-equal 2))
     (it "Z has indegree 1"
       (expect (gethash "Z" in-deg) :to-equal 1)))))
;;
(describe "digraphel-backend-levels"
  (digraphel-backend-demo
   (let ((levels (digraphel-backend-levels
		  :hash-table ht)))
     (it "level for \"W\" is 0"
       (expect (gethash "W" levels) :to-equal 0))
     (it "level for \"X\" is 1"
       (expect (gethash "X" levels) :to-equal 1))
     (it "level for \"Y\" is 2"
       (expect (gethash "Y" levels) :to-equal 2))
     (it "level for \"Z\" is 2"
       (expect (gethash "Z" levels) :to-equal 2))
     )))
