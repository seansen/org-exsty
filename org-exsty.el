;;; org-exsty.el --- Bypass that mental block when writing your papers. -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Sean Averhoff

;; Author: Sean Averhoff <seanathan@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.12.0") (s "1.12.0") (ht "2.0") (emacs "24"))
;; Keywords: org, export, html
;; Homepage: https://github.com/seansen/org-exsty

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; When writing your academic paper, you might get stuck trying to find the
;; Wallwork.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)


(if (boundp 'org-exsty-directory)
	(message "Variable defin")
	(setq org-exsty-directory (concat user-emacs-directory "straight/repos/org-exsty")))

(defvar org-exsty--all-phrases
      (ht
       (:cat1 (ht (:title "Choose #+Setupfile:")
                  (:items (list
                           (ht (:id 1)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styled-html/big_inline.theme"))                               (:choices '(())))
                           (ht (:id 2)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/comfy_inline.theme"))
                               (:choices '(())))
                           (ht (:id 3)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/darksun.theme"))
                               (:choices '(())))
                           (ht (:id 4)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/imagin_light.theme"))
                               (:choices '(())))
                           (ht (:id 5)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/readtheorg_inline.theme"))
                               (:choices '(())))
                           (ht (:id 6)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/rethink_inline.theme"))
                               (:choices '(())))
                           (ht (:id 7)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/retro_dark.theme"))
                               (:choices '(())))
                           (ht (:id 8)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/simple_gray.theme"))
                               (:choices '(())))
                           (ht (:id 9)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/simple_inline.theme"))
                               (:choices '(())))
                           (ht (:id 10)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/simple_white.theme"))
                               (:choices '(())))
                           (ht (:id 11)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/simple_whiteblue.theme"))
                               (:choices '(())))
                           (ht (:id 12)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/solarized_dark.theme"))
                               (:choices '(())))
                           (ht (:id 13)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/solarized_light.theme"))
                               (:choices '(())))
                           (ht (:id 14)
                               (:template (concat "#+Setupfile: "org-exsty-directory "/styles-html/stylish_white.theme"))
                               (:choices '(())))))))
       (:cat2 (ht (:title "Choose #+Startup:")
                   (:items (list
                           (ht (:id 22)
                               (:template "#+Startup: [{1}]")
                               (:choices '(("overview" "content" "showall" "showverything"))))))))
       (:cat3 (ht (:title "Choose #+Roam:")
                   (:items (list
                           (ht (:id 22)
                               (:template "[{1}]")
                               (:choices '(("#+ROAM_ALIAS:" "#+ROAM_Tags:"))))))))
       (:cat4 (ht (:title "Choose #+Pandoc-Options:")
                   (:items (list
                           (ht (:id 21)
                               (:template (concat "#+Pandoc-Options: reference-doc:"org-exsty-directory "/styles-doc/light_blue.docx"))
                               (:choices '(())))
                           (ht (:id 22)
                               (:template (concat "#+Pandoc_Options: reference-doc:"org-exsty-directory "/syles-doc/red_black.docx"))
                               (:choices '(())))))))))


;;; Set Org-Exsty directory
;; The next two functions (org-exsty--ht-get* and
;; org-exsty--ht-select-keys) are taken from the package ht.el
;; <https://github.com/Wilfred/ht.el> and are available for ht.el >= 2.2. I
;; included them here just to be self-contained and because spacemacs uses an
;; older version of ht.el (version 2.0)
(defun org-exsty--ht-get* (table &rest keys)
  "Starting with TABLE, Look up KEYS in nested hash tables.
The lookup for each key should return another hash table, except
for the final key, which may return any value."
  (if (cdr keys)
      (apply #'org-exsty--ht-get* (ht-get table (car keys)) (cdr keys))
    (ht-get table (car keys))))

(defun org-exsty--ht-select-keys (table keys)
  "Return a copy of TABLE with only the specified KEYS."
  (let (result)
    (setq result (make-hash-table :test (hash-table-test table)))
    (dolist (key keys result)
      (if (not (equal (gethash key table 'key-not-found) 'key-not-found))
          (puthash key (gethash key table) result)))))

(defun org-exsty--replace-placeholders (tmp choices)
  (s-replace-all `(("{1}" . ,(s-join "/" (car choices)))
                   ("{2}" . ,(s-join "/" (cadr choices)))
                   ("{3}" . ,(s-join "/" (cl-caddr choices))))
                 tmp))

(defun org-exsty--prompt-categories (phrases)
  (ht-map (lambda (k _) (org-exsty--ht-get* phrases k :title))
          phrases))

(defun org-exsty--prompt-items (cat &optional phrases)
  (unless phrases (setq phrases org-exsty--all-phrases))
  (mapcar (lambda (h)
           (cons (org-exsty--replace-placeholders
                   (ht-get h :template)
                   (ht-get h :choices))
            (ht-get h :id)))
         (org-exsty--get-items cat phrases)))

(defun org-exsty--filter-item (cat id &optional phrases)
  (unless phrases (setq phrases org-exsty--all-phrases))
  (let* ((items (org-exsty--get-items cat phrases))
         (match (car (-filter (lambda (i)
                                (equal (ht-get i :id) id))
                              items))))
       match))

(defun org-exsty--get-cat (res &optional phrases)
  (unless phrases (setq phrases org-exsty--all-phrases))
  (let ((item (ht-find (lambda (_ v)
                         (equal (ht-get v :title)
                                res))
                       phrases)))
    (car item)))

(defun org-exsty--get-items (cat &optional phrases)
  (unless phrases (setq phrases org-exsty--all-phrases))
  (org-exsty--ht-get* phrases cat :items))

(defun org-exsty--gen-cats-keywords (s e)
  (let* ((i (-iterate '1+ s (- e (1- s))))
         (cats (mapcar (lambda (c)
                         (concat ":cat" (number-to-string c))) i)))
    (mapcar 'intern-soft cats)))

(defun org-exsty--insert (phrases)
  (let* ((res (completing-read "Choose a category: "
                               (org-exsty--prompt-categories phrases) nil t))
         (cat (org-exsty--get-cat res phrases))
         (items (org-exsty--prompt-items cat))
         (id (cdr (assoc (completing-read "Choose a phrase: "
                                          items nil t)
                         items)))
         (item (org-exsty--filter-item cat id))
         (phrase-prompt (car (rassoc id items)))
         (template (ht-get item :template))
         (choice1 (when (s-contains? "{1}" template)
                        (completing-read phrase-prompt
                                         (car (ht-get item :choices))
                                         nil t)))
         (choice2 (when (s-contains? "{2}" template)
                        (completing-read phrase-prompt
                                         (cadr (ht-get item :choices))
                                         nil t)))
         (choice3 (when (s-contains? "{3}" template)
                        (completing-read phrase-prompt
                                     (caddr (ht-get item :choices))
                                     nil t)))
         (phrase (s-replace-all `(("[{1}]" . ,choice1)
                                  ("[{2}]" . ,choice2)
                                  ("[{3}]" . ,choice3))
                                 template)))
    (insert phrase)))

(defun org-exsty--insert-by-section (section &optional phrases)
  (unless phrases (setq phrases org-exsty--all-phrases))
  (let ((cats '()))
    (cond ((equal section :abstract) (setq cats '(:cat1 :cat2 :cat4 :cat5)))
          ((equal section :intro) (setq cats (org-exsty--gen-cats-keywords 1 16)))
          ((equal section :review) (setq cats (concatenate 'list '(:cat4) (org-exsty--gen-cats-keywords 9 16))))
          ((equal section :methods) (setq cats (org-exsty--gen-cats-keywords 17 30)))
          ((equal section :results) (setq cats (org-exsty--gen-cats-keywords 29 40)))
          ((equal section :discussion) (setq cats (org-exsty--gen-cats-keywords 35 45)))
          ((equal section :conclusion) (setq cats (org-exsty--gen-cats-keywords 45 51)))
          ((equal section :acknowledgments) (setq cats '(:cat52)))
          (t (setq cats (org-exsty--gen-cats-keywords 1 57))))
    (org-exsty--insert (org-exsty--ht-select-keys phrases cats))))


;;;###autoload
(defun org-exsty ()
  "Insert a phrase from a list of academic phrases by topic."
  (interactive)
  (org-exsty--insert org-exsty--all-phrases))

;;;###autoload
(defun org-exsty-by-section ()
  "Insert a phrase from a list of academic phrases by the paper section."
  (interactive)
  (let* ((sections '(("Abstract" . :abstract)
                     ("Introduction" . :intro)
                     ("Literature Review" . :review)
                     ("Methods" . :methods)
                     ("Results" . :results)
                     ("Discussion" . :discussion)
                     ("Conclusions" . :conclusion)
                     ("Acknowledgements" . :acknowledgments)))
         (res (completing-read "Choose a section: "
                               sections nil t))
         (sec (cdr (assoc res sections))))
   (org-exsty--insert-by-section sec)))

(provide 'org-exsty)
;;; org-exsty.el ends here
