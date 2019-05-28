;;; leaf-keywords-tests.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/leaf.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'leaf-keywords)
(require 'cort-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test settings
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support legacy Emacs
;;

(when (not (fboundp 'autoload-do-load))
  (defun autoload-do-load (fundef &optional funname macro-only)
    (if (or (not (consp fundef)) (not (eql 'autoload (car fundef))))
        fundef
      (let ((kind (nth 4 fundef)))
        (if (and (eql macro-only 'macro)
                 (not (or (eql kind t)
                          (eql kind 'macro))))
            fundef)
        (if purify-flag
            (error "Attempt to autoload %s while preparing to dump" (symbol-name funnname)))
        (unwind-protect
            (let ((ignore-errors (if (or (eql kind t) (eql kind 'macro)) nil macro_only)))
              (load (cadr fundef) ignore-errors t nil t))
          ;; FIXME: revert partially performed defuns
          ())
        (if (or (not funname) ignore-errors)
            nil
          (let ((fun (indirect-function funname, nil)))
            (if (equal fun fundef)
                (error "Autoloading file %s failed to define function %s"
                       (caar load-history)
                       (symbol-name funname))
              fun)))))))

(when (not (fboundp 'macroexpand-1))
  (defun macroexpand-1 (form &optional environment)
    "Perform (at most) one step of macroexpansion."
    (cond
     ((consp form)
      (let* ((head (car form))
             (env-expander (assq head environment)))
        (if env-expander
            (if (cdr env-expander)
                (apply (cdr env-expander) (cdr form))
              form)
          (if (not (and (symbolp head) (fboundp head)))
              form
            (let ((def (autoload-do-load (symbol-function head) head 'macro)))
              (cond
               ;; Follow alias, but only for macros, otherwise we may end up
               ;; skipping an important compiler-macro (e.g. cl--block-wrapper).
               ((and (symbolp def) (macrop def)) (cons def (cdr form)))
               ((not (consp def)) form)
               (t
                (if (eq 'macro (car def))
                    (apply (cdr def) (cdr form))
                  form))))))))
     (t form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support macros for test definition
;;

(defmacro cort-deftest-with-macroexpand (name form)
  "Return `cort-deftest' compare by `equal' for NAME, FORM.

Example
  (p (cort-deftest-with-equal leaf/disabled
       '((asdf asdf)
         (uiop uiop))))
   => (cort-deftest leaf/disabled
        '((:equal asdf asdf)
          (:equal uiop uiop)))
"
  (declare (indent 1))
  `(cort-deftest ,name
     ',(mapcar (lambda (elm)
                 `(:equal
                   ',(cadr elm)
                   (macroexpand-1 ',(car elm))))
               (cadr form))))

(defmacro match-expansion-let (letform form expect)
  (declare (indent 1))
  `(:equal (let ,letform (macroexpand-1 ',form)) ,expect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test definition
;;

(custom-set-variables '(leaf-expand-leaf-protect nil))
(leaf-keywords-init)

(cort-deftest-with-macroexpand leaf/diminish
  '(((leaf leaf
       :init (leaf-pre-init)
       :diminish t
       :config (leaf-init))
     (prog1 'leaf
       (diminish leaf)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :diminish nil
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :diminish leaf leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (diminish leaf)
       (diminish leaf-polyfill)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :diminish t
       :diminish leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (diminish leaf)
       (diminish leaf-polyfill)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :diminish t leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (diminish leaf)
       (diminish leaf-polyfill)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :diminish (leaf leaf-polyfill leaf-sub leaf-subsub)
       :config (leaf-init))
     (prog1 'leaf
       (diminish leaf)
       (diminish leaf-polyfill)
       (diminish leaf-sub)
       (diminish leaf-subsub)
       (leaf-pre-init)
       (leaf-init)))))

(cort-deftest-with-macroexpand leaf/el-get
  '(((leaf leaf
       :init (leaf-pre-init)
       :el-get t
       :config (leaf-init))
     (prog1 'leaf
       (el-get-bundle leaf)
       (leaf-pre-init)
       (leaf-init)))

    ;; ((leaf leaf
    ;;    :init (leaf-pre-init)
    ;;    :el-get nil
    ;;    :config (leaf-init))
    ;;  (prog1 'leaf
    ;;    (leaf-pre-init)
    ;;    (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :el-get leaf leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (el-get-bundle leaf)
       (el-get-bundle leaf-polyfill)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :el-get t
       :el-get leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (el-get-bundle leaf)
       (el-get-bundle leaf-polyfill)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :el-get t leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (el-get-bundle leaf)
       (el-get-bundle leaf-polyfill)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :el-get (zenburn-theme
                :url "https://raw.githubusercontent.com/bbatsov/zenburn-emacs/master/zenburn-theme.el"
                (load-theme 'zenburn t))
       :config (leaf-init))
     (prog1 'leaf
       (el-get-bundle zenburn-theme :url "https://raw.githubusercontent.com/bbatsov/zenburn-emacs/master/zenburn-theme.el"
         (load-theme 'zenburn t))
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :el-get
       (yaicomplete
        :url "https://github.com/tarao/elisp.git"
        :features yaicomplete)
       (zenburn-theme
        :url "https://raw.githubusercontent.com/bbatsov/zenburn-emacs/master/zenburn-theme.el"
        (load-theme 'zenburn t))
       :config (leaf-init))
     (prog1 'leaf
       (el-get-bundle yaicomplete :url "https://github.com/tarao/elisp.git" :features yaicomplete)
       (el-get-bundle zenburn-theme :url "https://raw.githubusercontent.com/bbatsov/zenburn-emacs/master/zenburn-theme.el"
         (load-theme 'zenburn t))
       (leaf-pre-init)
       (leaf-init)))))

(cort-deftest-with-macroexpand leaf/chord
  '(((leaf macrostep
       :ensure t
       :chord (("jk" . macrostep-expand)))
     (prog1 'macrostep
       (autoload #'macrostep-expand "macrostep" nil t)
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chord "jk" #'macrostep-expand nil)))

    ((leaf macrostep
       :ensure t
       :chord ("jk" . macrostep-expand))
     (prog1 'macrostep
       (autoload #'macrostep-expand "macrostep" nil t)
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chord "jk" #'macrostep-expand nil)))

    ((leaf color-moccur
       :chord
       ("jk" . moccur)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chord "jk" #'moccur nil)
       (leaf-key-chord "fi" #'isearch-moccur nil)))

    ((leaf color-moccur
       :chord (("jk" . moccur)
               ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chord "jk" #'moccur nil)
       (leaf-key-chord "fi" #'isearch-moccur nil)))

    ((leaf color-moccur
       :chord
       ("jk" . nil)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chord "jk" #'nil nil)
       (leaf-key-chord "fi" #'isearch-moccur nil)))

    ((leaf color-moccur
       :chord (("jk" . nil)
               ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chord "jk" #'nil nil)
       (leaf-key-chord "fi" #'isearch-moccur nil)))

    ((leaf color-moccur
       :chord
       ("jk" . moccur)
       (:isearch-mode-map
        :package isearch
        ("ji" . isearch-moccur)
        ("jo" . isearch-moccur-all)))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-key-chord "jk" #'moccur nil)
       (leaf-key-chord "ji" #'isearch-moccur isearch-mode-map)
       (leaf-key-chord "jo" #'isearch-moccur-all isearch-mode-map)))

    ((leaf color-moccur
       :chord (("jk" . moccur)
               (:isearch-mode-map
                :package isearch
                ("ji" . isearch-moccur)
                ("jo" . isearch-moccur-all))))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-key-chord "jk" #'moccur nil)
       (leaf-key-chord "ji" #'isearch-moccur isearch-mode-map)
       (leaf-key-chord "jo" #'isearch-moccur-all isearch-mode-map)))))

(cort-deftest-with-macroexpand leaf/chord*
  '(((leaf macrostep
       :ensure t
       :chord* (("jk" . macrostep-expand)))
     (prog1 'macrostep
       (autoload #'macrostep-expand "macrostep" nil t)
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chord* "jk" #'macrostep-expand nil)))

    ((leaf macrostep
       :ensure t
       :chord* ("jk" . macrostep-expand))
     (prog1 'macrostep
       (autoload #'macrostep-expand "macrostep" nil t)
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chord* "jk" #'macrostep-expand nil)))

    ((leaf color-moccur
       :chord*
       ("jk" . moccur)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chord* "jk" #'moccur nil)
       (leaf-key-chord* "fi" #'isearch-moccur nil)))

    ((leaf color-moccur
       :chord* (("jk" . moccur)
               ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chord* "jk" #'moccur nil)
       (leaf-key-chord* "fi" #'isearch-moccur nil)))

    ((leaf color-moccur
       :chord*
       ("jk" . nil)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chord* "jk" #'nil nil)
       (leaf-key-chord* "fi" #'isearch-moccur nil)))

    ((leaf color-moccur
       :chord* (("jk" . nil)
               ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chord* "jk" #'nil nil)
       (leaf-key-chord* "fi" #'isearch-moccur nil)))

    ((leaf color-moccur
       :chord*
       ("jk" . moccur)
       (:isearch-mode-map
        :package isearch
        ("ji" . isearch-moccur)
        ("jo" . isearch-moccur-all)))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-key-chord* "jk" #'moccur nil)
       (leaf-key-chord* "ji" #'isearch-moccur isearch-mode-map)
       (leaf-key-chord* "jo" #'isearch-moccur-all isearch-mode-map)))

    ((leaf color-moccur
       :chord* (("jk" . moccur)
               (:isearch-mode-map
                :package isearch
                ("ji" . isearch-moccur)
                ("jo" . isearch-moccur-all))))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-key-chord* "jk" #'moccur nil)
       (leaf-key-chord* "ji" #'isearch-moccur isearch-mode-map)
       (leaf-key-chord* "jo" #'isearch-moccur-all isearch-mode-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support leaf macros
;;

(cort-deftest-with-macroexpand leaf/leaf-key-chord
  '(((leaf-key-chord "jj" 'undo 'c-mode-map)
     (leaf-key [key-chord 106 106] 'undo 'c-mode-map))

    ((leaf-key-chord "jk" 'undo 'c-mode-map)
     (progn
       (leaf-key [key-chord 106 107] 'undo 'c-mode-map)
       (leaf-key [key-chord 107 106] 'undo 'c-mode-map)))

    ((leaf-key-chord "jj" 'undo)
     (leaf-key [key-chord 106 106] 'undo nil))

    ((leaf-key-chord "jk" 'undo)
     (progn
       (leaf-key [key-chord 106 107] 'undo nil)
       (leaf-key [key-chord 107 106] 'undo nil)))))

(cort-deftest-with-macroexpand leaf/leaf-key-chord*
  '(((leaf-key-chord* "jj" 'undo)
     (leaf-key-chord "jj" 'undo 'leaf-key-override-global-map))

    ((leaf-key-chord* "jk" 'undo)
     (leaf-key-chord "jk" 'undo 'leaf-key-override-global-map))))

(cort-deftest-with-macroexpand leaf/leaf-key-chords
  '(((leaf-key-chords :bind ("jk" . undo))
     (progn
       (leaf-key-chord "jk" 'undo 'nil)))

    ((leaf-key-chords :bind (("jk" . undo)
                             ("fi" . find-file)))
     (progn
       (leaf-key-chord "jk" 'undo 'nil)
       (leaf-key-chord "fi" 'find-file 'nil)))

    ((leaf-key-chords :map c-mode-map :bind ("jk" . undo))
     (progn
       (leaf-key-chord "jk" 'undo 'c-mode-map)))

    ((leaf-key-chords :map c-mode-map :bind (("jk" . undo)
                                             ("fi" . find-file)))
     (progn
       (leaf-key-chord "jk" 'undo 'c-mode-map)
       (leaf-key-chord "fi" 'find-file 'c-mode-map)))))

(cort-deftest-with-macroexpand leaf/leaf-key-chords*
  '(((leaf-key-chords* :bind ("jk" . undo))
     (leaf-key-chords :map 'leaf-key-override-global-map
                      :bind ("jk" . undo)))

    ((leaf-key-chords* :bind (("jk" . undo)
                             ("fi" . find-file)))
     (leaf-key-chords :map 'leaf-key-override-global-map
                      :bind (("jk" . undo)
                             ("fi" . find-file))))))

(provide 'leaf-keywords-tests)
;;; leaf-keywords-tests.el ends here

