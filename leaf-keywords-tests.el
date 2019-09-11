;;; leaf-keywords-tests.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/leaf.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the Affero GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Affero
;; GNU General Public License for more details.

;; You should have received a copy of the Affero GNU General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(load "cort-test")
(require 'leaf-keywords)

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
  '(((leaf autorevert
       :diminish t)
     (prog1 'autorevert
       (eval-after-load 'diminish
         '(progn
            (diminish 'autorevert)))))

    ((leaf autorevert
       :diminish autorevert)
     (prog1 'autorevert
       (eval-after-load 'diminish
         '(progn
            (diminish 'autorevert)))))

    ((leaf autorevert
       :diminish t
       :diminish autorevert-polyfill)
     (prog1 'autorevert
       (eval-after-load 'diminish
         '(progn
            (diminish 'autorevert)
            (diminish 'autorevert-polyfill)))))

    ((leaf autorevert
       :diminish t autorevert-polyfill)
     (prog1 'autorevert
       (eval-after-load 'diminish
         '(progn
            (diminish 'autorevert)
            (diminish 'autorevert-polyfill)))))

    ((leaf go-mode
       :diminish " Go")
     (prog1 'go-mode
       (eval-after-load 'diminish
         '(progn
            (diminish 'go-mode " Go")))))

    ((leaf abbrev
       :diminish (abbrev-mode " Abv"))
     (prog1 'abbrev
       (eval-after-load 'diminish
         '(progn
            (diminish 'abbrev-mode " Abv")))))

    ((leaf projectile
       :diminish (projectile (:eval (concat " " (projectile-project-name)))))
     (prog1 'projectile
       (eval-after-load 'diminish
         '(progn
            (diminish 'projectile (:eval (concat " " (projectile-project-name))))))))))

(cort-deftest-with-macroexpand leaf/delight
  '(((leaf autorevert
       :delight t)
     (prog1 'autorevert
       (eval-after-load 'delight
         '(progn
            (delight 'autorevert)))))

    ((leaf autorevert
       :delight autorevert)
     (prog1 'autorevert
       (eval-after-load 'delight
         '(progn
            (delight 'autorevert)))))

    ((leaf autorevert
       :delight t
       :delight autorevert-polyfill)
     (prog1 'autorevert
       (eval-after-load 'delight
         '(progn
            (delight 'autorevert)
            (delight 'autorevert-polyfill)))))

    ((leaf autorevert
       :delight t autorevert-polyfill)
     (prog1 'autorevert
       (eval-after-load 'delight
         '(progn
            (delight 'autorevert)
            (delight 'autorevert-polyfill)))))

    ((leaf go-mode
       :delight " Go")
     (prog1 'go-mode
       (eval-after-load 'delight
         '(progn
            (delight 'go-mode " Go")))))

    ((leaf abbrev
       :delight (abbrev-mode " Abv"))
     (prog1 'abbrev
       (eval-after-load 'delight
         '(progn
            (delight 'abbrev-mode " Abv")))))

    ((leaf projectile
       :delight (projectile (:eval (concat " " (projectile-project-name)))))
     (prog1 'projectile
       (eval-after-load 'delight
         '(progn
            (delight 'projectile (:eval (concat " " (projectile-project-name))))))))

    ((leaf delight
       :delight ((abbrev-mode " Abv" "abbrev")
                 (smart-tab-mode " \\t" "smart-tab")
                 (eldoc-mode nil "eldoc")
                 (rainbow-mode)
                 (overwrite-mode " Ov" t)
                 (emacs-lisp-mode "Elisp" :major)))
     (prog1 'delight
       (eval-after-load 'delight
         '(progn
            (delight 'abbrev-mode " Abv" "abbrev")
            (delight 'smart-tab-mode " \\t" "smart-tab")
            (delight 'eldoc-mode nil "eldoc")
            (delight 'rainbow-mode)
            (delight 'overwrite-mode " Ov" t)
            (delight 'emacs-lisp-mode "Elisp" :major)))))))

(cort-deftest-with-macroexpand leaf/el-get
  '(((leaf leaf
       :init (leaf-pre-init)
       :el-get t
       :config (leaf-init))
     (prog1 'leaf
       (el-get-bundle leaf)
       (leaf-pre-init)
       (leaf-init)))

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
       (kazu-yamamoto/Mew :name mew :build ("./configure" "make"))
       :config (leaf-init))
     (prog1 'leaf
       (el-get-bundle yaicomplete :url "https://github.com/tarao/elisp.git" :features yaicomplete)
       (el-get-bundle zenburn-theme :url "https://raw.githubusercontent.com/bbatsov/zenburn-emacs/master/zenburn-theme.el"
         (load-theme 'zenburn t))
       (el-get-bundle kazu-yamamoto/Mew :name mew :build
         ("./configure" "make"))
       (leaf-pre-init)
       (leaf-init)))))

(cort-deftest-with-macroexpand leaf/straight
  '(((leaf leaf
       :init (leaf-pre-init)
       :straight t
       :config (leaf-init))
     (prog1 'leaf
       (straight-use-package 'leaf)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :straight leaf leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (straight-use-package 'leaf)
       (straight-use-package 'leaf-polyfill)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :straight t
       :straight leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (straight-use-package 'leaf)
       (straight-use-package 'leaf-polyfill)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :straight t leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (straight-use-package 'leaf)
       (straight-use-package 'leaf-polyfill)
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :straight (zenburn-theme :type git :host github :repo "fake/fake")
       :config (leaf-init))
     (prog1 'leaf
       (straight-use-package '(zenburn-theme :type git :host github :repo "fake/fake"))
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :straight
       (zenburn-theme :type git :host github :repo "fake/fake")
       (yaicomplete :type git :host github :repo "fake/faker")
       (mew :type git :host gitlab :repo "fake/fakest" :no-build)
       :config (leaf-init))
     (prog1 'leaf
       (straight-use-package '(zenburn-theme :type git :host github :repo "fake/fake"))
       (straight-use-package '(yaicomplete :type git :host github :repo "fake/faker"))
       (straight-use-package '(mew :type git :host gitlab :repo "fake/fakest" :no-build))
       (leaf-pre-init)
       (leaf-init)))))

(cort-deftest-with-macroexpand leaf/key-combo
  '(((leaf key-combo
       :combo (("="   . (" = " " == " " === " ))
               ("=>"  . " => ")
               ("C-a" . (back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
               ("C-e" . (move-end-of-line end-of-buffer key-combo-return))))
     (prog1 'key-combo
       (autoload #'back-to-indentation "key-combo" nil t)
       (autoload #'move-beginning-of-line "key-combo" nil t)
       (autoload #'beginning-of-buffer "key-combo" nil t)
       (autoload #'key-combo-return "key-combo" nil t)
       (autoload #'move-end-of-line "key-combo" nil t)
       (autoload #'end-of-buffer "key-combo" nil t)
       (key-combo-define global-map "=>" " => ")
       (key-combo-define global-map "C-a" '(back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
       (key-combo-define global-map "C-e" '(move-end-of-line end-of-buffer key-combo-return))))

    ((leaf key-combo
       :combo (emacs-lisp-mode-map
               ("="   . (" = " " == " " === " ))
               ("=>"  . " => ")
               ("C-a" . (back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
               ("C-e" . (move-end-of-line end-of-buffer key-combo-return))))
     (prog1 'key-combo
       (autoload #'back-to-indentation "key-combo" nil t)
       (autoload #'move-beginning-of-line "key-combo" nil t)
       (autoload #'beginning-of-buffer "key-combo" nil t)
       (autoload #'key-combo-return "key-combo" nil t)
       (autoload #'move-end-of-line "key-combo" nil t)
       (autoload #'end-of-buffer "key-combo" nil t)
       (key-combo-define emacs-lisp-mode-map "=" '(" = " " == " " === "))
       (key-combo-define emacs-lisp-mode-map "=>" " => ")
       (key-combo-define emacs-lisp-mode-map "C-a" '(back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
       (key-combo-define emacs-lisp-mode-map "C-e" '(move-end-of-line end-of-buffer key-combo-return))))

    ((leaf key-combo
       :combo ((("="   . (" = " " == " " === " ))
                ("=>"  . " => ")
                ("C-a" . (back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
                ("C-e" . (move-end-of-line end-of-buffer key-combo-return)))
               (emacs-lisp-mode-map
                ("."  . ("." " . "))
                ("="  . ("= " "eq " "equal ")))))
     (prog1 'key-combo
       (autoload #'back-to-indentation "key-combo" nil t)
       (autoload #'move-beginning-of-line "key-combo" nil t)
       (autoload #'beginning-of-buffer "key-combo" nil t)
       (autoload #'key-combo-return "key-combo" nil t)
       (autoload #'move-end-of-line "key-combo" nil t)
       (autoload #'end-of-buffer "key-combo" nil t)
       (key-combo-define global-map "=>" " => ")
       (key-combo-define global-map "C-a" '(back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
       (key-combo-define global-map "C-e" '(move-end-of-line end-of-buffer key-combo-return))
       (key-combo-define emacs-lisp-mode-map "." '("." " . "))
       (key-combo-define emacs-lisp-mode-map "=" '("= " "eq " "equal "))))))

(cort-deftest-with-macroexpand leaf/key-combo*
  '(((leaf key-combo
       :combo* (("="   . (" = " " == " " === " ))
                ("=>"  . " => ")
                ("C-a" . (back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
                ("C-e" . (move-end-of-line end-of-buffer key-combo-return))))
     (prog1 'key-combo
       (autoload #'back-to-indentation "key-combo" nil t)
       (autoload #'move-beginning-of-line "key-combo" nil t)
       (autoload #'beginning-of-buffer "key-combo" nil t)
       (autoload #'key-combo-return "key-combo" nil t)
       (autoload #'move-end-of-line "key-combo" nil t)
       (autoload #'end-of-buffer "key-combo" nil t)
       (key-combo-define leaf-key-override-global-map "=>" " => ")
       (key-combo-define leaf-key-override-global-map "C-a" '(back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
       (key-combo-define leaf-key-override-global-map "C-e" '(move-end-of-line end-of-buffer key-combo-return))))

    ((leaf key-combo
       :combo* (emacs-lisp-mode-map
                ("="   . (" = " " == " " === " ))
                ("=>"  . " => ")
                ("C-a" . (back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
                ("C-e" . (move-end-of-line end-of-buffer key-combo-return))))
     (prog1 'key-combo
       (autoload #'back-to-indentation "key-combo" nil t)
       (autoload #'move-beginning-of-line "key-combo" nil t)
       (autoload #'beginning-of-buffer "key-combo" nil t)
       (autoload #'key-combo-return "key-combo" nil t)
       (autoload #'move-end-of-line "key-combo" nil t)
       (autoload #'end-of-buffer "key-combo" nil t)
       (key-combo-define emacs-lisp-mode-map "=" '(" = " " == " " === "))
       (key-combo-define emacs-lisp-mode-map "=>" " => ")
       (key-combo-define emacs-lisp-mode-map "C-a" '(back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
       (key-combo-define emacs-lisp-mode-map "C-e" '(move-end-of-line end-of-buffer key-combo-return))))

    ((leaf key-combo
       :combo* ((("="   . (" = " " == " " === " ))
                 ("=>"  . " => ")
                 ("C-a" . (back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
                 ("C-e" . (move-end-of-line end-of-buffer key-combo-return)))
                (emacs-lisp-mode-map
                 ("."  . ("." " . "))
                 ("="  . ("= " "eq " "equal ")))))
     (prog1 'key-combo
       (autoload #'back-to-indentation "key-combo" nil t)
       (autoload #'move-beginning-of-line "key-combo" nil t)
       (autoload #'beginning-of-buffer "key-combo" nil t)
       (autoload #'key-combo-return "key-combo" nil t)
       (autoload #'move-end-of-line "key-combo" nil t)
       (autoload #'end-of-buffer "key-combo" nil t)
       (key-combo-define leaf-key-override-global-map "=>" " => ")
       (key-combo-define leaf-key-override-global-map "C-a" '(back-to-indentation move-beginning-of-line beginning-of-buffer key-combo-return))
       (key-combo-define leaf-key-override-global-map "C-e" '(move-end-of-line end-of-buffer key-combo-return))
       (key-combo-define emacs-lisp-mode-map "." '("." " . "))
       (key-combo-define emacs-lisp-mode-map "=" '("= " "eq " "equal "))))))

(cort-deftest-with-macroexpand leaf/chord
  '(((leaf macrostep
       :ensure t
       :chord (("jk" . macrostep-expand)))
     (prog1 'macrostep
       (autoload #'macrostep-expand "macrostep" nil t)
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chords
        (("jk" . macrostep-expand)))))

    ((leaf macrostep
       :ensure t
       :chord ("jk" . macrostep-expand))
     (prog1 'macrostep
       (autoload #'macrostep-expand "macrostep" nil t)
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chords
        (("jk" . macrostep-expand)))))

    ((leaf color-moccur
       :chord
       ("jk" . moccur)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chords
        (("jk" . moccur)
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord (("jk" . moccur)
               ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chords
        (("jk" . moccur)
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord
       ("jk" . nil)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chords
        (("jk")
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord (("jk" . nil)
               ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chords
        (("jk")
         ("fi" . isearch-moccur)))))

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
       (leaf-key-chords
        (("jk" . moccur)
         (:isearch-mode-map
          :package isearch
          ("ji" . isearch-moccur)
          ("jo" . isearch-moccur-all))))))

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
       (leaf-key-chords
        (("jk" . moccur)
         (:isearch-mode-map
          :package isearch
          ("ji" . isearch-moccur)
          ("jo" . isearch-moccur-all))))))

    ;; you also use symbol instead of keyword to specify keymap
    ((leaf color-moccur
       :chord (("jk" . moccur)
               (isearch-mode-map
                :package isearch
                ("ji" . isearch-moccur)
                ("jo" . isearch-moccur-all))))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-key-chords
        (("jk" . moccur)
         (isearch-mode-map
          :package isearch
          ("ji" . isearch-moccur)
          ("jo" . isearch-moccur-all))))))))

(cort-deftest-with-macroexpand leaf/chord*
  '(((leaf macrostep
       :ensure t
       :chord* (("jk" . macrostep-expand)))
     (prog1 'macrostep
       (autoload #'macrostep-expand "macrostep" nil t)
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chords*
        (("jk" . macrostep-expand)))))

    ((leaf macrostep
       :ensure t
       :chord* ("jk" . macrostep-expand))
     (prog1 'macrostep
       (autoload #'macrostep-expand "macrostep" nil t)
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chords*
        (("jk" . macrostep-expand)))))

    ((leaf color-moccur
       :chord*
       ("jk" . moccur)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chords*
        (("jk" . moccur)
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord* (("jk" . moccur)
                ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chords*
        (("jk" . moccur)
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord*
       ("jk" . nil)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chords*
        (("jk")
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord* (("jk" . nil)
                ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (autoload #'isearch-moccur "color-moccur" nil t)
       (leaf-key-chords*
        (("jk")
         ("fi" . isearch-moccur)))))

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
       (leaf-key-chords*
        (("jk" . moccur)
         (:isearch-mode-map
          :package isearch
          ("ji" . isearch-moccur)
          ("jo" . isearch-moccur-all))))))

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
       (leaf-key-chords*
        (("jk" . moccur)
         (:isearch-mode-map
          :package isearch
          ("ji" . isearch-moccur)
          ("jo" . isearch-moccur-all))))))))

(cort-deftest-with-macroexpand leaf/smartrep
  '(((leaf multiple-cursors
       :smartrep ("C-t"
                  (("C-p" . mc/mark-previous-like-this)
                   ("C-n" . mc/mark-next-like-this)
                   ("u"   . mc/unmark-next-like-this)
                   ("U"   . mc/unmark-previous-like-this)
                   ("s"   . mc/skip-to-next-like-this)
                   ("S"   . mc/skip-to-previous-like-this)
                   ("*"   . mc/mark-all-like-this))))
     (prog1 'multiple-cursors
       (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t)
       (autoload #'mc/mark-next-like-this "multiple-cursors" nil t)
       (autoload #'mc/unmark-next-like-this "multiple-cursors" nil t)
       (autoload #'mc/unmark-previous-like-this "multiple-cursors" nil t)
       (autoload #'mc/skip-to-next-like-this "multiple-cursors" nil t)
       (autoload #'mc/skip-to-previous-like-this "multiple-cursors" nil t)
       (autoload #'mc/mark-all-like-this "multiple-cursors" nil t)
       (smartrep-define-key global-map "C-t"
                            '(("C-p" . mc/mark-previous-like-this)
                              ("C-n" . mc/mark-next-like-this)
                              ("u" . mc/unmark-next-like-this)
                              ("U" . mc/unmark-previous-like-this)
                              ("s" . mc/skip-to-next-like-this)
                              ("S" . mc/skip-to-previous-like-this)
                              ("*" . mc/mark-all-like-this)))))

    ((leaf multiple-cursors
       :smartrep (global-map
                  "C-t"
                  (("C-p" . mc/mark-previous-like-this)
                   ("C-n" . mc/mark-next-like-this))))
     (prog1 'multiple-cursors
       (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t)
       (autoload #'mc/mark-next-like-this "multiple-cursors" nil t)
       (smartrep-define-key global-map "C-t"
                            '(("C-p" . mc/mark-previous-like-this)
                              ("C-n" . mc/mark-next-like-this)))))

    ((leaf multiple-cursors
       :smartrep (global-map
                  "C-t"
                  (("C-p" . 'mc/mark-previous-like-this)
                   ("C-n" . 'mc/mark-next-like-this))))
     (prog1 'multiple-cursors
       (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t)
       (autoload #'mc/mark-next-like-this "multiple-cursors" nil t)
       (smartrep-define-key global-map "C-t"
                            '(("C-p" quote mc/mark-previous-like-this)
                              ("C-n" quote mc/mark-next-like-this)))))

    ((leaf multiple-cursors
       :smartrep (global-map
                  "C-t"
                  '(("C-p" . 'mc/mark-previous-like-this)
                    ("C-n" . 'mc/mark-next-like-this))))
     (prog1 'multiple-cursors
       (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t)
       (autoload #'mc/mark-next-like-this "multiple-cursors" nil t)
       (smartrep-define-key global-map "C-t"
                            '(("C-p" quote mc/mark-previous-like-this)
                              ("C-n" quote mc/mark-next-like-this)))))

    ((leaf org
       :smartrep (org-mode-map
                  "C-c"
                  (("C-n" . (outline-next-visible-heading 1))
                   ("C-p" . (outline-previous-visible-heading 1)))))
     (prog1 'org
       (smartrep-define-key org-mode-map "C-c"
                            '(("C-n" outline-next-visible-heading 1)
                              ("C-p" outline-previous-visible-heading 1)))))

    ((leaf org
       :smartrep ((org-mode-map
                   "C-c"
                   (("C-n" . (outline-next-visible-heading 1))
                    ("C-p" . (outline-previous-visible-heading 1))))
                  ("s-c"
                   (("M-n" . (outline-next-visible-heading 1))
                    ("M-p" . (outline-previous-visible-heading 1))))))
     (prog1 'org
       (smartrep-define-key org-mode-map "C-c"
                            '(("C-n" outline-next-visible-heading 1)
                              ("C-p" outline-previous-visible-heading 1)))
       (smartrep-define-key global-map "s-c"
                            '(("M-n" outline-next-visible-heading 1)
                              ("M-p" outline-previous-visible-heading 1)))))))

(cort-deftest-with-macroexpand leaf/smartrep*
  '(((leaf multiple-cursors
       :smartrep* ("C-t"
                   (("C-p" . mc/mark-previous-like-this)
                    ("C-n" . mc/mark-next-like-this)
                    ("u"   . mc/unmark-next-like-this)
                    ("U"   . mc/unmark-previous-like-this)
                    ("s"   . mc/skip-to-next-like-this)
                    ("S"   . mc/skip-to-previous-like-this)
                    ("*"   . mc/mark-all-like-this))))
     (prog1 'multiple-cursors
       (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t)
       (autoload #'mc/mark-next-like-this "multiple-cursors" nil t)
       (autoload #'mc/unmark-next-like-this "multiple-cursors" nil t)
       (autoload #'mc/unmark-previous-like-this "multiple-cursors" nil t)
       (autoload #'mc/skip-to-next-like-this "multiple-cursors" nil t)
       (autoload #'mc/skip-to-previous-like-this "multiple-cursors" nil t)
       (autoload #'mc/mark-all-like-this "multiple-cursors" nil t)
       (smartrep-define-key leaf-key-override-global-map "C-t"
                            '(("C-p" . mc/mark-previous-like-this)
                              ("C-n" . mc/mark-next-like-this)
                              ("u" . mc/unmark-next-like-this)
                              ("U" . mc/unmark-previous-like-this)
                              ("s" . mc/skip-to-next-like-this)
                              ("S" . mc/skip-to-previous-like-this)
                              ("*" . mc/mark-all-like-this)))))

    ((leaf org
       :smartrep* ((org-mode-map
                    "C-c"
                    (("C-n" . (outline-next-visible-heading 1))
                     ("C-p" . (outline-previous-visible-heading 1))))
                   ("s-c"
                    (("M-n" . (outline-next-visible-heading 1))
                     ("M-p" . (outline-previous-visible-heading 1))))))
     (prog1 'org
       (smartrep-define-key org-mode-map "C-c"
                            '(("C-n" outline-next-visible-heading 1)
                              ("C-p" outline-previous-visible-heading 1)))
       (smartrep-define-key leaf-key-override-global-map "s-c"
                            '(("M-n" outline-next-visible-heading 1)
                              ("M-p" outline-previous-visible-heading 1)))))))

(cort-deftest-with-macroexpand leaf/hydra
  '(((leaf face-remap
       :hydra (hydra-zoom
               (global-map "<f2>")
               "zoom"
               ("g" text-scale-increase "in")
               ("l" text-scale-decrease "out")))
     (prog1 'face-remap
       (autoload #'text-scale-increase "face-remap" nil t)
       (autoload #'text-scale-decrease "face-remap" nil t)
       (defhydra hydra-zoom
         (global-map "<f2>")
         "zoom"
         ("g" text-scale-increase "in")
         ("l" text-scale-decrease "out"))))

    ((leaf yasnippet
       :bind (:yas-minor-mode-map
              ("<f3>" . hydra-yas-primary/body)
              ("<f2>" . hydra-yas/body))
       :hydra ((hydra-yas-primary
                (:hint nil)
                "yas-primary"
                ("i" yas-insert-snippet)
                ("n" yas-new-snippet)
                ("v" yas-visit-snippet-file))
               (hydra-yas
                (:color blue :hint nil)
                "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
                ("d" yas-load-directory)
                ("e" yas-activate-extra-mode)
                ("i" yas-insert-snippet)
                ("f" yas-visit-snippet-file :color blue)
                ("n" yas-new-snippet)
                ("t" yas-tryout-snippet)
                ("l" yas-describe-tables)
                ("g" yas/global-mode)
                ("m" yas/minor-mode)
                ("a" yas-reload-all))))
     (prog1 'yasnippet
       (autoload #'yas-insert-snippet "yasnippet" nil t)
       (autoload #'yas-new-snippet "yasnippet" nil t)
       (autoload #'yas-visit-snippet-file "yasnippet" nil t)
       (autoload #'yas-load-directory "yasnippet" nil t)
       (autoload #'yas-activate-extra-mode "yasnippet" nil t)
       (autoload #'yas-tryout-snippet "yasnippet" nil t)
       (autoload #'yas-describe-tables "yasnippet" nil t)
       (autoload #'yas/global-mode "yasnippet" nil t)
       (autoload #'yas/minor-mode "yasnippet" nil t)
       (autoload #'yas-reload-all "yasnippet" nil t)
       (autoload #'hydra-yas-primary/body "yasnippet" nil t)
       (autoload #'hydra-yas/body "yasnippet" nil t)
       (leaf-keys
        ((:yas-minor-mode-map :package yasnippet
                              ("<f3>" . hydra-yas-primary/body)
                              ("<f2>" . hydra-yas/body))))
       (defhydra hydra-yas-primary
         (:hint nil)
         "yas-primary"
         ("i" yas-insert-snippet)
         ("n" yas-new-snippet)
         ("v" yas-visit-snippet-file))
       (defhydra hydra-yas
         (:color blue :hint nil)
         "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
         ("d" yas-load-directory)
         ("e" yas-activate-extra-mode)
         ("i" yas-insert-snippet)
         ("f" yas-visit-snippet-file :color blue)
         ("n" yas-new-snippet)
         ("t" yas-tryout-snippet)
         ("l" yas-describe-tables)
         ("g" yas/global-mode)
         ("m" yas/minor-mode)
         ("a" yas-reload-all))))))

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
  '(((leaf-key-chords ("jk" . undo))
     (leaf-key-chord "jk" #'undo))

    ((leaf-key-chords (("jk" . undo)
                       ("fi" . find-file)))
     (progn
       (leaf-key-chord "jk" #'undo)
       (leaf-key-chord "fi" #'find-file)))

    ((leaf-key-chords (:c-mode-map ("jk" . undo)))
     (progn
       (leaf-key-chord "jk" #'undo 'c-mode-map)))

    ((leaf-key-chords (:c-mode-map (("jk" . undo)
                                    ("fi" . find-file))))
     (progn
       (leaf-key-chord "jk" #'undo 'c-mode-map)
       (leaf-key-chord "fi" #'find-file 'c-mode-map)))))

(cort-deftest-with-macroexpand leaf/leaf-key-chords*
  '(((leaf-key-chords* ("jk" . undo))
     (leaf-key-chords (:leaf-key-override-global-map
                       ("jk" . undo))))

    ((leaf-key-chords* (("jk" . undo)
                        ("fi" . find-file)))
     (leaf-key-chords (:leaf-key-override-global-map
                       ("jk" . undo)
                       ("fi" . find-file))))))

;; (provide 'leaf-keywords-tests)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-keywords-tests.el ends here
