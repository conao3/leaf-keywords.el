;;; leaf-keywords-tests.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/leaf.el

;; This program is free software: you can redistribute it and/or modify
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
          (:equal uiop uiop)))"
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
       (with-eval-after-load 'autorevert
         (diminish 'autorevert-mode nil))))

    ((leaf autorevert
       :diminish autorevert-mode)
     (prog1 'autorevert
       (with-eval-after-load 'autorevert
         (diminish 'autorevert-mode nil))))

    ((leaf autorevert
       :diminish t
       :diminish autorevert-polyfill)
     (prog1 'autorevert
       (with-eval-after-load 'autorevert
         (diminish 'autorevert-mode nil)
         (diminish 'autorevert-polyfill-mode nil))))

    ((leaf autorevert
       :diminish t autorevert-polyfill)
     (prog1 'autorevert
       (with-eval-after-load 'autorevert
         (diminish 'autorevert-mode nil)
         (diminish 'autorevert-polyfill-mode nil))))

    ((leaf go-mode
       :diminish " Go")
     (prog1 'go-mode
       (with-eval-after-load 'go-mode
         (diminish 'go-mode " Go"))))

    ((leaf abbrev
       :diminish (abbrev-mode . " Abv"))
     (prog1 'abbrev
       (with-eval-after-load 'abbrev
         (diminish 'abbrev-mode " Abv"))))

    ((leaf projectile
       :diminish (projectile-mode . '(:eval (concat " " (projectile-project-name)))))
     (prog1 'projectile
       (with-eval-after-load 'projectile
         (diminish 'projectile-mode
                   '(:eval (concat " " (projectile-project-name)))))))))

(cort-deftest-with-macroexpand leaf/delight
  '(((leaf autorevert
       :delight t)
     (prog1 'autorevert
       (delight 'autorevert-mode)))

    ((leaf autorevert
       :delight autorevert)
     (prog1 'autorevert
       (delight 'autorevert-mode)))

    ((leaf autorevert
       :delight t
       :delight autorevert-polyfill)
     (prog1 'autorevert
       (delight 'autorevert-mode)
       (delight 'autorevert-polyfill-mode)))

    ((leaf autorevert
       :delight t autorevert-polyfill)
     (prog1 'autorevert
       (delight 'autorevert-mode)
       (delight 'autorevert-polyfill-mode)))

    ((leaf go-mode
       :delight " Go")
     (prog1 'go-mode
       (delight 'go-mode " Go")))

    ((leaf abbrev
       :delight (abbrev-mode " Abv"))
     (prog1 'abbrev
       (delight 'abbrev-mode " Abv")))

    ((leaf projectile
       :delight (projectile-mode '(:eval (concat " " (projectile-project-name)))))
     (prog1 'projectile
       (delight 'projectile-mode
                '(:eval
                  (concat " "
                          (projectile-project-name))))))

    ((leaf delight
       :delight ((abbrev-mode " Abv" "abbrev")
                 (smart-tab-mode " \\t" "smart-tab")
                 (eldoc-mode nil "eldoc")
                 (rainbow-mode)
                 (overwrite-mode " Ov" t)
                 (emacs-lisp-mode "Elisp" :major)))
     (prog1 'delight
       (delight 'abbrev-mode " Abv" "abbrev")
       (delight 'smart-tab-mode " \\t" "smart-tab")
       (delight 'eldoc-mode nil "eldoc")
       (delight 'rainbow-mode)
       (delight 'overwrite-mode " Ov" t)
       (delight 'emacs-lisp-mode "Elisp" :major)))))

(cort-deftest-with-macroexpand leaf/blackout
  '(
    ;; t will be converted leaf--name
    ((leaf foo-mode
       :blackout t)
     (prog1 'foo-mode
       (with-eval-after-load 'foo-mode
         (blackout 'foo-mode nil))))

    ;; guess leaf--name as mode-name
    ((leaf foo
       :blackout t)
     (prog1 'foo
       (with-eval-after-load 'foo
         (blackout 'foo-mode nil))))

    ;; blackout if specify symbol only
    ((leaf simple
       :blackout auto-fill-mode)
     (prog1 'simple
       (with-eval-after-load 'simple
         (blackout 'auto-fill-mode nil))))

    ;; expect cons-cell to change display of a mode
    ((leaf simple
       :blackout (auto-fill-mode . " Auto-Fill"))
     (prog1 'simple
       (with-eval-after-load 'simple
         (blackout 'auto-fill-mode " Auto-Fill"))))

    ;; change major-mode display by same way
    ((leaf elisp-mode
       :blackout (emacs-lisp-mode . "Elisp"))
     (prog1 'elisp-mode
       (with-eval-after-load 'elisp-mode
         (blackout 'emacs-lisp-mode "Elisp"))))

    ;; cons-cell list also accepted
    ((leaf simple
       :blackout ((auto-fill-mode . " Auto-Fill")
                  (overwrite-mode . " Overwrite")))
     (prog1 'simple
       (with-eval-after-load 'simple
         (blackout 'auto-fill-mode " Auto-Fill")
         (blackout 'overwrite-mode " Overwrite"))))

    ;; multi cons-cell also accepted
    ((leaf simple
       :blackout
       (auto-fill-mode . " Auto-Fill")
       (overwrite-mode . " Overwrite"))
     (prog1 'simple
       (with-eval-after-load 'simple
         (blackout 'auto-fill-mode " Auto-Fill")
         (blackout 'overwrite-mode " Overwrite"))))

    ;; multi keyword also accepted
    ((leaf simple
       :blackout (auto-fill-mode . " Auto-Fill")
       :blackout (overwrite-mode . " Overwrite"))
     (prog1 'simple
       (with-eval-after-load 'simple
         (blackout 'auto-fill-mode " Auto-Fill")
         (blackout 'overwrite-mode " Overwrite"))))))

(cort-deftest-with-macroexpand leaf/feather
  '(
    ;; 't will be converted leaf--name
    ((leaf leaf
       :init (leaf-pre-init)
       :feather t
       :config (leaf-init))
     (prog1 'leaf
       (leaf-handler-package leaf leaf nil)
       (feather-add-after-installed-hook-sexp leaf
         (leaf-pre-init)
         (leaf-init))))

    ;; multi symbols will be accepted
    ((leaf leaf
       :init (leaf-pre-init)
       :feather leaf leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (leaf-handler-package leaf leaf nil)
       (leaf-handler-package leaf leaf-polyfill nil)
       (feather-add-after-installed-hook-sexp leaf-polyfill
         (leaf-pre-init)
         (leaf-init))))

    ;; multi symbols in list will be accepted
    ((leaf leaf
       :feather (feather leaf-key leaf-browser)
       :config (leaf-init))
     (prog1 'leaf
       (leaf-handler-package leaf feather nil)
       (leaf-handler-package leaf leaf-key nil)
       (leaf-handler-package leaf leaf-browser nil)
       (feather-add-after-installed-hook-sexp leaf-browser
         (leaf-init))))

    ;; multi keyword will be accepted
    ((leaf leaf
       :init (leaf-pre-init)
       :feather t
       :feather leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (leaf-handler-package leaf leaf nil)
       (leaf-handler-package leaf leaf-polyfill nil)
       (feather-add-after-installed-hook-sexp leaf-polyfill
         (leaf-pre-init)
         (leaf-init))))

    ;; keywords such as :preface that expand before :feather
    ;; are not registered in the hook of feather
    ((leaf leaf
       :preface (leaf-preface)
       :init (leaf-pre-init)
       :feather t
       :config (leaf-init))
     (prog1 'leaf
       (leaf-preface)
       (leaf-handler-package leaf leaf nil)
       (feather-add-after-installed-hook-sexp leaf
         (leaf-pre-init)
         (leaf-init))))))

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
       :el-get nil leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
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

(cort-deftest-with-macroexpand leaf/defaults
  '(((leaf helm
       :ensure t
       :defaults t)
     (prog1 'helm
       (leaf-handler-package helm helm nil)
       (leaf-defaults--helm)))

    ((leaf helm
       :when nil
       :ensure t
       :defaults t)
     (prog1 'helm
       (when nil
         (leaf-handler-package helm helm nil)
         (leaf-defaults--helm))))

    ((leaf helm
       :ensure t
       :defaults conao3)
     (prog1 'helm
       (leaf-handler-package helm helm nil)
       (leaf-defaults--helm--conao3)))

    ((leaf helm
       :ensure t
       :defaults conao3 garario3)
     (prog1 'helm
       (leaf-handler-package helm helm nil)
       (leaf-defaults--helm--conao3)
       (leaf-defaults--helm--garario3)))

    ((leaf helm
       :ensure t
       :defaults conao3
       :defaults garario3)
     (prog1 'helm
       (leaf-handler-package helm helm nil)
       (leaf-defaults--helm--conao3)
       (leaf-defaults--helm--garario3)))

    ((leaf helm
       :ensure t
       :defaults nil
       :defaults conao3
       :defaults garario3)
     (prog1 'helm
       (leaf-handler-package helm helm nil)))))

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
       :straight nil t
       :config (leaf-init))
     (prog1 'leaf
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
       (unless (fboundp 'back-to-indentation) (autoload #'back-to-indentation "key-combo" nil t))
       (unless (fboundp 'move-beginning-of-line) (autoload #'move-beginning-of-line "key-combo" nil t))
       (unless (fboundp 'beginning-of-buffer) (autoload #'beginning-of-buffer "key-combo" nil t))
       (unless (fboundp 'key-combo-return) (autoload #'key-combo-return "key-combo" nil t))
       (unless (fboundp 'move-end-of-line) (autoload #'move-end-of-line "key-combo" nil t))
       (unless (fboundp 'end-of-buffer) (autoload #'end-of-buffer "key-combo" nil t))
       (declare-function back-to-indentation "key-combo")
       (declare-function move-beginning-of-line "key-combo")
       (declare-function beginning-of-buffer "key-combo")
       (declare-function key-combo-return "key-combo")
       (declare-function move-end-of-line "key-combo")
       (declare-function end-of-buffer "key-combo")
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
       (unless (fboundp 'back-to-indentation) (autoload #'back-to-indentation "key-combo" nil t))
       (unless (fboundp 'move-beginning-of-line) (autoload #'move-beginning-of-line "key-combo" nil t))
       (unless (fboundp 'beginning-of-buffer) (autoload #'beginning-of-buffer "key-combo" nil t))
       (unless (fboundp 'key-combo-return) (autoload #'key-combo-return "key-combo" nil t))
       (unless (fboundp 'move-end-of-line) (autoload #'move-end-of-line "key-combo" nil t))
       (unless (fboundp 'end-of-buffer) (autoload #'end-of-buffer "key-combo" nil t))
       (declare-function back-to-indentation "key-combo")
       (declare-function move-beginning-of-line "key-combo")
       (declare-function beginning-of-buffer "key-combo")
       (declare-function key-combo-return "key-combo")
       (declare-function move-end-of-line "key-combo")
       (declare-function end-of-buffer "key-combo")
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
       (unless (fboundp 'back-to-indentation) (autoload #'back-to-indentation "key-combo" nil t))
       (unless (fboundp 'move-beginning-of-line) (autoload #'move-beginning-of-line "key-combo" nil t))
       (unless (fboundp 'beginning-of-buffer) (autoload #'beginning-of-buffer "key-combo" nil t))
       (unless (fboundp 'key-combo-return) (autoload #'key-combo-return "key-combo" nil t))
       (unless (fboundp 'move-end-of-line) (autoload #'move-end-of-line "key-combo" nil t))
       (unless (fboundp 'end-of-buffer) (autoload #'end-of-buffer "key-combo" nil t))
       (declare-function back-to-indentation "key-combo")
       (declare-function move-beginning-of-line "key-combo")
       (declare-function beginning-of-buffer "key-combo")
       (declare-function key-combo-return "key-combo")
       (declare-function move-end-of-line "key-combo")
       (declare-function end-of-buffer "key-combo")
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
       (unless (fboundp 'back-to-indentation) (autoload #'back-to-indentation "key-combo" nil t))
       (unless (fboundp 'move-beginning-of-line) (autoload #'move-beginning-of-line "key-combo" nil t))
       (unless (fboundp 'beginning-of-buffer) (autoload #'beginning-of-buffer "key-combo" nil t))
       (unless (fboundp 'key-combo-return) (autoload #'key-combo-return "key-combo" nil t))
       (unless (fboundp 'move-end-of-line) (autoload #'move-end-of-line "key-combo" nil t))
       (unless (fboundp 'end-of-buffer) (autoload #'end-of-buffer "key-combo" nil t))
       (declare-function back-to-indentation "key-combo")
       (declare-function move-beginning-of-line "key-combo")
       (declare-function beginning-of-buffer "key-combo")
       (declare-function key-combo-return "key-combo")
       (declare-function move-end-of-line "key-combo")
       (declare-function end-of-buffer "key-combo")
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
       (unless (fboundp 'back-to-indentation) (autoload #'back-to-indentation "key-combo" nil t))
       (unless (fboundp 'move-beginning-of-line) (autoload #'move-beginning-of-line "key-combo" nil t))
       (unless (fboundp 'beginning-of-buffer) (autoload #'beginning-of-buffer "key-combo" nil t))
       (unless (fboundp 'key-combo-return) (autoload #'key-combo-return "key-combo" nil t))
       (unless (fboundp 'move-end-of-line) (autoload #'move-end-of-line "key-combo" nil t))
       (unless (fboundp 'end-of-buffer) (autoload #'end-of-buffer "key-combo" nil t))
       (declare-function back-to-indentation "key-combo")
       (declare-function move-beginning-of-line "key-combo")
       (declare-function beginning-of-buffer "key-combo")
       (declare-function key-combo-return "key-combo")
       (declare-function move-end-of-line "key-combo")
       (declare-function end-of-buffer "key-combo")
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
       (unless (fboundp 'back-to-indentation) (autoload #'back-to-indentation "key-combo" nil t))
       (unless (fboundp 'move-beginning-of-line) (autoload #'move-beginning-of-line "key-combo" nil t))
       (unless (fboundp 'beginning-of-buffer) (autoload #'beginning-of-buffer "key-combo" nil t))
       (unless (fboundp 'key-combo-return) (autoload #'key-combo-return "key-combo" nil t))
       (unless (fboundp 'move-end-of-line) (autoload #'move-end-of-line "key-combo" nil t))
       (unless (fboundp 'end-of-buffer) (autoload #'end-of-buffer "key-combo" nil t))
       (declare-function back-to-indentation "key-combo")
       (declare-function move-beginning-of-line "key-combo")
       (declare-function beginning-of-buffer "key-combo")
       (declare-function key-combo-return "key-combo")
       (declare-function move-end-of-line "key-combo")
       (declare-function end-of-buffer "key-combo")
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
       (unless (fboundp 'macrostep-expand) (autoload #'macrostep-expand "macrostep" nil t))
       (declare-function macrostep-expand "macrostep")
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chords
        (("jk" . macrostep-expand)))))

    ((leaf macrostep
       :ensure t
       :chord ("jk" . macrostep-expand))
     (prog1 'macrostep
       (unless (fboundp 'macrostep-expand) (autoload #'macrostep-expand "macrostep" nil t))
       (declare-function macrostep-expand "macrostep")
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chords
        (("jk" . macrostep-expand)))))

    ((leaf color-moccur
       :chord
       ("jk" . moccur)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (declare-function moccur "color-moccur")
       (declare-function isearch-moccur "color-moccur")
       (leaf-key-chords
        (("jk" . moccur)
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord (("jk" . moccur)
               ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (declare-function moccur "color-moccur")
       (declare-function isearch-moccur "color-moccur")
       (leaf-key-chords
        (("jk" . moccur)
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord
       ("jk" . nil)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (declare-function isearch-moccur "color-moccur")
       (leaf-key-chords
        (("jk")
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord (("jk" . nil)
               ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (declare-function isearch-moccur "color-moccur")
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
       (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
       (declare-function moccur "color-moccur")
       (declare-function isearch-moccur "color-moccur")
       (declare-function isearch-moccur-all "color-moccur")
       (leaf-key-chords
        (("jk" . moccur)
         (:isearch-mode-map :package isearch
                            ("ji" . isearch-moccur)
                            ("jo" . isearch-moccur-all))))))

    ((leaf color-moccur
       :chord (("jk" . moccur)
               (:isearch-mode-map
                :package isearch
                ("ji" . isearch-moccur)
                ("jo" . isearch-moccur-all))))
     (prog1 'color-moccur
       (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
       (declare-function moccur "color-moccur")
       (declare-function isearch-moccur "color-moccur")
       (declare-function isearch-moccur-all "color-moccur")
       (leaf-key-chords
        (("jk" . moccur)
         (:isearch-mode-map :package isearch
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
       (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
       (declare-function moccur "color-moccur")
       (declare-function isearch-moccur "color-moccur")
       (declare-function isearch-moccur-all "color-moccur")
       (leaf-key-chords
        (("jk" . moccur)
         (isearch-mode-map :package isearch
                           ("ji" . isearch-moccur)
                           ("jo" . isearch-moccur-all))))))))

(cort-deftest-with-macroexpand leaf/chord*
  '(((leaf macrostep
       :ensure t
       :chord* (("jk" . macrostep-expand)))
     (prog1 'macrostep
       (unless (fboundp 'macrostep-expand) (autoload #'macrostep-expand "macrostep" nil t))
       (declare-function macrostep-expand "macrostep")
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chords*
        (("jk" . macrostep-expand)))))

    ((leaf macrostep
       :ensure t
       :chord* ("jk" . macrostep-expand))
     (prog1 'macrostep
       (unless (fboundp 'macrostep-expand) (autoload #'macrostep-expand "macrostep" nil t))
       (declare-function macrostep-expand "macrostep")
       (leaf-handler-package macrostep macrostep nil)
       (leaf-key-chords*
        (("jk" . macrostep-expand)))))

    ((leaf color-moccur
       :chord*
       ("jk" . moccur)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (declare-function moccur "color-moccur")
       (declare-function isearch-moccur "color-moccur")
       (leaf-key-chords*
        (("jk" . moccur)
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord* (("jk" . moccur)
                ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (declare-function moccur "color-moccur")
       (declare-function isearch-moccur "color-moccur")
       (leaf-key-chords*
        (("jk" . moccur)
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord*
       ("jk" . nil)
       ("fi" . isearch-moccur))
     (prog1 'color-moccur
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (declare-function isearch-moccur "color-moccur")
       (leaf-key-chords*
        (("jk")
         ("fi" . isearch-moccur)))))

    ((leaf color-moccur
       :chord* (("jk" . nil)
                ("fi" . isearch-moccur)))
     (prog1 'color-moccur
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (declare-function isearch-moccur "color-moccur")
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
       (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
       (declare-function moccur "color-moccur")
       (declare-function isearch-moccur "color-moccur")
       (declare-function isearch-moccur-all "color-moccur")
       (leaf-key-chords*
        (("jk" . moccur)
         (:isearch-mode-map :package isearch
                            ("ji" . isearch-moccur)
                            ("jo" . isearch-moccur-all))))))

    ((leaf color-moccur
       :chord* (("jk" . moccur)
                (:isearch-mode-map
                 :package isearch
                 ("ji" . isearch-moccur)
                 ("jo" . isearch-moccur-all))))
     (prog1 'color-moccur
       (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
       (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
       (declare-function moccur "color-moccur")
       (declare-function isearch-moccur "color-moccur")
       (declare-function isearch-moccur-all "color-moccur")
       (leaf-key-chords*
        (("jk" . moccur)
         (:isearch-mode-map :package isearch
                            ("ji" . isearch-moccur)
                            ("jo" . isearch-moccur-all))))))))

(cort-deftest-with-macroexpand leaf/grugru
  '(
    ;; grugru difinition with :grugru keyword
    ((leaf cc-mode
       :grugru
       (c-mode
        (symbol "true" "false")))
     (prog1 'cc-mode
       (grugru-define-multiple
        (c-mode (symbol "true" "false")))))

    ;; definition list also accepted
    ((leaf cc-mode
       :grugru
       ((c-mode
         (symbol "true" "false"))))
     (prog1 'cc-mode
       (grugru-define-multiple
        (c-mode (symbol "true" "false")))))

    ;; grugru definition with major-mode list
    ((leaf cc-mode
       :grugru
       ((c-mode c++-mode)
        (symbol "true" "false")))
     (prog1 'cc-mode
       (grugru-define-multiple
        ((c-mode c++-mode)
         (symbol "true" "false")))))

    ;; definition list with major-mode list
    ((leaf cc-mode
       :grugru
       (((c-mode c++-mode)
         (symbol "true" "false"))))
     (prog1 'cc-mode
       (grugru-define-multiple
        ((c-mode c++-mode) (symbol "true" "false")))))

    ;; simple listed definition are inferred to be for leaf--name
    ((leaf lisp-mode
       :grugru
       (symbol "nil" "t")
       (emacs-lisp-mode
        (word "add" "remove")))
     (prog1 'lisp-mode
       (grugru-define-multiple
        (lisp-mode (symbol "nil" "t"))
        (emacs-lisp-mode (word "add" "remove")))))

    ;; simple listed definition list are inferred to be for leaf--name
    ((leaf lisp-mode
       :grugru
       ((symbol "nil" "t")
        (emacs-lisp-mode
         (word "add" "remove"))))
     (prog1 'lisp-mode
       (grugru-define-multiple
        (lisp-mode (symbol "nil" "t"))
        (emacs-lisp-mode (word "add" "remove")))))

    ;; assume major-mode name from leaf--name
    ((leaf gnuplot
       :grugru
       ((symbol "sin" "cos" "tan")
        (symbol "log" "log10")))
     (prog1 'gnuplot
       (grugru-define-multiple
        (gnuplot-mode
         (symbol "sin" "cos" "tan"))
        (gnuplot-mode
         (symbol "log" "log10")))))

    ;; shuffle variation
    ((leaf lisp-mode
       :grugru
       (emacs-lisp-mode
        (word "add" "remove"))
       (symbol "nil" "t"))
     (prog1 'lisp-mode
       (grugru-define-multiple
        (emacs-lisp-mode (word "add" "remove"))
        (lisp-mode (symbol "nil" "t")))))

    ;; shuffle variation
    ((leaf lisp-mode
       :grugru
       ((emacs-lisp-mode
         (word "add" "remove"))
        (symbol "nil" "t")))
     (prog1 'lisp-mode
       (grugru-define-multiple
        (emacs-lisp-mode (word "add" "remove"))
        (lisp-mode (symbol "nil" "t")))))))

(cort-deftest-with-macroexpand leaf/mode-hook
  '((;; you can place sexp(s) like :config
     (leaf cc-mode
       :mode-hook
       (electric-pair-mode 1)
       (delete-selection-mode 1))
     (prog1 'cc-mode
       (leaf-keywords-handler-mode-hook cc-mode cc-mode-hook
         (electric-pair-mode 1)
         (delete-selection-mode 1))))

    (;; you can configure multiple mode hooks
     (leaf cc-mode
       :config
       (setq-default c-basic-offset 8)
       :mode-hook
       (c-mode-common-hook . ((setq-local tab-width 8)))
       (java-mode-hook . ((setq-local tab-width 4)
                          (setq-local c-basic-offset 4))))
     (prog1 'cc-mode
       (leaf-keywords-handler-mode-hook cc-mode c-mode-common-hook
         (setq-local tab-width 8))
       (leaf-keywords-handler-mode-hook cc-mode java-mode-hook
         (setq-local tab-width 4)
         (setq-local c-basic-offset 4))
       (setq-default c-basic-offset 8)))

    (;; you can apply same sexp to multiple mode hooks
     (leaf cc-mode
       :config
       (setq-default c-basic-offset 8)
       :mode-hook
       (c-mode-common-hook emacs-lisp-mode-hook lisp-mode-hook . ((setq-local tab-width 8)))
       (java-mode-hook . ((setq-local tab-width 4)
                          (setq-local c-basic-offset 4))))
     (prog1 'cc-mode
       (leaf-keywords-handler-mode-hook cc-mode c-mode-common-hook
         (setq-local tab-width 8))
       (leaf-keywords-handler-mode-hook cc-mode emacs-lisp-mode-hook
         (setq-local tab-width 8))
       (leaf-keywords-handler-mode-hook cc-mode lisp-mode-hook
         (setq-local tab-width 8))
       (leaf-keywords-handler-mode-hook cc-mode java-mode-hook
         (setq-local tab-width 4)
         (setq-local c-basic-offset 4))
       (setq-default c-basic-offset 8)))

    (;; you can mix abobe two specification method
     (leaf cc-mode
       :config
       (setq-default c-basic-offset 8)
       :mode-hook
       (setq-local tab-width 8)
       (java-mode-hook . ((setq-local tab-width 4)
                          (setq-local c-basic-offset 4))))
     (prog1 'cc-mode
       (leaf-keywords-handler-mode-hook cc-mode cc-mode-hook
         (setq-local tab-width 8))
       (leaf-keywords-handler-mode-hook cc-mode java-mode-hook
         (setq-local tab-width 4)
         (setq-local c-basic-offset 4))
       (setq-default c-basic-offset 8)))

    (;; multiple keyword specification is supported
     (leaf cc-mode
       :config
       (setq-default c-basic-offset 8)
       :mode-hook
       (setq-local tab-width 8)
       (c-mode-common-hook . ((setq-local tab-width 8)))
       :mode-hook
       (java-mode-hook . ((setq-local tab-width 4)
                          (setq-local c-basic-offset 4))))
     (prog1 'cc-mode
       (leaf-keywords-handler-mode-hook cc-mode cc-mode-hook
         (setq-local tab-width 8))
       (leaf-keywords-handler-mode-hook cc-mode c-mode-common-hook
         (setq-local tab-width 8))
       (leaf-keywords-handler-mode-hook cc-mode java-mode-hook
         (setq-local tab-width 4)
         (setq-local c-basic-offset 4))
       (setq-default c-basic-offset 8)))

    (;; leaf-keywords-handler-mode-hook expand like below
     (leaf-keywords-handler-mode-hook cc-mode cc-mode-hook
       (electric-pair-mode 1)
       (delete-selection-mode 1))
     (progn
       (defun leaf-keywords-mode-hook--cc-mode--cc-mode-hook ()
         "Function autogenerated by leaf-keywords in leaf-block `cc-mode' for hook `cc-mode-hook'."
         (electric-pair-mode 1)
         (delete-selection-mode 1))
       (add-hook 'cc-mode-hook 'leaf-keywords-mode-hook--cc-mode--cc-mode-hook)))))

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
       (unless (fboundp 'mc/mark-previous-like-this) (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/mark-next-like-this) (autoload #'mc/mark-next-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/unmark-next-like-this) (autoload #'mc/unmark-next-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/unmark-previous-like-this) (autoload #'mc/unmark-previous-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/skip-to-next-like-this) (autoload #'mc/skip-to-next-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/skip-to-previous-like-this) (autoload #'mc/skip-to-previous-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/mark-all-like-this) (autoload #'mc/mark-all-like-this "multiple-cursors" nil t))
       (declare-function mc/mark-previous-like-this "multiple-cursors")
       (declare-function mc/mark-next-like-this "multiple-cursors")
       (declare-function mc/unmark-next-like-this "multiple-cursors")
       (declare-function mc/unmark-previous-like-this "multiple-cursors")
       (declare-function mc/skip-to-next-like-this "multiple-cursors")
       (declare-function mc/skip-to-previous-like-this "multiple-cursors")
       (declare-function mc/mark-all-like-this "multiple-cursors")
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
       (unless (fboundp 'mc/mark-previous-like-this) (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/mark-next-like-this) (autoload #'mc/mark-next-like-this "multiple-cursors" nil t))
       (declare-function mc/mark-previous-like-this "multiple-cursors")
       (declare-function mc/mark-next-like-this "multiple-cursors")
       (smartrep-define-key global-map "C-t"
                            '(("C-p" . mc/mark-previous-like-this)
                              ("C-n" . mc/mark-next-like-this)))))

    ((leaf multiple-cursors
       :smartrep (global-map
                  "C-t"
                  (("C-p" . 'mc/mark-previous-like-this)
                   ("C-n" . 'mc/mark-next-like-this))))
     (prog1 'multiple-cursors
       (unless (fboundp 'mc/mark-previous-like-this) (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/mark-next-like-this) (autoload #'mc/mark-next-like-this "multiple-cursors" nil t))
       (declare-function mc/mark-previous-like-this "multiple-cursors")
       (declare-function mc/mark-next-like-this "multiple-cursors")
       (smartrep-define-key global-map "C-t"
                            '(("C-p" quote mc/mark-previous-like-this)
                              ("C-n" quote mc/mark-next-like-this)))))

    ((leaf multiple-cursors
       :smartrep (global-map
                  "C-t"
                  '(("C-p" . 'mc/mark-previous-like-this)
                    ("C-n" . 'mc/mark-next-like-this))))
     (prog1 'multiple-cursors
       (unless (fboundp 'mc/mark-previous-like-this) (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/mark-next-like-this) (autoload #'mc/mark-next-like-this "multiple-cursors" nil t))
       (declare-function mc/mark-previous-like-this "multiple-cursors")
       (declare-function mc/mark-next-like-this "multiple-cursors")
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
       (unless (fboundp 'mc/mark-previous-like-this) (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/mark-next-like-this) (autoload #'mc/mark-next-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/unmark-next-like-this) (autoload #'mc/unmark-next-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/unmark-previous-like-this) (autoload #'mc/unmark-previous-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/skip-to-next-like-this) (autoload #'mc/skip-to-next-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/skip-to-previous-like-this) (autoload #'mc/skip-to-previous-like-this "multiple-cursors" nil t))
       (unless (fboundp 'mc/mark-all-like-this) (autoload #'mc/mark-all-like-this "multiple-cursors" nil t))
       (declare-function mc/mark-previous-like-this "multiple-cursors")
       (declare-function mc/mark-next-like-this "multiple-cursors")
       (declare-function mc/unmark-next-like-this "multiple-cursors")
       (declare-function mc/unmark-previous-like-this "multiple-cursors")
       (declare-function mc/skip-to-next-like-this "multiple-cursors")
       (declare-function mc/skip-to-previous-like-this "multiple-cursors")
       (declare-function mc/mark-all-like-this "multiple-cursors")
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
       (unless (fboundp 'text-scale-increase) (autoload #'text-scale-increase "face-remap" nil t))
       (unless (fboundp 'text-scale-decrease) (autoload #'text-scale-decrease "face-remap" nil t))
       (declare-function text-scale-increase "face-remap")
       (declare-function text-scale-decrease "face-remap")
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
       (unless (fboundp 'yas-insert-snippet) (autoload #'yas-insert-snippet "yasnippet" nil t))
       (unless (fboundp 'yas-new-snippet) (autoload #'yas-new-snippet "yasnippet" nil t))
       (unless (fboundp 'yas-visit-snippet-file) (autoload #'yas-visit-snippet-file "yasnippet" nil t))
       (unless (fboundp 'yas-load-directory) (autoload #'yas-load-directory "yasnippet" nil t))
       (unless (fboundp 'yas-activate-extra-mode) (autoload #'yas-activate-extra-mode "yasnippet" nil t))
       (unless (fboundp 'yas-tryout-snippet) (autoload #'yas-tryout-snippet "yasnippet" nil t))
       (unless (fboundp 'yas-describe-tables) (autoload #'yas-describe-tables "yasnippet" nil t))
       (unless (fboundp 'yas/global-mode) (autoload #'yas/global-mode "yasnippet" nil t))
       (unless (fboundp 'yas/minor-mode) (autoload #'yas/minor-mode "yasnippet" nil t))
       (unless (fboundp 'yas-reload-all) (autoload #'yas-reload-all "yasnippet" nil t))
       (unless (fboundp 'hydra-yas-primary/body) (autoload #'hydra-yas-primary/body "yasnippet" nil t))
       (unless (fboundp 'hydra-yas/body) (autoload #'hydra-yas/body "yasnippet" nil t))
       (declare-function yas-insert-snippet "yasnippet")
       (declare-function yas-new-snippet "yasnippet")
       (declare-function yas-visit-snippet-file "yasnippet")
       (declare-function yas-load-directory "yasnippet")
       (declare-function yas-activate-extra-mode "yasnippet")
       (declare-function yas-tryout-snippet "yasnippet")
       (declare-function yas-describe-tables "yasnippet")
       (declare-function yas/global-mode "yasnippet")
       (declare-function yas/minor-mode "yasnippet")
       (declare-function yas-reload-all "yasnippet")
       (declare-function hydra-yas-primary/body "yasnippet")
       (declare-function hydra-yas/body "yasnippet")
       (defvar yas-minor-mode-map)
       (leaf-keys
        ((yas-minor-mode-map :package yasnippet
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

(cort-deftest-with-macroexpand leaf/transient
  '(((leaf dired-git
       :transient
       (transient-dwim-dired-mode--git
        ()
        "Transient-dwim for `dired-mode--git'."
        [["Worktree"
          ("c" "Commit" dired-git-commit)
          ("S" "Stage" dired-git-stage)
          ("U" "Unstage" dired-git-unstage)
          ("zz" "Stash" dired-git-stash)
          ("zp" "Stash pop" dired-git-stash-pop)
          ("X" "Reset --hard" dired-git-reset-hard)]
         ["Branch"
          ("b" "Branch" dired-git-branch)
          ("t" "Tag" dired-git-tag)
          ("f" "Fetch" dired-git-fetch)
          ("F" "Pull" dired-git-pull)
          ("m" "Merge" dired-git-merge)
          ("P" "Push" dired-git-push)
          ("!" "Run" dired-git-run)]]))

     (prog1 'dired-git
       (transient-define-prefix transient-dwim-dired-mode--git ()
         "Transient-dwim for `dired-mode--git'."
         [["Worktree"
           ("c" "Commit" dired-git-commit)
           ("S" "Stage" dired-git-stage)
           ("U" "Unstage" dired-git-unstage)
           ("zz" "Stash" dired-git-stash)
           ("zp" "Stash pop" dired-git-stash-pop)
           ("X" "Reset --hard" dired-git-reset-hard)]
          ["Branch"
           ("b" "Branch" dired-git-branch)
           ("t" "Tag" dired-git-tag)
           ("f" "Fetch" dired-git-fetch)
           ("F" "Pull" dired-git-pull)
           ("m" "Merge" dired-git-merge)
           ("P" "Push" dired-git-push)
           ("!" "Run" dired-git-run)]])))))

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
