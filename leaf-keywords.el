;;; leaf-keywords.el --- Additional keywords for leaf.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp settings
;; Version: 0.1.0
;; URL: https://github.com/conao3/leaf-keywords.el
;; Package-Requires: ((emacs "24.4"))

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

;; simpify init.el

;;; Code:

(require 'leaf)

(defgroup leaf-keywords nil
  "Additional keywords for `leaf'."
  :group 'lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customize backend
;;

(progn
  (setq leaf-keywords
        (leaf-insert-list-before leaf-keywords :ensure
          (cdr
           '(:dummy
             :el-get `(,@(mapcar (lambda (elm) `(el-get-bundle ,@elm)) leaf--value) ,@leaf--body)))))
  (setq leaf-keywords
        (leaf-insert-list-before leaf-keywords :commands
          (cdr
           '(:dummy
             :diminish `(,@(mapcar (lambda (elm) `(diminish ,elm)) leaf--value) ,@leaf--body)
             :chord    (progn
                         (mapc (lambda (elm) (leaf-register-autoload (leaf-plist-get :func elm) leaf--name)) leaf--value)
                         `(,@(mapcar (lambda (elm) `(leaf-key-chord ,(leaf-plist-get :key elm) #',(leaf-plist-get :func elm) ,(leaf-plist-get :map elm))) leaf--value) ,@leaf--body))
             :chord*   (progn
                         (mapc (lambda (elm) (leaf-register-autoload (leaf-plist-get :func elm) leaf--name)) leaf--value)
                         `(,@(mapcar (lambda (elm) `(leaf-key-chord* ,(leaf-plist-get :key elm) #',(leaf-plist-get :func elm) ,(leaf-plist-get :map elm))) leaf--value) ,@leaf--body))))))
  (setq leaf-normalize
        (append
         '(((memq leaf--key '(:diminish))
            ;; Accept: 't, 'nil, symbol and list of these (and nested)
            ;; Return: symbol list.
            ;; Note  : 't will convert to 'leaf--name
            ;;         if 'nil placed on top, ignore all argument
            ;;         remove duplicate element
            (let ((ret (leaf-flatten leaf--value)))
              (if (eq nil (car ret))
                  nil
                (delete-dups (delq nil (leaf-subst t leaf--name ret))))))

           ((memq leaf--key '(:chord :chord*))
            ;; Accept: list of pair (bind . func), (bind . nil)
            ;;         ([:{{hoge}}-map] [:package {{pkg}}](bind . func) (bind . func) ...)
            ;;         optional, [:{{hoge}}-map] [:package {{pkg}}]
            ;; Return: list of ([:{{hoge}}-map] [:package {{pkg}}] (bind . func))
            (mapcan (lambda (elm)
                      (cond
                       ((leaf-pairp elm 'allow-nil)
                        (list `(:package ,leaf--name :key ,(car elm) :func ,(cdr elm))))
                       ((not (keywordp (car elm)))
                        (mapcar
                         (lambda (el) `(:package ,leaf--name :key ,(car el) :func ,(cdr el))) elm))
                       (t
                        (delq nil
                              (mapcar (lambda (el)
                                        (when (leaf-pairp el 'allow-nil)
                                          (let ((map (intern (substring (symbol-name (car elm)) 1)))
                                                (pkg (leaf-plist-get :package (cdr elm))))
                                            (cdr `(:dummy
                                                   :map ,map
                                                   :package ,(if pkg pkg leaf--name)
                                                   :key ,(car el)
                                                   :func ,(cdr el))))))
                                      (cdr elm))))))
                    (mapcan (lambda (elm)
                              (if (or (and (listp elm) (keywordp (car elm)))
                                      (and (listp elm) (atom (car elm)) (atom (cdr elm))))
                                  (list elm)
                                elm))
                            leaf--value)))

           ((memq leaf--key '(:el-get))
            (mapcar
             (lambda (elm)
               (leaf-normalize-list-in-list (if (eq t elm) leaf--name elm) 'dotlistp))
             leaf--value)))
         leaf-normalize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support functions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  leaf keywords definition
;;

(eval
 `(progn
    ,@(mapcar
       (lambda (elm)
         (let ((keyname (substring (symbol-name elm) 1)))
           `(defcustom ,(intern (format "leaf-expand-%s" keyname)) t
              ,(format "If nil, do not expand values for :%s." keyname)
              :type 'boolean
              :group 'leaf)))
       (leaf-plist-keys leaf-keywords))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Handler
;;

(defmacro leaf-key-chord (chord command &optional keymap)
  "Bind CHORD to COMMAND in KEYMAP (`global-map' if not passed).

CHORD must be 2 length of string
COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap and not a quoted symbol.
For example:
  (leaf-key-chord \"jk\" 'undo 'c-mode-map)"
  (let ((key1 (logand 255 (aref chord 0)))
        (key2 (logand 255 (aref chord 1))))
    (if (eq key1 key2)
        `(leaf-key [key-chord ,key1 ,key2] ,command ,keymap)
      `(progn
         (leaf-key [key-chord ,key1 ,key2] ,command ,keymap)
         (leaf-key [key-chord ,key2 ,key1] ,command ,keymap)))))

(defmacro leaf-key-chord* (key command)
  "Similar to `leaf-key-chord', but overrides any mode-specific bindings."
  `(leaf-key-chord ,key ,command 'leaf-key-override-global-map))

(defmacro leaf-key-chords (&rest plist)
  "Bind multiple keys at once.

Accepts keyword arguments:
MUST:
  :bind (KEY . COMMAND) - bind KEY to COMMAND
        (KEY . nil)     - unbind KEY

OPTIONAL:
  :map MAP              - a keymap into which the keybind should be added
  :package PKG          - a package in which the MAP defined in
                          (wrap `eval-after-load' PKG)

NOTE: :package, :bind can accept list of these.
  :package (PKG ... PKG)
  :bind ((KEY . COMMAND) ... (KEY . COMMAND))"
  (let ((mmap  (leaf-plist-get :map plist))
        (mpkg  (leaf-plist-get :package plist))
        (mbind (leaf-plist-get :bind plist))
        (mform))
    (unless mbind (leaf-error "leaf-key-chords need :bind argument.  arg: %s" plist))
    (when (atom mpkg) (setq mpkg `(,mpkg)))
    (when (leaf-pairp mbind 'allow-nil) (setq mbind `(,mbind)))
    (setq mform `(progn
                  ,@(mapcar
                     (lambda (elm) `(leaf-key-chord ,(car elm) ',(cdr elm) ',mmap))
                     mbind)))
    (if (equal '(nil) mpkg)
        mform
      (dolist (pkg mpkg) (setq mform `(eval-after-load ',pkg ',mform)))
      mform)))

(defmacro leaf-key-chords* (&rest plist)
  "Similar to `leaf-key-chords', but overrides any mode-specific bindings."
  (when (leaf-plist-get :map plist)
    (leaf-error "leaf-keys* should not call with :map argument.  arg: %s" plist))
  `(leaf-key-chords :map 'leaf-key-override-global-map ,@plist))

(provide 'leaf-keywords)
;;; leaf-keywords.el ends here

