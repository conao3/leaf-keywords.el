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

(leaf leaf
  :defvar
  leaf-defaults leaf-system-defaults leaf-defer-keywords
  leaf--raw leaf--name leaf--key leaf--keyname
  leaf--value leaf--body leaf--rest leaf--autoload
  leaf-keywords leaf-normarize
  :defun
  leaf-warn leaf-error
  leaf-truep leaf-pairp
  leaf-dotlistp leaf-list-memq
  leaf-subst leaf-flatten
  leaf-insert-before leaf-insert-after
  leaf-insert-list-before leaf-insert-list-after
  leaf-plist-keys leaf-plist-get leaf-register-autoload
  leaf-to-string leaf-add-keyword-before
  leaf-add-keyword-after leaf-normalize-list-in-list)

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
             :diminish `(,@(mapcar (lambda (elm) `(diminish ,elm)) leaf--value) ,@leaf--body)))))
  (setq leaf-normarize
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
           ((memq leaf--key '(:el-get))
            (mapcar
             (lambda (elm)
               (leaf-normalize-list-in-list (if (eq t elm) leaf--name elm) 'dotlistp))
             leaf--value)))
         leaf-normarize)))

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

(provide 'leaf-keywords)
;;; leaf-keywords.el ends here

