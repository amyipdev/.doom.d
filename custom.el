;;; custom.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <amy@clairo>

;;(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages '(super-save)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Manual import of super-save
(require 'seq)

(defgroup super-save nil
  "Smart-saving of buffers."
  :group 'tools
  :group 'convenience)

(defvar super-save-mode-map
  (make-sparse-keymap)
  "super-save mode's keymap.")

(defcustom super-save-triggers
  '(switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right next-buffer previous-buffer)
  "A list of commands which would trigger `super-save-command'."
  :group 'super-save
  :type '(repeat symbol)
  :package-version '(super-save . "0.1.0"))

(defcustom super-save-hook-triggers
  '(mouse-leave-buffer-hook focus-out-hook)
  "A list of hooks which would trigger `super-save-command'."
  :group 'super-save
  :type '(repeat symbol)
  :package-version '(super-save . "0.3.0"))

(defcustom super-save-auto-save-when-idle nil
  "Save automatically when Emacs is idle."
  :group 'super-save
  :type 'boolean
  :package-version '(super-save . "0.2.0"))

(defcustom super-save-all-buffers nil
  "Auto-save all buffers, not just the current one.

Setting this to t can be interesting when you make indirect buffer edits, like
when editing `grep's results with `occur-mode' and `occur-edit-mode', or when
running a project-wide search and replace with `project-query-replace-regexp'
and so on.  In these cases, we can indirectly edit several buffers without
actually visiting or switching to these buffers.  Hence, this option allow to
automatically save these buffers, even when they aren't visible in any window."
  :group 'super-save
  :type 'boolean
  :package-version '(super-save . "0.4.0"))

(defcustom super-save-idle-duration 5
  "Delay in seconds for which Emacs has to be idle before auto-saving.
See `super-save-auto-save-when-idle'."
  :group 'super-save
  :type 'integer
  :package-version '(super-save . "0.2.0"))

(defcustom super-save-remote-files t
  "Save remote files when t, ignore them otherwise."
  :group 'super-save
  :type 'boolean
  :package-version '(super-save . "0.3.0"))

(defcustom super-save-silent nil
  "Save silently, don't display any message."
  :group 'super-save
  :type 'boolean
  :package-version '(super-save . "0.4.0"))

(defcustom super-save-delete-trailing-whitespace nil
  "Controls whether to delete the trailing whitespace before saving.
Set to `except-current-line' if you want to avoid the current line."
  :group 'super-save
  :type '(choice (boolean :tag "Enable/disable deleting trailing whitespace for the whole buffer.")
          (symbol :tag "Delete trailing whitespace except the current line." except-current-line))
  :package-version '(super-save . "0.4.0"))

(defcustom super-save-exclude nil
  "A list of regexps for `buffer-file-name' excluded from super-save.
When a `buffer-file-name' matches any of the regexps it is ignored."
  :group 'super-save
  :type '(repeat (choice regexp))
  :package-version '(super-save . "0.4.0"))

(defcustom super-save-max-buffer-size nil
  "Maximal size of buffer (in characters), for which super-save work.
Exists mostly because saving constantly huge buffers can be slow in some cases.
Set to 0 or nil to disable."
  :group 'super-save
  :type 'integer
  :package-version '(super-save . "0.4.0"))

(defcustom super-save-predicates
  '((lambda () buffer-file-name)
    (lambda () (buffer-modified-p (current-buffer)))
    (lambda () (file-writable-p buffer-file-name))
    (lambda () (if (and super-save-max-buffer-size (> super-save-max-buffer-size 0))
                   (< (buffer-size) super-save-max-buffer-size)
                 t))
    (lambda ()
      (if (file-remote-p buffer-file-name) super-save-remote-files t))
    (lambda () (super-save-include-p buffer-file-name)))
  "Predicates, which return nil, when the buffer doesn't need to be saved.
Predicate functions don't take any arguments.  If a predicate doesn't know
whether this buffer needs to be super-saved or not, then it must return t."
  :group 'super-save
  :type 'integer
  :package-version '(super-save . "0.4.0"))

(defun super-save-include-p (filename)
  "Return non-nil if FILENAME doesn't match any of the `super-save-exclude'."
  (not (seq-some (lambda (regexp) (string-match-p regexp filename)) super-save-exclude)))

(defun super-save-p ()
  "Return t when current buffer should be saved, otherwise return nil.

This function relies on the variable `super-save-predicates'."
  (seq-every-p #'funcall super-save-predicates))

(defun super-save-delete-trailing-whitespace-maybe ()
  "Delete trailing whitespace, optionally avoiding the current line.

See `super-save-delete-trailing-whitespace'."
  (cond
   ((eq super-save-delete-trailing-whitespace 'except-current-line)
    (let ((start (line-beginning-position))
          (current (point)))
      (save-excursion
        (when (< (point-min) start)
          (save-restriction
            (narrow-to-region (point-min) (1- start))
            (delete-trailing-whitespace)))
        (when (> (point-max) current)
          (save-restriction
            (narrow-to-region current (point-max))
            (delete-trailing-whitespace))))))
   (super-save-delete-trailing-whitespace
    (delete-trailing-whitespace))))

(defun super-save-buffer (buffer)
  "Save BUFFER if needed, super-save style."
  (with-current-buffer buffer
    (save-excursion
      (when (super-save-p)
        (super-save-delete-trailing-whitespace-maybe)
        (if super-save-silent
            (with-temp-message ""
              (let ((inhibit-message t)
                    (inhibit-redisplay t)
                    (message-log-max nil))
                (basic-save-buffer)))
          (basic-save-buffer))))))

(defun super-save-command ()
  "Save the relevant buffers if needed.

When `super-save-all-buffers' is non-nil, save all modified buffers, else, save
only the current buffer."
  (mapc #'super-save-buffer (if super-save-all-buffers (buffer-list) (list (current-buffer)))))

(defvar super-save-idle-timer)

(defun super-save-command-advice (&rest _args)
  "A simple wrapper around `super-save-command' that's advice-friendly."
  (super-save-command))

(defun super-save-advise-trigger-commands ()
  "Apply super-save advice to the commands listed in `super-save-triggers'."
  (mapc
   (lambda (command)
     (advice-add command :before #'super-save-command-advice))
   super-save-triggers))

(defun super-save-remove-advice-from-trigger-commands ()
  "Remove super-save advice from to the commands listed in `super-save-triggers'."
  (mapc
   (lambda (command)
     (advice-remove command #'super-save-command-advice))
   super-save-triggers))

(defun super-save-initialize-idle-timer ()
  "Initialize super-save idle timer if `super-save-auto-save-when-idle' is true."
  (setq super-save-idle-timer
        (when super-save-auto-save-when-idle
          (run-with-idle-timer super-save-idle-duration t #'super-save-command))))

(defun super-save-stop-idle-timer ()
  "Stop super-save idle timer if `super-save-idle-timer' is set."
  (when super-save-idle-timer (cancel-timer super-save-idle-timer)))

(defun super-save-initialize ()
  "Setup super-save's advices and hooks."
  (super-save-advise-trigger-commands)
  (super-save-initialize-idle-timer)
  (dolist (hook super-save-hook-triggers)
    (add-hook hook #'super-save-command)))

(defun super-save-stop ()
  "Cleanup super-save's advices and hooks."
  (super-save-remove-advice-from-trigger-commands)
  (super-save-stop-idle-timer)
  (dolist (hook super-save-hook-triggers)
    (remove-hook hook #'super-save-command)))

;;;###autoload
(define-minor-mode super-save-mode
  "A minor mode that saves your Emacs buffers when they lose focus."
  :lighter " super-save"
  :keymap super-save-mode-map
  :group 'super-save
  :global t
  (cond
   (super-save-mode (super-save-initialize))
   (t (super-save-stop))))

(super-save-mode +1)
(setq super-save-auto-save-when-idle t)
(setq auto-save-default nil)

(require 'cc-mode)

(eval-when-compile
  (and (= emacs-major-version 24)
       (>= emacs-minor-version 4)
       (require 'cl-lib))
  (require 'cc-langs)
  (require 'cc-fonts))

;; This mode does not inherit properties from other modes. So, we do not use
;; the usual `c-add-language' function.
(eval-and-compile
  (put 'protobuf-mode 'c-mode-prefix "protobuf-"))

;; The following code uses of the `c-lang-defconst' macro define syntactic
;; features of protocol buffer language.  Refer to the documentation in the
;; cc-langs.el file for information about the meaning of the -kwds variables.

(c-lang-defconst c-primitive-type-kwds
  protobuf '("double" "float" "int32" "int64" "uint32" "uint64" "sint32"
             "sint64" "fixed32" "fixed64" "sfixed32" "sfixed64" "bool"
             "string" "bytes" "group"))

(c-lang-defconst c-modifier-kwds
  protobuf '("required" "optional" "repeated" "oneof"))

(c-lang-defconst c-class-decl-kwds
  protobuf '("message" "enum" "service"))

(c-lang-defconst c-constant-kwds
  protobuf '("true" "false"))

(c-lang-defconst c-other-decl-kwds
  protobuf '("package" "import" "syntax"))

(c-lang-defconst c-other-kwds
  protobuf '("default" "max"))

(c-lang-defconst c-identifier-ops
  ;; Handle extended identifiers like google.protobuf.MessageOptions
  protobuf '((left-assoc ".")))

;; The following keywords do not fit well in keyword classes defined by
;; cc-mode.  So, we approximate as best we can.

(c-lang-defconst c-type-list-kwds
  protobuf '("extensions" "to" "reserved"))

(c-lang-defconst c-typeless-decl-kwds
  protobuf '("extend" "rpc" "stream" "option" "returns"))


;; Here we remove default syntax for loops, if-statements and other C
;; syntactic features that are not supported by the protocol buffer language.

(c-lang-defconst c-brace-list-decl-kwds
  ;; Remove syntax for C-style enumerations.
  protobuf nil)

(c-lang-defconst c-block-stmt-1-kwds
  ;; Remove syntax for "do" and "else" keywords.
  protobuf nil)

(c-lang-defconst c-block-stmt-2-kwds
  ;; Remove syntax for "for", "if", "switch" and "while" keywords.
  protobuf nil)

(c-lang-defconst c-simple-stmt-kwds
  ;; Remove syntax for "break", "continue", "goto" and "return" keywords.
  protobuf nil)

(c-lang-defconst c-paren-stmt-kwds
  ;; Remove special case for the "(;;)" in for-loops.
  protobuf nil)

(c-lang-defconst c-label-kwds
  ;; Remove case label syntax for the "case" and "default" keywords.
  protobuf nil)

(c-lang-defconst c-before-label-kwds
  ;; Remove special case for the label in a goto statement.
  protobuf nil)

(c-lang-defconst c-cpp-matchers
  ;; Disable all the C preprocessor syntax.
  protobuf nil)

(c-lang-defconst c-decl-prefix-re
  ;; Same as for C, except it does not match "(". This is needed for disabling
  ;; the syntax for casts.
  protobuf "\\([\{\};,]+\\)")


;; Add support for variable levels of syntax highlighting.

(defconst protobuf-font-lock-keywords-1 (c-lang-const c-matchers-1 protobuf)
  "Minimal highlighting for protobuf-mode.")

(defconst protobuf-font-lock-keywords-2 (c-lang-const c-matchers-2 protobuf)
  "Fast normal highlighting for protobuf-mode.")

(defconst protobuf-font-lock-keywords-3 (c-lang-const c-matchers-3 protobuf)
  "Accurate normal highlighting for protobuf-mode.")

(defvar protobuf-font-lock-keywords protobuf-font-lock-keywords-3
  "Default expressions to highlight in protobuf-mode.")

;; Our syntax table is auto-generated from the keyword classes we defined
;; previously with the `c-lang-const' macro.
(defvar protobuf-mode-syntax-table nil
  "Syntax table used in protobuf-mode buffers.")
(or protobuf-mode-syntax-table
    (setq protobuf-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table protobuf))))

(defvar protobuf-mode-abbrev-table nil
  "Abbreviation table used in protobuf-mode buffers.")

(defvar protobuf-mode-map nil
  "Keymap used in protobuf-mode buffers.")
(or protobuf-mode-map
    (setq protobuf-mode-map (c-make-inherited-keymap)))

(easy-menu-define protobuf-menu protobuf-mode-map
  "Protocol Buffers Mode Commands"
  (cons "Protocol Buffers" (c-lang-const c-mode-menu protobuf)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;;;###autoload
(define-derived-mode protobuf-mode prog-mode "Protocol-Buffers"
  "Major mode for editing Protocol Buffers description language.

The hook `c-mode-common-hook' is run with no argument at mode
initialization, then `protobuf-mode-hook'.

Key bindings:
\\{protobuf-mode-map}"
  :after-hook (c-update-modeline)
  (setq abbrev-mode t)
  (c-initialize-cc-mode t)
  (c-init-language-vars protobuf-mode)
  (c-common-init 'protobuf-mode)
  (setq imenu-generic-expression
	    '(("Message" "^[[:space:]]*message[[:space:]]+\\([[:alnum:]]+\\)" 1)
          ("Enum" "^[[:space:]]*enum[[:space:]]+\\([[:alnum:]]+\\)" 1)
          ("Service" "^[[:space:]]*service[[:space:]]+\\([[:alnum:]]+\\)" 1)))
  (c-run-mode-hooks 'c-mode-common-hook))

(provide 'protobuf-mode)
(require 'protobuf-mode)
