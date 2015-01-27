;; Remove minimize button
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

;; Backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Set up packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
  )

;; Add some additonal modules
(require 'expand-region)
(global-set-key (kbd "C-;") 'er/expand-region)

(require 'yasnippet)

;; Copy $PATH variables
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Java
;; Check style of Java files.
(require 'flymake)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(defun flymake-java-init ()
  (require 'flymake-cursor)
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "java"
          (list "-cp"
                (expand-file-name "~/Projects/CQ5/Tools/CheckStyle/checkstyle-5.7-all.jar")
                "com.puppycrawl.tools.checkstyle.Main"
                "-c"
                (expand-file-name "~/Projects/CQ5/Tools/CheckStyle/slate_checkstyle.xml")
                local-file))))
(setq flymake-allowed-file-name-masks
      (cons '(".+\\.java$"
              flymake-java-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))
(setq flymake-err-line-patterns
      (cons '("\\(.*\\.java\\):\\([0-9]+\\):[0-9]+: \\(.+\\)" 1 2 nil 3)
            flymake-err-line-patterns))
;; Check *Message* buffer for errors. If you don't find any, you can remove this line.
;;(setq flymake-log-level 3)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(customize-set-variable 'indent-tabs-mode nil)




 (defun java-mode-untabify ()
   (save-excursion
     (goto-char (point-min))
     (while (re-search-forward "[ \t]+$" nil t)
       (delete-region (match-beginning 0) (match-end 0)))
     (goto-char (point-min))
     (if (search-forward "\t" nil t)
         (untabify (1- (point)) (point-max))))
   nil)

(add-hook 'java-mode-hook
           (lambda () (add-hook 'write-contents-hooks 'java-mode-untabify nil t)))

(add-hook 'java-mode-hook
          (lambda () (setq c-basic-offset 4)))

(defun javadoc-class ()
  (insert "
/**
 *
 *
 */"))

(defun javadoc-method ()
  (insert "
    /**
     *
     *
     * @return
     */")
  (backward-char 18)
  )

(defun javadoc-accessor ()
    (let* ((var (read-from-minibuffer "Property name: "))
           (accessor-doc (format "\n    /**\n     * Accessor for %s.\n     *\n     * @return %s\n     */\n" var var)))
      (insert accessor-doc)))


(global-set-key (kbd "C-c j c") '(lambda () (interactive)(javadoc-class)))
(global-set-key (kbd "C-c j m") '(lambda () (interactive)(javadoc-method)))
(global-set-key (kbd "C-c j a") '(lambda () (interactive)(javadoc-accessor)))

(add-hook 'java-mode-hook
          (lambda ()
            (progn
              (hs-minor-mode)
              (flymake-mode)
              (local-set-key (kbd "C-z") 'hs-toggle-hiding)
              (local-set-key (kbd "C-x C-z") 'hs-hide-level)
              )))

; JavaScript
(setq js-indent-level 2)

; More general variables
(tool-bar-mode -1)
(when window-system (set-frame-size (selected-frame) 200 60))

(global-set-key (kbd "C-c r") 'revert-buffer)

(customize-set-variable 'standard-indent 8)
(customize-set-variable 'tab-width 4)

(auto-fill-mode 0)
(auto-fill-mode -1)

(setq exec-path (cons "/opt/bin" (cons "/opt/local/bin" exec-path)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Menlo")))))

(require 'cider)
(require 'company)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq org-babel-clojure-backend 'cider)
(add-hook 'clojure-mode-hook 'turn-on-orgstruct)

(ido-mode)
