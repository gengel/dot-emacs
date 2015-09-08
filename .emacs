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
(yas-global-mode 1)

;; Copy $PATH variables
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Java
;; Check style of Java files.
;; (require 'flymake)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
;; (defun flymake-java-init ()
;;   (require 'flymake-cursor)
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "java"
;;           (list "-cp"
;;                 (expand-file-name "~/Projects/CQ5/Tools/CheckStyle/checkstyle-5.7-all.jar")
;;                 "com.puppycrawl.tools.checkstyle.Main"
;;                 "-c"
;;                 (expand-file-name "~/Projects/CQ5/Tools/CheckStyle/slate_checkstyle.xml")
;;                 local-file))))
;; (setq flymake-allowed-file-name-masks
;;       (cons '(".+\\.java$"
;;               flymake-java-init
;;               flymake-simple-cleanup
;;               flymake-get-real-file-name)
;;             flymake-allowed-file-name-masks))
;; (setq flymake-err-line-patterns
;;       (cons '("\\(.*\\.java\\):\\([0-9]+\\):[0-9]+: \\(.+\\)" 1 2 nil 3)
;;             flymake-err-line-patterns))
;; ;; Check *Message* buffer for errors. If you don't find any, you can remove this line.
;; ;;(setq flymake-log-level 3)



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
;              (flymake-mode)
              (local-set-key (kbd "C-z") 'hs-toggle-hiding)
              (local-set-key (kbd "C-x C-z") 'hs-hide-level)
              )))

(add-hook 'scala-mode-hook
          (lambda ()
            (progn
              (hs-minor-mode)
              (local-set-key (kbd "C-z") 'hs-toggle-hiding)
              )))

(defun java-indent-setup ()
  (progn
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    )
  )

(add-hook 'java-mode-hook 'java-indent-setup)

;; JavaScript
(setq js-indent-level 2)

;; Web Mode
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-auto-closing nil)
)

(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

(set-variable 'auto-mode-alist (remove '("\\.hbs$" . handlebars-mode) auto-mode-alist))

;; XML Mode

(set-variable 'nxml-child-indent 4)

;; More general variables
(tool-bar-mode -1)
(when window-system (set-frame-size (selected-frame) 177 54))

(global-set-key (kbd "C-c r") 'revert-buffer)

(customize-set-variable 'standard-indent 8)
(customize-set-variable 'tab-width 4)

(auto-fill-mode 0)
(auto-fill-mode -1)

(setq exec-path (cons "/opt/bin" (cons "/opt/local/bin" exec-path)))

(setq js2-basic-offset 2)

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
;(require 'company)
(require 'auto-complete)
(require 'auto-complete-config)

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dictionaries")
(ac-config-default)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq org-babel-clojure-backend 'cider)
(add-hook 'clojure-mode-hook 'turn-on-orgstruct)

(ido-mode)

(setq magit-last-seen-setup-instructions "1.4.0")
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)


(require 'simple-httpd)
(setq httpd-root "~/src/")
(setq httpd-port 8000)
;(httpd-start)


(global-set-key (kbd "C-z") 'hs-toggle-hiding)

;;(require 'crappy-jsp-mode)
(load "~/.emacs.d/local/crappy-jsp-mode/crappy-jsp-mode.el")

(add-to-list 'auto-mode-alist '("\\.jsp\\'" . crappy-jsp-mode))
(add-to-list 'auto-mode-alist '("\\.tag\\'" . crappy-jsp-mode))
;;(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(set-variable 'css-indent-offset 2)

(require 'eclim)
(global-eclim-mode)
(set-variable 'eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim")

(defun reverse-words (beg end)
  "Reverse the order of words in region."
  (interactive "*r")
  (apply
   'insert
   (reverse
    (split-string
     (delete-and-extract-region beg end) "\\b"))))

(global-set-key (kbd "C-c <left>") 'reverse-words)

;(load "~/.emacs.d/local/reddit.el")
;(add-to-list 'load-path "/Users/engelg/.emacs.d/local/")
;(require 'reddit)


;;YASNippets
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
