;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (p '(
	auto-complete
	auto-complete-clang
	evil
	color-theme
	color-theme-solarized
	highlight-indentation
	cider
	clojure-mode
	ac-nrepl
	clojure-cheatsheet
    clojure-snippets
	paredit
	slime
	))
 (when (not (package-installed-p p))
      (package-install p)))

;;(add-to-list 'load-path "~/.emacs.d/elisp/")
;;(require 'install-elisp)
;;(setq install-elisp-repository-directory "~/.emacs.d/elisp/")
;;
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil)
(global-auto-complete-mode t)

(add-to-list 'load-path "~/.emacs.d/elisp/themes/")
(require 'color-theme)
;;(setq color-theme-is-global t)
;;(color-theme-initialize)
;;(color-theme-solarized-dark)

(require 'highlight-indentation)
(setq highlight-indentation-mode t)
(setq highlight-indentation-offset 4)
(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(set-language-environment "Japanese")
;;fontjs
(if (eq window-system 'ns) 
  (progn
	(create-fontset-from-ascii-font "Source Code Pro-16:weight=normal:slant=normal" nil "menlokakugo")
	(set-fontset-font "fontset-menlokakugo" 'unicode (font-spec :family "Hiragino Kaku Gothic ProN" ) nil 'append)
	(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
	(setq face-font-rescale-alist '((".*Hiragino.*" . 1.2) ))))

;; Dired 
;; ディレクトリを開く場合バッファを増やさない
(defadvice dired-find-file (around kill-dired-buffer activate)
		   (let ((before (current-buffer)))
			 ad-do-it
			 (when (eq major-mode 'dired-mode)
			   (kill-buffer before))))
;; ディレクトリを上がる場合バッファを増やさない
(defadvice dired-up-directory (around kill-up-dired-buffer activate)
		   (let ((before (current-buffer)))
			 ad-do-it
			 (when (eq major-mode 'dired-mode)
			   (kill-buffer before))))

;;括弧のハイライト
(show-paren-mode
  (global-linum-mode))

(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(current-language-environment "UTF-8")
  '(font-use-system-font t)
  '(show-paren-mode t)
  '(tab-width 4))

;;;; clojure
(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-mode-hook 'cider-mode)
(eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'cider-repl-mode))

;; pareditの設定
(require 'paredit)
(dolist (hook '(cider-repl-mode-hook
				 clojure-mode-hook
				 scheme-mode-hook))
  (add-hook hook 'paredit-mode))

;;CL
;(setq inferior-lisp-program "/usr/local/bin/sbcl")
;; SLIMEがある場所をEmacsのロードパスに追加
;(add-to-list 'load-path "~/.emacs.d/slime")
;; SLIMEを実行するときに自動的にロードさせる
(require 'slime)
(setq inferior-lisp-program "sbcl")
(require 'slime-autoloads)
;; どのcontribパッケージを読み込むかの設定
(slime-setup '(slime-repl slime-fancy slime-banner))
(setq slime-net-coding-system 'utf-8-unix)
;; SLIME終了
(defun slime-smart-quit ()
  (interactive)
  (when (slime-connected-p)
	(if (equal (slime-machine-instance) "my.workstation")
	  (slime-quit-lisp)
	  (slime-disconnect)))
  (slime-kill-all-buffers))

;;ウィンドウサイズ調整
(global-set-key "\C-c\C-r" 'window-resizer)
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
		(current-width (window-width))
		(current-height (window-height))
		(dx (if (= (nth 0 (window-edges)) 0) 1
			  -1))
		(dy (if (= (nth 1 (window-edges)) 0) 1
			  -1))
		c)
	(catch 'end-flag
		   (while t
				  (message "size[%dx%d]"
						   (window-width) (window-height))
				  (setq c (read-char))
				  (cond ((= c ?l)
						 (enlarge-window-horizontally dx))
						((= c ?h)
						 (shrink-window-horizontally dx))
						((= c ?j)
						 (enlarge-window dy))
						((= c ?k)
						 (shrink-window dy))
						;; otherwise
						(t
						  (message "Quit")
						  (throw 'end-flag t)))))))

;;pain-move like vim
(global-set-key (kbd "C-x h")  'windmove-left)
(global-set-key (kbd "C-x j")  'windmove-down)
(global-set-key (kbd "C-x i")    'windmove-up) ;avoid conflicting with kill buffer
(global-set-key (kbd "C-x l") 'windmove-righ)

;;setting for scheme
(require 'cmuscheme)
(setq scheme-program-name "gosh")
;;scheme-mode-hook
(defvar ac-source-scheme
  '((candidates
	  . (lambda ()
		  (require 'scheme-complete)
		  (all-completions ac-target (car (scheme-current-env))))))
  "Source for scheme keywords.")
(add-hook 'scheme-mode-hook
		  '(lambda ()
			 (make-local-variable 'ac-sources)
			 (setq ac-sources (append ac-sources '(ac-source-scheme)))))

;; YaTeX
(add-to-list 'load-path "~/.emacs.d/elisp/yatex")
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
	  (append '(("\\.tex$" . yatex-mode)
				("\\.ltx$" . yatex-mode)
				("\\.cls$" . yatex-mode)
				("\\.sty$" . yatex-mode)
				("\\.clo$" . yatex-mode)
				("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
	  '(("open\\|Preview\\|TeXShop\\|TeXworks\\|Skim\\|mupdf\\|Firefox\\|Adobe" . ".pdf")))
(setq tex-command "/usr/texbin/platex")
(setq bibtex-command (cond ((string-match "uplatex\\|ptex2pdf" tex-command) "/usr/texbin/upbibtex")
						   ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "/usr/texbin/bibtexu")
						   ((string-match "pdflatex" tex-command) "/usr/texbin/bibtex")
						   (t "/usr/texbin/upbibtex")))
(setq makeindex-command (cond ((string-match "uplatex\\|ptex2pdf" tex-command) "/usr/texbin/mendex")
							  ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "/usr/texbin/texindy")
							  ((string-match "pdflatex" tex-command) "/usr/texbin/makeindex")
							  (t "/usr/texbin/mendex")))
(setq dvi2-command "/usr/bin/open -a Preview")
(setq dviprint-command-format "/usr/bin/open -a \"Adobe Reader\" `echo %s | gsed -e \"s/\\.[^.]*$/\\.pdf/\"`")

(defun skim-forward-search ()
  (interactive)
  (progn
	(process-kill-without-query
	  (start-process
		"displayline"
		nil
		"/Applications/Skim.app/Contents/SharedSupport/displayline"
		(number-to-string (save-restriction
							(widen)
							(count-lines (point-min) (point))))
		(expand-file-name
		  (concat (file-name-sans-extension (or YaTeX-parent-file
												(save-excursion
												  (YaTeX-visit-main t)
												  buffer-file-name)))
				  ".pdf"))
		buffer-file-name))))

(add-hook 'yatex-mode-hook
		  '(lambda ()
			 (define-key YaTeX-mode-map (kbd "C-c s") 'skim-forward-search)))

(add-hook 'yatex-mode-hook
		  '(lambda ()
			 (auto-fill-mode -1)))

;; RefTeX with yatex-mode-hook;;
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
		  '(lambda ()
			 (reftex-mode 1)
			 (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
			 (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))

;; setting for evil
;; scroll with C-u
(setq evil-want-C-u-scroll t)
;; remapping from C-z
(setq evil-toggle-key "C-_")
(require 'evil)
(evil-mode 1)


(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
                 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
(setq-default ispell-program-name "/usr/local/bin/aspell")

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(require 'google-translate)

(global-set-key "\C-xt" 'google-translate-at-point)
(global-set-key "\C-xT" 'google-translate-query-translate)

;; 翻訳のデフォルト値を設定(ja -> en)（無効化は C-u する）
(custom-set-variables
  '(google-translate-default-source-language "ja")
   '(google-translate-default-target-language "en"))

;; google-translate.elの翻訳バッファをポップアップで表示させる
(push '("*Google Translate*") popwin:special-display-config)
