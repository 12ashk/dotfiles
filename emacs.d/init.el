;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;; package management
(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(package-refresh-contents)

;; install packages by package.el
(mapc
  (lambda (package)
	(or (package-installed-p package)
		(package-install package)))
  '(
	auto-complete
	auto-complete-clang
	evil
	color-theme
	color-theme-solarized
	twittering-mode
	w3m
	))

(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")

(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil)

(add-to-list 'load-path "~/.emacs.d/elisp/themes/")
(color-theme-initialize)
(color-theme-solarized-dark)

(global-auto-complete-mode t)

(set-language-environment "Japanese")
;;fonts
(if (eq window-system 'ns) 
  (progn
	(create-fontset-from-ascii-font "Source Code Pro-16:weight=normal:slant=normal" nil "menlokakugo")
	(set-fontset-font "fontset-menlokakugo" 'unicode (font-spec :family "Hiragino Kaku Gothic ProN" ) nil 'append)
	(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
	(setq face-font-rescale-alist '((".*Hiragino.*" . 1.2) ))))

;;;;key remapping
;; set command as meta
(setq ns-command-modifier (quote meta))
;; set alt as super
(setq ns-alternate-modifier (quote super))

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

;;iswitchb
;; C-x bでバッファの一覧がニバッファに表示
;; C-s，C-r でバッファの選択を切り替え． 
;; C-n, C-p, C-f, C-bでもok
(iswitchb-mode 1)
(add-hook 'iswitchb-define-mode-map-hook
		  (lambda ()
			(define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
			(define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match)
			(define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
			(define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)))

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

;;Lisp処理系のコマンド
;;(setq inferior-lisp-program "/usr/local/Cellar/sbcl/1.1.4/bin/sbcl")
(setq inferior-lisp-program "/usr/local/bin/sbcl")
;; SLIMEがある場所をEmacsのロードパスに追加
(add-to-list 'load-path "~/.emacs.d/slime")
;; SLIMEを実行するときに自動的にロードさせる
(require 'slime-autoloads)
;; どのcontribパッケージを読み込むかの設定
(slime-setup '(slime-repl slime-fancy slime-banner))
(setq slime-net-coding-system 'utf-8-unix)
;; SLIME終了のための関数
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

;;;;twitter
(setq twittering-account-authorization 'authorized)
(setq twittering-oauth-access-token-alist
	  '(("oauth_token" . "129039695-7sTY5Yqf9isTJwHra7DEXityy1RePYZByrlh8om7")
		("oauth_token_secret" . "TONDa9zIRK5m4ubcWqpjSbwl1Ko81kE4Nm2CXByQXfQjU")
		("user_id" . "129039695")
		("screen_name" . "12ashk")))
;; 120秒おきに更新
(setq twittering-timer-interval 120)
;; API残数表示
(setq twittering-display-remaining t)
;; 表示方式の変更
(setq twittering-status-format "%i %S(%s)%p, %@:
	  %FILL{  %T // from %f%L%r%R}
	  ")

;; setting for evil
;; scroll with C-u
(setq evil-want-C-u-scroll t)
;; remapping from C-z
(setq evil-toggle-key "C-_")
(require 'evil)
(evil-mode 1)
