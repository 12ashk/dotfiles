;; setting for evil
;; scroll with C-u
(setq evil-want-C-u-scroll t)
;; emacs directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
;; package management
(require 'package)
(add-to-list 'package-archives
			 '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(defun package-install-with-refresh (package)
  (unless (assq package package-alist)
	(package-refresh-contents))
  (unless (package-installed-p package)
	(package-install package)))
;; install evil
(package-install-with-refresh 'evil)
;; enable evil(require 'evil)
(evil-mode 1)

(setq load-path (cons "~/.emacs.d/elisp" load-path))
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

(set-language-environment "Japanese")
(iswitchb-mode 1)

(add-hook 'iswitchb-define-mode-map-hook
		  (lambda ()
			(define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
			(define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match)
			(define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
			(define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)))
(show-paren-mode
 (global-linum-mode))

;; mouse on
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; set command as meta
(setq ns-command-modifier (quote meta))
;; set alt as super
(setq ns-alternate-modifier (quote super))
(add-to-list 'load-path "~/.emacs.d/elisp/themes")
(require 'color-theme)
(color-theme-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-use-system-font t)
 '(show-paren-mode t)
 '(tab-width 4))
(custom-set-faces)
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; initial size of emacs
;; Emacsの初期サイズ
;;(setq initial-frame-alist
;;      (append ;;       '((top . 22)    ; フレームの Y 位置(ピクセル数) ;;	  (left . 45)    ; フレームの X 位置(ピクセル数)
;;	   (width . 82)    ; フレーム幅(文字数)
;;	    (height . 30)   ; フレーム高(文字数)
;;	     ) initial-frame-alist))

(add-hook 'c-mode-common-hook
		  '(lambda ()
			 ;; センテンスの終了である ';' を入力したら、自動改行+インデント
			 (c-toggle-auto-hungry-state 1)
			 ;; RET キーで自動改行+インデント
			 (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
(setq-default tab-width 4)

;; SLIME終了のための関数
(defun slime-smart-quit ()
  (interactive)
  (when (slime-connected-p)
	(if (equal (slime-machine-instance) "my.workstation")
		(slime-quit-lisp)
	  (slime-disconnect)))
  (slime-kill-all-buffers))

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

(require 'cmuscheme)

;;ウィンドウサイズ調整
(define-key global-map "\C-^" 'my-window-resizer)
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
		(current-width (window-width))
		(current-height (window-height))
		(dx (if (= (nth 0 (window-edges)) 0)
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
;;pain-move
(global-set-key (kbd "C-x h")  'windmove-left)
(global-set-key (kbd "C-x j")  'windmove-down)
(global-set-key (kbd "C-x i")    'windmove-up)
(global-set-key (kbd "C-x l") 'windmove-right)

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

;;(load "/opt/local/var/macports/sources/rsync.macports.org/release/tarballs/ports/lang/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

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
										;(setq tex-command "/usr/texbin/ptex2pdf -l -u -ot '-synctex=1'")
										;(setq tex-command "/usr/texbin/pdflatex -synctex=1")
										;(setq tex-command "/usr/texbin/lualatex -synctex=1")
										;(setq tex-command "/usr/texbin/luajitlatex -synctex=1")
										;(setq tex-command "/usr/texbin/xelatex -synctex=1")
										;(setq tex-command "/usr/texbin/latexmk")
										;(setq tex-command "/usr/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$makeindex=q/mendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
										;(setq tex-command "/usr/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$makeindex=q/mendex %O -o %D %S/' -e '$dvips=q/dvips %O -z -f %S | convbkmk -u > %D/' -e '$ps2pdf=q/ps2pdf %O %S %D/' -norc -gg -pdfps")
										;(setq tex-command "/usr/texbin/latexmk -e '$pdflatex=q/pdflatex %O -synctex=1 %S/' -e '$bibtex=q/bibtex %O %B/' -e '$makeindex=q/makeindex %O -o %D %S/' -norc -gg -pdf")
										;(setq tex-command "/usr/texbin/latexmk -e '$pdflatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/bibtexu %O %B/' -e '$makeindex=q/texindy %O -o %D %S/' -norc -gg -lualatex")
										;(setq tex-command "/usr/texbin/latexmk -e '$pdflatex=q/luajitlatex %O -synctex=1 %S/' -e '$bibtex=q/bibtexu %O %B/' -e '$makeindex=q/texindy %O -o %D %S/' -norc -gg -lualatex")
										;(setq tex-command "/usr/texbin/latexmk -e '$pdflatex=q/xelatex %O -synctex=1 %S/' -e '$bibtex=q/bibtexu %O %B/' -e '$makeindex=q/texindy %O -o %D %S/' -norc -gg -xelatex")
(setq bibtex-command (cond ((string-match "uplatex\\|ptex2pdf" tex-command) "/usr/texbin/upbibtex")
						   ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "/usr/texbin/bibtexu")
						   ((string-match "pdflatex" tex-command) "/usr/texbin/bibtex")
						   (t "/usr/texbin/upbibtex")))
(setq makeindex-command (cond ((string-match "uplatex\\|ptex2pdf" tex-command) "/usr/texbin/mendex")
							  ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "/usr/texbin/texindy")
							  ((string-match "pdflatex" tex-command) "/usr/texbin/makeindex")
							  (t "/usr/texbin/mendex")))
(setq dvi2-command "/usr/bin/open -a Preview")
										;(setq dvi2-command "/usr/bin/open -a Skim")
										;(setq dvi2-command "/usr/bin/open -a TeXShop")
										;(setq dvi2-command "/usr/bin/open -a TeXworks")
										;(setq dvi2-command "/usr/bin/open -a Firefox")
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
