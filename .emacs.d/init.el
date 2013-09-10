(setq load-path (cons "~/.emacs.d/elisp" load-path))
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")
(require 'auto-complete)
(global-auto-complete-mode t)
(set-language-environment "Japanese")
(iswitchb-mode 1)
(add-hook 'iswitchb-define-mode-map-hook
  (lambda ()
	(define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
	(define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match)
	(define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
	(define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)))
(show-paren-mode)
(add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/themes")
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
(setq inferior-lisp-program "/usr/local/Cellar/sbcl/1.1.4/bin/sbcl")
;; SLIMEがある場所をEmacsのロードパスに追加
(add-to-list 'load-path "~/.emacs.d/slime")
;; SLIMEを実行するときに自動的にロードさせる
(require 'slime-autoloads)
;; どのcontribパッケージを読み込むかの設定
(slime-setup '(slime-repl slime-fancy slime-banner))
(setq slime-net-coding-system 'utf-8-unix)

(setq scheme-program-name "gosh")
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

;;YaTeX path
(setq load-path (cons "~/.emacs.d/elisp/yatex" load-path))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
	  (append '(("\\.tex$" . yatex-mode)
				("\\.ltx$" . yatex-mode)
				("\\.cls$" . yatex-mode)
				("\\.sty$" . yatex-mode)
				("\\.clo$" . yatex-mode)
				("\\.bbl$" . yatex-mode)) auto-mode-alist))

;; 文章作成時の漢字コードの設定
;; 1 = Shift_JIS, 2 = ISO-2022-JP, 3 = EUC-JP, 4 = UTF-8
;; default は 2
(setq YaTeX-kanji-code 4) ; euc-jp
(setq YaTeX-inhibit-prefix-letter t)

;;(load "/opt/local/var/macports/sources/rsync.macports.org/release/tarballs/ports/lang/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
