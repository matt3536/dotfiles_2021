;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;; "conf"ディレクトリ配下読み込み
;; (load "init-perl")

;; カスタムファイル(自動で書き込まれるファイル)を別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))
;; カスタムファイルが存在しない場合は作成する
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;; カスタムファイルを読み込む
(load custom-file)

;; init-loader.el(分割ファイル自動読み込み拡張機能)
;;  1.)2桁の数字から始まる設定ファイルを数字の順番から読み込む(00-env.el,10-perl.el)
;;  2.)Meadow(Windows)の場合、「meadow」から始まる名前のファイルを読み込む(meadow-*.el)
;;  3.)CocoEmacs(Mac)の場合、「cocoa-emacs」から始まる名前のファイルを読み込む(cocoa-emacs-*.el)
;; 設定ファイルの読み込みはerrorが出ても中断されず、スキップされ次のファイルの読み込みが始まる
;; >読み込んだログは「*Message*」バッファに蓄積される
(require 'init-loader')
(init-loader-load "~/.emacs.d/conf")

;; gitがインストールされている場合、magitを読み込む
(when (executable-find "git")
  (require 'magit nil))

;;
;; キーバインド関連
;;

;; C-mで改行＋インデントを行う
(global-set-key (kbd "C-m") 'newline-and-indent)

;; C-tでウィンドウを切り替える。初期値はtranspose-chars
(global-set-key (kbd "C-t") 'other-window)

;; C-hを<DEL>に置き換え、"C-x ?"にヘルプコマンドを入れ替える
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key global-map (kbd "C-x ?") 'help-command)

;;
;; フレーム関連
;;

;; カラム番号も表示
(column-number-mode t)
;; ファイルサイズを表示
(size-indication-mode t)
;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")
;; 行番号を左側に常に表示p
(global-linum-mode t)

;;
;; インデント関連
;;

;; TAB幅
(setq-default tab-width 4)
;; インデントにTAB文字を使用しない
(setq-default indent-tabs-mode nil)

;;
;; フォント関連
;;

;; AsciiフォントをRictyに設定
(set-face-attribute 'default nil
                    :family "Ricty"
                    :height 120)

;; 日本語フォントをRictyに設定
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))

;; Notoフォントの横幅を調整
(add-to-list 'face-font-rescale-alist '("Ricty" . 1.1))

;; 現在行のハイライト
(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color)	(background dark))
      (:background "NavyBlue" t))
     ;;背景がlightならば背景色を青に
     (((class color) (background light))
      (:background "LightSkyBule" t))
     (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; paren-mode：対応する括弧を強調して表示する
(setq show-paren-delay 0.1) ;
(show-paren-mode t)
;; parenのスタイル：expressionは括弧ないも強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-under-p 'show-paren-match-face "darkgreen")

