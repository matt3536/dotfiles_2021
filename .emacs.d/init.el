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
  (require 'magit nil t))

