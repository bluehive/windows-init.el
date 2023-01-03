;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) bluehive@github.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; My init.el.
;; byte compile ;;;;;;;;;;;;;; emacs --batch -f batch-byte-compile init.el
;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el

;; GC�̐ݒ�
;; �N���ɂ��e������̂�leaf�����ōŏ��ɂ��܂�
;; https://github.com/ncaq/.emacs.d/blob/master/init.el
(setq gc-cons-threshold 200000000)            ; 200MB
(run-with-idle-timer 120 t #'garbage-collect) ; 2���̃A�C�h�����Ԃ��Ƃɖ����I�ɃK�x�[�W�R���N�g���Ăяo��

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
             ;;      ("melpa" . "http://melpa.milkbox.net/packages/")
                       ("melpa" . "https://melpa.org/packages/")
             ;;     ("melpa-stable" . "https://stable.melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  ;;
  
  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 以下、コピペ
;;;;　https://gist.github.com/noqisofon/749270/321bd6caf77c8a170300b61a083a9fb95b8022df
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; init.el
(setq debug-on-error t)

;; ===============================================================================
;;
;; Environmental discrimination(環境識別)
;;
;; ===============================================================================
(defvar *run-unix-like*
  (or (equal system-type 'gnu/linux)
      (equal system-type 'darwin)
      (equal system-type 'usg-unix-v)))
(defvar *run-windows*
  (and (null *run-unix-like*)
       (or (equal system-type 'windows-nt)
           (equal system-type 'ms-dos))))
(defvar *run-macosx*
  (and *run-unix-like*
       (null *run-windows*)
       (and (equal window-system 'ns)
            (equal system-type 'darwin))))
(defvar *run-win32*
  (and (null *run-unix-like*)
       *run-windows*
       (equal system-type 'windows-nt)))
(defvar *run-x-window-system*
  (and *run-unix-like* (eq window-system 'x)))
(defvar *run-emacs20*
  (and (equal emacs-major-version 20)
       (null (featurep 'xemacs))))
(defvar *run-emacs21*
  (and (equal emacs-major-version 21)
       (null (featurep 'xemacs))))
(defvar *run-emacs22*
  (and (equal emacs-major-version 22)
       (null (featurep 'xemacs))))
(defvar *run-emacs23*
  (and (equal emacs-major-version 23)
       (null (featurep 'xemacs))))
(defvar *run-meadow* (featurep 'meadow))
(defvar *run-meadow1* (and *run-meadow* *run-emacs20*))
(defvar *run-meadow2* (and *run-meadow* *run-emacs21*))
(defvar *run-meadow3* (and *run-meadow* *run-emacs22*))
(defvar *run-xemacs* (featurep 'xemacs))
(defvar *run-xemacs-no-mule*
  (and *run-xemacs* (not (featurep 'mule))))


;; ===============================================================================
;;
;; Macros(マクロ)
;;
;; ===============================================================================
;; (via: "http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html")
;; lambda を書かずに、/'.*/ の次の引数に関数をズラズラ書けます。
(defmacro add-hook-fn (name &rest body)
  `(add-hook ,name #'(lambda () ,@body)))

;; lambda を書かずに関数をそのまま書くことができる global-set-key です。
(defmacro global-set-key-fn (key args &rest body)
  `(global-set-key ,key (lambda ,args ,@body)))

;; append して setq します。
(defmacro append-to-list (to list)
  `(setq ,to (append ,list ,to)))

;; ライブラリがあったら require します。
(defmacro require-if-exists (library &rest body)
  `(when (locate-library ,(symbol-name library))
     (require ',library) ,@body))

;; 遅延ロードします。
(defmacro lazyload (func library-name &rest body)
  `(when (locate-library ,library-name)
     ,@(mapcar (lambda (f) `(autoload ',f ,library-name nil t)) func)
     (eval-after-load ,library-name
                      '(progn
                        ,@body))))


;; ===============================================================================
;;
;; User script directory(ユーザースクリプト用ディレクトリ)
;;
;; ===============================================================================
(cond (*run-meadow*
       (append-to-list load-path '("c:/usr/editor/meadow/site-lisp/apel"
                                   "c:/usr/editor/meadow/site-lisp/emu")))
      (*run-macosx*
       (append-to-list load-path '("/opt/local/share/emacs/site-lisp/apel"
                                "/opt/local/share/emacs/23.2/site-lisp/"))))
(append-to-list load-path '("~/.emacs.d/conf"
                            "~/.emacs.d/site-lisp"
                            "~/.emacs.d/vendor"
                            "~/.emacs.d/vendor/elscreen"
                            "~/.emacs.d/vendor/egg"
                            "~/.emacs.d/vendor/textmate"
                            "~/.emacs.d/vendor/ruby"
                            "~/.emacs.d/vendor/yasnippet"))


;; ===============================================================================
;;
;; i18n(国際化と地域化)
;;
;; ===============================================================================
;; 日本語環境を指定します。
(set-language-environment "Japanese")

;; メニューを日本語化します。
;; 文字化けする場合は、menu-tree.elをロードする前に menu-tree-coding-system に適当な coding-system をセットします。
(if (and (= emacs-major-version 22)
         (equal window-system 'x))
    (setq menu-tree-coding-system 'utf-8))
(require-if-exists menu-tree)
;; 「utf-8」の部分は各自の環境に合わせて設定してください。
;(require 'menu-tree nil t)


;; ===============================================================================
;;
;; Charset Encoding(文字エンコーディング)
;;
;; ===============================================================================
(if *run-windows*
    (progn
      (if *run-meadow*
          (set-w32-system-coding-system 'sjis))
      (if (fboundp 'set-default-coding-sytem)
          (set-default-coding-sytem 'sjis))
      (set-terminal-coding-system 'sjis)
      (set-clipboard-coding-system 'sjis-dos)
      (set-keyboard-coding-system 'sjis)
      (prefer-coding-system 'sjis))
  ;; else
  (progn
    (if (fboundp 'set-default-coding-sytem)
        (set-default-coding-sytem 'utf-8))
    (set-terminal-coding-system 'utf-8)
    (set-clipboard-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)))


;; ===============================================================================
;;
;; IME configuration(IME 設定)
;;
;; ===============================================================================
(when *run-meadow*
  ;; IME のモードごとにカーソル色を変えます。
  (add-hook-fn 'mw32-ime-on-hook
               nil
               (set-cursor-height 4)
               (set-cursor-color "maroon"))
  (add-hook-fn 'mw32-ime-off-hook
               nil
               (set-cursor-height 4)
               (set-cursor-color "black"))
  ;; IME を初期化します。
  (mw32-ime-initialize)
  (setq default-input-method "MW32-IME")
  ;; IME ON/OFF mode-line
  (setq mw32-ime-show-mode-line t)
  ;; IME mode-line indicator
  ;; OFF : [--]
  ;; ON  : [あ]
  (setq-default mw32-ime-mode-line-state-indicator "[--]")
  (setq mw32-ime-mode-line-state-indicator "[--]")
  (setq mw32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))

  ;; IMEの制御(yes/noをタイプするところでは IME を off にします)
  (wrap-function-to-control-ime 'universal-argument t nil)
  (wrap-function-to-control-ime 'read-string nil nil)
  (wrap-function-to-control-ime 'read-char nil nil)
  (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
  (wrap-function-to-control-ime 'y-or-n-p nil nil)
  (wrap-function-to-control-ime 'yes-or-no-p nil nil)
  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)

  (eval-after-load "ange-ftp"
                   '(wrap-function-to-control-ime 'ange-ftp-get-passwd nil nil)))


;; ===============================================================================
;;
;; Fonts(フォント)
;;
;; ===============================================================================
;; * フォントの設定
;;
;; (when *run-windows*
;;   ;; フォントセットを追加します。
;;   (load "ms-gothic-13.el")

;;   ;; 起動時およびnew-frame時のフレーム(ウィンドウ)の設定。
;;   (add-to-list 'default-frame-alist '(font . "MS Gothic 13"))
;;   ;; 現在のフレームの設定(.emacs中では省略可)
;;   (set-frame-font "MS Gothic 13")

;;   ;; IMEのフォントを設定。こういう書式でないとだめらしい。
;;   (let ((logfont '(w32-logfont "MS Gothic" 0 0 400 0 nil nil nil 128 1 3 0)))
;;     (modify-frame-parameters (selected-frame) (list (cons 'ime-font logfont)))
;;     (add-to-list 'default-frame-alist (cons 'ime-font logfont)))

;;   (set-face-font 'mode-line "MS Gothic 13")
;;   (set-face-font 'mode-line-inactive "MS Gothic 13"))


;; ===============================================================================
;;
;; Display theme(ディスプレイテーマ)
;;
;; ===============================================================================

;; (when window-system
;;   ;; 選択範囲に色をつけます。
;;   (setq-default transient-mark-mode t)
;;   ;; 選択範囲の色を指定します。
;;   (set-face-background 'region "SkyBlue")
;;   (set-face-foreground 'region "black")

;;   ;; カーソルを縦棒にします。
;;   (if (fboundp 'set-cursor-type)
;;       (set-cursor-type 'hairline-caret)
;;     ;; else
;;     (if *run-meadow3*
;;         (add-to-list 'default-frame-alist '(cursor-type . hairline-caret))
;;       ;; else
;;       (add-to-list 'default-frame-alist '(cursor-type . '(bar . 5)))))
;;   ;; カーソルの色を黒にします。
;;   (set-cursor-color "black")

;;   ;; フレームのアルファ値です。
;;   (setq frame-alpha 85)

;;   ;;
;;   ;; ** フレーム透過設定
;;   (if *run-meadow3*
;;       ;; カレントウィンドウ(ウィンドウ全体？)の透明度です。
;;       (set-frame-parameter nil 'alpha frame-alpha)
;;     ;; デフォルトの透明度です。
;;     (add-to-list 'default-frame-alist '(alpha . frame-alpha)))

;;   ;; 一行あたりの文字数は 170 です。
;;   (add-to-list 'default-frame-alist '(width . 170))
;;   ;; 20 行を表示します。
;;   (add-to-list 'default-frame-alist '(height . 20))
;;   ;; ディスプレイのX座標(ピクセル)
;;   (add-to-list 'default-frame-alist '(top . 5))
;;   ;; ディスプレイのY座標(ピクセル)
;;   (add-to-list 'default-frame-alist '(left . 5))

;;   ;; 背景色を白にします。
;;   (add-to-list 'default-frame-alist '(background-color . "white"))
;;   ;; 文字の色を黒にします。
;;   (add-to-list 'default-frame-alist '(foreground-color . "black"))

;;   ;; モードラインの文字色。
;;   (set-face-foreground 'mode-line "white")
;;   ;; モードラインの背景色。
;;   (set-face-background 'mode-line "black")

;;   ;;
;;   ;; *** アクティブでないモードラインの文字、背景色
;;   ;; Meadow 2 では定義されていないようです。
;;   ;;
;;   (when (not *run-meadow2*)
;;     ;; アクティブでないモードラインの文字色。
;;     (set-face-foreground 'mode-line-inactive "gray30")
;;     ;; アクティブでないモードラインの背景色。
;;     (set-face-background 'mode-line-inactive "gray85")))



;; ===============================================================================
;;
;; Highlights(ハイライト)
;;
;; ===============================================================================
(when window-system
  ;; キーワードのカラー表示を有効化します。
  (global-font-lock-mode t)

  ;; コメント
  (set-face-foreground 'font-lock-comment-face "gray55")
  ;; キーワード
  (set-face-foreground 'font-lock-keyword-face "green4")
  ;; キーワードを太文字にします。
  (make-face-bold 'font-lock-keyword-face)
  ;; 関数名
  (set-face-foreground 'font-lock-function-name-face "royal blue")
  ;; 関数名を太文字にします。
  (make-face-bold 'font-lock-function-name-face)
  ;; 変数
  (set-face-foreground 'font-lock-variable-name-face "gray29")
  ;; 文字列
  (set-face-foreground 'font-lock-string-face "blue")
  ;; 定数
  (set-face-foreground 'font-lock-constant-face "forest green")
  ;; 変数の型
  (set-face-foreground 'font-lock-type-face "dark olive green")
  ;; シンボル
  (set-face-foreground 'font-lock-builtin-face "MediumPurple3")
  ;; シンボルを太文字にします。
  (make-face-bold 'font-lock-builtin-face)
  ;; わーにんぐふぇいす？
  (set-face-foreground 'font-lock-warning-face "LightSteelBlue4")

  ;; ハイライトの文字色。
  (set-face-foreground 'highlight "gray88")
  ;; ハイライトの背景色。
  (set-face-background 'highlight "white"))


;; ===============================================================================
;;
;; Highlights parentheses(括弧のハイライト)
;;
;; ===============================================================================
;; 対応するカッコをハイライト表示します。
(show-paren-mode 1)

;(require 'paren)
;(set-face-background 'show-paren-match-face (face-background 'default))
;;(set-face-background 'show-paren-match-face "gray85")
;(set-face-foreground 'show-paren-match-face "#def")
;(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)


;; ===============================================================================
;;
;; Setting the title bar and toolbar(タイトルバーとツールバーの設定)
;;
;; ===============================================================================
;; 現在選択中のバッファがファイルからできているかどうか判別します。
(defun is-selected-buffer-from-file ()
  "現在選択中のバッファがファイルからできているかどうか判別します。"
  (let ((selected-buffer-filename (buffer-file-name))
        (selected-buffer-name (buffer-name)))
    (if selected-buffer-filename t nil)))

(when *run-meadow*
  ;; タイトルキャプションを更新します。
  (defun update-title-caption ()
    "タイトルキャプションを更新します。"
    (setq about-meadow (let* ((meadow-about-text (car (split-string (Meadow-version) " ")))
                              (first-hyphen (string-match "-" meadow-about-text)))
                         (store-substring meadow-about-text first-hyphen " ")))
    (setq frame-title-format
          (list (if (is-selected-buffer-from-file) "%f" "%b") " - " about-meadow "@" (system-name))))
  ;; とりあえずタイトル更新。
  (update-title-caption))
;;; %f だと、フルパス名。 %b ならバッファの名前。

;; 現在は meadow 用に update-title-caption 関数を定義していますが、
;; 他の Emacen では定義していないので、ガード句でくるんでいます。
;; meadow 以外でも同じようにしたい場合は 同じ名前の関数を定義してください。
(if (fboundp 'update-title-caption)
    ;; switch-to-buffer の後に frame-title-format の値を更新します。
    (defadvice switch-to-buffer
               (after switch-to-buffer-after-update-the-title-captions first () activate)
               (update-title-caption)))


;; ===============================================================================
;;
;; Highlight cursor line(カーソル行ハイライト)
;;
;; ===============================================================================
;; カーソルのある行をハイライトします。
(setq hl-line-face 'underline)
(global-hl-line-mode)

(defface hlline-face
         '((((class color)
             (background dark))
            (:background "blue" :foreground "white"))
           (((class color)
             (background light))
            (:background "AliceBlue"))
           (t
            ()))
         "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)


;; ===============================================================================
;;
;; Scrolling(スクロール)
;;
;; ===============================================================================
;; ウィンドウの一番下にカーソルがある場合、そこから下に移動したときに何行スクロールするかの設定です。
;; 以下は 15 行の場合。
;; line-setting
(setq next-line-add-newlines nil)
(when (not next-line-add-newlines)
  (line-number-mode 15)
  (column-number-mode 15))

;; スクロール行単位を 1 行にします。
(setq scroll-step 1)
;; 画面をはみ出す場合に 1 行だけスクロールするようにします。
(setq scroll-conservatively 1)
;; カーソルを一番上か一番下まで持っていけます。
(setq scroll-margin 0)


;; ===============================================================================
;;
;; Fringe(フリンジ)
;;
;; ===============================================================================
;; 左フリンジの上下にマークをつける
(setq-default indicate-buffer-boundaries 'left)
;; 右フリンジの上下にマークをつける
(setq-default indicate-buffer-boundaries 'right)

;; 左フリンジの上と右フリンジの下にマークをつける
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))
;; 右フリンジの上と左フリンジの下にマークをつける
(setq-default indicate-buffer-boundaries '((top . right) (t . left)))
;; 右フリンジの上にのみマークをつける
(setq-default indicate-buffer-boundaries '((top . right) (t . nil)))


;; ===============================================================================
;;
;; Key binds(キー割り当て)
;;
;; ===============================================================================
;; i-search for japanese
(define-key isearch-mode-map "\C-k" 'isearch-edit-string)

;; M-g で指定行にカーソルを飛ばします。
(global-set-key "\M-g"
                '(lambda (x)
                  (interactive "Line to goto: ")
                  (goto-line x)))

;; *** comment/uncomment-regeon
;; C-x ; でコメントアウト
;; C-x : でコメントをはずす
;; (global-set-key "\C-x;" 'comment-region)
;; (fset 'uncomment-region "\C-u\C-[xcomment-region\C-m")
;; (global-set-key "\C-x:" 'uncomment-region)

;; 自動インデントを行ないます。
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
;; 最近使ったファイルを別のバッファ領域に表示します。
(define-key global-map "\C-cr" 'recentf-open-files)


;; ===============================================================================
;;
;; Useful functions(便利関数)
;;
;; ===============================================================================
;; ** [EOF] を表示します。
;;
(if *run-meadow1*
    (progn
      ;; Meadow 1.99 なら以下のようにすると同じことができるようです。
      (defun set-buffer-end-mark1()
        "At the end of the buffer [EOF] padding. This string can not be edited to be reflected in the file."
        (let ((overlay (make-overlay (point-max) (point-max))))
          (overlay-put overlay 'before-string #("[EOF]" 0 5 (face highlight)))
          (overlay-put overlay 'insert-behind-hooks
                       '((lambda (overlay after beg end &optional len)
                           (when after
                             (move-overlay overlay (point-max) (point-max))))))))
      (add-hook 'find-file-hooks 'set-buffer-end-mark1))
  ;; else
  (progn
    ;; あるいは
    (defun set-buffer-end-mark2()
      "At the end of the buffer [EOF] padding. This string can not be edited to be reflected in the file."
      (let ((overlay (make-overlay (point-max) (point-max))))
        (overlay-put overlay 'before-string #("[EOF]" 0 5 (face highlight)))
        (overlay-put overlay 'insert-behind-hooks
                     '((lambda (overlay after beg end &optional len)
                         (when after
                           (move-overlay overlay (point-max) (point-max))))))))
    (add-hook 'find-file-hooks 'set-buffer-end-mark2)))
;; でもいい。


;; ** スクリプトを保存する時，自動的に chmod +x を行なうようにする
;;
;; 次のようなコードを ~/.emacs に加えておくと、
;; + ファイルの先頭に #! で始まる行が含まれている
;; + ファイル名の先頭がピリオド以外
;; の場合，実行用のスクリプトファイルであると見なして、保存時に実行許可属性を自動的に設定します。
(defun make-file-executable ()
  "Make the file of this buffer executable, when it is a script source."
  (save-restriction
    (widen)
    (if (string= "#!" (buffer-substring-no-properties 1 (min 3 (point-max))))
        (let ((name (buffer-file-name)))
          (or (equal ?. (string-to-char (file-name-nondirectory name)))
              (let ((mode (file-modes name)))
                (set-file-modes name (logior mode (logand (/ mode 4) 73)))
                (message (concat "Wrote " name " (+x)"))))))))
(add-hook 'after-save-hook 'make-file-executable)


;; 日付を挿入します。
(defun my-insert-time ()
  "Insert the date the current cursor location."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))


;; 現在選択中のバッファのファイル名、あるいはバッファ名を返します。
(defun get-selected-buffer-name ()
  "現在選択中のバッファのファイル名、あるいはバッファ名を返します。"
  (let ((selected-buffer-filename (buffer-file-name ))
        (selected-buffer-name (buffer-name)))
    (if (not selected-buffer-filename)
        selected-buffer-name
      ;; else
      selected-buffer-filename)))


;; テンポラリバッファを作成し、それをウィンドウに表示します。
(defun create-temporary-buffer ()
  "テンポラリバッファを作成し、それをウィンドウに表示します。"
  (interactive)
  ;; *temp* なバッファを作成し、それをウィンドウに表示します。
  (switch-to-buffer (generate-new-buffer "*temp*"))
  ;; セーブが必要ないことを指定します？
  (setq buffer-offer-save nil))
;; C-c t でテンポラリバッファを作成します。
(global-set-key "\C-ct" 'create-temporary-buffer)


;; ===============================================================================
;;
;; SHELL(M-x shell のときの設定)
;;
;; ===============================================================================
(when *run-windows*
  ;; MSYS の bash を使用します。
  (setq explicit-shell-file-name "bash.exe")
  (setq shell-file-name "sh.exe")

  ;; SHELL で ^M が付く場合は ^M を削除します。
  (add-hook 'shell-mode-hook
            (lambda ()
              (set-buffer-process-coding-system 'undecided-dos 'sjis-unix)))
  ;; shell-mode での保管(for drive letter)
  (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")

  ;; shell-modeで上下で補完しモード。
  (setq shell-mode-hook
        (function (lambda ()
                    (define-key shell-mode-map [up] 'comint-previous-input)
                    (define-key shell-mode-map [down]
                                'comint-next-input)))))


;; ===============================================================================
;;
;; Language mode(各言語モード)
;;
;; ===============================================================================
;; 通常のインデントで半角スペースを使います。
(setq-default indent-tabs-mode nil)
;; Ruby
;(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(lazyload (ruby-mode) "ruby-mode"
          (setq auto-mode-alist
                (append '(("\\.rb$" . ruby-mode))
                        auto-mode-alist))
          (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                               interpreter-mode-alist)))
;; ECMA Scrirpt
(require-if-exists ecmascript-mode)

(defun my-c/c++-mode ()
  ;; cc-mode スタイルにします。
  (c-set-style "cc-mode")
  ;; インデントは空白文字で行ないます。
  (setq indent-tabs-mode nil)
  ;; `;' を押すと自動で改行されるようにします。
  (setq c-auto-newline nil)
  ;; タブキーでインデントを行います。
  (setq c-tab-always-indent t)
  ;; タブ幅を 4 にします。
  (setq tab-width 4)
  ;; 標準オフセットを tab-width と同じにします。
  (setq c++-basic-offset tab-width)
  ;; インデントを tab-width と同じにします。
  (setq c-indent-level tab-width)
  ;; コメントだけの行は 0 にします。
  (setq c-comment-only-line-offset 0)

  ;; 引数リストの閉じ括弧もインデントします。
  (c-set-offset 'arglist-close 0)
  ;; public などのアクセス修飾子は 1 インデントします。
  (c-set-offset 'access-label 1)
  ;; switch 構文のラベルは tab-width だけインデントします。
  (c-set-offset 'case-label tab-width))

;; C++ モード。
(add-hook 'c++-mode-hook 'my-c/c++-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Windows 固有ファイル用のモードを追加します。
;;
(when *run-meadow*
  (require-if-exists generic-x)

  (if (and (fboundp 'bat-generic-mode)
           (fboundp 'ini-generic-mode))
      ;; が、Meadow3 ではこれらのモードが無いため、出来ませんでした。
      ;; 関数用のには fboundp を使います。
      (append-to-list auto-mode-alist '(("\\.bat$" . bat-generic-mode)
                                        ("\\.ini$ ." ini-generic-mode)))
    ;; else
     (progn
       (require-if-exists any-ini-mode)
       (append-to-list auto-mode-alist '((".*\\.ini$" . any-ini-mode)
                                         (".*\\.conf$" . any-ini-mode)))

       (require-if-exists bat-mode)
       ;; bat-mode
       (append-to-list auto-mode-alist '(("\\.[Bb][Aa][Tt]$" . 'bat-mode)
                                         ;; For DOS init files
                                         ("CONFIG\\." . 'bat-mode)
                                         ("AUTOEXEC\\." . 'bat-mode)))

       ;(autoload 'bat-mode "bat-mode" "DOS and Windows BAT files" t)
       (lazyload (bat-mode) "bat-mode")))

  ;; visual basic もーど。
  ;(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
  (lazyload (visual-basic-mode) "visual-basic-mode")
  (append-to-list auto-mode-alist '(("\\.\\(frm\\|bas\\|vb\\|cls\\)$" . visual-basic-mode))))

;; C# モード。
;; (require-if-exists csharp-mode)
;; (setq auto-mode-alist
;;       (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;; ;; C# モード。
;; (add-hook 'csharp-mode-hook 'my-c/c++-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C# support
;;
;(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(lazyload (csharp-mode) "csharp-mode")
(append-to-list auto-mode-alist '(("\\.cs$" . csharp-mode)))
(add-hook 'csharp-mode-hook 'my-c/c++-mode)


;; ===============================================================================
;;
;; Line number(行番号)
;;
;; ===============================================================================
;;; 行番号・桁番号をモードラインに表示する・しない設定
(line-number-mode t)   ;; 行番号。t なら表示、nil なら非表示
(column-number-mode t) ;; 桁番号。t なら表示、nil なら非表示
;;
;; ** 行番号をデフォルトで表示します。
;;
(if (>= emacs-major-version 22)
    ;; 23 以上から linum.el が入っているので要りませんが、Meadow3 は 22 なので
    ;; require-if-exists が必要です。
    (require-if-exists linum))
(require-if-exists linum+)
;; デフォルトで linum-mode を有効にします。
(global-linum-mode t)
;; 5 桁分の領域を確保して行番号を入れます。
(setq linum-format "%7d")


;; ===============================================================================
;;
;; Time stamp(ファイル更新日)
;;
;; ===============================================================================
(require-if-exists time-stamp)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-start "@date ")
(setq time-stamp-format "%04y-%02m-%02d")
(setq time-stamp-end " \\|$")


;; ===============================================================================
;;
;; Toolbar(ツールバー)
;;
;; ===============================================================================
;; ツールバーを表示しないようにします。
(tool-bar-mode -1)
(require-if-exists tool-bar+)


;; ===============================================================================
;;
;; Mode line Date display(モードラインの時刻表示)
;;
;; ===============================================================================
;; 時刻のフォーマット。
(setq display-time-string-forms
      '((let ((system-time-locale "C"))
          (format-time-string "%Y-%m-%dT%H:%M"))))
;; モードラインに現在時刻を表示します。
(if *run-meadow*
    (display-time)
  ;; else
  (display-time-mode 1))


;; ===============================================================================
;;
;; *-selection
;;
;; ===============================================================================
;; 選択領域を入力で置き換えるようにします。
(delete-selection-mode 1)
;; S-[←→]で範囲を選択できるようになります。
;; (if *run-meadow3*
;;     (pc-selection-mode)
;;   (pc-selection-mode 1))

;; ===============================================================================
;;
;; egg
;;
;; ===============================================================================
(require-if-exists egg)


;; ===============================================================================
;;
;; Textmate
;;
;; ===============================================================================
;; Textmate モードです。
(require-if-exists textmate
                   (textmate-mode))


;; ===============================================================================
;;
;; Zen Coding
;;
;; ===============================================================================
(require-if-exists zencoding-mode
                   (add-hook 'sgml-mode-hook 'zencoding-mode)
                   (add-hook 'text-mode-hook 'zencoding-mode))


;; ===============================================================================
;;
;; YASnippet
;;
;; ===============================================================================
(require-if-exists yasnippet ;; not yasnippet-bundle
                   (yas/initialize)
                   (yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets"))


;; ===============================================================================
;;
;; ELScreen
;;
;; ===============================================================================
(require-if-exists elscreen)


;; ===============================================================================
;;
;; Gauche
;;
;; ===============================================================================
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "scheme-mode" "Major mode for Shceme." t)
(autoload 'run-scheme "run-scheme" "Run an inferior Scheme process." t)


;; ===============================================================================
;;
;; Backup file(バックアップファイル)
;;
;; ===============================================================================
;; オートセーブを有効にします。
(auto-save-mode t)
;; バックアップファイルを作ります。
(setq backup-inhibited nil)

;; 簡易バージョンコントロール機能を有効にします。
(setq version-control t)

(when version-control
  ;; 新しいものを 6 つまで残すようにします。
  (setq kept-new-versions 6)
  ;; 古いものを 6 つまで残すようにします。
  (setq kept-old-versions 6)
  ;; 古いバージョンを消す際に尋ねないようにします。
  (setq delete-old-versions t))

;; 終了時にオートセーブファイルが削除されません。
(setq delete-auto-save-files nil)


;; ===============================================================================
;;
;; Miscellaneous Settings(雑多な設定)
;;
;; ===============================================================================
;; カレントディレクトリをホームディレクトリに設定
;; ""内は任意のディレクトリを指定可能
(cd "~/")

;; 起動時のメッセージを表示しません。
(setq inhibit-startup-message t)
;;; エラー時に画面がフラッシュするようにします。
;(setq visible-bell t)
(setq ring-bell-function 'ignore)
;;;; フラッシュするのがうざいのでしないようにしました。

;; 最近使ったファイルの一覧を表示します。
(recentf-mode t)
;; メニューに表示するファイル名を 10 までにします。
(setq recentf-max-menu-items 10)
;; 最大 15 ファイルまで記録します。
(setq recentf-max-saved-items 15)

;; 他のプロセスによって変更されたとき、バッファをリロードします。
(global-auto-revert-mode t)

(if (fboundp 'server-start)
    ;; emacsclient を使います。
    (server-start))

;; auto-fill モードを使用します。
(setq-default auto-fill-function 'do-auto-fill)

;; キー入力を記憶しておきます。
(open-dribble-file "~/.emacs_dribble")

;; 70 字目で自動的に改行されないようにします。
;; これでも駄目なときは M-x set-fill-column RET 300 を実行してください。
(setq-default fill-column 300)

;; タブ文字による空白を 4 つにします。
(setq-default tab-width 4)

;; 空白だけで字下げを行うようにします。
(setq-default indent-tabs-mode nil)

;; X システムのクリップボードとキルリングを共有できるようにします。
;; (if *run-x-window-system*
;;     (setq x-select-enable-clipboard))



;; ===================================================================================================

;;;;;;;;;;;;;;;;;;;;;;
;; Colors and Fonts ;;
;; https://github.com/cdepillabout/docs/blob/53086c3cd34db01d001997e24d79ad9e0ec4cc8e/dot_files/dot_emacs
;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'manoj-dark)

;; Set the default font.
(set-face-attribute 'default nil
  :family "Source Code Pro"
  :height 140
  :weight 'normal
  :width 'normal)

;; Set the cursor color to red to match Vim in the terminal.
(set-cursor-color "red")

;; Set the EOL whitespace to be colored in white.
(set-face-attribute 'trailing-whitespace nil
   :background "white")

;; Set the default font for Japanese characters.
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "IPAPGothic"))

;; Style the tab-bar so it looks like my Vim tab-bar.
;; (set-face-attribute 'tab-bar nil
;;   :background "white"
;;   :foreground "black")
;; (set-face-attribute 'tab-bar-tab nil
;;   :background "deep sky blue"
;;   :foreground "white"
;;   :box 'nil
;;   :weight 'bold)
;; (set-face-attribute 'tab-bar-tab-inactive nil
;;   ;; :background "deep sky blue"
;;   :foreground "black"
;;   :box 'nil
;;   :weight 'normal
;;   )

(with-eval-after-load "org"
  (if (display-graphic-p)

    ;; faces to set if we are in the GUI
    (progn
      (set-face-attribute 'org-level-2 nil :foreground "dark goldenrod" :weight 'bold)
      (set-face-attribute 'org-level-3 nil :foreground "firebrick" :weight 'bold)
      (set-face-attribute 'org-special-keyword nil :foreground "light gray" :weight 'light)
      (set-face-attribute 'org-date nil :foreground "dark magenta" :underline nil :weight 'normal)
      (set-face-attribute 'org-tag nil :foreground "cornflower blue" :weight 'light)
    )

    ;; faces to set if we are in the CUI
    (set-face-attribute 'org-level-2 nil :foreground "color-116" :weight 'bold)
    (set-face-attribute 'org-level-3 nil :foreground "color-41" :weight 'bold)
    (set-face-attribute 'org-level-4 nil :weight 'bold)
    (set-face-attribute 'org-level-5 nil :weight 'bold)
    (set-face-attribute 'org-special-keyword nil :foreground "color-95" :weight 'light)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ここにいっぱい設定を書く
;; https://emacs-jp.github.io/tips/emacs-in-2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;leafの :custom で設定するとinit.elにcustomが勝手に設定を追記します。 この状況になると、変数の二重管理になってしまうので、customがinit.elに追記しないように設定します。
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

;;cus-start.c EmacsのC言語部分で定義されている変数をcustomで扱えるようにまとめているファイルです。 私の設定を書いておくので、取捨選択して頂ければと思います。変数の説明は F1 v で確認できます。

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '(;;(user-full-name . "Naoya Yamashita")
            ;;(user-mail-address . "conao3@gmail.com")
            ;;(user-login-name . "conao3")
            (create-lockfiles . nil)
            (debug-on-error . t)
            (init-file-debug . t)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 1000)
            (history-delete-duplicates . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (text-quoting-style . 'straight)
            (truncate-lines . t)
            ;; (use-dialog-box . nil)
            ;; (use-file-dialog . nil)
             (menu-bar-mode . t)
            ;; (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

;;
(eval-and-compile
  (leaf bytecomp
    :doc "compilation of Lisp code into byte code"
    :tag "builtin" "lisp"
    :custom (byte-compile-warnings . '(cl-functions))))


;;Emacsの外でファイルが書き変わったときに自動的に読み直すマイナーモードです。 もちろん、Emacsで編集している場合は外の変更で上書きされることはありません。
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.3)
           (auto-revert-check-vc-info . t))
  :global-minor-mode global-auto-revert-mode)


;;delsel
;;選択している状態で入力したときに、regionを削除して挿入するマイナーモードです。 おそらくこの挙動のほうが現代人の意図に合っていると思います。

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

;; local init.el ends here
