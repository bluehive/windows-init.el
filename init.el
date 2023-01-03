;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; My init.el.
;; byte compile ;;;;;;;;;;;;;; emacs --batch -f batch-byte-compile init.el
;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el

;; GC      
;;  N       e          leaf                    
;; https://github.com/ncaq/.emacs.d/blob/master/init.el
(setq gc-cons-threshold 200000000)            ; 200MB
(run-with-idle-timer 120 t #'garbage-collect) ; 2     A C h                 I   K x [ W R   N g       o  

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
;;;;                   
;;;;   https://gist.github.com/noqisofon/749270/321bd6caf77c8a170300b61a083a9fb95b8022df
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
;; (cond (*run-meadow*
;;        (append-to-list load-path '("c:/usr/editor/meadow/site-lisp/apel"
;;                                    "c:/usr/editor/meadow/site-lisp/emu")))
;;       (*run-macosx*
;;        (append-to-list load-path '("/opt/local/share/emacs/site-lisp/apel"
;;                                 "/opt/local/share/emacs/23.2/site-lisp/"))))
;; (append-to-list load-path '("~/.emacs.d/conf"
;;                             "~/.emacs.d/site-lisp"
;;                             "~/.emacs.d/vendor"
;;                             "~/.emacs.d/vendor/elscreen"
;;                             "~/.emacs.d/vendor/egg"
;;                             "~/.emacs.d/vendor/textmate"
;;                             "~/.emacs.d/vendor/ruby"
;;                             "~/.emacs.d/vendor/yasnippet"))


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
;; Highlights parentheses(括弧のハイライト)
;;
;; ===============================================================================
;; 対応するカッコをハイライト表示します。
(show-paren-mode 1)


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
;; Miscellaneous Settings(雑多な設定)
;;
;; ===============================================================================


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
;; (eval-and-compile
;;   (leaf bytecomp
;;     :doc "compilation of Lisp code into byte code"
;;     :tag "builtin" "lisp"
;;     :custom (byte-compile-warnings . '(cl-functions))))


;;Emacsの外でファイルが書き変わったときに自動的に読み直すマイナーモードです。 もちろん、Emacsで編集している場合は外の変更で上書きされることはありません。
;; (leaf autorevert
;;   :doc "revert buffers when files on disk change"
;;   :tag "builtin"
;;   :custom ((auto-revert-interval . 0.3)
;;            (auto-revert-check-vc-info . t))
;;   :global-minor-mode global-auto-revert-mode)


;;delsel
;;選択している状態で入力したときに、regionを削除して挿入するマイナーモードです。 おそらくこの挙動のほうが現代人の意図に合っていると思います。

;; (leaf delsel
;;   :doc "delete selection if you insert"
;;   :tag "builtin"
;;   :global-minor-mode delete-selection-mode)

;; local init.el ends here
