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

;; GCの設定
;; 起動にも影響するのでleaf無しで最初にやります
;; https://github.com/ncaq/.emacs.d/blob/master/init.el
(setq gc-cons-threshold 200000000)            ; 200MB
(run-with-idle-timer 120 t #'garbage-collect) ; 2分のアイドル時間ごとに明示的にガベージコレクトを呼び出す

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
;;;; 莉･荳九�√さ繝斐��
;;;;縲�https://gist.github.com/noqisofon/749270/321bd6caf77c8a170300b61a083a9fb95b8022df
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; init.el
(setq debug-on-error t)

;; ===============================================================================
;;
;; Environmental discrimination(迺ｰ蠅�隴伜挨)
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
;; Macros(繝槭け繝ｭ)
;;
;; ===============================================================================
;; (via: "http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html")
;; lambda 繧呈嶌縺九★縺ｫ縲�/'.*/ 縺ｮ谺｡縺ｮ蠑墓焚縺ｫ髢｢謨ｰ繧偵ぜ繝ｩ繧ｺ繝ｩ譖ｸ縺代∪縺吶��
(defmacro add-hook-fn (name &rest body)
  `(add-hook ,name #'(lambda () ,@body)))

;; lambda 繧呈嶌縺九★縺ｫ髢｢謨ｰ繧偵◎縺ｮ縺ｾ縺ｾ譖ｸ縺上％縺ｨ縺後〒縺阪ｋ global-set-key 縺ｧ縺吶��
(defmacro global-set-key-fn (key args &rest body)
  `(global-set-key ,key (lambda ,args ,@body)))

;; append 縺励※ setq 縺励∪縺吶��
(defmacro append-to-list (to list)
  `(setq ,to (append ,list ,to)))

;; 繝ｩ繧､繝悶Λ繝ｪ縺後≠縺｣縺溘ｉ require 縺励∪縺吶��
(defmacro require-if-exists (library &rest body)
  `(when (locate-library ,(symbol-name library))
     (require ',library) ,@body))

;; 驕�蟒ｶ繝ｭ繝ｼ繝峨＠縺ｾ縺吶��
(defmacro lazyload (func library-name &rest body)
  `(when (locate-library ,library-name)
     ,@(mapcar (lambda (f) `(autoload ',f ,library-name nil t)) func)
     (eval-after-load ,library-name
                      '(progn
                        ,@body))))


;; ===============================================================================
;;
;; User script directory(繝ｦ繝ｼ繧ｶ繝ｼ繧ｹ繧ｯ繝ｪ繝励ヨ逕ｨ繝�繧｣繝ｬ繧ｯ繝医Μ)
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
;; i18n(蝗ｽ髫帛喧縺ｨ蝨ｰ蝓溷喧)
;;
;; ===============================================================================
;; 譌･譛ｬ隱樒腸蠅�繧呈欠螳壹＠縺ｾ縺吶��
(set-language-environment "Japanese")

;; 繝｡繝九Η繝ｼ繧呈律譛ｬ隱槫喧縺励∪縺吶��
;; 譁�蟄怜喧縺代☆繧句�ｴ蜷医�ｯ縲［enu-tree.el繧偵Ο繝ｼ繝峨☆繧句燕縺ｫ menu-tree-coding-system 縺ｫ驕ｩ蠖薙↑ coding-system 繧偵そ繝�繝医＠縺ｾ縺吶��
(if (and (= emacs-major-version 22)
         (equal window-system 'x))
    (setq menu-tree-coding-system 'utf-8))
(require-if-exists menu-tree)
;; 縲蛍tf-8縲阪�ｮ驛ｨ蛻�縺ｯ蜷�閾ｪ縺ｮ迺ｰ蠅�縺ｫ蜷医ｏ縺帙※險ｭ螳壹＠縺ｦ縺上□縺輔＞縲�
;(require 'menu-tree nil t)


;; ===============================================================================
;;
;; Charset Encoding(譁�蟄励お繝ｳ繧ｳ繝ｼ繝�繧｣繝ｳ繧ｰ)
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
;; IME configuration(IME 險ｭ螳�)
;;
;; ===============================================================================
(when *run-meadow*
  ;; IME 縺ｮ繝｢繝ｼ繝峨＃縺ｨ縺ｫ繧ｫ繝ｼ繧ｽ繝ｫ濶ｲ繧貞､峨∴縺ｾ縺吶��
  (add-hook-fn 'mw32-ime-on-hook
               nil
               (set-cursor-height 4)
               (set-cursor-color "maroon"))
  (add-hook-fn 'mw32-ime-off-hook
               nil
               (set-cursor-height 4)
               (set-cursor-color "black"))
  ;; IME 繧貞�晄悄蛹悶＠縺ｾ縺吶��
  (mw32-ime-initialize)
  (setq default-input-method "MW32-IME")
  ;; IME ON/OFF mode-line
  (setq mw32-ime-show-mode-line t)
  ;; IME mode-line indicator
  ;; OFF : [--]
  ;; ON  : [縺�]
  (setq-default mw32-ime-mode-line-state-indicator "[--]")
  (setq mw32-ime-mode-line-state-indicator "[--]")
  (setq mw32-ime-mode-line-state-indicator-list '("[--]" "[縺�]" "[--]"))

  ;; IME縺ｮ蛻ｶ蠕｡(yes/no繧偵ち繧､繝励☆繧九→縺薙ｍ縺ｧ縺ｯ IME 繧� off 縺ｫ縺励∪縺�)
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
;; Fonts(繝輔か繝ｳ繝�)
;;
;; ===============================================================================
;; * 繝輔か繝ｳ繝医�ｮ險ｭ螳�
;;
;; (when *run-windows*
;;   ;; 繝輔か繝ｳ繝医そ繝�繝医ｒ霑ｽ蜉�縺励∪縺吶��
;;   (load "ms-gothic-13.el")

;;   ;; 襍ｷ蜍墓凾縺翫ｈ縺ｳnew-frame譎ゅ�ｮ繝輔Ξ繝ｼ繝�(繧ｦ繧｣繝ｳ繝峨え)縺ｮ險ｭ螳壹��
;;   (add-to-list 'default-frame-alist '(font . "MS Gothic 13"))
;;   ;; 迴ｾ蝨ｨ縺ｮ繝輔Ξ繝ｼ繝�縺ｮ險ｭ螳�(.emacs荳ｭ縺ｧ縺ｯ逵∫払蜿ｯ)
;;   (set-frame-font "MS Gothic 13")

;;   ;; IME縺ｮ繝輔か繝ｳ繝医ｒ險ｭ螳壹�ゅ％縺�縺�縺�譖ｸ蠑上〒縺ｪ縺�縺ｨ縺�繧√ｉ縺励＞縲�
;;   (let ((logfont '(w32-logfont "MS Gothic" 0 0 400 0 nil nil nil 128 1 3 0)))
;;     (modify-frame-parameters (selected-frame) (list (cons 'ime-font logfont)))
;;     (add-to-list 'default-frame-alist (cons 'ime-font logfont)))

;;   (set-face-font 'mode-line "MS Gothic 13")
;;   (set-face-font 'mode-line-inactive "MS Gothic 13"))


;; ===============================================================================
;;
;; Display theme(繝�繧｣繧ｹ繝励Ξ繧､繝�繝ｼ繝�)
;;
;; ===============================================================================

;; (when window-system
;;   ;; 驕ｸ謚樒ｯ�蝗ｲ縺ｫ濶ｲ繧偵▽縺代∪縺吶��
;;   (setq-default transient-mark-mode t)
;;   ;; 驕ｸ謚樒ｯ�蝗ｲ縺ｮ濶ｲ繧呈欠螳壹＠縺ｾ縺吶��
;;   (set-face-background 'region "SkyBlue")
;;   (set-face-foreground 'region "black")

;;   ;; 繧ｫ繝ｼ繧ｽ繝ｫ繧堤ｸｦ譽偵↓縺励∪縺吶��
;;   (if (fboundp 'set-cursor-type)
;;       (set-cursor-type 'hairline-caret)
;;     ;; else
;;     (if *run-meadow3*
;;         (add-to-list 'default-frame-alist '(cursor-type . hairline-caret))
;;       ;; else
;;       (add-to-list 'default-frame-alist '(cursor-type . '(bar . 5)))))
;;   ;; 繧ｫ繝ｼ繧ｽ繝ｫ縺ｮ濶ｲ繧帝ｻ偵↓縺励∪縺吶��
;;   (set-cursor-color "black")

;;   ;; 繝輔Ξ繝ｼ繝�縺ｮ繧｢繝ｫ繝輔ぃ蛟､縺ｧ縺吶��
;;   (setq frame-alpha 85)

;;   ;;
;;   ;; ** 繝輔Ξ繝ｼ繝�騾城℃險ｭ螳�
;;   (if *run-meadow3*
;;       ;; 繧ｫ繝ｬ繝ｳ繝医え繧｣繝ｳ繝峨え(繧ｦ繧｣繝ｳ繝峨え蜈ｨ菴難ｼ�)縺ｮ騾乗�主ｺｦ縺ｧ縺吶��
;;       (set-frame-parameter nil 'alpha frame-alpha)
;;     ;; 繝�繝輔か繝ｫ繝医�ｮ騾乗�主ｺｦ縺ｧ縺吶��
;;     (add-to-list 'default-frame-alist '(alpha . frame-alpha)))

;;   ;; 荳�陦後≠縺溘ｊ縺ｮ譁�蟄玲焚縺ｯ 170 縺ｧ縺吶��
;;   (add-to-list 'default-frame-alist '(width . 170))
;;   ;; 20 陦後ｒ陦ｨ遉ｺ縺励∪縺吶��
;;   (add-to-list 'default-frame-alist '(height . 20))
;;   ;; 繝�繧｣繧ｹ繝励Ξ繧､縺ｮX蠎ｧ讓�(繝斐け繧ｻ繝ｫ)
;;   (add-to-list 'default-frame-alist '(top . 5))
;;   ;; 繝�繧｣繧ｹ繝励Ξ繧､縺ｮY蠎ｧ讓�(繝斐け繧ｻ繝ｫ)
;;   (add-to-list 'default-frame-alist '(left . 5))

;;   ;; 閭梧勹濶ｲ繧堤區縺ｫ縺励∪縺吶��
;;   (add-to-list 'default-frame-alist '(background-color . "white"))
;;   ;; 譁�蟄励�ｮ濶ｲ繧帝ｻ偵↓縺励∪縺吶��
;;   (add-to-list 'default-frame-alist '(foreground-color . "black"))

;;   ;; 繝｢繝ｼ繝峨Λ繧､繝ｳ縺ｮ譁�蟄苓牡縲�
;;   (set-face-foreground 'mode-line "white")
;;   ;; 繝｢繝ｼ繝峨Λ繧､繝ｳ縺ｮ閭梧勹濶ｲ縲�
;;   (set-face-background 'mode-line "black")

;;   ;;
;;   ;; *** 繧｢繧ｯ繝�繧｣繝悶〒縺ｪ縺�繝｢繝ｼ繝峨Λ繧､繝ｳ縺ｮ譁�蟄励�∬レ譎ｯ濶ｲ
;;   ;; Meadow 2 縺ｧ縺ｯ螳夂ｾｩ縺輔ｌ縺ｦ縺�縺ｪ縺�繧医≧縺ｧ縺吶��
;;   ;;
;;   (when (not *run-meadow2*)
;;     ;; 繧｢繧ｯ繝�繧｣繝悶〒縺ｪ縺�繝｢繝ｼ繝峨Λ繧､繝ｳ縺ｮ譁�蟄苓牡縲�
;;     (set-face-foreground 'mode-line-inactive "gray30")
;;     ;; 繧｢繧ｯ繝�繧｣繝悶〒縺ｪ縺�繝｢繝ｼ繝峨Λ繧､繝ｳ縺ｮ閭梧勹濶ｲ縲�
;;     (set-face-background 'mode-line-inactive "gray85")))



;; ===============================================================================
;;
;; Highlights(繝上う繝ｩ繧､繝�)
;;
;; ===============================================================================
(when window-system
  ;; 繧ｭ繝ｼ繝ｯ繝ｼ繝峨�ｮ繧ｫ繝ｩ繝ｼ陦ｨ遉ｺ繧呈怏蜉ｹ蛹悶＠縺ｾ縺吶��
  (global-font-lock-mode t)

  ;; 繧ｳ繝｡繝ｳ繝�
  (set-face-foreground 'font-lock-comment-face "gray55")
  ;; 繧ｭ繝ｼ繝ｯ繝ｼ繝�
  (set-face-foreground 'font-lock-keyword-face "green4")
  ;; 繧ｭ繝ｼ繝ｯ繝ｼ繝峨ｒ螟ｪ譁�蟄励↓縺励∪縺吶��
  (make-face-bold 'font-lock-keyword-face)
  ;; 髢｢謨ｰ蜷�
  (set-face-foreground 'font-lock-function-name-face "royal blue")
  ;; 髢｢謨ｰ蜷阪ｒ螟ｪ譁�蟄励↓縺励∪縺吶��
  (make-face-bold 'font-lock-function-name-face)
  ;; 螟画焚
  (set-face-foreground 'font-lock-variable-name-face "gray29")
  ;; 譁�蟄怜��
  (set-face-foreground 'font-lock-string-face "blue")
  ;; 螳壽焚
  (set-face-foreground 'font-lock-constant-face "forest green")
  ;; 螟画焚縺ｮ蝙�
  (set-face-foreground 'font-lock-type-face "dark olive green")
  ;; 繧ｷ繝ｳ繝懊Ν
  (set-face-foreground 'font-lock-builtin-face "MediumPurple3")
  ;; 繧ｷ繝ｳ繝懊Ν繧貞､ｪ譁�蟄励↓縺励∪縺吶��
  (make-face-bold 'font-lock-builtin-face)
  ;; 繧上�ｼ縺ｫ繧薙＄縺ｵ縺�縺�縺呻ｼ�
  (set-face-foreground 'font-lock-warning-face "LightSteelBlue4")

  ;; 繝上う繝ｩ繧､繝医�ｮ譁�蟄苓牡縲�
  (set-face-foreground 'highlight "gray88")
  ;; 繝上う繝ｩ繧､繝医�ｮ閭梧勹濶ｲ縲�
  (set-face-background 'highlight "white"))


;; ===============================================================================
;;
;; Highlights parentheses(諡ｬ蠑ｧ縺ｮ繝上う繝ｩ繧､繝�)
;;
;; ===============================================================================
;; 蟇ｾ蠢懊☆繧九き繝�繧ｳ繧偵ワ繧､繝ｩ繧､繝郁｡ｨ遉ｺ縺励∪縺吶��
(show-paren-mode 1)

;(require 'paren)
;(set-face-background 'show-paren-match-face (face-background 'default))
;;(set-face-background 'show-paren-match-face "gray85")
;(set-face-foreground 'show-paren-match-face "#def")
;(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)


;; ===============================================================================
;;
;; Setting the title bar and toolbar(繧ｿ繧､繝医Ν繝舌�ｼ縺ｨ繝�繝ｼ繝ｫ繝舌�ｼ縺ｮ險ｭ螳�)
;;
;; ===============================================================================
;; 迴ｾ蝨ｨ驕ｸ謚樔ｸｭ縺ｮ繝舌ャ繝輔ぃ縺後ヵ繧｡繧､繝ｫ縺九ｉ縺ｧ縺阪※縺�繧九°縺ｩ縺�縺句愛蛻･縺励∪縺吶��
(defun is-selected-buffer-from-file ()
  "迴ｾ蝨ｨ驕ｸ謚樔ｸｭ縺ｮ繝舌ャ繝輔ぃ縺後ヵ繧｡繧､繝ｫ縺九ｉ縺ｧ縺阪※縺�繧九°縺ｩ縺�縺句愛蛻･縺励∪縺吶��"
  (let ((selected-buffer-filename (buffer-file-name))
        (selected-buffer-name (buffer-name)))
    (if selected-buffer-filename t nil)))

(when *run-meadow*
  ;; 繧ｿ繧､繝医Ν繧ｭ繝｣繝励す繝ｧ繝ｳ繧呈峩譁ｰ縺励∪縺吶��
  (defun update-title-caption ()
    "繧ｿ繧､繝医Ν繧ｭ繝｣繝励す繝ｧ繝ｳ繧呈峩譁ｰ縺励∪縺吶��"
    (setq about-meadow (let* ((meadow-about-text (car (split-string (Meadow-version) " ")))
                              (first-hyphen (string-match "-" meadow-about-text)))
                         (store-substring meadow-about-text first-hyphen " ")))
    (setq frame-title-format
          (list (if (is-selected-buffer-from-file) "%f" "%b") " - " about-meadow "@" (system-name))))
  ;; 縺ｨ繧翫≠縺医★繧ｿ繧､繝医Ν譖ｴ譁ｰ縲�
  (update-title-caption))
;;; %f 縺�縺ｨ縲√ヵ繝ｫ繝代せ蜷阪�� %b 縺ｪ繧峨ヰ繝�繝輔ぃ縺ｮ蜷榊燕縲�

;; 迴ｾ蝨ｨ縺ｯ meadow 逕ｨ縺ｫ update-title-caption 髢｢謨ｰ繧貞ｮ夂ｾｩ縺励※縺�縺ｾ縺吶′縲�
;; 莉悶�ｮ Emacen 縺ｧ縺ｯ螳夂ｾｩ縺励※縺�縺ｪ縺�縺ｮ縺ｧ縲√ぎ繝ｼ繝牙唱縺ｧ縺上ｋ繧薙〒縺�縺ｾ縺吶��
;; meadow 莉･螟悶〒繧ょ酔縺倥ｈ縺�縺ｫ縺励◆縺�蝣ｴ蜷医�ｯ 蜷後§蜷榊燕縺ｮ髢｢謨ｰ繧貞ｮ夂ｾｩ縺励※縺上□縺輔＞縲�
(if (fboundp 'update-title-caption)
    ;; switch-to-buffer 縺ｮ蠕後↓ frame-title-format 縺ｮ蛟､繧呈峩譁ｰ縺励∪縺吶��
    (defadvice switch-to-buffer
               (after switch-to-buffer-after-update-the-title-captions first () activate)
               (update-title-caption)))


;; ===============================================================================
;;
;; Highlight cursor line(繧ｫ繝ｼ繧ｽ繝ｫ陦後ワ繧､繝ｩ繧､繝�)
;;
;; ===============================================================================
;; 繧ｫ繝ｼ繧ｽ繝ｫ縺ｮ縺ゅｋ陦後ｒ繝上う繝ｩ繧､繝医＠縺ｾ縺吶��
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
;; Scrolling(繧ｹ繧ｯ繝ｭ繝ｼ繝ｫ)
;;
;; ===============================================================================
;; 繧ｦ繧｣繝ｳ繝峨え縺ｮ荳�逡ｪ荳九↓繧ｫ繝ｼ繧ｽ繝ｫ縺後≠繧句�ｴ蜷医�√◎縺薙°繧我ｸ九↓遘ｻ蜍輔＠縺溘→縺阪↓菴戊｡後せ繧ｯ繝ｭ繝ｼ繝ｫ縺吶ｋ縺九�ｮ險ｭ螳壹〒縺吶��
;; 莉･荳九�ｯ 15 陦後�ｮ蝣ｴ蜷医��
;; line-setting
(setq next-line-add-newlines nil)
(when (not next-line-add-newlines)
  (line-number-mode 15)
  (column-number-mode 15))

;; 繧ｹ繧ｯ繝ｭ繝ｼ繝ｫ陦悟腰菴阪ｒ 1 陦後↓縺励∪縺吶��
(setq scroll-step 1)
;; 逕ｻ髱｢繧偵�ｯ縺ｿ蜃ｺ縺吝�ｴ蜷医↓ 1 陦後□縺代せ繧ｯ繝ｭ繝ｼ繝ｫ縺吶ｋ繧医≧縺ｫ縺励∪縺吶��
(setq scroll-conservatively 1)
;; 繧ｫ繝ｼ繧ｽ繝ｫ繧剃ｸ�逡ｪ荳翫°荳�逡ｪ荳九∪縺ｧ謖√▲縺ｦ縺�縺代∪縺吶��
(setq scroll-margin 0)


;; ===============================================================================
;;
;; Fringe(繝輔Μ繝ｳ繧ｸ)
;;
;; ===============================================================================
;; 蟾ｦ繝輔Μ繝ｳ繧ｸ縺ｮ荳贋ｸ九↓繝槭�ｼ繧ｯ繧偵▽縺代ｋ
(setq-default indicate-buffer-boundaries 'left)
;; 蜿ｳ繝輔Μ繝ｳ繧ｸ縺ｮ荳贋ｸ九↓繝槭�ｼ繧ｯ繧偵▽縺代ｋ
(setq-default indicate-buffer-boundaries 'right)

;; 蟾ｦ繝輔Μ繝ｳ繧ｸ縺ｮ荳翫→蜿ｳ繝輔Μ繝ｳ繧ｸ縺ｮ荳九↓繝槭�ｼ繧ｯ繧偵▽縺代ｋ
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))
;; 蜿ｳ繝輔Μ繝ｳ繧ｸ縺ｮ荳翫→蟾ｦ繝輔Μ繝ｳ繧ｸ縺ｮ荳九↓繝槭�ｼ繧ｯ繧偵▽縺代ｋ
(setq-default indicate-buffer-boundaries '((top . right) (t . left)))
;; 蜿ｳ繝輔Μ繝ｳ繧ｸ縺ｮ荳翫↓縺ｮ縺ｿ繝槭�ｼ繧ｯ繧偵▽縺代ｋ
(setq-default indicate-buffer-boundaries '((top . right) (t . nil)))


;; ===============================================================================
;;
;; Key binds(繧ｭ繝ｼ蜑ｲ繧雁ｽ薙※)
;;
;; ===============================================================================
;; i-search for japanese
(define-key isearch-mode-map "\C-k" 'isearch-edit-string)

;; M-g 縺ｧ謖�螳夊｡後↓繧ｫ繝ｼ繧ｽ繝ｫ繧帝｣帙�ｰ縺励∪縺吶��
(global-set-key "\M-g"
                '(lambda (x)
                  (interactive "Line to goto: ")
                  (goto-line x)))

;; *** comment/uncomment-regeon
;; C-x ; 縺ｧ繧ｳ繝｡繝ｳ繝医い繧ｦ繝�
;; C-x : 縺ｧ繧ｳ繝｡繝ｳ繝医ｒ縺ｯ縺壹☆
;; (global-set-key "\C-x;" 'comment-region)
;; (fset 'uncomment-region "\C-u\C-[xcomment-region\C-m")
;; (global-set-key "\C-x:" 'uncomment-region)

;; 閾ｪ蜍輔う繝ｳ繝�繝ｳ繝医ｒ陦後↑縺�縺ｾ縺吶��
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
;; 譛�霑台ｽｿ縺｣縺溘ヵ繧｡繧､繝ｫ繧貞挨縺ｮ繝舌ャ繝輔ぃ鬆伜沺縺ｫ陦ｨ遉ｺ縺励∪縺吶��
(define-key global-map "\C-cr" 'recentf-open-files)


;; ===============================================================================
;;
;; Useful functions(萓ｿ蛻ｩ髢｢謨ｰ)
;;
;; ===============================================================================
;; ** [EOF] 繧定｡ｨ遉ｺ縺励∪縺吶��
;;
(if *run-meadow1*
    (progn
      ;; Meadow 1.99 縺ｪ繧我ｻ･荳九�ｮ繧医≧縺ｫ縺吶ｋ縺ｨ蜷後§縺薙→縺後〒縺阪ｋ繧医≧縺ｧ縺吶��
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
    ;; 縺ゅｋ縺�縺ｯ
    (defun set-buffer-end-mark2()
      "At the end of the buffer [EOF] padding. This string can not be edited to be reflected in the file."
      (let ((overlay (make-overlay (point-max) (point-max))))
        (overlay-put overlay 'before-string #("[EOF]" 0 5 (face highlight)))
        (overlay-put overlay 'insert-behind-hooks
                     '((lambda (overlay after beg end &optional len)
                         (when after
                           (move-overlay overlay (point-max) (point-max))))))))
    (add-hook 'find-file-hooks 'set-buffer-end-mark2)))
;; 縺ｧ繧ゅ＞縺�縲�


;; ** 繧ｹ繧ｯ繝ｪ繝励ヨ繧剃ｿ晏ｭ倥☆繧区凾�ｼ瑚�ｪ蜍慕噪縺ｫ chmod +x 繧定｡後↑縺�繧医≧縺ｫ縺吶ｋ
;;
;; 谺｡縺ｮ繧医≧縺ｪ繧ｳ繝ｼ繝峨ｒ ~/.emacs 縺ｫ蜉�縺医※縺翫￥縺ｨ縲�
;; + 繝輔ぃ繧､繝ｫ縺ｮ蜈磯�ｭ縺ｫ #! 縺ｧ蟋九∪繧玖｡後′蜷ｫ縺ｾ繧後※縺�繧�
;; + 繝輔ぃ繧､繝ｫ蜷阪�ｮ蜈磯�ｭ縺後ヴ繝ｪ繧ｪ繝我ｻ･螟�
;; 縺ｮ蝣ｴ蜷茨ｼ悟ｮ溯｡檎畑縺ｮ繧ｹ繧ｯ繝ｪ繝励ヨ繝輔ぃ繧､繝ｫ縺ｧ縺ゅｋ縺ｨ隕九↑縺励※縲∽ｿ晏ｭ俶凾縺ｫ螳溯｡瑚ｨｱ蜿ｯ螻樊�ｧ繧定�ｪ蜍慕噪縺ｫ險ｭ螳壹＠縺ｾ縺吶��
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


;; 譌･莉倥ｒ謖ｿ蜈･縺励∪縺吶��
(defun my-insert-time ()
  "Insert the date the current cursor location."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))


;; 迴ｾ蝨ｨ驕ｸ謚樔ｸｭ縺ｮ繝舌ャ繝輔ぃ縺ｮ繝輔ぃ繧､繝ｫ蜷阪�√≠繧九＞縺ｯ繝舌ャ繝輔ぃ蜷阪ｒ霑斐＠縺ｾ縺吶��
(defun get-selected-buffer-name ()
  "迴ｾ蝨ｨ驕ｸ謚樔ｸｭ縺ｮ繝舌ャ繝輔ぃ縺ｮ繝輔ぃ繧､繝ｫ蜷阪�√≠繧九＞縺ｯ繝舌ャ繝輔ぃ蜷阪ｒ霑斐＠縺ｾ縺吶��"
  (let ((selected-buffer-filename (buffer-file-name ))
        (selected-buffer-name (buffer-name)))
    (if (not selected-buffer-filename)
        selected-buffer-name
      ;; else
      selected-buffer-filename)))


;; 繝�繝ｳ繝昴Λ繝ｪ繝舌ャ繝輔ぃ繧剃ｽ懈�舌＠縲√◎繧後ｒ繧ｦ繧｣繝ｳ繝峨え縺ｫ陦ｨ遉ｺ縺励∪縺吶��
(defun create-temporary-buffer ()
  "繝�繝ｳ繝昴Λ繝ｪ繝舌ャ繝輔ぃ繧剃ｽ懈�舌＠縲√◎繧後ｒ繧ｦ繧｣繝ｳ繝峨え縺ｫ陦ｨ遉ｺ縺励∪縺吶��"
  (interactive)
  ;; *temp* 縺ｪ繝舌ャ繝輔ぃ繧剃ｽ懈�舌＠縲√◎繧後ｒ繧ｦ繧｣繝ｳ繝峨え縺ｫ陦ｨ遉ｺ縺励∪縺吶��
  (switch-to-buffer (generate-new-buffer "*temp*"))
  ;; 繧ｻ繝ｼ繝悶′蠢�隕√↑縺�縺薙→繧呈欠螳壹＠縺ｾ縺呻ｼ�
  (setq buffer-offer-save nil))
;; C-c t 縺ｧ繝�繝ｳ繝昴Λ繝ｪ繝舌ャ繝輔ぃ繧剃ｽ懈�舌＠縺ｾ縺吶��
(global-set-key "\C-ct" 'create-temporary-buffer)


;; ===============================================================================
;;
;; SHELL(M-x shell 縺ｮ縺ｨ縺阪�ｮ險ｭ螳�)
;;
;; ===============================================================================
(when *run-windows*
  ;; MSYS 縺ｮ bash 繧剃ｽｿ逕ｨ縺励∪縺吶��
  (setq explicit-shell-file-name "bash.exe")
  (setq shell-file-name "sh.exe")

  ;; SHELL 縺ｧ ^M 縺御ｻ倥￥蝣ｴ蜷医�ｯ ^M 繧貞炎髯､縺励∪縺吶��
  (add-hook 'shell-mode-hook
            (lambda ()
              (set-buffer-process-coding-system 'undecided-dos 'sjis-unix)))
  ;; shell-mode 縺ｧ縺ｮ菫晉ｮ｡(for drive letter)
  (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")

  ;; shell-mode縺ｧ荳贋ｸ九〒陬懷ｮ後＠繝｢繝ｼ繝峨��
  (setq shell-mode-hook
        (function (lambda ()
                    (define-key shell-mode-map [up] 'comint-previous-input)
                    (define-key shell-mode-map [down]
                                'comint-next-input)))))


;; ===============================================================================
;;
;; Language mode(蜷�險�隱槭Δ繝ｼ繝�)
;;
;; ===============================================================================
;; 騾壼ｸｸ縺ｮ繧､繝ｳ繝�繝ｳ繝医〒蜊願ｧ偵せ繝壹�ｼ繧ｹ繧剃ｽｿ縺�縺ｾ縺吶��
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
  ;; cc-mode 繧ｹ繧ｿ繧､繝ｫ縺ｫ縺励∪縺吶��
  (c-set-style "cc-mode")
  ;; 繧､繝ｳ繝�繝ｳ繝医�ｯ遨ｺ逋ｽ譁�蟄励〒陦後↑縺�縺ｾ縺吶��
  (setq indent-tabs-mode nil)
  ;; `;' 繧呈款縺吶→閾ｪ蜍輔〒謾ｹ陦後＆繧後ｋ繧医≧縺ｫ縺励∪縺吶��
  (setq c-auto-newline nil)
  ;; 繧ｿ繝悶く繝ｼ縺ｧ繧､繝ｳ繝�繝ｳ繝医ｒ陦後＞縺ｾ縺吶��
  (setq c-tab-always-indent t)
  ;; 繧ｿ繝門ｹ�繧� 4 縺ｫ縺励∪縺吶��
  (setq tab-width 4)
  ;; 讓呎ｺ悶が繝輔そ繝�繝医ｒ tab-width 縺ｨ蜷後§縺ｫ縺励∪縺吶��
  (setq c++-basic-offset tab-width)
  ;; 繧､繝ｳ繝�繝ｳ繝医ｒ tab-width 縺ｨ蜷後§縺ｫ縺励∪縺吶��
  (setq c-indent-level tab-width)
  ;; 繧ｳ繝｡繝ｳ繝医□縺代�ｮ陦後�ｯ 0 縺ｫ縺励∪縺吶��
  (setq c-comment-only-line-offset 0)

  ;; 蠑墓焚繝ｪ繧ｹ繝医�ｮ髢峨§諡ｬ蠑ｧ繧ゅう繝ｳ繝�繝ｳ繝医＠縺ｾ縺吶��
  (c-set-offset 'arglist-close 0)
  ;; public 縺ｪ縺ｩ縺ｮ繧｢繧ｯ繧ｻ繧ｹ菫ｮ鬟ｾ蟄舌�ｯ 1 繧､繝ｳ繝�繝ｳ繝医＠縺ｾ縺吶��
  (c-set-offset 'access-label 1)
  ;; switch 讒区枚縺ｮ繝ｩ繝吶Ν縺ｯ tab-width 縺�縺代う繝ｳ繝�繝ｳ繝医＠縺ｾ縺吶��
  (c-set-offset 'case-label tab-width))

;; C++ 繝｢繝ｼ繝峨��
(add-hook 'c++-mode-hook 'my-c/c++-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Windows 蝗ｺ譛峨ヵ繧｡繧､繝ｫ逕ｨ縺ｮ繝｢繝ｼ繝峨ｒ霑ｽ蜉�縺励∪縺吶��
;;
(when *run-meadow*
  (require-if-exists generic-x)

  (if (and (fboundp 'bat-generic-mode)
           (fboundp 'ini-generic-mode))
      ;; 縺後�｀eadow3 縺ｧ縺ｯ縺薙ｌ繧峨�ｮ繝｢繝ｼ繝峨′辟｡縺�縺溘ａ縲∝�ｺ譚･縺ｾ縺帙ｓ縺ｧ縺励◆縲�
      ;; 髢｢謨ｰ逕ｨ縺ｮ縺ｫ縺ｯ fboundp 繧剃ｽｿ縺�縺ｾ縺吶��
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

  ;; visual basic 繧ゅ�ｼ縺ｩ縲�
  ;(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
  (lazyload (visual-basic-mode) "visual-basic-mode")
  (append-to-list auto-mode-alist '(("\\.\\(frm\\|bas\\|vb\\|cls\\)$" . visual-basic-mode))))

;; C# 繝｢繝ｼ繝峨��
;; (require-if-exists csharp-mode)
;; (setq auto-mode-alist
;;       (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;; ;; C# 繝｢繝ｼ繝峨��
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
;; Line number(陦檎分蜿ｷ)
;;
;; ===============================================================================
;;; 陦檎分蜿ｷ繝ｻ譯∫分蜿ｷ繧偵Δ繝ｼ繝峨Λ繧､繝ｳ縺ｫ陦ｨ遉ｺ縺吶ｋ繝ｻ縺励↑縺�險ｭ螳�
(line-number-mode t)   ;; 陦檎分蜿ｷ縲Ｕ 縺ｪ繧芽｡ｨ遉ｺ縲］il 縺ｪ繧蛾撼陦ｨ遉ｺ
(column-number-mode t) ;; 譯∫分蜿ｷ縲Ｕ 縺ｪ繧芽｡ｨ遉ｺ縲］il 縺ｪ繧蛾撼陦ｨ遉ｺ
;;
;; ** 陦檎分蜿ｷ繧偵ョ繝輔か繝ｫ繝医〒陦ｨ遉ｺ縺励∪縺吶��
;;
(if (>= emacs-major-version 22)
    ;; 23 莉･荳翫°繧� linum.el 縺悟�･縺｣縺ｦ縺�繧九�ｮ縺ｧ隕√ｊ縺ｾ縺帙ｓ縺後�｀eadow3 縺ｯ 22 縺ｪ縺ｮ縺ｧ
    ;; require-if-exists 縺悟ｿ�隕√〒縺吶��
    (require-if-exists linum))
(require-if-exists linum+)
;; 繝�繝輔か繝ｫ繝医〒 linum-mode 繧呈怏蜉ｹ縺ｫ縺励∪縺吶��
(global-linum-mode t)
;; 5 譯∝��縺ｮ鬆伜沺繧堤｢ｺ菫昴＠縺ｦ陦檎分蜿ｷ繧貞�･繧後∪縺吶��
(setq linum-format "%7d")


;; ===============================================================================
;;
;; Time stamp(繝輔ぃ繧､繝ｫ譖ｴ譁ｰ譌･)
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
;; Toolbar(繝�繝ｼ繝ｫ繝舌�ｼ)
;;
;; ===============================================================================
;; 繝�繝ｼ繝ｫ繝舌�ｼ繧定｡ｨ遉ｺ縺励↑縺�繧医≧縺ｫ縺励∪縺吶��
(tool-bar-mode -1)
(require-if-exists tool-bar+)


;; ===============================================================================
;;
;; Mode line Date display(繝｢繝ｼ繝峨Λ繧､繝ｳ縺ｮ譎ょ綾陦ｨ遉ｺ)
;;
;; ===============================================================================
;; 譎ょ綾縺ｮ繝輔か繝ｼ繝槭ャ繝医��
(setq display-time-string-forms
      '((let ((system-time-locale "C"))
          (format-time-string "%Y-%m-%dT%H:%M"))))
;; 繝｢繝ｼ繝峨Λ繧､繝ｳ縺ｫ迴ｾ蝨ｨ譎ょ綾繧定｡ｨ遉ｺ縺励∪縺吶��
(if *run-meadow*
    (display-time)
  ;; else
  (display-time-mode 1))


;; ===============================================================================
;;
;; *-selection
;;
;; ===============================================================================
;; 驕ｸ謚樣�伜沺繧貞�･蜉帙〒鄂ｮ縺肴鋤縺医ｋ繧医≧縺ｫ縺励∪縺吶��
(delete-selection-mode 1)
;; S-[竊絶�綻縺ｧ遽�蝗ｲ繧帝∈謚槭〒縺阪ｋ繧医≧縺ｫ縺ｪ繧翫∪縺吶��
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
;; Textmate 繝｢繝ｼ繝峨〒縺吶��
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
;; Backup file(繝舌ャ繧ｯ繧｢繝�繝励ヵ繧｡繧､繝ｫ)
;;
;; ===============================================================================
;; 繧ｪ繝ｼ繝医そ繝ｼ繝悶ｒ譛牙柑縺ｫ縺励∪縺吶��
(auto-save-mode t)
;; 繝舌ャ繧ｯ繧｢繝�繝励ヵ繧｡繧､繝ｫ繧剃ｽ懊ｊ縺ｾ縺吶��
(setq backup-inhibited nil)

;; 邁｡譏薙ヰ繝ｼ繧ｸ繝ｧ繝ｳ繧ｳ繝ｳ繝医Ο繝ｼ繝ｫ讖溯�ｽ繧呈怏蜉ｹ縺ｫ縺励∪縺吶��
(setq version-control t)

(when version-control
  ;; 譁ｰ縺励＞繧ゅ�ｮ繧� 6 縺､縺ｾ縺ｧ谿九☆繧医≧縺ｫ縺励∪縺吶��
  (setq kept-new-versions 6)
  ;; 蜿､縺�繧ゅ�ｮ繧� 6 縺､縺ｾ縺ｧ谿九☆繧医≧縺ｫ縺励∪縺吶��
  (setq kept-old-versions 6)
  ;; 蜿､縺�繝舌�ｼ繧ｸ繝ｧ繝ｳ繧呈ｶ医☆髫帙↓蟆九�ｭ縺ｪ縺�繧医≧縺ｫ縺励∪縺吶��
  (setq delete-old-versions t))

;; 邨ゆｺ�譎ゅ↓繧ｪ繝ｼ繝医そ繝ｼ繝悶ヵ繧｡繧､繝ｫ縺悟炎髯､縺輔ｌ縺ｾ縺帙ｓ縲�
(setq delete-auto-save-files nil)


;; ===============================================================================
;;
;; Miscellaneous Settings(髮大､壹↑險ｭ螳�)
;;
;; ===============================================================================
;; 繧ｫ繝ｬ繝ｳ繝医ョ繧｣繝ｬ繧ｯ繝医Μ繧偵�帙�ｼ繝�繝�繧｣繝ｬ繧ｯ繝医Μ縺ｫ險ｭ螳�
;; ""蜀�縺ｯ莉ｻ諢上�ｮ繝�繧｣繝ｬ繧ｯ繝医Μ繧呈欠螳壼庄閭ｽ
(cd "~/")

;; 襍ｷ蜍墓凾縺ｮ繝｡繝�繧ｻ繝ｼ繧ｸ繧定｡ｨ遉ｺ縺励∪縺帙ｓ縲�
(setq inhibit-startup-message t)
;;; 繧ｨ繝ｩ繝ｼ譎ゅ↓逕ｻ髱｢縺後ヵ繝ｩ繝�繧ｷ繝･縺吶ｋ繧医≧縺ｫ縺励∪縺吶��
;(setq visible-bell t)
(setq ring-bell-function 'ignore)
;;;; 繝輔Λ繝�繧ｷ繝･縺吶ｋ縺ｮ縺後≧縺悶＞縺ｮ縺ｧ縺励↑縺�繧医≧縺ｫ縺励∪縺励◆縲�

;; 譛�霑台ｽｿ縺｣縺溘ヵ繧｡繧､繝ｫ縺ｮ荳�隕ｧ繧定｡ｨ遉ｺ縺励∪縺吶��
(recentf-mode t)
;; 繝｡繝九Η繝ｼ縺ｫ陦ｨ遉ｺ縺吶ｋ繝輔ぃ繧､繝ｫ蜷阪ｒ 10 縺ｾ縺ｧ縺ｫ縺励∪縺吶��
(setq recentf-max-menu-items 10)
;; 譛�螟ｧ 15 繝輔ぃ繧､繝ｫ縺ｾ縺ｧ險倬鹸縺励∪縺吶��
(setq recentf-max-saved-items 15)

;; 莉悶�ｮ繝励Ο繧ｻ繧ｹ縺ｫ繧医▲縺ｦ螟画峩縺輔ｌ縺溘→縺阪�√ヰ繝�繝輔ぃ繧偵Μ繝ｭ繝ｼ繝峨＠縺ｾ縺吶��
(global-auto-revert-mode t)

(if (fboundp 'server-start)
    ;; emacsclient 繧剃ｽｿ縺�縺ｾ縺吶��
    (server-start))

;; auto-fill 繝｢繝ｼ繝峨ｒ菴ｿ逕ｨ縺励∪縺吶��
(setq-default auto-fill-function 'do-auto-fill)

;; 繧ｭ繝ｼ蜈･蜉帙ｒ險俶�ｶ縺励※縺翫″縺ｾ縺吶��
(open-dribble-file "~/.emacs_dribble")

;; 70 蟄礼岼縺ｧ閾ｪ蜍慕噪縺ｫ謾ｹ陦後＆繧後↑縺�繧医≧縺ｫ縺励∪縺吶��
;; 縺薙ｌ縺ｧ繧るｧ�逶ｮ縺ｪ縺ｨ縺阪�ｯ M-x set-fill-column RET 300 繧貞ｮ溯｡後＠縺ｦ縺上□縺輔＞縲�
(setq-default fill-column 300)

;; 繧ｿ繝匁枚蟄励↓繧医ｋ遨ｺ逋ｽ繧� 4 縺､縺ｫ縺励∪縺吶��
(setq-default tab-width 4)

;; 遨ｺ逋ｽ縺�縺代〒蟄嶺ｸ九£繧定｡後≧繧医≧縺ｫ縺励∪縺吶��
(setq-default indent-tabs-mode nil)

;; X 繧ｷ繧ｹ繝�繝�縺ｮ繧ｯ繝ｪ繝�繝励�懊�ｼ繝峨→繧ｭ繝ｫ繝ｪ繝ｳ繧ｰ繧貞�ｱ譛峨〒縺阪ｋ繧医≧縺ｫ縺励∪縺吶��
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
;; 縺薙％縺ｫ縺�縺｣縺ｱ縺�險ｭ螳壹ｒ譖ｸ縺�
;; https://emacs-jp.github.io/tips/emacs-in-2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;leaf縺ｮ :custom 縺ｧ險ｭ螳壹☆繧九→init.el縺ｫcustom縺悟享謇九↓險ｭ螳壹ｒ霑ｽ險倥＠縺ｾ縺吶�� 縺薙�ｮ迥ｶ豕√↓縺ｪ繧九→縲∝､画焚縺ｮ莠碁�咲ｮ｡逅�縺ｫ縺ｪ縺｣縺ｦ縺励∪縺�縺ｮ縺ｧ縲…ustom縺景nit.el縺ｫ霑ｽ險倥＠縺ｪ縺�繧医≧縺ｫ險ｭ螳壹＠縺ｾ縺吶��
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

;;cus-start.c Emacs縺ｮC險�隱樣Κ蛻�縺ｧ螳夂ｾｩ縺輔ｌ縺ｦ縺�繧句､画焚繧団ustom縺ｧ謇ｱ縺医ｋ繧医≧縺ｫ縺ｾ縺ｨ繧√※縺�繧九ヵ繧｡繧､繝ｫ縺ｧ縺吶�� 遘√�ｮ險ｭ螳壹ｒ譖ｸ縺�縺ｦ縺翫￥縺ｮ縺ｧ縲∝叙謐ｨ驕ｸ謚槭＠縺ｦ鬆ゅ¢繧後�ｰ縺ｨ諤昴＞縺ｾ縺吶�ょ､画焚縺ｮ隱ｬ譏弱�ｯ F1 v 縺ｧ遒ｺ隱阪〒縺阪∪縺吶��

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


;;Emacs縺ｮ螟悶〒繝輔ぃ繧､繝ｫ縺梧嶌縺榊､峨ｏ縺｣縺溘→縺阪↓閾ｪ蜍慕噪縺ｫ隱ｭ縺ｿ逶ｴ縺吶�槭う繝翫�ｼ繝｢繝ｼ繝峨〒縺吶�� 繧ゅ■繧阪ｓ縲・macs縺ｧ邱ｨ髮�縺励※縺�繧句�ｴ蜷医�ｯ螟悶�ｮ螟画峩縺ｧ荳頑嶌縺阪＆繧後ｋ縺薙→縺ｯ縺ゅｊ縺ｾ縺帙ｓ縲�
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.3)
           (auto-revert-check-vc-info . t))
  :global-minor-mode global-auto-revert-mode)


;;delsel
;;驕ｸ謚槭＠縺ｦ縺�繧狗憾諷九〒蜈･蜉帙＠縺溘→縺阪↓縲〉egion繧貞炎髯､縺励※謖ｿ蜈･縺吶ｋ繝槭う繝翫�ｼ繝｢繝ｼ繝峨〒縺吶�� 縺翫◎繧峨￥縺薙�ｮ謖吝虚縺ｮ縺ｻ縺�縺檎樟莉｣莠ｺ縺ｮ諢丞峙縺ｫ蜷医▲縺ｦ縺�繧九→諤昴＞縺ｾ縺吶��

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

;; local init.el ends here
