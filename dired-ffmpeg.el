;;; dired-ffmpeg.el --- Smart FFmpeg interface for Dired -*- lexical-binding: t -*-

;; Copyright (C) 2024 Laluxx

;; Author: Laluxx <laluxx@protonmail.com>
;; Maintainer: Laluxx <laluxx@protonmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (hydra "0.15.0"))
;; Keywords: multimedia files
;; URL: https://github.com/laluxx/dired-ffmpeg

;;; Commentary:

;; Smart `FFmpeg' interface that automatically activates when media files are present in Dired.
;; Features a live-updating `hydra' interface and intelligent handling of marked files.

;; TODO remember a list of all the images that we edited, or views, it should be a single
;; list that adds both existing lists, update the list whenever we enter `image-mode' or use `ffmpeg'
;; or press `TAB' in `dired-image-preview'

;;; Code:

(require 'dired)
(require 'hydra)
(require 'cl-lib)

;;; Customization

(defgroup dired-ffmpeg nil
  "FFmpeg integration for Dired."
  :group 'dired
  :prefix "dired-ffmpeg-")

(defcustom dired-ffmpeg-media-extensions
  '("svg" "png" "jpg" "jpeg" "gif" "webp" "mp4" "mkv" "avi" "mov" "flv" "wmv")
  "List of media file extensions that activate dired-ffmpeg."
  :type '(repeat string)
  :group 'dired-ffmpeg)

(defcustom dired-ffmpeg-presets
  '(("png" . ((:command "-vcodec png -compression_level 9")
              (:description "High-quality PNG with maximum compression")
              (:icon "🖼")))
    ("jpg" . ((:command "-qscale:v 2 -sampling-factor 4:2:0")
              (:description "High-quality JPEG with 4:2:0 chroma subsampling")
              (:icon "📸")))
    ("webp" . ((:command "-quality 90 -preset picture -lossless 0")
               (:description "WebP with excellent quality-size ratio")
               (:icon "🌐")))
    ("gif" . ((:command "-loop 0 -filter_complex [0:v] split [a][b];[a] palettegen [p];[b][p] paletteuse")
              (:description "Optimized GIF with generated palette")
              (:icon "🎞"))))
  "Alist of format presets for FFmpeg conversion."
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type string))
  :group 'dired-ffmpeg)

;;; Variables

(defvar dired-ffmpeg--current-file nil
  "Current file being processed by FFmpeg.")

(defvar dired-ffmpeg--process nil
  "Current FFmpeg process.")

(defvar dired-ffmpeg--quality 90
  "Current quality setting (1-100).")

(defvar dired-ffmpeg--scale "1920:-1"
  "Current scale setting (width:height).")

(defvar dired-ffmpeg--setup-done nil
  "Whether dired-ffmpeg has been setup in the current buffer.")

;;; Utility Functions

(defun dired-ffmpeg--has-media-files-p ()
  "Check if current dired buffer has media files."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (when-let* ((file (dired-get-filename nil t))
                    (ext (and file (file-name-extension file))))
          (setq found (member (downcase ext) dired-ffmpeg-media-extensions)))
        (forward-line 1))
      found)))

(defun dired-ffmpeg--format-size (file)
  "Format FILE size human readable."
  (let ((size (file-attribute-size (file-attributes file))))
    (cond
     ((< size 1024) (format "%dB" size))
     ((< size 1048576) (format "%.1fK" (/ size 1024.0)))
     (t (format "%.1fM" (/ size 1048576.0))))))

(defun dired-ffmpeg--format-current-file ()
  "Format current file info with icon based on type."
  (when dired-ffmpeg--current-file
    (let* ((name (file-name-nondirectory dired-ffmpeg--current-file))
           (ext (file-name-extension name))
           (preset (cdr (assoc ext dired-ffmpeg-presets)))
           (icon (or (and preset (plist-get preset :icon)) "📄")))
      (format "%s %s (%s)"
              icon
              (propertize name 'face 'font-lock-string-face)
              (propertize (dired-ffmpeg--format-size dired-ffmpeg--current-file)
                          'face 'font-lock-comment-face)))))

;;; Core Conversion Logic

(defun dired-ffmpeg--build-command (input format)
  "Build FFmpeg command for INPUT using FORMAT preset."
  (let* ((preset (cdr (assoc format dired-ffmpeg-presets)))
         (output (concat (file-name-sans-extension input) "." format)))
    (concat "ffmpeg -y -i " (shell-quote-argument input)
            " " (plist-get preset :command)
            " -quality " (number-to-string dired-ffmpeg--quality)
            " -vf scale=" dired-ffmpeg--scale
            " " (shell-quote-argument output))))

(defun dired-ffmpeg--convert (format)
  "Convert current file to FORMAT."
  (let ((command (dired-ffmpeg--build-command dired-ffmpeg--current-file format)))
    (setq dired-ffmpeg--process
          (make-process
           :name "ffmpeg-process"
           :buffer "*FFmpeg Output*"
           :command (split-string command " ")
           :sentinel
           (lambda (process event)
             (when (string= event "finished\n")
               (message "✓ Converted to %s: %s"
                        format
                        (file-name-nondirectory dired-ffmpeg--current-file))
               (dired-revert)))))))

;;; Hydra Interface

(defun dired-ffmpeg--update-hydra ()
  "Force hydra to update its display."
  (hydra-show-hint dired-ffmpeg-hydra t))

(defhydra dired-ffmpeg-hydra
  (:hint nil
         :foreign-keys run
         :post (dired-ffmpeg--update-hydra))
  (format "
%s

^Parameters^              ^Scale^                 ^Quick Convert^
^^──────────────────────────────────────────────────────────────────
_q_ Quality (%d) ↑       _w_ Width (%s)     _p_ PNG   🖼
_Q_ Quality (%d) ↓       _h_ Height          _j_ JPEG  📸
                       _r_ Reset           _W_ WebP  🌐
                                         _g_ GIF   🎞

_c_ Custom Format   _k_ Kill Process   _a_ Advanced   _RET_ Convert
_?_ Help           _x_ Exit
"
          (or (dired-ffmpeg--format-current-file) "No file selected")
          dired-ffmpeg--quality
          dired-ffmpeg--scale
          dired-ffmpeg--quality)
  
  ;; Quality controls with instant feedback
  ("q" (progn 
         (setq dired-ffmpeg--quality (min 100 (+ dired-ffmpeg--quality 5)))
         (hydra-show-hint dired-ffmpeg-hydra t)))
  ("Q" (progn 
         (setq dired-ffmpeg--quality (max 1 (- dired-ffmpeg--quality 5)))
         (hydra-show-hint dired-ffmpeg-hydra t)))

  ;; Scale controls
  ("w" (let ((width (read-number "Width: ")))
         (setq dired-ffmpeg--scale (format "%d:-1" width))
         (hydra-show-hint dired-ffmpeg-hydra t)))
  ("h" (let ((height (read-number "Height: ")))
         (setq dired-ffmpeg--scale (format "-1:%d" height))
         (hydra-show-hint dired-ffmpeg-hydra t)))
  ("r" (progn 
         (setq dired-ffmpeg--scale "1920:-1")
         (hydra-show-hint dired-ffmpeg-hydra t)))

  ;; Quick convert actions
  ("p" (dired-ffmpeg--convert "png") :exit t)
  ("j" (dired-ffmpeg--convert "jpg") :exit t)
  ("W" (dired-ffmpeg--convert "webp") :exit t)
  ("g" (dired-ffmpeg--convert "gif") :exit t)

  ;; Other actions
  ("c" (call-interactively #'dired-ffmpeg-convert-custom) :exit t)
  ("k" (when (and dired-ffmpeg--process
                  (process-live-p dired-ffmpeg--process))
         (kill-process dired-ffmpeg--process)
         (message "⚠ FFmpeg process killed")))
  ("a" (message "Advanced options coming soon"))
  ("?" (message "Help: q/Q - Quality, w/h - Scale, p/j/W/g - Convert"))
  ("x" nil "exit" :exit t)
  ("RET" nil "done" :exit t))

;; (defhydra dired-ffmpeg-hydra
;;   (:hint nil
;;          :foreign-keys run
;;          :post (dired-ffmpeg--update-hydra))
;;   "
;; %s

;; ^Parameters^              ^Scale^                 ^Quick Convert^
;; ^^──────────────────────────────────────────────────────────────────
;; _q_ Quality (%d) ↑      _w_ Width (%s)     _p_ PNG   🖼
;; _Q_ Quality (%d) ↓      _h_ Height          _j_ JPEG  📸
;;                       _r_ Reset           _W_ WebP  🌐
;;                                         _g_ GIF   🎞

;; _c_ Custom Format   _k_ Kill Process   _a_ Advanced   _RET_ Convert
;; _?_ Help           _x_ Exit
;; "
;;   ;; Quality controls with instant feedback
;;   ("q" (progn 
;;          (setq dired-ffmpeg--quality (min 100 (+ dired-ffmpeg--quality 5)))
;;          (hydra-show-hint dired-ffmpeg-hydra t)))
;;   ("Q" (progn 
;;          (setq dired-ffmpeg--quality (max 1 (- dired-ffmpeg--quality 5)))
;;          (hydra-show-hint dired-ffmpeg-hydra t)))

;;   ;; Scale controls
;;   ("w" (let ((width (read-number "Width: ")))
;;          (setq dired-ffmpeg--scale (format "%d:-1" width))
;;          (hydra-show-hint dired-ffmpeg-hydra t)))
;;   ("h" (let ((height (read-number "Height: ")))
;;          (setq dired-ffmpeg--scale (format "-1:%d" height))
;;          (hydra-show-hint dired-ffmpeg-hydra t)))
;;   ("r" (progn 
;;          (setq dired-ffmpeg--scale "1920:-1")
;;          (hydra-show-hint dired-ffmpeg-hydra t)))

;;   ;; Quick convert actions
;;   ("p" (dired-ffmpeg--convert "png") :exit t)
;;   ("j" (dired-ffmpeg--convert "jpg") :exit t)
;;   ("W" (dired-ffmpeg--convert "webp") :exit t)
;;   ("g" (dired-ffmpeg--convert "gif") :exit t)

;;   ;; Other actions
;;   ("c" (call-interactively #'dired-ffmpeg-convert-custom) :exit t)
;;   ("k" (when (and dired-ffmpeg--process
;;                   (process-live-p dired-ffmpeg--process))
;;          (kill-process dired-ffmpeg--process)
;;          (message "⚠ FFmpeg process killed")))
;;   ("a" (message "Advanced options coming soon"))
;;   ("?" (message "Help: q/Q - Quality, w/h - Scale, p/j/W/g - Convert"))
;;   ("x" nil "exit" :exit t)
;;   ("RET" nil "done" :exit t))

(defun dired-ffmpeg-hydra/format-hint ()
  "Format the hydra hint with current values."
  (let ((file-info (or (dired-ffmpeg--format-current-file) "No file selected")))
    (format (format (hydra--format nil :hint dired-ffmpeg-hydra)
                    file-info
                    (number-to-string dired-ffmpeg--quality)
                    dired-ffmpeg--scale
                    (number-to-string dired-ffmpeg--quality)))))

;; (defun dired-ffmpeg-hydra/format-hint ()
;;   "Format the hydra hint with current values."
;;   (format (hydra-get-hint dired-ffmpeg-hydra)
;;           (or (dired-ffmpeg--format-current-file) "No file selected")
;;           dired-ffmpeg--quality
;;           dired-ffmpeg--scale
;;           dired-ffmpeg--quality))

;;; Interactive Functions

(defun dired-ffmpeg-convert-custom (format)
  "Convert current file to a custom FORMAT."
  (interactive "sOutput format: ")
  (dired-ffmpeg--convert format))

;;;###autoload
(defun dired-ffmpeg ()
  "Launch FFmpeg interface for file at point or marked files."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (user-error "Not in dired mode"))

  (let ((marked (dired-get-marked-files nil nil nil t)))
    (cond
     ((eq marked t) (user-error "No file at point"))
     ((= (length marked) 1)
      (setq dired-ffmpeg--current-file (car marked)))
     (t
      (setq dired-ffmpeg--current-file (car marked))))
    
    (let* ((ext (file-name-extension dired-ffmpeg--current-file)))
      (unless (member (downcase ext) dired-ffmpeg-media-extensions)
        (user-error "Unsupported format: %s" ext)))
    
    (dired-ffmpeg-hydra/body)))

(defun dired-ffmpeg--setup-buffer ()
  "Setup dired-ffmpeg in current buffer if needed."
  (when (and (eq major-mode 'dired-mode)
             (not dired-ffmpeg--setup-done)
             (dired-ffmpeg--has-media-files-p))
    (define-key dired-mode-map (kbd "C-j") #'dired-ffmpeg)
    (setq-local dired-ffmpeg--setup-done t)))

;;;###autoload
(define-minor-mode dired-ffmpeg-mode
  "Minor mode for FFmpeg integration in Dired."
  :global t
  (if dired-ffmpeg-mode
      (add-hook 'dired-after-readin-hook #'dired-ffmpeg--setup-buffer)
    (remove-hook 'dired-after-readin-hook #'dired-ffmpeg--setup-buffer)))

(provide 'dired-ffmpeg)

;;; dired-ffmpeg.el ends here
