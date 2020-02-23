;;; blink1.el --- Emacs client for the blink1 USB LED device  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Erik Anderson

;; Author: Erik Anderson <erik@ebpa.link>
;; Keywords: tools
;; Package-Requires: ((emacs "24.4") (dash "2.16.0") (s "1.12.0"))
;; Version: 0.0.1
;; URL: https://github.com/ebpa/blink1.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This is a simple wrapper around the blink1 command line utility.

;;; Code:

(require 'cl-lib)
(require 'term/tty-colors)

(require 'dash)
(require 's)

(defvar blink1-pattern-alist '() "List of saved patterns")

;;; Utility functions

(defun blink1--prefix-device-id ()
  "Helper to select ``all'' for no prefix argument and otherwise use the numeric ID."
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    "all"))

(defun blink1-device-ids ()
  "Return a list of blink1 devices."
  (mapcar
   #'cadr
   (s-match-strings-all "id:\\([0-9]+\\)"
                        (blink1-command "--list"))))

(defun blink1-command (command)
  "Run blink1 COMMAND."
  (shell-command-to-string (format "blink1-tool %s" command)))

(defconst blink1--rgb-regexp "#?\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)")

(defun blink1--rbg-string-p (str)
  "Return t if STR is a valid RGB color string."
  (and (string-match-p blink1--rgb-regexp str) t))

(defun blink1--rbg-string-to-triple (color)
  "Convert RGB COLOR string (ex: \"#ff0000\") to internal triple format (ex: '(255 0 0))."
  (when (stringp color)
    (save-match-data
      (when (string-match blink1--rgb-regexp color)
        (list (string-to-number (match-string 1 color) 16)
              (string-to-number (match-string 2 color) 16)
              (string-to-number (match-string 3 color) 16))))))

(defun blink1--rgb-color (color)
  "Return the RGB value of COLOR as a triple."
  (when (symbolp color)
    (setq color (symbol-name color)))
  (cond
   ((listp color)
    color)
   ((blink1--rbg-string-p color)
    (blink1--rbg-string-to-triple color))
   (t
    (-let* (((r g b) (assoc-default color color-name-rgb-alist)))
      (when (and r g b)
        (list (/ r 256) (/ g 256) (/ b 256)))))))

(defun blink1--format-rgb (rgb)
  "Format RGB value as a six character hexidecimal value (\"#00FF00\")."
  (-let* (((r g b) rgb))
    (when (and r g b)
      (format "#%02x%02x%02x" r g b))))

(cl-defun blink1-read-device-id (&optional (prompt "Device: "))
  "Prompt the user to select a device ID."
  (completing-read prompt (cons "all" (blink1-device-ids)) nil t nil nil "all"))

(cl-defun blink1-read-rgb-color (&optional (prompt "Color: "))
  "Prompt the user for a color and return a RGB color value as a triple (R G B) with values in the range of 0 to 255."
  (-let* ((color (completing-read prompt (mapcar #'car color-name-rgb-alist))))
    (or (blink1--rgb-color color)
        color)))

(defun blink1--normalize-pattern-entry (entry)
  "Internal helper to accommodate shorthand descriptions of pattern entries.  Convert ENTRY to the standard pattern format used internally."
  (declare (wip CLEANUP "use a struct?"))
  (if (not (listp entry))
      (list (blink1--rgb-color entry) 500)
    entry))

(cl-defun blink1-set-pattern-line (num &optional (color "#000000") (fade 0))
  "Set the line NUM of the stored pattern to COLOR with FADE duration (in ms)."
  (unless (and (stringp color)
               (s-starts-with-p "#" color))
    (setq color (blink1--format-rgb (or (blink1--rgb-color color) color))))
  (blink1-command (format "-m %d --rgb=%s --setpattline %d" fade color num)))

;;; Interactive commands

(cl-defun blink1-set-color (color &optional (device-id "all"))
  "Set Blink1 LED color to COLOR.

Optionally specify DEVICE-ID to control.  Controls all devices by default.

COLOR may be an hexadecimal RGB value formatted as \"#333333\" or a symbol or string for a color from `color-name-rgb-list'."
  (interactive (list (blink1-read-rgb-color) (blink1--prefix-device-id)))
  (setq color (blink1--rgb-color color))
  (shell-command
   (format "blink1-tool --rgb=%s --id %s"
           (blink1--format-rgb
            (or (blink1--rgb-color color)
                color))
           device-id)))

(defun blink1-play-pattern (pattern)
  "Play PATTERN continuously."
  (blink1-command "--play 0 --id all")
  (cl-loop for line from 0 to 31
           for entry = (nth line pattern)
           do
           (apply #'blink1-set-pattern-line line (blink1--normalize-pattern-entry entry)))
  (blink1-command "--play 1"))

(cl-defun blink1-play (&optional (state 1) (device-id "all"))
  "Start playing the current pattern.

Optionally specify DEVICE-ID to control.  Controls all devices by default."
  (interactive (list 1 (blink1--prefix-device-id)))
  (blink1-command (format "--play %d --id %s" state device-id)))

(cl-defun blink1-stop (&optional (_device-id "all"))
  "Start playing the current pattern.

Optionally specify DEVICE-ID to control.  Controls all devices by default."
  (interactive (list (blink1--prefix-device-id)))
  (blink1-play 0))

(cl-defun blink1-random (&optional (num 1))
  "Flash a random color NUM times."
  (interactive)
  (blink1-command (format "--random=%d" num)))

(cl-defun blink1-glimmer (&key (color 'white) (num 1) (device-id "all"))
  "Glimmer COLOR NUM times.

Optionally specify DEVICE-ID to control.  Controls all devices by default."
  (interactive (list :device-id (blink1--prefix-device-id)))
  (blink1-command (format "--rgb=%s --glimmer=%d --id %s " (blink1--format-rgb (blink1--rgb-color color)) num device-id)))

(cl-defun blink1-off (&key (device-id "all"))
  "Turn the blink1 off.

Optionally specify DEVICE-ID to control.  Controls all devices by default."
  (interactive (list :device-id (blink1--prefix-device-id)))
  (blink1-command (format "--off --id %s" device-id)))

(cl-defun blink1-blink (color-a &optional (color-b "black"))
  "Blink between COLOR-A and COLOR-B."
  (declare (wip CLEANUP "clarify documentation"
                TODO "accept device-id"))
  (interactive (list (blink1-read-rgb-color)))
  (blink1-play-pattern (list color-a color-b)))

(provide 'blink1)
;;; blink1.el ends here
