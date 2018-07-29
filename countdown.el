;; countdown.el --- Specify and run benchmarks and collect data 

;; Copyright (C) 2018 
;; Author: Joel Svensson <svenssonjoel@yahoo.se> 

;; This file is part of Emacs-Countdown.

;; Emacs-Countdown is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Emacs-Countdown is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs-Countdown.  If not, see <https://www.gnu.org/licenses/>.

(require 'seq)
(require 'cl)

;; ------------------------------------------------------------
;; State of Emacs-Countdown
(defvar countdown-timers-list '())

(defvar counter-idle-func nil) 

(cl-defstruct
    countdown-timer
  description-str
  start-date-time
  end-date-time     
  buffer)            ;; Buffer where countdown can be viewed
  
;; ------------------------------------------------------------
;; Code

(defun countdown-create (description start-t end-t)
  "Create a new coundown object at conc it to countdown timers list. Also starts a idle timer function for timer updates if not already running"
  (let ((countdown (make-countdown-timer)))
    (setq (countdown-timer-description-str countdown) description)
    (setq (countdown-timer-start-date-time countdown) start-t)
    (setq (countdown-timer-end-date-time countdown) end-t)
    (setq (countdown-timer-buffer countdown) (generate-new-buffer "timer"))
    (setq countdown-timers-list
	  (cons countdown countdown-timers-list))))


