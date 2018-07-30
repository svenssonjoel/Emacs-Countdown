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

(defvar countdown-idle-func-timer nil) 

(cl-defstruct
    countdown-timer
  description-str
  start-date-time
  end-date-time     
  buffer)            ;; Buffer where countdown can be viewed
  
;; ------------------------------------------------------------
;; Code

(defun countdown-create (description start-t end-t)
  "Create a new coundown object and conc it to countdown timers list. Also starts a idle timer function for timer updates if not already running"
  (let ((countdown (make-countdown-timer)))
    (setf (countdown-timer-description-str countdown) description)
    (setf (countdown-timer-start-date-time countdown) start-t)
    (setf (countdown-timer-end-date-time countdown) end-t)
    (setf (countdown-timer-buffer countdown) (generate-new-buffer "timer"))
    (setq countdown-timers-list
	  (cons countdown countdown-timers-list))
    (if (not countdown-idle-func-timer)
	(setq countdown-idle-func-timer (run-at-time t 1 'countdown-idle-func))
      ())))

(defun countdown-idle-func ()
  "Update state of all ongoing countdowns present in the countdown-timers-list"
  (let ((curr-time (current-time)))
    (countdown-update-timers curr-time))
  (message "Running idle-func")
  (message "%s" countdown-timers-list))

(defun countdown-update-timers (curr-time)
  "update all countdowns"
  (dolist (elt countdown-timers-list ())
    (if (time-less-p (countdown-timer-end-date-time elt) curr-time)
	(with-current-buffer (countdown-timer-buffer elt)
	  (setf (buffer-string) "TIMEOUT!"))
      (let ((time-left (time-subtract (countdown-timer-end-date-time elt) curr-time))) 
	(with-current-buffer (countdown-timer-buffer elt)
	  (setf (buffer-string) (format "%s" time-left)))))))
	

(defun countdown-cancel-all ()
  "Cancels all running timers and kills the idle-func"
  (setq countdown-timers-list '())
  (cancel-timer counter-idle-func-timer)
  (setq counter-idle-func-timer nil))


;; ------------------------------------------------------------
;; Interface functions

(defun countdown-new (description time-str)
  "Start a new timer given a string XX:YY:ZZ for XX hours, YY minutes, ZZ seconds"
  (let ((hms (split-string time-str ":")))
    (if (not (= (length hms) 3))
	(message "Error parsing time string")
      (let* ((h (string-to-number (car hms)))
	     (m (string-to-number (car (cdr hms))))
	     (s (string-to-number (car (cdr (cdr hms)))))
	     (countdown-seconds (+ (* 3600 h) (* 60 m) s))
	     (curr-time (current-time))
	     (end-time (time-add curr-time countdown-seconds)))
	(countdown-create description curr-time end-time)))))
	
	
	     
	  
	    
	
    


