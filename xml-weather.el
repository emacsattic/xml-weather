;;; xml-weather.el --- Get xml-weather infos in emacs. 
;; 
;; Author: Thierry Volpiatto
;; Maintainer: Thierry Volpiatto
;; Copyright (C) 2009, Thierry Volpiatto, all rights reserved.

;; Created: lun. août  3 10:41:46 2009 (+0200)
;; Version: 
;; X-URL: http://mercurial.intuxication.org/hg/xml-weather  
;; Keywords: 
;; Compatibility: emacs23
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;;; Commentary:
;;
;; Install:
;; =======
;;
;; 1)
;; To obtain the feed, you must first register with weather.com[1] to obtain a license key.
;; The registration is free but mandatory.
;; Once you've registered, you'll receive a confirmation email and a link to the software developer's kit.
;; Included in the kit is a comprehensive user's guide, numerous icons, and instructions (and restrictions)
;; on use of The Weather Channel logos.
;; [1]http://www.weather.com/services/xmloap.html
;; 2)
;; Once you have your login and license key, you have two ways to setup them:
;; A) The bad way:
;;    Add to .emacs:
;;    (setq xml-weather-login "your login")
;;    (setq xml-weather-key "your key")
;; B) The good way:
;;    Be sure to require `auth-sources' and set it up.
;;    Add to your ~/.authinfo file this line:(change login and pwd)
;;    machine xoap.weather.com port http login xxxxx password xxxxxx
;; 3) Add to .emacs (be sure xml-weather.el is in your load-path):
;;    (require 'xml-weather)
;; 4) Get the icons set from the link to the software developer's kit
;;    you will find in your email.
;;    Put the icons in the directory of your choice and set it in your .emacs:
;;    (setq xml-weather-default-icons-directory "path/to/your/icons")
;;
;; Usage:
;; =====
;; M-x xml-weather-today-at (you will have a button for forecast)
;; M-x xml-weather-forecast-at (go straight to forecast)
;; xml-weather.el provide a ticker that show a builtin for current conditions
;; all the `xml-weather-timer-delay' seconds.
;; You will have to set your current location with `xml-weather-default-id'.
;; You can fetch it with M-x xml-weather-show-id.
;; Run the ticker with M-x xml-weather-ticker-run-with-timer
;; Stop timer with M-x xml-weather-ticker-cancel-timer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)
(require 'derived)

(defvar xml-weather-format-id-url
  "http://xoap.weather.com/search/search?where=%s")

(defvar xml-weather-format-xml-from-id-url ; id, unit=m,day-forecast=5,login,key
  "http://xoap.weather.com/weather/local/%s?cc=*&unit=%s&dayf=%s&prod=xoap&par=%s&key=%s")

(defvar xml-weather-unit "m"
  "*m mean metric, you will have wind speed in km/h, temperature in °C and so on.")

(defvar xml-weather-login nil
  "*Your xml-weather Login.
You should not set this variable directly. See `xml-weather-authentify'.
If you have an xml-weather entry in ~/.authinfo, leave it to nil.")

(defvar xml-weather-key nil
  "*Your xml-weather key.
You should not set this variable directly. See `xml-weather-authentify'.
If you have an xml-weather entry in ~/.authinfo, leave it to nil.")

(defvar xml-weather-day-forecast-num 5
  "*Number of days for forecast; Maximum 5.")

(defvar xml-weather-default-show-message-times 1
  "*Number of times ticker will show message before stopping.")

(defvar xml-weather-default-id "FRXX0098"
  "*Your favorite place for weather builtin.
You can get it with `xml-weather-show-id'.")

(defvar xml-weather-timer-delay 3600
  "*Send a new Builtin all the `xml-weather-timer-delay' seconds.")

(defvar xml-weather-default-icons-directory
  "/home/thierry/download/xml-weather-icons/icons/31x31")

(defvar xml-weather-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q] 'xml-weather-quit)
    (define-key map (kbd "S-<down>") 'xml-weather-next-day)
    (define-key map (kbd "S-<up>") 'xml-weather-precedent-day)
    map)
  "Keymap used for `xml-weather' commands.")

(define-derived-mode xml-weather-mode text-mode "xml-weather"
                     "Major mode to get info from xml-weather.

Special commands:
\\{xml-weather-mode-map}")

;;;###autoload
(defun xml-weather-quit ()
  (interactive)
  (quit-window))

;;;###autoload
(defun xml-weather-next-day ()
  (interactive)
  (forward-char 1) (search-forward "*" nil t) (forward-line 0))

;;;###autoload
(defun xml-weather-precedent-day ()
  (interactive)
  (forward-char -1) (search-backward "*" nil t) (forward-line 0))

(defun xml-weather-authentify ()
  "Authentify user from .authinfo file.
You have to setup correctly `auth-sources' to make this function
finding the path of your .authinfo file that is normally ~/.authinfo.
Entry in .authinfo should be:
machine xoap.weather.com port http login xxxxx password xxxxxx"
  (let ((xml-weather-auth
         (auth-source-user-or-password  '("login" "password")
                                        "xoap.weather.com"
                                        "http")))
    (when xml-weather-auth
      (setq xml-weather-login (car xml-weather-auth)
            xml-weather-key (cadr xml-weather-auth))
      nil)))

;; First step: Get ID of places
(defun xml-weather-get-place-id (place)
  "Get all ID corresponding to place."
  (let* ((url              (format xml-weather-format-id-url place))
         (url-request-data (encode-coding-string place 'utf-8))
         (data             (with-current-buffer (url-retrieve-synchronously url)
                             (buffer-string))))
    (with-current-buffer (get-buffer-create "*xml-weather*")
      (erase-buffer)
      (insert data)
        (loop
           with l = (xml-get-children (car (xml-parse-region (point-min)
                                                             (point-max)))
                                      'loc)
           for i in l
           collect (cons (car (last i)) (xml-get-attribute i 'id))))))

;; Second step: Get the xml info for the ID choosen:
;; http://xoap.weather.com/weather/local/[locid]

;; Replace the [locid], of course, with the location ID obtained in the previous step.
;; Appended to this URL is a mix of other parameters,
;; some required and some optional. A typical example might be:

;; http://xoap.weather.com/weather/local/NLXX0002?cc=*&dayf=5
;; &prod=xoap&par=[partner id]&key=[license key]

(defun xml-weather-get-info-on-id (id)
  (let* (xml-weather-login
         xml-weather-key
         (url  (progn
                 (unless (and xml-weather-login xml-weather-key)
                   (xml-weather-authentify))
                 (format xml-weather-format-xml-from-id-url
                       id
                       xml-weather-unit
                       xml-weather-day-forecast-num
                       xml-weather-login
                       xml-weather-key)))
         (data (with-current-buffer (url-retrieve-synchronously url)
                 (buffer-string))))
    (with-current-buffer (get-buffer-create "*xml-weather*")
      (erase-buffer)
      (insert data))))

;;;###autoload
(defun xml-weather-show-id (place)
  (interactive "sName: ")
  (let* ((id-list   (xml-weather-get-place-id place))
         (name-list (loop for i in id-list collect (car i)))
         (id        (completing-read "Choose a place: " name-list)))
    (setq id (cdr (assoc id id-list)))
    (message "ID code for %s is %s" place id)))

;; Third step convert xml info to alist
(defun xml-weather-get-alist ()
  (with-current-buffer "*xml-weather*"
    (let* ((loc           (xml-get-children (car (xml-parse-region (point-min)
                                                                   (point-max)))
                                            'loc))
           (cc            (xml-get-children (car (xml-parse-region (point-min)
                                                                   (point-max)))
                                            'cc))
           (info-cc       (loop for i in cc
                             for lsup = (caddr (assoc 'lsup i))
                             for obst = (caddr (assoc 'obst i))
                             for tmp = (caddr (assoc 'tmp i))
                             for wea = (caddr (assoc 't i))
                             for icon = (caddr (assoc 'icon i))
                             for bar = (caddr (assoc 'r (assoc 'bar i)))
                             for wind-dir-d = (caddr (assoc 'd (assoc 'wind i)))
                             for wind-dir = (caddr (assoc 't (assoc 'wind i)))
                             for gust = (caddr (assoc 'gust (assoc 'wind i)))
                             collect (list (cons "Date:" (or lsup ""))
                                           (cons "Observatory:" (or obst ""))
                                           (cons "Temperature:" (concat (or tmp "") "°C"))
                                           (cons "Cond:" (or (list (concat icon ".png") wea) ""))
                                           (cons "Pression:" (or bar ""))
                                           (cons "Wind dir:" (or (concat wind-dir  "(" wind-dir-d "°)") ""))
                                           (cons "Gust:" (or gust "")))))
           (today-info    (loop for i in loc
                             for dnam = (caddr (assoc 'dnam i))
                             for tm = (caddr (assoc 'tm i))
                             for lat = (caddr (assoc 'lat i))
                             for lon = (caddr (assoc 'lon i))
                             for sunr = (caddr (assoc 'sunr i))
                             for suns = (caddr (assoc 'suns i))
                             collect (cons (concat dnam " " tm)
                                           (append (list (cons "Latitude:" (or lat ""))
                                                         (cons "Longitude: " (or lon ""))
                                                         (cons "Sunrise:" (or sunr ""))
                                                         (cons "Sunset:" (or suns "")))
                                                   (car info-cc)))))
           (dayf          (xml-get-children (car (xml-parse-region (point-min)
                                                                   (point-max)))
                                            'dayf))
           (day-list      (loop for i in (xml-get-children (car dayf) 'day)
                             collect i))
           (morning-alist (loop for i in day-list
                             for d = (concat (cdr (assoc 't (cadr i))) " " (cdr (assoc 'dt (cadr i))))
                             for hi-temp = (caddr (assoc 'hi (cdr i)))
                             for low-temp = (caddr (assoc 'low (cdr i)))
                             for sunr = (caddr (assoc 'sunr (cdr i)))
                             for suns = (caddr (assoc 'suns (cdr i)))
                             for wind-dir = (caddr (assoc 't (assoc 'wind (assoc 'part (cdr i)))))
                             for wind-spd = (caddr (assoc 's (assoc 'wind (assoc 'part (cdr i)))))
                             for wea = (caddr (assoc 't (assoc 'part (cdr i))))
                             for icon = (caddr (assoc 'icon (assoc 'part (cdr i))))
                             for hmid = (caddr (assoc 'hmid (assoc 'part (cdr i))))
                             collect (cons d (list (cons "maxi:" (or hi-temp ""))
                                                   (cons "mini:" (or low-temp ""))
                                                   (cons "sunrise:" (or sunr ""))
                                                   (cons "sunset:" (or suns ""))
                                                   (cons "Wind direction:" (or wind-dir ""))
                                                   (cons "Wind speed:" (or wind-spd ""))
                                                   (cons "Cond:" (or (list (concat icon ".png") wea) ""))
                                                   (cons "Humidity:" (concat (or hmid "") "%"))))))
           (night-alist   (loop for i in day-list
                             for d = (concat (cdr (assoc 't (cadr i))) " " (cdr (assoc 'dt (cadr i))))
                             for hi-temp = (caddr (assoc 'hi (cdr i)))
                             for low-temp = (caddr (assoc 'low (cdr i)))
                             for sunr = (caddr (assoc 'sunr (cdr i)))
                             for suns = (caddr (assoc 'suns (cdr i)))
                             for all-part1 = (remove (assoc 'part (cdr i)) (cdr i))
                             for part2 = (assoc 'part all-part1)
                             for wind-dir = (caddr (assoc 't (assoc 'wind part2)))
                             for wind-spd = (caddr (assoc 's (assoc 'wind part2)))
                             for wea = (caddr (assoc 't part2))
                             for icon = (caddr (assoc 'icon (assoc 'part (cdr i))))
                             for hmid = (caddr (assoc 'hmid part2))
                             collect (cons d (list (cons "maxi:" (or hi-temp ""))
                                                   (cons "mini:" (or low-temp ""))
                                                   (cons "sunrise:" (or sunr ""))
                                                   (cons "sunset:" (or suns ""))
                                                   (cons "Wind direction:" (or wind-dir ""))
                                                   (cons "Wind speed:" (or wind-spd ""))
                                                   (cons "Cond:" (or (list (concat icon ".png") wea) ""))
                                                   (cons "Humidity:" (concat (or hmid "") "%")))))))
      (setq morning-alist (cons 'morning morning-alist))
      (setq night-alist (cons 'night night-alist))
      (setq today-info (cons 'info today-info))
      (list today-info morning-alist night-alist))))

;; Last step pprint the infos in alist
(defun xml-weather-pprint-today ()
  (let ((data (xml-weather-get-alist)))
    (with-current-buffer (get-buffer-create "*xml-weather-meteo*")
      (erase-buffer)
      (insert (propertize "* XML-WEATHER\n  ===========\n\n" 'face '((:foreground "Lightgreen"))))
      (loop for i in (cadr (assoc 'info data))
         if (listp i)
         do
           (xml-weather-insert-maybe-icons i)
         else
         do
           (insert (concat i "\n\n")))))
  (switch-to-buffer "*xml-weather-meteo*")
  (goto-char (point-max))
  (newline)
  (insert-button "[Forecast for next 4 days]"
                 'action 'xml-weather-button-func1
                 'face '((:background "green")))
  (goto-char (point-min))
  (xml-weather-mode))

(defun xml-weather-insert-maybe-icons (elm)
  (insert (concat "  " (car elm)))
  (if (file-exists-p xml-weather-default-icons-directory)
      (if (equal (car elm) "Cond:")
          (let* ((fname (cadr elm))
                 (img   (unless (equal fname ".png")
                          (create-image (expand-file-name fname xml-weather-default-icons-directory)))))
            (if img
                (progn
                  (insert-image img)
                  (insert (propertize (car (last elm)) 'face '((:foreground "red"))) "\n"))
                (insert "")))
          (insert (propertize (cdr elm) 'face '((:foreground "red"))) "\n"))
      (let ((info (if (eq (safe-length elm) 1)
                      (cdr elm)
                      (car (last elm)))))
        (insert (propertize info 'face '((:foreground "red"))) "\n"))))
  
(defun xml-weather-pprint-forecast (station)
  (let ((data (xml-weather-get-alist)))
    (with-current-buffer (get-buffer-create "*xml-weather-meteo*")
      (erase-buffer)
      (insert (propertize "* XML-WEATHER\n  ===========\n\n" 'face '((:foreground "Lightgreen"))))
      (insert (concat (propertize station 'face '((:foreground "magenta"))) "\n"))
      (loop
         for i in (assoc 'morning data)
         if (listp i) 
         do
           (loop
              for m in i
              if (listp m)
              do
                (xml-weather-insert-maybe-icons m)
              else
              do
                (insert (concat "\n* " (propertize m 'face '((:foreground "blue")))"\n\n"))
                (insert (propertize "Morning:\n" 'face '((:foreground "lightgreen")))))
         for j in (assoc 'night data)
         if (listp j)
         do
           (insert (propertize "Afternoon:\n" 'face '((:foreground "lightgreen"))))
           (loop
              for a in j
              if (listp a)
              do
                (xml-weather-insert-maybe-icons a))))
    (switch-to-buffer "*xml-weather-meteo*")
    (goto-char (point-min))
    (xml-weather-mode)))

(defvar xml-weather-last-id nil)
(defun xml-weather-now (id-pair)
  (let ((id      (cdr id-pair)))
    (setq xml-weather-last-id id-pair)
    (xml-weather-get-info-on-id id)
    (xml-weather-pprint-today)))

(defun xml-weather-forecast (id-pair)
  (let ((id      (cdr id-pair))
        (station (car id-pair)))
  (setq xml-weather-last-id id-pair)
  (xml-weather-get-info-on-id  id)
  (xml-weather-pprint-forecast station)))

(defun xml-weather-button-func1 (button)
  (xml-weather-forecast xml-weather-last-id))

;;;###autoload
(defun xml-weather-today-at (place)
  (interactive "sName: ")
  (let* ((id-list   (xml-weather-get-place-id place))
         (name-list (loop for i in id-list collect (car i)))
         (id        (completing-read "Choose a place: " name-list))
         (id-pair   (assoc id id-list)))
    (xml-weather-now id-pair)))

;;;###autoload
(defun xml-weather-forecast-at (place)
  (interactive "sName: ")
  (let* ((id-list   (xml-weather-get-place-id place))
         (name-list (loop for i in id-list collect (car i)))
         (id        (completing-read "Choose a place: " name-list))
         (id-pair   (assoc id id-list)))
    ;; setup buffer

    (xml-weather-forecast id-pair)))

;;; xml-weather ticker
(defun xml-weather-get-today-list ()
  (let ((data (xml-weather-get-alist)))
    (loop for i in (cadr (assoc 'info data))
       if (listp i)
       collect (if (eq (safe-length i) 1)
                   (concat (car i) (cdr i))
                   (concat (car i) (car (last i)))) into a
       else
       collect i into b
       finally return (append a b))))

(defun xml-weather-get-today-string ()
  (mapconcat 'identity (xml-weather-get-today-list) " | "))


(defun* xml-weather-message (&rest msg)
  (setq msg (concat "  <XML-WEATHER-BUILTIN>: "
                    (apply 'format msg) ; == (car msg)
                    "            "))
  (let* ((minibuf-size   (window-width (minibuffer-window)))
         (start-msg-size (+ 1 (length "[<] ")))
         (width          (- minibuf-size start-msg-size))
         (msglen         (length msg))
         submsg
         (count          0)
         (normal-range   (- msglen width)))
    (if (< msglen width)
        (message "%s" msg)
        (while t
          (when (> count xml-weather-default-show-message-times)
            (return-from xml-weather-message
              (when xml-weather-ticker-timer1
                (message "Next xml-weather Builtin in %d mn" (/ xml-weather-timer-delay 60)))))
          (incf count)
          (dotimes (i msglen)
            (setq submsg (if (< i normal-range)
                             (substring msg i (+ i width))
                             ;; Rolling is needed.
                             (concat (substring msg i)
                                     (substring msg 0 (- (+ i width) msglen)))))
            (message "[<] %s" submsg)
            (when (eq i 0) (incf count))
            (unless (sit-for (cond
                               ((eq i 0) 1.0)
                               (t 0.2)))
              (return-from xml-weather-message)))
          (garbage-collect)))))


(defun xml-weather-run-message-builtin (&optional id)
  (when id
    (xml-weather-get-info-on-id id))
  (xml-weather-message (xml-weather-get-today-string)))

(defvar xml-weather-ticker-timer1 nil)
(defvar xml-weather-ticker-timer2 nil)
(defun xml-weather-start-ticker-timers ()
  (setq xml-weather-ticker-timer1
        (run-with-timer 60
                        xml-weather-timer-delay
                        #'(lambda ()
                            (xml-weather-get-info-on-id xml-weather-default-id))))
  (setq xml-weather-ticker-timer2
        (run-with-idle-timer 65
                             xml-weather-timer-delay
                             #'(lambda ()
                                 (xml-weather-run-message-builtin)))))
  
  
;;;###autoload
(defun xml-weather-run-ticker ()
  (interactive)
  (xml-weather-start-ticker-timers))

;;;###autoload
(defun xml-weather-ticker-cancel-timer ()
  (interactive)
  (cancel-timer xml-weather-ticker-timer1)
  (setq xml-weather-ticker-timer1 nil)
  (cancel-timer xml-weather-ticker-timer2)
  (setq xml-weather-ticker-timer2 nil))

;; Provide
(provide 'xml-weather)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xml-weather.el ends here
