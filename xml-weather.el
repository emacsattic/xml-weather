;;; xml-weather.el --- 
;; 
;; Author: thierry
;; Maintainer: 
;; 
;; Created: lun. août  3 10:41:46 2009 (+0200)
;; Version: 
;; URL: 
;; Keywords: 
;; Compatibility: 
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
;; 
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
(defvar xml-weather-unit "m")
(defvar xml-weather-login nil) ; TODO:use auth-sources
(defvar xml-weather-key nil)
(defvar xml-weather-day-forecast-num 5)

(defvar xml-weather-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q] 'xml-weather-quit)
    (define-key map (kbd "M-<down>") 'xml-weather-next-day)
    (define-key map (kbd "M-<up>") 'xml-weather-precedent-day)
    map)
  "Keymap used for `xml-weather' commands.")

(define-derived-mode xml-weather-mode text-mode "xml-weather"
                     "Major mode to recurse in a tree and perform diverses actions on files.

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
(defun tv-xml-weather-get-place-id (place)
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

(defun tv-xml-weather-get-info-on-id (id)
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

;; Third step convert xml info to alist
(defun tv-xml-weather-get-alist ()
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
                             for bar = (caddr (assoc 'r (assoc 'bar i)))
                             for wind-dir-d = (caddr (assoc 'd (assoc 'wind i)))
                             for wind-dir = (caddr (assoc 't (assoc 'wind i)))
                             for gust = (caddr (assoc 'gust (assoc 'wind i)))
                             collect (list (cons "Date:" (or lsup ""))
                                           (cons "Observatory:" (or obst ""))
                                           (cons "Temperature:" (concat (or tmp "") "°C"))
                                           (cons "Conditions:" (or wea ""))
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
                             for wea = (caddr (assoc 'bt (assoc 'part (cdr i))))
                             for hmid = (caddr (assoc 'hmid (assoc 'part (cdr i))))
                             collect (cons d (list (cons "maxi:" (or hi-temp ""))
                                                   (cons "mini:" (or low-temp ""))
                                                   (cons "sunrise:" (or sunr ""))
                                                   (cons "sunset:" (or suns ""))
                                                   (cons "Wind direction:" (or wind-dir ""))
                                                   (cons "Wind speed:" (or wind-spd ""))
                                                   (cons "Cond:" (or wea ""))
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
                             for wea = (caddr (assoc 'bt part2))
                             for hmid = (caddr (assoc 'hmid part2))
                             collect (cons d (list (cons "maxi:" (or hi-temp ""))
                                                   (cons "mini:" (or low-temp ""))
                                                   (cons "sunrise:" (or sunr ""))
                                                   (cons "sunset:" (or suns ""))
                                                   (cons "Wind direction:" (or wind-dir ""))
                                                   (cons "Wind speed:" (or wind-spd ""))
                                                   (cons "Cond:" (or wea ""))
                                                   (cons "Humidity:" (concat (or hmid "") "%")))))))
      (setq morning-alist (cons 'morning morning-alist))
      (setq night-alist (cons 'night night-alist))
      (setq today-info (cons 'info today-info))
      (list today-info morning-alist night-alist))))

;; Last step pprint the infos in alist
(defun tv-xml-weather-pprint-today ()
  (let ((data (tv-xml-weather-get-alist)))
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

(defun xml-weather-insert-maybe-icons (str)
  (insert (concat "  " (car str)))
  (cond ((string-match "\\(cloud\\|P Cloudy\\)" (cdr str))
         (let ((img (create-image "~/download/weather-icons2/cloudy_sm_anim.gif")))
           (insert-image img)
           (insert (propertize (cdr str) 'face '((:foreground "red"))) "\n")))
        ((string-match "\\(T-Showers\\|T-Shwrs\\|Rain/Thunder\\|shwrs\\|showers\\|rain\\)" (cdr str))
         (let ((img (create-image "~/download/weather-icons2/tstorms_sm_anim.gif")))
           (insert-image img)
           (insert (propertize (cdr str) 'face '((:foreground "red"))) "\n")))
        ((string-match "\\(sunny\\|clear\\|fair\\)" (cdr str))
         (let ((img (create-image "~/download/weather-icons2/sun_sm_anim.gif")))
           (insert-image img)
           (insert (propertize (cdr str) 'face '((:foreground "red"))) "\n")))
        (t
         (insert (propertize (cdr str) 'face '((:foreground "red"))) "\n"))))
  
(defun tv-xml-weather-pprint-forecast (station)
  (let ((data (tv-xml-weather-get-alist)))
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
(defun tv-xml-weather-now (id-pair)
  (let ((id      (cdr id-pair)))
    (setq xml-weather-last-id id-pair)
    (tv-xml-weather-get-info-on-id id)
    (tv-xml-weather-pprint-today)))

(defun tv-xml-weather-forecast (id-pair)
  (let ((id      (cdr id-pair))
        (station (car id-pair)))
  (setq xml-weather-last-id id-pair)
  (tv-xml-weather-get-info-on-id  id)
  (tv-xml-weather-pprint-forecast station)))

(defun xml-weather-button-func1 (button)
  (tv-xml-weather-forecast xml-weather-last-id))

;;;###autoload
(defun tv-xml-weather-today-at (place)
  (interactive "sName: ")
  (let* ((id-list   (tv-xml-weather-get-place-id place))
         (name-list (loop for i in id-list collect (car i)))
         (id        (completing-read "Choose a place: " name-list))
         (id-pair   (assoc id id-list)))
    (tv-xml-weather-now id-pair)))

;;;###autoload
(defun tv-xml-weather-forecast-at (place)
  (interactive "sName: ")
  (let* ((id-list   (tv-xml-weather-get-place-id place))
         (name-list (loop for i in id-list collect (car i)))
         (id        (completing-read "Choose a place: " name-list))
         (id-pair   (assoc id id-list)))
    ;; setup buffer
    (tv-xml-weather-forecast id-pair)))


;; Provide
(provide 'xml-weather)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xml-weather.el ends here
