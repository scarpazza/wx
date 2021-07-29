;; Emacs Weather Mode
;;
;; Daniele P. Scarpazza 
;; 2021-07-25

(require 'xml)
(require 'dom)
(require 'all-the-icons)
(require 'iso8601)
(require 'timezone)


(defgroup wx nil
  "Weather observations and forecasts - retrieved from the U.S. National Weather Service."
  :prefix "wx/"
  :group 'Weather
  :link "http://www.github.com/scarpazza/wx"
  )


(defcustom wx/metar/metar_url_template
  "https://www.aviationweather.gov/adds/dataserver_current/httpparam?dataSource=metars&requestType=retrieve&format=xml&stationString=%s&hoursBeforeNow=%i"
  "This is the URL template to access the METAR (observed weather) service from the National Weather Service. Don't fiddle with it unless you know well what you are doing."
  :type 'string
;;  :require 'wx
  :group 'Weather)

(defcustom wx/metar/closest_airport_to_home
  "KJFK"
  "This is the four-letter FAA identifier of the airport you are fetching weather observations from."
  :type 'string
;;  :require 'wx
  :group 'Weather)

(defcustom wx/metar/look_back_hours
  18
  "This is the four-letter FAA identifier of the airport you are using to determine observed weather."
  :type 'integer
;;  :require 'wx
  :group 'Weather)

        
(defun wx/revert-hook () 
  (setq url (format wx/metar/metar_url_template wx/metar/closest_airport_to_home wx/metar/look_back_hours ))
  (message "Fetching weather from: %s" url)
  (setq xmlbuf (url-retrieve-synchronously url))
  (with-current-buffer xmlbuf
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (setq dom (libxml-parse-html-region (point-min) (point-max) ))
    )
  (setq metars (dom-by-tag dom 'metar))
  )


(switch-to-buffer xmlbuf)

;;

(defun wx/rel_humidity (T Td)
  "Relative Humidity, computed per RH=100* (EXP((17.625*TD)/(243.04+TD))/EXP((17.625*T)/(243.04+T)))
      https://bmcnoldy.rsmas.miami.edu/Humidity.html"
  (* 100.
     (/ (exp (/ (* 17.625 Td) (+ 243.04 Td)))
        (exp (/ (* 17.625 T)  (+ 243.04 T ))) )
     )
  )


(message "Entire XML response %s" (buffer-string))


;; GOOD
(defun utc_to_local (utc)
  "Convert UTC calendrical time to local calendrical time"
  (decode-time (encode-time utc) (current-time-zone)))  

(utc_to_local (iso8601-parse  "2021-08-26T18:43:00Z"))

;;
;; http://www.moratech.com/aviation/metar-class/metar-pg9-ww.html
;;
(setq wx/wx_to_icon_map
      (list
       '( "strong-wind"  ("SQ") )
       '( "raindrops"    ("PY" "BLPY" ) )
       '( "tornado"      ("FC" "+FC" )  )
       '( "fog"          ("BR" "FG" "MIFG" "BCFG" "PRFG" )  )
       '( "day-haze"     ("HZ")  )
       '( "sprinkle"     ("-RA" "-DZ" "DZ" "+DZ") )
       '( "sleet"        ("FZRA" "FZDZ" "RASN") )
       '( "smoke"        ("FU") )
       '( "dust"         ("DS""PO" "DU" "VA" "SS" "BLDU" "BLSA") )
       '( "hail"         ("GS" "GR" "SP") )
       '( "snow"         ("SN"  "PL"  "SG") )
       '( "snow-wind"    ("BLSN") )
       '( "showers"      ("RA" "+RA") )
       '( "thunderstorm"          ("VCTS" "TSRA") )
       '( "day-snow-thunderstorm" ("TSSN" "TSSNGS") )
       )
      )

(defun choose-icon (tuple)
  (let (( wx_event         (nth 0 tuple))
        ( wicon_name       (nth 1 tuple))
        ( metar_wx_codes   (nth 2 tuple))
        )
    (when (member wx_event metar_wx_codes) (all-the-icons-wicon wicon_name))
    )
  )

(defun process_present_weather (wx_string)
  (if wx_string
      (progn 
        (setq result "")
        (dolist (w (split-string wx_string " ") result)
          (setq w_match_table (mapcar (lambda (l) (push w l)) wx/wx_to_icon_map))
          (setq result (concat result
                               (mapconcat 'choose-icon w_match_table "" )))
                            
 ; (when (string-match-p (regexp-quote ".?FZ.*") w)
 ; (all-the-icons-wicon "snowflake-cold"))     
          )
        )
     ""
     ))



(defun process_sky_cover (conds is_day)
  "Determine the worst-coverage layer and generate a symbol accordingly"
  (setq covers '())
  (dolist (c conds)
    (dolist (layer (car (cdr c)))
      (when (string= (car layer) "sky_cover")
        (add-to-list 'covers (cdr layer)) ) ) )
  ;; (princ (format "DBG: Sky cover list %s" covers))

  (setq worst ;; worst sky coverage
        (catch 'loop
          (dolist (c '("OVC" "BKN" "SCT" "FEW" "CLR" "SKC"))
            (when (member c covers) (throw 'loop c)))))

  (if is_day
      (cond ( (string= worst "OVC")  ( all-the-icons-wicon "cloudy"))
            ( (string= worst "BKN")  ( all-the-icons-wicon "day-cloudy"))
            ( (string= worst "SCT")  ( all-the-icons-wicon "day-cloudy-high"))
            ( t                      ( all-the-icons-wicon "day-sunny"))
            )
    ;; else it's night
    (cond ( (string= worst "OVC")  ( all-the-icons-wicon "cloudy"))
          ( (string= worst "BKN")  ( all-the-icons-wicon "night-alt-cloudy"))
          ( (string= worst "SCT")  ( all-the-icons-wicon "night-alt-partly-cloudy"))
          ( t                      ( all-the-icons-wicon "night-clear"))
          ) )
   ;; (princ (format "DBG: Sky cover list %s" covers))
  )


(defun wx/synopsis/refresh-contents (&optional async)
  (with-output-to-temp-buffer "*Weather*"
    (dolist (m metars)
    ;;(print m)
      (let* (
             (station        (nth 2 (car (dom-by-tag m 'station_id))))
             (tempC          (string-to-number (nth 2 (car (dom-by-tag m 'temp_c)))))
             (dewpC          (string-to-number (nth 2 (car (dom-by-tag m 'dewpoint_c)))))
             (rh             (wx/rel_humidity tempC dewpC))
             (raw_time       (nth 2 (car (dom-by-tag m 'observation_time))))
             (time8601       (utc_to_local (iso8601-parse  raw_time)) )
             (raw            (nth 2 (car (dom-by-tag m 'raw_text))))
             (wx_string      (nth 2 (car (dom-by-tag m 'wx_string))))
             (metar_type     (nth 2 (car (dom-by-tag m 'metar_type))))
             (visibility_sm  (string-to-number (nth 2 (car (dom-by-tag m 'visibility_statute_mi)))))
             (flight_category (nth 2 (car (dom-by-tag m 'flight_category))))
             (sky_conditions (dom-by-tag m 'sky_condition))
             (p_mbar         (nth 2 (car (dom-by-tag m 'sea_level_pressure_mb))))
             (is_day         (and (>= (decoded-time-hour time8601) 6)
                                  (<= (decoded-time-hour time8601) 17)))
             (wind_speed     (string-to-number (nth 2 (car (dom-by-tag m 'wind_speed_kt)))))
             )
        ;; (sec min hour day mon year dow dst tz)
        
        (setq entry (list time8601
                     ;(format "%s-%s" station time8601)
                          (vector station
                                (format "%02i:%02i" (decoded-time-hour   time8601)
                                        (decoded-time-minute time8601)
                                        )
                                (if (string= metar_type "SPECI")
                                    (all-the-icons-octicon "alert");;"!"
                                  " ")
                                (concat (process_present_weather wx_string)
                                        (process_sky_cover sky_conditions is_day))
                                (format "%.0f°C" tempC)
                                (format "%.0f%%"  rh)
                                (if p_mbar (format "%4.0f mbar" (string-to-number p_mbar)) "N/A")
                                (format "%3.0f kts" wind_speed)
                                flight_category
                                (format "%2.0f SM" visibility_sm) 
                                (format "day=%s %s " is_day raw))
                          )
              )
        (add-to-list 'tabulated-list-entries entry)
      
      ;;(process_sky_cover sky_conditions is_day)
        )
      )
    )
  )




(define-derived-mode wx-mode tabulated-list-mode "Weather"
  "Weather mode"
  ;; (setq mode-line-process '((package--downloads-in-progress ":Loading")
  ;;                           (package-menu--transaction-status
  ;;                            package-menu--transaction-status)))
  (setq tabulated-list-format
        `[("Location"  8  nil)
          ("Issued"    6  t)
          ("!"         3  t)
          ("Weather"  10  nil)
          ("Temp"      4  t)
          ("Humid"     5  t)
          ("Pressure" 10  t)
          ("Wind"      9  t)
          ("Flight"    6  t)
          ("Vis'ty"    7  t)          
          ("Comments" 20 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Issued" . 'from-most-recent))
  (setq buf (get-buffer-create "*Weather*"))
  (tabulated-list-init-header)
  (setq revert-buffer-function 'wx/synopsis/refresh-contents)
  
  ;;(setf imenu-prev-index-position-function #'package--imenu-prev-index-position-function)
  ;;(setf imenu-extract-index-name-function #'package--imenu-extract-index-name-function)
  )

(defvar wx-mode/keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "d" 'wx/observed/menu-delete)
    (define-key map "?" #'wx/observed/menu-help)
    map)
  "Local keymap for `wx-mode' buffers.")

(easy-menu-define wx-mode/menu wx-mode/keymap
  "Menu for `wx-mode'."
  '("MenuChoice1"
    ["Menu Option 1" wx/observed/do_something  :help "Help for option 1"])
  )

(defun wx-observed (&optional no-fetch)
  "Display weather."
  (interactive "P")
  (require 'finder-inf nil t)
  ;; Initialize the package system if necessary.
  (unless package--initialized
    (package-initialize t))
  ;; Integrate the package-menu with updating the archives.
  ;;(add-hook 'package--post-download-archives-hook
  ;;          #'package-menu--post-refresh)
  ;;(add-hook 'package--post-download-archives-hook
  ;;          #'package-menu--mark-or-notify-upgrades 'append)

  (setq tabulated-list-entries '())
  (setq buf (get-buffer-create "*Weather*"))
  (set-buffer buf)
  (setq buffer-file-coding-system 'utf-8)
  (wx/synopsis/refresh-contents)
  (wx-mode)
  (tabulated-list-print)
  (switch-to-buffer buf)
  )

(wx-observed)


;;
