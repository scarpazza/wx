;; Emacs Weather Mode
;;
;; Daniele P. Scarpazza 
;; 2021-07-25

(require 'xml)
(require 'dom)
(require 'all-the-icons)
(require 'iso8601)
(require 'timezone)


;; to-do
;; - compute flight cat from taf
;; - finish more metar wx codes
;; - compute day from ephemerides


;; Customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup wx nil
  "Weather observations and forecasts - retrieved from the U.S. National Weather Service."
  :prefix "wx/"
  :group 'Weather
  :link "http://www.github.com/scarpazza/wx"
  )


(defcustom wx/metar/url_template
  "https://www.aviationweather.gov/adds/dataserver_current/httpparam?dataSource=metars&requestType=retrieve&format=xml&stationString=%s&hoursBeforeNow=%i"
  "This is the URL template to access the METAR (observed weather) service from the National Weather Service. Don't fiddle with it unless you know well what you are doing."
  :type 'string
;;  :require 'wx
  :group 'Weather)

(defcustom wx/taf/url_template
  "https://www.aviationweather.gov/adds/dataserver_current/httpparam?dataSource=tafs&requestType=retrieve&format=xml&stationString=%s&hoursBeforeNow=%i" 
  "This is the URL template to access the TAF (weather forecasts) from the National Weather Service. Don't fiddle with it unless you know well what you are doing."
  :type 'string
;;  :require 'wx
  :group 'Weather)


(defcustom wx/metar/closest_airport_to_home  "KJFK"
  "Four-letter FAA identifier of the airport you are fetching weather observations from. This might be a smaller and closer airport than the one for which you request forecasts."
  :type 'string
;;  :require 'wx
  :group 'Weather)

(defcustom wx/taf/closest_airport_to_home  "KJFK"
  "Four-letter FAA identifier of the airport you are requesting weather forecasts for. This might be a larger and further away from home airport than the one where you get observations from."
  :type 'string
;;  :require 'wx
  :group 'Weather)


(defcustom wx/metar/look_back_hours 18
  "How many hours are you interested in looking back when requesting observed weather."
  :type 'integer
;;  :require 'wx
  :group 'Weather)

(defcustom wx/taf/look_back_hours 3
  "How many hours of weather forecasts are you requesting. We recommend you don't change this value. Usually only the last forecast is immediately useful."
  :type 'integer
;;  :require 'wx
  :group 'Weather)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request_dom (url root_tag)
  "Fetches the URL given, loads it into the buffer whose name is given,
parses its XML and returns its DOM"

  (message (format "Fetching weather forecasts from: %s" url))
  (setq buf (url-retrieve-synchronously url))

  (message (format "Populating buffer" buf))

  (with-current-buffer buf
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (setq dom (libxml-parse-html-region (point-min) (point-max) ))
    )

   (message (format "Extracting DOM for tag %s" root_tag))
   (message (format "Extracting DOM %s" root_tag))

  
  (cons (dom-by-tag dom root_tag) buf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq wx/metars nil)
(setq wx/tafs nil)


;; (message "Entire XML response %s" (buffer-string))
;; ;; GOOD

(defun utc_to_local (utc)
   "Convert UTC calendrical time to local calendrical time"
   (decode-time (encode-time utc) (current-time-zone)))  

;; (utc_to_local (iso8601-parse  "2021-08-26T18:43:00Z"))


(defun wx/rel_humidity (T Td)
  "Relative Humidity, computed per RH=100* (EXP((17.625*TD)/(243.04+TD))/EXP((17.625*T)/(243.04+T)))
      https://bmcnoldy.rsmas.miami.edu/Humidity.html"
  (* 100.
     (/ (exp (/ (* 17.625 Td) (+ 243.04 Td)))
        (exp (/ (* 17.625 T)  (+ 243.04 T ))) )
     )
  )

;; More weather strings at:
;; http://www.moratech.com/aviation/metar-class/metar-pg9-ww.html
;;
(setq wx/wx_to_icon_map
      (list
       '( "strong-wind"  ("SQ") )
       '( "raindrops"    ("PY" "BLPY" ) )
       '( "tornado"      ("FC" "+FC" )  )
       '( "fog"          ("BR" "FG" "MIFG" "BCFG" "PRFG" )  )
       '( "day-haze"     ("HZ")  )
       '( "sprinkle"     ("-RA" "-DZ" "DZ" "+DZ" ) )
       '( "sleet"        ("FZRA" "FZDZ" "RASN") )
       '( "smoke"        ("FU") )
       '( "dust"         ("DS""PO" "DU" "VA" "SS" "BLDU" "BLSA") )
       '( "hail"         ("GS" "GR" "SP") )
       '( "snow"         ("SN"  "PL"  "SG") )
       '( "snow-wind"    ("BLSN") )
       '( "showers"      ("RA" "+RA" "VCSH" "SHRA") )
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

(defun wx/process_present_weather (wx_string)
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





(defun wx/format_taf_forecast (f station)
  "Returns a tabulated entry representing one TAF forecast"
  (let* (
         (raw_time       (nth 2 (car (dom-by-tag f 'fcst_time_from))))
         (time8601       (utc_to_local (iso8601-parse  raw_time)) )         
         (wind_block     (dom-by-tag f 'wind_speed_kt))
         (wind_speed     (if wind_block (string-to-number (nth 2 (car wind_block))) -1))
         (wx_string      (nth 2 (car (dom-by-tag f 'wx_string))))
         (visibility_sm  (string-to-number (nth 2 (car (dom-by-tag f 'visibility_statute_mi)))))
         (is_day         (and (>= (decoded-time-hour time8601) 6)
                              (<= (decoded-time-hour time8601) 17))) )        
    (list time8601
          (vector station
                  (format "%02i/%02i %02i:%02i" (decoded-time-month   time8601) (decoded-time-day   time8601) (decoded-time-hour   time8601) (decoded-time-minute time8601) ) 
                  "->"
                  (wx/process_present_weather wx_string)
                  "" ;; temperature
                  "" ;; humidity
                  "" ;; pressure
                  (if (< wind_speed 0) "N/A" (format "%3.0f kts" wind_speed))
                  "??VFR"
                  (format "%2.0f SM" visibility_sm) 
                  (format "day=%s WX=%s " is_day wx_string))
          )
    )
  )



(defun wx/format_metar (m)
  "Returns a tabulated entry representing one METAR observation"
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
         (wind_block     (dom-by-tag m 'wind_speed_kt))
         (wind_speed     (if wind_block (string-to-number (nth 2 (car wind_block))) -1))
         )
        
    (list time8601
          (vector station
                  (format "%02i/%02i %02i:%02i" (decoded-time-month   time8601) (decoded-time-day   time8601) (decoded-time-hour   time8601) (decoded-time-minute time8601) )
                  (if (string= metar_type "SPECI")
                      (all-the-icons-octicon "alert");;"!"
                    " ")
                  (concat (wx/process_present_weather wx_string)
                          (process_sky_cover sky_conditions is_day))
                  (format "%.0f°C" tempC)
                  (format "%.0f%%"  rh)
                  (if p_mbar (format "%4.0f mbar" (string-to-number p_mbar)) "N/A")
                  (if (< wind_speed 0) "N/A" (format "%3.0f kts" wind_speed))
                  flight_category
                  (format "%2.0f SM" visibility_sm) 
                  (format "day=%s %s " is_day raw))
          )
    )
  )



(defun wx/refresh-contents (&optional _arg _noconfirm)
  ;;(wx/ensure-wx-mode)
  (setq tabulated-list-entries '())

  ;; only fetch the metars once
  (if (not wx/metars)
      (let* ( (url (format wx/metar/url_template wx/metar/closest_airport_to_home wx/metar/look_back_hours ))
              (answer (request_dom url 'metar)) )
        (progn
          (setq wx/metars        (car answer))
          (setq wx/metars/xmlbuf (cdr answer)) )
        )
    )  
  (if (not wx/tafs)
      (let* ( (url (format wx/taf/url_template wx/taf/closest_airport_to_home wx/taf/look_back_hours ))
              ( answer (request_dom url 'taf))
              )
        (progn
          (setq wx/tafs        (car answer))
          (setq wx/tafs/xmlbuf (cdr answer))
          (print wx/tafs)
          )
        
        )
    )

  (setq wx/firsttaf       (car wx/tafs))
  (print wx/firsttaf)
            
  (setq station           (nth 2 (car (dom-by-tag wx/firsttaf  'station_id))))
  (setq wx/tafs/forecasts (dom-by-tag wx/firsttaf 'forecast))
  (dolist (f wx/tafs/forecasts)
    (add-to-list 'tabulated-list-entries (wx/format_taf_forecast f station) ) )
  
  (dolist (m wx/metars)
    (add-to-list 'tabulated-list-entries (wx/format_metar m)) )

  (tabulated-list-init-header)
  (tabulated-list-print)
  )




(define-derived-mode wx/mode tabulated-list-mode "Weather"
  "Weather mode"
  ;; (setq mode-line-process '((package--downloads-in-progress ":Loading")
  ;;                           (package-menu--transaction-status
  ;;                            package-menu--transaction-status)))
  (setq tabulated-list-format
        `[("Location"  8  nil)
          ("Date & Time" 11  t)
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
  (setq tabulated-list-sort-key '("Date & Time" . 'from-most-recent))
  (setq wx/buf (get-buffer-create "*Weather*"))
  (use-local-map wx/keymap)
  (setq revert-buffer-function 'wx/refresh-contents)
  
  ;;(setf imenu-prev-index-position-function #'package--imenu-prev-index-position-function)
  ;;(setf imenu-extract-index-name-function #'package--imenu-extract-index-name-function)
  )


(defun wx/kill            (&optional whatever)  (interactive) (kill-buffer wx/buf)  )
(defun wx/metars/show-xml (&optional whatever)  (interactive) (switch-to-buffer wx/metars/xmlbuf) )
(defun wx/tafs/show-xml   (&optional whatever)  (interactive) (switch-to-buffer wx/tafs/xmlbuf) )


(defun wx/force-refresh (&optional whatever)
  (interactive)
  (progn
    (message "Weather: user forced information refresh from remote server...")
    (setq wx/metars nil)
    (setq wx/tafs   nil)
    (wx/refresh-contents)
    (tabulated-list-print t))
  )

(defvar wx/keymap nil "Keymap the Weather mode")

(defun wx (&optional no-fetch)
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
  (setq wx/buf (get-buffer-create "*Weather*"))
  (set-buffer wx/buf)

  (setq wx/keymap (make-sparse-keymap))
  (set-keymap-parent wx/keymap tabulated-list-mode-map)
  (define-key wx/keymap (kbd "G") 'wx/observed/force-refresh)
  (define-key wx/keymap (kbd "q") 'wx/kill)
  (define-key wx/keymap (kbd "x") 'wx/metars/show-xml)
  (define-key wx/keymap (kbd "X") 'wx/tafs/show-xml)

  (define-key wx/keymap (kbd "1") 'delete-other-windows)

  (setq buffer-file-coding-system 'utf-8)
  (wx/refresh-contents)
  (wx/mode)
  ;;(tabulated-list-print)
  (switch-to-buffer wx/buf)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(wx)

