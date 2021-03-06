;; Emacs Weather Mode
;;
;; Daniele P. Scarpazza 
;; 2021-07-25

(require 'xml)
(require 'dom)
(require 'all-the-icons)
(require 'iso8601)
(require 'timezone)
(require 'solar)


;; to-do
;; - finish more metar wx codes
;; done
;; computes day and night from ephemeres

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
  :require 'wx
  :group 'Weather)

(defcustom wx/taf/url_template
  "https://www.aviationweather.gov/adds/dataserver_current/httpparam?dataSource=tafs&requestType=retrieve&format=xml&stationString=%s&hoursBeforeNow=%i" 
  "This is the URL template to access the TAF (weather forecasts) from the National Weather Service. Don't fiddle with it unless you know well what you are doing."
  :type 'string
  :require 'wx
  :group 'Weather)


(defcustom wx/nearby/url_template
  "https://www.aviationweather.gov/adds/dataserver_current/httpparam?dataSource=stations&requestType=retrieve&format=xml&radialDistance=%i;%.2f,%.2f"
  "This is the URL template to query the National Weather Service for the closest weather stations to a given longitude/latitude. Don't fiddle with it unless you know well what you are doing."
  :type 'string
  :require 'wx
  :group 'Weather
  )



(defcustom wx/metar/closest_airport_to_home  "KJFK"
  "Four-letter FAA identifier of the airport you are fetching weather observations from. This might be a smaller and closer airport than the one for which you request forecasts."
  :type 'string
  :require 'wx
  :group 'Weather)

(defcustom wx/taf/closest_airport_to_home  "KJFK"
  "Four-letter FAA identifier of the airport you are requesting weather forecasts for. This might be a larger and further away from home airport than the one where you get observations from."
  :type 'string
  :require 'wx
  :group 'Weather)


(defcustom wx/metar/look_back_hours 18
  "How many hours are you interested in looking back when requesting observed weather."
  :type 'integer
  :require 'wx
  :group 'Weather)

(defcustom wx/taf/look_back_hours 2
  "How many hours of weather forecasts are you requesting. We recommend you don't change this value. Usually only the last forecast is immediately useful."
  :type 'integer
  :require 'wx
  :group 'Weather)

(defcustom wx/nearby/radial_distance 35
  "How far away from your current position we are considering weather stations. Increase this value if you live in a sparse area and don't have many airports around you."
  :type 'integer
  :require 'wx
  :group 'Weather)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq wx/metars nil)
(setq wx/tafs nil)
(setq wx/nearby/data nil)

(defvar wx/home_lon)
(defvar wx/home_lat)

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
       '( "dust"         ("DS" "PO" "DU" "SS" "BLDU" "BLSA") )
       '( "volcano"      ("VA") )
       '( "hail"         ("GS" "GR" "SP") )
       '( "snow"         ("SN"  "PL"  "SG") )
       '( "snow-wind"    ("BLSN") )
       '( "showers"      ("RA" "+RA" "VCSH" "SHRA") )
       '( "thunderstorm"          ("VCTS" "TSRA" "TSGS") )
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


(defun wx/fake_raw_sky_condition (conds)
  (setq result "")
  (dolist (c conds)
    (setq cov "???")
    (setq alt "???")
    (dolist (attribute (car (cdr c)))
      (progn        
        (when (string= (car attribute) "sky_cover")         (setq cov (cdr attribute)))
        (when (string= (car attribute) "cloud_base_ft_agl") (setq alt (string-to-number (cdr attribute)))))
      )
    (setq result (format "%s %s%03i" result cov (/ alt 100)) ) 
    )
  result
  )

(defun wx/determine_ceiling (conds)
  "Recreate a raw string depicting sky condition from the TAF DOM"
  (setq cig_coverage nil)
  (setq cig_altitude 999999)

  (dolist (c conds)
    (dolist (attribute (car (cdr c)))
       (progn
         (when (string= (car attribute) "sky_cover")         (setq cov (cdr attribute)))
         (when (string= (car attribute) "cloud_base_ft_agl") (setq alt (string-to-number (cdr attribute)))))
       )

    (when (and (or (equal cov "OVC") (equal cov "BKN"))
               (< alt cig_altitude))
      (progn
        (setq cig_coverage cov)
        (setq cig_altitude alt)
        )
      )
    )
  (cons cig_coverage cig_altitude)
  )

(defun wx/colorize_flight_category (name)
  (propertize name 'font-lock-face
              (if     (equal name "LIFR")  '(:foreground "magenta")
                (if   (equal name "IFR")   '(:foreground "red")
                  (if (equal name "MVFR")  '(:foreground "blue")
                    '(:foreground "green"))))))


(defun wx/determine_flight_category (vis_sm ceiling_ft)
  (if (> vis_sm 0)
      (wx/colorize_flight_category
       (if     (or (< vis_sm 1) (< ceiling_ft 500))   "LIFR" 
         (if   (or (< vis_sm 3) (< ceiling_ft 1000))  "IFR"  
           (if (or (< vis_sm 5) (< ceiling_ft 3000))  "MVFR" 
             "VFR" 
             ))))
    "-"
    )
  )


(defun wx/taf/format_forecast (f station sunrise_t sunset_t)
  "Returns a tabulated entry representing one TAF forecast"
  (let* (
         (raw_time_from    (nth 2 (car (dom-by-tag f 'fcst_time_from))))
         (raw_time_to      (nth 2 (car (dom-by-tag f 'fcst_time_to))))
         (from_time8601    (utc_to_local (iso8601-parse  raw_time_from)) )         
         (to_time8601      (utc_to_local (iso8601-parse  raw_time_to)) )         
         (wind_block       (dom-by-tag f 'wind_speed_kt))
         (wind_speed       (if wind_block (string-to-number (nth 2 (car wind_block))) -1))
         (wx_string        (nth 2 (car (dom-by-tag f 'wx_string))))
         (change_indicator (nth 2 (car (dom-by-tag f 'change_indicator))))
         (visibility_str (nth 2 (car (dom-by-tag f 'visibility_statute_mi))))
         (visibility_sm  (if visibility_str (string-to-number visibility_str) -1))
         (sky_conditions (dom-by-tag f 'sky_condition))
         (ceiling_pair   (wx/determine_ceiling sky_conditions))
         (cig_coverage   (car ceiling_pair))
         (cig_altitude   (cdr ceiling_pair))         
         (is_day         (and (>= (decoded-time-hour from_time8601) 6)
                              (<= (decoded-time-hour from_time8601) 17))) )
    (list from_time8601
          (vector station
                  (format "%02i/%02i %02i:%02i%s%02i:%02i" (decoded-time-month   from_time8601) (decoded-time-day   from_time8601)
                          (decoded-time-hour   from_time8601) (decoded-time-minute from_time8601)
                          (all-the-icons-octicon "arrow-right")
                          (decoded-time-hour   to_time8601) (decoded-time-minute to_time8601) ) 
                  "F"
                  (propertize (concat (wx/process_present_weather wx_string) (process_sky_cover sky_conditions (wx/is_day from_time8601 sunrise_t sunset_t)))) 
                  "" ;; temperature
                  "" ;; humidity
                  "" ;; pressure
                  (if (< wind_speed 0) "N/A" (format "%3.0f %s" wind_speed  (propertize "kts" 'font-lock-face '(:height 0.7))))
                  (wx/determine_flight_category visibility_sm (if cig_altitude cig_altitude 10000))
                  (if (> visibility_sm 0) (format "%2.0f SM" visibility_sm) "N/A")
                  (if cig_coverage (format "%s %4.1fk ft" cig_coverage (/ cig_altitude 1000.0)) "None" )
                  (format "Forecast %s %s" change_indicator wx_string (wx/fake_raw_sky_condition sky_conditions ))
          )
    )
  )
)


(defun wx/is_day (time8601 sunrise_t sunset_t)
  (let ((now (+ (decoded-time-hour time8601) (/ (decoded-time-minute time8601) 60.0))))
    (and (>= now sunrise_t) (<= now sunset_t))
    )
  )

(defun wx/metar/format (m sunrise_t sunset_t)
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
         (wind_block     (dom-by-tag m 'wind_speed_kt))
         (wind_speed     (if wind_block (string-to-number (nth 2 (car wind_block))) -1))
         (ceiling_pair   (wx/determine_ceiling sky_conditions))
         (cig_coverage   (car ceiling_pair))
         (cig_altitude   (cdr ceiling_pair))
         )
      (list time8601
          (vector station
                  (format "%02i/%02i %02i:%02i" (decoded-time-month   time8601) (decoded-time-day   time8601)
                          (decoded-time-hour   time8601) (decoded-time-minute time8601) )
                  (if (string= metar_type "SPECI")
                      (all-the-icons-octicon "alert")
                    " ")
                  (concat (wx/process_present_weather wx_string) (process_sky_cover sky_conditions (wx/is_day time8601 sunrise_t sunset_t)))
                  (format "%.0f??C" tempC)
                  (format "%.0f%%"  rh)
                  (if p_mbar (format "%4.0f %s" (string-to-number p_mbar)
                                     (propertize "mbar" 'font-lock-face '(:height 0.7)) ) "")
                  (if (< wind_speed 0) "N/A" (format "%3.0f %s" wind_speed
                                                     (propertize "kts" 'font-lock-face '(:height 0.7))))
                  (wx/colorize_flight_category flight_category)
                  (format "%2.0f SM" visibility_sm)
                  (if cig_coverage (format "%s %4.1fk ft" cig_coverage (/ cig_altitude 1000.0)) "None" )
                  (format "%s" raw)
          )
    ) )
  )


(defun wx/propertize-icon (icon)
  (add-face-text-property   0 (length icon)   :inherit t icon)  icon)



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
          )
        
        )
    )

  (setq wx/firsttaf        (car wx/tafs))            
  (setq station            (nth 2 (car (dom-by-tag wx/firsttaf  'station_id))))
  (setq now                (decode-time (current-time)))
  (setq calendar-latitude  (string-to-number (nth 2 (car (dom-by-tag wx/firsttaf  'latitude)))))
  (setq calendar-longitude (string-to-number (nth 2 (car (dom-by-tag wx/firsttaf  'longitude)))))
  (setq calendar-time-zone  (/ (car (current-time-zone)) 60))
  (setq sss (solar-sunrise-sunset (calendar-current-date)))
  (setq sunrise_t (nth 0 (nth 0 sss)))
  (setq sunset_t  (nth 0 (nth 1 sss)))
  (setq sunrise_h   (truncate sunrise_t))
  (setq sunrise_min (round (* 60 (- sunrise_t sunrise_h))))
  (setq sunset_h    (truncate sunset_t))
  (setq sunset_min  (round (* 60 (- sunset_t sunset_h))))

  (add-to-list 'tabulated-list-entries
               (list "Sunset"
                     (vector station
                             (format "%02i/%02i %02i:%02i" (decoded-time-month now) (decoded-time-day now) sunset_h sunset_min)

                             (wx/propertize-icon (all-the-icons-wicon "sunset" :face 'all-the-icons-orange ))
                             "" "" "" "" "" "" "" ""
                             (propertize "Sunset" 'font-lock-face '(:foreground "orange"))
                             ) ) )
  (add-to-list 'tabulated-list-entries
               (list "Sunrise"
                     (vector station
                             (format "%02i/%02i %02i:%02i" (decoded-time-month now) (decoded-time-day now) sunrise_h sunrise_min)
                             (wx/propertize-icon (all-the-icons-wicon "sunrise" :face 'all-the-icons-yellow ))
                             "" "" "" "" "" "" "" ""
                             (propertize "Sunrise" 'font-lock-face '(:foreground "yellow")))) ) 
    
  (setq wx/tafs/forecasts (dom-by-tag wx/firsttaf 'forecast))
  (dolist (f wx/tafs/forecasts)
    (add-to-list 'tabulated-list-entries (wx/taf/format_forecast f station sunrise_t sunset_t) ) )
  
  (dolist (m wx/metars)
    (add-to-list 'tabulated-list-entries (wx/metar/format m sunrise_t sunset_t)) )

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
          ("Date & Time" 17  t)
          ("!"         3  t)
          ("Weather"  10  nil)
          ("Temp"      4  t)
          ("Humid"     5  t)
          ("Pressure" 10  t)
          ("Wind"      9  t)
          ("Flight"    6  t)
          ("Vis'ty"    7  t)          
          ("Ceiling"  15  t)          
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
  (define-key wx/keymap (kbd "G") 'wx/force-refresh)
  (define-key wx/keymap (kbd "q") 'wx/kill)
  (define-key wx/keymap (kbd "x") 'wx/metars/show-xml)
  (define-key wx/keymap (kbd "X") 'wx/tafs/show-xml)

  (define-key wx/keymap (kbd "1") 'delete-other-windows)

  (setq buffer-file-coding-system 'utf-8)
  (wx/refresh-contents)
  (wx/mode)
  (switch-to-buffer wx/buf)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(wx)



(define-derived-mode wx/nearby/mode tabulated-list-mode "Weather/Nearby"
  "Weather mode"
  ;; (setq mode-line-process '((package--downloads-in-progress ":Loading")
  ;;                           (package-menu--transaction-status
  ;;                            package-menu--transaction-status)))
  (setq tabulated-list-format
        `[("Identifier"  11  t)
          ("Distance"     9  t)
          ("WMO ID"       6  t)
          ("Latitude"    10  t)
          ("Longitude"   10  t)
          ("Elevation"   10  t)
          ("Site"        30  t)
          ("State"        6  t)
          ("Country"      7  t)
          ("METAR"        5  t)
          ("TAF"          5  t)          
          ("Comments"    20 7)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Distance" . nil))
  (setq wx/nearby/buf (get-buffer-create "*Weather / Nearby*"))
  (use-local-map wx/keymap)
  (setq revert-buffer-function 'wx/nearby/refresh-contents)  
  )

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))


(defun wx/nearby/refresh-contents (&optional _arg _noconfirm)
  ;;(wx/ensure-wx-mode)
  (setq tabulated-list-entries '())
  
  (dolist (s (dom-by-tag  (nth 0 wx/nearby/data) 'station))
    (add-to-list 'tabulated-list-entries (wx/nearby/format_station s) )
    )
  (tabulated-list-init-header)
  (tabulated-list-print)
  )



(defun wx/haversine_distance_nm (lat1 lon1 lat2 lon2)
  (let* ( ( R      3440.1 )
          ( pi_180 (/ float-pi 180.0 ))
          ( ??1     (* lat1 pi_180 ))
          ( ??2     (* lat2 pi_180 ))
          ( ????     (* (- lat2 lat1) pi_180 ))
          ( ????     (* (- lon2 lon1) pi_180 ))
          ( a      (+ (expt (sin (/ ???? 2)) 2)
                      (* (cos ??1) (cos ??2) (expt (sin (/ ???? 2)) 2))) )
          ( c      (* 2 (atan (/ (sqrt a) (sqrt (- 1 a))))))
          ( d      (* R c))
          )
    d
    )
  )



(defun wx/nearby/format_station (d)
  "Returns a tabulated entry representing one nearby weather station"
  (let* (
         (station_id     (nth 2 (car (dom-by-tag d 'station_id))))
         (wmo_id         ( if (dom-by-tag d 'wmo_id)
                             (nth 2 (car (dom-by-tag d 'wmo_id)))
                           "-" ))
         (station_lat    (string-to-number (nth 2 (car (dom-by-tag d 'latitude)))))
         (station_lon    (string-to-number (nth 2 (car (dom-by-tag d 'longitude)))))
         (distance_nm    (wx/haversine_distance_nm station_lat station_lon home_lat home_lon))
         (elevation_ft   (* 3.28084 (string-to-number (nth 2 (car (dom-by-tag d 'elevation_m))))))
         (site_name      (nth 2 (car (dom-by-tag d 'site))))
         (state          (nth 2 (car (dom-by-tag d 'state))))
         (country        (nth 2 (car (dom-by-tag d 'country))))
         )    
    (list station_id
          (vector station_id
                  (format "%.2f NM" distance_nm)
                  wmo_id
                  (format "%.2f" station_lat)
                  (format "%.2f" station_lon)
                  (format "%7.0f ft"  elevation_ft)
                  site_name state country "to-do" "to-do" "to-do")
          )
      )
  )



(defun wx-lookup-zip (&optional no-fetch)
  "Look up the closest weather station to your zip."
  (interactive "P")

  (if (not wx/nearby/data)
    (progn 
      (setq zipcode (read-from-minibuffer "Enter your zipcode: "))
      (message (format "user specified zipcode: %s" zipcode) )
      (setq ziptable   (read-lines "~/development/wx/zip-codes.csv"))
      (setq zip_entry  (split-string (seq-find (lambda (x) (string-prefix-p zipcode x)) ziptable) "," ))
      (setq home_lat   (string-to-number (nth 1 zip_entry)))
      (setq home_lon   (string-to-number (nth 2 zip_entry)))
      (setq url (format wx/nearby/url_template wx/nearby/radial_distance home_lon home_lat ))
      (setq wx/nearby/data (request_dom url 'data))
      ))

 ;(switch-to-buffer buf)
  (setq tabulated-list-entries '())
  (setq wx/nearby/buf (get-buffer-create "*Weather / Nearby*"))
  (set-buffer wx/nearby/buf)
  (setq wx/keymap (make-sparse-keymap))
  (set-keymap-parent wx/keymap tabulated-list-mode-map)
  (define-key wx/keymap (kbd "1") 'delete-other-windows)
  (setq buffer-file-coding-system 'utf-8)
  (wx/nearby/refresh-contents)
  (wx/nearby/mode)
  (print tabulated-list-entries)
  (switch-to-buffer wx/nearby/buf)
  )


(setq wx/nearby/data nil)
(wx-lookup-zip)






