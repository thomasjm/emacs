;;; weather-metno-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-weather-metno" "org-weather-metno.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-weather-metno.el
 (put 'org-weather-metno-query 'risky-local-variable t)
 (put 'org-weather-metno-format 'risky-local-variable t)

(autoload 'org-weather-metno "org-weather-metno" "\
Display weather in diary/‘org-mode’.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-weather-metno" '("org-weather-metno")))

;;;***

;;;### (autoloads nil "weather-metno" "weather-metno.el" (0 0 0 0))
;;; Generated autoloads from weather-metno.el

(autoload 'weather-metno-update "weather-metno" "\
Update weather data.

\(fn &optional LAT LON MSL)" t nil)

(autoload 'weather-metno-forecast "weather-metno" "\
Display weather forecast.
If NO-SWITCH is non-nil then do not switch to weather forecast buffer.

\(fn &optional NO-SWITCH)" t nil)

(autoload 'weather-metno-forecast-location "weather-metno" "\


\(fn LAT LON &optional MSL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weather-metno" '("weather-metno-")))

;;;***

;;;### (autoloads nil "weather-metno-mode-line" "weather-metno-mode-line.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from weather-metno-mode-line.el
 (put 'weather-metno-mode-line-string 'risky-local-variable t)

(defvar weather-metno-mode-line nil "\
Non-nil if Weather-Metno-Mode-Line mode is enabled.
See the `weather-metno-mode-line' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `weather-metno-mode-line'.")

(custom-autoload 'weather-metno-mode-line "weather-metno-mode-line" nil)

(autoload 'weather-metno-mode-line "weather-metno-mode-line" "\
Toggle weather forecast display in mode line.
With a prefix argument ARG, enable display if ARG is positive, and disable
it otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weather-metno-mode-line" '("weather-metno-mode-line-")))

;;;***

;;;### (autoloads nil "weather-metno-query" "weather-metno-query.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from weather-metno-query.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weather-metno-query" '("weather-metno-")))

;;;***

;;;### (autoloads nil nil ("weather-metno-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; weather-metno-autoloads.el ends here
