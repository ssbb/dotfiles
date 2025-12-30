(define-module (ssbb services pm)
  #:use-module (ssbb packages pm)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:export (throttled-configuration
            throttled-configuration?
            throttled-service-type))

(define (non-negative-integer? val)
  (and (exact-integer? val) (not (negative? val))))

(define (non-negative-number? val)
  (and (number? val) (not (negative? val))))

(define-maybe/no-serialization string)

(define-configuration throttled-configuration
  (throttled
   (file-like throttled)
   "The throttled package.")

  (enabled?
   (boolean #t)
   "Enable or disable the script execution.")

  (sysfs-power-path
   (string "/sys/class/power_supply/AC*/online")
   "SYSFS path for checking if the system is running on AC power.")

  (autoreload?
   (boolean #t)
   "Auto reload config on changes.")

  ;; Battery settings
  (bat-update-rate-s
   (non-negative-integer 30)
   "Update the registers every this many seconds (battery).")

  (bat-pl1-tdp-w
   (non-negative-integer 29)
   "Max package power for time window #1 (battery).")

  (bat-pl1-duration-s
   (non-negative-integer 28)
   "Time window #1 duration (battery).")

  (bat-pl2-tdp-w
   (non-negative-integer 44)
   "Max package power for time window #2 (battery).")

  (bat-pl2-duration-s
   (non-negative-number 0.002)
   "Time window #2 duration (battery).")

  (bat-trip-temp-c
   (non-negative-integer 85)
   "Max allowed temperature before throttling (battery).")

  (bat-ctdp
   (non-negative-integer 0)
   "Set cTDP: normal=0, down=1, up=2 (battery).")

  (bat-disable-bdprochot?
   (boolean #f)
   "Disable BDPROCHOT (battery).")

  ;; AC settings
  (ac-update-rate-s
   (non-negative-integer 5)
   "Update the registers every this many seconds (AC).")

  (ac-pl1-tdp-w
   (non-negative-integer 44)
   "Max package power for time window #1 (AC).")

  (ac-pl1-duration-s
   (non-negative-integer 28)
   "Time window #1 duration (AC).")

  (ac-pl2-tdp-w
   (non-negative-integer 44)
   "Max package power for time window #2 (AC).")

  (ac-pl2-duration-s
   (non-negative-number 0.002)
   "Time window #2 duration (AC).")

  (ac-trip-temp-c
   (non-negative-integer 95)
   "Max allowed temperature before throttling (AC).")

  (ac-hwp-mode?
   (boolean #f)
   "Set HWP energy performance hints to 'performance' on high load (AC).")

  (ac-ctdp
   (non-negative-integer 0)
   "Set cTDP: normal=0, down=1, up=2 (AC).")

  (ac-disable-bdprochot?
   (boolean #f)
   "Disable BDPROCHOT (AC).")

  ;; Undervolt settings (battery)
  (bat-uv-core
   (non-negative-integer 0)
   "CPU core voltage offset in mV (battery).")

  (bat-uv-gpu
   (non-negative-integer 0)
   "Integrated GPU voltage offset in mV (battery).")

  (bat-uv-cache
   (non-negative-integer 0)
   "CPU cache voltage offset in mV (battery).")

  (bat-uv-uncore
   (non-negative-integer 0)
   "System Agent voltage offset in mV (battery).")

  (bat-uv-analogio
   (non-negative-integer 0)
   "Analog I/O voltage offset in mV (battery).")

  ;; Undervolt settings (AC)
  (ac-uv-core
   (non-negative-integer 0)
   "CPU core voltage offset in mV (AC).")

  (ac-uv-gpu
   (non-negative-integer 0)
   "Integrated GPU voltage offset in mV (AC).")

  (ac-uv-cache
   (non-negative-integer 0)
   "CPU cache voltage offset in mV (AC).")

  (ac-uv-uncore
   (non-negative-integer 0)
   "System Agent voltage offset in mV (AC).")

  (ac-uv-analogio
   (non-negative-integer 0)
   "Analog I/O voltage offset in mV (AC).")

  (no-serialization))

(define (bool->python bool)
  "Convert Scheme boolean to Python boolean string."
  (if bool "True" "False"))

(define (generate-throttled-config config)
  "Generate throttled.conf content from configuration record."
  (string-append
   "[GENERAL]\n"
   "Enabled: " (bool->python (throttled-configuration-enabled? config)) "\n"
   "Sysfs_Power_Path: " (throttled-configuration-sysfs-power-path config) "\n"
   "Autoreload: " (bool->python (throttled-configuration-autoreload? config)) "\n"
   "\n"
   "[BATTERY]\n"
   "Update_Rate_s: " (number->string (throttled-configuration-bat-update-rate-s config)) "\n"
   "PL1_Tdp_W: " (number->string (throttled-configuration-bat-pl1-tdp-w config)) "\n"
   "PL1_Duration_s: " (number->string (throttled-configuration-bat-pl1-duration-s config)) "\n"
   "PL2_Tdp_W: " (number->string (throttled-configuration-bat-pl2-tdp-w config)) "\n"
   "PL2_Duration_S: " (number->string (throttled-configuration-bat-pl2-duration-s config)) "\n"
   "Trip_Temp_C: " (number->string (throttled-configuration-bat-trip-temp-c config)) "\n"
   "cTDP: " (number->string (throttled-configuration-bat-ctdp config)) "\n"
   "Disable_BDPROCHOT: " (bool->python (throttled-configuration-bat-disable-bdprochot? config)) "\n"
   "\n"
   "[AC]\n"
   "Update_Rate_s: " (number->string (throttled-configuration-ac-update-rate-s config)) "\n"
   "PL1_Tdp_W: " (number->string (throttled-configuration-ac-pl1-tdp-w config)) "\n"
   "PL1_Duration_s: " (number->string (throttled-configuration-ac-pl1-duration-s config)) "\n"
   "PL2_Tdp_W: " (number->string (throttled-configuration-ac-pl2-tdp-w config)) "\n"
   "PL2_Duration_S: " (number->string (throttled-configuration-ac-pl2-duration-s config)) "\n"
   "Trip_Temp_C: " (number->string (throttled-configuration-ac-trip-temp-c config)) "\n"
   (if (throttled-configuration-ac-hwp-mode? config)
       "HWP_Mode: True\n"
       "")
   "cTDP: " (number->string (throttled-configuration-ac-ctdp config)) "\n"
   "Disable_BDPROCHOT: " (bool->python (throttled-configuration-ac-disable-bdprochot? config)) "\n"
   "\n"
   "[UNDERVOLT.BATTERY]\n"
   "CORE: -" (number->string (throttled-configuration-bat-uv-core config)) "\n"
   "GPU: -" (number->string (throttled-configuration-bat-uv-gpu config)) "\n"
   "CACHE: -" (number->string (throttled-configuration-bat-uv-cache config)) "\n"
   "UNCORE: -" (number->string (throttled-configuration-bat-uv-uncore config)) "\n"
   "ANALOGIO: -" (number->string (throttled-configuration-bat-uv-analogio config)) "\n"
   "\n"
   "[UNDERVOLT.AC]\n"
   "CORE: -" (number->string (throttled-configuration-ac-uv-core config)) "\n"
   "GPU: -" (number->string (throttled-configuration-ac-uv-gpu config)) "\n"
   "CACHE: -" (number->string (throttled-configuration-ac-uv-cache config)) "\n"
   "UNCORE: -" (number->string (throttled-configuration-ac-uv-uncore config)) "\n"
   "ANALOGIO: -" (number->string (throttled-configuration-ac-uv-analogio config)) "\n"))

(define (throttled-shepherd-service config)
  "Return a Shepherd service for throttled."
  (list
   (shepherd-service
    (documentation "Run throttled to fix Intel CPU throttling issues.")
    (provision '(throttled))
    (requirement '(user-processes))
    (start #~(make-forkexec-constructor
              (list #$(file-append (throttled-configuration-throttled config)
                                   "/bin/throttled")
                    "--config" "/etc/throttled.conf")
              #:environment-variables
              (list (string-append "PATH=/run/current-system/profile/bin:"
                                   "/run/current-system/profile/sbin")
                    "LINUX_MODULE_DIRECTORY=/run/booted-system/kernel/lib/modules"
                    "SSL_CERT_DIR=/etc/ssl/certs")
              #:log-file "/var/log/throttled.log"))
    (stop #~(make-kill-destructor)))))

(define (throttled-activation config)
  "Create /etc/throttled.conf on system activation."
  (let ((config-file (plain-file "throttled.conf"
                                 (generate-throttled-config config))))
    (with-imported-modules '((guix build utils))
                           #~(begin
                               (use-modules (guix build utils))
                               (copy-file #$config-file "/etc/throttled.conf")))))

(define throttled-service-type
  (service-type
   (name 'throttled)
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        throttled-shepherd-service)
     (service-extension activation-service-type
                        throttled-activation)))
   (default-value (throttled-configuration))
   (description
    "Run throttled, a tool to fix Intel CPU throttling issues on Linux.
It overrides power limits in MSR and MCHBAR registers to prevent the
Embedded Controller from throttling the CPU.")))
