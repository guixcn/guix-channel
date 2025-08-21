;;; guixcn-channel -- Guix 中文社区特供软件频道
;;; Copyright © 2025 Zephyr Du <elecleus@outlook.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guixcn services networking)
  #:use-module (guixcn system file-systems)
  #:use-module (guixcn packages networking)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (dae-service-type
            dae-service-configuration))

;;;
;;; dae
;;;

(define (file-object? val)
  (or (string? val)
      (file-like? val)))

(define-configuration/no-serialization dae-service-configuration
  (dae
   (file-like dae-bin)
   "dae package to be used")
  (config-file
   file-object
   "dae configuration")
  (assets-path
   (string "")
   "Directory containing geoip.dat and geosite.dat")
  (log-file
   (string "/var/log/dae.log")
   "The file to which dae’s standard output and standard error are redirected")
  (auto-start?
   (boolean #t)
   "Whether to auto start dae"))

(define dae-activation
  (match-record-lambda <dae-service-configuration>
      (dae assets-path)
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$assets-path))))

(define dae-shepherd-service
  (match-record-lambda
   <dae-service-configuration>
   (dae config-file assets-path
        log-file auto-start?)
   (let ((environment
          #~(list (string-append "DAE_LOCATION_ASSET="
                                 (if (string=? #$assets-path "")
                                     #$(file-append dae "/share/dae")
                                     #$assets-path)))))
     (list (shepherd-service
            (provision '(dae))
            (requirement '(user-processes
                           networking
                           root-file-system
                           file-system-/sys/fs/bpf))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append dae "/bin/dae")
                      "run"
                      "--config" #$config-file)
                #:environment-variables #$environment
                #:log-file #$log-file))
            (stop #~(make-kill-destructor))
            (auto-start? auto-start?))))))

(define dae-service-type
  (service-type
    (name 'dae)
    (extensions
     (list (service-extension activation-service-type
                              dae-activation)
           (service-extension shepherd-root-service-type
                              dae-shepherd-service)
           (service-extension file-system-service-type
                              (const (list bpf-file-system)))))
    (description "Service for dae, a proxy solution using eBPF.")))
