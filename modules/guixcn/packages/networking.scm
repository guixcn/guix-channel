;;; guixcn-channel -- Guix 中文社区特供软件频道
;;; Copyright © 2025 Zhu Zihao <all_but_last@163.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guixcn packages networking)
  #:use-module (guix base16)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public xray-bin
  (package
    (name "xray-bin")
    (version "25.6.8")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/XTLS/Xray-core/releases/download/v"
                           version "/Xray-linux-64.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base16-string->bytevector
         "51bcd3304fdbd64b58048b056da005fbaa6c83577fc351cae34024760e111e4b"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("xray" "bin/")
          ("." "share/xray-geodata" #:include ("dat")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-geodata
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (assets-dir (string-append #$output
                                                "/share/xray-geodata")))
                (wrap-program (string-append bin "/xray")
                  `("XRAY_LOCATION_ASSET" = (,assets-dir)))))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/XTLS/Xray-core")
    (synopsis "Binary version of Xray")
    (description
     "Xray-bin is platform for building proxies to bypass network restrictions.")
    (license license:mpl2.0)))
