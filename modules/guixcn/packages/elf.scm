;;; guixcn-channel -- Guix 中文社区特供软件频道
;;; Copyright © 2025 Zhu Zihao <all_but_last@163.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guixcn packages elf)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)

  #:use-module (guix build-system meson)

  #:use-module (gnu packages elf))

(define-public nix-ld
  (package
    (name "nix-ld")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Mic92/nix-ld")
             (commit version)))
       (sha256
        (base32 "0jybbs9hr5rgqs17b0fswmxvfb0h3gbb4vqzh2b4mjnc5zk98m8f"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dnix-system="
                             #$(or (and=> (%current-target-system)
                                          gnu-triplet->nix-system)
                                   (%current-system))))))
    (native-inputs (list patchelf))
    (home-page "https://github.com/Mic92/nix-ld")
    (synopsis "Run unpatched dynamic binaries on non-FHS systems")
    (description "Nix-ld is a dynamic linker to execute unpatched binaries on
non-FHS systems.")
    (license license:expat)))
