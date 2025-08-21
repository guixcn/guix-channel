;;; guixcn-channel -- Guix 中文社区特供软件频道
;;; Copyright © 2025 Zephyr Du <elecleus@outlook.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guixcn system file-systems)
  #:use-module (gnu system file-systems)
  #:export (bpf-file-system))

(define bpf-file-system
  (file-system
   (device "none")
   (mount-point "/sys/fs/bpf")
   (type "bpf")
   (check? #f)
   (needed-for-boot? #f)
   (mount-may-fail? #t)
   (create-mount-point? #f)))
