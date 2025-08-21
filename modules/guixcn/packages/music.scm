;;; guixcn-channel -- Guix 中文社区特供软件频道
;;; Copyright © 2025 Rivule Cedar <rivulet_cedar@yeah.net>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guixcn packages music)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages python)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public jianpu-ly
  (package
    (name "jianpu-ly")
    (version "1.860")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ssb22/jianpu-ly")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gffpp3f9g1pnd1hp4g03427zd7vlyi1dqxrq8fkxx0m9xq08c5f"))))
    (build-system copy-build-system)
    (inputs (list python-minimal))
    (arguments
     (list
      #:install-plan
      #~'(("jianpu-ly" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'rename-file
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (rename-file "./jianpu-ly.py" "./jianpu-ly") #t))
          (add-before 'install 'patch-shebang
            (lambda _
              (substitute* "jianpu-ly"
                (("#!/usr/bin/env python")
                 "#!/usr/bin/env python3"))))
          (add-after 'install 'set-executable-permissions
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin")))
                (chmod (string-append bin "/jianpu-ly") #o755)))))))
    (synopsis "在 Lilypond 音乐排版软件中打印简谱")
    (description
     "jianpu-ly 是一个 Python 程序（兼容 Python 2 和 Python 3），用于协助在 GNU 软件 Lilypond 中打印简谱（数字谱）。")
    (home-page "https://ssb22.user.srcf.net/mwrhome/jianpu-ly.html")
    (license license:asl2.0)))
