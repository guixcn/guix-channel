(define-module (guixcn packages emacs)
  #:use-module (gnu packages emacs)
  #:use-module (guix build utils)
  #:use-module (guix build-system emacs)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

;; TODO: 如果这个包活跃开发，就发到 guix 仓库
(define-public emacs-llvm-mode
  (let ((commit "96b7e911c4fc292fdde9cc100a446065aa7dd052")
        (revision "0"))
    (package
      (name "emacs-llvm-mode")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nverno/llvm-mode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "186niw4azgdpyj471cn24vjf7svwqa4k3svdf0v1i1pwig59qjv1"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/nverno/llvm-mode")
      (synopsis "Major mode for the LLVM IR language")
      (description
       "This package provides a improved emacs major-mode for LLVM IR source.")
      (license license:gpl3+))))
