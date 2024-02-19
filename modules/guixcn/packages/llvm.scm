(define-module (guixcn packages llvm)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:))

(define-public wllvm
  (package
    (name "wllvm")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wllvm" version))
       (sha256
        (base32 "0cf31hixzq5bzkxv91rvadlhrpxzy934134scv4frj85bxbpl19y"))))
    (build-system pyproject-build-system)
    (home-page "https://github.com/SRI-CSL/whole-program-llvm")
    (synopsis "Whole Program LLVM")
    (description "Whole Program LLVM")
    (license license:expat)))
