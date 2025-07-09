;;; guixcn-channel -- Guix 中文社区特供软件频道
;;; Copyright © 2025 Zhu Zihao <all_but_last@163.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guixcn packages games)
  #:use-module (guix base16)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu packages bash)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages xorg))

(define-public hmcl
  (package
    (name "hmcl")
    (version "3.6.14")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/HMCL-dev/HMCL/releases/download/release-"
         version "/HMCL-" version ".jar"))
       (file-name (string-append name "-" version ".jar"))
       (sha256
        (base16-string->bytevector
         "f00be201800c9bbe2e25e32f80448f3476d3e5bf759834f10e02d887ef151dbf"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (srfi srfi-26))
      #:builder
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-26))
          (let* ((libdir (string-append #$output "/lib/hmcl"))
                 (filename (string-append #$name "-" #$version ".jar"))
                 (jar-dest (string-append libdir "/" filename))
                 (bindir (string-append #$output "/bin"))
                 (hmcl-bin (string-append bindir "/hmcl"))
                 (bash (string-append #$(this-package-input "bash-minimal")
                                      "/bin/bash"))
                 (jre (string-append #$(this-package-input "openjdk")
                                     "/bin/java"))
                 (libs
                  (list
                   #$@(map (lambda (name) (this-package-input name))
                           '("libx11"
                             "libxtst"
                             "glib"
                             "gtk+"
                             "mesa"
                             "pulseaudio"
                             "flite")))))
            (mkdir-p libdir)
            (mkdir-p bindir)
            (copy-file #$(package-source this-package) jar-dest)
            (call-with-output-file hmcl-bin
              (lambda (p)
                (format p "#!~a
exec -a \"$0\" \"~a\" -jar \"~a\" \"$@\""
                        bash jre jar-dest)))
            (chmod hmcl-bin #o555)
            (wrap-program hmcl-bin
              #:sh bash
              `("LD_LIBRARY_PATH" ":" prefix
                ,(map (cut string-append <> "/lib") libs)))))))
    (inputs (list bash-minimal
                  openjdk
                  ;; TODO: 使用系统 JavaFX
                  ;; 待决议：从源码编译 JavaFX 或下载 Maven 提供的 JAR
                  ;; JavaFX 依赖
                  libx11
                  libxtst
                  glib
                  gtk+
                  ;; Minecraft 运行时依赖
                  mesa
                  pulseaudio
                  flite))
    (home-page "https://hmcl.huangyuhui.net/")
    (synopsis "Multi-functional cross-platform Minecraft launcher")
    (description
     "HMCL(Hello Minecraft! Laucher) is a free, cross-platform Minecraft
launcher that supports following features and more

@itemize
@item Mod Management
@item Game Customizing
@item ModLoader Installing (Forge, NeoForge, Fabric, Quilt, LiteLoader, OptiFine)
@item Modpack Creating
@item UI Customization
@end itemize")
    (license license:gpl3)))
