;;; guixcn-channel -- Guix 中文社区特供软件频道
;;; Copyright © 2023 pat-hwaki
;;; Copyright © 2023 Zheng Junjie <z572@z572.online>
;;; Copyright © 2025 Zhu Zihao <all_but_last@163.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guixcn packages ebooks)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages webkit)
  #:use-module (guix build utils)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public foliate
  (let* ((commit "afc4b033e76904f1d20604640b2727a58a2e8106")
         (revision "0")
         (version* (git-version "3.2.1" revision commit))
         (foliate-js
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/johnfactotum/foliate-js")
                  (commit "cc2882ff396d27ca4133af2e8f2978f19c21846e")))
            (file-name
             (git-file-name "foliate-js" version*))
            (sha256
             (base32
              "0zhsk7h249dq3rcacchhk0jshksx7da26b909lnxwkk554243mjl")))))
    (package
      (name "foliate")
      (version version*)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/johnfactotum/foliate.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1g2jxa3jm0mfj4jx45gw5vq5rimdsyjym3lgw2dvhns7nb42ik8j"))))
      (build-system meson-build-system)
      (arguments
       (list #:glib-or-gtk? #t
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'copy-foliate-js
                   (lambda _
                     (copy-recursively #$foliate-js "src/foliate-js")))
                 (add-after 'unpack 'skip-gtk-update-icon-cache
                   (lambda _
                     (substitute* "meson.build"
                       (("gtk_update_icon_cache: true")
                        "gtk_update_icon_cache: false"))))
                 (add-after 'glib-or-gtk-wrap 'wrap-program
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((prog (string-append (assoc-ref outputs "out")
                                                "/bin/foliate")))
                       ;; Put existing typelibs before sushi's deps, so as to
                       ;; correctly infer gdk-pixbuf.
                       (wrap-program prog
                         `("GI_TYPELIB_PATH" suffix
                           (,(getenv "GI_TYPELIB_PATH")))
                         ;; for icon.
                         `("GDK_PIXBUF_MODULE_FILE" =
                           (,(getenv "GDK_PIXBUF_MODULE_FILE"))))))))))

      (native-inputs
       (list pkg-config
             `(,glib "bin")
             gettext-minimal
             gobject-introspection
             desktop-file-utils
             foliate-js))
      (inputs
       (list libadwaita
             glib
             glib-networking
             webkitgtk
             gtk
             gjs))
      (home-page "https://johnfactotum.github.io/foliate/")
      (synopsis "Read books in style")
      (description "A simple and modern GTK e-book reader")
      ;;             foliate       foliate-js
      (license (list license:gpl3 license:expat)))))
