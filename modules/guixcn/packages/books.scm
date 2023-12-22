;;; Copyright © 2023 pat-hwaki
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>

(define-module (guixcn packages books)
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
  (let* ((commit "efddb107ae3c3a7c3acca73ad0c6a19981234e04")
         (revision "0")
         (version* (git-version "3.0.0" revision commit))
         (foliate-js
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/johnfactotum/foliate-js")
                  (commit "ad25f6b5c3d9d044941d8ea858324b20a5352512")))
            (file-name
             (git-file-name "foliate-js" version*))
            (sha256
             (base32
              "0yfjyfhnd2nd3yln7r3h1pm08x5pv5kxvkh853s9nd66n3h6k1j4")))))
    (package
      (name "foliate")
      (version version*)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/johnfactotum/foliate.git")
                      (commit "efddb107ae3c3a7c3acca73ad0c6a19981234e04")))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1wicjwa8gdsjcbrj5g2bv88x0ddsf6ql2j8r7hp2p692l1bhb9dp"))))
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
