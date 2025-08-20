;;; guixcn-channel -- Guix 中文社区特供软件频道
;;; Copyright © 2023 Zheng Junjie <z572@z572.online>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guixcn packages systemd)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages apparmor)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)                ;intltool
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public systemd
  (package
    (name "systemd")
    (version "254")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/systemd/systemd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xdw8zdayhz2pabfn89almp5ajc57v2vcqkgh9y9csbi518aqvr2"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:tests? #f ;; TODO: 37 fail
      #:configure-flags
      #~(let* ((out #$output)
               (sysconf (string-append out "/etc"))
               (rootpkglibdir (string-append out "/lib/systemd"))
               (dbuspolicy (string-append out "/etc/dbus-1/system.d"))
               (kexec-tools #$(this-package-input "kexec-tools"))
               (shadow #$(this-package-input "shadow"))
               (kexec-path (string-append kexec-tools "/sbin/kexec"))
               (nologin-path (string-append shadow "/sbin/nologin")))
          (list
           "-Dinstall-sysconfdir=false"
           (string-append "-Drootprefix=" out)
           (string-append "-Dsysconfdir=" sysconf)
           ;; (string-append "-Drootlibexecdir=" libexec)
           (string-append "-Ddbuspolicydir=" dbuspolicy)
           (string-append "-Dc_link_args=-Wl,-rpath=" rootpkglibdir)
           (string-append "-Dcpp_link_args=-Wl,-rpath=" rootpkglibdir)
           ;; (string-append "-Dhalt-path=" halt-path)
           (string-append "-Dkexec-path=" kexec-path)
           "-Dsysvinit-path="
           "-Dsysvrcnd-path="
           "-Dcreate-log-dirs=false"
           "-Dhwdb=false"
           "-Dmode=release"
           "-Drpmmacrosdir=no"
           "-Dsbat-distro='Guix System'"
           "-Dsbat-distro-summary='guix'"
           "-Dsbat-distro-url='https://lists.gnu.org/mailman/listinfo/bug-guix'"
           "-Dbootloader=true"
           (string-append "-Dnologin-path=" nologin-path)
           ;; "-Dcgroup-controller=elogind"
           "-Dman=true"
           ;; Disable some tests.
           "-Dslow-tests=false"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pkttyagent-path
            (lambda _
              (substitute* "meson.build"
                (("join_paths\\(bindir, 'pkttyagent'\\)")
                 "'\"/run/current-system/profile/bin/pkttyagent\"'"))))
          (add-after 'unpack 'adjust-tests
            (lambda _
              ;; Skip the following test, which depends on users such as 'root'
              ;; existing in the build environment.
              (invoke "sed" "/src\\/test\\/test-user-util.c/,+2s/^/#/g"
                      "-i" "src/test/meson.build")
              ;; This test tries to copy some bytes from /usr/lib/os-release,
              ;; which does not exist in the build container.  Choose something
              ;; more likely to be available.
              (substitute* "src/test/test-copy.c"
                (("/usr/lib/os-release")
                 "/etc/passwd"))
              (substitute* '("src/test/test-copy.c"
                             "src/test/test-xattr-util.c")
                (("/var/tmp")
                 "/tmp"))
              ;; Use a shebang that works in the build container.
              (substitute* "src/test/test-exec-util.c"
                (("#!/bin/sh")
                 (string-append "#!" (which "sh"))))
              (substitute* '("src/test/test-cgroup-util.c"
                             "src/test/test-cgroup.c")
                ;; skip test
                (("r == -ENOMEDIUM") "1"))
              ;; Do not look for files or directories that do not exist.
              (substitute* "src/test/test-fs-util.c"
                (("usr") "etc")
                (("/etc/machine-id") "/etc/passwd")
                (("/bin/sh") (which "sh")))
              ;; (substitute* "src/test/test-fileio.c"
              ;;   (("\"/etc/nsswitch.conf\",") "")
              ;;   (("\"/proc/kcore\",") "")
              ;;   (("\"/sys/kernel/uevent_seqnum\",") ""))
              (substitute* '("src/test/test-execve.c")
                (("/bin/true") (which "true")))
              (substitute* "src/libsystemd/sd-bus/test-bus-creds.c"
                ;; skip test
                (("\\(cg_unified\\(\\) == -ENOMEDIUM\\)") "(true)"))
              (substitute* '("src/test/test-load-fragment.c"
                             "src/test/test-fileio.c"
                             "src/test/test-env-file.c"
                             "src/analyze/test-verify.c"
                             "test/units/testsuite-15.sh"
                             "test/testsuite-03.units/hello.service"
                             "test/testsuite-03.units/unstoppable.service"
                             "test/testsuite-06.units/hola.service"
                             "test/testsuite-23.units/testsuite-23-retry-upheld.service"
                             "test/test-execute/exec-systemcallfilter-failing3.service"
                             "test/test-execute/exec-systemcallfilter-failing2.service"
                             "test/test-execute/exec-systemcallfilter-failing.service"
                             "test/test-udev.py")
                (("/bin/echo") (which "echo")))
              (substitute* "src/test/test-mountpoint-util.c"
                (("assert_se\\(path_is_mount_point\\(\"/sys.*")
                 ""))
              ;; /bin/sh does not exist in the build container.
              (substitute* "src/test/test-path-util.c"
                (("/bin/sh") (which "sh")))))
          (add-after 'unpack 'no-create-/var/lib/systemd
            (lambda _
              (substitute* "meson.build"
                (("meson.add_install_script\\('sh', '-c', mkdir_p\\.format\\(systemdstatedir\\)\\)")
                 ""))))
          (add-after 'install 'remove-99-environment.conf
            (lambda _
              (delete-file (string-append #$output "/lib/environment.d/99-environment.conf")))))))
    (native-inputs
     (list
      docbook-xml-4.5
      docbook-xml-4.2
      docbook-xsl
      gettext-minimal
      gperf
      libxml2
      m4
      pkg-config
      python
      libxslt
      python-jinja2
      glib))
    (inputs
     (list kexec-tools
           python-pyelftools
           linux-pam
           libidn2
           cryptsetup
           passwdqc
           libpwquality
           libmicrohttpd
           libgcrypt
           libseccomp
           libselinux
           libxcrypt
           libapparmor
           libbpf
           libcap
           libxkbcommon
           curl
           pcre2
           gobject-introspection
           p11-kit
           kmod
           audit
           lz4
           xz
           bzip2
           zstd
           qrencode
           util-linux
           (list util-linux "lib")
           gnutls
           openssl
           shadow         ; for 'nologin'
           acl))         ; to add individual users to ACLs on /dev nodes
    (home-page "https://github.com/systemd/systemd")
    (synopsis "System and Service Manager")
    (description "The systemd System and Service Manager")
    (license license:lgpl2.1+)))
