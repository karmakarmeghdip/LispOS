;;;; src/syscalls.lisp - Linux System Call Wrappers for LispOS

(in-package :lispos.sys)

;; Mount flags
(defconstant +ms-rdonly+ 1)
(defconstant +ms-nosuid+ 2)
(defconstant +ms-nodev+ 4)
(defconstant +ms-noexec+ 8)
(defconstant +ms-bind+ 4096)
(defconstant +ms-move+ 8192)

;; Umount flags
(defconstant +mnt-detach+ 2)

;; Network ioctl constants
(defconstant +siocsifflags+ #x8914)
(defconstant +siocgifflags+ #x8913)
(defconstant +siocsifaddr+ #x8916)
(defconstant +siocsifnetmask+ #x891c)

(defconstant +iff-up+ #x0001)
(defconstant +iff-broadcast+ #x0002)
(defconstant +iff-loopback+ #x0008)
(defconstant +iff-running+ #x0040)

;; AF_INET constant
(defconstant +af-inet+ 2)
(defconstant +sock-dgram+ 2)

;; Reboot magic constants
(defconstant +linux-reboot-magic1+ #xfee1dead)
(defconstant +linux-reboot-magic2+ 672274793)
(defconstant +linux-reboot-cmd-restart+ #x01234567)
(defconstant +linux-reboot-cmd-power-off+ #x4321fedc)

;; CFFI Bindings to libc
(cffi:defcfun ("mount" %c-mount) :int
  (source :string)
  (target :string)
  (filesystemtype :string)
  (mountflags :unsigned-long)
  (data :pointer))

(cffi:defcfun ("umount2" %c-umount2) :int
  (target :string)
  (flags :int))

(cffi:defcfun ("sync" %c-sync) :void)

(cffi:defcfun ("reboot" %c-reboot) :int
  (cmd :int))

(cffi:defcfun ("ioctl" %c-ioctl) :int
  (fd :int)
  (request :unsigned-long)
  (arg :pointer))

(cffi:defcfun ("socket" %c-socket) :int
  (domain :int)
  (type :int)
  (protocol :int))

(cffi:defcfun ("close" %c-close) :int
  (fd :int))

(defun mount (source target fstype &key (flags 0) (data nil))
  "Mount a filesystem at target. Returns t on success, nil and errno on failure."
  (let ((data-ptr (if data (cffi:foreign-string-alloc data) (cffi:null-pointer))))
    (unwind-protect
         (let ((ret (%c-mount source target fstype flags data-ptr)))
           (if (zerop ret)
               t
               (values nil (sb-alien:get-errno))))
      (unless (cffi:null-pointer-p data-ptr)
        (cffi:foreign-string-free data-ptr)))))

(defun umount (target &optional (flags 0))
  "Unmount filesystem at target."
  (zerop (%c-umount2 target flags)))

(defun sync-filesystem ()
  "Flush filesystem buffers."
  (%c-sync))

(defun reboot ()
  "Reboot the machine."
  (%c-sync)
  (%c-reboot +linux-reboot-cmd-restart+))

(defun poweroff ()
  "Power off the machine."
  (%c-sync)
  (%c-reboot +linux-reboot-cmd-power-off+))

(defun ioctl (fd request arg-pointer)
  "Invoke ioctl on fd."
  (%c-ioctl fd request arg-pointer))

(defun parse-ipv4 (ip-str)
  "Convert dotted decimal string IP to 4 bytes list."
  (mapcar #'parse-integer (uiop:split-string ip-str :separator ".")))

(defun setup-network-interface (name ip-str &key (netmask-str "255.255.255.0") (is-loopback nil))
  "Configure an IPv4 network interface using SIOCSIFADDR and SIOCSIFFLAGS."
  (let ((sock (%c-socket +af-inet+ +sock-dgram+ 0)))
    (when (< sock 0)
      (error "Failed to create socket for interface configuration: errno ~A" (sb-alien:get-errno)))
    (unwind-protect
         (cffi:with-foreign-pointer (ifr 40)
           ;; Zero out ifr
           (dotimes (i 40)
             (setf (cffi:mem-aref ifr :uint8 i) 0))
           ;; Set ifr_name (first 16 bytes)
           (loop for i from 0 below (min 15 (length name))
                 do (setf (cffi:mem-aref ifr :char i)
                          (char-code (char name i))))
           ;; Set IP address via SIOCSIFADDR
           (let ((ip-bytes (parse-ipv4 ip-str)))
             ;; sockaddr_in: offset 16 is sa_family (2 bytes), offset 18 is port (2 bytes), offset 20 is ip (4 bytes)
             (setf (cffi:mem-aref ifr :uint16 8) +af-inet+) ;; 8 * 2 bytes = byte offset 16
             (loop for b in ip-bytes
                   for idx from 20
                   do (setf (cffi:mem-aref ifr :uint8 idx) b))
             (let ((ret (%c-ioctl sock +siocsifaddr+ ifr)))
               (when (< ret 0)
                 (format t "[SYS] Warning: SIOCSIFADDR on ~A failed (errno ~A)~%" name (sb-alien:get-errno)))))
           ;; Set netmask via SIOCSIFNETMASK
           (when netmask-str
             (let ((mask-bytes (parse-ipv4 netmask-str)))
               (setf (cffi:mem-aref ifr :uint16 8) +af-inet+)
               (loop for b in mask-bytes
                     for idx from 20
                     do (setf (cffi:mem-aref ifr :uint8 idx) b))
               (%c-ioctl sock +siocsifnetmask+ ifr)))
           ;; Bring interface UP: get flags then set flags
           (%c-ioctl sock +siocgifflags+ ifr)
           (let* ((cur-flags (cffi:mem-aref ifr :uint16 8))
                  (new-flags (logior cur-flags +iff-up+ +iff-running+
                                     (if is-loopback +iff-loopback+ +iff-broadcast+))))
             (setf (cffi:mem-aref ifr :uint16 8) new-flags)
             (let ((ret (%c-ioctl sock +siocsifflags+ ifr)))
               (if (zerop ret)
                   (format t "[SYS] Interface ~A is UP with ~A~%" name ip-str)
                   (format t "[SYS] Failed to bring up ~A (errno ~A)~%" name (sb-alien:get-errno))))))
      (%c-close sock))))

(defun reap-zombies ()
  "Reap all terminated child processes to prevent zombie accumulation."
  (loop
    (multiple-value-bind (pid status)
        (ignore-errors (sb-posix:waitpid -1 sb-posix:wnohang))
      (declare (ignore status))
      (unless (and pid (> pid 0))
        (return)))))

(defun install-zombie-reaper ()
  "Install a SIGCHLD handler to reap child processes."
  (sb-sys:enable-interrupt sb-posix:sigchld
                           (lambda (signo info context)
                             (declare (ignore signo info context))
                             (reap-zombies))))
