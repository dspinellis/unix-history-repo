(setq rcs-syscall-
   "$Header: /usr/lib/lisp/RCS/syscall.l,v 1.2 83/01/30 11:52:43 jkf Exp $")

;
;   syscall
;
; This file contains macro definitions of some of the Unix system calls.
; The documentation for these system calls can be found in the Unix
; manual.
;
; It is believed that all of these system calls can be executed by the
; Unix emulator Eunice under VMS.
;
; Unix system calls which return values in registers other than r0 cannot
; be called in this way.  An example of this is fork, for which there is
; a seperate lisp function.

(declare (macros t))

(defmacro sys_access (name mode)
  `(syscall 33 ,name ,mode))

(defmacro sys_alarm (secs)
  `(syscall 27 ,secs))

(defmacro sys_brk (addr)
  `(syscall 17 ,addr))

(defmacro sys_chdir (dir)
  `(syscall 12 ,dir))

(defmacro sys_chmod (name mode)
  `(syscall 15 ,name ,mode))

(defmacro sys_chown (name ownerid groupid)
  `(syscall 16 name ownerid groupid))

(defmacro sys_close (filedes)
  `(syscall 6 ,filedes))

(defmacro sys_creat (name mode)
  `(syscall 8 ,name ,mode))

(defmacro sys_exit (status)
  `(syscall 1 ,status))

(defmacro sys_getpid nil
  `(syscall 20))

(defmacro sys_getuid nil
  `(syscall 24))

(defmacro sys_getgid nil
  `(syscall 47))

; sys_kill - need to get value into r0
(defmacro sys_kill (pid)
  `(syscall 37 ,pid))


(defmacro sys_link (name newname)
  `(syscall 9 ,name ,newname))

(defmacro sys_nice (value)
  `(syscall 34 ,value))

; sys_lseek this may not be correct the explanation is given for a pdp-11
; where certain values must be stored in two words.
; also need to get value into r0

(defmacro sys_open (name how)  
  `(syscall 5 ,name ,how))

(defmacro sys_pause nil
  `(syscall 29))

(defmacro sys_setuid (uid)
  `(syscall 23 ,uid))


(defmacro sys_setgid (gid)
  `(syscall 46 ,gid))

(defmacro sys_sync nil
  `(syscall 36))

(defmacro sys_time nil
  `(syscall 13))

(defmacro sys_umask (complementmode)
  `(syscall 60 ,complementmode))

(defmacro sys_unlink (name)
  `(syscall 10 ,name))

(defmacro sys_wait nil
  `(syscall 7))

(defmacro sys_ioctl (portnumber arg)
  `(syscall 54 ,portnumber ,arg))


(putprop 'syscall t 'version)	; flag that this file has been loaded
