# kosciusko.esd.3com.com - SunOS 4.0.3 
# From: "Mark D. Baushke" <mdb@kosciusko.ESD.3Com.COM>
# Sun Hardware: 	Sun-4c/60
# Sun Software:	SunOS 4.0.3
# 		DNS libc.so.1.3.1 (from uunet:sun-fixes)
# 		YP/NIS passwd + automounter maps
# 		automounter for home directories and /n tree
# VAX Hardware:	VAX 11/785
# VAX Software:	MORE/bsd 4.3 (mt. Xinu)
# Window Systems (Sun hardware only):
# 		X11R4 (+ fixes 1-9)
# 		OpenWindows 1.0.1
# 		sunview
# Interfaces tested:
# 		xmh (from X11R4),
# 		GNU Emacs 18.55 (mh-e version 3.6)
# 		Epoch 3.1	(mh-e version 3.6)
# 		shelltool (standard command-based user-interface)
# 		/bin/csh  (standard command-based user-interface)
#
# The /n tree is a replicated filesystem (auto)mounted based on hardware
# and operating system.
#
bin	/n/mh/mh-6.7/bin
etc	/n/mh/mh-6.7/etc
mail	/usr/spool/mail
mandir  /n/mh/mh-6.7/man
manuals	standard
mts	sendmail/smtp
options	BSD42 BSD43 SUN40 DUMB ATHENA
options	MHE MHRC NFS BIND DBMPWD
options	SBACKUP='"\\043"' OVERHEAD
ranlib on
ldoptions -n -s -O
bboards	off
pop	off
sharedlib off
tma off
