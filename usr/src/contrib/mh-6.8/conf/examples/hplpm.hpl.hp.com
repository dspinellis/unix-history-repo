# hplpm.hpl.hp.com - Hewlett-Packard 9000 machines
# From:    Peter Marvit <marvit%hplpm.hpl.hp.com@hplb.hpl.hp.COM>
# For a reasonably heterogeneous environemnt fof Hewlett-Packard 9000
# machines (both series 300 and 800), conf/MH:
#
bin	/usr/local/bin/mh
etc	/usr/local/lib/mh
bboards	off
bbdelivery off
debug	off
pop	off
ranlib  off
mail	/usr/mail
mailgroup mail
mandir	/usr/local/man
manuals	standard
chown	/bin/chown
mts	sendmail/smtp
editor  vi
mf 	off
remove	rm -f
options	ATHENA ATZ BIND ISI MHE
options	MHRC MORE='"/usr/bin/more"' NDIR NFS
options	OVERHEAD RPATHS SENDMAILBUG SOCKETS SYS5 TZNAME WHATNOW
options	SBACKUP='"\\043"' 
curses	-lcurses
ldoptions	-nqs
