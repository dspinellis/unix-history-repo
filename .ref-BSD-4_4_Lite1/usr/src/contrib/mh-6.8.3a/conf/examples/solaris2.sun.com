# solaris2.sun.com - MH 6.8 - Solaris 2.x
# Solaris 2.x (native, no source/binary compatability)
# contributed by Greg.Onufer@cheers.Bungi.COM
# Wed, 11 Nov 1992 00:43:19 PST

# Bulletin Boards
bbdelivery      off
bboards         off

# Compilation Environment
cc              cc
ccoptions       -O -g
curses          -lcurses
# ldoptions may also need -R/opt/mh/lib
ldoptions       -s
ldoptlibs       -lnsl -lsocket
sharedlib       sys5
slflags         -K pic
ranlib          off
chown           /usr/bin/chown
oldload         none

# RunTime Environment
bin             /opt/mh/bin
etc             /opt/mh/lib
slibdir         /opt/mh/lib
mandir          /opt/mh/man
manuals         standard
editor          /usr/bin/vi
mail            /var/mail
mts             sendmail/smtp

# Compilation Options
options SYS5
options SVR4
options FCNTL
options RENAME
options VSPRINTF
options ATTVIBUG
options DBMPWD
options DUMB
options MORE='"/usr/bin/more"'
options MSGPROT='"0600"'
options MSGID
options RPATHS
options SOCKETS
options FOLDPROT='"0711"'
options SHADOW
options SYS5DIR
options MHRC
options MHE
options MIME
signal  void
sprintf int
