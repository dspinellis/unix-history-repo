$!
$!	Build the GNU "C" pre-processor on VMS
$!
$
$!
$!	C compiler
$!
$ CC	:=	gcc
$!
$!	Compiler options
$!
$ CFLAGS =	"/debug/inc=([],[.config])"
$!
$!	Link options
$!
$ LDFLAGS :=	/nomap
$!
$!	Link libraries
$!
$ LIBS :=	gnu_cc:[000000]gcclib/libr,sys$share:vaxcrtl/libr
$ if "''p1'" .eqs. "LINK" then goto Link
$ 'CC 'CFLAGS cccp.c
$ t1:='f$search("CEXP.C")'
$ if "''t1'" .eqs. "" then goto 10$
$ t1:='f$file_attributes("CEXP.Y","RDT")'
$ t1:='f$cvtime(t1)'
$ t2:='f$file_attributes("CEXP.C","RDT")'
$ t2:='f$cvtime(t2)'
$ if t1 .les. t2 then goto 20$
$ 10$:
$ bison cexp.y
$ rename cexp_tab.c cexp.c
$ 20$:
$!
$ 'CC 'CFLAGS cexp.c
$ 'CC 'CFLAGS version.c
$ Link:
$ link 'LDFLAGS /exe=gcc-cpp cccp,cexp,version,'LIBS'
$!
$!	Done
$!
$ exit
