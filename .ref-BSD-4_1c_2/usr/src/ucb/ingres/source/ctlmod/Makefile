#
#  Makefile for the INGRES control module
#
#	@(#)Makefile	7.1	2/5/81
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib

OBJS=	main.o error.o proc_err.o initp.o setp.o resetp.o \
	init_qt.o call.o sysdump.o mapvars.o \
	do_seq.o readinput.o do_st.o send_off.o call_fn.o \
	readmon.o readqry.o writeqry.o range.o treepr.o \
	pb_prime.o pb_get.o pb_tput.o pb_put.o pb_flush.o \
	pb_read.o pb_write.o pb_rphys.o pb_wphys.o pb_dump.o \
	cm_reset.o cm_cleanup.o cm_close.o \
	prvect.o getp.o monitor.o av_files.o
SRCS1=	pipes.h proc.h state.h ctlmod.h \
	main.c error.c proc_err.c initp.c setp.c resetp.c \
	init_qt.c call.c sysdump.c mapvars.c \
	do_seq.c readinput.c do_st.c send_off.c call_fn.c \
	readmon.c readqry.c writeqry.c range.c treepr.c
SRCS2=	pb_prime.c pb_get.c pb_tput.c pb_put.c pb_flush.c \
	pb_read.c pb_write.c pb_rphys.c pb_wphys.c pb_dump.c \
	cm_reset.c cm_cleanup.c cm_close.c \
	prvect.c getp.c monitor.c av_files.c

CFLAGS=	-O -I$H
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Control Module
CTAGS=	/bin/csh /usr/bin/ctags
CXREF=	/bin/csh /mnt/eric/bin/cxref
AR=	-ar
ARFLAGS=rvu
RANLIB=	ranlib
GET=	sccs get
CP=	cp
CHMOD=	chmod
LIBMODE=664
NETCP=	netcp
MACH=	csvax

all: ctlmod

ctlmod: $(OBJS)
	$(AR) $(ARFLAGS) ctlmod $?
	$(CHMOD) $(LIBMODE) ctlmod
	$(RANLIB) ctlmod

install: $(LIB)/ctlmod

$(LIB)/ctlmod: ctlmod
	$(CP) ctlmod $(LIB)/ctlmod
	$(RANLIB) $(LIB)/ctlmod

new:
	rm -f ctlmod

clean:
	rm -f *.o
	rm -f a.out ctlmod core

sources: $(SRCS1) $(SRCS2)

$(SRCS1):
	$(GET) $(REL) SCCS/s.$@
$(SRCS2):
	$(GET) $(REL) SCCS/s.$@

ctags:
	$(CTAGS) *.c

print: sources
	$(PR) Makefile TraceFlags *.[hm] *.[csy]

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release 7)" -n Makefile TraceFlags
	$(VGRIND) -h "$(HEADER) (Release 7)" *.[hm] *.[csy]
	$(VGRIND) -h "$(HEADER) (Release 7)" -x index

archive: sources
	$(AR) rvu ctlmod1.a Makefile *.h [0-h]*.[cys]
	$(AR) rvu ctlmod2.a [i-z]*.[cys]

netcp: archive
	$(NETCP) $(LOGIN) ctlmod1.a $(MACH):ctlmod1.a
	$(NETCP) $(LOGIN) ctlmod2.a $(MACH):ctlmod2.a

setjmp.o:
	$(AR) x /usr/nlib/libretro.a setjmp.o

depend:
	grep '^#[ 	]*include' $(SRCS1) $(SRCS2) | sed -n -f ../h/depend.sed > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

ctlmod.h:state.h
ctlmod.h:proc.h
ctlmod.h:$H/pv.h
ctlmod.h:pipes.h
ctlmod.h:$H/func.h
ctlmod.h:$H/pmon.h
ctlmod.h:$H/useful.h
main.o:ctlmod.h
main.o:$H/resp.h
main.o:$H/ingres.h
main.o:$H/aux.h
error.o:ctlmod.h
error.o:$H/pv.h
proc_err.o:ctlmod.h
initp.o:ctlmod.h
initp.o:$H/ingres.h
initp.o:$H/aux.h
initp.o:$H/tree.h
setp.o:ctlmod.h
setp.o:$H/ingres.h
setp.o:$H/tree.h
setp.o:$H/aux.h
resetp.o:ctlmod.h
resetp.o:$H/ingres.h
resetp.o:$H/tree.h
init_qt.o:ctlmod.h
init_qt.o:$H/ingres.h
init_qt.o:$H/aux.h
init_qt.o:$H/tree.h
call.o:ctlmod.h
call.o:$H/resp.h
call.o:$H/ingres.h
call.o:$H/aux.h
call.o:$H/tree.h
sysdump.o:ctlmod.h
sysdump.o:$H/tree.h
mapvars.o:$H/ingres.h
mapvars.o:$H/symbol.h
mapvars.o:$H/tree.h
do_seq.o:ctlmod.h
do_seq.o:$H/resp.h
readinput.o:ctlmod.h
readinput.o:$H/resp.h
readinput.o:$H/ingres.h
readinput.o:$H/aux.h
readinput.o:$H/tree.h
do_st.o:ctlmod.h
send_off.o:ctlmod.h
send_off.o:$H/symbol.h
send_off.o:$H/tree.h
send_off.o:$H/resp.h
call_fn.o:ctlmod.h
call_fn.o:$H/resp.h
readqry.o:ctlmod.h
readqry.o:$H/ingres.h
readqry.o:$H/aux.h
readqry.o:$H/tree.h
readqry.o:$H/symbol.h
writeqry.o:$H/ingres.h
writeqry.o:$H/aux.h
writeqry.o:$H/tree.h
writeqry.o:$H/symbol.h
range.o:$H/ingres.h
range.o:$H/symbol.h
range.o:$H/range.h
range.o:$H/tree.h
treepr.o:$H/ingres.h
treepr.o:$H/symbol.h
treepr.o:$H/tree.h
treepr.o:$H/aux.h
pb_prime.o:ctlmod.h
pb_get.o:ctlmod.h
pb_tput.o:ctlmod.h
pb_put.o:ctlmod.h
pb_flush.o:ctlmod.h
pb_read.o:ctlmod.h
pb_write.o:ctlmod.h
pb_wphys.o:$H/useful.h
cm_reset.o:ctlmod.h
cm_cleanup.o:ctlmod.h
cm_close.o:ctlmod.h
prvect.o:$H/tree.h
prvect.o:$H/pv.h
getp.o:ctlmod.h
getp.o:$H/pv.h
monitor.o:$H/useful.h
monitor.o:$H/opsys.h
monitor.o:$H/pmon.h
av_files.o:$H/ingres.h
av_files.o:$H/aux.h
av_files.o:$H/opsys.h
av_files.o:ctlmod.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
