###############################################################################
#   Instructions to Make, for compilation of ISODE VT processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/vt/RCS/Makefile,v 7.3 91/02/22 09:47:50 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.3  91/02/22  09:47:50  mrose
# Interim 6.8
# 
# Revision 7.2  90/12/23  18:43:28  mrose
# update
# 
# Revision 7.1  90/07/09  14:51:45  mrose
# sync
# 
# Revision 7.0  89/11/23  22:31:25  mrose
# Release 6.0
# 
###############################################################################

###############################################################################
#
#				 NOTICE
#
#    Acquisition, use, and distribution of this module and related
#    materials are subject to the restrictions of a license agreement.
#    Consult the Preface in the User's Manual for the full terms of
#    this agreement.
#
###############################################################################


.SUFFIXES:	.py .c .o

.c.o:;		$(CC) $(CFLAGS) $(INCLUDES) -c $*.c

.py.c:;		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) $<


LIBES	=	$(TOPDIR)libisode.a
LLIBS	=	$(TOPDIR)llib-lisode
VTH	=	vtpm.h $(HDIR)acsap.h $(HDIR)psap.h $(HDIR)manifest.h \
		$(HDIR)config.h $(HDIR)general.h $(HDIR)psap2.h $(HDIR)logger.h
CFILES	=	vtpm.c vtuser.c vt_telnet.c map.c \
		actions1.c actions5.c states1.c states5.c
OFILES	=	vtpm.o vtuser.o vt_telnet.o map.o \
		actions1.o actions5.o states1.o states5.o
PYFILES	=	bk_content.py rcv_asq.py rcv_asr.py rcv_text.py send_asq.py \
		send_asr.py send_text.py send_udq.py rcv_udq.py print_vt.py
HFILES	=	eventmsg.h sector1.h sector5.h vtpm.h
PYFILES-C=	BK_CONTENT.c RCV_ASQ.c RCV_ASR.c RCV_TEXT.c RCV_UDQ.c \
		SEND_ASQ.c SEND_ASR.c SEND_TEXT.c SEND_UDQ.c PRINT_VT.c
PYFILES-O=	BK_CONTENT.o RCV_ASQ.o RCV_ASR.o RCV_TEXT.o RCV_UDQ.o \
		SEND_ASQ.o SEND_ASR.o SEND_TEXT.o SEND_UDQ.o PRINT_VT.o


##################################################################
# Here it is...
##################################################################

all:		vtd vt
inst-all:	inst-vtd inst-vt manuals
install:	inst-all clean
lint:		l-vtd l-vt


##################################################################
# vtd
##################################################################

inst-vtd:	$(SBINDIR)iso.vt

$(SBINDIR)iso.vt:	xvtd
		-cp $@ ziso.vt
		-rm -f $@
		cp xvtd $@
		-@ls -gls $@
		-@echo ""

vtd:		xvtd

xvtd:		vtd.o $(OFILES) libvt.a
		$(LDCC) $(LDFLAGS) -o $@ vtd.o $(OFILES) libvt.a \
			$(LIBES) $(LSOCKET)

vtd.o:		$(VTH) sector1.h $(HDIR)tailor.h
vtpm.o:		$(VTH) eventmsg.h sector1.h
vtuser.o:	$(VTH) sector1.h sector5.h
vt_telnet.o:	$(VTH) sector1.h
map.o:		$(VTH) sector1.h
actions1.o:	$(VTH) sector1.h
actions5.o:	$(VTH) sector1.h
states1.o:	$(VTH)
states5.o:	

l-vtd:		$(PYFILES-C) true
		$(LINT) $(LFLAGS) vtd.c $(CFILES) $(PYFILES-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


##################################################################
# vt
##################################################################

inst-vt:	$(BINDIR)vt

$(BINDIR)vt:	xvt
		-cp $@ zxvt
		-rm -f $@
		cp xvt $@
		-@ls -gls $@
		-@echo ""

vt:		xvt

xvt:		vt.o $(OFILES) libvt.a
		$(LDCC) $(LDFLAGS) -o $@ vt.o $(OFILES) libvt.a \
			$(LIBES) $(LSOCKET)

vt.o:		$(VTH) sector1.h $(HDIR)tailor.h

l-vt:		$(PYFILES-C) true
		$(LINT) $(LFLAGS) vt.c $(CFILES) $(PYFILES-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


##################################################################
# libvt
##################################################################

libvt.a:	$(PYFILES-O)
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(PYFILES-O)
		-@ls -l $@
		-@echo "VT library built normally"

BK_CONTENT.c:	bk_content.py $(TOPDIR)pepy/xpepy
		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) -o $@ \
			bk_content.py

RCV_ASQ.c:	rcv_asq.py $(TOPDIR)pepy/xpepy
		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) -o $@ \
			rcv_asq.py

RCV_ASR.c:	rcv_asr.py $(TOPDIR)pepy/xpepy
		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) -o $@ \
			rcv_asr.py

RCV_TEXT.c:	rcv_text.py $(TOPDIR)pepy/xpepy
		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) -o $@ \
			rcv_text.py

RCV_UDQ.c:	rcv_udq.py $(TOPDIR)pepy/xpepy
		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) -o $@ \
			rcv_udq.py

SEND_ASQ.c:	send_asq.py $(TOPDIR)pepy/xpepy
		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) -o $@ \
			send_asq.py

SEND_ASR.c:	send_asr.py $(TOPDIR)pepy/xpepy
		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) -o $@ \
			send_asr.py

SEND_TEXT.c:	send_text.py $(TOPDIR)pepy/xpepy
		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) -o $@ \
			send_text.py

SEND_UDQ.c:	send_udq.py $(TOPDIR)pepy/xpepy
		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) -o $@ \
			send_udq.py

PRINT_VT.c:	print_vt.py $(TOPDIR)pepy/xpepy
		$(TOPDIR)pepy/xpepy -a PY_advise -m $(PYFLAGS) -o $@ \
			print_vt.py


##################################################################
# manual pages
##################################################################

MANUALS	=	vtd.8c vt.1c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


##################################################################
# clean
##################################################################

clean:;		rm -f *.ph *.o *.a $(PYFILES-C) x* z* _* core

grind:;		iprint Makefile
		tgrind -lc vt.c vtd.c $(HFILES) $(CFILES)
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs ($PYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e 's%.*%itroff -man &%" | \
			sh -ve

true:;
