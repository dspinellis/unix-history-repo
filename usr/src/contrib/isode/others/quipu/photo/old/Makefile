###############################################################################
#   Instructions to Make, for compilation of QUIPU photo programs
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/quipu/photo/RCS/Makefile,v 7.5 90/12/23 18:46:23 mrose Exp $
#
#
# $Log:	Makefile,v $
# Revision 7.5  90/12/23  18:46:23  mrose
# update
# 
# Revision 7.4  90/09/24  16:25:26  mrose
# touch-up
# 
# Revision 7.3  90/09/24  16:22:11  mrose
# touch-up
# 
# Revision 7.2  90/09/24  16:21:19  mrose
# touch-up
# 
# Revision 7.1  90/03/27  08:52:54  mrose
# pbmplus
# 
# Revision 7.0  89/11/23  22:01:31  mrose
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


# should really go in config/CONFIG.make
DISPLAY = ttyphoto hexphoto
# DISPLAY = sunphoto Xphoto ttyphoto hexphoto t4014


#######################################################################

DEC     = decode.o build_trees.o 
ENC     = encode.o code_word.o 
UTIL    = interface.o 


all:    libphoto.a $(DISPLAY)

lint:;	$(LINT) $(LFLAGS) decode.c build_trees.c interface.c d_main.c tty.c \
		| grep -v "warning: possible pointer alignment problem"

libphoto.a: $(DEC) $(UTIL)
	-rm -f $@
	@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(DEC) $(UTIL)

pr2pe: $(ENC) $(UTIL) e_main.o
	$(LDCC) $(LDFLAGS) -o pr2pe e_main.o $(ENC) $(UTIL) -lpixrect

sunphoto: libphoto.a d_main.o sunview.o
	$(LDCC) $(LDFLAGS) -o sunphoto  \
		d_main.o libphoto.a sunview.o -lsuntool -lsunwindow -lpixrect

Xphoto: libphoto.a d_main.o winx.o
	$(LDCC) $(LDFLAGS) -o Xphoto \
		d_main.o libphoto.a winx.o -lX11

t4014: libphoto.a d_main.o t4014.o
	$(LDCC) $(LDFLAGS) -o t4014 \
		d_main.o libphoto.a t4014.o -l4014 -lm

ttyphoto: libphoto.a d_main.o tty.o
	$(LDCC) $(LDFLAGS) -o ttyphoto \
		d_main.o  libphoto.a tty.o

hexphoto: hexphoto.o d_main.o
	$(LDCC) $(LDFLAGS) -o hexphoto hexphoto.o d_main.o 

decode.o: decode.c $(HDIR)quipu/photo.h
	$(CC) -c $(CFLAGS) -DFAXDIR=\"$(SBINDIR)g3fax\" $*.c


###############################################################################
# some programs for Jef Poskanzer's pbm package
###############################################################################

PBMDIR	=	/usr/src/pbmplus/
LIBPBM	=	$(PBMDIR)pbm/libpbm.a

pbm:		faxtopbm pbmtofax

faxtopbm:	faxtopbm.o libphoto.a $(LIBPBM)
		$(LDCC) $(LDFLAGS) -o $@ faxtopbm.o libphoto.a $(LIBPBM) \
			$(TOPDIR)libisode.a $(LSOCKET)

faxtopbm.o:	faxtopbm.c
		$(CC) $(CFLAGS) -I$(PBMDIR) -c faxtopbm.c

pbmtofax:	pbmtofax.o encode.o code_word.o interface.o $(LIBPBM)
		$(LDCC) $(LDFLAGS) -o $@ pbmtofax.o encode.o code_word.o \
			interface.o $(LIBPBM)

pbmtofax.o:	pbmtofax.c
		$(CC) $(CFLAGS) -I$(PBMDIR) -c pbmtofax.c


#####################################################################
install:  inst-all clean

inst-all:  $(SBINDIR)g3fax inst-disp

inst-disp: $(DISPLAY) libphoto.a
	-mv $(SBINDIR)g3fax/ttyphoto zttyphoto
	-cp ttyphoto $(SBINDIR)g3fax/ttyphoto
	-mv $(SBINDIR)g3fax/hexphoto zhexphoto
	-cp hexphoto $(SBINDIR)g3fax/hexphoto
	-mv $(SBINDIR)g3fax/sunphoto zsunphoto
	-cp sunphoto $(SBINDIR)g3fax/sunphoto
	-mv $(SBINDIR)g3fax/Xphoto zXphoto
	-cp Xphoto $(SBINDIR)g3fax/Xphoto
	-mv $(SBINDIR)g3fax/t4014 zt4014
	-cp t4014 $(SBINDIR)g3fax/t4014
	-mv $(SBINDIR)g3fax/faxtopbm zfaxtopbm
	-cp faxtopbm $(SBINDIR)g3fax/faxtopbm
	-mv $(SBINDIR)g3fax/pbmtofax zpbmtofax
	-cp pbmtofax $(SBINDIR)g3fax/pbmtofax
	-mv $(SBINDIR)g3fax/pr2pe zpr2pe
	-cp pr2pe $(SBINDIR)g3fax/pr2pe
	-rm -f $(SBINDIR)g3fax/libphoto.a
	cp libphoto.a $(SBINDIR)g3fax/libphoto.a
	@$(UTILDIR)make-lib.sh $(SYSTEM) $(SBINDIR)g3fax/libphoto.a -ranlib
	-@ls -gls $(SBINDIR)g3fax/libphoto.a
	-@echo ""

$(SBINDIR)g3fax:  g3fax
	-rm -rf $@
	-cp -r g3fax $(SBINDIR)

clean:
	rm -f _* *.o *.old *% foo* core z* *.a \
		*.orig \
		sunphoto hexphoto ttyphoto \
		pr2pe Xphoto t4014 \
		faxtopbm pbmtofax
