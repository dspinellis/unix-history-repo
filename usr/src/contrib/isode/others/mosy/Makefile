###############################################################################
#   Instructions to Make, for compilation of ISODE MOSY processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/mosy/RCS/Makefile,v 7.8 91/02/22 09:28:39 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.8  91/02/22  09:28:39  mrose
# Interim 6.8
# 
# Revision 7.7  90/12/23  18:44:31  mrose
# update
# 
# Revision 7.6  90/11/04  19:15:23  mrose
# update
# 
# Revision 7.5  90/09/07  17:33:57  mrose
# new-macros
# 
# Revision 7.4  90/08/29  12:26:59  mrose
# touch-up
# 
# Revision 7.3  90/07/09  14:39:59  mrose
# sync
# 
# Revision 7.2  90/07/01  21:04:35  mrose
# pepsy
# 
# Revision 7.1  90/01/16  20:43:13  mrose
# last check-out
# 
# Revision 7.0  89/11/23  22:00:33  mrose
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


###############################################################################
# Generation Rules for program modules
###############################################################################

.SUFFIXES:	.c .o

.c.o:;		$(CC) $(CFLAGS) -c $*.c


###############################################################################
# Programs and Libraries
###############################################################################

LIBES   =	$(TOPDIR)libisode.a
LLIBS   =	$(TOPDIR)llib-lisode


###############################################################################
# Files
###############################################################################

HFILES	=	mosy-defs.h 
YFILES	=	yacc.y lex.l


##################################################################
# Here it is...
##################################################################

all:		mosy
inst-all:	inst-mosy manuals
install:	inst-all clean
lint:		l-mosy


##################################################################
# mosy
##################################################################

inst-mosy:	$(BINDIR)mosy

$(BINDIR)mosy:	xmosy
		-cp $@ zxmosy
		-rm -f $@
		cp xmosy $@
		-@ls -gls $@
		-@echo ""

mosy:		xmosy

xmosy:		mosyvrsn.o
		$(LDCC) $(LDFLAGS) -o $@ mosy.o yacc.o pepy_misc.o mosyvrsn.o \
			$(LIBES) $(LSOCKET) -lm

mosy.o:		mosy.c $(HFILES)

pepy_misc.o:	$(TOPDIR)pepy/pepy_misc.c
		$(CC) $(CFLAGS) -c $?

mosyvrsn.c:	mosy.o yacc.o pepy_misc.o
		@$(UTILDIR)version.sh mosy > $@

yacc.o:		yacc.c lex.c $(HFILES)

yacc.c:		yacc.y
		-@echo "expect 21 shift/reduce and 9 reduce/reduce conflicts"
		yacc $(YACCFLAGS) yacc.y
		mv y.tab.c $@

yacc.y:		$(TOPDIR)pepy/yacc.y.gnrc
		$(UTILDIR)extract.sh MOSY < $? > $@

lex.c:		lex.l
		$(LEX) $(LEXFLAGS) lex.l
		mv lex.yy.c $@

lex.l:		$(TOPDIR)pepy/lex.l.gnrc
		$(UTILDIR)extract.sh MOSY < $? > $@

mosy-defs.h:	$(TOPDIR)pepsy/pepsy.h.gnrc
		$(UTILDIR)extract.sh MOSY < $? > $@

l-mosy:		yacc.c lex.c true
		$(LINT) -I$(TOPDIR)h $(LFLAGS) mosy.c yacc.c \
		    $(TOPDIR)pepy/pepy_misc.c mosyvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# manual pages
################################################################

MANUALS	=	mosy.1

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a x* z* _* core mosy-defs.h yacc.y yacc.c lex.l \
			lex.c mosyvrsn.c *.defs

grind:;		iprint Makefile
		tgrind -lc $(HFILES) mosy.c mosyvrsn.c
		tgrind -ly $(YFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
