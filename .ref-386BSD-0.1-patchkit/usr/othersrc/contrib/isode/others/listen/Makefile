###############################################################################
#   Instructions to Make, for compilation of ISODE listen demo
###############################################################################

###############################################################################
# Options
###############################################################################

BINDIR	=	/usr/new/


###############################################################################
# Generation Rules for program modules
###############################################################################

.c.o:;		$(CC) $(CFLAGS) -c $*.c


###############################################################################
# Programs and Libraries
###############################################################################

LIBES	=	$(TOPDIR)libisode.a
LLIBS   =	$(TOPDIR)llib-lisode


###############################################################################
# Files
###############################################################################

HFILES	=	listen.h
CFILES	=	listen.c initiate.c


##################################################################
# Here it is...
##################################################################

all:		listen initiate
inst-all:	inst-listen inst-initiate
install:	inst-all clean
lint:		l-listen l-initiate


##################################################################
# listen
##################################################################

inst-listen:	$(BINDIR)listen

$(BINDIR)listen:	xlisten
		-cp $@ zxlisten
		-rm -f $@
		cp xlisten $@
		-@ls -gls $@
		-@echo ""

listen:		xlisten

xlisten:	listen.o report.o
		$(LDCC) $(LDFLAGS) -o $@ listen.o report.o $(LIBES) \
			$(LSOCKET)

l-listen:;	$(LINT) $(LFLAGS) listen.c ../callback/report.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

listen.o:	$(HFILES)

report.o:	../callback/report.c
		$(CC) $(CFLAGS) -c ../callback/report.c


##################################################################
# initiate
##################################################################

inst-initiate:	$(BINDIR)initiate

$(BINDIR)initiate:	xinitiate
		-cp $@ zxinitiate
		-rm -f $@
		cp xinitiate $@
		-@ls -gls $@
		-@echo ""

initiate:	xinitiate

xinitiate:	initiate.o report.o
		$(LDCC) $(LDFLAGS)  -o $@ initiate.o report.o $(LIBES) \
			$(LSOCKET)

l-initiate:;	$(LINT) $(LFLAGS) initiate.c ../callback/report.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


initiate.o:	$(HFILES)


##################################################################
# clean
##################################################################

clean:;		rm -f *.o x* z* _* core 

grind:;		iprint Makefile
		tgrind -lc $(HFILES) $(CFILES)

true:;
