#
#  makefile for INGRES parser
#
#	@(#)Makefile	7.1	2/5/81
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib

SOBJS=	s_yylex.o s_inout.o s_name.o s_number.o s_operator.o s_string.o \
	s_symtab.o s_comment.o s_cmap.o
OBJS=	parser.o control.o grammar.o att_fcn.o xdot.o \
	range_fcn.o tree.o norml.o \
	call_tree.o format.o par_util.o get_scan.o \
	par_init.o ack_err.o
SSRCS=	s_yylex.c s_inout.c s_name.c s_number.c s_operator.c s_string.c \
	s_symtab.c s_comment.c s_cmap.c scanner.h
SRCS=	att_fcn.c xdot.c range_fcn.c tree.c norml.c \
	call_tree.c control.c format.c par_util.c get_scan.c \
	parser.c par_init.c ack_err.c yyerror.y tables.y \
	parser.h grammar.y

IUTIL=	$(LIB)/iutil
GUTIL=	$(LIB)/gutil
CTLMOD=	$(LIB)/ctlmod
LIBS=	$(IUTIL) $(GUTIL)

YFLAGS= 
CFLAGS=	-O -I$H
LDFLAGS=-z
AR=	-ar
ARFLAGS=rvu
RANLIB=	ranlib
CHMOD=	chmod
LIBMODE=664
BINMODE=4740
ROMODE=	444
CP=	cp
GET=	sccs get
REL=
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Parser
PRT=	/mnt/marc/bin/prt

parlib: $(OBJS) $(SOBJS)
	$(AR) $(ARFLAGS) parlib $?
	$(CHMOD) $(LIBMODE) parlib
	$(RANLIB) parlib

install: $(LIB)/parser

$(LIB)/parser: parlib
	$(CP) parlib $(LIB)/parser
	$(RANLIB) $(LIB)/parser

parser: $(OBJS) $(SOBJS) config.o
	$(CC) $(LDFLAGS) config.o $(OBJS) $(SOBJS) $(CTLMOD) $(LIBS)
	mv a.out parser
	$(CHMOD) $(BINMODE) parser
	ls -l parser; size parser

grammar.c: grammar.y
	grep -v "DDD" grammar.y > grammar.z
	$(YACC) $(YFLAGS) grammar.z
	mv y.tab.c grammar.c
	rm -f grammar.z

grammar.o: tables.y yyerror.y scanner.h parser.h

new:
	rm -f parser parlib

clean:
	rm -f *.o
	rm -f parser a.out core parlib

sources: $(SRCS) $(SSRCS)

print: sources
	$(PR) Makefile TraceFlags *.[hm] *.[csy]

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release 7)" -n Makefile TraceFlags
	$(VGRIND) -h "$(HEADER) (Release 7)" *.[hm] *.[csy]
	$(VGRIND) -h "$(HEADER) (Release 7)" -x index

prt: sources
	$(PRT) parser6.3 parser.h scanner.h grammar.y yyerror.y tables.y [a-r]*.c [t-x]*.c
	$(PRT) scanner6.3 s*.c

$(SRCS) $(SSRCS):
	$(GET) $(REL) SCCS/s.$@

config.c: ../conf/SCCS/s.parser.c
	$(GET) $(REL) -p ../conf/SCCS/s.parser.c > config.c
	$(CHMOD) $(ROMODE) config.c

depend:
	grep '^#[ 	]*include' $(SRCS) | sed -n -f ../h/depend.sed > makedep
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

att_fcn.o:$H/ingres.h
att_fcn.o:$H/aux.h
att_fcn.o:$H/tree.h
att_fcn.o:$H/symbol.h
att_fcn.o:parser.h
xdot.o:$H/ingres.h
xdot.o:$H/aux.h
xdot.o:$H/tree.h
xdot.o:$H/symbol.h
xdot.o:parser.h
range_fcn.o:$H/ingres.h
range_fcn.o:$H/aux.h
range_fcn.o:$H/tree.h
range_fcn.o:$H/symbol.h
range_fcn.o:$H/range.h
range_fcn.o:parser.h
tree.o:$H/ingres.h
tree.o:$H/aux.h
tree.o:$H/tree.h
tree.o:$H/pv.h
tree.o:parser.h
tree.o:$H/symbol.h
norml.o:$H/ingres.h
norml.o:$H/aux.h
norml.o:$H/tree.h
norml.o:$H/symbol.h
call_tree.o:$H/ingres.h
call_tree.o:$H/symbol.h
call_tree.o:$H/tree.h
call_tree.o:$H/pv.h
call_tree.o:parser.h
control.o:$H/ingres.h
control.o:$H/aux.h
control.o:$H/symbol.h
control.o:$H/tree.h
control.o:parser.h
control.o:$H/pv.h
control.o:scanner.h
format.o:$H/ingres.h
format.o:$H/aux.h
format.o:$H/tree.h
format.o:$H/symbol.h
format.o:parser.h
par_util.o:$H/ingres.h
par_util.o:$H/aux.h
par_util.o:$H/tree.h
par_util.o:parser.h
par_util.o:$H/catalog.h
par_util.o:$H/pv.h
par_util.o:$H/symbol.h
get_scan.o:$H/ingres.h
get_scan.o:scanner.h
parser.o:$H/ingres.h
parser.o:$H/range.h
parser.o:$H/tree.h
parser.o:$H/func.h
parser.o:$H/pv.h
parser.o:parser.h
par_init.o:$H/ingres.h
par_init.o:$H/symbol.h
par_init.o:parser.h
par_init.o:$H/access.h
ack_err.o:$H/ingres.h
ack_err.o:$H/pv.h
grammar.y:$H/ingres.h
grammar.y:$H/aux.h
grammar.y:$H/tree.h
grammar.y:$H/symbol.h
grammar.y:$H/pv.h
grammar.y:parser.h
grammar.y:scanner.h
grammar.y:tables.y
grammar.y:yyerror.y
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
