#
# System Makefile for:
#	ISI68K running BSD UNIX
#
# These are the variables used to specify the nature of the system on which
# pmake is running. These names may later be used in #if expressions for
# conditional reading of the enclosed portion of the Makefile
#
isi68k		= Machine is an isi68k
mc68000		= so it has a Motorola 68000-family chip.
unix		= It runs UNIX.
mc68020		= It has a 68020 microprocessor.

.SUFFIXES 	: .out .a .ln .o .c .F .f .e .r .y .l .s .cl .p .h \
		.c,v .y,v .l,v .s,v .h,v
.INCLUDES 	: .h
.LIBS		: .a
.NULL		: .out

YACC		= yacc
YFLAGS		=
LEX		= lex
LFLAGS		=
CC		= cc
#if defined(vax) || defined(sun)
AS		= as
#else
AS		= as -
#endif
PC		= pc
PFLAGS		=
CFLAGS		=
#ifdef SYSV
ASFLAGS		= 
#else
AFLAGS		=
#endif SYSV
RC		= f77
RFLAGS		=
FC		= f77
EFLAGS		=
FFLAGS		=
LOADLIBES	=
CO		= co
COFLAGS		=
CI		= ci
CIFLAGS		=
AR		= ar
ARFLAGS		= rl

.c,v.c .y,v.y .l,v.l .s,v.s .h,v.h :
	$(CO) $(COFLAGS) $(.IMPSRC) $(.TARGET)

.c.o 		:
	$(CC) $(CFLAGS) -c $(.IMPSRC)

.p.o 		:
	$(PC) $(PFLAGS) -c $(.IMPSRC)

.cl.o 		:
	class -c $(.IMPSRC)

.e.o .r.o .F.o .f.o :
	$(FC) $(RFLAGS) $(EFLAGS) $(FFLAGS) -c $(.IMPSRC)

#ifdef SYSV
.s.o 		:
	$(AS) $(ASFLAGS) -o $(.TARGET) $(.IMPSRC)
#else
.s.o 		:
	$(AS) $(AFLAGS) -o $(.TARGET) $(.IMPSRC)
#endif

.y.o 		:
	$(YACC) $(YFLAGS) $(.IMPSRC)
	$(CC) $(CFLAGS) -c y.tab.c
	rm y.tab.c
	mv y.tab.o $(.TARGET)

.l.o 		:
	$(LEX) $(LFLAGS) $(.IMPSRC)
	$(CC) $(CFLAGS) -c lex.yy.c
	rm lex.yy.c
	mv lex.yy.o $(.TARGET)

.y.c 		:
	$(YACC) $(YFLAGS) $(.IMPSRC)
	mv y.tab.c $(.TARGET)

.l.c 		:
	$(LEX) $(LFLAGS) $(.IMPSRC)
	mv lex.yy.c $(.TARGET)

.s.out .c.out .o.out :
	$(CC) $(CFLAGS) $(.IMPSRC) $(LOADLIBES) -o $(.TARGET)

.f.out .F.out .r.out .e.out :
	$(FC) $(EFLAGS) $(RFLAGS) $(FFLAGS) $(.IMPSRC) \
		$(LOADLIBES) -o $(.TARGET)
	rm -f $(.PREFIX).o

.y.out 		:
	$(YACC) $(YFLAGS) $(.IMPSRC)
	$(CC) $(CFLAGS) y.tab.c $(LOADLIBES) -ly -o $(.TARGET)
	rm y.tab.c

.l.out 		:
	$(LEX) $(LFLAGS) $(.IMPSRC)
	$(CC) $(CFLAGS) lex.yy.c $(LOADLIBES) -ll -o $(.TARGET)
	rm lex.yy.c
