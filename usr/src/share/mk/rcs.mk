# for RCS
.SUFFIXES: .out .a .ln .o .c .F .f .e .r .y .l .s .cl .p .h \
	       .c,v .y,v .l,v .s,v .h,v .8 .7 .6 .5 .4 .3 .2 .1 .0

CI=		/usr/new/ci
CIFLAGS=

CO=		/usr/new/co
COFLAGS=

.c,v.c .y,v.y .l,v.l .s,v.s .h,v.h:
	${CO} ${COFLAGS} ${.IMPSRC} ${.TARGET}
