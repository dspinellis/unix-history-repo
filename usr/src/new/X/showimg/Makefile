# makefile for showimg

DESTDIR =
CONFDIR = /usr/users/wyatt/bin

# versions for OIR Vaxen
CFLAGS = -O -DVAX
LIBS = -lnm
#CFLAGS = -DVAX -g -DGLOPEROO

# versions for HEA Sun3 version
#CFLAGS = -O -DMC68000 -DXRAY
#CFLAGS = -DMC68000 -g -DGLOPEROO -DXRAY
#LIBS = -lm
#XRAYLIBS = -lrosat -lheao -lxray -lU77 -lF77 -lI77

XLIB = -lXMenu -lX
INCS = shimg.h shopt.h
CURS = quad_arrow.cursor quad_arrow_mask.cursor\
       pan.cursor pan_mask.cursor print.cursor print_mask.cursor\
       zoom2.cursor zoom2_mask.cursor \
       zoom4.cursor zoom4_mask.cursor \
       pal.cursor pal_mask.cursor show.icon.ic \
       wzoom.cursor wzoom_mask.cursor
SRCS = showimg.c calcmap.c pictio.c menusetup.c shutil.c shzoom.c shgetopt.c
OBJS = showimg.o calcmap.o pictio.o menusetup.o shutil.o shzoom.o shgetopt.o

PROGRAM = showimg


all: showimg

showimg: $(OBJS) /usr/include/X/Xlib.h
	cc $(CFLAGS) $(OBJS) -o $(PROGRAM) $(XLIB) $(LIBS) $(XRAYLIBS)

install:
	cp $(PROGRAM) $(CONFDIR)
clean:
	rm -f core a.out \#* *.o *~

showimg.o: showimg.c shimg.h shopt.h show.icon.ic
shgetopt.o: shgetopt.c shimg.h shopt.h
menusetup.o: menusetup.c shimg.h
calcmap.o: calcmap.c shimg.h
pictio.o: pictio.c
shutil.o: shutil.c $(CURS)
shzoom.o: shzoom.c wzoom.cursor wzoom_mask.cursor

print: 
	print Makefile $(SRCS) $(INCS) $(CURS)

itwork: 
	cp Makefile showimg.1 $(SRCS) $(INCS) $(CURS) $(PROGRAM) Working.version

bbackup:
	rcp -r Makefile showimg.1 Working.version $(SRCS) $(INCS) $(CURS)\
	cfa:progs/saodemo

paper:
	vgrind -lcsh Makefile &
	vgrind $(INCS) $(CURS) $(SRCS) &
