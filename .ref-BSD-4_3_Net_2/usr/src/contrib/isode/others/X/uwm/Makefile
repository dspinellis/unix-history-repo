
X = /cs/sys/src/package/X11/R3/clients/uwm
I = /cs/research/dsa/hubris/incads/sun4.0/isode-5.8uk

OBJS=	$X/gram.o $X/lex.o $X/globals.o $X/Beep.o $X/Bindings.o \
	$X/CircleDown.o $X/CircleUp.o\
	$X/Cursors.o $X/Focus.o $X/GetButton.o $X/GridBox.o \
	$X/Iconify.o $X/Icons.o $X/Kill.o \
	$X/Lower.o\
	$X/Menu.o $X/Move.o $X/MoveOpaque.o $X/NewIconify.o $X/Pause.o \
	$X/Push.o $X/Error.o\
	$X/Raise.o $X/Refresh.o $X/Resize.o $X/Restart.o $X/RubberBand.o \
	$X/StoreBox.o \
	$X/StoreZap.o $X/XError.o $X/uwm.o $X/colormap.o $X/windowinfo.o

OBJSXI = ../client/XlibInt.o ../client/XConnDis.o

ISODELIBS = ../tsap/tsapmisc.o $I/libtsap.a $I/libisode.a $I/libcompat.a

UWMDIR=/usr/ucl/lib/X11/uwm/

CFLAGS = -DSYSFILE=\"$(UWMDIR)system.uwmrc\"

L = /usr/ucl/lib/libXaw.a /usr/ucl/lib/libXt.a  \
	/usr/ucl/lib/libXmu.a /usr/ucl/lib/libX11.a \
	$(ISODELIBS) \
	-ltermlib -lm -ll

uwm: $(OBJS) 
	$(CC) -o $T$@ $(OBJS) $(OBJSXI) $L


