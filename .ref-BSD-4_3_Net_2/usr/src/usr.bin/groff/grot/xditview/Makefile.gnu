CC=cc
CFLAGS=-g
LDFLAGS=-g -Bstatic
RANLIB=ranlib
AR=ar
BINDIR=/usr/local/bin
GROFFLIBDIR=/usr/local/lib/groff
FONTDIR=$(GROFFLIBDIR)/font
MACRODIR=$(GROFFLIBDIR)/tmac
MANEXT=1
MANDIR=/usr/local/man/man$(MANEXT)
# Delete devX100 if you don't have 100dpi fonts installed.
DEVICES=devX75 devX100

all: xditview xtotroff

xditview: xditview.o libXdvi/libXdvi.a
	$(CC) $(LDFLAGS) -o $@ xditview.o libXdvi/libXdvi.a \
	-lXaw -lXmu -lXt -lXext -lX11 -lm

xtotroff: xtotroff.o libXdvi/libXdvi.a
	$(CC) $(LDFLAGS) -o $@ xtotroff.o libXdvi/libXdvi.a \
	-lXext -lX11 -lm

clean:
	-rm -f xditview xtotroff *.o core
	@echo Making clean in libXdvi
	@cd libXdvi; $(MAKE) clean

libXdvi/libXdvi.a: FORCE
	@echo Making all in libXdvi
	@cd libXdvi; \
	$(MAKE) "CC=$(CC)" "CFLAGS=$(CFLAGS)" "RANLIB=$(RANLIB)" "AR=$(AR)" \
		libXdvi.a

FORCE:

xditview.o: libXdvi/Dvi.h xdit.bm xdit_mask.bm
xtotroff.o: libXdvi/XFontName.h libXdvi/DviChar.h

install: all
	@for dir in $(DEVICES); do \
	echo Making install in $$dir; \
	(cd $$dir; $(MAKE) "FONTDIR=$(FONTDIR)" install); done
	-[ -d $(MACRODIR) ] || mkdir $(MACRODIR)
	cp tmac.X $(MACRODIR)
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	cp xditview $(BINDIR)/gxditview
	cp xditview.man $(MANDIR)/gxditview.$(MANEXT)
