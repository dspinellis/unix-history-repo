#	@(#)Makefile	8.2 (Berkeley) 4/20/94

SRCS=	vi.man ../vi.ref/set.opt.roff
DOCS=	vi.0.txt vi.0.ps

all: ${DOCS}

vi.0.txt: vi.1
	groff -man -Tascii < vi.1 > $@
vi.0.ps: vi.1
	groff -man < vi.1 > $@
vi.1: ${SRCS}
	cpp -DMANUAL -I../vi.ref vi.man | sed -e '/^#/d' -e '/^$$/d' > $@

clean:
	rm -f ${DOCS} vi.1
