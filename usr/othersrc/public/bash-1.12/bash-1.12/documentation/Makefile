# This Makefile is for the Bash/documentation directory -*- text -*-.
#

all: features.dvi features.info bash.txt

features.dvi: texindex features.texi
	tex features.texi
	./texindex features.??
	tex features.texi

features.info: features.texi
	makeinfo features.texi

bash.dvi: texindex bash.texinfo
	tex bash.texinfo
	./texindex bash.??
	tex bash.texinfo

bash.txt: bash.1
	nroff -man bash.1 >bash.txt

texindex: texindex.o
	$(CC) -o $@ $(LDFLAGS) $(CFLAGS) $?
texindex.o: texindex.c

clean:
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr *.cps *.pgs \
	      *.fns *.kys *.tps *.vrs *.o core texindex

squeaky-clean:
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr *.cps *.pgs \
	      *.dvi *.info *.info-* *.fns *.kys *.tps *.vrs *.o core texindex
