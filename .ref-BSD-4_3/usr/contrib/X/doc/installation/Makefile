# Copyright 1985, Massachusetts Institute of Technology
# Makefile for the Installation document.

TROFF= troff -T480
PRINTER= lpr -n -Pim
FILES= 	in.1 \
	in.2 \
	in.3 \
	in.4 \
	in.5 \
	in.6 \
	appendix

all: 

paper: $(FILES)
	eqn ${FILES} | tbl |${TROFF} -ms | ${PRINTER}

preview:$(FILES)
	neqn ${FILES} | tbl | nroff -ms | colcrt


clean:
	rm -f *~ \#* *.bak errs 
