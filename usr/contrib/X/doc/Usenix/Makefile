# Copyright 1985, Massachusetts Institute of Technology
# Makefile for the Xlib document.

TROFF= troff -T480
PRINTER= lpr -n -Pim
FILES= macros.t\
	abstract.t\
	x.t\
	kernel.t\
	memory.t\
	fd.t\
	pty.t\
	startup.t\
	naming.t\
	rpc.t\
	select.t\
	summary.t\
	ack.t\
	references.t


all:	paper

paper: $(FILES)
	cat $(FILES) |${TROFF} -ms | ${PRINTER}

cat: $(FILES)
	cat $(FILES) >catfiles

preview:$(FILES)
	cat $(FILES) | nroff -ms | colcrt

clean:
	rm -f *~ \#* *.bak errs
