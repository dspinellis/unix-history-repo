CFLAGS = 
FILES =   alloc.c alloc3.c bbox.c clip.c compare.c draw.c edgepath.c\
	edges.c error.c \
	extractor.c fill.c fonts.c heap.c input.c interpeter.c layers.c \
	line.c main.c make.c parser.y pat.l plot.c prim.c print.c queue.c \
	readpat.c scanner.c select.c stifdummy.c string.c symbol.c text.c \
	transforms.c
OBJECTS = main.o input.o parser.o error.o string.o interpeter.o make.o \
	layers.o symbol.o transforms.o bbox.o alloc.o plot.o print.o \
	queue.o clip.o edges.o select.o prim.o fill.o draw.o line.o \
	compare.o alloc3.o heap.o fonts.o text.o readpat.o pat.o \
	extractor.o stifdummy.o edgepath.o
PLOTLIB = print.o queue.o clip.o edges.o select.o prim.o fill.o draw.o \
	line.o compare.o alloc3.o heap.o fonts.o text.o readpat.o pat.o\
	edgepath.o
OTHERS = main.o input.o parser.o error.o string.o interpeter.o make.o \
	layers.o symbol.o transforms.o bbox.o alloc.o plot.o extractor.o
HEADERS = alloc.h defs.h lextable.h globals.h masks.h out_structs.h \
	parser_defs.h pats.h structs.h scanner.h
LIBES =   -ll -lm
all: cifplot vdump
cifplot :   $(OBJECTS)
	  cc $(CFLAGS) -o cifplot $(OBJECTS) $(LIBES)
r : 	  $(OBJECTS)
	  cc $(CFLAGS) -o r $(OBJECTS) $(LIBES)
libplot : $(PLOTLIB)
	ar r libplot $(PLOTLIB) $(OTHERS)
vdump: dump.c
	cc $(CFLAGS) dump.c -o vdump
main.o :  main.c defs.h globals.h scanner.h
input.o : input.c defs.h globals.h scanner.h
parser.o: parser.c scanner.c defs.h globals.h structs.h lextable.h
parser.c: parser.y
error.o:  error.c defs.h globals.h scanner.h
string.o: string.c defs.h globals.h
interpeter.o: interpeter.c defs.h globals.h parser_defs.h structs.h
make.o :  make.c defs.h globals.h parser_defs.h structs.h
layers.o: layers.c defs.h globals.h parser_defs.h structs.h pats.h
symbol.o: symbol.c defs.h globals.h parser_defs.h structs.h
transforms.o : transforms.c parser_defs.h structs.h
bbox.o :  bbox.c defs.h parser_defs.h structs.h
alloc.o : alloc.c defs.h globals.h
debug.o : debug.c defs.h parser_defs.h structs.h
print.o: print.c defs.h globals.h parser_defs.h structs.h out_structs.h
plot.o: plot.c defs.h globals.h parser_defs.h structs.h out_structs.h
queue.o: queue.c defs.h
clip.o: clip.c defs.h globals.h parser_defs.h structs.h out_structs.h
edges.o: edges.c defs.h globals.h parser_defs.h structs.h out_structs.h
edgepath.o: edgepath.c defs.h globals.h parser_defs.h structs.h out_structs.h
select.o: select.c defs.h globals.h parser_defs.h structs.h out_structs.h
prim.o: prim.c defs.h globals.h parser_defs.h structs.h out_structs.h
draw.o: draw.c defs.h globals.h parser_defs.h structs.h out_structs.h
line.o: line.c defs.h globals.h parser_defs.h structs.h out_structs.h
compare.o: compare.c defs.h globals.h parser_defs.h structs.h out_structs.h
alloc3.o: alloc3.c defs.h globals.h parser_defs.h structs.h out_structs.h
fill.o: fill.c defs.h masks.h
heap.o: heap.c defs.h structs.h
fonts.o: fonts.c defs.h
text.o: text.c defs.h globals.h parser_defs.h structs.h out_structs.h
readpat.o: readpat.c defs.h
scan.c : scan.l
extractor.o: extractor.c defs.h globals.h parser_defs.h structs.h pats.h ext_defs.h
stifdummy.o : stifdummy.c
.list : $(HEADERS) $(FILES)
	/vb/grad/fitz/bin/npr $? | /usr/ucb/vpr
	@ touch .list
count : $(HEADERS) $(FILES)
	cat $? | wc
.update: defs.h globals.h parser_defs.h structs.h pats.h $(FILES)
	csh /vb/grad/fitz/bin/ppc $?
	csh /usr/ucb/vgrind -x index
	@ touch .update
.move : $(HEADERS) $(FILES)
	csh move $?
	csh move .makefile
	@ touch .move
clean:
	rm -f $(OBJECTS) vdump
install:
	install -s cifplot $(DESTDIR)/usr/ucb
	install -s vdump $(DESTDIR)/usr/ucb
