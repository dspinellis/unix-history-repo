cp:	all
	for i in *.o; do cp $$i /usr/lib/font/`basename $$i .o`; done
	rm *.o

cmp:	all
	for i in *.o; do cmp $$i /usr/lib/font/`basename $$i .o`; done
	rm *.o

.c.o:
	cc -c $<; strip $@

all:	ftB.o ftBC.o ftC.o ftCE.o ftCI.o ftCK.o ftCS.o ftCW.o ftG.o ftGI.o ftGM.o ftGR.o
all:	ftI.o ftL.o ftLI.o ftPA.o ftPB.o ftPI.o ftR.o ftS.o ftSB.o ftSI.o ftSM.o ftUD.o ftXM.o

mkfont:mkfont.c mkfont1.c -o mkfont
	cc -n -s mkfont.c
