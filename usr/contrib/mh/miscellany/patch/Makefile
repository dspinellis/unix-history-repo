################################################################
#   Instructions to Make, for compilation of UIP processes
################################################################

OPTIONS	=	-DUCI -DBSD42
BINDIR	=	/usr/uci

CC      =	cc
CFLAGS  =	-O   $(OPTIONS)
LDFLAGS =	
LIBES   =


################################################################
# Here it is...
################################################################

all:		patch
inst-all:	inst-patch


################################################################
# patch
################################################################

inst-patch:	$(BINDIR)/patch

$(BINDIR)/patch:	xpatch
		-cp $@ zxpatch
		cp xpatch $@
		-@ls -l $@
		-@echo ""

patch:		xpatch

xpatch:		patch.o $(LIBES) 
		$(CC) $(LDFLAGS) -o $@ patch.o $(LIBES) 


################################################################
# clean
################################################################

clean:;		rm -f *.o x* z* _*
