# @(#)makefile	1.1
#
# Makefile for Korn Shell Library
#
D = -DKSHELL $(BSD) $(NAME_SCOPE) $(MULTIBYTE)

CC_CMD = $(CC) $D -O -c 

ARK = lib.a
OFILES = adjust.o \
arith.o \
assign.o \
assnum.o \
cannon.o \
chkid.o \
convert.o \
findnod.o \
gettree.o \
gmatch.o \
growaray.o \
gsort.o \
linknod.o \
namscan.o \
rjust.o \
tilde.o \
unassign.o \
utos.o \
valup.o 


all:	$(ARK)

ucb:	$(ARK) $(OFILES)
	ranlib $(ARK)

$(ARK):	$(OFILES)
	 ar r $(ARK) $(OFILES)

clean:
	-/bin/rm -f *.o

clobber:	clean
	-rm $(ARK)
print:
	echo $(OFILES)

########
#
#	All dependencies and rules not explicitly stated
#	(including header and nested header dependencies)
#
########

adjust.o:	adjust.c
adjust.o:	flags.h
adjust.o:	name.h
	$(CC_CMD) adjust.c

arith.o:	arith.c
arith.o:	flags.h
arith.o:	name.h
arith.o:	shtype.h
	$(CC_CMD) arith.c

assign.o:	assign.c
assign.o:	flags.h
assign.o:	name.h
assign.o:	national.h
	$(CC_CMD) assign.c

assnum.o:	assnum.c
assnum.o:	flags.h
assnum.o:	name.h
	$(CC_CMD) assnum.c

cannon.o:	cannon.c
	$(CC_CMD) cannon.c

chkid.o:	chkid.c
chkid.o:	shtype.h
	$(CC_CMD) chkid.c

convert.o:	shtype.h
convert.o:	convert.c
	$(CC_CMD) convert.c

findnod.o:	findnod.c
findnod.o:	flags.h
findnod.o:	name.h
	$(CC_CMD) findnod.c

gettree.o:	flags.h
gettree.o:	gettree.c
gettree.o:	name.h
	$(CC_CMD) gettree.c

gmatch.o:	gmatch.c
	$(CC_CMD) gmatch.c

growaray.o:	flags.h
growaray.o:	growaray.c
growaray.o:	name.h
	$(CC_CMD) growaray.c

gsort.o:	gsort.c
gsort.o:	national.h
	$(CC_CMD) gsort.c

linknod.o:	flags.h
linknod.o:	linknod.c
linknod.o:	name.h
	$(CC_CMD) linknod.c

namscan.o:	name.h
namscan.o:	namscan.c
	$(CC_CMD) namscan.c

rjust.o:	rjust.c
rjust.o:	shtype.h
	$(CC_CMD) rjust.c

tilde.o:	builtins.h
tilde.o:	flags.h
tilde.o:	name.h
tilde.o:	tilde.c
	$(CC_CMD) tilde.c

unassign.o:	flags.h
unassign.o:	name.h
unassign.o:	unassign.c
	$(CC_CMD) unassign.c

utos.o:	utos.c
	$(CC_CMD) utos.c

valup.o:	flags.h
valup.o:	name.h
valup.o:	valup.c
	$(CC_CMD) valup.c

xformx.o:	flags.h
xformx.o:	name.h
xformx.o:	xformx.c
	$(CC_CMD) xformx.c
