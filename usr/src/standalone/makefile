RELOC=50000
 
d2dcpy: d2dcpy.o
	ld d2dcpy.o ulib.a
	vrmhead a.out d2dcpy
	rm d2dcpy.o a.out
	ls -l d2dcpy
d2dcpy.o: d2dcpy.c
	cc -c d2dcpy.c
 
d2tcpy: d2tcpy.o
	ld d2tcpy.o ulib.a
	vrmhead a.out d2tcpy
	rm d2tcpy.o a.out
	ls -l d2tcpy
d2tcpy.o: d2tcpy.c
	cc -c d2tcpy.c
 
ddump: ddump.o
	ld ddump.o ulib.a
	vrmhead a.out ddump
	rm a.out ddump.o
	ls -l ddump
ddump.o: ddump.c
	cc -c ddump.c
 
fboot: fboot.o
	ld -T $(RELOC) fboot.o
	vrmhead a.out fboot
	rm a.out fboot.o
	ls -l fboot
fboot.o: fboot.c
	cc -c fboot.c
 
fdump: fdump.o
	ld fdump.o iolib.a ulib.a
	vrmhead a.out fdump
	rm a.out fdump.o
	ls -l fdump
fdump.o: fdump.c
	cc -c fdump.c
 
flpcpy: flpcpy.o
	ld flpcpy.o iolib.a ulib.a
	vrmhead a.out flpcpy
	rm a.out flpcpy.o
	ls -l flpcpy
flpcpy.o: flpcpy.c
	cc -c flpcpy.c
 
rp6fmt: rp6fmt.o
	ld rp6fmt.o ulib.a
	vrmhead a.out rp6fmt
	rm rp6fmt.o a.out
	ls -l rp6fmt
rp6fmt.o: rp6fmt.c
	cc -c rp6fmt.c
 
rpboot: rpboot.o
	ld rpboot.o
	vrmhead a.out rpboot
	rm a.out rpboot.o
	ls -l rpboot
rpboot.o: rpboot.s
	as -o rpboot.o rpboot.s
 
rpread: rpread.o
	ld rpread.o ulib.a
	vrmhead a.out rpread
	rm a.out rpread.o
	ls -l rpread
rpread.o: rpread.c
	cc -c rpread.c
 
tdcopy : tdcopy.o
	ld tdcopy.o iolib.a ulib.a
	vrmhead a.out tdcopy
	rm a.out tdcopy.o
	ls -l tdcopy
tdcopy.o : tdcopy.c
	cc -c tdcopy.c
 
vrmhead : vrmhead.o
	cc -o vrmhead vrmhead.o
	rm vrmhead.o
	ls -l vrmhead
vrmhead.o : vrmhead.c
	cc -c vrmhead.c
