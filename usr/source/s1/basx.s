/
/ copyright 1972 bell telephone laboratories inc.
/

/ basx -- data

one = 40200

b.out:	<b.out\0>
vt:	</dev/vt0\0>
	.even

resnam:
	<list>
	<done>
	<run\0>
	<prin>
	<disp>
	<if\0\0>
	<goto>
	<retu>
	<for\0>
	<next>
	<octa>
	<draw>
	<eras>
	<prom>
	<save>
	<dump>
eresnam:

symtnam:
	<arg\0>
	<exp\0>
	<log\0>
	<sin\0>
	<cos\0>
	<atn\0>
	<rnd\0>
	<expr>
	<int\0>
	<abs\0>
	<sqr\0>
esymtnam:

.data
fo:	1
tmpf:	</tmp/btma\0>
	.even

.bss
drx:	.=.+8
dry:	.=.+8
drfo:	.=.+2
ch:	.=.+1
drflg:	.=.+1
randx:	.=.+2
fsgn:	.=.+2
gsp:	.=.+2
forp:	.=.+2
exprloc:.=.+2
sstack:	.=.+2
sublev:	.=.+2
val:	.=.+2
line:	.=.+100.
tfi:	.=.+2
fi:	.=.+2
lineno:	.=.+2
nameb:	.=.+6
tfo:	.=.+2
symtab:	.=.+1000.; esymtab:
space:	.=.+5000.; espace:
exline:	.=.+1000.; eexline:
lintab:	.=.+1000.; elintab:
stack:	.=.+1000.; estack:

