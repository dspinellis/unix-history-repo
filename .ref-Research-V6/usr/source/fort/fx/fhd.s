/
/

/ fhd -- data segment header
/

	symsize = 1600.
	linsize = 800.
	namsize = 1200.
	smblsize = 399.

	intcon = 4\<8+1			/ type of integer constant
	realcon = 4\<8+2
	logcon = 2\<8+3			/ type of logical constant
	log1con = 1\<8+3
	int1con = 1\<8+1
	int2con = 2\<8+1
	dblcon = 10\<8+2
	dcplxcon = 20\<8+4
	cplxcon = 10\<8+4

.globl data
.globl ibuf
.globl obuf
.globl tbuf
.globl line
.globl eline
.globl ifno
.globl efno
.globl errp
.globl errb
.globl eerrb
.globl symtab
.globl esymtab
.globl esymp
.globl symtp
.globl namebuf
.globl enamebuf
.globl namep
.globl symbuf
.globl esymbuf
.globl ch
.globl ch1
.globl progt
.globl holquo
.globl nxtaloc
.globl imptab
.globl nerror
.globl temp
.globl functm
.globl edata
.globl	dsize

