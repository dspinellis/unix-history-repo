/ C runtime startoff

/
/ Specially modified for ex to
/ allocate a "glob" buffer at the top of the stack.
/ This buffer is 922 bytes in size (at least) and will
/ thus hold up to 522 chars of arguments and 200 pointers
/ thereto.  It is described by the glob structure (in ex_glob.h).
/
/ Our job here is to set the stack point low enough to leave this
/ space and to initialize the pointer G to point at it.
/ We also put the argc of the main program in the variable
/ xargc0 and the argv thereof in xargv0. This is easier than
/ moving them down the stack.
/

.globl	savr5
.globl	_exit

.globl	_main

.globl	_G, _xargc0, _xargv0

start:
	setd
	mov	sp,r0
	mov	(r0),_xargc0
	tst	(r0)+
	mov	r0,_xargv0
	cmp	$-1024.,sp
	bhi	1f
	mov	$-1024.,sp
1:
	mov	sp,_G
	jmp	_main		/ No return
/	jsr	pc,_main
/	cmp	(sp)+,(sp)+
/	mov	r0,(sp)
/	jsr	pc,*$_exit
/	sys	exit

.bss
savr5:	.=.+2
