/ C library -- exit

/	@(#)exit.PDP.s	7.1	2/5/81

/ exit(code)
/ code is return in r0 to system

/ modified by INGRES group to call _cleanup (standard v7)
/ and to require the loading of '_putchar' (so that
/ the library loads correctly).

.globl	_exit
.globl	_putchar
.globl	__cleanup

_exit:
	mov	r5,-(sp)
	mov	sp,r5
	jsr	pc,__cleanup
	mov	4(r5),r0
	sys	exit

	jsr	pc,_putchar	/ doesn't actually get called.
