#include "../h/config.h"
#ifdef VAX
/* No csv type stuff on the Vax, the procedure call mechanism
 *  handles it all.
 */
#endif VAX

#ifdef PDP11
.globl	_boundary		/ Icon/C boundary address

/ csv,cret - modified C calling sequence to save registers,
/ restore on return, and clear Icon/C boundary on return
/ to Icon code.  This sequence performs the same function as the
/ sequence in the C library, except for setting and clearing the
/ boundary.  When returning from C to Icon, arguments are popped.

.globl	csv, cret
csv:
	tst	_boundary	/ is call being made from Icon to C?
	bne	1f		/   no, branch
	mov	sp,_boundary	/ yes, set boundary
1:
	mov	r5,r0		/ make new procedure frame
	mov	sp,r5
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	jsr	pc,(r0)		/ enter procedure

cret:
	cmp	r5,_boundary	/ is return being made from C to Icon?
	bne	1f		/   no, branch
	clr	_boundary	/ yes, clear boundary address
	mov	r5,sp		/ restore old procedure frame
	mov	-(r5),r4
	mov	-(r5),r3
	mov	-(r5),r2
	mov	(sp)+,r5
	mov	(sp)+,r1	/ pop return pc
	mov	(sp)+,r0	/ pop nargs
	asl	r0
	asl	r0
	add	r0,sp		/ pop all arguments
	jmp	(r1)		/ return to Icon procedure
1:
	mov	r5,sp		/ restore old procedure frame
	mov	-(r5),r4
	mov	-(r5),r3
	mov	-(r5),r2
	mov	(sp)+,r5
	rts	pc		/ return to C procedure
#endif PDP11
