#ifdef LIBC_SCCS
	.asciz	"@(#)alloca.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* like alloc, but automatic automatic free in return */

#include "DEFS.h"

ENTRY(alloca, 0)
	moval	(sp),r0		# current sp
	subl2	4(fp),r0	# allocation size
	andl2	$0xfffffffc,r0	# allignment
	movl	-8(fp),r1	# old pc
	movl	(fp),fp		# old fp
	addl2	$4*4,r0		# reuse space of mscp
	movl	r0,sp		# new sp
	jmp 	(r1)		# funny return
