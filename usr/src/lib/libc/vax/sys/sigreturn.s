/* sigreturn.s 4.2 85/05/27 */

#include "SYS.h"

/*
 * We must preserve the state of the registers as the user has set them up.
 */
#ifdef PROF
#undef ENTRY
#define	ENTRY(x) \
	.globl _/**/x; .align 2; _/**/x: .word 0; pushr $0x3f; \
	.data; 1:; .long 0; .text; moval 1b,r0; jsb mcount; popr $0x3f
#endif PROF

SYSCALL(sigreturn)
	ret
