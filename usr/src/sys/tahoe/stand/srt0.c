/*	srt0.c	1.3	86/11/03	*/

#include "../machine/mtpr.h"
#define	LOCORE

/*
 * Startup code for standalone system
 */

	.globl	_end
	.globl	_main
	.globl	__rtt
	.globl	_openfirst
	.globl	_start

	.set	HIGH,31		# mask for total disable

	.globl	_entry
_entry:
#ifndef	REL
	.word	0x00			# 'call' by relsrt0.
#endif
_start:
	mtpr	$HIGH,$IPL		# just in case
#ifdef REL
	movl	$RELOC,sp

	movl	$0x800,r0	/* source address to copy from */
	movl	$RELOC,r1	/* destination address */
	movl	aedata,r2	/* length to copy */
	addl2	r2,r0
	addl2	r2,r1
mvloop:
	decl	r0
	decl	r1
	movb	(r0),(r1)
	decl	r2
	bgeq	mvloop

/*
 * zero bss
 */
	movab	_edata,r1	/* destination address */
	subl3	aend,aedata,r2	/* length to zero */
zloop:
	movb	$0,(r1)
	incl	r1
	decl	r2
	bgeq	zloop

	mtpr	$0,$PACC
	jmp	*abegin
#endif

begin:
	movl	fp,ofp
	movl	$1,_openfirst
	callf	$4,_main
#ifdef REL
	halt
#endif
	ret

__rtt:
	.word	0x0
#ifdef REL
	halt
#endif
	movl	ofp,fp
	ret

	.data
abegin:	.long	begin
aend:	.long	_end-RELOC
aedata:	.long	_edata-RELOC
ofp:	.long	0
