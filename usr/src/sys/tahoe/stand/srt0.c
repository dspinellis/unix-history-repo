/*	srt0.c	1.2	86/07/13	*/

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

#ifndef	REL
	.word	0x00			# 'call' by relsrt0.
#endif
_start:
	mtpr	$HIGH,$IPL		# just in case
	movl	$RELOC,sp

	movl	$0x800,r0	/* source address to copy from */
	movl	$RELOC,r1	/* destination address */
	movl	aend,r2		/* length */
	addl2	r2,r0
	addl2	r2,r1
mvloop:
	decl	r0
	decl	r1
	movb	(r0),(r1)
	decl	r2
	bgeq	mvloop

	mtpr	$0,$PACC
	jmp	*abegin

begin:
	movl	$1,_openfirst
	callf	$4,_main
	jmp	begin

__rtt:
	.word	0x0
	jmp	begin

	.data
abegin:	.long	begin
aend:	.long	_end-RELOC-0x800
aedata:	.long	_edata-RELOC-0x800
	.globl	_entry
	.set	_entry,0x800
