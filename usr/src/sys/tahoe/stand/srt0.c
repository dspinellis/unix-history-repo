/*	srt0.c	1.1	86/01/12	*/
/*	srt0.c	4.8	81/04/03	*/
/*  Tahoe version.	82/11/09	*/

#include "../machine/mtpr.h"
#define	LOCORE

/*
 * Startup code for standalone system
 */

	.globl	_end
	.globl	_edata
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
	movl	$(RELOC),sp
	movl	$RELOC,r0
	subl2	aedata,r0
	shar	$2,r0,r0
	clrl	r1		/* used to clear the memory */
clr:
	pushl	r1
	decl	r0		/* decrement counter */
	jgeq	clr		/* more to clear */

	movl	$RELOC,sp
	movl	$0x800,r0	/* source address to copy from */
	movl	$RELOC,r1	/* destination address */
	movl	aend,r2		/* length */
	subl2	$0x800,r2
 #	movblk
 # simple loop replaces movblk - until hardware people are ready
mvloop:
	movb	(r0),(r1)
	incl	r0
	incl	r1
	decl	r2
	bneq	mvloop
	.globl	goup
	mtpr	$0,$PACC
jumphigh:
	.set	goup,jumphigh+0x800-RELOC
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
aend:	.long	_end-RELOC+0x800
aedata:	.long	_edata-RELOC+0x800
	.globl	_entry
	.set	_entry,0x800
