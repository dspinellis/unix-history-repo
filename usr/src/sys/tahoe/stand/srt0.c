/*	srt0.c	1.4	86/12/19	*/

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
	.globl	_device_space
_device_space:			# Tapemaster config block, etc.
	.space	0x1000-RELOC

	.globl	_entry
_entry:
	.word	0x00			# 'call' by relsrt0.
#endif
_start:
	mtpr	$HIGH,$IPL		# just in case
#ifdef REL
	movl	$BOOTRELOC,sp

	movl	$RELOC,r0	/* source address to copy from */
	movl	$BOOTRELOC,r1	/* destination address */
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
	mtpr	$0,$PADC
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
#ifdef REL
aend:	.long	_end-BOOTRELOC
aedata:	.long	_edata-BOOTRELOC
#else
aend:	.long	_end-RELOC
aedata:	.long	_edata-RELOC
#endif
ofp:	.long	0
