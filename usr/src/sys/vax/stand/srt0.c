/*	srt0.c	4.6	81/03/21	*/

#include "../h/mtpr.h"

/*
 * Startup code for standalone system
 * Non-relocating version -- for programs which are loaded by boot
 */

	.globl	_edata
	.globl	_main
	.globl	__rtt
	.globl	_configure
	.set	reloc,RELOC

	.set	HIGH,31		# mask for total disable

entry:	.globl	entry
	.word	0x0
	mtpr	$HIGH,$IPL		# just in case
	movl	$reloc-0x2400,sp
start:
	movab	_edata,r0
clr:
	clrl	(r0)+
	cmpl	r0,sp
	jlss	clr
	calls	$0,_configure
	calls	$0,_main
	jmp	start

__rtt:
	.word	0x0
	jmp	start
