/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)bsd_audiointr.s	7.3 (Berkeley) %G%
 *
 * from: $Header: bsd_audiointr.s,v 1.4 92/07/03 23:24:17 mccanne Exp $ (LBL)
 */
	
#ifndef AUDIO_C_HANDLER
#ifndef LOCORE
#define LOCORE
#endif
#include "assym.s"
#if BSD < 199103
/* SunOS */
#include <machine/intreg.h>
#define IE_L4 IR_SOFT_INT4
#else
/* 4.4BSD */
#include <sparc/sparc/intreg.h>
/* XXX this goes in a header file -- currently, it's hidden in locore.s */
#define INTREG_ADDR 0xf8002000
#endif

#define R_amd	%l2
#define R_cb	%l3
#define R_h	%l4
#define R_t	%l5

#if AUCB_SIZE > 4096
#define MASK	%l1
#else
#define MASK	AUCB_SIZE - 1
#endif

	.seg	"data"
	.align	8
audio_savepc:
	.word	0
#if AUCB_SIZE > 4096
	.word	0
#endif
	.seg	"text"
	.align	4
	.global _audio_trap
	.global	_audio_au

_audio_trap:
#if AUCB_SIZE > 4096
	set	audio_savepc, %l7
	st	%l1, [%l7]
	st	%l2, [%l7 + 4]
	set	AUCB_SIZE - 1, %l1
#else
	sethi	%hi(audio_savepc), %l7
	st	%l2, [%l7 + %lo(audio_savepc)]
#endif
	sethi	%hi(_audio_au), %l7
	ld	[%l7 + %lo(_audio_au)], %l7
	ld	[%l7 + AU_AMD], R_amd
	add	%l7, AU_RB, R_cb		! set up read cb ptr
	ldsb    [R_amd + AMD_IR], %g0		! clear interrupt
	ld	[%l7 + AU_STAMP], %l6
	inc	%l6
	st	%l6, [%l7 + AU_STAMP]		! bump time stamp

	! receive incoming data
	ld	[R_cb + CB_HEAD], R_h
	ld	[R_cb + CB_TAIL], R_t
	add	R_t, 1, %l7			! compute next tail ptr
	and	%l7, MASK, %l7
	cmp	R_h, %l7
	bne	2f
	 nop
	ld	[R_cb + CB_DROPS], %l7
	inc	%l7
	ba	7f
	 st	%l7, [R_cb + CB_DROPS]
2:
	lduh	[R_cb + CB_PAUSE], %l6
	tst	%l6
	be	3f
	 nop
	ld	[R_cb + CB_PDROPS], %l7
	inc	%l7
	ba	7f
	 st	%l7, [R_cb + CB_PDROPS]
3:
	ldsb	[R_amd + AMD_BBRB], %l6		! get sample
	add	R_t, CB_DATA, R_t		! adjust for struct offset
	stb	%l6, [R_cb + R_t]		! store sample in buffer
	st	%l7, [R_cb + CB_TAIL]		! update tail
	mov	%l7, R_t			!   "     "
7:
	ld	[R_cb + CB_THRESH], %l6
	sub	R_t, R_h, %l7			! enough data?
	and	%l7, MASK, %l7
	cmp	%l7, %l6
	bl	1f
	 nop
#if AUCB_SIZE >= 4096
	set	AUCB_SIZE, %l7
#else
	mov	AUCB_SIZE, %l7
#endif
	st	%l7, [R_cb + CB_THRESH]		! disable threshold
	mov	1, %l7
	sth	%l7, [R_cb + CB_WAKING]		! set waking

	sethi	%hi(INTREG_ADDR), %l7
	ldub	[%l7 + %lo(INTREG_ADDR)], %l6
	or	%l6, IE_L4, %l6
	stb	%l6, [%l7 + %lo(INTREG_ADDR)]	! set software interrupt
1:
	/* write */
	set	AU_WB - AU_RB, %l6		! avoid loading _audio_au ptr
	add	R_cb, %l6, R_cb			! set up write cb ptr

	ld	[R_cb + CB_HEAD], R_h
	ld	[R_cb + CB_TAIL], R_t
	cmp	R_h, R_t
	bne	2f
	 nop
	ld	[R_cb + CB_DROPS], %l7
	inc	%l7
	ba	4f
	 st	%l7, [R_cb + CB_DROPS]
2:
	lduh	[R_cb + CB_PAUSE], %l7
	tst	%l7
	be	3f
	 nop
	ld	[R_cb + CB_PDROPS], %l7
	inc	%l7
	ba	4f
	 st	%l7, [R_cb + CB_PDROPS]
3:
	inc	R_h
	and	R_h, MASK, R_h			! compute new head ptr
	st	R_h, [R_cb + CB_HEAD]
	add	R_h, CB_DATA, R_h		! adjust for struct offset
	ldsb	[R_cb + R_h], %l6		! load sample from buffer
	stb	%l6, [R_amd + AMD_BBTB]		! output sample to device
4:
	ld	[R_cb + CB_THRESH], %l6
	sub	R_t, R_h, %l7			! test if below low water
	and	%l7, MASK, %l7
	cmp	%l7, %l6
	bg	5f
	! nop

	mov	-1, %l7
	st	%l7, [R_cb + CB_THRESH]		! disable threshold
	sth	%l7, [R_cb + CB_WAKING]		! set waking
	
	! set software interrupt
	sethi	%hi(INTREG_ADDR), %l7
	ldsb	[%l7 + %lo(INTREG_ADDR)], %l6
	or	%l6, IE_L4, %l6
	stb	%l6, [%l7 + %lo(INTREG_ADDR)]
5:
	/*
	 * Restore psr -- note: psr delay honored by pc restore loads.
	 */
	mov	%l0, %psr
#if AUCB_SIZE > 4096
	sethi	%hi(audio_savepc), %l7
	ldd	[%l7 + %lo(audio_savepc)], %l2
	jmp	%l2
	rett	%l3
#else
	sethi	%hi(audio_savepc), %l7
	ld	[%l7 + %lo(audio_savepc)], %l2
	jmp	%l1
	rett	%l2
#endif
#endif
