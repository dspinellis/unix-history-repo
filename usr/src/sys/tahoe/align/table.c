/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)table.c	7.1 (Berkeley) 12/6/90
 */

#include "align.h"
/*   WARNING !!!  Do not use register 6 and register 7 in any of the emulation
 *       code.  The macro Set_psl has use these two registers to set the
 *       user psl to the current kernel psl.
 *
 */

#define	insque	cannot_do	/* Can't emulate un-interruptable opcode ! */
#define	remque	cannot_do	/* Can't emulate un-interruptable opcode ! */
#define	addb2	add2
#define	addb3	add3
#define	addl2	add2
#define	addl3	add3
#define	addw2	add2
#define	addw3	add3
#define	andb2	and2
#define	andb3	and3
#define	andl2	and2
#define	andl3	and3
#define	andw2	and2
#define	andw3	and3
#define	atanf	not_needed
#define	kcall	cannot_do	/* Too complicated to emulate completely */
#define	bbssi	cannot_do	/* Interlock memory during emulation ??? */
#define	bcc	not_needed
#define	bcs	not_needed
#define	beql	not_needed
#define	bgeq	not_needed
#define	bgtr	not_needed
#define	bgtru	not_needed
#define	bitb	bit
#define	bitl	bit
#define	bitw	bit
#define	bleq	not_needed
#define	blequ	not_needed
#define	blss	not_needed
#define	bnequ	not_needed
#define	bpt	not_needed
#define	brb	not_needed
#define	brw	not_needed
#define	bvc	not_needed
#define	bvs	not_needed
#define	callf	not_needed 
#define	calls	not_needed 
#define	clrb	clr
#define	clrl	clr
#define	clrw	clr
#define	cmpb	cmp
#define	cmpl	cmp
#define	cmps2	not_needed
#define	cmps3	not_needed
#define	cmpw	cmp
#define	cosf	not_needed
#define	cvdf	not_needed
#define	cvtbl	cvt
#define	cvtbw	cvt
#define	cvtwl	cvt
#define	cvtwb	cvtlb
#define	decb	dec
#define	decl	dec
#define	decw	dec
#define	expf	not_needed
#define	ffs	ffs_op
#define	incb	inc
#define	incl	inc
#define	incw	inc
#define	ldpctx	not_needed
#define	logf	not_needed
#define	mcomb	mcom
#define	mcoml	mcom
#define	mcomw	mcom
#define	mnegb	mneg
#define	mnegl	mneg
#define	mnegw	mneg
#define	movab	mova
#define	moval	mova
#define	movaw	mova
#define	movow	cannot_do	/* 2 X movob != movow !! See any HW spec ! */
#define movob	movob_op
#define	movb	mov
#define	movblk	not_needed
#define	movl	mov
#define	movs2	not_needed
#define	movs3	not_needed
#define	movw	mov
#define	negd	not_needed
#define	negf	not_needed
#define	nop	not_needed
#define	orb2	or2
#define	orb3	or3
#define	orl2	or2
#define	orl3	or3
#define	orw2	or2
#define	orw3	or3
#define	pushab	pusha
#define	pushal	pusha
#define	pushaw	pusha
#define	pushb	pushx
#define	pushd	not_needed
#define	pushl	pushx
#define	pushw	pushx
#define	rei	not_needed
#define	ret	not_needed
#define	sinf	not_needed
#define	sqrtf	not_needed
#define	subb2	sub2
#define	subb3	sub3
#define	subl2	sub2
#define	subl3	sub3
#define	subw2	sub2
#define	subw3	sub3
#define	svpctx	not_needed
#define	tstb	tst
#define	tstd	not_needed
#define	tstf	not_needed
#define	tstl	tst
#define	tstw	tst
#define	xorb2	xor2
#define	xorb3	xor3
#define	xorl2	xor2
#define	xorl3	xor3
#define	xorw2	xor2
#define	xorw3	xor3
#define movzbl	movzb
#define movzbw	movzb
#define	halt	not_needed		/* Privileged to user */
#define	illegal	not_needed		/* Should be trapped by HW */
#define	mtpr	not_needed		/* Privileged to user */
#define	mfpr	not_needed		/* Privileged to user */
#define	btcs	not_needed		/* Privileged to user */

int	add2();
int	add3();
int	adda();
int	addd();
int	addf();
int	adwc();
int	and2();
int	and3();
int	aobleq();
int	aoblss();
int	bbc();
int	bbs();
int	bbssi();
int	bcc();
int	bcs();
int	beql();
int	bgeq();
int	bgtr();
int	bgtru();
int	bicpsw();
int	bispsw();
int	bit();
int	bleq();
int	blequ();
int	blss();
int	bnequ();
int	btcs();
int	bvc();
int	bvs();
int	call();
int	casel();
int	clr();
int	cmp();
int	cmpd();
int	cmpd2();
int	cmpf();
int	cmpf2();
int	cvdl();
int	cvfl();
int	cvld();
int	cvlf();
int	cvt();
int	cvt();
int	cvtlb();
int	cvtlw();
int	dec();
int	divd();
int	divf();
int	divl2();
int	divl3();
int	ediv();
int	emul();
int	ffc();
int	ffs_op();
int	halt();
int	illegal();
int	inc();
int	insque();
int	jmp();
int	kcall();
int	ldd();
int	ldf();
int	ldfd();
int	lnd();
int	lnf();
int	loadr();
int	mcom();
int	mfpr();
int	mneg();
int	mov();
int	mova();
int	movob_op();
int	movow();
int	movpsl();
int	movzb();
int	movzwl();
int	mtpr();
int	muld();
int	mulf();
int	mull2();
int	mull3();
int	or2();
int	or3();
int	prober();
int	probew();
int	pusha();
int	pushx();
int	remque();
int	sbwc();
int	shal();
int	shar();
int	shll();
int	shlq();
int	shrl();
int	shrq();
int	std();
int	stf();
int	storer();
int	sub2();
int	sub3();
int	suba();
int	subd();
int	subf();
int	tst();
int	xor2();
int	xor3();
int	not_needed();


/**************************************************/
/*  The great opcodes table, it drives everything */
/**************************************************/

struct	opcode_des	Table[]= {

/* 00 */ halt      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 01 */ halt      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 02 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 03 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 04 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 05 */ sinf      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 06 */ ldf       ,RADF,   4,   0,   0,   0,   0,   0,   0,
/* 07 */ ldd       ,RADF,   8,   0,   0,   0,   0,   0,   0,
/* 08 */ addb2     ,RADI,   1, MAD,   1,   0,   0,   0,   0,
/* 09 */ movb      ,RADI,   1,NWAD,   1,   0,   0,   0,   0,
/* 0A */ addw2     ,RADI,   2, MAD,   2,   0,   0,   0,   0,
/* 0B */ movw      ,RADI,   2,NWAD,   2,   0,   0,   0,   0,
/* 0C */ addl2     ,RADI,   4, MAD,   4,   0,   0,   0,   0,
/* 0D */ movl      ,RADI,   4,NWAD,   4,   0,   0,   0,   0,
/* 0E */ bbs       ,RADI,   4, RAD,   4, Brd,   2,   0,   0,
/* 0F */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 10 */ nop       ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 11 */ brb       , Brd,   1,   0,   0,   0,   0,   0,   0,
/* 12 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 13 */ brw       , Brd,   2,   0,   0,   0,   0,   0,   0,
/* 14 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 15 */ cosf      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 16 */ lnf       ,RADF,   4,   0,   0,   0,   0,   0,   0,
/* 17 */ lnd       ,RADF,   8,   0,   0,   0,   0,   0,   0,
/* 18 */ addb3     ,RADI,   1,RADI,   1, WAD,   1,   0,   0,
/* 19 */ cmpb      ,RADI,   1,RADI,   1,   0,   0,   0,   0,
/* 1A */ addw3     ,RADI,   2,RADI,   2, WAD,   2,   0,   0,
/* 1B */ cmpw      ,RADI,   2,RADI,   2,   0,   0,   0,   0,
/* 1C */ addl3     ,RADI,   4,RADI,   4, WAD,   4,   0,   0,
/* 1D */ cmpl      ,RADI,   4,RADI,   4,   0,   0,   0,   0,
/* 1E */ bbc       ,RADI,   4, RAD,   4, Brd,   2,   0,   0,
/* 1F */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 20 */ rei       ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 21 */ bnequ     , Brd,   1,   0,   0,   0,   0,   0,   0,
/* 22 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 23 */ cvtwl     ,RADI,   2, WAD,   4,   0,   0,   0,   0,
/* 24 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 25 */ atanf     ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 26 */ stf       , WAD,   4,   0,   0,   0,   0,   0,   0,
/* 27 */ std       , WAD,   8,   0,   0,   0,   0,   0,   0,
/* 28 */ subb2     ,RADI,   1, MAD,   1,   0,   0,   0,   0,
/* 29 */ mcomb     ,RADI,   1,NWAD,   1,   0,   0,   0,   0,
/* 2A */ subw2     ,RADI,   2, MAD,   2,   0,   0,   0,   0,
/* 2B */ mcomw     ,RADI,   2,NWAD,   2,   0,   0,   0,   0,
/* 2C */ subl2     ,RADI,   4, MAD,   4,   0,   0,   0,   0,
/* 2D */ mcoml     ,RADI,   4, WAD,   4,   0,   0,   0,   0,
/* 2E */ emul      ,RADI,   4,RADI,   4,RADI,   4, WAD,   8,
/* 2F */ aoblss    ,RADI,   4, MAD,   4, Brd,   2,   0,   0,
/* 30 */ bpt       ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 31 */ beql      , Brd,   1,   0,   0,   0,   0,   0,   0,
/* 32 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 33 */ cvtwb     ,RADI,   2, WAD,   1,   0,   0,   0,   0,
/* 34 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 35 */ logf      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 36 */ cmpf      ,RADF,   4,   0,   0,   0,   0,   0,   0,
/* 37 */ cmpd      ,RADF,   8,   0,   0,   0,   0,   0,   0,
/* 38 */ subb3     ,RADI,   1,RADI,   1, WAD,   1,   0,   0,
/* 39 */ bitb      ,RADI,   1,RADI,   1,   0,   0,   0,   0,
/* 3A */ subw3     ,RADI,   2,RADI,   2, WAD,   2,   0,   0,
/* 3B */ bitw      ,RADI,   2,RADI,   2,   0,   0,   0,   0,
/* 3C */ subl3     ,RADI,   4,RADI,   4, WAD,   4,   0,   0,
/* 3D */ bitl      ,RADI,   4,RADI,   4,   0,   0,   0,   0,
/* 3E */ ediv      ,RADI,   4,RADI,   8, WAD,   4,  WD,   4,
/* 3F */ aobleq    ,RADI,   4, MAD,   4, Brd,   2,   0,   0,
/* 40 */ ret       ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 41 */ bgtr      , Brd,   1,   0,   0,   0,   0,   0,   0,
/* 42 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 43 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 44 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 45 */ sqrtf     ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 46 */ cmpf2     ,RADF,   4,RADF,   4,   0,   0,   0,   0,
/* 47 */ cmpd2     ,RADF,   8,RADF,   8,   0,   0,   0,   0,
/* 48 */ shll      ,RADI,   1,RADI,   4, WAD,   4,   0,   0,
/* 49 */ clrb      , WAD,   1,   0,   0,   0,   0,   0,   0,
/* 4A */ shlq      ,RADI,   1,RADI,   8, WAD,   8,   0,   0,
/* 4B */ clrw      , WAD,   2,   0,   0,   0,   0,   0,   0,
/* 4C */ mull2     ,RADI,   4, MAD,   4,   0,   0,   0,   0, 
/* 4D */ clrl      , WAD,   4,   0,   0,   0,   0,   0,   0,
/* 4E */ shal      ,RADI,   1,RADI,   4, WAD,   4,   0,   0,
/* 4F */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 50 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 51 */ bleq      , Brd,   1,   0,   0,   0,   0,   0,   0,
/* 52 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 53 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 54 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 55 */ expf      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 56 */ tstf      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 57 */ tstd      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 58 */ shrl      ,RADI,   1,RADI,   4, WAD,   4,   0,   0,
/* 59 */ tstb      ,RADI,   1,   0,   0,   0,   0,   0,   0,
/* 5A */ shrq      ,RADI,   1,RADI,   8, WAD,   8,   0,   0,
/* 5B */ tstw      ,RADI,   2,   0,   0,   0,   0,   0,   0,
/* 5C */ mull3     ,RADI,   4,RADI,   4, WAD,   4,   0,   0,
/* 5D */ tstl      ,RADI,   4,   0,   0,   0,   0,   0,   0,
/* 5E */ shar      ,RADI,   1,RADI,   4, WAD,   4,   0,   0,
/* 5F */ bbssi     ,RADI,   4, MAD,   4, Brd,   2,   0,   0,
/* 60 */ ldpctx    ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 61 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 62 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 63 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 64 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 65 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 66 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 67 */ pushd     ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 68 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 69 */ incb      , MAD,   1,   0,   0,   0,   0,   0,   0,
/* 6A */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 6B */ incw      , MAD,   2,   0,   0,   0,   0,   0,   0,
/* 6C */ divl2     ,RADI,   4, MAD,   4,   0,   0,   0,   0,
/* 6D */ incl      , MAD,   4,   0,   0,   0,   0,   0,   0,
/* 6E */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 6F */ cvtlb     ,RADI,   4, WAD,   1,   0,   0,   0,   0,
/* 70 */ svpctx    ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 71 */ jmp       ,ADDR,   1,   0,   0,   0,   0,   0,   0,
/* 72 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 73 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 74 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 75 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 76 */ cvlf      , RAD,   4,   0,   0,   0,   0,   0,   0,
/* 77 */ cvld      , RAD,   4,   0,   0,   0,   0,   0,   0,
/* 78 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 79 */ decb      , MAD,   1,   0,   0,   0,   0,   0,   0,
/* 7A */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 7B */ decw      , MAD,   2,   0,   0,   0,   0,   0,   0,
/* 7C */ divl3     ,RADI,   4,RADI,   4, WAD,   4,   0,   0,
/* 7D */ decl      , MAD,   4,   0,   0,   0,   0,   0,   0,
/* 7E */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 7F */ cvtlw     ,RADI,   4, WAD,   2,   0,   0,   0,   0,
/* 80 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 81 */ bgeq      , Brd,   1,   0,   0,   0,   0,   0,   0,
/* 82 */ movs2     ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 83 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 84 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 85 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 86 */ cvfl      , WAD,   4,   0,   0,   0,   0,   0,   0,
/* 87 */ cvdl      , WAD,   4,   0,   0,   0,   0,   0,   0,
/* 88 */ orb2      ,RADI,   1,NMAD,   1,   0,   0,   0,   0, 
/* 89 */ cvtbl     ,RADI,   1, WAD,   4,   0,   0,   0,   0,
/* 8A */ orw2      ,RADI,   2,NMAD,   2,   0,   0,   0,   0,
/* 8B */ bispsw    ,RADI,   2,   0,   0,   0,   0,   0,   0,
/* 8C */ orl2      ,RADI,   4, MAD,   4,   0,   0,   0,   0,
/* 8D */ adwc      ,RADI,   4, MAD,   4,   0,   0,   0,   0,
/* 8E */ adda      ,RADI,   4, MAD,   4,   0,   0,   0,   0,
/* 8F */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 90 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 91 */ blss      , Brd,   1,   0,   0,   0,   0,   0,   0,
/* 92 */ cmps2     ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 93 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 94 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 95 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 96 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* 97 */ ldfd      ,RADF,   4,   0,   0,   0,   0,   0,   0,
/* 98 */ orb3      ,RADI,   1,RADI,   1,NWAD,   1,   0,   0,
/* 99 */ cvtbw     ,RADI,   1, WAD,   2,   0,   0,   0,   0,
/* 9A */ orw3      ,RADI,   2,RADI,   2,NWAD,   2,   0,   0,
/* 9B */ bicpsw    ,RADI,   2,   0,   0,   0,   0,   0,   0,
/* 9C */ orl3      ,RADI,   4,RADI,   4, WAD,   4,   0,   0,
/* 9D */ sbwc      ,RADI,   4, MAD,   4,   0,   0,   0,   0,
/* 9E */ suba      ,RADI,   4, MAD,   4,   0,   0,   0,   0,
/* 9F */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* A0 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* A1 */ bgtru     , Brd,   1,   0,   0,   0,   0,   0,   0,
/* A2 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* A3 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* A4 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* A5 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* A6 */ cvdf      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* A7 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* A8 */ andb2     ,RADI,   1,NMAD,   1,   0,   0,   0,   0,
/* A9 */ movzbl    ,RADI,   1, WAD,   4,   0,   0,   0,   0,
/* AA */ andw2     ,RADI,   2,NMAD,   2,   0,   0,   0,   0,
/* AB */ loadr     ,RADI,   2,ADDR,   4,   0,   0,   0,   0,
/* AC */ andl2     ,RADI,   4, MAD,   4,   0,   0,   0,   0,
/* AD */ mtpr      ,RADI,   4,RADI,   4,   0,   0,   0,   0,
/* AE */ ffs       ,RADI,   4, WAD,   4,   0,   0,   0,   0,
/* AF */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* B0 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* B1 */ blequ     , Brd,   1,   0,   0,   0,   0,   0,   0,
/* B2 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* B3 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* B4 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* B5 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* B6 */ negf      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* B7 */ negd      ,   0,   0,   0,   0,   0,   0,   0,   0,
/* B8 */ andb3     ,RADI,   1,RADI,   1,NWAD,   1,   0,   0,
/* B9 */ movzbw    ,RADI,   1, WAD,   2,   0,   0,   0,   0,
/* BA */ andw3     ,RADI,   2,RADI,   2,NWAD,   2,   0,   0,
/* BB */ storer    ,RADI,   2,W|ADDR, 4,   0,   0,   0,   0,
/* BC */ andl3     ,RADI,   4,RADI,   4, WAD,   4,   0,   0,
/* BD */ mfpr      ,RADI,   4, WAD,   4,   0,   0,   0,   0,
/* BE */ ffc       ,RADI,   4, WAD,   4,   0,   0,   0,   0,
/* BF */ calls     ,RADI,   1,ADDR,   1,   0,   0,   0,   0,
/* C0 */ prober    ,RADI,   1,ADDR,   1,RADI,   4,   0,   0,
/* C1 */ bvc       , Brd,   1,   0,   0,   0,   0,   0,   0,
/* C2 */ movs3     ,   0,   0,   0,   0,   0,   0,   0,   0,
/* C3 */ movzwl    ,RADI,   2, WAD,   4,   0,   0,   0,   0,
/* C4 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* C5 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* C6 */ addf      ,RADF,   4,   0,   0,   0,   0,   0,   0,
/* C7 */ addd      ,RADF,   8,   0,   0,   0,   0,   0,   0,
/* C8 */ xorb2     ,RADI,   1,NMAD,   1,   0,   0,   0,   0,
/* C9 */ movob     ,RADI,   1,NWAD,   1,   0,   0,   0,   0,
/* CA */ xorw2     ,RADI,   2,NMAD,   2,   0,   0,   0,   0,
/* CB */ movow     ,RADI,   2,NWAD,   2,   0,   0,   0,   0,
/* CC */ xorl2     ,RADI,   4, MAD,   4,   0,   0,   0,   0,
/* CD */ movpsl    , WAD,   4,   0,   0,   0,   0,   0,   0,
/* CE */ btcs      ,RADI,   1,   0,   0,   0,   0,   0,   0,
/* CF */ kcall     ,RADI,   2,   0,   0,   0,   0,   0,   0,
/* D0 */ probew    ,RADI,   1,ADDR,   1,RADI,   4,   0,   0,
/* D1 */ bvs       , Brd,   1,   0,   0,   0,   0,   0,   0,
/* D2 */ cmps3     ,   0,   0,   0,   0,   0,   0,   0,   0,
/* D3 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* D4 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* D5 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* D6 */ subf      ,RADF,   4,   0,   0,   0,   0,   0,   0,
/* D7 */ subd      ,RADF,   8,   0,   0,   0,   0,   0,   0,
/* D8 */ xorb3     ,RADI,   1,RADI,   1,NWAD,   1,   0,   0,
/* D9 */ pushb     ,RADI,   1,   0,   0,   0,   0,   0,   0,
/* DA */ xorw3     ,RADI,   2,RADI,   2,NWAD,   2,   0,   0,
/* DB */ pushw     ,RADI,   2,   0,   0,   0,   0,   0,   0,
/* DC */ xorl3     ,RADI,   4,RADI,   4, WAD,   4,   0,   0,
/* DD */ pushl     ,RADI,   4,   0,   0,   0,   0,   0,   0,
/* DE */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* DF */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* E0 */ insque    ,ADDR,   4,ADDR,   4,   0,   0,   0,   0,
/* E1 */ bcs       , Brd,   1,   0,   0,   0,   0,   0,   0,
/* E2 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* E3 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* E4 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* E5 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* E6 */ mulf      ,RADF,   4,   0,   0,   0,   0,   0,   0,
/* E7 */ muld      ,RADF,   8,   0,   0,   0,   0,   0,   0,
/* E8 */ mnegb     ,RADI,   1, WAD,   1,   0,   0,   0,   0,
/* E9 */ movab     ,ADDR,   1, WAD,   4,   0,   0,   0,   0,
/* EA */ mnegw     ,RADI,   2, WAD,   2,   0,   0,   0,   0,
/* EB */ movaw     ,ADDR,   2, WAD,   4,   0,   0,   0,   0,
/* EC */ mnegl     ,RADI,   4, WAD,   4,   0,   0,   0,   0,
/* ED */ moval     ,ADDR,   4, WAD,   4,   0,   0,   0,   0,
/* EE */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* EF */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* F0 */ remque    ,ADDR,   4,   0,   0,   0,   0,   0,   0,
/* F1 */ bcc       , Brd,   1,   0,   0,   0,   0,   0,   0,
/* F2 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* F3 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* F4 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* F5 */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* F6 */ divf      ,RADF,   4,   0,   0,   0,   0,   0,   0,
/* F7 */ divd      ,RADF,   8,   0,   0,   0,   0,   0,   0,
/* F8 */ movblk    ,   0,   0,   0,   0,   0,   0,   0,   0,
/* F9 */ pushab    ,ADDR,   1,   0,   0,   0,   0,   0,   0,
/* FA */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0,
/* FB */ pushaw    ,ADDR,   2,   0,   0,   0,   0,   0,   0,
/* FC */ casel     ,RADI,   4,RADI,   4,RADI,   4,   0,   0,
/* FD */ pushal    ,ADDR,   4,   0,   0,   0,   0,   0,   0,
/* FE */ callf    ,Imm|Lit, 1,  PR,   1,   0,   0,   0,   0,
/* FF */ illegal   ,   0,   0,   0,   0,   0,   0,   0,   0
};
