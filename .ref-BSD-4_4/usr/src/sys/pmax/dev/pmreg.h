/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
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
 *	@(#)pmreg.h	8.1 (Berkeley) 6/10/93
 */

/*
 * Defines for the graphics hardware.
 */

/*
 * Structure to descripte the programmable cursor chip (DC503) from DEC.
 */
typedef volatile struct PCCRegs {
	u_short	cmdr;	/* cursor command register */
	short	pad1;
	u_short	xpos;	/* cursor X position */
	short	pad2;
	u_short	ypos;	/* cursor Y position */
	short	pad3;
	u_short	xmin1;	/* region 1 left edge */
	short	pad4;
	u_short	xmax1;	/* region 1 right edge */
	short	pad5;
	u_short	ymin1;	/* region 1 top edge */
	short	pad6;
	u_short	ymax1;	/* region 1 bottom edge */
	short	pad7[9];
	u_short	xmin2;	/* region 2 left edge */
	short	pad8;
	u_short	xmax2;	/* region 2 right edge */
	short	pad9;
	u_short	ymin2;	/* region 2 top edge */
	short	pad10;
	u_short	ymax2;	/* region 2 bottom edge */
	short	pad11;
	u_short	memory;	/* cursor sprite pattern load */
} PCCRegs;

	/* define bits in pcc_cmdr */
#define PCC_TEST	0x8000
#define PCC_HSHI	0x4000
#define PCC_VBHI	0x2000
#define PCC_LODSA	0x1000
#define PCC_FORG2	0x0800
#define PCC_ENRG2	0x0400
#define PCC_FORG1	0x0200
#define PCC_ENRG1	0x0100
#define PCC_XHWID	0x0080
#define PCC_XHCL1	0x0040
#define PCC_XHCLP	0x0020
#define PCC_XHAIR	0x0010
#define PCC_FOPB	0x0008
#define PCC_ENPB	0x0004
#define PCC_FOPA	0x0002
#define PCC_ENPA	0x0001

	/* offset for cursor X & Y locations */
#define PCC_X_OFFSET	212
#define PCC_Y_OFFSET	34

typedef volatile struct VDACRegs {
	u_char	mapWA;		/* address register (color map write) */
	char	pad1[3];
	u_char	map;		/* color map */
	char	pad2[3];
	u_char	mask;		/* pixel read mask */
	char	pad3[3];
	u_char	mapRA;		/* address register (color map read) */
	char	pad4[3];
	u_char	overWA;		/* address register (overlay map write) */
	char	pad5[3];
	u_char	over;		/* overlay map */
	char	pad6[7];
	u_char	overRA;		/* address register (overlay map read) */
} VDACRegs;
