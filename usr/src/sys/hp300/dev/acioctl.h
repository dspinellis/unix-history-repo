/*
 * Copyright (c) 1991 University of Utah.
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: acioctl.h 1.1 91/06/19$
 *
 *	@(#)acioctl.h	8.1 (Berkeley) 6/10/93
 */

struct acinfo {
	short	fmte;		/* 1st medium transport elt (picker) */
	short	nmte;		/* # medium transport elts */
	short	fse;		/* 1st storage elt (slot) */
	short	nse;		/* # storage elts */
	short	fiee;		/* 1st import/export elt (mailslot) */
	short	niee;		/* # import/export elts */
	short	fdte;		/* 1st data transport elt (drive) */
	short	ndte;		/* # data transport elts */
};

struct aceltstat {
	short	eaddr;		/* element adress */
	char	type;		/* type of element */
	char	flags;		/* flags */
};

/* types */
#define AC_MTE		0x01	/* picker */
#define AC_SE		0x02	/* slot */
#define AC_IEE		0x03	/* mailslot */
#define AC_DTE		0x04	/* drive */
/* flags */
#define AC_FULL		0x01	/* media present */
#define	AC_ERROR	0x04	/* error accessing element */
#define AC_ACCESS	0x08	/* element accessible */
#define AC_INVERT	0x80	/* media inverted prior to insertion */

struct acmove {
	short	srcelem;
	short	dstelem;
	short	flags;
};

struct acbuffer {
	char	*bufptr;
	int	buflen;
};

#define ACIOCINIT	_IO('A', 0x1)			/* init elt status */
#define ACIOCGINFO	_IOR('A', 0x2, struct acinfo)	/* mode sense */
#define ACIOCGSTAT	_IOW('A', 0x3, struct acbuffer)	/* read elem status */
#define ACIOCMOVE	_IOW('A', 0x4, struct acmove)	/* move elem */
#define ACIOCRAWES	_IOW('A', 0x5, struct acbuffer)	/* raw element stat */
