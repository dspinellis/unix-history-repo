/*-
 * Copyright (c) 1985 The Regents of the University of California.
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
 *	@(#)Kfp.h	7.1 (Berkeley) 12/6/90
 */

/*
 * Opcodes to be emulated by kernel software.
 */
#define	CVLF	0x76
#define	CVLD	0x77
#define	CVFL	0x86
#define	CVDL	0x87
#define	LDFD	0x97
#define	CVDF	0xa6
#define	ADDF   	0xc6
#define	ADDD	0xc7
#define	SUBF	0xd6
#define	SUBD	0xd7
#define	MULF	0xe6
#define	MULD	0xe7
#define	DIVF	0xf6
#define	DIVD	0xf7
#define	SINF	0x05
#define	COSF	0x15
#define	ATANF	0x25
#define	LOGF	0x35
#define	SQRTF	0x45
#define	EXPF	0x55

/* HFS bits:	*/
#define HFS_UNDF	0x008	/* float underflow */
#define HFS_OVF		0x004	/* float overflow */

#define HFS_RANGE	0x8000	/* set u_error to ERANGE */
#define HFS_DOM		0x4000	/* set u_error to EDOM */
#define HFS_DIVZ	0x2000  /* divide by zero flag */

/* destination types for the f.p. opcodes:	*/
#define LONG	01
#define FLOAT	02
#define	DOUBLE	03

/* exceptions:	*/
#define OVF_EXC		0x003	/* floating overflow */
#define UNDF_EXC	0x004 	/* floating underflow */
#define DIV0_EXC	0x005	/* floating divide by 0 */
