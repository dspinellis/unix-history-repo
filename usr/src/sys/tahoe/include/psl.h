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
 *	@(#)psl.h	7.2 (Berkeley) 5/8/91
 */

/*
 * TAHOE processor status longword.
 */
#define	PSL_C		0x00000001	/* carry bit */
#define	PSL_V		0x00000002	/* overflow bit */
#define	PSL_Z		0x00000004	/* zero bit */
#define	PSL_N		0x00000008	/* negative bit */
#define	PSL_ALLCC	0x0000000f	/* all cc bits - unlikely */
#define	PSL_T		0x00000010	/* trace enable bit */
#define	PSL_IV		0x00000020	/* integer overflow enable bit */
#define	PSL_FU		0x00000040	/* float underflow enable 	*/
#define PSL_DBL		0x00000080	/* f.p. prescision indicator	*/
#define	PSL_SFE		0x00000100	/* system-forced-exception */
#define	PSL_IPL		0x001f0000	/* interrupt priority level */
#define	PSL_PRVMOD	0x00000000	/* previous mode (kernel mode) */
#define	PSL_CURMOD	0x01000000	/* current mode (all on is user) */
#define	PSL_IS		0x04000000	/* interrupt stack */
#define	PSL_TP		0x40000000	/* trace pending */

#define	PSL_MBZ		0xbae0fe00	/* must be zero bits */

#define	PSL_USERSET	(PSL_CURMOD)
#define	PSL_USERCLR	(PSL_IS|PSL_IPL|PSL_MBZ|PSL_SFE|PSL_DBL|PSL_FU)
