/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
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
 *
 *	@(#)nvram.h	8.1 (Berkeley) 6/10/93
 */

/*
 * OMRON: $Id: nvram.h,v 1.1 92/05/27 14:32:51 moti Exp $
 * by Shigeto Mochida
 */

/*
 * Non Volatile RAM
 */

#define	NVRAMSZ		2040	/* Size of NVRAM. (Total 2040 bytes) */
#define NVSYMSZ		16
#define NVVALSZ		16
#define NVSYSSZ		42
#define NVUSRSZ		512

/*
 * Battery back-up memory space.
 */
struct nvram {
	char	nv_testwrite[4];	/* for battery check */
	char	nv_hdr[4];		/* header name */
	long	nv_machtype;		/* machine-type ID */
	long	nv_machno;		/* machine number */
	char	nv_calclock[12];	/* RTC initialize */
	char	nv_checksum[4];		/* check sum for "nv_system" */
	struct nv_system {
		char	nv_symbol[NVSYMSZ];
		char	nv_value[NVVALSZ];
	} nv_system[NVSYSSZ];		/* system define */
	char	nv_reserve[152];	/* reserved */
	char	nv_user[NVUSRSZ];		/* user avail area */
};
