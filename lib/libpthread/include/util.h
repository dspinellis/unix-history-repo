/* ==== util.h ============================================================
 * Copyright (c) 1991, 1992, 1993 by Chris Provenzano, proven@athena.mit.edu	
 *
 * Description : Header file for generic utility functions.
 *
 * 91/08/31 proven - Added exchange.
 * Exchange any two objects of any size in any table.
 *
 * 91/10/06 proven - Cleaned out all the old junk. 
 *
 * 91/03/06 proven - Added getint. 
 */

/*
 * Copyright (c) 1993 by Chris Provenzano and contributors, proven@mit.edu
 * All rights reserved.
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
 *  This product includes software developed by Chris Provenzano,
 *	the University of California, Berkeley, and contributors.
 * 4. Neither the name of Chris Provenzano, the University, nor the names of
 *	  contributors may be used to endorse or promote products derived
 *	  from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY CHRIS PROVENZANO AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL CHRIS PROVENZANO, THE REGENTS OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef	NULL
#define NULL	0
#endif

#undef FALSE
#undef TRUE

typedef enum Boolean {
	FALSE,
	TRUE,
} Boolean;

#define OK					0
#define NUL					'\0'
#define NOTOK				-1

#if ! defined(min)
#define min(a,b) (((a)<(b))?(a):(b))
#define max(a,b) (((a)>(b))?(a):(b))
#endif

/* Alingn the size to the next multiple of 4 bytes */
#define ALIGN4(size)	((size + 3) & ~3)
#define ALIGN8(size)	((size + 7) & ~7)

#ifdef DEBUG
#define	DEBUG0(s)		printf(s)
#define	DEBUG1(s,a)		printf(s,a)
#define	DEBUG2(s,a,b)	printf(s,a,b)
#define	DEBUG3(s,a,b,c)	printf(s,a,b,c)
#else
#define	DEBUG0(s)	
#define	DEBUG1(s)	
#define	DEBUG2(s)	
#define	DEBUG3(s)	
#endif
