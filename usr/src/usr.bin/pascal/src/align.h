/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)align.h	8.1 (Berkeley) 6/6/93
 */

    /*
     *	alignment of various types in bytes.
     *	sizes are found using sizeof( type ).
     */
#ifdef vax
#   define A_CHAR	1
#   define A_INT	4
#   define A_FLOAT	4
#   define A_DOUBLE	4
#   define A_LONG	4
#   define A_SHORT	2
#   define A_POINT	4
#   define A_STRUCT	1
#   define A_STACK	4
#   define A_FILET	4
#   define A_SET	4
#   define A_MIN	1
#   define A_MAX	4
#endif vax
#ifdef tahoe
#   define A_CHAR	1
#   define A_INT	4
#   define A_FLOAT	4
#   define A_DOUBLE	4
#   define A_LONG	4
#   define A_SHORT	2
#   define A_POINT	4
#   define A_STRUCT	4
#   define A_STACK	4
#   define A_FILET	4
#   define A_SET	4
#   define A_MIN	1
#   define A_MAX	4
#endif tahoe
#ifdef mc68000
#   define A_CHAR	1
#   define A_INT	2
#   define A_FLOAT	2
#   define A_DOUBLE	2
#   define A_LONG	2
#   define A_SHORT	2
#   define A_POINT	2
#   define A_STRUCT	2
#   define A_STACK	2
#   define A_FILET	2
#   define A_SET	2
#   define A_MIN	1
#   define A_MAX	2
#endif mc68000
