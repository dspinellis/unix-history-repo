/*-
 * Copyright (c) 1994
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
 *	@(#)util.h	9.4 (Berkeley) 12/3/94
 */

/*
 * XXX
 * MIN/MAX have traditionally been in <sys/param.h>.  Don't
 * try to get them from there, it's just not worth the effort.
 */
#ifndef	MAX
#define	MAX(_a,_b)	((_a)<(_b)?(_b):(_a))
#endif
#ifndef	MIN
#define	MIN(_a,_b)	((_a)<(_b)?(_a):(_b))
#endif

/*
 * Number handling defines and protoypes.
 *
 * NNFITS:	test for addition of two negative numbers under a limit
 * NPFITS:	test for addition of two positive numbers under a limit
 * NADD_SLONG:	test for addition of two signed longs
 * NADD_USLONG:	test for addition of two unsigned longs
 */
enum nresult { NUM_ERR, NUM_OK, NUM_OVER, NUM_UNDER };
#define	NNFITS(min, cur, add)						\
	(((long)(min)) - (cur) <= (add))
#define	NPFITS(max, cur, add)						\
	(((unsigned long)(max)) - (cur) >= (add))
#define	NADD_SLONG(sp, v1, v2)						\
	((v1) < 0 ?							\
	    ((v2) < 0 &&						\
	    NNFITS(LONG_MIN, (v1), (v2))) ? NUM_UNDER : NUM_OK :	\
	 (v1) > 0 ?							\
	    (v2) > 0 &&							\
	    NPFITS(LONG_MAX, (v1), (v2)) ? NUM_OK : NUM_OVER :		\
	 NUM_OK)
#define	NADD_USLONG(sp, v1, v2)						\
	(NPFITS(ULONG_MAX, (v1), (v2)) ? NUM_OK : NUM_OVER)
enum nresult nget_slong __P((SCR *, long *, char *, char **, int));
enum nresult nget_uslong __P((SCR *, u_long *, char *, char **, int));

/* Digraphs (not currently real). */
int	digraph __P((SCR *, int, int));
int	digraph_init __P((SCR *));
void	digraph_save __P((SCR *, int));

/* Function prototypes that don't seem to belong anywhere else. */
int	 nonblank __P((SCR *, recno_t, size_t *));
void	 set_alt_name __P((SCR *, char *));
char	*tail __P((char *));
CHAR_T	*v_strdup __P((SCR *, const CHAR_T *, size_t));
void	 vi_putchar __P((int));

#ifdef DEBUG
void	TRACE __P((SCR *, const char *, ...));
#endif
