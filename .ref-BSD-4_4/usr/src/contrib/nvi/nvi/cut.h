/*-
 * Copyright (c) 1991, 1993
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
 *	@(#)cut.h	8.1 (Berkeley) 6/9/93
 */

typedef struct _cb {			/* Cut buffer. */
	struct _hdr	txthdr;		/* Linked list of TEXT structures. */
	u_long	 len;			/* Total length of cut text. */

#define	CB_LMODE	0x01		/* Line mode. */
	u_char	 flags;
} CB;
		
typedef struct _text {			/* Text: a linked list of lines. */
	struct _text *next, *prev;	/* Linked list of text structures. */
	char	*lb;			/* Line buffer. */
	size_t	 lb_len;		/* Line buffer length. */
	size_t	 len;			/* Line length. */

	/* These fields are used by the vi text input routine. */
	recno_t	 lno;			/* 1-N: line number. */
	size_t	 ai;			/* 0-N: autoindent bytes. */
	size_t	 insert;		/* 0-N: bytes to insert (push). */
	size_t	 offset;		/* 0-N: initial, unerasable bytes. */
	size_t	 overwrite;		/* 0-N: bytes to overwrite. */

	/* These fields are used by the ex text input routine. */
	u_char	*wd;			/* Width buffer. */
	size_t	 wd_len;		/* Width buffer length. */
} TEXT;

#define	OOBCB	-1			/* Out-of-band cut buffer name. */
#define	DEFCB	UCHAR_MAX + 1		/* Default cut buffer. */

					/* Vi: cut buffer to use. */
#define	VICB(vp)	((vp)->buffer == OOBCB ? DEFCB : (vp)->buffer)

/* Check to see if a cut buffer has contents. */
#define	CBEMPTY(sp, buf, cb) {						\
	if ((cb)->txthdr.next == NULL ||				\
	    (cb)->txthdr.next == &(cb)->txthdr) {			\
		if (buf == DEFCB)					\
			msgq(sp, M_ERR,					\
			    "The default buffer is empty.");		\
		else							\
			msgq(sp, M_ERR,					\
			    "Buffer %s is empty.", charname(sp, buf));	\
		return (1);						\
	}								\
}

/*
 * Check a buffer name for validity.
 * Translate upper-case buffer names to lower-case buffer names.
 */
#define	CBNAME(sp, bname, cb) {						\
	if ((bname) > sizeof(sp->cuts) - 1) {				\
		msgq(sp, M_ERR, "Invalid cut buffer name.");		\
		return (1);						\
	}								\
	if (isupper(bname))						\
		(bname) = tolower(bname);				\
	cb = &sp->cuts[bname];						\
}

/* Cut routines. */
int	 cut __P((struct _scr *,
	    struct _exf *, int, struct _mark *, struct _mark *, int));
int	 delete __P((struct _scr *,
	    struct _exf *, struct _mark *, struct _mark *, int));
int	 put __P((struct _scr *,
	    struct _exf *, int, struct _mark *, struct _mark *, int));
void	 text_free __P((TEXT *));
void	 hdr_text_free __P((struct _hdr *));
TEXT	*text_init __P((struct _scr *, char *, size_t, size_t));
