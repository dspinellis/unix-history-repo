/*-
 * Copyright (c) 1992, 1993
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
 *	@(#)seq.h	8.1 (Berkeley) 6/9/93
 */

/*
 * Map and abbreviation structures.
 *
 * The map structure is an UCHAR_MAX size array of SEQ pointers which are
 * NULL or valid depending if the offset key begins any map or abbreviation
 * sequences.  If the pointer is valid, it references a doubly linked list
 * of SEQ pointers, threaded through the snext and sprev pointers.  This is
 * based on the belief that most normal characters won't start sequences so
 * lookup will be fast on non-mapped characters.  Only a single pointer is
 * maintained to keep the overhead of the map array fairly small, so the first
 * element of the linked list has a NULL sprev pointer and the last element
 * of the list has a NULL snext pointer.  The structures in this list are
 * ordered by length, shortest to longest.  This is so that short matches 
 * will happen before long matches when the list is searched.
 *
 * In addition, each SEQ structure is on another doubly linked list of SEQ
 * pointers, threaded through the next and prev pointers.  This is a list
 * of all of the sequences.  This list is used by the routines that display
 * all of the sequences to the screen or write them to a file.
 */
					/* Sequence type. */
enum seqtype { SEQ_ABBREV, SEQ_COMMAND, SEQ_INPUT };

typedef struct _seq {
	struct _seq *next, *prev;	/* Linked list of all sequences. */
	struct _seq *snext, *sprev;	/* Linked list of ch sequences. */
	enum seqtype stype;		/* Sequence type. */
	char	*name;			/* Name of the sequence, if any. */
	char	*input;			/* Input key sequence. */
	size_t	 ilen;			/* Input key sequence length. */
	char	*output;		/* Output key sequence. */
	size_t	 olen;			/* Output key sequence length. */

#define	S_USERDEF	0x01		/* If sequence user defined. */
	u_char	 flags;
} SEQ;

int	 abbr_save __P((struct _scr *, FILE *));
int	 map_save __P((struct _scr *, FILE *));
int	 seq_delete __P((struct _scr *, char *, enum seqtype));
int	 seq_dump __P((struct _scr *, enum seqtype, int));
SEQ	*seq_find __P((struct _scr *, char *, size_t, enum seqtype, int *));
void	 seq_init __P((struct _scr *));
int	 seq_save __P((struct _scr *, FILE *, char *, enum seqtype));
int	 seq_set __P((struct _scr *,
	    char *, char *, char *, enum seqtype, int));
