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
 */

#ifndef lint
static char sccsid[] = "@(#)seq.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "excmd.h"

/*
 * seq_set --
 *	Internal version to enter a sequence.
 */
int
seq_set(sp, name, input, output, stype, userdef)
	SCR *sp;
	char *name, *input, *output;
	enum seqtype stype;
	int userdef;
{
	HDR *hp;
	SEQ *ip, *qp;
	int ilen;

#if DEBUG && 0
	TRACE(sp, "seq_set: name {%s} input {%s} output {%s}\n",
	    name ? name : "", input, output);
#endif
	/* Find any previous occurrence, and replace the output field. */
	ilen = strlen(input);

	ip = NULL;
	hp = &sp->seq[*input];
	if (hp->snext == NULL) {
		HDR_INIT(sp->seq[*input], snext, sprev);
	} else for (qp = hp->snext;
	    qp != (SEQ *)hp && qp->ilen <= ilen; ip = qp, qp = qp->snext)
		if (qp->ilen == ilen && stype == qp->stype &&
		    !strcmp(qp->input, input)) {
			free(qp->output);
			if ((qp->output = strdup(output)) == NULL)
				goto mem1;
			return (0);
		}

	/* Allocate and initialize space. */
	if ((qp = malloc(sizeof(SEQ))) == NULL) 
		goto mem1;
	if (name == NULL)
		qp->name = NULL;
	else if ((qp->name = strdup(name)) == NULL)
		goto mem2;
	if ((qp->input = strdup(input)) == NULL)
		goto mem3;
	if ((qp->output = strdup(output)) == NULL) {
		free(qp->input);
mem3:		if (qp->name)
			free(qp->name);
mem2:		free(qp);
mem1:		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}

	qp->stype = stype;
	qp->ilen = ilen;
	qp->olen = strlen(output);;
	qp->flags = userdef ? S_USERDEF : 0;

	/* Link into the chains. */
	HDR_INSERT(qp, &sp->seqhdr, next, prev, SEQ);
	if (ip == NULL) {
		HDR_APPEND(qp, hp, snext, sprev, SEQ);
	} else
		HDR_INSERT(qp, ip, snext, sprev, SEQ);
	return (0);
}

/*
 * seq_delete --
 *	Delete a sequence.
 */
int
seq_delete(sp, input, stype)
	SCR *sp;
	char *input;
	enum seqtype stype;
{
	register SEQ *qp;

	if ((qp = seq_find(sp, input, strlen(input), stype, NULL)) == NULL)
		return (1);

	HDR_DELETE(qp, next, prev, SEQ);
	HDR_DELETE(qp, snext, sprev, SEQ);

	/* Free up the space. */
	if (qp->name)
		FREE(qp->name, strlen(qp->name));
	FREE(qp->input, qp->ilen);
	FREE(qp->output, qp->olen);
	FREE(qp, sizeof(SEQ));
	return (0);
}

/*
 * seq_find --
 *	Search the sequence list for a match to a buffer, if ispartial
 *	isn't NULL, partial matches count.
 */
SEQ *
seq_find(sp, input, ilen, stype, ispartialp)
	SCR *sp;
	char *input;
	size_t ilen;
	enum seqtype stype;
	int *ispartialp;
{
	HDR *hp;
	SEQ *qp;

	hp = &sp->seq[*input];
	if (hp->snext == NULL)
		return (NULL);
	if (ispartialp) {
		*ispartialp = 0;
		for (qp = hp->snext; qp != (SEQ *)hp; qp = qp->snext) {
			if (stype != qp->stype)
				continue;
			/*
			 * If sequence is shorter or the same length as the
			 * input, can only find an exact match.  If input is
			 * shorter than the sequence, can only find a partial.
			 */
			if (qp->ilen <= ilen) {
				if (!strncmp(qp->input, input, qp->ilen))
					return (qp);
			} else {
				if (!strncmp(qp->input, input, ilen))
					*ispartialp = 1;
			}
		}
	} else
		for (qp = hp->snext; qp != (SEQ *)hp; qp = qp->snext)
			if (stype == qp->stype && qp->ilen == ilen &&
			    !strncmp(qp->input, input, ilen))
				return (qp);
	return (NULL);
}

/*
 * seq_dump --
 *	Display the sequence entries of a specified type.
 */
int
seq_dump(sp, stype, isname)
	SCR *sp;
	enum seqtype stype;
	int isname;
{
	register SEQ *qp;
	register int ch, cnt, len, tablen;
	register char *p;

	if (sp->seqhdr.next == (SEQ *)&sp->seqhdr)
		return (0);

	cnt = 0;
	tablen = O_VAL(sp, O_TABSTOP);
	for (qp = sp->seqhdr.next; qp != (SEQ *)&sp->seqhdr; qp = qp->next) {
		if (stype != qp->stype)
			continue;
		++cnt;
		for (p = qp->input, len = 0; (ch = *p); ++p, ++len)
			if (iscntrl(ch)) {
				(void)putc('^', sp->stdfp);
				(void)putc(ch + 0x40, sp->stdfp);
			} else
				(void)putc(ch, sp->stdfp);
		for (len = tablen - len % tablen; len; --len)
			(void)putc(' ', sp->stdfp);

		for (p = qp->output; (ch = *p); ++p)
			if (iscntrl(ch)) {
				(void)putc('^', sp->stdfp);
				(void)putc(ch + 0x40, sp->stdfp);
			} else
				(void)putc(ch, sp->stdfp);

		if (isname && qp->name) {
			for (len = tablen - len % tablen; len; --len)
				(void)putc(' ', sp->stdfp);
			for (p = qp->name, len = 0; (ch = *p); ++p, ++len)
				if (iscntrl(ch)) {
					(void)putc('^', sp->stdfp);
					(void)putc(ch + 0x40, sp->stdfp);
				} else
					(void)putc(ch, sp->stdfp);
		}
		(void)putc('\n', sp->stdfp);
	}
	return (cnt);
}

/*
 * seq_save --
 *	Save the sequence entries to a file.
 */
int
seq_save(sp, fp, prefix, stype)
	SCR *sp;
	FILE *fp;
	char *prefix;
	enum seqtype stype;
{
	register SEQ *qp;
	register int ch;
	register char *p;

	/* Write a sequence command for all keys the user defined. */
	for (qp = sp->seqhdr.next; qp != (SEQ *)&sp->seqhdr; qp = qp->next) {
		if (!(qp->flags & S_USERDEF))
			continue;
		if (prefix)
			(void)fprintf(fp, "%s", prefix);
		for (p = qp->input; ch = *p; ++p) {
			if (!isprint(ch) || ch == '|')
				(void)putc('\026', fp);
			(void)putc(ch, fp);
		}
		(void)putc(' ', fp);
		for (p = qp->output; ch = *p; ++p) {
			if (!isprint(ch) || ch == '|')
				(void)putc('\026', fp);		/* 026 == ^V */
			(void)putc(ch, fp);
		}
	}
	return (0);
}
