/*-
 * Copyright (c) 1993
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
static char sccsid[] = "@(#)screen.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"
#include "excmd.h"
#include "tag.h"

static int	 cut_copy __P((SCR *, SCR *));
static int	 opt_copy __P((SCR *, SCR *));
static int	 seq_copy __P((SCR *, SCR *));
static int	 tag_copy __P((SCR *, SCR *));

/*
 * scr_init --
 *	Do the default initialization of an SCR structure.
 */
int
scr_init(orig, sp)
	SCR *orig, *sp;
{
	extern CHNAME asciiname[];		/* XXX */
	sigset_t bmask, omask;
	int nore;

/* INITIALIZED AT SCREEN CREATE. */

	/*
	 * NULL the sp->ep field first, so the recovery timer won't access.
	 * Block SIGALRM and SIGHUP when manipulating the SCR chain.
	 */
	memset(sp, 0, sizeof(SCR));
	if (orig != NULL) {
		sigemptyset(&bmask);
		sigaddset(&bmask, SIGALRM);
		sigaddset(&bmask, SIGHUP);
		(void)sigprocmask(SIG_BLOCK, &bmask, &omask);
		HDR_APPEND(sp, orig, next, prev, SCR);
		(void)sigprocmask(SIG_SETMASK, &omask, NULL);
	}

	sp->olno = OOBLNO;
	sp->lno = 1;
	sp->cno = sp->ocno = 0;

#ifdef FWOPEN_NOT_AVAILABLE
	sp->trapped_fd = -1;
#endif
	FD_ZERO(&sp->rdfd);

	HDR_INIT(sp->bhdr, next, prev);
	HDR_INIT(sp->txthdr, next, prev);

	sp->lastcmd = &cmds[C_PRINT];

/* PARTIALLY OR COMPLETELY COPIED FROM PREVIOUS SCREEN. */
	if (orig != NULL) {
		sp->gp = orig->gp;
		
		/* User can replay the last input, but nothing else. */
		if (orig->rep_len != 0)
			if ((sp->rep = malloc(orig->rep_len)) == NULL)
				goto mem;
			else {
				memmove(sp->rep, orig->rep, orig->rep_len);
				sp->rep_len = orig->rep_len;
			}

		if (orig->VB != NULL && (sp->VB = strdup(orig->VB)) == NULL)
			goto mem;
		
		if (orig->lastbcomm != NULL &&
		    (sp->lastbcomm = strdup(orig->lastbcomm)) == NULL)
			goto mem;

		if (orig->altfname != NULL &&
		    (sp->altfname = strdup(orig->altfname)) == NULL)
			goto mem;

		sp->inc_lastch = orig->inc_lastch;
		sp->inc_lastval = orig->inc_lastval;

		if (orig->paragraph != NULL &&
		    (sp->paragraph = strdup(orig->paragraph)) == NULL)
			goto mem;
	
		if (cut_copy(orig, sp))
			goto mem;

		if (tag_copy(orig, sp))
			goto mem;
			
		/* Retain all searching/substitution information. */
		if (orig->searchdir == NOTSET)
			sp->searchdir = NOTSET;
		else {
			sp->sre = orig->sre;
			sp->searchdir = FORWARD;
		}
		sp->csearchdir = CNOTSET;
		sp->lastckey = orig->lastckey;

		nore = 0;
		if (orig->matchsize && (sp->match =
		    malloc(orig->matchsize * sizeof(regmatch_t))) == NULL)
			goto mem;
		else {
			if (sp->matchsize = orig->matchsize)
				memmove(sp->match, orig->match,
				    orig->matchsize * sizeof(regmatch_t));
		}
		if (sp->repl_len &&
		    (sp->repl = malloc(orig->repl_len)) == NULL)
			goto mem;
		else {
			if (sp->repl_len = orig->repl_len)
				memmove(sp->repl, orig->repl, orig->repl_len);
		}
		if (sp->newl_len && (sp->newl =
		    malloc(orig->newl_len * sizeof(size_t))) == NULL)
			goto mem;
		else {
			sp->newl_len = orig->newl_len;
			if (sp->newl_cnt = orig->newl_cnt)
				memmove(sp->newl, orig->newl,
				    orig->newl_len * sizeof(size_t));
		}
		if (!nore && F_ISSET(orig, S_RE_SET))
			F_SET(sp, S_RE_SET);

		sp->cname = orig->cname;
		memmove(sp->special, orig->special, sizeof(sp->special));

		if (seq_copy(orig, sp))
			goto mem;

		if (opt_copy(orig, sp)) {
mem:			msgq(orig, M_ERR,
			    "new screen attributes: %s", strerror(errno));
			scr_end(sp);
			return (1);
		}

		sp->flags = orig->flags & S_SCREEN_RETAIN;
		sp->flags |= S_REDRAW | S_REFORMAT;
	} else {
		if (isatty(STDIN_FILENO))
			F_SET(sp, S_ISFROMTTY);

		sp->inc_lastch = '+';
		sp->inc_lastval = 1;

		HDR_INIT(sp->taghdr, next, prev);

		sp->searchdir = NOTSET;
		sp->csearchdir = CNOTSET;

		sp->cname = asciiname;			/* XXX */

		sp->flags |= S_REDRAW | S_REFORMAT;
	}

	return (0);
}

/*
 * scr_end --
 *	Release a screen.
 */
int
scr_end(sp)
	SCR *sp;
{
	sigset_t bmask, omask;

	/* Free the memory map. */
	if (sp->h_smap != NULL)
		FREE(sp->h_smap, sp->w_rows * sizeof(SMAP));

	/* Free the argument list. */
	{ int cnt;
		for (cnt = 0; cnt < sp->argscnt; ++cnt)
			if (F_ISSET(&sp->args[cnt], A_ALLOCATED))
				FREE(sp->args[cnt].bp, sp->args[cnt].len);
		FREE(sp->args, sp->argscnt * sizeof(ARGS *));
		FREE(sp->argv, sp->argscnt * sizeof(char *));
	}

	/* Free line input buffer. */
	if (sp->ibp != NULL)
		FREE(sp->ibp, sp->ibp_len);

	/* Free text input, command chains. */
	hdr_text_free(&sp->txthdr);
	hdr_text_free(&sp->bhdr);

	/* Free vi text input memory. */
	if (sp->rep != NULL)
		FREE(sp->rep, sp->rep_len);

	/* Free visual bell termcap string. */
	if (sp->VB != NULL)
		FREE(sp->VB, strlen(sp->VB) + 1);

	/* Free last bang command. */
	if (sp->lastbcomm != NULL)
		FREE(sp->lastbcomm, strlen(sp->lastbcomm) + 1);

	if (sp->altfname != NULL)
		FREE(sp->altfname, strlen(sp->altfname) + 1);

	/* Free cut buffers. */
	{ CB *cb; int cnt;
		for (cb = sp->cuts, cnt = 0; cnt < UCHAR_MAX; ++cb, ++cnt)
			if (cb->txthdr.next != NULL)
				hdr_text_free(&cb->txthdr);
	}

	/* Free paragraph search list. */
	if (sp->paragraph != NULL)
		FREE(sp->paragraph, strlen(sp->paragraph) + 1);

	/* Free up tag information. */
	{ int cnt;
		if (F_ISSET(&sp->opts[O_TAGS], OPT_ALLOCATED) &&
		    sp->tfhead != NULL) {
			for (cnt = 0; sp->tfhead[cnt] != NULL; ++cnt)
				FREE(sp->tfhead[cnt]->fname,
				    strlen(sp->tfhead[cnt]->fname) + 1);
			free(sp->tfhead);
		}
		if (sp->tlast != NULL)
			FREE(sp->tlast, strlen(sp->tlast) + 1);
	}

	{ TAG *tp;
		while ((tp = sp->taghdr.next) != (TAG *)&sp->taghdr) {
			HDR_DELETE(tp, next, prev, TAG);
			FREE(tp, sizeof(TAG));
		}
	}

	/* Free up search information. */
	if (sp->match != NULL)
		FREE(sp->match, sizeof(regmatch_t));
	if (sp->repl != NULL)
		FREE(sp->repl, sp->repl_len);
	if (sp->newl != NULL)
		FREE(sp->newl, sp->newl_len);

	/* Free up linked lists of sequences. */
	{ SEQ *qp, *next;
		for (qp = sp->seqhdr.next;
		    qp != (SEQ *)&sp->seqhdr; qp = next) {
			next = qp->next;
			if (qp->name != NULL)
				FREE(qp->name, strlen(qp->name) + 1);
			FREE(qp->output, strlen(qp->output) + 1);
			FREE(qp->input, strlen(qp->input) + 1);
			FREE(qp, sizeof(SEQ));
		}
	}

	/* Free up executed buffer. */
	if (sp->atkey_buf)
		FREE(sp->atkey_buf, sp->atkey_len);

	/*
	 * Free the message chain last, so previous failures have a place
	 * to put messages.  Copy messages to (in order) a related screen,
	 * any screen, the global area. 
	 */
	{ SCR *c_sp; MSG *c_mp, *mp, *next;
		if (sp->parent != NULL) {
			c_sp = sp->parent;
			c_mp = c_sp->msgp;
			if (F_ISSET(sp, S_BELLSCHED))
				F_SET(c_sp, S_BELLSCHED);
		} else if (sp->child != NULL) {
			c_sp = sp->child;
			c_mp = c_sp->msgp;
			if (F_ISSET(sp, S_BELLSCHED))
				F_SET(c_sp, S_BELLSCHED);
		} else if (sp->next != (SCR *)&sp->gp->scrhdr) {
			c_sp = sp->next;
			c_mp = c_sp->msgp;
			if (F_ISSET(sp, S_BELLSCHED))
				F_SET(c_sp, S_BELLSCHED);
		} else {
			c_sp = NULL;
			c_mp = sp->gp->msgp;
			if (F_ISSET(sp, S_BELLSCHED))
				F_SET(sp->gp, S_BELLSCHED);
		}

		for (mp = sp->msgp;
		    mp != NULL && !F_ISSET(mp, M_EMPTY); mp = mp->next)
			msg_app(sp->gp, c_sp,
			    mp->flags & M_INV_VIDEO, mp->mbuf, mp->len);

		for (mp = sp->msgp; mp != NULL; mp = next) {
			next = mp->next;
			if (mp->mbuf != NULL)
				FREE(mp->mbuf, mp->blen);
			FREE(mp, sizeof(MSG));
		}
	}

	/*
	 * Remove the screen from the global chain of screens.
	 * Block SIGALRM and SIGHUP when manipulating the SCR chain.
	 */
	sigemptyset(&bmask);
	sigaddset(&bmask, SIGALRM);
	sigaddset(&bmask, SIGHUP);
	(void)sigprocmask(SIG_BLOCK, &bmask, &omask);
	HDR_DELETE(sp, next, prev, SCR);
	(void)sigprocmask(SIG_SETMASK, &omask, NULL);

	/* Remove the screen from the chain of related screens. */
	if (sp->parent != NULL) {
		sp->parent->child = sp->child;
		if (sp->child != NULL)
			sp->child->parent = sp->parent;
	} else if (sp->child != NULL)
		sp->child->parent = NULL;

	/* Free the screen itself. */
	FREE(sp, sizeof(SCR));

	return (0);
}

/*
 * cut_copy --
 *	Copy a screen's cut buffers.
 */
static int
cut_copy(a, b)
	SCR *a, *b;
{
	CB *acb, *bcb;
	TEXT *atp, *tp;
	int cnt;

	for (acb = a->cuts, bcb = b->cuts, cnt = 0;
	    cnt < UCHAR_MAX; ++acb, ++bcb, ++cnt) {
		if (acb->txthdr.next == NULL ||
		    acb->txthdr.next == &acb->txthdr)
			continue;
		HDR_INIT(bcb->txthdr, next, prev);
		for (atp = acb->txthdr.next;
		    atp != (TEXT *)&acb->txthdr; atp = atp->next) {
			if ((tp = malloc(sizeof(TEXT))) == NULL)
				return (1);
			if ((tp->lb = malloc(atp->len)) == NULL) {
				FREE(tp, sizeof(TEXT));
				return (1);
			}
			tp->lb_len = tp->len = atp->len;
			tp->wd = NULL;
			memmove(tp->lb, atp->lb, atp->len);
			HDR_INSERT(tp, &bcb->txthdr, next, prev, TEXT);
		}
		bcb->len = acb->len;
		bcb->flags = acb->flags;
	}
	return (0);
}

/*
 * opt_copy --
 *	Copy a screen's OPTION array.
 */
static int
opt_copy(a, b)
	SCR *a, *b;
{
	OPTION *op;
	int cnt;

	/* Copy most everything without change. */
	memmove(b->opts, a->opts, sizeof(a->opts));

	/*
	 * Allocate copies of the strings -- keep trying to reallocate
	 * after ENOMEM failure, otherwise end up with more than one
	 * screen referencing the original memory.
	 */
	for (op = b->opts, cnt = 0; cnt < O_OPTIONCOUNT; ++cnt, ++op)
		if (F_ISSET(&b->opts[cnt], OPT_ALLOCATED) &&
		    (O_STR(b, cnt) = strdup(O_STR(b, cnt))) == NULL) {
			msgq(a, M_ERR,
			    "Error: option copy: %s", strerror(errno));
			return (1);
		}
	return (0);
}

/*
 * seq_copy --
 *	Copy a screen's SEQ structures.
 */
static int
seq_copy(a, b)
	SCR *a, *b;
{
	SEQ *ap;

	/* Initialize linked list. */
	HDR_INIT(b->seqhdr, next, prev);

	for (ap = a->seqhdr.next; ap != (SEQ *)&a->seqhdr; ap = ap->next)
		if (seq_set(b,
		    ap->name, ap->input, ap->output, ap->stype,
		    F_ISSET(ap, S_USERDEF)))
			return (1);
	return (0);
}

/*
 * tag_copy --
 *	Copy a screen's tag structures.
 */
static int
tag_copy(a, b)
	SCR *a, *b;
{
	TAG *ap, *tp;
	TAGF **atfp, **btfp;
	int cnt;

	/* Initialize linked list. */
	HDR_INIT(b->taghdr, next, prev);

	for (ap = a->taghdr.next; ap != (TAG *)&a->taghdr; ap = ap->next) {
		if ((tp = malloc(sizeof(TAG))) == NULL)
			goto nomem;
		*tp = *ap;
		HDR_INSERT(tp, &b->taghdr, next, prev, TAG);
	}

	/* Copy the list of tag files. */
	for (atfp = a->tfhead, cnt = 1; *atfp != NULL; ++atfp, ++cnt);

	if (cnt > 1) {
		if ((b->tfhead = malloc(cnt * sizeof(TAGF **))) == NULL)
			goto nomem;
		for (atfp = a->tfhead,
		    btfp = b->tfhead; *atfp != NULL; ++atfp, ++btfp) {
			if ((*btfp = malloc(sizeof(TAGF))) == NULL)
				goto nomem;
			if (((*btfp)->fname = strdup((*atfp)->fname)) == NULL) {
				FREE(*btfp, sizeof(TAGF));
				*btfp = NULL;
				goto nomem;
			}
			(*btfp)->flags = 0;
		}
		*btfp = NULL;
	}
		
	/* Copy the last tag. */
	if (a->tlast != NULL && (b->tlast = strdup(a->tlast)) == NULL)
		goto nomem;
	return (0);

nomem:	msgq(a, M_ERR, "Error: tag copy: %s", strerror(errno));
	return (1);
}
