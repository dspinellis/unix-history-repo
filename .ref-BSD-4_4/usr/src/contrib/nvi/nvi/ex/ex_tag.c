/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * David Hitz of Auspex Systems, Inc.
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
static char sccsid[] = "@(#)ex_tag.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"
#include "excmd.h"
#include "tag.h"

static char	*binary_search __P((char *, char *, char *));
static int	 compare __P((char *, char *, char *));
static char	*linear_search __P((char *, char *, char *));
static int	 search __P((char *, char *, char **));
static int	 tag_change __P((SCR *, EXF *, char *, char *, char *, int));
static int	 tag_get __P((SCR *, char *, char **, char **, char **));

/*
 * ex_tagfirst --
 *	The tag code can be entered from main, i.e. "vi -t tag".
 */
int
ex_tagfirst(sp, tagarg)
	SCR *sp;
	char *tagarg;
{
	EXF *tep;
	MARK m;
	long tl;
	int sval;
	char *p, *tag, *fname, *search, *argv[2];

	/* Taglength may limit the number of characters. */
	if ((tl = O_VAL(sp, O_TAGLENGTH)) != 0 && strlen(tagarg) > tl)
		tagarg[tl] = '\0';

	/* Get the tag information. */
	if (tag_get(sp, tagarg, &tag, &fname, &search))
		return (1);

	/* Create the file entry. */
	argv[0] = fname;
	argv[1] = NULL;
	if (file_set(sp, 1, argv) ||
	    (tep = file_first(sp, 0)) == NULL ||
	    (tep = file_start(sp, tep, NULL)) == NULL)
		return (1);

	/*
	 * Search for the tag; cheap fallback for C functions
	 * if the name is the same but the arguments have changed.
	 */
	m.lno = 1;
	m.cno = 0;
	sval = f_search(sp, tep, &m, &m, search,
	    NULL, SEARCH_FILE | SEARCH_TAG | SEARCH_TERM);
	if (sval && (p = strrchr(search, '(')) != NULL) {
		p[1] = '\0';
		sval = f_search(sp, tep, &m, &m, search,
		    NULL, SEARCH_FILE | SEARCH_TAG | SEARCH_TERM);
	}
	if (sval) {
		msgq(sp, M_ERR, "%s: search pattern not found.", tag);
		return (1);
	}

	/* Set up the screen/file. */
	sp->lno = m.lno;
	sp->cno = m.cno;
	sp->ep = tep;
	F_CLR(tep, F_NOSETPOS);

	/* Might as well make this a default tag. */
	if ((sp->tlast = strdup(tagarg)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}
	return (0);
}

/*
 * ex_tagpush -- :tag [file]
 *	Move to a new tag.
 */
int
ex_tagpush(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	TAG *tp;
	long tl;
	char *fname, *search, *tag;
	
	switch (cmdp->argc) {
	case 1:
		if (sp->tlast != NULL)
			free(sp->tlast);
		if ((sp->tlast = strdup((char *)cmdp->argv[0])) == NULL) {
			msgq(sp, M_ERR, "Error: %s", strerror(errno));
			return (1);
		}
		break;
	case 0:
		if (sp->tlast == NULL) {
			msgq(sp, M_ERR, "No previous tag entered.");
			return (1);
		}
		break;
	default:
		abort();
	}

	/* Taglength may limit the number of characters. */
	if ((tl = O_VAL(sp, O_TAGLENGTH)) != 0 && strlen(sp->tlast) > tl)
		sp->tlast[tl] = '\0';

	/* Get the tag information. */
	if (tag_get(sp, sp->tlast, &tag, &fname, &search))
		return (1);

	/* Save enough information that we can get back. */
	if ((tp = malloc(sizeof(TAG))) == NULL) {
		msgq(sp, M_ERR, "Error: %s.", strerror(errno));
		return (1);
	}
	tp->ep = ep;
	tp->lno = sp->lno;
	tp->cno = sp->cno;

	/* Try to change. */
	if (tag_change(sp, ep, tag, fname, search, F_ISSET(cmdp, E_FORCE))) {
		free(tag);
		FREE(tp, sizeof(TAG));
		return (1);
	}

	/* Push saved information. */
	HDR_APPEND(tp, &sp->taghdr, next, prev, TAG);
	return (0);
}

/*
 * ex_tagpop -- :tagp[op][!]
 *	Pop the tag stack.
 */
int
ex_tagpop(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	TAG *tp;

	/* Pop newest saved information. */
	tp = sp->taghdr.next;
	if (tp == (TAG *)&sp->taghdr) {
		msgq(sp, M_INFO, "The tags stack is empty.");
		return (1);
	}

	/* If not switching files, it's easy; else do the work. */
	if (tp->ep == ep) {
		sp->lno = tp->lno;
		sp->cno = tp->cno;
	} else {
		MODIFY_CHECK(sp, ep, F_ISSET(cmdp, E_FORCE));
		sp->enext = tp->ep;
		set_altfname(sp, ep->name);
		F_SET(sp, S_FSWITCH);
	}

	/* Delete the saved information from the stack. */
	HDR_DELETE(tp, next, prev, TAG);
	return (0);
}

/*
 * ex_tagtop -- :tagt[op][!]
 *	Clear the tag stack.
 */	
int
ex_tagtop(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	TAG *tp;

	/* Pop oldest saved information. */
	tp = sp->taghdr.prev;
	if (tp == (TAG *)&sp->taghdr) {
		msgq(sp, M_INFO, "The tags stack is empty.");
		return (1);
	}

	/* If not switching files, it's easy; else do the work. */
	if (tp->ep == ep) {
		sp->lno = tp->lno;
		sp->cno = tp->cno;
	} else {
		MODIFY_CHECK(sp, ep, F_ISSET(cmdp, E_FORCE));
		sp->enext = tp->ep;
		sp->lno = tp->lno;
		sp->cno = tp->cno;
		set_altfname(sp, ep->name);
		F_SET(sp, S_FSWITCH);
	}

	/* Delete the stack. */
	while ((tp = sp->taghdr.next) != (TAG *)&sp->taghdr) {
		HDR_DELETE(tp, next, prev, TAG);
		FREE(tp, sizeof(TAG));
	}
	return (0);
}

static int
tag_change(sp, ep, tag, fname, search, force)
	SCR *sp;
	EXF *ep;
	char *tag, *fname, *search;
	int force;
{
	enum {TC_CHANGE, TC_CURRENT} which;
	EXF *tep;
	MARK m;
	int sval;
	char *p;

	if ((tep = file_get(sp, ep, fname, 1)) == NULL)
		return (1);
	if (ep == tep)
		which = TC_CURRENT;
	else {
		MODIFY_CHECK(sp, ep, force);
		if ((tep = file_start(sp, tep, NULL)) == NULL)
			return (1);
		which = TC_CHANGE;
	}

	/*
	 * Search for the tag; cheap fallback for C functions
	 * if the name is the same but the arguments have changed.
	 */
	m.lno = 1;
	m.cno = 0;
	sval = f_search(sp, tep == NULL ? ep : tep, &m, &m, search,
	    NULL, SEARCH_FILE | SEARCH_TAG | SEARCH_TERM);
	if (sval && (p = strrchr(search, '(')) != NULL) {
		p[1] = '\0';
		sval = f_search(sp, tep == NULL ? ep : tep, &m, &m, search,
		    NULL, SEARCH_FILE | SEARCH_TAG | SEARCH_TERM);
	}
	if (sval) {
		msgq(sp, M_ERR, "%s: search pattern not found.", tag);
		switch (which) {
		case TC_CHANGE:
			sp->enext = tep;
			set_altfname(sp, ep->name);
			F_SET(sp, S_FSWITCH);
			return (0);
		case TC_CURRENT:
			return (1);
		}
	} else switch (which) {
		case TC_CHANGE:
			sp->lno = m.lno;
			sp->cno = m.cno;
			F_CLR(tep, F_NOSETPOS);
			sp->enext = tep;
			set_altfname(sp, ep->name);
			F_SET(sp, S_FSWITCH);
			break;
		case TC_CURRENT:
			sp->lno = m.lno;
			sp->cno = m.cno;
			break;
		}
	return (0);
}

/*
 * tag_get --
 *	Get a tag from the tags files.
 */
static int
tag_get(sp, tag, tagp, filep, searchp)
	SCR *sp;
	char *tag, **tagp, **filep, **searchp;
{
	char *p;
	TAGF **tfp;

	/* Find the tag, only display missing file messages once. */
	p = NULL;
	for (tfp = sp->tfhead; *tfp != NULL && p == NULL; ++tfp)
		if (search((*tfp)->fname, tag, &p) &&
		    (errno != ENOENT || !F_ISSET((*tfp), TAGF_ERROR))) {
			msgq(sp, M_ERR,
			    "%s: %s", (*tfp)->fname, strerror(errno));
			F_SET((*tfp),TAGF_ERROR);
		}
	
	if (p == NULL) {
		msgq(sp, M_ERR, "%s: tag not found.", tag);
		return (1);
	}

	/*
	 * Set the return pointers; tagp points to the tag, and, incidentally
	 * the allocated string, filep points to the nul-terminated file name,
	 * searchp points to the nul-terminated search string.
	 */
	for (*tagp = p; *p && !isspace(*p); ++p);
	if (*p == '\0')
		goto malformed;
	for (*p++ = '\0'; isspace(*p); ++p);
	for (*filep = p; *p && !isspace(*p); ++p);
	if (*p == '\0')
		goto malformed;
	for (*p++ = '\0'; isspace(*p); ++p);
	*searchp = p;
	if (*p == '\0') {
malformed:	free(*tagp);
		msgq(sp, M_ERR, "%s: corrupted tag in %s.", tag, (*tfp)->fname);
		return (1);
	}
	return (0);
}

#define	EQUAL		0
#define	GREATER		1
#define	LESS		(-1)

/*
 * search --
 *	Search a file for a tag.
 */
static int
search(fname, tname, tag)
	char *fname, *tname, **tag;
{
	struct stat sb;
	int fd, len;
	char *endp, *back, *front, *p;

	if ((fd = open(fname, O_RDONLY, 0)) < 0)
		return (1);
	if (fstat(fd, &sb) || (front = mmap(NULL,
	    sb.st_size, PROT_READ, 0, fd, (off_t)0)) == (caddr_t)-1) {
		(void)close(fd);
		return (1);
	}
	back = front + sb.st_size;

	front = binary_search(tname, front, back);
	front = linear_search(tname, front, back);

	if (front == NULL)
		goto done;
	if ((endp = strchr(front, '\n')) == NULL)
		goto done;

	len = endp - front;
	if ((p = malloc(len + 1)) == NULL) {
done:		(void)close(fd);
		*tag = NULL;
		return (0);
	}

	memmove(p, front, len);
	p[len] = '\0';
	(void)close(fd);
	*tag = p;
	return (0);
}

/*
 * Binary search for "string" in memory between "front" and "back".
 * 
 * This routine is expected to return a pointer to the start of a line at
 * *or before* the first word matching "string".  Relaxing the constraint
 * this way simplifies the algorithm.
 * 
 * Invariants:
 * 	front points to the beginning of a line at or before the first 
 *	matching string.
 * 
 * 	back points to the beginning of a line at or after the first 
 *	matching line.
 * 
 * Base of the Invariants.
 * 	front = NULL; 
 *	back = EOF;
 * 
 * Advancing the Invariants:
 * 
 * 	p = first newline after halfway point from front to back.
 * 
 * 	If the string at "p" is not greater than the string to match, 
 *	p is the new front.  Otherwise it is the new back.
 * 
 * Termination:
 * 
 * 	The definition of the routine allows it return at any point, 
 *	since front is always at or before the line to print.
 * 
 * 	In fact, it returns when the chosen "p" equals "back".  This 
 *	implies that there exists a string is least half as long as 
 *	(back - front), which in turn implies that a linear search will 
 *	be no more expensive than the cost of simply printing a string or two.
 * 
 * 	Trying to continue with binary search at this point would be 
 *	more trouble than it's worth.
 */
#define	SKIP_PAST_NEWLINE(p, back)	while (p < back && *p++ != '\n');

static char *
binary_search(string, front, back)
	register char *string, *front, *back;
{
	register char *p;

	p = front + (back - front) / 2;
	SKIP_PAST_NEWLINE(p, back);

	while (p != back) {
		if (compare(string, p, back) == GREATER)
			front = p;
		else
			back = p;
		p = front + (back - front) / 2;
		SKIP_PAST_NEWLINE(p, back);
	}
	return (front);
}

/*
 * Find the first line that starts with string, linearly searching from front
 * to back.
 * 
 * Return NULL for no such line.
 * 
 * This routine assumes:
 * 
 * 	o front points at the first character in a line. 
 *	o front is before or at the first line to be printed.
 */
static char *
linear_search(string, front, back)
	char *string, *front, *back;
{
	while (front < back) {
		switch (compare(string, front, back)) {
		case EQUAL:		/* Found it. */
			return (front);
			break;
		case LESS:		/* No such string. */
			return (NULL);
			break;
		case GREATER:		/* Keep going. */
			break;
		}
		SKIP_PAST_NEWLINE(front, back);
	}
	return (NULL);
}

/*
 * Return LESS, GREATER, or EQUAL depending on how the string1 compares with
 * string2 (s1 ??? s2).
 * 
 * 	o Matches up to len(s1) are EQUAL. 
 *	o Matches up to len(s2) are GREATER.
 * 
 * The string "s1" is null terminated.  The string s2 is '\n' terminated (or
 * "back" terminated).
 */
static int
compare(s1, s2, back)
	register char *s1, *s2, *back;
{
	for (; *s1 && s2 < back && *s2 != '\n'; ++s1, ++s2)
		if (*s1 != *s2)
			return (*s1 < *s2 ? LESS : GREATER);
	return (*s1 ? GREATER : EQUAL);
}
