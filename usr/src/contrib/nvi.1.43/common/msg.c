/*-
 * Copyright (c) 1991, 1993, 1994
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
static char sccsid[] = "@(#)msg.c	9.9 (Berkeley) 11/24/94";
#endif /* not lint */

#include <sys/param.h>
#include <sys/queue.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <bitstring.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include "compat.h"
#include <db.h>
#include <regex.h>

#include "vi.h"

static const char *msg_cat __P((GS *, const char *, size_t *));

/*
 * msgq --
 *	Display a message.
 */
void
#ifdef __STDC__
msgq(SCR *sp, enum msgtype mt, const char *fmt, ...)
#else
msgq(sp, mt, fmt, va_alist)
	SCR *sp;
	enum msgtype mt;
        char *fmt;
        va_dcl
#endif
{
#ifndef NL_ARGMAX
#define	__NL_ARGMAX	20		/* Set to 9 by System V. */
	struct {
		const char *str;	/* String pointer. */
		size_t	 arg;		/* Argument number. */
		size_t	 prefix;	/* Prefix string length. */
		size_t	 skip;		/* Skipped string length. */
		size_t	 suffix;	/* Suffix string length. */
	} str[__NL_ARGMAX];
#endif
	CHAR_T ch;
	GS *gp;
	size_t blen, cnt1, cnt2, len, mlen, nlen, soff;
	const char *p, *t, *u;
	char *bp, *mp, *rbp, *s_rbp;
        va_list ap;

#ifdef __STDC__
        va_start(ap, fmt);
#else
        va_start(ap);
#endif
	/*
	 * It's possible to enter msg when there's no screen to hold
	 * the message.  If sp is NULL, ignore the special cases and
	 * just build the message, using __global_list.
	 */
	if (sp == NULL)
		gp = __global_list;
	else {
		gp = sp->gp;
		switch (mt) {
		case M_BERR:
			if (!IN_EX_MODE(sp) && !F_ISSET(sp, S_EXSILENT) &&
			    F_ISSET(gp, G_STDIN_TTY) &&
			    !O_ISSET(sp, O_VERBOSE)) {
				F_SET(sp, S_BELLSCHED);
				return;
			}
			mt = M_ERR;
			break;
		case M_VINFO:
			if (!O_ISSET(sp, O_VERBOSE))
				return;
			mt = M_INFO;
			/* FALLTHROUGH */
		case M_INFO:
			if (F_ISSET(sp, S_EXSILENT))
				return;
			break;
		case M_ERR:
		case M_SYSERR:
			break;
		default:
			abort();
		}
	}

	/* Get space for the message. */
	nlen = 1024;
	if (0) {
retry:		FREE_SPACE(sp, bp, blen);
		nlen *= 2;
	}
	bp = NULL;
	blen = 0;
	GET_SPACE_GOTO(sp, bp, blen, nlen);
	if (0) {
binc_err:	return;
	}

	/*
	 * Error prefix.
	 *
	 * mp:	 pointer to the current next character to be written
	 * mlen: length of the already written characters
	 * blen: total length of the buffer
	 */
#define	REM	(blen - mlen)
	mp = bp;
	mlen = 0;
	if (mt == M_SYSERR) {
		p = msg_cat(gp, "094|Error: ", &len);
		if (REM < len)
			goto retry;
		memmove(mp, p, len);
		mp += len;
		mlen += len;
	}

	/* File name, line number prefix for errors. */
	if ((mt == M_ERR || mt == M_SYSERR) &&
	    sp != NULL && sp->if_name != NULL) {
		for (p = sp->if_name; *p != '\0'; ++p) {
			len = snprintf(mp, REM, "%s", KEY_NAME(sp, *p));
			mp += len;
			if ((mlen += len) > blen)
				goto retry;
		}
		len = snprintf(mp, REM, ", %d: ", sp->if_lno);
		mp += len;
		if ((mlen += len) > blen)
			goto retry;
	}

	if (fmt == NULL)
		goto nofmt;
	fmt = msg_cat(gp, fmt, NULL);

#ifndef NL_ARGMAX
	/*
	 * Nvi should run on machines that don't support the numbered argument
	 * specifications (%[digit]*$).  We do this by reformatting the string
	 * so that we can hand it to vsprintf(3) and it will use the arguments
	 * in the right order.  When vsprintf returns, we put the string back
	 * into the right order.  It's undefined, according to SVID III, to mix
	 * numbered argument specifications with the standard style arguments,
	 * so this should be safe.
	 *
	 * In addition, we also need a character that is known to not occur in
	 * any vi message, for separating the parts of the string.  As callers
	 * of msgq are responsible for making sure that all the non-printable
	 * characters are formatted for printing before calling msgq, we use a
	 * random non-printable character selected at terminal initialization
	 * time.  This code isn't fast by any means, but since as messages are
	 * relatively short and normally have only a few arguments, it won't be
	 * too bad.  Regardless, nobody has come up with any other solution.
	 *
	 * The result of this loop is an array of pointers into the message
	 * string, with associated lengths and argument numbers.  The array
	 * is in the "correct" order, and the arg field contains the argument
	 * order.
	 */
	for (p = fmt, soff = 0; soff < __NL_ARGMAX;) {
		for (t = p; *p != '\0' && *p != '%'; ++p);
		if (*p == '\0')
			break;
		++p;
		if (!isdigit(*p)) {
			if (*p == '%')
				++p;
			continue;
		}
		for (u = p; *++p != '\0' && isdigit(*p););
		if (*p != '$')
			continue;

		/* Up to, and including the % character. */
		str[soff].str = t;
		str[soff].prefix = u - t;

		/* Up to, and including the $ character. */
		str[soff].arg = atoi(u);
		str[soff].skip = (p - u) + 1;
		if (str[soff].arg >= __NL_ARGMAX)
			goto err;

		/* Up to, and including the conversion character. */
		for (u = p; (ch = *++p) != '\0';)
			if (isalpha(ch) &&
			    strchr("diouxXfeEgGcspn", ch) != NULL)
				break;
		str[soff].suffix = p - u;
		if (ch != '\0')
			++p;
		++soff;
	}

	/* If no magic strings, we're done. */
	if (soff == 0)
		goto format;

	 /* Get space for the reordered strings. */
	if ((rbp = malloc(nlen)) == NULL)
		goto err;
	s_rbp = rbp;

	/*
	 * Reorder the strings into the message string based on argument
	 * order.
	 *
	 * !!!
	 * We ignore arguments that are out of order, i.e. if we don't find
	 * an argument, we continue.  Assume (almost certainly incorrectly)
	 * that whoever created the string knew what they were doing.
	 *
	 * !!!
	 * Brute force "sort", but since we don't expect more than one or two
	 * arguments in a string, the setup cost of a fast sort will be more
	 * expensive than the loop.
	 */
	for (cnt1 = 1; cnt1 <= soff; ++cnt1)
		for (cnt2 = 0; cnt2 < soff; ++cnt2)
			if (cnt1 == str[cnt2].arg) {
				memmove(s_rbp, str[cnt2].str, str[cnt2].prefix);
				memmove(s_rbp + str[cnt2].prefix,
				    str[cnt2].str + str[cnt2].prefix +
				    str[cnt2].skip, str[cnt2].suffix);
				s_rbp += str[cnt2].prefix + str[cnt2].suffix;
				*s_rbp++ = gp->noprint;
				break;
			}
	*s_rbp = '\0';
	fmt = rbp;
#endif

	/* Format the arguments into the string. */
format:	len = vsnprintf(mp, REM, fmt, ap);
	if (len >= nlen)
		goto retry;

#ifndef NL_ARGMAX
	if (soff == 0)
		goto nofmt;

	/*
	 * Go through the resulting string, and, for each separator character
	 * separated string, enter its new starting position and length in the
	 * array.
	 */
	for (p = t = mp, cnt1 = 1, ch = gp->noprint; *p != '\0'; ++p)
		if (*p == ch) {
			for (cnt2 = 0; cnt2 < soff; ++cnt2)
				if (str[cnt2].arg == cnt1)
					break;
			str[cnt2].str = t;
			str[cnt2].prefix = p - t;
			t = p + 1;
			++cnt1;
		}

	/*
	 * Reorder the strings once again, putting them back into the
	 * message buffer.
	 *
	 * !!!
	 * Note, the length of the message gets decremented once for
	 * each substring, when we discard the separator character.
	 */
	for (s_rbp = rbp, cnt1 = 0; cnt1 < soff; ++cnt1) {
		memmove(rbp, str[cnt1].str, str[cnt1].prefix);
		rbp += str[cnt1].prefix;
		--len;
	}
	memmove(mp, s_rbp, rbp - s_rbp);

	/* Free the reordered string memory. */
	free(s_rbp);
#endif

nofmt:	mp += len;
	if ((mlen += len) > blen)
		goto retry;
	if (mt == M_SYSERR) {
		len = snprintf(mp, REM, ": %s", strerror(errno));
		mp += len;
		if ((mlen += len) > blen)
			goto retry;
	}

#ifdef DEBUG
	if (sp != NULL)
		TRACE(sp, "mesg: {%.*s}\n", mlen, bp);
#endif
	msg_app(__global_list, sp,
	    mt == M_ERR || mt == M_SYSERR ? 1 : 0, bp, mlen);

err:	FREE_SPACE(sp, bp, blen);
}

/*
 * msg_app --
 *	Append a message into the queue.  This can fail, but there's
 *	nothing we can do if it does.
 */
void
msg_app(gp, sp, inv_video, p, len)
	GS *gp;
	SCR *sp;
	int inv_video;
	char *p;
	size_t len;
{
	static int reenter;		/* STATIC: Re-entrancy check. */
	MSG *mp, *nmp;

	/*
	 * It's possible to reenter msg when it allocates space.
	 * We're probably dead anyway, but no reason to drop core.
	 */
	if (reenter)
		return;
	reenter = 1;

	/*
	 * We can be entered as the result of a signal arriving, trying
	 * to sync the file and failing.  This shouldn't be a hot spot,
	 * block the signals.
	 */
	SIGBLOCK(gp);

	/*
	 * Find an empty structure, or allocate a new one.  Use the
	 * screen structure if it exists, otherwise the global one.
	 */
	if (sp != NULL) {
		if ((mp = sp->msgq.lh_first) == NULL) {
			CALLOC(sp, mp, MSG *, 1, sizeof(MSG));
			if (mp == NULL)
				goto ret;
			LIST_INSERT_HEAD(&sp->msgq, mp, q);
			goto store;
		}
	} else if ((mp = gp->msgq.lh_first) == NULL) {
		CALLOC(sp, mp, MSG *, 1, sizeof(MSG));
		if (mp == NULL)
			goto ret;
		LIST_INSERT_HEAD(&gp->msgq, mp, q);
		goto store;
	}
	while (!F_ISSET(mp, M_EMPTY) && mp->q.le_next != NULL)
		mp = mp->q.le_next;
	if (!F_ISSET(mp, M_EMPTY)) {
		CALLOC(sp, nmp, MSG *, 1, sizeof(MSG));
		if (nmp == NULL)
			goto ret;
		LIST_INSERT_AFTER(mp, nmp, q);
		mp = nmp;
	}

	/* Get enough memory for the message. */
store:	if (len > mp->blen &&
	    (mp->mbuf = binc(sp, mp->mbuf, &mp->blen, len)) == NULL)
		goto ret;

	/* Store the message. */
	memmove(mp->mbuf, p, len);
	mp->len = len;
	mp->flags = inv_video ? M_INV_VIDEO : 0;

ret:	reenter = 0;
	SIGUNBLOCK(gp);
}

/*
 * msg_rpt --
 *	Report on the lines that changed.
 *
 * !!!
 * Historic vi documentation (USD:15-8) claimed that "The editor will also
 * always tell you when a change you make affects text which you cannot see."
 * This isn't true -- edit a large file and do "100d|1".  We don't implement
 * this semantic as it would require that we track each line that changes
 * during a command instead of just keeping count.
 *
 * Line counts weren't right in historic vi, either.  For example, given the
 * file:
 *	abc
 *	def
 * the command 2d}, from the 'b' would report that two lines were deleted,
 * not one.
 */
int
msg_rpt(sp, is_message)
	SCR *sp;
	int is_message;
{
	static char * const action[] = {
		"added",
		"changed",
		"deleted",
		"joined",
		"moved",
		"shifted",
		"yanked",
		NULL,
	};
	recno_t total;
	u_long rptval;
	int first, cnt;
	size_t blen, len;
	char * const *ap;
	char *bp, *p, number[40];

	/* Change reports are turned off in batch mode. */
	if (F_ISSET(sp, S_EXSILENT))
		return (0);

	GET_SPACE_RET(sp, bp, blen, 512);
	p = bp;

	for (ap = action,
	    cnt = 0, first = 1, total = 0; *ap != NULL; ++ap, ++cnt)
		if (sp->rptlines[cnt] != 0) {
			total += sp->rptlines[cnt];
			len = snprintf(number, sizeof(number),
			    "%s%lu lines %s",
			    first ? "" : "; ", sp->rptlines[cnt], *ap);
			memmove(p, number, len);
			p += len;
			first = 0;
		}

	/*
	 * If nothing to report, return.
	 *
	 * !!!
	 * And now, a vi clone test.  Historically, vi reported if the number
	 * of changed lines was > than the value, not >=, unless it was a yank
	 * command, which used >=.  No lie.  I got complaints, so we conform
	 * to historic practice.  In addition, setting report to 0 in the 4BSD
	 * historic vi was equivalent to setting it to 1, for an unknown reason
	 * (this bug was apparently fixed in System V at some point).
	 */
	rptval = O_VAL(sp, O_REPORT);
	if (total > rptval || sp->rptlines[L_YANKED] >= rptval) {
		*p = '\0';
		if (is_message)
			msgq(sp, M_INFO, "%s", bp);
		else {
			F_SET(sp, S_SCR_EXWROTE);
			(void)ex_printf(EXCOOKIE, "%s\n", bp);
		}
	}

	FREE_SPACE(sp, bp, blen);

	/* Clear after each report. */
	sp->rptlchange = OOBLNO;
	memset(sp->rptlines, 0, sizeof(sp->rptlines));
	return (0);
}

/*
 * msg_status --
 *	Report on the file's status.
 */
int
msg_status(sp, lno, showlast)
	SCR *sp;
	recno_t lno;
	int showlast;
{
	recno_t last;
	char *mo, *nc, *nf, *pid, *ro, *ul;
#ifdef DEBUG
	char pbuf[50];

	(void)snprintf(pbuf, sizeof(pbuf), " (pid %u)", getpid());
	pid = pbuf;
#else
	pid = "";
#endif
	/*
	 * See nvi/exf.c:file_init() for a description of how and
	 * when the read-only bit is set.
	 *
	 * !!!
	 * The historic display for "name changed" was "[Not edited]".
	 */
	if (F_ISSET(sp->frp, FR_NEWFILE)) {
		F_CLR(sp->frp, FR_NEWFILE);
		nf = "new file";
		mo = nc = "";
	} else {
		nf = "";
		if (F_ISSET(sp->frp, FR_NAMECHANGE)) {
			nc = "name changed";
			mo = F_ISSET(sp->ep, F_MODIFIED) ?
			    ", modified" : ", unmodified";
		} else {
			nc = "";
			mo = F_ISSET(sp->ep, F_MODIFIED) ?
			    "modified" : "unmodified";
		}
	}
	ro = F_ISSET(sp->frp, FR_RDONLY) ? ", readonly" : "";
	ul = F_ISSET(sp->frp, FR_UNLOCKED) ? ", UNLOCKED" : "";
	if (showlast) {
		if (file_lline(sp, &last))
			return (1);
		if (last >= 1)
			msgq(sp, M_INFO,
			    "%s: %s%s%s%s%s: line %lu of %lu [%ld%%]%s",
			    sp->frp->name, nf, nc, mo, ul, ro, lno,
			    last, (lno * 100) / last, pid);
		else
			msgq(sp, M_INFO, "%s: %s%s%s%s%s: empty file%s",
			    sp->frp->name, nf, nc, mo, ul, ro, pid);
	} else
		msgq(sp, M_INFO, "%s: %s%s%s%s%s: line %lu%s",
		    sp->frp->name, nf, nc, mo, ul, ro, lno, pid);
	return (0);
}

/*
 * msg_open --
 *	Open the message catalogs.
 */
int
msg_open(sp, file)
	SCR *sp;
	char *file;
{
	DB *db;
	DBT data, key;
	recno_t msgno;
	int nf;
	char *p, *t, buf[MAXPATHLEN];

	/*
	 * !!!
	 * Assume that the first file opened is the system default, and that
	 * all subsequent ones user defined.  Only display error messages
	 * if we can't open the user defined ones -- it's useful to know if
	 * the system one wasn't there, but if nvi is being shipped with an
	 * installed system, the file will be there, if it's not, then the
	 * message will be repeated every time nvi is started up.
	 */
	if ((p = strrchr(file, '/')) != NULL && p[1] == '\0' &&
	    ((t = getenv("LANG")) != NULL ||
	    (t = getenv("LC_MESSAGES")) != NULL)) {
		(void)snprintf(buf, sizeof(buf), "%svi_%s", file, t);
		p = buf;
	} else
		p = file;
	if ((db = dbopen(p,
	    O_NONBLOCK | O_RDONLY, 0, DB_RECNO, NULL)) == NULL) {
		if (O_STR(sp, O_MSGCAT) == NULL)
			return (1);
		p = msg_print(sp, p, &nf);
		msgq(sp, M_SYSERR, "%s", p);
		if (nf)
			FREE_SPACE(sp, p, 0);
		return (1);
	}

	/*
	 * Test record 1 for the magic string.  The msgq call
	 * is here so the message catalog build finds it.
	 */
#define	VMC	"VI_MESSAGE_CATALOG"
	key.data = &msgno;
	key.size = sizeof(recno_t);
	msgno = 1;
	if (db->get(db, &key, &data, 0) != 0 ||
	    data.size != sizeof(VMC) - 1 ||
	    memcmp(data.data, VMC, sizeof(VMC) - 1)) {
		(void)db->close(db);
		if (O_STR(sp, O_MSGCAT) == NULL)
			return (1);
		p = msg_print(sp, p, &nf);
		msgq(sp, M_ERR, "232|The file %s is not a message catalog", p);
		if (nf)
			FREE_SPACE(sp, p, 0);
		return (1);
	}

	if (sp->gp->msg != NULL)
		(void)sp->gp->msg->close(sp->gp->msg);
	sp->gp->msg = db;
	return (0);
}

/*
 * msg_close --
 *	Close the message catalogs.
 */
void
msg_close(gp)
	GS *gp;
{
	if (gp->msg != NULL)
		(void)gp->msg->close(gp->msg);
}

/*
 * msg_cat --
 *	Return a single message from the catalog, plus its length.
 *
 * !!!
 * Only a single catalog message can be accessed at a time, if multiple
 * ones are needed, they must be copied into local memory.
 */
static const char *
msg_cat(gp, str, lenp)
	GS *gp;
	const char *str;
	size_t *lenp;
{
	DBT data, key;
	recno_t msgno;

	/*
	 * If it's not a catalog message, i.e. has doesn't have a leading
	 * number and '|' symbol, we're done.
	 */
	if (isdigit(str[0]) &&
	    isdigit(str[1]) && isdigit(str[2]) && str[3] == '|') {
		key.data = &msgno;
		key.size = sizeof(recno_t);
		msgno = atoi(str);

		/*
		 * XXX
		 * Really sleazy hack -- we put an extra character on the
		 * end of the format string, and then we change it to be
		 * the nul termination of the string.  There ought to be
		 * a better way.  Once we can allocate multiple temporary
		 * memory buffers, maybe we can use one of them instead.
		 */
		if (gp->msg != NULL &&
		    gp->msg->get(gp->msg, &key, &data, 0) == 0 &&
		    data.size != 0) {
			if (lenp != NULL)
				*lenp = data.size - 1;
			((char *)data.data)[data.size - 1] = '\0';
			return (data.data);
		}
		str = &str[4];
	}
	if (lenp != NULL)
		*lenp = strlen(str);
	return (str);
}

/*
 * msg_print --
 *	Return a printable version of a string, in allocated memory.
 */
char *
msg_print(sp, s, needfree)
	SCR *sp;
	char *s;
	int *needfree;
{
	size_t blen, nlen;
	char *bp, *ep, *p, *t;

	*needfree = 0;
	for (p = s; *p != '\0'; ++p)
		if (!isprint(*p))
			break;
	if (*p == '\0')
		return (s);

	nlen = 0;
	if (0) {
retry:		FREE_SPACE(sp, bp, blen);
	}
	nlen += 256;
	GET_SPACE_GOTO(sp, bp, blen, nlen);
	if (0) {
binc_err:	return ("");
	}

	for (p = bp, ep = (bp + blen) - 1; *s != '\0' && p < ep; ++s)
		for (t = KEY_NAME(sp, *s); *t != '\0' && p < ep; *p++ = *t++);
	if (p == ep)
		goto retry;
	*p = '\0';
	return (bp);
}
