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
static char sccsid[] = "@(#)ex_substitute.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "excmd.h"

enum which {AGAIN, MUSTSETR, FIRST};

static int		checkmatchsize __P((SCR *, regex_t *));
static inline int	regsub __P((SCR *,
			    char *, char **, size_t *, size_t *));
static int		substitute __P((SCR *, EXF *,
			    EXCMDARG *, char *, regex_t *, enum which));

int
ex_substitute(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	regex_t *re, lre;
	int eval, reflags;
	char *endp, *sub;
	char delim[2];

	/*
	 * Historic vi only permitted '/' to begin the substitution command.
	 * We permit ';' as well, since users often want to operate on UNIX
	 * pathnames.  We don't just allow anything because the flag chars
	 * wouldn't work.
	 */
	if (*cmdp->string == '/' || *cmdp->string == ';') {
		/* Delimiter is the first character. */
		delim[0] = cmdp->string[0];
		delim[1] = '\0';

		/* Get the substitute string. */
		endp = cmdp->string + 1;
		sub = strsep(&endp, delim);

		/* Get the replacement string, save it off. */
		if (endp == NULL || *endp == NULL) {
			msgq(sp, M_ERR, "No replacement string specified.");
			return (1);
		}
		if (sp->repl != NULL)
			free(sp->repl);
		sp->repl = strsep(&endp, delim);
		sp->repl = strdup(sp->repl);
		sp->repl_len = strlen(sp->repl);

		/* If the substitute string is empty, use the last one. */
		if (*sub == NULL) {
			if (!F_ISSET(sp, S_RE_SET)) {
				msgq(sp, M_ERR,
				    "No previous regular expression.");
				return (1);
			}
			if (checkmatchsize(sp, &sp->sre))
				return (1);
			return (substitute(sp, ep,
			    cmdp, endp ? endp : "", &sp->sre, AGAIN));
		}

		/* Set RE flags. */
		reflags = 0;
		if (O_ISSET(sp, O_EXTENDED))
			reflags |= REG_EXTENDED;
		if (O_ISSET(sp, O_IGNORECASE))
			reflags |= REG_ICASE;

		/* Compile the RE. */
		re = &lre;
		if (eval = regcomp(re, (char *)sub, reflags)) {
			re_error(sp, eval, re);
			return (1);
		}

		/*
		 * Set saved RE.  Historic practice is that substitutes set
		 * direction as well as the RE.
		 */
		sp->sre = lre;
		sp->searchdir = FORWARD;
		F_SET(sp, S_RE_SET);

		if (checkmatchsize(sp, &sp->sre))
			return (1);
		return (substitute(sp, ep, cmdp, endp ? endp : "", re, FIRST));
	}
	return (substitute(sp, ep, cmdp, cmdp->string, &sp->sre, MUSTSETR));
}

int
ex_subagain(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	if (!F_ISSET(sp, S_RE_SET)) {
		msgq(sp, M_ERR, "No previous regular expression.");
		return (1);
	}
	return (substitute(sp, ep, cmdp, cmdp->string, &sp->sre, AGAIN));
}

/* 
 * The nasty part of the substitution is what happens when the replacement
 * string contains newlines.  It's a bit tricky -- consider the information
 * that has to be retained for "s/f\(o\)o/^M\1^M\1/".  The solution here is
 * to build a set of newline offets which we use to break the line up later,
 * when the replacement is done.  Don't change it unless you're pretty damned
 * confident.
 */
#define	NEEDNEWLINE(sp) {						\
	if (sp->newl_len == sp->newl_cnt) {				\
		sp->newl_len += 25;					\
		if ((sp->newl = realloc(sp->newl,			\
		    sp->newl_len * sizeof(size_t))) == NULL) {		\
			msgq(sp, M_ERR,					\
			    "Error: %s", strerror(errno));		\
			sp->newl_len = 0;				\
			return (1);					\
		}							\
	}								\
}

#define	BUILD(sp, l, len) {						\
	if (lbclen + (len) > lblen) {					\
		lblen += MAX(lbclen + (len), 256);			\
		if ((lb = realloc(lb, lblen)) == NULL) {		\
			msgq(sp, M_ERR,					\
			    "Error: %s", strerror(errno));		\
			lbclen = 0;					\
			return (1);					\
		}							\
	}								\
	memmove(lb + lbclen, l, len);					\
	lbclen += len;							\
}

#define	NEEDSP(sp, len, pnt) {						\
	if (lbclen + (len) > lblen) {					\
		lblen += MAX(lbclen + (len), 256);			\
		if ((lb = realloc(lb, lblen)) == NULL) {		\
			msgq(sp, M_ERR,					\
			    "Error: %s", strerror(errno));		\
			lbclen = 0;					\
			return (1);					\
		}							\
		pnt = lb + lbclen;					\
	}								\
}

/*
 * substitute --
 *	Do the substitution.  This stuff is *really* tricky.  There are
 *	lots of special cases, and general nastiness.  Don't mess with it
 * 	unless you're pretty confident.
 */
static int
substitute(sp, ep, cmdp, s, re, cmd)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
	char *s;
	regex_t *re;
	enum which cmd;
{
	MARK from, to;
	recno_t elno, lno, lastline;
	size_t blen, cnt, last, lbclen, lblen, len, offset;
	int eflags, eval, linechanged, quit;
	int cflag, gflag, lflag, nflag, pflag, rflag;
	char *bp, *lb;

	/*
	 * Historic vi permitted the '#', 'l' and 'p' options in vi mode,
	 * but it only displayed the last change and they really don't
	 * make any sense.  In the current model the problem is combining
	 * them with the 'c' flag -- the screen would have to flip back
	 * and forth between the confirm screen and the ex print screen,
	 * which would be pretty awful.  Not worth the effort.
	 */
	cflag = gflag = lflag = nflag = pflag = rflag = 0;
	for (; *s; ++s)
		switch (*s) {
		case ' ':
		case '\t':
			break;
		case '#':
			if (F_ISSET(sp, S_MODE_VI)) {
				msgq(sp, M_ERR,
				    "'#' flag not supported in vi mode.");
				return (1);
			}
			nflag = 1;
			break;
		case 'c':
			cflag = 1;
			break;
		case 'g':
			gflag = 1;
			break;
		case 'l':
			if (F_ISSET(sp, S_MODE_VI)) {
				msgq(sp, M_ERR,
				    "'l' flag not supported in vi mode.");
				return (1);
			}
			lflag = 1;
			break;
		case 'p':
			if (F_ISSET(sp, S_MODE_VI)) {
				msgq(sp, M_ERR,
				    "'p' flag not supported in vi mode.");
				return (1);
			}
			pflag = 1;
			break;
		case 'r':
			if (cmd == FIRST) {
				msgq(sp, M_ERR,
		    "Regular expression specified; r flag meaningless.");
				return (1);
			}
			if (!F_ISSET(sp, S_RE_SET)) {
				msgq(sp, M_ERR,
				    "No previous regular expression.");
				return (1);
			}
			rflag = 1;
			break;
		default:
			goto usage;
		}

	if (rflag == 0 && cmd == MUSTSETR) {
usage:		msgq(sp, M_ERR, "Usage: %s", cmdp->cmd->usage);
		return (1);
	}

	/* Get some space. */
	GET_SPACE(sp, bp, blen, 512);

	/*
	 * lb:		build buffer pointer.
	 * lbclen:	current length of built buffer.
	 * lblen;	length of build buffer.
	 */
	lb = NULL;
	lbclen = lblen = 0;

	/*
	 * Since multiple changes can happen in a line, we only increment
	 * the change count on the first change to a line.
	 */
	lastline = OOBLNO;

	/* For each line... */
	for (quit = 0, lno = cmdp->addr1.lno,
	    elno = cmdp->addr2.lno; !quit && lno <= elno; ++lno) {

		/* Get the line. */
		if ((s = file_gline(sp, ep, lno, &len)) == NULL) {
			GETLINE_ERR(sp, lno);
			return (1);
		}

		/*
		 * Make a local copy if doing confirmation -- when calling
		 * the confirm routine we're likely to lose our cached copy.
		 */
		if (cflag) {
			ADD_SPACE(sp, bp, blen, len)
			memmove(bp, s, len);
			s = bp;
		}

		/* Reset the buffer pointer. */
		lbclen = 0;

		/*
		 * We don't want to have to do a setline if the line didn't
		 * change -- keep track of whether or not this line changed.
		 */
		linechanged = 0;

		/* It's not nul terminated, but we pretend it is. */
		eflags = REG_STARTEND;

		/* The search area is from 's' to the end of the line. */
nextmatch:	sp->match[0].rm_so = 0;
		sp->match[0].rm_eo = len;

		/* Get the next match. */
skipmatch:	eval = regexec(re,
		    (char *)s, re->re_nsub + 1, sp->match, eflags);

		/*
		 * There wasn't a match -- if there was an error, deal with
		 * it.  If there was a previous match in this line, resolve
		 * the changes into the database.  Otherwise, just move on.
		 */
		if (eval == REG_NOMATCH) {
			if (linechanged)
				goto endmatch;
			continue;
		}
		if (eval != 0) {
			re_error(sp, eval, re);
			goto ret1;
		}

		/* Confirm change. */
		if (cflag) {
			/*
			 * Set the cursor position for confirmation.  Note,
			 * if we matched on a '$', the cursor may be past
			 * the end of line.
			 *
			 * XXX
			 * May want to "fix" this in the confirm routine;
			 * the confirm routine may be able to display a
			 * cursor past EOL.
			 */
			from.lno = lno;
			from.cno = sp->match[0].rm_so;
			to.lno = lno;
			to.cno = sp->match[0].rm_eo;
			if (len != 0) {
				if (to.cno >= len)
					to.cno = len - 1;
				if (from.cno >= len)
					from.cno = len - 1;
			}

			switch (sp->s_confirm(sp, ep, &from, &to)) {
			case YES:
				break;
			case NO:
				/*
				 * Copy the bytes before the match and the
				 * bytes in the match into the build buffer.
				 */
				BUILD(sp, s, sp->match[0].rm_eo);
				goto skip;
			case QUIT:
				/* Set the quit flag. */
				quit = 1;

				/* If in a global, pass the info back. */
				if (F_ISSET(sp, S_GLOBAL))
					F_SET(sp, S_GLOBAL_QUIT);
				
				/*
				 * If any changes, resolve them, otherwise
				 * return to the main loop.
				 */
				if (linechanged)
					goto endmatch;
				continue;
			}
		}

		/* Copy the bytes before the match into the build buffer. */
		BUILD(sp, s, sp->match[0].rm_so);

		/* Substitute the matching bytes. */
		if (regsub(sp, s, &lb, &lbclen, &lblen))
			goto ret1;

		/* Set the change flag so we know this line was modified. */
		linechanged = 1;

		/* Move the pointers past the matched bytes. */
skip:		s += sp->match[0].rm_eo;
		len -= sp->match[0].rm_eo;

		/*
		 * If doing a global change with confirmation, we have to
		 * update the screen.  The basic idea is to store the line
		 * so the screen update routines can find it, but start at
		 * the old offset.
		 */
		if (linechanged && gflag && cflag) {
			/* Save offset. */
			offset = lbclen;
			
			/* Copy the suffix. */
			if (len)
				BUILD(sp, s, len)

			/* Store inserted lines, adjusting the build buffer. */
			last = 0;
			if (sp->newl_cnt) {
				for (cnt = 0; cnt < sp->newl_cnt;
				    ++cnt, ++lno, ++elno, ++lastline) {
					if (file_iline(sp, ep, lno,
					    lb + last, sp->newl[cnt] - last))
						goto ret1;
					last = sp->newl[cnt] + 1;
					++sp->rptlines[L_ADDED];
				}
				lbclen -= last;
				offset -= last;

				sp->newl_cnt = 0;
				linechanged = 1;
			}

			/* Store the changed line. */
			if (linechanged)
				if (file_sline(sp, ep, lno, lb + last, lbclen))
					goto ret1;

			/* Get a new copy of the line. */
			if ((s = file_gline(sp, ep, lno, &len)) == NULL) {
				GETLINE_ERR(sp, lno);
				goto ret1;
			}
			ADD_SPACE(sp, bp, blen, len)
			memmove(bp, s, len);
			s = bp;

			/* Restart the build. */
			lbclen = 0;

			/* Update changed line counter. */
			if (lastline != lno) {
				++sp->rptlines[L_CHANGED];
				lastline = lno;
			}

			/* Start in the middle of the line. */
			sp->match[0].rm_so = offset;
			sp->match[0].rm_eo = len;
			goto skipmatch;
		}

		/*
		 * If it's a global change, and there's something left in
		 * the line, check it.
		 */
		if (len && gflag) {
			eflags |= REG_NOTBOL;
			goto nextmatch;
		}

		/* Copy any remaining bytes into the build buffer. */
endmatch:	if (len)
			BUILD(sp, s, len)

		/* Store inserted lines, adjusting the build buffer. */
		last = 0;
		if (sp->newl_cnt) {
			for (cnt = 0; cnt < sp->newl_cnt;
			    ++cnt, ++lno, ++elno, ++lastline) {
				if (file_iline(sp, ep,
				    lno, lb + last, sp->newl[cnt] - last))
					goto ret1;
				last = sp->newl[cnt] + 1;
				++sp->rptlines[L_ADDED];
			}
			lbclen -= last;

			sp->newl_cnt = 0;
			linechanged = 1;
		}

		/* Store the changed line. */
		if (linechanged)
			if (file_sline(sp, ep, lno, lb + last, lbclen))
				goto ret1;

		/* Update changed line counter. */
		if (lastline != lno) {
			++sp->rptlines[L_CHANGED];
			lastline = lno;
		}

		/* Display as necessary. */
		if (lflag || nflag || pflag) {
			from.lno = to.lno = lno;
			from.cno = to.cno = 0;
			if (lflag)
				ex_print(sp, ep, &from, &to, E_F_LIST);
			if (nflag)
				ex_print(sp, ep, &from, &to, E_F_HASH);
			if (pflag)
				ex_print(sp, ep, &from, &to, E_F_PRINT);
		}
	}

	/*
	 * Cursor moves to last line changed, unless doing confirm,
	 * in which case don't move it.
	 */
	if (!cflag && lastline != OOBLNO)
		sp->lno = lastline;

	/*
	 * Note if nothing found.  Else, if nothing displayed to the
	 * screen, put something up.
	 */
	if (sp->rptlines[L_CHANGED] == 0 && !F_ISSET(sp, S_GLOBAL))
		msgq(sp, M_INFO, "No match found.");
	else if (!lflag && !nflag && !pflag)
		F_SET(sp, S_AUTOPRINT);

	FREE_SPACE(sp, bp, blen);
	return (0);

ret1:	FREE_SPACE(sp, bp, blen);
	return (1);
}

/*
 * regsub --
 * 	Do the substitution for a regular expression.
 */
static inline int
regsub(sp, ip, lbp, lbclenp, lblenp)
	SCR *sp;
	char *ip;			/* Input line. */
	char **lbp;
	size_t *lbclenp, *lblenp;
{
	size_t lbclen, lblen;		/* Local copies. */
	size_t mlen;			/* Match length. */
	size_t rpl;			/* Remaining replacement length. */
	char *rp;			/* Replacement pointer. */
	int ch;
	int no;				/* Match replacement offset. */
	char *p;			/* Build buffer pointer. */
	char *lb;			/* Local copies. */

	lb = *lbp;			/* Get local copies. */
	lbclen = *lbclenp;
	lblen = *lblenp;

	rp = sp->repl;			/* Set up replacment info. */
	rpl = sp->repl_len;
	for (p = lb + lbclen; rpl--;) {
		ch = *rp++;
		if (ch == '&') {	/* Entire pattern. */
			no = 0;
			goto sub;
					/* Partial pattern. */
		} else if (ch == '\\' && isdigit(*rp)) {
			no = *rp++ - '0';
			--rpl;
sub:			if (sp->match[no].rm_so != -1 &&
			    sp->match[no].rm_eo != -1) {
				mlen =
				    sp->match[no].rm_eo - sp->match[no].rm_so;
				NEEDSP(sp, mlen, p);
				memmove(p, ip + sp->match[no].rm_so, mlen);
				p += mlen;
				lbclen += mlen;
			}
		} else {		/* Newline, ordinary characters. */
			if (sp->special[ch] == K_CR ||
			    sp->special[ch] == K_NL) {
				NEEDNEWLINE(sp);
				sp->newl[sp->newl_cnt++] = lbclen;
			} else if (ch == '\\' && (*rp == '\\' || *rp == '&'))
 				ch = *rp++;
			NEEDSP(sp, 1, p);
 			*p++ = ch;
			++lbclen;
		}
	}

	*lbp = lb;			/* Update caller's information. */
	*lbclenp = lbclen;
	*lblenp = lblen;
	return (0);
}

static int
checkmatchsize(sp, re)
	SCR *sp;
	regex_t *re;
{
	/* Build nsub array as necessary. */
	if (sp->matchsize < re->re_nsub + 1) {
		sp->matchsize = re->re_nsub + 1;
		if ((sp->match = realloc(sp->match,
		    sp->matchsize * sizeof(regmatch_t))) == NULL) {
			msgq(sp, M_ERR, "Error: %s", strerror(errno));
			sp->matchsize = 0;
			return (1);
		}
	}
	return (0);
}
