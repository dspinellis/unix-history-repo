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
static char sccsid[] = "@(#)vi.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "vcmd.h"

static int getcmd
	    __P((SCR *, EXF *, VICMDARG *, VICMDARG *, VICMDARG *, int *));
static int getkeyword __P((SCR *, EXF *, VICMDARG *, u_int));
static int getmotion
	    __P((SCR *, EXF *, VICMDARG *, VICMDARG *, MARK *, MARK *));

/*
 * vi --
 * 	Main vi command loop.
 */
int
vi(sp, ep)
	SCR *sp;
	EXF *ep;
{
	MARK fm, tm, m;
	VICMDARG cmd, dot, dotmotion, *vp;
	int comcount, eval;
	u_int flags;

	/* Start vi. */
	if (v_init(sp, ep))
		return (1);

	/* Paint the screen. */
	if (sp->s_refresh(sp, ep))
		return (v_end(sp));

	/* Command initialization. */
	cmd.keyword = dot.keyword = dotmotion.keyword = NULL;
	cmd.kbuflen = dot.kbuflen = dotmotion.kbuflen = 0;

	for (eval = 0;;) {
		if (!term_more_pseudo(sp) && log_cursor(sp, ep))
			goto err;

		/*
		 * We get a command, which may or may not have an associated
		 * motion.  If it does, we get it too, calling its underlying
		 * function to get the resulting mark.  We then call the
		 * command setting the cursor to the resulting mark.
		 */
		vp = &cmd;
		if (getcmd(sp, ep, &dot, vp, NULL, &comcount))
			goto err;

		/*
		 * Historical practice: if a dot command gets a new count,
		 * any motion component goes away, i.e. "d3w2." deletes a
		 * total of 5 words.
		 */
		if (F_ISSET(vp, VC_ISDOT) && comcount)
			dotmotion.count = 1;

		flags = vp->kp->flags;

		/* Get any associated keyword. */
		if (LF_ISSET(V_KEYNUM | V_KEYW) &&
		    getkeyword(sp, ep, vp, flags))
			goto err;

		/* If a non-relative movement, set the '' mark. */
		if (LF_ISSET(V_ABS)) {
			m.lno = sp->lno;
			m.cno = sp->cno;
			if (mark_set(sp, ep, ABSMARK1, &m))
				goto err;
		}

		/*
		 * Do any required motion.  If no motion specified, and it's
		 * a line-oriented command, set the to MARK anyway, it's used
		 * by some commands.  If count specified, then the to MARK is
		 * set to that many lines, counting the current one.
		 */
		if (LF_ISSET(V_MOTION)) {
			if (getmotion(sp, ep, &dotmotion, vp, &fm, &tm))
				goto err;
		} else {
			fm.lno = sp->lno;
			fm.cno = sp->cno;
			if (F_ISSET(vp->kp, V_LMODE) &&
			    F_ISSET(vp, VC_C1SET)) {
				tm.lno = sp->lno + vp->count - 1;
				tm.cno = sp->cno;
			} else
				tm = fm;
		}

		/*
		 * Call the function.  Set the return cursor first for
		 * purely defensive reasons.  Sometimes the underlying
		 * routines forget.
		 */
		m = fm;
		if ((vp->kp->func)(sp, ep, vp, &fm, &tm, &m))
			goto err;
		
		/*
		 * If that command took us out of vi or changed the screen,
		 * then exit the loop without further action.
		 */
		if (!F_ISSET(sp, S_MODE_VI) || F_ISSET(sp, S_MAJOR_CHANGE))
			break;

		/* Set the dot command structure. */
		if (LF_ISSET(V_DOT)) {
			dot = cmd;
			F_SET(&dot, VC_ISDOT);
			/*
			 * If a count was supplied for both the command and
			 * its motion, the count was used only for the motion.
			 * Turn the count back on for the dot structure.
			 */
			if (F_ISSET(vp, VC_C1RESET))
				F_SET(&dot, VC_C1SET);
		}

		/*
		 * Some vi row movements are "attracted" to the last position
		 * set, i.e. the V_RCM commands are moths to the V_RCM_SET
		 * commands' candle.  It's broken into two parts.  Here we deal
		 * with the command flags.  In sp->relative(), we deal with the
		 * screen flags.  If the movement is to the EOL the vi command
		 * handles it.  If it's to the beginning, we handle it here.
		 *
		 * Does this totally violate the screen and editor layering?
		 * You betcha.  As they say, if you think you understand it,
		 * you don't.
		 */
		if (LF_ISSET(V_RCM))
			m.cno = sp->s_relative(sp, ep, m.lno);
		else if (LF_ISSET(V_RCM_SETFNB)) {
			if (nonblank(sp, ep, m.lno, &m.cno))
				goto err;
			sp->rcmflags = RCM_FNB;
		}
		else if (LF_ISSET(V_RCM_SETLAST))
			sp->rcmflags = RCM_LAST;
			
		/* Update the cursor. */
		sp->lno = m.lno;
		sp->cno = m.cno;

		if (!term_more_pseudo(sp)) {
			msg_rpt(sp, NULL);

			if (0)
err:				term_flush_pseudo(sp);

			/* Refresh the screen. */
			if (sp->s_refresh(sp, ep)) {
				eval = 1;
				break;
			}
		}

		/* Set the new favorite position. */
		if (LF_ISSET(V_RCM_SET)) {
			sp->rcmflags = 0;
			sp->rcm = sp->sc_col;
		}
	}
	return (v_end(sp) || eval);
}

#define	KEY(sp, k) {							\
	(k) = term_key(sp, TXT_MAPCOMMAND);				\
	if (F_ISSET(sp, S_UPDATE_MODE)) {				\
		F_CLR(sp, S_UPDATE_MODE);				\
		sp->s_refresh(sp, ep);					\
	}								\
	if (sp->special[(k)] == K_VLNEXT)				\
		(k) = term_key(sp, TXT_MAPCOMMAND);			\
	if (sp->special[(k)] == K_ESCAPE) {				\
		if (esc_bell)						\
		    msgq(sp, M_BERR, "Already in command mode");	\
		return (1);						\
	}								\
}

#define	GETCOUNT(sp, count) {						\
	count = 0;							\
	do {								\
		hold = count * 10 + key - '0';				\
		if (count > hold) {					\
			msgq(sp, M_ERR,					\
			    "Number larger than %lu", ULONG_MAX);	\
			return (NULL);					\
		}							\
		count = hold;						\
		KEY(sp, key);						\
	} while (isdigit(key));						\
}

/*
 * getcmd --
 *
 * The command structure for vi is less complex than ex (and don't think
 * I'm not grateful!)  The command syntax is:
 *
 *	[count] [buffer] [count] key [[motion] | [buffer] [character]]
 *
 * and there are several special cases.  The motion value is itself a vi
 * command, with the syntax:
 *
 *	[count] key [character]
 */
static int
getcmd(sp, ep, dp, vp, ismotion, comcountp)
	SCR *sp;
	EXF *ep;
	VICMDARG *dp, *vp;
	VICMDARG *ismotion;	/* Previous key if getting motion component. */
	int *comcountp;
{
	register VIKEYS const *kp;
	register u_int flags;
	u_long hold;
	int esc_bell, key;

	memset(&vp->vpstartzero, 0,
	    (char *)&vp->vpendzero - (char *)&vp->vpstartzero);

	/* An escape bells the user only if already in command mode. */
	esc_bell = ismotion == NULL ? 1 : 0;
	KEY(sp, key)
	esc_bell = 0;
	if (key < 0 || key > MAXVIKEY) {
		msgq(sp, M_BERR, "%s isn't a vi command", charname(sp, key));
		return (1);
	}

	/* Pick up optional buffer. */
	if (key == '"') {
		KEY(sp, key);
		if (!isalnum(key))
			goto ebuf;
		vp->buffer = key;
		KEY(sp, key);
	} else
		vp->buffer = OOBCB;

	/*
	 * Pick up optional count, where a leading 0 is not a count,
	 * it's a command.
	 */
	if (isdigit(key) && key != '0') {
		GETCOUNT(sp, vp->count);
		F_SET(vp, VC_C1SET);
		*comcountp = 1;
	} else
		*comcountp = 0;

	/* Pick up optional buffer. */
	if (key == '"') {
		if (vp->buffer != OOBCB) {
			msgq(sp, M_ERR,
			    "Only one buffer can be specified.");
			return (1);
		}
		KEY(sp, key);
		if (!isalnum(key))
			goto ebuf;
		vp->buffer = key;
		KEY(sp, key);
	}

	/* Find the command. */
	kp = vp->kp = &vikeys[vp->key = key];
	if (kp->func == NULL) {
		/* If dot, set new count/buffer, if any, and return. */
		if (key == '.') {
			if (F_ISSET(dp, VC_ISDOT)) {
				if (F_ISSET(vp, VC_C1SET)) {
					F_SET(dp, VC_C1SET);
					dp->count = vp->count;
				}
				if (vp->buffer != OOBCB)
					dp->buffer = vp->buffer;
				*vp = *dp;
				return (0);
			}
			msgq(sp, M_ERR,
			    "No commands which set dot executed yet.");
		} else
			msgq(sp, M_ERR,
			    "%s isn't a command", charname(sp, key));
		return (1);
	}

	flags = kp->flags;

	/* Check for illegal count. */
	if (F_ISSET(vp, VC_C1SET) && !LF_ISSET(V_CNT))
		goto usage;

	/* Illegal motion command. */
	if (ismotion == NULL) {
		/* Illegal buffer. */
		if (!LF_ISSET(V_OBUF) && vp->buffer != OOBCB)
			goto usage;

		/* Required buffer. */
		if (LF_ISSET(V_RBUF)) {
			KEY(sp, key);
			if (key != '"')
				goto usage;
			KEY(sp, key);
			if (key > UCHAR_MAX) {
ebuf:				msgq(sp, M_ERR, "Invalid buffer name.");
				return (1);
			}
			vp->buffer = key;
		}

		/*
		 * Special case: '[' and ']' commands.  Doesn't the fact
		 * that the *single* characters don't mean anything but
		 * the *doubled* characters do just frost your shorts?
		 */
		if (vp->key == '[' || vp->key == ']') {
			KEY(sp, key);
			if (vp->key != key)
				goto usage;
		}
		/* Special case: 'Z' command. */
		if (vp->key == 'Z') {
			KEY(sp, key);
			if (vp->key != key)
				goto usage;
		}
		/* Special case: 'z' command. */
		if (vp->key == 'z') {
			KEY(sp, key);
			if (isdigit(key)) {
				GETCOUNT(sp, vp->count2);
				F_SET(vp, VC_C2SET);
			}
			vp->character = key;
		}
	}

	/*
	 * Commands that have motion components can be doubled to
	 * imply the current line.
	 */
	else if (ismotion->key != key && !LF_ISSET(V_MOVE)) {
usage:		msgq(sp, M_ERR, "Usage: %s", ismotion != NULL ?
		    vikeys[ismotion->key].usage : kp->usage);
		return (1);
	}

	/* Required character. */
	if (LF_ISSET(V_CHAR))
		KEY(sp, vp->character);

	return (0);
}

/*
 * getmotion --
 *
 * Get resulting motion mark.
 */
static int
getmotion(sp, ep, dm, vp, fm, tm)
	SCR *sp;
	EXF *ep;
	VICMDARG *dm, *vp;
	MARK *fm, *tm;
{
	MARK m;
	VICMDARG motion;
	size_t len;
	u_long cnt;
	int notused;

	/* If '.' command, use the dot motion, else get the motion command. */
	if (F_ISSET(vp, VC_ISDOT))
		motion = *dm;
	else if (getcmd(sp, ep, NULL, &motion, vp, &notused))
		return (1);

	/*
	 * A count may be provided both to the command and to the motion, in
	 * which case the count is multiplicative.  For example, "3y4y" is the
	 * same as "12yy".  This count is provided to the motion command and
	 * not to the regular function. 
	 */
	cnt = motion.count = F_ISSET(&motion, VC_C1SET) ? motion.count : 1;
	if (F_ISSET(vp, VC_C1SET)) {
		motion.count *= vp->count;
		F_SET(&motion, VC_C1SET);

		/*
		 * Set flags to restore the original values of the command
		 * structure so dot commands can change the count values,
		 * e.g. "2dw" "3." deletes a total of five words.
		 */
		F_CLR(vp, VC_C1SET);
		F_SET(vp, VC_C1RESET);
	}

	/*
	 * Some commands can be repeated to indicate the current line.  In
	 * this case, or if the command is a "line command", set the flags
	 * appropriately.  If not a doubled command, run the function to get
	 * the resulting mark.
 	 */
	if (vp->key == motion.key) {
		F_SET(vp, VC_LMODE);

		/*
		 * Set the end of the command; the column is after the
		 * last line.
		 *
		 * If the current line is missing, i.e. the file is empty,
		 * historic vi permitted a "cc" command to change it.
		 */
		tm->lno = sp->lno + motion.count - 1;
		len = 0;
		if (file_gline(sp, ep, tm->lno, &len) == NULL &&
		    (!F_ISSET(vp->kp, VC_C) || tm->lno != 1)) {
			m.lno = sp->lno;
			m.cno = sp->cno;
			v_eof(sp, ep, &m);
			return (1);
		}
		tm->cno = len;

		/*
		 * Set the origin of the command.  Don't set the cursor column
		 * value, since some commands use it to set the return column
		 * value -- underlying functions are expected to recognized
		 * line mode without using the column number.
		 */
		fm->lno = sp->lno;
	} else {
		/*
		 * Motion commands change the underlying movement (*snarl*).
		 * For example, "l" is illegal at the end of a line, but "dl"
		 * is not.  Set flags so the function knows the situation.
		 */
		F_SET(&motion, vp->kp->flags & VC_COMMASK);

		m.lno = sp->lno;
		m.cno = sp->cno;
		if ((motion.kp->func)(sp, ep, &motion, &m, NULL, tm))
			return (1);

		/*
		 * If the underlying motion was a line motion, set the flag
		 * in the command structure.
		 */
		if (F_ISSET(motion.kp, V_LMODE))
			F_SET(vp, VC_LMODE);

		/*
		 * If the motion is in a backward direction, switch the current
		 * location so that we're always moving in the same direction.
		 */
		if (tm->lno < sp->lno ||
		    tm->lno == sp->lno && tm->cno < sp->cno) {
			*fm = *tm;
			tm->lno = sp->lno;
			tm->cno = sp->cno;
		} else {
			fm->lno = sp->lno;
			fm->cno = sp->cno;
		}
	}

	/*
	 * If a dot command save motion structure.  Note that the motion count
	 * was changed above and needs to be reset.
	 */
	if (F_ISSET(vp->kp, V_DOT)) {
		*dm = motion;
		dm->count = cnt;
	}
	return (0);
}

#define	innum(c)	(isdigit(c) || strchr("abcdefABCDEF", c))

static int
getkeyword(sp, ep, kp, flags)
	SCR *sp;
	EXF *ep;
	VICMDARG *kp;
	u_int flags;
{
	register size_t beg, end;
	size_t len;
	char *p;

	p = file_gline(sp, ep, sp->lno, &len);
	beg = sp->cno;

	/* May not be a keyword at all. */
	if (p == NULL || len == 0 ||
	    LF_ISSET(V_KEYW) && !inword(p[beg]) ||
	    LF_ISSET(V_KEYNUM) && !innum(p[beg])) {
noword:		msgq(sp, M_BERR, "Cursor not in a %s",
		    LF_ISSET(V_KEYW) ? "word" : "number");
		return (1);
	}

	/* Find the beginning/end of the keyword. */
	if (beg != 0)
		if (LF_ISSET(V_KEYW)) {
			for (;;) {
				--beg;
				if (!inword(p[beg])) {
					++beg;
					break;
				}
				if (beg == 0)
					break;
			}
		} else {
			for (;;) {
				--beg;
				if (!innum(p[beg])) {
					if (beg > 0 && p[beg - 1] == '0' &&
					    (p[beg] == 'X' || p[beg] == 'x'))
						--beg;
					else
						++beg;
					break;
				}
				if (beg == 0)
					break;
			}

			/* Skip possible leading sign. */
			if (beg != 0 && p[beg] != '0' &&
			    (p[beg - 1] == '+' || p[beg - 1] == '-'))
				--beg;
		}

	if (LF_ISSET(V_KEYW)) {
		for (end = sp->cno; ++end < len && inword(p[end]););
		--end;
	} else {
		for (end = sp->cno; ++end < len;) {
			if (p[end] == 'X' || p[end] == 'x') {
				if (end != beg + 1 || p[beg] != '0')
					break;
				continue;
			}
			if (!innum(p[end]))
				break;
		}

		/* Just a sign isn't a number. */
		if (end == beg && (p[beg] == '+' || p[beg] == '-'))
			goto noword;
		--end;
	}

	/*
	 * Getting a keyword implies moving the cursor to its beginning.
	 * Refresh now.
	 */
	if (beg != sp->cno) {
		sp->cno = beg;
		sp->s_refresh(sp, ep);
	}

	/*
	 * XXX
	 * 8-bit clean problem.  Numeric keywords are handled using strtol(3)
	 * and friends.  This would have to be fixed in v_increment and here
	 * to not depend on a trailing NULL.
	 */
	len = (end - beg) + 2;				/* XXX */
	kp->klen = (end - beg) + 1;
	BINC(sp, kp->keyword, kp->kbuflen, len);
	memmove(kp->keyword, p + beg, kp->klen);
	kp->keyword[kp->klen] = '\0';			/* XXX */
	return (0);
}
