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
 */

#ifndef lint
static char sccsid[] = "@(#)term.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>

#include <ctype.h>
#include <curses.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"

static void	check_sigwinch __P((SCR *));
static void	onwinch __P((int));
static int	term_read __P((SCR *, char *, int, int));

typedef struct _klist {
	char *ts;				/* Key's termcap string. */
	char *output;				/* Corresponding vi command. */
	char *name;				/* Name. */
} KLIST;

static KLIST klist[] = {
	{"kA",    "O", "insert line"},
	{"kD",    "x", "delete character"},
	{"kd",    "j", "cursor down"},
	{"kE",    "D", "delete to eol"},
	{"kF", "\004", "scroll down"},
	{"kH",    "$", "go to eol"},
	{"kh",    "^", "go to sol"},
	{"kI",    "i", "insert at cursor"},
	{"kL",   "dd", "delete line"},
	{"kl",    "h", "cursor left"},
	{"kN", "\006", "page down"},
	{"kP", "\002", "page up"},
	{"kR", "\025", "scroll up"},
	{"kS",	 "dG", "delete to end of screen"},
	{"kr",    "l", "cursor right"},
	{"ku",    "k", "cursor up"},
	{NULL},
};

/*
 * term_init --
 *	Initialize the special array and special keys.  The special array
 *	has a value for each special character that we can use in a switch
 *	statement.
 */
int
term_init(sp)
	SCR *sp;
{
	KLIST *kp;
	char *sbp, *t, buf[2 * 1024], sbuf[128];

	/* Keys that are treated specially. */
	sp->special['^'] = K_CARAT;
	sp->special['\004'] = K_CNTRLD;
	sp->special['\022'] = K_CNTRLR;
	sp->special['\024'] = K_CNTRLT;
	sp->special['\032'] = K_CNTRLZ;
	sp->special[':'] = K_COLON;
	sp->special['\r'] = K_CR;
	sp->special['\033'] = K_ESCAPE;
	sp->special['\f'] = K_FORMFEED;
	sp->special['\n'] = K_NL;
	sp->special[')'] = K_RIGHTPAREN;
	sp->special['}'] = K_RIGHTBRACE;
	sp->special['\t'] = K_TAB;
	sp->special[sp->gp->original_termios.c_cc[VERASE]] = K_VERASE;
	sp->special[sp->gp->original_termios.c_cc[VKILL]] = K_VKILL;
	sp->special[sp->gp->original_termios.c_cc[VLNEXT]] = K_VLNEXT;
	sp->special[sp->gp->original_termios.c_cc[VWERASE]] = K_VWERASE;
	sp->special['0'] = K_ZERO;

	/*
	 * Special terminal keys.
	 * Get the termcap entry.
	 */
	switch (tgetent(buf, O_STR(sp, O_TERM))) {
	case -1:
		msgq(sp, M_ERR, "%s tgetent: %s.",
		    O_STR(sp, O_TERM), strerror(errno));
		return (1);
	case 0:
		msgq(sp, M_ERR, "%s: unknown terminal type.",
		    O_STR(sp, O_TERM), strerror(errno));
		return (1);
	}

	for (kp = klist; kp->name != NULL; ++kp) {
		sbp = sbuf;
		if ((t = tgetstr(kp->ts, &sbp)) == NULL)
			continue;
		if (seq_set(sp, kp->name, t, kp->output, SEQ_COMMAND, 0))
			return (1);
	}
	return (0);
}

/*
 * term_flush_pseudo --
 *	Flush the pseudo keys if an error occurred during the map.
 */
void
term_flush_pseudo(sp)
	SCR *sp;
{
	if (sp->atkey_len != 0) {
		free(sp->atkey_buf);
		sp->atkey_len = 0;
	}
	sp->mappedkey = NULL;
}

/*
 * term_more_pseudo --
 *	Return if there are more pseudo keys.
 */
int
term_more_pseudo(sp)
	SCR *sp;
{
	return (sp->atkey_len || sp->mappedkey != NULL);
}

/*
 * term_waiting --
 *	Return keys waiting.
 */
int
term_waiting(sp)
	SCR *sp;
{
	struct timeval t;

	if (sp->atkey_len || sp->mappedkey != NULL)
		return (1);

	t.tv_sec = t.tv_usec = 0;
	FD_SET(STDIN_FILENO, &sp->rdfd);
	return (select(32, &sp->rdfd, NULL, NULL, &t));
}

/*
 * term_key --
 *	This function reads in a keystroke, as well as handling
 *	mapped keys and executed cut buffers.
 */
int
term_key(sp, flags)
	SCR *sp;
	u_int flags;			/* TXT_MAPCOMMAND */
{
	int ch;
	SEQ *qp;
	int ispartial, nr;

	/* Sync the recovery file if necessary. */
	if (F_ISSET(sp->ep, F_RCV_ALRM)) {
		(void)rcv_sync(sp, sp->ep);
		F_CLR(sp->ep, F_RCV_ALRM);
	}

	/* If in the middle of an @ macro, return the next char. */
	if (sp->atkey_len) {
		ch = *sp->atkey_cur++;
		if (--sp->atkey_len == 0)
			free(sp->atkey_buf);
		goto ret;
	}

	/* If returning a mapped key, return the next char. */
	if (sp->mappedkey) {
		ch = *sp->mappedkey;
		if (*++sp->mappedkey == '\0')
			sp->mappedkey = NULL;
		goto ret;
	}

	/* Read in more keys if necessary. */
	if (sp->nkeybuf == 0) {
		/* Read the keystrokes. */
		sp->nkeybuf = term_read(sp, sp->keybuf, sizeof(sp->keybuf), 0);
		sp->nextkey = 0;
		
		/*
		 * If no keys read, then we've reached EOF of an ex script.
		 * XXX
		 * This is just wrong...
		 */
		if (sp->nkeybuf == 0) {
			F_SET(sp, S_EXIT_FORCE);
			return(0);
		}
	}

	/*
	 * Check for mapped keys. If get a partial match, copy the current
	 * keys down in memory to maximize the space for new keys, and try
	 * to read more keys to complete the map.  Max map is sizeof(keybuf)
	 * and probably not worth fixing.
	 */
	if (LF_ISSET(TXT_MAPCOMMAND)) {
retry:		qp = seq_find(sp, &sp->keybuf[sp->nextkey], sp->nkeybuf,
		    LF_ISSET(TXT_MAPCOMMAND) ? SEQ_COMMAND : SEQ_INPUT,
		    &ispartial);
		if (qp == NULL)
			goto nomap;
		if (ispartial) {
			if (sizeof(sp->keybuf) == sp->nkeybuf) {
				msgq(sp, M_ERR,
				    "Partial map is too long; keys corrupted.");
				goto nomap;
			} else {
				memmove(&sp->keybuf[sp->nextkey],
				    sp->keybuf, sp->nkeybuf);
				sp->nextkey = 0;
				nr = term_read(sp, sp->keybuf + sp->nkeybuf,
				    sizeof(sp->keybuf) - sp->nkeybuf, 1);
				if (nr) {
					sp->nkeybuf += nr;
					goto retry;
				}
			}
		} else {
			sp->nkeybuf -= qp->ilen;
			sp->nextkey += qp->ilen;
			if (qp->output[1] == '\0')
				ch = *qp->output;
			else {
				sp->mappedkey = qp->output;
				ch = *sp->mappedkey++;
			}
			goto ret;
		}
	}

nomap:	--sp->nkeybuf;
	ch = sp->keybuf[sp->nextkey++];

	/*
	 * O_BEAUTIFY eliminates all control characters except tab,
	 * newline, form-feed and escape.
	 */
ret:	if (LF_ISSET(TXT_BEAUTIFY) && O_ISSET(sp, O_BEAUTIFY)) {
		if (isprint(ch) || sp->special[ch] == K_ESCAPE ||
		    sp->special[ch] == K_FORMFEED || sp->special[ch] == K_NL ||
		    sp->special[ch] == K_TAB)
			return (ch);
		sp->s_bell(sp);
		return (term_key(sp, flags));
	}
	return (ch);
}

static int __check_sig_winch;				/* XXX GLOBAL */
static int __set_sig_winch;				/* XXX GLOBAL */

static int
term_read(sp, buf, len, timeout)
	SCR *sp;
	char *buf;		/* Where to store the characters. */
	int len;		/* Max characters to read. */
	int timeout;		/* If timeout set. */
{
	struct timeval t, *tp;
	int nr;

	/* Set up SIGWINCH handler. */
	if (__set_sig_winch == 0) {
		(void)signal(SIGWINCH, onwinch);
		__set_sig_winch = 1;
	}

	/*
	 * If reading from a file or pipe, never timeout.  This
	 * also affects the way that EOF is detected.
	 */
	if (!F_ISSET(sp, S_ISFROMTTY)) {
		if ((nr = read(STDIN_FILENO, buf, len)) == 0)
			F_SET(sp, S_EXIT_FORCE);
		return (0);
	}

	/* Compute the timeout value. */
	if (timeout) {
		t.tv_sec = O_VAL(sp, O_KEYTIME) / 10;
		t.tv_usec = (O_VAL(sp, O_KEYTIME) % 10) * 100000L;
		tp = &t;

		FD_SET(STDIN_FILENO, &sp->rdfd);
	}

	/* Select until characters become available, and then read them. */
	for (;;) {
		if (timeout)
			switch (select(32, &sp->rdfd, NULL, NULL, tp)) {
			case -1:		/* Error or interrupt. */
				if (errno == EINTR) {
					check_sigwinch(sp);
					continue;
				}
				msgq(sp, M_ERR,
				    "Terminal read error: %s", strerror(errno));
				return (0);
			case 0:			/* Timeout. */
				return (0);
		}
		switch (nr = read(STDIN_FILENO, buf, len)) {
		case -1:			/* Error or interrupt. */
			if (errno == EINTR) {
				check_sigwinch(sp);
				continue;
			}
			F_SET(sp, S_EXIT_FORCE);
			msgq(sp, M_ERR,
			    "Terminal read error: %s", strerror(errno));
			return (0);
		case 0:				/* EOF. */
			F_SET(sp, S_EXIT_FORCE);
			return (0);
		default:
			return (nr);
		}
	}
	/* NOTREACHED */
}

/*
 * onwinch --
 *	Handle SIGWINCH.
 */
static void
onwinch(signo)
	int signo;
{
	__check_sig_winch = 1;
}

/*
 * check_sigwinch --
 *	Check for window size change event.   Done here because it's
 *	the only place we block.
 */
static void
check_sigwinch(sp)
	SCR *sp;
{
	sigset_t bmask, omask;

	while (__check_sig_winch == 1) {
		sigemptyset(&bmask);
		sigaddset(&bmask, SIGWINCH);
		(void)sigprocmask(SIG_BLOCK, &bmask, &omask);

		set_window_size(sp, 0);
		F_SET(sp, S_RESIZE);
		if (F_ISSET(sp, S_MODE_VI)) {
			/*
			 * XXX
			 * This code needs an EXF structure!!
			 * (void)scr_update(sp);
			 */
			refresh();
		}
		__check_sig_winch = 0;

		(void)sigprocmask(SIG_SETMASK, &omask, NULL);
	}
}
