/*-
 * Copyright (c) 1993, 1994
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
static char sccsid[] = "@(#)sex_screen.c	9.2 (Berkeley) 11/10/94";
#endif /* not lint */

#include <sys/param.h>
#include <sys/queue.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <bitstring.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

#include "compat.h"
#include <curses.h>
#include <db.h>
#include <regex.h>
#include <pathnames.h>

#include "vi.h"
#include "excmd.h"
#include "sex_screen.h"
#include "../svi/svi_screen.h"

enum rc { NOEXIST, NOPERM, RCOK };
static enum rc	exrc_isok __P((SCR *, struct stat *, char *, int, int));

static void	sex_abort __P((void));
static int	sex_noop __P((void));
static int	sex_nope __P((SCR *));
static void	sex_screen_init __P((SCR *));
static int	sex_screen_setup __P((SCR *, struct termios *));
static int	sex_screen_teardown __P((SCR *, struct termios *, int));

/*
 * sex_screen_copy --
 *	Copy to a new screen.
 */
int
sex_screen_copy(orig, sp)
	SCR *orig, *sp;
{
	SEX_PRIVATE *osex, *nsex;

	/* Create the private screen structure. */
	CALLOC_RET(orig, nsex, SEX_PRIVATE *, 1, sizeof(SEX_PRIVATE));
	sp->sex_private = nsex;

/* INITIALIZED AT SCREEN CREATE. */

/* PARTIALLY OR COMPLETELY COPIED FROM PREVIOUS SCREEN. */
	if (orig == NULL) {
	} else {
		osex = SXP(orig);
#ifndef SYSV_CURSES
		if (osex->SE != NULL && (nsex->SE = strdup(osex->SE)) == NULL) {
			msgq(sp, M_SYSERR, NULL);
			return (1);
		}
		if (osex->SO != NULL && (nsex->SO = strdup(osex->SO)) == NULL) {
			msgq(sp, M_SYSERR, NULL);
			free(osex->SE);
			return (1);
		}
#endif
	}

	return (0);
}

/*
 * sex_screen_end --
 *	End a screen.
 */
int
sex_screen_end(sp)
	SCR *sp;
{
#ifndef SYSV_CURSES
	/* Free inverse video strings. */
	if (SXP(sp)->SE != NULL)
		free(SXP(sp)->SE);
	if (SXP(sp)->SO != NULL)
		free(SXP(sp)->SO);
#endif

	/* Free private memory. */
	FREE(SXP(sp), sizeof(SEX_PRIVATE));
	sp->sex_private = NULL;

	return (0);
}

#define	PFLAGS	(EXPAR_BLIGNORE | EXPAR_NOAUTO | EXPAR_NOPRDEF | EXPAR_VLITONLY)
#define	RUN_EXRC(p) {							\
	(void)ex_cfile(sp, p, PFLAGS);					\
	if (F_ISSET(sp, S_EXIT | S_EXIT_FORCE))				\
		goto ret;						\
}
#define	RUN_ICMD(s) {							\
	(void)ex_icmd(sp, s, strlen(s), PFLAGS);			\
	if (F_ISSET(sp, S_EXIT | S_EXIT_FORCE))				\
		goto ret;						\
}

/*
 * sex_screen_icmd --
 *	Execute the command-line ex commands.  This code is here because it
 *	has to run in ex mode.  This code sets up and tears down ex so that
 *	it's transparent.
 */
int
sex_screen_icmd(sp, s)
	SCR *sp;
	char *s;
{
	struct termios t;

	if (sex_screen_setup(sp, &t))
		return (1);
	RUN_ICMD(s);
ret:	return (sex_screen_teardown(sp,
	    &t, !F_ISSET(sp, S_EXIT | S_EXIT_FORCE)));
}

/*
 * sex_screen_exrc --
 *	Read the EXINIT environment variable and the startup exrc files,
 *	and execute their commands.  This code is here because it has to
 *	run in ex mode.  This code sets up and tears down ex so that it's
 *	transparent.
 */
int
sex_screen_exrc(sp)
	SCR *sp;
{
	struct termios t;
	struct stat hsb, lsb;
	int rval;
	char *p, path[MAXPATHLEN];

	if (sex_screen_setup(sp, &t))
		return (1);

	/*
	 * Source the system, environment, $HOME and local .exrc values.
	 * Vi historically didn't check $HOME/.exrc if the environment
	 * variable EXINIT was set.  This is all done before the file is
	 * read in, because things in the .exrc information can set, for
	 * example, the recovery directory.
	 *
	 * !!!
	 * While nvi can handle any of the options settings of historic vi,
	 * the converse is not true.  Since users are going to have to have
	 * files and environmental variables that work with both, we use nvi
	 * versions of both the $HOME and local startup files if they exist,
	 * otherwise the historic ones.
	 *
	 * !!!
	 * For a discussion of permissions and when what .exrc files are
	 * read, see the comment above the exrc_isok() function below.
	 *
	 * !!!
	 * If the user started the historic of vi in $HOME, vi read the user's
	 * .exrc file twice, as $HOME/.exrc and as ./.exrc.  We avoid this, as
	 * it's going to make some commands behave oddly, and I can't imagine
	 * anyone depending on it.
	 */
	rval = 0;
	switch (exrc_isok(sp, &hsb, _PATH_SYSEXRC, 1, 0)) {
	case NOEXIST:
	case NOPERM:
		break;
	case RCOK:
		RUN_EXRC(_PATH_SYSEXRC);
		break;
	}

	if ((p = getenv("NEXINIT")) != NULL || (p = getenv("EXINIT")) != NULL)
		if ((p = strdup(p)) == NULL) {
			rval = 1;
			msgq(sp, M_SYSERR, NULL);
			goto ret;
		} else {
			RUN_ICMD(p);
			free(p);
		}
	else if ((p = getenv("HOME")) != NULL && *p) {
		(void)snprintf(path, sizeof(path), "%s/%s", p, _PATH_NEXRC);
		switch (exrc_isok(sp, &hsb, path, 0, 1)) {
		case NOEXIST:
			(void)snprintf(path,
			    sizeof(path), "%s/%s", p, _PATH_EXRC);
			if (exrc_isok(sp, &hsb, path, 0, 1) == RCOK)
				RUN_EXRC(path);
			break;
		case NOPERM:
			break;
		case RCOK:
			RUN_EXRC(path);
			break;
		}
	}

	if (O_ISSET(sp, O_EXRC))
		switch (exrc_isok(sp, &lsb, _PATH_NEXRC, 0, 0)) {
		case NOEXIST:
			if (exrc_isok(sp, &lsb, _PATH_EXRC, 0, 0) == RCOK &&
			    (lsb.st_dev != hsb.st_dev ||
			    lsb.st_ino != hsb.st_ino))
				RUN_EXRC(_PATH_EXRC);
			break;
		case NOPERM:
			break;
		case RCOK:
			if (lsb.st_dev != hsb.st_dev ||
			    lsb.st_ino != hsb.st_ino)
				RUN_EXRC(_PATH_NEXRC);
			break;
		}

ret:	return (sex_screen_teardown(sp,
	    &t, !F_ISSET(sp, S_EXIT | S_EXIT_FORCE)) || rval);
}

/*
 * sex_screen_edit --
 *	Main ex screen loop.  The ex screen is relatively uncomplicated.
 *	As long as it has a stdio FILE pointer for output, it's happy.
 */
int
sex_screen_edit(sp)
	SCR *sp;
{
	struct termios t;
	int escreen, force, rval;

	escreen = 0;

	if (sex_screen_setup(sp, &t))
		return (1);

	for (;;) {
		/*
		 * Run ex.  If ex fails, sex data structures
		 * may be corrupted, be careful what you do.
		 */
		if (rval = ex(sp)) {
			(void)rcv_sync(sp,
			    RCV_EMAIL | RCV_ENDSESSION | RCV_PRESERVE);
			escreen = 1;
			break;
		}

		force = 0;
		switch (F_ISSET(sp, S_MAJOR_CHANGE)) {
		case S_EXIT_FORCE:
			force = 1;
			/* FALLTHROUGH */
		case S_EXIT:
			F_CLR(sp, S_EXIT_FORCE | S_EXIT);
			if (file_end(sp, NULL, force))
				break;
			escreen = 1;
			/* FALLTHROUGH */
		case 0:				/* Changing from ex mode. */
			goto ret;
		case S_SSWITCH:
		default:
			abort();
		}
	}

ret:	if (sex_screen_teardown(sp, &t, 0))
		rval = 1;
	if (escreen && screen_end(sp))
		rval = 1;
	return (rval);
}

/*
 * sex_screen_setup --
 *	Set up the ex screen.
 */
static int
sex_screen_setup(sp, tp)
	SCR *sp;
	struct termios *tp;
{
	/*
	 * The resize bit may be set, but clear it, we're going to
	 * initialize the screen right now.
	 */
	F_CLR(sp, S_SCR_RESIZE);

	/* Initialize the function set. */
	sex_screen_init(sp);

	/* Initialize the terminal state. */
	if (F_ISSET(sp->gp, G_STDIN_TTY))
		SEX_RAW(tp);

	sp->rows = O_VAL(sp, O_LINES);
	sp->cols = O_VAL(sp, O_COLUMNS);
	return (0);
}

/*
 * sex_screen_teardown --
 *	Tear down the ex screen.
 */
static int
sex_screen_teardown(sp, tp, needwait)
	SCR *sp;
	struct termios *tp;
	int needwait;
{
	CH ikey;
	int rval;

	/* Reset the terminal state. */
	rval = F_ISSET(sp->gp, G_STDIN_TTY) && SEX_NORAW(tp);

	/* If the screen was dirtied, we have to wait. */
	if (needwait && F_ISSET(sp, S_SCR_EXWROTE)) {
		(void)write(STDOUT_FILENO, STR_CMSG, sizeof(STR_CMSG) - 1);
		(void)term_key(sp, &ikey, 0);
	}
	return (rval);
}

/*
 * exrc_isok --
 *	Check a .exrc file for source-ability.
 *
 * !!!
 * Historically, vi read the $HOME and local .exrc files if they were owned
 * by the user's real ID, or the "sourceany" option was set, regardless of
 * any other considerations.  We no longer support the sourceany option as
 * it's a security problem of mammoth proportions.  We require the system
 * .exrc file to be owned by root, the $HOME .exrc file to be owned by the
 * user's effective ID (or that the user's effective ID be root) and the
 * local .exrc files to be owned by the user's effective ID.  In all cases,
 * the file cannot be writeable by anyone other than its owner.
 *
 * In O'Reilly ("Learning the VI Editor", Fifth Ed., May 1992, page 106),
 * it notes that System V release 3.2 and later has an option "[no]exrc".
 * The behavior is that local .exrc files are read only if the exrc option
 * is set.  The default for the exrc option was off, so, by default, local
 * .exrc files were not read.  The problem this was intended to solve was
 * that System V permitted users to give away files, so there's no possible
 * ownership or writeability test to ensure that the file is safe.
 *
 * POSIX 1003.2-1992 standardized exrc as an option.  It required the exrc
 * option to be off by default, thus local .exrc files are not to be read
 * by default.  The Rationale noted (incorrectly) that this was a change
 * to historic practice, but correctly noted that a default of off improves
 * system security.  POSIX also required that vi check the effective user
 * ID instead of the real user ID, which is why we've switched from historic
 * practice.
 *
 * We initialize the exrc variable to off.  If it's turned on by the system
 * or $HOME .exrc files, and the local .exrc file passes the ownership and
 * writeability tests, then we read it.  This breaks historic 4BSD practice,
 * but it gives us a measure of security on systems where users can give away
 * files.
 */
static enum rc
exrc_isok(sp, sbp, path, rootown, rootid)
	SCR *sp;
	struct stat *sbp;
	char *path;
	int rootown, rootid;
{
	enum { ROOTOWN, OWN, WRITER } etype;
	uid_t euid;
	int nf1, nf2;
	char *a, *b, buf[MAXPATHLEN];

	/* Check for the file's existence. */
	if (stat(path, sbp))
		return (NOEXIST);

	/* Check ownership permissions. */
	euid = geteuid();
	if (!(rootown && sbp->st_uid == 0) &&
	    !(rootid && euid == 0) && sbp->st_uid != euid) {
		etype = rootown ? ROOTOWN : OWN;
		goto denied;
	}

	/* Check writeability. */
	if (sbp->st_mode & (S_IWGRP | S_IWOTH)) {
		etype = WRITER;
		goto denied;
	}
	return (RCOK);

denied:	a = msg_print(sp, path, &nf1);
	if (strchr(path, '/') == NULL && getcwd(buf, sizeof(buf)) != NULL) {
		b = msg_print(sp, buf, &nf2);
		switch (etype) {
		case ROOTOWN:
			msgq(sp, M_ERR,
			    "043|%s/%s: not sourced: not owned by you or root",
			    b, a);
			break;
		case OWN:
			msgq(sp, M_ERR,
			    "044|%s/%s: not sourced: not owned by you", b, a);
			break;
		case WRITER:
			msgq(sp, M_ERR,
    "045|%s/%s: not sourced: writeable by a user other than the owner", b, a);
			break;
		}
		if (nf2)
			FREE_SPACE(sp, b, 0);
	} else
		switch (etype) {
		case ROOTOWN:
			msgq(sp, M_ERR,
			    "046|%s: not sourced: not owned by you or root", a);
			break;
		case OWN:
			msgq(sp, M_ERR,
			    "047|%s: not sourced: not owned by you", a);
			break;
		case WRITER:
			msgq(sp, M_ERR,
	    "048|%s: not sourced: writeable by a user other than the owner", a);
			break;
		}

	if (nf1)
		FREE_SPACE(sp, a, 0);
	return (NOPERM);
}

/*
 * sex_screen_init --
 *	Initialize the ex screen.
 */
static void
sex_screen_init(sp)
	SCR *sp;
{
	/* Initialize support routines. */
	sp->s_bell		= sex_bell;
	sp->s_bg		= (int (*)())sex_nope;
	sp->s_busy		= (int (*)())sex_busy;
	sp->s_change		= (int (*)())sex_noop;
	sp->s_clear		= (int (*)())sex_noop;
	sp->s_colpos		= (size_t (*)())sex_abort;
	sp->s_column		= (int (*)())sex_abort;
	sp->s_confirm		= sex_confirm;
	sp->s_crel		= (int (*)())sex_nope;
	sp->s_edit		= sex_screen_edit;
	sp->s_end		= (int (*)())sex_noop;
	sp->s_ex_cmd		= (int (*)())sex_abort;
	sp->s_ex_run		= (int (*)())sex_abort;
	sp->s_ex_write		= (int (*)())sex_abort;
	sp->s_fg		= (int (*)())sex_nope;
	sp->s_fill		= (int (*)())sex_abort;
	sp->s_get		= sex_get;
	sp->s_key_read		= sex_key_read;
	sp->s_fmap		= (int (*)())sex_noop;
	sp->s_position		= (int (*)())sex_abort;
	sp->s_rabs		= (int (*)())sex_nope;
	sp->s_rcm		= (size_t (*)())sex_abort;
	sp->s_refresh		= sex_refresh;
	sp->s_scroll		= (int (*)())sex_abort;
	sp->s_split		= (int (*)())sex_nope;
	sp->s_suspend		= sex_suspend;
}

/*
 * sex_abort --
 *	Fake function.  Die.
 */
static void
sex_abort()
{
	abort();
}

/*
 * sex_noop --
 *	Fake function.  Do nothing.
 */
static int
sex_noop()
{
	return (0);
}

/*
 * sex_nope --
 *	Fake function.  Not in ex, you don't.
 */
static int
sex_nope(sp)
	SCR *sp;
{
	msgq(sp, M_ERR, "095|Command not available in ex mode");
	return (1);
}
