/*-
 * Copyright (c) 1992, 1993, 1994
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
static char copyright[] =
"@(#) Copyright (c) 1992, 1993, 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	9.9 (Berkeley) 12/3/94";
#endif /* not lint */

#include <sys/param.h>
#include <sys/queue.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <bitstring.h>
#include <ctype.h>
#include <err.h>
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
#include <pathnames.h>

#include "vi.h"
#include "excmd.h"
#include "../ex/tag.h"
#include "../sex/sex_screen.h"

static void	 gs_end __P((GS *));
static GS	*gs_init __P((void));
static void	 obsolete __P((char *[]));
static void	 usage __P((int));

GS *__global_list;			/* GLOBAL: List of screens. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern int optind;
	extern char *optarg;
	static int reenter;		/* STATIC: Re-entrancy check. */
	GS *gp;
	FREF *frp;
	SCR *sp;
	u_int flags, saved_vi_mode;
	int ch, eval, flagchk, lflag, need_lreset, readonly, silent, snapshot;
	char *excmdarg, *myname, *p, *tag_f, *trace_f, *wsizearg;
	char path[MAXPATHLEN];

	/* If loaded at 0 and indirecting through a NULL pointer, stop. */
	if (reenter++)
		abort();

#ifdef GDBATTACH
	(void)printf("%u waiting...\n", getpid());
	(void)read(0, &eval, 1);
#endif

	/* Set screen type and mode based on the program name. */
	readonly = 0;
	if ((myname = strrchr(*argv, '/')) == NULL)
		myname = *argv;
	else
		++myname;
	if (!strcmp(myname, "ex") || !strcmp(myname, "nex"))
		LF_INIT(S_EX);
	else {
		/* View is readonly. */
		if (!strcmp(myname, "nview") || !strcmp(myname, "view"))
			readonly = 1;
		LF_INIT(S_VI_CURSES);
	}
	saved_vi_mode = S_VI_CURSES;

	/* Convert old-style arguments into new-style ones. */
	obsolete(argv);

	/* Parse the arguments. */
	flagchk = '\0';
	excmdarg = tag_f = trace_f = wsizearg = NULL;
	lflag = silent = 0;
	snapshot = 1;
	while ((ch = getopt(argc, argv, "c:eFlRrsT:t:vw:X:")) != EOF)
		switch (ch) {
		case 'c':		/* Run the command. */
			excmdarg = optarg;
			break;
		case 'e':		/* Ex mode. */
			LF_CLR(S_SCREENS);
			LF_SET(S_EX);
			break;
		case 'F':		/* No snapshot. */
			snapshot = 0;
			break;
		case 'l':		/* Set lisp, showmatch options. */
			lflag = 1;
			break;
		case 'R':		/* Readonly. */
			readonly = 1;
			break;
		case 'r':		/* Recover. */
			if (flagchk == 't')
				errx(1,
				    "only one of -r and -t may be specified.");
			flagchk = 'r';
			break;
		case 's':
			silent = 1;
			break;
		case 'T':		/* Trace. */
			trace_f = optarg;
			break;
		case 't':		/* Tag. */
			if (flagchk == 'r')
				errx(1,
				    "only one of -r and -t may be specified.");
			if (flagchk == 't')
				errx(1,
				    "only one tag file may be specified.");
			flagchk = 't';
			tag_f = optarg;
			break;
		case 'v':		/* Vi mode. */
			LF_CLR(S_SCREENS);
			LF_SET(S_VI_CURSES);
			break;
		case 'w':
			wsizearg = optarg;
			break;
		case 'X':
			if (!strcmp(optarg, "aw")) {
				LF_CLR(S_SCREENS);
				LF_SET(S_VI_XAW);
				saved_vi_mode = S_VI_XAW;
				break;
			}
			/* FALLTHROUGH */
		case '?':
		default:
			usage(LF_ISSET(S_EX));
		}
	argc -= optind;
	argv += optind;

	/* Build and initialize the GS structure. */
	__global_list = gp = gs_init();

	/* Set the file snapshot flag. */
	if (snapshot)
		F_SET(gp, G_SNAPSHOT);

	/* Silent is only applicable to ex. */
	if (silent && !LF_ISSET(S_EX))
		errx(1, "-s only applicable to ex.");

	/*
	 * If not reading from a terminal, it's like -s was specified to
	 * ex.  Vi always reads from (and writes to) a terminal unless
	 * we're just listing recover files, so fail if it's not a terminal.
	 */
	if (LF_ISSET(S_EX)) {
		if (!F_ISSET(gp, G_STDIN_TTY))
			silent = 1;
	} else if (flagchk != 'r' &&
	    (!F_ISSET(gp, G_STDIN_TTY) || !isatty(STDOUT_FILENO))) {
		msgq(NULL, M_ERR,
		    "040|Vi's standard input and output must be a terminal");
		goto errexit;
	}

	if (trace_f != NULL) {		/* Trace file initialization. */
#ifdef DEBUG
		if ((gp->tracefp = fopen(trace_f, "w")) == NULL)
			err(1, "%s", trace_f);
		(void)fprintf(gp->tracefp, "\n===\ntrace: open %s\n", trace_f);
#else
		msgq(sp, M_ERR,
		    "041|-T support not compiled into this version");
#endif
	}

	/*
	 * Build and initialize the first/current screen.  This is a bit
	 * tricky.  If an error is returned, we may or may not have a
	 * screen structure.  If we have a screen structure, put it on a
	 * display queue so that the error messages get displayed.
	 *
	 * !!!
	 * Signals not turned on, don't block them for queue manipulation.
	 *
	 * !!!
	 * Everything we do until we go interactive is done in ex mode.
	 */
	if (screen_init(NULL, &sp)) {
		if (sp != NULL)
			CIRCLEQ_INSERT_HEAD(&__global_list->dq, sp, q);
		goto errexit;
	}
	F_SET(sp, S_EX);
	sp->saved_vi_mode = saved_vi_mode;
	CIRCLEQ_INSERT_HEAD(&__global_list->dq, sp, q);

	if (term_init(sp))		/* Terminal initialization. */
		goto errexit;
	if (term_window(sp, 0))		/* Screen size initialization. */
		goto errexit;

	{ int oargs[4], *oargp = oargs;
	if (readonly)			/* Command-line options. */
		*oargp++ = O_READONLY;
	if (lflag) {
		*oargp++ = O_LISP;
		*oargp++ = O_SHOWMATCH;
	}
	*oargp = -1;
	if (opts_init(sp, oargs))	/* Options initialization. */
		goto errexit;
	}

	if (silent) {			/* Ex batch mode. */
		O_CLR(sp, O_AUTOPRINT);
		O_CLR(sp, O_PROMPT);
		O_CLR(sp, O_VERBOSE);
		O_CLR(sp, O_WARN);
		F_SET(sp, S_EXSILENT);
	}
	if (wsizearg != NULL) {
		ARGS *av[2], a, b;
		errno = 0;
		if (strtol(wsizearg, &p, 10) < 0 || errno || *p)
			errx(1, "illegal window size -- %s.", wsizearg);
		(void)snprintf(path, sizeof(path), "window=%s", wsizearg);
		a.bp = (CHAR_T *)path;
		a.len = strlen(path);
		b.bp = NULL;
		b.len = 0;
		av[0] = &a;
		av[1] = &b;
		if (opts_set(sp, av, 0, NULL))
			 msgq(sp, M_ERR,
		     "042|Unable to set command line window size option");
	}

#ifdef DIGRAPHS
	if (digraph_init(sp))		/* Digraph initialization. */
		goto errexit;
#endif

	if (sig_init(sp))		/* Signal initialization. */
		goto errexit;

	if (!silent) {			/* Read EXINIT, exrc files. */
		if (sex_screen_exrc(sp))
			goto errexit;
		if (F_ISSET(sp, S_EXIT | S_EXIT_FORCE))
			goto done;
	}

	/*
	 * List recovery files if -r specified without file arguments.
	 * Note, options must be initialized and startup information
	 * read before doing this.
	 */
	if (flagchk == 'r' && argv[0] == NULL) {
		if (rcv_list(sp))
			goto errexit;
		goto done;
	}

	/*
	 * !!!
	 * Initialize the default ^D, ^U scrolling value here, after the
	 * user has had every opportunity to set the window option.
	 *
	 * It's historic practice that changing the value of the window
	 * option did not alter the default scrolling value, only giving
	 * a count to ^D/^U did that.
	 */
	sp->defscroll = (O_VAL(sp, O_WINDOW) + 1) / 2;

	/* Use a tag file if specified. */
	if (tag_f != NULL) {
		if (ex_tagfirst(sp, tag_f))
			goto errexit;
		need_lreset = 0;
	} else
		need_lreset = 1;

	/*
	 * Append any remaining arguments as file names.  Files are recovery
	 * files if -r specified.  If the tag option or ex startup commands
	 * loaded a file, then any file arguments are going to come after it.
	 */
	if (*argv != NULL) {
		if (sp->frp != NULL) {
			MALLOC_NOMSG(sp,
			    *--argv, char *, strlen(sp->frp->name) + 1);
			if (*argv == NULL)
				err(1, NULL);
			(void)strcpy(*argv, sp->frp->name);
		}
		sp->argv = sp->cargv = argv;
		F_SET(sp, S_ARGNOFREE);
		if (flagchk == 'r')
			F_SET(sp, S_ARGRECOVER);
	}

	/*
	 * If the ex startup commands and or/the tag option haven't already
	 * created a file, create one.  If no files as arguments, use a
	 * temporary file.
	 */
	if (sp->frp == NULL && tag_f == NULL) {
		if ((frp = file_add(sp,
		    sp->argv == NULL ? NULL : (CHAR_T *)(sp->argv[0]))) == NULL)
			goto errexit;
		if (F_ISSET(sp, S_ARGRECOVER))
			F_SET(frp, FR_RECOVER);
		if (file_init(sp, frp, NULL, 0))
			goto errexit;
		need_lreset = 1;
	}

	/*
	 * If there's an initial command, it was always executed from the
	 * last line of the file by default.  So, if we haven't already
	 * gotten an address in the file, move to the last line.  This
	 * happens before the screen type gets set, so that we initialize
	 * for ex mode and not for vi mode.
	 */
	if (excmdarg != NULL && need_lreset) {
		file_cinit(sp);
		need_lreset = 0;
	}

	/*
	 * Set the initial screen type.  The user may have tried to set
	 * it themselves in the startup information, but that's too bad
	 * -- they called us with a specific name, and that applies now.
	 */
	F_CLR(sp, S_SCREENS);
	F_SET(sp, LF_ISSET(S_SCREENS));

	/*
	 * If there's an initial command, execute it.  Historically, it
	 * was always an ex command, not vi in vi mode and ex in ex mode.
	 * The line value has already been set.  Note: the screen type
	 * has also been set -- any "default" commands will change the
	 * line number, not print the line.
	 */
	if (excmdarg != NULL) {
		if (sex_screen_icmd(sp, excmdarg))
			goto errexit;
		if (F_ISSET(sp, S_EXIT | S_EXIT_FORCE))
			goto done;
	}

	if (need_lreset)
		file_cinit(sp);

	for (;;) {
		/* Edit, ignoring errors -- other screens may succeed. */
		switch (F_ISSET(sp, S_SCREENS)) {
		case S_EX:
			(void)sex_screen_edit(sp);
			break;
		case S_VI_CURSES:
			(void)svi_screen_edit(sp);
			break;
		case S_VI_XAW:
			(void)xaw_screen_edit(sp);
			break;
		default:
			abort();
		}

		/*
		 * Edit the next screen on the display queue, or, move
		 * a screen from the hidden queue to the display queue.
		 */
		if ((sp = __global_list->dq.cqh_first) ==
		    (void *)&__global_list->dq)
			if ((sp = __global_list->hq.cqh_first) !=
			    (void *)&__global_list->hq) {
				SIGBLOCK(__global_list);
				CIRCLEQ_REMOVE(&sp->gp->hq, sp, q);
				CIRCLEQ_INSERT_TAIL(&sp->gp->dq, sp, q);
				SIGUNBLOCK(__global_list);
			} else
				break;
	}

done:	eval = 0;
	if (0)
errexit:	eval = 1;

	/*
	 * NOTE: sp may be GONE when the screen returns, so only
	 * the gp can be trusted.
	 */
	gs_end(gp);

	exit(eval);
}

/*
 * gs_init --
 *	Build and initialize the GS structure.
 */
static GS *
gs_init()
{
	GS *gp;
	int fd;

	CALLOC_NOMSG(NULL, gp, GS *, 1, sizeof(GS));
	if (gp == NULL)
		err(1, NULL);

	/*
	 * !!!
	 * Signals not on, no need to block them for queue manipulation.
	 */
	CIRCLEQ_INIT(&gp->dq);
	CIRCLEQ_INIT(&gp->hq);
	LIST_INIT(&gp->msgq);

	/* Structures shared by screens so stored in the GS structure. */
	CIRCLEQ_INIT(&gp->dcb_store.textq);
	LIST_INIT(&gp->cutq);
	LIST_INIT(&gp->seqq);

	/* Set a flag if we're reading from the tty. */
	if (isatty(STDIN_FILENO))
		F_SET(gp, G_STDIN_TTY);

	/*
	 * Set the G_STDIN_TTY flag.  It's purpose is to avoid setting and
	 * resetting the tty if the input isn't from there.
	 *
	 * Set the G_TERMIOS_SET flag.  It's purpose is to avoid using the
	 * original_termios information (mostly special character values)
	 * if it's not valid.  We expect that if we've lost our controlling
	 * terminal that the open() (but not the tcgetattr()) will fail.
	 */
	if (F_ISSET(gp, G_STDIN_TTY)) {
		if (tcgetattr(STDIN_FILENO, &gp->original_termios) == -1)
			err(1, "tcgetattr");
		F_SET(gp, G_TERMIOS_SET);
	} else if ((fd = open(_PATH_TTY, O_RDONLY, 0)) != -1) {
		if (tcgetattr(fd, &gp->original_termios) == -1)
			err(1, "tcgetattr");
		F_SET(gp, G_TERMIOS_SET);
		(void)close(fd);
	}
	return (gp);
}


/*
 * gs_end --
 *	End the GS structure.
 */
static void
gs_end(gp)
	GS *gp;
{
	MSG *mp;
	SCR *sp;
	char *tty;

	/* Default buffer storage. */
	(void)text_lfree(&gp->dcb_store.textq);

	/* Reset anything that needs resetting. */
	if (gp->flags & G_SETMODE)			/* O_MESG */
		if ((tty = ttyname(STDERR_FILENO)) == NULL)
			warn("ttyname");
		else if (chmod(tty, gp->origmode) < 0)
			warn("%s", tty);

	/* Close message catalogs. */
	msg_close(gp);

	/* Ring the bell if scheduled. */
	if (F_ISSET(gp, G_BELLSCHED))
		(void)fprintf(stderr, "\07");		/* \a */

	/* If there are any remaining screens, flush their messages. */
	for (sp = __global_list->dq.cqh_first;
	    sp != (void *)&__global_list->dq; sp = sp->q.cqe_next)
		for (mp = sp->msgq.lh_first;
		    mp != NULL && !(F_ISSET(mp, M_EMPTY)); mp = mp->q.le_next)
			(void)fprintf(stderr,
			    "%.*s.\n", (int)mp->len, mp->mbuf);
	for (sp = __global_list->hq.cqh_first;
	    sp != (void *)&__global_list->hq; sp = sp->q.cqe_next)
		for (mp = sp->msgq.lh_first;
		    mp != NULL && !(F_ISSET(mp, M_EMPTY)); mp = mp->q.le_next)
			(void)fprintf(stderr,
			    "%.*s.\n", (int)mp->len, mp->mbuf);
	/* Flush messages on the global queue. */
	for (mp = gp->msgq.lh_first;
	    mp != NULL && !(F_ISSET(mp, M_EMPTY)); mp = mp->q.le_next)
		(void)fprintf(stderr, "%.*s.\n", (int)mp->len, mp->mbuf);

	/*
	 * DON'T FREE THE GLOBAL STRUCTURE -- WE DIDN'T TURN
	 * OFF SIGNALS/TIMERS, SO IT MAY STILL BE REFERENCED.
	 */
}

static void
obsolete(argv)
	char *argv[];
{
	size_t len;
	char *p;

	/*
	 * Translate old style arguments into something getopt will like.
	 * Make sure it's not text space memory, because ex modifies the
	 * strings.
	 *	Change "+" into "-c$".
	 *	Change "+<anything else>" into "-c<anything else>".
	 *	Change "-" into "-s"
	 *	The c, T, t, w and X options take arguments, don't allow
	 *	    them to be special arguments.
	 */
	while (*++argv)
		if (argv[0][0] == '+') {
			if (argv[0][1] == '\0') {
				MALLOC_NOMSG(NULL, argv[0], char *, 4);
				if (argv[0] == NULL)
					err(1, NULL);
				(void)strcpy(argv[0], "-c$");
			} else  {
				p = argv[0];
				len = strlen(argv[0]);
				MALLOC_NOMSG(NULL, argv[0], char *, len + 2);
				if (argv[0] == NULL)
					err(1, NULL);
				argv[0][0] = '-';
				argv[0][1] = 'c';
				(void)strcpy(argv[0] + 2, p + 1);
			}
		} else if (argv[0][0] == '-')
			if (argv[0][1] == '\0') {
				MALLOC_NOMSG(NULL, argv[0], char *, 3);
				if (argv[0] == NULL)
					err(1, NULL);
				(void)strcpy(argv[0], "-s");
			} else if ((argv[0][1] == 'c' ||
			    argv[0][1] == 'T' || argv[0][1] == 't' ||
			    argv[0][1] == 'w' || argv[0][1] == 'X') &&
			    argv[0][2] == '\0')
				++argv;
}

static void
usage(is_ex)
	int is_ex;
{
#define	EX_USAGE \
    "ex [-eFRrsv] [-c command] [-t tag] [-w size] [files ...]"
#define	VI_USAGE \
    "vi [-eFlRrv] [-c command] [-t tag] [-w size] [files ...]"

	(void)fprintf(stderr, "usage: %s\n", is_ex ? EX_USAGE : VI_USAGE);
	exit(1);
}
