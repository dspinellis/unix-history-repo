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
static char copyright[] =
"@(#) Copyright (c) 1992, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>

#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include "vi.h"
#include "excmd.h"
#include "recover.h"
#include "pathnames.h"
#include "tag.h"

static void msgflush __P((GS *));
static void obsolete __P((char *[]));
static void reset __P((GS *));
static void usage __P((void));

GS *__global_list;			/* GLOBAL: List of screens. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern int optind;
	extern char *optarg;
	static int reenter;		/* STATIC: Re-entrancy check. */
	EXCMDARG cmd;
	GS *gp;
	SCR *sp;
	int ch, flagchk, eval;
	char *excmdarg, *errf, *myname, *p, *rfname, *tag;

	/* Stop if indirecting through a NULL pointer. */
	if (reenter++)
		abort();

	eval = 0;

	/* Figure out the program name. */
	if ((myname = strrchr(*argv, '/')) == NULL)
		myname = *argv;
	else
		++myname;

	/* Build and initialize the GS structure. */
	if ((gp = malloc(sizeof(GS))) == NULL)
		err(1, NULL);
	__global_list = gp;
	memset(gp, 0, sizeof(GS));
	HDR_INIT(gp->scrhdr, next, prev);
	HDR_INIT(gp->exfhdr, next, prev);
	if (tcgetattr(STDIN_FILENO, &gp->original_termios))
		err(1, "tcgetattr");
		
	/* Build and initialize the first/current screen. */
	if ((sp = malloc(sizeof(SCR))) == NULL)
		err(1, NULL);
	if (scr_init(NULL, sp))
		err(1, NULL);
	sp->gp = gp;		/* All screens point to the GS structure. */
	HDR_INIT(sp->seqhdr, next, prev);
				/* No need to block SIGALRM yet. */
	HDR_APPEND(sp, &gp->scrhdr, next, prev, SCR);

	if (set_window_size(sp, 0))	/* Set the window size. */
		goto err1;

	if (opts_init(sp))		/* Options initialization. */
		goto err1;

	/*
	 * Keymaps, special keys.
	 * Must follow options and sequence map initializations.
	 */
	if (term_init(sp))
		goto err1;

#ifndef NO_DIGRAPH
	if (digraph_init(sp))		/* Digraph initialization. */
		goto err1;
#endif
	/* Set screen mode based on the program name. */
	if (!strcmp(myname, "ex") || !strcmp(myname, "nex"))
		F_SET(sp, S_MODE_EX);
	else if (!strcmp(myname, "view")) {
		O_SET(sp, O_READONLY);
		F_SET(sp, S_MODE_VI);
	} else
		F_SET(sp, S_MODE_VI);

	/* Convert old-style arguments into new-style ones. */
	obsolete(argv);

	/* Parse the arguments. */
	flagchk = 0;
	F_SET(gp, G_SNAPSHOT);
	excmdarg = errf = rfname = tag = NULL;
	while ((ch = getopt(argc, argv, "c:elmRr:sT:t:v")) != EOF)
		switch(ch) {
		case 'c':		/* Run the command. */
			excmdarg = optarg;
			break;
		case 'e':		/* Ex mode. */
			F_SET(sp, S_MODE_EX);
			break;
		case 'l':
			exit(rcv_list());
#ifndef NO_ERRLIST
		case 'm':		/* Error list. */
			++flagchk;
			errf = optarg;
			break;
#endif
		case 'R':		/* Readonly. */
			O_SET(sp,O_READONLY);
			break;
		case 'r':		/* Recover. */
			++flagchk;
			rfname = optarg;
			break;
		case 's':		/* No snapshot. */
			F_CLR(gp, G_SNAPSHOT);
			break;
#ifdef DEBUG
		case 'T':		/* Trace. */
			if ((gp->tracefp = fopen(optarg, "w")) == NULL)
				err(1, "%s", optarg);
			(void)fprintf(gp->tracefp,
			    "\n===\ntrace: open %s\n", optarg);
			break;
#endif
		case 't':		/* Tag. */
			++flagchk;
			tag = optarg;
			break;
		case 'v':		/* Vi mode. */
			F_SET(sp, S_MODE_VI);
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/* Only permit one file specification. */
	if (flagchk > 1)
		err(1, "only one of -r and -t is permitted.");

	/*
	 * Source the system, environment, ~user and local .exrc values.
	 * If the environment exists, vi historically doesn't check ~user.
	 */
	(void)ex_cfile(sp, NULL, _PATH_SYSEXRC, 0);

	/* Source the EXINIT environment variable. */
	if ((p = getenv("EXINIT")) != NULL)
		if ((p = strdup(p)) == NULL) {
			msgq(sp, M_ERR, "Error: %s", strerror(errno));
			goto err1;
		} else {
			(void)ex_cstring(sp, NULL, p, strlen(p));
			free(p);
		}
	else if ((p = getenv("HOME")) != NULL && *p) {
		char path[MAXPATHLEN];

		(void)snprintf(path, sizeof(path), "%s/.exrc", p);
		(void)ex_cfile(sp, NULL, path, 0);
	}
	if (O_ISSET(sp, O_EXRC))
		(void)ex_cfile(sp, NULL, _PATH_EXRC, 0);

	/* Any remaining arguments are file names. */
	if (argc)
		file_set(sp, argc, argv);

	/* Use an error list file, recovery file or tag file if specified. */
#ifndef NO_ERRLIST
	if (errf != NULL) {
		SETCMDARG(cmd, C_ERRLIST, 0, OOBLNO, 0, 0, errf);
		if (ex_errlist(sp, NULL, &cmd))
			goto err1;
	} else
#endif
	if (tag != NULL) {
		if (ex_tagfirst(sp, tag))
			goto err1;
	} else if (rfname != NULL) {
		if ((sp->ep = rcv_read(sp, rfname)) == NULL)
			goto err1;
	} else if ((sp->ep = file_start(sp, file_first(sp, 1), NULL)) == NULL)
		goto err1;

	/* Do commands from the command line. */
	if (excmdarg != NULL)
		(void)ex_cstring(sp, sp->ep, excmdarg, strlen(excmdarg));

	/*
	 * The commands we executed probably positioned the file.  This
	 * isn't a wonderful test, but it's probably fairly safe.
	 */
	if ((sp->lno != 1 || sp->cno != 0) && F_ISSET(sp->ep, F_NOSETPOS))
		F_CLR(sp->ep, F_NOSETPOS);

	/* Call a screen. */
	while (sp != NULL)
		switch(F_ISSET(sp, S_MODE_EX | S_MODE_VI)) {
		case S_MODE_EX:
			if (sex(sp, sp->ep, &sp))
				goto err2;
			break;
		case S_MODE_VI:
			if (svi(sp, sp->ep, &sp))
				goto err2;
			break;
		default:
			abort();
		}

	/*
	 * Two error paths.  The first means that something failed before
	 * we called a screen routine.  Swap the message pointers between
	 * the SCR and the GS, so messages get displayed.  The second is
	 * something failed in a screen.  NOTE: sp may be GONE when the
	 * screen returns, so only the gp can be trusted.
	 */
	if (0) {
err1:		gp->msgp = sp->msgp;
err2:		eval = 1;
	}

	/* Turn off the recovery timer. */
	rcv_end();

	/* Reset anything that needs resetting. */
	reset(gp);

	/* Flush any left-over error messages. */
	msgflush(gp);

	if (tcsetattr(STDIN_FILENO, TCSADRAIN, &gp->original_termios))
		err(1, "tcsetattr");
	exit(eval);
}

/*
 * reset --
 *	Reset any changed global state.
 */
static void
reset(gp)
	GS *gp;
{
	char *tty;

	if (gp->flags & G_SETMODE)			/* O_MESG */
		if ((tty = ttyname(STDERR_FILENO)) == NULL)
			warn("ttyname");
		else if (chmod(tty, gp->origmode) < 0)
			warn("%s", tty);
}

/*
 * msgflush --
 *	Flush any remaining messages.
 */
static void
msgflush(gp)
	GS *gp;
{
	MSG *mp;

	/* Ring the bell. */
	if (F_ISSET(gp, S_BELLSCHED))
		(void)fprintf(stderr, "\07");		/* \a */

	/* Display the messages. */
	for (mp = gp->msgp;
	    mp != NULL && !(F_ISSET(mp, M_EMPTY)); mp = mp->next) 
		(void)fprintf(stderr, "%.*s\n", (int)mp->len, mp->mbuf);
}

static void
obsolete(argv)
	char *argv[];
{
	size_t len;
	char *p, *myname;

	/*
	 * Translate old style arguments into something getopt will like.
	 * Make sure it's not text space memory, because ex changes the
	 * strings.
	 *	Change "+/command" into "-ccommand".
	 *	Change "+" and "+$" into "-c$".
	 *	Change "+[0-9]*" into "-c[0-9]".
	 *	Change "-r" into "-l"
	 */
	for (myname = argv[0]; *++argv;)
		if (argv[0][0] == '+') {
			if (argv[0][1] == '\0' || argv[0][1] == '$') {
				if ((argv[0] = malloc(4)) == NULL)
					err(1, NULL);
				memmove(argv[0], "-c$", 4);
			} else if (argv[0][1] == '/') {
				p = argv[0];
				len = strlen(argv[0]);
				if ((argv[0] = malloc(len + 3)) == NULL)
					err(1, NULL);
				argv[0][0] = '-';
				argv[0][1] = 'c';
				(void)strcpy(argv[0] + 2, p + 1);
			} else if (isdigit(argv[0][1])) {
				p = argv[0];
				len = strlen(argv[0]);
				if ((argv[0] = malloc(len + 2)) == NULL)
					err(1, NULL);
				argv[0][0] = '-';
				argv[0][1] = 'c';
				(void)strcpy(argv[0] + 2, p + 1);
			}
		}
		else if (argv[0][0] == '-' &&
		    argv[0][1] == 'r' && argv[1] == NULL)
			argv[0][1] = 'l';
}

static void
usage()
{
	(void)fprintf(stderr,
	    "usage: vi [-eRrsv] [-c command] [-m file] [-t tag]\n");
	exit(1);
}
