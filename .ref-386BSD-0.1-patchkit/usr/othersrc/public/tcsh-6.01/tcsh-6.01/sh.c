/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/sh.c,v 3.20 1991/12/19 22:34:14 christos Exp $ */
/*
 * sh.c: Main shell routines
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
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
#define EXTERN	/* Intern */
#include "sh.h"

#ifndef lint
char    copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif				/* not lint */

RCSID("$Id: sh.c,v 3.20 1991/12/19 22:34:14 christos Exp $")

#include "tc.h"
#include "ed.h"

extern bool MapsAreInited;
extern bool NLSMapsAreInited;
extern bool NoNLSRebind;

/*
 * C Shell
 *
 * Bill Joy, UC Berkeley, California, USA
 * October 1978, May 1980
 *
 * Jim Kulp, IIASA, Laxenburg, Austria
 * April 1980
 *
 * Filename recognition added:
 * Ken Greer, Ind. Consultant, Palo Alto CA
 * October 1983.
 *
 * Karl Kleinpaste, Computer Consoles, Inc.
 * Added precmd, periodic/tperiod, prompt changes,
 * directory stack hack, and login watch.
 * Sometime March 1983 - Feb 1984.
 *
 * Added scheduled commands, including the "sched" command,
 * plus the call to sched_run near the precmd et al
 * routines.
 * Upgraded scheduled events for running events while
 * sitting idle at command input.
 *
 * Paul Placeway, Ohio State
 * added stuff for running with twenex/inputl  9 Oct 1984.
 *
 * ported to Apple Unix (TM) (OREO)  26 -- 29 Jun 1987
 */

jmp_buf reslab;

#ifdef TESLA
int do_logout;
#endif				/* TESLA */

Char   *dumphist[] = {STRhistory, STRmh, 0, 0};
Char   *loadhist[] = {STRsource, STRmh, STRtildothist, 0};

#ifdef CSHDIRS
Char   *loaddirs[] = {STRsource, STRdirfile, 0};
bool    dflag = 0;
#endif

#if defined(convex) || defined(__convex__)
bool    use_fork = 0;		/* use fork() instead of vfork()? */
#endif

int     nofile = 0;
bool    reenter = 0;
bool    nverbose = 0;
bool    nexececho = 0;
bool    quitit = 0;
bool    fast = 0;
bool    batch = 0;
bool    mflag = 0;
bool    prompt = 1;
bool    enterhist = 0;
bool    tellwhat = 0;
time_t  t_period;

extern char **environ;

static	int		  srccat	__P((Char *, Char *));
static	int		  srcfile	__P((char *, bool, bool));
static	sigret_t	  phup		__P((int));
static	void		  srcunit	__P((int, bool, bool));
static	void		  mailchk	__P((void));
static	Char	 	**defaultpath	__P((void));

int
main(argc, argv)
    int     argc;
    char  **argv;
{
    register Char *cp;
#ifdef AUTOLOGOUT
    register Char *cp2;
#endif
    register char *tcp, *ttyn;
    register int f;
    register char **tempv;

#ifdef BSDSIGS
    sigvec_t osv;
#endif				/* BSDSIGS */

    settimes();			/* Immed. estab. timing base */
#ifdef TESLA
    do_logout = 0;
#endif				/* TESLA */

    osinit();			/* Os dependent initialization */

    /*
     * Initialize non constant strings
     */
#ifdef _PATH_BSHELL
    STR_BSHELL = SAVE(_PATH_BSHELL);
#endif
#ifdef _PATH_CSHELL
    STR_SHELLPATH = SAVE(_PATH_CSHELL);
#endif
#ifdef _PATH_TCSHELL
    STR_SHELLPATH = SAVE(_PATH_TCSHELL);
#endif
    STR_environ = blk2short(environ);
    environ = short2blk(STR_environ);	/* So that we can free it */
    STR_WORD_CHARS = SAVE(WORD_CHARS);

    HIST = '!';
    HISTSUB = '^';
    word_chars = STR_WORD_CHARS;
    bslash_quote = 0;		/* PWP: do tcsh-style backslash quoting? */

    tempv = argv;
    if (eq(str2short(tempv[0]), STRaout))	/* A.out's are quittable */
	quitit = 1;
    uid = getuid();
    gid = getgid();
    /*
     * We are a login shell if: 1. we were invoked as -<something> and we had
     * no arguments 2. or we were invoked only with the -l flag
     */
    loginsh = (**tempv == '-' && argc == 1) || (argc == 2 &&
				   tempv[1][0] == '-' && tempv[1][1] == 'l' &&
						tempv[1][2] == '\0');
    if (loginsh && **tempv != '-') {
	/*
	 * Mangle the argv space
	 */
	tempv[1][0] = '\0';
	tempv[1][1] = '\0';
	tempv[1] = NULL;
	for (tcp = *tempv; *tcp++;);
	for (tcp--; tcp >= *tempv; tcp--)
	    tcp[1] = tcp[0];
	*++tcp = '-';
	argc--;
    }
    if (loginsh)
	(void) time(&chktim);

    AsciiOnly = 1;
    NoNLSRebind = getenv("NOREBIND") != NULL;
#ifdef NLS
    (void) setlocale(LC_ALL, "");
    {
	int     k;

	for (k = 0200; k <= 0377 && !Isprint(k); k++);
	AsciiOnly = k > 0377;
    }
#else
    AsciiOnly = getenv("LANG") == NULL && getenv("LC_CTYPE") == NULL;
#endif				/* NLS */
    if (MapsAreInited && !NLSMapsAreInited)
	ed_InitNLSMaps();

    /*
     * Initialize for periodic command intervals. Also, initialize the dummy
     * tty list for login-watch.
     */
    (void) time(&t_period);
    initwatch();

    /*
     * Move the descriptors to safe places. The variable didfds is 0 while we
     * have only FSH* to work with. When didfds is true, we have 0,1,2 and
     * prefer to use these.
     */
    initdesc();

    /*
     * Get and set the tty now
     */
    if (ttyn = ttyname(SHIN)) {
	/*
	 * Could use rindex to get rid of other possible path components, but
	 * hpux preserves the subdirectory /pty/ when storing the tty name in
	 * utmp, so we keep it too.
	 */
	if (strncmp(ttyn, "/dev/", 5) == 0)
	    set(STRtty, cp = SAVE(ttyn + 5));
	else
	    set(STRtty, cp = SAVE(ttyn));
    }
    else
	set(STRtty, cp = SAVE(""));
    /*
     * Initialize the shell variables. ARGV and PROMPT are initialized later.
     * STATUS is also munged in several places. CHILD is munged when
     * forking/waiting
     */

    /*
     * 7-10-87 Paul Placeway autologout should be set ONLY on login shells and
     * on shells running as root.  Out of these, autologout should NOT be set
     * for any psudo-terminals (this catches most window systems) and not for
     * any terminal running X windows.
     * 
     * At Ohio State, we have had problems with a user having his X session 
     * drop out from under him (on a Sun) because the shell in his master 
     * xterm timed out and exited.
     * 
     * Really, this should be done with a program external to the shell, that
     * watches for no activity (and NO running programs, such as dump) on a
     * terminal for a long peroid of time, and then SIGHUPS the shell on that
     * terminal.
     * 
     * bugfix by Rich Salz <rsalz@PINEAPPLE.BBN.COM>: For root rsh things 
     * allways first check to see if loginsh or really root, then do things 
     * with ttyname()
     * 
     * Also by Jean-Francois Lamy <lamy%ai.toronto.edu@RELAY.CS.NET>: check the
     * value of cp before using it! ("root can rsh too")
     * 
     * PWP: keep the nested ifs; the order of the tests matters and a good 
     * (smart) C compiler might re-arange things wrong.
     */
#ifdef AUTOLOGOUT
    if (loginsh || (uid == 0)) {
	if (*cp) {
	    /* only for login shells or root and we must have a tty */
	    if ((cp2 = Strrchr(cp, (Char) '/')) != NULL) {
		cp = cp2 + 1;
	    }
	    if (!((Strncmp(cp, STRtty, 3) == 0) &&
		  (cp[3] >= 'p' && cp[3] <= 'u'))) {
		if (getenv("DISPLAY") == NULL) {
		    /* NOT on X window shells */
		    set(STRautologout, Strsave(STRdefautologout));
		}
	    }
	}
    }
#endif				/* AUTOLOGOUT */

    (void) sigset(SIGALRM, alrmcatch);

    set(STRstatus, Strsave(STR0));
    fix_version();		/* publish the shell version */

    /*
     * increment the shell level.
     */
    shlvl(1);

    if ((tcp = getenv("HOME")) != NULL)
	cp = SAVE(tcp);
    else
	cp = NULL;
    if (cp == NULL)
	fast = 1;		/* No home -> can't read scripts */
    else
	set(STRhome, cp);
    dinit(cp);			/* dinit thinks that HOME == cwd in a login
				 * shell */
    /*
     * Grab other useful things from the environment. Should we grab
     * everything??
     */
    {
	char *cln, *cus;
	Char    buff[BUFSIZE];
	struct passwd *pw;


#ifdef apollo
	int     oid = getoid();

	Itoa(oid, buff);
	set(STRoid, Strsave(buff));
#endif /* apollo */

	Itoa(uid, buff);
	set(STRuid, Strsave(buff));

	Itoa(gid, buff);
	set(STRgid, Strsave(buff));

	cln = getenv("LOGNAME");
	cus = getenv("USER");
	if (cus != NULL)
	    set(STRuser, SAVE(cus));
	else if (cln != NULL)
	    set(STRuser, SAVE(cln));
	else if ((pw = getpwuid(uid)) == NULL)
	    set(STRuser, SAVE("unknown"));
	else
	    set(STRuser, SAVE(pw->pw_name));
	if (cln == NULL)
	    Setenv(STRLOGNAME, value(STRuser));
	if (cus == NULL)
	    Setenv(STRUSER, value(STRuser));
	    
    }

    /*
     * HOST may be wrong, since rexd transports the entire environment on sun
     * 3.x Just set it again
     */
    {
	char    cbuff[MAXHOSTNAMELEN];

	if (gethostname(cbuff, sizeof(cbuff)) >= 0) {
	    cbuff[sizeof(cbuff) - 1] = '\0';	/* just in case */
	    Setenv(STRHOST, str2short(cbuff));
	}
	else
	    Setenv(STRHOST, str2short("unknown"));
    }


    /*
     * HOSTTYPE, too. Just set it again.
     */
    Setenv(STRHOSTTYPE, gethosttype());
#ifdef apollo
    if ((tcp = getenv("SYSTYPE")) == NULL)
	tcp = "bsd4.3";
    Setenv(STRSYSTYPE, str2short(tcp));
#endif				/* apollo */

    /*
     * set editing on by default, unless running under Emacs as an inferior
     * shell.
     * We try to do this intelligently. If $TERM is available, then it
     * should determine if we should edit or not. $TERM is preserved
     * across rlogin sessions, so we will not get confused if we rlogin
     * under an emacs shell. Another advantage is that if we run an
     * xterm under an emacs shell, then the $TERM will be set to 
     * xterm, so we are going to want to edit. Unfortunately emacs
     * does not restore all the tty modes, so xterm is not very well
     * set up. But this is not the shell's fault.
     */
    if ((tcp = getenv("TERM")) != NULL) {
	set(STRterm, SAVE(tcp));
	editing = (strcmp(tcp, "emacs") != 0);
    }
    else 
	editing = ((tcp = getenv("EMACS")) == NULL || strcmp(tcp, "t") != 0);

    /* 
     * The 'edit' variable is either set or unset.  It doesn't 
     * need a value.  Making it 'emacs' might be confusing. 
     */
    if (editing)
	set(STRedit, Strsave(STRNULL));


    /*
     * still more mutability: make the complete routine automatically add the
     * suffix of file names...
     */
    set(STRaddsuffix, Strsave(STRNULL));

    /*
     * Re-initialize path if set in environment
     */
    if ((tcp = getenv("PATH")) == NULL)
	set1(STRpath, defaultpath(), &shvhed);
    else
	importpath(SAVE(tcp));

    set(STRshell, Strsave(STR_SHELLPATH));

    doldol = putn((int) getpid());	/* For $$ */
    shtemp = Strspl(STRtmpsh, doldol);	/* For << */

    /*
     * Record the interrupt states from the parent process. If the parent is
     * non-interruptible our hand must be forced or we (and our children) won't
     * be either. Our children inherit termination from our parent. We catch it
     * only if we are the login shell.
     */
#ifdef BSDSIGS
    /* parents interruptibility */
    (void) mysigvec(SIGINT, NULL, &osv);
    parintr = (sigret_t(*) ()) osv.sv_handler;
    (void) mysigvec(SIGTERM, NULL, &osv);
    parterm = (sigret_t(*) ()) osv.sv_handler;
#else				/* BSDSIGS */
    parintr = signal(SIGINT, SIG_IGN);	/* parents interruptibility */
    (void) sigset(SIGINT, parintr);	/* ... restore */
    parterm = signal(SIGTERM, SIG_IGN);	/* parents terminability */
    (void) sigset(SIGTERM, parterm);	/* ... restore */
#endif				/* BSDSIGS */

    if (loginsh) {
	(void) signal(SIGHUP, phup);	/* exit processing on HUP */
#ifdef SIGXCPU
	(void) signal(SIGXCPU, phup);	/* ...and on XCPU */
#endif				/* SIGXCPU */
#ifdef SIGXFSZ
	(void) signal(SIGXFSZ, phup);	/* ...and on XFSZ */
#endif				/* SIGXFSZ */
    }

#ifdef TCF
    /* Enable process migration on ourselves and our progeny */
    (void) signal(SIGMIGRATE, SIG_DFL);
#endif				/* TCF */

    /*
     * Process the arguments.
     * 
     * Note that processing of -v/-x is actually delayed till after script
     * processing.
     * 
     * We set the first character of our name to be '-' if we are a shell running
     * interruptible commands.  Many programs which examine ps'es use this to
     * filter such shells out.
     */
    argc--, tempv++;
    while (argc > 0 && (tcp = tempv[0])[0] == '-' &&
	   *++tcp != '\0' && !batch) {
	do
	    switch (*tcp++) {

	    case 0:		/* -	Interruptible, no prompt */
		prompt = 0;
		setintr = 1;
		nofile = 1;
		break;

	    case 'b':		/* -b	Next arg is input file */
		batch = 1;
		break;

	    case 'c':		/* -c	Command input from arg */
		if (argc == 1)
		    xexit(0);
		argc--, tempv++;
		arginp = SAVE(tempv[0]);
		/*
		 * * Give an error on -c arguments that end in * backslash to
		 * ensure that you don't make * nonportable csh scripts.
		 */
		{
		    register Char *cp;
		    register int count;

		    cp = arginp + Strlen(arginp);
		    count = 0;
		    while (cp > arginp && *--cp == '\\')
			++count;
		    if ((count & 1) != 0) {
			exiterr = 1;
			stderror(ERR_ARGC);
		    }
		}
		prompt = 0;
		nofile = 1;
		break;

#ifdef apollo
	    case 'D':		/* -D	Define environment variable */
		{
		    register Char *cp, *dp;

		    cp = str2short(tcp);
		    if (dp = Strchr(cp, '=')) {
			*dp++ = '\0';
			Setenv(cp, dp);
		    }
		    else
			Setenv(cp, STRNULL);
		}
		*tcp = '\0'; 	/* done with this argument */
		break;
#endif /* apollo */

#ifdef CSHDIRS
	    case 'd':		/* -d   Force load of ~/.cshdirs */
		dflag++;
		break;
#endif


	    case 'e':		/* -e	Exit on any error */
		exiterr = 1;
		break;

	    case 'f':		/* -f	Fast start */
		fast = 1;
		break;

	    case 'i':		/* -i	Interactive, even if !intty */
		intact = 1;
		nofile = 1;
		break;

	    case 'm':		/* -m	read .cshrc (from su) */
		mflag = 1;
		break;

	    case 'n':		/* -n	Don't execute */
		noexec = 1;
		break;

	    case 'q':		/* -q	(Undoc'd) ... die on quit */
		quitit = 1;
		break;

	    case 's':		/* -s	Read from std input */
		nofile = 1;
		break;

	    case 't':		/* -t	Read one line from input */
		onelflg = 2;
		prompt = 0;
		nofile = 1;
		break;

	    case 'v':		/* -v	Echo hist expanded input */
		nverbose = 1;	/* ... later */
		break;

	    case 'x':		/* -x	Echo just before execution */
		nexececho = 1;	/* ... later */
		break;

	    case 'V':		/* -V	Echo hist expanded input */
		setNS(STRverbose);	/* NOW! */
		break;

	    case 'X':		/* -X	Echo just before execution */
		setNS(STRecho);	/* NOW! */
		break;

#if defined(__convex__) || defined(convex)
	    case 'F':		/* Undocumented flag */
		/*
		 * This will cause children to be created using fork instead of
		 * vfork.
		 */
		use_fork = 1;
		break;
#endif
	    default:		/* Unknown command option */
		exiterr = 1;
		stderror(ERR_TCSHUSAGE, tcp-1);
		break;

	} while (*tcp);
	tempv++, argc--;
    }

    if (quitit)			/* With all due haste, for debugging */
	(void) signal(SIGQUIT, SIG_DFL);

    /*
     * Unless prevented by -, -c, -i, -s, or -t, if there are remaining
     * arguments the first of them is the name of a shell file from which to
     * read commands.
     */
    if (nofile == 0 && argc > 0) {
	nofile = open(tempv[0], O_RDONLY);
	if (nofile < 0) {
	    child = 1;		/* So this ... */
	    /* ... doesn't return */
	    stderror(ERR_SYSTEM, tempv[0], strerror(errno));
	}
	ffile = SAVE(tempv[0]);
	/* 
	 * Replace FSHIN. Handle /dev/std{in,out,err} specially
	 * since once they are closed we cannot open them again.
	 * In that case we use our own saved descriptors
	 */
	if ((SHIN = dmove(nofile, FSHIN)) < 0) 
	    switch(nofile) {
	    case 0:
		SHIN = FSHIN;
		break;
	    case 1:
		SHIN = FSHOUT;
		break;
	    case 2:
		SHIN = FSHDIAG;
		break;
	    default:
		stderror(ERR_SYSTEM, tempv[0], strerror(errno));
		break;
	    }
#ifdef FIOCLEX
	(void) ioctl(SHIN, FIOCLEX, NULL);
#endif
	prompt = 0;
	 /* argc not used any more */ tempv++;
    }
    /*
     * Consider input a tty if it really is or we are interactive. but not for
     * editing (christos)
     */
    if (!(intty = isatty(SHIN))) {
	if (adrof(STRedit))
	    unsetv(STRedit);
	editing = 0;
    }
    intty |= intact;
    if (intty || (intact && isatty(SHOUT))) {
	if (!batch && (uid != geteuid() || gid != getegid())) {
	    errno = EACCES;
	    child = 1;		/* So this ... */
	    /* ... doesn't return */
	    stderror(ERR_SYSTEM, "tcsh", strerror(errno));
	}
    }
    isoutatty = isatty(SHOUT);
    isdiagatty = isatty(SHDIAG);
    /*
     * Decide whether we should play with signals or not. If we are explicitly
     * told (via -i, or -) or we are a login shell (arg0 starts with -) or the
     * input and output are both the ttys("csh", or "csh</dev/ttyx>/dev/ttyx")
     * Note that in only the login shell is it likely that parent may have set
     * signals to be ignored
     */
    if (loginsh || intact || (intty && isatty(SHOUT)))
	setintr = 1;
    settell();
    /*
     * Save the remaining arguments in argv.
     */
    setq(STRargv, blk2short(tempv), &shvhed);

    /*
     * Set up the prompt.
     */
    if (prompt) {
	set(STRprompt, Strsave(uid == 0 ? STRsymhash : STRsymarrow));
	/* that's a meta-questionmark */
	set(STRprompt2, Strsave(STRmquestion));
	set(STRprompt3, Strsave(STRCORRECT));
    }

    /*
     * If we are an interactive shell, then start fiddling with the signals;
     * this is a tricky game.
     */
    shpgrp = mygetpgrp();
    opgrp = tpgrp = -1;
    if (setintr) {
	**argv = '-';
	if (!quitit)		/* Wary! */
	    (void) signal(SIGQUIT, SIG_IGN);
	(void) sigset(SIGINT, pintr);
	(void) sighold(SIGINT);
	(void) signal(SIGTERM, SIG_IGN);
	if (quitit == 0 && arginp == 0) {
#ifdef SIGTSTP
	    (void) signal(SIGTSTP, SIG_IGN);
#endif
#ifdef SIGTTIN
	    (void) signal(SIGTTIN, SIG_IGN);
#endif
#ifdef SIGTTOU
	    (void) signal(SIGTTOU, SIG_IGN);
#endif
	    /*
	     * Wait till in foreground, in case someone stupidly runs csh &
	     * dont want to try to grab away the tty.
	     */
	    if (isatty(FSHDIAG))
		f = FSHDIAG;
	    else if (isatty(FSHOUT))
		f = FSHOUT;
	    else if (isatty(OLDSTD))
		f = OLDSTD;
	    else
		f = -1;

#ifdef NeXT
	    /* NeXT 2.0 /usr/etc/rlogind, does not set our process group! */
	    if (shpgrp == 0) {
	        shpgrp = getpid();
		(void) setpgid(0, shpgrp);
	        (void) tcsetpgrp(f, shpgrp);
	    }
#endif /* NeXT */
#ifdef BSDJOBS			/* if we have tty job control */
    retry:
	    if ((tpgrp = tcgetpgrp(f)) != -1) {
		if (tpgrp != shpgrp) {
		    sigret_t(*old) () = signal(SIGTTIN, SIG_DFL);
		    (void) kill(0, SIGTTIN);
		    (void) signal(SIGTTIN, old);
		    goto retry;
		}
		/*
		 * Thanks to Matt Day for the POSIX references, and to
		 * Paul Close for the SGI clarification.
		 */
		if (setdisc(f) != -1) {
		    opgrp = shpgrp;
		    shpgrp = getpid();
		    tpgrp = shpgrp;
		    if (tcsetpgrp(f, shpgrp) == -1) {
			/*
			 * On hpux 7.03 this fails with EPERM. This happens on
			 * the 800 when opgrp != shpgrp at this point. (we were
			 * forked from a non job control shell)
			 * POSIX 7.2.4, says we failed because the process
			 * group specified did not belong to a process
			 * in the same session with the tty. So we set our
			 * process group and try again.
			 */
			if (setpgid(0, shpgrp) == -1) {
			    xprintf("setpgid:");
			    goto notty;
			}
			if (tcsetpgrp(f, shpgrp) == -1) {
			    xprintf("tcsetpgrp:");
			    goto notty;
			}
		    }
		    /*
		     * We check the process group now. If it is the same, then
		     * we don't need to set it again. On hpux 7.0 on the 300's
		     * if we set it again it fails with EPERM. This is the
		     * correct behavior according to POSIX 4.3.3 if the process
		     * was a session leader .
		     */
		    else if (shpgrp != mygetpgrp()) {
			if(setpgid(0, shpgrp) == -1) {
			    xprintf("setpgid:");
			    goto notty;
			}
		    }
#ifdef IRIS4D
		    /*
		     * But on irix 3.3 we need to set it again, even if it is
		     * the same. We do that to tell the system that we
		     * need BSD process group compatibility.
		     */
		    else
			(void) setpgid(0, shpgrp);
#endif
#ifdef FIOCLEX
		    (void) ioctl(dcopy(f, FSHTTY), FIOCLEX, NULL);
#else				/* FIOCLEX */
		    (void) dcopy(f, FSHTTY);
#endif				/* FIOCLEX */
		}
		else
		    tpgrp = -1;
	    }
	    if (tpgrp == -1) {
	notty:
		xprintf("Warning: no access to tty (%s).\n", strerror(errno));
		xprintf("Thus no job control in this shell.\n");
		/*
		 * Fix from:Sakari Jalovaara <sja@sirius.hut.fi> if we don't
		 * have access to tty, disable editing too
		 */
		if (adrof(STRedit))
		    unsetv(STRedit);
		editing = 0;
	    }
#else	/* BSDJOBS */		/* don't have job control, so frotz it */
	    tpgrp = -1;
#endif				/* BSDJOBS */
	}
    }
    if ((setintr == 0) && (parintr == SIG_DFL))
	setintr = 1;

/*
 * SVR4 doesn't send a SIGCHLD when a child is stopped or continued if the
 * handler is installed with signal(2) or sigset(2).  sigaction(2) must
 * be used instead.
 *
 * David Dawes (dawes@physics.su.oz.au) Sept 1991
 */

#if SVID > 3
    {
	struct sigaction act;
        act.sa_handler=pchild;
	sigemptyset(&(act.sa_mask)); /* Don't block any extra sigs when the
				      * handler is called
				      */
        act.sa_flags=0;	           /* want behaviour of sigset() without
                                    * SA_NOCLDSTOP
				    */
        sigaction(SIGCHLD,&act,(struct sigaction *)NULL);
    }
#else /* SVID <= 3 */
    (void) sigset(SIGCHLD, pchild);	/* while signals not ready */
#endif /* SVID <= 3 */


    if (intty && !arginp) 	
	(void) ed_Setup(editing);/* Get the tty state, and set defaults */
				 /* Only alter the tty state if editing */
    
    /*
     * Set an exit here in case of an interrupt or error reading the shell
     * start-up scripts.
     */
    reenter = setexit();	/* PWP */
    haderr = 0;			/* In case second time through */
    if (!fast && reenter == 0) {
	/* Will have value(STRhome) here because set fast if don't */
	{
	    int     osetintr = setintr;
	    sigret_t (*oparintr)() = parintr;

#ifdef BSDSIGS
	    sigmask_t omask = sigblock(sigmask(SIGINT));
#else
	    sighold(SIGINT);
#endif
	    setintr = 0;
	    parintr = SIG_IGN;	/* onintr in /etc/ files has no effect */
#ifdef _PATH_DOTCSHRC
	    (void) srcfile(_PATH_DOTCSHRC, 0, 0);
#endif
	    if (!arginp && !onelflg && !havhash)
		dohash(NULL,NULL);
#ifdef _PATH_DOTLOGIN
	    if (loginsh)
		(void) srcfile(_PATH_DOTLOGIN, 0, 0);
#endif
#ifdef BSDSIGS
	    (void) sigsetmask(omask);
#else
	    (void) sigrelse(SIGINT);
#endif
	    setintr = osetintr;
	    parintr = oparintr;
	}
#ifdef LOGINFIRST
	if (loginsh)
	    (void) srccat(value(STRhome), STRsldotlogin);
#endif
	/* upward compat. */
	if (!srccat(value(STRhome), STRsldottcshrc))
	    (void) srccat(value(STRhome), STRsldotcshrc);

	if (!fast && !arginp && !onelflg && !havhash)
	    dohash(NULL,NULL);
	/*
	 * Source history before .login so that it is available in .login
	 */
	if ((cp = value(STRhistfile)) != STRNULL)
	    loadhist[2] = cp;
	dosource(loadhist, NULL);
#ifndef LOGINFIRST
	if (loginsh)
	    (void) srccat(value(STRhome), STRsldotlogin);
#endif
#ifdef CSHDIRS
	/*
	 * if dflag then source ~/.cshdirs, but if fast ALWAYS skip the dirs
	 * restoring. (dflag used to get non-login shells to source the save
	 * dirs file). Of course, ~/.cshdirs must exist. -strike
	 */
	{
	    extern int bequiet;	/* make dirs shut up */
	    Char    cshd[BUFSIZE];
	    struct stat st;

	    (void) Strcpy(cshd, value(STRhome));
	    (void) Strcat(cshd, STRsldtdirs);
	    if (!stat(short2str(cshd), &st) &&
		(dflag || loginsh) && !fast) {
		bequiet = 1;
		dosource(loaddirs, NULL);
		bequiet = 0;
	    }
	}
#endif
    }
    /* Initing AFTER .cshrc is the Right Way */
    if (intty && !arginp) {	/* PWP setup stuff */
	ed_Init();		/* init the new line editor */
#ifdef SIG_WINDOW
	check_window_size(1);	/* mung environment */
#endif				/* SIG_WINDOW */
    }

    /*
     * Now are ready for the -v and -x flags
     */
    if (nverbose)
	setNS(STRverbose);
    if (nexececho)
	setNS(STRecho);
    /*
     * All the rest of the world is inside this call. The argument to process
     * indicates whether it should catch "error unwinds".  Thus if we are a
     * interactive shell our call here will never return by being blown past on
     * an error.
     */
    process(setintr);

    /*
     * Mop-up.
     */
    if (intty) {
	if (loginsh) {
	    xprintf("logout\n");
	    (void) close(SHIN);
	    child = 1;
#ifdef TESLA
	    do_logout = 1;
#endif				/* TESLA */
	    goodbye(NULL, NULL);
	}
	else {
	    xprintf("exit\n");
	}
    }
#ifdef CSHDIRS
    /*
     * save the directory stack -strike
     */
    recdirs();
#endif
    rechist();
    exitstat();
    return (0);
}

void
untty()
{
#ifdef BSDJOBS
    if (tpgrp > 0) {
	(void) setpgid(0, opgrp);
	(void) tcsetpgrp(FSHTTY, opgrp);
	(void) resetdisc(FSHTTY);
    }
#endif				/* BSDJOBS */
}

void
importpath(cp)
    Char   *cp;
{
    register int i = 0;
    register Char *dp;
    register Char **pv;
    int     c;

    for (dp = cp; *dp; dp++)
	if (*dp == ':')
	    i++;
    /*
     * i+2 where i is the number of colons in the path. There are i+1
     * directories in the path plus we need room for a zero terminator.
     */
    pv = (Char **) xcalloc((size_t) (i + 2), sizeof(Char *));
    dp = cp;
    i = 0;
    if (*dp)
	for (;;) {
	    if ((c = *dp) == ':' || c == 0) {
		*dp = 0;
		pv[i++] = Strsave(*cp ? cp : STRdot);
		if (c) {
		    cp = dp + 1;
		    *dp = ':';
		}
		else
		    break;
	    }
	    dp++;
	}
    pv[i] = 0;
    set1(STRpath, pv, &shvhed);
}

/*
 * Source to the file which is the catenation of the argument names.
 */
static int
srccat(cp, dp)
    Char   *cp, *dp;
{
    if (cp[0] == '/' && cp[1] == '\0') 
	return srcfile(short2str(dp), mflag ? 0 : 1, 0);
    else {
	register Char *ep = Strspl(cp, dp);
	char   *ptr = short2str(ep);

	xfree((ptr_t) ep);
	return srcfile(ptr, mflag ? 0 : 1, 0);
    }
}

/*
 * Source to a file putting the file descriptor in a safe place (> 2).
 */
static int
srcfile(f, onlyown, flag)
    char   *f;
    bool    onlyown, flag;
{
    register int unit;

    if ((unit = open(f, O_RDONLY)) == -1) 
	return 0;
    unit = dmove(unit, -1);

#ifdef FIOCLEX
    (void) ioctl(unit, FIOCLEX, NULL);
#endif
    srcunit(unit, onlyown, flag);
    return 1;
}

/*
 * Source to a unit.  If onlyown it must be our file or our group or
 * we don't chance it.	This occurs on ".cshrc"s and the like.
 */
int     insource;
static void
srcunit(unit, onlyown, hflg)
    register int unit;
    bool    onlyown, hflg;
{
    /*
     * PWP: this is arranged like this so that an optimizing compiler won't go
     * and put things like oSHIN in a register that longjmp() restores.  The
     * problem is that on my Sun 3/50, gcc will put oSHIN in a register.  That
     * would be OK, but setjmp() saves ALL of the registers and longjmp()
     * restores ALL of them, thus if you do a setjmp(), set oSHIN to something
     * (like SHIN), then do a longjmp(), the value of oSHIN will magically
     * become -1 again.
     * 
     * Perhaps setjmp() should only save the stack pointer, frame pointer, and
     * program counter...
     */

    /* We have to push down a lot of state here */
    /* All this could go into a structure */
    int     oSHIN = -1, oldintty = intty, oinsource = insource;
    struct whyle *oldwhyl = whyles;
    Char   *ogointr = gointr, *oarginp = arginp;
    Char   *oevalp = evalp, **oevalvec = evalvec;
    int     oonelflg = onelflg;
    bool    oenterhist = enterhist;
    char    OHIST = HIST;
    bool    otell = cantell;
    struct Bin saveB;
#ifdef BSDSIGS
    volatile sigmask_t omask = (sigmask_t) 0;
#endif
    jmp_buf oldexit;

    /* The (few) real local variables */
    int     my_reenter;

    if (unit < 0)
	return;
    if (didfds)
	donefds();
    if (onlyown) {
	struct stat stb;

	if (fstat(unit, &stb) < 0
	/* || (stb.st_uid != uid && stb.st_gid != gid) */
	    ) {
	    (void) close(unit);
	    return;
	}
    }

    /*
     * There is a critical section here while we are pushing down the input
     * stream since we have stuff in different structures. If we weren't
     * careful an interrupt could corrupt SHIN's Bin structure and kill the
     * shell.
     * 
     * We could avoid the critical region by grouping all the stuff in a single
     * structure and pointing at it to move it all at once.  This is less
     * efficient globally on many variable references however.
     */
    insource = 1;
    getexit(oldexit);

    if (setintr)
#ifdef BSDSIGS
	omask = sigblock(sigmask(SIGINT));
#else
	(void) sighold(SIGINT);
#endif
    /*
     * Bugfix for running out of memory by: Jak Kirman
     * <jak%cs.brown.edu@RELAY.CS.NET>.  Solution: pay attention to what
     * setexit() is returning because my_reenter _may_ be in a register, and
     * thus restored to 0 on a longjump(). (PWP: insert flames about
     * compiler-dependant code here) PWP: THANKS LOTS !!!
     */
    /* Setup the new values of the state stuff saved above */
    copy((char *) &(saveB), (char *) &B, sizeof(B));
    fbuf = NULL;
    fseekp = feobp = fblocks = 0;
    oSHIN = SHIN, SHIN = unit, arginp = 0, onelflg = 0;
    intty = isatty(SHIN), whyles = 0, gointr = 0;
    evalvec = 0;
    evalp = 0;
    enterhist = hflg;
    if (enterhist)
	HIST = '\0';

    /*
     * Now if we are allowing commands to be interrupted, we let ourselves be
     * interrupted.
     */
    if (setintr)
#ifdef BSDSIGS
	(void) sigsetmask(omask);
#else
	(void) sigrelse(SIGINT);
#endif
    settell();

/* PWP: think of this as like a LISP (unwind-protect ...) */
/* thanks to Diana Smetters for pointing out how this _should_ be written */
#ifdef cray
    my_reenter = 1;		/* assume non-zero return val */
    if (setexit() == 0) {
	my_reenter = 0;		/* Oh well, we were wrong */
#else
    if ((my_reenter = setexit()) == 0) {
#endif
	process(0);		/* 0 -> blow away on errors */
    }

    if (setintr)
#ifdef BSDSIGS
	(void) sigsetmask(omask);
#else
	(void) sigrelse(SIGINT);
#endif
    if (oSHIN >= 0) {
	register int i;

	/* We made it to the new state... free up its storage */
	/* This code could get run twice but xfree doesn't care */
	for (i = 0; i < fblocks; i++)
	    xfree((ptr_t) fbuf[i]);
	xfree((ptr_t) fbuf);

	/* Reset input arena */
	copy((char *) &B, (char *) &(saveB), sizeof(B));

	(void) close(SHIN), SHIN = oSHIN;
	arginp = oarginp, onelflg = oonelflg;
	evalp = oevalp, evalvec = oevalvec;
	intty = oldintty, whyles = oldwhyl, gointr = ogointr;
	if (enterhist)
	    HIST = OHIST;
	enterhist = oenterhist;
	cantell = otell;
    }

    resexit(oldexit);
    /*
     * If process reset() (effectively an unwind) then we must also unwind.
     */
    if (my_reenter)
	stderror(ERR_SILENT);
    insource = oinsource;
}

void
rechist()
{
    Char    buf[BUFSIZE], *hfile;
    int     fp, ftmp, oldidfds;

    if (!fast) {
	if (value(STRsavehist)[0] == '\0')
	    return;
	if ((hfile = value(STRhistfile)) == STRNULL) {
	    hfile = Strcpy(buf, value(STRhome));
	    (void) Strcat(buf, STRsldthist);
	}
	fp = creat(short2str(hfile), 0600);
	if (fp == -1) 
	    return;
	oldidfds = didfds;
	didfds = 0;
	ftmp = SHOUT;
	SHOUT = fp;
	(void) Strcpy(buf, value(STRsavehist));
	dumphist[2] = buf;
	dohist(dumphist, NULL);
	(void) close(fp);
	SHOUT = ftmp;
	didfds = oldidfds;
    }
}

/*ARGSUSED*/
void
goodbye(v, c)
    Char **v;
    struct command *c;
{
    rechist();

#ifdef CSHDIRS
    recdirs();
#endif

    if (loginsh) {
	(void) signal(SIGQUIT, SIG_IGN);
	(void) sigset(SIGINT, SIG_IGN);
	(void) signal(SIGTERM, SIG_IGN);
	setintr = 0;		/* No interrupts after "logout" */
	if (!(adrof(STRlogout)))
	    set(STRlogout, STRnormal);
#ifdef _PATH_DOTLOGOUT
	(void) srcfile(_PATH_DOTLOGOUT, 0, 0);
#endif
	if (adrof(STRhome))
	    (void) srccat(value(STRhome), STRsldtlogout);
#ifdef TESLA
	do_logout = 1;
#endif				/* TESLA */
    }
    exitstat();
}

void
exitstat()
{
    register Char *cp;
    register int i;
#ifdef PROF
    monitor(0);
#endif
    /*
     * Note that if STATUS is corrupted (i.e. getn bombs) then error will exit
     * directly because we poke child here. Otherwise we might continue
     * unwarrantedly (sic).
     */
    child = 1;

    /* 
     * PWP: do this step-by-step because we might get a bus error if
     * status isn't set, so we call getn(NULL).
     */
    cp = value(STRstatus);

    if (!cp)
	i = 13;
    else
	i = getn(cp);

    xexit(i);
}

/*
 * in the event of a HUP we want to save the history
 */
static  sigret_t
phup(snum)
int snum;
{
#ifdef UNRELSIGS
    if (snum)
	(void) sigset(snum, SIG_IGN);
#endif /* UNRELSIGS */
    rechist();
#ifdef CSHDIRS
    /*
     * save the directory stack on HUP - strike
     */
    recdirs();
#endif
    xexit(snum);
#ifndef SIGVOID
    return (snum);
#endif
}

Char   *jobargv[2] = {STRjobs, 0};

/*
 * Catch an interrupt, e.g. during lexical input.
 * If we are an interactive shell, we reset the interrupt catch
 * immediately.  In any case we drain the shell output,
 * and finally go through the normal error mechanism, which
 * gets a chance to make the shell go away.
 */
int     just_signaled;		/* bugfix by Michael Bloom (mg@ttidca.TTI.COM) */

#ifdef SIGVOID
/*ARGSUSED*/
#endif
sigret_t
pintr(snum)
int snum;
{
#ifdef UNRELSIGS
    if (snum)
	(void) sigset(snum, pintr);
#endif /* UNRELSIGS */
    just_signaled = 1;
    pintr1(1);
#ifndef SIGVOID
    return (snum);
#endif
}

void
pintr1(wantnl)
    bool    wantnl;
{
    register Char **v;
#ifdef BSDSIGS
    sigmask_t omask;
#endif

#ifdef BSDSIGS
    omask = sigblock((sigmask_t) 0);
#endif
    if (setintr) {
#ifdef BSDSIGS
	(void) sigsetmask(omask & ~sigmask(SIGINT));
#else
	(void) sigrelse(SIGINT);
#endif
	if (pjobs) {
	    pjobs = 0;
	    xprintf("\n");
	    dojobs(jobargv, NULL);
	    stderror(ERR_NAME | ERR_INTR);
	}
    }
    /* JV - Make sure we shut off inputl */
    {
	extern Char GettingInput;

	(void) Cookedmode();
	GettingInput = 0;
    }
#ifdef BSDSIGS
    (void) sigsetmask(omask & ~sigmask(SIGCHLD));
#else
    if (setintr)
	(void) sighold(SIGINT);
    (void) sigrelse(SIGCHLD);
#endif
    draino();
    (void) endpwent();

    /*
     * If we have an active "onintr" then we search for the label. Note that if
     * one does "onintr -" then we shan't be interruptible so we needn't worry
     * about that here.
     */
    if (gointr) {
	gotolab(gointr);
	timflg = 0;
	if (v = pargv)
	    pargv = 0, blkfree(v);
	if (v = gargv)
	    gargv = 0, blkfree(v);
	reset();
    }
    else if (intty && wantnl) {
	/* xprintf("\n"); *//* Some like this, others don't */
	(void) putraw('\r');
	(void) putraw('\n');
    }
    stderror(ERR_SILENT);
}

/*
 * Process is the main driving routine for the shell.
 * It runs all command processing, except for those within { ... }
 * in expressions (which is run by a routine evalav in sh.exp.c which
 * is a stripped down process), and `...` evaluation which is run
 * also by a subset of this code in sh.glob.c in the routine backeval.
 *
 * The code here is a little strange because part of it is interruptible
 * and hence freeing of structures appears to occur when none is necessary
 * if this is ignored.
 *
 * Note that if catch is not set then we will unwind on any error.
 * If an end-of-file occurs, we return.
 */
struct command *savet = NULL;
void
process(catch)
    bool    catch;
{
    extern char Expand;
    jmp_buf osetexit;
    /* PWP: This might get nuked my longjmp so don't make it a register var */
    struct command *t = savet;

    savet = NULL;
    getexit(osetexit);
    for (;;) {

	pendjob();
	paraml.next = paraml.prev = &paraml;
	paraml.word = STRNULL;
	(void) setexit();
	justpr = enterhist;	/* execute if not entering history */

	/*
	 * Interruptible during interactive reads
	 */
	if (setintr)
#ifdef BSDSIGS
	    (void) sigsetmask(sigblock((sigmask_t) 0) & ~sigmask(SIGINT));
#else
	    (void) sigrelse(SIGINT);
#endif


	/*
	 * For the sake of reset()
	 */
	freelex(&paraml);
	if (savet)
	    freesyn(savet), savet = NULL;

	if (haderr) {
	    if (!catch) {
		/* unwind */
		doneinp = 0;
		savet = t;
		resexit(osetexit);
		reset();
	    }
	    haderr = 0;
	    /*
	     * Every error is eventually caught here or the shell dies.  It is
	     * at this point that we clean up any left-over open files, by
	     * closing all but a fixed number of pre-defined files.  Thus
	     * routines don't have to worry about leaving files open due to
	     * deeper errors... they will get closed here.
	     */
	    closem();
	    continue;
	}
	if (doneinp) {
	    doneinp = 0;
	    break;
	}
	if (chkstop)
	    chkstop--;
	if (neednote)
	    pnote();
	if (intty && prompt && evalvec == 0) {
	    mailchk();
	    /*
	     * Watch for logins/logouts. Next is scheduled commands stored
	     * previously using "sched." Then execute periodic commands.
	     * Following that, the prompt precmd is run.
	     */
	    watch_login();
	    sched_run();
	    period_cmd();
	    precmd();
	    /*
	     * If we are at the end of the input buffer then we are going to
	     * read fresh stuff. Otherwise, we are rereading input and don't
	     * need or want to prompt.
	     */
	    if (fseekp == feobp && aret == F_SEEK)
		printprompt(0, NULL);
	    flush();
	    setalarm(1);
	}
	if (seterr) {
	    xfree((ptr_t) seterr);
	    seterr = NULL;
	}

	/*
	 * Echo not only on VERBOSE, but also with history expansion. If there
	 * is a lexical error then we forego history echo.
	 */
	if ((lex(&paraml) && !seterr && intty && !tellwhat && !Expand) ||
	    adrof(STRverbose)) {
	    haderr = 1;
	    prlex(&paraml);
	    haderr = 0;
	}
	(void) alarm(0);	/* Autologout OFF */

	/*
	 * The parser may lose space if interrupted.
	 */
	if (setintr)
#ifdef BSDSIGS
	    (void) sigblock(sigmask(SIGINT));
#else
	    (void) sighold(SIGINT);
#endif

	/*
	 * Save input text on the history list if reading in old history, or it
	 * is from the terminal at the top level and not in a loop.
	 * 
	 * PWP: entry of items in the history list while in a while loop is done
	 * elsewhere...
	 */
	if (enterhist || (catch && intty && !whyles && !tellwhat))
	    savehist(&paraml);

	if (Expand && seterr)
	    Expand = 0;

	/*
	 * Print lexical error messages, except when sourcing history lists.
	 */
	if (!enterhist && seterr)
	    stderror(ERR_OLD);

	/*
	 * If had a history command :p modifier then this is as far as we
	 * should go
	 */
	if (justpr)
	    reset();

	/*
	 * If had a tellwhat from twenex() then do
	 */
	if (tellwhat) {
	    tellmewhat(&paraml);
	    reset();
	}

	alias(&paraml);

#ifdef BSDJOBS
	/*
	 * If we are interactive, try to continue jobs that we have stopped
	 */
	if (prompt)
	    continue_jobs(&paraml);
#endif				/* BSDJOBS */

	/*
	 * Check to see if the user typed "rm * .o" or something
	 */
	if (prompt)
	    rmstar(&paraml);
	/*
	 * Parse the words of the input into a parse tree.
	 */
	savet = syntax(paraml.next, &paraml, 0);
	if (seterr)
	    stderror(ERR_OLD);

	/*
	 * Execute the parse tree From: Michael Schroeder
	 * <mlschroe@immd4.informatik.uni-erlangen.de> was execute(t, tpgrp);
	 */
	execute(savet, (tpgrp > 0 ? tpgrp : -1), NULL, NULL);

	/*
	 * Made it!
	 */
	freelex(&paraml);
	freesyn(savet), savet = NULL;
#ifdef SIG_WINDOW
	if (catch && intty && !whyles && !tellwhat)
	    window_change(0);	/* for window systems */
#endif				/* SIG_WINDOW */
    }
    savet = t;
    resexit(osetexit);
}

/*ARGSUSED*/
void
dosource(t, c)
    register Char **t;
    struct command *c;
{
    register Char *f;
    bool    hflg = 0;
    char    buf[BUFSIZE];

    t++;
    if (*t && eq(*t, STRmh)) {
	if (*++t == NULL)
	    stderror(ERR_NAME | ERR_HFLAG);
	hflg++;
    }
    f = globone(*t, G_ERROR);
    (void) strcpy(buf, short2str(f));
    xfree((ptr_t) f);
    if ((!srcfile(buf, 0, hflg)) && (!hflg))
	stderror(ERR_SYSTEM, buf, strerror(errno));
}

/*
 * Check for mail.
 * If we are a login shell, then we don't want to tell
 * about any mail file unless its been modified
 * after the time we started.
 * This prevents us from telling the user things he already
 * knows, since the login program insists on saying
 * "You have mail."
 */
static void
mailchk()
{
    register struct varent *v;
    register Char **vp;
    time_t  t;
    int     intvl, cnt;
    struct stat stb;
    bool    new;

    v = adrof(STRmail);
    if (v == 0)
	return;
    (void) time(&t);
    vp = v->vec;
    cnt = blklen(vp);
    intvl = (cnt && number(*vp)) ? (--cnt, getn(*vp++)) : MAILINTVL;
    if (intvl < 1)
	intvl = 1;
    if (chktim + intvl > t)
	return;
    for (; *vp; vp++) {
	if (stat(short2str(*vp), &stb) < 0)
	    continue;
#if defined(BSDTIMES) || defined(_SEQUENT_)
	new = stb.st_mtime > time0.tv_sec;
#else
	new = stb.st_mtime > time0;
#endif
	if (stb.st_size == 0 || stb.st_atime > stb.st_mtime ||
	    (stb.st_atime <= chktim && stb.st_mtime <= chktim) ||
	    (loginsh && !new))
	    continue;
	if (cnt == 1)
	    xprintf("You have %smail.\n", new ? "new " : "");
	else
	    xprintf("%s in %s.\n", new ? "New mail" : "Mail",
		    short2str(*vp));
    }
    chktim = t;
}

/*
 * Extract a home directory from the password file
 * The argument points to a buffer where the name of the
 * user whose home directory is sought is currently.
 * We write the home directory of the user back there.
 */
int
gethdir(home)
    Char   *home;
{
    Char   *h;

    /*
     * Is it us?
     */
    if (*home == '\0') {
	if (h = value(STRhome)) {
	    (void) Strcpy(home, h);
	    return 0;
	}
	else
	    return 1;
    }

    /*
     * Look in the cache
     */
    if ((h = gettilde(home)) == NULL)
	return 1;
    else {
	(void) Strcpy(home, h);
	return 0;
    }
}

/*
 * Move the initial descriptors to their eventual
 * resting places, closing all other units.
 */
void
initdesc()
{

    didfds = 0;			/* 0, 1, 2 aren't set up */
#ifdef FIOCLEX
    (void) ioctl(SHIN = dcopy(0, FSHIN), FIOCLEX, NULL);
    (void) ioctl(SHOUT = dcopy(1, FSHOUT), FIOCLEX, NULL);
    (void) ioctl(SHDIAG = dcopy(2, FSHDIAG), FIOCLEX, NULL);
    (void) ioctl(OLDSTD = dcopy(SHIN, FOLDSTD), FIOCLEX, NULL);
#else
    didcch = 0;			/* Havent closed for child */
    SHIN = dcopy(0, FSHIN);
    SHOUT = dcopy(1, FSHOUT);
    isoutatty = isatty(SHOUT);
    SHDIAG = dcopy(2, FSHDIAG);
    isdiagatty = isatty(SHDIAG);
    OLDSTD = dcopy(SHIN, FOLDSTD);
#endif
    closem();
}


void
#ifdef PROF
done(i)
#else
xexit(i)
#endif
    int     i;
{
#ifdef TESLA
    if (loginsh && do_logout) {
	/* this is to send hangup signal to the develcon */
	/* we toggle DTR. clear dtr - sleep 1 - set dtr */
	/* ioctl will return ENOTTY for pty's but we ignore it 	 */
	/* exitstat will run after disconnect */
	/* we sleep for 2 seconds to let things happen in */
	/* .logout and rechist() */
#ifdef TIOCCDTR
	(void) sleep(2);
	(void) ioctl(FSHTTY, TIOCCDTR, NULL);
	(void) sleep(1);
	(void) ioctl(FSHTTY, TIOCSDTR, NULL);
#endif				/* TIOCCDTR */
    }
#endif				/* TESLA */

    untty();
    _exit(i);
}

static Char **
defaultpath()
{
    char   *ptr;
    Char  **blk, **blkp;
    struct stat stb;

    blkp = blk = (Char **) xmalloc((size_t) sizeof(Char *) * 10);

#ifndef DOTLAST
    *blkp++ = Strsave(STRdot);
#endif

#define DIRAPPEND(a)  \
	if (stat(ptr = a, &stb) == 0 && S_ISDIR(stb.st_mode)) \
		*blkp++ = SAVE(ptr)

#ifdef _PATH_LOCAL
    DIRAPPEND(_PATH_LOCAL);
#endif

#ifdef _PATH_USRUCB
    DIRAPPEND(_PATH_USRUCB);
#endif

#ifdef _PATH_USRBSD
    DIRAPPEND(_PATH_USRBSD);
#endif

#ifdef _PATH_BIN
    DIRAPPEND(_PATH_BIN);
#endif

#ifdef _PATH_USRBIN
    DIRAPPEND(_PATH_USRBIN);
#endif

#undef DIRAPPEND

#ifdef DOTLAST
    *blkp++ = Strsave(STRdot);
#endif
    *blkp = NULL;
    return (blk);
}
