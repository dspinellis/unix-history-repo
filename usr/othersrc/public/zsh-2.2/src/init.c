/*
 *
 * init.c - main loop and initialization routines
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#define GLOBALS
#include "zsh.h"
#include <pwd.h>

void main(argc,argv,envp) /**/
int argc; char **argv; char **envp;
{
int notect = 0;

#ifdef LC_ALL
	setlocale(LC_ALL, "");
#endif
	environ = envp;
	meminit();
	setflags();
	parseargs(argv);
	setmoreflags();
	setupvals();
	initialize();
	heapalloc();
	runscripts();
	for(;;)
		{
		do
			loop();
		while (tok != ENDINPUT);
		if (!(isset(IGNOREEOF) && interact))
			{
#if 0
			if (interact)
				fputs(islogin ? "logout\n" : "exit\n",stderr);
#endif
			zexit(NULL);
			continue;
			}
		zerrnam("zsh",(!islogin) ? "use 'exit' to exit."
			: "use 'logout' to logout.",NULL,0);
		notect++;
		if (notect == 10)
			zexit(NULL);
		}
}

/* keep executing lists until EOF found */

void loop() /**/
{
List list;
Heap h = (Heap) peekfirst(heaplist);

	pushheap();
	for(;;)
		{
		freeheap();
		if (interact && isset(SHINSTDIN))
			preprompt();
		hbegin();		/* init history mech */
		intr();			/* interrupts on */
		ainit();			/* init alias mech */
		lexinit();
		errflag = 0;
		if (!(list = parse_event()))
			{				/* if we couldn't parse a list */
			hend();
			if (tok == ENDINPUT && !errflag)
				break;
			continue;
			}
		if (hend())
			{
			if (stopmsg)		/* unset 'you have stopped jobs' flag */
				stopmsg--;
			execlist(list);
			}
		if (ferror(stderr))
			{
			zerr("write error",NULL,0);
			clearerr(stderr);
			}
		if (subsh)				/* how'd we get this far in a subshell? */
			exit(lastval);
		if ((!interact && errflag) || retflag)
			break;
		if ((opts['t'] == OPT_SET) || (lastval && opts[ERREXIT] == OPT_SET))
			{
			if (sigtrapped[SIGEXIT])
				dotrap(SIGEXIT);
			exit(lastval);
			}
		}
	while ((Heap) peekfirst(heaplist) != h)
		popheap();
}

void setflags() /**/
{
int c;

	for (c = 0; c != 32; c++)		opts[c] = OPT_UNSET;
	for (c = 32; c != 128; c++)	opts[c] = OPT_INVALID;
	for (c = 'a'; c <= 'z'; c++)	opts[c] = opts[c-'a'+'A'] = OPT_UNSET;
	for (c = '0'; c <= '9'; c++)	opts[c] = OPT_UNSET;
	opts['A'] = OPT_INVALID;
	opts['i'] = (isatty(0)) ? OPT_SET : OPT_UNSET;
	opts[BGNICE] = opts[NOTIFY] = OPT_SET;
	opts[USEZLE] = (interact && SHTTY != -1) ? OPT_SET : OPT_UNSET;
	opts[HASHCMDS] = opts[HASHLISTALL] = opts[HASHDIRS] = OPT_SET;
}

static char *cmd;

void parseargs(argv) /**/
char **argv;
{
char **x;
int bk = 0,action;
Lklist paramlist;

	hackzero = argzero = *argv;
	opts[LOGINSHELL] = (**(argv++) == '-') ? OPT_SET : OPT_UNSET;
	SHIN = 0;
	while (!bk && *argv && (**argv == '-' || **argv == '+'))
		{
		action = (**argv == '-') ? OPT_SET : OPT_UNSET;
		while (*++*argv) {
			if (opts[**argv] == OPT_INVALID) {
				zerr("bad option: -%c",NULL,**argv);
				exit(1);
			}
			if (bk = **argv == 'b') break;
			if (**argv == 'c') { /* -c command */
				argv++;
				if (!*argv) {
					zerr("string expected after -c",NULL,0);
					exit(1);
				}
				cmd = *argv;
				opts[INTERACTIVE] = OPT_UNSET;
				opts['c'] = OPT_SET;
				break;
			} else if (**argv == 'o') {
				int c;

				if (!*++*argv)
					argv++;
				if (!*argv) {
					zerr("string expected after -o",NULL,0);
					exit(1);
				}
				c = optlookup(*argv);
				if (c == -1)
					zerr("no such option: %s",*argv,0);
				else
					opts[c] = action;
				break;
			} else opts[**argv] = action;
		}
		argv++;
	}
	paramlist = newlist();
	if (*argv)
		{
		if (opts[SHINSTDIN] == OPT_UNSET)
			{
			SHIN = movefd(open(argzero = *argv,O_RDONLY));
			if (SHIN == -1)
				{
				zerr("can't open input file: %s",*argv,0);
				exit(1);
				}
			opts[INTERACTIVE] = OPT_UNSET;
			argv++;
			}
		while (*argv)
			addnode(paramlist,ztrdup(*argv++));
		}
	else
		opts[SHINSTDIN] = OPT_SET;
	pparams = x = zcalloc((countnodes(paramlist)+1)*sizeof(char *));
	while (*x++ = getnode(paramlist));
	free(paramlist);
	argzero = ztrdup(argzero);
}

void setmoreflags() /**/
{
int t0;
long ttpgrp;

	/* stdout,stderr fully buffered */
#ifdef _IOFBF
	setvbuf(stdout,malloc(BUFSIZ),_IOFBF,BUFSIZ);
	setvbuf(stderr,malloc(BUFSIZ),_IOFBF,BUFSIZ);
#else
	setbuffer(stdout,malloc(BUFSIZ),BUFSIZ);
	setbuffer(stderr,malloc(BUFSIZ),BUFSIZ);
#endif
	subsh = 0;
#ifndef NOCLOSEFUNNYFDS
	/* this works around a bug in some versions of in.rshd */
	for (t0 = 3; t0 != 10; t0++)
		close(t0);
#endif
#ifdef JOB_CONTROL
	opts[MONITOR] = (interact) ? OPT_SET : OPT_UNSET;
	if (jobbing) {
		SHTTY = movefd((isatty(0)) ? dup(0) : open("/dev/tty",O_RDWR));
		if (SHTTY == -1)
			opts[MONITOR] = OPT_UNSET;
		else {
#ifdef TIOCSETD
#ifdef NTTYDISC
			int ldisc = NTTYDISC;
			ioctl(SHTTY, TIOCSETD, &ldisc);
#endif
#endif
			gettyinfo(&shttyinfo);	/* get tty state */
#ifdef sgi
			if (shttyinfo.tio.c_cc[VSWTCH] <= 0) /* hack for irises */
				shttyinfo.tio.c_cc[VSWTCH] = CSWTCH;
#endif
			savedttyinfo = shttyinfo;
		}
#ifdef sgi
		setpgrp(0,getpgrp(0));
#endif
		if ((mypgrp = getpgrp(0)) <= 0)
			opts[MONITOR] = OPT_UNSET;
		else while ((ttpgrp = gettygrp()) != -1 && ttpgrp != mypgrp) {
			sleep(1);
			mypgrp = getpgrp(0);
			if (mypgrp == gettygrp()) break;
			killpg(mypgrp,SIGTTIN);
			mypgrp = getpgrp(0);
		}
	} else
		SHTTY = -1;
#else
	opts[MONITOR] = OPT_UNSET;
	SHTTY = movefd((isatty(0)) ? dup(0) : open("/dev/tty",O_RDWR));
	if (SHTTY != -1) {
		gettyinfo(&shttyinfo);
		savedttyinfo = shttyinfo;
	}
#endif
}

void setupvals() /**/
{
struct passwd *pswd;
char *ptr,*s;
static long bauds[] = {
	0,50,75,110,134,150,200,300,600,1200,1800,2400,4800,9600,19200,38400
	};

	curhist = 0;
	histsiz = DEFAULT_HISTSIZE;
	lithistsiz = 5;
	inithist();
	mailcheck = logcheck = 60;
	dirstacksize = -1;
	listmax = 100;
	reporttime = -1;
	bangchar = '!';
	hashchar = '#';
	hatchar = '^';
	termok = 0;
	curjob = prevjob = coprocin = coprocout = -1;
	shtimer = time(NULL);	/* init $SECONDS */
	srand((unsigned int) shtimer);
	/* build various hash tables; argument to newhtable is table size */
	aliastab = newhtable(37);
	addreswords();
	paramtab = newhtable(151);
	cmdnamtab = newhtable(37);
	compctltab = newhtable(13);
	initxbindtab();
	nullcmd = ztrdup("cat");
	readnullcmd = ztrdup("more");
	prompt = ztrdup("%m%# ");
	prompt2 = ztrdup("> ");
	prompt3 = ztrdup("?# ");
	prompt4 = ztrdup("+ ");
	sprompt = ztrdup("zsh: correct `%R' to `%r' [nyae]? ");
	term = ztrdup("");
	ppid = getppid();
#ifdef TIO
#ifdef HAS_TCCRAP
	baud = cfgetospeed(&shttyinfo.tio);
	if (baud < 100) baud = bauds[baud]; /* aren't "standards" great?? */
#else
	baud = bauds[shttyinfo.tio.c_cflag & CBAUD];
#endif
#else
	baud = bauds[shttyinfo.sgttyb.sg_ospeed];
#endif
#ifdef TIOCGWINSZ
	if (!(columns = shttyinfo.winsize.ws_col))
		columns = 80;
	if (!(lines = shttyinfo.winsize.ws_row))
		lines = 24;
#else
	columns = 80;
	lines = 24;
#endif
	ifs = ztrdup(" \t\n");
	if (pswd = getpwuid(getuid())) {
		username = ztrdup(pswd->pw_name);
		home = ztrdup(pswd->pw_dir);
	} else {
		username = ztrdup("");
		home = ztrdup("/");
	}
	if (ptr = zgetenv("LOGNAME"))
		logname = ztrdup(ptr);
	else
		logname = ztrdup(username);
	timefmt = ztrdup(DEFTIMEFMT);
	watchfmt = ztrdup(DEFWATCHFMT);
	if (!(ttystrname = ztrdup(ttyname(SHTTY))))
		ttystrname = ztrdup("");
	wordchars = ztrdup(DEFWORDCHARS);
	fceditparam = ztrdup(DEFFCEDIT);
	tmpprefix = ztrdup(DEFTMPPREFIX);
	postedit = ztrdup("");
	if (ispwd(home)) pwd = ztrdup(home);
	else if ((ptr = zgetenv("PWD")) && ispwd(ptr)) pwd = ztrdup(ptr);
	else pwd = zgetwd();
	oldpwd = ztrdup(pwd);
	hostnam = zalloc(256);
	underscore = ztrdup("");
	gethostname(hostnam,256);
	mypid = getpid();
	cdpath = mkarray(NULL);
	manpath = mkarray(NULL);
	fignore = mkarray(NULL);
	fpath = mkarray(NULL);
	mailpath = mkarray(NULL);
	watch = mkarray(NULL);
	hosts = mkarray(NULL);
	compctlsetup();
	userdirs = (char **) zcalloc(sizeof(char *)*2);
	usernames = (char **) zcalloc(sizeof(char *)*2);
	userdirsz = 2;
	userdirct = 0;
	optarg = ztrdup("");
	zoptind = 1;
	schedcmds = NULL;
	path = (char **) zalloc(4*sizeof *path);
	path[0] = ztrdup("/bin"); path[1] = ztrdup("/usr/bin");
	path[2] = ztrdup("/usr/ucb"); path[3] = NULL;
	inittyptab();
	initlextabs();
	setupparams();
	setparams();
	inittyptab();
	if ((s = zgetenv("EMACS")) && !strcmp(s,"t") && !strcmp(term,"emacs"))
		opts[USEZLE] = OPT_UNSET;
#ifndef HAS_RUSAGE
	times(&shtms);
#endif
}

void compctlsetup() /**/
{
static char
	*hs[] = {"telnet","rlogin","ftp","rup","rusers","rsh",NULL},
	*os[] = {"setopt","unsetopt",NULL},
	*vs[] = {"export","typeset","vared","unset",NULL},
	*cs[] = {"which","builtin",NULL},
	*bs[] = {"bindkey",NULL};

	compctl_process(hs,CC_HOSTS|CC_FILES,NULL);
	compctl_process(os,CC_OPTIONS,NULL);
	compctl_process(vs,CC_VARS,NULL);
	compctl_process(bs,CC_BINDINGS,NULL);
	compctl_process(cs,CC_COMMPATH,NULL);
	cc_compos.mask   = CC_COMMPATH;
	cc_default.mask  = CC_FILES;
}

void initialize() /**/
{
int t0;

	breaks = loops = 0;
	lastmailcheck = time(NULL);
	locallist = NULL;
	dirstack = newlist();
	bufstack = newlist();
	newcmdnamtab();
	inbuf = zalloc(inbufsz = 256);
	inbufptr = inbuf+inbufsz;
	inbufct = 0;
#ifndef QDEBUG
	signal(SIGQUIT,SIG_IGN);
#endif
#ifdef RLIM_INFINITY
	for (t0 = 0; t0 != RLIM_NLIMITS; t0++)
		getrlimit(t0,limits+t0);
#endif
	hsubl = hsubr = NULL;
	lastpid = 0;
	bshin = fdopen(SHIN,"r");
	signal(SIGCHLD,handler);
	if (jobbing)
		{
		int ttypgrp;
		for (;;)
			{
#ifdef TIOCGPGRP
			ioctl(SHTTY, TIOCGPGRP, &ttypgrp);
#else
			ttypgrp = tcgetpgrp(SHTTY);
#endif
			if (ttypgrp == mypgrp)
				break;
			kill(0,SIGTTIN);
			}
		signal(SIGTTOU,SIG_IGN);
		signal(SIGTSTP,SIG_IGN);
		signal(SIGTTIN,SIG_IGN);
		signal(SIGPIPE,SIG_IGN);
		attachtty(mypgrp);
		}
	if (interact)
		{
		signal(SIGTERM,SIG_IGN);
#ifdef SIGWINCH
		signal(SIGWINCH,handler);
#endif
		signal(SIGALRM,handler);
		intr();
		}
}

void addreswords() /**/
{
static char *reswds[] = {
	"do", "done", "esac", "then", "elif", "else", "fi", "for", "case",
	"if", "while", "function", "repeat", "time", "until", "exec", "command",
	"select", "coproc", "noglob", "-", "nocorrect", "foreach", "end", NULL
	};
int t0;

	for (t0 = 0; reswds[t0]; t0++)
		addhperm(reswds[t0],mkanode(NULL,-1-t0),aliastab,NULL);
}

void runscripts() /**/
{
#ifdef GLOBALZSHENV
	source(GLOBALZSHENV);
#endif
	if (opts[NORCS] == OPT_SET) {
#ifdef GLOBALZPROFILE
		if (islogin) source(GLOBALZPROFILE);
#endif
#ifdef GLOBALZSHRC
		source(GLOBALZSHRC);
#endif
#ifdef GLOBALZLOGIN
		if (islogin) source(GLOBALZLOGIN);
#endif
	} else {
		sourcehome(".zshenv");
		if (opts[NORCS] == OPT_SET)
			return;
		if (interact) {
			if (islogin) {
				sourcehome(".zprofile");
#ifdef GLOBALZPROFILE
				source(GLOBALZPROFILE);
#endif
			}
#ifdef GLOBALZSHRC
			source(GLOBALZSHRC);
#endif
			sourcehome(".zshrc");
			if (islogin) {
#ifdef GLOBALZLOGIN
				source(GLOBALZLOGIN);
#endif
				sourcehome(".zlogin");
			}
		}
	}
	if (interact)
		readhistfile(getsparam("HISTFILE"),0);
	if (opts['c'] == OPT_SET)
		{
		if (SHIN >= 10)
			close(SHIN);
		SHIN = movefd(open("/dev/null",O_RDONLY));
		execstring(cmd);
		stopmsg = 1;
		zexit(NULL);
		}
#ifdef TIOCSWINSZ
	if (!(columns = shttyinfo.winsize.ws_col))
		columns = 80;
	if (!(lines = shttyinfo.winsize.ws_row))
		lines = 24;
#endif
}

void ainit() /**/
{
	alstackind = 0;		/* reset alias stack */
	alstat = 0;
	isfirstln = 1;
}

