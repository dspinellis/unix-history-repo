/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)help.c	1.2	(Berkeley)	8/14/85";
#endif not lint

/*
 * help - an easy way to find and use information
 *
 * Usage:  see definition of error()
 *
 * Files:  /usr/lib/help		root help subsystem
 *	   /usr/lib/help/log		log of system activity
 *	   /usr/lib/help/maint		help maintenance scripts
 *	   /usr/lib/help/config		defines the help system network
 *	   /usr/lib/help/src		nroff sources for /usr/lib/help/cat
 *	   /usr/lib/help/cat		root of system help topic files
 *	   "/index_{help,man,doc}	topics created by mkhelpindex
 *	   "/general			description of 'help'
 *	   "/*				all other files are 'help' text files
 *
 * Author:  John Kunze, UCB (sorting routines based on David Wasley's originals)
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <setjmp.h>
#include <signal.h>
/* #include <whoami.h> */	/* this would have defined BSD4_2 */
#define BSD4_2 1
#include <ctype.h>
#include <stdio.h>

					/* user-instruction return codes */

#define LIST_I	1
#define PASS_I	2
#define SAVE_I	3
#define TYPE_I	4
#define JUNK_I	5
#define BACK_I	6
#define LPRT_I	7
#define HELP_I	8
#define ROOT_I	9
#define YELL_I	10
#define QUIT_I	11
#define FIND_I	12
#define FLAG_I	13
#define NOOP_I	14
#define GOT_ONE	15

					/* symbols and macros */

#define HDBSIZE		256
#define HVSIZE		10
#define HELPROOT	"/usr/lib/help/cat"
#define TOPICINDEX	"index_help"
#define HELPMAINT	"../maint/do."
#ifdef	notdef
#define MANINDEX	"/usr/lib/whatis"
#else
#define MANINDEX	{ "/usr/lib/whatis", "/usr/man/whatis", NULL }
#endif
#define DOCINDEX	"/usr/lib/help/cat/index_doc"
#define HELPSAVE	"helpsave"
#ifdef	notdef
#define MAINTAINER	"help@ucbopal"
#else
#define MAINTAINER	"help"
#endif
#define HELPLOG		"/usr/lib/help/log"
#define DEFSHELL	"/bin/csh"
#define PERROR		{ perror("help"); exit(1); }
#define PUTNL		{ putchar('\n'); fflush(stdout); }
#define EXISTS(s)	(access(s, 0) == 0)
#define EXECABLE(s)	(access(s, 1) == 0)
#define WRITABLE(s)	(access(s, 2) == 0)
#define READABLE(s)	(access(s, 4) == 0)
#define DOT(s)		((s)[0]=='.'&&(s)[1]==0 ? 1 : 0)
#define DOTDOT(s)	((s)[0]=='.'&&(s)[1]=='.'&&(s)[2]==0 ? 1 : 0)
#define ROOT(s)		((s)[0]=='/'&&(s)[1]==0 ? 1 : 0)
#define isspecial(a)	(a == '\0' || a == '+' || a == '>' || a == '|')
#define lcase(a)	(isupper(a) ? tolower(a) : a)

					/* signal handling interface */

#if BSD4_2
struct sigvec vec;
#define GET_SIGPIPE	{ vec.sv_handler = onintr; sigvec(SIGPIPE, &vec, 0); }
#define SET_SIGPIPE	{ vec.sv_handler = SIG_DFL; sigvec(SIGPIPE, &vec, 0); }
#define NO_RUPTS	{ vec.sv_handler = SIG_IGN; sigvec(SIGINT, &vec, 0); }
#define OK_RUPTS	{ vec.sv_handler = SIG_DFL; sigvec(SIGINT, &vec, 0); }
#else
#define GET_SIGPIPE	signal(SIGPIPE, onintr)
#define SET_SIGPIPE	signal(SIGPIPE, SIG_DFL)
#define NO_RUPTS	signal(SIGINT, SIG_IGN)
#define OK_RUPTS	signal(SIGINT, SIG_DFL)
#ifndef MAXNAMLEN
#define MAXNAMLEN	255
#endif
#endif

					/* miscellaneous globals */

char hdbuf[HDBSIZE];		/* names of top level directores */
char *hvec[HVSIZE];		/* pointers to top level directories */
char **argp;			/* pointer to topic arguments */
char *dirlist;			/* unparsed list of root directories */
char cwd[MAXNAMLEN];		/* current directory */
char *dot, *dotdot;		/* tail parts of current and parent dirs */
char *subdir;			/* current subdirectory names */
short dirlevel = 0;		/* depth from root of current directory */
short keeppag;			/* for the >& command to keep pagination */
char *shell, shellprompt;	/* shell and its prompt */
char helpprompt[100];		/* help prompt string */
char indexprompt[100];		/* help-index prompt string */

/*
 * Topic names at the top (zero-th) directory level are stored permanently
 * as null terminated strings in the first segment of topicbuf, each of which
 * is pointed to by a pointer in the first segment of tptrs.  When a subtopic
 * at any directory level is under inspection, the second segment of topicbuf,
 * beginning with topicbuf[rtlen], contains the subtopic names, and the second
 * segment of tptrs, beginning with tptrs[subt], contains pointers to them.
 * At all times, tptrs[nt] contains zero to mark the end of the active segment.
 */

char topicbuf[4096];		/* null-terminated topic names */
char *tptrs[256];		/* pointers to topic names */
char **topics;			/* points to topics or subtopics */
int nt = 0, tlen = 0;		/* number and total length of topics */
int subt;			/* subtopic index in tptrs */
int rtlen;			/* length of root topics names */
int nhits = 0, hit = -1;	/* number and index of matched topics */

/*
 * Index references are stored in indexbuf, those for "help" preceding those
 * for "man", which start at iptrs[mansegment] and precede those for off-line
 * references starting at iptrs[docsegment].  Each iptrs[i] points to a pair
 * of null-terminated strings containing the first and second halves of a line.
 */

char *indexbuf;			/* names of index references */
char **iptrs;			/* pointers to index references */
int ni = 0, ilen = 0;		/* number and length of index refs */
int inhits = 0, ihit = -1;	/* number and index of matched index refs */
char *isrc, *idst;		/* partial match of index entry */
int mansegment;			/* beginning of UPM refs segment */
int docsegment;			/* beginning of off-line refs segment */

char line[BUFSIZ];		/* raw user instruction */
char *src, *dst, *dstarg;	/* source and dst parts of an instruction */
char fname[BUFSIZ];		/* full path name(s) of topic file */
short fnamect;			/* number of files in fname */
short interactive, iflag;	/* interactive session flag */
short number, quiet;		/* numbers accepted/printed, terse prompt */
char *more_d;			/* pointer to value of MORE env. variable */
char *progname;			/* name (argv[0]) of invoking program */
char *maintkey;			/* help maintenance key */

					/* miscellaneous routines */

char *getenv(), *strcpy(), *malloc(), *index(), *rindex();
FILE *outpipe();

main(argc,argv)
int argc;
char **argv;
{
	register int ins;		/* current user instruction */
	register int junkcount = 0;	/* how many times in a row bad ins. */

	setbuf(stdout, malloc(BUFSIZ));	/* speed up standard output */
	setbuf(stderr, malloc(BUFSIZ));	/* speed up error output */
	getoptions(argc, argv);		/* parse options */
	setgetenv();			/* make directory list, environment */

	/*
	 * main loop:  get instruction, execute
	 */
	for (ins = startup(); ins != QUIT_I; ins = nextins()) {
		if (ins != JUNK_I)
			junkcount = 0;
		switch (ins) {
		case LIST_I:
			list();
			break;
		case TYPE_I:
			if (isadir(fname)) {
				chwd(src);
				list();
			}
			else
				page();
			log('=');
			break;
		case BACK_I:
			chwd("..");
			list();
			break;
		case ROOT_I:
			chwd("/");
			list();
			break;
		case SAVE_I:
			save();
			log('>');
			break;
		case LPRT_I:
			lprt();
			log('|');
			break;
		case PASS_I:
			fflush(stdout);
			pass(src);
			break;
		case FLAG_I:
			flag(src, dst);
			break;
		case JUNK_I:
			printf("\nI%sdon't understand.\n",
				(junkcount++ ? " still " : " "));
			if (junkcount == 2)
				list();
			else if (junkcount > 2)
				comlist();
			break;
		case HELP_I:
			comlist();
			break;
		case YELL_I:
			yell();
			break;
		case FIND_I:
			find(src);
			log('+');
			break;
		case NOOP_I:
			break;
		default:
			puts("Unknown instruction - please report this.");
			exit(0);
			break;
		}
	}
	puts("Bye.");
}

save()			/* save a help file "src" in user file "dst" */
{
	register char *p;
	register FILE *destfile;
	register int lcount;
	char c;

	p = (EXISTS(dst) ? "appended" : "new file");
	if ((destfile = fopen(dst, "a")) == NULL)
		perror(dst);
	lcount = putfiles(NULL, destfile);
	fclose(destfile);
	printf("\nTopic \"%s\" is saved in \"%s\" (%s:  %d lines).\n",
		topics[hit], dst, p, lcount);
}

lprt()			/* lineprint (dst) all args in fname */
{
	register int i = 0;
	register char *fn;
	char *ap[HVSIZE];		/* arg pointers */

	ap[i++] = dst;
	if (strcmp(dst, "ipr") == 0)	/* kludge to force -p with ipr */
		ap[i++] = "-p";
	if (dstarg)		/* kludge to allow an option to lpr */
		ap[i++] = dstarg;
	fn = fname;
	while (fnamect--) {
		ap[i++] = fn;
		fn += strlen(fn) + 1;
	}
	ap[i] = 0;
	if (!fork()) {
		fputs("\n>> Executing [", stdout);
		for (i = 0; ap[i]; fputs(ap[i++], stdout))
			putchar(' ');
		puts(" ] ...");
		fflush(stdout);
		execvp(dst, ap);
		PERROR;
	}
	NO_RUPTS;
	wait(0);
	puts(">> Done.");
	OK_RUPTS;
}

yell()		/* send complaints or other input to the MAINTAINER of help */
{
	if (!fork()) {
		printf("\n%s\n%s\n%s\n",
	"Please enter your remarks.  The only way I will know you're done is if",
	"you enter a . (period) or control-d on a line by itself.  Don't forget!");
		fflush(stdout);
		execlp("mail", "mail", MAINTAINER, 0);
		PERROR;
	}
	NO_RUPTS;
	wait(0);
	puts("\nDuly noted.");
	OK_RUPTS;
}

log(insc)		/* to turn logging off, deny write access of HELPLOG */
char insc;			/* instruction code character to be written */
{
	long logtime;
	char *ctime();
	FILE *lp;

	if ((lp = fopen(HELPLOG, "a")) == NULL)
		return;
	time(&logtime);
	fprintf(lp, "%.12s  %c %s\n", ctime(&logtime) + 4, insc, src);
	fclose(lp);
}

getoptions(ac, av)			/* get command-line options */
int ac;			/* spaces need not separate -[dpm] from next arg */
register char **av;
{
	register char *p;

	interactive = isatty(1);
	progname = *av;
	for (p = progname; *p; p++);
	for (p--; p >= progname && *p != '/'; p--);
	progname = ++p;			/* set progname to its tail part */
	while (**++av == '-')
		switch (*(p = *av + 1)) {
		case 'd':
			if (*++p || *++av)
				dirlist = (*p ? p : *av);
			else
				error("Directory list must follow -d.");
			break;
		case 'p':
			if (*++p || *++av)
				progname = (*p ? p : *av);
			else
				error("Prompt string must follow -p.");
			break;
		case 'm':
			if (*++p || *++av)
				maintkey = (*p ? p : *av);
			else {
				maintkey = "default";
				av--;
			}
			break;
		case 'i':
			iflag = interactive = 1;
			break;
		case 'n':
			number = 1;
			break;
		case 'q':
			quiet = 1;
			break;
		default:
			printf("Unknown option -%c.\n", *p);
			break;
		}
	argp = av;
}

error(msg)		/* print a message for command-line errors */
char *msg;
{
	if (*msg)
		fprintf(stderr, "%s\n", msg);
	fprintf(stderr, "Usage:  help [ options ] [ topic [ subtopic [ subsubtopic [...] ] ] ]\n");
	fprintf(stderr, "Options are:\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s\n",
		"-d dirlist	override the default pool of help files",
		"-m key		do the help maintenance function given by key",
		"-p prompt	override the default prompt",
		"-i		force help to be interactive",
		"-n		use numbers in topic and index listings",
		"-q		suppress the instruction line before prompting");
	fprintf(stderr, "To get started just type \"help\".\n");
	exit(1);
}

helpmaint(key, dir, av)			/* invoke maintenance script */
char *key;				/* key specifying action */
char *dir;				/* first writable dir in HELPPOOL */
char **av;				/* topics, if any */
{
	char s[BUFSIZ];
	char *argv[BUFSIZ];
	register char **vp = argv;

	sprintf(s, "%s/%s%s", dir, HELPMAINT, key);
	if (!READABLE(s)) {
		printf("I don't know how to do \"%s\".\n", key);
		fflush(stdout);
		sprintf(s, "%s/%s%s", dir, HELPMAINT, "default");
	}
	*vp++ = "csh";
	*vp++ = "-f";
	*vp++ = s;
	*vp++ = dir;
	while (*av)
		*vp++ = *av++;
	*vp = 0;
	execv("/bin/csh", argv);
	PERROR;
}

setgetenv()	/* get directory list and shell, and set more -d for man */
{
	register char *p = dirlist, **vp;
	register int i = 0;
	char **myenv; char *t; int moredef = 0;
	extern char **environ;

	for (vp = environ; *vp; vp++)
		if (strncmp(*vp, "HELPPOOL=", 9) == 0 && !p)
			p = *vp + 9;
		else if (strncmp(*vp, "SHELL=", 6) == 0)
			shell = *vp + 6;
		else if (strncmp(*vp, "MORE=", 5) == 0)
			moredef++;
	if (p) {
		t = &hdbuf[0];
		while (*p) {
			hvec[i++] = t;
			while (*p == ':' || isspace(*p))
				p++;
			while (*p && *p != ':' && !isspace(*p))
				*t++ = *p++;
			*t++ = 0;
		}
	}
	if (!dirlist)
		hvec[i++] = HELPROOT;
	hvec[i] = 0;
	if (!shell)
		shell = DEFSHELL;
	shellprompt = (strcmp(shell, DEFSHELL) == 0 ? '%' : '$');
	if (number)
		sprintf(helpprompt,
		"\nTo see a topic, type its name or number, and RETURN; '%c' to quit, '?' for help.", shellprompt);
	else
		sprintf(helpprompt,
		"\nTo display a topic, type its name, and RETURN; type '%c' to quit, '?' for help.", shellprompt);
	if (number)
		sprintf(indexprompt,
		"\nTo display a subject, type its name or number, and RETURN; type '?' for help.", shellprompt);
	else
		sprintf(indexprompt,
		"\nTo display a subject, type its name, and RETURN; type '?' for help.", shellprompt);
	if (moredef)
		return;
	myenv = (char **) malloc((vp - environ + 2) * sizeof (char *));
	if (!myenv)
		PERROR;
	*myenv = (quiet ? "MORE=  " : "MORE=-d");
	more_d = *myenv + 5;		/* points to "-d" or "  " */
	for (i = 0, vp = myenv + 1; environ[i]; i++, vp++)
		*vp = environ[i];
	environ = myenv;
}

startup()	/* get topic named by args, return first instruction */
{
	register int i, ins;
	char **t, **u;

	if (maintkey) {
		for (i = 0; hvec[i]; i++)
			if (WRITABLE(hvec[i]) && isadir(hvec[i]))
				break;
		if (!hvec[i]) {
			fprintf(stderr, "You need write permission in at least one directory in HELPPOOL.\n");
			exit(1);
		}
		helpmaint(maintkey, hvec[i], argp);	/* no return */
	}
	for (i = 0; hvec[i]; i++)	/* collect first level (root) */
		getfiles(hvec[i]);	/* topics to be kept permanently */
	rtlen = tlen;			/* save root topic sizes */
	topics = tptrs;			/* active topic segment */
	vsort(tptrs);			/* sort -- replace dups with zero */
	t = u = tptrs;			/* kill off zeros */
	while (t < tptrs + nt)
		if (!*t)
			t++;
		else
			*u++ = *t++;
	*u = 0;				/* mark new end of first segment */
	nt = u - tptrs;			/* so tptrs[nt-1] is nil */
	subt = nt + 1;			/* mark start of subtopic segment */
	for (; *argp; argp++) {		/* go through topic arguments */
		if (!match(*argp))	/* if no match, try something else */
			if ((ins = whatnext(*argp)) != GOT_ONE)
				return ins;	/* user can escape this way */
		if (!chwd(topics[hit]))	/* if match, assume it's a directory */
			break;		/* not a directory, must be a file */
	}
	if (!*argp)
		return LIST_I;
	src = topics[hit];
	makefname(dirlevel, topics[hit]);
	page();
	if (!iflag)
		exit(0);
	interactive = 1;
	return NOOP_I;
}

whatnext(s)		/* match s with a file or find out what to do */
char *s;			/* if success, global src set from s */
{
	static char word[MAXNAMLEN];
	char rbuf[10];
	int wlen;

	strcpy(word, s);
	src = word;
	do {
		if (!interactive) {
			printf("\nThere is no topic \"%s\".  I'm looking in the index.\n", word);
			fflush(stdout);
			return FIND_I;
		}
		else if (nhits > 1) {
			printf("\nNot precise enough.  Enter more letters, or RETURN:  %s", word);
			fflush(stdout);
			wlen = strlen(word);
			if (gets(word + wlen) == NULL)
				return QUIT_I;
			if (strlen(word) <= wlen)	/* no new letters */
				return NOOP_I;
		}
		else {
			printf("\nThere is no topic \"%s\".  Shall I look in the index?  ", word);
			fflush(stdout);
			if (gets(rbuf) == NULL)
				return QUIT_I;
			if (*rbuf == 'y')
				return FIND_I;
			return NOOP_I;
		}
	} while (!match(word));
	src = topics[hit];
	return GOT_ONE;
}

char *cwdend = cwd;		/* end of current directory string */

chwd(s)				/* change directory routine */
register char *s;
{
	register char *p;
	register int i;

	if (DOT(s))
		return 1;
	if (DOTDOT(s)) {
		if (dirlevel == 0)
			return !printf("\nYou're at the top level already.\n");
		while (*--cwdend != '/');
		*cwdend = 0;
		dirlevel--;
	}
	else if (ROOT(s)) {
		if (dirlevel == 0)
			return !printf("\nYou're at the top level already.\n");
		dirlevel = 0;
	}
	else if (dirlevel > 0) {
		strcat(strcat(cwdend, "/"), s);
		if (!EXECABLE(cwd) || !isadir(cwd))
			return *cwdend = 0;
		while (*++cwdend);
		dirlevel++;
	}
	else {
		for (i = 0; hvec[i]; i++) {
			cwdend = strcpy(cwd, hvec[i]);
			while (*++cwdend);
			subdir = cwdend;
			strcat(strcat(cwdend, "/"), s);
			if (EXECABLE(cwd) && isadir(cwd))
				break;
			*cwdend = 0;
			subdir = 0;
		}
		if (!hvec[i])
			return 0;
		while (*++cwdend);
		dirlevel++;
	}

	/*
	 * reclaim subtopic storage, get new topics
	 */
	nhits = 0; hit = -1; tlen = rtlen;
	if (dirlevel > 0) {
		nt = subt;
		topics = tptrs + subt;
		getfiles(cwd);
		vsort(topics);
	}
	else {
		nt = subt - 1;
		topics = tptrs;
		subdir = 0;
	}
	return 1;
}

nextins()		 /* sets up globals:  src, dst, and fname */
{
	register char *p, *s;
	register int ins, got_one = 0;
	char c = 0;

	/*
	 * initialize fname, src, dst, and keeppag; get instruction
	 */
	if (!interactive)
		return QUIT_I;
	fname[0] = 0;
	src = dst = dstarg = 0;
	keeppag = 0;
	prompt();
	if (gets(line) == NULL)
		return QUIT_I;

	/*
	 * trim blanks from end and beginning of line
	 */
	for (p = line+strlen(line)-1; isspace(*p) && p >= line; p--);
	if (p < line)
		return NOOP_I;
	*++p = 0;
	for (p = line; isspace(*p); p++);

	/*
	 * parse zero operand instructions
	 */
	if (*p == '?')
		return HELP_I;
	if (*p == '/')
		return ROOT_I;
	if (DOT(p))
		return LIST_I;
	if (DOTDOT(p))
		return BACK_I;
	if (*p == '%' || *p == '$')
		return QUIT_I;
	if (*p == '<')
		return YELL_I;

	/*
	 * other instructions
	 */
	if (*p == '!') {
		src = ++p;
		return PASS_I;
	}
	if (*p == '*') {
		for (p++; isspace(*p); p++);
		if (*p)
			src = p;
		for (; *p && !isspace(*p); p++);
		if (*p)
			*p++ = 0;
		for (; *p && isspace(*p); p++);
		if (*p)
			dst = p;
		return FLAG_I;
	}
	if (*p == '=') {			/* = as topic */
		p++;
		if (hit < 0)
			return JUNK_I;
	}
	else if (number && isdigit(*p)) {
		for (s = p; *p; p++)
			if (!isdigit(*p))
				break;
		hit = atoi(s) - 1;
		if (hit < 0 || hit >= (dirlevel == 0 ? nt : nt - subt)) {
			printf("\nThere is no topic numbered %d.\n", atoi(s));
			return NOOP_I;
		}
		got_one++;
		src = topics[hit];
		makefname(dirlevel, topics[hit]);
		for (; isspace(*p); p++);
	}
	else if (isalpha(*p) || *p == '.' || *p == '-') {	/* put topic name in src */
		src = p;
		for (; !isspecial(*p) && !isspace(*p); p++);
		for (; isspace(*p); p++)
			*p = 0;			/* make sure it ends */
	}
	c = *p;
	*p++ = 0;
	if (!src) {			/* no topic, see if default exists */
		if (hit < 0) {
			printf("\nYou need to give a topic name for that.");
			return JUNK_I;
		}
		src = topics[hit];
	}
	if (c == '>' || c == '|') {	/* more args allowed */
		for (; isspace(*p); p++);
		if (*p == '&') {
			keeppag = 1;
			for (p++; isspace(*p); p++);
		}
		if (!*p)
			strcat(p, (c == '>' ? HELPSAVE : "lpr"));
		dst = p;
		for (; *p && !isspace(*p); p++);
		for (; *p && isspace(*p); p++)
			*p = 0;		/* terminate dst */
		if (*p) {
			dstarg = p;
			for (; *p && !isspace(*p); p++);
			*p = 0;			/* terminate dstarg */
		}
	}

	/*
	 * instructions requiring src
	 */
	if (c == '+')
		return FIND_I;
	if (!got_one) {
		if (!match(src) && (ins = whatnext(src)) != GOT_ONE)
			return ins;
		src = topics[hit];
		makefname(dirlevel, topics[hit]);
	}
	if (!c)
		return TYPE_I;
	if (c == '|')
		return LPRT_I;
	if (c == '>')
		return SAVE_I;
	return JUNK_I;
}

prompt()					/* prompt user */
{
	register char *p;

	if (!quiet)
		fputs(helpprompt, stdout);
	fputs("\n(", stdout);
	fputs(progname, stdout);
	if (subdir)
		for (p = subdir; *p; p++)
			if (*p == '/')
				putchar(' ');
			else
				putchar(*p);
	fputs(") ", stdout);
	fflush(stdout);
}

substr(s, abbr)			/* returns 1 if abbr abbreviates s */
register char *s, *abbr;
{
	for (; *s == *abbr && *abbr; s++, abbr++);
	return !*abbr;
}

fsubstr(s, abbr)		/* returns 1 if abbr abbreviates lcased s */
register char *s, *abbr;
{
	for (; lcase(*s) == *abbr && *abbr; s++, abbr++);
	return !*abbr;
}

getfiles(dname)			/* fill topicbuf and tptrs */
char *dname;
{
	struct direct dbuf;
	register struct direct *ep = &dbuf;	/* directory entry pointer */
	register int i;
#if BSD4_2
	DIR *dp;
#define OPENDIR(s)	((dp = opendir(s)) != NULL)
#define DIRLOOP(s)	for (s = readdir(dp); s != NULL; s = readdir(dp))
#define PATHSIZE 256
#define PATHSIZE 256
#define MAXDLEN		ep->d_namlen
#define CLOSEDIR	closedir(dp)
#else
	int fd;
#define OPENDIR(s)	((fd = open(s, 0)) >= 0)
#define DIRLOOP(s)	while (read(fd, s, sizeof *s) == sizeof *s)
#define MAXDLEN		DIRSIZ
#define CLOSEDIR	close(fd)
#endif

	if (!OPENDIR(dname))
		return perror(dname);
	tptrs[nt] = &topicbuf[tlen];
	DIRLOOP(ep) {
		if (ep->d_name[0] == NULL || ep->d_ino == 0
			|| DOT(ep->d_name) || DOTDOT(ep->d_name))
				continue;
		tptrs[nt++] = &topicbuf[tlen];
		for (i = 0; i < MAXDLEN && ep->d_name[i]; tlen++, i++)
			topicbuf[tlen] = ep->d_name[i];
		topicbuf[tlen++] = 0;
	}
	tptrs[nt] = 0;
	CLOSEDIR;
}

isadir(name)
char *name;
{
	struct stat buf;

	stat(name, &buf);
	return buf.st_mode & S_IFDIR;
}

jmp_buf jmpenv;

onintr()			/* catch broken pipe signals */
{
	NO_RUPTS;
	SET_SIGPIPE;
	longjmp(jmpenv, 1);
}

wrapup(fp)			/* close a file pointer and wait for child */
FILE *fp;
{
	fclose(fp);
	wait(0);
	OK_RUPTS;
}

int firstime = 1;		/* for first topic listing */

list()				/* list topics in 4 columns */
{
	register int col, row, i, last, nrows;
	FILE *more;

	fflush(stdout);
	NO_RUPTS;
	if ((more = outpipe()) == NULL)
		PERROR;
	if (setjmp(jmpenv)) {
		wrapup(more);
		return;
	}
	GET_SIGPIPE;
	if (firstime) {
		fprintf(more, "\n%s\n%s\n",
		"Here is a list of topics I know about.",
		"If you don't see the topic you want, I can look for it in the index.");
		if (match("general"))
			fprintf(more, "For a general introduction, please see \"general\" below.\n");
		firstime = 0;
	}
	fputc('\n', more);
	last = (dirlevel > 0 ? nt - subt : nt);
	nrows = last / 4 + (last % 4 != 0 ? 1 : 0);
	for (row = 0; row < nrows; row++)
		for (i = row, col = 0; col < 4; i += nrows, col++) {
			if (i >= last) {
				fputc('\n', more);
				col = 3;
			}
			else if (number)
				fprintf(more, "%3d%c%-14.14s %c",
					i + 1,
					(hit == i ? '=' : ' '),
					topics[i], (col == 3 ? '\n' : ' '));
			else
				fprintf(more, "%c%-17.17s %c",
					(hit == i ? '=' : ' '),
					topics[i], (col == 3 ? '\n' : ' '));
		}
	wrapup(more);
}

#define IBSIZE	16384
#define IPSIZE	512

find(s)
char *s;
{
	register char *p;
	register int i;
	FILE *fp, *popen();
	char sbuf[BUFSIZ];

	if (!iptrs) {			/* malloc storage once and for all */
		indexbuf = (char *) malloc(IBSIZE + BUFSIZ);
		iptrs = (char **) malloc((IPSIZE + 32) * sizeof(char *));
		if (iptrs == 0 || indexbuf == 0) {
			fprintf(stderr, "No index space.\n");
			exit(1);
		}
	}
	ni = 0; ilen = 0;
	for (i = 0; hvec[i]; i++) {
		sprintf(sbuf, "%s/%s", hvec[i], TOPICINDEX);
		if ((fp = fopen(sbuf, "r")) == NULL) {
			if (strcmp(hvec[i], HELPROOT) == 0) {
				perror(sbuf);
				exit(1);
			}
			continue;
		}
		getrefs(fp, 0);
		fclose(fp);
	}
#ifdef	notdef
	if ((fp = fopen(MANINDEX, "r")) == NULL) {
		perror(MANINDEX);
		exit(1);
	}
	mansegment = ni;
	getrefs(fp, 1);
	fclose(fp);
#else
	{
		static char *manindex[] = MANINDEX;
		register char **mi;

		mansegment = ni;

		for (mi = manindex; *mi != NULL; mi++)
			if ((fp = fopen(*mi, "r")) != NULL) {
				getrefs(fp, 1);
				fclose(fp);
				break;
			}
	}
#endif
	docsegment = ni;
	if ((fp = fopen(DOCINDEX, "r")) != NULL) {
		getrefs(fp, 0);
		fclose(fp);
	}
	if (ni == 0)
		return printf("\nNo relevant material; your request has been logged.\n");
	putrefs();
	if (!interactive)
		exit(0);
	while ((i = selectref()) != QUIT_I)
		switch (i) {
		case LIST_I:
			putrefs();
			break;
		case HELP_I:
			icomlist();
			break;
		case ROOT_I:
			chwd("/");
		case BACK_I:
			list();
			return;
		case YELL_I:
			yell();
			break;
		case PASS_I:
			fflush(stdout);
			pass(isrc);
			break;
		case FLAG_I:
			flag(isrc, idst);
			break;
		default:
			break;
		}
	puts("Bye.");
	exit(0);
}

icomlist()
{
	if (number)
		puts("\nTo see a subject, type its name, a unique abbreviation, or its number.");
	else
		puts("\nTo see a subject, type its name or a unique abbreviation.");
	puts("Other commands are:");
	printf("  %c             quit from help and return to the shell (control-d works also)\n", shellprompt);
	printf("  subject       display a \"subject\", whose name%syou supply\n",
		(number ? " or number " : " "));
	puts("  ?             display this command list");
	puts("  .             list subject references found");
	puts("  ..            go back to the previous list of help topics");
	puts("  /             back up to and list the top level of topics");
	puts("  <             send comments or other input to the maintainer of help");
	puts("  !command      do a Unix command and then return to help");
	puts("  * flag on/off set a \"flag\" on or off to adjust the behavior of help");
	puts("                (type * by itself for a list of flags you can use)");
	puts("The Unix command in brackets below each subject will display the same");
	puts("information that I do.  Sometimes information exists only off-line and I");
	puts("have nothing to show you; try the local distributor of printed documentation.");
}

getrefs(fp, upm)		/* get references to src from indexes qq.v. */
FILE *fp;
int upm;			/* whether looking at upm database "whatis" */
{
	/*
	 * indexbuf	str0\0str1\0str2\0str3\0 ... str(ni-1)\0
	 * iptrs	^     ^     ^     ^      ... ^          0
	 */
	register char *p, *ref;
	char s[MAXNAMLEN];	/* lower case version of src */
	char t[BUFSIZ];		/* temporary line buffer */
	int preamble = !upm;

	if (ilen > IBSIZE || ni > IPSIZE)
		return puts("Index space full.");
	for (p = src, ref = s; *p; p++, ref++)	/* ref becomes lower case */
		*ref = lcase(*p);			/* version of src */
	*ref = 0;
	ref = s;
	iptrs[ni] = &indexbuf[ilen];
	while (fgets(t, BUFSIZ, fp) != NULL) {
		if (preamble) {		/* indexes all have preamble to skip */
			if ((p = index(t, '-')) && fsubstr(p, "------"))
				preamble = 0;		/* preamble over */
			continue;
		}
		for (p = t; *p; p++)
			if (lcase(*p) == *ref && fsubstr(p, ref))
				break;
		if (!*p)
			continue;
		iptrs[ni++] = &indexbuf[ilen];
		for (p = t; *p && isspace(*p); p++);
		for (; *p && *p != ' '; ilen++, p++)
			indexbuf[ilen] = *p;
		if (upm)
			for (; *p && *p != '\t'; ilen++, p++)
				if (*p == '-' && *(p + 1) == ' ')
					break;	/* cover glitches in MANINDEX */
				else
					indexbuf[ilen] = *p;
		indexbuf[ilen++] = 0;
		for (; *p && isspace(*p); p++);
		if (upm && *p == '-' && *(p + 1) == ' ')
			p += 2;
		iptrs[ni++] = &indexbuf[ilen];
		for (; *p; ilen++, p++)
			indexbuf[ilen] = *p;
		indexbuf[ilen++] = 0;
	}
	iptrs[ni] = 0;
	fclose(fp);
}

putrefs()			/* list references stored in iptrs */
{
	register int i;
	register char *p, *format;
	FILE *more;

	NO_RUPTS;
	if ((more = outpipe()) == NULL)
		PERROR;
	if (setjmp(jmpenv)) {
		wrapup(more);
		return;
	}
	GET_SIGPIPE;
	format = (number ? "%3d  %s\t\t[ " : "%s\t\t[ ");
	fprintf(more, "\nThese subjects appear to be related to \"%s\".\n\n", src);
	for (i = 0; i < ni; i += 2) {
		if (number)
			fprintf(more, format, (i / 2 + 1), iptrs[i + 1]);
		else
			fprintf(more, format, iptrs[i + 1]);
		if (i < mansegment) {
			fputs("help ", more);
			for (p = iptrs[i]; *p; p++)
				putc((*p == '/' ? ' ' : *p), more);
		}
		else if (i >= docsegment)
			fprintf(more, "Off-line only document:  %s", iptrs[i]);
		else {
			fputs("man ", more);
			for (p = iptrs[i]; *p && *p != '('; p++);
			for (p++; *p != ')'; p++)
				putc(lcase(*p), more);
			putc(' ', more);
			for (p = iptrs[i]; *p != ',' && *p != ' '; p++)
				putc(*p, more);
		}
		fputs(" ]\n", more);
	}
	wrapup(more);
}

FILE *
outpipe()		/* return a file descriptor pointing to "more" */
{
	int fildes[2];
	FILE *fp, *fdopen();

	if (pipe(fildes) == -1) 
		PERROR;
	if (!fork()) {
		OK_RUPTS;
		close(fildes[1]);
		fclose(stdin);
		if (dup(fildes[0]) == -1)
			PERROR;
		close(fildes[0]);
		execlp("more", "more", "-s", 0);
		PERROR;
	}
	close(fildes[0]);
	return fdopen(fildes[1], "w");
}

iprompt()				/* prompt user - index version */
{
	if (!quiet)
		fputs(indexprompt, stdout);
	printf("\n(%s-index %s) ", progname, src);
	fflush(stdout);
}

selectref()			/* read user instruction for indexing */
{
	register char *p, *s;
	register int ins;
	char sbuf[BUFSIZ];

	isrc = idst = 0;
	iprompt();
	if (gets(sbuf) == NULL)
		exit(0);
	for (p = sbuf+strlen(sbuf)-1; isspace(*p) && p >= sbuf; p--);
	if (p < sbuf)
		return NOOP_I;
	*++p = 0;				/* blanks now trimmed */
	for (p = sbuf; isspace(*p); p++);
	if (*p == '%' || *p == '$')
		return QUIT_I;
	if (DOT(p))
		return LIST_I;
	if (DOTDOT(p))
		return BACK_I;
	if (*p == '?')
		return HELP_I;
	if (*p == '/')
		return ROOT_I;
	if (*p == '<')
		return YELL_I;
	if (*p == '!') {
		isrc = ++p;
		return PASS_I;
	}
	if (*p == '*') {
		for (p++; isspace(*p); p++);
		if (*p)
			isrc = p;
		for (; *p && !isspace(*p); p++);
		if (*p)
			*p++ = 0;
		for (; *p && isspace(*p); p++);
		if (*p)
			idst = p;
		return FLAG_I;
	}
	for (s = p; *s; s++)
		if (!isdigit(*s))
			break;
	if (!*s && number) {
		ihit = 2 * atoi(p) - 1;
		if (ihit < 1 || ihit > ni) {
			printf("\nThere is no subject numbered %d.\n", atoi(p));
			return NOOP_I;
		}
	}
	else
		if ((ins = iwhatnext(p)) != GOT_ONE)
			return ins;
	if (ihit < mansegment) {
		makefname(0, iptrs[ihit - 1]);
		page();
		return NOOP_I;
	}
	if (ihit >= docsegment && docsegment > 0) {
		puts("\nSorry, that reference is not available on the computer.");
		return NOOP_I;
	}
	if (!fork()) {
		for (s = sbuf, p = iptrs[ihit - 1]; *p != ' ' && *p != ','; p++)
			*s++ = *p;
		for (; *p != '('; p++);
		for (*s++ = 0, p++; *p != ')'; p++)
			*s++ = lcase(*p);
		for (*s-- = 0; *s; s--);
		execlp("man", "man", ++s, sbuf, 0);
		PERROR;
	}
	NO_RUPTS;
	wait(0);
	OK_RUPTS;
	return NOOP_I;
}

iwhatnext(s)				/* indexing version of whatnext */
char *s;
{
	static char word[MAXNAMLEN];
	int wlen;

	strcpy(word, s);
	isrc = word;
	while (!imatch(word))
		if (inhits > 1) {
			printf("\nNot precise enough.  Enter more letters, or RETURN:  %s", word);
			fflush(stdout);
			wlen = strlen(word);
			if (gets(word + wlen) == NULL)
				return QUIT_I;
			if (strlen(word) <= wlen)	/* no new letters */
				return NOOP_I;
		}
		else {
			printf("\nThere is no subject \"%s\".\n", word);
			return NOOP_I;
		}
	isrc = iptrs[ihit];
	return GOT_ONE;
}

imatch(abbr)		/* indexing version of match (on unsorted list) */
char *abbr;
{
	register char **t;
	register char *p = abbr;
	register char **last;

	last = iptrs + (docsegment < 0 ? ni : docsegment);
	inhits = 0;
	for (t = iptrs + 1; t < last; t += 2)
		if (**t != *p)		/* quickly check first character */
			continue;
		else if (substr(*t, abbr)) {
			inhits++;
			ihit = t - iptrs;
			if (strcmp(*t, abbr) == 0)
				return (inhits = 1);
		}
	return (inhits == 1);
}

makefname(dirlev, tail)	/* build fname from cwd and tail, return no. matched */
int dirlev;				/* directory level */
char *tail;				/* tail of pathname to use */
{
	register int i;
	register char *p;

	if (dirlev > 0) {
		sprintf(fname, "%s/%s", cwd, tail);
		fnamect = (EXISTS(fname) ? 1 : 0);
		return fnamect;
	}
	fnamect = 0;	/* count of number of dirs. where tail exists */
	p = fname;	/* full names of files with tails as above */
	for (i = 0; hvec[i]; i++) {
		sprintf(p, "%s/%s", hvec[i], tail);
		if (EXISTS(p)) {
			fnamect++;
			p += strlen(p) + 1;
		}
	}
	return fnamect;
}

pass(s)			/* replace = with fname and send to system */
register char *s;	/* allow \= to pass as =, but \x passes as \x */
{
	register char *p;
	register int escaped = 0;
	char buf[BUFSIZ];

	PUTNL;
	for (p = buf; *s; s++) {
		if (escaped) {
			if (*s != '=')
				*p++ = '\\';
			*p++ = *s;
			escaped = 0;
		}
		else if (*s == '\\')
			escaped = 1;
		else if (*s == '=' && hit >= 0) {
			makefname(dirlevel, topics[hit]);
			strcpy(p, fname);
			for (; *p; p++);
		}
		else
			*p++ = *s;
	}
	*p = 0;
	if (!fork()) {
		putchar('\n');
		execl(shell, shell, "-c", buf, 0);
		PERROR;
	}
	NO_RUPTS;
	wait(0);
	OK_RUPTS;
}

flag(f, val)		/* set flag on or off; 0 in src and dst gives help */
char *f;
char *val;
{
	if (!f) {
		puts("\nCurrent flag settings and their meanings are:");
		printf("  number\t%suse numbers in topic and index listings\n",
			(number ? "on\t" : "off\tdo not "));
		printf("  quiet \t%ssuppress the instruction line before prompting\n",
			(quiet ? "on\t" : "off\tdo not "));
		return;
	}
	if (substr("number", f)) {
		printf("\nnumber:  was %s,", (number ? "on" : "off"));
		if (!val) 
			number = (number ? 0 : 1);		/* toggle */
		else
			number = (val[1] == 'n' ? 1 : 0);
		printf(" is %s.\n", (number ? "on" : "off"));
	}
	else if (substr("quiet", f)) {
		printf("\nquiet:  was %s,", (quiet ? "on" : "off"));
		if (!val) 
			quiet = (quiet ? 0 : 1);		/* toggle */
		else
			quiet = (val[1] == 'n' ? 1 : 0);
		printf(" is %s.\n", (quiet ? "on" : "off"));
		if (more_d)
			strcpy(more_d, (quiet ? "  " : "-d"));
	}
	else
		puts("\nThat is not a flag I know about.  Type * for a complete list.");
}

page()			/* print a help file with more or run program */
{
	char c[2];
	FILE *fp, *more;

	if ((fp = fopen(fname, "r")) == NULL) {
		perror(fname);
		return;		/* try to continue on this error */
	}
	c[0] = getc(fp);	/* check first 2 characters of first file */
	c[1] = getc(fp);	/* looking for magic characters and numbers */
				/* (should check more than just the first) */
	if (runs(c)) {		/* if a program, run it and then return */
		fclose(fp);
		OK_RUPTS;
		return;
	}
	rewind(fp);
	NO_RUPTS;
	if ((more = outpipe()) == NULL)
		PERROR;
	if (setjmp(jmpenv)) {
		fclose(fp);
		wrapup(more);
		return;
	}
	GET_SIGPIPE;
	putfiles(fp, more);
	fclose(fp);
	wrapup(more);
}

#define isblank(s)	(*s == '\n')
#define sqspace(s)	{ if (!isblank(s) || !wasblank) fputs(s, out); else linect--; wasblank = isblank(s); }

putfiles(in, out)			/* print file(s) onto out file */
FILE *in;				/* first of the input files */
FILE *out;
{
	register char *fn = fname;
	register int linectsum = 0;

	while (fnamect--) {
		if (in == NULL && (in = fopen(fn, "r")) == NULL)
			perror(fn);
		linectsum += filter(in, out);
		fclose(in);
		in = NULL;
		fn += strlen(fn) + 1;
	}
	return linectsum;
}

filter(in, out)		/* filter out multiple blank lines and page banners */
FILE *in;
FILE *out;
{
	char lbuf[BUFSIZ];
	register int lineno, wasblank;
	register char *s = lbuf;
	int linect, i;
	char *p;

	/* check page one for proper headers; if none then keep pagination */
	wasblank = 0;
	for (lineno = 1; fgets(s, BUFSIZ, in) != NULL; lineno++)
		if (!isblank(s) || lineno >= 4)
			break;
	if (!(lineno == 4 &&
		(((p = index(s, 'H')) && substr(p, "HELP"))	/* help */
		|| index(s, ')') != rindex(s, ')'))))		/* man */
			keeppag = 1;			/* criteria not met */
	if (lineno == 1 && (*s = '#' || *s == ':')) {
		keeppag = 1;			/* this is a script file */
		lineno = 0;
		fputc('\n', out);
	}
	else {
		for (i = lineno - 1; i; i--)
			if (keeppag)
				fputc('\n', out);
			else
				sqspace("\n");
		if (keeppag)
			fputs(s, out);
		else
			sqspace(s);
	}
	linect = lineno;
	while (fgets(s, BUFSIZ, in) != NULL) {
		lineno++, linect++;
		if (lineno > 66)
			lineno = 1;
		if (keeppag)	/* this global overrides our page 1 analysis */
			fputs(s, out);
		else if (lineno > 7 && lineno < 60	/* skip page banners */
				|| linect < 8)		/* let first 7 go */
			sqspace(s)
	}
	return linect;
}

runs(c)		/* run program or script named by fname, else return 0 */
char c[];
{
	int *magic = (int *)c;

	if (c[0] != '#' && c[0] != ':'
		&& *magic != 0413 && *magic != 0410 && *magic != 0407)
			return 0;
	if (!fork()) {
		if (*magic == 0413 || *magic == 0410 || *magic == 0407)
			execv(fname, 0);
		else if (c[0] == '#')
			execlp("csh", "csh", "-f", fname, 0);
		else
			execlp("sh", "sh", fname, 0);
		perror(fname);
	}
	NO_RUPTS;
	wait(0);
	OK_RUPTS;
	return 1;
}

comlist()			/* list help instructions available */
{
	if (number)
		puts("\nTo see a topic, type its name, a unique abbreviation, or its number.");
	else
		puts("\nTo see a topic, type its name or a unique abbreviation.");
	puts("Here is a list of commands:");
	printf("  %c             quit from help and return to the shell (control-d works also)\n", shellprompt);
	printf("  topic         display a \"topic\", whose name%syou supply\n",
		(number ? " or number " : " "));
	puts("  topic +       see what more is known about a topic");
	puts("  topic > file  save a topic in a file (you supply the name \"file\")");
	puts("  topic | lpr   paginate and print a topic on the lineprinter");
	puts("  topic >& file save a topic in a file with pagination");
	puts("  ?             display this command list");
	puts("  .             list topics at the current level");
	puts("  ..            back up to and list the next higher level of topics");
	puts("  /             back up to and list the top level of topics");
	puts("  <             send comments or other input to the maintainer of help");
	puts("  !command      do a Unix command and then return to help");
	puts("  * flag on/off set a \"flag\" on or off to adjust the behavior of help");
	puts("                (type * by itself for a list of flags you can use)");
	puts("If you enter no topic in a command or just an equals sign (=),");
	puts("the most recent topic at this level is used.");
}

match(abbr)		/* find a match for abbr in current directory */
char *abbr;
{
	register char **t;
	register char *p = abbr;

	nhits = 0;
	for (t = topics; *t; t++)	/* find first string beginning */
		if (**t == *p)		/* with same letter */
			break;
	for (; *t && **t == *p; t++)
		if (substr(*t, abbr)) {
			nhits++;
			hit = t - topics;
			if (strcmp(*t, abbr) == 0)
				return (nhits = 1);
		}
	return (nhits == 1);
}

/*
 * radix sort of an alphanumeric list, identical keys deleted
 * Originally by D. Wasley, July 1980
 */

#define MSB	0100

vsort(list)
char *list[];
{
	char **endlist = tptrs + nt - 1;
	int offset = 0;

	if (endlist > list)
		_sortb(list, endlist, 0);		/* recursive sort */
}

_sortb(list, endlist, offset)
char **list, **endlist; int offset;
{
	register char **high, c;

	_sortr(list, endlist, offset, MSB);	/* radix sort on this char */
	while (list < endlist) {		/* now sort each sublist that */
		c = (*list)[offset];		/* starts with a common char */
		high = list;
		while ((*++high)[offset] == c && high <= endlist) ;
		if (high  - list > 1) {
			if (c)
				_sortb(list, high-1, offset+1);
			else			/* kill off identical keys */
				for (list++; list < high; list++)
					*list = 0;
		}
		list = high;
	}
}

_sortr(list, endlist, offset, mask)
int offset, mask; char **list, **endlist;
{
	register char **low, **high, *temp;

	low = list;
	high = endlist;
	while (low < high) {
		while (low < endlist && ((*low)[offset] & mask) == 0)
			low++;
		while (high > list && ((*high)[offset] & mask) != 0)
			high--;
		if (high > low) {
			temp = *high;
			*high = *low;
			*low = temp;
		}
	}
	if ((mask >>= 1) != 0) {	/* redefine mask and sort sublists */
		if (endlist > low)
			_sortr(low, endlist, offset, mask);
		if (high > list)
			_sortr(list, high, offset, mask);
	}
}
