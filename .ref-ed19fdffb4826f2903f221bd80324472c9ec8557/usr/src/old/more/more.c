static	char *sccsid = "@(#)more.c	4.4 (Berkeley) 81/04/23";

/*
** more.c - General purpose tty output filter and file perusal program
**
**	by Eric Shienbrood, UC Berkeley
**
**	modified by Geoff Peck, UCB to add underlining, single spacing
**	modified by John Foderaro, UCB to add -c and MORE environment variable
*/

#include <whoami.h>
#ifdef V6
#include <retrofit.h>
#endif
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <sgtty.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <local/uparm.h>

/* Help file will eventually go in libpath(more.help) on all systems */

#ifdef INGRES
#define VI		"/usr/bin/vi"
#define HELPFILE	"/mntp/doucette/more/more.help"
#endif

#ifndef INGRES
#ifndef HELPFILE
#define HELPFILE	libpath(more.help)
#endif
#define VI		binpath(vi)
#endif

#define Fopen(s,m)	(Currline = 0,file_pos=0,fopen(s,m))
#define Ftell(f)	file_pos
#define Fseek(f,off)	(file_pos=off,fseek(f,off,0))
#define Getc(f)		(++file_pos, getc(f))
#define Ungetc(c,f)	(--file_pos, ungetc(c,f))

#ifdef V6
#define MBIT	RAW
#define CBREAK	~RAW
#else
#define MBIT	CBREAK
#define stty(fd,argp)	ioctl(fd,TIOCSETN,argp)
#endif

#define TBUFSIZ	1024
#define LINSIZ	256
#define ctrl(letter)	('letter' & 077)
#define RUBOUT	'\177'
#define ESC	'\033'
#define QUIT	'\034'

struct sgttyb 	otty;
long		file_pos, file_size;
int		fnum, no_intty, no_tty, slow_tty;
int		dum_opt, dlines, onquit(), end_it();
#ifdef SIGTSTP
int		onsusp();
#endif
int		nscroll = 11;	/* Number of lines scrolled by 'd' */
int		fold_opt = 1;	/* Fold long lines */
int		stop_opt = 1;	/* Stop after form feeds */
int		ssp_opt = 0;	/* Suppress white space */
int		ul_opt = 1;	/* Underline as best we can */
int		promptlen;
int		Currline;	/* Line we are currently at */
int		startup = 1;
int		firstf = 1;
int		notell = 1;
int		bad_so;	/* True if overwriting does not turn off standout */
int		inwait, Pause, errors;
int		within;	/* true if we are within a file,
			false if we are between files */
int		hard, dumb, noscroll, hardtabs, clreol;
int		catch_susp;	/* We should catch the SIGTSTP signal */
char		**fnames;	/* The list of file names */
int		nfiles;		/* Number of files left to process */
char		*shell;		/* The name of the shell to use */
int		shellp;		/* A previous shell command exists */
char		ch;
jmp_buf		restore;
char		obuf[BUFSIZ];	/* stdout buffer */
char		Line[LINSIZ];	/* Line buffer */
int		Lpp = 24;	/* lines per page */
char		*Clear;		/* clear screen */
char		*eraseln;	/* erase line */
char		*Senter, *Sexit;/* enter and exit standout mode */
char		*ULenter, *ULexit;	/* enter and exit underline mode */
char		*chUL;		/* underline character */
char		*chBS;		/* backspace character */
char		*Home;		/* go to home */
char		*cursorm;	/* cursor movement */
char		cursorhome[40];	/* contains cursor movement to home */
char		*EodClr;	/* clear rest of screen */
char		*tgetstr();
int		Mcol = 80;	/* number of columns */
int		Wrap = 1;	/* set if automargins */
long		fseek();
char		*getenv();
struct {
    long chrctr, line;
} context, screen_start;
extern char	PC;		/* pad character */
extern short	ospeed;


main(argc, argv)
int argc;
char *argv[];
{
    register FILE	*f;
    register char	*s;
    register char	*p;
    register char	ch;
    register int	left;
    int			prnames = 0; 
    int			initopt = 0;
    int			srchopt = 0;
    int			clearit = 0;
    int			initline;
    char		initbuf[80];
    FILE		*checkf();

    nfiles = argc;
    fnames = argv;
    initterm ();
    if(s = getenv("MORE")) argscan(s);
    while (--nfiles > 0) {
	if ((ch = (*++fnames)[0]) == '-') {
	    argscan(*fnames+1);
	}
	else if (ch == '+') {
	    s = *fnames;
	    if (*++s == '/') {
		srchopt++;
		for (++s, p = initbuf; p < initbuf + 79 && *s != '\0';)
		    *p++ = *s++;
		*p = '\0';
	    }
	    else {
		initopt++;
		for (initline = 0; *s != '\0'; s++)
		    if (isdigit (*s))
			initline = initline*10 + *s -'0';
		--initline;
	    }
	}
	else break;
    }
    /* allow clreol only if Home and eraseln and EodClr strings are
     *  defined, and in that case, make sure we are in noscroll mode
     */
    if(clreol)
    {
	if ((*Home == '\0') || (*eraseln == '\0') || (*EodClr == '\0'))
	    clreol = 0;
	else noscroll = 1;
    }

    if (dlines == 0)
	dlines = Lpp - (noscroll ? 1 : 2);
    left = dlines;
    if (nfiles > 1)
	prnames++;
    if (!no_intty && nfiles == 0) {
	fputs("Usage: ",stderr);
	fputs(argv[0],stderr);
	fputs(" [-dfln] [+linenum | +/pattern] name1 name2 ...\n",stderr);
	exit(1);
    }
    else
	f = stdin;
    if (!no_tty) {
	signal(SIGQUIT, onquit);
	signal(SIGINT, end_it);
#ifdef SIGTSTP
	if (signal (SIGTSTP, SIG_IGN) == SIG_DFL) {
	    signal(SIGTSTP, onsusp);
	    catch_susp++;
	}
#endif
	stty (2, &otty);
    }
    if (no_intty) {
	if (no_tty)
	    copy_file (stdin);
	else {
	    if ((ch = Getc (f)) == '\f')
		doclear();
	    else {
		Ungetc (ch, f);
		if (noscroll && (ch != EOF)) {
		    if (clreol)
			home ();
		    else
			doclear ();
		}
	    }
	    if (srchopt)
	    {
		search (initbuf, stdin, 1);
		if (noscroll)
		    left--;
	    }
	    else if (initopt)
		skiplns (initline, stdin);
	    screen (stdin, left);
	}
	no_intty = 0;
	prnames++;
	firstf = 0;
    }

    while (fnum < nfiles) {
	if ((f = checkf (fnames[fnum], &clearit)) != NULL) {
	    context.line = context.chrctr = 0;
	    Currline = 0;
	    if (firstf) setjmp (restore);
	    if (firstf) {
		firstf = 0;
		if (srchopt)
		{
		    search (initbuf, f, 1);
		    if (noscroll)
			left--;
		}
		else if (initopt)
		    skiplns (initline, f);
	    }
	    else if (fnum < nfiles && !no_tty) {
		setjmp (restore);
		left = command (fnames[fnum], f);
	    }
	    if (left != 0) {
		if ((noscroll || clearit) && (file_size != 0x7fffffffffffffffL))
		    if (clreol)
			home ();
		    else
			doclear ();
		if (prnames) {
		    if (bad_so)
			erase (0);
		    if (clreol)
			cleareol ();
		    pr("::::::::::::::");
		    if (promptlen > 14)
			erase (14);
		    printf ("\n");
		    if(clreol) cleareol();
		    printf("%s\n", fnames[fnum]);
		    if(clreol) cleareol();
		    printf("::::::::::::::\n", fnames[fnum]);
		    if (left > Lpp - 4)
			left = Lpp - 4;
		}
		if (no_tty)
		    copy_file (f);
		else {
		    within++;
		    screen(f, left);
		    within = 0;
		}
	    }
	    setjmp (restore);
	    fflush(stdout);
	    fclose(f);
	    screen_start.line = screen_start.chrctr = 0L;
	    context.line = context.chrctr = 0L;
	}
	fnum++;
	firstf = 0;
    }
    reset_tty ();
    exit(0);
}

argscan(s)
char *s;
{
	    for (dlines = 0; *s != '\0'; s++)
		if (isdigit(*s))
		    dlines = dlines*10 + *s - '0';
		else if (*s == 'd')
		    dum_opt = 1;
		else if (*s == 'l')
		    stop_opt = 0;
		else if (*s == 'f')
		    fold_opt = 0;
		else if (*s == 'p')
		    noscroll++;
		else if (*s == 'c')
		    clreol++;
		else if (*s == 's')
		    ssp_opt = 1;
		else if (*s == 'u')
		    ul_opt = 0;
}


/*
** Check whether the file named by fs is an ASCII file which the user may
** access.  If it is, return the opened file. Otherwise return NULL.
*/

FILE *
checkf (fs, clearfirst)
register char *fs;
int *clearfirst;
{
    struct stat stbuf;
    register FILE *f;
    char c;

    if (stat (fs, &stbuf) == -1) {
	fflush(stdout);
	if (clreol)
	    cleareol ();
	perror(fs);
	return (NULL);
    }
    if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
	printf("\n*** %s: directory ***\n\n", fs);
	return (NULL);
    }
    if ((f=Fopen(fs, "r")) == NULL) {
	fflush(stdout);
	perror(fs);
	return (NULL);
    }
    c = Getc(f);

    /* Try to see whether it is an ASCII file */

    switch ((c | *f->_ptr << 8) & 0177777) {
    case 0405:
    case 0407:
    case 0410:
    case 0411:
    case 0413:
    case 0177545:
	printf("\n******** %s: Not a text file ********\n\n", fs);
	fclose (f);
	return (NULL);
    default:
	break;
    }
    if (c == '\f')
	*clearfirst = 1;
    else {
	*clearfirst = 0;
	Ungetc (c, f);
    }
    if ((file_size = stbuf.st_size) == 0)
	file_size = 0x7fffffffffffffffL;
    return (f);
}

/*
** A real function, for the tputs routine in termlib
*/

putch (ch)
char ch;
{
    putchar (ch);
}

/*
** Print out the contents of the file f, one screenful at a time.
*/

#define STOP -10

screen (f, num_lines)
register FILE *f;
register int num_lines;
{
    register int c;
    register int nchars;
    int length;			/* length of current line */
    static int prev_len = 1;	/* length of previous line */

    for (;;) {
	while (num_lines > 0 && !Pause) {
	    if ((nchars = getline (f, &length)) == EOF)
	    {
		if (clreol)
		    clreos();
		return;
	    }
	    if (ssp_opt && length == 0 && prev_len == 0)
		continue;
	    prev_len = length;
	    if (bad_so || (Senter && *Senter == ' ') && promptlen > 0)
		erase (0);
	    /* must clear before drawing line since tabs on some terminals
	     * do not erase what they tab over.
	     */
	    if (clreol)
		cleareol ();
	    prbuf (Line, length);
	    if (nchars < promptlen)
		erase (nchars);	/* erase () sets promptlen to 0 */
	    else promptlen = 0;
	    /* is this needed?
	     * if (clreol)
	     *	cleareol();	/* must clear again in case we wrapped *
	     */
	    if (nchars < Mcol || !fold_opt)
		putchar('\n');
	    if (nchars == STOP)
		break;
	    num_lines--;
	}
	fflush(stdout);
	if ((c = Getc(f)) == EOF)
	{
	    if (clreol)
		clreos ();
	    return;
	}

	if (Pause && clreol)
	    clreos ();
	Ungetc (c, f);
	setjmp (restore);
	Pause = 0; startup = 0;
	if ((num_lines = command (NULL, f)) == 0)
	    return;
	if (hard && promptlen > 0)
		erase (0);
	if (noscroll && num_lines == dlines)
	{ 
	    if (clreol)
		home();
	    else
		doclear ();
	}
	screen_start.line = Currline;
	screen_start.chrctr = Ftell (f);
    }
}

/*
** Come here if a quit signal is received
*/

onquit()
{
    signal(SIGQUIT, SIG_IGN);
    if (!inwait) {
	putchar ('\n');
	if (!startup) {
	    signal(SIGQUIT, onquit);
	    longjmp (restore, 1);
	}
	else
	    Pause++;
    }
    else if (!dum_opt && notell) {
	write (2, "[Use q or Q to quit]", 20);
	promptlen += 20;
	notell = 0;
    }
    signal(SIGQUIT, onquit);
}

/*
** Clean up terminal state and exit. Also come here if interrupt signal received
*/

end_it ()
{

    reset_tty ();
    if (clreol) {
	putchar ('\r');
	clreos ();
	fflush (stdout);
    }
    else if (!clreol && (promptlen > 0)) {
	kill_line ();
	fflush (stdout);
    }
    else
	write (2, "\n", 1);
    _exit(0);
}

copy_file(f)
register FILE *f;
{
    register int c;

    while ((c = getc(f)) != EOF)
	putchar(c);
}

/* Simplified printf function */

printf (fmt, args)
register char *fmt;
int args;
{
	register int *argp;
	register char ch;
	register int ccount;

	ccount = 0;
	argp = &args;
	while (*fmt) {
		while ((ch = *fmt++) != '%') {
			if (ch == '\0')
				return (ccount);
			ccount++;
			putchar (ch);
		}
		switch (*fmt++) {
		case 'd':
			ccount += printd (*argp);
			break;
		case 's':
			ccount += pr ((char *)*argp);
			break;
		case '%':
			ccount++;
			argp--;
			putchar ('%');
			break;
		case '0':
			return (ccount);
		default:
			break;
		}
		++argp;
	}
	return (ccount);

}

/*
** Print an integer as a string of decimal digits,
** returning the length of the print representation.
*/

printd (n)
int n;
{
    int a, nchars;

    if (a = n/10)
	nchars = 1 + printd(a);
    else
	nchars = 1;
    putchar (n % 10 + '0');
    return (nchars);
}

/* Put the print representation of an integer into a string */
static char *sptr;

scanstr (n, str)
int n;
char *str;
{
    sptr = str;
    sprintf (n);
    *sptr = '\0';
}

sprintf (n)
{
    int a;

    if (a = n/10)
	sprintf (a);
    *sptr++ = n % 10 + '0';
}

static char bell = ctrl(G);

strlen (s)
char *s;
{
    register char *p;

    p = s;
    while (*p++)
	;
    return (p - s - 1);
}

/* See whether the last component of the path name "path" is equal to the
** string "string"
*/

tailequ (path, string)
char *path;
register char *string;
{
	register char *tail;

	tail = path + strlen(path);
	while (tail >= path)
		if (*(--tail) == '/')
			break;
	++tail;
	while (*tail++ == *string++)
		if (*tail == '\0')
			return(1);
	return(0);
}

prompt (filename)
char *filename;
{
    if (clreol)
	cleareol ();
    else if (promptlen > 0)
	kill_line ();
    if (!hard) {
	promptlen = 8;
	if (Senter && Sexit)
	    tputs (Senter, 1, putch);
	if (clreol)
	    cleareol ();
	pr("--More--");
	if (filename != NULL) {
	    promptlen += printf ("(Next file: %s)", filename);
	}
	else if (!no_intty) {
	    promptlen += printf ("(%d%%)", (int)((file_pos * 100) / file_size));
	}
	if (dum_opt) {
	    promptlen += pr("[Hit space to continue, Rubout to abort]");
	}
	if (Senter && Sexit)
	    tputs (Sexit, 1, putch);
	if (clreol)
	    clreos ();
	fflush(stdout);
    }
    else
	write (2, &bell, 1);
    inwait++;
}

/*
** Get a logical line
*/

getline(f, length)
register FILE *f;
int *length;
{
    register int	c;
    register char	*p;
    register int	column;
    static int		colflg;

    p = Line;
    column = 0;
    c = Getc (f);
    if (colflg && c == '\n') {
	Currline++;
	c = Getc (f);
    }
    while (p < &Line[LINSIZ - 1]) {
	if (c == EOF) {
	    if (p > Line) {
		*p = '\0';
		*length = p - Line;
		return (column);
	    }
	    *length = p - Line;
	    return (EOF);
	}
	if (c == '\n') {
	    Currline++;
	    break;
	}
	*p++ = c;
	if (c == '\t')
	    if (hardtabs && column < promptlen && !hard) {
		if (eraseln && !dumb) {
		    column = 1 + (column | 7);
		    tputs (eraseln, 1, putch);
		    promptlen = 0;
		}
		else {
		    for (--p; column & 7 && p < &Line[LINSIZ - 1]; column++) {
			*p++ = ' ';
		    }
		    if (column >= promptlen) promptlen = 0;
		}
	    }
	    else
		column = 1 + (column | 7);
	else if (c == '\b')
	    column--;
	else if (c == '\r')
	    column = 0;
	else if (c == '\f' && stop_opt) {
		p[-1] = '^';
		*p++ = 'L';
		column += 2;
		Pause++;
	}
	else if (c == EOF) {
	    *length = p - Line;
	    return (column);
	}
	else if (c >= ' ' && c != RUBOUT)
	    column++;
	if (column >= Mcol && fold_opt) break;
	c = Getc (f);
    }
    if (column >= Mcol && Mcol > 0) {
	if (!Wrap) {
	    *p++ = '\n';
	}
    }
    colflg = column == Mcol && fold_opt;
    *length = p - Line;
    *p = 0;
    return (column);
}

/*
** Erase the rest of the prompt, assuming we are starting at column col.
*/

erase (col)
register int col;
{

    if (promptlen == 0)
	return;
    if (hard) {
	putchar ('\n');
    }
    else {
	if (col == 0)
	    putchar ('\r');
	if (!dumb && eraseln)
	    tputs (eraseln, 1, putch);
	else
	    for (col = promptlen - col; col > 0; col--)
		putchar (' ');
    }
    promptlen = 0;
}

/*
** Erase the current line entirely
*/

kill_line ()
{
    erase (0);
    if (!eraseln || dumb) putchar ('\r');
}

/*
 * force clear to end of line
 */
cleareol()
{
    tputs(eraseln, 1, putch);
}

clreos()
{
    tputs(EodClr, 1, putch);
}

/*
**  Print string and return number of characters
*/

pr(s1)
char	*s1;
{
    register char	*s;
    register char	c;

    for (s = s1; c = *s++; )
	putchar(c);
    return (s - s1 - 1);
}


/* Print a buffer of n characters */

prbuf (s, n)
register char *s;
register int n;
{
    char c;				/* next ouput character */
    register int state;			/* next output char's UL state */
    static int pstate = 0;		/* current terminal UL state (off) */

    while (--n >= 0)
	if (!ul_opt)
	    putchar (*s++);
	else {
	    if (n >= 2 && s[0] == '_' && s[1] == '\b') {
		n -= 2;
	        s += 2;
		c = *s++;
		state = 1;
	    } else if (n >= 2 && s[1] == '\b' && s[2] == '_') {
		n -= 2;
		c = *s++;
		s += 2;
		state = 1;
	    } else {
		c = *s++;
		state = 0;
	    }
	    if (state != pstate)
		tputs(state ? ULenter : ULexit, 1, putch);
	    pstate = state;
	    putchar(c);
	    if (state && *chUL) {
		pr(chBS);
		tputs(chUL, 1, putch);
	    }
	}
}

/*
**  Clear the screen
*/

doclear()
{
    if (Clear && !hard) {
	tputs(Clear, 1, putch);

	/* Put out carriage return so that system doesn't
	** get confused by escape sequences when expanding tabs
	*/
	putchar ('\r');
	promptlen = 0;
    }
}

/*
 * Go to home position
 */
home()
{
    tputs(Home,1,putch);
}

static int lastcmd, lastarg, lastp;
static int lastcolon;
char shell_line[132];

/*
** Read a command and do it. A command consists of an optional integer
** argument followed by the command character.  Return the number of lines
** to display in the next screenful.  If there is nothing more to display
** in the current file, zero is returned.
*/

command (filename, f)
char *filename;
register FILE *f;
{
    register int nlines;
    register int retval;
    register char c;
    char colonch;
    FILE *helpf;
    int done;
    char comchar, cmdbuf[80], *p;

#define ret(val) retval=val;done++;break

    done = 0;
    if (!errors)
	prompt (filename);
    else
	errors = 0;
    if (MBIT == RAW && slow_tty) {
	otty.sg_flags |= MBIT;
	stty(2, &otty);
    }
    for (;;) {
	nlines = number (&comchar);
	lastp = colonch = 0;
	if (comchar == '.') {	/* Repeat last command */
		lastp++;
		comchar = lastcmd;
		nlines = lastarg;
		if (lastcmd == ':')
			colonch = lastcolon;
	}
	lastcmd = comchar;
	lastarg = nlines;
	if (comchar == otty.sg_erase) {
	    kill_line ();
	    prompt (filename);
	    continue;
	}
	switch (comchar) {
	case ':':
	    retval = colon (filename, colonch, nlines);
	    if (retval >= 0)
		done++;
	    break;
	case ' ':
	case 'z':
	    if (nlines == 0) nlines = dlines;
	    else if (comchar == 'z') dlines = nlines;
	    ret (nlines);
	case 'd':
	case ctrl(D):
	    if (nlines != 0) nscroll = nlines;
	    ret (nscroll);
	case RUBOUT:
	case 'q':
	case 'Q':
	    end_it ();
	case 's':
	case 'f':
	    if (nlines == 0) nlines++;
	    if (comchar == 'f')
		nlines *= dlines;
	    putchar ('\r');
	    erase (0);
	    printf ("\n");
	    if (clreol)
		cleareol ();
	    printf ("...skipping %d line", nlines);
	    if (nlines > 1)
		pr ("s\n");
	    else
		pr ("\n");

	    if (clreol)
		cleareol ();
	    pr ("\n");

	    while (nlines > 0) {
		while ((c = Getc (f)) != '\n')
		    if (c == EOF) {
			retval = 0;
			done++;
			goto endsw;
		    }
		    Currline++;
		    nlines--;
	    }
	    ret (dlines);
	case '\n':
	    if (nlines != 0)
		dlines = nlines;
	    else
		nlines = 1;
	    ret (nlines);
	case '\f':
	    if (!no_intty) {
		doclear ();
		Fseek (f, screen_start.chrctr);
		Currline = screen_start.line;
		ret (dlines);
	    }
	    else {
		write (2, &bell, 1);
		break;
	    }
	case '\'':
	    if (!no_intty) {
		kill_line ();
		pr ("\n***Back***\n\n");
		Fseek (f, context.chrctr);
		Currline = context.line;
		ret (dlines);
	    }
	    else {
		write (2, &bell, 1);
		break;
	    }
	case '=':
	    kill_line ();
	    promptlen = printd (Currline);
	    fflush (stdout);
	    break;
	case 'n':
	    lastp++;
	case '/':
	    if (nlines == 0) nlines++;
	    kill_line ();
	    pr ("/");
	    promptlen = 1;
	    fflush (stdout);
	    if (lastp) {
		write (2,"\r", 1);
		search (NULL, f, nlines);	/* Use previous r.e. */
	    }
	    else {
		ttyin (cmdbuf, 78, '/');
		write (2, "\r", 1);
		search (cmdbuf, f, nlines);
	    }
	    ret (dlines-1);
	case '!':
	    do_shell (filename);
	    break;
	case 'h':
	    if ((helpf = fopen (HELPFILE, "r")) == NULL)
		error ("Can't open help file");
	    if (noscroll) doclear ();
	    copy_file (helpf);
	    close (helpf);
	    prompt (filename);
	    break;
	case 'v':	/* This case should go right before default */
	    if (!no_intty) {
		kill_line ();
		cmdbuf[0] = '+';
		scanstr (Currline, &cmdbuf[1]);
		pr ("vi "); pr (cmdbuf); putchar (' '); pr (fnames[fnum]);
		execute (filename, VI, "vi", cmdbuf, fnames[fnum], 0);
		break;
	    }
	default:
	    write (2, &bell, 1);
	    break;
	}
	if (done) break;
    }
    putchar ('\r');
endsw:
    inwait = 0;
    notell++;
    if (MBIT == RAW && slow_tty) {
	otty.sg_flags &= ~MBIT;
	stty(2, &otty);
    }
    return (retval);
}

char ch;

/*
 * Execute a colon-prefixed command.
 * Returns <0 if not a command that should cause
 * more of the file to be printed.
 */

colon (filename, cmd, nlines)
char *filename;
int cmd;
int nlines;
{
	if (cmd == 0)
		ch = readch ();
	else
		ch = cmd;
	lastcolon = ch;
	switch (ch) {
	case 'f':
		kill_line ();
		if (!no_intty)
			promptlen = printf ("\"%s\" line %d", fnames[fnum], Currline);
		else
			promptlen = printf ("[Not a file] line %d", Currline);
		fflush (stdout);
		return (-1);
	case 'n':
		if (nlines == 0) {
			if (fnum >= nfiles - 1)
				end_it ();
			nlines++;
		}
		putchar ('\r');
		erase (0);
		skipf (nlines);
		return (0);
	case 'p':
		if (no_intty) {
			write (2, &bell, 1);
			return (-1);
		}
		putchar ('\r');
		erase (0);
		if (nlines == 0)
			nlines++;
		skipf (-nlines);
		return (0);
	case '!':
		do_shell (filename);
		return (-1);
	case 'q':
	case 'Q':
		end_it ();
	default:
		write (2, &bell, 1);
		return (-1);
	}
}

/*
** Read a decimal number from the terminal. Set cmd to the non-digit which
** terminates the number.
*/

number(cmd)
char *cmd;
{
	register int i;

	i = 0; ch = otty.sg_kill;
	for (;;) {
		ch = readch ();
		if (ch >= '0' && ch <= '9')
			i = i*10 + ch - '0';
		else if (ch == otty.sg_kill)
			i = 0;
		else {
			*cmd = ch;
			break;
		}
	}
	return (i);
}

do_shell (filename)
char *filename;
{
	char cmdbuf[80];

	kill_line ();
	pr ("!");
	fflush (stdout);
	promptlen = 1;
	if (lastp)
		pr (shell_line);
	else {
		ttyin (cmdbuf, 78, '!');
		if (expand (shell_line, cmdbuf)) {
			kill_line ();
			promptlen = printf ("!%s", shell_line);
		}
	}
	fflush (stdout);
	write (2, "\n", 1);
	promptlen = 0;
	shellp = 1;
	execute (filename, shell, shell, "-c", shell_line, 0);
}

/*
** Search for nth ocurrence of regular expression contained in buf in the file
*/

search (buf, file, n)
char buf[];
FILE *file;
register int n;
{
    long startline = Ftell (file);
    register long line1 = startline;
    register long line2 = startline;
    register long line3 = startline;
    register int lncount;
    int saveln, rv, re_exec();
    char *s, *re_comp();

    context.line = saveln = Currline;
    context.chrctr = startline;
    lncount = 0;
    if ((s = re_comp (buf)) != 0)
	error (s);
    while (!feof (file)) {
	line3 = line2;
	line2 = line1;
	line1 = Ftell (file);
	rdline (file);
	lncount++;
	if ((rv = re_exec (Line)) == 1)
		if (--n == 0) {
		    if (lncount > 3 || (lncount > 1 && no_intty))
		    {
			pr ("\n");
			if (clreol)
			    cleareol ();
			pr("...skipping\n");
		    }
		    if (!no_intty) {
			Currline -= (lncount >= 3 ? 3 : lncount);
			Fseek (file, line3);
			if (noscroll)
			    if (clreol) {
				home ();
				cleareol ();
			    } 
			    else
				doclear ();
		    }
		    else {
			kill_line ();
			if (noscroll)
			    if (clreol) {
			        home (); 
			        cleareol ();
			    } 
			    else
				doclear ();
			pr (Line);
			putchar ('\n');
		    }
		    break;
		}
	else if (rv == -1)
	    error ("Regular expression botch");
    }
    if (feof (file)) {
	if (!no_intty) {
#ifdef V6
	    file->_flag &= ~_IOEOF; /* why doesn't fseek do this ??!!??! */
#endif
	    Currline = saveln;
	    Fseek (file, startline);
	}
	else {
	    pr ("\nPattern not found\n");
	    end_it ();
	}
	error ("Pattern not found");
    }
}

execute (filename, cmd, args)
char *filename;
char *cmd, *args;
{
	int id;

	fflush (stdout);
	reset_tty ();
	while ((id = fork ()) < 0)
	    sleep (5);
	if (id == 0) {
	    execv (cmd, &args);
	    write (2, "exec failed\n", 12);
	    exit (1);
	}
	signal (SIGINT, SIG_IGN);
	signal (SIGQUIT, SIG_IGN);
#ifdef SIGTSTP
	if (catch_susp)
	    signal(SIGTSTP, SIG_DFL);
#endif
	wait (0);
	signal (SIGINT, end_it);
	signal (SIGQUIT, onquit);
#ifdef SIGTSTP
	if (catch_susp)
	    signal(SIGTSTP, onsusp);
#endif
	set_tty ();
	pr ("------------------------\n");
	prompt (filename);
}
/*
** Skip n lines in the file f
*/

skiplns (n, f)
register int n;
register FILE *f;
{
    register char c;

    while (n > 0) {
	while ((c = Getc (f)) != '\n')
	    if (c == EOF)
		return;
	    n--;
	    Currline++;
    }
}

/*
** Skip nskip files in the file list (from the command line). Nskip may be
** negative.
*/

skipf (nskip)
register int nskip;
{
    if (nskip == 0) return;
    if (nskip > 0) {
	if (fnum + nskip > nfiles - 1)
	    nskip = nfiles - fnum - 1;
    }
    else if (within)
	++fnum;
    fnum += nskip;
    if (fnum < 0)
	fnum = 0;
    pr ("\n...Skipping ");
    pr ("\n");
    if (clreol)
	cleareol ();
    pr ("...Skipping ");
    pr (nskip > 0 ? "to file " : "back to file ");
    pr (fnames[fnum]);
    pr ("\n");
    if (clreol)
	cleareol ();
    pr ("\n");
    --fnum;
}

/*----------------------------- Terminal I/O -------------------------------*/

initterm ()
{
    char	buf[TBUFSIZ];
    char	clearbuf[100];
    char	*clearptr, *padstr;
    int		ldisc;

    setbuf(stdout, obuf);
    if (!(no_tty = gtty(1, &otty))) {
	if (tgetent(buf, getenv("TERM")) <= 0) {
	    dumb++; ul_opt = 0;
	}
	else {
	    if (((Lpp = tgetnum("li")) < 0) || tgetflag("hc")) {
		hard++;	/* Hard copy terminal */
		Lpp = 24;
	    }
	    if (tailequ (fnames[0], "page") || !hard && tgetflag("ns"))
		noscroll++;
	    if ((Mcol = tgetnum("co")) < 0)
		Mcol = 80;
	    Wrap = tgetflag("am");
	    bad_so = tgetflag ("xs");
	    clearptr = clearbuf;
	    eraseln = tgetstr("ce",&clearptr);
	    Clear = tgetstr("cl", &clearptr);
	    Senter = tgetstr("so", &clearptr);
	    Sexit = tgetstr("se", &clearptr);

	    /*
	     *  Set up for underlining:  some terminals don't need it;
	     *  others have start/stop sequences, still others have an
	     *  underline char sequence which is assumed to move the
	     *  cursor forward one character.  If underline sequence
	     *  isn't available, settle for standout sequence.
	     */

	    if (tgetflag("ul") || tgetflag("os"))
		ul_opt = 0;
	    if ((chUL = tgetstr("uc", &clearptr)) == NULL )
		chUL = "";
	    if ((ULenter = tgetstr("us", &clearptr)) == NULL &&
		(!*chUL) && (ULenter = tgetstr("so", &clearptr)) == NULL)
		ULenter = "";
	    if ((ULexit = tgetstr("ue", &clearptr)) == NULL &&
		(!*chUL) && (ULexit = tgetstr("se", &clearptr)) == NULL)
		ULexit = "";
	    
	    if (padstr = tgetstr("pc", &clearptr))
		PC = *padstr;
	    Home = tgetstr("ho",&clearptr);
	    if (*Home == '\0')
	    {
		if ((cursorm = tgetstr("cm", &clearptr)) != NULL) {
		    strcpy(cursorhome, tgoto(cursorm, 0, 0));
		    Home = cursorhome;
	       }
	    }
	    EodClr = tgetstr("cd", &clearptr);
	}
	if ((shell = getenv("SHELL")) == NULL)
	    shell = "/bin/sh";
    }
    no_intty = gtty(0, &otty);
    gtty(2, &otty);
    ospeed = otty.sg_ospeed;
    slow_tty = ospeed < B1200;
    hardtabs =  !(otty.sg_flags & XTABS);
    if (!no_tty) {
	otty.sg_flags &= ~ECHO;
	if (MBIT == CBREAK || !slow_tty)
	    otty.sg_flags |= MBIT;
    }
}

readch ()
{
	char ch;
	extern int errno;

	if (read (2, &ch, 1) <= 0)
		if (errno != EINTR)
			exit(0);
		else
			ch = otty.sg_kill;
	return (ch);
}

static char BS = '\b';
static char CARAT = '^';

ttyin (buf, nmax, pchar)
char buf[];
register int nmax;
char pchar;
{
    register char *sptr;
    register char ch;
    register int slash = 0;
    int	maxlen;
    char cbuf;

    sptr = buf;
    maxlen = 0;
    while (sptr - buf < nmax) {
	if (promptlen > maxlen) maxlen = promptlen;
	ch = readch ();
	if (ch == '\\') {
	    slash++;
	}
	else if ((ch == otty.sg_erase) && !slash) {
	    if (sptr > buf) {
		--promptlen;
		write (2, &BS, 1);
		--sptr;
		if ((*sptr < ' ' && *sptr != '\n') || *sptr == RUBOUT) {
		    --promptlen;
		    write (2, &BS, 1);
		}
		continue;
	    }
	    else {
		if (!eraseln) promptlen = maxlen;
		longjmp (restore, 1);
	    }
	}
	else if ((ch == otty.sg_kill) && !slash) {
	    if (hard) {
		show (ch);
		putchar ('\n');
		putchar (pchar);
	    }
	    else {
		putchar ('\r');
		putchar (pchar);
		if (eraseln)
		    erase (1);
		promptlen = 1;
	    }
	    sptr = buf;
	    fflush (stdout);
	    continue;
	}
	if (slash && (ch == otty.sg_kill || ch == otty.sg_erase)) {
	    write (2, &BS, 1);
	    --sptr;
	}
	if (ch != '\\')
	    slash = 0;
	*sptr++ = ch;
	if ((ch < ' ' && ch != '\n' && ch != ESC) || ch == RUBOUT) {
	    ch += ch == RUBOUT ? -0100 : 0100;
	    write (2, &CARAT, 1);
	    promptlen++;
	}
	cbuf = ch;
	if (ch != '\n' && ch != ESC) {
	    write (2, &cbuf, 1);
	    promptlen++;
	}
	else
	    break;
    }
    *--sptr = '\0';
    if (!eraseln) promptlen = maxlen;
    if (sptr - buf >= nmax - 1)
	error ("Line too long");
}

expand (outbuf, inbuf)
char *outbuf;
char *inbuf;
{
    register char *instr;
    register char *outstr;
    register char ch;
    char temp[200];
    int changed = 0;

    instr = inbuf;
    outstr = temp;
    while ((ch = *instr++) != '\0')
	switch (ch) {
	case '%':
	    if (!no_intty) {
		strcpy (outstr, fnames[fnum]);
		outstr += strlen (fnames[fnum]);
		changed++;
	    }
	    else
		*outstr++ = ch;
	    break;
	case '!':
	    if (!shellp)
		error ("No previous command to substitute for");
	    strcpy (outstr, shell_line);
	    outstr += strlen (shell_line);
	    changed++;
	    break;
	case '\\':
	    if (*instr == '%' || *instr == '!') {
		*outstr++ = *instr++;
		break;
	    }
	default:
	    *outstr++ = ch;
	}
    *outstr++ = '\0';
    strcpy (outbuf, temp);
    return (changed);
}

show (ch)
register char ch;
{
    char cbuf;

    if ((ch < ' ' && ch != '\n' && ch != ESC) || ch == RUBOUT) {
	ch += ch == RUBOUT ? -0100 : 0100;
	write (2, &CARAT, 1);
	promptlen++;
    }
    cbuf = ch;
    write (2, &cbuf, 1);
    promptlen++;
}

error (mess)
char *mess;
{
    if (clreol)
	cleareol ();
    else
	kill_line ();
    promptlen += strlen (mess);
    if (Senter && Sexit) {
	tputs (Senter, 1, putch);
	pr(mess);
	tputs (Sexit, 1, putch);
    }
    else
	pr (mess);
    fflush(stdout);
    errors++;
    longjmp (restore, 1);
}


set_tty ()
{
	otty.sg_flags |= MBIT;
	otty.sg_flags &= ~ECHO;
	stty(2, &otty);
}

reset_tty ()
{
    otty.sg_flags |= ECHO;
    otty.sg_flags &= ~MBIT;
    stty(2, &otty);
}

rdline (f)
register FILE *f;
{
    register char c;
    register char *p;

    p = Line;
    while ((c = Getc (f)) != '\n' && c != EOF && p - Line < LINSIZ - 1)
	*p++ = c;
    if (c == '\n')
	Currline++;
    *p = '\0';
}

/* Come here when we get a suspend signal from the terminal */

#ifdef SIGTSTP
onsusp ()
{
    reset_tty ();
    fflush (stdout);
    /* Send the TSTP signal to suspend our process group */
    kill (0, SIGTSTP);
    /* Pause for station break */

    /* We're back */
    signal (SIGTSTP, onsusp);
    set_tty ();
    if (inwait)
	    longjmp (restore);
}
#endif
