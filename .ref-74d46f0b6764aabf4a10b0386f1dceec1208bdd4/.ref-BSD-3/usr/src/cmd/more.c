#include <whoami.h>
#include <stdio.h>
#include <signal.h>
#include <sgtty.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>

#ifdef CORY
#define MBIT RAW
#else
#include <ctype.h>
#define MBIT CBREAK
#endif

#define TBUFSIZ	1024
#define LINSIZ	256
#define ctrl(letter)	('letter' & 077)
#define RUBOUT	'\177'
#define ESC	'\033'
#define QUIT	'\034'

struct sgttyb 	otty;
int		fnum, no_intty, no_tty, slow_tty;
int		dum_opt, dlines, onquit(), end_it();
int		stop_opt = 1;
int		promptlen;
int		startup = 1;
int		firstf = 1;
int		notell = 1;
int		inwait, pause, errors;
int		within;	/* true if we are within a file,
			false if we are between files */
int		hard, dumb, noscroll, hardtabs;
char		**fnames;
int		nfiles;
char		*shell;
char		ch;
jmp_buf		restore;
char		obuf[BUFSIZ];	/* stdout buffer */
char		Line[LINSIZ];
int		Lpp = 24;	/* lines per page */
char		*Clear;		/* clear screen */
char		*eraseln;	/* erase line */
char		*Senter, *Sexit;/* enter and exit standout mode */
char		*tgetstr();
int		Mcol = 80;	/* number of columns */
int		Wrap = 1;	/* set if automargins */
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
    int			initline;
    char		buf[TBUFSIZ];
    char		clearbuf[100];
    char		initbuf[80];
    char		*clearptr;
    char		*getenv();
    FILE		*checkf();

    nfiles = argc;
    fnames = argv;
    /* Put terminal setup stuff in separate procedure ?? (From here...) */
    setbuf(stdout, obuf);
    if (!(no_tty = gtty(1, &otty))) {
	if (tgetent(buf, getenv("TERM")) <= 0) {
	    dumb++;
	}
	else {
	    if (((Lpp = tgetnum("li")) < 0) || tgetflag("hc")) {
		hard++;	/* Hard copy terminal */
		Lpp = 24;
	    }
	    if (!hard && tgetflag("ns"))
		noscroll++;
	    if ((Mcol = tgetnum("co")) < 0)
		Mcol = 80;
	    Wrap = tgetflag("am");
	    clearptr = clearbuf;
	    eraseln = tgetstr("ce",&clearptr);
	    Clear = tgetstr("cl", &clearptr);
	    Senter = tgetstr("so", &clearptr);
	    Sexit = tgetstr("se", &clearptr);
	    PC = *tgetstr("pc", &clearptr);
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
    /* ... until here or so */
    while (--nfiles > 0) {
	if ((ch = (*++fnames)[0]) == '-') {
	    for (s = fnames[0] + 1, dlines = 0; *s != '\0'; s++)
		if (isdigit(*s))
		    dlines = dlines*10 + *s - '0';
		else if (*s == 'd')
		    dum_opt = 1;
		else if (*s == 'l')
		    stop_opt = 0;
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
    if (dlines == 0)
	dlines = Lpp - 2;
    left = dlines;
    if (nfiles > 1)
	prnames++;
    if (!no_intty && nfiles == 0) {
	fputs("Usage: ",stderr);
	fputs(argv[0],stderr);
	fputs(" [-dn] name1 name2 ...\n",stderr);
	exit(1);
    }
    else
	f = stdin;
    if (!no_tty) {
	signal(SIGQUIT, onquit);
	signal(SIGINT, end_it);
	stty (2, &otty);
    }
    if (no_intty) {
	if (no_tty)
	    copy_file (stdin);
	else {
	    if (srchopt)
		search (initbuf, stdin, 1);
	    else if (initopt)
		skiplns (initline, stdin);
	    screen (stdin, left);
	}
	end_it ();
    }

    while (fnum < nfiles) {
	if ((f = checkf (fnames[fnum])) != NULL) {
	    if (firstf) setjmp (restore);
	    if (firstf) {
		firstf = 0;
		if (srchopt)
		    search (initbuf, f, 1);
		else if (initopt)
		    skiplns (initline, f);
	    }
	    else if (fnum < nfiles && !no_tty) {
		setjmp (restore);
		left = command (fnames[fnum], f);
	    }
	    if (left != 0) {
		if (prnames) {
		    pr("::::::::::::::");
		    if (promptlen > 14)
			erase (14);
		    putchar ('\n');
		    pr(fnames[fnum]);
		    pr("\n::::::::::::::\n");
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
	}
	fnum++;
	firstf = 0;
    }
    otty.sg_flags |= ECHO;
    otty.sg_flags &= ~MBIT;
    stty(2, &otty);
    exit(0);
}

/*
** Check whether the file named by fs is an ASCII file which the user may
** access.  If it is, return the opened file. Otherwise return NULL.
*/

FILE *
checkf (fs)
register char *fs;
{
#ifdef CORY
    int space[3];	/* Why doesn't libretro have a V7 stat? */
#endif
    struct stat stbuf;
    register FILE *f;
    char c;

    if (stat (fs, &stbuf) == -1) {
	fflush(stdout);
	perror(fs);
	return (NULL);
    }
    if (stbuf.st_mode & S_IFDIR) {
	pr("\n*** ");
	pr(fs);
	pr(": directory ***\n\n");
	return (NULL);
    }
    if ((f=fopen(fs, "r")) == NULL) {
	fflush(stdout);
	perror(fs);
	return (NULL);
    }
    c = getc(f);

    /* Try to see whether it is an ASCII file */

    switch ((c | *f->_ptr << 8) & 0177777) {
    case 0405:
    case 0407:
    case 0410:
    case 0411:
    case 0177545:
	pr("\n******** ");
	pr(fs);
	pr(": Not a text file ********\n\n");
	fclose (f);
	return (NULL);
    default:
	break;
    }
    if (c == '\f') {
	c = 0;
	doclear ();
    }
    ungetc (c, f);
    return (f);
}

/*
** A real function, for the tputs routine in termlib
*/

putch (ch)
register char ch;
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
    int nchars;

    for (;;) {
	while (num_lines > 0 && !pause) {
	    if ((nchars = getline (f)) == EOF)
		return;
	    if (Senter && *Senter == ' ' && promptlen > 0)
		erase (0);
	    pr (Line);
	    if (nchars < promptlen)
		erase (nchars);	/* erase () sets promptlen to 0 */
	    else promptlen = 0;
	    if (nchars < Mcol)
		putchar('\n');
	    if (nchars == STOP)
		break;
	    num_lines--;
	}
	fflush(stdout);
	if ((c = getc(f)) == EOF) {
	    if (noscroll)
		doclear();
	    else
		erase (0);
	    return;
	}
	ungetc (c, f);
	setjmp (restore);
	pause = 0; startup = 0;
	if ((num_lines = command (NULL, f)) == 0)
	    return;
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
	    pause++;
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

    otty.sg_flags &= ~MBIT;
    otty.sg_flags |= ECHO;
    stty(2, &otty);
    if (promptlen > 0)
	kill_line ();
    else
	putchar ('\n');
    exit(0);
}

copy_file(f)
register FILE *f;
{
    register int c;

    while ((c = getc(f)) != EOF)
	putchar(c);
}


printd (n)
register int n;
{
    register int a;

    if (a = n/10)
	printd(a);
    putchar(n % 10 + '0');
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

prompt (filename)
char *filename;
{
    if (promptlen > 0)
	kill_line ();
    if (!hard) {
	promptlen = 8;
	if (Senter && Sexit)
	    tputs (Senter, 1, putch);
	pr("--More--");
	if (filename != NULL) {
	    pr("(Next file: ");
	    pr(filename);
	    putchar(')');
	    promptlen += 13 + strlen(filename);
	}
	if (dum_opt) {
	    pr("[Hit space to continue, Rubout to abort]");
	    promptlen += 40;
	}
	if (Senter && Sexit)
	    tputs (Sexit, 1, putch);
	fflush(stdout);
    }
    else
	write (2, &bell, 1);
    inwait++;
}

/*
** Get a logical line
*/

getline(f)
register FILE *f;
{
    register char	c;
    register char	*p;
    register int	column;
    register int	i;
    static int		colflg;

    p = Line;
    i = column = 0;
    c = getc (f);
    if (colflg && c == '\n') c = getc (f);
    for (i = 1; i < LINSIZ; i++) {
	if (c == EOF) {
	    if (p > Line) {
		*p = '\0';
		return (column);
	    }
	    return (EOF);
	}
	if (c == '\n')
	    break;
	*p++ = c;
	if (c == '\t')
	    if (hardtabs && column < promptlen && !hard) {
		if (eraseln && !dumb) {
		    tputs (eraseln, 1, putch);
		    promptlen = 0;
		}
		else {
		    for (--p; column & 7; column++)
			*p++ = ' ';
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
		break;
	}
	else if (c == EOF)
	    return (column);
	else if (c >= ' ')
	    column++;
	if (column >= Mcol) break;
	c = getc (f);
    }
    if (Mcol > 0 && column >= Mcol) {
	if (!Wrap) {
	    *p++ = '\n';
	    i++;
	}
    }
    colflg = (column == Mcol) || c == '\f';
    *p = 0;
    if (c == '\f' && stop_opt)
	return (STOP);
    return (column);
}

/*
** Erase the rest of the prompt, assuming we are starting column col.
*/

erase (col)
register int col;
{

    if (hard || promptlen == 0)
	return;
    if (col == 0)
	putchar ('\r');
    if (!dumb && eraseln)
	tputs (eraseln, 1, putch);
    else
	for (col = promptlen - col; col > 0; col--)
	    putchar (' ');
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
**  Print string
*/

pr(s1)
char	*s1;
{
    register char	*s;
    register char	c;

    for (s = s1; c = *s++; )
	putchar(c);
}

/*
**  Clear the screen
*/

doclear()
{
    if (Clear && Lpp > 0)
	tputs(Clear, 1, putch);
}


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
    int id, done;
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
	switch (comchar) {
	case ' ':
	case 'z':
	    if (nlines == 0) nlines = dlines;
	    else if (comchar == 'z') dlines = nlines;
	    ret (nlines);
	case 'd':
	case ctrl(D):
	    ret (11);
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
	    pr("\n...skipping ");
	    printd(nlines);
	    pr(" line");
	    if (nlines > 1)
		pr("s\n\n");
	    else
		pr("\n\n");
	    while (nlines > 0) {
		while ((c = getc (f)) != '\n')
		    if (c == EOF) {
			retval = 0;
			done++;
			goto endsw;
		    }
		    nlines--;
	    }
	    ret (dlines);
	    break;
	case '\n':
	    ret (1);
	case 'n':
	    if (nlines == 0)
		nlines++;
	    putchar ('\r');
	    erase (0);
	    skipf (nlines);
	    ret (0);
	case 'p':
	    if (no_intty) {
		write (2, &bell, 1);
		break;
	    }
	    putchar ('\r');
	    erase (0);
	    if (nlines == 0)
		nlines++;
	    skipf (-nlines);
	    ret (0);
	case '/':
	    kill_line ();
	    pr ("/");
	    promptlen = 1;
	    fflush (stdout);
	    ttyin (cmdbuf, 78, '/');
	    if (nlines == 0) nlines++;
	    write (2, "\r", 1);
	    search (cmdbuf, f, nlines);
	    ret (dlines);
	case '!':
	    kill_line ();
	    pr ("!");
	    promptlen = 1;
	    fflush (stdout);
	    ttyin (cmdbuf, 78, '!');
	    write (2, "\n", 1);
	    promptlen = 0;
	    otty.sg_flags |= ECHO;
	    otty.sg_flags &= ~MBIT;
	    stty(2, &otty);
	    while ((id = fork ()) < 0)
		;
	    if (id == 0) {
		execl (shell, shell, "-c", cmdbuf, 0);
		write (2, "exec failed\n", 12);
		exit (1);
	    }
	    signal (SIGINT, SIG_IGN);
	    signal (SIGQUIT, SIG_IGN);
	    wait (0);
	    signal (SIGINT, end_it);
	    signal (SIGQUIT, onquit);
	    otty.sg_flags |= MBIT;
	    otty.sg_flags &= ~ECHO;
	    stty(2, &otty);
	    pr ("----------\n(continue)\n");
	    fflush (stdout);
	    break;
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
** Read a decimal number from the terminal. Set cmd to the non-digit which
** terminates the number.
*/

number(cmd)
char *cmd;
{
    register int i;

    i = 0; ch = otty.sg_kill;
    for (;;) {
	read (2, &ch, 1);
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

/*
** Skip n lines in the file f
*/

skiplns (n, f)
register int n;
register FILE *f;
{
    register char c;

    while (n > 0) {
	while ((c = getc (f)) != '\n')
	    if (c == EOF)
		return;
	    n--;
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
	if (fnum > nfiles - 1)
	    end_it ();
    }
    else if (within)
	++fnum;
    fnum += nskip;
    if (fnum < 0)
	fnum = 0;
    else if (fnum > nfiles - 1)
	fnum = nfiles -1;
    pr ("\n...Skipping ");
    pr (nskip > 0 ? "to file " : "back to file ");
    pr (fnames[fnum]);
    pr ("\n\n");
    --fnum;
}

readch ()
{
    char ch;

    read (2, &ch, 1);
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
	else if (ch == otty.sg_erase && !slash) {
	    if (sptr > buf) {
		--promptlen;
		write (2, &BS, 1);
		--sptr;
		if (*sptr < ' ' && *sptr != '\n') {
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
	else if (ch == otty.sg_kill && !slash) {
	    if (hard)
		pr (" XXX\n");
	    else {
		putchar ('\r');
		putchar (pchar);
		if (eraseln)
		    erase (1);
		promptlen = 1;
		sptr = buf;
	    }
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
	if (ch < ' ' && ch != '\n' && ch != ESC) {
	    ch += 0100;
	    write (2, &CARAT, 1);
	    promptlen++;
	}
	cbuf = ch;
	if (ch != '\n' && ch != ESC) {
	    write (2, &cbuf, 1);
	    promptlen++;
	}
	else break;
    }
    *--sptr = '\0';
    if (!eraseln) promptlen = maxlen;
    if (sptr - buf >= nmax - 1)
	error ("Line too long");
}

/*
** Search for nth ocurrence of regular expression contained in buf in the file
*/

search (buf, file, n)
char buf[];
FILE *file;
register int n;
{
    long startline = ftell (file);
    register long line1 = startline;
    register long line2 = startline;
    register long line3 = startline;
    register int lncount;

    lncount = 0;
    compile (buf);
    while (!feof (file)) {
	line3 = line2;
	line2 = line1;
	line1 = ftell (file);
	rdline (file);
	lncount++;
	if (execute (Line))
		if (--n == 0) {
		    if (lncount > 3 || (lncount > 1 && no_intty))
			pr ("\n...skipping\n");
		    if (!no_intty)
			fseek (file, line3, 0);
		    else {
			kill_line ();
			pr (Line);
			putchar ('\n');
		    }
		    break;
		}
    }
    if (feof (file)) {
	if (!no_intty) {
#ifdef CORY
	    file->_flag &= ~_IOEOF; /* why doesn't fseek do this ??!!??! */
#endif
	    fseek (file, startline, 0);
	}
	else {
	    pr ("\nPattern not found\n");
	    end_it ();
	}
	error ("Pattern not found");
    }
}

/*
 * The following are adapted from the editor
 */

/*
 * Internal form of regular expressions.
 */
#define	CBRA	1	/* left \( bracket */
#define	CCHR	2	/* a particular character */
#define	CDOT	4	/* any char (.) */
#define	CCL	6	/* begin class ([) */
#define	NCCL	8	/* begin not class ([^) */
#define	CDOL	10	/* end of line ($) */
#define	CEOF	11	/* end of pattern */
#define	CKET	12	/* right \) bracket */
#define	CBACK	14	/* repeat previous match (\1, etc on lhs) */

#define	STAR	01	/* or'ed with some symbols to indicate * suffix */
 
#define	NBRA	5	/* max # of \( \) pairs */

char expbuf[BUFSIZ];
char *braslist[NBRA];
char *braelist[NBRA];
int nbra;
int circfl;
char *loc1;
char *loc2;
char *locs;


/*
 * compile: convert typed in regular expression into internal form.
 * eof is the char that delimits the r.e.
 * General structure of compiled r.e. in expbuf: A sequence of codes
 * from #defines above (CCHR, CDOT, etc). Some of these take arguments
 * which follow in line (e.g. CCHR is followed by the particular character
 * it is required to match.) CEOF terminates the r.e.
 */
compile(inbuf)
char inbuf[];
{
	register char c;
	register char *ep;
	register char *bp = inbuf;
	char *lastep;
	char bracket[NBRA], *bracketp;
	int cclcnt;

/* comerr: compilation error. Don't leave half baked r.e. around. */
#define comerr(msg) {expbuf[0] = 0; nbra = 0; error(msg); }
	ep = expbuf;
	bracketp = bracket;
	if ((c = *bp++) == '\0') {
		/* null r.e.: just re-use last r.e., which is still there */
		if (*ep==0)
			error("No previous regular expression");
		return;
	}
	nbra = 0;
	/* circfl: true if have ^ (anchored search). */
	circfl = 0;
	if (c == '^') {
		c = *bp++;
		circfl++;
	}
	lastep = 0;
	--bp;
	for (;;) {	/* for each character in the r.e. */
		if (ep >= &expbuf[BUFSIZ])
			comerr("r.e. too long");
		c = *bp++;
		if (c == '\0') {
			/* Hit trailing delim: clean up and quit */
			if (bracketp != bracket)
				comerr("unmatched \\(");
			*ep++ = CEOF;
			*ep++ = 0;
			return;
		}
		if (c!='*')
			lastep = ep;
		switch (c) {

		case '\\':
			if ((c = *bp++)=='(') {
				/* \(: start of subexpression */
				if (nbra >= NBRA)
					comerr("too many \\(\\) pairs");
				*bracketp++ = nbra;
				*ep++ = CBRA;
				*ep++ = nbra++;
				continue;
			}
			if (c == ')') {
				/* \): end of sub exp */
				if (bracketp <= bracket)
					comerr("unmatched \\)");
				*ep++ = CKET;
				*ep++ = *--bracketp;
				continue;
			}
			if (c>='1' && c<'1'+NBRA) {
				/* \1, \2, ...: rematch previous subexp */
				*ep++ = CBACK;
				*ep++ = c-'1';
				continue;
			}
			/* Otherwise just force that char, not specially */
			*ep++ = CCHR;
			if (c=='\n')
				/* Newlines can't possibly be in lines */
				comerr("multi line r.e. not allowed");
			*ep++ = c;
			continue;

		case '.':
			/* .: match any character */
			*ep++ = CDOT;
			continue;

		case '*':
			/* *: Repeat last char indefinitely */
			if (lastep==0 || *lastep==CBRA || *lastep==CKET)
				/* Not that smart, so treat * as nonspecial */
				goto defchar;
			*lastep |= STAR;
			continue;

		case '$':
			/* $: match end of line */
			if (*bp != '\0')
				/* $ only special at end of r.e. */
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			/*
			 * [...]: any of chars enclosed in brackets.
			 * Compiled form: CCL or NCCL, # of possible chars,
			 * then each char. -'s are expanded.
			 */
			*ep++ = CCL;
			*ep++ = 0;
			cclcnt = 1;
			if ((c = *bp++) == '^') {
				/* [^...]: reverse sense of match */
				c = *bp++;
				ep[-2] = NCCL;
			}
			do {	/* for each char in brackets */
				if (c=='\n')
					comerr("missing ]");
				if (c == '-' && ep[-1] != 0) {
					/* form ...a-z... but [- not special */
					if ((c = *bp++) == ']') {
						/* -] not special either */
						*ep++ = '-';
						cclcnt++;
						break;
					}
					while (ep[-1]<c) {
						/* insert all chars between */
						*ep = ep[-1]+1;
						ep++;
						cclcnt++;
						if (ep>=&expbuf[BUFSIZ])
							comerr("Too long");
					}
				}
				*ep++ = c;
				cclcnt++;
				if (ep >= &expbuf[BUFSIZ])
					comerr("Too long");
			} while ((c = *bp++) != ']');
			lastep[1] = cclcnt;	/* backpatch count */
			continue;

		defchar:
		default:
			/*
			 * An ordinary char or one treated as ordinary.
			 * Store CCHR followed by that char, rather than
			 * just the char. This causes most r.e.'s to take
			 * up about twice the space you would expect.
			 * On the other hand, it makes r.e.'s beautifully
			 * portable, even though the codes could be real
			 * characters.
			 */
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
}

/*
 * execute: look for the compiled r.e. on line addr.
 * gf is 0 if this is the first time on this line, otherwise nonzero.
 * If not first, start looking at locs, otherwise at beg of linebuf.
 * loc1 and loc2 are set to the ends of the pattern found, if any.
 * 1 is returned if successful, otherwise 0.
 */
execute(lptr)
char *lptr;
{
	register char *p1, *p2;
	register int c;

	for (c=0; c<NBRA; c++) {
		braslist[c] = 0;
		braelist[c] = 0;
	}
	p1 = lptr;
	p2 = expbuf;
	if (circfl) {
		/* anchored search (^): just try one advance. */
		loc1 = p1;
		return(advance(p1, p2));
	}
	/* fast check for first character */
	if (*p2==CCHR) {
		c = p2[1];
		do {
			if (*p1!=c)
				continue;
			if (advance(p1, p2)) {
				loc1 = p1;
				return(1);
			}
		} while (*p1++);
		return(0);
	}
	/* regular algorithm, try advance starting at each char position. */
	do {
		if (advance(p1, p2)) {
			loc1 = p1;
			return(1);
		}
	} while (*p1++);
	return(0);
}

/*
 * advance: does an anchored search for expression starting at ep,
 * looking in line starting at lp. Returns 1 if matches, else 0.
 * If found, loc2 is set to end of pattern.
 */
advance(lp, ep)
register char *ep, *lp;
{
	register char *curlp;
	int i;

	for (;;) switch (*ep++) {	/* for each code in r.e., look at it..*/

	case CCHR:
		if (*ep++ == *lp++)
			continue;
		return(0);

	case CDOT:
		if (*lp++)
			continue;
		return(0);

	case CDOL:
		if (*lp==0)
			continue;
		return(0);

	case CEOF:
		loc2 = lp;
		return(1);

	case CCL:
		if (cclass(ep, *lp++, 1)) {
			ep += *ep;
			continue;
		}
		return(0);

	case NCCL:
		if (cclass(ep, *lp++, 0)) {
			ep += *ep;
			continue;
		}
		return(0);

	case CBRA:
		braslist[*ep++] = lp;
		continue;

	case CKET:
		braelist[*ep++] = lp;
		continue;

	case CBACK:
		if (braelist[i = *ep++]==0)
			error("bad back reference");
		if (backref(i, lp)) {
			lp += braelist[i] - braslist[i];
			continue;
		}
		return(0);

	case CBACK|STAR:
		if (braelist[i = *ep++] == 0)
			error("bad back reference");
		curlp = lp;
		while (backref(i, lp))
			lp += braelist[i] - braslist[i];
		while (lp >= curlp) {
			if (advance(lp, ep))
				return(1);
			lp -= braelist[i] - braslist[i];
		}
		continue;

	case CDOT|STAR:
		curlp = lp;
		while (*lp++)
			;
		goto star;

	case CCHR|STAR:
		curlp = lp;
		while (*lp++ == *ep)
			;
		ep++;
		goto star;

	case CCL|STAR:
	case NCCL|STAR:
		curlp = lp;
		while (cclass(ep, *lp++, ep[-1]==(CCL|STAR)))
			;
		ep += *ep;
		goto star;

	star:
		/*
		 * star: special treatment. We have found as many of them
		 * as there are to find. Maybe this was too many, as dictated
		 * by what follows in the pattern. Try, starting from the
		 * end, to recursively advance after each char found,
		 * and return after first successful advance (thus finding
		 * largest possible string that matches).
		 */
		do {
			lp--;
			if (lp==locs)
				break;
			if (advance(lp, ep))
				return(1);
		} while (lp > curlp);
		/* star failed at all attempts, so whole pattern fails. */
		return(0);

	default:
		longjmp (restore, 1);
	}
}

/*
 * backref: checks to see that text starting at lp matches previous
 * sub-expression #i. Returns 1 if successful, else 0. (Used for \k
 * on lhs.)
 */
backref(i, lp)
register int i;
register char *lp;
{
	register char *bp;

	bp = braslist[i];
	while (*bp++ == *lp++)
		if (bp >= braelist[i])
			return(1);
	return(0);
}

/*
 * cclass: check to see if character c is in class starting at set.
 * ([...] construction on lhs of r.e.) af is sense of success/failure:
 * af=1 is normal (success returns 1), af=0 is reversed for [^ (success
 * returns 0).
 */
int
cclass(set, c, af)
register char *set, c;
int af;
{
	register n;

	if (c==0)
		return(0);
	n = *set++;
	while (--n)
		if (*set++ == c)
			return(af);
	return(!af);
}

error (mess)
char *mess;
{
    if (promptlen > 0)
	if (hard)
	    putchar ('\n');
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
    if (hard)
	putchar ('\n');
    fflush(stdout);
    errors++;
    longjmp (restore, 1);
}

rdline (f)
register FILE *f;
{
    register char c;
    register char *p;

    p = Line;
    while ((c = getc (f)) != '\n' && c != EOF && p - Line < LINSIZ - 1)
	*p++ = c;
    *p = '\0';
}
