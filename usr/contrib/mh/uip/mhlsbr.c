/* mhlsbr.c - implement the "nifty" message lister */

#include "../h/mh.h"
#include "../h/addrsbr.h"
#include "../h/formatsbr.h"
#include "../zotnet/tws.h"
#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>


/* MAJOR BUG:
   for a component containing addresses, ADDRFMT, if COMPRESS is also
   set, then addresses get split wrong (not at the spaces between commas).
   To fix this correctly, putstr() should know about "atomic" strings that
   must NOT be broken across lines.  That's too difficult for right now
   (it turns out that there are a number of degernate cases), so in
   oneline(), instead of

		     (*onelp == '\n' && !onelp[1])

   being a terminating condition,

	 (*onelp == '\n' && (!onelp[1] || (flags & ADDRFMT)))

   is used instead.  This cuts the line prematurely, and gives us a much
   better chance of getting things right.
 */


#define ONECOMP	0
#define TWOCOMP	1

#define	adios	mhladios
#define	done	mhldone

#define	QUOTE	'\\'

/*  */

static struct swit mhlswitches[] = {
#define	BELLSW	0
    "bell", 0,
#define	NBELLSW	1
    "nobell", 0,

#define	CLRSW	2
    "clear", 0,
#define	NCLRSW	3
    "noclear", 0,

#define	FOLDSW	4
    "folder +folder", 0,
#define	FORMSW	5
    "form formfile", 0,

#define	PROGSW	6
    "moreproc program", 0,
#define	NPROGSW	7
    "nomoreproc", 0,

#define	LENSW	8
    "length lines", 0,
#define	WIDSW	9
    "width columns", 0,

#define	HELPSW	10
    "help", 4,

#define	FORW1SW	11
    "forward", -7,		/* interface from forw */
#define	FORW2SW	12
    "forwall", -7,		/*   .. */
#define	DGSTSW	13
    "digest list", -6,

    NULL, NULL
};

/*  */

struct mcomp {
    char   *c_name;		/* component name			*/
    char   *c_text;		/* component text			*/
    char   *c_ovtxt;		/* text overflow indicator		*/
    char   *c_nfs;		/* iff FORMAT				*/
    struct format *c_fmt;	/*   ..					*/

    int     c_offset;		/* left margin indentation		*/
    int     c_ovoff;		/* overflow indentation			*/
    int     c_width;		/* width of field			*/
    int     c_cwidth;		/* width of component			*/
    int     c_length;		/* length in lines			*/
	
    short   c_flags;
#define NOCOMPONENT	0x0001	/* don't show component name            */
#define UPPERCASE       0x0002	/* display in all upper case            */
#define CENTER          0x0004	/* center line				*/
#define CLEARTEXT       0x0008	/* cleartext				*/
#define EXTRA		0x0010	/* an "extra" component			*/
#define HDROUTPUT	0x0020	/* already output			*/
#define CLEARSCR	0x0040	/* clear screen				*/
#define LEFTADJUST	0x0080	/* left justify multiple lines		*/
#define COMPRESS	0x0100	/* compress text			*/
#define	ADDRFMT		0x0200	/* contains addresses			*/
#define	BELL		0x0400	/* sound bell at EOP			*/
#define	DATEFMT		0x0800	/* contains dates			*/
#define	FORMAT		0x1000	/* parse address/date			*/
#define	INIT		0x2000
#define	LBITS	"\020\01NOCOMPONENT\02UPPERCASE\03CENTER\04CLEARTEXT\05EXTRA\06HDROUTPUT\07CLEARSCR\010LEFTADJUST\011COMPRESS\012ADDRFMT\013BELL\014DATEFMT\015FORMAT\016INIT"
#define	GFLAGS	(NOCOMPONENT | UPPERCASE | CENTER | LEFTADJUST | COMPRESS)

    struct mcomp *c_next;
};

static struct mcomp *msghd = NULL;
static struct mcomp *msgtl = NULL;
static struct mcomp *fmthd = NULL;
static struct mcomp *fmttl = NULL;

static struct mcomp global = {
    NULL, NULL, "", NULL, NULL, 0, -1, 80, -1, 40, BELL, NULL
};
static struct mcomp  holder =
{
    NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0, NOCOMPONENT, NULL
};


static struct pair {
    char   *p_name;
    short   p_flags;
}                   pairs[] = {
			"Date", DATEFMT,
                        "From", ADDRFMT,
                        "Sender", ADDRFMT,
                        "Reply-To", ADDRFMT,
                        "To", ADDRFMT,
                        "cc", ADDRFMT,
                        "Bcc", ADDRFMT,
			"Resent-Date", DATEFMT,
                        "Resent-From", ADDRFMT,
                        "Resent-Sender", ADDRFMT,
                        "Resent-Reply-To", ADDRFMT,
                        "Resent-To", ADDRFMT,
                        "Resent-cc", ADDRFMT,
                        "Resent-Bcc", ADDRFMT,

                        NULL
};

static struct triple {
    char   *t_name;
    short   t_on;
    short   t_off;
}                       triples[] = {
			    "nocomponent", NOCOMPONENT, 0,
                            "uppercase", UPPERCASE, 0,
                            "nouppercase", 0, UPPERCASE,
                            "center", CENTER, 0,
                            "nocenter", 0, CENTER,
                            "clearscreen", CLEARSCR, 0,
                            "noclearscreen", 0, CLEARSCR,
                            "noclear", 0, CLEARSCR,
                            "leftadjust", LEFTADJUST, 0,
                            "noleftadjust", 0, LEFTADJUST,
                            "compress", COMPRESS, 0,
                            "nocompress", 0, COMPRESS,
                            "addrfield", ADDRFMT, DATEFMT,
                            "bell", BELL, 0,
                            "nobell", 0, BELL,
                            "datefield", DATEFMT, ADDRFMT,

                            NULL
};

/*  */

static int  bellflg = 0;
static int  clearflg = 0;
static int  forwflg = 0;
static int  forwall = 0;

static char *digest = NULL;

static int  exitstat = 0;
static int  mhldebug = 0;

#define	PITTY	(-1)
#define	NOTTY	0
#define	ISTTY	1
static int  ontty = NOTTY;

static int  row;
static int  column;

static int  lm;
static int  llim;
static int  ovoff;
static int  term;
static int  wid;


static char *ovtxt;

static char *onelp;


static char *parptr;
static char *ignores[MAXARGS];


static  jmp_buf env;
static  jmp_buf mhlenv;


static char delim3[] =		/* from forw.c */
    "\n------------------------------------------------------------\n\n";
static char delim4[] = "\n------------------------------\n\n";


static  FP (*mhl_action) () = (FP (*) ()) 0;


void	mhladios (), mhldone ();
int	intrser (), pipeser (), quitser ();
char   *mcomp_add (), *oneline (), *parse ();
struct mcomp *add_queue ();


void	clear_screen ();

/*  */

/* ARGSUSED */

int     mhl (argc, argv)
int     argc;
char   *argv[];
{
    int     length = 0,
	    nomore = 0,
            width = 0,
            vecp = 0,
            i;
    register char   *cp,
		    *folder = NULL,
		    *form = NULL,
		    **ap,
		    **argp;
    char    buf[80],
           *arguments[MAXARGS],
           *files[MAXARGS];

    invo_name = r1bindex (argv[0], '/');
    if ((cp = getenv ("MHLDEBUG")) && *cp)
	mhldebug++;
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

/*  */

    vecp = 0;
    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, mhlswitches)) {
		case AMBIGSW:
		    ambigsw (cp, mhlswitches);
		    done (1);
		case UNKWNSW:
		    adios (NULLCP, "-%s unknown\n", cp);
		case HELPSW:
		    (void) sprintf (buf, "%s [switches] [files ...]",
			    invo_name);
		    help (buf, mhlswitches);
		    done (1);

		case BELLSW:
		    bellflg = 1;
		    continue;
		case NBELLSW:
		    bellflg = -1;
		    continue;

		case CLRSW:
		    clearflg = 1;
		    continue;
		case NCLRSW:
		    clearflg = -1;
		    continue;

		case FOLDSW:
		    if (!(folder = *argp++) || *folder == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case FORMSW:
		    if (!(form = *argp++) || *form == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case PROGSW:
		    if (!(moreproc = *argp++) || *moreproc == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case NPROGSW:
		    nomore++;
		    continue;

		case LENSW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if ((length = atoi (cp)) < 1)
			adios (NULLCP, "bad argument %s %s", argp[-2], cp);
		    continue;
		case WIDSW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if ((width = atoi (cp)) < 1)
			adios (NULLCP, "bad argument %s %s", argp[-2], cp);
		    continue;

		case DGSTSW:
		    if (!(digest = *argp++) || *digest == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		case FORW2SW:
		    forwall++;	/* fall */
		case FORW1SW:
		    forwflg++;
		    clearflg = -1;/* XXX */
		    continue;
	    }
	files[vecp++] = cp;
    }

/*  */

    if (!folder)
	folder = getenv ("mhfolder");

    if (isatty (fileno (stdout)))
	if (!nomore && moreproc && *moreproc) {
	    if (mhl_action) {
		setsig (SIGINT, SIG_IGN);
		setsig (SIGQUIT, quitser);
	    }
	    m_popen (moreproc);
	    ontty = PITTY;
	}
	else {
	    setsig (SIGINT, SIG_IGN);
	    setsig (SIGQUIT, quitser);
	    ontty = ISTTY;
	}
    else
	ontty = NOTTY;

    mhl_format (form ? form : mhlformat, length, width);

    if (vecp == 0)
	process (folder, NULLCP, 1, vecp = 1);
    else
	for (i = 0; i < vecp; i++)
	    process (folder, files[i], i + 1, vecp);

    if (forwall) {
	if (digest) {
	    printf ("%s", delim4);
	    (void) sprintf (buf, "End of %s Digest\n", digest);
	    i = strlen (buf);
	    for (cp = buf + i; i > 1; i--)
		*cp++ = '*';
	    *cp++ = '\n';
	    *cp = NULL;
	    printf ("%s", buf);
	}
	else
	    printf ("\n------- End of Forwarded Message%s\n\n",
		    vecp > 1 ? "s" : "");
    }

    if (clearflg > 0 && ontty == NOTTY)
	clear_screen ();

    if (ontty == PITTY)
	m_pclose ();

    return exitstat;
}

/*  */

static  mhl_format (file, length, width)
register char   *file;
int     length,
        width;
{
    int     i;
    register char  *bp,
		   *cp,
                  **ip;
    char   *ap,
	    buffer[BUFSIZ],
            name[NAMESZ];
    register struct mcomp   *c1;
    struct stat st;
    register    FILE *fp;
    static  dev_t dev = 0;
    static  ino_t ino = 0;
    static  time_t mtime = 0;

    if (fmthd != NULL)
	if (stat (libpath (file), &st) != NOTOK
		&& mtime == st.st_mtime
		&& dev == st.st_dev
		&& ino == st.st_ino)
	    goto out;
	else
	    free_queue (&fmthd, &fmttl);

    if ((fp = fopen (libpath (file), "r")) == NULL)
	adios (file, "unable to open format file");

    if (fstat (fileno (fp), &st) != NOTOK)
	mtime = st.st_mtime, dev = st.st_dev, ino = st.st_ino;

    global.c_ovtxt = global.c_nfs = NULL;
    global.c_fmt = NULL;
    global.c_offset = 0;
    global.c_ovoff = -1;
    if ((i = sc_width ()) > 5)
	global.c_width = i;
    global.c_cwidth = -1;
    if ((i = sc_length ()) > 5)
	global.c_length = i - 1;
    global.c_flags = BELL;		/* BELL is default */
    *(ip = ignores) = NULL;

    while (vfgets (fp, &ap) == OK) {
	bp = ap;
	if (*bp == ';')
	    continue;

	if (cp = index (bp, '\n'))
	    *cp = NULL;

	if (*bp == ':') {
	    c1 = add_queue (&fmthd, &fmttl, NULLCP, bp + 1, CLEARTEXT);
	    continue;
	}

	parptr = bp;
	(void) strcpy (name, parse ());
	switch (*parptr) {
	    case '\0':
	    case ',':
	    case '=':
		if (uleq (name, "ignores")) {
		    ip = copyip (brkstring (getcpy (++parptr), ",", NULLCP), ip);
		    continue;
		}
		parptr = bp;
		while (*parptr) {
		    if (evalvar (&global))
			adios (NULLCP, "format file syntax error: %s", bp);
		    if (*parptr)
			parptr++;
		}
		continue;

	    case ':':
		c1 = add_queue (&fmthd, &fmttl, name, NULLCP, INIT);
		while (*parptr == ':' || *parptr == ',') {
		    parptr++;
		    if (evalvar (c1))
			adios (NULLCP, "format file syntax error: %s", bp);
		}
		if (!c1 -> c_nfs && global.c_nfs)
		    if (c1 -> c_flags & DATEFMT) {
			if (global.c_flags & DATEFMT)
			    c1 -> c_nfs = getcpy (global.c_nfs);
		    }
		    else
			if (c1 -> c_flags & ADDRFMT) {
			    if (global.c_flags & ADDRFMT)
				c1 -> c_nfs = getcpy (global.c_nfs);
			}
		continue;

	    default:
		adios (NULLCP, "format file syntax error: %s", bp);
	}
    }
    (void) fclose (fp);

    if (mhldebug)
	for (c1 = fmthd; c1; c1 = c1 -> c_next) {
	    fprintf (stderr, "c1: name=\"%s\" text=\"%s\" ovtxt=\"%s\"\n",
		    c1 -> c_name, c1 -> c_text, c1 -> c_ovtxt);
	    fprintf (stderr, "\tnfs=0x%x fmt=0x%x\n",
		    c1 -> c_nfs, c1 -> c_fmt);
	    fprintf (stderr, "\toffset=%d ovoff=%d width=%d cwidth=%d length=%d\n",
		    c1 -> c_offset, c1 -> c_ovoff, c1 -> c_width,
		    c1 -> c_cwidth, c1 -> c_length);
	    fprintf (stderr, "\tflags=%s\n",
		    sprintb (buffer, (unsigned) c1 -> c_flags, LBITS));
	}

out: ;
    if (clearflg == 1)
	global.c_flags |= CLEARSCR;
    else
	if (clearflg == -1)
	    global.c_flags &= ~CLEARSCR;

    switch (bellflg) {		/* command line may override format file */
	case 1:
	    global.c_flags |= BELL;
	    break;
	case -1:
	    global.c_flags &= ~BELL;
	    break;
    }

    if (length)
	global.c_length = length;
    if (width)
	global.c_width = width;
    if (global.c_length < 5)
	global.c_length = 10000;
    if (global.c_width < 5)
	global.c_width = 10000;
}

/*  */

static  evalvar (c1)
register struct mcomp *c1;
{
    char   *cp,
            name[NAMESZ];
    register struct triple *ap;

    if (!*parptr)
	return 0;
    (void) strcpy (name, parse ());

    if (uleq (name, "component")) {
	if (ptos (name, &c1 -> c_text))
	    return 1;
	c1 -> c_flags &= ~NOCOMPONENT;
	return 0;
    }
    if (uleq (name, "overflowtext"))
	return ptos (name, &c1 -> c_ovtxt);
    if (uleq (name, "formatfield")) {
	if (ptos (name, &cp))
	    return 1;
	c1 -> c_nfs = getcpy (new_fs (NULLCP, NULLCP, cp));
	c1 -> c_flags |= FORMAT;
	return 0;
    }

    if (uleq (name, "offset"))
	return ptoi (name, &c1 -> c_offset);
    if (uleq (name, "overflowoffset"))
	return ptoi (name, &c1 -> c_ovoff);
    if (uleq (name, "width"))
	return ptoi (name, &c1 -> c_width);
    if (uleq (name, "compwidth"))
	return ptoi (name, &c1 -> c_cwidth);
    if (uleq (name, "length"))
	return ptoi (name, &c1 -> c_length);

    for (ap = triples; ap -> t_name; ap++)
	if (uleq (ap -> t_name, name)) {
	    c1 -> c_flags |= ap -> t_on;
	    c1 -> c_flags &= ~ap -> t_off;
	    return 0;
	}

    return 1;
}

/*  */

static int  ptoi (name, i)
register char  *name;
register int   *i;
{
    char   *cp;

    if (*parptr++ != '=' || !*(cp = parse ())) {
	advise (NULLCP, "missing argument to variable %s", name);
	return 1;
    }

    *i = atoi (cp);
    return 0;
}


static int  ptos (name, s)
register char  *name,
	      **s;
{
    char    c,
           *cp;

    if (*parptr++ != '=') {
	advise (NULLCP, "missing argument to variable %s", name);
	return 1;
    }

    if (*parptr != '"')
	for (cp = parptr;
		*parptr && *parptr != ':' && *parptr != ',';
		parptr++)
	    continue;
    else
	for (cp = ++parptr; *parptr && *parptr != '"'; parptr++)
	    if (*parptr == QUOTE)
		if (!*++parptr)
		    parptr--;
    c = *parptr;
    *parptr = NULL;
    *s = getcpy (cp);
    if ((*parptr = c) == '"')
	parptr++;
    return 0;
}

/*  */

static char *parse () {
    int     c;
    register char   *cp;
    static char result[NAMESZ];

    for (cp = result; c = *parptr; parptr++)
	if (isalnum (c)
		|| c == '.'
		|| c == '-'
		|| c == '_'
		|| c =='['
		|| c == ']')
	    *cp++ = c;
	else
	    break;
    *cp = NULL;

    return result;
}

/*  */

static  process (folder, fname, ofilen, ofilec)
register char   *folder,
		*fname;
int	ofilen,
	ofilec;
{
    register char  *cp;
    register struct mcomp  *c1;
    register FILE  *fp;

    switch (setjmp (env)) {
	case OK:
	    if (fname) {
		fp = mhl_action ? (*mhl_action) (fname) : fopen (fname, "r");
		if (fp == NULL) {
		    advise (fname, "unable to open");
		    exitstat++;
		    return;
		}
	    }
	    else {
		fname = "(stdin)";
		fp = stdin;
	    }
	    cp = folder ? concat (folder, ":", fname, NULLCP) : getcpy (fname);
	    if (ontty != PITTY)
		(void) signal (SIGINT, intrser);
	    mhlfile (fp, cp, ofilen, ofilec);/* fall */

	default:
	    if (ontty != PITTY)
		(void) signal (SIGINT, SIG_IGN);
	    if (mhl_action == NULL && fp != stdin)
		(void) fclose (fp);
	    free (cp);
	    if (holder.c_text) {
		free (holder.c_text);
		holder.c_text = NULL;
	    }
	    free_queue (&msghd, &msgtl);
	    for (c1 = fmthd; c1; c1 = c1 -> c_next)
		c1 -> c_flags &= ~HDROUTPUT;
	    break;
    }
}

/*  */

static mhlfile (fp, mname, ofilen, ofilec)
register FILE   *fp;
register char   *mname;
int	ofilen,
	ofilec;
{
    int     state;
    register struct mcomp  *c1,
                           *c2;
    register char **ip;
    char    name[NAMESZ],
            buf[BUFSIZ];

    if (forwall) {
	if (digest)
	    printf ("%s", ofilen == 1 ? delim3 : delim4);
	else {
	    printf ("\n-------");
	    if (ofilen == 1)
		printf (" Forwarded Message%s", ofilec > 1 ? "s" : "");
	    else
		printf (" Message %d", ofilen);
	    printf ("\n\n");
	}
    }
    else
	switch (ontty) {
	    case PITTY:
		if (ofilec > 1) {
		    if (ofilen > 1) {
			if ((global.c_flags & CLEARSCR))
			    clear_screen ();
			else
			    printf ("\n\n\n");
		    }
		    printf (">>> %s\n\n", mname);
		}
		break;

	    case ISTTY:
		(void) strcpy (buf, "\n");
		if (ofilec > 1) {
		    if (SOprintf ("Press <return> to list \"%s\"...", mname)) {
			if (ofilen > 1)
			    printf ("\n\n\n");
			printf ("Press <return> to list \"%s\"...", mname);
		    }
		    (void) fflush (stdout);
		    buf[0] = NULL;
		    (void) read (fileno (stdout), buf, sizeof buf);
		}
		if (index (buf, '\n')) {
		    if ((global.c_flags & CLEARSCR))
			clear_screen ();
		}
		else
		    printf ("\n");
		break;

	    default:
		if (ofilec > 1) {
		    if (ofilen > 1) {
			printf ("\n\n\n");
			if (clearflg > 0)
			    clear_screen ();
		    }
		    printf (">>> %s\n\n", mname);
		}
		break;
	}

/*  */

    for (state = FLD;;)
	switch (state = m_getfld (state, name, buf, sizeof buf, fp)) {
	    case FLD:
	    case FLDPLUS:
		for (ip = ignores; *ip; ip++)
		    if (uleq (name, *ip)) {
			while (state == FLDPLUS)
			    state = m_getfld (state, name, buf, sizeof buf, fp);
			break;
		    }
		if (*ip)
		    continue;

		for (c1 = msghd; c1; c1 = c1 -> c_next)
		    if (uleq (name, c1 -> c_name)) {
			c1 -> c_text =
			    mcomp_add (c1 -> c_flags, buf, c1 -> c_text);
			break;
		    }
		if (c1 == NULL)
		    c1 = add_queue (&msghd, &msgtl, name, buf, 0);
		while (state == FLDPLUS) {
		    state = m_getfld (state, name, buf, sizeof buf, fp);
		    c1 -> c_text = add (buf, c1 -> c_text);
		}

		for (c2 = fmthd; c2; c2 = c2 -> c_next)
		    if (uleq (c2 -> c_name, c1 -> c_name))
			break;
		if (c2 == NULL)
		    c1 -> c_flags |= EXTRA;
		continue;

	    case BODY:
	    case FILEEOF:
		row = column = 0;
		for (c1 = fmthd; c1; c1 = c1 -> c_next) {
		    if (c1 -> c_flags & CLEARTEXT) {
			putcomp (c1, c1, ONECOMP);
			continue;
		    }
		    if (uleq (c1 -> c_name, "messagename")) {
			holder.c_text = concat ("(Message ", mname, ")\n",
					    NULLCP);
			putcomp (c1, &holder, ONECOMP);
			free (holder.c_text);
			holder.c_text = NULL;
			continue;
		    }
		    if (uleq (c1 -> c_name, "extras")) {
			for (c2 = msghd; c2; c2 = c2 -> c_next)
			    if (c2 -> c_flags & EXTRA)
				putcomp (c1, c2, TWOCOMP);
			continue;
		    }
		    if (uleq (c1 -> c_name, "body")) {
			if ((holder.c_text = malloc (sizeof buf)) == NULL)
			    adios (NULLCP, "unable to allocate buffer memory");
			(void) strcpy (holder.c_text, buf);
			while (state == BODY) {
			    putcomp (c1, &holder, ONECOMP);
			    state = m_getfld (state, name, holder.c_text,
					sizeof buf, fp);
			}
			free (holder.c_text);
			holder.c_text = NULL;
			continue;
		    }
		    for (c2 = msghd; c2; c2 = c2 -> c_next)
			if (uleq (c2 -> c_name, c1 -> c_name)) {
			    putcomp (c1, c2, ONECOMP);
			    break;
			}
		}
		return;

	    case LENERR:
	    case FMTERR:
		advise (NULLCP, "format error in message %s", mname);
		exitstat++;
		return;

	    default:
		adios (NULLCP, "getfld() returned %d", state);
	}
}

/*  */

static int  mcomp_flags (name)
register char   *name;
{
    register struct pair   *ap;

    for (ap = pairs; ap -> p_name; ap++)
	if (uleq (ap -> p_name, name))
	    return (ap -> p_flags);

    return NULL;
}


static char *mcomp_add (flags, s1, s2)
short   flags;
register char   *s1,
		*s2;
{
    register char   *dp;

    if (!(flags & ADDRFMT))
	return add (s1, s2);

    if (s2 && *(dp = s2 + strlen (s2) - 1) == '\n')
	*dp = NULL;

    return add (s1, add (",\n", s2));
}

/*  */

struct pqpair {
    char    *pq_text;
    char    *pq_error;
    struct pqpair *pq_next;
};


static mcomp_format (c1, c2)
register struct mcomp *c1,
		      *c2;
{
    int     dat[4];
    register char  *ap,
                   *cp;
    char    buffer[BUFSIZ],
            error[BUFSIZ];
    register struct comp   *cptr;
    register struct pqpair *p,
                           *q;
    struct pqpair   pq;
    register struct mailname   *mp;

    ap = c2 -> c_text;
    c2 -> c_text = NULL;
    dat[0] = dat[1] = dat[2] = 0;
    dat[3] = sizeof buffer - 1;
    (void) fmt_compile (c1 -> c_nfs, &c1 -> c_fmt);

    if (c1 -> c_flags & DATEFMT) {
	FINDCOMP (cptr, "text");
	if (cptr)
	    cptr -> c_text = ap;

	(void) fmtscan (c1 -> c_fmt, buffer, sizeof buffer - 1, dat);
	c2 -> c_text = concat (buffer, "\n", NULLCP);

	free (ap);
	return;
    }

    (q = &pq) -> pq_next = NULL;
    while (cp = getname (ap)) {
	if ((p = (struct pqpair *) calloc ((unsigned) 1, sizeof *p)) == NULL)
	    adios (NULLCP, "unable to allocate pqpair memory");

	if ((mp = getm (cp, NULLCP, 0, AD_NAME, error)) == NULL) {
	    p -> pq_text = getcpy (cp);
	    p -> pq_error = getcpy (error);
	}
	else {
	    p -> pq_text = getcpy (mp -> m_text);
	    mnfree (mp);
	}
	q = (q -> pq_next = p);
    }

    for (p = pq.pq_next; p; p = q) {
	FINDCOMP (cptr, "text");
	if (cptr)
	    cptr -> c_text = p -> pq_text;
	FINDCOMP (cptr, "error");
	if (cptr)
	    cptr -> c_text = p -> pq_error;

	(void) fmtscan (c1 -> c_fmt, buffer, sizeof buffer - 1, dat);
	if (*buffer) {
	    if (c2 -> c_text)
		c2 -> c_text = add (",\n", c2 -> c_text);
	    if (*(cp = buffer + strlen (buffer) - 1) == '\n')
		*cp = NULL;
	    c2 -> c_text = add (buffer, c2 -> c_text);
	}

	free (p -> pq_text);
	if (p -> pq_error)
	    free (p -> pq_error);
	q = p -> pq_next;
	free ((char *) p);
    }

    c2 -> c_text = add ("\n", c2 -> c_text);
    free (ap);
}

/*  */

static struct mcomp *add_queue (head, tail, name, text, flags)
register struct mcomp **head,
		      **tail;
register char   *name,
		*text;
int     flags;
{
    register struct mcomp  *c1;

    if ((c1 = (struct mcomp *) calloc ((unsigned) 1, sizeof *c1)) == NULL)
	adios (NULLCP, "unable to allocate comp memory");

    c1 -> c_flags = flags & ~INIT;
    if (c1 -> c_name = name ? getcpy (name) : NULL)
	c1 -> c_flags |= mcomp_flags (c1 -> c_name);
    c1 -> c_text = text ? getcpy (text) : NULL;
    if (flags & INIT) {
	if (global.c_ovtxt)
	    c1 -> c_ovtxt = getcpy (global.c_ovtxt);
	c1 -> c_offset = global.c_offset;
	c1 -> c_ovoff = global. c_ovoff;
	c1 -> c_width = c1 -> c_length = 0;
	c1 -> c_cwidth = global.c_cwidth;
	c1 -> c_flags |= global.c_flags & GFLAGS;
    }
    if (*head == NULL)
	*head = c1;
    if (*tail != NULL)
	(*tail) -> c_next = c1;
    *tail = c1;

    return c1;
}


static  free_queue (head, tail)
register struct mcomp **head,
		      **tail;
{
    register struct mcomp *c1,
			  *c2;

    for (c1 = *head; c1; c1 = c2) {
	c2 = c1 -> c_next;
	if (c1 -> c_name)
	    free (c1 -> c_name);
	if (c1 -> c_text)
	    free (c1 -> c_text);
	if (c1 -> c_ovtxt)
	    free (c1 -> c_ovtxt);
	if (c1 -> c_nfs)
	    free (c1 -> c_nfs);
	if (c1 -> c_fmt)
	    free ((char *) c1 -> c_fmt);
	free ((char *) c1);
    }

    *head = *tail = NULL;
}

/*  */

static  putcomp (c1, c2, flag)
register struct mcomp *c1,
		      *c2;
int     flag;
{
    int     count,
            cchdr;
    register char   *cp;

    cchdr = 0;
    lm = 0;
    llim = c1 -> c_length ? c1 -> c_length : -1;
    wid = c1 -> c_width ? c1 -> c_width : global.c_width;
    ovoff = (c1 -> c_ovoff >= 0 ? c1 -> c_ovoff : global.c_ovoff)
	+ c1 -> c_offset;
    if ((ovtxt = c1 -> c_ovtxt ? c1 -> c_ovtxt : global.c_ovtxt) == NULL)
	ovtxt = "";
    if (wid < ovoff + strlen (ovtxt) + 5)
	adios (NULLCP, "component: %s width(%d) too small for overflow(%d)",
		c1 -> c_name, wid, ovoff + strlen (ovtxt) + 5);
    onelp = NULL;

    if (c1 -> c_flags & CLEARTEXT) {
	putstr (c1 -> c_text);
	putstr ("\n");
	return;
    }

    if (c1 -> c_nfs && (c1 -> c_flags & (ADDRFMT | DATEFMT)))
	mcomp_format (c1, c2);

    if (c1 -> c_flags & CENTER) {
	count = (c1 -> c_width ? c1 -> c_width : global.c_width)
	    - c1 -> c_offset - strlen (c2 -> c_text);
	if (!(c1 -> c_flags & HDROUTPUT) && !(c1 -> c_flags & NOCOMPONENT))
	    count -= strlen (c1 -> c_text ? c1 -> c_text : c1 -> c_name) + 2;
	lm = c1 -> c_offset + (count / 2);
    }
    else
	if (c1 -> c_offset)
	    lm = c1 -> c_offset;

    if (!(c1 -> c_flags & HDROUTPUT) && !(c1 -> c_flags & NOCOMPONENT)) {
        if (c1 -> c_flags & UPPERCASE)		/* uppercase component also */
	    for (cp = (c1 -> c_text ? c1 -> c_text : c1 -> c_name); *cp; cp++)
	        if (islower (*cp))
		    *cp = toupper (*cp);
	putstr (c1 -> c_text ? c1 -> c_text : c1 -> c_name);
	putstr (": ");
	c1 -> c_flags |= HDROUTPUT;

	cchdr++;
	if ((count = c1 -> c_cwidth -
		strlen (c1 -> c_text ? c1 -> c_text : c1 -> c_name) - 2) > 0)
	    while (count--)
		putstr (" ");
    }

    if (flag == TWOCOMP
	    && !(c2 -> c_flags & HDROUTPUT)
	    && !(c2 -> c_flags & NOCOMPONENT)) {
        if (c1 -> c_flags & UPPERCASE)
	    for (cp = c2 -> c_name; *cp; cp++)
	        if (islower (*cp))
		    *cp = toupper (*cp);
	putstr (c2 -> c_name);
	putstr (": ");
	c2 -> c_flags |= HDROUTPUT;
    }
    if (c1 -> c_flags & UPPERCASE)
	for (cp = c2 -> c_text; *cp; cp++)
	    if (islower (*cp))
		*cp = toupper (*cp);

    count = 0;
    if (cchdr)
	count = (c1 -> c_cwidth >= 0) ? c1 -> c_cwidth
		    : strlen (c1 -> c_text ? c1 -> c_text : c1 -> c_name) + 2;
    count += c1 -> c_offset;

    putstr (oneline (c2 -> c_text, c1 -> c_flags));
    if (term == '\n')
	putstr ("\n");
    while (cp = oneline (c2 -> c_text, c1 -> c_flags))
	if (*cp) {
	    lm = count;
	    putstr (cp);
	    if (term == '\n')
		putstr ("\n");
	}
	else
	    if (term == '\n')
		putstr ("\n");
}

/*  */

static char *oneline (stuff, flags)
register char   *stuff;
short   flags;
{
    int     spc;
    register char   *cp,
		    *ret;

    if (onelp == NULL)
	onelp = stuff;
    if (*onelp == NULL)
	return (onelp = NULL);

    ret = onelp;
    term = 0;
    if (flags & COMPRESS) {
	for (spc = 1, cp = ret; *onelp; onelp++)
	    if (isspace (*onelp)) {
		if (*onelp == '\n' && (!onelp[1] || (flags & ADDRFMT))) {
		    term = '\n';
		    *onelp++ = NULL;
		    break;
		}
		else
		    if (!spc) {
			*cp++ = ' ';
			spc++;
		    }
	    }
	    else {
		*cp++ = *onelp;
		spc = 0;
	    }

	*cp = NULL;
    }
    else {
	while (*onelp && *onelp != '\n')
	    onelp++;
	if (*onelp == '\n') {
	    term = '\n';
	    *onelp++ = NULL;
	}
	if (flags & LEFTADJUST)
	    while (*ret == ' ' || *ret == '\t')
		ret++;
    }

    return ret;
}

/*  */

static  putstr (string)
register char   *string;
{
    if (!column && lm > 0)
	while (lm > 0)
	    if (lm >= 8) {
		putch ('\t');
		lm -= 8;
	    }
	    else {
		putch (' ');
		lm--;
	    }
    lm = 0;
    while (*string)
	putch (*string++);
}

/*  */

static putch (ch)
register char	ch;
{
    char    buf[BUFSIZ];

    if (llim == 0)
	return;

    switch (ch) {
	case '\n':
	    if (llim > 0)
		llim--;
	    column = 0;
	    row++;
	    if (ontty != ISTTY || row != global.c_length)
		break;
	    if (global.c_flags & BELL)
		(void) putchar ('\007');
	    (void) fflush (stdout);
	    buf[0] = NULL;
	    (void) read (fileno (stdout), buf, sizeof buf);
	    if (index (buf, '\n')) {
		if (global.c_flags & CLEARSCR)
		    clear_screen ();
		row = 0;
	    }
	    else {
		(void) putchar ('\n');
		row = global.c_length / 3;
	    }
	    return;

	case '\t':
	    column |= 07;
	    column++;
	    break;

	case '\b':
	    column--;
	    break;

	case '\r':
	    column = 0;
	    break;

	default:
	    if (column == 0 && forwflg && ch == '-')
		(void) putchar ('-'), putchar (' ');
	    if (ch >= ' ')
		column++;
	    break;
    }

    if (column >= wid) {
	putch ('\n');
	if (ovoff > 0)
	    lm = ovoff;
	putstr (ovtxt ? ovtxt : "");
	putch (ch);
	return;
    }

    (void) putchar (ch);
}

/*  */

/* ARGSUSED */

static	int intrser (i)
int     i;
{
#ifndef	BSD42
    (void) signal (SIGINT, intrser);
#endif	BSD42

    discard (stdout);
    (void) putchar ('\n');

    longjmp (env, DONE);
}


/* ARGSUSED */

static	int pipeser (i)
int     i;
{
#ifndef	BSD42
    (void) signal (SIGPIPE, pipeser);
#endif	BSD42

    done (NOTOK);
}


/* ARGSUSED */

static	int quitser (i)
int     i;
{
#ifndef	BSD42
    (void) signal (SIGQUIT, quitser);
#endif	BSD42

    (void) putchar ('\n');
    (void) fflush (stdout);

    done (NOTOK);
}

/*  */

#undef	adios
#undef	done

int     mhlsbr (argc, argv, action)
int     argc;
register char  **argv;
register FP (*action) ();
{
    int     (*istat) (), (*pstat) (), (*qstat) ();
    register char   *cp;
    register struct mcomp  *c1;

    switch (setjmp (mhlenv)) {
	case OK:
	    cp = invo_name;
	    bellflg = clearflg = forwflg = forwall = exitstat = 0;
	    digest = NULL;
	    ontty = NOTTY;
	    mhl_action = action;
	    if ((istat = signal (SIGINT, SIG_IGN)) != SIG_DFL)
		(void) signal (SIGINT, istat);
	    if ((qstat = signal (SIGQUIT, SIG_IGN)) != SIG_DFL)
		(void) signal (SIGQUIT, qstat);
	    pstat = signal (SIGPIPE, pipeser);
	    (void) mhl (argc, argv);	/* fall */

	default:
	    (void) signal (SIGINT, istat);
	    (void) signal (SIGQUIT, qstat);
	    (void) signal (SIGPIPE, SIG_IGN);/* XXX */
	    if (ontty == PITTY)
		m_pclose ();
	    (void) signal (SIGPIPE, pstat);
	    invo_name = cp;
	    if (holder.c_text) {
		free (holder.c_text);
		holder.c_text = NULL;
	    }
	    free_queue (&msghd, &msgtl);
	    for (c1 = fmthd; c1; c1 = c1 -> c_next)
		c1 -> c_flags &= ~HDROUTPUT;
	    return exitstat;
    }
}

/*  */

/* VARARGS2 */

static void mhladios (what, fmt, a, b, c, d, e, f)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    advise (what, fmt, a, b, c, d, e, f);
    mhldone (1);
}


static void mhldone (status)
int     status;
{
    exitstat = status;
    if (mhl_action)
	longjmp (mhlenv, DONE);
    else
	done (exitstat);
}

/*  */

static	int m_pid = NOTOK;
static  int sd = NOTOK;


static	m_popen (name)
char *name;
{
    int     pd[2];

    if (mhl_action && (sd = dup (fileno (stdout))) == NOTOK)
	adios ("standard output", "unable to dup()");

    if (pipe (pd) == NOTOK)
	adios ("pipe", "unable to");

    switch (m_pid = vfork ()) {
	case NOTOK:
	    adios ("fork", "unable to");

	case OK:
	    (void) signal (SIGINT, SIG_DFL);
	    (void) signal (SIGQUIT, SIG_DFL);

	    (void) close (pd[1]);
	    if (pd[0] != fileno (stdin)) {
		(void) dup2 (pd[0], fileno (stdin));
		(void) close (pd[0]);
	    }
	    execlp (name, r1bindex (name, '/'), NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror (name);
	    _exit (-1);

	default:
	    (void) close (pd[0]);
	    if (pd[1] != fileno (stdout)) {
		(void) dup2 (pd[1], fileno (stdout));
		(void) close (pd[1]);
	    }
    }
}


m_pclose () {
    if (m_pid == NOTOK)
	return;

    if (sd != NOTOK) {
	(void) fflush (stdout);
	if (dup2 (sd, fileno (stdout)) == NOTOK)
	    adios ("standard output", "unable to dup2()");

	clearerr (stdout);
	(void) close (sd);
	sd = NOTOK;
    }
    else
	(void) fclose (stdout);

    (void) pidwait (m_pid, OK);
    m_pid = NOTOK;
}
