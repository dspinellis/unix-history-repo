/* prompter.c - prompting editor front-end */
#ifndef	lint
static char ident[] = "@(#)$Id: prompter.c,v 1.9 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#include <errno.h>
#ifndef	SYS5
#include <sgtty.h>
#else	/* SYS5 */
#include <sys/types.h>
#include <termio.h>
#ifndef	NOIOCTLH
#include <sys/ioctl.h>
#endif	/* NOIOCTLH */
#endif	/* SYS5 */
#ifdef	BSD42
#include <setjmp.h>
#endif	/* BSD42 */
#include <signal.h>
#ifdef LOCALE
#include	<locale.h>
#endif


#define	QUOTE	'\\'
#ifndef	CKILL
#define CKILL   '@'
#endif	/* not CKILL */
#ifndef	CERASE
#define CERASE  '#'
#endif	/* not CERASE */

/*  */

static struct swit switches[] = {
#define	ERASESW	0
    "erase chr", 0,
#define	KILLSW	1
    "kill chr", 0,

#define	PREPSW	2
    "prepend", 0,	
#define	NPREPSW	3
    "noprepend", 0,	

#define	RAPDSW	4
    "rapid", 0,	
#define	NRAPDSW	5
    "norapid", 0,	

#define	BODYSW	6
    "body", -4,
#define	NBODYSW	7
    "nobody", -6,

#define	DOTSW	8
    "doteof", 0,
#define	NDOTSW	9
    "nodoteof", 0,

#define	HELPSW	10
    "help", 4,		

    NULL, 0
};

/*  */

extern int  errno;


#ifndef	SYS5
#define	ERASE	sg.sg_erase
#define	KILL	sg.sg_kill
static struct sgttyb    sg;

#define	INTR	tc.t_intrc
static struct tchars    tc;
#else	/* SYS5 */
#define	ERASE	sg.c_cc[VERASE]
#define	KILL	sg.c_cc[VKILL]
#define	INTR	sg.c_cc[VINTR]
static struct termio    sg;
#endif	/* SYS5 */


static TYPESIG	intrser ();

static int  wtuser = 0;
static int  sigint = 0;

#ifdef	BSD42
static jmp_buf sigenv;
#endif	/* BSD42 */

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int     body = 1,
	    prepend = 1,
	    rapid = 0,
	    doteof = 0,
	    fdi,
	    fdo,
            i,
            state;
    char   *cp,
           *drft = NULL,
           *erasep = NULL,
           *killp = NULL,
            name[NAMESZ],
            field[BUFSIZ],
            buffer[BUFSIZ],
            tmpfil[BUFSIZ],
          **ap,
           *arguments[MAXARGS],
          **argp;
    FILE *in, *out;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

/*  */

    while (cp = *argp++)
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buffer, "%s [switches] file", invo_name);
		    help (buffer, switches);
		    done (1);

		case ERASESW: 
		    if (!(erasep = *argp++) || *erasep == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case KILLSW: 
		    if (!(killp = *argp++) || *killp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case PREPSW: 
		    prepend++;
		    continue;
		case NPREPSW: 
		    prepend = 0;
		    continue;

		case RAPDSW: 
		    rapid++;
		    continue;
		case NRAPDSW: 
		    rapid = 0;
		    continue;

		case BODYSW: 
		    body++;
		    continue;
		case NBODYSW: 
		    body = 0;
		    continue;

		case DOTSW: 
		    doteof++;
		    continue;
		case NDOTSW: 
		    doteof = 0;
		    continue;
	    }
	else
	    if (!drft)
		drft = cp;

/*  */

    if (!drft)
	adios (NULLCP, "usage: %s [switches] file", invo_name);
    if ((in = fopen (drft, "r")) == NULL)
	adios (drft, "unable to open");

    (void) strcpy (tmpfil, m_tmpfil (invo_name));
    if ((out = fopen (tmpfil, "w")) == NULL)
	adios (tmpfil, "unable to create");
    (void) chmod (tmpfil, 0600);

    if (killp || erasep) {
#ifndef	SYS5
	int    serase,
	       skill;
#else	/* SYS5 */
	char   serase,
	       skill;
#endif	/* SYS5 */

#ifndef	SYS5
	(void) ioctl (0, TIOCGETP, (char *) &sg);
	(void) ioctl (0, TIOCGETC, (char *) &tc);
#else	/* SYS5 */
	(void) ioctl(0, TCGETA, &sg);
#endif	/* SYS5 */
	skill = KILL;
	serase = ERASE;
	KILL = killp ? chrcnv (killp) : skill;
	ERASE = erasep ? chrcnv (erasep) : serase;
#ifndef	SYS5
	(void) ioctl (0, TIOCSETN, (char *) &sg);
#else	/* SYS5 */
	(void) ioctl(0, TCSETAW, &sg);
#endif	/* SYS5 */

	chrdsp ("erase", ERASE);
	chrdsp (", kill", KILL);
	chrdsp (", intr", INTR);
	(void) putchar ('\n');
	(void) fflush (stdout);

	KILL = skill;
	ERASE = serase;
    }

/*  */

    sigint = 0;
    setsig (SIGINT, intrser);

    for (state = FLD;;) {
	switch (state = m_getfld (state, name, field, sizeof field, in)) {
	    case FLD: 
	    case FLDEOF: 
	    case FLDPLUS: 
		for (cp = field; *cp; cp++)
		    if (*cp != ' ' && *cp != '\t')
			break;
		if (*cp++ != '\n' || *cp != 0) {
		    printf ("%s:%s", name, field);
		    fprintf (out, "%s:%s", name, field);
		    while (state == FLDPLUS) {
			state =
			    m_getfld (state, name, field, sizeof field, in);
			printf ("%s", field);
			fprintf (out, "%s", field);
		    }
		}
		else {
		    printf ("%s: ", name);
		    (void) fflush (stdout);
		    i = getln (field, sizeof field);
		    if (i == -1) {
abort: ;
			if (killp || erasep)
#ifndef	SYS5
			    (void) ioctl (0, TIOCSETN, (char *) &sg);
#else	/* SYS5 */
			    (void) ioctl (0, TCSETA, &sg);
#endif	/* SYS5 */
			(void) unlink (tmpfil);
			done (1);
		    }
		    if (i != 0 || (field[0] != '\n' && field[0] != 0)) {
			fprintf (out, "%s:", name);
			do {
			    if (field[0] != ' ' && field[0] != '\t')
				(void) putc (' ', out);
			    fprintf (out, "%s", field);
			} while (i == 1
				    && (i = getln (field, sizeof field)) >= 0);
			if (i == -1)
			    goto abort;
		    }
		}
		if (state == FLDEOF) {/* moby hack */
		    fprintf (out, "--------\n");
		    printf ("--------\n");
		    if (!body)
			break;
		    goto no_body;
		}
		continue;

	    case BODY: 
	    case BODYEOF:
	    case FILEEOF: 
	        if (!body)
	            break;
		fprintf (out, "--------\n");
		if (field[0] == 0 || !prepend)
		    printf ("--------\n");
		if (field[0]) {
		    if (prepend && body) {
			printf ("\n--------Enter initial text\n\n");
			(void) fflush (stdout);
			for (;;) {
			    (void) getln (buffer, sizeof buffer);
			    if (doteof && buffer[0] == '.' && buffer[1] == '\n')
				break;
			    if (buffer[0] == 0)
				break;
			    fprintf (out, "%s", buffer);
			}
		    }

		    do {
			fprintf (out, "%s", field);
			if (!rapid && !sigint)
			    printf ("%s", field);
		    } while (state == BODY &&
			    (state = m_getfld (state, name, field, sizeof field, in)));
		    if (prepend || !body)
			break;
		    else
			printf ("\n--------Enter additional text\n\n");
		}
no_body: ;
		(void) fflush (stdout);
		for (;;) {
		    (void) getln (field, sizeof field);
		    if (doteof && field[0] == '.' && field[1] == '\n')
			break;
		    if (field[0] == 0)
			break;
 		    fprintf (out, "%s", field);
		}
		break;

	    default: 
		adios (NULLCP, "skeleton is poorly formatted");
	}
	break;
    }

    if (body)
	printf ("--------\n");
    (void) fflush (stdout);

    (void) fclose (in);
    (void) fclose (out);

    (void) signal (SIGINT, SIG_IGN);

/*  */

    if (killp || erasep)
#ifndef	SYS5
	(void) ioctl (0, TIOCSETN, (char *) &sg);
#else	/* SYS5 */
	(void) ioctl (0, TCSETAW, &sg);
#endif	/* SYS5 */

    if ((fdi = open (tmpfil, 0)) == NOTOK)
	adios (tmpfil, "unable to re-open");
    if ((fdo = creat (drft, m_gmprot ())) == NOTOK)
	adios (drft, "unable to write");
    cpydata (fdi, fdo, tmpfil, drft);
    (void) close (fdi);
    (void) close (fdo);
    (void) unlink (tmpfil);

    m_update ();

    done (0);
}

/*  */

getln (buffer, n)
char   *buffer;
int     n;
{
    int     c;
    char   *cp;

    cp = buffer;
    *cp = 0;

#ifndef	BSD42
    wtuser = 1;
#else	/* BSD42 */
    switch (setjmp (sigenv)) {
	case OK: 
	    wtuser = 1;
	    break;

	case DONE: 
	    wtuser = 0;
	    return 0;

	default: 
	    wtuser = 0;
	    return NOTOK;
    }
#endif	/* BSD42 */

    for (;;)
	switch (c = getchar ()) {
	    case EOF: 
#ifndef BSD42
		wtuser = 0;
		return (errno != EINTR ? 0 : NOTOK);
#else	/* BSD42 */
		clearerr (stdin);
		longjmp (sigenv, DONE);
#endif	/* BSD42 */

	    case '\n': 
		if (cp[-1] == QUOTE) {
		    cp[-1] = c;
		    wtuser = 0;
		    return 1;
		}
		*cp++ = c;
		*cp = 0;
		wtuser = 0;
		return 0;

	    default: 
		if (cp < buffer + n)
		    *cp++ = c;
		*cp = 0;
	}
}

/*  */

/* ARGSUSED */

static	TYPESIG intrser (i)
int    i;
{
#ifndef	BSD42
    (void) signal (SIGINT, intrser);
    if (!wtuser)
	sigint++;
#else	/* BSD42 */
    if (wtuser)
	longjmp (sigenv, NOTOK);
    sigint++;
#endif	/* BSD42 */
}


chrcnv (cp)
register char   *cp;
{
    return (*cp != QUOTE ? *cp : m_atoi (++cp));
}


chrdsp (s, c)
char   *s,
	c;
{
    printf ("%s ", s);
    if (c < ' ' || c == 0177)
	printf ("^%c", c ^ 0100);
    else
	printf ("%c", c);
}
