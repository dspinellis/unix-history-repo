/* mhn.c - multi-media MH */
#ifndef	lint
static char ident[] = "@(#)$Id: mhn.c,v 2.35 1993/10/26 22:17:44 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include "../zotnet/mts.h"
#include <sys/types.h>
#include <sys/stat.h>
#ifdef	BSD42
#include <sys/wait.h>
#endif
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	AUTOSW	   0
    "auto", 0,
#define	NAUTOSW	   1
    "noauto", 0,

#define	CACHESW	   2
    "cache", 0,
#define	NCACHESW   3
    "nocache", 0,

#define	CHECKSW    4
    "check", 0,
#define	NCHECKSW   5
    "nocheck", 0,

#define	DEBUGSW	   6
    "debug", -5,
    
#define	EBCDICSW   7
    "ebcdicsafe", 0,
#define	NEBCDICSW  8
    "noebcdicsafe", 0,

#define	FILESW	 9		/* interface from show */
    "file file", 0,

#define	FORMSW	   10
    "form formfile", 4,

#define	HEADSW	   11
    "headers", 0,
#define	NHEADSW	  12
    "noheaders", 0,

#define	LISTSW	  13
    "list", 0,
#define	NLISTSW	  14
    "nolist", 0,

#define	PARTSW	  15
    "part number", 0,

#define	PAUSESW	  16
    "pause", 0,
#define	NPAUSESW  17
    "nopause", 0,

#define	RCACHESW  18
    "rcache policy", 0,

#define	SIZESW	  19
    "realsize", 0,
#define	NSIZESW	  20
    "norealsize", 0,

#define	RFC934SW  21
    "rfc934mode", 0,
#define	NRFC934SW 22
    "norfc934mode", 0,

#define	SERIALSW  23
    "serialonly", 0,
#define	NSERIALSW 24
    "noserialonly", 0,

#define	SHOWSW	  25
    "show", 0,
#define	NSHOWSW	  26
    "noshow", 0,

#define	STORESW	  27
    "store", 0,
#define	NSTORESW  28
    "nostore", 0,

#define	TYPESW	  29
    "type content", 0,

#define	VERBSW	  30
    "verbose", 0,
#define	NVERBSW	  31
    "noverbose", 0,

#define	WCACHESW  32
    "wcache policy", 0,
    
#define	HELPSW	 33
    "help", 4,

#define	PROGSW   34
    "moreproc program", -4,
#define	NPROGSW	 35
    "nomoreproc", -3,

#define	LENSW	 36
    "length lines", -4,
#define	WIDSW	 37
    "width columns", -4,

#define	VIAMSW	 38
    "viamail mailpath", -7,
#define	VIASSW	 39
    "viasubj subject", -7,
#define	VIAPSW	 40
    "viaparm arguments", -7,
#define	VIADSW	 41
    "viadesc text", -7,
#define	VIACSW	 42
    "viacmnt text", -7,
#define	VIAZSW	 43
    "viadelay seconds", -8,
#define	VIAFSW	 44
    "viafrom mailpath", -7,

    NULL, 0
};

/*  */

#define	NPARTS	50
#define	NTYPES	20

static struct swit caches[] = {
#define	CACHE_NEVER	0
    "never", 0,
#define	CACHE_PRIVATE	1
    "private", 0,
#define	CACHE_PUBLIC	2
    "public", 0,
#define	CACHE_ASK	3
    "ask", 0,

    NULL, 0
};

static	int	autosw = 0;
static	int	cachesw = 0;
static	int	checksw = 0;
	int	debugsw = 0;
static	int	ebcdicsw = 0;
static	char   *formsw = NULLCP;
static	int	headsw = 1;
static	int	listsw = 0;
static	int	nolist = 0;
static	int	nomore = 0;
static	int	npart = 0;
static	char   *parts[NPARTS + 1];
static	int	pausesw = 1;
static	char   *progsw = NULLCP;
static	int	rcachesw = CACHE_ASK;
static	int	rfc934sw = 1;
static	int	serialsw = 0;
static	int	showsw = 0;
static	int	sizesw = 1;
static	int	storesw = 0;
static	int	ntype = 0;
static	char   *types[NTYPES + 1];
	int	verbosw = 0;
static	int	wcachesw = CACHE_ASK;

static	int	endian = 0;
static  char   *mm_charset = NULL;

static	int	xpid = 0;
static	int	userrs = 0;

static	char   *cache_public;
static	char   *cache_private;
static	int	cwdlen;
static	char   *cwd;
static	char   *dir;
static	char   *errs = NULL;
static	char   *tmp;


extern	int	errno;
#ifndef	BSD44
extern	int	sys_nerr;
extern	char   *sys_errlist[];
#endif

off_t	lseek ();
time_t	time ();

/*  */

#define	LSTFMT1		"%4s %-5s %-24s %5s %-36s\n"

#define	LSTFMT2a	"%4d "
#define	LSTFMT2b	"%-5s %-24.24s "
#define	LSTFMT2c1	"%5lu"
#define	LSTFMT2c2	"%4lu%c"
#define	LSTFMT2c3	"huge "
#define	LSTFMT2c4	"     "
#define	LSTFMT2d1	" %-36.36s"
#define	LSTFMT2d2	"\t     %-65.65s\n"

static void  build_comp ();

typedef struct CTinfo {
    char   *ci_type;
    char   *ci_subtype;

#define	NPARMS	10
    char   *ci_attrs[NPARMS + 2];
    char   *ci_values[NPARMS];

    char   *ci_comment;

    char   *ci_magic;
}	CTInfo, *CI;
#define	NULLCI	((CI) 0)

static int get_ctinfo ();
static int get_comment ();


typedef struct Content {
    char   *c_partno;		/* within multipart content */

    char   *c_vrsn;		/* Body-Version: */

    char   *c_ctline;		/* Content-Type: */
    CTInfo  c_ctinfo;

    int	    c_type;		/* internal form */
#define	CT_UNKNOWN	0x00
#define	CT_APPLICATION	0x01
#define	CT_AUDIO	0x02
#define	CT_IMAGE	0x03
#define	CT_MESSAGE	0x04
#define	CT_MULTIPART	0x05
#define	CT_TEXT		0x06
#define	CT_VIDEO	0x07
#define	CT_EXTENSION	0x08

    int	    c_subtype;		/* filled-in by c_ctinitfnx */
    caddr_t c_ctparams;		/*   .. */
    caddr_t c_ctextern;		/*   .. */
    char   *c_showproc;		/* default, if not in profile */
    char   *c_termproc;		/* for charset madness... */
    char   *c_storeproc;	/* overrides profile entry, if any */

    int	  (*c_ctinitfnx) ();	/* parse content */
    int	  (*c_ctlistfnx) ();	/* list content */
    int	  (*c_ctshowfnx) ();	/* show content */
    int	  (*c_ctstorefnx) ();	/* store content */
    int	  (*c_ctfreefnx) ();	/* free content-specific structures */


    char   *c_celine;		/* Content-Transfer-Encoding: */

    int	    c_encoding;		/* internal form */
#define	CE_UNKNOWN	0x00
#define	CE_BASE64	0x01
#define	CE_QUOTED	0x02
#define	CE_8BIT		0x03
#define	CE_7BIT		0x04
#define	CE_BINARY	0x05
#define	CE_EXTENSION	0x06
#define	CE_EXTERNAL	0x07	/* for external-body */

    caddr_t c_ceparams;		/* filled-in by encoding initfnx */

    int	  (*c_ceopenfnx) ();	/* get a stream to decoded contents */
    int	  (*c_ceclosefnx) ();	/* release stream */
    int	  (*c_celistfnx) ();	/* list decoding info */
    unsigned long
	  (*c_cesizefnx) ();	/* size of decoded contents */
    int	  (*c_cefreefnx) ();	/* free encoding-specific structures */


    char   *c_id;		/* Content-ID: */
    char   *c_descr;		/* Content-Description: */

    int	    c_digested;		/* Content-MD5: */
    unsigned char c_digest[16];	/*   .. */

    FILE   *c_fp;		/* read contents (stream) */
    char   *c_file;		/* read contents (file) */
    int	    c_unlink;		/* remove file when done? */
    int	    c_umask;		/* associated umask */
    long    c_begin;		/* where content starts in file */
    long    c_end;		/*   ..		 ends           */

    int	    c_pid;		/* process doing display */
    char   *c_storage;		/* write contents (file) */

    int	    c_rfc934;		/* rfc934 compatibility */
}	Content, *CT;
#define	NULLCT	((CT) 0)

static CT	get_content ();
static int	list_content (), show_content (), store_content ();
static int	cache_content ();
static int	user_content (), compose_content (), output_content ();
static void	free_content (), flush_errors (), set_id ();

#if	defined(__STDC__) && defined(VSPRINTF)
static void content_error (char *, register CT, char *, ...);
#else
static void content_error ();
#endif

static int   init_encoding (), type_ok (), copy_some_headers (), set_endian ();
static int   make_intermediates ();
static int   find_cache (), find_cache_aux (), find_cache_aux2 ();
static int   write7Bit (), writeQuoted (), writeBase64 (), writeBase64aux ();
static int   writeDigest (), readDigest ();
static int   via_mail (), via_post (), pidcheck ();

static	CT    *cts = NULL;


#define	quitser	pipeser
static	TYPESIG	pipeser ();
static	char   *fgetstr ();

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    int     f6 = 0,
	    msgp = 0,
            msgnum,
	   *icachesw;
    char   *cp,
	   *f1 = NULL,
	   *f2 = NULL,
	   *f3 = NULL,
	   *f4 = NULL,
	   *f5 = NULL,
	   *f7 = NULL,
	   *file = NULL,
           *folder = NULL,
           *maildir,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *msgs[MAXARGS];
    struct msgs *mp;
    register CT	 ct,
		*ctp;
    FILE	*fp;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    if (argv[1] && uprf (argv[1], "-via"))
	m_foil (NULLCP);
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [+folder] [msgs] [switches]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case AUTOSW:
		    autosw++;
		    continue;
		case NAUTOSW:
		    autosw = 0;
		    continue;

		case CACHESW:
		    cachesw++;
		    continue;
		case NCACHESW:
		    cachesw = 0;
		    continue;

		case RCACHESW:
		    icachesw = &rcachesw;
		    goto do_cache;
		case WCACHESW:
		    icachesw = &wcachesw;
do_cache: ;
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    switch (*icachesw = smatch (cp, caches)) {
			case AMBIGSW:
			    ambigsw (cp, caches);
			    done (1);
			case UNKWNSW:
			    adios (NULLCP, "%s unknown", cp);
			default:
			    break;
		    }
		    continue;

		case CHECKSW:
		    checksw++;
		    continue;
		case NCHECKSW:
		    checksw = 0;
		    continue;

		case DEBUGSW:
		    debugsw++;
		    continue;
    
		case EBCDICSW:
		    ebcdicsw++;
		    continue;
		case NEBCDICSW:
		    ebcdicsw = 0;
		    continue;

		case FORMSW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if (formsw)
			free (formsw);
		    formsw = getcpy (libpath (cp));
		    continue;

		case HEADSW:
		    headsw++;
		    continue;
		case NHEADSW:
		    headsw = 0;
		    continue;

		case LISTSW:
		    listsw++;
		    continue;
		case NLISTSW:
		    listsw = 0;
		    continue;

		case PARTSW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if (npart >= NPARTS)
			adios (NULLCP,
			       "too many parts (starting with %s), %d max",
			       cp, NPARTS);
		    parts[npart++] = cp;
		    continue;

		case PAUSESW:
		    pausesw++;
		    continue;
		case NPAUSESW:
		    pausesw = 0;
		    continue;

		case RFC934SW:
		    rfc934sw++;
		    continue;
		case NRFC934SW:
		    rfc934sw = 0;
		    continue;

		case SERIALSW:
		    serialsw++;
		    continue;
		case NSERIALSW:
		    serialsw = 0;
		    continue;

		case SHOWSW:
		    showsw++;
		    continue;
		case NSHOWSW:
		    showsw = 0;
		    continue;

		case SIZESW:
		    sizesw++;
		    continue;
		case NSIZESW:
		    sizesw = 0;
		    continue;

		case STORESW:
		    storesw++;
		    continue;
		case NSTORESW:
		    storesw = 0;
		    continue;

		case TYPESW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if (ntype >= NTYPES)
			adios (NULLCP,
			       "too many types (starting with %s), %d max",
			       cp, NTYPES);
		    types[ntype++] = cp;
		    continue;

		case VERBSW: 
		    verbosw++;
		    continue;
		case NVERBSW: 
		    verbosw = 0;
		    continue;

		case PROGSW:
		    if (!(progsw = *argp++) || *progsw == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case NPROGSW:
		    nomore++;
		    continue;

		case LENSW:
		case WIDSW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case FILESW:
		    if (!(cp = *argp++) || (*cp == '-' && cp[1]))
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    file = *cp == '-' ? cp : path (cp, TFILE);
		    continue;

	       case VIAMSW:
		    if (!(f1 = *argp++))
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
	       case VIASSW:
		    if (!(f2 = *argp++))
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
	       case VIAPSW:
		    if (!(f3 = *argp++))
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
	       case VIADSW:
		    if (!(f4 = *argp++))
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
	       case VIACSW:
		    if (!(f5 = *argp++))
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
	       case VIAZSW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if (sscanf (cp, "%d", &f6) != 1 || f6 < 0)
			f6 = 300;
		    continue;
	       case VIAFSW:
		    if (!(f7 = *argp++))
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    if (folder)
		adios (NULLCP, "only one folder at a time!");
	    else
		folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	}
	else
	    msgs[msgp++] = cp;
    }
    parts[npart] = NULL, types[ntype] = NULL;
    if (!formsw)
	formsw = getcpy (libpath ("mhl.headers"));

    set_endian ();

/*  */

    if (f1) {
	via_mail (f1, f2, f3, f4, f5, f6, f7);
	/* NOTREACHED */
    }
    else
	if (f2 || f3 || f4 || f5 || f6 || f7)
	    adios (NULLCP, "missing -viamail \"mailpath\" switch");

    if (cp = getenv ("MHN")) {
	if (fp = fopen (cp, "r")) {
	    m_readefs ((struct node **) 0, fp, cp, 0);

	    (void) fclose (fp);
	}
	else
	    admonish ("", "unable to read $MHN profile (%s)", cp);
    }
    if (fp = fopen (cp = libpath ("mhn_defaults"), "r")) {
	m_readefs ((struct node **) 0, fp, cp, 0);

	(void) fclose (fp);
    }

    (void) sprintf (buf, "%s-cache", invo_name);
    if ((cache_public = m_find (buf)) && *cache_public != '/')
	cache_public = NULL;
    (void) sprintf (buf, "%s-private-cache", invo_name);
    if (!(cache_private = m_find (buf)))
	cache_private = ".cache";
    cache_private = getcpy (m_maildir (cache_private));

    cwdlen = strlen (cwd = getcpy (pwd ()));
    (void) sprintf (buf, "%s-storage", invo_name);
    dir = getcpy ((cp = m_find (buf)) && *cp ? cp : cwd);
    tmp = cp && *cp ? concat (cp, "/", invo_name, NULLCP)
		    : add (m_maildir (invo_name), NULLCP);

    if (!m_find ("path"))
	free (path ("./", TFOLDER));

    if (msgp == 1
	    && !folder
	    && !npart
	    && !cachesw
	    && !showsw
	    && !storesw
	    && !ntype
	    && !file
	    && (cp = getenv ("mhdraft"))
	    && strcmp (cp, msgs[0]) == 0) {
	build_comp (cp);
	/* NOTREACHED */
    }

    if (file) {
	int	stdinP;

	if (msgp)
	    adios (NULLCP, "only one file at a time!");

	if ((cts = (CT *) calloc ((unsigned) 2, sizeof *cts)) == NULL)
	    adios (NULLCP, "out of memory");

	ctp = cts;
	if (stdinP = (strcmp (file, "-") == 0)) {
	    char    buffer[BUFSIZ];

	    file = add (m_tmpfil (invo_name), NULLCP);

	    if ((fp = fopen (file, "w+")) == NULL)
		adios (file, "unable to fopen for writing and reading");
	    (void) chmod (file, 0600);
	    while (fgets (buffer, sizeof buffer, stdin))
		(void) fputs (buffer, fp);
	    (void) fflush (fp);

	    if (ferror (stdin)) {
		(void) unlink (file);
		adios ("stdin", "error reading");
	    }

	    if (ferror (fp)) {
		(void) unlink (file);
		adios (file, "error writing");
	    }

	    (void) fseek (fp, 0L, 0);
	}
	else
	    if ((fp = fopen (file, "r")) == NULL)
		adios (file, "unable to read");

	if (ct = get_content (fp, file, 1)) {
	    if (stdinP)
		ct -> c_unlink = 1;

	    ct -> c_fp = NULL;
	    if (ct -> c_end == 0L) {
		(void) fseek (fp, 0L, 2);
		ct -> c_end = ftell (fp);
	    }
	    if (ct -> c_ctinitfnx && (*ct -> c_ctinitfnx) (ct) == NOTOK)
		free_content (ct);
	    else
		*ctp++ = ct;
	}
	else
	    advise (NULLCP, "unable to decode %s", file);

	(void) fclose (fp);

	mp = NULL;
	goto go_to_it;
    }

    if (!msgp)
	msgs[msgp++] = "cur";
    if (!folder)
	folder = m_getfolder ();
    maildir = m_maildir (folder);

    if (chdir (maildir) == NOTOK)
	adios (maildir, "unable to change directory to");
    if (!(mp = m_gmsg (folder)))
	adios (NULLCP, "unable to read folder %s", folder);
    if (mp -> hghmsg == 0)
	adios (NULLCP, "no messages in %s", folder);

    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    done (1);
    m_setseq (mp);

    if ((cts = (CT *) calloc ((unsigned) (mp -> numsel + 1), sizeof *cts))
	    == NULL)
	adios (NULLCP, "out of memory");

    ctp = cts;
    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED) {
	    char   *msgnam;

	    if ((fp = fopen (msgnam = m_name (msgnum), "r")) == NULL)
		adios (msgnam, "unable to read message");

	    if (ct = get_content (fp, msgnam, 1)) {
		ct -> c_fp = NULL;
		if (ct -> c_end == 0L) {
		    (void) fseek (fp, 0L, 2);
		    ct -> c_end = ftell (fp);
		}
		if (ct -> c_ctinitfnx && (*ct -> c_ctinitfnx) (ct) == NOTOK)
		    free_content (ct);
		else
		    *ctp++ = ct;
	    }
	    else
		advise (NULLCP, "unable to decode message %s", msgnam);

	    (void) fclose (fp);
	}

go_to_it: ;
    if (!*cts)
	done (1);

    if (!listsw && !showsw && !storesw && !cachesw)
	showsw++;

/* listsw && showsw		-> user wants per-message listing,
				   so delay until showsw processing

	  && storesw && sizesw	-> evaluating size will cause more work,
				   so delay until after storesw processing
 */
    userrs = 1;
    (void) signal (SIGQUIT, quitser);
    (void) signal (SIGPIPE, pipeser);

    for (ctp = cts; ct = *ctp; ctp++)
	if (type_ok (ct, 1)
	        && (ct -> c_ctlistfnx
		        || ct -> c_ctstorefnx
		        || ct -> c_ctshowfnx)) {
	    struct	stat st;

	    if (!ct -> c_umask)
		ct -> c_umask = ~(stat (ct -> c_file, &st) != NOTOK
					? (st.st_mode & 0777) : m_gmprot ());
	}

    if (listsw && !showsw && (!storesw || !sizesw)) {
	if (headsw)
	    printf (LSTFMT1, "msg", "part", "type/subtype", "size",
		    "description");

	for (ctp = cts; ct = *ctp; ctp++)
	    if (type_ok (ct, 1) && ct -> c_ctlistfnx) {
		(void) umask (ct -> c_umask);
		(void) (*ct -> c_ctlistfnx) (ct, 1);
		if (ct -> c_fp)
		    (void) fclose (ct -> c_fp), ct -> c_fp = NULL;
		if (ct -> c_ceclosefnx)
		    (*ct -> c_ceclosefnx) (ct);
	    }

	flush_errors ();
    }

    if (storesw) {
	for (ctp = cts; ct = *ctp; ctp++)
	    if (type_ok (ct, 1) && ct -> c_ctstorefnx) {
		(void) umask (ct -> c_umask);
		(void) (*ct -> c_ctstorefnx) (ct, NULLCP);
		if (ct -> c_fp)
		    (void) fclose (ct -> c_fp), ct -> c_fp = NULL;
		if (ct -> c_ceclosefnx)
		    (*ct -> c_ceclosefnx) (ct);
	    }

	flush_errors ();
    }

    if (cachesw) {
	for (ctp = cts; ct = *ctp; ctp++)
	    if (type_ok (ct, 1)) {
		cache_content (ct);
		if (ct -> c_fp)
		    (void) fclose (ct -> c_fp), ct -> c_fp = NULL;
		if (ct -> c_ceclosefnx)
		    (*ct -> c_ceclosefnx) (ct);
	    }

	flush_errors ();
    }

    if (listsw && !showsw && storesw && sizesw) {
	if (headsw)
	    printf (LSTFMT1, "msg", "part", "type/subtype", "size",
		    "description");

	for (ctp = cts; ct = *ctp; ctp++)
	    if (type_ok (ct, 1) && ct -> c_ctlistfnx) {
		(void) umask (ct -> c_umask);
		(void) (*ct -> c_ctlistfnx) (ct, 1);
		if (ct -> c_fp)
		    (void) fclose (ct -> c_fp), ct -> c_fp = NULL;
		if (ct -> c_ceclosefnx)
		    (*ct -> c_ceclosefnx) (ct);
	    }

	flush_errors ();
	listsw = 0;
    }

    if (showsw)
	for (ctp = cts; ct = *ctp; ctp++) {
#if defined(BSD42) && !defined(WAITINT)
	    union wait status;
#else
	    int     status;
#endif
	    TYPESIG (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();

	    if (!type_ok (ct, 0))
		continue;

	    (void) umask (ct -> c_umask);

	    if (listsw) {
		if (headsw)
		    printf (LSTFMT1, "msg", "part", "type/subtype", "size",
			    "description");

		if (ct -> c_ctlistfnx)
		    (void) (*ct -> c_ctlistfnx) (ct, 1);
	    }

	    if (!ct -> c_ctshowfnx) {
		if (ct -> c_fp)
		    (void) fclose (ct -> c_fp), ct -> c_fp = NULL;
		if (ct -> c_ceclosefnx)
		    (*ct -> c_ceclosefnx) (ct);
		continue;
	    }

	    if (strcmp (formsw, "mhl.null")) {
		int	child_id,
			i,
			vecp;
		char   *vec[8];

		vecp = 0;
		vec[vecp++] = r1bindex (mhlproc, '/');
		vec[vecp++] = "-form";
		vec[vecp++] = formsw;
		vec[vecp++] = "-nobody";
		vec[vecp++] = ct -> c_file;
		if (nomore)
		    vec[vecp++] = "-nomoreproc";
		else
		    if (progsw) {
			vec[vecp++] = "-moreproc";
			vec[vecp++] = progsw;
		    }
		vec[vecp] = NULL;

		(void) fflush (stdout);

		for (i = 0; (child_id = vfork ()) == NOTOK && i < 5; i++)
		    sleep (5);
		switch (child_id) {
	            case NOTOK:
		        adios ("fork", "unable to");
			/* NOTREACHED */

		    case OK:
			(void) execvp (mhlproc, vec);
			fprintf (stderr, "unable to exec ");
			perror (mhlproc);
			_exit (-1);
			/* NOTREACHED */

		    default:
			xpid = -child_id;
			break;
		}
	    }
	    else
		xpid = 0;

	    (void) (*ct -> c_ctshowfnx) (ct, 1, 0);
	    if (ct -> c_fp)
		(void) fclose (ct -> c_fp), ct -> c_fp = NULL;
	    if (ct -> c_ceclosefnx)
		(*ct -> c_ceclosefnx) (ct);

	    hstat = signal (SIGHUP, SIG_IGN);
	    istat = signal (SIGINT, SIG_IGN);
	    qstat = signal (SIGQUIT, SIG_IGN);
	    tstat = signal (SIGTERM, SIG_IGN);

	    while (wait (&status) != NOTOK) {
#if defined(BSD42) && !defined(WAITINT)
		(void) pidcheck (status.w_status);
#else
		(void) pidcheck (status);
#endif
		continue;
	    }

	    (void) signal (SIGHUP, hstat);
	    (void) signal (SIGINT, istat);
	    (void) signal (SIGQUIT, qstat);
	    (void) signal (SIGTERM, tstat);

	    xpid = 0;

	    flush_errors ();
	}

    for (ctp = cts; *ctp; ctp++)
	free_content (*ctp);
    free ((char *) cts);
    cts = NULL;

    if (mp) {
	m_replace (pfolder, folder);
	if (mp -> hghsel != mp -> curmsg)
	    m_setcur (mp, mp -> hghsel);
	m_sync (mp);
	m_update ();
    }

    done (0);
    /* NOTREACHED */
}

/*  */

static TYPESIG  pipeser (i)
int	i;
{
    if (i == SIGQUIT) {
	(void) unlink ("core");

	(void) fflush (stdout);

	fprintf (stderr, "\n");
	(void) fflush (stderr);
    }

    done (1);
    /* NOTREACHED */
}

/*  */

#include "../h/mhn.h"


struct str2init {
    char   *si_key;
    int	    si_value;
    int   (*si_init) ();
};


static int InitApplication (), InitMessage (), InitMultiPart (), InitText (),
    	   InitGeneric ();
    	

static struct str2init str2cts[] = {
    "application",  CT_APPLICATION, InitApplication,
    "audio",	    CT_AUDIO,	    InitGeneric,
    "image",	    CT_IMAGE,	    InitGeneric,
    "message",	    CT_MESSAGE,	    InitMessage,
    "multipart",    CT_MULTIPART,   InitMultiPart,
    "text",	    CT_TEXT,	    InitText,
    "video",	    CT_VIDEO,	    InitGeneric,

    NULL,	    CT_EXTENSION,   NULL,	/* these two must be last! */
    NULL,	    CT_UNKNOWN,	    NULL,
};


static int InitBase64 (), InitQuoted (), Init7Bit ();

static struct str2init str2ces[] = {
    "base64",		CE_BASE64,	InitBase64,
    "quoted-printable", CE_QUOTED,	InitQuoted,
    "8bit",		CE_8BIT,	Init7Bit,
    "7bit",		CE_7BIT,	Init7Bit,
    "binary",		CE_BINARY,	NULL,

    NULL,		CE_EXTENSION,	NULL,	/* these two must be last! */
    NULL,		CE_UNKNOWN,	NULL,
};

/*  */

static	CT	get_content (in, file, toplevel)
FILE   *in;
char   *file;
int	toplevel;
{
    int     compnum,
	    state;
    char    buf[BUFSIZ],
	    name[NAMESZ];
    register CT	ct;

    if ((ct = (CT) calloc (1, sizeof *ct)) == NULL)
	adios (NULLCP, "out of memory");

    ct -> c_begin = ftell (ct -> c_fp = in) + 1;
    ct -> c_file = add (file, NULLCP);
    for (compnum = 1, state = FLD;;) {
	switch (state = m_getfld (state, name, buf, sizeof buf, in)) {
	    case FLD:
	    case FLDPLUS:
	    case FLDEOF:
	        compnum++;

		if (uleq (name, VRSN_FIELD)) {
		    int	    ucmp;
		    char    c,
		           *cp,
			   *dp;

		    cp = add (buf, NULLCP);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, in);
			cp = add (buf, cp);
		    }

		    if (ct -> c_vrsn) {
			advise (NULLCP,
				"message %s has multiple %s: fields (%s)",
				ct -> c_file, VRSN_FIELD, dp = trimcpy (cp));
			free (dp);
			free (cp);
			goto out;
		    }

		    ct -> c_vrsn = cp;
		    while (isspace (*cp))
			cp++;
		    for (dp = index (cp, '\n'); dp; dp = index (dp, '\n'))
			*dp++ = ' ';
		    for (dp = cp + strlen (cp) - 1; dp >= cp; dp--)
			if (!isspace (*dp))
			    break;
		    *++dp = '\0';
		    if (debugsw)
			fprintf (stderr, "%s: %s\n", VRSN_FIELD, cp);

		    if (*cp == '(' && get_comment (ct, &cp, 0) == NOTOK)
			goto out;

		    for (dp = cp; istoken (*dp); dp++)
			continue;
		    c = *dp, *dp = '\0';
		    ucmp = uleq (cp, VRSN_VALUE);
		    *dp = c;
		    if (!ucmp)
			admonish (NULLCP,
				"message %s has unknown value for %s: field (%s)",
				ct -> c_file, VRSN_FIELD, cp);
		    goto got_header;
		}

		if (uleq (name, TYPE_FIELD)) {
		    register char  *cp;
		    register struct str2init *s2i;
		    register CI	    ci = &ct -> c_ctinfo;

		    cp = add (buf, NULLCP);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, in);
			cp = add (buf, cp);
		    }

		    if (ct -> c_ctline) {
			char   *dp = trimcpy (cp);

			advise (NULLCP,
				"message %s has multiple %s: fields (%s)",
				ct -> c_file, TYPE_FIELD, dp);
			free (dp);
			free (cp);
			goto out;
		    }

		    if (get_ctinfo (cp, ct, 0) == NOTOK)
			goto out;
		    for (s2i = str2cts; s2i -> si_key; s2i++)
			if (uleq (ci -> ci_type, s2i -> si_key))
			    break;
		    if (!s2i -> si_key && !uprf (ci -> ci_type, "X-"))
			s2i++;
		    ct -> c_type = s2i -> si_value;
		    ct -> c_ctinitfnx = s2i -> si_init;
		    goto got_header;
		}

		if (uleq (name, ENCODING_FIELD)) {
		    register char *cp,
				  *dp;
		    char    c;
		    register struct str2init *s2i;

		    cp = add (buf, NULLCP);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, in);
			cp = add (buf, cp);
		    }

		    if (ct -> c_celine) {
			advise (NULLCP,
				"message %s has multiple %s: fields (%s)",
				ct -> c_file, ENCODING_FIELD,
				dp = trimcpy (cp));
			free (dp);
			free (cp);
			goto out;
		    }

		    ct -> c_celine = cp;
		    while (isspace (*cp))
			cp++;
		    for (dp = cp; istoken (*dp); dp++)
			continue;
		    c = *dp, *dp = '\0';
		    for (s2i = str2ces; s2i -> si_key; s2i++)
			if (uleq (cp, s2i -> si_key))
			    break;
		    if (!s2i -> si_key && !uprf (cp, "X-"))
			s2i++;
		    *dp = c;
		    ct -> c_encoding = s2i -> si_value;
		    if (s2i -> si_init && (*s2i -> si_init) (ct) == NOTOK)
			goto out;
		    goto got_header;
		}

		if (uleq (name, ID_FIELD)) {
		    ct -> c_id = add (buf, ct -> c_id);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, in);
			ct -> c_id = add (buf, ct -> c_id);
		    }
		    goto got_header;
		}

		if (uleq (name, DESCR_FIELD)) {
		    ct -> c_descr = add (buf, ct -> c_descr);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, in);
			ct -> c_descr = add (buf, ct -> c_descr);
		    }
		    goto got_header;
		}

		if (uleq (name, MD5_FIELD)) {
		    char   *cp,
		           *dp,
		           *ep;

		    cp = add (buf, NULLCP);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, in);
			cp = add (buf, cp);
		    }

		    if (!checksw) {
			free (cp);
			goto got_header;
		    }

		    if (ct -> c_digested) {
			advise (NULLCP,
				"message %s has multiple %s: fields (%s)",
				ct -> c_file, MD5_FIELD,
				dp = trimcpy (cp));
			free (dp);
			free (cp);
			goto out;
		    }

		    ep = cp;
		    while (isspace (*cp))
			cp++;
		    for (dp = index (cp, '\n'); dp; dp = index (dp, '\n'))
			*dp++ = ' ';
		    for (dp = cp + strlen (cp) - 1; dp >= cp; dp--)
			if (!isspace (*dp))
			    break;
		    *++dp = '\0';
		    if (debugsw)
			fprintf (stderr, "%s: %s\n", MD5_FIELD, cp);

		    if (*cp == '(' && get_comment (ct, &cp, 0) == NOTOK) {
			free (ep);
			goto out;
		    }

		    for (dp = cp; *dp && !isspace (*dp); dp++)
			continue;
		    *dp = '\0';

		    (void) readDigest (ct, cp);
		    free (ep);
		    ct -> c_digested++;
		    goto got_header;
		}

#ifdef	notdef
		if (uprf (name, XXX_FIELD_PRF))
		    advise (NULLCP, "unknown field (%s) in message %s",
			    name, ct -> c_file);
		/* and fall... */
#endif

		while (state == FLDPLUS)
		    state = m_getfld (state, name, buf, sizeof buf, in);
got_header: ;
		if (state != FLDEOF) {
		    ct -> c_begin = ftell (in) + 1;
		    continue;
		}
		/* else fall... */

	    case BODY:
	    case BODYEOF:
		break;

	    case FILEEOF:
		ct -> c_begin = ftell (in);
		break;

	    case LENERR:
	    case FMTERR:
		adios (NULLCP, "message format error in component #%d",
		       compnum);

	    default:
		adios (NULLCP, "getfld() returned %d", state);
	}
	break;
    }

    if (!ct -> c_ctline) {
	if (toplevel < 0) {
	    if (get_ctinfo ("message/rfc822", ct, 0) == NOTOK)
		goto out;
	    ct -> c_type = CT_MESSAGE;
	    ct -> c_ctinitfnx = InitMessage;
	}
	else {
	    if (get_ctinfo ("text/plain", ct, 0) == NOTOK)
		goto out;
	    ct -> c_type = CT_TEXT;
	    ct -> c_ctinitfnx = InitText;
	}
    }
    if (!ct -> c_ctlistfnx)
	ct -> c_ctlistfnx = list_content;
    if (!ct -> c_ctshowfnx)
	ct -> c_ctshowfnx = show_content;
    if (!ct -> c_ctstorefnx)
	ct -> c_ctstorefnx = store_content;

    if (!ct -> c_celine) {
	ct -> c_encoding = CE_7BIT;
	(void) Init7Bit (ct);
    }

    return ct;

out:
    free_content (ct);
    return NULLCT;
}

/*  */

static int  get_ctinfo (cp, ct, magic)
char  *cp;
register CT	ct;
int	magic;
{
    int	    i = strlen (invo_name) + 2;
    register char  *dp,
		  **ap,
		  **ep;
    char    c;
    register CI	    ci = &ct -> c_ctinfo;

    cp = ct -> c_ctline = add (cp, NULLCP);
    while (isspace (*cp))
	cp++;
    for (dp = index (cp, '\n'); dp; dp = index (dp, '\n'))
	*dp++ = ' ';
    for (dp = cp + strlen (cp) - 1; dp >= cp; dp--)
	if (!isspace (*dp))
	    break;
    *++dp = '\0';
    if (debugsw)
	fprintf (stderr, "%s: %s\n", TYPE_FIELD, cp);

    if (*cp == '(' && get_comment (ct, &cp, 1) == NOTOK)
	return NOTOK;

    for (dp = cp; istoken (*dp); dp++)
	continue;
    c = *dp, *dp = '\0';
    ci -> ci_type = add (cp, NULLCP);
    *dp = c, cp = dp;

    if (!*ci -> ci_type) {
	advise (NULLCP, "invalid %s: field in message %s (empty type)", 
		TYPE_FIELD, ct -> c_file);
	return NOTOK;
    }

    for (dp = ci -> ci_type; *dp; dp++)
	if (isalpha(*dp) && isupper (*dp))
	    *dp = tolower (*dp);

    while (isspace (*cp))
	cp++;

    if (*cp == '(' && get_comment (ct, &cp, 1) == NOTOK)
	return NOTOK;

    if (*cp != '/') {
	if (!magic)
	    ci -> ci_subtype = add ("", NULLCP);
	goto magic_skip;
    }

    cp++;
    while (isspace (*cp))
	cp++;

    if (*cp == '(' && get_comment (ct, &cp, 1) == NOTOK)
	return NOTOK;

    for (dp = cp; istoken (*dp); dp++)
	continue;
    c = *dp, *dp = '\0';
    ci -> ci_subtype = add (cp, NULLCP);
    *dp = c, cp = dp;

    if (!*ci -> ci_subtype) {
	advise (NULLCP,
		"invalid %s: field in message %s (empty subtype for \"%s\")",
		TYPE_FIELD, ct -> c_file, ci -> ci_type);
	return NOTOK;
    }

    for (dp = ci -> ci_subtype; *dp; dp++)
	if (isalpha(*dp) && isupper (*dp))
	    *dp = tolower (*dp);

magic_skip: ;
    while (isspace (*cp))
	cp++;

    if (*cp == '(' && get_comment (ct, &cp, 1) == NOTOK)
	return NOTOK;

    ep = (ap = ci -> ci_attrs) + NPARMS;
    while (*cp == ';') {
	char   *vp,
	       *up;

	if (ap >= ep) {
	    advise (NULLCP,
		    "too many parameters in message %s's %s: field (%d max)",
		    ct -> c_file, TYPE_FIELD, NPARMS);
	    return NOTOK;
	}

	cp++;
	while (isspace (*cp))
	    cp++;

	if (*cp == '(' && get_comment (ct, &cp, 1) == NOTOK)
	    return NOTOK;

	if (*cp == 0) {
	    advise (NULLCP,
		    "extraneous trailing ';' in message %s's %s: parameter list",
		    ct -> c_file, TYPE_FIELD);
	    return OK;
	}

	for (dp = cp; istoken (*dp); dp++)
	    if (isalpha(*dp) && isupper (*dp))
		*dp = tolower (*dp);
	for (up = dp; isspace (*dp); )
	    dp++;
	if (dp == cp || *dp != '=') {
	    advise (NULLCP,
		    "invalid parameter in message %s's %s: field\n%*.*sparameter %s (error detected at offset %d)",
		    ct -> c_file, TYPE_FIELD, i, i, "", cp, dp - cp);
	    return NOTOK;
	}

	vp = (*ap = add (cp, NULLCP)) + (up - cp);
	*vp = '\0';
	for (dp++; isspace (*dp); )
	    dp++;
	ci -> ci_values[ap - ci -> ci_attrs] = vp = *ap + (dp - cp);
	if (*dp == '"') {
	    for (cp = ++dp, dp = vp;;) {
		switch (c = *cp++) {
		    case '\0':
bad_quote: ;
		        advise (NULLCP,
				"invalid quoted-string in message %s's %s: field\n%*.*s(parameter %s)",
				ct -> c_file, TYPE_FIELD, i, i, "", *ap);
			return NOTOK;

		    case '\\':
			*dp++ = c;
			if ((c = *cp++) == '\0')
			    goto bad_quote;
			/* else fall... */

		    default:
    			*dp++ = c;
    			continue;

		    case '"':
			*dp = '\0';
			break;
		}
		break;
	    }
	}
	else {
	    for (cp = dp, dp = vp; istoken (*cp); cp++, dp++)
		continue;
	    *dp = '\0';
	}
	if (!*vp) {
	    advise (NULLCP,
		    "invalid parameter in message %s's %s: field\n%*.*s(parameter %s)",
		    ct -> c_file, TYPE_FIELD, i, i, "", *ap);
	    return NOTOK;
	}
	ap++;

	while (isspace (*cp))
	    cp++;

	if (*cp == '(' && get_comment (ct, &cp, 1) == NOTOK)
	    return NOTOK;
    }

    if (magic && *cp == '<') {
	if (ct -> c_id)
	    free (ct -> c_id), ct -> c_id = NULL;

	if (!(dp = index (ct -> c_id = ++cp, '>'))) {
	    advise (NULLCP, "invalid ID in message %s", ct -> c_file);
	    return NOTOK;
	}
	
	c = *dp, *dp = '\0';
	if (*ct -> c_id)
	    ct -> c_id = concat ("<", ct -> c_id, ">\n", NULLCP);
	else
	    ct -> c_id = NULL;
	*dp++ = c, cp = dp;

	while (isspace (*cp))
	    cp++;
    }

    if (magic && *cp == '[') {
	ct -> c_descr = ++cp;
	for (dp = cp + strlen (cp) - 1; dp >= cp; dp--)
	    if (*dp == ']')
		break;
	if (dp < cp) {
	    advise (NULLCP, "invalid description in message %s", ct -> c_file);
	    ct -> c_descr = NULL;
	    return NOTOK;
	}
	
	c = *dp, *dp = '\0';
	if (*ct -> c_descr)
	    ct -> c_descr = concat (ct -> c_descr, "\n", NULLCP);
	else
	    ct -> c_descr = NULL;
	*dp++ = c, cp = dp;

	while (isspace (*cp))
	    cp++;
    }

    if (*cp) {
	if (magic)
	    ci -> ci_magic = add (cp, NULLCP);
	else
	    advise (NULLCP,
		    "extraneous information in message %s's %s: field\n%*.*s(%s)",
		ct -> c_file, TYPE_FIELD, i, i, "", cp);
    }

    return OK;
}

/*  */

static int  get_comment (ct, ap, istype)
CT	ct;
char  **ap;
int	istype;
{
    register int    i;
    register char  *bp,
		   *cp;
    char    c,
	    buffer[BUFSIZ],
	   *dp;
    register CI	    ci = &ct -> c_ctinfo;

    cp = *ap;

    bp = buffer;
    cp++;
    for (i = 0;;) {
	switch (c = *cp++) {
	    case '\0':
invalid: ;
	        advise (NULLCP, "invalid comment in message %s's %s: field",
			ct -> c_file, istype ? TYPE_FIELD : VRSN_FIELD);
		return NOTOK;

	    case '\\':
		*bp++ = c;
		if ((c = *cp++) == '\0')
		    goto invalid;
		*bp++ = c;
		continue;

	    case '(':
		i++;
		/* and fall... */
    	    default:
		*bp++ = c;
		continue;

	    case ')':
		if (--i < 0)
		    break;
		*bp++ = c;
		continue;
	}
	break;
    }
    *bp = '\0';

    if (istype) {
	if (dp = ci -> ci_comment) {
	    ci -> ci_comment = concat (dp, " ", buffer, NULLCP);
	    free (dp);
	}
	else
	    ci -> ci_comment = add (buffer, NULLCP);
    }

    while (isspace (*cp))
	cp++;

    *ap = cp;

    return OK;
}

/*  */

#define	empty(s)	((s) ? (s) : "")


static int  list_content (ct, toplevel)
register CT	ct;
int	toplevel;
{
    unsigned long size;
    register char **ap,
		  **ep;
    char   *cp,
	    buffer[BUFSIZ];
    register CI	    ci = &ct -> c_ctinfo;

    printf (toplevel > 0 ? LSTFMT2a : toplevel < 0 ? "part " : "     ",
	    atoi (r1bindex (empty (ct -> c_file), '/')));
    (void) sprintf (buffer, "%s/%s", empty (ci -> ci_type),
		    empty (ci -> ci_subtype));
    printf (LSTFMT2b, empty (ct -> c_partno), buffer);

    size = ct -> c_cesizefnx && sizesw ? (*ct -> c_cesizefnx) (ct)
				       : ct -> c_end - ct -> c_begin;

    for (cp = " KMGT"; size > 9999; size >>= 10)
	if (!*++cp)
	    break;
    switch (*cp) {
        case ' ':
	    if (size > 0 || ct -> c_encoding != CE_EXTERNAL)
		printf (LSTFMT2c1, size);
	    else
		printf (LSTFMT2c4);
	    break;

	default:
	    printf (LSTFMT2c2, size, *cp);
	    break;

	case '\0':
	    printf (LSTFMT2c3);
    }

    if (ct -> c_descr) {
	char   *dp;

	dp = trimcpy (cp = add (ct -> c_descr, NULLCP));
	free (cp);
	printf (LSTFMT2d1, dp);
	free (dp);
    }

    printf ("\n");

    if (verbosw && ci -> ci_comment) {
	char   *dp;

	dp = trimcpy (cp = add (ci -> ci_comment, NULLCP));
	free (cp);
	(void) sprintf (buffer, "(%s)", dp);
	free (dp);
	printf (LSTFMT2d2, buffer);
    }

    if (!debugsw)
	return OK;

    (void) fflush (stdout);

    fprintf (stderr, "  partno \"%s\"\n", empty (ct -> c_partno));

    if (ct -> c_vrsn)
	fprintf (stderr, "  %s:%s\n", VRSN_FIELD, ct -> c_vrsn);

    if (ct -> c_ctline)
	fprintf (stderr, "  %s:%s", TYPE_FIELD, ct -> c_ctline);
    fprintf (stderr,
	     "    type \"%s\"  subtype \"%s\"  comment \"%s\"  magic \"%s\"\n",
	     empty (ci -> ci_type), empty (ci -> ci_subtype),
	     empty (ci -> ci_comment), empty (ci -> ci_magic));
    for (ap = ci -> ci_attrs, ep = ci -> ci_values; *ap; ap++, ep++)
	fprintf (stderr, "      parameter %s=\"%s\"\n", *ap, *ep);
    fprintf (stderr,
	     "    type 0x%x subtype 0x%x params 0x%x\n",
	     ct -> c_type, ct -> c_subtype, ct -> c_ctparams);

    fprintf (stderr, "     showproc \"%s\"\n", empty (ct -> c_showproc));
    fprintf (stderr, "     termproc \"%s\"\n", empty (ct -> c_termproc));
    fprintf (stderr, "    storeproc \"%s\"\n", empty (ct -> c_storeproc));

    if (ct -> c_celine)
	fprintf (stderr, "  %s:%s", ENCODING_FIELD, ct -> c_celine);
    fprintf (stderr, "    encoding 0x%x params 0x%x\n",
	     ct -> c_encoding, ct -> c_ceparams);

    if (ct -> c_id)
	fprintf (stderr, "  %s:%s", ID_FIELD, ct -> c_id);
    if (ct -> c_descr)
	fprintf (stderr, "  %s:%s", DESCR_FIELD, ct -> c_descr);

    fprintf (stderr, "  fp 0x%x file \"%s\" begin %d end %d\n",
	     ct -> c_fp, empty (ct -> c_file), ct -> c_begin, ct -> c_end);

    if (ct -> c_celistfnx)
	(void) (*ct -> c_celistfnx) (ct);

    return OK;
}
#undef	empty

/*  */

#ifdef VSPRINTF
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#endif

#ifdef VSPRINTF
#ifdef __STDC__
static void content_error (char *what, register CT ct, char *fmt, ...)
#else
static void  content_error (va_alist)
va_dcl
#endif
#else	/* !VSPRINTF */
/* VARARGS3 */
static void  content_error (what, ct, fmt, a, b, c, d, e, f)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
register CT	ct;
#endif
{
#ifdef VSPRINTF
    va_list arglist;
#endif
#if defined(VSPRINTF) && !defined(__STDC__)
    char *what, *fmt;
    register CT ct;
#endif
    int	    i;
    register char *bp;
    char   buffer[BUFSIZ];
    register CI	   ci;

    bp = buffer;

    if (userrs && invo_name && *invo_name) {
	(void) sprintf (bp, "%s: ", invo_name);
	bp += strlen (bp);
    }

#ifdef VSPRINTF
#ifdef __STDC__
    va_start (arglist, fmt);
#else
    va_start (arglist);
    what = va_arg(arglist, char *);
    ct   = va_arg(arglist, CT);
    fmt  = va_arg(arglist, char *);
#endif
    (void) vsprintf (bp, fmt, arglist);
    bp += strlen (bp);
#else
    (void) sprintf (bp, fmt, a, b, c, d, e, f);
    bp += strlen (bp);
#endif
    ci = &ct -> c_ctinfo;

    if (what) {
	if (*what) {
	    (void) sprintf (bp, " %s: ", what);
	    bp += strlen (bp);
	}

	if (errno > 0 && errno < sys_nerr)
	    (void) sprintf (bp, "%s", sys_errlist[errno]);
	else
	    (void) sprintf (bp, "Error %d", errno);
	bp += strlen (bp);
    }

    i = strlen (invo_name) + 2;
    (void) sprintf (bp, "\n%*.*s(content %s/%s", i, i, "", ci -> ci_type,
		    ci -> ci_subtype);
    bp += strlen (bp);
    if (ct -> c_file) {
	(void) sprintf (bp, " in message %s", ct -> c_file);
	bp += strlen (bp);
	if (ct -> c_partno) {
	    (void) sprintf (bp, ", part %s", ct -> c_partno);
	    bp += strlen (bp);
	}
    }
    (void) sprintf (bp, ")");
    bp += strlen (bp);

    if (userrs) {
	*bp++ = '\n';
	*bp = '\0';

	errs = add (buffer, errs);
    }
    else
	advise (NULLCP, "%s", buffer);
}


static void  flush_errors ()
{
    if (errs) {
	(void) fflush (stdout);
	fprintf (stderr, "%s", errs);
	free (errs);
	errs = NULL;
    }
}

/*  */

static	jmp_buf	intrenv;


/* ARGSUSED */

static TYPESIG  intrser (i)
int	i;
{
#ifdef	BSD42
    (void) signal (SIGINT, intrser);
#endif

    (void) putchar ('\n');

    longjmp (intrenv, DONE);
}

/*  */

static int	show_content_aux (), show_content_aux2 ();


static int  show_content (ct, serial, alternate)
register CT	ct;
int	serial,
	alternate;
{
    register char  *cp;
    char    buffer[BUFSIZ];
    register CI ci = &ct -> c_ctinfo;

    (void) sprintf (buffer, "%s-show-%s/%s", invo_name, ci -> ci_type,
		    ci -> ci_subtype);
    if ((cp = m_find (buffer)) == NULL || *cp == 0) {
	(void) sprintf (buffer, "%s-show-%s", invo_name, ci -> ci_type);
	if (((cp = m_find (buffer)) == NULL || *cp == 0)
	        && (cp = ct -> c_showproc) == NULL) {
	    if (!alternate)
		content_error (NULLCP, ct,
			       "don't know how to display content");

	    return NOTOK;
	}
    }

    return show_content_aux (ct, serial, alternate, cp, NULLCP);
}


static int  show_content_aux (ct, serial, alternate, cp, cracked)
register CT	ct;
int	serial,
	alternate;
register char   *cp;
char   *cracked;
{
    int	    fd,
	    xlist,
	    xpause,
	    xstdin,
	    xtty;
    register char  *bp;
    char   *file,
	    buffer[BUFSIZ];
    register CI ci = &ct -> c_ctinfo;

    if (!ct -> c_ceopenfnx) {
	if (!alternate)
	    content_error (NULLCP, ct, "don't know how to decode content");

	return NOTOK;
    }

    file = NULL;
    if ((fd = (*ct -> c_ceopenfnx) (ct, &file)) == NOTOK)
	return NOTOK;
    if (ct -> c_showproc && strcmp (ct -> c_showproc, "true") == 0)
	return (alternate ? DONE : OK);
    
    xlist = xpause = xstdin = xtty = 0;
    if (cracked) {
	(void) strcpy (buffer, cp);
	goto got_command;
    }
    buffer[0] = '\0';
    for (bp = buffer; *cp; cp++)
	if (*cp == '%') {
	    switch (*++cp) {
		case 'a':	/* additional arguments */
		    {
			register char **ap,
				      **ep;
			char   *s = "";

			for (ap = ci -> ci_attrs, ep = ci -> ci_values;
			         *ap;
			         ap++, ep++) {
			    (void) sprintf (bp, "%s%s=\"%s\"", s, *ap, *ep);
			    bp += strlen (bp);
			    s = " ";
			}
		    }
		    break;

		case 'd':	/* content description */
		    if (ct -> c_descr) {
			char   *s;

			(void) strcpy (bp, s = trimcpy (ct -> c_descr));
			free (s);
		    }
		    break;

		case 'e':	/* exclusive execution */
		    xtty = 1;
		    break;

		case 'F':	/* %e, %f, and stdin is terminal not content */
		    xstdin = xtty = 1;
		    /* and fall... */
	        case 'f':	/* filename */
		    (void) sprintf (bp, "%s", file);
		    break;

		case 'p':	/* pause prior to displaying content */
		    xpause = pausesw;
		    /* and fall... */
		case 'l':	/* display listing prior to displaying
				   content */
		    xlist = !nolist;
		    break;

		case 's':	/* subtype */
		    (void) strcpy (bp, ci -> ci_subtype);
		    break;

		case '%':
		    goto raw;

		default:
		    *bp++ = *--cp;
		    *bp = '\0';
		    continue;
	    }
	    bp += strlen (bp);
	}
        else {
raw: ;
	    *bp++ = *cp;
	    *bp = '\0';
	}
    if (ct -> c_termproc) {
	char	term[BUFSIZ];

	(void) strcpy (term, buffer);
	(void) sprintf (buffer, ct -> c_termproc, term);
    }

got_command: ;
    return show_content_aux2 (ct, serial, alternate, cracked, buffer,
			      fd, xlist, xpause, xstdin, xtty);
}


static int  show_content_aux2 (ct, serial, alternate, cracked, buffer,
			      fd, xlist, xpause, xstdin, xtty)
register CT	ct;
int	serial,
	alternate;
char   *cracked,
       *buffer;
int	fd,
	xlist,
    	xpause,
    	xstdin,
    	xtty;
{
    int	    child_id,
	    i;
    char   *vec[4],
	    exec[BUFSIZ + sizeof "exec "];
    register CI ci = &ct -> c_ctinfo;
    
    if (debugsw || cracked) {
	(void) fflush (stdout);

	fprintf (stderr, "%s msg %s", cracked ? "storing" : "show",
		 ct -> c_file);
	if (ct -> c_partno)
	    fprintf (stderr, " part %s", ct -> c_partno);
	if (cracked)
	    fprintf (stderr, " using command (cd %s; %s)\n", cracked, buffer);
	else
	    fprintf (stderr, " using command %s\n", buffer);
    }

    if (xpid < 0 || (xtty && xpid)) {
	if (xpid < 0)
	    xpid = -xpid;
	(void) pidcheck (pidwait (xpid, NOTOK));
	xpid = 0;
    }

    if (xlist) {
	char    prompt[BUFSIZ];

	if (ct -> c_ctlistfnx) {
	    if (ct -> c_type == CT_MULTIPART)
		(void) list_content (ct, -1);
	    else
		(*ct -> c_ctlistfnx) (ct, -1);

	    if (xpause && SOprintf ("Press <return> to show content..."))
		printf ("Press <return> to show content...");
	}
	else {
	    register char *pp;

	    pp = prompt;
	    if (ct -> c_descr) {
		(void) sprintf (pp, "%s (", ct -> c_descr);
		pp += strlen (pp);
	    }

	    (void) sprintf (pp, "content %s/%s", ci -> ci_type,
			    ci -> ci_subtype);
	    pp += strlen (pp);
	    if (ct -> c_file) {
		(void) sprintf (pp, " in message %s", ct -> c_file);
		pp += strlen (pp);
		if (ct -> c_partno) {
		    (void) sprintf (pp, ", part %s", ct -> c_partno);
		    pp += strlen (pp);
		}
	    }

	    if (ct -> c_descr) {
		(void) sprintf (pp, ")");
		pp += strlen (pp);
	    }

	    if (!xpause)
		printf ("%s\n", prompt);
	    else
		if (SOprintf ("Press <return> to show %s...", prompt))
		    printf ("Press <return> to show %s...", prompt);
	}

	if (xpause) {
	    int	    intr;
	    TYPESIG (*istat) ();

	    istat = signal (SIGINT, intrser);
	    if ((intr = setjmp (intrenv)) == OK) {
		(void) fflush (stdout);
		prompt[0] = 0;
		(void) read (fileno (stdout), prompt, sizeof prompt);
	    }
	    (void) signal (SIGINT, istat);
	    if (intr != OK) {
		(void) (*ct -> c_ceclosefnx) (ct);
		return (alternate ? DONE : NOTOK);
	    }
	}
    }

    (void) sprintf (exec, "exec %s", buffer);

    vec[0] = "/bin/sh";
    vec[1] = "-c";
    vec[2] = exec;
    vec[3] = NULL;

    (void) fflush (stdout);

    for (i = 0; (child_id = vfork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK:
	    advise ("fork", "unable to");
	    (void) (*ct -> c_ceclosefnx) (ct);
	    return NOTOK;

	case OK:
	    if (cracked)
		(void) chdir (cracked);
	    if (!xstdin)
		(void) dup2 (fd, 0);
	    (void) close (fd);
	    (void) execvp ("/bin/sh", vec);
	    fprintf (stderr, "unable to exec ");
	    perror ("/bin/sh");
	    _exit (-1);
	    /* NOTREACHED */

	default:
	    if (!serial) {
		ct -> c_pid = child_id;
		if (xtty)
		    xpid = child_id;
	    }
	    else
		(void) pidcheck (pidXwait (child_id, NULLCP));

	    if (fd != NOTOK)
		(void) (*ct -> c_ceclosefnx) (ct);
	    return (alternate ? DONE : OK);
    }
}

/*  */

static int  store_content (ct, append)
register CT	ct;
char   *append;
{
    int	    appending = append && *append;
    long    last,
	    pos;
    register char  *bp,
		   *cp;
    char   *file,
	    buffer[BUFSIZ];
    register CI ci = &ct -> c_ctinfo;
    FILE       *fp;

    if (appending) {
	(void) strcpy (buffer, append);
	goto got_filename;
    }

    if ((cp = ct -> c_storeproc) == NULL || *cp == 0) {
	(void) sprintf (buffer, "%s-store-%s/%s", invo_name, ci -> ci_type,
			ci -> ci_subtype);
	if ((cp = m_find (buffer)) == NULL || *cp == 0) {
	    (void) sprintf (buffer, "%s-store-%s", invo_name, ci -> ci_type);
	    if ((cp = m_find (buffer)) == NULL || *cp == 0)
		cp = ct -> c_type == CT_MESSAGE ? "+" : "%m%P.%s";
	}
    }

    switch (*cp) {
	case '+':
	case '@':
	    {
		char   *folder = cp[1] ? path (cp + 1, *cp == '+' ? TFOLDER 
							          : TSUBCWF)
				       : m_getfolder ();
		struct msgs *mp = NULL;
		struct stat st;

		if (stat (bp = m_mailpath (folder), &st) == NOTOK) {
		    int	    answer;
		    char   *ep;

		    if (errno != ENOENT) {
			advise (bp, "error on folder");
			goto losing_folder;
		    }

		    ep = concat ("Create folder \"", bp, "\"? ", NULLCP);
		    answer = getanswer (ep);
		    free (ep);

		    if (!answer)
			goto losing_folder;
		    if (!makedir (bp)) {
			advise (NULLCP, "unable to create folder %s", bp);
			goto losing_folder;
		    }
		}

		if (mp = m_gmsg (folder))
		    (void) sprintf (buffer, "%s/%d", mp -> foldpath,
				   mp -> hghmsg + 1);
		else
		    advise (NULLCP, "unable to read folder %s", folder);
losing_folder: ;
		if (cp[1])
		    free (folder);
		if (mp)
		    m_fmsg (mp);
		else
		    return NOTOK;
	    }
	    goto got_filename;

	case '/':
	case '|':
	case '!':
	    bp = buffer;
	    buffer[0] = '\0';
	    break;

	default:
	    bp = autosw ? cwd : dir;
	    (void) sprintf (buffer, "%s/", bp[1] ? bp : "");
	    bp = buffer + strlen (buffer);
	    break;
    }
    for (; *cp; cp++)
	if (*cp == '%') {
	    switch (*++cp) {
		case 'a':	/* additional arguments */
		    if (buffer[0] != '|' && buffer[0] != '!') {
			*bp++ = *--cp;
			*bp = '\0';
			continue;
		    }
		    else {
			register char **ap,
				      **ep;
			char   *s = "";

			for (ap = ci -> ci_attrs, ep = ci -> ci_values;
			         *ap;
			         ap++, ep++) {
			    (void) sprintf (bp, "%s%s=\"%s\"", s, *ap, *ep);
			    bp += strlen (bp);
			    s = " ";
			}
		    }
		    break;

		case 'm':	/* message */
		    (void) sprintf (bp, "%s", r1bindex (ct -> c_file, '/'));
		    break;

		case 'P':	/* .part */
		    if (ct -> c_partno)
			(void) sprintf (bp, ".%s", ct -> c_partno);
		    break;

		case 'p':	/* part */
		    if (ct -> c_partno)
			(void) strcpy (bp, ct -> c_partno);
		    break;

		case 't':	/* type */
		    (void) strcpy (bp, ci -> ci_type);
		    break;

		case 's':	/* subtype */
		    (void) strcpy (bp, ci -> ci_subtype);
		    break;

		case '%':
		    goto raw;

		default:
		    *bp++ = *--cp;
		    *bp = '\0';
		    continue;
	    }
	    bp += strlen (bp);
	}
	else {
raw: ;
	    *bp++ = *cp;
	    *bp = '\0';
	}
    if (buffer[0] == '|' || buffer[0] == '!')
	return show_content_aux (ct, 1, 0, buffer + 1, autosw ? cwd : dir);
got_filename: ;

    ct -> c_storage = add (buffer, NULLCP);
    (void) fflush (stdout);
    fprintf (stderr, "storing message %s", ct -> c_file);
    if (ct -> c_partno)
	fprintf (stderr, " part %s", ct -> c_partno);
    fprintf (stderr, " as file %s\n",
	     strncmp (ct -> c_storage, cwd, cwdlen)
		    || ct -> c_storage[cwdlen] != '/'
	         ? ct -> c_storage : ct -> c_storage + cwdlen + 1);
    if (index (ct -> c_storage, '/')
	    && make_intermediates (ct -> c_storage) == NOTOK)
	return NOTOK;

    if (ct -> c_encoding != CE_7BIT) {
	int	cc,
		fd;

	if (!ct -> c_ceopenfnx) {
	    advise (NULLCP, "don't know how to decode part %s of message %s",
		    ct -> c_partno, ct -> c_file);
	    return NOTOK;
	}

	file = appending || !strcmp (ct -> c_storage, "-") ? NULLCP
							   : ct -> c_storage;
	if ((fd = (*ct -> c_ceopenfnx) (ct, &file)) == NOTOK)
	    return NOTOK;
	if (strcmp (file, ct -> c_storage) == 0) {
	    (void) (*ct -> c_ceclosefnx) (ct);
	    return OK;
	}

	if (!strcmp (ct -> c_storage, "-")) {
	    int	    gd;

	    if ((gd = dup (fileno (stdout))) == NOTOK) {
		advise ("stdout", "unable to dup");
losing: ;
		(void) (*ct -> c_ceclosefnx) (ct);
		return NOTOK;
	    }
	    if ((fp = fdopen (gd, appending ? "a" : "w")) == NULL) {
		advise ("stdout", "unable to fdopen (%d, \"%s\") from", gd,
			appending ? "a" : "w");
		(void) close (gd);
		goto losing;
	    }
	}
	else
	    if ((fp = fopen (ct -> c_storage, appending ? "a" : "w"))
		    == NULL) {
		advise (ct -> c_storage, "unable to fopen for %s",
			appending ? "appending" : "writing");
		goto losing;
	    }

	if (append && !*append)
	    (void) copy_some_headers (fp, ct);

	for (;;) {
	    switch (cc = read (fd, buffer, sizeof buffer)) {
		case NOTOK:
		    advise (file, "error reading content from");
		    break;

		case OK:
		    break;

		default:
		    (void) fwrite (buffer, sizeof *buffer, cc, fp);
		    continue;
	    }
	    break;
	}

	(void) (*ct -> c_ceclosefnx) (ct);

	if (cc != NOTOK && fflush (fp))
	    advise (ct -> c_storage, "error writing to");

	(void) fclose (fp);

	return (cc != NOTOK ? OK : NOTOK);
    }

    if (!ct -> c_fp && (ct -> c_fp = fopen (ct -> c_file, "r")) == NULL) {
	advise (ct -> c_file, "unable to open for reading");
	return NOTOK;
    }

    (void) fseek (ct -> c_fp, pos = ct -> c_begin, 0);
    last = ct -> c_end;

    if (!strcmp (ct -> c_storage, "-")) {
	int	gd;

	if ((gd = dup (fileno (stdout))) == NOTOK) {
	    advise ("stdout", "unable to dup");
	    return NOTOK;
	}
	if ((fp = fdopen (gd, appending ? "a" : "w")) == NULL) {
	    advise ("stdout", "unable to fdopen (%d, \"%s\") from", gd,
		    appending ? "a" : "w");
	    (void) close (gd);
	    return NOTOK;
	}
    }
    else
	if ((fp = fopen (ct -> c_storage, appending ? "a" : "w")) == NULL) {
	    advise (ct -> c_storage, "unable to fopen for %s",
		    appending ? "appending" : "writing");
	    return NOTOK;
	}

    if (append && !*append) {
	(void) copy_some_headers (fp, ct);
	appending = 1;
    }
    else
	appending = 0;

    while (fgets (buffer, sizeof buffer - 1, ct -> c_fp)) {
	if ((pos += strlen (buffer)) > last) {
	    int	    diff = strlen (buffer) - (pos - last);

	    if (diff >= 0)
		buffer[diff] = '\0';
	}

	if (appending)
	    switch (buffer[0]) {
		case ' ':
		case '\t':
		    if (appending < 0)
			buffer[0] = 0;
		    break;

		case '\n':
		    appending = 0;
		    break;

		default:
		    if (!uprf (buffer, XXX_FIELD_PRF)
			    && !uprf (buffer, "Encrypted:")
			    && !uprf (buffer, "Message-ID:")) {
			appending = -1;
			buffer[0] = 0;
			break;
		    }
		    appending = 1;
		    break;
	    }

	(void) fputs (buffer, fp);
	if (pos >= last)
	    break;
    }

    if (fflush (fp))
	advise (ct -> c_storage, "error writing to");

    (void) fclose (fp);

    (void) fclose (ct -> c_fp), ct -> c_fp = NULL;

    return OK;
}


static int  copy_some_headers (out, ct)
FILE   *out;
register CT	ct;
{
    int	    state;
    char    buf[BUFSIZ],
	    name[NAMESZ];
    FILE   *in;

    if ((in = fopen (ct -> c_file, "r")) == NULL) {
	advise (ct -> c_file, "unable to open for reading");
	return NOTOK;
    }

    for (state = FLD;;) {
	switch (state = m_getfld (state, name, buf, sizeof buf, in)) {
	    case FLD:
	    case FLDPLUS:
	    case FLDEOF:
		if (uprf (name, XXX_FIELD_PRF)
		        || uleq (name, "Encrypted")
		        || uleq (name, "Message-ID")) {
		    while (state == FLDPLUS)
			state = m_getfld (state, name, buf, sizeof buf, in);
		    continue;
		}

		fprintf (out, "%s:%s", name, buf);
		while (state == FLDPLUS) {
		    state = m_getfld (state, name, buf, sizeof buf, in);
		    (void) fputs (buf, out);
		}
		if (state != FLDEOF)
		    continue;
		/* else fall... */
	   case BODY:
	   case BODYEOF:
	   case FILEEOF:
		break;

	   case LENERR:
	   case FMTERR:
	   default:
		break;
	}

	break;
    }

    (void) fclose (in);

    return OK;
}


static int make_intermediates (file)
char   *file;
{
    register char *cp;

    for (cp = file + 1; cp = index (cp, '/'); cp++) {
	struct stat st;

	*cp = NULL;

	if (stat (file, &st) == NOTOK) {
	    int	    answer;
	    char   *ep;

	    if (errno != ENOENT) {
		advise (file, "error on directory");
losing_directory: ;
		*cp = '/';
		return NOTOK;
	    }

	    ep = concat ("Create directory \"", file, "\"? ", NULLCP);
	    answer = getanswer (ep);
	    free (ep);

	    if (!answer)
		goto losing_directory;
	    if (!makedir (file)) {
		advise (NULLCP, "unable to create directory %s", file);
		goto losing_directory;
	    }
	}

	*cp = '/';
    }

    return OK;
}

/*  */

static void  free_ctinfo (ct)
register CT	ct;
{
    register char **ap;
    register CI	ci = &ct -> c_ctinfo;

    if (ci -> ci_type)
	free (ci -> ci_type);
    if (ci -> ci_subtype)
	free (ci -> ci_subtype);
    for (ap = ci -> ci_attrs; *ap; ap++)
	free (*ap);
    if (ci -> ci_comment)
	free (ci -> ci_comment);
    if (ci -> ci_magic)
	free (ci -> ci_magic);
}


static void  free_content (ct)
register CT	ct;
{
    if (!ct)
	return;

    if (ct -> c_partno)
	free (ct -> c_partno);

    if (ct -> c_vrsn)
	free (ct -> c_vrsn);

    if (ct -> c_ctline)
	free (ct -> c_ctline);

    free_ctinfo (ct);

    if (ct -> c_ctfreefnx)
	(void) (*ct -> c_ctfreefnx) (ct);

    if (ct -> c_showproc)
	free (ct -> c_showproc);
    if (ct -> c_termproc)
	free (ct -> c_termproc);
    if (ct -> c_storeproc)
	free (ct -> c_storeproc);

    if (ct -> c_celine)
	free (ct -> c_celine);
    if (ct -> c_cefreefnx)
	(void) (*ct -> c_cefreefnx) (ct, 1);

    if (ct -> c_id)
	free (ct -> c_id);
    if (ct -> c_descr)
	free (ct -> c_descr);

    if (ct -> c_file) {
	if (ct -> c_unlink)
	    (void) unlink (ct -> c_file);
	free (ct -> c_file);
    }
    if (ct -> c_fp)
	(void) fclose (ct -> c_fp);

    if (ct -> c_storage)
	(void) free (ct -> c_storage);

    free ((char *) ct);
}

/*  */

static int  part_ok (ct, sP)
register CT	ct;
int	sP;
{
    register char **ap;

    if ((ct -> c_type == CT_MULTIPART && (sP || ct -> c_subtype))
	    || npart == 0)
	return 1;

    for (ap = parts; *ap; ap++)
	if (strcmp (*ap, ct -> c_partno) == 0)
	    return 1;

    return 0;
}



static int  type_ok (ct, sP)
register CT	ct;
int	sP;
{
    register char **ap;
    char    buffer[BUFSIZ];
    register CI	    ci = &ct -> c_ctinfo;

    if ((ct -> c_type == CT_MULTIPART && (sP || ct -> c_subtype))
	    || ntype == 0)
	return 1;

    (void) sprintf (buffer, "%s/%s", ci -> ci_type, ci -> ci_subtype);
    for (ap = types; *ap; ap++)
	if (uleq (*ap, ci -> ci_type) || uleq (*ap, buffer))
	    return 1;

    return 0;
}

/*    CONTENTS */

struct k2v {
    char   *kv_key;
    int	    kv_value;
};


static int  InitGeneric (ct)
register CT	ct;
{
    register char **ap,
		  **ep;
    register CI	ci = &ct -> c_ctinfo;

    for (ap = ci -> ci_attrs, ep = ci -> ci_values; *ap; ap++, ep++)
	if (autosw && !ct -> c_storeproc && uleq (*ap, "x-name")) {
	    register char   *cp;

	    if (*(cp = *ep) != '/'
		    && *cp != '.'
		    && *cp != '|'
		    && *cp != '!'
		    && !index (cp, '%'))
		ct -> c_storeproc = add (cp, NULLCP);
	}

    return OK;
}

/*    TEXT */

#define	TEXT_UNKNOWN	0x00
#define	TEXT_PLAIN	0x01
#define	TEXT_RICHTEXT	0x02

struct text {
    int	    tx_charset;
#define	CHARSET_UNKNOWN	0x00
#define	CHARSET_USASCII	0x01
#define	CHARSET_LATIN	0x02
};

static struct k2v	Charset[] = {
    "us-ascii",	    CHARSET_USASCII,
    "iso-8859-1",   CHARSET_LATIN,

    NULL,	    CHARSET_UNKNOWN		/* this one must be last! */
};

static int  free_text (ct)
register CT	ct;
{
    register struct text *t = (struct text *) ct -> c_ctparams;

    if (!t)
	return;

    free ((char *) t);
    ct -> c_ctparams = NULL;
}


static struct k2v	SubText[] = {
    "plain",	    TEXT_PLAIN,
    "richtext",	    TEXT_RICHTEXT,

    NULL,	    TEXT_UNKNOWN		/* this one must be last! */
};

static int  InitText (ct)
register CT	ct;
{
    char   buffer[BUFSIZ];
    register char **ap,
		  **ep;
    register struct k2v *kv;
    register CI	ci = &ct -> c_ctinfo;

    if (!*ci -> ci_subtype)	/* XXX: attmail bogosity! */
	ci -> ci_subtype = add ("plain", ci -> ci_subtype);
    for (kv = SubText; kv -> kv_key; kv++)
	if (uleq (ci -> ci_subtype, kv -> kv_key))
	    break;
    if ((ct -> c_subtype = kv -> kv_value) == TEXT_PLAIN) {
	(void) sprintf (buffer, "%%p%s '%%F'",
			progsw ? progsw
			       : moreproc && *moreproc ? moreproc : "more");
	ct -> c_showproc = add (buffer, NULLCP);
    }

    for (ap = ci -> ci_attrs, ep = ci -> ci_values; *ap; ap++, ep++)
	if (!ct -> c_ctparams && uleq (*ap, "charset")) {
	    char   *cp;
	    register struct text *t;

	    if ((t = (struct text *) calloc (1, sizeof *t)) == NULL)
		adios (NULLCP, "out of memory");
	    ct -> c_ctparams = (caddr_t) t;
	    ct -> c_ctfreefnx = free_text;

	    for (kv = Charset; kv -> kv_key; kv++)
		if (uleq (*ep, kv -> kv_key)) {
		    (void) sprintf (buffer, "%s-charset-%s", invo_name,
				    kv -> kv_key);
		    break;
		}
	    t -> tx_charset = kv -> kv_value;
	    if (!kv -> kv_key)
		(void) sprintf (buffer, "%s-charset-%s", invo_name, *ep);
	    if ((!mm_charset || !uleq (mm_charset, *ep))
		    && (cp = m_find (buffer)))
		ct -> c_termproc = getcpy (cp);
	}
        else
	    if (autosw && !ct -> c_storeproc && uleq (*ap, "x-name")) {
		register char   *cp;

		if (*(cp = *ep) != '/'
		        && *cp != '.'
		        && *cp != '|'
		        && *cp != '!'
		        && !index (cp, '%'))
		    ct -> c_storeproc = add (cp, NULLCP);
	    }

    return OK;
}

/*    MULTIPART */

#define	MULTI_UNKNOWN	0x00
#define	MULTI_MIXED	0x01
#define	MULTI_ALTERNATE	0x02
#define	MULTI_DIGEST	0x03
#define	MULTI_PARALLEL	0x04


struct multipart {
    char   *mp_start;
    char   *mp_stop;

    struct part {
	CT  mp_part;

	struct part *mp_next;
    }	   *mp_parts;
};


static int  list_multi (ct, toplevel)
register CT	ct;
int	toplevel;
{
    register struct multipart *m = (struct multipart *) ct -> c_ctparams;
    register struct part *part;

    (void) list_content (ct, toplevel);

    for (part = m -> mp_parts; part; part = part -> mp_next) {
	register CT  p = part -> mp_part;

	if (part_ok (p, 1) && type_ok (p, 1) && p -> c_ctlistfnx)
	    (void) (*p -> c_ctlistfnx) (p, 0);
    }

    return OK;
}


static int  show_multi (ct, serial, alternate)
register CT	ct;
int	serial,
	alternate;
{
    int	    alternating,
	    nowalternate,
	    nowserial,
	    result;
    register struct multipart *m = (struct multipart *) ct -> c_ctparams;
    register struct part *part;
    register CT  p;
    TYPESIG (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();

    alternating = 0;
    nowalternate = alternate;
    switch (ct -> c_subtype) {
	case MULTI_PARALLEL:
	    if (!(nowserial = serialsw)) {
set_signals: ;
		hstat = signal (SIGHUP, SIG_IGN);
		istat = signal (SIGINT, SIG_IGN);
		qstat = signal (SIGQUIT, SIG_IGN);
		tstat = signal (SIGTERM, SIG_IGN);
	    }
	    break;

	case MULTI_ALTERNATE:
	    nowalternate = alternating = 1;
	    /* and fall... */
	default:
	    if (!(nowserial = serial))
		goto set_signals;
	    break;
    }

/* alternate -> we're inside an alternative
   alternating -> we are an alternative
 */

    result = alternate ? NOTOK : OK;
    for (part = m -> mp_parts; part; part = part -> mp_next) {
	p = part -> mp_part;

	if (part_ok (p, 0) && type_ok (p, 0) && p -> c_ctshowfnx) {
	    int	    inneresult;

	    switch (inneresult = (*p -> c_ctshowfnx) (p, nowserial,
						      nowalternate)) {
		case NOTOK:
		    if (alternate && !alternating) {
			result = NOTOK;
			goto out;
		    }
		    continue;

		case OK:
		case DONE:
		    if (alternating) {
			result = DONE;
			break;
		    }
		    if (alternate) {
			alternate = nowalternate = 0;
			if (result == NOTOK)
			    result = inneresult;
		    }
		    continue;
	    }
	    break;
	}
    }
    if (alternating && !part) {
	if (!alternate)
	    content_error (NULLCP, ct,
			   "don't know how to display any of the contents");

	result = NOTOK;
	goto out;
    }

    if (serial && !nowserial) {
	int	pid,
		kids;
#if defined(BSD42) && !defined(WAITINT)
	union wait status;
#else
	int     status;
#endif

	kids = 0;
	for (part = m -> mp_parts; part; part = part -> mp_next) {
	    p = part -> mp_part;

	    if (p -> c_pid > OK)
		if (kill (p -> c_pid, 0) == NOTOK)
		    p -> c_pid = 0;
		else
		    kids++;
	}

	while (kids > 0 && (pid = wait (&status)) != NOTOK) {
#if defined(BSD42) && !defined(WAITINT)
	    (void) pidcheck (status.w_status);
#else
	    (void) pidcheck (status);
#endif

	    for (part = m -> mp_parts; part; part = part -> mp_next) {
		p = part -> mp_part;

		if (xpid == pid)
		    xpid = 0;
		if (p -> c_pid == pid) {
		    p -> c_pid = 0;
		    kids--;
		    break;
		}
	    }
	}
    }

out: ;
    if (!nowserial) {
	(void) signal (SIGHUP, hstat);
	(void) signal (SIGINT, istat);
	(void) signal (SIGQUIT, qstat);
	(void) signal (SIGTERM, tstat);
    }

    return result;
}


static int  show_unknown_multi (ct, serial, alternate)
register CT	ct;
int	serial,
	alternate;
{
    int	    xlist,
	    xpause,
	    xtty;
    register char  *bp,
		   *cp;
    char    buffer[BUFSIZ];
    register struct multipart *m = (struct multipart *) ct -> c_ctparams;
    register struct part *part;
    register CI ci = &ct -> c_ctinfo;
    register CT  p;

    (void) sprintf (buffer, "%s-show-%s/%s", invo_name, ci -> ci_type,
		    ci -> ci_subtype);
    if ((cp = m_find (buffer)) == NULL || *cp == 0) {
	(void) sprintf (buffer, "%s-show-%s", invo_name, ci -> ci_type);
	if (((cp = m_find (buffer)) == NULL || *cp == 0)
	        && (cp = ct -> c_showproc) == NULL) {
	    if (!alternate)
		content_error (NULLCP, ct,
			       "don't know how to display content");

	    return NOTOK;
	}
    }

    for (part = m -> mp_parts; part; part = part -> mp_next) {
	p = part -> mp_part;

	if (!p -> c_ceopenfnx) {
	    if (!alternate)
		content_error (NULLCP, p, "don't know how to decode content");

	    return NOTOK;
	}

	if (p -> c_storage == NULL) {
	    if ((*p -> c_ceopenfnx) (p, &p -> c_storage) == NOTOK)
		return NOTOK;

	    if (p -> c_showproc && strcmp (p -> c_showproc, "true") == 0)
		return (alternate ? DONE : OK);
	    (*p -> c_ceclosefnx) (p);
	}
    }

    xlist = xpause = xtty = 0;
    buffer[0] = '\0';
    for (bp = buffer; *cp; cp++)
	if (*cp == '%') {
	    switch (*++cp) {
		case 'a':	/* additional arguments */
		    {
			register char **ap,
				      **ep;
			char   *s = "";

			for (ap = ci -> ci_attrs, ep = ci -> ci_values;
			         *ap;
			         ap++, ep++) {
			    (void) sprintf (bp, "%s%s=\"%s\"", s, *ap, *ep);
			    bp += strlen (bp);
			    s = " ";
			}
		    }
		    break;

		case 'd':	/* content description */
		    if (ct -> c_descr) {
			char   *s;

			(void) strcpy (bp, s = trimcpy (ct -> c_descr));
			free (s);
		    }
		    break;

		case 'e':	/* exclusive execution */
		    xtty = 1;
		    break;

		case 'F':	/* %e and %f */
		    xtty = 1;
		    /* and fall... */
	        case 'f':	/* filename(s) */
		    {
			char   *s = "";
			
			for (part = m -> mp_parts;
			         part;
			         part = part -> mp_next) {
			    p = part -> mp_part;

			    (void) sprintf (bp, "%s'%s'", s, p -> c_storage);
			    bp += strlen (bp);
			    s = " ";
			}
		    }
		    break;

		case 'p':	/* pause prior to displaying content */
		    xpause = pausesw;
		    /* and fall... */
		case 'l':	/* display listing prior to displaying
				   content */
		    xlist = !nolist;
		    break;

		case 's':	/* subtype */
		    (void) strcpy (bp, ci -> ci_subtype);
		    break;

		case '%':
		    goto raw;

		default:
		    *bp++ = *--cp;
		    *bp = '\0';
		    continue;
	    }
	    bp += strlen (bp);
	}
        else {
raw: ;
	    *bp++ = *cp;
	    *bp = '\0';
	}
    if (ct -> c_termproc) {
	char	term[BUFSIZ];

	(void) strcpy (term, buffer);
	(void) sprintf (buffer, ct -> c_termproc, term);
    }

    return show_content_aux2 (ct, serial, alternate, NULLCP, buffer,
			      NOTOK, xlist, xpause, 0, xtty);
}


/* ARGSUSED */

static int  store_multi (ct, unused)
register CT	ct;
char   *unused;
{
    int	    result;
    register struct multipart *m = (struct multipart *) ct -> c_ctparams;
    register struct part *part;

    result = NOTOK;
    for (part = m -> mp_parts; part; part = part -> mp_next) {
	register CT  p = part -> mp_part;

	if (part_ok (p, 1)
	        && type_ok (p, 1)
	        && p -> c_ctstorefnx
	        && (result = (*p -> c_ctstorefnx) (p, NULLCP)) == OK
	        && ct -> c_subtype == MULTI_ALTERNATE)
	    break;
    }

    return result;
}


static int  free_multi (ct)
register CT	ct;
{
    register struct multipart *m = (struct multipart *) ct -> c_ctparams;
    register struct part   *part,
			   *next;

    if (!m)
	return;

    if (m -> mp_start)
	free (m -> mp_start);
    if (m -> mp_stop)
	free (m -> mp_stop);
	
    for (part = m -> mp_parts; part; part = next) {
	next = part -> mp_next;

	free_content (part -> mp_part);

	free ((char *) part);
    }
    m -> mp_parts = NULL;

    free ((char *) m);
    ct -> c_ctparams = NULL;
}


static struct k2v SubMultiPart[] = {
    "mixed",	    MULTI_MIXED,
    "alternative",  MULTI_ALTERNATE,
    "digest",	    MULTI_DIGEST,
    "parallel",	    MULTI_PARALLEL,

    NULL,	    MULTI_UNKNOWN		/* this one must be last! */
};

static int  InitMultiPart (ct)
register CT	ct;
{
    int	    inout;
    long    last,
	    pos;
    register char   *cp,
		    *dp,
		   **ap,
		   **ep;
    char   *bp,
	    buffer[BUFSIZ];
    register struct multipart *m;
    register struct k2v *kv;
    register struct part  *part,
			 **next;
    register CI	    ci = &ct -> c_ctinfo;
    register CT	    p;
    FILE   *fp;

    ct -> c_ctshowfnx = NULL;
    ct -> c_ctstorefnx = NULL;

    if (ct -> c_encoding != CE_7BIT) {
	admonish (NULLCP,
		  "\"%s/%s\" type in message %s should be encoded in 7bit",
		  ci -> ci_type, ci -> ci_subtype, ct -> c_file);
	return NOTOK;
    }

    for (kv = SubMultiPart; kv -> kv_key; kv++)
	if (uleq (ci -> ci_subtype, kv -> kv_key))
	    break;
    ct -> c_subtype = kv -> kv_value;

    for (ap = ci -> ci_attrs, ep = ci -> ci_values; *ap; ap++, ep++)
	if (uleq (*ap, "boundary")) {
	    bp = *ep;
	    break;
	}
    if (!*ap) {
	advise (NULLCP,
		"a \"boundary\" parameter is mandatory for \"%s/%s\" type in message %s's %s: field",
		ci -> ci_type, ci -> ci_subtype, ct -> c_file, TYPE_FIELD);
	return NOTOK;
    }
    
    if ((m = (struct multipart *) calloc (1, sizeof *m)) == NULL)
	adios (NULLCP, "out of memory");
    ct -> c_ctparams = (caddr_t) m;
    ct -> c_ctlistfnx = list_multi;
    ct -> c_ctshowfnx = ct -> c_subtype != MULTI_UNKNOWN ? show_multi
							 : show_unknown_multi;
    ct -> c_ctstorefnx = store_multi;
    ct -> c_ctfreefnx = free_multi;
    
    for (cp = bp; isspace (*cp); cp++)
	continue;
    if (!*cp) {
	advise (NULLCP, "invalid \"boundary\" parameter for \"%s/%s\" type in message %s's %s: field",
		ci -> ci_type, ci -> ci_subtype, ct -> c_file, TYPE_FIELD);
	return NOTOK;
    }
    for (cp = bp, dp = cp + strlen (cp) - 1; dp > cp; dp--)
	if (!isspace (*dp))
	    break;
    *++dp = '\0';
    m -> mp_start = concat (bp, "\n", NULLCP);
    m -> mp_stop = concat (bp, "--\n", NULLCP);

    if (!ct -> c_fp && (ct -> c_fp = fopen (ct -> c_file, "r")) == NULL) {
	advise (ct -> c_file, "unable to open for reading");
	return NOTOK;
    }

    (void) fseek (fp = ct -> c_fp, pos = ct -> c_begin, 0);
    last = ct -> c_end;

    next = &m -> mp_parts, part = NULL, inout = 1;
    while (fgets (buffer, sizeof buffer - 1, fp)) {
	if (pos > last)
	    break;

	pos += strlen (buffer);
	
	if (buffer[0] != '-' || buffer[1] != '-')
	    continue;

	if (inout) {
	    if (strcmp (buffer + 2, m -> mp_start))
		continue;

next_part: ;
	    if ((part = (struct part *) calloc (1, sizeof *part)) == NULL)
		adios (NULLCP, "out of memory");
	    *next = part, next = &part -> mp_next;

	    if ((p = get_content (fp, ct -> c_file,
				  rfc934sw && ct -> c_subtype == MULTI_DIGEST
						    ? -1 : 0)) == NULLCT) {
		(void) fclose (ct -> c_fp), ct -> c_fp = NULL;
		return NOTOK;
	    }
	    p -> c_fp = NULL;

	    part -> mp_part = p;
	    (void) fseek (fp, pos = p -> c_begin, 0);
	    inout = 0;
	}
	else
	    if (strcmp (buffer + 2, m -> mp_start) == 0) {
		inout = 1;

end_part: ;
		p = part -> mp_part;
		p -> c_end = ftell (fp) - (strlen (buffer) + 1);
		if (p -> c_end < p -> c_begin)
		    p -> c_begin = p -> c_end;
		if (inout)
		    goto next_part;
		goto last_part;
	    }
	    else
		if (strcmp (buffer + 2, m -> mp_stop) == 0)
		    goto end_part;
    }
    advise (NULLCP, "bogus multipart content in message %s", ct -> c_file);
    if (!inout && part) {
	p = part -> mp_part;
	p -> c_end = ct -> c_end;

	if (p -> c_begin >= p -> c_end) {
	    for (next = &m -> mp_parts;
		     *next != part;
		     next = &((*next) -> mp_next))
		continue;
	    *next = NULL;
	    free_content (p);
	    free ((char *) part);
	}
    }

last_part: ;
    if (ct -> c_subtype == MULTI_ALTERNATE && m -> mp_parts -> mp_next) {
	register int	i;
	register struct part **base,
			     **bmp;

	i = 0;
	for (part = m -> mp_parts; part; part = part -> mp_next)
	    i++;
	if ((base = (struct part **) calloc ((unsigned) (i + 1), sizeof *base))
	        == NULL)
	    adios (NULLCP, "out of memory");
	bmp = base;
	for (part = m -> mp_parts; part; part = part -> mp_next)
	    *bmp++ = part;
	*bmp = NULL;

	next = &m -> mp_parts;
	for (bmp--; bmp >= base; bmp--) {
	    part = *bmp;
	    *next = part, next = &part -> mp_next;
	}
	*next = NULL;

	free ((char *) base);
    }

    {
	int	partnum;
	register char *pp;
	char    partnam[BUFSIZ];

	if (ct -> c_partno) {
	    (void) sprintf (partnam, "%s.", ct -> c_partno);
	    pp = partnam + strlen (partnam);
	}
	else
	    pp = partnam;

	for (part = m -> mp_parts, partnum = 1;
	         part;
	         part = part -> mp_next, partnum++) {
	    p = part -> mp_part;

	    (void) sprintf (pp, "%d", partnum);
	    p -> c_partno = add (partnam, NULLCP);

	    if (p -> c_ctinitfnx && (*p -> c_ctinitfnx) (p) == NOTOK) {
		(void) fclose (ct -> c_fp), ct -> c_fp = NULL;
		return NOTOK;
	    }
	}
    }

    (void) fclose (ct -> c_fp), ct -> c_fp = NULL;

    return OK;
}

/*    MESSAGE */

#define	MESSAGE_UNKNOWN	 0x00
#define	MESSAGE_RFC822	 0x01
#define	MESSAGE_PARTIAL	 0x02
#define	MESSAGE_EXTERNAL 0x03


struct partial {
    char   *pm_partid;

    int	    pm_partno;
    int	    pm_maxno;

    int	    pm_marked;
    int	    pm_stored;
};


static int  list_partial (ct, toplevel)
register CT	ct;
int	toplevel;
{
    register struct partial *p = (struct partial *) ct -> c_ctparams;

    (void) list_content (ct, toplevel);
    if (verbosw) {
	printf ("\t     [message %s, part %d", p -> pm_partid, p -> pm_partno);
	if (p -> pm_maxno)
	    printf (" of %d", p -> pm_maxno);
	printf ("]\n");
    }

    return OK;
}


static int  ct_compar (a, b)
CT   *a,
     *b;
{
    register struct  partial *am = (struct partial *) ((*a) -> c_ctparams);
    register struct  partial *bm = (struct partial *) ((*b) -> c_ctparams);

    return (am -> pm_marked - bm -> pm_marked);
}


/* ARGSUSED */

static int  store_partial (ct, unused)
register CT	ct;
char   *unused;
{
    int	    cur,
	    hi,
	    i;
    register CT	    p,
		   *ctp,
		   *ctq;
    CT	   *base;
    struct partial *qm = (struct partial *) ct -> c_ctparams;

    if (qm -> pm_stored)
	return OK;

    hi = i = 0;
    for (ctp = cts; p = *ctp; ctp++)
	if (p -> c_type == CT_MESSAGE && p -> c_subtype == ct -> c_subtype) {
	    register struct partial *pm = (struct partial *) p -> c_ctparams;

	    if (!pm -> pm_stored
	            && strcmp (qm -> pm_partid, pm -> pm_partid) == 0) {
		pm -> pm_marked = pm -> pm_partno;
		if (pm -> pm_maxno)
		    hi = pm -> pm_maxno;
		pm -> pm_stored = 1;
		i++;
	    }
	    else
		pm -> pm_marked = 0;
	}
    if (hi == 0) {
	advise (NULLCP, "missing (at least) last part of multipart message");
	return NOTOK;
    }

    if ((base = (CT *) calloc ((unsigned) (i + 1), sizeof *base)) == NULL)
	adios (NULLCP, "out of memory");

    ctq = base;
    for (ctp = cts; p = *ctp; ctp++)
	if (p -> c_type == CT_MESSAGE && p -> c_subtype == ct -> c_subtype) {
	    register struct partial *pm = (struct partial *) p -> c_ctparams;

	    if (pm -> pm_marked)
		*ctq++ = p;
	}
    *ctq = NULL;

    if (i > 1)
	qsort ((char *) base, i, sizeof *base, ct_compar);

    cur = 1;
    for (ctq = base; p = *ctq; ctq++) {
	register struct partial *pm = (struct partial *) p -> c_ctparams;

	if (pm -> pm_marked != cur) {
	    if (pm -> pm_marked == cur - 1) {
		admonish (NULLCP,
			  "duplicate part %d of %d part multipart message",
			  pm -> pm_marked, hi);
		continue;
	    }

missing_part: ;
	    advise (NULLCP,
		    "missing %spart %d of %d part multipart message",
		    cur != hi ? "(at least) " : "", cur, hi);
	    goto losing;
	}
        else
	    cur++;
    }
    if (hi != --cur) {
	cur = hi;
	goto missing_part;
    }

    ctq = base;
    ct = *ctq++;
    if (store_content (ct, "") == NOTOK) {
losing: ;
	free ((char *) base);
	return NOTOK;
    }

    for (; p = *ctq; ctq++)
	if (store_content (p, ct -> c_storage) == NOTOK)
	    goto losing;

    free ((char *) base);
    return OK;
}


static int  free_partial (ct)
register CT	ct;
{
    register struct partial *p = (struct partial *) ct -> c_ctparams;

    if (!p)
	return;

    if (p -> pm_partid)
	free (p -> pm_partid);

    free ((char *) p);
    ct -> c_ctparams = NULL;
}


struct exbody {
    CT	    eb_parent;
    CT	    eb_content;
    char   *eb_partno;

    char   *eb_access;
    int	    eb_flags;

    char   *eb_name;
    char   *eb_permission;

    char   *eb_site;
    char   *eb_dir;
    char   *eb_mode;
    unsigned long
	    eb_size;

    char   *eb_server;
    char   *eb_subject;
    char   *eb_body;
};


static	int	openFile ();
static	int	openFTP ();
static	int	openMail ();

/* NOTE WELL: si_key MUST NOT have value of NOTOK */

static struct str2init str2methods[] = {
    "afs",	   1,	openFile,
    "anon-ftp",	   1,	openFTP,
    "ftp",	   0,	openFTP,
    "local-file",  0,	openFile,
    "mail-server", 0,	openMail,

    NULL
};


static int  params_external (ct, composing)
register CT	ct;
int	composing;
{
    register char  **ap,
		   **ep;
    register struct exbody *e = (struct exbody *) ct -> c_ctparams;
    register CI	    ci = &ct -> c_ctinfo;

    for (ap = ci -> ci_attrs, ep = ci -> ci_values; *ap; ap++, ep++) {
	if (uleq (*ap, "access-type")) {
	    register struct str2init *s2i;
	    register	CT	p = e -> eb_content;

	    for (s2i = str2methods; s2i -> si_key; s2i++)
		if (uleq (*ep, s2i -> si_key))
		    break;
	    if (!s2i -> si_key) {
		e -> eb_access = *ep;
		e -> eb_flags = NOTOK;
		p -> c_encoding = CE_EXTERNAL;
		continue;
	    }
	    e -> eb_access = s2i -> si_key;
	    e -> eb_flags = s2i -> si_value;
	    p -> c_encoding = CE_EXTERNAL;
	    if (init_encoding (p, s2i -> si_init) == NOTOK)
		return NOTOK;
	    continue;
	}
	if (uleq (*ap, "name")) {
	    e -> eb_name = *ep;
	    continue;
	}
	if (uleq (*ap, "permission")) {
	    e -> eb_permission = *ep;
	    continue;
	}
	if (uleq (*ap, "site")) {
	    e -> eb_site = *ep;
	    continue;
	}
	if (uleq (*ap, "directory")) {
	    e -> eb_dir = *ep;
	    continue;
	}
	if (uleq (*ap, "mode")) {
	    e -> eb_mode = *ep;
	    continue;
	}
	if (uleq (*ap, "size")) {
	    (void) sscanf (*ep, "%lu", &e -> eb_size);
	    continue;
	}
	if (uleq (*ap, "server")) {
	    e -> eb_server = *ep;
	    continue;
	}
	if (uleq (*ap, "subject")) {
	    e -> eb_subject = *ep;
	    continue;
	}
	if (composing && uleq (*ap, "body")) {
	    e -> eb_body = getcpy (*ep);
	    continue;
	}
    }

    if (!e -> eb_access) {
	advise (NULLCP,
		"invalid parameters for \"%s/%s\" type in message %s's %s field",
		ci -> ci_type, ci -> ci_subtype,
		ct -> c_file, TYPE_FIELD);
	return NOTOK;
    }

    return OK;
}

static int  list_external (ct, toplevel)
register CT	ct;
int	toplevel;
{
    register struct exbody *e = (struct exbody *) ct -> c_ctparams;

    (void) list_content (ct, toplevel);
    if (verbosw) {
	if (e -> eb_name)
	    printf ("\t     retrieve %s\n", e -> eb_name);
	if (e -> eb_dir)
	    printf ("\t in directory %s\n", e -> eb_dir);
	if (e -> eb_site)
	    printf ("\t         from %s\n", e -> eb_site);
	if (e -> eb_server)
	    printf ("\t from mailbox %s\n", e -> eb_server);
	if (e -> eb_subject)
	    printf ("\t with subject %s\n", e -> eb_subject);
	printf     ("\t        using %s", e -> eb_access);
	if (e -> eb_mode)
	    printf (" (in %s mode)", e -> eb_mode);
	if (e -> eb_permission)
	    printf (" (permission %s)", e -> eb_permission);
	if (e -> eb_flags == NOTOK)
	    printf (" [service unavailable]");
	printf ("\n");
    }
    (void) list_content (e -> eb_content, 0);

    return OK;
}


static int  show_external (ct, serial, alternate)
register CT	ct;
int	serial,
	alternate;
{
    register struct exbody *e = (struct exbody *) ct -> c_ctparams;
    register CT	    p = e -> eb_content;

    if (!type_ok (p, 0))
	return OK;

    if (p -> c_ctshowfnx)
	return (*p -> c_ctshowfnx) (p, serial, alternate);

    content_error (NULLCP, p, "don't know how to display content");
    return NOTOK;
}


static int  store_external (ct)
register CT	ct;
{
    int	    result = NOTOK;
    register struct exbody *e = (struct exbody *) ct -> c_ctparams;
    register CT	    p = e -> eb_content;

    if (!type_ok (p, 1))
	return OK;

    p -> c_partno = ct -> c_partno;
    if (p -> c_ctstorefnx)
	result = (*p -> c_ctstorefnx) (p, NULLCP);
    p -> c_partno = NULL;

    return result;
}


static int  free_external (ct)
register CT	ct;
{
    register struct exbody *e = (struct exbody *) ct -> c_ctparams;

    if (!e)
	return;

    free_content (e -> eb_content);
    if (e -> eb_body)
	free (e -> eb_body);

    free ((char *) e);
    ct -> c_ctparams = NULL;
}


static struct k2v SubMessage[] = {
    "rfc822",	     MESSAGE_RFC822,
    "partial", 	     MESSAGE_PARTIAL,
    "external-body", MESSAGE_EXTERNAL,

    NULL,	     MESSAGE_UNKNOWN		/* this one must be last! */
};

static int  InitMessage (ct)
register CT	ct;
{
    register struct k2v *kv;
    register CI	    ci = &ct -> c_ctinfo;

    if (ct -> c_encoding != CE_7BIT) {
	admonish (NULLCP,
		  "\"%s/%s\" type in message %s should be encoded in 7bit",
		  ci -> ci_type, ci -> ci_subtype, ct -> c_file);
	return NOTOK;
    }

    if (!*ci -> ci_subtype)	/* XXX: attmail bogosity! */
	ci -> ci_subtype = add ("rfc822", ci -> ci_subtype);
    for (kv = SubMessage; kv -> kv_key; kv++)
	if (uleq (ci -> ci_subtype, kv -> kv_key))
	    break;

    switch (ct -> c_subtype = kv -> kv_value) {
	case MESSAGE_RFC822:
	    ct -> c_showproc = add ("%pshow -file '%F'", NULLCP);
	    break;

	case MESSAGE_PARTIAL:
	    {
		register char **ap,
			      **ep;
		register struct partial *p;

		ct -> c_ctshowfnx = NULL;
		ct -> c_ctstorefnx = NULL;

		if ((p = (struct partial *) calloc (1, sizeof *p)) == NULL)
		    adios (NULLCP, "out of memory");
		ct -> c_ctparams = (caddr_t) p;
		ct -> c_ctfreefnx = free_partial;

		for (ap = ci -> ci_attrs, ep = ci -> ci_values;
		         *ap; 
		         ap++, ep++) {
		    if (uleq (*ap, "id")) {
			p -> pm_partid = add (*ep, NULLCP);

			continue;
		    }

		    if (uleq (*ap, "number")) {
			if (sscanf (*ep, "%d", &p -> pm_partno) != 1
			        || p -> pm_partno < 1) {
invalid_param: ;
			    advise (NULLCP,
				    "invalid %s parameter for \"%s/%s\" type in message %s's %s field",
				    *ap, ci -> ci_type, ci -> ci_subtype,
				    ct -> c_file, TYPE_FIELD);
			    return NOTOK;
			}

			continue;
		    }

		    if (uleq (*ap, "total")) {
			if (sscanf (*ep, "%d", &p -> pm_maxno) != 1
			        || p -> pm_maxno < 1)
			    goto invalid_param;

			continue;
		    }
		}

		if (!p -> pm_partid
		        || !p -> pm_partno
		        || (p -> pm_maxno && p -> pm_partno > p -> pm_maxno)) {
		    advise (NULLCP,
			    "invalid parameters for \"%s/%s\" type in message %s's %s field",
			    ci -> ci_type, ci -> ci_subtype,
			    ct -> c_file, TYPE_FIELD);
		    return NOTOK;
		}

		ct -> c_ctlistfnx = list_partial;
		ct -> c_ctstorefnx = store_partial;
	    }
	    break;

	case MESSAGE_EXTERNAL:
	    {
		int	exresult;
		register struct exbody *e;
		CT	p;
		FILE   *fp;

		ct -> c_ctshowfnx = NULL;
		ct -> c_ctstorefnx = NULL;

		if ((e = (struct exbody *) calloc (1, sizeof *e)) == NULL)
		    adios (NULLCP, "out of memory");
		ct -> c_ctparams = (caddr_t) e;
		ct -> c_ctfreefnx = free_external;

		if (!ct -> c_fp
		        && (ct -> c_fp = fopen (ct -> c_file, "r")) == NULL) {
		    advise (ct -> c_file, "unable to open for reading");
		    return NOTOK;
		}

		(void) fseek (fp = ct -> c_fp, ct -> c_begin, 0);

		if ((p = get_content (fp, ct -> c_file, 0)) == NULLCT) {
		    (void) fclose (ct -> c_fp), ct -> c_fp = NULL;
		    return NOTOK;
		}

		e -> eb_parent = ct;
		e -> eb_content = p;
		p -> c_ctextern = (caddr_t) e;
		if ((exresult = params_external (ct, 0)) != NOTOK
		        && p -> c_ceopenfnx == openMail) {
		    int	    cc,
			    size;
		    char   *bp;
		    
		    if ((size = ct -> c_end - p -> c_begin) <= 0) {
			if (!e -> eb_subject)
			    content_error (NULLCP, ct,
					   "empty body for access-type=mail-server");
			goto no_body;
		    }
		    
		    if ((e -> eb_body = bp = malloc ((unsigned) size)) == NULL)
			adios (NULLCP, "out of memory");
		    (void) fseek (p -> c_fp, p -> c_begin, 0);
		    while (size > 0)
			switch (cc = fread (bp, sizeof *bp, size, p -> c_fp)) {
			    case NOTOK:
			        adios ("failed", "fread");

			    case OK:
				adios (NULLCP, "unexpected EOF from fread");

			    default:
				bp += cc, size -= cc;
				break;
			}
		    *bp = 0;
no_body: ;
		}
		p -> c_fp = NULL;
		p -> c_end = p -> c_begin;

		(void) fclose (ct -> c_fp), ct -> c_fp = NULL;

		ct -> c_ctlistfnx = list_external;

		if (exresult == NOTOK)
		    return NOTOK;
		if (e -> eb_flags == NOTOK)
		    return OK;

		if (e -> eb_name && autosw) {
		    char   *cp = e -> eb_name;

		    if (*cp != '/'
			    && *cp != '.'
			    && *cp != '|'
			    && *cp != '!'
			    && !index (cp, '%')) {
			if (!ct -> c_storeproc)
			    ct -> c_storeproc = add (cp, NULLCP);
			if (!p -> c_storeproc)
			    p -> c_storeproc = add (cp, NULLCP);
		    }
		}

		ct -> c_ctshowfnx = show_external;
		ct -> c_ctstorefnx = store_external;
		switch (p -> c_type) {
		    case CT_MULTIPART:
		        break;

		    case CT_MESSAGE:
			if (p -> c_subtype != MESSAGE_RFC822)
			    break;
			/* else fall... */
		    default:
			e -> eb_partno = ct -> c_partno;
			if (p -> c_ctinitfnx)
			    (void) (*p -> c_ctinitfnx) (p);
			break;
		}
	    }
	    break;

	default:
	    break;
    }

    return OK;
}

/*    APPLICATION */

#define	APPLICATION_UNKNOWN	0x00
#define	APPLICATION_OCTETS	0x01
#define	APPLICATION_POSTSCRIPT	0x02


static int  list_application (ct, toplevel)
register CT	ct;
int	toplevel;
{
    (void) list_content (ct, toplevel);
    if (verbosw) {
	register char **ap,
		      **ep;
	register CI	ci = &ct -> c_ctinfo;

	for (ap = ci -> ci_attrs, ep = ci -> ci_values; *ap; ap++, ep++)
	    printf ("\t     %s=\"%s\"\n", *ap, *ep);
    }

    return OK;
}


static struct k2v SubApplication[] = {
    "octet-stream",	  APPLICATION_OCTETS,
    "postscript",	  APPLICATION_POSTSCRIPT,
    NULL,	    	  APPLICATION_UNKNOWN	/* this one must be last! */
};

static int  InitApplication (ct)
register CT	ct;
{
    register char **ap,
		  **ep;
    register struct k2v *kv;
    register CI	    ci = &ct -> c_ctinfo;

    ct -> c_ctlistfnx = list_application;
    
    for (kv = SubApplication; kv -> kv_key; kv++)
	if (uleq (ci -> ci_subtype, kv -> kv_key))
	    break;

    for (ap = ci -> ci_attrs, ep = ci -> ci_values; *ap; ap++, ep++)
	if (autosw && !ct -> c_storeproc && uleq (*ap, "x-name")) {
	    register char   *cp;

	    if (*(cp = *ep) != '/'
		    && *cp != '.'
		    && *cp != '|'
		    && *cp != '!'
		    && !index (cp, '%'))
		ct -> c_storeproc = add (cp, NULLCP);
	}

    if ((ct -> c_subtype = kv -> kv_value) == APPLICATION_OCTETS) {
	int	tarP,
		zP;

	tarP = zP = 0;
	for (ap = ci -> ci_attrs, ep = ci -> ci_values; *ap; ap++, ep++) {
	    if (uleq (*ap, "type")) {
		if (!uleq (*ep, "tar"))
		    break;

		tarP = 1;
		continue;
	    }

	    if ((uleq (*ap, "conversions") || uleq (*ap, "x-conversions"))
		    && (uleq (*ep, "compress") || uleq (*ep, "x-compress"))) {
		zP = 1;
		continue;
	    }
	}

	if (tarP) {
	    ct -> c_showproc = add (zP ? "%euncompress | tar tvf -"
				       : "%etar tvf -", NULLCP);
	    if (!ct -> c_storeproc)
		if (autosw) {
		    ct -> c_storeproc = add (zP ? "| uncompress | tar xvpf -"
					        : "| tar xvpf -", NULLCP);
		    ct -> c_umask = 0022;
		}
		else
		    ct -> c_storeproc = add (zP ? "%m%P.tar.Z" : "%m%P.tar",
					     NULLCP);
	}
    }

    return OK;
}

/*    ENCODINGS */

#include "md5.c"


struct cefile {
    char   *ce_file;
    int	    ce_unlink;

    FILE   *ce_fp;
};


static int  list_encoding (ct)
register CT	ct;
{
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;

    if (ce)
	fprintf (stderr, "  decoded fp 0x%x file \"%s\"\n", ce -> ce_fp,
		 ce -> ce_file ? ce -> ce_file : "");

    return OK;
}


static int  close_encoding (ct)
register CT	ct;
{
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;

    if (!ce)
	return;

    if (ce -> ce_fp) {
	(void) fclose (ce -> ce_fp);
	ce -> ce_fp = NULL;
    }
}


static unsigned long  size_encoding (ct)
register CT	ct;
{
    int	    fd;
    unsigned long size;
    char   *file;
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;
    struct stat st;

    if (!ce) {
estimate: ;

	return (ct -> c_end - ct -> c_begin);
    }

    if (ce -> ce_fp && fstat (fileno (ce -> ce_fp), &st) != NOTOK)
	return (long) st.st_size;

    if (ce -> ce_file)
	return stat (ce -> ce_file, &st) != NOTOK ? (long) st.st_size : 0L;

    if (ct -> c_encoding == CE_EXTERNAL)
	goto estimate;

    file = NULL;
    if ((fd = (*ct -> c_ceopenfnx) (ct, &file)) == NOTOK)
	goto estimate;

    size = fstat (fd, &st) != NOTOK ? (long) st.st_size : 0L;

    (*ct -> c_ceclosefnx) (ct);

    return size;
}


static int  free_encoding (ct, toplevel)
register CT	ct;
int	toplevel;
{
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;

    if (!ce)
	return;

    if (ce -> ce_fp) {
	(void) fclose (ce -> ce_fp);
	ce -> ce_fp = NULL;
    }

    if (ce -> ce_file) {
	if (ce -> ce_unlink)
	    (void) unlink (ce -> ce_file);
	free (ce -> ce_file);
    }

    if (toplevel) {
	free ((char *) ce);
	ct -> c_ceparams = NULL;
    }
    else
	ct -> c_ceopenfnx = NULL;
}


static	init_encoding (ct, openfnx)
register CT	ct;
int   (*openfnx) ();
{
    register struct cefile *ce;

    if ((ce = (struct cefile *) calloc (1, sizeof *ce)) == NULL)
	adios (NULLCP, "out of memory");

    ct -> c_ceparams = (caddr_t) ce;
    ct -> c_ceopenfnx = openfnx;
    ct -> c_ceclosefnx = close_encoding;
    ct -> c_cesizefnx = size_encoding;
    ct -> c_celistfnx = list_encoding;
    ct -> c_cefreefnx = free_encoding;

    return OK;
}

/*    BASE64 */

static unsigned char b642nib[0x80] = {
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0x3e, 0xff, 0xff, 0xff, 0x3f,
    0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b,
    0x3c, 0x3d, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 
    0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
    0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
    0x17, 0x18, 0x19, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 
    0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
    0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30,
    0x31, 0x32, 0x33, 0xff, 0xff, 0xff, 0xff, 0xff
};


static int  openBase64 (ct, file)
register CT	ct;
char  **file;
{
    int	    bitno,
	    cc,
	    digested,
	    fd,
	    len,
	    skip;
    unsigned long    bits;
    register char  *cp,
		   *ep;
    unsigned char   value,
		   *b = (unsigned char *) &bits,
		   *b1 = &b[endian > 0 ? 1 : 2],
		   *b2 = &b[endian > 0 ? 2 : 1],
		   *b3 = &b[endian > 0 ? 3 : 0];
    char    buffer[BUFSIZ];
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;
    MD5_CTX mdContext;

    if (ce -> ce_fp)
	goto ready_to_go;
    if (ce -> ce_file) {
	if ((ce -> ce_fp = fopen (ce -> ce_file, "r")) == NULL) {
	    content_error (ce -> ce_file, ct, "unable to fopen for reading");
	    return NOTOK;
	}

	*file = ce -> ce_file;
	return fileno (ce -> ce_fp);
    }

    ce -> ce_unlink = *file == NULL;
    if ((ce -> ce_fp = fopen (ce -> ce_file =
				    add (*file ? *file : m_scratch ("", tmp),
					 NULLCP),
			      "w+")) == NULL) {
	content_error (ce -> ce_file, ct,
		       "unable to fopen for writing and reading");
	return NOTOK;
    }

    if ((len = ct -> c_end - ct -> c_begin) < 0)
	adios (NULLCP, "internal error(1)");

    if (!ct -> c_fp && (ct -> c_fp = fopen (ct -> c_file, "r")) == NULL) {
	content_error (ct -> c_file, ct, "unable to open for reading");
	return NOTOK;
    }
    
    if (digested = ct -> c_digested)
	MD5Init (&mdContext);

    (void) lseek (fd = fileno (ct -> c_fp), (off_t)ct -> c_begin, 0);
    bitno = 18, bits = 0L, skip = 0;
    while (len > 0)
	switch (cc = read (fd, buffer, sizeof buffer - 1)) {
	    case NOTOK:
	        content_error (ct -> c_file, ct, "error reading from");
	        goto clean_up;

	    case OK:
		content_error (NULLCP, ct, "premature eof");
	        goto clean_up;

	    default:
		if (cc > len)
		    cc = len;
		len -= cc;

		for (ep = (cp = buffer) + cc; cp < ep; cp++)
		    switch (*cp) {
			default:
			    if (isspace (*cp))
				break;
			    if (skip
				    || (*cp & 0x80)
				    || (value = b642nib[*cp & 0x7f]) > 0x3f) {
				if (debugsw)
				    fprintf (stderr,
					     "*cp=0x%x pos=%ld skip=%d\n", *cp,
					     (long) lseek (fd, (off_t)0, 1) - (ep - cp),
					     skip);
				content_error (NULLCP, ct,
				      "invalid BASE64 encoding -- continuing");
				continue;
			    }

			    bits |= value << bitno;
test_end: ;
			    if ((bitno -= 6) < 0) {
				(void) putc ((char) *b1, ce -> ce_fp);
				if (digested)
				    MD5Update (&mdContext, b1, 1);
				if (skip < 2) {
				    (void) putc ((char) *b2, ce -> ce_fp);
				    if (digested)
					MD5Update (&mdContext, b2, 1);
				    if (skip < 1) {
					(void) putc ((char) *b3, ce -> ce_fp);
					if (digested)
					    MD5Update (&mdContext, b3, 1);
				    }
				}

				if (ferror (ce -> ce_fp)) {
				    content_error (ce -> ce_file, ct,
						   "error writing to");
				    goto clean_up;
				}
				bitno = 18, bits = 0L, skip = 0;
			    }
			    break;

			case '=':
			    if (++skip > 3)
				goto self_delimiting;
			    goto test_end;
		    }
	}
    if (bitno != 18) {
	if (debugsw)
	    fprintf (stderr, "premature ending (bitno %d)\n", bitno);

	content_error (NULLCP, ct, "invalid BASE64 encoding");
	goto clean_up;
    }
self_delimiting: ;
    (void) fseek (ct -> c_fp, 0L, 0);

    if (fflush (ce -> ce_fp)) {
	content_error (ce -> ce_file, ct, "error writing to");
	goto clean_up;
    }

    if (digested) {
	unsigned char  digest[16];

	MD5Final (digest, &mdContext);
	if (bcmp ((char *) digest, (char *) ct -> c_digest,
		  sizeof digest / sizeof digest[0]))
	    content_error (NULLCP, ct,
			   "content integrity suspect (digest mismatch) -- continuing");
	else
	    if (debugsw)
		fprintf (stderr, "content integrity confirmed\n");
    }

ready_to_go: ;
    (void) fseek (ce -> ce_fp, 0L, 0);
    *file = ce -> ce_file;
    return fileno (ce -> ce_fp);

clean_up: ;
    free_encoding (ct, 0);

    return NOTOK;
}


static int  InitBase64 (ct)
register CT	ct;
{
    return init_encoding (ct, openBase64);
}


static int  set_endian ()
{
    char   *cp;
    union {
	long	l;
	char	c[sizeof (long)];
    }	un;

    un.l = 1;
    endian = un.c[0] ? -1 : 1;
    if (debugsw)
	fprintf (stderr, "%s endian architecture\n",
		 endian > 0 ? "big" : "little");

    mm_charset = getenv ("MM_CHARSET");

    if ((cp = getenv ("MM_NOASK")) && strcmp (cp, "1") == 0)
	nolist = 1, listsw = pausesw = 0;
}

/*    QUOTED */

static char hex2nib[0x80] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};


static int  openQuoted (ct, file)
register CT	ct;
char  **file;
{
    int	    cc,
	    digested,
	    len,
	    quoted;
    register char  *cp,
		   *ep;
    char    buffer[BUFSIZ];
    unsigned char mask;
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;
    MD5_CTX mdContext;

    if (ce -> ce_fp)
	goto ready_to_go;
    if (ce -> ce_file) {
	if ((ce -> ce_fp = fopen (ce -> ce_file, "r")) == NULL) {
	    content_error (ce -> ce_file, ct, "unable to fopen for reading");
	    return NOTOK;
	}

	*file = ce -> ce_file;
	return fileno (ce -> ce_fp);
    }

    ce -> ce_unlink = *file == NULL;
    if ((ce -> ce_fp = fopen (ce -> ce_file =
				    add (*file ? *file : m_scratch ("", tmp),
					 NULLCP),
			      "w+")) == NULL) {
	content_error (ce -> ce_file, ct,
		       "unable to fopen for writing and reading");
	return NOTOK;
    }

    if ((len = ct -> c_end - ct -> c_begin) < 0)
	adios (NULLCP, "internal error(2)");

    if (!ct -> c_fp && (ct -> c_fp = fopen (ct -> c_file, "r")) == NULL) {
	content_error (ct -> c_file, ct, "unable to open for reading");
	return NOTOK;
    }

    if (digested = ct -> c_digested)
	MD5Init (&mdContext);

    (void) fseek (ct -> c_fp, ct -> c_begin, 0);
    quoted = 0;
#ifdef	lint
    mask = 0;
#endif
    while (len > 0) {
	char   *dp;

	if (fgets (buffer, sizeof buffer - 1, ct -> c_fp) == NULL) {
	    content_error (NULLCP, ct, "premature eof");
	    goto clean_up;
	}

	if ((cc = strlen (buffer)) > len)
	    cc = len;
	len -= cc;

	for (ep = (cp = buffer) + cc - 1; cp <= ep; ep--)
	    if (!isspace (*ep))
		break;
	*++ep = '\n', ep++;

	for (; cp < ep; cp++) {
	    if (quoted) {
		if (quoted > 1) {
		    if (!isxdigit (*cp)) {
invalid_hex: ;
			dp = "expecting hexidecimal-digit";
			goto invalid_encoding;
		    }
		    mask <<= 4;
		    mask |= hex2nib[*cp & 0x7f];
		    (void) putc (mask, ce -> ce_fp);
		    if (digested)
			MD5Update (&mdContext, &mask, 1);
		}
		else
		    switch (*cp) {
		        case ':':
			    (void) putc (*cp, ce -> ce_fp);
			    if (digested)
				MD5Update (&mdContext, (unsigned char *) ":",
					   1);
			    break;

			default:
			    if (!isxdigit (*cp))
				goto invalid_hex;
			    mask = hex2nib[*cp & 0x7f];
			    quoted = 2;
			    continue;
		    }

		if (ferror (ce -> ce_fp)) {
		    content_error (ce -> ce_file, ct, "error writing to");
		    goto clean_up;
		}
		quoted = 0;
		continue;
	    }

	    switch (*cp) {
	        default:
		    if (*cp < '!' || *cp > '~') {
			int	i;
			dp = "expecting character in range [!..~]";

invalid_encoding: ;
			i = strlen (invo_name) + 2;
			content_error (NULLCP, ct,
				       "invalid QUOTED-PRINTABLE encoding -- %s,\n%*.*sbut got char 0x%x",
				       dp, i, i, "", *cp);
			goto clean_up;
		    }
		    /* and fall...*/
		case ' ':
		case '\t':
		case '\n':
		    (void) putc (*cp, ce -> ce_fp);
		    if (digested) {
			if (*cp == '\n')
			    MD5Update (&mdContext, (unsigned char *) "\r\n",2);
			else
			    MD5Update (&mdContext, (unsigned char *) cp, 1);
		    }
		    if (ferror (ce -> ce_fp)) {
			content_error (ce -> ce_file, ct, "error writing to");
			goto clean_up;
		    }
		    break;

		case '=':
		    if (*++cp != '\n') {
			quoted = 1;
			cp--;
		    }
		    break;
	    }
	}
    }
    if (quoted) {
	content_error (NULLCP, ct,
		       "invalid QUOTED-PRINTABLE encoding -- end-of-content while still quoting");
	goto clean_up;
    }
    (void) fseek (ct -> c_fp, 0L, 0);

    if (fflush (ce -> ce_fp)) {
	content_error (ce -> ce_file, ct, "error writing to");
	goto clean_up;
    }

    if (digested) {
	unsigned char  digest[16];

	MD5Final (digest, &mdContext);
	if (bcmp ((char *) digest, (char *) ct -> c_digest,
		  sizeof digest / sizeof digest[0]))
	    content_error (NULLCP, ct,
			   "content integrity suspect (digest mismatch) -- continuing");
	else
	    if (debugsw)
		fprintf (stderr, "content integrity confirmed\n");
    }

ready_to_go: ;
    (void) fseek (ce -> ce_fp, 0L, 0);
    *file = ce -> ce_file;
    return fileno (ce -> ce_fp);

clean_up: ;
    free_encoding (ct, 0);

    return NOTOK;
}


static int  InitQuoted (ct)
register CT	ct;
{
    return init_encoding (ct, openQuoted);
}

/*    7BIT */

static int  open7Bit (ct, file)
register CT	ct;
char  **file;
{
    int	    cc,
	    fd,
	    len;
    char    buffer[BUFSIZ];
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;

    if (ce -> ce_fp)
	goto ready_to_go;
    if (ce -> ce_file) {
	if ((ce -> ce_fp = fopen (ce -> ce_file, "r")) == NULL) {
	    content_error (ce -> ce_file, ct, "unable to fopen for reading");
	    return NOTOK;
	}

	*file = ce -> ce_file;
	return fileno (ce -> ce_fp);
    }

    ce -> ce_unlink = *file == NULL;
    if ((ce -> ce_fp = fopen (ce -> ce_file =
				    add (*file ? *file : m_scratch ("", tmp),
					 NULLCP),
			      "w+")) == NULL) {
	content_error (ce -> ce_file, ct,
		       "unable to fopen for writing and reading");
	return NOTOK;
    }

    if (ct -> c_type == CT_MULTIPART) {
	register char **ap,
		      **ep;
	register CI	ci = &ct -> c_ctinfo;

	len = 0;

	fprintf (ce -> ce_fp, "%s: %s/%s", TYPE_FIELD, ci -> ci_type,
		 ci -> ci_subtype);
	len += strlen (TYPE_FIELD) + 2 + strlen (ci -> ci_type)
				   + 1 + strlen (ci -> ci_subtype);
	for (ap = ci -> ci_attrs, ep = ci -> ci_values; *ap; ap++, ep++) {
	    (void) putc (';', ce -> ce_fp);
	    len++;

	    (void) sprintf (buffer, "%s=\"%s\"", *ap, *ep);

	    if (len + 1 + (cc = strlen (buffer)) >= CPERLIN) {
		(void) fputs ("\n\t", ce -> ce_fp);
		len = 8;
	    }
	    else {
		(void) putc (' ', ce -> ce_fp);
		len++;
	    }
	    fprintf (ce -> ce_fp, "%s", buffer);
	    len += cc;
	}
	if (ci -> ci_comment) {
	    if (len + 1 + (cc = 2 + strlen (ci -> ci_comment)) >= CPERLIN) {
		(void) fputs ("\n\t", ce -> ce_fp);
		len = 8;
	    }
	    else {
		(void) putc (' ', ce -> ce_fp);
		len++;
	    }
	    fprintf (ce -> ce_fp, "(%s)", ci -> ci_comment);
	    len += cc;
	}
	fprintf (ce -> ce_fp, "\n");
	if (ct -> c_id)
	    fprintf (ce -> ce_fp, "%s:%s", ID_FIELD, ct -> c_id);
	if (ct -> c_descr)
	    fprintf (ce -> ce_fp, "%s:%s", DESCR_FIELD, ct -> c_descr);
	fprintf (ce -> ce_fp, "\n");
    }

    if ((len = ct -> c_end - ct -> c_begin) < 0)
	adios (NULLCP, "internal error(3)");

    if (!ct -> c_fp && (ct -> c_fp = fopen (ct -> c_file, "r")) == NULL) {
	content_error (ct -> c_file, ct, "unable to open for reading");
	return NOTOK;
    }

    (void) lseek (fd = fileno (ct -> c_fp), (off_t) ct -> c_begin, 0);
    while (len > 0)
	switch (cc = read (fd, buffer, sizeof buffer - 1)) {
	    case NOTOK:
	        content_error (ct -> c_file, ct, "error reading from");
	        goto clean_up;

	    case OK:
		content_error (NULLCP, ct, "premature eof");
	        goto clean_up;

	    default:
		if (cc > len)
		    cc = len;
		len -= cc;

		(void) fwrite (buffer, sizeof *buffer, cc, ce -> ce_fp);
		if (ferror (ce -> ce_fp)) {
		    content_error (ce -> ce_file, ct, "error writing to");
		    goto clean_up;
		}
	}
    (void) fseek (ct -> c_fp, 0L, 0);

    if (fflush (ce -> ce_fp)) {
	content_error (ce -> ce_file, ct, "error writing to");
	goto clean_up;
    }

ready_to_go: ;
    (void) fseek (ce -> ce_fp, 0L, 0);
    *file = ce -> ce_file;
    return fileno (ce -> ce_fp);

clean_up: ;
    free_encoding (ct, 0);

    return NOTOK;
}


static int  Init7Bit (ct)
register CT	ct;
{
    if (init_encoding (ct, open7Bit) == NOTOK)
	return NOTOK;
    ct -> c_cesizefnx = NULL;

    return OK;
}

/*    External */

static int  openExternal (ct, cb, ce, file, fd)
register CT	ct;
CT	cb;
struct cefile *ce;
char  **file;
int    *fd;
{
    char    cachefile[BUFSIZ];

    if (ce -> ce_fp) {
	(void) fseek (ce -> ce_fp, 0L, 0);

ready_already: ;
	*file = ce -> ce_file, *fd = fileno (ce -> ce_fp);
	return DONE;
    }

    if (ce -> ce_file) {
	if ((ce -> ce_fp = fopen (ce -> ce_file, "r")) == NULL) {
	    content_error (ce -> ce_file, ct, "unable to fopen for reading");
	    return NOTOK;
	}

	goto ready_already;
    }

    if (find_cache (ct, rcachesw, (int *) 0, cb -> c_id, cachefile) != NOTOK) {
	if (ce -> ce_fp = fopen (cachefile, "r")) {
	    ce -> ce_unlink = 0;
	    ce -> ce_file = getcpy (cachefile);
	    goto ready_already;
	}
	else
	    admonish (cachefile, "unable to fopen for reading");
    }

    return OK;
}

/*    File */

static int  openFile (ct, file)
register CT	ct;
char  **file;
{
    int	    fd,
	    cachetype;
    char    cachefile[BUFSIZ];
    register struct exbody *e = (struct exbody *) ct -> c_ctextern;
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;

    switch (openExternal (e -> eb_parent, e -> eb_content, ce, file, &fd)) {
	case NOTOK:
	    return NOTOK;

	case OK:
	    break;

	case DONE:
	    return fd;
    }

    if (!e -> eb_name) {
	content_error (NULLCP, ct, "missing name parameter");
	return NOTOK;
    }

    ce -> ce_unlink = 0;
    if ((ce -> ce_fp = fopen (ce -> ce_file = getcpy (e -> eb_name), "r"))
	    == NULL) {
	content_error (ce -> ce_file, ct, "unable to fopen for reading");
	return NOTOK;
    }

    if ((!e -> eb_permission || !uleq (e -> eb_permission, "read-write"))
	    && find_cache (NULLCT, wcachesw, &cachetype,
			   e -> eb_content -> c_id, cachefile) != NOTOK) {
	int	mask;
	FILE   *fp;

	mask = umask (cachetype ? ~m_gmprot () : 0222);
	if (fp = fopen (cachefile, "w")) {
	    int	    cc;
	    char    buffer[BUFSIZ];
	    FILE   *gp = ce -> ce_fp;

	    (void) fseek (gp, 0L, 0);

	    while ((cc = fread (buffer, sizeof *buffer, sizeof buffer, gp))
		       > 0)
		(void) fwrite (buffer, sizeof *buffer, cc, fp);
	    (void) fflush (fp);

	    if (ferror (gp)) {
		admonish (ce -> ce_file, "error reading");
		(void) unlink (cachefile);
	    }
	    else
		if (ferror (fp)) {
		    admonish (cachefile, "error writing");
		    (void) unlink (cachefile);
		}
	    (void) fclose (fp);
	}
	(void) umask (mask);
    }

    (void) fseek (ce -> ce_fp, 0L, 0);
    *file = ce -> ce_file;
    return fileno (ce -> ce_fp);
}

/*    FTP */

static int  openFTP (ct, file)
register CT	ct;
char  **file;
{
    int	    cachetype,
	    caching,
	    fd;
    char   *bp,
	   *ftp,
	   *user,
	   *pass,
	    buffer[BUFSIZ],
	    cachefile[BUFSIZ];
    register struct exbody *e = (struct exbody *) ct -> c_ctextern;
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;
    static char   *username = NULL;
    static char   *password = NULL;

    (void) sprintf (buffer, "%s-access-ftp", invo_name);
    if ((ftp = m_find (buffer)) && !*ftp)
	ftp = NULLCP;
#ifndef	FTP
    if (!ftp)
	return NOTOK;
#endif
    switch (openExternal (e -> eb_parent, e -> eb_content, ce, file, &fd)) {
	case NOTOK:
	    return NOTOK;

	case OK:
	    break;

	case DONE:
	    return fd;
    }

    if (!e -> eb_name || !e -> eb_site) {
	content_error (NULLCP, ct, "missing %s parameter",
		       e -> eb_name ? "site": "name");
	return NOTOK;
    }

    if (xpid) {
	if (xpid < 0)
	    xpid = -xpid;
	(void) pidcheck (pidwait (xpid, NOTOK));
	xpid = 0;
    }

    bp = buffer;
    (void) sprintf (bp, "Retrieve %s", e -> eb_name);
    bp += strlen (bp);
    if (e -> eb_partno) {
	(void) sprintf (bp, " (content %s)", e -> eb_partno);
	bp += strlen (bp);
    }
    (void) sprintf (bp, "\n    using %sFTP from site %s",
		    e -> eb_flags ? "anonymous " : "", e -> eb_site);
    bp += strlen (bp);
    if (e -> eb_size > 0) {
	(void) sprintf (bp, " (%lu octets)", e -> eb_size);
	bp += strlen (bp);
    }
    (void) sprintf (bp, "? ");
    if (!getanswer (buffer))
	return NOTOK;

    if (e -> eb_flags) {
	user = "anonymous";
	(void) sprintf (pass = buffer, "%s@%s", getusr (), LocalName ());
    }
    else {
	ruserpass (e -> eb_site, &username, &password);
	user = username, pass = password;
    }

    ce -> ce_unlink = *file == NULL, caching = 0, cachefile[0] = 0;
    if ((!e -> eb_permission || !uleq (e -> eb_permission, "read-write"))
	    && find_cache (NULLCT, wcachesw, &cachetype,
			   e -> eb_content -> c_id, cachefile) != NOTOK) {
	if (*file == NULL) {
	    ce -> ce_unlink = 0;
	    caching = 1;
	}
    }

    if ((ce -> ce_fp = fopen (ce -> ce_file =
				    add (*file ? *file
					       : caching ? cachefile
					       : m_scratch ("", tmp),
					 NULLCP),
			      "w+")) == NULL) {
	content_error (ce -> ce_file, ct,
		       "unable to fopen for writing and reading");
	return NOTOK;
    }

#ifdef	FTP
    if (ftp)
#endif
    {
	int	child_id,
		i,
		vecp;
	char   *vec[9];

	vecp = 0;
	vec[vecp++] = r1bindex (ftp, '/');
	vec[vecp++] = e -> eb_site;
	vec[vecp++] = user;
	vec[vecp++] = pass;
	vec[vecp++] = e -> eb_dir;
	vec[vecp++] = e -> eb_name;
	vec[vecp++] = ce -> ce_file,
	vec[vecp++] = e -> eb_mode && uleq (e -> eb_mode, "ascii")
	    		? "ascii" : "binary";
	vec[vecp] = NULL;

	(void) fflush (stdout);

	for (i = 0; (child_id = vfork ()) == NOTOK && i < 5; i++)
	    sleep (5);
	switch (child_id) {
	    case NOTOK:
	        adios ("fork", "unable to");
		/* NOTREACHED */

	    case OK:
		(void) close (fileno (ce -> ce_fp));
		(void) execvp (ftp, vec);
		fprintf (stderr, "unable to exec ");
		perror (ftp);
		_exit (-1);
		/* NOTREACHED */

	    default:
		if (pidXwait (child_id, NULLCP)) {
#ifdef	FTP
losing_ftp: ;
#endif
		    username = password = NULL;
		    ce -> ce_unlink = 1;
		    return NOTOK;
		}
		break;
	}
    }
#ifdef	FTP
    else
	if (ftp_get (e -> eb_site, user, pass, e -> eb_dir, e -> eb_name,
		     ce -> ce_file,
		     e -> eb_mode && uleq (e -> eb_mode, "ascii"), 0)
	        == NOTOK)
	    goto losing_ftp;
#endif

    if (cachefile[0])
	if (caching)
	    (void) chmod (cachefile, cachetype ? m_gmprot () : 0444);
	else {
	    int	    mask;
	    FILE   *fp;

	    mask = umask (cachetype ? ~m_gmprot () : 0222);
	    if (fp = fopen (cachefile, "w")) {
		int	cc;
		FILE   *gp = ce -> ce_fp;

		(void) fseek (gp, 0L, 0);

		while ((cc = fread (buffer, sizeof *buffer, sizeof buffer, gp))
		           > 0)
		    (void) fwrite (buffer, sizeof *buffer, cc, fp);
		(void) fflush (fp);

		if (ferror (gp)) {
		    admonish (ce -> ce_file, "error reading");
		    (void) unlink (cachefile);
		}
		else
		    if (ferror (fp)) {
			admonish (cachefile, "error writing");
			(void) unlink (cachefile);
		    }
		(void) fclose (fp);
	    }
	    (void) umask (mask);
	}

    (void) fseek (ce -> ce_fp, 0L, 0);
    *file = ce -> ce_file;
    return fileno (ce -> ce_fp);
}

/*    Mail */

static int  openMail (ct, file)
register CT	ct;
char  **file;
{
    int	    child_id,
	    fd,
	    i,
	    vecp;
    char   *bp,
	    buffer[BUFSIZ],
	   *vec[7];
    register struct exbody *e = (struct exbody *) ct -> c_ctextern;
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;

    switch (openExternal (e -> eb_parent, e -> eb_content, ce, file, &fd)) {
	case NOTOK:
	    return NOTOK;

	case OK:
	    break;

	case DONE:
	    return fd;
    }

    if (!e -> eb_server) {
	content_error (NULLCP, ct, "missing server parameter");
	return NOTOK;
    }

    if (xpid) {
	if (xpid < 0)
	    xpid = -xpid;
	(void) pidcheck (pidwait (xpid, NOTOK));
	xpid = 0;
    }

    bp = buffer;
    (void) sprintf (bp, "Retrieve content");
    bp += strlen (bp);
    if (e -> eb_partno) {
	(void) sprintf (bp, " %s", e -> eb_partno);
	bp += strlen (bp);
    }
    (void) sprintf (bp, " by asking %s\n\n%s\n? ",
		    e -> eb_server,
		    e -> eb_subject ? e -> eb_subject : e -> eb_body);
    if (!getanswer (buffer))
	return NOTOK;

    vecp = 0;
    vec[vecp++] = r1bindex (mailproc, '/');
    vec[vecp++] = e -> eb_server;
    vec[vecp++] = "-subject";
    vec[vecp++] = e -> eb_subject ? e -> eb_subject : "mail-server request";
    vec[vecp++] = "-body";
    vec[vecp++] = e -> eb_body;
    vec[vecp] = NULL;

    for (i = 0; (child_id = vfork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK:
	    advise ("fork", "unable to");
	    return NOTOK;

	case OK:
	    (void) execvp (mailproc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (mailproc);
	    _exit (-1);
	    /* NOTREACHED */

	default:
	    if (pidXwait (child_id, NULLCP) == OK)
		advise (NULLCP, "request sent");
	    break;
    }

    ce -> ce_unlink = *file == NULL;
    if ((ce -> ce_fp = fopen (ce -> ce_file =
				    add (*file ? *file : m_scratch ("", tmp),
					 NULLCP),
			      "w+")) == NULL) {
	content_error (ce -> ce_file, ct,
		       "unable to fopen for writing and reading");
	return NOTOK;
    }
    if (ct -> c_showproc)
	free (ct -> c_showproc);
    ct -> c_showproc = add ("true", NULLCP);

    (void) fseek (ce -> ce_fp, 0L, 0);
    *file = ce -> ce_file;
    return fileno (ce -> ce_fp);
}

/*    CACHE */

static int  find_cache (ct, policy, writing, id, buffer)
CT	ct;
int	policy,
       *writing;
char   *id,
       *buffer;
{
    int	    status = NOTOK;

    if (id == NULL)
	return NOTOK;
    id = trimcpy (id);

    if (debugsw)
	fprintf (stderr, "find_cache %s(%d) %s %s\n",
		 caches[policy].sw, policy, writing ? "writing" : "reading",
		 id);

    switch (policy) {
	case CACHE_NEVER:
	default:
	    break;

	case CACHE_ASK:
	case CACHE_PUBLIC:
	    if (cache_private
		    && !writing
		    && find_cache_aux (writing ? 2 : 0, cache_private, id,
				       buffer) == OK) {
		if (access (buffer, 04) != NOTOK) {
got_private: ;
		    if (writing)
			*writing = 1;
got_it: ; 
		    status = OK;
		    break;
		}
	    }
	    if (cache_public
		    && find_cache_aux (writing ? 1 : 0, cache_public, id,
				       buffer) == OK) {
		if (writing || access (buffer, 04) != NOTOK) {
		    if (writing)
			*writing = 0;
		    goto got_it;
		}
	    }
	    break;

	case CACHE_PRIVATE:
	    if (cache_private
		    && find_cache_aux (writing ? 2 : 0, cache_private, id,
				       buffer) == OK) {
		if (writing || access (buffer, 04) != NOTOK)
		    goto got_private;
	    }
	    break;

    }

    if (status == OK && policy == CACHE_ASK) {
	char   *bp,
		query[BUFSIZ];

	if (xpid) {
	    if (xpid < 0)
		xpid = -xpid;
	    (void) pidcheck (pidwait (xpid, NOTOK));
	    xpid = 0;
	}

	bp = query;
	if (writing)
	    (void) sprintf (bp, "Make cached, publically-accessible copy");
	else {
	    struct stat st;

	    (void) sprintf (bp, "Use cached copy");
	    bp += strlen (bp);
	    if (ct -> c_partno) {
		(void) sprintf (bp, " of content %s", ct -> c_partno);
		bp += strlen (bp);
	    }
	    (void) stat (buffer, &st);
	    (void) sprintf (bp, " (size %lu octets)",
			    (unsigned long) st.st_size);
	}
	bp += strlen (bp);
	(void) sprintf (bp, "\n    in file %s? ", buffer);
	if (!getanswer (query))
	    status = NOTOK;
    }
    if (status == OK && writing) {
	if (*writing && index (buffer, '/'))
	    (void) make_intermediates (buffer);
	(void) unlink (buffer);
    }

    free (id);
    return status;
}

/*  */

static int  find_cache_aux (writing, directory, id, buffer)
int	writing;
char   *directory,
       *id,
       *buffer;
{
    int	    mask;
#ifdef	BSD42
    int	    usemap = index (id, '/') ? 1 : 0;
#else
    int	    usemap = 1;
#endif
    char    mapfile[BUFSIZ],
	    mapname[BUFSIZ];
    FILE   *fp;
    static int	partno,
		pid;
    static long clock = 0L;

    if (debugsw)
	fprintf (stderr, "find_cache_aux %s usemap=%d\n", directory, usemap);

    (void) sprintf (mapfile, "%s/cache.map", directory);
    if (find_cache_aux2 (mapfile, id, mapname) == OK)
	goto done_map;

    if (!writing) {
	if (usemap)
	    return NOTOK;

use_raw: ;
	(void) sprintf (buffer, "%s/%s", directory, id);
	return OK;
    }

    if (!usemap && access (mapfile, 02) == NOTOK)
	goto use_raw;

    if (clock != 0L) {
	long	now;
	
	(void) time (&now);
	if (now > clock)
	    clock = 0L;
    }
    else
	pid = getpid ();
    if (clock == 0L) {
	(void) time (&clock);
	partno = 0;
    }
    else
	if (partno > 0xff)
	    clock++, partno = 0;

    (void) sprintf (mapname, "%08x%04x%02x", clock & 0xffffffff,
		    pid & 0xffff, partno++ & 0xff);
    if (debugsw)
	fprintf (stderr, "creating mapping %s -> %s\n", mapname, id);

    (void) make_intermediates (mapfile);
    mask = umask (writing == 2 ? 0077 : 0);
    if (!(fp = lkfopen (mapfile, "a")) && errno == ENOENT) {
	int	fd = creat (mapfile, 0666);

	if (fd != NOTOK) {
	    (void) close (fd);
	    fp = lkfopen (mapfile, "a");
	}
    }
    (void) umask (mask);
    if (!fp)
	return NOTOK;
    fprintf (fp, "%s: %s\n", mapname, id);
    (void) lkfclose (fp, mapfile);

done_map: ;
    if (*mapname == '/')
	(void) strcpy (buffer, mapname);
    else
	(void) sprintf (buffer, "%s/%s", directory, mapname);
    if (debugsw)
	fprintf (stderr, "use %s\n", buffer);

    return OK;
}

/*  */

static int  find_cache_aux2 (mapfile, id, mapname)
char   *mapfile,
       *id,
       *mapname;
{
    int	    state;
    char    buf[BUFSIZ],
	    name[NAMESZ];
    FILE   *fp;

    if (!(fp = lkfopen (mapfile, "r")))
	return NOTOK;

    for (state = FLD;;) {
	int	result;
	register char  *cp,
		       *dp;

	switch (state = m_getfld (state, name, buf, sizeof buf, fp)) {
	    case FLD:
	    case FLDPLUS:
	    case FLDEOF:
	        (void) strcpy (mapname, name);
		if (state != FLDPLUS)
		    cp = buf;
		else {
		    cp = add (buf, NULLCP);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, fp);
			cp = add (buf, cp);
		    }
		}
		dp = trimcpy (cp);
		if (cp != buf)
		    free (cp);
		if (debugsw)
		    fprintf (stderr, "compare %s to %s <- %s\n", id, dp,
			     mapname);
		result = strcmp (id, dp);
		free (dp);
		if (result == 0) {
		    (void) lkfclose (fp, mapfile);
		    return OK;
		}
		if (state != FLDEOF)
		    continue;
		/* else fall... */

	    case BODY:
	    case BODYEOF:
	    case FILEEOF:
	    default:
		break;
	}
	break;
    }

    (void) lkfclose (fp, mapfile);
    return NOTOK;
}

/*  */

static int  cache_content (ct)
register CT    ct;
{
    int	    cachetype;
    char   *file,
	    cachefile[BUFSIZ];
    register struct cefile *ce = (struct cefile *) ct -> c_ceparams;

    if (!ct -> c_id) {
	advise (NULLCP, "no %s: field in %s", ID_FIELD, ct -> c_file);
	return;
    }

    if (!ce) {
	advise (NULLCP, "unable to decode %s", ct -> c_file);
	return;
    }

    if (ct -> c_ceopenfnx == openMail) {
	advise (NULLCP, "a radish may no know Greek, but I do...");
	return;
    }

    if (find_cache (NULLCT, wcachesw != CACHE_NEVER ? wcachesw : CACHE_ASK,
		    &cachetype, ct -> c_id, cachefile)
	    == NOTOK) {
	advise (NULLCP, "unable to cache %s's contents", ct -> c_file);
	return;
    }
    if (wcachesw != CACHE_NEVER && wcachesw != CACHE_ASK) {
	(void) fflush (stdout);
	fprintf (stderr, "caching message %s as file %s\n", ct -> c_file,
		 cachefile);
    }

    if (ce -> ce_file) {
	int	mask = umask (cachetype ? ~m_gmprot () : 0222);
	FILE   *fp;

	if (debugsw)
	    fprintf (stderr, "caching by copying %s...\n", ce -> ce_file);

	file = NULL;
	if ((*ct -> c_ceopenfnx) (ct, &file) == NOTOK)
	    goto reset_umask;

	if (fp = fopen (cachefile, "w")) {
	    int	    cc;
	    char    buffer[BUFSIZ];
	    FILE   *gp = ce -> ce_fp;

	    (void) fseek (gp, 0L, 0);

	    while ((cc = fread (buffer, sizeof *buffer, sizeof buffer, gp))
		       > 0)
		(void) fwrite (buffer, sizeof *buffer, cc, fp);
	    (void) fflush (fp);

	    if (ferror (gp)) {
		admonish (ce -> ce_file, "error reading");
		(void) unlink (cachefile);
	    }
	    else
		if (ferror (fp)) {
		    admonish (cachefile, "error writing");
		    (void) unlink (cachefile);
		}
	    (void) fclose (fp);
	}
	else
	    content_error (cachefile, ct, "unable to fopen for writing");
reset_umask: ;
	(void) umask (mask);
    }
    else {
	if (debugsw)
	    fprintf (stderr, "in place caching...\n");

	file = cachefile;
	if ((*ct -> c_ceopenfnx) (ct, &file) != NOTOK)
	    (void) chmod (cachefile, cachetype ? m_gmprot () : 0444);
    }
}

/*    COMPOSITION */

static	char	prefix[] = "----- =_aaaaaaaaaa";

static	char   *free_file = NULL;
static	CT	free_ct = NULL;


static void  build_comp (file)
char   *file;
{
    int	    compnum,
	    state;
    char   *cp,
	    buf[BUFSIZ],
	    name[NAMESZ],
	    tmpfil[BUFSIZ];
    struct multipart *m;
    register struct part **pp;
    CT	    ct;
    FILE   *in,
	   *out;

    if ((in = fopen (file, "r")) == NULL)
	adios (file, "unable to open for reading");

    (void) umask (~m_gmprot ());

    (void) strcpy (tmpfil, m_scratch (file, invo_name));
    if ((out = fopen (tmpfil, "w")) == NULL)
	adios (tmpfil, "unable to open for writing");
    free_file = tmpfil;

    for (compnum = 1, state = FLD;;) {
	switch (state = m_getfld (state, name, buf, sizeof buf, in)) {
	    case FLD:
	    case FLDPLUS:
	    case FLDEOF:
		compnum++;

		if (uleq (name, VRSN_FIELD))
		    adios (NULLCP, "draft shouldn't contain %s: field",
			   VRSN_FIELD);

		if (uleq (name, TYPE_FIELD)) {
		    while (state == FLDPLUS)
			state = m_getfld (state, name, buf, sizeof buf, in);
		    goto finish_field;
		}

		if (uleq (name, ENCODING_FIELD))
		    adios (NULLCP, "draft shouldn't contain %s: field",
			   ENCODING_FIELD);

		fprintf (out, "%s:%s", name, buf);
		while (state == FLDPLUS) {
		    state = m_getfld (state, name, buf, sizeof buf, in);
		    (void) fputs (buf, out);
		}
finish_field: ;
		if (state != FLDEOF)
		    continue;
		/* else fall... */

	    case FILEEOF:
		adios (NULLCP, "draft has empty body -- no directives!");
		/* NOTREACHED */

	    case BODY:
	    case BODYEOF:
		(void) fseek (in, (long) (-strlen (buf)), 1);
		break;

	    case LENERR:
	    case FMTERR:
		adios (NULLCP, "message format error in component #%d",
		       compnum);

	    default:
		adios (NULLCP, "getfld() returned %d", state);
	}
	break;
    }

    if ((free_ct = ct = (CT) calloc (1, sizeof *ct)) == NULL)
	adios (NULLCP, "out of memory");
    if (get_ctinfo ("multipart/mixed", ct, 0) == NOTOK)
	done (1);
    ct -> c_type = CT_MULTIPART;
    ct -> c_subtype = MULTI_MIXED;
    ct -> c_ctlistfnx = list_multi;
    ct -> c_ctfreefnx = free_multi;
    ct -> c_file = add (file, NULLCP);

    if ((m = (struct multipart *) calloc (1, sizeof *m)) == NULL)
	adios (NULLCP, "out of memory");
    ct -> c_ctparams = (caddr_t) m;

    pp = &m -> mp_parts;
    while (fgetstr (buf, sizeof buf - 1, in)) {
	register struct part *part;
	CT	p;

	if (user_content (in, file, buf, &p) == DONE) {
	    admonish (NULLCP, "ignoring spurious #end");
	    continue;
	}
	if (!p)
	    continue;

	if ((part = (struct part *) calloc (1, sizeof *part)) == NULL)
	    adios (NULLCP, "out of memory");
	*pp = part, pp = &part -> mp_next;
	part -> mp_part = p;
    }

    (void) fclose (in);

    if (!m -> mp_parts)
	adios (NULLCP, "no content directives found");
    if (!m -> mp_parts -> mp_next) {
	CT	p = m -> mp_parts -> mp_part;

	m -> mp_parts -> mp_part = NULL;
	free_content (ct);
	free_ct = ct = p;
    }
    else
	set_id (ct, 1);

    if ((cp = index (prefix, 'a')) == NULL)
	adios (NULLCP, "internal error(4)");

    while (compose_content (ct) == NOTOK)
	if (*cp < 'z')
	    (*cp)++;
        else
	    if (*++cp == 0)
		adios (NULLCP,
		       "giving up trying to find a unique delimiter string");
    	    else
		(*cp)++;

    fprintf (out, "%s: %s\n", VRSN_FIELD, VRSN_VALUE);
    (void) output_content (ct, out);

    if (fflush (out))
	adios (tmpfil, "error writing to");

    (void) fclose (out);

    if (listsw && ct -> c_ctlistfnx) {
	char   *savfile;

	if (headsw)
	    printf (LSTFMT1, "msg", "part", "type/subtype", "size",
		    "description");

				/* to get msgno */
	savfile = ct -> c_file, ct -> c_file = file;
	(*ct -> c_ctlistfnx) (ct, 1);
	ct -> c_file = savfile;
    }

    free_content (ct);
    free_ct = NULL;

    (void) sprintf (buf, "%s.orig", m_backup (file));
    if (rename (file, buf) == NOTOK)
	adios (buf, "unable to rename %s to", file);
    if (rename (tmpfil, file) == NOTOK) {
	advise (file, "unable to rename %s to", tmpfil);
	(void) rename (buf, file);
	done (1);
    }
    free_file = NULL;

    done (0);
}

/*  */

static char *fgetstr (s, n, stream)
char   *s;
int	n;
FILE   *stream;
{
    register char *cp,
		  *ep;

    for (ep = (cp = s) + n; cp < ep; ) {
	register int	i;

	if (!fgets (cp, n, stream))
	    return (cp != s ? s : NULL);
	if (cp == s && *cp != '#')
	    return s;

	cp += (i = strlen (cp)) - 1;
	if (i <= 1 || *cp-- != '\n' || *cp != '\\')
	    break;
	*cp = 0, n -= (i - 2);
    }

    return s;
}

/*  */

static int  user_content (in, file, buf, ctp)
FILE   *in;
char   *file,
       *buf;
CT     *ctp;
{
    int	    extrnal,
	    vrsn;
    register char  *cp,
		  **ap;
    char    buffer[BUFSIZ];
    struct multipart *m;
    register struct part **pp;
    struct stat st;
    register struct str2init *s2i;
    register CI	    ci;
    register CT	    ct;

    if (buf[0] == '\n' || strcmp (buf, "#\n") == 0) {
	*ctp = NULL;
	return OK;
    }

    if ((ct = (CT) calloc (1, sizeof *ct)) == NULL)
	adios (NULLCP, "out of memory");
    *ctp = ct;
    ci = &ct -> c_ctinfo;
    ct -> c_ctlistfnx = list_content;
    set_id (ct, 0);

    if (buf[0] != '#' || buf[1] == '#' || buf[1] == '<') {
	int	headers,
		inlineD;
	long	pos;
	char    content[BUFSIZ];
	FILE   *out;

	ct -> c_file = add (m_tmpfil (invo_name), NULLCP);
	ct -> c_unlink = 1;

	if ((out = fopen (ct -> c_file, "w")) == NULL)
	    adios (ct -> c_file, "unable to open for writing");

	if (buf[0] == '#' && buf[1] == '<') {
	    (void) strcpy (content, buf + 2);
	    inlineD = 1;
	    goto rock_and_roll;
	}
	else
	    inlineD = 0;

	(void) strcpy (content, "text/plain");
	headers = 0;
	(void) strcpy (buffer, buf[0] != '#' ? buf : buf + 1);
	for (;;) {
	    int	    i;

	    if (headers >= 0
		    && uprf (buffer, DESCR_FIELD)
		    && buffer[i = strlen (DESCR_FIELD)] == ':') {
		    headers = 1;

again_descr: ;
		    ct -> c_descr = add (buffer + i + 1, ct -> c_descr);
		    if (!fgetstr (buffer, sizeof buffer - 1, in))
			adios (NULLCP,
			       "end-of-file after %s: field in plaintext",
			       DESCR_FIELD);
		    switch (buffer[0]) {
		        case ' ':
		        case '\t':
			    i = -1;
			    goto again_descr;

			case '#':
			    adios (NULLCP,
				   "#-directive after %s: field in plaintext",
				   DESCR_FIELD);
			    /* NOTREACHED */

			default:
			    break;
		    }
	    }

	    if (headers != 1 || buffer[0] != '\n')
		(void) fputs (buffer, out);
rock_and_roll: ;
	    headers = -1;

	    pos = ftell (in);
	    if ((cp = fgetstr (buffer, sizeof buffer - 1, in)) == NULL)
		break;
	    if (buffer[0] == '#') {
		register char  *bp;

		if (buffer[1] != '#')
		    break;
		for (cp = (bp = buffer) + 1; *cp; cp++)
		    *bp++ = *cp;
		*bp = '\0';
	    }
	}

	if (listsw)
	    ct -> c_end = ftell (out);
	(void) fclose (out);

	if (get_ctinfo (content, ct, inlineD) == NOTOK)
	    done (1);
	for (s2i = str2cts; s2i -> si_key; s2i++)
	    if (uleq (ci -> ci_type, s2i -> si_key))
		break;
	if (!s2i -> si_key && !uprf (ci -> ci_type, "X-"))
	    s2i++;
	switch (ct -> c_type = s2i -> si_value) {
	    case CT_MESSAGE:
	        if (uleq (ci -> ci_subtype, "rfc822")) {
		    ct -> c_encoding = CE_7BIT;		/* XXX */
		    goto call_init;
		}
		/* else fall... */
	    case CT_MULTIPART:
		adios (NULLCP,
		       "it makes sense to define a in-line %s content... NOT!",
		       ct -> c_type == CT_MESSAGE ? "message" : "multipart");
		/* NOTREACHED */

	    default:
call_init: ;
		if (ct -> c_ctinitfnx = s2i -> si_init)
		    (void) (*ct -> c_ctinitfnx) (ct);
	        break;
	}

	if (cp)
	    (void) fseek (in, pos, 0);
	return OK;
    }

    extrnal = buf[1] == '@';
    if (get_ctinfo (buf + (extrnal ? 2 : 1), ct, 1) == NOTOK)
	done (1);

    for (s2i = str2cts; s2i -> si_key; s2i++)
	if (uleq (ci -> ci_type, s2i -> si_key))
	    break;
    if (s2i -> si_key) {		/* type/subtype [file] */
	if (!ci -> ci_subtype)
	    adios (NULLCP, "missing subtype in \"#%s\"", ci -> ci_type);

	switch (ct -> c_type = s2i -> si_value) {
	    case CT_MULTIPART:
	        adios (NULLCP, "use \"#begin ... #end\" instead of \"#%s/%s\"",
		       ci -> ci_type, ci -> ci_subtype);
		/* NOTREACHED */

	    case CT_MESSAGE:
		if (uleq (ci -> ci_subtype, "partial"))
		    adios (NULLCP, "sorry, \"#%s/%s\" isn't supported",
			   ci -> ci_type, ci -> ci_subtype);
		if (uleq (ci -> ci_subtype, "external-body"))
		    adios (NULLCP, "use \"#@type/subtype ... [] ...\" instead of \"#%s/%s\"",
			   ci -> ci_type, ci -> ci_subtype);
use_forw: ;
		adios (NULLCP,
		       "use \"#forw [+folder] [msgs]\" instead of \"#%s/%s\"",
		       ci -> ci_type, ci -> ci_subtype);
		/* NOTREACHED */

	    default:
		if (ct -> c_ctinitfnx = s2i -> si_init)
		    (void) (*ct -> c_ctinitfnx) (ct);
		break;
	}

	if (extrnal) {
	    register struct exbody *e;
	    CT	    p;

	    if (!ci -> ci_magic)
		adios (NULLCP, "need external information for \"#@%s/%s\"",
		       ci -> ci_type, ci -> ci_subtype);
	    p = ct;

	    (void) sprintf (buffer, "message/external-body; %s",
			    ci -> ci_magic);
	    free (ci -> ci_magic), ci -> ci_magic = NULL;

	    if ((ct = (CT) calloc (1, sizeof *ct)) == NULL)
		adios (NULLCP, "out of memory");
	    *ctp = ct;
	    ci = &ct -> c_ctinfo;
	    ct -> c_ctlistfnx = list_content;
	    if (get_ctinfo (buffer, ct, 0) == NOTOK)
		done (1);
	    ct -> c_type = CT_MESSAGE;
	    ct -> c_subtype = MESSAGE_EXTERNAL;

	    if ((e = (struct exbody *) calloc (1, sizeof *e)) == NULL)
		adios (NULLCP, "out of memory");
	    ct -> c_ctparams = (caddr_t) e;
	    ct -> c_ctfreefnx = free_external;

	    e -> eb_parent = ct;
	    e -> eb_content = p;
	    p -> c_ctextern = (caddr_t) e;

	    ct -> c_ctlistfnx = list_external;

	    if (params_external (ct, 1) == NOTOK)
		done (1);

	    return OK;
	}

	if (ci -> ci_magic) {
	    if (*ci -> ci_magic == '|' || *ci -> ci_magic == '!') {
		for (cp = ci -> ci_magic + 1; isspace (*cp); cp++)
		    continue;
		if (!*cp)
		    adios (NULLCP, "empty pipe command for #%s directive",
			   ci -> ci_type);
		cp = add (cp, NULLCP);
		free (ci -> ci_magic);
		ci -> ci_magic = cp;
	    }
	    else {
		if (access (ct -> c_file = ci -> ci_magic, 04) == NOTOK)
		    adios ("reading", "unable to access %s for", ct -> c_file);
		if (listsw && stat (ct -> c_file, &st) != NOTOK)
		    ct -> c_end = (long) st.st_size;
		ci -> ci_magic = NULL;
	    }
	    return OK;
	}

	(void) sprintf (buffer, "%s-compose-%s/%s", invo_name, ci -> ci_type,
			ci -> ci_subtype);
	if ((cp = m_find (buffer)) == NULL || *cp == 0) {
	    (void) sprintf (buffer, "%s-compose-%s", invo_name, ci -> ci_type);
	    if ((cp = m_find (buffer)) == NULL || *cp == 0) {
		content_error (NULLCP, ct,
			       "don't know how to compose content");
		done (1);
	    }
	}
	ci -> ci_magic = add (cp, NULLCP);
	return OK;
    }

    if (extrnal)
	adios (NULLCP, "externally definition not allowed for \"#%s\"",
	       ci -> ci_type);

    if (uleq (ci -> ci_type, "forw")) {	/* #forw [+folder] [msgs] */
	int	msgnum;
	char   *folder,
	       *arguments[MAXARGS];
	struct msgs *mp;

	if (ci -> ci_magic) {
	    ap = brkstring (ci -> ci_magic, " ", "\n");
	    ap = copyip (ap, arguments);
	}
	else
	    arguments[0] = "cur", arguments[1] = NULL;

	folder = NULL;
	for (ap = arguments; cp = *ap; ap++)
	    if (*cp == '+' || *cp == '@')
		if (folder)
		    adios (NULLCP, "only one folder per #forw directive");
		else
		    folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	if (!folder)
	    folder = add (m_getfolder (), NULLCP);

	if ((mp = m_gmsg (folder)) == NULL)
	    adios (NULLCP, "unable to read folder %s", folder);
	for (ap = arguments; cp = *ap; ap++)
	    if (*cp != '+' && *cp != '@')
		if (!m_convert (mp, cp))
		    done (1);
	free (folder);

	free_ctinfo (ct);
	if (mp -> numsel > 1) {
	    if (get_ctinfo ("multipart/digest", ct, 0) == NOTOK)
		done (1);
	    ct -> c_type = CT_MULTIPART;
	    ct -> c_subtype = MULTI_DIGEST;
	    ct -> c_ctlistfnx = list_multi;
	    ct -> c_ctfreefnx = free_multi;

	    if ((m = (struct multipart *) calloc (1, sizeof *m)) == NULL)
		adios (NULLCP, "out of memory");
	    ct -> c_ctparams = (caddr_t) m;

	    pp = &m -> mp_parts;
	}
	else
	    free_content (ct);
	for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED) {
		register struct part *part;
		register CT	p;

		if ((p = (CT) calloc (1, sizeof *p)) == NULL)
		    adios (NULLCP, "out of memory");
		if (get_ctinfo ("message/rfc822", p, 0) == NOTOK)
		    done (1);
		p -> c_type = CT_MESSAGE;
		p -> c_subtype = MESSAGE_RFC822;
		p -> c_ctlistfnx = list_content;

		(void) sprintf (buffer, "%s/%d", mp -> foldpath, msgnum);
		p -> c_file = add (buffer, NULLCP);
		if (listsw && stat (p -> c_file, &st) != NOTOK)
		    p -> c_end = (long) st.st_size;

		if (mp -> numsel > 1) {
		    if ((part = (struct part *) calloc (1, sizeof *part))
			    == NULL)
			adios (NULLCP, "out of memory");
		    *pp = part, pp = &part -> mp_next;
		    part -> mp_part = p;
		}
		else
		    *ctp = ct = p;
	    }

	m_fmsg (mp);

	return OK;
    }

    if (uleq (ci -> ci_type, "end")) {
	free_content (ct);
	*ctp = NULL;
	return DONE;
    }

    if (!uleq (ci -> ci_type, "begin"))
	adios (NULLCP, "unknown directive \"#%s\"", ci -> ci_type);

					/* #begin [ alternative | parallel ] */
    if (!ci -> ci_magic)
	cp = SubMultiPart[(vrsn = MULTI_MIXED) - 1].kv_key;
    else
	if (uleq (ci -> ci_magic, "alternative"))
	    cp = SubMultiPart[(vrsn = MULTI_ALTERNATE) - 1].kv_key;
	else
	    if (uleq (ci -> ci_magic, "parallel"))
		cp = SubMultiPart[(vrsn = MULTI_PARALLEL) - 1].kv_key;
	else
	    if (uprf (ci -> ci_magic, "digest"))
		goto use_forw;
	    else
		cp = ci -> ci_magic, vrsn = MULTI_UNKNOWN;
    free_ctinfo (ct);
    (void) sprintf (buffer, "multipart/%s", cp);
    if (get_ctinfo (buffer, ct, 0) == NOTOK)
	done (1);
    ct -> c_type = CT_MULTIPART;
    ct -> c_subtype = vrsn;
    ct -> c_ctlistfnx = list_multi;
    ct -> c_ctfreefnx = free_multi;

    if ((m = (struct multipart *) calloc (1, sizeof *m)) == NULL)
	adios (NULLCP, "out of memory");
    ct -> c_ctparams = (caddr_t) m;

    pp = &m -> mp_parts;
    while (fgetstr (buffer, sizeof buffer - 1, in)) {
	register struct part *part;
	CT	p;

	if (user_content (in, file, buffer, &p) == DONE) {
	    if (!m -> mp_parts)
		adios (NULLCP, "empty \"#begin ... #end\" sequence");
	    return OK;
	}
	if (!p)
	    continue;

	if ((part = (struct part *) calloc (1, sizeof *part)) == NULL)
	    adios (NULLCP, "out of memory");
	*pp = part, pp = &part -> mp_next;
	part -> mp_part = p;
    }
    admonish (NULLCP, "premature end-of-file, missing #end");
    return OK;
}

/*  */

static void set_id (ct, top)
CT	ct;
int	top;
{
    char    msgid[BUFSIZ];
    static int  partno;
    static long clock = 0L;
    static char *msgfmt;

    if (clock == 0L) {
	(void) time (&clock);
	(void) sprintf (msgid, "<%d.%ld.%%d@%s>\n", getpid (), clock,
			LocalName ());
	partno = 0;
	msgfmt = getcpy (msgid);
    }
    (void) sprintf (msgid, msgfmt, top ? 0 : ++partno);
    ct -> c_id = getcpy (msgid);
}

/*  */

static char ebcdicsafe[0x100] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x00, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x01,
    0x00, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

static int  compose_content (ct)
register CT	ct;
{
    register char   *cp;
    char    buffer[BUFSIZ];
    register CI	    ci = &ct -> c_ctinfo;

    if (ct -> c_type == CT_MESSAGE && ct -> c_subtype == MESSAGE_EXTERNAL)
	return OK;

    switch (ct -> c_type) {
	case CT_MULTIPART:
	    {
		int	partnum;
		register char *pp;
		char	partnam[BUFSIZ];
		struct multipart *m = (struct multipart *) ct -> c_ctparams;
		register struct part *part;

		if (ct -> c_partno) {
		    (void) sprintf (partnam, "%s.", ct -> c_partno);
		    pp = partnam + strlen (partnam);
		}
		else
		    pp = partnam;

		for (part = m -> mp_parts, partnum = 1;
		         part;
		         part = part -> mp_next, partnum++) {
		    register CT  p = part -> mp_part;

		    (void) sprintf (pp, "%d", partnum);
		    p -> c_partno = add (partnam, NULLCP);

		    if (compose_content (p) == NOTOK)
			return NOTOK;
		}

		if (rfc934sw && ct -> c_subtype == MULTI_DIGEST) {
		    int	    is934 = 1;

		    for (part = m -> mp_parts; part; part = part -> mp_next) {
			register CT  p = part -> mp_part;

			if (p -> c_subtype != MESSAGE_RFC822) {
			    is934 = 0;
			    break;
			}
		    }

		    ct -> c_rfc934 = is934;
		    for (part = m -> mp_parts; part; part = part -> mp_next) {
			register CT  p = part -> mp_part;

			if (p -> c_rfc934 = is934)
			    p -> c_end++;
		    }
		}

		if (listsw) {
		    ct -> c_end = (partnum = strlen (prefix) + 2) + 2;
		    if (ct -> c_rfc934)
			ct -> c_end += 1;

		    for (part = m -> mp_parts; part; part = part -> mp_next)
			ct -> c_end += part -> mp_part -> c_end + partnum;
		}
	    }
	    break;


	default:
	    if (!ct -> c_file) {
		int	child_id,
			i,
			xstdout;
		register char  *bp,
			      **ap;
		char   *vec[4];
		FILE   *out;

		if (!(cp = ci -> ci_magic))
		    adios (NULLCP, "internal error(5)");

		ct -> c_file = add (m_tmpfil (invo_name), NULLCP);
		ct -> c_unlink = 1;

		xstdout = 0;
		buffer[0] = '\0';
		for (bp = buffer; *cp; cp++)
		    if (*cp == '%') {
			switch (*++cp) {
		            case 'a':	/* additional arguments */
			        {
				    register char **ep;
				    char   *s = "";

				    for (ap = ci -> ci_attrs, ep = ci -> ci_values;
					     *ap;
					     ap++, ep++) {
					(void) sprintf (bp, "%s%s=\"%s\"", s,
							*ap, *ep);
					bp += strlen (bp);
					s = " ";
				    }
				}
				break;

			    case 'F':	/* %f, and stdout is not-redirected */
				xstdout = 1;
				/* and fall... */
			    case 'f':	/* filename */
				(void) sprintf (bp, "%s", ct -> c_file);
				break;

			    case 's':	/* subtype */
				(void) strcpy (bp, ci -> ci_subtype);
				break;

			    case '%':
				goto raw;

			    default:
				*bp++ = *--cp;
				*bp = '\0';
				continue;
			}
			bp += strlen (bp);
		    }
		    else {
raw: ;
			*bp++ = *cp;
			*bp = '\0';
		    }

		printf ("composing content %s/%s from command\n\t%s\n",
			ci -> ci_type, ci -> ci_subtype, buffer);
		(void) fflush (stdout);

		vec[0] = "/bin/sh";
		vec[1] = "-c";
		vec[2] = buffer;
		vec[3] = NULL;

		if ((out = fopen (ct -> c_file, "w")) == NULL)
		    adios (ct -> c_file, "unable to open for writing");

		for (i = 0; (child_id = vfork ()) == NOTOK && i > 5; i++)
		    sleep (5);
		switch (child_id) {
		    case NOTOK:
		        adios ("fork", "unable to fork");
			/* NOTREACHED */

		    case OK:
			if (!xstdout)
			    (void) dup2 (fileno (out), 1);
			(void) close (fileno (out));
			(void) execvp ("/bin/sh", vec);
			fprintf (stderr, "unable to exec ");
			perror ("/bin/sh");
			_exit (-1);
			/* NOTREACHED */

		   default:
			(void) fclose (out);
			if (pidXwait (child_id, NULLCP))
			    done (1);
			break;
		}
	    }
	    if (listsw && ct -> c_end == 0L) {
		struct stat st;

		if (stat (ct -> c_file, &st) != NOTOK)
		    ct -> c_end = (long) st.st_size;
	    }
	    if (ct -> c_type != CT_TEXT
		    && !(ct -> c_type == CT_APPLICATION
			     && ct -> c_subtype == APPLICATION_POSTSCRIPT))
		break;
	    /* else fall... */

	case CT_MESSAGE:
	    {
		int	charset,
			len,
			linelen,
			result;
		FILE   *in;

		if ((in = fopen (ct -> c_file, "r")) == NULL)
		    adios (ct -> c_file, "unable to open for reading");

		len = strlen (prefix);
		result = OK;
		switch (ct -> c_type) {
		    case CT_TEXT:
		        charset = ct -> c_ctparams ? 0 : -1;
			linelen = ct -> c_subtype == TEXT_PLAIN ? 0 : -1;
			break;

		    case CT_APPLICATION:
			charset = linelen = ct -> c_encoding ? 0 : -1;
			break;

		    default:
			charset = linelen = 0;
			break;
		}
		while (fgets (buffer, sizeof buffer - 1, in)) {
		    if (charset == -1) {
			for (cp = buffer; *cp; cp++) {
			    if (!isascii (*cp)) {
				charset = CHARSET_UNKNOWN;
				break;
			    }
			    if (linelen == -1
				    && ebcdicsw
				    && !ebcdicsafe[*cp & 0xff])
				linelen = 1;
			}
			if ((linelen == -1) && (cp - buffer > CPERLIN + 1))
			    linelen = 1;
			if (result == NOTOK)
			    break;
		    }
		    else
			if ((linelen == -1) && (strlen (buffer) > CPERLIN + 1))
			    linelen = 1;
		    if (result == NOTOK)
			continue;

		    if (linelen == -1
			    && (cp = buffer + strlen (buffer) - 2) > buffer
			    && isspace (*cp))
			linelen = 1;

		    if (buffer[0] == '-' && buffer[1] == '-') {
			for (cp = buffer + strlen (buffer) - 1;
			         cp >= buffer;
			         cp--)
			    if (!isspace (*cp))
				break;
			*++cp = '\0';
			if (strncmp (buffer + 2, prefix, len) == 0
			        && isdigit (buffer[2 + len])) {
			    result = NOTOK;
			    if (charset != -1 && linelen != -1)
				break;
			}
		    }
		}
		if (ct -> c_type == CT_APPLICATION && !ct -> c_encoding)
		    ct -> c_encoding = linelen == -1
					        && charset != CHARSET_UNKNOWN
					    ? CE_7BIT : CE_QUOTED;
		if (ct -> c_type == CT_TEXT && !ct -> c_ctparams) {
		    register char  **ap,
				   **ep;
		    register struct text *t;

		    if (charset == CHARSET_UNKNOWN && mm_charset)
			charset = -2;
		    else
			if (charset == -1)
			    charset = CHARSET_USASCII;

		    if ((t = (struct text *) calloc (1, sizeof *t)) == NULL)
			adios (NULLCP, "out of memory");
		    ct -> c_ctparams = (caddr_t) t;
		    for (ap = ci -> ci_attrs, ep = ci -> ci_values;
			     *ap;
			     ap++, ep++)
			continue;
		    switch (t -> tx_charset = charset) {
			case CHARSET_USASCII:
			    *ap = add ("charset=us-ascii", NULLCP);
			    break;

			case CHARSET_UNKNOWN:
			default:
			    *ap = add ("charset=x-unknown", NULLCP);
			    break;

			case -2:
			    *ap = concat ("charset=", mm_charset, NULLCP);
			    break;
		    }
		    cp = index (*ap++, '=');
		    *ap = NULL;
		    *cp++ = '\0';
		    *ep = cp;
		}
		if (ct -> c_type == CT_TEXT && ct -> c_subtype != TEXT_PLAIN)
		    ct -> c_encoding = linelen == -1 ? CE_7BIT : CE_QUOTED;

		(void) fclose (in);

		return result;
	    }
    }

    return OK;
}

/*  */

static int  output_content (ct, out)
register CT	ct;
FILE   *out;
{
    int	    cc,
	    mailbody,
	    len;
    register char   **ap,
		    **ep;
    char    buffer[BUFSIZ];
    register CI	    ci = &ct -> c_ctinfo;

    if (ct -> c_type == CT_MULTIPART) {
	register char  *cp;
	static	int	encl = 0;

	ap = ci -> ci_attrs, ep = ci -> ci_values;

	(void) sprintf (buffer, "boundary=%s%d", prefix, encl++);
	cp = index (*ap++ = add (buffer, NULLCP), '=');
	*ap = NULL;
	*cp++ = '\0';
	*ep = cp;
    }
    else
	if (ct -> c_type == CT_MESSAGE && ct -> c_rfc934)
	    goto rfc934_mode;

    len = 0;
    fprintf (out, "%s: %s/%s", TYPE_FIELD, ci -> ci_type, ci -> ci_subtype);
    len += strlen (TYPE_FIELD) + 2 + strlen (ci -> ci_type)
			       + 1 + strlen (ci -> ci_subtype);
    mailbody = ct -> c_type == CT_MESSAGE
		&& ct -> c_subtype == MESSAGE_EXTERNAL
	        && ((struct exbody *) ct -> c_ctparams) -> eb_body;
    for (ap = ci -> ci_attrs, ep = ci -> ci_values; *ap; ap++, ep++) {
	if (mailbody && uleq (*ap, "body"))
	    continue;

	(void) putc (';', out);
	len++;

	(void) sprintf (buffer, "%s=\"%s\"", *ap, *ep);

	if (len + 1 + (cc = strlen (buffer)) >= CPERLIN) {
	    (void) fputs ("\n\t", out);
	    len = 8;
	}
	else {
	    (void) putc (' ', out);
	    len++;
	}
	fputs (buffer, out);
	len += cc;
    }
    if (ci -> ci_comment) {
	if (len + 1 + (cc = 2 + strlen (ci -> ci_comment)) >= CPERLIN) {
	    (void) fputs ("\n\t", out);
	    len = 8;
	}
	else {
	    (void) putc (' ', out);
	    len++;
	}
	fprintf (out, "(%s)", ci -> ci_comment);
	len += cc;
    }
    (void) putc ('\n', out);
    if (ct -> c_id)
	fprintf (out, "%s: %s", ID_FIELD, ct -> c_id);
    if (ct -> c_descr)
	fprintf (out, "%s: %s", DESCR_FIELD, ct -> c_descr);

rfc934_mode: ;
    if (ct -> c_ctextern)
	return OK;
    switch (ct -> c_type) {
	case CT_MULTIPART:
	    {
		struct multipart *m = (struct multipart *) ct -> c_ctparams;
		register struct part *part;

		if (ct -> c_rfc934)
		    (void) putc ('\n', out);

		for (part = m -> mp_parts; part; part = part -> mp_next) {
		    register CT  p = part -> mp_part;

		    fprintf (out, "\n--%s\n", ci -> ci_values[0]);
		    if (p -> c_type == CT_MESSAGE
			    && p -> c_subtype != MESSAGE_EXTERNAL
			    && !p -> c_rfc934)
			fprintf (out, "%s: %s\n", VRSN_FIELD, VRSN_VALUE);
		    (void) output_content (p, out);
		}

		fprintf (out, "\n--%s--\n", ci -> ci_values[0]);
	    }
	    break;

	case CT_TEXT:
	    if (ct -> c_ctparams
			    && ((struct text *) ct -> c_ctparams) -> tx_charset
					!= CHARSET_USASCII) {
quoted_printable: ;
		if (checksw)
		    writeDigest (ct, out, 1);
		fprintf (out, "%s: %s\n\n", ENCODING_FIELD,
			 "quoted-printable");
		(void) writeQuoted (ct, out);
		break;
	    }
	    if ((ct -> c_subtype != TEXT_PLAIN && ct -> c_encoding != CE_7BIT)
		    || checksw)
		goto quoted_printable;
	    /* else fall... */

	case CT_MESSAGE:
seven_bit: ;
	    (void) putc ('\n', out);
	    if (ct -> c_type == CT_MESSAGE
		    && ct -> c_subtype == MESSAGE_EXTERNAL) {
		register struct exbody *e = (struct exbody *) ct -> c_ctparams;
		
		(void) output_content (e -> eb_content, out);
		if (e -> eb_body) {
		    register char   *cp;

		    putc ('\n', out);
		    for (cp = e -> eb_body; *cp; cp++) {
			CT	ct2 = e -> eb_content;
			CI	ci2 = &ct2 -> c_ctinfo;

			if (*cp == '\\')
			    switch (*++cp) {
				case 'I':
				    if (ct2 -> c_id) {
					char *dp = trimcpy (ct2 -> c_id);
					
					(void) fputs (dp, out);
					free (dp);
				    }
				    continue;

				case 'N':
				    for (ap = ci2 -> ci_attrs,
					 	ep = ci2 -> ci_values;
					     *ap;
					     ap++, ep++)
					if (uleq (*ap, "name")) {
					    fprintf (out, "%s", *ep);
					    break;
					}
				    continue;
					    
				case 'T':
				    fprintf (out, "%s/%s", ci2 -> ci_type,
					     ci2 -> ci_subtype);
				    for (ap = ci2 -> ci_attrs,
					 	ep = ci2 -> ci_values;
					     *ap;
					     ap++, ep++)
					fprintf (out, "; %s=\"%s\"", *ap, *ep);
				    continue;

				case 'n':
				    (void) putc ('\n', out);
				    continue;

				case 't':
				    (void) putc ('\t', out);
				    continue;

				case '\0':
				    cp--;
				    break;

				case '\\':
				case '"':
				    break;

				default:
				    (void) putc ('\\', out);
				    break;
			    }

			(void) putc (*cp, out);
		    }
		    putc ('\n', out);
		}
	    }
	    else
		(void) write7Bit (ct, out);
	    break;

	case CT_APPLICATION:
	    if (ct -> c_subtype == APPLICATION_POSTSCRIPT) {
		if (ct -> c_encoding == CE_7BIT)
		    goto seven_bit;
		goto quoted_printable;
	    }
	    /* else fall... */

	default:
	    if (checksw)
		writeDigest (ct, out, 0);
	    fprintf (out, "%s: %s\n\n", ENCODING_FIELD, "base64");
	    (void) writeBase64 (ct, out);
	    break;
    }

    return OK;
}

/*  */

static int  write7Bit (ct, out)
register CT	ct;
FILE   *out;
{
    char    c,
	    buffer[BUFSIZ];
    FILE   *in;

    if ((in = fopen (ct -> c_file, "r")) == NULL)
	adios (ct -> c_file, "unable to open for reading");

    c = '\n';
    while (fgets (buffer, sizeof buffer - 1, in)) {
	c = buffer[strlen (buffer) - 1];
	(void) fputs (buffer, out);
    }
    if (c != '\n')
	(void) putc ('\n', out);

    (void) fclose (in);

    return OK;
}

/*  */

static int  writeQuoted (ct, out)
register CT	ct;
FILE   *out;
{
    register char *cp;
    char    c,
	    buffer[BUFSIZ];
    FILE   *in;

    if ((in = fopen (ct -> c_file, "r")) == NULL)
	adios (ct -> c_file, "unable to open for reading");

    while (fgets (buffer, sizeof buffer - 1, in)) {
	register int	n;

	cp = buffer + strlen (buffer) - 1;
	if ((c = *cp) == '\n')
	    *cp = '\0';

	if (strncmp (cp = buffer, "From ", sizeof "From " - 1) == 0) {
	    (void) fprintf (out, "=%02X", *cp++ & 0xff);
	    n = 3;
	}
	else
	    n = 0;
	for (; *cp; cp++) {
	    if (n > CPERLIN - 3) {
		(void) fputs ("=\n", out);
		n = 0;
	    }

	    switch (*cp) {
		case ' ':
		case '\t':
		    (void) putc (*cp, out);
		    n++;
		    break;

		default:
		    if (*cp < '!'
			    || *cp > '~'
			    || (ebcdicsw && !ebcdicsafe[*cp & 0xff]))
			goto three_print;
		    (void) putc (*cp, out);
		    n++;
		    break;

		case '=':
three_print: ;
		    (void) fprintf (out, "=%02X", *cp & 0xff);
		    n += 3;
		    break;
	    }
	}

	if (c == '\n') {
	    if (cp > buffer && (*--cp == ' ' || *cp == '\t'))
		(void) fputs ("=\n", out);

	    (void) putc ('\n', out);
	}
	else
	    (void) fputs ("=\n", out);
    }

    (void) fclose (in);

    return OK;
}

/*  */

static char nib2b64[0x40+1] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";


static int  writeBase64 (ct, out)
register CT	ct;
FILE   *out;
{
    int	    result;
    FILE   *in;

    if ((in = fopen (ct -> c_file, "r")) == NULL)
	adios (ct -> c_file, "unable to open for reading");

    result = writeBase64aux (in, out);

    (void) fclose (in);

    return result;
}


static int  writeBase64aux (in, out)
FILE   *in,
       *out;
{
    int	    cc,
	    n;
    char    inbuf[3];

    n = BPERLIN;
    while ((cc = fread (inbuf, sizeof *inbuf, sizeof inbuf, in)) > 0) {
	unsigned long bits;
	register char *bp;
	char    outbuf[4];

	if (cc < sizeof inbuf) {
	    inbuf[2] = 0;
	    if (cc < sizeof inbuf - 1)
		inbuf[1] = 0;
	}
	bits = (inbuf[0] & 0xff) << 16;
	bits |= (inbuf[1] & 0xff) << 8;
	bits |= inbuf[2] & 0xff;

	for (bp = outbuf + sizeof outbuf; bp > outbuf; bits >>= 6)
	    *--bp = nib2b64[bits & 0x3f];
	if (cc < sizeof inbuf) {
	    outbuf[3] = '=';
	    if (cc < sizeof inbuf - 1)
		outbuf[2] = '=';
	}

	(void) fwrite (outbuf, sizeof *outbuf, sizeof outbuf, out);

	if (cc < sizeof inbuf) {
	    (void) putc ('\n', out);
	    return OK;
	}

	if (--n <= 0) {
	    n = BPERLIN;
	    (void) putc ('\n', out);
	}
    }
    if (n != BPERLIN)
	(void) putc ('\n', out);

    return OK;
}

/*  */

static int  writeDigest (ct, out, asciiP)
register CT	ct;
FILE   *out;
int	asciiP;
{
    int	    cc;
    char    buffer[BUFSIZ];
    register unsigned char *dp;
    unsigned char  digest[16];
    FILE   *in;
    MD5_CTX mdContext;

    if ((in = fopen (ct -> c_file, "r")) == NULL)
	adios (ct -> c_file, "unable to open for reading");

    MD5Init (&mdContext);
    if (asciiP) {
	while (fgets (buffer, sizeof buffer - 1, in)) {
	    register char *cp;
	    char    c;

	    cp = buffer + strlen (buffer) - 1;
	    if ((c = *cp) == '\n')
		*cp = '\0';

	    MD5Update (&mdContext, (unsigned char *) buffer,
		       (unsigned int) strlen (buffer));

	    if (c == '\n')
		MD5Update (&mdContext, (unsigned char *) "\r\n", 2);
	}
    }
    else
	while ((cc = fread (buffer, sizeof *buffer, sizeof buffer, in)) > 0)
	    MD5Update (&mdContext, (unsigned char *) buffer,
		       (unsigned int) cc);
    MD5Final (digest, &mdContext);
    if (debugsw) {
	unsigned char *ep;

	fprintf (stderr, "MD5 digest=");
	for (ep = (dp = digest) + sizeof digest / sizeof digest[0];
	         dp < ep;
	         dp++)
	    fprintf (stderr, "%02x", *dp & 0xff);
	fprintf (stderr, "\n");
    }

    (void) fclose (in);

    fprintf (out, "%s: ", MD5_FIELD);
    for (dp = digest, cc = sizeof digest / sizeof digest[0]; cc > 0; cc -= 3) {
	unsigned long bits;
	register char *bp;
	char outbuf[4];

	bits = (*dp++ & 0xff) << 16;
	if (cc > 1) {
	    bits |= (*dp++ & 0xff) << 8;
	    if (cc > 2)
		bits |= *dp++ & 0xff;
	}

	for (bp = outbuf + sizeof outbuf; bp > outbuf; bits >>= 6)
	    *--bp = nib2b64[bits & 0x3f];
	if (cc < 3) {
	    outbuf[3] = '=';
	    if (cc < 2)
		outbuf[2] = '=';
	}

        (void) fwrite (outbuf, sizeof *outbuf, sizeof outbuf, out);
    }
    fprintf (out, "\n");
}

/*  */

static int  readDigest (ct, cp)
register CT	ct;
register char *cp;
{
    int	    bitno,
	    skip;
    unsigned long    bits;
    char   *bp = cp;
    register unsigned char *dp;
    unsigned char   value,
                   *ep,
		   *b = (unsigned char *) &bits,
		   *b1 = &b[endian > 0 ? 1 : 2],
		   *b2 = &b[endian > 0 ? 2 : 1],
		   *b3 = &b[endian > 0 ? 3 : 0];

    bitno = 18, bits = 0L, skip = 0;
    for (ep = (dp = ct -> c_digest)
	         + sizeof ct -> c_digest / sizeof ct -> c_digest[0];
	     *cp;
	     cp++)
	switch (*cp) {
	    default:
	        if (skip
		        || (*cp & 0x80)
		        || (value = b642nib[*cp & 0x7f]) > 0x3f) {
		    if (debugsw)
			fprintf (stderr, "invalid BASE64 encoding\n");
		    return NOTOK;
		}

		bits |= value << bitno;
test_end: ;
		if ((bitno -= 6) < 0) {
		    if (dp + (3 - skip) > ep)
			goto invalid_digest;
		    *dp++ = *b1;
		    if (skip < 2) {
			*dp++ = *b2;
			if (skip < 1)
			    *dp++ = *b3;
		    }
		    bitno = 18, bits = 0L, skip = 0;
		}
		break;

	    case '=':
		if (++skip > 3)
		    goto self_delimiting;
		goto test_end;
	}
    if (bitno != 18) {
	if (debugsw)
	    fprintf (stderr, "premature ending (bitno %d)\n", bitno);

	return NOTOK;
    }
self_delimiting: ;
    if (dp != ep) {
invalid_digest: ;
	if (debugsw) {
	    while (*cp)
		cp++;
	    fprintf (stderr, "invalid MD5 digest (got %d octets)\n",
		     cp - bp);
	}

	return NOTOK;
    }

    if (debugsw) {
	fprintf (stderr, "MD5 digest=");
	for (dp = ct -> c_digest; dp < ep; dp++)
	    fprintf (stderr, "%02x", *dp & 0xff);
	fprintf (stderr, "\n");
    }

    return OK;
}

/*    VIAMAIL */

#include "../zotnet/tws.h"


static int  via_mail (mailsw, subjsw, parmsw, descsw, cmntsw, slowsw, fromsw)
char   *mailsw,
       *subjsw,
       *parmsw,
       *descsw,
       *cmntsw,
       *fromsw;
int	slowsw;
{
    int	    nlines,
	    nparts,
	    status;
    long    pos;
    long    offset;
    char    tmpfil[BUFSIZ];
    struct stat st;
    FILE   *fp;

    (void) umask (~m_gmprot ());

    (void) strcpy (tmpfil, m_tmpfil (invo_name));
    if ((fp = fopen (tmpfil, "w+")) == NULL)
	adios (tmpfil, "unable to open for writing");
    (void) chmod (tmpfil, 0600);

    if (!index (mailsw, '@'))
	mailsw = concat (mailsw, "@", LocalName (), NULLCP);
    fprintf (fp, "To: %s\n", mailsw);
    nlines = 1;
    if (subjsw)
	fprintf (fp, "Subject: %s\n", subjsw), nlines++;
    if (fromsw) {
	if (!index (fromsw, '@'))
	    fromsw = concat (fromsw, "@", LocalName (), NULLCP);
	fprintf (fp, "From: %s\n", fromsw), nlines++;
    }
    fprintf (fp, "%s: %s\n", VRSN_FIELD, VRSN_VALUE), nlines++;
    offset = ftell (fp);
    fprintf (fp, "%s: application/octet-stream", TYPE_FIELD);
    if (parmsw)
	fprintf (fp, "; %s", parmsw);
    if (cmntsw)
	fprintf (fp, "\n\t(%s)", cmntsw), nlines++;
    if (descsw)
	fprintf (fp, "\n%s: %s", DESCR_FIELD, descsw), nlines++;
    fprintf (fp, "\n%s: %s\n\n", ENCODING_FIELD, "base64"), nlines += 2;
    if (fflush (fp))
	adios (tmpfil, "error writing to");

    pos = ftell (fp);
    (void) writeBase64aux (stdin, fp);
    if (fflush (fp))
	adios (tmpfil, "error writing to");

    if (fstat (fileno (fp), &st) == NOTOK)
	adios ("failed", "fstat of %s", tmpfil);
    nlines += (((long) st.st_size - pos) + CPERLIN) / (CPERLIN + 1);
    nparts = (nlines + (LPERMSG - 1)) / LPERMSG;

    if (nparts <= 1)
	status = via_post (tmpfil, 0);
    else {
	int	partno;
	long	clock;
	char	buffer[BUFSIZ],
		msgid[BUFSIZ];

	if (verbosw) {
	    printf ("sending binary image as %d partial messages\n", nparts);
	    (void) fflush (stdout);
	}

	(void) time (&clock);
	(void) sprintf (msgid, "<%d.%ld@%s>", getpid (), clock, LocalName ());

	(void) fseek (fp, offset, 0);
	for (partno = 1; partno <= nparts; partno++) {
	    int	    lineno;
	    char    tmpdrf[BUFSIZ];
	    FILE   *out;

	    (void) strcpy (tmpdrf, m_tmpfil (invo_name));
	    if ((out = fopen (tmpdrf, "w")) == NULL)
		adios (tmpdrf, "unable to open for writing");
	    (void) chmod (tmpdrf, 0600);

	    fprintf (out, "To: %s\n", mailsw);
	    if (subjsw)
		fprintf (out, "Subject: %s\n", subjsw);
	    fprintf (out, "%s: %s\n", VRSN_FIELD, VRSN_VALUE);
	    fprintf (out,
		     "%s: message/partial; id=\"%s\";\n\tnumber=%d; total=%d\n",
		     TYPE_FIELD, msgid, partno, nparts);
	    fprintf (out, "%s: part %d of %d\n\n", DESCR_FIELD, partno,
		     nparts);

	    if (partno == 1)
		fprintf (out, "Message-ID: %s\n", msgid);

	    for (lineno = LPERMSG; lineno > 0; lineno--) {
		if (!fgets (buffer, sizeof buffer, fp)) {
		    if (partno == nparts)
			break;
		    adios (NULLCP, "premature eof");
		}

		(void) fputs (buffer, out);
	    }
	    offset = ftell (fp);

	    if (fflush (out))
		adios (tmpdrf, "error writing to");

	    (void) fclose (out);

	    status = via_post (tmpdrf, slowsw == 0);
	    (void) unlink (tmpdrf);
	    if (status)
		break;

	    if (slowsw > 0 && partno < nparts) {
		if (verbosw) {
		    printf ("pausing %d seconds before sending part %d...\n",
			    slowsw, partno + 1);
		    (void) fflush (stdout);
		}

		sleep ((unsigned) slowsw);
	    }
	}
    }

    (void) fclose (fp);
    (void) unlink (tmpfil);

    done (status ? 1 : 0);
}

/*  */

static int  via_post (file, queued)
char   *file;
int	queued;
{
    int	    child_id,
	    i;

    for (i = 0; (child_id = fork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
        case NOTOK:
	    adios ("fork", "unable to");
	    /* NOTREACHED */

	case OK:
	    (void) execlp (postproc, r1bindex (postproc, '/'), file,
			   queued ? "-queued" : NULLCP, NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror (postproc);
	    _exit (-1);
	    /* NOTREACHED */

	default:
	    return pidXwait (child_id, postproc);
    }
}

/*  */

void done (status)
int	status;
{
    register CT	   *ctp;

    if (ctp = cts)
	for (; *ctp; ctp++)
	    free_content (*ctp);
    if (free_ct)
	free_content (free_ct);
    if (free_file)
	(void) unlink (free_file);

    exit (status);
}


static int  pidcheck (status)
int	status;
{
    if ((status & 0xff00) == 0xff00 || (status & 0x007f) != SIGQUIT)
	return status;

    (void) unlink ("core");

    (void) fflush (stdout);

    (void) fflush (stderr);

    done (1);
    /* NOTREACHED */
}
