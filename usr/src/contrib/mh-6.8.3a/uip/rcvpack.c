/* rcvpack.c - a rcvmail program to keep a copy */
#ifndef	lint
static char ident[] = "@(#)$Id: rcvpack.c,v 1.5 1993/08/25 17:27:19 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/dropsbr.h"
#include "../h/rcvmail.h"
#include "../zotnet/tws.h"
#include "../zotnet/mts.h"
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	HELPSW	0
    "help", 4,

    NULL, 0
};

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    int     md;
    char   *cp,
           *file = NULL,
            buf[100],
	    ddate[BUFSIZ],
          **ap,
          **argp,
           *arguments[MAXARGS];

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    mts_init (invo_name);
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
		    (void) sprintf (buf, "%s [switches] file", invo_name);
		    help (buf, switches);
		    done (1);
	    }
	if (file)
	    adios (NULLCP, "only one file at a time!");
	else
	    file = cp;
    }

/*  */

    if (!file)
	adios (NULLCP, "%s [switches] file", invo_name);

    (void) sprintf (ddate, "Delivery-Date: %s\n", dtimenow ());
    rewind (stdin);
    if ((md = mbx_open (file, getuid (), getgid (), m_gmprot ())) == NOTOK
	    || mbx_copy (file, md, fileno (stdin), 1, ddate, 0) == NOTOK
	    || mbx_close (file, md) == NOTOK) {
	if (md != NOTOK)
	    (void) mbx_close (file, md);
	done (RCV_MBX);
    }

    done (RCV_MOK);
}
