/* dp.c  - parse dates 822-style */
#ifndef	lint
static char ident[] = "@(#)$Id: dp.c,v 1.7 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/formatsbr.h"
#include "../zotnet/tws.h"
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif


#define	NDATES	100

#define	WIDTH	78
#define	WBUFSIZ	BUFSIZ

#define	FORMAT	"%<(nodate{text})error: %{text}%|%(putstr(pretty{text}))%>"

/*  */

static struct swit switches[] = {
#define	FORMSW	0
    "form formatfile", 0,
#define	FMTSW	1
    "format string", 5,

#define	WIDSW	2
    "width columns", 0,

#define	HELPSW	3
    "help", 4,

    NULL, 0
};

/*  */

static struct format *fmt;

static int dat[5];

static int	process();
/*  */

/* ARGSUSED */

main (argc, argv)
int argc;
char **argv;
{
    int     datep = 0,
            width = 0,
            status = 0;
    char   *cp,
           *form = NULL,
           *format = NULL,
	   *nfs,
            buf[80],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *dates[NDATES];

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

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [switches] dates ...", invo_name);
		    help (buf, switches);
		    done (1);

		case FORMSW: 
		    if (!(form = *argp++) || *form == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    format = NULL;
		    continue;
		case FMTSW: 
		    if (!(format = *argp++) || *format == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    form = NULL;
		    continue;

		case WIDSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    width = atoi (cp);
		    continue;
	    }
	if (datep > NDATES)
	    adios (NULLCP, "more than %d dates", NDATES);
	else
	    dates[datep++] = cp;
    }
    dates[datep] = NULL;

/*  */

    if (datep == 0)
	adios (NULLCP, "usage: %s [switches] dates ...", invo_name);

    nfs = new_fs (form, format, FORMAT);
    if (width == 0) {
	if ((width = sc_width ()) < WIDTH / 2)
	    width = WIDTH / 2;
	width -= 2;
    }
    if (width > WBUFSIZ)
	width = WBUFSIZ;
    (void) fmt_compile (nfs, &fmt);
    dat[0] = dat[1] = dat[2] = dat[4] = 0;
    dat[3] = width;

    for (datep = 0; dates[datep]; datep++)
	status += process (dates[datep], width);

    m_update ();

    done (status);
}

/*  */

static	int process (date, length)
register char   *date;
int	length;
{
    int     status = 0;
    char    buffer[WBUFSIZ + 1];
    register struct comp   *cptr;

    FINDCOMP (cptr, "text");
    if (cptr)
	cptr -> c_text = date;
    (void) fmtscan (fmt, buffer, length, dat);
    (void) fputs (buffer, stdout);

    return status;
}
