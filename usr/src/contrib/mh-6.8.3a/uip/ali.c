/* ali.c - the new ali */
#ifndef	lint
static char ident[] = "@(#)$Id: ali.c,v 1.9 1993/09/02 00:05:15 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/addrsbr.h"
#include "../h/aliasbr.h"
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif


#define	NVEC	50		/* maximum number of names */

/*  */

static struct swit switches[] = {
#define	ALIASW	0
    "alias aliasfile", 0,
#define	NALIASW	1
    "noalias", -7,

#define	LISTSW	2
    "list", 0,
#define	NLISTSW	3
    "nolist", 0,

#define	NORMSW	4
    "normalize", 0,
#define	NNORMSW	5
    "nonormalize", 0,

#define	USERSW	6
    "user", 0,
#define	NUSERSW	7
    "nouser", 0,

#define	HELPSW	8
    "help", 4,

    NULL, 0
};

/*  */

static	int     pos = 1;

extern struct aka  *akahead;

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int     i,
            vecp = 0,
	    inverted = 0,
            list = 0,
	    noalias = 0,
	    normalize = AD_NHST;
    char   *cp,
          **ap,
          **argp,
            buf[100],
           *vec[NVEC],
           *arguments[MAXARGS];
    struct aka *ak;

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
		    (void) sprintf (buf, "%s [switches] aliases ...",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case ALIASW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if ((i = alias (cp)) != AK_OK)
			adios (NULLCP, "aliasing error in %s - %s",
				cp, akerror (i));
		    continue;
		case NALIASW: 
		    noalias++;
		    continue;

		case LISTSW: 
		    list++;
		    continue;
		case NLISTSW: 
		    list = 0;
		    continue;

		case NORMSW: 
		    normalize = AD_HOST;
		    continue;
		case NNORMSW: 
		    normalize = AD_NHST;
		    continue;

		case USERSW: 
		    inverted++;
		    continue;
		case NUSERSW: 
		    inverted = 0;
		    continue;
	    }
	vec[vecp++] = cp;
    }

    if (!noalias) {
	if (cp = m_find ("Aliasfile")) { /* allow Aliasfile: profile entry */
	    char *dp = NULL;

	    for (ap = brkstring(dp = getcpy(cp), " ", "\n"); ap && *ap; ap++)
		if ((i = alias (*ap)) != AK_OK)
		    adios (NULLCP,
			    "aliasing error in %s - %s", *ap, akerror (i));
	    if (dp)
		free(dp);
	}
	(void) alias (AliasFile);
    }
	

/*  */

    if (vecp)
	for (i = 0; i < vecp; i++)
	    if (inverted)
		print_usr (vec[i], list, normalize);
	    else
		print_aka (akvalue (vec[i]), list, 0);
    else {
	if (inverted)
	    adios (NULLCP,
		    "usage: %s -user addresses ...  (you forgot the addresses)",
		invo_name);

	for (ak = akahead; ak; ak = ak -> ak_next) {
	    printf ("%s: ", ak -> ak_name);
	    pos += strlen (ak -> ak_name) + 1;
	    print_aka (akresult (ak), list, pos);
	}
    }

    done (0);
}

/*  */

print_aka (p, list, margin)
register char  *p;
int     list,
        margin;
{
    register char   c;

    if (p == NULL) {
	printf ("<empty>\n");
	return;
    }

    while (c = *p++)
	switch (c) {
	    case ',': 
		if (*p)
		    if (list)
			printf ("\n%*s", margin, "");
		    else
			if (pos >= 68) {
			    printf (",\n ");
			    pos = 2;
			}
			else {
			    printf (", ");
			    pos += 2;
			}

	    case 0: 
		break;

	    default: 
		pos++;
		(void) putchar (c);
	}

    (void) putchar ('\n');
    pos = 1;
}

/*  */

print_usr (s, list, norm)
register char  *s;
int     list,
        norm;
{
    register char  *cp,
                   *pp,
                   *vp;
    register struct aka *ak;
    register struct mailname   *mp,
                               *np;

    if ((pp = getname (s)) == NULL)
	adios (NULLCP, "no address in \"%s\"", s);
    if ((mp = getm (pp, NULLCP, 0, norm, NULLCP)) == NULL)
	adios (NULLCP, "bad address \"%s\"", s);
    while (getname (""))
	continue;

    vp = NULL;
    for (ak = akahead; ak; ak = ak -> ak_next) {
	pp = akresult (ak);
	while (cp = getname (pp)) {
	    if ((np = getm (cp, NULLCP, 0, norm, NULLCP)) == NULL)
		continue;
	    if (uleq (mp -> m_host, np -> m_host)
		    && uleq (mp -> m_mbox, np -> m_mbox)) {
		vp = vp ? add (ak -> ak_name, add (",", vp))
		    : getcpy (ak -> ak_name);
		mnfree (np);
		while (getname (""))
		    continue;
		break;
	    }
	    mnfree (np);
	}
    }
    mnfree (mp);

#ifdef	notdef
    printf ("%s: ", s);
    print_aka (vp ? vp : s, list, pos += strlen (s) + 1);
#else
    print_aka (vp ? vp : s, list, 0);
#endif
    if (vp)
	free (vp);
}
