/* conflict.c - the new conflict */
#ifndef	lint
static char ident[] = "@(#)$Id: conflict.c,v 2.9 1992/12/15 00:07:52 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/aliasbr.h"
#include "../h/local.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <grp.h>
#include <pwd.h>
#ifdef LOCALE
#include	<locale.h>
#endif

#define	NDIRS	100
#define	NGRPS	100

/*  */

static struct swit switches[] = {
#define	MAILSW	0
    "mail name", 0,

#define	SERCHSW	1
    "search directory", 0,

#define	HELPSW	2
    "help", 4,

    NULL, 0
};

/*  */

static	char   *mail = NULL;

static	char   *dirs[NDIRS];

static	FILE * out = NULL;


extern struct aka  *akahead;
extern struct home *homehead;


#if !defined(__STDC__) && !defined(__stdc__)	/* __stdc__ for convex */
struct	group *getgrent (), *getgrgid();
#endif /* !__STDC__ */

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int	    akp = 0,
            dp = 0;
    char   *cp,
          **argp = argv + 1,
            buf[80],
           *akv[50];

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    m_foil (NULLCP);
    mts_init (invo_name);

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
		    (void) sprintf (buf, "%s [switches] [aliasfiles ...]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case MAILSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if (mail)
			adios (NULLCP, "mail to one address only");
		    else
			mail = cp;
		    continue;

		case SERCHSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if (dp >= NDIRS)
			adios (NULLCP, "more than %d directories", NDIRS);
		    dirs[dp++] = cp;
		    continue;
	    }
	akv[akp++] = cp;
    }

/*  */

    if (akp == 0)
	akv[akp++] = AliasFile;
    if (!homehead)
	init_pw ();
    if (!mail)
	out = stdout;
    dirs[dp] = NULL;

    alias_files (akp, akv);
    pwd_names ();
    grp_names ();
    grp_members ();
    grp_ids ();
#ifdef	UCI
    ldr_names ();
    ldr_ship ();
#endif	/* UCI */
    maildrops ();

    done (0);
}

/*  */

alias_files (akp, akv)
int     akp;
register char   **akv;
{
    register int    i, err;

    for (i = 0; i < akp; i++)
	if ((err = alias (akv[i])) != AK_OK) {
	    setup ();
	    fprintf (out, "aliasing error in %s - %s\n", akv[i], akerror (err));
	}
	else
	    if (out && !mail)
		fprintf (out, "alias file %s is ok\n", akv[i]);
}

/*  */

pwd_names () {
    int     hit = 0;
    register struct home   *hm,
                           *lm;

    for (hm = homehead; hm; hm = hm -> h_next)
	for (lm = hm -> h_next; lm; lm = lm -> h_next)
	    if (strcmp (hm -> h_name, lm -> h_name) == 0) {
		setup ();
		fprintf (out, "duplicate user %s(uid=%d)\n",
			lm -> h_name, lm -> h_uid);
		hit++;
	    }

    if (!hit && out && !mail)
	fprintf (out, "no duplicate users\n");
}


grp_names () {
    register int    gp,
                    hit = 0;
    char   *grps[NGRPS];
    register struct group  *gr;

    grps[0] = NULL;
    (void) setgrent ();
    while (gr = getgrent ()) {
	for (gp = 0; grps[gp]; gp++)
	    if (strcmp (grps[gp], gr -> gr_name) == 0) {
		setup ();
		fprintf (out, "duplicate group %s(gid=%d)\n",
			gr -> gr_name, gr -> gr_gid);
		hit++;
		break;
	    }
	if (grps[gp] == NULL)
	    if (gp < NGRPS) {
		grps[gp++] = getcpy (gr -> gr_name);
		grps[gp] = NULL;
	    }
	    else {
		setup ();
		fprintf (out, "more than %d groups (time to recompile)\n",
			NGRPS - 1);
		hit++;
	    }
    }
    (void) endgrent ();

    for (gp = 0; grps[gp]; gp++)
	free (grps[gp]);

    if (!hit && out && !mail)
	fprintf (out, "no duplicate groups\n");
}

/*  */

grp_members () {
    register int    hit = 0;
    register char **cp,
                  **dp;
    register struct group  *gr;
    register struct home   *hm;

    (void) setgrent ();
    while (gr = getgrent ())
	for (cp = gr -> gr_mem; *cp; cp++) {
	    for (hm = homehead; hm; hm = hm -> h_next)
		if (!strcmp (*cp, hm -> h_name))
		    break;
	    if (hm == NULL) {
		setup ();
		fprintf (out, "group %s(gid=%d) has unknown member %s\n",
			gr -> gr_name, gr -> gr_gid, *cp);
		hit++;
	    }
#ifdef	BSD42
	    else
		hm -> h_ngrps++;
#endif	/* BSD42 */

	    for (dp = cp + 1; *dp; dp++)
		if (strcmp (*cp, *dp) == 0) {
		    setup ();
		    fprintf (out, "group %s(gid=%d) has duplicate member %s\n",
			    gr -> gr_name, gr -> gr_gid, *cp);
		    hit++;
		}
	}
    (void) endgrent ();

#ifdef	BSD42
    for (hm = homehead; hm; hm = hm -> h_next)
	if (hm -> h_ngrps > NGROUPS) {
	    setup ();
	    fprintf (out, "user %s is a member of %d groups (max %d)",
		    hm -> h_name, hm -> h_ngrps, NGROUPS);
	    hit++;
	}
#endif	/* BSD42 */

    if (!hit && out && !mail)
	fprintf (out, "all group members accounted for\n");
}


grp_ids () {		/* -DRAND not implemented at most places */
    register int    hit = 0;
    register struct home   *hm;

    for (hm = homehead; hm; hm = hm -> h_next)
	if (getgrgid (hm -> h_gid) == NULL) {
	    setup ();
	    fprintf (out, "user %s(uid=%d) has unknown group-id %d\n",
		    hm -> h_name, hm -> h_uid, hm -> h_gid);
	    hit++;
	}

    if (!hit && out && !mail)
	fprintf (out, "all group-id users accounted for\n");
}

/*  */

maildrops () 
{
    register int    i;

    if (mmdfldir && *mmdfldir)
	mdrop (mmdfldir);
    if (uucpldir && *uucpldir)
	mdrop (uucpldir);
    for (i = 0; dirs[i]; i++)
	mdrop (dirs[i]);
}


mdrop(drop)
register char *drop;
{
    register int    hit = 0;
#ifdef SYS5DIR
    register struct dirent *dp;
#else	/*  SYS5DIR */
    register struct direct *dp;
#endif	/* SYS5DIR */
    register DIR *dd = opendir (drop);

    if (!dd) {
	setup ();
	fprintf (out, "unable to open maildrop area %s\n", drop);
	return;
    }

    while (dp = readdir (dd))
	if (dp -> d_name[0] != '.' && !check (dp ->d_name)) {
	    setup ();
	    fprintf (out,
		    "there is a maildrop for the unknown user %s in %s\n",
		    dp -> d_name, drop);
	    hit++;
	}

    closedir (dd);
    if (!hit && out && !mail)
	fprintf (out, "all maildrops accounted for in %s\n", drop);
}


/*  */

int     check (s)
register char   *s;
{
    register struct home *hm;

    for (hm = homehead; hm; hm = hm -> h_next)
	if (!strcmp (s, hm -> h_name))
	    return 1;
    return 0;
}

/*  */

setup () {
    int     fd,
            pd[2];

    if (out)
	return;

    if (mail) {
	if (pipe (pd) == NOTOK)
	    adios ("pipe", "unable to");

	switch (fork ()) {
	    case NOTOK: 
		adios ("fork", "unable to");

	    case OK: 
		(void) close (pd[1]);
		if (pd[0] != 0) {
		    (void) dup2 (pd[0], 0);
		    (void) close (pd[0]);
		}
		if ((fd = open ("/dev/null", 1)) != NOTOK)
		    if (fd != 1) {
			(void) dup2 (fd, 1);
			(void) close (fd);
		    }
		execlp (mailproc, r1bindex (mailproc, '/'),
			mail, "-subject", invo_name, NULLCP);
		adios (mailproc, "unable to exec ");

	    default: 
		(void) close (pd[0]);
		out = fdopen (pd[1], "w");
		fprintf (out, "%s: the following is suspicious\n\n",
			invo_name);
	}
    }
}

/*  */

#ifdef	UCI
/* UCI specific stuff for conflict */

/* taken from <grpldr.h> */

#define	GLDRS	"/admin/etc/GroupLeaders"

struct grpldr {
    char *gl_name;
    char **gl_ldr;
};

int	setglent (), endglent ();
struct grpldr *getglent (), *getglnam ();


/* taken from the getglent() routines */

#include <ctype.h>

#define	MAXGLS	100


static FILE *glp = NULL;
static char line[BUFSIZ+1];
static struct grpldr grpldr;
static char *gl_ldr[MAXGLS + 1];

/*  */

setglent() {
    if (glp == NULL)
	glp = fopen (GLDRS, "r");
    else
	rewind (glp);

    return (glp != NULL);
}


endglent() {
    if (glp != NULL) {
	(void) fclose (glp);
	glp = NULL;
    }

    return 1;
}

/*  */

struct grpldr  *getglent () {
    register char  *cp,
                  **q;

    if (glp == NULL && !setglent ())
	return NULL;
    if ((cp = fgets (line, BUFSIZ, glp)) == NULL)
	return NULL;

    grpldr.gl_name = cp;
    grpldr.gl_ldr = q = gl_ldr;

    while (*cp) {
	while (*cp && !isspace (*cp))
	    cp++;
	while (*cp && isspace (*cp))
	    *cp++ = '\0';
	if (*cp == '\0')
	    break;
	if (q < gl_ldr + MAXGLS)
	    *q++ = cp;
	else
	    break;
    }
    *q = NULL;

    return (&grpldr);
}

/*  */

struct grpldr  *getglnam (name)
char   *name;
{
    register struct grpldr  *gl = NULL;

    (void) setglent ();
    while (gl = getglent ())
	if (strcmp (name, gl -> gl_name) == 0)
	    break;
    (void) endglent ();

    return gl;
}

/*  */

ldr_names () {
    register int     gp,
		     hit = 0;
    char   *gldrs[NGRPS];
    register struct grpldr  *gl;

    gldrs[0] = NULL;
    (void) setglent ();
    while (gl = getglent ()) {
	if (getgrnam (gl -> gl_name) == NULL) {
	    setup ();
	    fprintf (out, "unknown group %s in group leaders file\n",
		    gl -> gl_name);
	    hit++;
	}
	for (gp = 0; gldrs[gp]; gp++)
	    if (strcmp (gldrs[gp], gl -> gl_name) == 0) {
		setup ();
		fprintf (out, "duplicate group %s in group leaders file\n",
			gl -> gl_name);
		hit++;
		break;
	    }
	if (gldrs[gp] == NULL)
	    if (gp < NGRPS) {
		gldrs[gp++] = getcpy (gl -> gl_name);
		gldrs[gp] = NULL;
	    }
	    else {
		setup ();
		fprintf (out, "more than %d groups in group leaders file%s\n",
			" (time to recompile)", NGRPS - 1);
		hit++;
	    }
    }
    (void) endglent ();

    for (gp = 0; gldrs[gp]; gp++)
	free (gldrs[gp]);

    if (!hit && out && !mail)
	fprintf (out, "all groups in group leaders file accounted for\n");
}


ldr_ship () {
    register int     hit = 0;
    register char  **cp,
		   **dp;
    register struct grpldr  *gl;

    (void) setglent ();
    while (gl = getglent ())
	for (cp = gl -> gl_ldr; *cp; cp++) {
	    if (!check (*cp)) {
		setup ();
		fprintf (out, "group %s has unknown leader %s\n",
			gl -> gl_name, *cp);
		hit++;
	    }

	    for (dp = cp + 1; *dp; dp++)
		if (strcmp (*cp, *dp) == 0) {
		    setup ();
		    fprintf (out, "group %s had duplicate leader %s\n",
			    gl -> gl_name, *cp);
		    hit++;
		}
	}
    (void) endglent ();

    if (!hit && out && !mail)
	fprintf (out, "all group leaders accounted for\n");
}
#endif	/* UCI */
