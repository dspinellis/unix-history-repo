/* install-mh.c - initialize the MH environment */
#ifndef	lint
static char ident[] = "@(#)$Id: install-mh.c,v 1.8 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <pwd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static char *message[] = {
    "Prior to using MH, it is necessary to have a file in your login",
    "directory (%s) named %s which contains information",
    "to direct certain MH operations.  The only item which is required",
    "is the path to use for all MH folder operations.  The suggested MH",
    "path for you is %s/Mail...",
    NULL
};


static char   *geta ();

#ifndef __STDC__
#ifdef	SYS5
struct passwd  *getpwuid ();
#endif
#endif /* !__STDC__ */

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    int     autof,
	    i;
    char   *cp,
           *path;
    struct node *np;
    struct passwd *pw;
    struct stat st;
    FILE   *in,
	   *out;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');

#ifdef	COMPAT
    if (argc == 2 && strcmp (argv[1], "-compat") == 0) {
	context = "/dev/null";	/* hack past m_getdefs() */
	
	m_getdefs ();
	for (np = m_defs; np; np = np -> n_next)
	    if (uleq (pfolder, np -> n_name)
		    || ssequal ("atr-", np -> n_name)
		    || ssequal ("cur-", np -> n_name))
		np -> n_context = 1;

	ctxpath = getcpy (m_maildir (context = "context"));
	ctxflags |= CTXMOD;
	m_update ();

	if ((out = fopen (defpath, "w")) == NULL)
	    adios (defpath, "unable to write");
	for (np = m_defs; np; np = np -> n_next)
	    if (!np -> n_context)
		fprintf (out, "%s: %s\n", np -> n_name, np -> n_field);
	(void) fclose (out);

	done (0);
    }
#endif	/* COMPAT */

    autof = (argc == 2 && strcmp (argv[1], "-auto") == 0);
    if (mypath == NULL) {	/* straight from m_getdefs... */
	if (mypath = getenv ("HOME"))
	    mypath = getcpy (mypath);
	else
	    if ((pw = getpwuid (getuid ())) == NULL
		    || pw -> pw_dir == NULL
		    || *pw -> pw_dir == 0)
		adios (NULLCP, "no HOME envariable");
	    else
		mypath = getcpy (pw -> pw_dir);
	if ((cp = mypath + strlen (mypath) - 1) > mypath && *cp == '/')
	    *cp = 0;
    }
    defpath = concat (mypath, "/", mh_profile, NULLCP);

    if (stat (defpath, &st) != NOTOK)
	if (autof)
	    adios (NULLCP, "invocation error");
	else
	    adios (NULLCP,
		    "You already have an MH profile, use an editor to modify it");

    if (!autof && gans ("Do you want help? ", anoyes)) {
	(void) putchar ('\n');
	for (i = 0; message[i]; i++) {
	    printf (message[i], mypath, mh_profile);
	    (void) putchar ('\n');
	}
	(void) putchar ('\n');
    }

/*  */

    cp = concat (mypath, "/", "Mail", NULLCP);
    if (stat (cp, &st) != NOTOK) {
	if ((st.st_mode & S_IFMT) == S_IFDIR) {
	    cp = concat ("You already have the standard MH directory \"",
		    cp, "\".\nDo you want to use it for MH? ", NULLCP);
	    if (gans (cp, anoyes))
		path = "Mail";
	    else
		goto query;
	}
	else
	    goto query;
    }
    else {
	if (autof)
	    printf ("I'm going to create the standard MH path for you.\n");
	else
	    cp = concat ("Do you want the standard MH path \"",
		    mypath, "/", "Mail\"? ", NULLCP);
	if (autof || gans (cp, anoyes))
	    path = "Mail";
	else {
    query:  ;
	    if (gans ("Do you want a path below your login directory? ",
			anoyes)) {
		printf ("What is the path?  %s/", mypath);
		path = geta ();
	    }
	    else {
		printf ("What is the whole path?  /");
		path = concat ("/", geta (), NULLCP);
	    }
	}
    }

    (void) chdir (mypath);
    if (chdir (path) == NOTOK) {
	cp = concat ("\"", path, "\" doesn't exist; Create it? ", NULLCP);
	if (autof || gans (cp, anoyes))
	    if (makedir (path) == 0)
		adios (NULLCP, "unable to create %s", path);
    }
    else
	printf ("[Using existing directory]\n");

/*  */

    np = m_defs = (struct node *) malloc (sizeof *np);
    if (np == NULL)
	adios (NULLCP, "unable to allocate profile storage");
    np -> n_name = getcpy ("Path");
    np -> n_field = getcpy (path);
    np -> n_context = 0;
    np -> n_next = NULL;

    if (in = fopen (mh_defaults, "r")) {
	m_readefs (&np -> n_next, in, mh_defaults, 0);
	(void) fclose (in);
    }

    ctxpath = getcpy (m_maildir (context = "context"));
    m_replace (pfolder, defalt);
    m_update ();

    if ((out = fopen (defpath, "w")) == NULL)
	adios (defpath, "unable to write");
    for (np = m_defs; np; np = np -> n_next)
	if (!np -> n_context)
	    fprintf (out, "%s: %s\n", np -> n_name, np -> n_field);
    (void) fclose (out);

    done (0);
}

/*  */

static char *geta () {
    register char  *cp;
    static char line[BUFSIZ];

    (void) fflush (stdout);
    if (fgets (line, sizeof line, stdin) == NULL)
	done (1);
    if (cp = index (line, '\n'))
	*cp = 0;
    return line;
}
