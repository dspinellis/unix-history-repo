/* popauth.c - manipulate POP authorization DB */
#ifndef	lint
static char ident[] = "@(#)$Id: popauth.c,v 1.7 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "popauth.h"
#undef	DBM		/* used by mts.c and ndbm.h */
#include <ndbm.h>
#include <pwd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#ifdef	SYS5
#include <fcntl.h>
#endif
#include "../zotnet/bboards.h"
#include "../zotnet/mts.h"

/*  */

static struct swit switches[] = {
#define	INITSW	0
    "init", 0,
#define	LISTSW	1
    "list", 0,
#define	USERSW	2
    "user name", 0,

#define	HELPSW	3
    "help", 4,

    NULL, 0
};

/*  */

char   *getpass ();

/*  */

/* ARGSUSED */

main (argc, argv)
int	argc;
char   *argv[];
{
    int	    flags,
	    i,
	    initsw = 0,
	    insist,
	    listsw = 0;
    long    clock;
    char   *bp,
	   *cp,
	   *usersw = NULL,
	    buf[100],
	  **ap,
	  **argp,
	   *arguments[MAXARGS];
    datum   key,
	    value;
    DBM    *db;
    struct authinfo auth;

    invo_name = r1bindex (argv[0], '/');
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
		    (void) sprintf (buf, "%s [switches]", invo_name);
		    help (buf, switches);
		    done (1);

		case INITSW:
		    initsw = 1, listsw = 0;
		    continue;
		case LISTSW:
		    listsw = 1, initsw = 0;
		    continue;
		case USERSW:
		    if (!(usersw = *argp++) || *usersw == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
	    }
	adios (NULLCP, "usage: %s [switches]", invo_name);
    }

/*  */

#ifndef	APOP
    adios (NULLCP, "not compiled with APOP option");
#else
    if (getuid ())
	initsw = listsw = 0, usersw = NULL;

    if (initsw) {
	struct passwd *pw;
	struct stat st;

	if ((pw = getpwnam (POPUID)) == NULL)
	    adios (NULLCP, "POP user-id unknown");

	(void) sprintf (buf, "%s.dir", APOP);
	if (stat (buf, &st) != NOTOK) {
	    if (!getanswer ("Really initialize POP authorization DB? "))
		done (1);
	    (void) unlink (buf);
	    (void) sprintf (buf, "%s.pag", APOP);	    
	    (void) unlink (buf);
	}
	if ((db = dbm_open (APOP, O_RDWR | O_CREAT, 0600)) == NULL)
	    adios (APOP, "unable to create POP authorization DB");
	if (fchown (dbm_dirfno (db), pw -> pw_uid, pw -> pw_gid) == NOTOK
	        || fchown (dbm_pagfno (db), pw -> pw_uid, pw -> pw_gid)
	    		== NOTOK)
	    advise (" ", "error setting ownership of POP authorization DB");

	done (0);
    }

    if ((db = dbm_open (APOP, O_RDONLY, 0)) == NULL)
	adios (APOP, "unable to open POP authorization DB");

    if (flock (dbm_pagfno (db), LOCK_SH) == NOTOK)
	adios (APOP, "unable to lock POP authorization DB");

    if (listsw) {
	if (usersw) {
	    key.dsize = strlen (key.dptr = usersw) + 1;
	    value = dbm_fetch (db, key);
	    if (value.dptr == NULL)
		adios (NULLCP, "no such entry in POP authorization DB");
	    bcopy (value.dptr, (char *) &auth, sizeof auth);
	    printf ("%s\n", key.dptr);
	}
	else
	    for (key = dbm_firstkey (db); key.dptr; key = dbm_nextkey (db)) {
		printf ("%s", key.dptr);
		value = dbm_fetch (db, key);
		if (value.dptr == NULL)
		    printf (" - no information?!?\n");
		else {
		    bcopy (value.dptr, (char *) &auth, sizeof auth);
		    printf ("\n");
		}
	    }

	dbm_close (db);

	done (0);
    }

    if (usersw == NULL)
	usersw = getusr ();

    fprintf (stderr, "Changing POP password for %s.\n", usersw);

    key.dsize = strlen (key.dptr = usersw) + 1;
    value = dbm_fetch (db, key);
    if (value.dptr != NULL) {
	bcopy (value.dptr, (char *) &auth, sizeof auth);
	dbm_close (db);

	if ((i = strlen (strcpy (buf, getpass ("Old password:")))) == 0
	        || auth.auth_secretlen != i
	    	|| bcmp (buf, auth.auth_secret, i))
	    fprintf (stderr, "Sorry.\n"), exit (1);
    }
    else
	dbm_close (db);

#ifdef	lint
    flags = 0;
#endif	/* lint */
    for (insist = 0; insist < 2; insist++) {
	int	i;
	char    c;

	if (insist)
	    printf ("Please use %s.\n",
		    flags == 1 ? "at least one non-numeric character"
		    : "a longer password");

	if ((i = strlen (strcpy (buf, getpass ("New password:")))) == 0) {
	    fprintf (stderr, "Password unchanged.\n");
	    exit (1);
	}

	flags = 0;
	for (cp = buf; c = *cp++;)
	    if (c >= 'a' && c <= 'z')
		flags |= 2;
	    else
		if (c >= 'A' && c <= 'Z')
		    flags |= 4;
		else
		    if (c >= '0' && c <= '9')
			flags |= 1;
		    else
			flags |= 8;

	if ((flags >= 7 && i >= 4)
		|| ((flags == 2 || flags == 4) && i >= 6)
		|| ((flags == 3 || flags == 5 || flags == 6) && i >= 5))
	    break;
    }

    if (strcmp (buf, getpass ("Retype new password:"))) {
	fprintf (stderr, "Mismatch - password unchanged.\n");
	exit (1);
    }

    if ((db = dbm_open (APOP, O_RDWR, 0)) == NULL)
	adios (APOP, "unable to open POP authorization DB");

    if (flock (dbm_pagfno (db), LOCK_EX) == NOTOK)
	adios (APOP, "unable to lock POP authorization DB");

    key.dsize = strlen (key.dptr = usersw) + 1;

    buf[sizeof auth.auth_secret] = NULL;
    bcopy (buf, auth.auth_secret, auth.auth_secretlen = strlen (buf));
    value.dptr = (char *) &auth, value.dsize = sizeof auth;

    if (dbm_store (db, key, value, DBM_REPLACE))
	adios (NULLCP, "POP authorization DB may be corrupt?!?");
    dbm_close (db);
#endif

    done (0);
    /* NOTREACHED */
}
