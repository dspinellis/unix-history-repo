/* dsaconfig.c - build a database directory for a Level-1 DSA */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/tools/dsaconfig/RCS/dsaconfig.c,v 7.10 91/03/09 11:54:38 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/others/quipu/tools/dsaconfig/RCS/dsaconfig.c,v 7.10 91/03/09 11:54:38 mrose Exp $
 *
 *
 * $Log:	dsaconfig.c,v $
 * Revision 7.10  91/03/09  11:54:38  mrose
 * update
 * 
 * Revision 7.9  91/02/22  09:29:42  mrose
 * Interim 6.8
 * 
 * Revision 7.8  91/02/14  14:13:45  mrose
 * toption
 * 
 * Revision 7.7  91/01/11  15:37:51  mrose
 * acl
 * 
 * Revision 7.6  90/12/11  10:55:08  mrose
 * lock-and-load
 * 
 * Revision 7.5  90/10/18  11:33:37  mrose
 * psi
 * 
 * Revision 7.4  90/07/27  08:44:58  mrose
 * update
 * 
 * Revision 7.3  90/03/22  08:38:31  mrose
 * touch-up
 * 
 * Revision 7.2  90/01/11  18:36:09  mrose
 * real-sync
 * 
 * Revision 7.1  89/12/06  17:30:24  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:02:38  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <ctype.h>
#include <errno.h>
#include <grp.h>
#include <pwd.h>
#include <stdio.h>
#include <varargs.h>
#include "general.h"
#include "manifest.h"
#include "internet.h"
#include "psap.h"
#include "tailor.h"


#ifdef	SYS5
struct group *getgrnam ();
struct passwd *getpwnam (), *getpwuid ();
#endif

/*    DATA */

static int debug =0;

static int uid = 0;
static int gid = 0;

static char *myname = "dsaconfig";

static char *wildlife = NULL;
static char sedfil[BUFSIZ];


void	adios (), advise ();
char   *version ();


extern int errno;

extern char  *quipuversion;

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    char    buffer[BUFSIZ];

    arginit (argv);
    if (access (wildlife, 0x00) != NOTOK)
	adios (NULLCP, "%s already exists, choose a different name", wildlife);

    read_config ();
    generate_sed ();

    build_root ();
    build_TLC ();
    build_organization ();
    build_unit ();

    build_tailor ();
    build_startup ();
    build_nightly ();

    build_dsap ();
    build_fred ();

#ifndef	SYS5
    (void) sprintf (buffer, "find %s -exec /etc/chown %d {} \\;",
		    wildlife, uid);
#else
    (void) sprintf (buffer, "find %s -exec chown %d {} \\;", wildlife, uid);
#endif
    if (debug)
	fprintf (stderr, "%s\n", buffer);
    (void) system (buffer);

    (void) sprintf (buffer, "find %s -exec chgrp %d {} \\;",
		    wildlife, gid);
    if (debug)
	fprintf (stderr, "%s\n", buffer);
    (void) system (buffer);

    (void) unlink (sedfil);

    exit (0);
}

/*    CONFIG */

struct country {
    char   *c_code;
    int	    c_number;
    char   *c_name;

    char   *c_root;
    char   *c_master;
    char   *c_other;
    
    char   *c_phone;

    int	    c_flags;
#define	C_SHORT	0x01
};

struct country *read_country ();

/*  */

struct pair {
    char   *p_name;
    char   *p_value;

    int	    p_flags;
#define	P_NULL	0x00
#define	P_OPT	0x01
#define	P_MBOX	0x02
#define	P_XXX	0x04
#define	P_ZAP	0x08
#define	P_POST	0x10
};

static struct pair pairs[] = {
    "dsa", NULL,			/* Spectacled Bear */
	P_NULL,

    "country", NULL,			/* US */
        P_OPT,
    "organization", NULL,		/* Performance Systems International */
	P_NULL,
    "domain", NULL,			/* psi.com */
        P_NULL,
    "unit", NULL,			/* Development */
	P_NULL,
    "street", NULL,			/* 11800 Sunrise Valley Drive */
	P_OPT,
    "pob", NULL,			/* 1234 */
	P_OPT,
    "town", NULL,			/* Reston */
	P_NULL,
    "state", NULL,			/* Virginia */
	P_NULL,
    "zipcode", NULL,			/* 22091 */
	P_NULL,
    "locality", NULL,			/* Reston, VA */
        P_OPT,
    "postaladdress", NULL,		/* org $ address */
	P_OPT | P_POST,

    "telephone", NULL,			/* +1 703-620-6651 */
	P_NULL,
    "fax", NULL,			/* +1 703-620-4586 */
	P_OPT,

    "description", NULL,		/* value-added ... */
	P_NULL,

    "ipaddr", NULL,			/* 127.0.0.1 */
        P_OPT,
    "port", NULL,			/* 17003 */
        P_OPT,

    "firstname", NULL,			/* Wengyik */
	P_NULL,
    "lastname", NULL,			/* Yeong */
	P_NULL,
    "middleinitial", NULL,		/* */
	P_OPT,
    "middlename", NULL,			/* */
	P_OPT,
    "mailbox", NULL,			/* yeong@psi.com */
	P_MBOX,
    "title", NULL,			/* Staff Scientist */
	P_NULL,
    "userid", NULL,			/* yeongw */
	P_OPT,
    "groupid", NULL,			/* whitepages */
	P_OPT,
    "password", NULL,			/* secret */
	P_NULL,
    "extension", NULL,			/* x1234 */
	P_OPT,

    "wildlife", NULL,			/* spectacled-bear */
       P_XXX,
    "bindir", NULL,			/* /usr/local/bin/ */
        P_XXX,
    "sbindir", NULL,			/* /usr/etc/ */
        P_XXX,
    "etcdir", NULL,			/* /usr/etc/ */
        P_XXX,
    "quipuversion", NULL,		/* from -lquipu */
        P_XXX,
    "rootDSA", NULL,			/* root upstream */
        P_XXX,
    "countryDSA", NULL,			/* country upstream */
        P_XXX,
    "otherDSA", NULL,			/* hack... */
	P_XXX,
    "rootDSAaddress", NULL,		/* PSAP of root upstream */
        P_XXX,
    "countryDSAaddress", NULL,		/* PSAP of country upstream */
        P_XXX,
    "otherDSAaddress", NULL,		/* PSAP of hack... */
	P_XXX,

    NULL
};

struct pair *n2p ();

/*  */

static read_config () {
    int	    lineno;
    register char *cp,
		  *dp;
    char    buffer[BUFSIZ],
	    file[BUFSIZ],
	    line[BUFSIZ],
	   *vec[NVEC + 1];
    FILE   *fp;
    register struct country *c;
    register struct pair *p,
			 *q;
    register struct hostent *hp;
    struct sockaddr_in in_socket;
    register struct sockaddr_in *isock = &in_socket;

    (void) sprintf (file, "%s.dsa", wildlife);
    if ((fp = fopen (file, "r")) == NULL)
	adios (file, "unable to read");

    for (lineno = 1; fgets (buffer, sizeof buffer, fp); lineno++) {
	if (*buffer == '#')
	    continue;
	if (cp = index (buffer, '\n'))
	    *cp = NULL;
	(void) strcpy (line, buffer);

	bzero ((char *) vec, sizeof vec);
	switch (str2vec (buffer, vec)) {
	    case 0:
	        continue;

	    case 1:
		for (p = pairs; p -> p_name; p++)
		    if (strcmp (p -> p_name, vec[0]) == 0)
			break;
		if (!p -> p_name || (p -> p_flags & P_XXX))
		    adios (NULLCP, "unknown variable \"%s\"", vec[0]);
	        continue;

	    case 2:
		break;

	    default:
		adios (NULLCP, "syntax error on line %d:\n%s", lineno, line);
		/* NOTREACHED */
	}

	for (p = pairs; p -> p_name; p++)
	    if (strcmp (p -> p_name, vec[0]) == 0)
		break;
	if (!p -> p_name || (p -> p_flags & P_XXX))
	    adios (NULLCP, "unknown variable \"%s\"", vec[0]);
	if (p -> p_value)
	    adios (NULLCP, "multiple values for \"%s\" starting at line %d",
		   p -> p_name, lineno);

	if (p -> p_flags & P_MBOX) {
	    for (cp = vec[1]; *cp; cp++)
		if (!isascii (*cp) || *cp == '$' || *cp == '&') {
illegal: ;
		    adios (NULLCP,
			   "illegal character %c (0%o) in value for \"%s\"",
			   *cp, *cp & 0xff, p -> p_name);
		}
	}
	else
	    for (cp = vec[1]; *cp; cp++) {
		if (isalpha (*cp) || isdigit (*cp))
		    continue;
		switch (*cp) {
		    case 047: /* ' */
		    case '(':
		    case ')':
		    case '+':
		    case ',':
		    case '-':
		    case '.':
		    case '/':
		    case ':':
		    case '?':
		    case ' ':
			continue;

		    default:
			if ((p -> p_flags & P_POST) && *cp == '$')
			    continue;
			goto illegal;
		}
	    }

	p -> p_value = strdup (vec[1]);
    }

    if (ferror (fp) && !feof (fp))
	adios (file, "error reading");
    (void) fclose (fp);

    for (p = pairs; p -> p_name; p++)
	if (!p -> p_value && !(p -> p_flags & (P_OPT | P_XXX)))
	    adios (NULLCP, "missing value for \"%s\"", p -> p_name);

    if (n2p ("country", 0) == NULL)
	n2p ("country", 1) -> p_value = strdup ("US");

    if ((c = read_country (cp = n2p ("country", 1) -> p_value)) == NULL)
	adios (NULLCP, "unknown country code \"%s\"", cp);
    n2p ("rootDSA", 1) -> p_value =
			    strdup (c -> c_root ? c -> c_root : c -> c_master);
    if (c -> c_root)
	read_psap (c -> c_root, &(n2p ("rootDSAaddress", 1) -> p_value));
    n2p ("countryDSA", 1) -> p_value = strdup (c -> c_master);
    read_psap (c -> c_master, &(n2p ("countryDSAaddress", 1) -> p_value));
    if (c -> c_other) {
	n2p ("otherDSA", 1) -> p_value = strdup (c -> c_other);
	read_psap (c -> c_other, &(n2p ("otherDSAaddress", 1) -> p_value));
    }

    if (n2p ("postaladdress", 0) == NULL
	    && n2p ("street", 0) == NULL
	    && n2p ("pob", 0) == NULL)
	adios (NULLCP, "must specify either \"street\" or \"pob\"");

    if ((p = n2p ("locality", 1)) -> p_value == NULL) {
	(void) sprintf (buffer, "%s, %s", n2p ("town", 1) -> p_value,
			n2p ("state", 1) -> p_value);

	p -> p_value = strdup (buffer);

	if (debug)
	    printf ("setting locality to \"%s\"\n", p -> p_value);
    }
    
    if (*(p = n2p ("telephone", 1)) -> p_value != '+') {
	(void) sprintf (buffer, "+%s %s", c -> c_phone, p -> p_value);
	free (p -> p_value);
	p -> p_value = strdup (buffer);

	if (debug)
	    printf ("setting telephone number to \"%s\"\n", p -> p_value);
    }

    if ((p = n2p ("fax", 0)) && *p -> p_value != '+') {
	(void) sprintf (buffer, "+%s %s", c -> c_phone, p -> p_value);
	free (p -> p_value);
	p -> p_value = strdup (buffer);

	if (debug)
	    printf ("setting fax number to \"%s\"\n", p -> p_value);
    }

    if ((p = n2p ("ipaddr", 1)) -> p_value == NULL) {
	if ((hp = gethostbyname (cp = getlocalhost ())) == NULL)
	    adios (NULLCP, "%s: unknown host", cp);
    }
    else {
	if ((hp = gethostbystring (p -> p_value)) == NULL)
	    adios (NULLCP, "%s: unknown host", p -> p_value);
    }
    bzero ((char *) isock, sizeof *isock);
    isock -> sin_family = hp -> h_addrtype;
    inaddr_copy (hp, isock);
    p -> p_value = strdup (inet_ntoa (isock -> sin_addr));
    if (debug)
	printf ("setting IP address of DSA to \"%s\"\n", p -> p_value);

    if ((p = n2p ("port", 1)) -> p_value == NULL) {
	p -> p_value = strdup ("17003");

	if (debug)
	    printf ("setting TCP port of DSA to \"%s\"\n", p -> p_value);
    }

    if ((p = n2p ("middlename", 1)) -> p_value
	    && (q = n2p ("middleinitial", 1)) -> p_value == NULL) {
	(void) sprintf (buffer, "%c", *p -> p_value);
	p -> p_value = strdup (buffer);

	if (debug)
	    printf ("setting MiddleInitial to \"%s\"\n", p -> p_value);
    }

    uid = getuid (), gid = getgid ();
    if ((p = n2p ("userid", 1)) -> p_value) {
	register struct passwd *pw = getpwnam (p -> p_value);

	if (pw == NULL)
	    adios (NULLCP, "unknown user name \"%s\"", p -> p_value);
	uid = pw -> pw_uid, gid = pw -> pw_gid;
    }
    else {
	register struct passwd *pw = getpwuid (uid);

	if (pw == NULL)
	    adios (NULLCP, "unknown user name \"%s\"", p -> p_value);
	p -> p_value = strdup (pw -> pw_name);

	if (debug)
	    printf ("setting userid to \"%s\"\n", p -> p_value);
    }

    if (p = n2p ("groupid", 0)) {
	register struct group *gr = getgrnam (p -> p_value);

	if (gr == NULL)
	    adios (NULLCP, "unknown group name \"%s\"", p -> p_value);
	gid = gr -> gr_gid;
    }

    p = n2p ("extension", 1), q = n2p ("telephone", 1);
    if (cp = p -> p_value) {
	if (*cp == 'X' || *cp == 'x') {
	    (void) sprintf (buffer, "%s x%s", q -> p_value, p -> p_value);
	    free (p -> p_value);
	    p -> p_value = strdup (buffer);

	    if (debug)
		printf ("setting user's telephone number to \"%s\"\n",
			p -> p_value);
	}
    }
    else {
	p -> p_value = strdup (q -> p_value);

	if (debug)
	    printf ("setting user telephone number to \"%s\"\n", p -> p_value);
    }

    n2p ("wildlife", 1) -> p_value = strdup (wildlife);
    n2p ("bindir", 1) -> p_value = strdup (isodebinpath);
    n2p ("sbindir", 1) -> p_value = strdup (isodesbinpath);
    n2p ("etcdir", 1) -> p_value = strdup (isodetcpath);
    n2p ("quipuversion", 1) -> p_value = strdup (quipuversion);

    if (dp = (p = n2p ("postaladdress", 1)) -> p_value) {
	int	i;

	for (i = 1; cp = index (dp, '$'); dp = cp + 1, i++) {
	    *cp = NULL;
	    if (strlen (dp) > 30)
		goto too_long;
	    *cp = '$';
	}
	if (strlen (dp) > 30) {
too_long: ;
	    adios (NULLCP,
		   "item %d is too long (30 characters maximum): \"%s\"", i,
		   dp);
	}
	if (i > 6)
	    adios (NULLCP, "too many items (%d) in postalAddress, 6 maximum",
		   i);
    }
    else {
	int	i;

	cp = buffer;

	(void) sprintf (cp, "%s $ ", n2p ("organization", 1) -> p_value);
	if ((i = strlen (cp)) > 30 + 3) {
	    advise (NULLCP,
		    "your organization name is longer than 30 characters!");
postal_problem: ;
	    adios (NULLCP,
          "You must explicitly define the postalAddress attribute in the\n\
           configuration file for your Level-1 DSA.\n\
\n\
           The format is:\n\
\n\
               postaladdress \"item1 $ item2 $ ... $ itemN\"\n\
\n\
           where each item is <= 30 characters in length and there are no\n\
           more than six items.  Refer to the Administrator's Guide for more\n\
           information.\n\
");
	}
	cp += i;

	if (q = n2p ("pob", 0)) {
	    (void) sprintf (cp, "POB %s $ ", q -> p_value);
	    if ((i = strlen (cp)) > 30 + 3) {
		advise (NULLCP, "your POB is longer than 26 characters!");
		goto postal_problem;
	    }
	    cp += i;
	}
	else
	    if (q = n2p ("street", 0)) {
		(void) sprintf (cp, "%s $ ", q -> p_value);
		if ((i = strlen (cp)) > 30 + 3) {
		    advise (NULLCP,
			"your street address is longer than 30 characters!");
		    goto postal_problem;
		}
		cp += i;

		q -> p_flags |= P_ZAP;
	    }

	(void) sprintf (cp, "%s, %s %s $ ",
			n2p ("town", 1) -> p_value,
			n2p ("state", 1) -> p_value,
			n2p ("zipcode", 1) -> p_value);
	if ((i = strlen (cp)) > 30 + 3) {
	    advise (NULLCP,
		    "your town/state/zipcode is longer than 30 characters!");
	    goto postal_problem;
	}
	cp += i;

	if ((c -> c_flags & C_SHORT) || (i = strlen (dp = c -> c_name)) > 30)
	    i = strlen (dp = c -> c_code);
	(void) strcpy (cp, dp);
	cp += i;

	p -> p_value = strdup (buffer);
    }
}

/*  */

static struct country *read_country (code)
char   *code;
{
    int	    vecp;
    register char *cp,
		  *dp;
    char    d,
	   *ep,
	    buffer[BUFSIZ + 1],
	    file[BUFSIZ],
	   *vec[NVEC + NSLACK + 1];
    FILE   *fp;
    static struct country cs;
    register struct country *c = NULL;

    (void) strcpy (file, "iso3166");
    if ((fp = fopen (file, "r")) == NULL)
	adios (file, "unable to open");

    ep = (dp = buffer) + sizeof buffer;
    while (fgets (dp, ep - dp, fp)) {
	if (*buffer == '#')
	    continue;
	if (cp = index (buffer, '\n')) {
	    *cp = NULL;
	    if ((d = getc (fp)) != EOF)
		(void) ungetc (d, fp);
	    switch (d) {
		case '#':
		case '\n':
		case EOF:
		    break;

		default:
		    *cp++ = ' ';
		    if ((dp = cp) + 1 >= ep) {
			*ep = NULL;
			adios (NULLCP,
			       "virtual line too long in %s: \"%s\"",
			       file, cp);
		    }
		    continue;
	    }
	}

	dp = buffer;
	switch (vecp = str2vec (buffer, vec)) {
	    case 7:		/* standard entry */
	    case 8:
	        if (strcmp (vec[0], code))
		    continue;
		c = &cs;
		bzero ((char *) c, sizeof *c);
		c -> c_code = strdup (vec[0]);
		if (sscanf (vec[1], "%d", &c -> c_number) != 1)
		    adios (NULLCP,
			   "malformed ISO3166 number for country %s in %s",
			   code, file);
		c -> c_number = atoi (vec[1]);
		c -> c_name = strdup (vec[2]);
		if (strcmp (vec[3], "NULL"))
		    c -> c_root = strdup (vec[3]);
		if (strcmp (vec[4], "NULL"))
		    c -> c_master = strdup (vec[4]);
		else
		    adios (NULLCP, "no masterDSA known for country %s in %s",
			   code,file);
		if (strcmp (vec[5], "NULL"))
		    c -> c_other = strdup (vec[5]);
		c -> c_phone = strdup (vec[6]);
		if (vecp == 8) {
		    char   *bp;

		    if (strncmp (cp = vec[7], "0x", 2) == 0)
			cp += 2, bp = "%x";
		    else
			if (*cp == '0')
			    cp += 1, bp = "%o";
		        else
			    bp = "%d";
		    if (sscanf (cp, bp, &c -> c_flags) != 1)
			adios (NULLCP,
			      "malformed entry for country %s in %s",
			       code, file);
		}
		break;

	    default:
	        continue;
	}
	break;
    }

    (void) fclose (fp);

    return c;
}

/*  */

static	read_psap (dsa, addr)
char   *dsa,
     **addr;
{
    int	    i;
    register char *cp,
		  *dp;
    char    buffer[BUFSIZ],
	    stuff[BUFSIZ];
    FILE   *fp;

    if ((fp = fopen (cp = isodefile ("dsaptailor", 0), "r")) == NULL)
	adios (cp, "unable to open");

    while (fgets (buffer, sizeof buffer, fp)) {
	if (lexnequ (buffer, "dsa_address", sizeof "dsa_address" - 1)
	        || (cp = index (buffer, '"')) == NULL
	        || lexnequ (++cp, dsa + 3, i = strlen (dsa + 3))
	        || *(cp += i ) != '"')
	    continue;
	for (cp++; isspace (*cp); cp++)
	    continue;
	if (*cp == NULL) {
malformed: ;
	    adios (NULLCP, "malformed entry in dsaptailor for DSA %s", dsa);
	}
	for (dp = stuff; *cp; *dp++ = *cp++) {
	    switch (*cp) {
		case '\n':
		    break;

		case '\'':
		    *dp++ = '\\';
		    /* and fall... */
		case '\\':
		    *dp++ = '\\';
		    continue;

		default:
		    continue;
	    }
	    break;
	}
	*dp = NULL;
	if (*cp != '\n')
	    goto malformed;
	*addr = strdup (stuff);
	goto hit;
    }
    adios (NULLCP, "unable to find presentationAddress for DSA %s", dsa);

hit: ;
    (void) fclose (fp);
}

/*  */

generate_sed () {
    FILE   *fp;
    register struct pair *p;

    (void) sprintf (sedfil, "/tmp/%sXXXXXX", myname);
    (void) unlink (mktemp (sedfil));

    if ((fp = fopen (sedfil, "w")) == NULL)
	adios (sedfil, "unable to create");

    for (p = pairs; p -> p_name; p++)
	if (!(p -> p_flags & P_MBOX) && p -> p_value)
	    fprintf (fp, "s!@(%s)!%s!\n", p -> p_name, p -> p_value);

    if (ferror (fp))
	adios (sedfil, "error writing");
    (void) fclose (fp);
}

/*  */

static struct pair *n2p (name, any)
char   *name;
int	any;
{
    register struct pair *p;

    for (p = pairs; p -> p_name; p++)
	if (strcmp (p -> p_name, name) == 0)
	    return (any || (p -> p_value && !(p -> p_flags & P_ZAP)) ? p
                                                                     : NULL);

    adios (NULLCP, "internal error -- unable to find \"%s\"", name);
    /* NOTREACHED */
}

/*  */

static	munge (fp, entries)
FILE   *fp;
char   *entries[];
{
    register char   c,
		   *bp,
		   *cp,
		   *dp,
		  **pp;
    char    buffer[BUFSIZ];
    register struct pair *p;


    for (pp = entries; cp = *pp; pp++) {
	bp = buffer;

	while (c = *cp++) {
	    if (c != '@' || *cp != '(') {
		*bp++ = c;
		continue;
	    }
	    if ((dp = index (++cp, ')')) == NULL)
		adios (NULLCP, "internal error -- you lose big");
	    *dp = NULL;

	    if ((p = n2p (cp, 0)) == NULL) {
		bp = buffer;
		goto no_match;
	    }
	    (void) strcpy (bp, p -> p_value);
	    bp += strlen (bp);

	    cp = ++dp;
	}

	*bp = NULL;
	fprintf (fp, "%s\n", buffer);
no_match: ;
    }
}

/*    EDB */

static	build_root () {
    if (debug)
	fprintf (stderr, "mkdir %s\n", wildlife);
    if (mkdir (wildlife, 0700) == NOTOK)
	adios (wildlife, "unable to create directory");

    make_file ("root.edb", "EDB", 0600, 0);
}

/*  */

static char *c_TLC[] = {
    "o=@(organization)",
    "masterDSA= c=@(country)@cn=@(dsa)#",
    "acl= group # c=@(country)@o=@(organization)@cn=Manager # write # entry",
    "acl= others # read # entry",
    "acl= group # c=@(country)@o=@(organization)@cn=Manager # write # default",
    "acl= others # read # default",
    "acl= group # c=@(country)@o=@(organization)@cn=Manager # write # attributes # userPassword",
    "acl= others # compare # attributes # userPassword",
    "o= @(organization)",
    "streetAddress= @(street)",
    "postOfficeBox= @(pob)",
    "physicalDeliveryOfficeName= @(town)",
    "stateOrProvinceName= @(state)",
    "postalCode= @(zipcode)",
    "postalAddress= @(postaladdress)",
    "telephoneNumber= @(telephone)",
    "facsimileTelephoneNumber= @(fax)",
    "localityName= @(locality)",
    "description= @(description)",
    "associatedDomain= @(domain)",
    "objectClass= top & quipuObject & quipuNonLeafObject",
    "objectClass= domainRelatedObject",
    "objectClass= organization",
    "",

    "cn=@(dsa)",
    "acl= group # c=@(country)@o=@(organization)@cn=Manager # write # entry",
    "acl= others # read # entry",
    "acl= group # c=@(country)@o=@(organization)@cn=Manager # write # default",
    "acl= others # read # default",
    "cn= @(dsa)",
    "eDBinfo= # @(rootDSA) #",
    "eDBinfo= c=@(country) # @(countryDSA) #",
    "eDBinfo= c=@(country)@o=@(organization) # # @(countryDSA)",
    "eDBinfo= c=@(country)@o=@(organization) # # @(rootDSA)",
    "eDBinfo= c=@(country)@o=@(organization) # # @(otherDSA)",
    "presentationAddress= '0101'H/Internet=@(ipaddr)+@(port)",
    "manager= c=@(country)@o=@(organization)@cn=Manager#",
    "manager= c=@(country)@cn=Manager#",
    "userPassword= @(wildlife)",
    "quipuVersion= @(quipuversion)",
    "localityName= @(locality)",
    "description= The Endangered @(dsa)",
    "description= Master DSA for @(organization) in the @(country)",
    "objectClass= top & quipuObject",
    "objectClass= applicationEntity & dSA & quipuDSA",
    "supportedApplicationContext= x500DSP & x500DAP & quipuDSP",
    "",

    NULL
};


static	build_TLC () {
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, "%s/c=%s", wildlife,
		    n2p ("country", 1) -> p_value);
    make_edb (buffer, "SLAVE", "0000000000Z", c_TLC);
}

/*  */

static char *o_I[] = {
    "cn=Manager",
    "acl=",
    "cn= Manager",
    "aliasedObjectName= c=@(country)@o=@(organization)@ou=@(unit)@cn=@(firstname) @(lastname)#",
    "objectClass= top & quipuObject",
    "objectClass= alias",
    "",

    "cn=Postmaster",
    "acl=",
    "cn= Postmaster",
    "aliasedObjectName= c=@(country)@o=@(organization)@ou=@(unit)@cn=@(firstname) @(lastname)#",
    "objectClass= top & quipuObject",
    "objectClass= alias",
    "",

    "ou=@(unit)",
    "masterDSA= c=@(country)@cn=@(dsa)#",
    "acl= others # read # entry",
    "acl= others # read # default",
    "acl= others # compare # attributes # userPassword",
    "ou= @(unit)",
    "objectClass= top & quipuObject & quipuNonLeafObject",
    "objectClass= organizationalUnit",
    "",

    NULL
};
	

static	build_organization () {
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, "%s/c=%s/o=%s", wildlife,
		   n2p ("country", 1) -> p_value,
		   n2p ("organization", 1) -> p_value);
    make_edb (buffer, "MASTER", version (), o_I);
}

/*  */

static char *u_J[] = {
    "cn=@(firstname) @(lastname)",
    "acl= self # write # entry",
    "acl= others # read # entry",
    "acl= self # write # default",
    "acl= others # read # default",
    "acl= self # write # attributes # userPassword",
    "acl= others # compare # attributes # userPassword",
    "rfc822Mailbox= @(mailbox)",
    "otherMailbox = internet $ @(mailbox)",
    "title= @(title)",
    "userid= @(userid)",
    "userPassword= @(password)",
    "telephoneNumber= @(extension)",
    "surname= @(lastname)",
    "cn= @(firstname) @(lastname)",
    "cn= @(firstname) @(middleinitial). @(lastname)",
    "cn= @(firstname) @(middlename) @(lastname)",
    "objectClass= top & quipuObject",
    "objectClass= person & thornPerson & pilotPerson",
    "",

    NULL
};


static	build_unit () {
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, "%s/c=%s/o=%s/ou=%s", wildlife,
		   n2p ("country", 1) -> p_value,
		   n2p ("organization", 1) -> p_value,
		   n2p ("unit", 1) -> p_value);
    make_edb (buffer, "MASTER", version (), u_J);
}

/*  */

static	make_edb (dir, type, date, entries)
char   *dir,
       *type,
       *date;
char   *entries[];
{
    char    edb[BUFSIZ];
    FILE   *fp;

    if (debug)
	fprintf (stderr, "mkdir %s\n", dir);
    if (mkdir (dir, 0700) == NOTOK)
	adios (dir, "unable to create directory");

    (void) sprintf (edb, "%s/EDB", dir);
    if (debug)
	fprintf (stderr, "create %s\n", edb);
    if ((fp = fopen (edb, "w")) == NULL)
	adios (edb, "unable to create");

    fprintf (fp, "%s\n%s\n", type, date);
    munge (fp, entries);
    
    if (ferror (fp))
	adios (edb, "error writing");
    (void) fclose (fp);

    (void) chmod (edb, 0600);
}

/*  */

static char *version () {
    long    clock;
    struct UTCtime ut;
    static char buffer[BUFSIZ];

    (void) time (&clock);
    tm2ut (gmtime (&clock), &ut);
    (void) strcpy (buffer, gent2str (&ut));

    return buffer;
}

/*    FILES */

static	build_tailor () { make_file ("quiputailor", "quiputailor", 0644, 1); }

static	build_startup () { make_file ("startup.sh", "startup.sh", 0755, 1); }

static	build_nightly () { make_file ("nightly.sh", "nightly.sh", 0755, 1); }

/*  */

static	make_file (infile, outfile, mode, dosed)
char   *infile,
       *outfile;
int	mode,
	dosed;
{
    char    buffer[BUFSIZ];

    if (dosed)
	(void) sprintf (buffer, "sed -f %s < templates/%s > %s/%s",
			sedfil, infile, wildlife, outfile);
    else
	(void) sprintf (buffer, "cp templates/%s %s/%s", infile, wildlife,
			outfile);

    if (debug)
	fprintf (stderr, "%s\n", buffer);
    if (system (buffer) != 0)
	adios (NULLCP, "%s of %s failed", dosed ? "sed" : "cp", infile);

    (void) sprintf (buffer, "%s/%s", wildlife, outfile);
    (void) chmod (buffer, mode);
}

/*    SED */

static	build_dsap () { fudge_file ("dsaptailor"); }

static	build_fred () { fudge_file ("fredrc"); fudge_file ("ufnrc"); }

/*  */

static	fudge_file (name)
char   *name;
{
    char   buffer[BUFSIZ],
	   file[BUFSIZ],
	   oldfil[BUFSIZ],
	   tmpfil[BUFSIZ];

    (void) strcpy (file, isodefile (name, 0));
    
    (void) sprintf (buffer, "%sXXXXXX", myname);
    (void) strcpy (tmpfil, isodefile (buffer, 0));
    (void) unlink (mktemp (tmpfil));

    (void) sprintf (buffer, "sed -f %s < %s > %s", sedfil, file, tmpfil);
    if (debug)
	fprintf (stderr, "%s\n", buffer);
    if (system (buffer) != 0) {
	(void) unlink (tmpfil);
	adios (NULLCP, "sed failed");
    }

    (void) sprintf (oldfil, "%s.old", file);
    if (access (oldfil, 0x00) == NOTOK)
	(void) rename (file, oldfil);
    if (rename (tmpfil, file) == NOTOK)
	adios (file, "unable to rename %s to", tmpfil);
}

/*    ARGINIT */

static	arginit (vec)
char  **vec;
{
    register char  *ap;

    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;

    isodetailor (myname, 1);

    for (vec++; ap = *vec; vec++) {
	if (*ap == '-') {
	    while (*++ap)
		switch (*ap) {
		    case 'd':
		        debug++;
			break;

		    case 'c':
			parse_3166 ();
			exit (0);

		    case 't':
			table_3166 ();
			exit (0);

 		    default:
			adios (NULLCP, "unknown switch -%c", *ap);
		}
	    continue;
	}

	if (wildlife) {
usage: ;
	    adios (NULLCP, "usage: %s config-file", myname);
	}
	wildlife = ap;
    }

    if (!wildlife)
	goto usage;
}

/*  */

static	parse_3166 ()
{
    int	    bitno;
    unsigned int bits_size;
    register char *cp,
		  *dp;
    char    d,
	   *ep,
	    buffer[BUFSIZ + 1],
	    file[BUFSIZ],
	   *vec[NVEC + NSLACK + 1];
    unsigned char *bits_3166;
    FILE   *fp;

    (void) strcpy (file, isodefile ("quipu/iso3166", 0));
    if ((fp = fopen (file, "r")) == NULL)
	adios (file, "unable to open");

    bits_size = ((26 * 26) >> 3) + 1;
    if ((bits_3166 = (unsigned char *) calloc (bits_size, sizeof *bits_3166))
	    == NULL)
	adios (NULLCP, "out of memory");

    ep = (dp = buffer) + sizeof buffer;
    while (fgets (dp, ep - dp, fp)) {
	if (*buffer == '#')
	    continue;
	if (cp = index (buffer, '\n')) {
	    *cp = NULL;
	    if ((d = getc (fp)) != EOF)
		(void) ungetc (d, fp);
	    switch (d) {
		case '#':
		case '\n':
		case EOF:
		    break;

		default:
		    *cp++ = ' ';
		    if ((dp = cp) + 1 >= ep) {
			*ep = NULL;
			adios (NULLCP,
			       "virtual line too long in %s: \"%s\"",
			       file, cp);
		    }
		    continue;
	    }
	}

	dp = buffer;
	switch (str2vec (buffer, vec)) {
	    case 3:		/* basic entry */
	    case 7:		/* standard entry */
	    case 8:
	        cp = vec[0];
		bitno = (((*cp - 'A') & 0xff) * 26) + ((cp[1] - 'A') & 0xff);
		bits_3166[bitno >> 3] |= 1 << (bitno % 8);
		break;

	    default:
	        break;
	}
	continue;
    }

    {
	register unsigned char *bp;
	unsigned char *xp;

	printf ("static unsigned char bits_3166[%u] = {\n", bits_size);
	for (xp = (bp = bits_3166) + bits_size; bp < xp; bp++)
	    printf ("    0x%02x,\n", *bp & 0xff);
	printf ("};\n");
    }

    (void) fclose (fp);
}

/*  */

static	table_3166 ()
{
    register char *cp,
		  *dp;
    char    d,
	   *ep,
	    buffer[BUFSIZ + 1],
	    file[BUFSIZ],
	   *vec[NVEC + NSLACK + 1];
    FILE   *fp;

    (void) strcpy (file, isodefile ("quipu/iso3166", 0));
    if ((fp = fopen (file, "r")) == NULL)
	adios (file, "unable to open");

    ep = (dp = buffer) + sizeof buffer;
    while (fgets (dp, ep - dp, fp)) {
	if (*buffer == '#')
	    continue;
	if (cp = index (buffer, '\n')) {
	    *cp = NULL;
	    if ((d = getc (fp)) != EOF)
		(void) ungetc (d, fp);
	    switch (d) {
		case '#':
		case '\n':
		case EOF:
		    break;

		default:
		    *cp++ = ' ';
		    if ((dp = cp) + 1 >= ep) {
			*ep = NULL;
			adios (NULLCP,
			       "virtual line too long in %s: \"%s\"",
			       file, cp);
		    }
		    continue;
	    }
	}

	dp = buffer;
	switch (str2vec (buffer, vec)) {
	    case 3:		/* basic entry */
	    case 7:		/* standard entry */
	    case 8:
	        printf ("%s&\t%s&\t%s\\\\\n", vec[0], vec[1], vec[2]);
		break;

	    default:
	        break;
	}
	continue;
    }

    (void) fclose (fp);
}

/*    ERRORS */

#ifndef	lint
void	_advise ();


static void  adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

static void  adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
static void  advise (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);
}


static void  _advise (ap)
va_list	ap;
{
    char    buffer[BUFSIZ];

    asprintf (buffer, ap);

    (void) fflush (stdout);

    fprintf (stderr, "%s: ", myname);
    (void) fputs (buffer, stderr);
    (void) fputc ('\n', stderr);

    (void) fflush (stderr);
}
#else
/* VARARGS */

static void  advise (what, fmt)
char   *what,
       *fmt;
{
    advise (what, fmt);
}
#endif

/*    MISCELLANY */

#ifndef	lint
static char *strdup (s)
char   *s;
{
    char    *p;

    if ((p = malloc((unsigned) (strlen (s) + 1))) == NULL)
	adios (NULLCP, "out of memory");

    (void) strcpy (p, s);

    return p;
}
#endif
