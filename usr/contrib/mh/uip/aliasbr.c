/* aliasbr.c - new aliasing mechanism */

#include "../h/mh.h"
#include "../h/aliasbr.h"
#include <ctype.h>
#include <grp.h>
#include <pwd.h>
#include <stdio.h>


static int  akvis;
static char *akerrst;

struct aka *akahead = NULL;
struct aka *akatail = NULL;

struct home *homehead = NULL;
struct home *hometail = NULL;

char   *scanp (), *getp (), *seekp (), *akval (), *getalias ();
struct aka *akalloc ();
struct home *hmalloc ();


struct passwd  *getpwent ();
struct group   *getgrnam (), *getgrgid ();

/*  */

char   *akvalue (s)
register char   *s;
{
    register char  *v;

    if (akahead == NULL)
	(void) alias (AliasFile);

    akvis = -1;
    v = akval (akahead, s);
    if (akvis == -1)
	akvis = 0;
    return v;
}


int     akvisible () {
    return akvis;
}

/*  */

char   *akresult (ak)
register struct aka *ak;
{
    register char  *cp = NULL,
                   *dp,
                   *pp;
    register struct adr *ad;

    for (ad = ak -> ak_addr; ad; ad = ad -> ad_next) {
	pp = ad -> ad_local ? akval (ak -> ak_next, ad -> ad_text)
	    : getcpy (ad -> ad_text);

	if (dp = cp) {
	    cp = concat (cp, ",", pp, NULLCP);
	    free (dp);
	    free (pp);
	}
	else
	    cp = pp;
    }

    if (akvis == -1)
	akvis = ak -> ak_visible;
    return cp;
}


static	char   *akval (ak, s)
register struct aka *ak;
register char   *s;
{
    for (; ak; ak = ak -> ak_next)
	if (aleq (s, ak -> ak_name))
	    return akresult (ak);

    return getcpy (s);
}


static	int aleq (string, aliasent)
register char   *string,
		*aliasent;
{
    register char    c;

    while (c = *string++)
	if (*aliasent == '*')
	    return 1;
	else
	    if ((c | 040) != (*aliasent | 040))
		return 0;
	    else
		aliasent++;

    return (*aliasent == NULL || *aliasent == '*');
}

/*  */

int     alias (file)
register char   *file;
{
    int     i;
    register char  *bp,
		   *cp,
                   *pp;
    char    lc,
	   *ap;
    register struct aka *ak = NULL;
    register    FILE *fp;

    if (*file != '/' && *file != '.')
	file = libpath (file);
    if ((fp = fopen (file, "r")) == NULL) {
	akerrst = file;
	return AK_NOFILE;
    }

    while (vfgets (fp, &ap) == OK) {
	bp = ap;
	switch (*(pp = scanp (bp))) {
	    case '<': 		/* recurse a level */
		if (!*(cp = getp (pp + 1))) {
		    akerrst = "'<' without alias-file";
		    (void) fclose (fp);
		    return AK_ERROR;
		}
		if ((i = alias (cp) != AK_OK)) {
		    (void) fclose (fp);
		    return i;
		}

	    case ':': 		/* comment */
	    case ';': 
	    case NULL: 
		continue;
	}

	akerrst = bp;
	if (!*(cp = seekp (pp, &lc, &ap))) {
	    (void) fclose (fp);
	    return AK_ERROR;
	}
	if (!(ak = akalloc (cp))) {
	    (void) fclose (fp);
	    return AK_LIMIT;
	}
	switch (lc) {
	    case ':': 
		ak -> ak_visible = 0;
		break;

	    case ';': 
		ak -> ak_visible = 1;
		break;

	    default: 
		(void) fclose (fp);
		return AK_ERROR;
	}

	switch (*(pp = scanp (ap))) {
	    case NULL: 		/* EOL */
		(void) fclose (fp);
		return AK_ERROR;

	    case '<': 		/* read values from file */
		if (!*(cp = getp (pp + 1))) {
		    (void) fclose (fp);
		    return AK_ERROR;
		}
		if (!addfile (ak, cp)) {
		    (void) fclose (fp);
		    return AK_NOFILE;
		}
		break;

	    case '=': 		/* UNIX group */
		if (!*(cp = getp (pp + 1))) {
		    (void) fclose (fp);
		    return AK_ERROR;
		}
		if (!addgroup (ak, cp)) {
		    (void) fclose (fp);
		    return AK_NOGROUP;
		}
		break;

	    case '+': 		/* UNIX group members */
		if (!*(cp = getp (pp + 1))) {
		    (void) fclose (fp);
		    return AK_ERROR;
		}
		if (!addmember (ak, cp)) {
		    (void) fclose (fp);
		    return AK_NOGROUP;
		}
		break;

	    case '*': 		/* Everyone */
		(void) addall (ak);
		break;

	    default: 		/* list */
		while (cp = getalias (pp))
		    add_aka (ak, cp);
		break;
	}
    }

    (void) fclose (fp);
    return AK_OK;
}

/*  */

char   *akerror (i)
int     i;
{
    static char buffer[BUFSIZ];

    switch (i) {
	case AK_NOFILE: 
	    (void) sprintf (buffer, "unable to read '%s'", akerrst);
	    break;

	case AK_ERROR: 
	    (void) sprintf (buffer, "error in line '%s'", akerrst);
	    break;

	case AK_LIMIT: 
	    (void) sprintf (buffer, "out of memory while on '%s'", akerrst);
	    break;

	case AK_NOGROUP: 
	    (void) sprintf (buffer, "no such group as '%s'", akerrst);
	    break;

	default: 
	    (void) sprintf (buffer, "unknown error (%d)", i);
	    break;
    }

    return buffer;
}

/*  */

static char   *scanp (p)
register char   *p;
{
    while (isspace (*p))
	p++;
    return p;
}


static char   *getp (p)
register char   *p;
{
    register char  *cp = scanp (p);

    p = cp;
    while (!isspace (*cp) && *cp)
	cp++;
    *cp = NULL;

    return p;
}


static char   *seekp (p, c, a)
register char   *p,
		*c,
	       **a;
{
    register char  *cp = scanp (p);

    p = cp;
    while (!isspace (*cp) && *cp && *cp != ':' && *cp != ';')
	cp++;
    *c = *cp;
    *cp++ = NULL;
    *a = cp;

    return p;
}

/*  */

static	int addfile (ak, file)
register struct aka *ak;
register char   *file;
{
    register char  *cp;
    char    buffer[BUFSIZ];
    register    FILE *fp;

    if ((fp = fopen (libpath (file), "r")) == NULL) {
	akerrst = file;
	return NULL;
    }

    while (fgets (buffer, sizeof buffer, fp) != NULL)
	while (cp = getalias (buffer))
	    add_aka (ak, cp);

    (void) fclose (fp);
    return 1;
}

/*  */

static	int addgroup (ak, grp)
register struct aka *ak;
register char   *grp;
{
    register char  *gp;
    register struct group  *gr = getgrnam (grp);
    register struct home   *hm = NULL;

    if (!gr)
	gr = getgrgid (atoi (grp));
    if (!gr) {
	akerrst = grp;
	return NULL;
    }

    if (homehead == NULL)
	init_pw ();

    while (gp = *gr -> gr_mem++)
	for (hm = homehead; hm; hm = hm -> h_next)
	    if (!strcmp (hm -> h_name, gp)) {
		add_aka (ak, hm -> h_name);
		break;
	    }

    return 1;
}

/*  */

static	int addmember (ak, grp)
register struct aka *ak;
register char   *grp;
{
    int     gid;
    register struct group  *gr = getgrnam (grp);
    register struct home   *hm = NULL;

    if (gr)
	gid = gr -> gr_gid;
    else {
	gid = atoi (grp);
	gr = getgrgid (gid);
    }
    if (!gr) {
	akerrst = grp;
	return NULL;
    }

    if (homehead == NULL)
	init_pw ();

    for (hm = homehead; hm; hm = hm -> h_next)
	if (hm -> h_gid == gid)
	    add_aka (ak, hm -> h_name);

    return 1;
}

/*  */

static	int addall (ak)
register struct aka *ak;
{
    int     noshell = NoShell == NULLCP || *NoShell == NULL;
    register struct home   *hm;

    if (homehead == NULL)
	init_pw ();
    if (Everyone < 0)
	Everyone = EVERYONE;

    for (hm = homehead; hm; hm = hm -> h_next)
	if (hm -> h_uid > Everyone
		&& (noshell || strcmp (hm -> h_shell, NoShell)))
	    add_aka (ak, hm -> h_name);

    return homehead != NULL;
}

/*  */

static char   *getalias (addrs)
register char   *addrs;
{
    register char  *pp,
                   *qp;
    static char *cp = NULL;

    if (cp == NULL)
	cp = addrs;
    else
	if (*cp == NULL)
	    return (cp = NULL);

    for (pp = cp; isspace (*pp); pp++)
	continue;
    if (*pp == NULL)
	return (cp = NULL);
    for (qp = pp; *qp != NULL && *qp != ','; qp++)
	continue;
    if (*qp == ',')
	*qp++ = NULL;
    for (cp = qp, qp--; qp > pp; qp--)
	if (*qp != NULL)
	    if (isspace (*qp))
		*qp = NULL;
	    else
		break;

    return pp;
}

/*  */

static	add_aka (ak, pp)
register struct aka *ak;
register char   *pp;
{
    register struct adr *ad,
			*ld;

    for (ad = ak -> ak_addr, ld = NULL; ad; ld = ad, ad = ad -> ad_next)
	if (!strcmp (pp, ad -> ad_text))
	    return;

    ad = (struct adr   *) malloc (sizeof *ad);
    if (ad == NULL)
	return;
    ad -> ad_text = getcpy (pp);
    ad -> ad_local = index (pp, '@') == NULL && index (pp, '!') == NULL;
    ad -> ad_next = NULL;
    if (ak -> ak_addr)
	ld -> ad_next = ad;
    else
	ak -> ak_addr = ad;
}


init_pw () {
    register struct passwd  *pw;

    (void) setpwent ();

    while (pw = getpwent ())
	if (!hmalloc (pw))
	    break;

    (void) endpwent ();
}

/*  */

static struct aka *akalloc (id)
register char   *id;
{
    register struct aka *p = (struct aka   *) malloc (sizeof *p);

    if (!p)
	return NULL;

    p -> ak_name = getcpy (id);
    p -> ak_visible = 0;
    p -> ak_addr = NULL;
    p -> ak_next = NULL;
    if (akatail != NULL)
	akatail -> ak_next = p;
    if (akahead == NULL)
	akahead = p;
    akatail = p;

    return p;
}


static struct home *hmalloc (pw)
struct passwd *pw;
{
    register struct home   *p = (struct home   *) malloc (sizeof *p);

    if (!p)
	return NULL;

    p -> h_name = getcpy (pw -> pw_name);
    p -> h_uid = pw -> pw_uid;
    p -> h_gid = pw -> pw_gid;
    p -> h_home = getcpy (pw -> pw_dir);
    p -> h_shell = getcpy (pw -> pw_shell);
#ifdef	BSD42
    p -> h_ngrps = 0;
#endif	BSD42
    p -> h_next = NULL;
    if (hometail != NULL)
	hometail -> h_next = p;
    if (homehead == NULL)
	homehead = p;
    hometail = p;

    return p;
}

/*  */

#ifndef	MMDFMTS
struct home *seek_home (name)
register char   *name;
{
    register struct home *hp;

    if (homehead == NULL)
	init_pw ();

    for (hp = homehead; hp; hp = hp -> h_next)
	if (uleq (name, hp -> h_name))
	    return hp;
	
    return NULL;
}
#endif	MMDFMTS
