From James.Gosling@CMU-VLSI@CMU-10A  Thu Sep 10 07:13:52 1981
Date: 10 Sep 1981 10:07:48-EDT
From: James.Gosling at CMU-VLSI at CMU-10A
Reply-To: James.Gosling at CMU-10A
To: IngVAX.eric@Berkeley
Subject: getpwwho.c
Status: R

/*
 **********************************************************************
 * HISTORY
 * 11-Dec-80  Mike Accetta(mja) at Carnegie-Mellon University
 *	Changed to convert all separator characters to spaces when
 *	copying name into internal buffer so that dots in names with
 *	more than two parts will match password file entries.
 *
 * 03-Dec-80  Mike Accetta (mja) at Carnegie-Mellon University
 *	Changed to treat everything before last blank in name as first
 *	name and modified to close password file when called with null
 *	pointer.
 *
 * 29-Nov-80  Mike Accetta (mja) at Carnegie-Mellon University
 *	Changed to allow '.' as separator in names.
 *
 * 07-Nov-80  Mike Accetta (mja) at Carnegie-Mellon University
 *	Removed reference to (extinct) alias name.
 *
 **********************************************************************
 */

#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <gecos.h>

#define	dotisspace(c)	(isspace(c) || (c) == '.')

struct gecos *parsgecos();

static char PASSWD[]	= "/etc/passwd";
static char EMPTY[] = "";
static FILE *pwf = NULL;
static char line[BUFSIZ+1], auxline[BUFSIZ+1];
static struct passwd passwd, auxpasswd;
static struct gecos auxgecos;
static int ambigstate;

static setpwent()
{
	if( pwf == NULL )
		pwf = fopen( PASSWD, "r" );
	else
		rewind( pwf );
}

static endpwent()
{
	if( pwf != NULL ){
		fclose( pwf );
		pwf = NULL;
	}
}

static char *
pwskip(p)
register char *p;
{
	while( *p && *p != ':' )
		++p;
	if( *p ) *p++ = 0;
	return(p);
}

static struct passwd *getpwent(passwd,line)
register struct passwd *passwd;
char * line;
{
	register char *p;

	if (pwf == NULL) {
		if( (pwf = fopen( PASSWD, "r" )) == NULL )
			return(0);
	}
	p = fgets(line, BUFSIZ, pwf);
	if (p==NULL)
		return(0);
	passwd->pw_name = p;
	p = pwskip(p);
	passwd->pw_passwd = p;
	p = pwskip(p);
	passwd->pw_uid = atoi(p);
	p = pwskip(p);
	passwd->pw_gid = atoi(p);
	passwd->pw_quota = 0;
	passwd->pw_comment = EMPTY;
	p = pwskip(p);
	passwd->pw_gecos = p;
	p = pwskip(p);
	passwd->pw_dir = p;
	p = pwskip(p);
	passwd->pw_shell = p;
	while(*p && *p != '\n') p++;
	*p = '\0';
	return(passwd);
}

struct gecos *_gecos;

static char name1[100], name2[100];
static int name1l, name2l;

static scanpw (pw, buf)
register struct passwd *pw;
char   *buf;
{
    register struct gecos  *ge;
    register char  *cp;

    while (getpwent (pw, buf)) {
	_gecos = ge = parsgecos (pw -> pw_gecos);
	if (*name1 == '\0' && strcmp (name2, pw -> pw_name) == 0)
	    return 1;
	if (IsName (ge -> pw_who))
	    return 2;
    }
    return 0;
}

static  IsName (name)
char   *name; {
    register char  *p1,
                   *p2;
    if (!FoldedEQ (name, name1, name1l))
	return (0);
    p1 = p2 = name;
    while (*p2){
	if (dotisspace (*p2))
	    p1 = p2+1;
	p2++;
    }
    return (FoldedEQ (p1, name2, name2l));
}

struct passwd  *
getpwwho (name)
char   *name; {
    register char *s, *d;
    ambigstate = -1;
    if (name == NULL)
    {
	endpwent();
	return(0);
    }
    s = d = name2;
    name1[0] = '\0';
    strcpy(name2, name);
    for(; *d; d++)
	if (dotisspace(*d)) {
	    *d = ' ';
	    s = d;
	}
    if(dotisspace(*s)){
	*s++ = 0;
	strcpy (name1, name2);
	strcpy (name2, s);
    }
    name1l = strlen (name1);
    name2l = strlen (name2);
    setpwent ();
    switch (scanpw (&passwd, line)) {
	case 0: 
	    return (0);
	case 1: 
	    return (&passwd);
	default: ;
    }
    auxgecos = *_gecos;
    while (1) {
	switch (scanpw (&auxpasswd, auxline)) {
	    case 0: 
		if (ambigstate == -1) {
		    _gecos = &auxgecos;
		    return & passwd;
		}
		return (struct passwd  *) - 1;
	    case 1: 
		return & auxpasswd;
	    case 2: 
		ambigstate = 0;
		break;
	}
    }
    /*  NOTREACHED */
}

struct passwd  *getpwambig () {
    if (ambigstate < 0)
	return 0;
    if (ambigstate++ == 0)
	setpwent();
    if (!scanpw (&passwd, line)) {
	ambigstate = -1;
	return 0;
    }
    return & passwd;
}

static FoldedEQ(s1,s2,len)
register char *s1,*s2;
register len; {
	while(--len>=0 && *s1 && (isupper(*s1) ? tolower(*s1) : *s1)
			      == (isupper(*s2) ? tolower(*s2) : *s2))
		s1++, s2++;
	return len<0 || *s1==0 && *s2==0;
}


