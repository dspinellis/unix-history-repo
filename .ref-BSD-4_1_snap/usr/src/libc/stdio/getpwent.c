/* @(#)getpwent.c	4.1 (Berkeley) 12/21/80 */
#include <stdio.h>
#include <pwd.h>

static char PASSWD[]	= "/etc/passwd";
static char EMPTY[] = "";
static FILE *pwf = NULL;
static char line[BUFSIZ+1];
static struct passwd passwd;

setpwent()
{
	if( pwf == NULL )
		pwf = fopen( PASSWD, "r" );
	else
		rewind( pwf );
}

endpwent()
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

struct passwd *
getpwent()
{
	register char *p;

	if (pwf == NULL) {
		if( (pwf = fopen( PASSWD, "r" )) == NULL )
			return(0);
	}
	p = fgets(line, BUFSIZ, pwf);
	if (p==NULL)
		return(0);
	passwd.pw_name = p;
	p = pwskip(p);
	passwd.pw_passwd = p;
	p = pwskip(p);
	passwd.pw_uid = atoi(p);
	p = pwskip(p);
	passwd.pw_gid = atoi(p);
	passwd.pw_quota = 0;
	passwd.pw_comment = EMPTY;
	p = pwskip(p);
	passwd.pw_gecos = p;
	p = pwskip(p);
	passwd.pw_dir = p;
	p = pwskip(p);
	passwd.pw_shell = p;
	while(*p && *p != '\n') p++;
	*p = '\0';
	return(&passwd);
}
