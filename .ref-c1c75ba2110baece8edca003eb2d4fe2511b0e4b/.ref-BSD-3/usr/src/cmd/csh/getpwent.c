/* Copyright (c) 1979 Regents of the University of California */
#include <pwd.h>

#define	BUFSIZ	160

static int pwf = -1;
static char line[BUFSIZ+1];
static struct passwd passwd;

setpwent()
{
	if( pwf == -1 )
		pwf = open( "/etc/passwd", 0 );
	else
		lseek(pwf, 0l, 0);
}

endpwent()
{
	if( pwf != -1 ){
		close( pwf );
		pwf = -1;
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
	register char *p, *q;
	register int i, j;

	if (pwf == -1) {
		if( (pwf = open( "/etc/passwd", 0 )) == -1 )
			return(0);
	}
	i = read(pwf, line, BUFSIZ);
	for (j = 0; j < i; j++)
		if (line[j] == '\n')
			break;
	if (j >= i)
		return(0);
	line[++j] = 0;
	lseek(pwf, (long) (j - i), 1);
	p = line;
	passwd.pw_name = p;
	p = pwskip(p);
/*	passwd.pw_passwd = p; */
	p = q = pwskip(p);
/*	passwd.pw_uid = atoi(p); */
	p = pwskip(p);
	p[-1] = 0;
	passwd.pw_uid = atou(q);
/*	passwd.pw_gid = atoi(p); */
/*	passwd.pw_quota = 0; */
/*	passwd.pw_comment = ""; */
	q = p;
	p = pwskip(p);
	p[-1] = 0;
#ifdef CORY
	passwd.pw_uid =+ atou(q) << 8;
#endif
#ifdef CC
	passwd.pw_uid =+ atou(q) << 8;
#endif
#ifndef CORY
#ifndef CC
	passwd.pw_gid = atou(q);
#endif
#endif
/*	passwd.pw_gecos = p; */
	p = pwskip(p);
	passwd.pw_dir = p;
	p = pwskip(p);
/*	passwd.pw_shell = p; */
/* 	while(*p && *p != '\n') p++; */
	*p = '\0';
	return(&passwd);
}

atou(p)
	register char *p;
{
	register int i = 0;

	if (p != 0)
		while (*p)
			i = i * 10 + *p++ - '0';
	return (i);
}
