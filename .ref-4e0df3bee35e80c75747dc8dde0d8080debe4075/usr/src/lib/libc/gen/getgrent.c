/* @(#)getgrent.c	4.2 (Berkeley) %G% */

#include <stdio.h>
#include <grp.h>

#define	MAXGRP	100

static char GROUP[] = "/etc/group";
static FILE *grf = NULL;
static char line[BUFSIZ+1];
static struct group group;
static char *gr_mem[MAXGRP];

setgrent()
{
	if( !grf )
		grf = fopen( GROUP, "r" );
	else
		rewind( grf );
}

endgrent()
{
	if( grf ){
		fclose( grf );
		grf = NULL;
	}
}

static char *
grskip(p,c)
register char *p;
register c;
{
	while( *p && *p != c ) ++p;
	if( *p ) *p++ = 0;
	return( p );
}

struct group *
getgrent()
{
	register char *p, **q;

	if( !grf && !(grf = fopen( GROUP, "r" )) )
		return(NULL);
	if( !(p = fgets( line, BUFSIZ, grf )) )
		return(NULL);
	group.gr_name = p;
	group.gr_passwd = p = grskip(p,':');
	group.gr_gid = atoi( p = grskip(p,':') );
	group.gr_mem = gr_mem;
	p = grskip(p,':');
	grskip(p,'\n');
	q = gr_mem;
	while( *p ){
		if (q < &gr_mem[MAXGRP-1])
			*q++ = p;
		p = grskip(p,',');
	}
	*q = NULL;
	return( &group );
}
