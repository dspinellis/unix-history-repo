/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)expand.c	1.1 */
/*
 *	UNIX shell
 *
 *	S. R. Bourne
 *	Rewritten by David Korn
 *	AT&T Bell Laboratories
 *
 */

#include	<sys/types.h>
#include	<sys/stat.h>
#include	<sys/dir.h>
#include	"defs.h"
#include	"brkincr.h"
#include	"stak.h"
#include	"sym.h"
#include	"shtype.h"

void	rm_files();
int	expand();

extern	STKPTR locstak(),endstak();
extern	FILE *fdopen();
extern	char *movstr();
extern	char *strrchr();
extern	void trim();


/* globals (file name generation)
 *
 * "*" in params matches r.e ".*"
 * "?" in params matches r.e. "."
 * "[...]" in params matches character class
 * "[...a-z...]" in params matches a through z.
 *
 */

static void	addg();

int expand(as,rcnt)
char *as;
{
	int 	count;
#ifdef BSD_4_2
	DIR 	*dirf;
#else
	FILE 	*dirf;
#endif	/* BSD_4_2 */
	BOOL 	nometa=0;
	BOOL 	dir=0;
	char *rescan = 0;
	char *slashsav = 0;
	register char *s, *cs;
	int quote = 0;
	int slash;
	int add_slash = 1;	/* insert a separator slash */
	char *sname;
	ARGPTR 	schain = gchain;
	/* this union forces enough space for the NULL byte */
	union Dirent
	{
		struct direct	entry;
		char entrybuf[sizeof(struct direct)+1]; /* room for null byte */
	};
	union Dirent dirent;
	struct direct	*entry = &dirent.entry;
#ifndef BSD_4_2
	char dirbuff[BUFSIZ];
#endif	/* BSD_4_2 */
	if(trapnote&SIGSET)
		return(0);
	s=cs=as;
#ifndef BSD_4_2
	entry->d_name[DIRSIZ]=0; /* to end the string */
#endif	/* BSD_4_2 */
	/* check for meta chars */
	{
		register int open = 0;
		slash=0;
		do
		{
			switch(*cs++)
			{
				case 0:
				{
					nometa = '/';
					if (rcnt && slash)
						break;
					else
						return(0);
				}

				case '/':
					slash++;
					open = 0;
					continue;

				case '[':
					open++;
					continue;

				case ']':
					if(open)
						break;
					continue;

				case '?':
				case '*':
					if(rcnt > slash)
						continue;
					cs--;
					break;

				case ESCAPE:
					quote++;
					cs++;
				default:
					continue;
			}
			break;
		}
		while(1);
	}
	while(1)
	{
		if(cs==s)
		{
			s=nullstr;
			break;
		}
		else if(*--cs == '/')
		{
			*cs=nometa;
			if(s==cs)
			{
				s= "/";
				add_slash = 0;
			}
			break;
		}
	}
	if(quote && s!=cs)
	{
		s = cpystak(s);
		trim(s);
	}
	/* special case where there are no meta-chars left in path */
	if(nometa)
	{
		/* read permission on directories not needed */
		if(access(s,0)==0)
		{
			addg(s,nullstr,NIL,0);
			return(1);
		}
		return(0);
	}
	sname = (*s?s:dot);
	if(ftype(sname,S_IFMT,S_IFDIR)
#ifdef BSD_4_2
		   && (dirf=opendir(sname))!=NULL)
#else
		   && (dirf=fdopen(open(sname,0),"r"))!=NULL)
#endif	/* BSD_4_2 */
	{
		dir++;
#ifndef BSD_4_2
		setbuf(dirf,dirbuff);
#endif
	}
	count=0;
	if(*cs==0)
		slashsav = cs++;
	if(dir)
		/* check for rescan */
	{
		register char *rs = cs;
		do
		{
			if(*rs=='/')
			{
				rescan=rs;
				*rs=0;
				gchain=0;
			}
		}
		while(*rs++);
#ifdef BSD_4_2
		while((entry=readdir(dirf)) && (trapnote&SIGSET)==0)
#else
		while(fread((char*)entry,sizeof(struct direct),1,dirf)==1 && (trapnote&SIGSET)==0)
#endif	/* BSD_4_2 */
		{
			if(entry->d_ino==0 || (*entry->d_name=='.' && *cs!='.'))
				continue;

			if(gmatch(entry->d_name, cs))
			{
				addg(s,entry->d_name,rescan,add_slash);
				count++;
			}
		}
#ifdef BSD_4_2
		closedir(dirf);
#else
		closefd(dirf);
#endif	/* BSD_4_2 */
		if(rescan)
		{
			register ARGPTR	rchain;
			rchain=gchain; gchain=schain;
			if(count)
			{
				count=0;
				while(rchain)
				{
					count += expand(rchain->argval,slash+1);
					rchain=rchain->argchn;
				}
			}
			*rescan='/';
		}
	}
	if(slashsav)
		*slashsav = '/';
	return(count);
}

static  void addg(as1,as2,as3,add_slash)
char *as1, *as2, *as3;
int add_slash;
{
	register char *s1, *s2;
	register int 	c;
	register ARGPTR argp = (ARGPTR)locstak();
	argp->argflag &= ~(A_MAKE|A_RAW);
	s2 = argp->argval;
	s1=as1;
	/* directory */
	while(c = *s1)
	{
		s1++;
		if(c == ESCAPE)
			*s2++ = ESCAPE;
		*s2++ = c;
	}
	if(add_slash && s1 > as1 && *as2)
		*s2++='/';
	s1=as2;
	/* component */
	while(c = *s1++)
	{
		/* escape the ESCAPE characters */
		if(c == ESCAPE)
			*s2++ = ESCAPE;
		*s2++ = c;
	}
	/* rescan */
	if(s1=as3)
	{
		*s2++='/';
		while(*s2++ = *++s1);
	}
	if(is_option(MARKDIR))
	{
		*s2 = 0;
		if(ftype(argp->argval,S_IFMT,S_IFDIR))
			*s2++ = '/';
	}
	endstak(s2);
	argp->argchn= gchain;
	gchain = argp;
}


/*
 * remove tmp files
 * template of the form /tmp/sh$$.???
 */

void	rm_files(template)
register char *template;
{
	register char *cp;
	ARGPTR schain;
	cp = strrchr(template,'.');
	*(cp+1) = 0;
	f_complete(template,"*");
	schain = gchain;
	while(schain)
	{
		unlink(schain->argval);
		schain = schain->argchn;
	}
}

/*
 * file name completion
 * generate the list of files found by adding an suffix to end of name
 * The number of matches is returned
 */

f_complete(name,suffix)
char *name;
register char *suffix;
{
	register char *cp;
	register char *dp;
	gchain =  NULL;
	dp = (char*)locstak();
	cp = movstr(name,dp);
	if(suffix)
		cp =  movstr(suffix,cp);
	endstak(cp);
	return(expand(dp,0));
}
