/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)tilde.c	1.1 */

/*
 *  logdir - get login directory given login name
 *
 *   David Korn
 *   AT&T Bell Laboratories
 *   Room 5D-112
 *   Murray Hill, N. J. 07974
 *   Tel. x7975
 *
 *   February, 1983
 */                                              


#include	<stdio.h>
#ifdef KSHELL
#include	"flags.h"
#include	"name.h"
#include	"builtins.h"
extern struct Namnod *bltin_nodes;
#else
#define HOMENOD	"HOME"
#define valup(arg)	getenv(arg)
#endif	/* KSHELL */

#define UNAME	20
#define LOGDIR	64            
static char u_name[UNAME];
static char u_logdir[LOGDIR];

#ifdef BSD
#define strrchr rindex
#endif	/* BSD */
extern char	*strrchr();
extern char	*strcpy();
extern char	*valup();
extern void	setbuf();

static char	*logdir();
static int	passwdent();

/*
 * This routine is used to resolve ~ filenames.
 * If string starts with ~ then ~name is replaced with login directory of name.
 * A ~ by itself is replaced with the users login directory.
 * A ~- is replaced by the last working directory in Shell.
 * If string doesn't start with ~ then NULL returned.
 * If not found then the NULL string is returned.
 */
                                                            
char *tilde(string)
char *string;
{
	register char *sp = string;
	register char *cp;
	register int c;
	if(*sp++!='~')
		return(NULL);
	if((c = *sp)==0 || c=='/')
	{
		return(valup(HOME));
	}
#ifdef KSHELL
	if((c=='-' || c=='+') && ((c= *(sp+1))==0 || c=='/'))
	{
		char *oldpwd;
		if(*sp=='+')
			return(valup(PWDNOD));
		else if(oldpwd=valup(OLDPWDNOD))
			return(oldpwd);
		else
			return(valup(HOME));
	}
#endif	/* KSHELL */
	if((cp=strrchr(sp,'/')) != NULL)
		*cp = 0;
	sp = logdir(sp);
	if(cp)
		*cp = '/';
	return(sp);
}
 

/*
 * This routine returns a pointer to a null-terminated string that
 * contains the login directory of the given <user>.
 * NULL is returned if there is no entry for the given user in the
 * /etc/passwd file or if no room for directory entry.
 * The most recent login directory is saved for future access
 */

static char *logdir(user)
char *user;
{
	if(strcmp(user,u_name))
	{
		if(passwdent(user)<0)
			return(NULL);
	}
	return(u_logdir);
}


/*
 * read the passwd entry for a given <user> and save the uid, gid and home
 */

static int passwdent(user)
char *user;
{
	register char *sp=user;
	register int c;
	register int field=0;
	register FILE *fd;
	int rval = -1;
	int val = 0;
	char buff[BUFSIZ];
	if(strlen(sp)>=UNAME)
		return(-1);
	if((fd=fdopen(open("/etc/passwd",0),"r"))==NULL)
		return(-1);
	setbuf(fd,buff);
	while((c=getc(fd)) !=EOF)
	{
		if(c == ':')
		{
			if(field==5)
				goto good;
			else if(field++ ==0)
			{
				if(*sp)
					field = 10;
				else
					sp = u_logdir;
			}
		}
		else if(c=='\n')
		{
			if(field==5)
				goto good;
			sp = user;
			field = 0;
		}
		else
		{
			switch(field)
			{
				/* match name */
				case 0:
					if(c != *sp++)
						field = 10;
					break;

				case 5:
					*sp++ = c;
					/* see if too big */
					if(sp >= (u_logdir+LOGDIR))
						goto leave;
			}
		}
	}
	while(c != EOF);
	goto leave;
good:
	rval = 0;
	*sp = 0;
	strcpy(u_name,user);
leave:
	setbuf(fd,(char*)0);
	fclose(fd);
	return(rval);
}

