static	char *sccsid = "@(#)dosys.c	4.1 (Berkeley) 81/02/28";
#include "defs"
#include <signal.h>

dosys(comstring,nohalt)
register char *comstring;
int nohalt;
{
register int status;

if(metas(comstring))
	status = doshell(comstring,nohalt);
else	status = doexec(comstring);

return(status);
}



metas(s)   /* Are there are any  Shell meta-characters? */
register char *s;
{
register char c;

while( (funny[c = *s++] & META) == 0 )
	;
return( c );
}

doshell(comstring,nohalt)
char *comstring;
int nohalt;
{
#ifdef SHELLENV
char *getenv(), *rindex();
char *shellcom = getenv("SHELL");
char *shellstr;
#endif
if((waitpid = vfork()) == 0)
	{
	enbint(SIG_DFL);
	doclose();

#ifdef SHELLENV
	if (shellcom == 0) shellcom = SHELLCOM;
	shellstr = rindex(shellcom, '/') + 1;
	execl(shellcom, shellstr, (nohalt ? "-c" : "-ce"), comstring, 0);
#else
	execl(SHELLCOM, "sh", (nohalt ? "-c" : "-ce"), comstring, 0);
#endif
	fatal("Couldn't load Shell");
	}

return( await() );
}




int intrupt();

await()
{
int status;
register int pid;

enbint(SIG_IGN);
while( (pid = wait(&status)) != waitpid)
	if(pid == -1)
		fatal("bad wait code");
waitpid = 0;
enbint(intrupt);
return(status);
}






doclose()	/* Close open directory files before exec'ing */
{
register struct opendir *od;

for (od = firstod; od; od = od->nxtopendir)
	if (od->dirfc != NULL)
		/* fclose(od->dirfc); */
		close(od->dirfc->_file);
}





doexec(str)
register char *str;
{
register char *t;
char *argv[200];
register char **p;

while( *str==' ' || *str=='\t' )
	++str;
if( *str == '\0' )
	return(-1);	/* no command */

p = argv;
for(t = str ; *t ; )
	{
	*p++ = t;
	while(*t!=' ' && *t!='\t' && *t!='\0')
		++t;
	if(*t)
		for( *t++ = '\0' ; *t==' ' || *t=='\t'  ; ++t)
			;
	}

*p = NULL;

if((waitpid = vfork()) == 0)
	{
	enbint(SIG_DFL);
	doclose();
	enbint(intrupt);
	execvp(str, argv);
	fatal1("Cannot load %s",str);
	}

return( await() );
}

#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>




touch(force, name)
int force;
char *name;
{
struct stat stbuff;
char junk[1];
int fd;

if( stat(name,&stbuff) < 0)
	if(force)
		goto create;
	else
		{
		fprintf(stderr, "touch: file %s does not exist.\n", name);
		return;
		}

if(stbuff.st_size == 0)
	goto create;

if( (fd = open(name, 2)) < 0)
	goto bad;

if( read(fd, junk, 1) < 1)
	{
	close(fd);
	goto bad;
	}
lseek(fd, 0L, 0);
if( write(fd, junk, 1) < 1 )
	{
	close(fd);
	goto bad;
	}
close(fd);
return;

bad:
	fprintf(stderr, "Cannot touch %s\n", name);
	return;

create:
	if( (fd = creat(name, 0666)) < 0)
		goto bad;
	close(fd);
}
