/* shell.c */

/* Author:
 *	Guntram Blohm
 *	Buchenstrasse 19
 *	7904 Erbach, West Germany
 *	Tel. ++49-7305-6997
 *	sorry - no regular network connection
 */

/*
 * This file contains a minimal version of a shell for TOS. It allows the
 * setting of an environment, calling programs, and exiting.
 * If you don't have another one, this might be sufficient, but you should 
 * prefer *any* other shell.
 * You may, however, want to set your SHELL environment variable to this
 * shell: it implements the -c switch, which is required by Elvis, and
 * not supported by most other atari shells.
 */
 
#include <stdio.h>
#include <string.h>
#include <osbind.h>
extern char *getenv(), *malloc();
extern char **environ;
long _stksize=16384;

#define	MAXENV	50

struct
{
	char *name;
	char *value;
} myenv[MAXENV];

int cmd_set(), cmd_exit();

struct buildins
{
	char *name;
	int (*func)();
} buildins[]=
{	"exit", cmd_exit,
	"set", cmd_set,
	0,
};

main(argc, argv)
	int argc;
	char **argv;
{
	char buf[128];
	int i;

	for (i=0; environ[i] && strncmp(environ[i],"ARGV=",5); i++)
		cmd_set(environ[i]);
	script("profile.sh");

	if (argc>1 && !strcmp(argv[1], "-c"))
	{
		buf[0]='\0';
		for (i=2; i<argc; i++)
		{	if (i>2)
				strcat(buf, " ");
			strcat(buf, argv[i]);
		}
		execute(buf);
	}
	else
		while (fputs("$ ", stdout), gets(buf))
			execute(buf);
}

execute(buf)
	char *buf;
{
	char *scan=buf;
	char cmd[80];
	char line[128];
	char env[4096], *ep=env;
	int i;

	while (*scan==' ')
		scan++;
	if (!*scan)
		return;
	while (*scan && *scan!=' ')
		scan++;
	if (*scan)
		*scan++='\0';

	for (i=0; buildins[i].name; i++)
		if (!strcmp(buf, buildins[i].name))
			return (*buildins[i].func)(scan);

	if (!searchpath(buf, cmd))
	{	printf("%s: not found\n", buf);
		return -1;
	}

	strcpy(line+1, scan);
	line[0]=strlen(scan);
	for (i=0; i<MAXENV && myenv[i].name; i++)
	{	strcpy(ep, myenv[i].name);
		strcat(ep, "=");
		strcat(ep, myenv[i].value);
		ep+=strlen(ep)+1;
	}
	
	*ep='\0';

	return Pexec(0, cmd, line, env);
}

searchpath(from, to)
	char *from, *to;
{
	char *path="";
	char *scan;
	char *end;
	char *q;
	int i;

	for (i=0; i<MAXENV && myenv[i].name; i++)
		if (!strcmp(myenv[i].name,"PATH"))
			path=myenv[i].value;
	for (scan=from; *scan; scan++)
		if (*scan==':' || *scan=='\\')
		{	path=0;
			break;
		}
	if (!path)
	{	strcpy(to, from);
		end=to+strlen(to);
		strcpy(end, ".prg"); if (try(to)) return 1;
		strcpy(end, ".ttp"); if (try(to)) return 1;
		strcpy(end, ".tos"); if (try(to)) return 1;
		*to='\0'; return 0;
	}
	for (scan=path; *scan; )
	{
		for (q=to; *scan && *scan!=';' && *scan!=','; scan++)
			*q++=*scan;
		if (*scan==';' || *scan==',')
			scan++;
		*q++='\\';
		*q='\0';
		strcpy(q, from);
		end=q+strlen(q);
		strcpy(end, ".prg"); if (try(to)) return 1;
		strcpy(end, ".ttp"); if (try(to)) return 1;
		strcpy(end, ".tos"); if (try(to)) return 1;
	}
	*to='\0';
	return 0;
}

try(name)
	char *name;
{
	if (Fattrib(name, 0, 0) < 0)
		return 0;
	return 1;
}

cmd_exit()
{
	exit(0);
}

cmd_set(line)
	char *line;
{
	char *value;
	int i;

	if (!*line)
	{
		for (i=0; i<MAXENV && myenv[i].name; i++)
			printf("%s=%s\n", myenv[i].name, myenv[i].value);
		return 0;
	}

	for (value=line; *value && *value!='='; value++)
		;
	if (!*value)
	{	printf("Usage: set name=var\n");
		return -1;
	}
	*value++='\0';
	doset(line, value);
}

doset(line, value)
	char *line, *value;
{
	int i;

	for (i=0; i<MAXENV && myenv[i].name && strcmp(myenv[i].name, line); i++)
		;
	if (i==MAXENV)
	{	printf("No Space\n");
		return -1;
	}
	if (!myenv[i].name)
	{	myenv[i].name=malloc(strlen(line)+1);
		strcpy(myenv[i].name, line);
	}
	if (myenv[i].value)
		free(myenv[i].value);
	myenv[i].value=malloc(strlen(value)+1);
	strcpy(myenv[i].value, value);
	return 0;
}

script(name)
	char *name;
{
	FILE *fp;
	char buf[128], *p;

	if ((fp=fopen(name, "r"))==0)
		return;
	while (fgets(buf, sizeof buf, fp))
	{
		if ((p=strchr(buf, '\n'))!=0)
			*p='\0';
		execute(buf);
	}
	fclose(fp);
}
