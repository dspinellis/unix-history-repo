#ifndef lint
static char sccsid[] = "@(#)mysys.c	4.6	(Berkeley)	%G%";
#endif not lint

#include <sys/signal.h>
#include "stdio.h"
#include "pathnames.h"

#define	EASY	1
#define	MEDIUM	2
#define	HARD	3
#define	EMAX	256

char *envp[EMAX+1];

/*
 * This routine edits the PATH environment variable so that
 * special commands that learners may need will be found.
 * EXINIT is modified so that the editor will always prompt,
 * will not print \r's, and will be usable with open mode.
 */

chgenv()
{
	register char **p;
	register int i;
	extern char **environ;
	extern char *direct;
	char path[BUFSIZ], exinit[BUFSIZ];
	char *malloc();

	sprintf(path, _PATH_DEFPATH, direct);
	sprintf(exinit, "EXINIT=set prompt noopt open window=23");
#if BSD4_2
	system("stty old");
	for (p=environ,i=3; *p != 0 && i < EMAX; p++,i++)   {
#else
	for (p=environ,i=2; *p != 0 && i < EMAX; p++,i++)   {
#endif
		envp[i] = *p;
		if (**p != 'P' && **p != 'E')
			continue;
		if (strncmp(*p, "PATH=", 5) == 0)
			sprintf(path, "PATH=%s/bin:%s", direct, &envp[i--][5]);
		else if (strncmp(*p, "EXINIT=", 7) == 0)
			sprintf(exinit, "%s|set prompt noopt open window=23", envp[i--]);
#if BSD4_2
		else if (strncmp(*p, "PS1=", 4) == 0)
			i--;
	}
	envp[2] = malloc(7);
	strcpy(envp[2], "PS1=% ");
#else
	}
#endif
	envp[0] = malloc(strlen(path) + 1);
	strcpy(envp[0], path);
	envp[1] = malloc(strlen(exinit) + 1);
	strcpy(envp[1], exinit);
	envp[i] = 0;
	environ = envp;
}

mysys(s)
char *s;
{
	/* like "system" but rips off "mv", etc.*/
	/* also tries to guess if can get away with exec cmd */
	/* instead of sh cmd */
	char p[300];
	char *np[40];
	register char *t;
	int nv, type, stat;

	type = EASY;	/* we hope */
	for (t = s; *t && type != HARD; t++) {
		switch (*t) {
		case '*': 
		case '[': 
		case '?': 
		case '>': 
		case '<': 
		case '$':
		case '\'':
		case '"':
		case '`':
		case '{':
		case '~':
			type = MEDIUM;
			break;
		case '|': 
		case ';': 
		case '&':
			type = HARD;
			break;
		}
	}
	switch (type) {
	case HARD:
		return(system(s));
	case MEDIUM:
		strcpy(p, "exec ");
		strcat(p, s);
		return(system(p));
	case EASY:
		strcpy(p,s);
		nv = getargs(p, np);
		t=np[0];
		if ((strcmp(t, "mv") == 0)||
		    (strcmp(t, "cp") == 0)||
		    (strcmp(t, "rm") == 0)||
		    (strcmp(t, "ls") == 0) ) {
			if (fork() == 0) {
				char b[100];
				signal(SIGINT, SIG_DFL);
				np[nv] = 0;
				execvp(t, np);
				perror(t);
				fprintf(stderr, "Mysys:  execv failed on %s\n", np);
				exit(1);
			}
			wait(&stat);
			return(stat);
		}
		return(system(s));
	}
}

/*
 * system():
 *	same as library version, except that resets
 *	default handling of signals in child, so that
 *	user gets the behavior he expects.
 */

system(s)
char *s;
{
	int status, pid, w;
	register int (*istat)(), (*qstat)();

	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	if ((pid = fork()) == 0) {
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		execl(_PATH_CSHELL, "csh", "-cf", s, 0);
		_exit(127);
	}
	while ((w = wait(&status)) != pid && w != -1)
		;
	if (w == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	return(status);
}

getargs(s, v)
char *s, **v;
{
	int i;

	i = 0;
	for (;;) {
		v[i++]=s;
		while (*s != 0 && *s!=' '&& *s != '\t')
			s++;
		if (*s == 0)
			break;
		*s++ =0;
		while (*s == ' ' || *s == '\t')
			s++;
		if (*s == 0)
			break;
	}
	return(i);
}
