/*
 * function system() - slightly modified from sources dLibs 1.2
 * - a freely distributable C library for Atari ST.
 * Authors: Dale Schumacher and John Stanley, I believe.
 * Changes for gcc compiler and gnulib.olb - Michal Jaegermann
 */

#include <osbind.h>
#include <stdio.h>
#include <string.h>
#include <basepage.h>
#ifdef __GNUC__
#include <process.h>
#define ERROR 2
#endif

/* #define DEBUG  */
#ifdef DEBUG
#define _COOKIE(x) puts(x);putchar('\n')
#endif

void static
parse_args(char *cmdln, register char **argv)
{
	register char *p;
	static char delim[] = " \t\r\n";

	if(p = strtok(cmdln, delim)) {
		do {
			*argv++ = p;
		} while(p = strtok(NULL, delim));
	}
}

#ifdef __GNUC__
/* this is used by assembler statement to keep a copy of registers */
static volatile long savearea[16];
#endif

int
system(const char *command)
{
	register char *p;
	register int (*shell)();
#ifndef __GNUC__
	char rv[2];
#endif
	char cmdln[1024];
	char *args[64];
	char *getenv(const char *);

	if(!command)
		return(ERROR);

	/* get _shell_p value */
	p = (char *) Super(0L);  /* supervisor mode */
	shell = (int (*)()) *((long *) 0x4F6L);
	(void) Super(p);	 /* restore user mode */

	/* validate _shell_p */
	if((shell) &&				/* Shell available. */
	   (((long) shell) < ((long) _base)) &&	/* Reasonable shell pointer. */
	   (strncmp((char *)shell, "PATH", 4)))	/* Not corrupted */
		{
#ifdef __GNUC__
		int ret;
#endif
		/* execute the command */
#ifdef DEBUG
_COOKIE("system: using _shell_p");
printf("'shell' got value 0x%08lx\n", (long)shell);
#endif
/* a bit of paranoia caused by some misbehaving programs */
#ifdef __GNUC__
asm("moveml d1-d7/a0-a7,_savearea");	
		ret = (*shell)(command);
asm("moveml _savearea,d1-d7/a0-a7");
		return (ret);
#else
		return ((*shell)(command));
#endif
		}

	strcpy(cmdln, command);	/* copy the command line for parsing */

	if((p = getenv("SHELL")) && (*p))	/* SHELL= variable? */
		{
		args[0] = p;
		parse_args(cmdln, (args + 1));
#ifdef DEBUG
_COOKIE("system: executing SHELL");
_COOKIE(p);
#endif
		}
	else	/* attempt to find first token as a program on the path */
		{
		parse_args(cmdln, args);
		p = args[0];
#ifdef DEBUG
_COOKIE("system: directly executing program");
_COOKIE(p);
#endif
		}

#ifdef __GNUC__
	return(spawnvp(0, p, args));
#else   /* original from dLibs */
	forkvpe(p, args, NULL);
	wait(rv);
	return((rv[1] == 0) ? rv[0] : rv[1]);
#endif
}
