/* added real/protect mode branch at runtime and real mode version
 * names changed for perl
 * Kai Uwe Rommel
 */

/*
Several people in the past have asked about having Unix-like pipe
calls in OS/2.  The following source file, adapted from 4.3 BSD Unix,
uses a #define to give you a pipe(2) call, and contains function
definitions for popen(3) and pclose(3).  Anyone with problems should
send mail to me; they seem to work fine.

Mark Towfigh
Racal Interlan, Inc.
----------------------------------cut-here------------------------------------
*/

/*
 * The following code segment is derived from BSD 4.3 Unix.  See
 * copyright below.  Any bugs, questions, improvements, or problems
 * should be sent to Mark Towfigh (towfiq@interlan.interlan.com).
 *
 * Racal InterLan Inc.
 */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <string.h>
#include <process.h>
#include <errno.h>

#define INCL_NOPM
#define	INCL_DOS
#include <os2.h>

static FILE *dos_popen(const char *cmd, const char *flags);
static int dos_pclose(FILE *pipe);

/*
 * emulate Unix pipe(2) call
 */

#define	tst(a,b)	(*mode == 'r'? (b) : (a))
#define READH           0
#define WRITEH          1

static  int       popen_pid[20];

FILE *mypopen(char *cmd, char *mode)
{
	int p[2];
        register myside, hisside, save_stream;
        char *shell = getenv("COMPSPEC");

        if ( shell == NULL )
          shell = "C:\\OS2\\CMD.EXE";

        if ( _osmode == DOS_MODE )
          return dos_popen(cmd, mode);

        if ( _pipe(p, 4096, 0) )
                return NULL;

        myside = tst(p[WRITEH], p[READH]);
        hisside = tst(p[READH], p[WRITEH]);

	/* set up file descriptors for remote function */
	save_stream = dup(tst(0, 1));		/* don't lose stdin/out! */
        if (dup2(hisside, tst(0, 1)) < 0)
        {
		perror("dup2");
		return NULL;
	}
        close(hisside);

	/*
	 * make sure that we can close our side of the pipe, by
	 * preventing it from being inherited!
	 */

	/* set no-inheritance flag */
	DosSetFHandState(myside, OPEN_FLAGS_NOINHERIT);

	/* execute the command:  it will inherit our other file descriptors */
        popen_pid[myside] = spawnlp(P_NOWAIT, shell, shell, "/C", cmd, NULL);

	/* now restore our previous file descriptors */
        if (dup2(save_stream, tst(0, 1)) < 0)   /* retrieve stdin/out */
        {
		perror("dup2");
		return NULL;
	}
        close(save_stream);

	return fdopen(myside, mode);		/* return a FILE pointer */
}

int mypclose(FILE *ptr)
{
	register f;
        int status;

        if ( _osmode == DOS_MODE )
          return dos_pclose(ptr);

	f = fileno(ptr);
        fclose(ptr);

	/* wait for process to terminate */
	cwait(&status, popen_pid[f], WAIT_GRANDCHILD);

	return status;
}


int pipe(int *filedes)
{
  int res;

  if ( res = _pipe(filedes, 4096, 0) )
    return res;

  DosSetFHandState(filedes[0], OPEN_FLAGS_NOINHERIT);
  DosSetFHandState(filedes[1], OPEN_FLAGS_NOINHERIT);
  return 0;
}


/* this is the MS-DOS version */

typedef enum { unopened = 0, reading, writing } pipemode;

static struct
{
    char *name;
    char *command;
    pipemode pmode;
}
pipes[_NFILE];

static FILE *dos_popen(const char *command, const char *mode)
{
    FILE *current;
    char name[128];
    char *tmp = getenv("TMP");
    int cur;
    pipemode curmode;

    /*
    ** decide on mode.
    */
    if(strchr(mode, 'r') != NULL)
        curmode = reading;
    else if(strchr(mode, 'w') != NULL)
        curmode = writing;
    else
        return NULL;

    /*
    ** get a name to use.
    */
    strcpy(name, tmp ? tmp : "\\");
    if ( name[strlen(name) - 1] != '\\' )
      strcat(name, "\\");
    strcat(name, "piXXXXXX");
    mktemp(name);

    /*
    ** If we're reading, just call system to get a file filled with
    ** output.
    */
    if(curmode == reading)
    {
        char cmd[256];
        sprintf(cmd,"%s > %s", command, name);
        system(cmd);

        if((current = fopen(name, mode)) == NULL)
            return NULL;
    }
    else
    {
        if((current = fopen(name, mode)) == NULL)
            return NULL;
    }

    cur = fileno(current);
    pipes[cur].name = strdup(name);
    pipes[cur].command = strdup(command);
    pipes[cur].pmode = curmode;

    return current;
}

static int dos_pclose(FILE * current)
{
    int cur = fileno(current), rval;
    char command[256];

    /*
    ** check for an open file.
    */
    if(pipes[cur].pmode == unopened)
        return -1;

    if(pipes[cur].pmode == reading)
    {
        /*
        ** input pipes are just files we're done with.
        */
        rval = fclose(current);
        unlink(pipes[cur].name);
    }
    else
    {
        /*
        ** output pipes are temporary files we have
        ** to cram down the throats of programs.
        */
        fclose(current);
        sprintf(command,"%s < %s", pipes[cur].command, pipes[cur].name);
        rval = system(command);
        unlink(pipes[cur].name);
    }

    /*
    ** clean up current pipe.
    */
    free(pipes[cur].name);
    free(pipes[cur].command);
    pipes[cur].pmode = unopened;

    return rval;
}
