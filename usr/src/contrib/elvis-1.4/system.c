/* system.c  -- UNIX version */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains a new version of the system() function and related stuff.
 *
 * Entry points are:
 *	system(cmd)	- run a single shell command
 *	wildcard(names)	- expand wildcard characters in filanames
 *	filter(m,n,cmd)	- run text lines through a filter program
 *
 * This is probably the single least portable file in the program.  The code
 * shown here should work correctly if it links at all; it will work on UNIX
 * and any O.S./Compiler combination which adheres to UNIX forking conventions.
 */

#include "config.h"
#include "vi.h"
#include <signal.h>
extern char	**environ;

#if ANY_UNIX

/* This is a new version of the system() function.  The only difference
 * between this one and the library one is: this one uses the o_shell option.
 */
int system(cmd)
	char	*cmd;	/* a command to run */
{
	int	status;	/* exit status of the command */

	/* warn the user if the file hasn't been saved yet */
	if (*o_warn && tstflag(file, MODIFIED))
	{
		if (mode == MODE_VI)
		{
			mode = MODE_COLON;
		}
		msg("Warning: \"%s\" has been modified but not yet saved", origname);
	}

	signal(SIGINT, SIG_IGN);
	switch (fork())
	{
	  case -1:						/* error */
		msg("fork() failed");
		status = -1;
		break;

	  case 0:						/* child */
		/* for the child, close all files except stdin/out/err */
		for (status = 3; status < 60 && (close(status), errno != EINVAL); status++)
		{
		}

		signal(SIGINT, SIG_DFL);
		if (cmd == o_shell)
		{
			execle(o_shell, o_shell, (char *)0, environ);
		}
		else
		{
			execle(o_shell, o_shell, "-c", cmd, (char *)0, environ);
		}
		msg("execle(\"%s\", ...) failed", o_shell);
		exit(1); /* if we get here, the exec failed */

	  default:						/* parent */
		wait(&status);
		signal(SIGINT, trapint);
	}

	return status;
}

/* This private function opens a pipe from a filter.  It is similar to the
 * system() function above, and to popen(cmd, "r").
 */
static int rpipe(cmd, in)
	char	*cmd;	/* the filter command to use */
	int	in;	/* the fd to use for stdin */
{
	int	r0w1[2];/* the pipe fd's */

	/* make the pipe */
	if (pipe(r0w1) < 0)
	{
		return -1;	/* pipe failed */
	}

	/* The parent process (elvis) ignores signals while the filter runs.
	 * The child (the filter program) will reset this, so that it can
	 * catch the signal.
	 */
	signal(SIGINT, SIG_IGN);

	switch (fork())
	{
	  case -1:						/* error */
		return -1;

	  case 0:						/* child */
		/* close the "read" end of the pipe */
		close(r0w1[0]);

		/* redirect stdout to go to the "write" end of the pipe */
		close(1);
		dup(r0w1[1]);
		close(2);
		dup(r0w1[1]);
		close(r0w1[1]);

		/* redirect stdin */
		if (in != 0)
		{
			close(0);
			dup(in);
			close(in);
		}

		/* the filter should accept SIGINT signals */
		signal(SIGINT, SIG_DFL);

		/* exec the shell to run the command */
		execle(o_shell, o_shell, "-c", cmd, (char *)0, environ);
		exit(1); /* if we get here, exec failed */

	  default:						/* parent */
		/* close the "write" end of the pipe */	
		close(r0w1[1]);

		return r0w1[0];
	}
}

#endif /* non-DOS */

#if OSK

/* This private function opens a pipe from a filter.  It is similar to the
 * system() function above, and to popen(cmd, "r").
 */
static int rpipe(cmd, in)
	char	*cmd;	/* the filter command to use */
	int	in;	/* the fd to use for stdin */
{

    char **argblk;
	char *p, *buffer, *command;
	int stdinp, stdoutp;
	unsigned addstack = 0;
	int words, len, loop = 1;
	int fp, pipe_pid;
	extern int os9forkc();
	extern char *index();

	command = cmd;
	words = 0;
	if ((buffer = (char*) malloc(strlen(cmd))) == (char*) 0)
		return 0;

	do {
		if (!(p = index(command, ' '))) {
			loop--;
			len = strlen(command);
		}
		else
			len = p - command;
		words++;	
		while (command[len] && command[len] == ' ')
			len++;
		if (!command[len])
			break;
		command = command + len;
	}	
	while (loop);
	if ((argblk = (char **)malloc((words+1) * sizeof(char*))) == (char **)0)
		return 0;
	command = cmd;
	words = 0;
	do {
		if (!(p = index(command, ' '))) {
			loop--;
			len = strlen(command);
		}
		else
			len = p - command;
		strncpy(buffer, command, len);
		argblk[words++] = buffer;
		buffer += len;
		*buffer++ = '\0';
		while (command[len] && command[len] == ' ')
			len++;
		if (!command[len])
			break;
		command = command + len;
	} while (loop);
	if (argblk[words - 1][0] == '#') 
		addstack = 1024 * atoi(&argblk[--words][1]);
	argblk[words] = 0;

	stdoutp = dup(1);
	close(1);               /* close stdout */
	if ((fp = open("/pipe",S_IREAD)) < 0) {
		dup(stdoutp);
		close(stdoutp);
		return 0;
	}
	if (in != 0) {
		stdinp = dup(0);
		close(0);
		dup(in);
		close(in);
	}
	pipe_pid = os9exec(os9forkc,argblk[0],argblk,environ,addstack,0,3);
	if (pipe_pid == -1) {
		fclose(fp);
		dup(stdoutp);
		close(stdoutp);
		if (in != 0) {
			close(0);
			dup(stdinp);
		}
		return 0;
	}
	fp = (short)dup(1);     /* save pipe */
	close(1);               /* get rid of the pipe */
	dup(stdoutp);           /* restore old stdout */
	close(stdoutp);         /* close path to stdout copy */
	if (in != 0) {
		close(0);
		dup(stdinp);
	}
	return fp;
}	
#endif

#if ANY_UNIX || OSK

/* This function closes the pipe opened by rpipe(), and returns 0 for success */
static int rpclose(fd)
	int	fd;
{
	int	status;

	close(fd);
	wait(&status);
	signal(SIGINT, trapint);
	return status;
}

#endif /* non-DOS */

/* This function expands wildcards in a filename or filenames.  It does this
 * by running the "echo" command on the filenames via the shell; it is assumed
 * that the shell will expand the names for you.  If for any reason it can't
 * run echo, then it returns the names unmodified.
 */

#if MSDOS || TOS
#define	PROG	"wildcard "
#define	PROGLEN	9
#include <string.h>
#else
#define	PROG	"echo "
#define	PROGLEN	5
#endif

char *wildcard(names)
	char	*names;
{
	int	i, j, fd;
	REG char *s, *d;


	/* build the echo command */
	if (names != tmpblk.c)
	{
		/* the names aren't in tmpblk.c, so we can do it the easy way */
		strcpy(tmpblk.c, PROG);
		strcat(tmpblk.c, names);
	}
	else
	{
		/* the names are already in tmpblk.c, so shift them to make
		 * room for the word "echo "
		 */
		for (s = names + strlen(names) + 1, d = s + PROGLEN; s > names; )
		{
			*--d = *--s;
		}
		strncpy(names, PROG, PROGLEN);
	}

	/* run the command & read the resulting names */
	fd = rpipe(tmpblk.c, 0);
	if (fd < 0) return names;
	i = 0;
	do
	{
		j = tread(fd, tmpblk.c + i, BLKSIZE - i);
		i += j;
	} while (j > 0);

	/* successful? */
	if (rpclose(fd) == 0 && j == 0 && i < BLKSIZE && i > 0)
	{
		tmpblk.c[i-1] = '\0'; /* "i-1" so we clip off the newline */
		return tmpblk.c;
	}
	else
	{
		return names;
	}
}

/* This function runs a range of lines through a filter program, and replaces
 * the original text with the filtered version.  As a special case, if "to"
 * is MARK_UNSET, then it runs the filter program with stdin coming from
 * /dev/null, and inserts any output lines.
 */
int filter(from, to, cmd)
	MARK	from, to;	/* the range of lines to filter */
	char	*cmd;		/* the filter command */
{
	int	scratch;	/* fd of the scratch file */
	int	fd;		/* fd of the pipe from the filter */
	char	scrout[50];	/* name of the scratch out file */
	MARK	new;		/* place where new text should go */
	int	i;

	/* write the lines (if specified) to a temp file */
	if (to)
	{
		/* we have lines */
#if MSDOS || TOS
		strcpy(scrout, o_directory);
		if ((i=strlen(scrout)) && strchr("\\/:", scrout[i-1]))
			scrout[i++]=SLASH;
		strcpy(scrout+i, SCRATCHOUT+3);
#else
		sprintf(scrout, SCRATCHOUT, o_directory);
#endif
		mktemp(scrout);
		cmd_write(from, to, CMD_BANG, 0, scrout);

		/* use those lines as stdin */
		scratch = open(scrout, O_RDONLY);
		if (scratch < 0)
		{
			unlink(scrout);
			return -1;
		}
	}
	else
	{
		scratch = 0;
	}

	/* start the filter program */
	fd = rpipe(cmd, scratch);
	if (fd < 0)
	{
		if (to)
		{
			close(scratch);
			unlink(scrout);
		}
		return -1;
	}

	ChangeText
	{
		/* adjust MARKs for whole lines, and set "new" */
		from &= ~(BLKSIZE - 1);
		if (to)
		{
			to &= ~(BLKSIZE - 1);
			to += BLKSIZE;
			new = to;
		}
		else
		{
			new = from + BLKSIZE;
		}

		/* repeatedly read in new text and add it */
		while ((i = tread(fd, tmpblk.c, BLKSIZE - 1)) > 0)
		{
			tmpblk.c[i] = '\0';
			add(new, tmpblk.c);
			for (i = 0; tmpblk.c[i]; i++)
			{
				if (tmpblk.c[i] == '\n')
				{
					new = (new & ~(BLKSIZE - 1)) + BLKSIZE;
				}
				else
				{
					new++;
				}
			}
		}
	}

	/* delete old text, if any */
	if (to)
	{
		delete(from, to);
	}

	/* Reporting... */
	rptlabel = "more";
	if (rptlines < 0)
	{
		rptlines = -rptlines;
		rptlabel = "less";
	}

	/* cleanup */
	rpclose(fd);
	if (to)
	{
		close(scratch);
		unlink(scrout);
	}
	return 0;
}
