/*
 * Routines to execute other programs.
 * Necessarily very OS dependent.
 */

#include <stdio.h>
#include <signal.h>

#include "less.h"
#include "position.h"

#if __MSDOS__
#include <process.h>
#include <dos.h>
#include <fcntl.h>
#include <io.h>
#include <errno.h>
#include <dir.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
char get_swchar();
void swchar_to_dos();
void swchar_to_unix();
#endif

extern char *getenv();

extern int screen_trashed;
extern IFILE curr_ifile;


/*
 * Pass the specified command to a shell to be executed.
 * Like plain "system()", but handles resetting terminal modes, etc.
 */
	public void
lsystem(cmd)
	char *cmd;
{
	register int inp;
	register char *shell;
	register char *p;
	register char *curr_filename;

	/*
	 * Print the command which is to be executed,
	 * unless the command starts with a "-".
	 */
	if (cmd[0] == '-')
		cmd++;
	else
	{
		lower_left();
		clear_eol();
		putstr("!");
		putstr(cmd);
		putstr("\n");
	}

	/*
	 * Close the current input file.
	 */
	curr_filename = get_filename(curr_ifile);
	(void) edit(NULL, 0);

	/*
	 * De-initialize the terminal and take out of raw mode.
	 */
	deinit();
	flush();	/* Make sure the deinit chars get out */
	raw_mode(0);

	/*
	 * Restore signals to their defaults.
	 */
	init_signals(0);

	/*
	 * Force standard input to be the user's terminal
	 * (the normal standard input), even if less's standard input 
	 * is coming from a pipe.
	 */
#if __MSDOS__
{
	register int inp2;

	inp = dup(0);
	inp2 = open("CON", O_TEXT|O_RDONLY);
	dup2(0,inp2);
}
#else
	inp = dup(0);
	close(0);
	if (open("/dev/tty", 0) < 0)
		dup(inp);
#endif

	/*
	 * Pass the command to the system to be executed.
	 * If we have a SHELL environment variable, use
	 * <$SHELL -c "command"> instead of just <command>.
	 * If the command is empty, just invoke a shell.
	 */
#if __MSDOS__
{
	int result;
	char sw_char;

	sw_char = get_swchar();
	swchar_to_dos();
	result = system(cmd);
	if (result != 0)
		perror("less");
	if (sw_char == '-')
		swchar_to_unix();
}
#else
	p = NULL;
	if ((shell = getenv("SHELL")) != NULL && *shell != '\0')
	{
		if (*cmd == '\0')
			p = save(shell);
		else
		{
			p = (char *) ecalloc(strlen(shell) + strlen(cmd) + 7, 
					sizeof(char));
			sprintf(p, "%s -c \"%s\"", shell, cmd);
		}
	}
	if (p == NULL)
	{
		if (*cmd == '\0')
			p = save("sh");
		else
			p = save(cmd);
	}

	system(p);
	free(p);
#endif

	/*
	 * Restore standard input, reset signals, raw mode, etc.
	 */
#if __MSDOS__
	close(inp2);
	dup2(0,inp);
	close(inp);
#else
	close(0);
	dup(inp);
	close(inp);
#endif

	init_signals(1);
	raw_mode(1);
	init();
	screen_trashed = 1;

	/*
	 * Reopen the current input file.
	 */
	(void) edit(curr_filename, 0);

#if defined(SIGWINCH) || defined(SIGWIND)
	/*
	 * Since we were ignoring window change signals while we executed
	 * the system command, we must assume the window changed.
	 * Warning: this leaves a signal pending (in "sigs"),
	 * so psignals() should be called soon after lsystem().
	 */
	winch();
#endif
}

#if PIPEC

/*
 * Pipe a section of the input file into the given shell command.
 * The section to be piped is the section "between" the current
 * position and the position marked by the given letter.
 *
 * The "current" position means the top line displayed if the mark
 * is after the current screen, or the bottom line displayed if
 * the mark is before the current screen.
 * If the mark is on the current screen, the whole screen is displayed.
 */
	public int
pipe_mark(c, cmd)
	int c;
	char *cmd;
{
	POSITION mpos, tpos, bpos;

	/*
	 * mpos = the marked position.
	 * tpos = top of screen.
	 * bpos = bottom of screen.
	 */
	mpos = markpos(c);
	if (mpos == NULL_POSITION)
		return (-1);
	tpos = position(TOP);
	if (tpos == NULL_POSITION)
		tpos = ch_zero();
	bpos = position(BOTTOM);

 	if (c == '.') 
 		return (pipe_data(cmd, tpos, bpos));
 	else if (mpos <= tpos)
 		return (pipe_data(cmd, mpos, tpos));
 	else if (bpos == NULL_POSITION)
 		return (pipe_data(cmd, tpos, bpos));
 	else
 		return (pipe_data(cmd, tpos, mpos));
}

/*
 * Create a pipe to the given shell command.
 * Feed it the file contents between the positions spos and epos.
 */
	public int
pipe_data(cmd, spos, epos)
	char *cmd;
	POSITION spos;
	POSITION epos;
{
	register FILE *f;
	register int c;
	extern FILE *popen();

	/*
	 * This is structured much like lsystem().
	 * Since we're running a shell program, we must be careful
	 * to perform the necessary deinitialization before running
	 * the command, and reinitialization after it.
	 */
	if (ch_seek(spos) != 0)
	{
		error("Cannot seek to start position", NULL_PARG);
		return (-1);
	}

	if ((f = popen(cmd, "w")) == NULL)
	{
		error("Cannot create pipe", NULL_PARG);
		return (-1);
	}
	lower_left();
	clear_eol();
	putstr("!");
	putstr(cmd);
	putstr("\n");

	deinit();
	flush();
	raw_mode(0);
	init_signals(0);
#ifdef SIGPIPE
	SIGNAL(SIGPIPE, SIG_IGN);
#endif

	while (epos == NULL_POSITION || spos++ <= epos)
	{
		/*
		 * Read a character from the file and give it to the pipe.
		 */
		c = ch_forw_get();
		if (c == EOI)
			break;
		if (putc(c, f) == EOF)
			break;
	}

	/*
	 * Finish up the last line.
	 */
 	while (c != '\n' && c != EOI ) 
 	{
 		c = ch_forw_get();
 		if (c == EOI)
 			break;
 		if (putc(c, f) == EOF)
 			break;
 	}

	pclose(f);

#ifdef SIGPIPE
	SIGNAL(SIGPIPE, SIG_DFL);
#endif
	init_signals(1);
	raw_mode(1);
	init();
	screen_trashed = 1;
#if defined(SIGWINCH) || defined(SIGWIND)
	/* {{ Probably don't need this here. }} */
	winch();
#endif
	return (0);
}

#endif
