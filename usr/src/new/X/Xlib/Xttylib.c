#ifndef lint
static char *rcsid_Xttylib_c = "$Header: Xttylib.c,v 10.7 86/04/22 15:18:10 jg Rel $";
#endif
/* This version has a single reverse-video argument instead of colors.  It
   really only works well on monochrome displays */

/* Library of routines to create a terminal emulator window
 *
 * Routines in this library are:
 *
 *	CreateTTYWindow		Creates a new instance of a terminal window
 *	DestroyTTYWindow	Destroys a terminal window
 *	TTYPutString		Puts a string in a terminal window
 *	TTYPutChar		Puts a character in a terminal window
 *	TTYPrintf		Does a printf in a terminal window
 *	TTYGetString		Gets s string from a terminal window
 *	TTYGetChar		Gets a char from a terminal window
 *	SetStdout		Flushes stdout and assigns it to a window
 *	ResetStdout		Resets stdout to its inital value
 *
 * The terminal window created responds to exactly the same character
 * sequences as xterm (not surprising).  Creating a window automatically
 * maps it
 *
 * These routines pass around a pointer to a TTYWindow:
 *
 * typedef struct _TTYWindow {
 * 	Window w;		The window id
 *	int pid;		The pid of the subprocess xterm
 *	short file;		The file id of the tty to read/write characters to/from
 * } TTYWindow;
 *
 *
 * The SetStdout routine is highly useful in conjunction with curses
 * since curses always writes to stdout.
 */

#include <X/Xlib.h>
#include <stdio.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sgtty.h>
#include "Xtty.h"

TTYWindow *CreateTTYWindow(cols, lines, x, y, normFont, boldFont, bwidth,
		reverse)
	int lines, cols, x, y, bwidth, reverse;
	char *normFont, *boldFont;
{
	TTYWindow *t;

	if ((t = (TTYWindow *) malloc(sizeof(TTYWindow))) ==
		NULL) return NULL;

	if (Start_slave_xterm(t, lines, cols, x, y, normFont, boldFont,
		bwidth, reverse) == 0) {
	    free((char *)t);
	    fprintf(stderr, "Couldn't start slave xterm\n");
	    return NULL;
	}

	return t;
}

int ttyMasterPty;
int keepMasterOpen = 0;

int Start_slave_xterm(t, lines, cols, x, y, normFont, boldFont, bwidth,
		reverse)
	TTYWindow *t;
	int lines, cols, x, y, bwidth, reverse;
	char *normFont, *boldFont;
{
#define BUFSIZE 20

	char ttyName[BUFSIZE];
	char Sbuf[BUFSIZE], sizeBuf[BUFSIZE], wdBuf[BUFSIZE],
		inputBuffer[BUFSIZE];
	int bytesRead, len;

	if (boldFont == NULL) boldFont = normFont;

	ttyMasterPty = GetPty(ttyName);
	if (ttyMasterPty == -1) return 0;

	if ((t->pid = vfork()) < 0) return 0;

	if (t->pid == 0) {
	    sprintf(Sbuf, "-S%c%c%d", ttyName[8], ttyName[9], ttyMasterPty);
	    sprintf(sizeBuf, "=%dx%d+%d+%d", cols, lines, x, y);
	    sprintf(wdBuf, "%d", bwidth);

	    execlp("xterm", "xterm", Sbuf, "-fn", normFont, "-fb", boldFont,
		    sizeBuf, "-bw", wdBuf, reverse ? "-rv" : (char *) 0,
		    (char *) 0);

	} else {
	    if (!keepMasterOpen) close(ttyMasterPty);

	    /* Open the slave end of the pty */

	    ttyName[5] = 't';	/* Change /dev/pty?? to /dev/tty?? */

	    t->file = open(ttyName, O_RDWR, 0777);
	    
	    if (t->file < 0) {
		/* Couldn't open the tty--better get rid of the process */
		kill (t->pid, SIGINT);
		return 0;
	    }

	    /* Read the windowid from the pty */

	    len = 0;
	    while ((bytesRead = read(t->file, inputBuffer + len,
		    sizeof(Window) - len)) > 0) len += bytesRead;

	    /* Flush the rest of the garbahge */

	    ioctl(t->file, TIOCFLUSH, (struct sgttyb *) NULL);

	    /* the data consists of a binary window ID */

	    t->w = *(Window *) inputBuffer;
	}
	return 1;
#undef BUFSIZE
}

int GetPty(name)
	char *name;
{
	register int devindex, letter;
	int fd;

	strcpy(name, "/dev/ptyp0");

	for (letter = 0; letter < 4; letter++) {
	    name[8] = "pqrs"[letter];
	    
	    for (devindex = 0; devindex < 16; devindex++) {
		name[9] = "0123456789abcdef"[devindex];
		if ((fd = open (name, O_RDWR)) >= 0) return fd;
	    }
	}
	
	return -1;
}	

DestroyTTYWindow(t)
	TTYWindow *t;
{
	/* close the tty; this should cause the xterm to terminate with an I/O error */
	close(t->file);  
	free((char *)t);
}

TTYPutString(t, str)
	TTYWindow *t;
	char *str;
{
	write(t->file, str, strlen(str));
}

TTYPutChar(t, ch)
	TTYWindow *t;
	char ch;
{
	write(t->file, &ch, 1);
}

TTYPrintf(t, format, args)
	TTYWindow *t;
	char *format;
{
#define TTY_BUFSIZE 2048
	char buffer[TTY_BUFSIZE+1];
	struct _iobuf _strbuf;

	_strbuf._flag = _IOWRT+_IOSTRG;
	_strbuf._ptr = buffer;
	_strbuf._cnt = TTY_BUFSIZE;
	_doprnt(format, &args, &_strbuf);
	_strbuf._cnt++;	    /* Be sure there's room for the \0 */
	putc('\0', &_strbuf);
	TTYPutString(t, buffer);
#undef TTY_BUFSIZE
}

static initial_stdout = -1;

SetStdout(t)
	TTYWindow *t;
{
	if (initial_stdout == -1) initial_stdout = stdout->_file;
	fflush(stdout);
	stdout->_file = t->file;
}

ResetStdout()
{
	fflush(stdout);
	stdout->_file = initial_stdout;
	initial_stdout = -1;
}

#define CMASK 0377

int TTYGetChar(t)
	TTYWindow *t;
{
	char c;

	if (read(t->file, &c, 1) > 0)
	    return (c & CMASK);
	else return (EOF);
}

char *TTYGetString(t, str, n)
	register TTYWindow *t;
	char *str;
	register int n;
{
	register char *cs;

	cs = str;
	while (--n > 0 && read(t->file, cs, 1) > 0) {
	    if (*cs++ == '\n') break;
	}

	if (cs == str) return NULL;

	*cs = '\0';
	return str;

}
