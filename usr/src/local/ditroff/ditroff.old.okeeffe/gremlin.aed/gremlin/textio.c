/* @(#)textio.c	1.2	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *
 * This file contains functions that manipulate the text interface
 *
 * (Modified from software written by John Ousterhout for the caesar
 *  program)
 */

#include "gremlin.h"
#include <sgtty.h>

/* Library routines: */

char *tgetstr(), *sprintf(), *tgoto(), *strcpy(), *strcat();

/* The following definitions are used by the termlib routines
 */

char PC;
char *BC, *UP, *cm, *ce, *vs, *cl; 
short ospeed, lastline=23,msgline=23,inline=22;

static char bp[1024];		/* Holds the termcap entry for the tty */
static char buf[256];		/* Holds selected strings from bp */
static struct sgttyb sttybuf;	/* Holds stty information for resetting tty */
static int sttyflags;		/* Flag word from sttybuf */
static struct tchars tchars;	/* Holds tchars information for resetting */
static char brkc, eofc;		/* Fields from tchars, saved for resetting */
static int c100;		/* TRUE means text display is c100 */
static putmsg;			/* TRUE means TxPutMsg has been called
				 * since last character input.
				 */


/* The following structures define the various fields that are available
 * on the terminal screen.
 */

TXFIELD
    TAlign = {56, 1, 3},
    TAdjust = {63, 1, 13},
    TBrush = {30, 1, 1},
    TEdit = {10, 2, 32},
    TJustmode = {76, 2, 2},
    TFont = {10, 1, 1},
    TGravity = {43, 1, 4},
    TCSize = {19, 1, 1};


OutChar(val)
char val;			/* The character to be output */

/*-----------------------------------------------------------------------------
 *	OutChar merely outputs a character on the file being used for
 *	text I/O.  This routine is passed as a parameter to the tputs
 *	pad-generating routine.
 *
 *	Results:	None.
 *	Side Effects:	The character is output.
 *	Errors:		None.
 *-----------------------------------------------------------------------------
 */

{
    putc(val, stdout);
}


TxInit(ttytype)
char *ttytype;			/* The name of the terminal type, as returned
				 * by getenv("TERM") */

/*-----------------------------------------------------------------------------
 *	TxInit initializes the text terminal and saves state information
 *	so that we can restore its characteristics later.
 *
 *	Results:	None.
 *	Side Effects:	Terminal characteristics are saved.
 *-----------------------------------------------------------------------------
 */

{
    char *p1, *p2, *pcap;
    int i;

    /* Turn off buffering.  This is necessary to avoid mess-ups during
     * the constant mode changes.
     */

    setbuf(stdin, NULL);

    /* Find the termcap entry, and set up the strings and constants
     * required by the termlib routines.
     */

    if (tgetent(bp, ttytype) != 1)
	error("Cannot find terminal characteristics");
    (void) gtty(fileno(stdout), &sttybuf);
    ospeed = sttybuf.sg_ospeed;
    pcap = buf;
    p1 = tgetstr("pc", &pcap);  if (p1 == NULL) PC = 0; else PC = *p1;
    pcap = buf;
    BC = tgetstr("bc", &pcap);
    UP = tgetstr("up", &pcap);
    cm = tgetstr("cm", &pcap);
    ce = tgetstr("ce", &pcap);
    vs = tgetstr("vs", &pcap);
    cl = tgetstr("cl", &pcap);
    (void) gtty(fileno(stdin), &sttybuf);
    sttyflags = sttybuf.sg_flags;
    (void) ioctl(fileno(stdin), TIOCGETC, (char *) &tchars);
    eofc = tchars.t_eofc;
    brkc = tchars.t_brkc;
    p1 = "c100";
    p2 = ttytype;
    c100 = FALSE;
    putmsg = FALSE;
    for (i = 4; i; i = i-1)
    {
	if (*p1 != *p2) break;
	if (i=1) c100 = TRUE;
    }
}


TxRedisplay()

/*-----------------------------------------------------------------------------
 *	This routine merely redisplays the command menu on the text screen,
 *	and modifies text display parameters for menu processing.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The text display is cleared, menu information is reprinted, and
 *	terminal characteristics are set for menu processing.
 *-----------------------------------------------------------------------------
 */

{
    static char initstring1[] = "\n\
     font 1   size 1    brush 6    gravi\
ty OFF    align   4    NO ADJUSTMENT \n\
Editing:                                \
                     Justification: BL \n\
----------------------------------------\
---------------------------------------\n\
    Long Commands:          |\
         Short Commands:\n\
                            |\n\
   Align          MSize     |\
   a - draw arc         ! - shell escape\n\
   BRush          MText     |\
   b - draw curve       . - repeat last command\n\
   BUffer         Orient    |\
   c - copy set        ^L - redraw picture\n\
   Clearpoints    PAth      |\
   d - define set       l - redisplay text screen\n\
";
    static char initstring2[] = "\
   Deletepoint    POint     |\
   e - erase      1,2,3,4 - store set buffer\n\
   Edit<!>        Quit<!>   |\
   f - define area \n\
   Font           Read      |\
   g - gravity\n\
   Gripe          SAveset<!>|\
   q - grid\n\
   Hadjust        SHowpoints|\
   r - rotate set\n\
   Includeset     SIze      |\
   s - scale set\n\
   Justify        Text      |\
   t - translate set\n\
   Littlepoint    Undo      |\
   v - draw vectors\n\
   MBrush         Vadjust   |\
   w - draw arrow \n\
   MFont          Write<!>  |\
   x - draw box\n\
   MIrror                   |\
   z - manhattan adjust\n\
   MPoint                   |\
   \n\
";

    /* Clear the terminal screen and output the gremlin menu.  The
     * TxClose call is necessary to put the terminal back into its standard
     * operating mode;  otherwise this stuff will appear as garbage!
     * However, turn off echoing to keep type-ahead from appearing
     * in the middle of the screen.
     */

    TxClose();
    sttybuf.sg_flags &= ~ECHO;
    (void) ioctl(fileno(stdin), TIOCSETN, (char *) &sttybuf);
    fputs(vs, stdout);
    tputs(cl, tgetnum("li"), OutChar);
    fputs(tgoto(cm, 0, 0), stdout);
    fputs(initstring1, stdout);
    fputs(initstring2, stdout);

    /* If the terminal is a Concept 100, then output special characters
     * for the border lines between the three screen areas.
     */

    if (c100)
    {
	fputs(tgoto(cm, 0, 3), stdout);
	fputs("\33U\33r\1p", stdout);
	putchar(0);
	putchar(0);
	putchar(0);
	putchar(0);
	fputs(tgoto(cm, 28, 4), stdout);
	putchar('\0');
	putchar('\0');
	fputs("\33R", stdout);
	putchar('\0');
	putchar('2'); 
    }

    /* Place the cursor in the lower-left corner, save terminal
     * characteristics so we can restore them later, then make ^M
     * into a break character, put the terminal into CBREAK mode,
     * and turn off echoing.
     */

    fputs(tgoto(cm, 0, lastline), stdout);
    sttybuf.sg_flags |= CBREAK;
    sttybuf.sg_flags &= ~(ECHO|CRMOD);
    (void) ioctl(fileno(stdin), TIOCSETN, (char *) &sttybuf);
    tchars.t_eofc = -1;
    tchars.t_brkc = '\r';
    (void) ioctl(fileno(stdin), TIOCSETC, (char *) &tchars);
    (void) fflush(stdout);
    putmsg = FALSE;
}


TxClose()

/*-----------------------------------------------------------------------------
 *	TxClose resets the terminal to the way we found it.
 *
 *	Results:	None.
 *	Side Effects:	None.
 *	Errors:		None.
 *-----------------------------------------------------------------------------
 */

{
    char *dummy, buf [100];
    sttybuf.sg_flags = sttyflags;
    (void) ioctl(fileno(stdin), TIOCSETN, (char *) &sttybuf);
    tchars.t_eofc = eofc;
    tchars.t_brkc = brkc;
    (void) ioctl(fileno(stdin), TIOCSETC, (char *) &tchars);
    fputs(tgoto(cm, 0, tgetnum("li")-1), stdout);
    dummy = buf;
    fputs(tgetstr("ve", &dummy), stdout);
    fputs("\n", stdout);
    (void) fflush(stdout);
}


TxPutString(field, string)
TXFIELD *field;			/* The screen field to be overwritten */
char *string;			/* The character string to be written */

/*-----------------------------------------------------------------------------
 *	TxPutString displays a string in a given field of the text display.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The given text screen field is overwritten with the given text
 *	string, blank filled and left justified.
 *
 *	Errors:		None.
 *-----------------------------------------------------------------------------
 */

{
    char format[20];
    (void) sprintf(format, "%%s%%-%d.%ds", field->tx_size, field->tx_size);
    (void) printf(format, tgoto(cm, field->tx_x, field->tx_y), string);
    (void) fflush(stdout);
}


char
TxGetChar()

/*-----------------------------------------------------------------------------
 *	TxGetChar gets the next character from the text keyboard.
 *
 *	Results:	The next character.
 *	Side Effects:	None.
 *-----------------------------------------------------------------------------
 */

{
    putmsg = FALSE;
    return getchar();
}


TxMsgOK()

/*-----------------------------------------------------------------------------
 *	This routine marks it OK to output messages again, just as if text
 *	had been input.
 *
 *	Results:	None.
 *	Side Effects:	As above.
 *-----------------------------------------------------------------------------
 */

{
	if (putmsg)          /* message on the screen to be blanked */
	{
		TxLine(msgline);
		printf("                                                                            ");
		TxLine(msgline);
	}
	putmsg = FALSE;
}


TxGetLine(prompt, ptr, maxsize)
char *prompt;			/* Prompt to be output at beginning of line */
char *ptr;			/* Place to store the input line */
int maxsize;			/* Maximum number of characters to be read */

/*-----------------------------------------------------------------------------
 *	TxGetLine reads a line of text from the terminal.  It does so
 *	by positioning the cursor at the bottom of the screen and enabling
 *	echoes again.  When the line has been read, echoes etc. are turned
 *	off once more.
 *
 *	Results:	None.
 *	Side Effects:	The last line of the text screen is destroyed.
 *	Errors:		None.
 *-----------------------------------------------------------------------------
 */

{
    int i, j;

    putmsg = FALSE;
    fputs(tgoto(cm, 0, inline), stdout);
    tputs(ce, 1, OutChar);
    fputs(prompt, stdout);
    sttybuf.sg_flags |= ECHO;
    sttybuf.sg_flags &= ~CBREAK;
    (void) ioctl(fileno(stdin), TIOCSETN, (char *) &sttybuf);

    /* Input characters until a carriage return is found.  Then
     * erase the control character from the screen.
     */

    for (i=0; i<maxsize; i++)
    {
	ptr[i] = getchar();
	if (ptr[i] == '\r') break;
    }
    for (j=i; j<maxsize; j++)
        ptr[j] = '\0';
    fputs("\b\b  \b\b", stdout);
    (void) fflush(stdout);

    /* Reset the terminal into no-echo mode */

    sttybuf.sg_flags |= CBREAK;
    sttybuf.sg_flags &= ~ECHO;
    (void) ioctl(fileno(stdin), TIOCSETN, (char *) &sttybuf);
}


TxPutMsg(msg)
char *msg;			/* A message (not containing \r or \n) to
				 * be output on the text screen.  It must
				 * fit on a single line.  /*

/*-----------------------------------------------------------------------------
 *	TPutMsg outputs a one-line message onto the text screen.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The string in msg is output on the last line of the text display.
 *	If TxPutMsg is called twice between calls to TxGetLine or TxGetChar
 *	then we output the message "More" at the end of the line and
 *	wait for a space to be typed.  This is to protect against
 *	multiple error messages overwriting each other.
 *
 *	Errors:		None.
 *-----------------------------------------------------------------------------
 */

{
    if (putmsg)
    {
	fputs(tgoto(cm, 50, msgline), stdout);
	if (msg[0] == '\7')
	{
	    putchar ('\7');
	    msg++;
	}
	fputs("--More--", stdout);
	(void) fflush(stdout);
	while (getchar() != ' ');
    }
    putmsg = TRUE;
    fputs(tgoto(cm, 0, msgline), stdout);
    tputs(ce, 1, OutChar);
    fputs(msg, stdout);
    (void) fflush(stdout);
}

TxLine(line)
short line;

/*-----------------------------------------------------------------------------
 *	TxLastLine moves the cursor to the first character of the specified
 *	line of the text display.
 *
 *	Results:	None.
 *	Side Effects:	None.
 *-----------------------------------------------------------------------------
 */

{
    fputs(tgoto(cm, 0, line), stdout);
    (void) fflush(stdout);
}
