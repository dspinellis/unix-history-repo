/*
 * Routines to decode user commands.
 *
 * This is all table driven.
 * A command table is a sequence of command descriptors.
 * Each command descriptor is a sequence of bytes with the following format:
 *	<c1><c2>...<cN><0><action>
 * The characters c1,c2,...,cN are the command string; that is,
 * the characters which the user must type.
 * It is terminated by a null <0> byte.
 * The byte after the null byte is the action code associated
 * with the command string.
 * If an action byte is OR-ed with A_EXTRA, this indicates
 * that the option byte is followed by an extra string.
 *
 * There may be many command tables.
 * The first (default) table is built-in.
 * Other tables are read in from "lesskey" files.
 * All the tables are linked together and are searched in order.
 */

#include "less.h"
#include "cmd.h"
#if __MSDOS__
#include <io.h>
#include <stdlib.h>
#endif

/*
 * Command table is ordered roughly according to expected
 * frequency of use, so the common commands are near the beginning.
 */
static char cmdtable[] =
{
#if __MSDOS__
	/*
	 * PC function keys.
	 * Note that '\0' is converted to '\200' on input.
	 */
	'\200','\120',0,		A_F_LINE,		/* down arrow */
	'\200','\121',0,		A_F_SCREEN,		/* page down */
	'\200','\110',0,		A_B_LINE,		/* up arrow */
	'\200','\111',0,		A_B_SCREEN,		/* page up */
	'\200','\107',0,		A_GOLINE,		/* home */
	'\200','\117',0,		A_GOEND,		/* end */
	'\200','\073',0,		A_HELP,			/* F1 */
	'\200','\104',0,		A_MODIFY_WINDOW,	/* F10 */
	'\200','\103',0,		A_MODIFY_COLOURS,	/* F9 */
#endif
	'\r',0,				A_F_LINE,
	'\n',0,				A_F_LINE,
	'e',0,				A_F_LINE,
	'j',0,				A_F_LINE,
	CONTROL('E'),0,			A_F_LINE,
	CONTROL('N'),0,			A_F_LINE,
	'k',0,				A_B_LINE,
	'y',0,				A_B_LINE,
	CONTROL('Y'),0,			A_B_LINE,
	CONTROL('K'),0,			A_B_LINE,
	CONTROL('P'),0,			A_B_LINE,
	'J',0,				A_FF_LINE,
	'K',0,				A_BF_LINE,
	'Y',0,				A_BF_LINE,
	'd',0,				A_F_SCROLL,
	CONTROL('D'),0,			A_F_SCROLL,
	'u',0,				A_B_SCROLL,
	CONTROL('U'),0,			A_B_SCROLL,
	' ',0,				A_F_SCREEN,
	'f',0,				A_F_SCREEN,
	CONTROL('F'),0,			A_F_SCREEN,
	CONTROL('V'),0,			A_F_SCREEN,
	'b',0,				A_B_SCREEN,
	CONTROL('B'),0,			A_B_SCREEN,
	ESC,'v',0,			A_B_SCREEN,
	'z',0,				A_F_WINDOW,
	'w',0,				A_B_WINDOW,
	'F',0,				A_F_FOREVER,
	'R',0,				A_FREPAINT,
	'r',0,				A_REPAINT,
	CONTROL('R'),0,			A_REPAINT,
	CONTROL('L'),0,			A_REPAINT,
	'g',0,				A_GOLINE,
	'<',0,				A_GOLINE,
	ESC,'<',0,			A_GOLINE,
	'p',0,				A_PERCENT,
	'%',0,				A_PERCENT,
	'{',0,				A_F_BRACKET|A_EXTRA,	'{','}',0,
	'}',0,				A_B_BRACKET|A_EXTRA,	'{','}',0,
	'(',0,				A_F_BRACKET|A_EXTRA,	'(',')',0,
	')',0,				A_B_BRACKET|A_EXTRA,	'(',')',0,
	'[',0,				A_F_BRACKET|A_EXTRA,	'[',']',0,
	']',0,				A_B_BRACKET|A_EXTRA,	'[',']',0,
	ESC,CONTROL('F'),0,		A_F_BRACKET,
	ESC,CONTROL('B'),0,		A_B_BRACKET,
	'G',0,				A_GOEND,
	ESC,'>',0,			A_GOEND,
	'>',0,				A_GOEND,
	'P',0,				A_GOPOS,

	'0',0,				A_DIGIT,
	'1',0,				A_DIGIT,
	'2',0,				A_DIGIT,
	'3',0,				A_DIGIT,
	'4',0,				A_DIGIT,
	'5',0,				A_DIGIT,
	'6',0,				A_DIGIT,
	'7',0,				A_DIGIT,
	'8',0,				A_DIGIT,
	'9',0,				A_DIGIT,

	'=',0,				A_STAT,
	CONTROL('G'),0,			A_STAT,
	':','f',0,			A_STAT,
	'/',0,				A_F_SEARCH,
	'?',0,				A_B_SEARCH,
	ESC,'/',0,			A_F_SEARCH|A_EXTRA,	'*',0,
	ESC,'?',0,			A_B_SEARCH|A_EXTRA,	'*',0,
	'n',0,				A_AGAIN_SEARCH,
	ESC,'n',0,			A_T_AGAIN_SEARCH,
	'N',0,				A_REVERSE_SEARCH,
	ESC,'N',0,			A_T_REVERSE_SEARCH,
	'm',0,				A_SETMARK,
	'\'',0,				A_GOMARK,
	CONTROL('X'),CONTROL('X'),0,	A_GOMARK,
	'E',0,				A_EXAMINE,
	':','e',0,			A_EXAMINE,
	CONTROL('X'),CONTROL('V'),0,	A_EXAMINE,
	':','n',0,			A_NEXT_FILE,
	':','p',0,			A_PREV_FILE,
	':','x',0,			A_INDEX_FILE,
	'-',0,				A_OPT_TOGGLE,
	':','t',0,			A_OPT_TOGGLE|A_EXTRA,	't',0,
	's',0,				A_OPT_TOGGLE|A_EXTRA,	'o',0,
	'_',0,				A_DISP_OPTION,
	'|',0,				A_PIPE,
	'v',0,				A_VISUAL,
	'!',0,				A_SHELL,
	'+',0,				A_FIRSTCMD,

	'H',0,				A_HELP,
	'h',0,				A_HELP,
	'V',0,				A_VERSION,
	'q',0,				A_QUIT,
	':','q',0,			A_QUIT,
	':','Q',0,			A_QUIT,
	'Z','Z',0,			A_QUIT,
	ESC,ESC,0,			A_QUIT,
};

/*
 * Structure to support a list of command tables.
 */
struct tablelist
{
	struct tablelist *t_next;
	char *t_start;
	char *t_end;
};

/*
 * Structure for the default command table.
 */
static struct tablelist deftable = 
	{ NULL, cmdtable, cmdtable+sizeof(cmdtable) };

/*
 * List of tables; initially contains only the default table.
 */
static struct tablelist *tables = &deftable;

static int cmd_search();

extern int erase_char, kill_char;

/*
 * Decode a command character and return the associated action.
 * The "extra" string, if any, is returned in sp.
 */
	public int
cmd_decode(cmd, sp)
	char *cmd;
	char **sp;
{
	register struct tablelist *t;
	register int action;

	/*
	 * Search thru all the command tables.
	 * Stop when we find an action which is not A_INVALID.
	 */
	for (t = tables;  t != NULL;  t = t->t_next)
	{
		action = cmd_search(cmd, t->t_start, t->t_end, sp);
		if (action != A_INVALID)
			break;
	}
	return (action);
}

/*
 * Search a command table for the current command string (in cmd).
 */
	static int
cmd_search(cmd, table, endtable, sp)
	char *cmd;
	char *table;
	char *endtable;
	char **sp;
{
	register char *p;
	register char *q;
	register int a;

	for (p = table, q = cmd;  p < endtable;  p++, q++)
	{
		if (*p == *q)
		{
			/*
			 * Current characters match.
			 * If we're at the end of the string, we've found it.
			 * Return the action code, which is the character
			 * after the null at the end of the string
			 * in the command table.
			 */
			if (*p == '\0')
			{
				a = *++p & 0377;
				/*
				 * Check for an "extra" string.
				 */
				if (a & A_EXTRA)
				{
					*sp = ++p;
					a &= ~A_EXTRA;
				} else
					*sp = NULL;
				return (a);
			}
		} else if (*q == '\0')
		{
			/*
			 * Hit the end of the user's command,
			 * but not the end of the string in the command table.
			 * The user's command is incomplete.
			 */
			return (A_PREFIX);
		} else
		{
			/*
			 * Not a match.
			 * Skip ahead to the next command in the
			 * command table, and reset the pointer
			 * to the beginning of the user's command.
			 */
			while (*p++ != '\0') ;
			if (*p & A_EXTRA)
				while (*++p != '\0') ;
			q = cmd-1;
		}
	}
	/*
	 * No match found in the entire command table.
	 */
	return (A_INVALID);
}

#if USERFILE
/*
 * Set up a user command table, based on a "lesskey" file.
 */
	public int
add_cmdtable(filename)
	char *filename;
{
	register struct tablelist *t;
	register POSITION len;
	register long n;
	register int f;

	/*
	 * Try to open the lesskey file.
	 * If we can't, return an error.
	 */
	f = open(filename, 0);
	if (f < 0)
		return (-1);

	/*
	 * Read the file into the user table.
	 * We first figure out the size of the file and allocate space for it.
	 * {{ Minimal error checking is done here.
	 *    A garbage .less file will produce strange results.
	 *    To avoid a large amount of error checking code here, we
	 *    rely on the lesskey program to generate a good .less file. }}
	 */
	len = filesize(f);
	if (len == NULL_POSITION || len < 3)
	{
		/*
		 * Bad file (valid file must have at least 3 chars).
		 */
		close(f);
		return (-1);
	}
	if ((t = (struct tablelist *) 
			calloc(1, sizeof(struct tablelist))) == NULL)
	{
		close(f);
		return (-1);
	}
	if ((t->t_start = (char *) calloc(len, sizeof(char))) == NULL)
	{
		free((char *)t);
		close(f);
		return (-1);
	}
	if (lseek(f, (offset_t)0, 0) == BAD_LSEEK)
	{
		free(t->t_start);
		free((char *)t);
		close(f);
		return (-1);
	}
	n = read(f, t->t_start, (unsigned int) len);
	close(f);

	/*
	 * In a valid lesskey file, the last byte or 
	 * the second to the last byte must be zero.
	 */
	if (n != len || (t->t_start[n-1] != '\0' && t->t_start[n-2] != '\0'))
	{
		free(t->t_start);
		free((char *)t);
		return (-1);
	}
	t->t_end = t->t_start + n;

	/*
	 * Link it into the list of tables.
	 */
	t->t_next = tables;
	tables = t;
	return (0);
}

/*
 * Try to add the lesskey file "$HOME/.less"
 */
	public void
add_hometable()
{
	char *filename;

#if __MSDOS__
	filename = homefile("_less");
#else
	filename = homefile(".less");
#endif
	if (filename == NULL)
		return;
	/*
	 * Ignore errors.
	 */
	(void) add_cmdtable(filename);
	free(filename);
}
#endif
