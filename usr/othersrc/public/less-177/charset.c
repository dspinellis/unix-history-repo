/*
 * Functions to define the character set
 * and do things specific to the character set.
 */

#include "less.h"

/*
 * Predefined character sets,
 * selected by the LESSCHARSET environment variable.
 */
struct charset {
	char *name;
	char *desc;
} charsets[] = {
	{ "ascii",	"8bcccbcc18b95.b"	},
	{ "latin1",	"8bcccbcc18b95.33b."	},
	{ NULL }
};

#define	IS_BINARY_CHAR	01
#define	IS_CONTROL_CHAR	02

static char chardef[256];
static char *binfmt = "\\%o";
public int binattr = BLINK;

extern char *getenv();

/*
 * Define a charset, given a description string.
 * The string consists of 256 letters,
 * one for each character in the charset.
 * If the string is shorter than 256 letters, missing letters
 * are taken to be identical to the last one.
 * A decimal number followed by a letter is taken to be a 
 * repetition of the letter.
 *
 * Each letter is one of:
 *	. normal character
 *	b binary character
 *	c control character
 */
	static void
ichardef(s)
	char *s;
{
	register char *cp;
	register int n;
	register char v;

	n = 0;
	cp = chardef;
	while (*s != '\0')
	{
		switch (*s++)
		{
		case '.':
			v = 0;
			break;
		case 'c':
			v = IS_CONTROL_CHAR;
			break;
		case 'b':
			v = IS_BINARY_CHAR|IS_CONTROL_CHAR;
			break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			n = (10 * n) + (s[-1] - '0');
			continue;

		default:
			error("invalid chardef", NULL_PARG);
			quit(1);
			/*NOTREACHED*/
		}

		do
		{
			if (cp >= chardef + sizeof(chardef))
			{
				error("chardef longer than 256", NULL_PARG);
				quit(1);
				/*NOTREACHED*/
			}
			*cp++ = v;
		} while (--n > 0);
		n = 0;
	}

	while (cp < chardef + sizeof(chardef))
		*cp++ = v;
}

/*
 * Define a charset, given a charset name.
 * The valid charset names are listed in the "charsets" array.
 */
	static int
icharset(name)
	register char *name;
{
	register struct charset *p;

	if (name == NULL || *name == '\0')
		return (0);

	for (p = charsets;  p->name != NULL;  p++)
	{
		if (strcmp(name, p->name) == 0)
		{
			ichardef(p->desc);
			return (1);
		}
	}

	error("invalid charset name", NULL_PARG);
	quit(1);
	/*NOTREACHED*/
}

/*
 * Initialize charset data structures.
 */
	public void
init_charset()
{
	register char *s;

	/*
	 * Try environment variable LESSCHARSET.
	 * If LESSCHARSET is not set, try LESSCHARDEF.
	 * If LESSCHARDEF is not set, default to "ascii" charset.
	 */
	s = getenv("LESSCHARSET");
	if (icharset(s))
		return;

	s = getenv("LESSCHARDEF");
	if (s != NULL && *s != '\0')
	{
		ichardef(s);
		return;
	}

	(void) icharset("ascii");

	s = getenv("LESSBINFMT");
	if (s != NULL && *s != '\0')
	{
		if (*s == '*')
		{
			switch (s[1])
			{
			case 'd':  binattr = BOLD;      break;
			case 'k':  binattr = BLINK;     break;
			case 'u':  binattr = UNDERLINE; break;
			default:   binattr = NORMAL;    break;
			}
			s += 2;
		}
		if (*s != '\0')
			binfmt = s;
	}
}

/*
 * Is a given character a "binary" character?
 */
	public int
binary_char(c)
	int c;
{
	return (chardef[c] & IS_BINARY_CHAR);
}

/*
 * Is a given character a "control" character?
 */
	public int
control_char(c)
	int c;
{
	return (chardef[c] & IS_CONTROL_CHAR);
}

/*
 * Return the printable form of a character.
 * For example, in the "ascii" charset '\3' is printed as "^C".
 */
	public char *
prchar(c)
	int c;
{
	static char buf[8];

	if (!control_char(c))
		sprintf(buf, "%c", c);
	else if (!control_char(c ^ 0100))
		sprintf(buf, "^%c", c ^ 0100);
	else
		sprintf(buf, binfmt, c);
	return (buf);
}
