/*************************************************************************
 *
 * Code to parse the ditroff output language and take the appropriate
 * action.
 *
 * For a description of the ditroff language, see the comment in main.c.
 *
 ************************************************************************/

#include "the.h"

/* from dev.c  */
extern int	output;		/* do we output at all? */
extern int	hpos, vpos;	/* current position	*/
extern int	virtRES;	/* resolution of input		*/

/* from main.c */
extern int	debug, dbg;


private char	*inname = "nver";	/* input was prepared for this dev */



conv(fp)
register FILE *fp;
{
	register int c, k;
	int m, n, n1, m1;
	char str[100], buf[300];

	while ((c = getc(fp)) != EOF) {
		switch (c) {
		case '\n':	/* when input is text */
		case 0:		/* occasional noise creeps in */
		case '\t':
		case ' ':
			break;
		case '{':	/* push down current environment */
			t_push();
			break;
		case '}':
			t_pop();
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			/* two motion digits plus a character */
			k = (c-'0')*10 + getc(fp)-'0';
			hmot(k);
			put1(getc(fp));
			break;
		case 'c':	/* single ascii character */
			put1(getc(fp));
			break;
		case 'C':
			fscanf(fp, "%s", str);
			put1s(str);
			break;
		case 't':	/* straight text */
			if (fgets(buf, sizeof(buf), fp) == NULL)
			    error(FATAL, "unexpected end of input");
			t_text(buf);
			break;
		case 'D':	/* draw function */
			if (fgets(buf, sizeof(buf), fp) == NULL)
			    error(FATAL, "unexpected end of input");
			switch (buf[0]) {
			case 'l':	/* draw a line */
			    sscanf(buf+1, "%d %d", &n, &m);
			    drawline(n, m);
			    break;
			case 'c':	/* circle */
			    sscanf(buf+1, "%d", &n);
			    drawcirc(n);
			    break;
			case 'e':	/* ellipse */
			    sscanf(buf+1, "%d %d", &m, &n);
			    drawellip(m, n);
			    break;
			case 'a':	/* arc */
			    sscanf(buf+1, "%d %d %d %d", &n, &m, &n1, &m1);
			    drawarc(n, m, n1, m1);
			    break;

			case '~':	/* wiggly line */
			    drawwig(buf+1, fp, buf[0] == '~');
			    break;
			default:
			    error(FATAL, "unknown drawing function %s", buf);
			    break;
			}
			break;
		case 's':
			fscanf(fp, "%d", &n);
			setsize(t_size(n));
			break;
		case 'f':
			fscanf(fp, "%s", str);
			setfont(t_font(str));
			break;
		case 'H':	/* absolute horizontal motion */
			while ((c = getc(fp)) == ' ')
				;
			k = 0;
			do {
				k = 10 * k + c - '0';
			} while (isdigit(c = getc(fp)));
			ungetc(c, fp);
			hgoto(k);
			break;
		case 'h':	/* relative horizontal motion */
			while ((c = getc(fp)) == ' ')
				;
			k = 0;
			do {
				k = 10 * k + c - '0';
			} while (isdigit(c = getc(fp)));
			ungetc(c, fp);
			hmot(k);
			break;
		case 'w':	/* word space */
			break;
		case 'V':
			fscanf(fp, "%d", &n);
			vgoto(n);
			break;
		case 'v':
			fscanf(fp, "%d", &n);
			vmot(n);
			break;
		case 'P':	/* new spread */

			DBGPRINT(0, ("output = %d\n", output));

			if (output) outband(OVERBAND);
			break;
		case 'p':	/* new page */
			fscanf(fp, "%d", &n);
			t_page(n);
			break;
		case 'n':	/* end of line */
			t_newline();

		case '#':	/* comment */
			do
				c = getc(fp);
			while (c != '\n' && c != EOF);
			break;
		case 'x':	/* device control */
			if (devcntrl(fp)) return;
			break;
		default:
			error(FATAL, "unknown input character %o %c", c, c);
		}
	}
}

int
devcntrl(fp)		/* interpret device control functions */
FILE *fp;		/* returns -1 apon recieving "stop" command */
{
        char str[20], str1[50], buf[50];
	int c, n;

	fscanf(fp, "%s", str);
	switch (str[0]) {	/* crude for now */
	case 'i':			/* initialize */
		fileinit();
		t_init();
		break;
	case 't':			/* trailer */
		break;
	case 'p':			/* pause -- can restart */
		t_reset('p');
		break;
	case 's':			/* stop */
		t_reset('s');
		return -1;
	case 'r':			/* resolution assumed when prepared */
		fscanf(fp, "%d", &virtRES);
		initgraph(virtRES);		/* graph needs to know about this */
		break;
	case 'f':			/* font used */
		fscanf(fp, "%d %s", &n, str);
		(void) fgets(buf, sizeof buf, fp);   /* in case of filename */
		ungetc('\n', fp);		/* fgets goes too far */
		str1[0] = 0;			/* in case nothing comes in */
		sscanf(buf, "%s", str1);
		loadfont(n, str, str1);
		break;
	case 'H':			/* char height */
		fscanf(fp, "%d", &n);
		t_charht(n);
		break;
	case 'S':			/* slant */
		fscanf(fp, "%d", &n);
		t_slant(n);
		break;
	case 'T':			/* device name */
		fscanf(fp, "%s", inname);
		break;

	}
	while ((c = getc(fp)) != '\n')	/* skip rest of input line */
		if (c == EOF)
			return -1;
	return 0;
}
