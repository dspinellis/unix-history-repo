#ifndef lint
static char sccsid[] = "@(#)fed.c	4.2 (Berkeley) 8/11/83";
#endif

/*
 *	Font editor for the HP 2648.
 *
 *	Mark Horton, 1/80
 */

#include "fed.h"

main(argc,argv)
int argc;
char **argv;
{

	signal(SIGINT, onintr);
	signal(SIGQUIT, onsig);
	signal(SIGILL, onsig);
	signal(SIGBUS, onsig);
	signal(SIGSEGV, onsig);
	signal(SIGSYS, onsig);

	while (argc > 1 && argv[1][0] == '-') {
		switch(argv[1][1]) {
		case 'T':
			trace = fopen("trace", "w");
			setbuf(trace, tracebuf);
			break;
		case 'i':
		case 'v':
			vidinv();
			break;
		case 'q':
			QUIET = 1;
			break;
		default:
			printf("Bad flag: %s\n", argv[1]);
		}
		argc--; argv++;
	}
	if (argc < 2) {
		fprintf(stderr,"Usage: %s filename\n", argv[0]);
		exit(1);
	}

	if (setjmp(env) == 0) {
		initialize();
		editfont(argv[1]);
	}

	cmdloop();
}

cmdloop()
{
	char cmd;

	setjmp(env);
	for (;;) {
		cmd = inchar();
		if (cmd == ESC)
			cmd = esccmd();
		switch (cmd) {

		/* ^L: redraw munged up screen */
		case '\14':
			redraw();
			break;

		/* b: move cursor to base point of window */
		case 'b':
			cch();
			curs_r = cht[curchar].rcent;
			curs_c = cht[curchar].ccent;
			turnoncurs();
			break;

		/* c: toggle whether cursor is on */
		case 'c':
			if (curcurs)
				turnofcurs();
			else
				turnoncurs();
			break;

		/* d: draw line of current flavor from pen to cursor */
		case 'd':
			cch();
			bufmod();
			drawline(pen_r, pen_c, curs_r, curs_c);
			turnofcurs();
			turnofrb();
			pen_r = curs_r; pen_c = curs_c;
			syncwind(curwind);
			break;

		/* f: fill in the current hole around the cursor */
		case 'f':
			cch();
			bufmod();
			if (trace)
				fprintf(trace, "fillin(%d, %d)\n", curs_r, curs_c);
			if (mat(wind[curwind].val, GLROW, GLCOL, curs_r, curs_c))
				error("Not in a hole");
			fillin(curs_r, curs_c);
			curoff();
			syncwind(curwind);
			break;

		/* g <x>: get glyph "x" as current. */
		case 'g':
			if (fontdes == NULL)
				error("No current font file");
			message("get glyph <char>");
			curchar = inchar();
			sprintf(msgbuf, "get glyph %s", rdchar(curchar));
			message(msgbuf);
			getglyph(curchar);
			break;

		/* h, left arrow: move cursor left */
		case 'h':
			cch();
			if (curs_c <= 0)
				error("Off edge");
			else
				curs_c--;
			turnoncurs();
			break;

		/* j, down arrow: move cursor down */
		case 'j':
			cch();
			if (curs_r >= GLROW-1)
				error("Off edge");
			else
				curs_r++;
			turnoncurs();
			break;

		/* k, up arrow: move cursor up */
		case 'k':
			cch();
			if (curs_r <= 0)
				error("Off edge");
			else
				curs_r--;
			turnoncurs();
			break;

		/* l, right arrow: move cursor down */
		case 'l':
			cch();
			if (curs_c >= GLCOL-1)
				error("Off edge");
			else
				curs_c++;
			turnoncurs();
			break;

		/* m: move the pen to where the cursor is */
		case 'm':
			cch();
			pen_r = curs_r; pen_c = curs_c;
			turnoncurs();
			move(base[curwind].c+curs_c, base[curwind].r+GLROW-1-curs_r);
			turnonrb();
			break;

		/* n <x>: make a new glyph with char x */
		case 'n':
			newglyph();
			break;

		/* p: print a hard copy on the printer of the screen */
		case 'p':
			printg();
			break;

		/* r: toggle rubber band line */
		case 'r':
			if (currb)
				turnofrb();
			else
				turnonrb();
			break;

		/* s <what> <where>: set <what> to <where> */
		case 's':
			setcmd();
			break;

		/* u: undo previous buffer modifying command */
		case 'u':
			cch();
			undo();
			break;

		/* z <n>: set zoom to n. */
		case 'z':
			message("zoom to <level>");
			curzoom = inchar();
			if (curzoom == '\r' || curzoom == '\n')
				curzoom = oldzoom;
			else {
				curzoom -= '0';
				oldzoom = curzoom;
			}
			zoomn(curzoom);
			break;

		/* space: reset zoom to last thing user asked for */
		case ' ':
			zoomn(curzoom = oldzoom);
			break;

		/* A: artificially embolden/italicize <range> by heavy pen size */
		case 'A':
			bufmod();
			artificial();
			break;

		/* B: move base point of window to cursor */
		case 'B':
			cch();
			cht[curchar].rcent = curs_r;
			cht[curchar].ccent = curs_c;
			turnoncurs();
			break;

		/*
		 * C <from> <to>: copy glyph <from> to <to>.
		 * M <from> <to>: move glyph <from> to <to>.
		 */
		case 'C':
		case 'M':
			copymove(cmd);
			break;

		/* D <char1> <char2>: delete range from font */
		case 'D':
			delchar();
			break;

		/* F: display the entire font on the screen. */
		case 'F':
			showfont();
			break;

		/* I: invert the current glyph */
		case 'I':
			cch();
			bufmod();
			invert();
			break;

		/* K: kill (wipe clean) current glyph. */
		case 'K':
			cch();
			bufmod();
			zermat(wind[curwind].val, GLROW, GLCOL);
			syncwind(curwind);
			if (trace)
				fprintf(trace, "kill: curs_r = %d, curs_c = %d\n", curs_r, curs_c);
			break;

		/* P <first> <last> <file>: read partial font */
		case 'P':
			readchars();
			break;

		/* Q: quit the editor, not saving work. */
		case 'Q':
			confirm();
			done();
			exit(0);

		/* T: typeset a line of input text */
		case 'T':
			typein();
			break;

		/* V: toggle video between inverse and normal */
		case 'V':
			togvid();
			break;

		/*
		 * E <file>: edit new font file <file>.
		 * N <file>: write, then edit <file>
		 * R <file>: read <file> on top of buffer.
		 * W <file>: write out on <file> without quitting
		 */
		case 'E':
		case 'N':
		case 'R':
		case 'W':
			fileiocmd(cmd);
			break;

		/* Z: exit, writing out work */
		case 'Z':
			message("Z");
			if (inchar() != 'Z') {
				error("No second Z");
			}
			if (changes)
				writeback();
			done();
			exit(0);

		/*
		 * ".", ">".  Set and clear the bit under the cursor.
		 */
		case '.':
		case '>':
			bufmod();
			setmat(wind[curwind].val, GLROW, GLCOL, curs_r, curs_c, cmd=='.' ? 1 : 0);
			turnofcurs();
			syncwind(curwind);
			break;

		/*
		 * "#": edit the numerical parameters
		 */
		case '#':
			numedit();
			break;

		default:
			sprintf(msgbuf, "No such command as %s", rdchar(cmd));
			message(msgbuf);
		}

	}
}

/*
 * esccmd: a command beginning with an escape.
 * Map it into the corresponding regular command.
 */
char
esccmd()
{
	char cmd;
	char *p;
	char escseqbuf[20];

	cmd = inchar();
	switch(cmd) {
	case 'A':	return ('k');	/* up arrow */
	case 'B':	return ('j');	/* down arrow */
	case 'C':	return ('l');	/* right arrow */
	case 'D':	return ('h');	/* left arrow */
	case 'h':	return ('b');	/* home */
	case '2':	return ('u');	/* clear tab = undo */
	case '1':	return (' ');	/* set tab = rezoom */
	case 'J':	return ('f');	/* clear display = fill area */
	case 'S':	return ('m');	/* roll up = move */
	case 'U':	return ('d');	/* next page = draw */
	case 'T':	return ('.');	/* roll down = set bit */
	case 'V':	return ('>');	/* prev page = clear bit */
	default:
		/*
		 * Eat up rest of (possibly long) escape sequence.
		 * They all end in an upper case letter, with
		 * a few exceptions.
		 */
		p = escseqbuf;
		*p++ = '$';
		*p++ = cmd;
		while (!isupper(cmd) && cmd != 'h' && cmd != '\n')
			*p++ = cmd = inchar();
		*p++ = 0;
		sprintf(msgbuf, "Bad escape sequence: %s\n", escseqbuf);
		error(msgbuf);
	}
}

onsig(signo)
int signo;
{
	char *mes;

	switch(signo) {
		case SIGQUIT:	mes = "quit"; break;
		case SIGILL:	mes = "illegal instruction"; break;
		case SIGBUS:	mes = "bus error"; break;
		case SIGSEGV:	mes = "segmentation violation"; break;
		case SIGSYS:	mes = "bad system call"; break;
		default:	mes = "random signal"; break;
	}
	if (trace) {
		fprintf(trace, "%s: core dumped\n", mes);
		fflush(trace);
	}
	signal(SIGILL, SIG_DFL);
	done();
	printf("fed: %s: core dumped\n", mes);
	fflush(stdout);
	abort();
}

onintr()
{
	signal(SIGINT, onintr);
	error("Interrupted");
	longjmp(env);
}
