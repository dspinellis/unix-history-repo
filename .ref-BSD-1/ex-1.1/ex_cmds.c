#include "ex.h"
#include "ex_glob.h"
#include "ex_tty.h"
/*
 * Ex - a text editor
 * Bill Joy UCB June-October 1977
 */

char	version[];

char	CHANGE[]	"change";
char	PRINT[]		"print";
char	RECOVER[]	"recover";
char	PRESERVE[]	"preserve";
char	READ[]		"read";

static	char pflag, nflag, kflag;
static	int poffset;

int	xpand(), tabulate();

commands(noprompt, exitoneof)
	int noprompt;
	char exitoneof;
{
	register int *addr;
	register c;
	register char *p;
	int lchngflag, cnt;
	char hadpr;

	resetflav();
	lchngflag = chngflag;
	if (noprompt == -1) {
		noprompt = 0;
		c = 'e';
		if (recov) {
			recov = 0;
			goto dorecover;
		}
		goto doecmd;
		/* magic! */
	}
	for (;;) {
		shudclob = 0;
		if (pflag || lchngflag != chngflag && value(AUTOPRINT) &&
		    !inglobal && endline) {
			pflag = 0;
			lchngflag = chngflag;
			if (dol != zero) {
				addr1 = addr2 = dot + poffset;
				if (addr1 < one || addr1 > dol)
					error("Offset out-of-bounds|Offset after command too large");
				setdot1();
				goto print;
			}
		}
		if (value(STICKY) == 0)
			resetflav();
		lchngflag = chngflag;
		if (inglobal == 0) {
			flush();
			if (value(HUSH) == 0 && value(PROMPT) && globp == 0 &&
			    noprompt == 0 && intty && endline) {
				putchar(':');
				hadpr = 1;
				flush();
			}
			TSYNC();
		}
		addr2 = 0;
		do {
			addr1 = addr2;
			addr = address();
			c = getchar();
			if (addr == 0)
				if (c == ',')
					addr = dot;
				else if (addr1 != 0) {
					addr2 = dot;
					break;
				} else
					break;
			addr2 = addr;
			if (c == ';') {
				c = ',';
				dot = addr;
			}
		} while (c == ',');
		if (addr1 == 0)
			addr1 = addr2;
		tailspec(c);
		switch (c) {

		case 'a':
			if (peekchar() == 'r') {
				tail("args");
				setnoaddr();
				c = 0;
				if (!exclam())
					c = xargc0 - xargc;
				newline();
				for (; c < xargc0; c++)
					printf("%6d  %s\n", c - (xargc0 - xargc) + 1, xargv0[c]);
				continue;
			}
			tail("append");
			setdot();
			setai(exclam());
			newline();
			deletenone();
			append(gettty, setin(addr2));
			lchngflag = chngflag;
			continue;
		case 'c':
			switch (peekchar()) {
				case 'o':
					getchar();
					if (peekchar() == 'm') {
						tail2of("comment");
						do
							c = getchar();
						while (c == '|' || !endcmd(c));
						continue;
					}
					tail2of("Copy");
					move();
					continue;
				case 'd':
					tail("cd");
					goto changdir;
				case 'h':
					getchar();
					if (peekchar() == 'd') {
						tail2of("chdir");
changdir:
#ifdef UNIMP
						if (savedfile[0] == '/')
							exclam();
						else
							quickly();
#endif
						skipwh();
						if (endcmd(peekchar())) {
							newline();
							p = home;
						} else
							getone(), p = file;
						if (chdir(p) < 0)
							filioerr(p);
						if (file[0] != '/')
							value(EDITED) = 0;
						continue;
					}
					tail2of(CHANGE);
					break;
				default:
					tail(CHANGE);
			}
			setai(exclam());
			setCNL();
			setin(addr1);
			delete();
			append(gettty, addr1 - 1);
			lchngflag = chngflag;
			continue;
		case 'd':
			tail("delete");
			setCNL();
			delete();
			appendnone();
			continue;
		case 'e':
			switch (peekchar()) {
				case 'x':
					getchar();
					if (peekchar() == 'p') {
						tail2of("expand");
						setCNL();
						xop(&xpand);
						continue;
					}
					tail2of("ex");
					break;
				case 'c':
					tail("echo");
					skipwh();
					do {
						c = getchar();
						switch (c) {
							case '|':
								putnl();
								break;
							case '\\':
								c = getchar();
							default:
								putchar(c);
						}
					} while (!endcmd(c));
					continue;
				default:
					tail("edit");
			}
			if (!exclam() && chngflag)
				c = 'E';
doecmd:
			filename(c);
			if (c == 'E') {
				ungetchar(lastchar());
				quickly();
			}
			setnoaddr();
			init();
			addr2 = zero;
			laste++;
			sync();
			rop(c);
			lchngflag = chngflag;
			continue;
		case 'f':
			tail("file");
			setnoaddr();
			filename(c);
			putnl();
			continue;
		case 'g':
			tail("global");
			global(!exclam());
			lchngflag = chngflag;
			continue;
		case 'i':
			tail("insert");
			setdot();
			nonzero();
			setai(exclam());
			newline();
			deletenone();
			setin(addr2);
			append(gettty, addr2 - 1);
			if (dot == zero && dol > zero)
				dot = one;
			lchngflag = chngflag;
			continue;
		case 'j':
			tail("join");
			c = exclam();
			setcount();
			nonzero();
			newline();
			if (addr1 == addr2) {
				if (addr1 == dol) {
					if (dol != one)
				error("Illegal $ in join|$join not allowed unless only one line in buffer");
				} else
					addr2++;
			}
			join(c);
			continue;
		case 'k':
casek:
			skipwh();
			c = getchar();
			if (endcmd(c))
				error("Mark what?|%s requires following letter", Command);
			newline();
			if (c != letter(c))
				error("Bad mark|Mark must specify a letter");
			setdot();
			nonzero();
			names[c - 'a'] = *addr2 &~ 01;
			continue;
		case 'm':
			if (peekchar() == 'a') {
				tail("mark");
				goto casek;
			}
			tail("Move");
			move();
			continue;
		case 'o':
			tail("open");
			oop();
			pflag = 0;
			lchngflag = chngflag;
			continue;
		case '|':
			endline = 0;
			goto caseline;
		case '\n':
			endline = 1;
caseline:
			notempty();
			if (addr2 == 0) {
				if (dot == dol)
					error("At EOF|At end-of-file");
				if (UPLINE && c == '\n' && !inglobal)
					c = '\013';
				addr2 = dot + 1;
			}
			addr1 = addr2;
			setdot();
			nonzero();
			getline(*addr1);
			if (c == '\013') {
				putchar('\013' | QUOTE);	/* prototype UPLINE */
				if (hadpr)
					shudclob = 1;
			}
			plines(addr1, addr2, 1);
			continue;
		case 'l':
			tail("list");
			setCNL();
			setlist();
			pflag = 0;
			goto print;
		case ':':
			setCNL();
			kflag = 1;
			setflav();
			pflag = 0;
			goto print;
		case '#':
numberit:
			setCNL();
			setnumb();
			pflag = 0;
			goto print;
		case 'p':
			switch (peekchar()) {

			case 'u':
				tail("put");
				setdot();
				eol();
				PUT();
				continue;
			case 'r':
				getchar();
				if (peekchar() == 'e') {
					tail2of(PRESERVE);
					eol();
					preserve();
					continue;
				}
				tail2of(PRINT);
				break;
			default:
				tail(PRINT);
				break;
			}
			setCNL();
			pflag = 0;
print:
			nonzero();
			if (CLEAR && addr2 - addr1 >= LINES)
				putchar('\032' | QUOTE);	/* proto */
			plines(addr1, addr2, 1);
			continue;
		case 'n':
			if (peekchar() == 'u') {
				tail("number");
				goto numberit;
			}
			tail("next");
			setnoaddr();
			quickly();
			if (getargs() == 0) {
				ungetchar(lastchar());
				return (1);
			}
			glob(genbuf, G);
			xargc0 = gargc;
			xargv0 = &G->Ava[0];
			rewind();
			return (2);
		case 'q':
			tail("quit");
			setnoaddr();
			c = quickly();
			eol();
			if (c)
				argc = 0;
			return (0);
		case 'r':
			if (peekchar() == 'e') {
				getchar();
				switch (peekchar()) {

				case 'w':
					tail2of("rewind");
					setnoaddr();
					quickly();
					newline();
					rewind();
					return (2);

				case 's':
					tail2of("reset");
					setNAEOL();
					REset();
					continue;

				case 'c':
					tail2of(RECOVER);
					setnoaddr();
					c = 'e';
					if (!exclam() && chngflag)
						c = 'E';
dorecover:
					filename(c);
					if (c == 'E') {
						ungetchar(lastchar());
						quickly();
					}
					init();
					addr2 = zero;
					laste++;
					sync();
					recover();
					rop2();
					revocer();
					if (status == 0)
						rop3(c);
					if (dol != zero)
						change();
					lchngflag = chngflag;
					continue;
				}
				tail2of(READ);
			} else
				tail(READ);
			if (savedfile[0] == 0 && dol == zero)
				c = 'e';
			filename(c);
			rop(c);
			lchngflag = chngflag;
			continue;
		case 's':
			switch (peekchar()) {

			case 'e':
				tail("set");
				setnoaddr();
				set(skipwh());
				continue;

			case 'h':
				tail("shell");
				setNAEOL();
				unix2("-i", 0, NIL);
				continue;

			case 'o':
				tail("source");
				setnoaddr();
				getone();
				source(file, 0);
				continue;

			case 'y':
				tail("sync");
				setNAEOL();
				synctmp();
				continue;
			}
		case '&':
		case '~':
			Command = "substitute";
			if (c == 's')
				tail(Command);
			if (!substitute(c))
				pflag = 0;
			continue;
		case 't':
			if (peekchar() == 'a') {
				tail("tabulate");
				c = exclam();
				setCNL();
				xop(&tabulate, c);
				continue;
			}
			tail("Transcribe");
			move();
			continue;
		case 'u':
			tail("undo");
			setnoaddr();
			markDOT();
			c = exclam();
			newline();
			undo(c);
			continue;
		case 'v':
			switch (peekchar()) {

			case 'e':
				tail("version");
				setNAEOL();
				printf("%s\n", version);
				flush();
				continue;

			case 'i':
				tail("visual");
				vop();
				pflag = 0;
				lchngflag = chngflag;
				continue;
			}
			tail("v");
			global(0);
			lchngflag = chngflag;
			continue;
		case 'w':
			tail("write");
			setall();
			wop();
			lchngflag = chngflag;
			continue;
		case 'x':
			tail("xpand");
			setCNL();
			xop(&xpand);
			continue;
		case 'y':
			tail("yank");
			setcount();
			eol();
			yank();
			continue;
		case 'z':
			zop();
			pflag = 0;
			continue;
		case '=':
			newline();
			setall();
			printf("%d\n", addr2 - zero);
			continue;
		case '!':
			unix();
			continue;
		case '<':
		case '>':
			for (cnt = 1; peekchar() == c; cnt++)
				getchar();
			setCNL();
			shift(c, cnt);
			continue;
		case EOF:
			/* onhup for chtty !?! */
			if (exitoneof || gTTY(0) == -1)
				return (0);
			if (dol == zero) {
				putnl();
				notempty();
			}
			ungetchar(EOF);
			zop(hadpr);
			continue;
		case 'h':
			tail("help");
			setnoaddr();
			helpthem();
			continue;
		default:
			if (!letter(c))
				break;
			ungetchar(c);
			tailof("", 0);
		}
		error("What?|Unknown command character '%c'", c);
	}
}

quickly()
{

	if (exclam())
		return (1);
	if (chngflag) {
		chngflag = 0;
		xchngflag = 0;
		error("No write@since last change");
	}
	return (0);
}

change()
{

	tchngflag++;
	chngflag = tchngflag;
}

sync()
{

	chngflag = 0;
	tchngflag = 0;
	xchngflag = 0;
}

plines(adr1, adr2, movedot)
	int *adr1;
	register int *adr2;
	char movedot;
{
	register int *addr;

	for (addr = adr1; addr <= adr2; addr++) {
		getline(*addr);
		pline(addr - zero);
		if (movedot)
			dot = addr;
	}
}

int	normline();

newline()
{
	register c;

	resetflav();
	for (;;) {
		c = getchar();
		switch (c) {
			case '^':
			case '-':
				poffset--;
				break;
			case '+':
				poffset++;
				break;
			case 'l':
				listf++;
				break;
			case ':':
				kflag++;
				break;
			case '#':
				nflag++;
				break;
			case 'p':
				listf = 0;
				break;
			case ' ':
			case '\t':
				continue;
			default:
				if (!endcmd(c))
					error("Extra chars|Extra characters at end of \"%s\" command", Command);
				if (c == EOF)
					ungetchar(c);
				setflav();
				return;
		}
		pflag++;
	}
}

eol()
{

	skipwh();
	if (!endcmd(getchar()))
		error("Extra chars|Extra characters at end of command");
}

resetflav()
{

	listf = 0;
	kflag = 0;
	nflag = 0;
	pflag = 0;
	poffset = 0;
	setflav();
}

int	(*Pline)(), normline(), numbline();

setflav()
{

	if (nflag || kflag == 0 && value(NUMBER))
		setnumb();
	else
	     /* setnonumb(); */
		Pline = &normline;
	if (listf || kflag == 0 && value(LIST))
		setlist();
	else
		setnorm();
	setoutt();
}

endcmd(ch)
{
	switch (ch) {
		case '\n':
		case EOF:
			endline = 1;
			return (1);
		case '|':
			endline = 0;
			return (1);
	}
	return (0);
}

tailspec(c)
	char c;
{
	static char foocmd[2];

	foocmd[0] = c;
	Command = foocmd;
}

tail(comm)
	char *comm;
{

	tailof(comm, 1);
	return (comm);
}

tail2of(comm)
	char *comm;
{

	return (tailof(comm, 2));
}

tailof(comm, i)
	register char *comm;
	int i;
{
	register char *cp;
	register int c;
	char command[20];

	Command = comm;
	for (cp = command; i > 0; i--)
		*cp++ = *comm++;
	while (*comm && peekchar() == *comm)
		*cp++ = getchar(), comm++;
	c = peekchar();
	if (letter(c) && c != 'l' && c != 'p') {
		do
			*cp++ = getchar();
		while (cp < &command[19] && letter(peekchar()));
		*cp = 0;
		error("What?|%s: Not an editor command", command);
	}
}

exclam()
{

	skipwh();
	if (peekchar() == '!') {
		getchar();
		return (1);
	}
	return (0);
}
