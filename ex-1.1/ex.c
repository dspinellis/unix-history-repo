#
/*
 * Ex - a text editor
 * Bill Joy UCB
 * Version 1.0 September, 1977
 */

#include "ex.h"
#include "ex_glob.h"

int	vyancnt;
char	endline	1;

/*
 * Ex uses a modified C runtime startup which allocates
 * and associated pointers for the "next" command.
 * The pointer G is initialized by this startup to point
 * to this area, and the pointer xargv0 and the counter
 * xargc0 are also initialized to point to the arguments.
 */
main()
{
	char oop, nop, inite, reenter;
	register char **axargv0;
	register int c;

	axargv0 = xargv0;
	if (axargv0[0][0] == 'a')
		erpath =+ 9;
	if (axargv0[0][1] == 'd' || axargv0[0][2] == 'd') {
		value(OPEN) = 0;
		value(NOTIFY) = 1;
		value(MAGIC) = 0;
	}
	axargv0++;
	xargc0--;
	draino();
	helpinit();
	oldhup = signal(HUP, onhup);
	oldquit = signal(QUIT, 1);
	ruptible = (signal(INTR, 1) & 01) == 0;
	fendcore = sbrk(0);
	oop = 0;
	nop = 0;
	while (xargc0 && axargv0[0][0] == '-') {
		c = axargv0[0][1];
		if (c == 0) {
			value(HUSH) = 1;
			value(AUTOPRINT) = 0;
			value(OPTIMIZE) = 0;
			nop = 1;
		} else switch (c) {
			case 'n':
				value(OPTIMIZE) = 0;
				nop = 1;
				break;
#ifdef UNIMP
			case 'a':
				allredraw++;
				break;
#endif
			case 'p':
				value(PROMPT) = 0;
				break;
			case 'r':
				recov++;
				break;
			case 'o':
				oop = 1;
				break;
			default:
				printf("Unknown option %s\n", xargv[0]);
				flush();
				break;
		}
		xargc0--;
		axargv0++;
	}
	if (recov && xargc0 == 0) {
		printf("Recover which file?\n");
		flush();
		recov = 0;
	}
	xargv0 = axargv0;
	rewind();
	reenter = 0;
	setexit();
	if (reenter == 0) {
		reenter++;
		if (ruptible)
			signal(INTR, onintr);
		if (nop == 0)
			initoptions(oop);
		else
			setterm("un");
	}
top:
	noteargs();
	if (argc) {
		ungetchar('\n');
		setlastchar('\n');
	}
	for(;;) {
		xargc = argc;
		if (argc > 0) {
			argc--;
			if (savedfile[0])
				strcpy(altfile, savedfile);
			strcpy(savedfile, *xargv);
			inite = -2;
		}
		init();
		setexit();
		for(;;) {
			if (inite)
				inite++;
			switch (commands(inite, 0)) {
				case 1:
					goto next;
				case 2:
					if (argc == 0) {
						xargc = 0;
						goto nofiles;
					}
					goto top;
			}
			if (argc == 0 || argc == xargc) {
				cleanup();
				exit(0);
			}
			xargc = argc;
			if (savedfile[0])
				strcpy(altfile, savedfile);
			savedfile[0] = 0;
			init();
			error("%d more file%s@to edit", argc, plural(argc));
		}
next:
		if (argc > 0)
			xargv++;
		else {
nofiles:
			if (savedfile[0])
				strcpy(altfile, savedfile);
			savedfile[0] = 0;
			init();
			error("No more files@to edit");
		}
	}
}

onintr()
{

	signal(INTR, onintr);
	pstop();
	setlastchar('\n');
     /* termreset() */
	error("\nInterrupt");
}

rewind()
{

	xargc = argc = xargc0;
	xargv = xargv0;
}

init()
{
	register int i;
	extern int vyancnt;

	fileinit();
	brk(fendcore + 1);
	dot = zero = dol = fendcore;
	one = zero+1;
	undkind = UNDNONE;
	endcore = fendcore - 2;
	chngflag = 0;
	value(EDITED) = 0;
	for (i = 0; i <= 26; i++)
		names[i] = 1;
	vyancnt = 0;
}
