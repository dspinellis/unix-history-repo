#include "apl.h"

#ifdef FULLD

char *opname[]
{
	"eol",		/* 0 */
	"add",		/* 1 */
	"plus",		/* 2 */
	"sub",		/* 3 */
	"minus",	/* 4 */
	"mul",		/* 5 */
	"sgn",		/* 6 */
	"div",		/* 7 */
	"recip",	/* 8 */
	"mod",		/* 9 */
	"abs",		/* 10 */
	"min",		/* 11 */
	"floor",	/* 12 */
	"max",		/* 13 */
	"ceil",		/* 14 */
	"pwr",		/* 15 */
	"exp",		/* 16 */
	"log",		/* 17 */
	"loge",		/* 18 */
	"cir",		/* 19 */
	"pi",		/* 20 */
	"comb",		/* 21 */
	"fac",		/* 22 */
	"deal",		/* 23 */
	"rand",		/* 24 */
	"drho",		/* 25 */
	"mrho",		/* 26 */
	"diot",		/* 27 */
	"miot",		/* 28 */
	"rot0",		/* 29 */
	"rev0",		/* 30 */
	"dtrn",		/* 31 */
	"mtrn",		/* 32 */
	"dibm",		/* 33 */
	"mibm",		/* 34 */
	"gdu",		/* 35 */
	"gduk",		/* 36 */
	"gdd",		/* 37 */
	"gddk",		/* 38 */
	"exd",		/* 39 */
	"scan",		/* 40 */
	"exdk",		/* 41 */
	"scank",	/* 42 */
	"iprod",	/* 43 */
	"oprod",	/* 44 */
	"quad",		/* 45 */
	"qquad",	/* 46 */
	"br0",		/* 47 */
	"br",		/* 48 */
	"ddom",		/* 49 */
	"mdom",		/* 50 */
	"com",		/* 51 */
	"red",		/* 52 */
	"comk",		/* 53 */
	"redk",		/* 54 */
	"rot",		/* 55 */
	"rev",		/* 56 */
	"rotk",		/* 57 */
	"revk",		/* 58 */
	"cat",		/* 59 */
	"rav",		/* 60 */
	"catk",		/* 61 */
	"ravk",		/* 62 */
	"print",	/* 63 */
	"quot",		/* 64 */
	"elid",		/* 65 */
	"cquad",	/* 66 */
	"comnt",	/* 67 */
	"index",	/* 68 */
	"hprint",	/* 69 */
	0,		/* 70 */
	"lt",		/* 71 */
	"le",		/* 72 */
	"gt",		/* 73 */
	"ge",		/* 74 */
	"eq",		/* 75 */
	"ne",		/* 76 */
	"and",		/* 77 */
	"or",		/* 78 */
	"nand",		/* 79 */
	"nor",		/* 80 */
	"not",		/* 81 */
	"eps",		/* 82 */
	"meps",		/* 83 */
	"rep",		/* 84 */
	"take",		/* 85 */
	"drop",		/* 86 */
	"exd0",		/* 87 */
	"asgn",		/* 88 */
	"immed",	/* 89 */
	"name",		/* 90 */
	"const",	/* 91 */
	"fun",		/* 92 */
	"arg1",		/* 93 */
	"arg2",		/* 94 */
	"auto",		/* 95 */
	"rest",		/* 96 */
	"com0",		/* 97 */
	"red0",		/* 98 */
	"exd0",		/* 99 */
	"scan0",	/*100 */
	"base",		/*101 */
	"menc",         /*102 */        /*      monadic encode  */
	"label",	/*103 */
};

#endif

#ifdef SOMED

dump(cp)
char *cp;
{
	register char *s, *t;
	register i;

	s = cp;

loop:
	putchar(' ');
	if(column > 50)
		putchar('\n');
	i = *s++;
#ifdef FULLD
	if(i >= 0 && i <= 103 && opname[i]) {
		t = opname[i];
		while(*t)
			putchar(*t++);
	} else
#endif
		printf("%d", i);
	switch(i) {

	case EOL:
		if(*s != EOL)
			break;
	case EOF:
		putchar('\n');
		return;

	case QUOT:
		i = *s++;
		s =+ i;
		break;

	case CONST:
		i = *s++;
		s =+ i*SDAT;
		break;

	case NAME:
	case FUN:
	case ARG1:
	case ARG2:
	case AUTO:
	case REST:
		s =+ copy(IN, s, &cp, 1);
		putchar('-');
		t = cp->namep;
		while(*t)
			putchar(*t++);
		break;

	case INDEX:
	case IMMED:
		s++;
		break;
	}
	goto loop;
}
#endif
