static char Sccsid[] = "aw.c @(#)aw.c	1.1	10/1/82 Berkeley ";
#include "apl.h"

char *opname[] = {

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
	"format",	/*102 */	/*	format		*/
	"label",	/*103 */
	"psi",		/*104 */
	"psi1",		/*105 */
	"psi2",		/*106 */
	"isp",		/*107 */
	"isp1",		/*108 */
	"isp2",		/*109 */
	"Lwidth",	/*110 */
	"Lfuzz",	/*111 */
	"Lrun",		/*112 */
	"Lfork",	/*113 */
	"Lwait",	/*114 */
	"Lexec",	/*115 */
	"Lfx",		/*116 */
	"Lexit",	/*117 */
	"Lpipe",	/*118 */
	"Lchdir",	/*119 */
	"Lopen",	/*120 */
	"Lclose",	/*121 */
	"Lread",	/*122 */
	"Lwrite",	/*123 */
	"Lcreat",	/*124 */
	"Lseek",	/*125 */
	"Lrm",		/*126 */
	"Lrd",		/*127 */
	"Ldup",		/*128 */
	"Lap",		/*129 */
	"Lkill",	/*130 */
	"Lcr",		/*131 */
	"dfmt",		/*132 */
	"mfmt",		/*133 */
	"Lnc",		/*134 */
	"nilret",	/*135 */
	"Llx",		/*136 */
	"ibr",		/*137 */
	"ibr0",		/*138 */
	"rval",		/*139 */
	"Lsig",		/*140 */
	"Lfloat",	/*141 */
	"Lnl",		/*142 */
};

dump(cp, flag)
char *cp;
{
	register char *s, *t;
	register i;

	s = cp;
	if(flag)
		printf("[ ");

loop:
	putchar(' ');
	if(column > 50)
		if(flag)
			printf(" ]\n[ ");
		else
			putchar('\n');
	i = *s++;
	if(i != EOF)
		i &= 0377;
	if(i >= 0 && i <= 140 && opname[i]) {
		t = opname[i];
		while(*t)
			putchar(*t++);
	} else if(i != EOF)
		printf("%d ", i);
	switch(i) {

	case EOL:
		if(*s != EOL)
			break;
	case EOF:
		printf(".");
		if(flag)
			printf(" ].");
		putchar('\n');
		return;

	case QUOT:
		i = *s++;
		s += i;
		break;

	case CONST:
		i = *s++;
		s += i*SDAT;
		break;

	case NAME:
	case FUN:
	case ARG1:
	case ARG2:
	case AUTO:
	case REST:
	case RVAL:
		s += copy(IN, s, &cp, 1);
		putchar('-');
		t = ((struct nlist *)cp)->namep;
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
