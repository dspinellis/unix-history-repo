#ifndef lint
static char sccsid[] = "@(#)print.c	5.3 (Berkeley) 4/9/89";
#endif

/*
 * adb - printing routines
 */

#include "defs.h"
#include <sys/file.h>

extern char LONGFIL[];
extern char NOTOPEN[];
extern char BADMOD[];

int	infile;			/* XXX */
int	outfile;		/* XXX */

off_t	lseek();

/* general printing routines ($) */

printtrace(modif)
	int modif;
{
	int stack, i;

	switch (modif) {

	case '<':
		if (ecount == 0) {
			while (readchar() != '\n')
				/* void */;
			unreadc();
			break;
		}
		if (rdc() == '<')
			stack = 1;
		else {
			stack = 0;
			unreadc();
		}
		/* FALLTHROUGH */

	case '>': {
		char file[64];
		char Ifile[128];
		extern char *Ipath;
		int index;
		char *strcpy(), *strcat();

		index = 0;
		if (rdc() != '\n') {
			do {
				file[index++] = lastc;
				if (index >= 63)
					error(LONGFIL);
			} while (readchar() != '\n');
			file[index] = 0;
			if (modif == '<') {
				if (Ipath) {
					(void) strcpy(Ifile, Ipath);
					(void) strcat(Ifile, "/");
					(void) strcat(Ifile, file);
				}
				if (strcmp(file, "-") != 0) {
					iclose(stack, 0);
					infile = open(file, 0);
					if (infile < 0)
						infile = open(Ifile, 0);
				} else
					(void) lseek(infile, 0L, 0);
				if (infile < 0) {
					infile = 0;
					error(NOTOPEN);
					/* NOTREACHED */
				}
				var[9] = ecount;
			} else {
				oclose();
				outfile = open(file, O_CREAT|O_WRONLY, 0644);
				(void) lseek(outfile, 0L, 2);
			}
		} else {
			if (modif == '<')
				iclose(-1, 0);
			else
				oclose();
		}
		unreadc();
		break;
	}

	case 'p':
		if (!kernel)
			prints("not debugging kernel\n");
		else {
			if (gavedot)
				setpcb(dot);
			getpcb();
		}
		break;

	case 'd':
		if (gavedot) {
			i = edot;
			if (!(i >= 2 && i <= 16 || i <= -2 && i >= -16)) {
				adbprintf("illegal radix %d base ten",
				    (expr_t)i);
				break;
			}
			radix = i;
		}
		adbprintf("radix=%D base ten", (expr_t)radix);
		break;

	case 'q':
	case 'Q':
	case '%':
		done(0);

	case 'w':
	case 'W':
		maxcol = gavedot ? edot : MAXCOL;
		break;

	case 's':
	case 'S':
		maxoff = gavedot ? edot : MAXOFF;
		break;

	case 'v':
	case 'V':
		prints("variables\n");
		for (i = 0; i <= 35; i++)
			if (var[i])
				adbprintf("%c = %R\n",
				    i > 9 ? i + 'a' - 10 : i + '0',
				    var[i]);
		break;

	case 'm':
	case 'M':
		printmap("? map", &txtmap);
		printmap("/ map", &datmap);
		break;

	case 0:
	case '?':
		if (pid)
			adbprintf("pcs id = %D\n", (expr_t)pid);
		else
			prints("no process\n");
		sigprint();
		flushbuf();
		/* FALLTHROUGH */

	case 'r':
	case 'R':
		printregs();
		return;

	case 'c':
	case 'C':
		printstack(modif == 'C', gavecount ? (int)ecount : -1);
		break;

	case 'e':
	case 'E':
		/* print externals */
		printsyms(modif == 'E');
		break;


	case 'b':
	case 'B':
		/* print breakpoints */
		printbkpts();
		break;

	default:
		error(BADMOD);
	}
}

printmap(s, m)
	char *s;
	register struct map *m;
{

	adbprintf("%s%12t`%s'\n", s, m->ufd < 0 ? "-" :
		m->ufd == corefile.fd ? corefile.name : symfile.name);
	adbprintf("b1 = %-16Re1 = %-16Rf1 = %-16R\n",
	    m->m1.b, m->m1.e, m->m1.f);
	adbprintf("b2 = %-16Re2 = %-16Rf2 = %-16R\n",
	    m->m2.b, m->m2.e, m->m2.f);
}

/*
 * Print global data and bss symbols, and if texttoo, text symbols too.
 */
printsyms(texttoo)
	int texttoo;
{
	register struct nlist *sp;

	if (symtab == NULL)
		return;
	for (sp = symtab; sp < esymtab; sp++) {
		if ((sp->n_type & N_EXT) == 0)
			continue;
		switch (sp->n_type) {

		case N_TEXT|N_EXT:
			if (texttoo)
				adbprintf("%s:%12t@ %R\n",
					  sp->n_un.n_name,
					  (expr_t)sp->n_value);
			break;

		case N_DATA|N_EXT:
		case N_BSS|N_EXT:
			adbprintf("%s:%12t", sp->n_un.n_name);
			prfrom((addr_t)sp->n_value, '\n');
			break;
		}
	}
}

/*
 * Print the value stored in some location, or `?' if it cannot be read,
 * then the character c (usually '\n' or ',').
 */
prfrom(a, c)
	addr_t a;
	int c;
{
	expr_t v;

	errflag = NULL;
	if (adbread(SP_DATA, a, &v, sizeof(v)) == sizeof(v) && errflag == NULL)
		adbprintf("%R\%c", v, c);
	else {
		errflag = NULL;
		adbprintf("?%c", c);
	}
}

#ifdef busted
/*
 * Print a local symbol (called from printstack()).
 * Local symbols end with ':', so cannot use %s format.
 */
printlsym(cp)
	register char *cp;
{

	while (*cp && *cp != ':')
		printc(*cp++);
}
#endif

printregs()
{
	register struct reglist *p;
	expr_t v;
	extern struct reglist reglist[];

	for (p = reglist; p->r_name != NULL; p++) {
		v = getreg(p);
		adbprintf("%s%6t%R\%16t", p->r_name, v);
		valpr(v, ispace_reg(p) ? SP_INSTR : SP_DATA);
		printc('\n');
	}
	printpc();
}

printpc()
{

	dot = getpc();
	pdot();
	printins(SP_INSTR);
	printc('\n');
}
