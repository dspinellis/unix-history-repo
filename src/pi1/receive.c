/* Copyright (c) 1979 Regents of the University of California */
#include "0.h"
#include "send.h"
#include "tree.h"
/*
 * Pi - Pascal interpreter code translator
 *
 * Bill Joy UCB February 6, 1978
 */

int	fp2[DSPLYSZ];
int	pin[259];	/* Unit 0 */
int	ackd, acker;

#ifdef DEBUG
extern	char *trnames[];
#endif
extern	int *spacep;
extern	char printed, hadsome, *lastname, *obj;

#ifdef DEBUG
char	*rnames[] {
	"",
	"RINIT",
	"RENQ",
	"RTREE",
	"RTRFREE",
	"RTRCHK",
	"REVENIT",
	"RSTRING",
	"REVLAB",
	"REVCNST",
	"REVTBEG",
	"REVTYPE",
	"REVTEND",
	"REVVBEG",
	"REVVAR",
	"REVVEND",
	"REVFHDR",
	"REVFFWD",
	"REVFBDY",
	"REVFEND",
	"ROPUSH",
	"ROPOP",
	"ROSET",
	"RKILL",
	"RFINISH",
};
#endif

#define	getaw()		getw(pin)
#define	getac()		getc(pin)
#define	sgetaw()	sreloc(getaw())
#define	tgetaw()	treloc(getaw())

receive()
{
	register int i, *ip;
	register char *cp;
#define TREENMAX 6	/* From tree.c */
	int loctree[TREENMAX * 2], locstring[514];
	int ch, j;

	for (;;) {
		i = getac();
#ifdef DEBUG
		if (i > 0 && i <= RLAST)
			dprintf("%s\t", rnames[i]);
		else if (i == -1)
			dprintf("EOF\t");
		else
			dprintf("OOPS!\t");
#endif
		holdderr = 1;
		switch (i) {

		case RINIT:
			lastname = sreloc(getaw());
			obj = sreloc(getaw());
			ackd = getac();
			for (i = 0; i < 26; i++)
				opts[i] = getac();
			efil = getac();
			errfile = sreloc(getaw());
			ofil = getac();
			magic();
#ifdef DEBUG
			dprintf("RINIT\n\t");
			dprintf("lastname \"%s\"\n\t", lastname);
			dprintf("ackd %d\n\t", ackd);
			dprintf("options: ");
			for (i = 0; i < 26; i++)
			if (opts[i])
				if (i == 1)
					dprintf("b%d ", opts[1]);
				else
					dprintf("%c ", i + 'a');
			dprintf("\n\tefil %d\n", efil);
			dprintf("\terrfile \"%s\"\n", errfile);
			dprintf("\tofil %d\n", ofil);
#endif
			break;

		case RENQ:
#ifdef DEBUG
			dprintf("\tACK\n");
#endif
			ack();
			break;

		case RTREE:
#ifdef DEBUG
			dprintf("%d ", toffset(spacep));
#endif
			i = getac();
			if (i < 0 || i > T_LAST) {
#ifdef DEBUG
				dprintf("\tBAD: %d\n", i);
#endif
				panic("recv RTREE");
				exit(1);
			}
			cp = trdesc[i];
#ifdef DEBUG
			dprintf("\t%s:", trnames[i]);
#endif
			ip = loctree;
			*ip++ = i;
			j = 1;
			while (*cp) {
				j++;
				switch (*cp++) {

				case 's':
					cp = locstring;
					i = 512;
#ifdef DEBUG
					dprintf(" \"");
#endif
					while ((ch = getac()) && ch != -1) {
						if (--i == 0)
							panic("RTREE case s");
						*cp++ = ch;
#ifdef DEBUG
						dprintf("%c", ch);
#endif
					}
#ifdef DEBUG
					dprintf("\"\n");
#endif
					*cp++ = 0;
					copystr(locstring);
					goto out;

				case 'd':
					*ip++ = getac();
#ifdef DEBUG
					dprintf(" d%d", ip[-1]);
#endif
					continue;

				case 'n':
					*ip++ = getaw();
#ifdef DEBUG
					dprintf(" n%d", ip[-1]);
#endif
					continue;

				case '"':
					*ip++ = sreloc(getaw());
#ifdef DEBUG
					if (ip[-1] == NIL)
						dprintf(" NIL\"");
					else
						dprintf(" \"%s\"", ip[-1]);
#endif
					continue;

				case 'p':
					*ip++ = treloc(getaw());
#ifdef DEBUG
					dptree(ip[-1]);
#endif
					continue;
				}
			}
#ifdef DEBUG
			dprintf("\n");
#endif
			treev(j, loctree);
out:
			break;

		case RTRCHK:
			i = getaw();
#ifdef DEBUG
			dprintf(" %d\n", i);
			if (i != toffset(spacep))
				dprintf("trchk %d, have %d\n", i, toffset(spacep));
#endif
			break;

		case RTRFREE:
#ifdef DEBUG
			dprintf("\t%d\n", toffset(spacep));
#endif
			trfree();
			break;

		case REVTBEG:
#ifdef DEBUG
			dprintf("\n");
#endif
			typebeg();
			break;

		case REVTEND:
#ifdef DEBUG
			dprintf("\n");
#endif
			typeend();
			break;

		case REVVBEG:
#ifdef DEBUG
			dprintf("\n");
#endif
			varbeg();
			break;

		case REVVEND:
#ifdef DEBUG
			dprintf("\n");
#endif
			varend();
			break;

		case REVENIT:
#ifdef DEBUG
			dprintf("\n");
#endif
			evenit();
			break;

		case RSTRING:
#ifdef DEBUG
			dprintf(" \"");
#endif
			cp = locstring;
			i = 512;
			while ((ch = getac()) && ch != -1) {
				if (--i == 0)
					panic("RSTRING length");
				*cp++ = ch;
#ifdef DEBUG
				dprintf("%c", ch);
#endif
			}
#ifdef DEBUG
			dprintf("\"\n");
#endif
			*cp++ = 0;
			savestr(locstring);
			break;

		case REVLAB:
			loctree[0] = treloc(getaw());
#ifdef DEBUG
			dptree(loctree[0]);
			dprintf("\n");
#endif
			label(loctree[0]);
			break;

		case REVCNST:
			loctree[0] = getaw();
			loctree[1] = sreloc(getaw());
			loctree[2] = treloc(getaw());
#ifdef DEBUG
			dprintf(" %d", loctree[0]);
			dprintf(" \"%s\"", loctree[1]);
			dptree(loctree[2]);
			dprintf("\n");
#endif
			const(loctree[0], loctree[1], loctree[2]);
			break;

		case REVTYPE:
			loctree[0] = getaw();
			loctree[1] = sreloc(getaw());
			loctree[2] = treloc(getaw());
#ifdef DEBUG
			dprintf(" %d", loctree[0]);
			dprintf(" \"%s\"", loctree[1]);
			dptree(loctree[2]);
			dprintf("\n");
#endif
			type(loctree[0], loctree[1], loctree[2]);
			break;

		case REVVAR:
			loctree[0] = getaw();
			loctree[1] = treloc(getaw());
			loctree[2] = treloc(getaw());
#ifdef DEBUG
			dprintf(" %d", loctree[0]);
			dptree(loctree[1]);
			dptree(loctree[2]);
			dprintf("\n");
#endif
			var(loctree[0], loctree[1], loctree[2]);
			break;

		case REVFHDR:
			loctree[0] = treloc(getaw());
#ifdef DEBUG
			dptree(loctree[0]);
			dprintf("\n");
#endif
			fp2[cbn] = funchdr(loctree[0]);
			break;

		case REVFBDY:
#ifdef DEBUG
			dprintf("\n");
#endif
			funcbody(fp2[cbn]);
			break;

		case REVFEND:
			holdderr = 0;
			loctree[0] = treloc(getaw());
			loctree[1] = getaw();
			loctree[2] = getaw();
			lastname = sreloc(getaw());
			filename = sreloc(getaw());
			printed = getac();
			hadsome = getac();
#ifdef DEBUG
			dptree(loctree[0]);
			dprintf(" %d", loctree[1]);
			dprintf(" %d", loctree[2]);
			dprintf(" lastname=%s", lastname);
			dprintf(" filename=%s", filename);
			dprintf(" printed=%d", printed);
			dprintf(" hadsome=%d", hadsome);
			dprintf("\n");
#endif
			funcend(fp2[cbn-1], loctree[0], loctree[1], loctree[2]);
			break;

		case ROPUSH:
			i = getaw();
			opush(i);
#ifdef DEBUG
			dprintf(" %c\n", i);
#endif
			break;

		case ROPOP:
			i = getaw();
			opop(i);
#ifdef DEBUG
			dprintf(" %c\n", i);
#endif
			break;

		case ROSET:
			ch = getac();
			i = getaw();
#ifdef DEBUG
			dprintf(" %c=", ch);
			dprintf("%d\n", i);
#endif
			opt(ch) = i;
			break;

		case RKILL:
#ifdef DEBUG
			dprintf("I should be dead!\n");
#endif
			panic("RKILL");
			break;

		case RFINISH:
#ifdef DEBUG
			dprintf("\n");
#endif
			magic2();
			write(ackd, &eflg, 2);
			break;

		case -1:
			ack();
#ifdef DEBUG
			dprintf("\nEXIT\n");
#endif
			exit(0);

		default:
#ifdef DEBUG
			dprintf("CODE=%d\n", i);
#endif
			panic("rcv CODE");
		}
	}
}

ack()
{
	extern Fp;
	int i[3];

	i[0] = lastname;
	i[1] = Fp;
	i[2] = (hadsome << 8) | printed;
	write(ackd, i, 6);
}

#ifdef DEBUG
dprintf(a1, a2, a3, a4, a5)
{
	if (opt('d'))
		printf(a1, a2, a3, a4, a5);
}

dptree(j)
	int j;
{
	register int i;

	i = toffset(j);
	if (i >= ITREE)
		dprintf(" p%d", i);
	else if (i == 0)
		dprintf(" NIL");
	else
		dprintf("  \"%s\"", j);
}
#endif
