/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_command.c	7.3 (Berkeley) 12/15/86
 */

#include "../kdb/defs.h"

char	*BADEQ;
char	*NOMATCH;
char	*BADVAR;
char	*BADCOM;

int	executing;
char	*lp;

char	lastc;
char	eqformat[512] = "z";
char	stformat[512] = "X\"= \"^i";

long	ditto;
int	lastcom = '=';
long	locval;
long	locmsk;
long	expv;

/* command decoding */

command(buf, defcom)
	char *buf, defcom;
{
	register itype, ptype, modifier, regptr;
	int longpr, eqcom;
	char wformat[1], savc;
	register long w, savdot;
	char *savlp=lp;

	if (buf) {
		if (*buf==EOR)
			return (0);
		lp=buf;
	}

	do {
		if (adrflg=expr(0)) {
			dot=expv;
			ditto=dot;
		}
		adrval=dot;
		cntflg = (rdc() == ',' && expr(0));
		if (cntflg)
			cntval=expv;
		else
			cntval=1, lp--;
		if (eol(rdc())) {
			if (!adrflg)
				dot=inkdot(dotinc);
			lp--; lastcom=defcom;
		} else
			lastcom=lastc;
		switch (lastcom&STRIP) {

		case '/':
			itype=DSP; ptype=DSYM;
			goto trystar;

		case '=':
			itype=NSP; ptype=0;
			goto trypr;

		case '?':
			itype=ISP; ptype=ISYM;
			goto trystar;

		trystar:
			if (rdc()=='*')
				lastcom |= QUOTE;
			else
				lp--;
			if (lastcom&QUOTE) {
				itype |= STAR;
				ptype = (DSYM+ISYM)-ptype;
			}

		trypr:
			longpr=0; eqcom=lastcom=='=';
			switch (rdc()) {

			case 'L':
				longpr=1;
			case 'l':
				/*search for exp*/
				if (eqcom)
					error(BADEQ);
				dotinc=(longpr?4:2); savdot=dot;
				(void) expr(1); locval=expv;
				if (expr(0))
					locmsk=expv;
				else
					locmsk = -1L;
				if (!longpr) {
					locmsk &= 0xFFFF;
					locval &= 0xFFFF;
				}
				for (;;) {
					w=get(dot,itype);
					if (errflg || mkfault ||
					    (w&locmsk)==locval)
						break;
					 dot=inkdot(dotinc);
				}
				if (errflg) {
					dot=savdot;
					errflg=NOMATCH;
				}
				psymoff(dot,ptype,"");
				break;

			case 'W':
				longpr=1;
			case 'w':
				if (eqcom)
					error(BADEQ);
				wformat[0]=lastc; (void) expr(1);
				do {
					savdot=dot;
					psymoff(dot,ptype,":%16t");
					(void) exform(1,wformat,itype,ptype);
					errflg=0; dot=savdot;
					if (longpr)
						put(dot,itype,expv);
					else
						put(dot,itype,
						    itol(expv,get(dot,itype)));
					savdot=dot;
					printf("=%8t");
					(void) exform(1,wformat,itype,ptype);
					printc(EOR);
				} while (expr(0) && errflg==0);
				dot=savdot;
				chkerr();
				break;

			default:
				lp--;
				getformat(eqcom ? eqformat : stformat);
				if (!eqcom)
					psymoff(dot,ptype,":%16t");
				scanform(cntval,
				    (eqcom?eqformat:stformat),itype,ptype);
			}
			break;

		case '>':
			lastcom=0; savc=rdc();
			if ((regptr=getreg(savc)) != -1)
				*(int *)regptr = dot;
			else if ((modifier=varchk(savc)) != -1)
				var[modifier]=dot;
			else
				error(BADVAR);
			break;

		case '$':
			lastcom=0;
			printtrace(nextchar());
			break;

		case ':':
			if (executing)
				break;
			executing=1; subpcs(nextchar()); executing=0;
			lastcom=0;
			break;

		case '\0':
			break;

		default:
			error(BADCOM);
		}
		flushbuf();
	} while (rdc()==';');
	if (buf)
		lp=savlp;
	else
		lp--;
	return (adrflg && dot!=0);
}
