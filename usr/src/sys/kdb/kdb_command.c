/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_command.c	7.4 (Berkeley) 5/3/90
 */

#include "../kdb/defs.h"

char	*kdbBADEQ;
char	*kdbNOMATCH;
char	*kdbBADVAR;
char	*kdbBADCOM;

int	kdbexecuting;
char	*kdblp;

char	kdblastc;
char	kdbeqformat[512] = "z";
char	kdbstformat[512] = "X\"= \"^i";

long	kdbditto;
int	kdblastcom = '=';
long	kdblocval;
long	kdblocmsk;
long	kdbexpv;

/* command decoding */

kdbcommand(buf, defcom)
	char *buf, defcom;
{
	register itype, ptype, modifier, regptr;
	int longpr, eqcom;
	char wformat[1], savc;
	register long w, savdot;
	char *savlp=kdblp;

	if (buf) {
		if (*buf==EOR)
			return (0);
		kdblp=buf;
	}

	do {
		if (kdbadrflg=kdbexpr(0)) {
			kdbdot=kdbexpv;
			kdbditto=kdbdot;
		}
		kdbadrval=kdbdot;
		kdbcntflg = (kdbrdc() == ',' && kdbexpr(0));
		if (kdbcntflg)
			kdbcntval=kdbexpv;
		else
			kdbcntval=1, kdblp--;
		if (kdbeol(kdbrdc())) {
			if (!kdbadrflg)
				kdbdot=kdbinkdot(kdbdotinc);
			kdblp--; kdblastcom=defcom;
		} else
			kdblastcom=kdblastc;
		switch (kdblastcom&STRIP) {

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
			if (kdbrdc()=='*')
				kdblastcom |= QUOTE;
			else
				kdblp--;
			if (kdblastcom&QUOTE) {
				itype |= STAR;
				ptype = (DSYM+ISYM)-ptype;
			}

		trypr:
			longpr=0; eqcom=kdblastcom=='=';
			switch (kdbrdc()) {

			case 'L':
				longpr=1;
			case 'l':
				/*search for exp*/
				if (eqcom)
					kdberror(kdbBADEQ);
				kdbdotinc=(longpr?4:2); savdot=kdbdot;
				(void) kdbexpr(1); kdblocval=kdbexpv;
				if (kdbexpr(0))
					kdblocmsk=kdbexpv;
				else
					kdblocmsk = -1L;
				if (!longpr) {
					kdblocmsk &= 0xFFFF;
					kdblocval &= 0xFFFF;
				}
				for (;;) {
					w=kdbget(kdbdot,itype);
					if (kdberrflg || kdbmkfault ||
					    (w&kdblocmsk)==kdblocval)
						break;
					 kdbdot=kdbinkdot(kdbdotinc);
				}
				if (kdberrflg) {
					kdbdot=savdot;
					kdberrflg=kdbNOMATCH;
				}
				kdbpsymoff(kdbdot,ptype,"");
				break;

			case 'W':
				longpr=1;
			case 'w':
				if (eqcom)
					kdberror(kdbBADEQ);
				wformat[0]=kdblastc; (void) kdbexpr(1);
				do {
					savdot=kdbdot;
					kdbpsymoff(kdbdot,ptype,":%16t");
					(void) kdbexform(1,wformat,itype,ptype);
					kdberrflg=0; kdbdot=savdot;
					if (longpr)
						kdbput(kdbdot,itype,kdbexpv);
					else
						kdbput(kdbdot,itype,
						    itol(kdbexpv,kdbget(kdbdot,itype)));
					savdot=kdbdot;
					kdbprintf("=%8t");
					(void) kdbexform(1,wformat,itype,ptype);
					kdbprintc(EOR);
				} while (kdbexpr(0) && kdberrflg==0);
				kdbdot=savdot;
				kdbchkerr();
				break;

			default:
				kdblp--;
				kdbgetformat(eqcom ? kdbeqformat : kdbstformat);
				if (!eqcom)
					kdbpsymoff(kdbdot,ptype,":%16t");
				kdbscanform(kdbcntval,
				    (eqcom?kdbeqformat:kdbstformat),itype,ptype);
			}
			break;

		case '>':
			kdblastcom=0; savc=kdbrdc();
			if ((regptr=kdbgetreg(savc)) != -1)
				*(int *)regptr = kdbdot;
			else if ((modifier=kdbvarchk(savc)) != -1)
				kdbvar[modifier]=kdbdot;
			else
				kdberror(kdbBADVAR);
			break;

		case '$':
			kdblastcom=0;
			kdbprinttrace(kdbnextchar());
			break;

		case ':':
			if (kdbexecuting)
				break;
			kdbexecuting=1; kdbsubpcs(kdbnextchar()); kdbexecuting=0;
			kdblastcom=0;
			break;

		case '\0':
			break;

		default:
			kdberror(kdbBADCOM);
		}
		kdbflushbuf();
	} while (kdbrdc()==';');
	if (buf)
		kdblp=savlp;
	else
		kdblp--;
	return (kdbadrflg && kdbdot!=0);
}
