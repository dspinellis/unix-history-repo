/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_format.c	7.5 (Berkeley) %G%
 */

#include "../kdb/defs.h"

char	*kdbBADMOD;
char	*kdbADWRAP;

char	*kdblp;
char	kdblastc,kdbpeekc;
long	kdbexpv;

kdbscanform(icount,ifp,itype,ptype)
	long icount;
	char *ifp;
{
	register char *fp;
	char modifier;
	register fcount, init=1;
	long savdot;
	int exact;

	while (icount) {
		fp=ifp;
		savdot=kdbdot; init=0;
		if (!init && (exact=(kdbfindsym(kdbdot,ptype)==0)) && kdbmaxoff)
			kdbprintf("\n%s:%16t",kdbcursym->n_un.n_name);
		/*now loop over format*/
		while (*fp && kdberrflg==0) {
			if (isdigit(modifier = *fp)) {
				fcount = 0;
				while (isdigit(modifier = *fp++)) {
				   fcount *= 10;
				   fcount += modifier-'0';
				}
				fp--;
			} else
				fcount = 1;
			if (*fp==0)
				break;
#ifdef	ENTRYMASK
			/* check for entry mask */
			if (exact && kdbdot==savdot && 
			   (kdbcursym->n_type&N_TYPE)==N_TEXT &&
			   kdbcursym->n_un.n_name[0]=='_' && *fp=='i') {
				(void) kdbexform(1,"x",itype,ptype);
				fp++;
				kdbprintc(EOR);
			} else
#endif
				fp = kdbexform(fcount,fp,itype,ptype);
		}
		kdbdotinc=kdbdot-savdot;
		kdbdot=savdot;

		if (kdberrflg) {
			if (icount<0) {
				 kdberrflg=0;
				 break;
			}
			kdberror(kdberrflg);
		}
		if (--icount)
			kdbdot=kdbinkdot(kdbdotinc);
		if (kdbmkfault)
			kdberror((char *)0);
	}
}

/*
 * Execute single format item `fcount' times
 * sets `dotinc' and moves `dot'
 * returns address of next format item
 */
char *
kdbexform(fcount,ifp,itype,ptype)
	int fcount;
	char *ifp;
{
	register POS w;
	register long savdot, wx;
	register char *fp;
	char c, modifier, longpr;

	while (fcount>0) {
	  	fp = ifp; c = *fp;
		longpr = (isupper(c) || c=='f' || c=='4' || c=='p');
		if (itype != NSP && *fp != 'a') {
			wx = kdbget(kdbdot, itype);
			w = shorten(wx);
		} else {
			wx = w = kdbdot;
			if (itype == NSP &&
			    (c == 'b' || c == 'B' ||
			     c == 'c' || c == 'C' || c == '1'))
				w = btol(wx);
		}
		if (kdberrflg)
			return (fp);
		if (kdbmkfault)
			kdberror((char *)0);
		kdbvar[0] = wx;
		modifier = *fp++;
		kdbdotinc = (longpr ? sizeof (long):sizeof (short));

		if (kdbcharpos()==0 && modifier!='a')
			kdbprintf("%16m");
		switch (modifier) {

		case SP: case TB:
			break;

		case 't': case 'T':
			kdbprintf("%T",fcount); return (fp);

		case 'r': case 'R':
			kdbprintf("%M",fcount); return (fp);

		case 'a':
			kdbpsymoff(kdbdot,ptype,":%16t"); kdbdotinc=0; break;

		case 'p':
			kdbpsymoff(kdbvar[0],ptype,"%16t"); break;

		case 'u':
			kdbprintf("%-8u",w); break;

		case 'U':
			kdbprintf("%-16U",wx); break;

		case 'c': case 'C':
			if (modifier == 'C')
				kdbprintesc((int)byte(w));
			else
				kdbprintc((char)byte(w));
			kdbdotinc=1; break;

		case 'b': case 'B':
			kdbprintf("%-8o", byte(w)); kdbdotinc=1; break;

		case '1':
			kdbprintf("%-8R", byte(w)); kdbdotinc=1; break;

		case 'w': case '2':
			kdbprintf("%-8R", w); break;

		case 'W': case '4':
			kdbprintf("%-16R", wx); break;

		case 's': case 'S':
			savdot=kdbdot; kdbdotinc=1;
			while ((c=byte(kdbget(kdbdot,itype))) && kdberrflg==0) {
				kdbdot=kdbinkdot(1);
				if (modifier == 'S')
					kdbprintesc((int)c);
				else
					kdbprintc(c);
				kdbendline();
			}
			kdbdotinc=kdbdot-savdot+1; kdbdot=savdot; break;

		case 'x':
			kdbprintf("%-8x",w); break;

		case 'X':
			kdbprintf("%-16X", wx); break;

		case 'z':
			kdbprintf("%-8z",w); break;

		case 'Z':
			kdbprintf("%-16Z", wx); break;

		case 'Y':
			kdbprintf("%-24Y", wx); break;

		case 'q':
			kdbprintf("%-8q", w); break;

		case 'Q':
			kdbprintf("%-16Q", wx); break;

		case 'o':
			kdbprintf("%-8o", w); break;

		case 'O':
			kdbprintf("%-16O", wx); break;

		case 'i': case 'I':
			kdbprintins(itype,wx); kdbprintc(EOR); break;

		case 'd':
			kdbprintf("%-8d", w); break;

		case 'D':
			kdbprintf("%-16D", wx); break;

		case 'n': case 'N':
			kdbprintc('\n'); kdbdotinc=0; break;

		case '"':
			kdbdotinc=0;
			while (*fp != '"' && *fp)
				kdbprintc(*fp++);
			if (*fp)
				fp++;
			break;

		case '^':
			kdbdot=kdbinkdot(-kdbdotinc*fcount); return (fp);

		case '+':
			kdbdot=kdbinkdot(fcount); return (fp);

		case '-':
			kdbdot=kdbinkdot(-fcount); return (fp);

		default:
			kdberror(kdbBADMOD);
		}
		if (itype!=NSP)
			kdbdot=kdbinkdot(kdbdotinc);
		fcount--; kdbendline();
	}
	return (fp);
}

static
kdbprintesc(c)
	register int c;
{

	c &= STRIP;
	if (c==0177 || c<SP)
		kdbprintf("^%c", c ^ 0100);
	else
		kdbprintc(c);
}

long
kdbinkdot(incr)
{
	register long newdot;

	newdot=kdbdot+incr;
	if (addrwrap(kdbdot, newdot))
		kdberror(kdbADWRAP);
	return (newdot);
}
