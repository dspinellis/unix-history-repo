/*	kdb_format.c	7.1	86/11/20	*/

#include "../kdb/defs.h"

char	*BADMOD;
char	*ADWRAP;

char	*lp;
char	lastc,peekc;
long	expv;

scanform(icount,ifp,itype,ptype)
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
		savdot=dot; init=0;
		if (!init && (exact=(findsym(dot,ptype)==0)) && maxoff)
			printf("\n%s:%16t",cursym->n_un.n_name);
		/*now loop over format*/
		while (*fp && errflg==0) {
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
			/* check for entry mask */
			if (exact && dot==savdot && 
			   (cursym->n_type&N_TYPE)==N_TEXT &&
			   cursym->n_un.n_name[0]=='_' && *fp=='i') {
				exform(1,"x",itype,ptype);
				fp++;
				printc(EOR);
			} else
				fp=exform(fcount,fp,itype,ptype);
		}
		dotinc=dot-savdot;
		dot=savdot;

		if (errflg) {
			if (icount<0) {
				 errflg=0;
				 break;
			}
			error(errflg);
		}
		if (--icount)
			dot=inkdot(dotinc);
		if (mkfault)
			error(0);
	}
}

/*
 * Execute single format item `fcount' times
 * sets `dotinc' and moves `dot'
 * returns address of next format item
 */
char *
exform(fcount,ifp,itype,ptype)
	int fcount;
	char *ifp;
{
	register POS w;
	register long savdot, wx;
	register char *fp;
	char c, modifier, longpr;
	union{	/* compatible with both VAX and TAHOE */
		double	d;
		int	s[4];
	} fw;

	while (fcount>0) {
	  	fp = ifp; c = *fp;
		longpr=(c>='A')&&(c<='Z')||(c=='f')||(c=='4')||(c=='p');
		if (itype==NSP || *fp=='a') {
			wx=dot; w=dot;
		} else {
			wx=get(dot,itype);
			w=shorten(wx);
		}
		if (errflg)
			return (fp);
		if (mkfault)
			error(0);
		var[0]=wx;
		modifier = *fp++;
		dotinc=(longpr?4:2);

		if (charpos()==0 && modifier!='a')
			printf("%16m");
		switch (modifier) {

		case SP: case TB:
			break;

		case 't': case 'T':
			printf("%T",fcount); return (fp);

		case 'r': case 'R':
			printf("%M",fcount); return (fp);

		case 'a':
			psymoff(dot,ptype,":%16t"); dotinc=0; break;

		case 'p':
			psymoff(var[0],ptype,"%16t"); break;

		case 'u':
			printf("%-8u",w); break;

		case 'U':
			printf("%-16U",wx); break;

		case 'c': case 'C':
			if (modifier=='C')
				printesc(byte(wx));
			else
				printc(byte(wx));
			dotinc=1; break;

		case 'b': case 'B':
			printf("%-8o", byte(wx)); dotinc=1; break;

		case '1':
			printf("%-8R", byte(wx)); dotinc=1; break;

		case 'w': case '2':
			printf("%-8R", w); break;

		case 'W': case '4':
			printf("%-16R", wx); break;

		case 's': case 'S':
			savdot=dot; dotinc=1;
			while ((c=byte(get(dot,itype))) && errflg==0) {
				dot=inkdot(1);
				if (modifier == 'S')
					printesc(c);
				else
					printc(c);
				endline();
			}
			dotinc=dot-savdot+1; dot=savdot; break;

		case 'x':
			printf("%-8x",w); break;

		case 'X':
			printf("%-16X", wx); break;

		case 'z':
			printf("%-8z",w); break;

		case 'Z':
			printf("%-16Z", wx); break;

		case 'Y':
			printf("%-24Y", wx); break;

		case 'q':
			printf("%-8q", w); break;

		case 'Q':
			printf("%-16Q", wx); break;

		case 'o':
			printf("%-8o", w); break;

		case 'O':
			printf("%-16O", wx); break;

		case 'i': case 'I':
			printins(itype,wx); printc(EOR); break;

		case 'd':
			printf("%-8d", w); break;

		case 'D':
			printf("%-16D", wx); break;

		case 'f':
			fw.d = 0;
			fw.s[0] = w;
			fw.s[1] = wx&0xffff;
			printf("%-16.9f", fw.d);
			dotinc=4; break;

		case 'F':	/* may be done with one get call on TAHOE */
			fw.s[0] = w;
			fw.s[1] = wx&0xffff;
			fw.s[2]=shorten(get(inkdot(4),itype));
			fw.s[3]=shorten(get(inkdot(6),itype));
			if (errflg)
				return (fp);
			printf("%-32.18F", fw.d);
			dotinc=8; break;

		case 'n': case 'N':
			printc('\n'); dotinc=0; break;

		case '"':
			dotinc=0;
			while (*fp != '"' && *fp)
				printc(*fp++);
			if (*fp)
				fp++;
			break;

		case '^':
			dot=inkdot(-dotinc*fcount); return (fp);

		case '+':
			dot=inkdot(fcount); return (fp);

		case '-':
			dot=inkdot(-fcount); return (fp);

		default:
			error(BADMOD);
		}
		if (itype!=NSP)
			dot=inkdot(dotinc);
		fcount--; endline();
	}
	return (fp);
}

static
printesc(c)
	register c;
{

	c &= STRIP;
	if (c==0177 || c<SP)
		printf("^%c", c ^ 0100);
	else
		printc(c);
}

long
inkdot(incr)
{
	register long newdot;

	newdot=dot+incr;
	if ((dot ^ newdot) >> 24)
		error(ADWRAP);
	return (newdot);
}
