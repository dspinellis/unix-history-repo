#ifndef lint
static	char sccsid[] = "@(#)format.c	1.4 (Berkeley) 4/1/87";
#endif
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"

MSG		BADMOD;
MSG		NOFORK;
MSG		ADWRAP;

INT		mkfault;
CHAR		*lp;
ADDR		maxoff;
SIG		sigint;
SIG		sigqit;
STRING		errflg;
CHAR		lastc,peekc;
L_INT		dot;
INT		dotinc;
L_INT		expv;
L_INT		var[];

scanform(icount,ifp,itype,ptype)
L_INT		icount;
STRING		ifp;
{
	REG	STRING		fp;
	CHAR		modifier;
	REG		fcount, init=1;
	L_INT		savdot;
	BOOL exact;

	WHILE icount
	DO  fp=ifp;
	    savdot=dot; init=0;

	    IF init==0 ANDF (exact=(findsym(dot,ptype)==0)) ANDF maxoff
	    THEN printf("\n%s:%16t",cursym->n_un.n_name);
	    FI

	    /*now loop over format*/
	    WHILE *fp ANDF errflg==0
	    DO  IF isdigit(modifier = *fp)
		THEN fcount = 0;
			WHILE isdigit(modifier = *fp++)
			DO fcount *= 10;
			   fcount += modifier-'0';
			OD
			fp--;
		ELSE fcount = 1;
		FI

		IF *fp==0 THEN break; FI
		fp=exform(fcount,fp,itype,ptype);
	    OD
	    dotinc=dot-savdot;
	    dot=savdot;

	    IF errflg
	    THEN IF icount<0
		 THEN errflg=0; break;
		 ELSE error(errflg);
		 FI
	    FI
	    IF --icount
	    THEN dot=inkdot(dotinc);
	    FI
	    IF mkfault THEN error(0); FI
	OD
}

STRING
exform(fcount,ifp,itype,ptype)
INT		fcount;
STRING		ifp;
{
	/* execute single format item `fcount' times
	 * sets `dotinc' and moves `dot'
	 * returns address of next format item
	 */
	REG	POS	w;
	REG	L_INT	savdot, wx;
	REG	STRING		fp;
	CHAR		c, modifier, longpr;
	union{	/* compatible with both VAX and TAHOE */
		L_REAL	d;
		INT	s[4];
	}fw;

	WHILE fcount>0
	DO	fp = ifp; c = *fp;
		longpr=(c>='A')&&(c<='Z')||(c=='f')||(c=='4')||(c=='p');
		IF itype==NSP ORF *fp=='a'
		THEN wx=dot; w=dot;
		     IF c=='b' ORF c=='B' ORF c=='c' ORF c=='C' ORF c=='1'
		     THEN w=btol(wx); FI
		ELSE wx=get(dot,itype);
		     w=shorten(wx);
		FI
		IF errflg THEN return(fp); FI
		IF mkfault THEN error(0); FI
		var[0]=wx;
		modifier = *fp++;
		dotinc=(longpr?4:2);

		IF charpos()==0 ANDF modifier!='a' THEN printf("%16m"); FI

		switch(modifier) {

		    case SP: case TB:
			break;

		    case 't': case 'T':
			printf("%T",fcount); return(fp);

		    case 'r': case 'R':
			printf("%M",fcount); return(fp);

		    case 'a':
			psymoff(dot,ptype,":%16t"); dotinc=0; break;

		    case 'p':
			psymoff(var[0],ptype,"%16t"); break;

		    case 'u':
			printf("%-8u",w); break;

		    case 'U':
			printf("%-16U",wx); break;

		    case 'c': case 'C':
			IF modifier=='C'
			THEN printesc((w>>8)&0xff);
			ELSE printc((w>>8)&0xff);
			FI
			dotinc=1; break;

		    case 'b': case 'B':
			printf("%-8o", (w>>8)&0xff); dotinc=1; break;

			case '1':
			printf("%-8R", byte(wx)); dotinc=1; break;

			case '2':
		    case 'w':
			printf("%-8R", w); break;

			case '4':
		    case 'W':
			printf("%-16R", wx); break;

		    case 's': case 'S':
			savdot=dot; dotinc=1;
			WHILE (c=byte(get(dot,itype))) ANDF errflg==0
			DO dot=inkdot(1);
			   IF modifier == 'S'
			   THEN printesc(c);
			   ELSE printc(c);
			   FI
			   endline();
			OD
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

		    case 'i':
		    case 'I':
			printins(itype,wx); printc(EOR); break;

		    case 'd':
			printf("%-8d", w); break;

		    case 'D':
			printf("%-16D", wx); break;

		    case 'f':
			if ((w & ~0xFFFF00FF) == 0x8000)
				printf("(reserved oprnd)");
			else {
				fw.d = 0;
				fw.s[0] = w;
				fw.s[1] = wx&0xffff;
				printf("%-16.9f", fw.d);
			}
			dotinc = 4;
			break;

		    case 'F':	/* may be done with one get call on TAHOE */
			if ((w & ~0xFFFF00FF) == 0x8000)
				printf("(reserved oprnd)");
			else {
				fw.s[2] = shorten(get(inkdot(4),itype));
				fw.s[3] = shorten(get(inkdot(6),itype));
				if (errflg)
					return(fp);
				fw.s[0] = w;
				fw.s[1] = wx&0xffff;
				printf("%-32.18F", fw.d);
			}
			dotinc = 8;
			break;

		    case 'n': case 'N':
			printc('\n'); dotinc=0; break;

		    case '"':
			dotinc=0;
			WHILE *fp != '"' ANDF *fp
			DO printc(*fp++); OD
			IF *fp THEN fp++; FI
			break;

		    case '^':
			dot=inkdot(-dotinc*fcount); return(fp);

		    case '+':
			dot=inkdot(fcount); return(fp);

		    case '-':
			dot=inkdot(-fcount); return(fp);

		    default: error(BADMOD);
		}
		IF itype!=NSP
		THEN	dot=inkdot(dotinc);
		FI
		fcount--; endline();
	OD

	return(fp);
}

shell()
{
#ifndef EDDT
	REG	rc, unixpid;
	int	status;
	REG STRING	argp = lp;
	STRING	getenv(), shell = getenv("SHELL");
#ifdef VFORK
	char		oldstlp;
#endif

	if (shell == 0)
		shell = "/bin/sh";
	WHILE lastc!=EOR DO rdc(); OD
#ifndef VFORK
	IF (unixpid=fork())==0
#else
	oldstlp = *lp;
	IF (unixpid=vfork())==0
#endif
	THEN	signal(SIGINT,sigint); signal(SIGQUIT,sigqit);
		*lp=0; execl(shell, "sh", "-c", argp, 0);
		_exit(16);
#ifndef VFORK
	ELIF unixpid == -1
#else
	ELIF *lp = oldstlp, unixpid == -1
#endif
	THEN	error(NOFORK);
	ELSE	signal(SIGINT,1);
		WHILE (rc = wait(&status)) != unixpid ANDF rc != -1 DONE
		signal(SIGINT,sigint);
		printc('!'); lp--;
	FI
#endif
}


printesc(c)
REG c;
{
	c &= STRIP;
	IF c==0177 ORF c<SP
	THEN printf("^%c", c ^ 0100);
	ELSE printc(c);
	FI
}

L_INT	inkdot(incr)
{
	REG	L_INT	newdot;

	newdot=dot+incr;
	IF (dot ^ newdot) >> 24 THEN error(ADWRAP); FI
	return(newdot);
}
