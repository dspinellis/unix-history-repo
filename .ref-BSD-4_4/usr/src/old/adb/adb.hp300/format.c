#ifndef lint
static	char sccsid[] = "@(#)format.c	4.3 2/27/86";
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
L_INT		maxoff;
ADDR		sigint;
ADDR		sigqit;
STRING		errflg;
CHAR		lastc,peekc;
L_INT		dot;
INT		dotinc;
L_INT		expv;
L_INT		var[];


STRING		fphack;
rdfp()
{
	return(lastc= *fphack++);
}

scanform(icount,ifp,itype,ptype)
L_INT		icount;
STRING		ifp;
{
	STRING		fp;
	CHAR		modifier;
	INT		fcount, init=1;
	L_INT		savdot;
	BOOL exact;
	BOOL doit = 1;

	WHILE icount
	DO  fp=ifp;
	    savdot=dot; init=0;

	    IF init==0 ANDF ptype!=NSYM ANDF
	       (exact=(findsym(dot,ptype)==0)) ANDF maxoff
	    THEN printf("\n%s:%16t",cursym->n_un.n_name);
	    FI

	    /*now loop over format*/
	    WHILE *fp ANDF errflg==0
	    DO  IF digit(modifier = *fp)
		THEN fcount = 0;
			WHILE digit(modifier = *fp++)
			DO fcount *= 10;
			   fcount += modifier-'0';
			OD
			fp--;
			IF fcount==0 THEN fcount = 1; FI
		ELSE fcount = 1;
		FI

		IF *fp==0 THEN break; FI
#ifdef vax
		IF exact ANDF dot==savdot ANDF itype==ISP ANDF cursym->n_un.n_name[0]=='_' ANDF *fp=='i'
		THEN exform(1,"x",itype,ptype); fp++; printc(EOR); /* entry mask */
		ELSE fp=exform(fcount,fp,itype,ptype);
		FI
#else
		fp=exform(fcount,fp,itype,ptype);
#endif
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
	POS		w;
	L_INT		savdot, wx;
	STRING		fp;
	CHAR		c, modifier, longpr;
	union{
		REAL	f;
		L_REAL	d;
		L_INT	l[2];
	}fw;

	WHILE fcount>0
	DO	fp = ifp; c = *fp;
		longpr=((c>='A')&&(c<='Z'))||(c=='f')||(c=='4')||(c=='p');
		IF itype==NSP ORF *fp=='a'
		THEN wx=dot; w=dot;
		ELSE w=get(dot,itype);
		     IF longpr
		     THEN
#ifdef mc68000
		     wx=itol(w,get(inkdot(2),itype));
#else
		     wx=itol(get(inkdot(2),itype),w);
#endif
		     ELSE wx=w;
		     FI
		FI
		IF errflg THEN return(fp); FI
		IF mkfault THEN error(0); FI
		var[0]=wx;
		modifier = *fp++;
		dotinc=(longpr?4:2);;

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
			THEN printesc(lobyte(w));
			ELSE printc(lobyte(w));
			FI
			dotinc=1; break;

		    case 'b': case 'B':
			printf("%-8o", lobyte(w)); dotinc=1; break;

			case '1':
			printf("%-8r", lobyte(w)); dotinc=1; break;

			case '2':
		    case 'w':
			printf("%-8r", w); break;

			case '4':
		    case 'W':
			printf("%-16R", wx); break;

		    case 's': case 'S':
			savdot=dot; dotinc=1;
			WHILE (c=lobyte(get(dot,itype))) ANDF errflg==0
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
			printins(0,itype,w); printc(EOR); break;

		    case 'd':
			printf("%-8d", w); break;

		    case 'D':
			printf("%-16D", wx); break;

		    case 'f':
#if vax || pdp11
			IF (w & 0xff80) == 0x8000
			THEN	printf("(reserved oprnd)");
				break;
			FI
#endif
			fw.l[0] = wx;
			printf("%-16.9f", fw.f);
			dotinc = 4;
			break;

		    case 'F':
#if vax || pdp11
			IF (w & 0xff80) == 0x8000
			THEN	printf("(reserved oprnd)");
				break;
			FI
#endif
			fw.l[0] = wx;
			fw.l[1] = lget(inkdot(4),itype);
			printf("%-16.9f", fw.d);
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
	INT		rc, unixpid;
	STRING		argp = lp;
	STRING		getenv(), shell = getenv("SHELL");
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
		WHILE (rc = wait(0)) != unixpid ANDF rc != -1 DONE
		signal(SIGINT,sigint);
		prints("!"); lp--;
	FI
#endif
}


printesc(c)
{
	c &= STRIP;
	IF c==0177 THEN printf("^?");
	ELIF c<SP
	THEN printf("^%c", c + '@');
	ELSE printc(c);
	FI
}

L_INT	inkdot(incr)
{
	L_INT		newdot;

	newdot=dot+incr;
	IF (dot NEQ newdot) >> 24 THEN error(ADWRAP); FI
	return(newdot);
}

digit(c)
{
	return c >= '0' && c <= '9';
}
