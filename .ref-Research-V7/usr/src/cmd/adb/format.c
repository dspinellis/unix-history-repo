#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"


MSG		BADMOD;
MSG		NOFORK;
MSG		ADWRAP;

SYMTAB		symbol;

INT		mkfault;
CHAR		*lp;
INT		maxoff;
INT		sigint;
INT		sigqit;
STRING		errflg;
CHAR		lastc;
L_INT		dot;
INT		dotinc;
L_INT		var[];


scanform(icount,ifp,itype,ptype)
L_INT		icount;
STRING		ifp;
{
	STRING		fp;
	CHAR		modifier;
	INT		fcount, init=1;
	L_INT		savdot;

	WHILE icount
	DO  fp=ifp;
	    IF init==0 ANDF findsym(shorten(dot),ptype)==0 ANDF maxoff
	    THEN printf("\n%.8s:%16t",symbol.symc);
	    FI
	    savdot=dot; init=0;

	    /*now loop over format*/
	    WHILE *fp ANDF errflg==0
	    DO  IF digit(modifier = *fp)
		THEN fcount=0;
		     WHILE digit(modifier = *fp++)
		     DO fcount *= 10;
			fcount += modifier-'0';
		     OD
		     fp--;
		ELSE fcount=1;
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

STRING	exform(fcount,ifp,itype,ptype)
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
	L_REAL		fw;
	struct{
		L_INT	sa;
		INT	sb,sc;
	};

	WHILE fcount>0
	DO	fp = ifp; c = *fp;
		longpr=(c>='A')&(c<='Z')|(c=='f');
		IF itype==NSP ORF *fp=='a'
		THEN wx=dot; w=dot;
		ELSE w=get(dot,itype);
		     IF longpr
		     THEN wx=itol(w,get(inkdot(2),itype));
		     ELSE wx=w;
		     FI
		FI
		IF c=='F'
		THEN fw.sb=get(inkdot(4),itype);
		     fw.sc=get(inkdot(6),itype);
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
			THEN printesc(w&LOBYTE);
			ELSE printc(w&LOBYTE);
			FI
			dotinc=1; break;

		    case 'b': case 'B':
			printf("%-8o", w&LOBYTE); dotinc=1; break;

		    case 's': case 'S':
			savdot=dot; dotinc=1;
			WHILE (c=get(dot,itype)&LOBYTE) ANDF errflg==0
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
		    case 'w':
			printf("%-8o", w); break;

		    case 'O':
		    case 'W':
			printf("%-16O", wx); break;

		    case 'i':
			printins(0,itype,w); printc(EOR); break;

		    case 'd':
			printf("%-8d", w); break;

		    case 'D':
			printf("%-16D", wx); break;

		    case 'f':
			fw = 0;
			fw.sa = wx;
			printf("%-16.9f", fw);
			dotinc=4; break;

		    case 'F':
			fw.sa = wx;
			printf("%-32.18F", fw);
			dotinc=8; break;

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

unox()
{
	INT		rc, status, unixpid;
	STRING		argp lp;

	WHILE lastc!=EOR DO rdc(); OD
	IF (unixpid=fork())==0
	THEN	signal(SIGINT,sigint); signal(SIGQUIT,sigqit);
		*lp=0; execl("/bin/sh", "sh", "-c", argp, 0);
		exit(16);
	ELIF unixpid == -1
	THEN	error(NOFORK);
	ELSE	signal(SIGINT,1);
		WHILE (rc = wait(&status)) != unixpid ANDF rc != -1 DONE
		signal(SIGINT,sigint);
		prints("!"); lp--;
	FI
}


printesc(c)
{
	c &= STRIP;
	IF c<SP ORF c>'~' ORF c=='@'
	THEN printf("@%c",(c=='@' ? '@' : c^0140));
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
