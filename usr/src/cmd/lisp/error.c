
static char *sccsid = "@(#)error.c	34.3 11/7/80";

#include "global.h"
/* error ****************************************************************/
/* this routine is always called on a non-fatal error.  The first argu-	*/
/* ment is printed out.  The second a boolean flag indicating if the	*/
/* error routine is permitted to return a pointer to a lisp value if	*/
/* the "cont" command is executed.					*/

/* error from lisp C code, this temporarily replaces the old error
 * allowing us to interface with the new errset scheme with minimum
 * difficulty.  We assume that an error which comes to this routine
 * is of an "undefined error type" ER%misc .  Soon all calls to this
 * routine will be removed.
 *
 */

lispval
error(mesg,contvl)
char *mesg;
lispval contvl;
{
    lispval errorh();

    return(errorh(Vermisc,mesg,nil,contvl,0));
}


/* new error handler, works with errset 
 * 
 * call is errorh(type,message,valret,contuab) where
 * type is an atom which classifys the error, and whose clb, if not nil
 * is the name of a function to call to handle the error.
 * message is a character string to print to describe the error
 * valret is the value to return to an errset if one is found,
 * and contuab is non nil if this error is continuable.
 */
 
#include "catchfram.h"

lispval
errorh(type,message,valret,contuab,uniqid)
lispval type,valret;
int uniqid,contuab;
char *message;
{
 	register struct catchfr *curp; /* must be first register decl */
	register lispval handy;
	lispval *work = 1 + (lispval *) &uniqid; int limit = nargs() - 5;
	lispval Lread(), calhan();
	lispval contatm;
	lispval handy2;
	struct argent *savedlbot = lbot;
	struct nament * savedbnp = bnp;
	int curdep ;	/* error depth */
	typedef struct catchfr *cp;
	extern long errp;
	long myerrp = errp, what;
	int pass,founduw;
	int saveme[SAVSIZE];
	snpand(2);

	contatm = (contuab == TRUE ? tatom : nil);

	/* if there is a catch every error handler */
	if((handy = Verall->a.clb) != nil)	
	{
	    handy = Verall->a.clb;
	    Verall->a.clb = nil;		/* turn off before calling */
	    handy = calhan(limit,work,type,uniqid,contatm,message,handy);
	    if(contuab && (TYPE(handy) == DTPR))
		return(handy->d.car);
	}

	if((handy = type->a.clb) != nil)	/* if there is an error handler */
	{
	    handy = calhan(limit,work,type,uniqid,contatm,message,handy);
	    if(contuab && (TYPE(handy) == DTPR))
		return(handy->d.car);
	}

	pass = 1;
	/* search stack for error catcher */
  ps2:
	founduw = FALSE;

	for (curp = (cp) errp ; curp != (cp) nil ; curp = curp->link)
	{
	   if(curp->labl == Veruwpt) founduw = TRUE;
	   if(((pass == 2) && founduw)
	      || (curp->labl == type)  
	      || ( (TYPE(curp->labl) == DTPR) && (curp->labl->d.car == Verall)))
	   {
	      if((pass == 1) && founduw) 
	      {  pass = 2;
		 goto ps2;
	      }

	       if(founduw)
	       {   protect(handy2 = newdot());
		   handy2->d.car = Veruwpt;
		   handy = handy2->d.cdr = newdot();
		   handy->d.car = nil;		/* indicates error */
		   handy = handy->d.cdr = newdot();
		   handy->d.car = type;
		   handy = handy->d.cdr = newdot();
		   handy->d.car = matom(message);
		   handy = handy->d.cdr = newdot();
		   handy->d.car = valret;
		   handy = handy->d.cdr = newdot();
		   handy->d.car = inewint(uniqid);
		   handy = handy->d.cdr = newdot();
		   handy->d.car = inewint(contuab);
		   while (limit-- > 0)		/* put in optional args */
		   {  handy = handy->d.cdr = newdot();
		      handy->d.car = *work++;
		   }
		   valret = handy2;		/* return this as value */
		}
		else if(  (curp->flag != nil)
			    && (type != Vererr)) {
			/* print the full error message */
			printf("%s  ",message);
			while(limit-->0) {
				printr(*work++,stdout);
				fflush(stdout);
			}
			fputc('\n',stdout);
			fflush(stdout);
	       }
	       if(!founduw && ((handy=Verrset->a.clb) != nil))
	       {
	           calhan(limit,work,type,uniqid,contatm,message,handy);
	       }
	       popnames(curp->svbnp);	/* un shallow bind */
	       errp = (int) curp->link;	/* set error to next frame */
	       /*
		* return value goes into r7 until after movc3 instruction
		* which clobbers r0
		*/
	       asm("	movl	12(ap),r7");	/* set return value (valret)*/
	       asm("	addl3	$16,r11,sp");	/* skip link,flag,labl,svbnp */
	       asm("	movc3	$44,(sp),_setsav");/*restore (return) context*/
	       asm("	movab	44(sp),sp");	   /* skip past ""     "" */
	       asm("	popr	$0x2540");	/* restore registers */
	       asm("	movl	r7,r0");
	       asm("	rsb");		/* return to errset */
	       /* NOT REACHED */
	   }
	}
	    
	/* no one will catch this error, we must see if there is an
	   error-goes-to-top-level catcher */
	
	if (Vertpl->a.clb != nil)
	{
	    
	    handy = calhan(limit,work,type,uniqid,contatm,message,Vertpl->a.clb);
	    if( contuab  && (TYPE(handy) == DTPR))
		   return(handy->d.car);
	}

	/* at this point, print error mssage and break, just like
	   the current error scheme */
	printf("%s ",message);
	while(limit-->0) {
		printr(*work++,stdout);
		fflush(stdout);
	}


	/* If automatic-reset is set
	   we will now jump to top level, calling the reset function
	   if it exists, or using the c rest function if it does not 
	 */

	if(Sautor)
	{
		if ((handy = reseta->a.fnbnd) != nil)
		{ 	lbot = np;
			protect(reseta);
			protect(nil);
			Lapply();
		}
	 	contval = 0;
	  	reset(BRRETB);
	}
	
  	curdep = ++depth;
	getexit(saveme);
	while(what = setexit()) {
		errp = myerrp;
		depth = curdep;
		switch(what) {
		case BRRETB:
			if (curdep == (int) contval) {
				popnames(savedbnp);
				lbot = savedlbot;
				continue;
			}
		default:
			resexit(saveme);
			reset(what);

		case	BRRETN:
			if (contuab)
			{
				popnames(savedbnp);
				lbot = savedlbot;
				depth = curdep -1;
				resexit(saveme);
				return(contval);
			}
			printf("CAN'T CONTINUE\n");
			
		}
	}
	lbot = np;
	np++->val = P(stdin);
	np++->val = eofa;
	while(TRUE) {
		
		depth = curdep; /* In case of freturn, reset this global */
		fprintf(stdout,"\n%d:>",curdep);
		dmpport(stdout);
		vtemp = Lread();
		if(vtemp == eofa) exit(0);
		printr(eval(vtemp),stdout);
	}
}
lispval
calhan(limit,work,type,uniqid,contuab,message,handler)
register lispval *work;
lispval handler,type,contuab;
register limit;
register char *message;
int uniqid;
{
	    register lispval handy;
	    register struct argent *lbot, *np;
	    lbot = np;
	    protect(handler);		/* funcall the handler */
	    protect(handy = newdot());		/* with a list consisting of */
	    handy->d.car = type;			/* type, */
	    handy = (handy->d.cdr = newdot());
	    handy->d.car = inewint(uniqid);	/* identifying number, */
	    handy = (handy->d.cdr = newdot());
	    handy->d.car = contuab;
	    handy = (handy->d.cdr = newdot());
	    handy->d.car = matom(message);	/* message to be typed out, */
	    while(limit-- > 0)
	    {					/* any other args. */
		    handy = handy->d.cdr = newdot();
		    handy->d.car = *work++;
	    }
	    handy->d.cdr = nil;

	    handy = Lfuncal();
	    np=lbot;
	    return(handy);
}

/* lispend **************************************************************/
/* Fatal errors come here, with their epitaph.				*/
lispend(mesg)
	char	mesg[];
	{
	dmpport(poport);
	fprintf(errport,"%s\n",mesg);
	dmpport(errport);
	exit(0);
	}

/* namerr ***************************************************************/
/* handles namestack overflow, at present by simply giving a message	*/

namerr()
{
	if((nplim = np + NAMINC) > orgnp + NAMESIZE) 
	{  
	  printf("Unrecoverable Namestack Overflow, (reset) is forced\n");
	  fflush(stdout);
	  nplim = orgnp + NAMESIZE - 4*NAMINC;
	  lbot = np = nplim - NAMINC;
	  protect(matom("reset"));
	  Lfuncal();
	}
	error("NAMESTACK OVERFLOW",FALSE);
	/* NOT REACHED */
}
binderr()
{
	bnp -= 10;
	error("Bindstack overflow.",FALSE);
}
rtaberr()
{
	bindfix(Vreadtable,strtab,nil);
	error("Illegal read table.",FALSE);
}
badmem()
{
	error("Attempt to allocate beyond static structures.",FALSE);
}
argerr(msg)
char *msg;
{
	Lshostk();
	errorh(Vermisc,"incorrect number of args to",
				  nil,FALSE,0,matom(msg));
}
