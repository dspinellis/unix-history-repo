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
 
#include "catchframe.h"

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
	struct argent *savedlbot = lbot;
	struct nament * savedbnp = bnp;
	int curdep ;	/* error depth */
	typedef struct catchfr *cp;
	extern int errp;
	int myerrp = errp, what;
	int saveme[SAVSIZE];
	snpand(2);

	if(type->clb != nil)	/* if there is an error handler */
	{
	    handy = calhan(limit,work,type->clb,uniqid,message);
	    if(contuab && (TYPE(handy) == DTPR))
		return(handy->car);
	}

	/* search stack for error catcher */

	for (curp = (cp) errp ; curp != (cp) nil ; curp = curp->link)
	{
	   if((curp->labl == type)  
	      || ( (TYPE(curp->labl) == DTPR) && (curp->labl->car == Verall)))
	   {
	       if((curp->flag != nil)
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
	       popnames(curp->svbnp);	/* un shallow bind */
	       errp = (int) curp->link;	/* set error to next frame */
	       asm("	addl3	$16,r11,sp");	/* skip link,flag,labl,svbnp */
	       asm("	movc3	$40,(sp),_setsav");/*restore (return) context*/
	       asm("	movab	40(sp),sp");	   /* skip past ""     "" */
	       asm("	popr	$0x2540");	/* restore registers */
	       asm("	movl	12(ap),r0");	/* set return value */
	       asm("	rsb");		/* return to errset */
	       /* NOT REACHED */
	   }
	}
	    
	/* no one will catch this error, we must see if there is an
	   error-goes-to-top-level catcher */
	
	if (Vertpl->clb != nil)
	{
	    
	    handy = calhan(limit,work,Vertpl,uniqid,message);
	    if( contuab  && (TYPE(handy) == DTPR))
		   return(handy->car);
	}

	/* at this point, print error mssage and break, just like
	   the current error scheme */
	printf("%s: ",message);
	while(limit-->0) {
		printr(*work++,stdout);
		fflush(stdout);
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
		
		fprintf(stdout,"\n%d:>",curdep);
		dmpport(stdout);
		vtemp = Lread();
		if(vtemp == eofa) exit(0);
		printr(eval(vtemp),stdout);
	}
}
static lispval
calhan(limit,work,handler,uniqid,message)
register lispval *work;
lispval handler;
register limit;
register char *message;
int uniqid;
{
	    register lispval handy;
	    register struct argent *lbot, *np;
	    lbot = np;
	    protect(handler->clb);		/* funcall the handler */
	    protect(handy = newdot());		/* with a list consisting of */
	    handy->car = inewint(uniqid);	/* identifying number, */
	    handy = handy->cdr = newdot();
	    handy->car = matom(message);	/* message to be typed out, */
	    while(limit-- > 0)
	    {					/* any other args. */
		    handy = handy->cdr = newdot();
		    handy->car = *work++;
	    }
	    handy->cdr = nil;

	    handy = Lfuncal();
	    np=lbot;
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
	np -= 10;
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
