static char *sccsid = "@(#)fex1.c	34.2 11/7/80";

#include "global.h"
/* Nprog ****************************************************************/
/* This first sets the local variables to nil while saving their old	*/
/* values on the name stack.  Then, pointers to various things are	*/
/* saved as this function may be returned to by an "Ngo" or by a	*/
/* "Lreturn".  At the end is the loop that cycles through the contents	*/
/* of the prog.								*/

lispval
Nprog() {
	int	saveme[SAVSIZE];
	register struct nament *mybnp = bnp;
	register struct argent *savednp;
	register lispval where, temp;
	register struct argent *lbot, *np;
	struct argent *savedlbot;
	struct nament *savedbnp;
	struct nament *topbind;
	long myerrp; extern long errp;

	savednp = np;
	savedlbot = lbot;
	savedbnp = bnp;
	temp = where = (lbot->val)->d.car;
	while (TYPE(temp) == DTPR)
	{
		temp = where->d.car;
		if (TYPE(temp) == ATOM)
			{
			bnp->atm = temp;
			bnp->val = (temp)->a.clb;
			(temp)->a.clb = nil;
			temp = where = where->d.cdr;
			if(bnp++ > bnplim)
				binderr();
			}
		else return(CNIL);
	}
	topbind = bnp;
	myerrp = errp;
	if (where != nil) return(CNIL);
	temp = where = savedlbot->val->d.cdr;
	getexit(saveme);
	while (retval = setexit()) {
		errp = myerrp;
		switch (retval)	{

		case BRRETN:	resexit(saveme);
				popnames(savedbnp);
				lbot = savedlbot;
				return(contval);

		case BRGOTO:	where = (savedlbot->val)->d.cdr;
				while ((TYPE(where) == DTPR) && (where->d.car != contval))
					where = where->d.cdr;
				if (where->d.car == contval) {
					/* This seems wrong - M Marcus
					resexit(saveme);	*/
					popnames(topbind);
					lbot = savedlbot;
					break;
				}

		default:
			resexit(saveme);
			reset(retval);

		}
	}
	while (TYPE(where) == DTPR)
		{
		temp = where->d.car;
		if((TYPE(temp))!=ATOM) eval(temp);
		where = where->d.cdr;
		}
	resexit(saveme);
	return((where == nil) ? nil : CNIL);
	}

lispval globtag;
/*
   Ncatch is now actually *catch , which has the form
     (*catch tag form)
    tag is evaluated and then the catch entry is set up.
      then form is evaluated
    finally the catch entry is removed.

    (catch form [tag]) is translated to (*catch 'tag form)
     by a macro.
 */
lispval
Ncatch()
{
	struct	argent	*savednp,*savedlbot;
	register lispval where, tag, todo;
	register temp;
	register struct argent *lbot, *np;
	int type;


	where = lbot->val;
	if((TYPE(where))!=DTPR) return(nil);
	todo = where->d.cdr->d.car;
	tag = eval(where->d.car);
	while((TYPE(tag)!=ATOM) && (TYPE(tag) != DTPR))
		tag = error("Bad type of tag in *catch.",TRUE);
	asm("	pushab	On1");
	asm("	pushr	$0x2540");
	asm("	subl2	$44,sp");	/* THIS IS A CROCK ....
					   saves current environment
					   for (return) z.B. */
	asm("	movc3	$44,_setsav,(sp)");
	asm("	pushl	_bnp");
	asm("	pushl	r10");
	asm("	pushl	$1");
	asm("	pushl	_errp");
	asm("	movl	sp,_errp");
	where = (eval(todo));
	asm("	movl	(sp),_errp");
	return(where);
	asm("On1:ret");
}

/* (errset form [flag])  
   if present, flag determines if the error message will be printed
   if an error reaches the errset.
   if no error occurs, errset returns a list of one element, the 
    value returned from form.
   if an error occurs, nil is usually returned although it could
    be non nil if err threw a non nil value 
 */

lispval Nerrset()
{
	register lispval flag,where,todo; /* order important */
	register lispval handy = Vlerall;	  /* to access this easily */
	register struct argent *lbot, *np;
	where = lbot->val;

	if(TYPE(where) != DTPR) return(nil);	/* no form */

	todo = where->d.car;		/* form to eval */
	flag = where->d.cdr;
	if(flag != nil) flag = eval(flag->d.car);	/* tag to tell if er messg */
	else flag = tatom; 	/* if not present , assume t */

	/* push on a catch frame */

	asm("	pushab	On2");		/* where to jump if error */
	asm("	pushr	$0x2540");
	asm("	subl2	$44,sp");	/* THIS IS A CROCK ....
					   saves current environment
					   for (return) z.B. */
	asm("	movc3	$44,_setsav,(sp)");
	asm("	pushl	_bnp");
	asm("	pushl	r8");	/* tag , (ER%all) 	*/
	asm("	pushl	r11");		/* flag    		*/
	asm("	pushl	_errp");	/* link in 		*/
	asm("	movl	sp,_errp");	/*  "	   		*/

	/* evaluate form, and if ok, listify */

	handy = eval(todo);
	asm("	movl	(sp),_errp");	/* unlink this frame 	*/
	protect(handy);			/* may gc on nxt call  	*/
	(flag = newdot()) ->d.car = handy; /* listify arg */

	return(flag);

	asm("On2: ret");		/* if error occured */
	
}
	
/* this was changed from throw to *throw 21nov79
   it really should be called Lthrow
*/
Nthrow()
{
	register lispval todo, where;
	lispval globtag,contval;
	snpand(2);  /* save register mask */
	switch(np-lbot) {
	case 0:
		protect(nil);
	case 1:
		protect(nil);
	case 2: break;
	default:
		argerr("throw");
	}
	globtag = lbot->val;
	contval = (lbot+1)->val;
	Idothrow(globtag,contval);
	error("Uncaught throw",FALSE);
}
#include "catchfram.h"

Idothrow(tag,value)
lispval tag,value;
{
	typedef struct catchfr *cp;
	register cp curp;	/* must be first register */
	extern long errp;
	extern lispval globtag;
	int pass1,founduw;
	lispval handy,handy2;
	snpand(1);

	globtag = tag;
	/*
	printf("throw,value ");printr(tag,stdout); printf(" ");
	printr(value,stdout); fflush(stdout);
	*/
	pass1 = TRUE;
  ps2:
	founduw = FALSE;

	for (curp=(cp)errp ; curp != (cp) nil ; curp =curp->link)
	{
	  /*  printf(" lbl: ");printr(curp->labl,stdout);fflush(stdout); */
	    if(curp->labl == Veruwpt) 
	    {  founduw = TRUE;
	       if(!pass1) goto foundit;
	    }
	    if(curp->labl == nil || curp->labl == tag) goto foundit;
	    if(TYPE(curp->labl) == DTPR)
	    {
	       for( handy = curp->labl ; handy != nil ; handy = handy->d.cdr)
	       {
		   if(handy->d.car == tag) goto foundit;
	       }
	    }
	}
	return;

	   foundit: 			/* restore context at catch */
	       if(pass1 && founduw)
	       {   pass1 = FALSE;
		   goto ps2;
		}
		if(founduw)		/* remember the state */
		{   protect(handy2 = newdot());
		    handy2->d.car = Veruwpt;
		    handy = handy2->d.cdr = newdot();
		    handy->d.car = tatom;	/* t for throw */
		    handy = handy->d.cdr = newdot();
		    handy->d.car = tag;
		    handy = handy->d.cdr = newdot();
		    handy->d.car = value;
		    value = handy2;
		 /*   printf("Ret uwp: ");printr(value,stdout);fflush(stdout);*/
		}

		popnames(curp->svbnp);
		errp = (int) curp->link;
		/* 
		 * return value must go into r7 until after movc3 since
		 * a movc3 clobbers r0
		 */
		asm("	movl	8(ap),r7");   /* return value */
		asm("	addl3	$16,r11,sp");
				/* account for current (return) */
		asm("	movc3	$44,(sp),_setsav");
		asm("	addl2	$44,sp");
		asm("	popr	$0x2540");
		asm("	movl	r7,r0");
		asm("	rsb");
		
}



/* Ngo ******************************************************************/
/* First argument only is checked - and must be an atom or evaluate	*/
/* to one.								*/
Ngo()
	{
	contval = (lbot->val)->d.car;
	while (TYPE(contval) != ATOM)
		{
		contval = eval(contval);
		while (TYPE(contval) != ATOM) contval = error("GO ARG NOT ATOM",TRUE);
		}
	reset(BRGOTO);
	}


/* Nreset ***************************************************************/
/* All arguments are ignored.  This just returns-from-break to depth 0.	*/
Nreset()
	{
	contval = 0;
	reset(BRRETB);
	}

/* Nresetio *************************************************************/

lispval
Nresetio() {
	register FILE *p;

	for(p = &_iob[3]; p < _iob + _NFILE; p++) {
		if(p->_flag & (_IOWRT | _IOREAD)) fclose(p);
		}
	return(nil);

}


/* Nbreak ***************************************************************/
/* If first argument is not nil, this is evaluated and printed.  Then	*/
/* error is called with the "breaking" message.				*/

lispval
Nbreak()
{
	register lispval hold; register FILE *port;
	port = okport(Vpoport->a.clb,stdout);
	fprintf(port,"Breaking:");

	if ((hold = lbot->val) != nil && ((hold = hold->d.car) != nil))
	{
		printr(hold,port);
	}
	putc('\n',port);
	dmpport(port);
	return(errorh(Verbrk,"",nil,TRUE,0));
}


/* Nexit ****************************************************************/
/* Just calls lispend with no message.					*/
Nexit()
	{
	lispend("");
	}


/* Nsys *****************************************************************/
/* Just calls lispend with no message.					*/

lispval
Nsys()
	{
	lispend("");
	}




lispval
Ndef() {
	register lispval arglist, body, name, form;
	snpand(4);
	
	form = lbot->val;
	name = form->d.car;
	body = form->d.cdr->d.car;
	arglist = body->d.cdr->d.car;
	if((TYPE(arglist))!=DTPR && arglist != nil)
		error("Warning: defining function with nonlist of args",
			TRUE);
	name->a.fnbnd = body;
	return(name);
}


lispval
Nquote()
{
	snpand(0);
	return((lbot->val)->d.car);
}


lispval
Nsetq()
{	register lispval handy, where, value;
	register int lefttype;
	register struct argent *lbot, *np;


	for(where = lbot->val; where != nil; where = handy->d.cdr) {
		handy = where->d.cdr;
		if((TYPE(handy))!=DTPR)
			error("odd number of args to setq",FALSE);
		if((lefttype=TYPE(where->d.car))==ATOM) {
			if(where->d.car==nil)
				error("Attempt to set nil",FALSE);
			where->d.car->a.clb = value = eval(handy->d.car);
		 }else if(lefttype==VALUE)
			where->d.car->l = value = eval(handy->d.car);
		else error("CAN ONLY SETQ ATOMS OR VALUES",FALSE);
	}
	return(value);
}


lispval
Ncond()
{
	register lispval  where, last;
	snpand(2);

	where = lbot->val;
	last = nil;
	for(;;) {
		if ((TYPE(where))!=DTPR)
			break;
		if ((TYPE(where->d.car))!=DTPR)
			break;
		if ((last=eval((where->d.car)->d.car)) != nil)
			break;
		where = where->d.cdr;
	}

	if ((TYPE(where)) != DTPR)
			return(nil);
	where = (where->d.car)->d.cdr;
	while ((TYPE(where))==DTPR) {
			last = eval(where->d.car);
			where = where->d.cdr;
	}
	return(last);
}

lispval
Nand()
{
	register lispval current, temp;
	snpand(2);

	current = lbot->val;
	temp = tatom;
	while (current != nil)
		if ( (temp = current->d.car)!=nil && (temp = eval(temp))!=nil) 
			current = current->d.cdr;
		else {
			current = nil;
			temp = nil;
		}
	return(temp);
}


lispval
Nor()
{
	register lispval current, temp;
	snpand(2);

	current = lbot->val;
	temp = nil;
	while (current != nil)
		if ( (temp = eval(current->d.car)) == nil)
			current = current->d.cdr;
		else
			break;
	return(temp);
}


lispval
Nprocess() {
	int wflag , childsi , childso , childnum, child;
	register lispval current, temp;
	char * sharg;
	int handler;
	int itemp;
	FILE *bufs[2],*obufs[2];

	wflag = 1;
	childsi = 0;
	childso = 1;
	current = lbot->val;
	if( (TYPE(current))!=DTPR )
		return(nil);
	temp = current->d.car;
	if( (TYPE(temp))!=ATOM )
		return(nil);

	sharg = temp->a.pname;

	if( (current = current->d.cdr)!=nil && (TYPE((temp = current->d.car)))==ATOM ) {
	
		if (temp == tatom) {
			wflag = 0;
			childsi = 0;
		} else if (temp != nil) {
			fpipe(bufs);
			wflag = 0;
			temp->a.clb = P(bufs[1]);
			childsi = fileno(bufs[0]);
		}
	
		if( (current = current->d.cdr)!=nil && (TYPE((temp = current->d.car)))==ATOM ) {
	
			if (temp != nil) {
				fpipe(obufs);
				temp->a.clb = P(obufs[0]);
				childso = fileno(obufs[1]);
			}
		}
	}
	handler = signal(2,1);
	if((child = fork()) == 0 ) {
		if(wflag!=0 && handler !=1)
			signal(2,0);
		else
			signal(2,1);
		if(childsi != 0) {
			close(0);
			dup(childsi);
		}
		if (childso !=1) {
			close(1);
			dup(childso);
		}
		execlp("csh", "csh", "-c",sharg,0);
		execlp("sh", "sh", "-c",sharg,0);
		exit(-1); /* if exec fails, signal problems*/
	}

	if(childsi != 0) fclose(bufs[0]);
	if(childso != 1) fclose(obufs[1]);

	if(wflag && child!= -1) {
		int status=0;
		wait(&status);
		itemp = status >> 8;
	} else
		itemp = child;
	signal(2,handler);
	return(inewint(itemp));
}
