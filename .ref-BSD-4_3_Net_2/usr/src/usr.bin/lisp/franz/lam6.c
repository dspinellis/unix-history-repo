#ifndef lint
static char *rcsid =
   "$Header: lam6.c,v 1.7 85/03/24 11:04:21 sklower Exp $";
#endif

/*					-[Sun Sep  4 08:56:19 1983 by jkf]-
 * 	lam6.c				$Locker:  $
 * lambda functions
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"
#include "frame.h"
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#include "chkrtab.h"
#include "chars.h"


lispval
Lreadli()
{
	register lispval work, handy;
	register FILE *p;
	register char *string; char *alloca();
	FILE *fstopen();
	lispval Lread();
	int count;
	pbuf pb;
	Savestack(4);
#ifdef SPISFP
	Keepxs();
#endif

	if(lbot->val==nil) {		/*effectively, return(matom(""));*/
		strbuf[0] = 0;
		return(getatom(FALSE));
	}
	chkarg(1,"readlist");
	count = 1;

	/* compute length of list */
	for(work = lbot->val; TYPE(work)==DTPR; work=work->d.cdr)
		count++;
	string = alloca(count);
	p = fstopen(string, count - 1, "r");
	for(work = lbot->val; TYPE(work)==DTPR; work=work->d.cdr) {
		handy = work->d.car;
		switch(TYPE(handy)) {
		case SDOT:
		case INT:
			*string++=handy->i;
			break;
		case ATOM:
			*string++ = *(handy->a.pname);
			break;
		case STRNG:
			*string++ = *(char *)handy;
			break;
		default:
		        fclose(p);
			error("Non atom or int to readlist",FALSE);
		}
	}
	*string = 0;
	errp = Pushframe(F_CATCH,Veruwpt,nil);	/* must unwind protect
						   so can deallocate p
						 */
	switch(retval) { lispval Lctcherr();
	case C_THROW:
			/* an error has occured and we are given a chance
			   to unwind before the control goes higher
			   lispretval contains the error descriptor in
			   it's cdr
			 */
		      fclose(p);	/* free port */
		      errp = Popframe();
		      Freexs();
		      lbot = np;
		      protect(lispretval->d.cdr); /* error descriptor */
		      return(Lctcherr());	/* do a I-do-throw */
		      
	case C_INITIAL: 
			lbot = np;
			protect(P(p));
			work = Lread();  /* error  could occur here */
			Freexs();
			fclose(p);	/* whew.. no errors */
			errp = Popframe();	/* remove unwind-protect */
			Restorestack();
			return(work);
	}
	/* NOTREACHED */
}

lispval
Lgetenv()
{
	char *getenv(), *strcpy();
	char *res;
	chkarg(1,"getenv");
	

	if((TYPE(lbot->val))!=ATOM)
		error("argument to getenv must be atom",FALSE);

	res = getenv(lbot->val->a.pname);
	if(res) strcpy(strbuf,res);
	else strbuf[0] = '\0';
	return(getatom(FALSE));
}

lispval
Lboundp()
{
	register lispval result, handy;

	chkarg(1,"boundp");

	if((TYPE(lbot->val))!=ATOM)
		error("argument to boundp must be symbol",FALSE);
	if( (handy = lbot->val)->a.clb==CNIL)
		result = nil;
	else
		(result = newdot())->d.cdr = handy->a.clb;
	return(result);
}


lispval
Lplist()
{	
	register lispval atm;
	/* get property list of an atom or disembodied property list */

	chkarg(1,"plist");
	atm = lbot->val;
	switch(TYPE(atm)) {
	case ATOM:
	case DTPR:
		break;
	default:
		error("Only Atoms and disembodied property lists allowed for plist",FALSE);
	}
	if(atm==nil) return(nilplist);
	return(atm->a.plist);
}


lispval
Lsetpli()
{	/* set the property list of the given atom to the given list */
	register lispval atm, vall;

	chkarg(2,"setplist");
	atm = lbot->val;
	if (TYPE(atm) != ATOM) 
	   error("setplist: First argument must be an symbol",FALSE);
	vall = (np-1)->val;
	if (TYPE(vall)!= DTPR && vall !=nil)
	    error("setplist: Second argument must be a list",FALSE);
	if (atm==nil)
		nilplist = vall;
	else
		atm->a.plist = vall;
	return(vall);
}

lispval
Lsignal()
{
	register lispval handy, old, routine;
	int i;
	int sginth();

	switch(np-lbot) {

	case 1: routine = nil;		/* second arg defaults to nil */
		break;

	case 2: routine = lbot[1].val;
		break;			/* both args given 		*/

	default: argerr("signal");
	}

	handy = lbot->val;
	if(TYPE(handy)!=INT)
		error("First arg to signal must be an int",FALSE);
	i = handy->i & 15;

	if(TYPE(routine)!=ATOM)
		error("Second arg to signal must be an atom",FALSE);
	old = sigacts[i];

	if(old==0) old = nil;

	if(routine==nil)
		sigacts[i]=((lispval) 0);
	else
		sigacts[i]=routine;
	if(routine == nil)
	    signal(i,SIG_IGN);	/* ignore this signals */
	else if (old == nil)
	    signal(i,sginth);	/* look for this signal */
	if(i == SIGINT) sigintcnt = 0; /* clear memory */
	return(old);
}

lispval
Lassq()
{
	register lispval work, handy;

	chkarg(2,"assq");

	for(work = lbot[1].val, handy = lbot[0].val; 
	    (work->d.car->d.car != handy) && (work != nil);
	    work = work->d.cdr);
	return(work->d.car);
}

lispval
Lkilcopy()
{
	if(fork()==0) {
		abort();
	}
}

lispval
Larg()
{
	register lispval handy; register offset, count;

	handy = lexpr_atom->a.clb;
	if(handy==CNIL || TYPE(handy)!=DTPR)
		error("Arg: not in context of Lexpr.",FALSE);
	count = ((long *)handy->d.cdr) -1 - (long *)handy->d.car;
	if(np==lbot || lbot->val==nil)
		return(inewint(count+1));
	if(TYPE(lbot->val)!=INT || (offset = lbot->val->i - 1) > count || offset < 0 )
		error("Out of bounds: arg to \"Arg\"",FALSE);
	return( ((struct argent *)handy->d.car)[offset].val);
}

lispval
Lsetarg()
{
	register lispval handy, work;
	register limit, index;

	chkarg(2,"setarg");
	handy = lexpr_atom->a.clb;
	if(handy==CNIL || TYPE(handy)!=DTPR)
		error("Arg: not in context of Lexpr.",FALSE);
	limit = ((long *)handy->d.cdr) - 1 -  (long *)(work = handy->d.car);
	handy = lbot->val;
	if(TYPE(handy)!=INT)
		error("setarg: first argument not integer",FALSE);
	if((index = handy->i - 1) < 0 || index > limit)
		error("setarg: index out of range",FALSE);
	return(((struct argent *) work)[index].val = lbot[1].val);
}

lispval
Lptime(){
	extern int gctime;
	int lgctime = gctime;
	struct tms current;
	register lispval result, handy;
	Savestack(2);

	times(&current);
	result = newdot();
	handy = result;
	protect(result);
	result->d.cdr = newdot();
	result->d.car = inewint(current.tms_utime);
	handy = result->d.cdr;
	handy->d.car = inewint(lgctime);
	handy->d.cdr = nil;
	if(gctime==0)
		gctime = 1;
	Restorestack();
	return(result);
}

/* (err-with-message message [value])
   'message' is the error message to print.
   'value' is the value to return from the errset (if present).
	it defaults to nil.
    The message may not be printed if there is an (errset ... nil)
    pending.
 */

lispval Lerr()
{
	lispval errorh();
	lispval valret = nil;
	char *mesg;
	

	switch(np-lbot) {
	 case 2: valret = lbot[1].val;	/* return non nil */
	 case 1: mesg = (char *)verify(lbot[0].val,
	  			  "err-with-message: non atom or string arg");
		 break;
	 default: argerr("err-with-message");
	}
	
	return(errorh(Vererr,mesg,valret,FALSE,1));
}

/*
 *  (tyi ['p_port ['g_eofval]])
 * normally -1 is return on eof, but g_eofval will be returned if given.
 */
lispval
Ltyi()
{
	register FILE *port;
	register lispval handy;
	lispval eofval;
	int val;	/* really char but getc returns int on eof */
	int eofvalgiven;

	handy = nil;   /* default port */
	eofvalgiven = FALSE;  /* assume no eof value given */
	switch(np-lbot)
	{
	    case 2:  eofval = lbot[1].val;
	    	     eofvalgiven = TRUE;
	    case 1:  handy = lbot[0].val;	/* port to read */
	    case 0: 
		     break;
	    default: argerr("tyi");
	}

	port = okport(handy,okport(Vpiport->a.clb,stdin));


	fflush(stdout);		/* flush any pending output characters */
	val = getc(port);
	if(val==EOF)
	{
		clearerr(port);
		if(sigintcnt > 0) sigcall(SIGINT);  /* eof might mean int */
		if(eofvalgiven) return(eofval);
		else return(inewint(-1));
	}
	return(inewint(val));
}

/* Untyi (added by DNC Feb. '80) - (untyi number port) puts the
   character with ascii code number in the front of the input buffer of
   port.  Note that this buffer is limited to 1 character.  That buffer is
   also written by tyipeek, so a peek followed by an untyi will result in
   the loss of the peeked char.
 */
   
lispval
Luntyi()
{

    lispval port,ch;

    port = nil;

    switch(np-lbot) {
	case 2: port = lbot[1].val;
	case 1: ch = lbot[0].val;
		break;
	default:
		argerr("untyi");
    }

    if(TYPE(ch) != INT) {
       errorh1(Vermisc, "untyi: expects fixnum character ",
       			nil,FALSE,0,ch);
    }	

    ungetc((int) ch->i,okport(port,okport(Vpiport->a.clb,stdin)));
    return(ch);
}

lispval
Ltyipeek()
{
	register FILE *port;
	register lispval handy;
	int val;

	switch(np-lbot)
	{
	    case 0:  handy = nil;	/* default port */
		     break;
	    case 1:  handy = lbot->val;
		     break;
	    default: argerr("tyipeek");
	}

	port = okport(handy,okport(Vpiport->a.clb,stdin));

	fflush(stdout);		/* flush any pending output characters */
	val = getc(port);
	if(val==EOF)
		clearerr(port);
	ungetc(val,port);
	return(inewint(val));
}

lispval
Ltyo()
{
	register FILE *port;
	register lispval handy, where;
	char val;

	switch(np-lbot)
	{
	    case 1:  where = nil;	/* default port */
		     break;
	    case 2:  where = lbot[1].val;
		     break;
	    default: argerr("tyo");
	}

	handy = lbot->val;
	if(TYPE(handy)!=INT)
		error("Tyo demands number for 1st arg",FALSE);
	val = handy->i;

	port = (FILE *) okport(where,okport(Vpoport->a.clb,stdout));
	putc(val,port);
	return(handy);
}

lispval
Imkrtab(current)
{
	extern struct rtab {
		unsigned char ctable[132];
	} initread;
	register lispval handy; extern lispval lastrtab;

	static int cycle = 0;
	static char *nextfree;
	Savestack(3);
	
	if((cycle++)%3==0) {
		nextfree = (char *) csegment(STRNG,1,FALSE);
		mrtabspace = (lispval) nextfree;
		/* need to protect partially allocated read tables
		   from garbage collection. */
	}
	handy = newarray();
	protect(handy);
	
	handy->ar.data = nextfree;
	if(current == 0)
		*(struct rtab *)nextfree = initread;
	else
	{
		register index = 0; register char *cp = nextfree;
		lispval c;

		*(struct rtab *)cp = *(struct rtab *)ctable;
		for(; index < 128; index++) {
		    switch(synclass(cp[index])) {
		    case CSPL: case CSSPL: case CMAC: case CSMAC:
		    case CINF: case CSINF:
			strbuf[0] = index;
			strbuf[1] = 0;
			c = (getatom(TRUE));
			Iputprop(c,Iget(c,lastrtab),handy);
		    }
		}
	}
	handy->ar.delta = inewint(4);
	handy->ar.length = inewint(sizeof(struct rtab)/sizeof(int));
	handy->ar.accfun = handy->ar.aux  = nil;
	nextfree += sizeof(struct rtab);
	Restorestack();
	return(handy);
}

/* makereadtable - arg : t or nil
	returns a readtable, t means return a copy of the initial readtable

			     nil means return a copy of the current readtable
*/
lispval
Lmakertbl()
{
	lispval handy = Vreadtable->a.clb;
	lispval value;
	chkrtab(handy);

	if(lbot==np) value = nil;
	else if(TYPE(value=(lbot->val)) != ATOM) 
		error("makereadtable: arg must be atom",FALSE);

	if(value == nil) return(Imkrtab(1));
	else return(Imkrtab(0));
}

lispval
Lcpy1()
{
	register lispval handy = lbot->val, result = handy;

top:
	switch(TYPE(handy))
	{
	case INT:
		result = inewint(handy->i);
		break;
	case VALUE:
		(result = newval())->l = handy->l;
		break;
	case DOUB:
		(result = newdoub())->r = handy->r;
		break;
	default:
		lbot->val =
		    errorh1(Vermisc,"Bad arg to cpy1",nil,TRUE,67,handy);
		goto top;
	}
	return(result);
}

/* copyint* . This returns a copy of its integer argument.  The copy will
 *	 be a fresh integer cell, and will not point into the read only
 *	 small integer table.
 */
lispval
Lcopyint()
{
	register lispval handy = lbot->val;
	register lispval ret;

  	while (TYPE(handy) != INT)
	{ handy=errorh1(Vermisc,"copyint* : non integer arg",nil,TRUE,0,handy);}
	(ret = newint())->i = handy->i;
	return(ret);
}


