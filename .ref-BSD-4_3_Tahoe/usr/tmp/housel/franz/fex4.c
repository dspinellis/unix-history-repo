#ifndef lint
static char *rcsid =
   "$Header: fex4.c,v 1.5 85/03/13 17:19:04 sklower Exp $";
#endif

/*					-[Sat Jan 29 12:40:56 1983 by jkf]-
 * 	fex4.c				$Locker:  $
 * nlambda functions
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include "global.h"
#include "lfuncs.h"
#include "chkrtab.h"
#include <signal.h>
#include <sys/types.h>

#if (os_4_2 || os_4_3)
#include <sys/time.h>
#else
#include <time.h>
#endif

/* this is now a lambda function instead of a nlambda.
   the only reason that it wasn't a lambda to begin with is that 
   the person who wrote it didn't know how to write a lexpr
						- jkf
*/
lispval
Lsyscall() {
	register lispval temp;
	register struct argent *aptr;
	register int acount = 1;
	extern syscall();
	int args[50];
	Savestack(3);

	/* there must be at least one argument */

	if (np==lbot) { chkarg(1,"syscall"); }

	aptr = lbot;
	temp = lbot->val;
	if (TYPE(temp) != INT) {
		Restorestack();
		return(error("syscall: bad first argument ", FALSE));
	}
	args[acount++] = temp->i;
	while( ++aptr < np && acount < 48) {
		temp = aptr->val;
		switch(TYPE(temp)) {

			case ATOM:	
				args[acount++] = (int)temp->a.pname;
				break;

			case STRNG:
				args[acount++] = (int) temp;
				break;

			case INT:
				args[acount++] = (int)temp->i;
				break;

			default:
				Restorestack();
				return(error("syscall: arg not symbol, string or fixnum", FALSE));
		}
	}

	Restorestack();
	args[0] = acount - 1;
	return(inewint(callg_(syscall,args)));
}

/* eval-when: this has the form (eval-when <list> <form1> <form2> ...)
   where the list may contain any combination of `eval', `load', `compile'.
   The interpreter (us) looks for the atom `eval', if it is present
   we treat the rest of the forms as a progn.
*/

lispval
Nevwhen()
{
	register lispval handy;
	register lispval handy2;
	Savestack(2);

	for (handy=(lbot->val)->d.car ; handy != nil ; handy = handy->d.cdr) {
	   if (handy->d.car == (lispval) Veval) {
		lbot=np;
		protect(((lbot-1)->val)->d.cdr);
		handy2 = Nprogn();
		Restorestack();
		return(handy2);
	    }
	}


	Restorestack();
	return(nil);	/* eval not seen */
}


/*	Status functions. 
 *  These operate on the statuslist stlist which has the form:
 *	( status_elem_1 status_elem_2 status_elem_3 ...)
 *  where each status element has the form:
 *	( name readcode setcode .  readvalue)
 *  where
 *	name - name of the status feature (the first arg to the status
 *		function).
 *	readcode - fixnum which tells status how to read the value of
 *		this status name.  The codes are #defined.
 *	setcode - fixnum which tells sstatus how to set the value of
 *		this status name
 *	readvalue - the value of the status feature is usually stored
 *		here.
 *	
 * Readcodes:
 *
 *	ST_READ - if no second arg, return readvalue.
 *		  if the second arg is given, we return t if it is eq to
 *		  the readvalue.
 *	ST_FEATR - used in (status feature xxx) where we test for xxx being
 *		  in the status features list
 *	ST_SYNT - used in (status syntax c) where we return c's syntax code
 *	ST_INTB - read stattab entry
 *	ST_NFETR - used in (status nofeature xxx) where we test for xxx not
 *		  being in the status features list
 *	ST_DMPR - read the dumpmode 
 *	ST_UNDEF - return the undefined functions in the transfer table
 * 
 * Setcodes:
 *	ST_NO -  if not allowed to set this status through sstatus.
 *	ST_SET - if the second arg is made the readvalue.
 *	ST_FEATW - for (sstatus feature xxx), we add xxx to the 
 *		  (status features) list.
 *	ST_TOLC - if non nil, map upper case chars in atoms to lc.
 *	ST_CORE - if non nil, have bus errors and segmentation violations
 *		  dump core, if nil have them produce a bad-mem err msg
 *	ST_INTB - set stattab table entry
 *	ST_NFETW - use in (sstatus nofeature xxx) where we wish to remove xxx
 *		   from the status feature list.
 *	ST_DMPW - set the dumpmode
 *	ST_BCDTR - (ifdef RSET) if non nil, creat trace stack entries for
 *		   calls from BCD functions to BCD functions
 *	ST_GCSTR - (ifdef GCSTRINGS) garbage collect strings
 */

lispval
Nstatus()
{
	register lispval handy,curitm,valarg;
	int indx,ctim;
	int typ;
	char *cp;
	char *ctime();
	struct tm *lctime,*localtime();
	extern unsigned char *ctable;
	extern int dmpmode;
	extern lispval chktt();
	lispval Istsrch();
	Savestack(3);

	if(lbot->val == nil) return(nil);
	handy = lbot->val;		/* arg list */

	while(TYPE(handy) != DTPR) handy = error("status: bad arg list",TRUE); 
	
	curitm = Istsrch(handy->d.car);	/* look for feature */

	if( curitm == nil ) return(nil);	/* non existant */

	if( handy->d.cdr == nil ) valarg = (lispval) CNIL;
	else valarg = handy->d.cdr->d.car;

	/* now do the processing with curitm pointing to the requested
	   item in the status list 
	 */
	
	switch( typ = curitm->d.cdr->d.car->i ) {	/* look at readcode */


	case ST_READ:
		curitm = Istsrch(handy->d.car);	/* look for name */
		if(curitm == nil) return(nil);
		if( valarg != (lispval) CNIL) 
		    error("status: Second arg not allowed.",FALSE);
		else return(curitm->d.cdr->d.cdr->d.cdr);

	case ST_NFETR:				/* look for feature present */
	case ST_FEATR:				/* look for feature */
		curitm = Istsrch(matom("features"));
		if( valarg == (lispval) CNIL) 
		    error("status: need second arg",FALSE);

		for( handy = curitm->d.cdr->d.cdr->d.cdr;
		     handy != nil;
		     handy = handy->d.cdr)
		   if(handy->d.car == valarg) 
			 return(typ == ST_FEATR ? tatom : nil);
		
		return(typ == ST_FEATR ? nil : tatom);

	case ST_SYNT:				/* want character syntax */
		handy = Vreadtable->a.clb;
		chkrtab(handy);
		if( valarg == (lispval) CNIL)
			error("status: need second arg",FALSE);
		
		while (TYPE(valarg) != ATOM) 
		    valarg = error("status: second arg must be atom",TRUE);
		
		indx = valarg->a.pname[0];	/* get first char */

		if(valarg->a.pname[1] != '\0')
			error("status: only one character atom allowed",FALSE);

		handy = inewint((long) ctable[indx]);
		return(handy);

	case ST_RINTB:
		return(stattab[curitm->d.cdr->d.cdr->d.cdr->i]);

	case ST_DMPR:
		return(inewint(dmpmode));
		
	case ST_CTIM:
		 ctim = time((time_t *)0);
		 cp = ctime(&ctim);
		 cp[24] = '\0';
		 return(matom(cp));

	case ST_LOCT:
		 ctim = time((time_t *)0);
		 lctime = localtime(&ctim);
		 (handy = newdot())->d.car = inewint(lctime->tm_sec);
		 protect(handy);
		 handy->d.cdr =  (valarg = newdot());
		 valarg->d.car = inewint(lctime->tm_min);
		 valarg->d.cdr = (curitm = newdot());
		 curitm->d.car = inewint(lctime->tm_hour);
		 curitm->d.cdr = (valarg = newdot());
		 valarg->d.car = inewint(lctime->tm_mday);
		 valarg->d.cdr = (curitm = newdot());
		 curitm->d.car = inewint(lctime->tm_mon);
		 curitm->d.cdr = (valarg = newdot());
		 valarg->d.car = inewint(lctime->tm_year);
		 valarg->d.cdr = (curitm = newdot());
		 curitm->d.car = inewint(lctime->tm_wday);
		 curitm->d.cdr = (valarg = newdot());
		 valarg->d.car = inewint(lctime->tm_yday);
		 valarg->d.cdr = (curitm = newdot());
		 curitm->d.car = inewint(lctime->tm_isdst);
		 Restorestack();
		 return(handy);

	case ST_ISTTY:
		return( (isatty(0) == TRUE ? tatom : nil));

	case ST_UNDEF:
		return(chktt());
	}
	error("Internal error in status: Couldn't figure out request",FALSE);
	/* NOTREACHED */
}
lispval
Nsstatus()
{
	register lispval handy;
	lispval Isstatus();

	handy = lbot->val;

	while( TYPE(handy) != DTPR || TYPE(handy->d.cdr) != DTPR)
	     handy = error("sstatus: Bad args",TRUE);
	
	return(Isstatus(handy->d.car,handy->d.cdr->d.car));
}

/* Isstatus - internal routine to do a set status.	*/
lispval
Isstatus(curnam,curval)
lispval curnam,curval;
{
	register lispval curitm,head;
	lispval Istsrch(),Iaddstat();
	int badmr(),clrtt();
	extern int uctolc, dmpmode, bcdtrsw, gcstrings;

	curitm = Istsrch(curnam);
	/* if doesnt exist, make one up */

	if(curitm == nil) curitm = Iaddstat(curnam,ST_READ,ST_SET,nil);

	switch (curitm->d.cdr->d.cdr->d.car->i) {

	case ST_NO: error("sstatus: cannot set this status",FALSE);

	case ST_SET: goto setit;

	case ST_FEATW: curitm = Istsrch(matom("features"));
		      (curnam = newdot())->d.car = curval;
		      curnam->d.cdr = curitm->d.cdr->d.cdr->d.cdr;	/* old val */
		      curitm->d.cdr->d.cdr->d.cdr = curnam;
		      return(curval);

	case ST_NFETW:	/* remove from features list */
		      curitm = Istsrch(matom("features"))->d.cdr->d.cdr;
		      for(head = curitm->d.cdr; head != nil; head = head->d.cdr)
		      {
			   if(head->d.car == curval) curitm->d.cdr = head->d.cdr;
			   else curitm = head;
		      }
		      return(nil);

		      
	case ST_TOLC: if(curval == nil) uctolc = FALSE;
		      else uctolc = TRUE;	
		      goto setit;

	case ST_CORE: if(curval == nil)
		      {
			signal(SIGBUS,badmr);	 /* catch bus errors */
			signal(SIGSEGV,badmr); /* and segmentation viols */
		      }
		      else {
			signal(SIGBUS,SIG_DFL);	/* let them core dump */
			signal(SIGSEGV,SIG_DFL);
		      }
		      goto setit;

	case ST_INTB: 
		      stattab[curitm->d.cdr->d.cdr->d.cdr->i] = curval;
		      return(curval);

	case ST_DMPW:	
		      if(TYPE(curval) != INT ||
			 (curval->i != 413    &&
			  curval->i != 407    &&
			  curval->i != 410)) errorh1(Vermisc,"sstatus: bad dump mode:",
						  nil,FALSE,0,curval);
		      dmpmode= curval->i;	
		      return(curval);

	 case ST_AUTR:
		      if(curval != nil) Sautor = (lispval) TRUE;
		      else Sautor = FALSE;
		      goto setit;
			
	 case ST_TRAN:
		      if(curval != nil) 
		      {     
			     Strans = (lispval) TRUE;
			     /* the atom `on' set to set up all table
			      * to their bcd fcn if possible
			      */
			     if(curval == matom("on")) clrtt(1);
		      }	
		      else { 
			     Strans = (lispval) FALSE;
			     clrtt(0);	/* clear all transfer tables */
		      }
		      goto setit;
	case ST_BCDTR:
		      if(curval == nil) bcdtrsw = FALSE;
		      else bcdtrsw = TRUE;
		      goto setit;
	case ST_GCSTR:
		      if(curval == nil) gcstrings = FALSE;
		      else gcstrings = TRUE;
		      goto setit;
	}

    setit:	      /* store value in status list */
		      curitm->d.cdr->d.cdr->d.cdr = curval;
		      return(curval);


}

/* Istsrch - utility routine to search the status list for the
   name given as an argument.  If such an entry is not found,
   we return nil
 */
			
lispval Istsrch(nam)
lispval nam;
{
	register lispval handy; 

	for(handy = stlist ; handy != nil ; handy = handy->d.cdr)
	  if(handy->d.car->d.car == nam) return(handy->d.car);

	return(nil);
}

/* Iaddstat - add a status entry to the status list	*/
/*	return new entry in status list */

lispval
Iaddstat(name,readcode,setcode,valu)
lispval name,valu;
int readcode,setcode;
{
	register lispval handy,handy2;
	Savestack(2);


	protect(handy=newdot());	/* build status list here */

	(handy2 = newdot())->d.car = name;

	handy->d.car = handy2;

	((handy2->d.cdr = newdot())->d.car = newint())->i = readcode;

	handy2 = handy2->d.cdr;

	((handy2->d.cdr = newdot())->d.car = newint())->i = setcode;

	handy2->d.cdr->d.cdr = valu;

	/* link this one in */

	handy->d.cdr = stlist;	
	stlist = handy;

	Restorestack();
	return(handy->d.car);	/* return new item in stlist */
}
