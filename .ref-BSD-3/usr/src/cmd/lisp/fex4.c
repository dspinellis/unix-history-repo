#include "global.h"
#include "lfuncs.h"
#include "chkrtab.h"
#include <signal.h>

lispval
Nsyscall() {
	register lispval aptr, temp;
	register int acount = 0;
	int args[50];
	snpand(3);

	aptr = lbot->val;
	temp = eval(aptr->car);
	if (TYPE(temp) != INT)
		return(error("syscall", FALSE));
	args[acount++] = temp->i;
	aptr = aptr->cdr;
	while( aptr != nil && acount < 49) {
		temp = eval(aptr->car);
		switch(TYPE(temp)) {

			case ATOM:	
				args[acount++] = (int)temp->a.pname;
				break;

			case INT:
				args[acount++] = (int)temp->i;
				break;

			default:
				return(error("syscall", FALSE));
		}
		aptr = aptr->cdr;
	}

	if (acount==0) chkarg(2);	/* produce arg count message */
	temp = newint();
	temp->i = vsyscall(args);
	return(temp);
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
	snpand(1);

	for(handy=(lbot->val)->car ; handy != nil ; handy = handy->cdr)
	   if (handy->car == (lispval) Veval) { lbot=np ;
						protect(((lbot-1)->val)->cdr);
						return(Nprogn()); } ;


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
 */


lispval
Nstatus()
{
	register lispval handy,curitm,valarg;
	int indx;
	int typ;
	extern char *ctable;
	extern int dmpmode;
	lispval Istsrch();

	if(lbot->val == nil) return(nil);
	handy = lbot->val;		/* arg list */

	while(TYPE(handy) != DTPR) handy = error("status: bad arg list",TRUE); 
	
	curitm = Istsrch(handy->car);	/* look for feature */

	if( curitm == nil ) return(nil);	/* non existant */

	if( handy->cdr == nil ) valarg = (lispval) CNIL;
	else valarg = handy->cdr->car;

	/* now do the processing with curitm pointing to the requested
	   item in the status list 
	 */
	
	switch( typ = curitm->cdr->car->i ) {		/* look at readcode */


	case ST_READ:
		curitm = Istsrch(handy->car);	/* look for name */
		if(curitm == nil) return(nil);
		if( valarg != (lispval) CNIL) 
		    error("status: Second arg not allowed.",FALSE);
		else return(curitm->cdr->cdr->cdr);

	case ST_NFETR:				/* look for feature present */
	case ST_FEATR:				/* look for feature */
		curitm = Istsrch(matom("features"));
		if( valarg == (lispval) CNIL) 
		    error("status: need second arg",FALSE);

		for( handy = curitm->cdr->cdr->cdr;
		     handy != nil;
		     handy = handy->cdr)
		   if(handy->car == valarg) 
			 return(typ == ST_FEATR ? tatom : nil);
		
		return(typ == ST_FEATR ? nil : tatom);

	case ST_SYNT:				/* want characcter syntax */
		handy = Vreadtable->clb;
		chkrtab(handy);
		if( valarg == (lispval) CNIL)
			error("status: need second arg",FALSE);
		
		while (TYPE(valarg) != ATOM) 
		    valarg = error("status: second arg must be atom",TRUE);
		
		indx = valarg->pname[0];	/* get first char */

		if(valarg->pname[1] != '\0')
			error("status: only one character atom allowed",FALSE);

		(handy = newint())->i = ctable[indx] & 0377;
		return(handy);

	case ST_RINTB:
		return(stattab[curitm->cdr->cdr->cdr->i]);

	case ST_DMPR:
		return(inewint(dmpmode));
		
	}
}
lispval
Nsstatus()
{
	register lispval handy;
	lispval Isstatus();

	handy = lbot->val;

	while( TYPE(handy) != DTPR || TYPE(handy->cdr) != DTPR)
	     handy = error("sstatus: Bad args",TRUE);
	
	return(Isstatus(handy->car,handy->cdr->car));
}

/* Isstatus - internal routine to do a set status.	*/
lispval
Isstatus(curnam,curval)
lispval curnam,curval;
{
	register lispval curitm,head;
	lispval Istsrch(),Iaddstat();
	int badmemr();
	extern int uctolc, dmpmode;

	curitm = Istsrch(curnam);
	/* if doesnt exist, make one up */

	if(curitm == nil) curitm = Iaddstat(curnam,ST_READ,ST_SET,nil);

	switch (curitm->cdr->cdr->car->i) {

	case ST_NO: error("sstatus: cannot set this status",FALSE);

	case ST_SET: goto setit;

	case ST_FEATW: curitm = Istsrch(matom("features"));
		      (curnam = newdot())->car = curval;
		      curnam->cdr = curitm->cdr->cdr->cdr;	/* old val */
		      curitm->cdr->cdr->cdr = curnam;
		      return(curval);

	case ST_NFETW:	/* remove from features list */
		      curitm = Istsrch(matom("features"))->cdr->cdr;
		      for(head = curitm->cdr; head != nil; head = head->cdr)
		      {
			   if(head->car == curval) curitm->cdr = head->cdr;
			   else curitm = head;
		      }
		      return(nil);

		      
	case ST_TOLC: if(curval == nil) uctolc = FALSE;
		      else uctolc = TRUE;	
		      goto setit;

	case ST_CORE: if(curval == nil)
		      {
			signal(SIGBUS,badmemr);	 /* catch bus errors */
			signal(SIGSEGV,badmemr); /* and segmentation viols */
		      }
		      else {
			signal(SIGBUS,SIG_DFL);	/* let them core dump */
			signal(SIGSEGV,SIG_DFL);
		      }
		      goto setit;

	case ST_INTB: 
		      stattab[curitm->cdr->cdr->cdr->i] = curval;
		      return(curval);

	case ST_DMPW:	
		      if(TYPE(curval) != INT ||
			 (curval->i != 413    &&
			  curval->i != 410)) errorh(Vermisc,"sstatus: bad dump mode:",
						  nil,FALSE,0,curval);
		      dmpmode= curval->i;	
		      return(curval);
	}

    setit:	      /* store value in status list */
		      curitm->cdr->cdr->cdr = curval;
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

	for(handy = stlist ; handy != nil ; handy = handy->cdr)
	  if(handy->car->car == nam) return(handy->car);

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
	snpand(2);


	protect(handy=newdot());	/* build status list here */

	(handy2 = newdot())->car = name;

	handy->car = handy2;

	((handy2->cdr = newdot())->car = newint())->i = readcode;

	handy2 = handy2->cdr;

	((handy2->cdr = newdot())->car = newint())->i = setcode;

	handy2->cdr->cdr = valu;

	/* link this one in */

	handy->cdr = stlist;	
	stlist = handy;

	return(handy->car);	/* return new item in stlist */
}
