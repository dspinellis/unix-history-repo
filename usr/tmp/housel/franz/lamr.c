#ifndef lint
static char *rcsid =
   "$Header: lamr.c,v 1.6 84/04/06 23:14:05 layer Exp $";
#endif

/*					-[Sat Jan 29 13:09:59 1983 by jkf]-
 * 	lamr.c				$Locker:  $
 * lambda functions
 *
 * (c) copyright 1982, Regents of the University of California
 */

# include "global.h"

/*
 *
 *  Lalloc
 *
 *  This lambda allows allocation of pages from lisp.  The first
 *  argument is the name of a space, n pages of which are allocated,
 *  if possible.  Returns the number of pages allocated.
 */

lispval
Lalloc()
	{
	long n;
	chkarg(2,"alloc");
	if(TYPE((lbot+1)->val) != INT && (lbot+1)->val != nil )
		error("2nd argument to allocate must be an integer",FALSE);
	n = 1;
	if((lbot+1)->val != nil) n = (lbot+1)->val->i;
	return(alloc((lbot)->val,n));	/*  call alloc to do the work  */
	}

lispval
Lsizeof()
	{
	chkarg(1,"sizeof");
	return(inewint(csizeof(lbot->val)));
	}

lispval
Lsegment()
	{
	chkarg(2,"segment");
chek:	while(TYPE(np[-1].val) != INT )
		np[-1].val=error("LENGTH ARG TO SEGMENT MUST BE INTEGER",TRUE);
	if( np[-1].val->i < 0 )
		{
		np[-1].val = error("LENGTH ARG TO SEGMENT MUST BE POSITIVE",TRUE);
		goto chek;
		}
	return(csegment(typenum((lbot)->val),(int)(np[-1].val->i),FALSE));
	}

/*  Lforget  *************************************************************/
/*									*/
/*  This function removes an atom from the hash table.			*/

lispval
Lforget()
	{
	char *name;
	struct atom *buckpt;
	int hash;
	chkarg(1,"forget");
	if(TYPE(lbot->val) != ATOM)
		error("remob: non-atom argument",FALSE);
	name = lbot->val->a.pname;
	hash = hashfcn(name);

	/*  We have found the hash bucket for the atom, now we remove it  */

	if( hasht[hash] == (struct atom *)lbot->val )
		{
		hasht[hash] = lbot->val->a.hshlnk;
		lbot->val->a.hshlnk = (struct atom *)CNIL;
		return(lbot->val);
		}

	buckpt = hasht[hash];
	while(buckpt != (struct atom *)CNIL)
		{
		if(buckpt->hshlnk == (struct atom *)lbot->val)
			{
			buckpt->hshlnk = lbot->val->a.hshlnk;
			lbot->val->a.hshlnk = (struct atom *)CNIL;
			return(lbot->val);
			}
		buckpt = buckpt->hshlnk;
		}

	/*  Whoops!  Guess it wasn't in the hash table after all.  */

	return(lbot->val);
	}

lispval
Lgetl()
	{
	chkarg(1,"getlength");
	if(TYPE(lbot->val) != ARRAY)
		error("ARG TO GETLENGTH MUST BE AN ARRAY",TRUE);
	return(lbot->val->ar.length);
	}

lispval
Lputl()
	{
	chkarg(2,"putlength");
	if(TYPE((lbot)->val) != ARRAY)
		error("ARG TO PUTLENGTH MUST BE AN ARRAY",FALSE);
chek:	while(TYPE(np[-1].val) != INT)
		np[-1].val = error("ARRAY LENGTH MUST BE AN INTEGER",FALSE);
	if(np[-1].val->i <= 0)
		{
		np[-1].val = error("ARRAY LENGTH MUST BE POSITIVE",TRUE);
		goto chek;
		}
	return((lbot)->val->ar.length = np[-1].val);
	}
lispval
Lgetdel()
	{
	chkarg(1,"getdelta");
	if(TYPE(lbot->val) != ARRAY)
		error("ARG TO GETDELTA MUST BE AN ARRAY",FALSE);
	return(lbot->val->ar.delta);
	}

lispval
Lputdel()
	{
	chkarg(2,"putdelta");
	if(TYPE((np-2)->val) != ARRAY)
		error("ARG TO PUTDELTA MUST BE AN ARRAY",FALSE);
chek:	while(TYPE(np[-1].val) != INT)
		np[-1].val = error("ARRAY LENGTH MUST BE AN INTEGER",TRUE);
	if(np[-1].val->i <= 0)
		{
		np[-1].val = error("Array delta must be positive",TRUE);
		goto chek;
		}
	return((lbot)->val->ar.delta = np[-1].val);
	}

lispval
Lgetaux()
	{
	chkarg(1,"getaux");
	if(TYPE(lbot->val)!=ARRAY)
		error("Arg to getaux must be an array", FALSE);
	return(lbot->val->ar.aux);
	}

lispval
Lputaux()
	{
	chkarg(2,"putaux");

	if(TYPE((lbot)->val)!=ARRAY)
		error("1st Arg to putaux must be array", FALSE);
	return((lbot)->val->ar.aux = np[-1].val);
	}

lispval
Lgetdata()
	{
	chkarg(1,"getdata");
	if(TYPE(lbot->val)!=ARRAY)
		error("Arg to getdata must be an array", FALSE);
	return((lispval)lbot->val->ar.data);
	}

lispval
Lputdata()
	{
	chkarg(2,"putdata");

	if(TYPE(lbot->val)!=ARRAY)
		error("1st Arg to putaux must be array", FALSE);
	return((lispval)(lbot->val->ar.data = (char *)(lbot[1].val)));
	}

lispval
Lgeta()
	{
	chkarg(1,"getaccess");
	if(TYPE(lbot->val) != ARRAY)
		error("ARG TO GETACCESS MUST BE AN ARRAY",FALSE);
	return(lbot->val->ar.accfun);
	}

lispval
Lputa()
	{
	chkarg(2,"putaccess");
	if(TYPE((lbot)->val) != ARRAY)
		error("ARG TO PUTACCESS MUST BE ARRAY",FALSE);
	return((lbot)->val->ar.accfun = np[-1].val);
	}

lispval
Lmarray()
{
	register lispval handy;

	chkarg(5,"marray");

	(handy = newarray());		/*  get a new array cell  */
	handy->ar.data=(char *)lbot->val;/*  insert data address  */
	handy->ar.accfun = lbot[1].val;	/*  insert access function  */
	handy->ar.aux = lbot[2].val;	/*  insert aux data  */
	handy->ar.length = lbot[3].val;	/*  insert length  */
	handy->ar.delta = lbot[4].val;	/*  push delta arg  */
	return(handy);
	}

lispval
Lgtentry()
	{
	chkarg(1,"getentry");
	if( TYPE(lbot->val) != BCD )
		error("ARG TO GETENTRY MUST BE FUNCTION",FALSE);
	return((lispval)(lbot->val->bcd.start));
	}

lispval
Lgetlang()
	{
	chkarg(1,"getlang");
	while(TYPE(lbot->val)!=BCD)
		lbot->val = error("ARG TO GETLANG MUST BE FUNCTION DESCRIPTOR",TRUE);
	return(lbot->val->bcd.language);
	}

lispval
Lputlang()
	{
	chkarg(2,"putlang");
	while(TYPE((lbot)->val)!=BCD)
		lbot->val = error("FIRST ARG TO PUTLANG MUST BE FUNCTION DESCRIPTOR",TRUE);
	(lbot)->val->bcd.language = np[-1].val;
	return(np[-1].val);
	}

lispval
Lgetparams()
	{
	chkarg(1,"getparams");
	if(TYPE(np[-1].val)!=BCD)
		error("ARG TO GETPARAMS MUST BE A FUNCTION DESCRIPTOR",FALSE);
	return(np[-1].val->bcd.params);
	}

lispval
Lputparams()
	{
	chkarg(2,"putparams");
	if(TYPE((lbot)->val)!=BCD)
		error("1st ARG TO PUTPARAMS MUST BE FUNCTION DESCRIPTOR",FALSE);
	return((lbot)->val->bcd.params = np[-1].val);
	}

lispval
Lgetdisc()
	{
	chkarg(1,"getdisc");
	if(TYPE(np[-1].val) != BCD)
		error("ARGUMENT OF GETDISC MUST BE FUNCTION",FALSE);
	return(np[-1].val->bcd.discipline);
	}

lispval
Lputdisc()
	{
	chkarg(2,"putdisc");
	if(TYPE(np[-2].val) != BCD)
		error("ARGUMENT OF PUTDISC MUST BE FUNCTION",FALSE);
	return((np-2)->val->bcd.discipline  = np[-1].val);
	}

lispval
Lgetloc()
	{
	chkarg(1,"getloc");
	if(TYPE(lbot->val)!=BCD)
		error("ARGUMENT TO GETLOC MUST BE FUNCTION",FALSE);
	return(lbot->val->bcd.loctab);
	}

lispval
Lputloc()
	{
	chkarg(2,"putloc");
	if(TYPE((lbot+1)->val)!=BCD);
		error("FIRST ARGUMENT TO PUTLOC MUST BE FUNCTION",FALSE);
	(lbot)->val->bcd.loctab = (lbot+1)->val;
	return((lbot+1)->val);
	}

lispval
Lmfunction()
	{
	register lispval handy;
	chkarg(2,"mfunction");
	handy = (newfunct());	/*  get a new function cell  */
	handy->bcd.start = (lispval (*)())((lbot)->val);	/* insert entry point */
	handy->bcd.discipline = ((lbot+1)->val); /*  insert discipline  */
	return(handy);
	}

/** Lreplace ************************************************************/
/*									*/
/*  Destructively modifies almost any kind of data.		 	*/

lispval
Lreplace()
	{
	register lispval a1, a2;
	register int t;
	chkarg(2,"replace");

	if((t = TYPE(a1 = (lbot)->val)) != TYPE(a2 = np[-1].val))
		error("REPLACE ARGS MUST BE SAME TYPE",FALSE);

	switch( t )
		{

	case VALUE:	a1->l = a2->l;
			return( a1 );

	case INT:	a1->i = a2->i;
			return( a1 );


	case ARRAY:	a1->ar.data = a2->ar.data;
			a1->ar.accfun = a2->ar.accfun;
			a1->ar.length = a2->ar.length;
			a1->ar.delta = a2->ar.delta;
			return( a1 );

	case DOUB:	a1->r = a2->r;
			return( a1 );

	case SDOT:
	case DTPR:	a1->d.car = a2->d.car;
			a1->d.cdr = a2->d.cdr;
			return( a1 );
	case BCD:	a1->bcd.start = a2->bcd.start;
			a1->bcd.discipline = a2->bcd.discipline;
			return( a1 );
	default:
			errorh1(Vermisc,"Replace: cannot handle the type of this arg",
						 nil,FALSE,0,a1);
		}
	/* NOTREACHED */
	}

/* Lvaluep */

lispval
Lvaluep()
	{
	chkarg(1,"valuep");
	if( TYPE(lbot->val) == VALUE ) return(tatom); else return(nil);
	}

CNTTYP() { return; /* HI! COUNT ONE TYPE CALL! */ }

lispval
Lod()
	{
	int i;
	chkarg(2,"od");

	while( TYPE(np[-1].val) != INT )
		np[-1].val = error("2nd ARG TO OD MUST BE INTEGER",TRUE);

	for( i = 0; i < np->val->i; ++i )
		printf(copval(odform,CNIL)->a.pname,((int *)(np[-2].val))[i]);

	dmpport(poport);
	return(nil);
	}
lispval
Lfake()
	{
	chkarg(1,"fake");

	if( TYPE(lbot->val) != INT )
		error("ARG TO FAKE MUST BE INTEGER",TRUE);

	return((lispval)(lbot->val->i));
	}

	/* this used to be Lwhat, but was changed to Lmaknum for maclisp
	   compatiblity
	*/
lispval
Lmaknum()
	{
	chkarg(1,"maknum");
	return(inewint((int)(lbot->val)));
	}
lispval
Lderef()
	{
	chkarg(1,"deref");

	if( TYPE(lbot->val) != INT )
		error("arg to deref must be integer",TRUE);

	return(inewint(*(int *)(lbot->val->i)));
	}

lispval
Lpname()
	{
	chkarg(1,"pname");
	if(TYPE(lbot->val) != ATOM)
		error("ARG TO PNAME MUST BE AN ATOM",FALSE);
	return((lispval)(lbot->val->a.pname));
	}

lispval
Larayref()
	{
	chkarg(2,"arrayref");
	if(TYPE((lbot)->val) != ARRAY)
		error("FIRST ARG TO ARRAYREF MUST BE ARRAY",FALSE);
	vtemp = (lbot + 1)->val;
chek:	while(TYPE(vtemp) != INT)
		vtemp = error("SECOND ARG TO ARRAYREF MUST BE INTEGER",TRUE);
	if( vtemp->i < 0 )
		{
		vtemp = error("NEGATIVE ARRAY OFFSET",TRUE);
		goto chek;
		}
	if( vtemp->i >= (np-2)->val->ar.length->i )
		{
		vtemp = error("ARRAY OFFSET TOO LARGE",TRUE);
		goto chek;
		}
	vtemp = (lispval)((np-2)->val->ar.data + ((np-2)->val->ar.delta->i)*(vtemp->i));
		/*  compute address of desired item  */
	return(vtemp);
			
	}

lispval
Lptr()
	{
	chkarg(1,"ptr");
	return(inewval(lbot->val));
	}

lispval
Llctrace()
	{
	chkarg(1,"lctrace");
	lctrace = (int)(lbot->val->a.clb);
	return((lispval)lctrace);
	}

lispval
Lslevel()
	{
	return(inewint(np-orgnp-2));
	}

lispval
Lsimpld()
	{
	register lispval pt;
	register char *cpt = strbuf;

	chkarg(1,"simpld");

	for(atmlen=1, pt=np->val; NOTNIL(pt); ++atmlen, pt = pt->d.cdr);

	if( atmlen > STRBLEN )
		{
		error("LCODE WAS TOO LONG",TRUE);
		return((lispval)inewstr(""));
		}

	for(pt=np->val; NOTNIL(pt); pt = pt->d.cdr) *(cpt++) = pt->d.car->i;
	*cpt = 0;

	return((lispval)newstr(1));
	}
	
	
/*  Lopval  *************************************************************/
/*									*/
/*  Routine which allows system registers and options to be examined	*/
/*  and modified.  Calls copval, the routine which is called by c code	*/
/*  to do the same thing from inside the system.			*/

lispval 
Lopval()
{
	lispval quant;

	if( lbot == np )
		return(error("bad call to opval",TRUE));
	quant = lbot->val;	 /*  get name of sys variable  */
	while( TYPE(quant) != ATOM )
		quant = error("first arg to opval must be an atom",TRUE);

	if(np > lbot+1)  vtemp = (lbot+1)->val ;
	else vtemp = CNIL;
	return(copval(quant,vtemp));
}
