# include "global.h"
# include <a.out.h>

/************************************************************************/
/*									*/
/*  Lalloc								*/
/*									*/
/*  This lambda allows allocation of pages from lisp.  The first	*/
/*  argument is the name of a space, n pages of which are allocated,	*/
/*  if possible.  Returns the number of pages allocated.		*/

lispval
Lalloc()
	{
	int n;
	register struct argent *mylbot = lbot;
	snpand(1);
	chkarg(2);
	if(TYPE((mylbot+1)->val) != INT && (mylbot+1)->val != nil )
		error("2nd ARGUMENT TO ALLOCATE MUST BE AN INTEGER",FALSE);
	n = 1;
	if((mylbot+1)->val != nil) n = (mylbot+1)->val->i;
	return(alloc((mylbot)->val,n));	/*  call alloc to do the work  */
	}

lispval
Lsizeof()
	{
	chkarg(1);
	return(inewint(csizeof(lbot->val)));
	}

lispval
Lsegment()
	{
	chkarg(2);
chek:	while(TYPE(np[-1].val) != INT )
		np[-1].val=error("LENGTH ARG TO SEGMENT MUST BE INTEGER",TRUE);
	if( np[-1].val->i < 0 )
		{
		np[-1].val = error("LENGTH ARG TO SEGMENT MUST BE POSITIVE",TRUE);
		goto chek;
		}
	return(csegment((lbot)->val,np[-1].val->i));
	}

/*  Lforget  *************************************************************/
/*									*/
/*  This function removes an atom from the hash table.			*/

lispval
Lforget()
	{
	char c,*name;
	struct atom *buckpt;
	int hash;
	chkarg(1);
	if(TYPE(lbot->val) != ATOM)
		error("CANNOT FORGET NON-ATOM",FALSE);
	name = lbot->val->pname;
	hash = 0;
	while( (c = *name++) != NULL_CHAR) hash ^= c;
	hash = hash & 0177;

	/*  We have found the hash bucket for the atom, now we remove it  */

	if( hasht[hash] == (struct atom *)lbot->val )
		{
		hasht[hash] = lbot->val->hshlnk;
		lbot->val->hshlnk = (struct atom *)CNIL;
		return(lbot->val);
		}

	buckpt = hasht[hash];
	while(buckpt != (struct atom *)CNIL)
		{
		if(buckpt->hshlnk == (struct atom *)lbot->val)
			{
			buckpt->hshlnk = lbot->val->hshlnk;
			lbot->val->hshlnk = (struct atom *)CNIL;
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
	chkarg(1);
	if(TYPE(lbot->val) != ARRAY)
		error("ARG TO GETLENGTH MUST BE AN ARRAY",TRUE);
	return(lbot->val->length);
	}

lispval
Lputl()
	{
	chkarg(2);
	if(TYPE((lbot)->val) != ARRAY)
		error("ARG TO PUTLENGTH MUST BE AN ARRAY",FALSE);
chek:	while(TYPE(np[-1].val) != INT)
		np[-1].val = error("ARRAY LENGTH MUST BE AN INTEGER",FALSE);
	if(np[-1].val->i <= 0)
		{
		np[-1].val = error("ARRAY LENGTH MUST BE POSITIVE",TRUE);
		goto chek;
		}
	return((lbot)->val->length = np[-1].val);
	}
lispval
Lgetdel()
	{
	chkarg(1);
	if(TYPE(lbot->val) != ARRAY)
		error("ARG TO GETDELTA MUST BE AN ARRAY",FALSE);
	return(lbot->val->delta);
	}

lispval
Lputdel()
	{
	chkarg(2);
	if(TYPE((np-2)->val) != ARRAY)
		error("ARG TO PUTDELTA MUST BE AN ARRAY",FALSE);
chek:	while(TYPE(np[-1].val) != INT)
		np[-1].val = error("ARRAY LENGTH MUST BE AN INTEGER",TRUE);
	if(np[-1].val->i <= 0)
		{
		np[-1].val = error("ARRAY DELTA MUST BE POSITIVE",TRUE);
		goto chek;
		}
	return((lbot)->val->delta = np[-1].val);
	}

lispval
Lgetaux()
	{
	chkarg(1);
	if(TYPE(lbot->val)!=ARRAY)
		error("ARG TO GETAUX MUST BE ARRAY",FALSE);
	return(lbot->val->aux);
	}

lispval
Lputaux()
	{
	chkarg(2);

	if(TYPE((lbot)->val)!=ARRAY)
		error("1st ARG TO PUTAUX MUST BBE ARRAY",FALSE);
	return((lbot)->val->aux = np[-1].val);
	}

lispval
Lgeta()
	{
	chkarg(1);
	if(TYPE(lbot->val) != ARRAY)
		error("ARG TO GETACCESS MUST BE AN ARRAY",FALSE);
	return(lbot->val->accfun);
	}

lispval
Lputa()
	{
	chkarg(2);
	if(TYPE((lbot)->val) != ARRAY)
		error("ARG TO PUTACCESS MUST BE ARRAY",FALSE);
	return((lbot)->val->accfun = np[-1].val);
	}

lispval
Lmarray()
{
	register struct argent *mylbot = lbot;
	register lispval handy;
	snpand(2);
	chkarg(5);
	(handy = newarray());		/*  get a new array cell  */
	handy->data=(char *)mylbot->val;/*  insert data address  */
	handy->accfun = mylbot[1].val;	/*  insert access function  */
	handy->aux = mylbot[2].val;	/*  insert aux data  */
	handy->length = mylbot[3].val;	/*  insert length  */
	handy->delta = mylbot[4].val;	/*  push delta arg  */
	return(handy);
	}

lispval
Lgetentry()
	{
	chkarg(1);
	if( TYPE(lbot->val) != BCD )
		error("ARG TO GETENTRY MUST BE FUNCTION",FALSE);
	return((lispval)(lbot->val->entry));
	}

lispval
Lgetlang()
	{
	chkarg(1);
	while(TYPE(lbot->val)!=BCD)
		lbot->val = error("ARG TO GETLANG MUST BE FUNCTION DESCRIPTOR",TRUE);
	return(lbot->val->language);
	}

lispval
Lputlang()
	{
	chkarg(2);
	while(TYPE((lbot)->val)!=BCD)
		lbot->val = error("FIRST ARG TO PUTLANG MUST BE FUNCTION DESCRIPTOR",TRUE);
	(lbot)->val->language = np[-1].val;
	return(np[-1].val);
	}

lispval
Lgetparams()
	{
	chkarg(1);
	if(TYPE(np[-1].val)!=BCD)
		error("ARG TO GETPARAMS MUST BE A FUNCTION DESCRIPTOR",FALSE);
	return(np[-1].val->params);
	}

lispval
Lputparams()
	{
	chkarg(2);
	if(TYPE((lbot)->val)!=BCD)
		error("1st ARG TO PUTPARAMS MUST BE FUNCTION DESCRIPTOR",FALSE);
	return((lbot)->val->params = np[-1].val);
	}

lispval
Lgetdisc()
	{
	chkarg(1);
	if(TYPE(np[-1].val) != BCD)
		error("ARGUMENT OF GETDISC MUST BE FUNCTION",FALSE);
	return(np[-1].val->discipline);
	}

lispval
Lputdisc()
	{
	chkarg(2);
	if(TYPE(np[-2].val) != BCD)
		error("ARGUMENT OF PUTDISC MUST BE FUNCTION",FALSE);
	return((np-2)->val->discipline  = np[-1].val);
	}

lispval
Lgetloc()
	{
	chkarg(1);
	if(TYPE(lbot->val)!=BCD)
		error("ARGUMENT TO GETLOC MUST BE FUNCTION",FALSE);
	return(lbot->val->loctab);
	}

lispval
Lputloc()
	{
	chkarg(2);
	if(TYPE((lbot+1)->val)!=BCD);
		error("FIRST ARGUMENT TO PUTLOC MUST BE FUNCTION",FALSE);
	(lbot)->val->loctab = (lbot+1)->val;
	return((lbot+1)->val);
	}

lispval
Lmfunction()
	{
	register lispval handy;
	chkarg(5);
	handy = (newfunct());	/*  get a new function cell  */
	handy->entry = (lispval (*)())((np-5)->val);	/* insert entry point */
	handy->discipline = ((np-4)->val); /*  insert discipline  */
#ifdef ROWAN
	handy->language = (np-3)->val;  /*  insert language  */
	handy->params = ((np-2)->val);     /*  insert parameters  */
	handy->loctab = ((np-1)->val);	/*  insert local table  */
#endif
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
	chkarg(2);

	if((t = TYPE(a1 = (lbot)->val)) != TYPE(a2 = np[-1].val))
		error("REPLACE ARGS MUST BE SAME TYPE",FALSE);

	switch( t )
		{
	case ATOM:	error("REPLACE CANNOT STORE ATOMS",FALSE);

	case VALUE:	a1->l = a2->l;
			return( a1 );

	case INT:	a1->i = a2->i;
			return( a1 );

	case STRNG:	error("STORE CANNOT STORE STRINGS",FALSE);

	case ARRAY:	a1->data = a2->data;
			a1->accfun = a2->accfun;
			a1->length = a2->length;
			a1->delta = a2->delta;
			return( a1 );

	case DOUB:	a1->r = a2->r;
			return( a1 );

	case SDOT:
	case DTPR:	a1->car = a2->car;
			a1->cdr = a2->cdr;
			return( a1 );
	case BCD:	a1->entry = a2->entry;
			a1->discipline = a2->discipline;
			return( a1 );
		}
	/* NOT REACHED */
	}

/* Lvaluep */

lispval
Lvaluep()
	{
	chkarg(1);
	if( TYPE(lbot->val) == VALUE ) return(tatom); else return(nil);
	}

CNTTYP() { return; /* HI! COUNT ONE TYPE CALL! */ }

lispval
Lod()
	{
	int i;
	chkarg(2);

	while( TYPE(np[-1].val) != INT )
		np[-1].val = error("2nd ARG TO OD MUST BE INTEGER",TRUE);

	for( i = 0; i < np->val->i; ++i )
		printf(copval(odform,CNIL)->pname,(int *)(np[-2].val)[i]);

	dmpport(poport);
	return(nil);
	}
lispval
Lfake()
	{
	chkarg(1);

	if( TYPE(lbot->val) != INT )
		error("ARG TO FAKE MUST BE INTEGER",TRUE);

	return((lispval)(lbot->val->i));
	}

lispval
Lwhat()
	{
	chkarg(1);
	return(inewint((int)(lbot->val)));
	}

lispval
Lpname()
	{
	chkarg(1);
	if(TYPE(lbot->val) != ATOM)
		error("ARG TO PNAME MUST BE AN ATOM",FALSE);
	return((lispval)(lbot->val->pname));
	}

lispval
Larrayref()
	{
	chkarg(2);
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
	if( vtemp->i >= (np-2)->val->length->i )
		{
		vtemp = error("ARRAY OFFSET TOO LARGE",TRUE);
		goto chek;
		}
	vtemp = (lispval)((np-2)->val->data + ((np-2)->val->delta->i)*(vtemp->i));
		/*  compute address of desired item  */
	return(vtemp);
			
	}

lispval
Lptr()
	{
	chkarg(1);
	return(inewval(lbot->val));
	}

lispval
Llctrace()
	{
	chkarg(1);
	lctrace = (int)(lbot->val->clb);
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

	chkarg(1);

	for(atmlen=1, pt=np->val; NOTNIL(pt); ++atmlen, pt = pt->cdr);

	if( atmlen > STRBLEN )
		{
		error("LCODE WAS TOO LONG",TRUE);
		return((lispval)inewstr(""));
		}

	for(pt=np->val; NOTNIL(pt); pt = pt->cdr) *(cpt++) = pt->car->i;
	*cpt = 0;

	return((lispval)newstr());
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
	snpand(0);

	if( lbot == np )
		return(error("BAD CALL TO OPVAL",TRUE));
	quant = lbot->val;	 /*  get name of sys variable  */
	while( TYPE(quant) != ATOM )
		quant = error("FIRST ARG TO OPVAL MUST BE AN ATOM",TRUE);

	if(np > lbot+1)  vtemp = (lbot+1)->val ;
	else vtemp = CNIL;
	return(copval(quant,vtemp));
}
	
