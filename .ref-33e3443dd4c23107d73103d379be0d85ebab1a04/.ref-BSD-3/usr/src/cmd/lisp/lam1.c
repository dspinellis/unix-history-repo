
# include "global.h"
# include <sgtty.h>
# include "chkrtab.h"
/**************************************************************************/
/*                                                                        */
/*   file: ccdfns.i                                                       */
/*   contents: LISP functions coded in C                                  */
/*                                                                        */
/*   These include LISP primitives, numeric and boolean functions and     */
/*   	predicates, some list-processing functions, i/o support functions */
/*  	and control flow functions (e.g. cont, break).                    */
/*   There are two types of functions: lambda (prefixed "L") and nlambda  */
/*	(prefixed "N"). 					  	  */
/*   Lambda's all call chkarg to insure that at least the minimum number  */
/* 	of necessary arguments are on the namestack.			  */
/*   All functions take their arguments from the namestack in a read-     */
/*  	only manner, and return their results via the normal C value	  */
/*	return mechanism.						  */
/*									  */



lispval
Leval()
{
	register lispval temp;

	chkarg(1);
	temp = lbot->val;
	    return(eval(temp));
}

lispval
Lxcar()
{	register int typ;
	register lispval temp, result;

	chkarg(1);
	temp = lbot->val;
	if (((typ = TYPE(temp)) == DTPR) || (typ == ATOM))
	    return(temp -> car);
	else if(typ == SDOT) {
		result = inewint(temp->i);
		return(result);
	} else if(Schainp!=nil && typ==ATOM)
		return(nil);
	else
		return(error("BAD ARG TO CAR",FALSE));

}

lispval
Lxcdr()
{	register int typ;
	register lispval temp, result;

	chkarg(1);
	temp = lbot->val;
	if(temp==nil) return (nil);

	if ((typ = TYPE(temp)) == DTPR) 
	    return(temp -> cdr);
	else if(typ==SDOT) {
		if(temp->CDR==0) return(nil);
		return(temp->CDR);
	} else if(Schainp!=nil && typ==ATOM)
		return(nil);
	else
		return(error("BAD ARG TO CDR",FALSE));
}

lispval
cxxr(as,ds)
register int as,ds;
{

	register lispval temp, temp2;
	int i, typ;
	lispval errorh();

	chkarg(1);
	temp = lbot->val;

	for( i=0 ; i<ds ; i++)
	{
	    if( temp != nil)
	    {
		if ((typ = TYPE(temp)) == DTPR) 
		    temp = temp -> cdr;
		else if(typ==SDOT) {
			if(temp->CDR==0) temp = nil;
			else temp = temp->CDR;
		}
		else if(Schainp!=nil && typ==ATOM)
			return(nil);
		else
			return(errorh(Vermisc,"BAD ARG TO CDR",nil,FALSE,5,temp));
	    }
	}

	for( i=0 ; i<as ; i++)
	{
	    if( temp != nil )
	    {
		if ((typ = TYPE(temp)) == DTPR)
		    temp = temp -> car;
		else if(typ == SDOT)
			temp2 = inewint(temp->i), temp = temp2;
		else if(Schainp!=nil && typ==ATOM)
			return(nil);
		else
			return(errorh(Vermisc,"BAD ARG TO CAR",nil,FALSE,5,temp));
	    }
	}

	return(temp);
}


lispval
Lcar()
{	return(cxxr(1,0));
}

lispval
Lcdr()
{	return(cxxr(0,1));
}

lispval
Lcadr()
{	return(cxxr(1,1));
}

lispval
Lcaar()
{	return(cxxr(2,0));
}

lispval
Lc02r()
{	return(cxxr(0,2));	/* cddr */
}

lispval
Lc12r()
{	return(cxxr(1,2));	/* caddr */
}

lispval
Lc03r()
{	return(cxxr(0,3));	/* cdddr */
}

lispval
Lc13r()
{	return(cxxr(1,3));	/* cadddr */
}

lispval
Lc04r()
{	return(cxxr(0,4));	/* cddddr */
}

lispval
Lc14r()
{	return(cxxr(1,4));	/* caddddr */
}

/*************************
*  
*  (nthelem num list)
* returns the num'th element of the list, by doing a caddddd...ddr
* where there are num-1 d's
* if num<=0 or greater than the length of the list, we return nil
******************************************************/

lispval
Lnthelem()
{
	register lispval temp;
	register int i;

	chkarg(2);

	if( TYPE(temp = lbot->val) != INT)
	return (error ("First arg to nthelem must be a fixnum",FALSE));

	i = temp->i;	/* pick up the first arg */

	if( i <= 0) return(nil);

	++lbot;			/* fix lbot for call to cxxr() 'cadddd..r' */
	temp = cxxr(1,i-1);
	--lbot;

	return(temp);
}





lispval
Lscons()
{
	register struct argent *argp = lbot;
	register lispval retp, handy;
	register int typ;

	chkarg(2);
	retp = newsdot();
	handy = (argp) -> val;
	if(TYPE(handy)!=INT)
		error("First arg to scons must be an int.",FALSE);
	retp->I = handy->i;
	handy = (argp+1)->val;
	if(handy==nil)
		retp->CDR = (lispval) 0;
	else {
		if(TYPE(handy)!=SDOT)
			error("Currently you may only link sdots to sdots.",FALSE);
		retp->CDR = handy;
	}
	return(retp);
}
lispval
Lcons()
{   register struct argent *argp;
	     lispval	   retp;

	chkarg(2);
	retp = newdot();
	retp -> cdr = ((argp = np-1) -> val);
	retp -> car = (--argp) -> val;
	return(retp);
}
#define CA 0
#define CD 1

lispval
rpla(what)
int what;
{	register struct argent *argp;
	register int typ; register lispval first, second;

	chkarg(2);
	argp = np-1;
	first = (argp-1)->val;
	while(first==nil)
		first = error("Attempt to rplac[ad] nil.",TRUE);
	second = argp->val;
	if (((typ = TYPE(first)) == DTPR) || (typ == ATOM)) {
		if (what == CA)
			first->car = second;
		else 
			first->cdr = second;
		return(first);
	}
	if (typ==SDOT) {
		if(what == CA) {
			typ = TYPE(second);
			if(typ!=INT) error("Rplacca of a bignum will only replace INTS",FALSE);
			first->i = second->i;
		} else {
			if(second==nil)
				first->CDR = (lispval) 0;
			else
				first->CDR = second;
		}
		return(first);
	}
	return(error("BAD ARG TO RPLA",FALSE));
}
lispval
Lrplaca()
{	return(rpla(CA));	}

lispval
Lrplacd()
{	return(rpla(CD));	}


lispval
Leq()
{
	register struct argent *mynp = lbot + AD;
	int itemp, flag;

	chkarg(2);
	if(mynp->val==(mynp+1)->val) return(tatom);
	return(nil);
}



lispval
Lnull()
{	chkarg(1);
	return ((lbot->val == nil) ? tatom : nil);
}



/* Lreturn **************************************************************/
/* Returns the first argument - which is nill if not specified.		*/
Lreturn()
	{
	chkarg(1);
	contval = lbot->val;
	reset(BRRETN);
	}


/* Lretbrk **************************************************************/
/* The first argument must be an integer and must be in the range	*/
/* -1 .. -depth.							*/
lispval
Lretbrk()
	{
	lispval number;
	register level;


	chkarg(1);
	number = lbot->val;
	if (TYPE(number) != INT)
		level = -1;
	else
		level = number->i;
	if(level < 0)
		level += depth;
	contval = (lispval) level;
	if (level < depth)
		reset(BRRETB);
	return(nil);
}



lispval
Linfile()
{
	FILE *port;
	register lispval name;
	snpand(1);

	chkarg(1);
	name = lbot->val;
	while (TYPE(name)!=ATOM)
		name = error("Please supply atom name for port.",TRUE);
	/* return nil if file couldnt be opened
	if ((port = fopen(name->pname,"r")) == NULL) return(nil); */	

	while ((port = fopen(name->pname,"r")) == NULL)
		name = errorh(Vermisc,"Unable to open file for reading.",nil,TRUE,31,name);
								
	return((lispval)(xports + (port - _iob)));
}

lispval
Loutfile()
{
	FILE *port; register lispval name;

	chkarg(1);
	name = lbot->val;
	while (TYPE(name)!=ATOM)
		name = error("Please supply atom name for port.",TRUE);
	while ((port = fopen(name->pname,"w")) == NULL)
		name = errorh(Vermisc,"Unable to open file for writing.",nil,TRUE,31,name);
	return((lispval)(xports + (port - _iob)));
}
lispval
Lterpr()
{
	FILE *port;

	chkarg(1);
	port = okport(lbot->val,okport(Vpoport->clb,stdout));
	putc('\n',port);
	fflush(port);
	return(nil);
}
lispval
Lclose()
{
	lispval port;

	if(lbot==np)
		port = error("Close requires one argument of type port",TRUE);
	port = lbot->val;
	if((TYPE(port))==PORT) fclose(port->p);
	return(tatom);
}

lispval
Lnwritn()
{
	register FILE *port;
	register value;

	chkarg(1);
	port = okport(lbot->val,okport(Vpoport->clb,stdout));
	value = port->_ptr - port->_base;
	return(inewint(value));
}

lispval
Ldrain()
{
	register FILE *port;
	register int iodes;
	struct sgttyb arg;

	chkarg(1);
	port = okport(lbot->val, okport(Vpoport->clb,stdout));
	if(port->_flag & _IOWRT) {
		fflush(port);
		return(nil);
	}
	if(! port->_flag & _IOREAD) return(nil);
	port->_cnt = 0;
	port->_ptr = port->_base;
	iodes = fileno(port);
	if(gtty(iodes,&arg) != -1) stty(iodes,&arg);
	return((lispval)(xports + (port - _iob)));
}
lispval
Llist()
{
	/* added for the benefit of mapping functions. */
	register struct argent *ulim, *namptr;
	register lispval temp, result;
	register struct argent *lbot, *np;

	ulim = np;
	namptr = lbot + AD;
	temp = result = (lispval) np;
	protect(nil);
	for(; namptr < ulim;) {
		temp = temp->l = newdot();
		temp->car = (namptr++)->val;
	}
	temp->l = nil;
	return(result->l);
}

lispval
Lnumberp()
{
	chkarg(1);
	switch(TYPE(lbot->val)) {
	case INT: case DOUB: case SDOT:
		return(tatom);
	}
	return(nil);
}

lispval
Latom()
{
	chkarg(1);
	if(TYPE(lbot->val)==DTPR)
		return(nil);
	else
		return(tatom);
}
lispval
Ltype()
{
	chkarg(1);
	switch(TYPE(lbot->val)) {
	case INT:
		return(int_name);
	case ATOM:
		return(atom_name);
	case SDOT:
		return(sdot_name);
	case DOUB:
		return(doub_name);
	case DTPR:
		return(dtpr_name);
	case STRNG:
		return(str_name);
	case ARRAY:
		return(array_name);
	case BCD:
		return(funct_name);
	case VALUE:
		return(val_name);
	case PORT:
		return(matom("port"));		/* fix this when name exists */
	}
	return(nil);
}

lispval
Ldtpr()
{
	chkarg(1);
	return(typred(DTPR,lbot->val));
}

lispval
Lbcdp()
{
	chkarg(1);
	return(typred(BCD,lbot->val));
}

lispval
Lportp()
{
	chkarg(1);
	return(typred(PORT,lbot->val));
}

lispval
Larrayp()
{
	chkarg(1);
	return(typred(ARRAY,lbot->val));
}
lispval
Lset()
{
	lispval varble;
	snpand(0);

	chkarg(2);
	varble = lbot->val;
	switch(TYPE(varble))
		{
	case ATOM:	return(varble->clb = lbot[1].val);

	case VALUE:	return(varble->l = lbot[1].val);
		}

	error("IMPROPER USE OF SET",FALSE);
}
lispval
Lequal()
{
	chkarg(2);

	if( lbot[1].val == lbot->val ) return(tatom);
	if(Iequal(lbot[1].val,lbot->val)) return(tatom); else return(nil);
}

Iequal(first,second) 
register lispval first, second;
{
	register type1, type2;
	register struct argent *lbot, *np;
	lispval Lsub(),Lzerop();

	if(first==second)
		return(1);
	type1=TYPE(first);
	type2=TYPE(second);
	if(type1!=type2) {
		if((type1==SDOT&&type2==INT)||(type1==INT&&type2==SDOT))
			goto dosub;
		return(0);
	}
	switch(type1) {
	case DTPR:
		 return(
			Iequal(first->car,second->car) &&
			Iequal(first->cdr,second->cdr) );
	case DOUB:
		return(first->r==second->r);
	case INT:
		return( (first->i==second->i));
dosub:
	case SDOT:
		lbot = np;
		np++->val = first;
		np++->val = second;
		lbot->val = Lsub();
		np = lbot + 1;
		return(Lzerop()!=nil);
	case VALUE:
		return( first->l==second->l );
	case STRNG:
		return(strcmp(first,second)==0);
	}
	return(0);
}

lispval
Lprint()
{
	chkarg(2);
	chkrtab(Vreadtable->clb);
	printr(lbot->val,okport(lbot[1].val,okport(Vpoport->clb,poport)));
	return(nil);
}

FILE *
okport(arg,proper) 
lispval arg;
FILE *proper;
{
	if(TYPE(arg)!=PORT)
		return(proper);
	else
		return(arg->p);
}
lispval
Lpatom()
{
	register lispval temp;
	FILE *port;

	chkarg(2);
	temp = Vreadtable->clb;
	chkrtab(temp);
	port = okport(lbot[1].val, okport(Vpoport->clb,stdout));
	if ((TYPE((temp = (lbot)->val)))!=ATOM)
		printr(temp, port);
	else
		fputs(temp->pname, port);
	return(temp);
}

/*
 * (pntlen thing) returns the length it takes to print out
 * an atom or number.
 */

lispval
Lpntlen()
{
	register lispval temp;
	return(inewint(Ipntlen()));
}
Ipntlen()
{
	register lispval temp;
	register char *handy;

	temp = np[-1].val;
loop:	switch(TYPE(temp)) {

	case ATOM:
		handy = temp->pname;
		break;

	case INT:
		sprintf(strbuf,"%d",temp->i);
		handy =strbuf;
		break;

	case DOUB:
		sprintf(strbuf,"%g",temp->r);
		handy =strbuf;
		break;

	default:
		temp = error("Non atom or number to pntlen\n",TRUE);
		goto loop;
	}

	return( strlen(handy));
}
