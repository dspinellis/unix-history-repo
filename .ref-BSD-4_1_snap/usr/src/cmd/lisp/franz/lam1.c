static char *sccsid = "@(#)lam1.c	35.3 7/8/81";

# include "global.h"
# include <sgtty.h>
# include "chkrtab.h"
# include "frame.h"
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

	chkarg(1,"eval");
	temp = lbot->val;
	    return(eval(temp));
}

lispval
Lxcar()
{	register int typ;
	register lispval temp, result;

	chkarg(1,"xcar");
	temp = lbot->val;
	if (((typ = TYPE(temp)) == DTPR) || (typ == ATOM) || HUNKP(temp))
	    return(temp->d.car);
	else if(typ == SDOT) {
		result = inewint(temp->i);
		return(result);
	} else if(Schainp!=nil && typ==ATOM)
		return(nil);
	else
		return(error("Bad arg to car",FALSE));

}

lispval
Lxcdr()
{	register int typ;
	register lispval temp, result;

	chkarg(1,"xcdr");
	temp = lbot->val;
	if(temp==nil) return (nil);

	if (((typ = TYPE(temp)) == DTPR) || HUNKP(temp)) 
	    return(temp->d.cdr);
	else if(typ==SDOT) {
		if(temp->s.CDR==0) return(nil);
		temp = temp->s.CDR;
		if(TYPE(temp)==DTPR)
		    errorh(Vermisc,"Fell of the end of a bignum",nil,FALSE,5,lbot->val);
		return(temp);
	} else if(Schainp!=nil && typ==ATOM)
		return(nil);
	else
		return(error("Bad arg to cdr", FALSE));
}

lispval
cxxr(as,ds)
register int as,ds;
{

	register lispval temp, temp2;
	int i, typ;
	lispval errorh();

	chkarg(1,"c{ad}+r");
	temp = lbot->val;

	for( i=0 ; i<ds ; i++)
	{
	    if( temp != nil)
	    {
		typ = TYPE(temp);
		if ((typ == DTPR) || HUNKP(temp))
		    temp = temp->d.cdr;
		else
		    if(typ==SDOT)
		    {
			if(temp->s.CDR==0)
			    temp = nil;
			else
			    temp = temp->s.CDR;
			if(TYPE(temp)==DTPR)
		    	    errorh(Vermisc,"Fell of the end of a bignum",nil,FALSE,5,lbot->val);
		    }
		else
		    if(Schainp!=nil && typ==ATOM)
			return(nil);
		else
		    return(errorh(Vermisc,"Bad arg to cdr",nil,FALSE,5,temp));
	    }
	}

	for( i=0 ; i<as ; i++)
	{
	    if( temp != nil )
	    {
		typ = TYPE(temp);
		if ((typ == DTPR) || HUNKP(temp))
		    temp = temp->d.car;
		else if(typ == SDOT)
			temp2 = inewint(temp->i), temp = temp2;
		else if(Schainp!=nil && typ==ATOM)
		    return(nil);
		else
		    return(errorh(Vermisc,"Bad arg to car",nil,FALSE,5,temp));
	    }
	}

	return(temp);
}

lispval
Lcar()
{	return(cxxr(1,0)); }

lispval
Lcdr()
{	return(cxxr(0,1)); }

lispval
Lcadr()
{	return(cxxr(1,1)); }

lispval
Lcaar()
{	return(cxxr(2,0)); }

lispval
Lc02r()
{	return(cxxr(0,2)); }	/* cddr */

lispval
Lc12r()
{	return(cxxr(1,2)); }	/* caddr */

lispval
Lc03r()
{	return(cxxr(0,3)); }	/* cdddr */

lispval
Lc13r()
{	return(cxxr(1,3)); }	/* cadddr */

lispval
Lc04r()
{	return(cxxr(0,4)); }	/* cddddr */

lispval
Lc14r()
{	return(cxxr(1,4)); }	/* caddddr */

/*
 *  
 *	(nthelem num list)
 *
 * Returns the num'th element of the list, by doing a caddddd...ddr
 * where there are num-1 d's. If num<=0 or greater than the length of
 * the list, we return nil.
 *
 */

lispval
Lnthelem()
{
	register lispval temp;
	register int i;

	chkarg(2,"nthelem");

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

	chkarg(2,"scons");
	retp = newsdot();
	handy = (argp) -> val;
	if(TYPE(handy)!=INT)
		error("First arg to scons must be an int.",FALSE);
	retp->s.I = handy->i;
	handy = (argp+1)->val;
	if(handy==nil)
		retp->s.CDR = (lispval) 0;
	else {
		if(TYPE(handy)!=SDOT)
		    error("Currently you may only link sdots to sdots.",FALSE);
		retp->s.CDR = handy;
	}
	return(retp);
}

lispval
Lbigtol(){
	register lispval handy,newp;

	chkarg(1,"Bignum-to-lisp");
	handy = lbot->val;
	while(TYPE(handy)!=SDOT)
		handy = error(Vermisc,"Non bignum argument to Bignum-to-list",
				nil,TRUE,5755,handy);
	protect(newp = newdot());
	for(; handy != nil; ) {
		newp->d.car = inewint(handy->s.I);
		if(handy->s.CDR==nil) break;
		newp->d.cdr = newdot();
		newp = newp->d.cdr;
		handy = handy->s.CDR;
	}
	handy = (--np)->val;
	return(handy);
}

lispval
Lcons()
{
	register lispval retp;
	register struct argent *argp;

	chkarg(2,"cons");
	retp = newdot();
	retp->d.car = ((argp = lbot) -> val);
	retp->d.cdr = argp[1].val;
	return(retp);
}
#define CA 0
#define CD 1

lispval
rpla(what)
int what;
{	register struct argent *argp;
	register int typ; register lispval first, second;

	chkarg(2,"rplac[ad]");
	argp = np-1;
	first = (argp-1)->val;
	while(first==nil)
		first = error("Attempt to rplac[ad] nil.",TRUE);
	second = argp->val;
	if (((typ = TYPE(first)) == DTPR) || (typ == ATOM) || HUNKP(first)) {
		if (what == CA)
			first->d.car = second;
		else 
			first->d.cdr = second;
		return(first);
	}
	if (typ==SDOT) {
		if(what == CA) {
			typ = TYPE(second);
			if(typ!=INT) error("Rplacca of a bignum will only replace INTS",FALSE);
			first->s.I = second->i;
		} else {
			if(second==nil)
				first->s.CDR = (lispval) 0;
			else
				first->s.CDR = second;
		}
		return(first);
	}
	return(error("Bad arg to rpla",FALSE));
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

	chkarg(2,"eq");
	if(mynp->val==(mynp+1)->val) return(tatom);
	return(nil);
}



lispval
Lnull()
{	chkarg(1,"null");
	return ((lbot->val == nil) ? tatom : nil);
}



/* Lreturn **************************************************************/
/* Returns the first argument - which is nill if not specified.		*/

Lreturn()
{
	if(lbot==np) protect (nil);
	Inonlocalgo(C_RET,lbot->val,nil);
	/* NOT REACHED */
}


lispval
Linfile()
{
	FILE *port;
	register lispval name;
	int typ;

	chkarg(1,"infile");
	name = lbot->val;
loop:
	name = verify(name,"infile: file name must be atom or string");
	/* return nil if file couldnt be opened
	if ((port = fopen((char *)name,"r")) == NULL) return(nil); */	

	if ((port = fopen((char *)name,"r")) == NULL) {
		name = errorh(Vermisc,"Unable to open file for reading.",nil,TRUE,31,name);
		goto loop;
	}
	ioname[PN(port)] = (lispval) inewstr(name);	/* remember name */
	return(P(port));
}

/* outfile - open a file for writing.  
 * 27feb81 [jkf] - modifed to accept two arguments, the second one being a
 *   string or atom, which if it begins with an `a' tells outfile to open the
 *   file in append mode
 */
lispval
Loutfile()
{
	FILE *port; register lispval name;
	char *mode ="w";    /* mode is w for create new file, a for append */
	char *given;

	if(lbot+1== np) protect(nil);
	chkarg(2,"outfile");
	name = lbot->val;
	given = (char *)verify((lbot+1)->val,"Illegal file open mode.");
	if(*given == 'a') mode = "a";
loop:
	name = verify(name,"Please supply atom or string name for port.");
	if ((port = fopen(name,mode)) == NULL) {
		name = errorh(Vermisc,"Unable to open file for writing.",nil,TRUE,31,name);
		goto loop;
	}
	ioname[PN(port)] = (lispval) inewstr(name);
	return(P(port));
}

lispval
Lterpr()
{
	register lispval handy;
	FILE *port;

	if(lbot==np) handy = nil;
	else 
	{ 
	    chkarg(1,"terpr");
	    handy = lbot->val;
	}

	port = okport(handy,okport(Vpoport->a.clb,stdout));
	putc('\n',port);
	fflush(port);
	return(nil);
}

lispval
Lclose()
{
	lispval port;

	chkarg(1,"close");
	port = lbot->val;
	if((TYPE(port))==PORT) fclose(port->p);
	ioname[PN(port->p)] = nil;
	return(tatom);
}

lispval
Lnwritn()
{
	register FILE *port;
	register value;
	register lispval handy;

	if(lbot==np) handy = nil;
	else 
	{
	    chkarg(1,"nwritn");
	    handy = lbot->val;
	}

	port = okport(handy,okport(Vpoport->a.clb,stdout));
	value = port->_ptr - port->_base;
	return(inewint(value));
}

lispval
Ldrain()
{
	register FILE *port;
	register int iodes;
	register lispval handy;
	struct sgttyb arg;

	if(lbot==np) handy = nil;
	else 
	{
	    chkarg(1,"nwritn");
	    handy = lbot->val;
	}
	port = okport(handy, okport(Vpoport->a.clb,stdout));
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
	Savestack(4);

	ulim = np;
	namptr = lbot + AD;
	temp = result = (lispval) np;
	protect(nil);
	for(; namptr < ulim;) {
		temp = temp->l = newdot();
		temp->d.car = (namptr++)->val;
	}
	temp->l = nil;
	Restorestack();
	return(result->l);
}

lispval
Lnumberp()
{
	chkarg(1,"numberp");
	switch(TYPE(lbot->val)) {
	case INT: case DOUB: case SDOT:
		return(tatom);
	}
	return(nil);
}

lispval
Latom()
{
	register struct argent *lb = lbot;
	chkarg(1,"atom");
	if(TYPE(lb->val)==DTPR || (HUNKP(lb->val)))
		return(nil);
	else
		return(tatom);
}

lispval
Ltype()
{
	chkarg(1,"type");
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

	case HUNK2:
		return(hunk_name[0]);
	case HUNK4:
		return(hunk_name[1]);
	case HUNK8:
		return(hunk_name[2]);
	case HUNK16:
		return(hunk_name[3]);
	case HUNK32:
		return(hunk_name[4]);
	case HUNK64:
		return(hunk_name[5]);
	case HUNK128:
		return(hunk_name[6]);

	case VALUE:
		return(val_name);
	case PORT:
		return(port_name);
	}
	return(nil);
}

lispval
Ldtpr()
{
	chkarg(1,"dtpr");
	return(typred(DTPR, lbot->val));
}

lispval
Lbcdp()
{
	chkarg(1,"bcdp");
	return(typred(BCD, lbot->val));
}

lispval
Lportp()
{
	chkarg(1,"portp");
	return(typred(PORT, lbot->val));
}

lispval
Larrayp()
{
	chkarg(1,"arrayp");
	return(typred(ARRAY, lbot->val));
}

/*
 *	(hunkp 'g_arg1)
 * Returns t if g_arg1 is a hunk, otherwise returns nil.
 */

lispval
Lhunkp()
{
	chkarg(1,"hunkp");
	if (HUNKP(lbot->val))
		return(tatom);		/* If a hunk, return t */
	else
		return(nil);		/* else nil */
}

lispval
Lset()
{
	lispval varble;

	chkarg(2,"set");
	varble = lbot->val;
	switch(TYPE(varble))
		{
	case ATOM:	return(varble->a.clb = lbot[1].val);

	case VALUE:	return(varble->l = lbot[1].val);
		}

	error("IMPROPER USE OF SET",FALSE);
}

lispval
Lequal()
{
	register lispval first, second;
	register type1, type2;
	register struct argent *lbot, *np;
	lispval Lsub(),Lzerop(), *stack(), unstack(), *sp();
	lispval *oldsp; int mustloop = FALSE, result;
	chkarg(2,"equal");


	if(lbot->val==lbot[1].val) return(tatom);

	for((oldsp=sp(), stack(lbot->val,lbot[1].val));
	    oldsp > sp();) {

	    first = unstack(); second = unstack();
    again:
	    if(first==second) continue;

	    type1=TYPE(first); type2=TYPE(second);
	    if(type1!=type2) {
		if((type1==SDOT&&type2==INT)||(type1==INT&&type2==SDOT))
		    goto dosub;
		return(nil);
	    }
	    switch(type1) {
	    case DTPR:
		stack(first->d.cdr,second->d.cdr);
		first = first->d.car; second = second->d.car;
		goto again;
	    case DOUB:
		if(first->r!=second->r)
		    return(nil);
		continue;
	    case INT:
		if(first->i!=second->i)
		    return(nil);
		continue;
    dosub:
	    case SDOT:
		lbot = np;
		np++->val = first;
		np++->val = second;
		lbot->val = Lsub();
		if(TYPE(lbot->val)!=INT || lbot->val->i!=0)
		    return(nil);
		np = lbot;
		continue;
	    case VALUE:
		if(first->l!=second->l)
		    return(nil);
		continue;
	    case STRNG:
		if(strcmp(first,second)!=0)
		    return(nil);
		continue;

	    default:
		return(nil);
	    }
	}
	return(tatom);
}
lispval
oLequal()
{
	chkarg(2,"equal");

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
			Iequal(first->d.car,second->d.car) &&
			Iequal(first->d.cdr,second->d.cdr) );
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
		return(TYPE(lbot->val)==INT&& lbot->val->i==0);
	case VALUE:
		return( first->l==second->l );
	case STRNG:
		return(strcmp(first,second)==0);
	}
	return(0);
}
lispval
Zequal()
{
	register lispval first, second;
	register type1, type2;
	register struct argent *lbot, *np;
	lispval Lsub(),Lzerop(), *stack(), unstack(), *sp();
	lispval *oldsp; int mustloop = FALSE, result;
	chkarg(2,"equal");


	if(lbot->val==lbot[1].val) return(tatom);

	for((oldsp=sp(), stack(lbot->val,lbot[1].val));
	    oldsp > sp();) {

	    first = unstack(); second = unstack();
    again:
	    if(first==second) continue;

	    type1=TYPE(first); type2=TYPE(second);
	    if(type1!=type2) {
		if((type1==SDOT&&type2==INT)||(type1==INT&&type2==SDOT))
		    goto dosub;
		return(nil);
	    }
	    switch(type1) {
	    case DTPR:
		stack(first->d.cdr,second->d.cdr);
		first = first->d.car; second = second->d.car;
		goto again;
	    case DOUB:
		if(first->r!=second->r)
		    return(nil);
		continue;
	    case INT:
		if(first->i!=second->i)
		    return(nil);
		continue;
    dosub:
	    case SDOT:
		lbot = np;
		np++->val = first;
		np++->val = second;
		lbot->val = Lsub();
		if(TYPE(lbot->val)!=INT || lbot->val->i!=0)
		    return(nil);
		np = lbot;
		continue;
	    case VALUE:
		if(first->l!=second->l)
		    return(nil);
		continue;
	    case STRNG:
		if(strcmp(first,second)!=0)
		    return(nil);
		continue;
	    }
	}
	return(tatom);
}

/*
 * (print 'expression ['port]) prints the given expression to the given
 * port or poport if no port is given.  The amount of structure
 * printed is a function of global lisp variables prinlevel and
 * prinlength.
 */
lispval
Lprint()
{
	register lispval handy;
	extern int prinlevel,prinlength;


	handy = nil;			/* port is optional, default nil */
	switch(np-lbot) 
	{
	    case 2: handy = lbot[1].val;
	    case 1: break;
	    default: argerr("print");
	}

	chkrtab(Vreadtable->a.clb);
	if(TYPE(Vprinlevel->a.clb) == INT)
	{ 
	   prinlevel = Vprinlevel->a.clb->i;
	}
	else prinlevel = -1;
	if(TYPE(Vprinlength->a.clb) == INT)
	{
	    prinlength = Vprinlength->a.clb->i;
	}
	else prinlength = -1;
	printr(lbot->val,okport(handy,okport(Vpoport->a.clb,poport)));
	return(nil);
}

/* patom does not use prinlevel or prinlength 
 *
 * form is (patom 'value ['port])
 */
lispval
Lpatom()
{
	register lispval temp;
	register lispval handy;
	register int typ;
	FILE *port;
	extern int prinlevel,prinlength;

	handy = nil;			/* port is optional, default nil */
	switch(np-lbot) 
	{
	    case 2: handy = lbot[1].val;
	    case 1: break;
	    default: argerr("patom");
	}

	temp = Vreadtable->a.clb;
	chkrtab(temp);
	port = okport(handy, okport(Vpoport->a.clb,stdout));
	if ((typ= TYPE((temp = (lbot)->val))) == ATOM)
		fputs(temp->a.pname, port);
	else if(typ == STRNG)
		fputs(temp,port);
	else
	{
		printr(temp, port);
	}
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
		handy = temp->a.pname;
		break;

	case STRNG:
		handy = (char *) temp;
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
#undef okport
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
