#include "global.h"

/* various functions from the c math library */
double sin(),cos(),asin(),acos(),atan2(),sqrt(), log(), exp();

lispval Imath(func)
double func();
{
	register lispval handy;
	register double res;
	chkarg(1);

	switch(TYPE(handy=lbot->val)) {
	 case INT: res = func((double)handy->i); 
		   break;

	 case DOUB: res = func(handy->r);
		   break;

	 default:  error("Non fixnum or flonum to math function",FALSE);
	}
	handy = newdoub();
	handy->r = res;
	return(handy);
}
lispval Lsin()
{
	return(Imath(sin));
}

lispval Lcos()
{
	return(Imath(cos));
}

lispval Lasin()
{
	return(Imath(asin));
}

lispval Lacos()
{
	return(Imath(acos));
}

lispval Lsqrt()
{
	return(Imath(sqrt));
}
lispval Lexp()
{
	return(Imath(exp));
}

lispval Llog()
{
	return(Imath(log));
}

/* although we call this atan, it is really atan2 to the c-world,
   that is, it takes two args
 */
lispval Latan()
{
	register lispval arg;
	register double arg1v;
	register double res;
	chkarg(2);

	switch(TYPE(arg=lbot->val)) {

	case INT:  arg1v = (double) arg->i;
		   break;

	case DOUB: arg1v = arg->r;
		   break;

	default:   error("Non fixnum or flonum arg to atan2",FALSE);
	}

	switch(TYPE(arg = (lbot+1)->val)) {

	case INT: res = atan2(arg1v,(double) arg->i);
		  break;

	case DOUB: res = atan2(arg1v, arg->r);
		  break;

	default:  error("Non fixnum or flonum to atan2",FALSE);
	}
	arg = newdoub();
	arg->r = res;
	return(arg);
}

/* (random) returns a fixnum in the range -2**30 to 2**30 -1
   (random fixnum) returns a fixnum in the range 0 to fixnum-1
 */
lispval
Lrandom()
{
	register int curval;
	float pow();

	curval = rand();	/* get numb from 0 to 2**31-1 */

	if(np==lbot) return(inewint(curval-(int)pow((double)2,(double)30)));

	if((TYPE(lbot->val) != INT)
	    || (lbot->val->i <= 0)) errorh(Vermisc,"random: non fixnum arg:",
						 nil, FALSE, 0, lbot->val);

	return(inewint(curval % lbot->val->i )); 

}
lispval
Lmakunb()
{
	register lispval work;

	chkarg(1);
	work = lbot->val;
	if(work==nil || (TYPE(work)!=ATOM))
		return(work);
	work->clb = CNIL;
	return(work);
}
lispval
Lpolyev()
{
	register int count; 
	register double *handy, *base;
	register struct argent *argp, *lbot, *np;
	lispval result; int type;

	count = 2 * (((int) np) - (int) lbot);
	if(count == 0) 
		return(inewint(0));
	if(count == 8)
		return(lbot->val);
	base = handy = (double *) alloca(count);
	for(argp = lbot; argp < np; argp++) {
		while((type = TYPE(argp->val))!=DOUB && type!=INT)
			argp->val = (lispval) errorh(Vermisc,"%%machine-polyev:non-real arg",nil,TRUE,73,lbot,argp->val);
		if(TYPE(argp->val)==INT) {
			*handy++ = argp->val->i;
		} else
			*handy++ = argp->val->r;
	}
	count = count/sizeof(double) - 2;
	asm("polyd	(r9),r11,8(r9)");
	asm("movd	r0,(r9)");
	result = newdoub();
	result->r = *base;
	return(result);
}
