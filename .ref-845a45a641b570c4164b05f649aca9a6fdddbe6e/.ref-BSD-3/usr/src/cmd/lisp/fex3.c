#include "global.h"

/* chkarg ***************************************************************/
/* This insures that there are at least expnum arguments passed to the	*/
/* BCD function that calls this.  If there are fewer, nil arguments	*/
/* are pushed onto the name stack and np adjusted accordingly.		*/
chkarg(expnum)
int	expnum;			/* expected number of args	*/
{
	register struct argent *work;
	register r10,r9,r8;
	register struct argent *lbot, *np;
	saveonly(1);

	for(work = np,np = lbot + expnum; work < np; )
		work++->val = nil;
		
}


/*
 *Ndumplisp -- create executable version of current state of this lisp.
 */
#include <a.out.h>

asm("	.globl	Dlast")
lispval
Ndumplisp()
{
	register struct exec *workp;
	register lispval argptr, temp;
	register char *fname;
	extern lispval reborn;
	struct exec work, old;
	extern etext;
	extern int dmpmode;
	int descrip, des2, count, ax,mode;
	char tbuf[BUFSIZ];
	snpand(4);

	/* dump mode is kept in decimal (which looks like octal in dmpmode)
	   and is changeable via (sstatus dumpmode n) where n is 413 or 410
	   base 10		
	*/
	if(dmpmode == 413) mode = 0413;
	else mode = 0410;

	workp = &work;
	workp->a_magic	= mode;
	workp->a_text	= ((((unsigned) (&etext)) - 1) & (~PAGRND)) + PAGSIZ;
	workp->a_data	= (unsigned) sbrk(0) - workp->a_text;
	workp->a_bss	= 0;
	workp->a_syms	= 0;
	workp->a_entry	= (unsigned) gstart();
	workp->a_trsize	= 0;
	workp->a_drsize	= 0;

	fname = "savedlisp"; /*set defaults*/
	reborn = CNIL;
	argptr = lbot->val;
	if (argptr != nil) {
		temp = argptr->car;
		if((TYPE(temp))==ATOM)
			fname = temp->pname;
	}
	des2 = open(gstab(),0);
	if(des2 >= 0) {
		if(read(des2,&old,sizeof(old))>=0)
			work.a_syms = old.a_syms;
	}
	descrip=creat(fname,0777); /*doit!*/
	if(-1==write(descrip,workp,sizeof(work)))
	{
		close(descrip);
		error("Dumplisp failed",FALSE);
	}
	if(mode == 0413) lseek(descrip,PAGSIZ,0); 
	if( -1==write(descrip,0,workp->a_text)    ||
	    -1==write(descrip,workp->a_text,workp->a_data) ) {
		close(descrip);
		error("Dumplisp failed",FALSE);
	}
	if(des2>0  && work.a_syms) {
		count = old.a_text + old.a_data + sizeof(old);
		if(-1==lseek(des2,count,0))
			error("Could not seek to stab",FALSE);
		asm("Dlast:");
		for(count = old.a_syms;count > 0; count -=BUFSIZ) {
			ax = read(des2,tbuf,BUFSIZ);
			if(ax==0) {
				printf("Unexpected end of syms",count);
				fflush(stdout);
				break;
			}
			if(ax >  0)
				write(descrip,tbuf,ax);
			else 
				error("Failure to write dumplisp stab",FALSE);
		}
	}
	close(descrip);
	if(des2>0) close(des2);
	reborn = 0;
	return(nil);
}
lispval
typred(typ,ptr)
int 	typ;
lispval	ptr;

{   int tx;
	if ((tx = TYPE(ptr)) == typ) return(tatom);
	if ((tx == INT) && (typ == ATOM)) return(tatom);
	return(nil);
}
lispval
Nfunction()
{
	register lispval handy;

	snpand(1);
	handy = lbot->val->car;
	if(TYPE(handy)==ATOM && handy->fnbnd!=nil)
		return(handy->fnbnd);
	else
		return(handy);
}
