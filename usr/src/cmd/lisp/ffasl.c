static char *sccsid = "@(#)ffasl.c	34.3 10/23/80";

#include "global.h"
#include <sys/types.h>
#include <pagsiz.h>
#include <sys/stat.h>
#include "naout.h"
#define round(x,s) ((((x)-1) & ~((s)-1)) + (s))

char *stabf = 0;
int fvirgin = 1;
static seed=0, mypid = 0;
lispval verify();

/* dispget - get discipline of function
 * this is used to handle the tricky defaulting of the discipline
 * field of such functions as cfasl and getaddress.
 * dispget is given the value supplied by the caller,
 *	the error message to print if something goes wrong,
 *	the default to use if nil was supplied.
 * the discipline can be an atom or string.  If an atom it is supplied
 * it must be lambda, nlambda or macro.  Otherwise the atoms pname
 * is used.
 */

lispval 
dispget(given,messg,defult)
lispval given,defult;
char *messg;
{
	int typ;

	while(TRUE)
	{
		if(given == nil) 
		   return(defult);
		if((typ=TYPE(given)) == ATOM)
		{  if(given == lambda ||
		      given == nlambda ||
		      given == macro) return(given);
		   else return((lispval) given->a.pname);
		} else if(typ == STRNG) return(given);

		given = errorh(Vermisc,messg,nil,TRUE,0,given);
	}
}

lispval
Lcfasl(){
	register struct argent *mlbot = lbot;
	register lispval work;
	register int fildes, totsize;
	int readsize;
	register struct argent *lbot, *np;
	lispval csegment();
	char *sbrk(), *currend, *tfile, cbuf[6000], *mytemp(), *gstab();
	char ostabf[128];
	struct exec header;
	char *largs;
	snpand(4);

	switch(np-lbot) {
	   case 3: protect(nil);	/* no discipline given */
	   case 4: protect(nil);        /* no library given  */
	}
	chkarg(5,"cfasl");
	mlbot[0].val = verify(mlbot[0].val,"Incorrect .o file specification");
	mlbot[1].val = verify(mlbot[1].val,"Incorrect entry specification for cfasl");
	mlbot[3].val = dispget(mlbot[3].val,"Incorrect discipline specification for cfasl",Vsubrou->a.pname);
	while(TYPE(mlbot[2].val)!= ATOM) 
	mlbot[2].val = errorh(Vermisc,"Bad associated atom name for fasl",
						 nil,TRUE,0,mlbot[2].val);
	work = mlbot[4].val;
	if(work==nil)
		largs = 0;
	else 
		largs = (char *) verify(work,"Bad loader flags");

	/*
	 * Invoke loader.
	 */
	strcpy(ostabf,gstab());
	currend = sbrk(0);
	tfile = mytemp();
	sprintf(cbuf,
		"/usr/lib/lisp/nld -N -A %s -T %x %s -e %s -o %s %s -lc",
		ostabf,
		currend,
		mlbot[0].val,
		mlbot[1].val,
		tfile,
		largs);
	printf(cbuf); fflush(stdout);
	if(system(cbuf)!=0) {
		unlink(tfile);
		ungstab();
		fprintf(stderr,"Ld returns error status\n");
		return(nil);
	}
	putchar('\n'); fflush(stdout);
	if(fvirgin)
		fvirgin = 0;
	else
		unlink(ostabf);
	stabf = tfile;
	if((fildes = open(tfile,0))<0) {
		fprintf(stderr,"Couldn't open temporary file: %s\n",tfile);
		return(nil);
	}
	/*
	 * Read a.out header to find out how much room to
	 * allocate and attempt to do so.
	 */
	if(read(fildes,(char *)&header,sizeof(header)) <= 0) {
		close(fildes);
		return(nil);
	}
	readsize = round(header.a_text,4) + round(header.a_data,4);
	totsize  = readsize + header.a_bss;
	totsize  = round(totsize,512);
	/*
	 * Fix up system indicators, typing info, etc.
	 */
	currend = (char *)csegment(str_name,totsize,FALSE);
	
	if(readsize!=read(fildes,currend,readsize))
		return(nil);
	work = newfunct();
	work->bcd.entry = (lispval (*)())header.a_entry;
	work->bcd.discipline = mlbot[3].val;
	return(mlbot[2].val->a.fnbnd = work);
}
static char myname[100];
char *
gstab()
{
	register char *cp, *cp2; char *getenv();
	struct stat stbuf;
	extern char **Xargv;

	if(stabf==0) {
		cp = getenv("PATH");
		if(cp==0)
			cp=":/usr/ucb:/bin:/usr/bin";
		if(*cp==':'||*Xargv[0]=='/') {
			cp++;
			if(stat(Xargv[0],&stbuf)==0) {
				strcpy(myname,Xargv[0]);
				return(stabf = myname);
			}
		}
		for(;*cp;) {

			/* copy over current directory
			   and then append argv[0] */

			for(cp2=myname;(*cp)!=0 && (*cp)!=':';)
				*cp2++ = *cp++;
			*cp2++ = '/';
			strcpy(cp2,Xargv[0]);
			if(*cp) cp++;
			if(0!=stat(myname,&stbuf)) continue;
			return(stabf = myname);
		}
		error("Could not find which file is being executed.",FALSE);
	} else return (stabf);
}

static char mybuff[40]; 
char *
mytemp()
{
	if(mypid==0) mypid = getpid();
	sprintf(mybuff,"/tmp/Li%d.%d",mypid,seed++);
	return(mybuff);
}
ungstab()
{
	seed--;
	sprintf(mybuff,"/tmp/Li%d.%d",mypid,seed-1);
	if(seed==0) {
		stabf = 0;
		fvirgin = 1;
	}
}
lispval
verify(in,error)
register lispval in;
char *error;
{
	for(EVER) {
		switch(TYPE(in)) {
		case STRNG:
			return(in);
		case ATOM:
			return((lispval)in->a.pname);
		}
		in = errorh(Vermisc,error,nil,TRUE,0,in);
	}
}
