#include "global.h"
#include "lfuncs.h"
#include "chkrtab.h"
#include <a.out.h>
#define round(x,s) ((((x)-1) & ~((s)-1)) + (s))
#define STRLIM 2048

/* this is the original fasl, which used nld to do relocation.
 * On nov 4, it was replaced by rfasl
 */

static lispval mkptr();
static char stabbuf[32]="";
static struct exec header;
static lispval *linkaddr;
static int fildes;
static char *currend;
extern char *stabf;
extern int  fvirgin;
static lispval currtab;
static lispval curibase;
lispval
Loldfasl(){
	register struct argent *mlbot = lbot;
	register lispval work;
	int totsize, readsize;
	lispval csegment(), errorh();
	char *sbrk(), *tfile, cbuf[512], *mytemp(), *gstab();
	struct nament *obnp = bnp;

	snpand(2);
	if(np - mlbot != 1 || TYPE(mlbot[0].val)!=ATOM)
		mlbot[0].val = errorh(Vermisc,
				      "fasl: Incorrect .o file specification:",
				      nil,
				      TRUE,
				      0,
				      mlbot[0].val);

	/*
	 * Invoke loader.
	 */
	currend = sbrk(0);
	tfile = mytemp();
	sprintf(cbuf,
		"/usr/lib/lisp/nld -A %s -T %x -N %s -o %s",
		gstab(),
		currend,
		mlbot[0].val->pname,
		tfile);
	/* printf(cbuf); fflush(stdout);   debugging */
	printf("[fasl: %s]",mlbot[0].val->pname);
	fflush(stdout);
	if(system(cbuf)!=0) {
		unlink(tfile);
		return(nil);
	}
	putchar('\n');			/* signal end of nld */
	fflush(stdout);
	if((fildes = open(tfile,0))<0)
		return(nil);
	if(fvirgin)
		fvirgin = 0;
	else
		unlink(stabf);
	strcpyn(stabbuf,tfile,31);
	stabf = stabbuf;
	/*
	 * Read a.out header to find out how much room to
	 * allocate and attempt to do so.
	 */
	if(read(fildes,(char *)&header,sizeof(header)) <= 0) {
		close(fildes);
		return(nil);
	}
	readsize = header.a_text;
	totsize  = readsize;
	totsize  = round(totsize,PAGSIZ);
	/*
	 * Fix up system indicators, typing info, etc.
	 */
	currend = (char *)csegment(int_name,totsize/(sizeof(int)));
	
	if(readsize!=read(fildes,currend,readsize))
		return(nil);
	linkaddr = (lispval *)*(int *)currend;
	currtab = Vreadtable->clb;
	Vreadtable->clb = strtab;
	curibase = ibase->clb;
	ibase->clb = inewint(10);
	do_linker();
	do_binder();
	ibase->clb=curibase;
	Vreadtable->clb = currtab;
	chkrtab(currtab);	/* added by jkf, shouldnt be needed */
	return(tatom);
}
static char mybuff[40]; 
char *
mytemp()
{
	static seed=0, mypid = 0;
	if(mypid==0) mypid = getpid();
	sprintf(mybuff,"/tmp/Li%d.%d",mypid,seed++);
	return(mybuff);
}

static
do_linker()
{
	register int *i, *end, temp;
	char array[STRLIM];
	extern lispval *bind_lists;

	/* first link this linkage table to the garbage
	   collector's list.  We will try to be tricky
       so that if the garbage collector is invoked
       by mkptr we will not cause markdp() to go off
       the deep end.
	*/
	*(linkaddr-1) = (lispval) bind_lists;
	bind_lists = linkaddr;
	i = (int *)linkaddr;
	end = (int *)(currend + header.a_text - 7);
	for(; i<end; i++) {
		temp = *i;
		*i = -1;    /* clobber to short circuit gc */
		findstr(temp, array);
		*i = (int)mkptr(array);
	}
}
static
do_binder()
{
	char array[STRLIM];
	struct argent *onp = np;
	int pos;
	register lispval handy;
	struct {lispval (*b_entry)();
			int b_atmlnk;
			int b_type;} bindage;

	snpand(0);
	pos = lseek(fildes, (sizeof header)+header.a_text, 0);
	while(read(fildes, &bindage, sizeof bindage)==sizeof bindage
			&& bindage.b_atmlnk != -1) {
		np = onp;
		if( bindage.b_type == 99) {
			/* we must evaluate this form for effect */
			/* and must take care that setsyntax works
			   on the proper read table */
 
			findstr(bindage.b_atmlnk, array);
			if(ISNIL(copval(gcload,CNIL)) && loading->clb != tatom)
				gc(CNIL);	/*  do a gc if gc will be off  */
			handy = (mkptr(array));
			ibase->clb=curibase;
			Vreadtable->clb = currtab;
			eval(handy);
			Vreadtable->clb = strtab;
			curibase = ibase->clb;
			ibase->clb = inewint(10);
			goto out;
		}
		handy = newfunct();
		protect(handy);
		handy->entry = bindage.b_entry;
		handy->discipline = (bindage.b_type == 0 ? lambda :
							 bindage.b_type == 1 ? nlambda :
							                       macro);

		findstr(bindage.b_atmlnk, array);
		if(*array != '(')
			mkptr(array)->fnbnd = handy;
		else {
			char *i,*j,*index();
			lispval prop, atom;

			i = index(array, ':');
			j = index(array, ')');
			*i = 0;
			*j = 0;
			protect(prop = mkptr(array+1));
			atom = mkptr(i+1);
			Iputprop(atom,handy,prop);
		}
	out:
		pos = lseek(fildes, pos + sizeof bindage, 0);
	}
}

static
findstr(ptr,array)
int ptr;
char *array;
{
	int cnt = 0;

	lseek(fildes, sizeof header + header.a_text + ptr, 0);
	while(cnt<STRLIM && read(fildes,&array[cnt],1)==1
				&& array[cnt]!=0) cnt++;
	if(cnt >= STRLIM) error("fasl string table overflow",FALSE);
}

static lispval
mkptr(str)
register char *str;
{
	lispval work;
	register FILE *p=stdin;
	snpand(2);

	/* find free file descriptor */
	for(;p->_flag&(_IOREAD|_IOWRT);p++)
		if(p >= _iob + _NFILE)
			error("Too many open files to do readlist",FALSE);
	p->_flag = _IOREAD | _IOSTRG;
	p->_base = p->_ptr = str;
	p->_cnt = strlen(str) + 1;
	
	lbot = np;
	protect(P(p));
	work = Lread();
	p->_cnt = 0;
	p->_ptr = p->_base = 0;
	p->_file = 0;
	p->_flag=0;
	return(work);
}

