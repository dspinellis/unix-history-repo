#include "global.h"
#include <a.out.h>
#define STRLIM 1024

static lispval mkptr();
static struct exec header;
static struct nlist nlist;
static lispval *linkaddr;
static int *bindaddr;
static int fildes;
static lispval currtab;
static lispval curibase;
extern int  fvirgin;
extern int  initflag;
lispval
Lbind(){
	register struct argent *mlbot = lbot;
	register lispval work;
	char *sbrk(), *tfile, cbuf[512], *mytemp(), *gstab();

	snpand(2);

	strcpy(cbuf, gstab());
	printf("getting symbol table from %s\n",cbuf); fflush(stdout);
	if((fildes = open(cbuf,0))<0)
		return(nil);
	/*
	 * Read a.out header to find out where symbol table is.
	 */
	if(read(fildes,(char *)&header,sizeof(header)) <= 0) {
		close(fildes);
		return(nil);
	}
	
	lseek(fildes, header.a_text+header.a_data+header.a_trsize
					+header.a_drsize, 1);

	currtab = Vreadtable->clb;
	Vreadtable->clb = strtab;
	curibase = ibase->clb;
	ibase->clb = inewint(10);
	while((sizeof nlist)==read(fildes,&nlist,sizeof nlist)) {
		if( nlist.n_name[0]!='.' || nlist.n_name[1]!='.')
			continue;
		
		linkaddr = (lispval *)*(int *)nlist.n_value;
		bindaddr = (int *)*(int *)(nlist.n_value+sizeof(int));
		do_linker();
		do_binder();
	}
	ibase->clb = curibase;
	Vreadtable->clb = currtab;
	return(tatom);
}

static do_linker()
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
	initflag = TRUE;
	for(; *i!=-1; i++) {
		temp = *i;
		*i = -1;    /* clobber to short circuit gc */
		findstr(temp, array);
		*i = (int)mkptr(array);
	}
	initflag = FALSE;
}
static do_binder()
{
	char array[STRLIM];
	register lispval handy;
	struct binder {lispval (*b_entry)();
			int b_atmlnk;
			int b_type;} bindage, *pos;

	pos = (struct binder *)bindaddr;
	initflag = TRUE;
	for(bindage= *pos++; bindage.b_atmlnk!=-1; bindage = *pos++) {
		if( bindage.b_type == 99) {
			struct argent *olbot;
			/* we must evaluate this form for effect */
 
			findstr(bindage.b_atmlnk, array);
			/* garbage collection appears to
			   cause problems at this point */
			/* if(ISNIL(copval(gcload,CNIL)) && loading->clb != tatom)
				gc(CNIL);	/*  do a gc if gc will be off  */
			handy = mkptr(array);
			olbot = lbot;
			lbot = np;
			ibase->clb=curibase;
			Vreadtable->clb = currtab;
			(np++)->val = handy;
			Leval();
			Vreadtable->clb = strtab;
			curibase = ibase->clb;
			ibase->clb = inewint(10);
			np = lbot;
			lbot = olbot;
		} else {
			handy = newfunct();
			handy->entry = bindage.b_entry;
			handy->discipline = (bindage.b_type == 0 ? lambda :
								 bindage.b_type == 1 ? nlambda :
								                       macro);

			findstr(bindage.b_atmlnk, array);
			protect(handy);
			mkptr(array)->fnbnd = handy;
		}
	}
	initflag = FALSE;
}

static
findstr(ptr,array)
int ptr;
char *array;
{
	int cnt = 0;
	char *cp;

	cp = ptr + (char *)bindaddr;
	while(cnt<STRLIM && (array[cnt++] = *cp++));
}

static
lispval
mkptr(str)
register char *str;
{
	lispval work, Lread();
	FILE *opiport = piport;
	register FILE *p=stdin;
	struct argent *olbot;
	snpand(2);

	/* find free file descriptor */
	for(;p->_flag&(_IOREAD|_IOWRT);p++)
		if(p >= _iob + _NFILE)
			error("Too many open files to do readlist",FALSE);
	p->_flag = _IOREAD | _IOSTRG;
	p->_base = p->_ptr = str;
	p->_cnt = strlen(str) + 1;
	
	olbot = lbot;
	lbot = np;
	piport = p;
	protect(P(p));
	work = Lread();
	piport = opiport;
	lbot = olbot;
	p->_cnt = 0;
	p->_ptr = p->_base = 0;
	p->_file = 0;
	p->_flag=0;
	return(work);
}




