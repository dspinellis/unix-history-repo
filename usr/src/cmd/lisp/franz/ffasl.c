static char *sccsid = "@(#)ffasl.c	35.1 5/6/81";

#include "global.h"
#include <sys/types.h>
#include <pagsiz.h>
#include <sys/stat.h>
#include "naout.h"
#define round(x,s) ((((x)-1) & ~((s)-1)) + (s))

char *stabf = 0;
extern int fvirgin;
static seed=0, mypid = 0;
static char myname[100];
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
#ifndef VMS
			/*** UNIX cfasl code ***/
	tfile = mytemp();
	sprintf(cbuf,
		"/usr/lib/lisp/nld -N -x -A %s -T %x %s -e %s -o %s %s -lc",
		ostabf,
		currend,
		mlbot[0].val,
		mlbot[1].val,
		tfile,
		largs);
	/* if nil don't print cfasl/nld message */
	if ( Vldprt->a.clb != nil ) {
		printf(cbuf);
		putchar('\n'); fflush(stdout);
	}
	if(system(cbuf)!=0) {
		unlink(tfile);
		ungstab();
		fprintf(stderr,"Ld returns error status\n");
		return(nil);
	}
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
#else
			/*** VMS cfasl code ***/
	{
	  int pid = getpid() & 0xffff;	/* Our process ID number */
	  char objfil[100];		/* Absolute object file name */
	  char symfil[100];		/* Old symbol table file */
	  char filename[100];		/* Random filename buffer */
	  int strlen();			/* String length function */
	  int Cvt_Unix_To_VMS();	/* Convert UNIX to VMS filename */
	  lispval Lgetaddress(),matom();
	  struct stat stbuf;
	  struct VMS_string_descriptor {
		  int   string_length;
		  char *string_pointer;
		  };
	  static struct VMS_string_descriptor link_command;
	  static struct VMS_string_descriptor noverify_command =
				{17,"$ V='F$VERIFY(0)'"};
	  static struct VMS_string_descriptor stop_command =
				{20,"$ STOP 'F$PROCESS()'"};

	  if (largs == 0) largs = " ";
	  sprintf(objfil,"tmp:cfasl%d.tmp",pid);
	  symfil[Cvt_Unix_To_VMS(ostabf,symfil)] = 0;
	  sprintf(cbuf,					/* Create link cmd. */
		"$ link/exe=%s/nom/syst=%%X%x/sym=tmp:sym%d.new %s,%s.stb%s",
		objfil,
		currend,
		pid,
		mlbot[0].val,
		symfil,
		largs);
	  printf(					/* Echo link cmd. */
		"$ link/exe=%s/nomap/system=%%X%x/symbol_table=tmp:sym%d.new %s,%s.stb%s\n",
		objfil,
		currend,
		pid,
		mlbot[0].val,
		symfil,
		largs);
	  fflush(stdout);
	  link_command.string_length=strlen(cbuf);
	  link_command.string_pointer=cbuf;
	  lib$execute_cli(&noverify_command,&link_command,&stop_command);

	  if ((fildes = open(objfil,0)) < 0) return(nil);/* Open abs file */
	  fstat(fildes,&stbuf);				/* Get its size */
	  readsize=stbuf.st_size;
	  currend = (char *)csegment(str_name,readsize,FALSE);
	  readsize = read(fildes,currend,10000000);
	  close(fildes);
	  /*
	   * Delete the absolute object file
	   */
	  unlink(objfil);
	  /*
	   * Delete the old symbol table (if temporary)
	   */
	  unlink(sprintf(filename,"tmp:sym%d.stb",pid));
	  /*
	   * Rename the new symbol table so it is now the old symbol table
	   */
	  link(sprintf(symfil,"tmp:sym%d.new",pid),filename);
	  unlink(symfil);
	  sprintf(myname,"tmp:sym%d",pid);
	  stabf = myname;
	  /*
	   * Return  Lgetaddress(entry,function_name,discipline)
	   */
	  {
	     struct argent *oldlbot, *oldnp;
	     lispval result;

	     oldlbot = lbot;
	     oldnp = np;
	     lbot = np;
	     np++->val = matom(mlbot[1].val);
	     np++->val = mlbot[2].val;
	     np++->val = matom(mlbot[3].val);
	     result = Lgetaddress();
	     lbot = oldlbot;
	     np = oldnp;
	     return(result);
	  }
	}
#endif
}
#ifdef VMS
#define M 4
#else
#define M 1
#endif
#define oktox(n) \
	(0==stat(n,&stbuf)&&(stbuf.st_mode&S_IFMT)==S_IFREG&&0==access(n,M))
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
			if(oktox(Xargv[0])) {
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
			if(!oktox(myname)) continue;
			return(stabf = myname);
		}
		error("Could not find which file is being executed.",FALSE);
	} else return (stabf);
}
static char mybuff[40]; 
char *
mytemp()
{
	if(mypid==0) mypid = (getpid() & 0xffff);
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


/* extern	int fvirgin; */
			/* declared in ffasl.c tells if this is original
			 *	lisp symbol table.
			 * if fvirgin is 1 then we must copy the symbol
			 * 	table, else we can overwrite it, since
			 * 	it is a temporary file which only
			 *	one user could be using(was not created
			 *	as an original lisp or by a (dumplisp)
			 *	or a (savelisp)).
			 */

/* copy a block of data from one file to another of size size */
copyblock(f1,f2,size)
FILE *f1, *f2;
int size;
{
	char block[BUFSIZ];

	    while ( size > BUFSIZ ) {
		size -= BUFSIZ;
		fread(block,BUFSIZ,1,f1);
		fwrite(block,BUFSIZ,1,f2);
	    }
	    if (size > 0 ) {
		fread(block,size,1,f1);
		fwrite(block,size,1,f2);
	    }
}

/* removeaddress --
 *
 * (removeaddress '|_entry1| '|_entry2| ...)
 *
 * 	removes the given entry points from the run time symbol table,
 *		so that later cfasl'd files can have these label names.
 *
 */

lispval
Lrmadd(){
	register struct argent *mlbot = lbot;
	register struct nlist *p, *q; 
	register int i;
	register struct argent *lbot, *np;
	int numberofargs, strsize;
	char *gstab();
	char ostabf[128];
	char *nstabf,*mytemp();
	char *strtbl,*alloca();
	int i2, n, m, nargleft, savem;
	FILE *f, *fa;
	FILE *fnew;
	off_t savesymadd,symadd;		/* symbol address */
	struct exec buf;
	struct nlist nlbuf[BUFSIZ/sizeof (struct nlist)];
	int maxlen;
	int change;

	snpand(2);

	numberofargs = (np - lbot);
	maxlen = 0;
	for ( i=0; i<numberofargs; i++,mlbot ++) {
		mlbot->val = verify(mlbot->val,"Incorrect entry specification.");
		n = strlen((char *)mlbot->val);
		if (n > maxlen)
			maxlen = n;
	}
	/* 
	 *  Must not disturb object file if it an original file which
	 *	other users can execute(signified by the variable fvirgin).
	 *	so the entire symbol table is copied to a new file.
	 */
	if (fvirgin) {
		strcpyn(ostabf,gstab(),128);
		nstabf = mytemp();
		/*
		 * copy over symbol table into a temporary file first
		 *
		 */
		f = fopen(ostabf, "r");
		fnew = fopen(nstabf, "w");
		if (( f == NULL ) || (fnew == NULL)) return ( nil );
		/* read exec header on file */
		fread((char *)&buf, sizeof buf, 1, f);

		/* Is this a legitimate a.out file? */
		if (N_BADMAG(buf)) {
			unlink(nstabf);
			ungstab();
			fclose(f);
			fclose(fnew);
			errorh(Vermisc,"Removeaddress: Bad file",nil,FALSE,0,inewstr(ostabf));
			return(nil);
		}
		/* set pointer on read file to symbol table */
		/* must be done before the structure buf is reassigned 
		 * so that it will be accurate for the read file 
		 */
		fseek(f,N_SYMOFF(buf),0);
		/* reset up exec header structure for new file */
		buf.a_magic = OMAGIC;
		buf.a_text = 0;
		buf.a_data = 0;
		buf.a_bss = 0;
		buf.a_entry = 0;
		buf.a_trsize = 0;
		buf.a_drsize = 0;
		fwrite((char *)&buf,
		       sizeof buf,1,fnew); 	/* write out exec header */
		copyblock(f,fnew,buf.a_syms);	/* copy symbol table */
		fread((char *)&strsize,
		      sizeof (int),1,f); 	/* find size of string table */
		fwrite((char *)&strsize,
		      sizeof (int),1,fnew); 	/* find size of string table */
		strsize -= 4;
		strtbl = alloca(strsize);
		fread(strtbl,strsize,1,f);	/* read and save string table*/
		fwrite(strtbl,strsize,1,fnew);	/* copy out string table     */
		fclose(f);fclose(fnew);
	} else {
		nstabf = gstab();
	}

	/*
	 * now unset the external bits it the entry points specified.
	 */
	f = fopen(nstabf, "r");
	fa = fopen(nstabf, "a");
	if (( f == NULL ) || (fa == NULL)) {
		unlink(nstabf);
		ungstab();
		if (f != NULL ) fclose(f);
		if (fa != NULL ) fclose(fa);
		return ( nil );
	}

	/* read exec header on file */
	fread((char *)&buf, sizeof buf, 1, f);

	/* Is this a legitimate a.out file? */
	if (N_BADMAG(buf)) {
		if (fvirgin) {
			unlink(nstabf);
			ungstab();
		}
		fclose(f);
		fclose(fa);
		errorh(Vermisc,"Removeaddress: Bad file",nil,FALSE,0,inewstr(ostabf));
		return(nil);
	} else {
		symadd = N_SYMOFF(buf);
		/*
		 * read in string table if not done during copying
		 */
		if (fvirgin==0){
			fseek(f,N_STROFF(buf),0);
			fread((char *)&strsize,sizeof (int),1,f);
			strsize -= 4;
			strtbl = alloca(strsize);
			fread(strtbl,strsize,1,f);
		}
		n = buf.a_syms;
		fseek(f, symadd, 0);
		while (n) {
			m = sizeof (nlbuf);
			if (n < m)
				m = n;

			/* read next block of symbols from a.out file */
			fread((char *)nlbuf, m, 1, f);
			savem = m;
			savesymadd = symadd;
			symadd += m;
			n -= m;
			change = 0;

		/* compare block of symbols against list of entry point
		 *	names given, if a match occurs, clear the N_EXT bit
		 *	for that given symbol and signal a change.
		 */
			for (q = nlbuf; (m -= sizeof(struct nlist)) >= 0; q++) {

				if (q->n_un.n_strx == 0 || q->n_type & N_STAB
				       /* make sure it is external */
					    || ((q->n_type & N_EXT)==0))
					continue;
			for (mlbot=lbot,i2 = 0;i2<numberofargs;i2++,mlbot++) {
				i = 0;
				while (((char *)mlbot->val)[i]) {
					if (((char *)mlbot->val)[i] != 
					    strtbl[i-4+q->n_un.n_strx])
						goto cont;
					i++;
				}
				if (strtbl[q->n_un.n_strx+i-4])
					goto cont;
				change = 1;
				q->n_type &= ~N_EXT;
				break;
			cont:   ;
			}
		}
		if ( change ) {
			fseek(fa,savesymadd,0);
			fwrite((char *)nlbuf, savem, 1, fa);
		}
		if (--nargleft == 0)
			goto alldone;
		}
	}
alldone:
	fclose(f);
	fclose(fa);
	if(fvirgin)
		fvirgin = 0;
	stabf = nstabf;
	return (tatom);
}
