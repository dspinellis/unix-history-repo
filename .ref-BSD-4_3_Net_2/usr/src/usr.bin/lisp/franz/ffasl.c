#ifndef lint
static char *rcsid =
   "$Header: ffasl.c,v 1.11 87/12/14 18:48:06 sklower Exp $";
#endif

/*					-[Mon Mar 21 19:37:21 1983 by jkf]-
 * 	ffasl.c				$Locker:  $
 * dynamically load C code
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include "global.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <aout.h>
#define round(x,s) ((((x)-1) & ~((s)-1)) + (s))

char *stabf = 0, *strcpy(), *Ilibdir();
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

		given = errorh1(Vermisc,messg,nil,TRUE,0,given);
	}
}

lispval
Lcfasl(){
	register struct argent *mlbot = lbot;
	register lispval work;
	register int fildes, totsize;
	int readsize;
	lispval csegment();
	char *sbrk(), *currend, *tfile, cbuf[6000], *mytemp(), *gstab();
	char ostabf[128];
	struct exec header;
	char *largs;
	Savestack(4);

	switch(np-lbot) {
	   case 3: protect(nil);	/* no discipline given */
	   case 4: protect(nil);        /* no library given  */
	}
	chkarg(5,"cfasl");
	mlbot[0].val = verify(mlbot[0].val,"Incorrect .o file specification");
	mlbot[1].val = verify(mlbot[1].val,"Incorrect entry specification for cfasl");
	mlbot[3].val = dispget(mlbot[3].val,"Incorrect discipline specification for cfasl",(lispval)Vsubrou->a.pname);
	while(TYPE(mlbot[2].val)!= ATOM) 
	mlbot[2].val = errorh1(Vermisc,"Bad associated atom name for fasl",
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
#if (!os_vms) | EUNICE_UNIX_OBJECT_FILE_CFASL
			/*** UNIX cfasl code ***/
	tfile = mytemp();
	sprintf(cbuf,
		"%s/nld -N -x -A %s -T %x %s -e %s -o %s %s -lc",
		Ilibdir(),
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
		Restorestack();
		return(nil);
	}
	if(fvirgin)
		fvirgin = 0;
	else
		unlink(ostabf);
	stabf = tfile;
	if((fildes = open(tfile,0))<0) {
		fprintf(stderr,"Couldn't open temporary file: %s\n",tfile);
		Restorestack();
		return(nil);
	}
	/*
	 * Read a.out header to find out how much room to
	 * allocate and attempt to do so.
	 */
	if(read(fildes,(char *)&header,sizeof(header)) <= 0) {
		close(fildes);
		Restorestack();
		return(nil);
	}
	readsize = round(header.a_text,4) + round(header.a_data,4);
	totsize  = readsize + header.a_bss;
	totsize  = round(totsize,512);
	/*
	 * Fix up system indicators, typing info, etc.
	 */
	currend = (char *)csegment(OTHER,totsize,FALSE);
	
	if(readsize!=read(fildes,currend,readsize))
		{close(fildes);Restorestack(); return(nil);}
	work = newfunct();
	work->bcd.start = (lispval (*)())header.a_entry;
	work->bcd.discipline = mlbot[3].val;
	close(fildes);
	Restorestack();
	return(mlbot[2].val->a.fnbnd = work);
#else
			/*** VMS cfasl code ***/
	{
	  int pid = getpid() & 0xffff;	/* Our process ID number */
	  char objfil[100];		/* Absolute object file name */
	  char symfil[100];		/* Old symbol table file */
	  char filename[100];		/* Random filename buffer */
	  int strlen();			/* String length function */
	  int cvt_unix_to_vms();	/* Convert UNIX to VMS filename */
	  lispval Lgetaddress(),matom();
	  struct stat stbuf;

	  if (largs == 0) largs = " ";
	  sprintf(objfil,"tmp:cfasl%d.tmp",pid);
	  symfil[cvt_unix_to_vms(ostabf,symfil)] = 0;
	  sprintf(cbuf,					/* Create link cmd. */
		"$ link/exe=%s/nom/syst=%%X%x/sym=tmp:sym%d.new %s,%s%s",
		objfil,
		currend,
		pid,
		mlbot[0].val,
		symfil,
		largs);
	  printf(					/* Echo link cmd. */
		"$ link/exe=%s/nomap/system=%%X%x/symbol_table=tmp:sym%d.new %s,%s%s\n",
		objfil,
		currend,
		pid,
		mlbot[0].val,
		symfil,
		largs);
	  fflush(stdout);
	  vms_system(cbuf,0);

	  if ((fildes = open(objfil,0)) < 0) /* Open abs file */
		{Restorestack(); return(nil);}
	  fstat(fildes,&stbuf);				/* Get its size */
	  readsize=stbuf.st_size;
	  currend = (char *)csegment(OTHER,readsize,FALSE);
	  readsize = read(fildes,currend,10000000);
	  close(fildes);
	  /*
	   * Delete the absolute object file
	   */
	  unlink(objfil);
	  /*
	   * Delete the old symbol table (if temporary)
	   */
	  sprintf(filename,"tmp:sym%d.stb",pid);
	  unlink(filename);
	  /*
	   * Rename the new symbol table so it is now the old symbol table
	   */
	  sprintf(symfil,"tmp:sym%d.new",pid);
	  link(symfil,filename);
	  unlink(symfil);
	  sprintf(myname,"tmp:sym%d.stb",pid);
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
#ifdef os_vms
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
#ifdef	os_vms
			/*
			 *	Try Xargv[0] with ".stb" concatenated
			 */
			strcpy(myname,Xargv[0]);
			strcat(myname,".stb");
			if (oktox(myname)) return(stabf = myname);
			/*
			 *	Try Xargv[0] with ".exe" concatenated
			 */
			strcpy(myname,Xargv[0]);
			strcat(myname,".exe");
			if (oktox(myname)) return(stabf = myname);
#endif
		}
		for(;*cp;) {

			/* copy over current directory
			   and then append argv[0] */

			for(cp2=myname;(*cp)!=0 && (*cp)!=':';)
				*cp2++ = *cp++;
			*cp2++ = '/';
			strcpy(cp2,Xargv[0]);
			if(*cp) cp++;
#ifndef	os_vms
			if(!oktox(myname)) continue;
#else
			/*
			 *	Also try ".stb" and ".exe" in VMS
			 */
			if(!oktox(myname)) {
				char *end_of_name;
				end_of_name = cp2 + strlen(cp2);
				strcat(cp2,".stb");
				if(!oktox(myname)) {
					/*
					 *	Try ".exe"
					 */
					*end_of_name = 0;   /* Kill ".stb" */
					strcat(cp2,".exe");
					if (!oktox(myname)) continue;
				}
			}
#endif
			return(stabf = myname);
		}
		/* one last try for dual systems */
		strcpy(myname,Xargv[0]);
		if(oktox(myname)) return(stabf = myname);
		error("Could not find which file is being executed.",FALSE);
		/* NOTREACHED */
	} else return (stabf);
}
static char mybuff[40]; 
char *
mytemp()
{
	/*if(mypid==0) mypid = (getpid() & 0xffff);
	  fails if you do a dumplisp after doing a
	  cfasl */
	sprintf(mybuff,"/tmp/Li%d.%d",(getpid() & 0xffff),seed++);
	return(mybuff);
}
ungstab()
{
	seed--;
	sprintf(mybuff,"/tmp/Li%d.%d",(getpid() & 0xffff),seed-1);
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
		in = errorh1(Vermisc,error,nil,TRUE,0,in);
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
long size;
{
	char block[BUFSIZ];

	    while ( size > BUFSIZ ) {
		size -= BUFSIZ;
		fread(block,BUFSIZ,1,f1);
		fwrite(block,BUFSIZ,1,f2);
	    }
	    if (size > 0 ) {
		fread(block,(int)size,1,f1);
		fwrite(block,(int)size,1,f2);
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
	register struct nlist *q; 
	register int i;
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
	Keepxs();

	numberofargs = (np - lbot);
	nargleft = numberofargs;
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
		strncpy(ostabf,gstab(),128);
		nstabf = mytemp();
		/*
		 * copy over symbol table into a temporary file first
		 *
		 */
		f = fopen(ostabf, "r");
		fnew = fopen(nstabf, "w");
		if (( f == NULL ) || (fnew == NULL)) {Freexs(); return( nil );}
		/* read exec header on file */
#ifndef	os_vms
		fread((char *)&buf, sizeof buf, 1, f);
#else	os_vms
		/*
		 *	Under VMS/EUNICE we have to try the 1st 512 byte
		 *	block and the 2nd 512 byte block (there may be
		 *	a VMS header in the 1st 512 bytes).
		 */
		get_aout_header(fileno(f),&buf);
#endif	os_vms

		/* Is this a legitimate a.out file? */
		if (N_BADMAG(buf)) {
			unlink(nstabf);
			ungstab();
			fclose(f);
			fclose(fnew);
			errorh1(Vermisc,"Removeaddress: Bad file",nil,FALSE,0,inewstr(ostabf));
			{Freexs(); return(nil);}
		}
		/* set pointer on read file to symbol table */
		/* must be done before the structure buf is reassigned 
		 * so that it will be accurate for the read file 
		 */
		fseek(f,(long)N_SYMOFF(buf),0);
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
		copyblock(f,fnew,(long)buf.a_syms); /* copy symbol table */
#if ! (os_unisoft | os_unix_ts)
		fread((char *)&strsize,
		      sizeof (int),1,f); 	/* find size of string table */
		fwrite((char *)&strsize,
		      sizeof (int),1,fnew); 	/* find size of string table */
		strsize -= 4;
		strtbl = alloca(strsize);
		fread(strtbl,strsize,1,f);	/* read and save string table*/
		fwrite(strtbl,strsize,1,fnew);	/* copy out string table     */
#endif
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
#ifndef	os_vms
	fread((char *)&buf, sizeof buf, 1, f);
#else	os_vms
	/*
	 *	Under VMS/EUNICE we have to try the 1st 512 byte
	 *	block and the 2nd 512 byte block (there may be
	 *	a VMS header in the 1st 512 bytes).
	 */
	get_aout_header(fileno(f),&buf);
#endif	os_vms

	/* Is this a legitimate a.out file? */
	if (N_BADMAG(buf)) {
		if (fvirgin) {
			unlink(nstabf);
			ungstab();
		}
		fclose(f);
		fclose(fa);
		errorh1(Vermisc,"Removeaddress: Bad file",nil,FALSE,0,inewstr(ostabf));
		{Freexs(); return(nil);}
	} else {
		symadd = N_SYMOFF(buf);
#if ! (os_unisoft | os_unix_ts)
		/*
		 * read in string table if not done during copying
		 */
		if (fvirgin==0){
			fseek(f,(long)N_STROFF(buf),0);
			fread((char *)&strsize,sizeof (int),1,f);
			strsize -= 4;
			strtbl = alloca(strsize);
			fread(strtbl,strsize,1,f);
		}
#endif
		n = buf.a_syms;
		fseek(f, (long)symadd, 0);
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

	       /* make sure it is external */
				if (
				    (q->n_type & N_EXT)==0
#if ! (os_unix_ts | os_unisoft)
				    || q->n_un.n_strx == 0 || q->n_type & N_STAB
#endif
				   )	continue;
			for (mlbot=lbot,i2 = 0;i2<numberofargs;i2++,mlbot++) {
#if ! (os_unix_ts | os_unisoft)
				if(strcmp((char *)mlbot->val,
					  strtbl+q->n_un.n_strx-4)!=0)
						continue;
#else
				if(strncmp((char *)mlbot->val,
					   q->n_name,8)!=0)
						continue;
#endif
				change = 1;
				q->n_type &= ~N_EXT;
				break;
			}
		}
		if ( change ) {
			fseek(fa,(long)savesymadd,0);
			fwrite((char *)nlbuf, savem, 1, fa);
			if (--nargleft == 0)
				goto alldone;
		}
		}
	}
alldone:
	fclose(f);
	fclose(fa);
	if(fvirgin)
		fvirgin = 0;
	stabf = nstabf;
	{Freexs(); return(tatom);}
}
char *
Ilibdir()
{
	register lispval handy;
tryagain:
	handy = Vlibdir->a.clb;
	switch(TYPE(handy)) {
	case ATOM:
		handy = (lispval) handy->a.pname;
	case STRNG:
		break;
	default:
		(void) error(
"cfasl or load: lisp-library-directory not bound to string or atom",
				TRUE);
		goto tryagain;
	}
	return((char *) handy);
}
