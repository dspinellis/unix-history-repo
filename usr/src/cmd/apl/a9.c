#include "apl.h"

ex_dibm()
{
int	 inde, fsize;
char	 fname[128];
register i;
register char *a, *b;

	inde = topfix();
	a = fetch1();
	if(a->type!=CH) {
		if(a->size==0||a->size==1&&fuzz(*a->datap,0.0)==0) {
			push(newdat(DA,1,0));
			switch(inde) {
			    case 1:
				if(i=ifile)
					close(i);
				ifile = 0;
				return;
			    case 2:
			    case 3:
				if((i=ofile)&&i!=1)
					close(i);
				ofile = 1;
				return;
			    default:
				error("mibm D");
			}
		}
		error("mibm T");
	}
	if(a->rank!=1)
		error("dibm R");
	if(!(1<=a->size&&a->size<128))
		error("fnam L");
	fsize = a->size;
	b = a->datap;
	a = fname;
	for(i=0; i<fsize; ++i)
		*a++ = *b++;
	*a = '\0';
	push(newdat(DA,1,0));
	switch(inde) {
	    case 1:		/* Open for reading */
		if(i=ifile)
			close(i);
		if((i=open(fname,0))<0)
			goto badfile;
		ifile = i;
		return;
	    case 2:		/* Open for writing */
		if((i=ofile)&&i!=1)
			close(i);
		if((i=creat(fname,0666))<0)
			goto badfile;
		ofile = i;
		return;
	    case 3:		/* Open and append  */
		if((i=ofile)&&i!=1)
			close(i);
		if((i=open(fname,1))<0)
			if((i=creat(fname,0666))<0)
				goto badfile;
		lseek(i, 0, 2);
		ofile = i;
		return;
	    case 10: {

		int shellpid, oldsignal, termproc;

		oldsignal = signal(2, 1);
		if(!(shellpid=vfork()))
			execl(getenv("SHELL") ? getenv("SHELL") : "/bin/sh", "sh", "-c", fname, 0);
		else
			while((termproc=wait(&termproc))!=-1)
				if(termproc==shellpid)
					break;
		signal(2, oldsignal);
		return;
	    }
	    default:
		error("dibm unk");
	}
badfile:
	error("bad file");
}

ex_mibm()
{
	register *p;
	int t[6];

	switch(topfix()) {

	default:
		error("ib unk");

	case 1:
		sclr();
		datum = 0;
		break;

	case 20: /* time of day */
		time(t);
		p = t;
		goto tod;

	case 21: /* CPU time */
		times(t);
		t[3] = t[0];
		t[0] = 0;
		t[2] = 0;
		datum = ltod(t) + ltod(t+2);
		break;

	case 22: /* Ws free */		/* RH 24-Apr-78 UCSF */
	{
		struct	freeblk {
			unsigned size;
			struct freeblk *nxtblk;
		};

		extern	 int freelist[], sbrk();
		register struct freeblk *runthru = freelist;
		register unsigned int freesum = 0160000;

		freesum -= sbrk(0);
		while(runthru->nxtblk!=-1) {
			freesum += runthru->size;
			runthru = runthru->nxtblk;
		}
		datum = freesum + runthru->size;
	}
		break;

	case 24: /* starting time */
		p = stime;

	tod:
		p = localtime(p);
		datum = 60.*(p[0]+60.*(p[1]+60.*p[2]));
		break;

	case 25: /* date */
		time(t);
		p = t;
		goto dt;

	/*
	 * non standard I functions
	 */

	case 28: /* starting date */
		p = stime;

	dt:
		p = localtime(p);
		datum = p[5]+100.*(p[3]+100.*(p[4]+1));
		break;

	case 29: /* iorg */
		datum = thread.iorg;
		break;

	case 30: /* width */
		datum = thread.width;
		break;

	case 31: /* digits */
		datum = thread.digits;
		break;

	case 32:
	    {
		int	shellpid, oldsignal, termproc;

		oldsignal = signal(2, 1);
		if(!(shellpid=fork()))
			execl("/bin/csh","-",0);
		else
			while((termproc=wait(&termproc))!=-1)
				if(termproc==shellpid)
					break;
		signal(2, oldsignal);
		push(newdat(DA,1,0));
		return;
	    }	
	}
	p = newdat(DA, 0, 1);
	p->datap[0] = datum;
	push(p);
}
