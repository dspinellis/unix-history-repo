static char Sccsid[] = "aj.c @(#)aj.c	1.2	10/1/82 Berkeley ";
#include "apl.h"
#include <signal.h>

#ifdef vax
#define	WSMESG "can't load pdp-11 workspace"
#else
#define	WSMESG "can't load vax workspace"
#endif


clear()
{
	register struct nlist *n;

	for(n=nlist; n->namep; n++) {
		n->use = 0;
		n->itemp = 0;
		n->namep = 0;
		}
		thread.iorg = 1;
		srand(thread.rl = 1);
		thread.width = 72;
		thread.fuzz = 1.0e-13;
		afreset();	/* release all dynamic memory */
		gsip = 0;	/* reset state indicator */
}

lsize(s)
char *s;
{
	register i;
	register char *p;

	i=1;
	p=s;
	while (*p++) i++;
	return(i);
}

isize(ip)
struct item *ip;
{
	register struct item *p;
	register i;

	p=ip;
	i = sizeof *p - (MRANK-p->rank)*SINT;
	if(p->type == DA)
		i += p->size*SDAT; else
	if(p->type == CH)
		i += p->size;
	return(i);
}

wsload(ffile)
{
	struct item *convrt();
	char name[NAMS];
	union uci iz;
	register i;
	register struct nlist *n;
	register struct item *p;
	char c;
	int dconv;
	struct {
		int word;
	};

	iz.i = 0;
/* Check for correct magic number */
	READF(ffile,&iz,sizeof iz);
	iz.i &= 0177777;                        /* Zap high bits */
	if((iz.i|1) != (MAGIC|1)){
barf:
		CLOSEF(ffile);
		if (((iz.i|1)^2) == (MAGIC|1))
			error(WSMESG);
		else
			error("bad ws file format");
	}
	if(iz.i > MAGIC){
		printf("single data converted to double\n");
		dconv = 2;
	} else if(iz.i < MAGIC){
		printf("double data converted to single\n");
		dconv = 1;
	} else
		dconv = 0;
	READF(ffile,&thread,sizeof thread);
	while(READF(ffile,&iz,sizeof iz) == sizeof iz){
		i = iz.cv[1];
/* read name of vbl or fn */
		READF(ffile,name,i);
		for(n=nlist; n->namep; n++)
			if(equal(name, n->namep)){
				erase(n);
				goto hokay;
			}
		n->namep = alloc(i);
		copy(CH,name,n->namep,i);
hokay:
		n->use = iz.cv[0];
		n->type = LV;
		switch(n->use) {
default:
			goto barf;

case DA:
			READF(ffile,&iz,sizeof iz);
			p=(struct item *)alloc(iz.i);
			READF(ffile,p,iz.i);
			p->datap = (data *)&p->dim[p->rank]; /*make absolute*/
				/*
				 * convert data type if neccessary
				 */
			n->itemp = convrt(dconv,p);
			continue;
case NF:
case MF:
case DF:
			n->itemp = 0;
			n->label = SEEKF(wfile, 0L, 2);
			do {
				if(READF(ffile,&c,1) != 1)
					error("wsload eof");
				WRITEF(wfile,&c,1);
			} while(c != 0);
		}
	}
	fdat(ffile);
	CLOSEF(ffile);
}

wssave(ffile)
{
	register struct nlist *n;

	nsave(ffile, 0);
	for(n=nlist; n->namep; n++)
		nsave(ffile, n);
	fdat(ffile);
	CLOSEF(ffile);
}

vsave(fd)
{
	register struct nlist *n;
	struct nlist *getnm();

	nsave(fd, 0);
	while(n = getnm())
		nsave(fd, n);
	fdat(fd);
	CLOSEF(fd);
}

nsave(ffile, an)
struct nlist *an;
{
	union uci iz;
	register struct nlist *n;
	register i;
	register struct item *p;
	char c;

	n = an;
	if(n == 0){
		iz.i = MAGIC;
		WRITEF(ffile,&iz,sizeof iz);
		WRITEF(ffile,&thread,sizeof thread);
		return(0);
	}

	if(n->use == 0 || (n->use == DA && n->itemp == 0))
		return(0);
	iz.cv[0] = n->use;
	iz.cv[1] = i = lsize(n->namep);
#ifdef vax
	iz.cv[2] = iz.cv[3] = 0;
#endif
	WRITEF(ffile,&iz,sizeof iz);
	WRITEF(ffile,n->namep,i);

	switch(n->use) {
default:
		CLOSEF(ffile);
		error("save B");
case DA:
		p = n->itemp;
		iz.i = i = isize(p);
		((struct nlist *)p)->label -= (int)p;
		WRITEF(ffile,&iz,sizeof iz);
		WRITEF(ffile,p,i);
		((struct nlist *)p)->label += (int)p;
		break;
case NF:
case MF:
case DF:
		SEEKF(wfile,(long)n->label,0);
		do {
			READF(wfile,&c,1);
			WRITEF(ffile,&c,1);
		} while(c != 0);
	}
	return(0);
}

struct nlist *
getnm()
{
	char name[100];
	register char *p;
	register struct nlist *n;
	register c;

	while(1){
		printf("variable name? ");
		c = READF(1, name, 100);
		if(c <= 1)
			return(0);
		name[c-1] = 0;
		for(n=nlist; n->namep; n++)
			if(equal(name, n->namep))
				return(n);
		printf("%s does not exist\n", name);
	}
}

#ifdef NDIR
listdir()
{
	register pid, i;
	register int (*oldint)();

	/* I am not AT ALL happy with the change in the directory
	 * format.  Until it settles down in an official 4.2BSD
	 * distribution, just bail out and call "ls".  This solution
	 * doesn't work properly with ")script" files, but eventually
	 * I hope to make it internal again.
	 *			--John Bruner (06-May-82)
	 */

	oldint = signal(SIGINT, SIG_IGN);
	while ((pid=FORKF(1)) < 0)
		sleep(5);
	if (!pid) {
		signal(SIGINT, SIG_DFL);
		execl("/usr/ucb/ls", "ls", 0);	/* for column output */
		execl("/bin/ls", "ls", 0);	/* last resort */
		write(2, "Can't find \"ls\"!\n", 17);
		exit(1);
	}
	while ((i=wait(0)) > 0 && i != pid);
	signal(SIGINT, oldint);
}
#else
listdir()
{
	register f;
	register char *p;
	struct direct dir;

	/* List the directory in columnar format. */

	if((f = OPENF(".",0)) < 0)
		error("directory B");
	while(READF(f,&dir,sizeof dir) == sizeof dir)
		if(dir.d_ino != 0 && dir.d_name[0] != '.') {
			if(column+10 >= thread.width)
				printf("\n\t");
			for(p=dir.d_name; p<dir.d_name+14 && *p; p++)
				putchar(*p);
			putchar('\t');
		}
	putchar('\n');
	CLOSEF(f);
}
#endif

fdat(f)
{
	struct stat b;
	register struct tm *p;
	struct tm *localtime();

	FSTATF(f,&b);
	p = localtime(&b.st_mtime);

	printf("  ");
	pr2d(p->tm_hour);
	putchar('.');
	pr2d(p->tm_min);
	putchar('.');
	pr2d(p->tm_sec);
	putchar(' ');
	pr2d(p->tm_mon+1);
	putchar('/');
	pr2d(p->tm_mday);
	putchar('/');
	pr2d(p->tm_year);
}

pr2d(i)
{
	putchar(i/10+'0');
	putchar(i % 10 + '0');
}

struct item *
convrt(m, p)
struct item *p;
{
	register i;
	register float *f;
	register double *d;
	struct item *q;

	if (p->type == CH) return(p);
	switch(m){
	case 0:
		return(p);

	case 1:		/* apl to apl2 */
		q = newdat(DA, p->rank, p->size);
		f = (float *)q->datap;
		d = (double *)p->datap;
		for(i=0; i<p->size; i++)
			*f++ = *d++;
		break;

	case 2:		/* apl2 to apl */
		q = newdat(DA, p->rank, p->size);
		f = (float *)p->datap;
		d = (double *)q->datap;
		for(i=0; i<p->size; i++)
			*d++ = *f++;
		break;
	}
	for(i=0; i<p->rank; i++)
		q->dim[i] = p->dim[i];
	free(p);
	return(q);
}
