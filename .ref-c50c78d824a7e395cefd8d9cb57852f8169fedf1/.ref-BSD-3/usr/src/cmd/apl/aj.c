#include "apl.h"

clear()
{
	register struct nlist *n;
	extern freelist[];

	for(n=nlist; n->namep; n++) {
		n->itemp = 0;
		n->use = 0;
		n->namep = 0;
		}
		freelist[0] = 0;
		freelist[1] = -1;	/*empty free list*/
		/* brk(memstart);	/*shrink core*/
		stack = sp = 0;
		Reset();
}

lsize(s)
char *s;
{
	register short i;
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
	i=sizeof *p + p->rank*SINT;
	if(p->type == DA)
		i += p->size*SDAT; else
	if(p->type == CH)
		i += p->size;
	return(i);
}

wsload(ffile)
{
	long b [ 256 ];
	int	ibuf, obuf;
	char name[NAMS];
	short  iz;
	long	vmagic[1];
	register i;
	register struct nlist *n;
	register struct item *p;
	char ch[1];

	iz = 0;
	ibuf = ffile;
	/* ibuf[1] = ibuf[2] = 0;*/
/* Check for correct magic number */
	read(ibuf,vmagic,4);
	if(vmagic[0] != (long)MAGIC) {
		close(ffile);
		error("not a ws");
	}
	read(ibuf,&thread,sizeof thread);
	while(read(ibuf,&iz,2) == 2) {
		i = iz.c[1];
/* read name of vbl or fn */
		read(ibuf,name,i);
		for(n=nlist; n->namep; n++)
			if(equal(name,n->namep)) {
				erase(n);
				goto hokay;
			}
		n->namep = alloc(i);
		copy(CH,name,n->namep,i);
hokay:
		n->use = iz.c[0];
		n->type = LV;
		switch(n->use) {
		    default:
			close(ffile);
			error("can not load ws");
		    case DA:
			read(ibuf,&iz,2);
			p=alloc(iz);
			read(ibuf,p,iz);
			p->label +=  ((int)p);/*make absolute*/
			n->itemp = p;
			continue;
		    case NF:
		    case MF:
		    case DF:
			n->itemp = 0;
			fstat(obuf=wfile,b);
			/*obuf[1] = obuf[2] = 0;*/
			n->label=b[4];
			lseek(wfile,b[4],0);
			do {
				read(ibuf,ch,1);
				write(obuf,ch,1);
			} while(ch[0] != '\0');
			fflush(obuf);
		}
	}
	fdat(ffile);
	close(ffile);
}

wssave(ffile)
{	long	vmagic[1];
	short   iz;
	int  jfile,ibuf, obuf;
	register struct nlist *n;
	register i;
	register struct item *p;
	char c[1];

	ibuf = jfile = dup(wfile);
	obuf = ffile;
	/* obuf[1] = obuf[2] = 0; */
	vmagic[0] = (long)MAGIC;
	write(obuf,vmagic,4);
	/*putw(vmagic, obuf); */
	write(obuf,&thread,sizeof thread);

	for(n=nlist; n->namep; n++) {
		if(n->use == 0 || (n->use == DA && n->itemp == 0))
			continue;
		iz.c[0] = n->use;
		i= lsize(n->namep);
		iz.c[1] = (char)i ;
		write(obuf,&iz,2);
		write(obuf,n->namep,i);

		switch(n->use) {
		    default:
			close(ffile);
			close(jfile);
			error("save B");
		    case DA:
			p = n->itemp;
			i = isize(p);
			iz = (short) i;
			p -> label -= ((int)p);
			write(obuf,&iz,2);
			write(obuf,p,i);
			p->label += ((int)p);
			continue;
		    case NF:
		    case MF:
		    case DF:
			lseek(ibuf,n->label,0);
			/* ibuf[1] = ibuf[2] = 0; */
			do {    
				read(ibuf,c,1);
				write(obuf,c,1);
			} while (c[0] != '\0') ;
		}
	}
	fflush(obuf);
	fdat(ffile);
	close(ffile);
	close(jfile);
};

listdir()
{
	register f;
	struct {
	short  in;
	char nam[14];
	} dir;

	if((f = open(".",0)) < 0)
		error("directory B");
	while(read(f,&dir,sizeof dir) == sizeof dir)
		if(dir.in != 0 && dir.nam[0] != '.') {
			if(column+10 >= thread.width){
				aprintf("\n\t");
			}
			aprintf(dir.nam);
			aputchar('\t');
		}
	aputchar('\n');
	close(f);
}

fdat(f)
{
	long b [ 256 ] ;
	register long *p;
	fstat(f,b);
	p = localtime(&b[7]);
	aprintf("  ");
	pr2d(p[2]);
	aputchar('.');
	pr2d(p[1]);
	aputchar('.');
	pr2d(p[0]);
	aputchar(' ');
	pr2d(p[4]+1);
	aputchar('/');
	pr2d(p[3]);
	aputchar('/');
	pr2d(p[5]);
}

pr2d(i)
long i;
{
	aputchar(i/10+'0');
	aputchar(i % 10 + '0');
}
