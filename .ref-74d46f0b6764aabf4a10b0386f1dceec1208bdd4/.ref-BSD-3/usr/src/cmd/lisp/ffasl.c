#include "global.h"
#include <a.out.h>
#define round(x,s) ((((x)-1) & ~((s)-1)) + (s))

char *stabf = 0;
int fvirgin = 1;

lispval
Lffasl(){
	register struct argent *mlbot = lbot;
	register lispval work;
	int fildes, totsize, readsize;
	lispval csegment();
	char *sbrk(), *currend, *tfile, cbuf[512], *mytemp(), *gstab();
	struct exec header;
	snpand(2);

	if(np - mlbot != 3 || TYPE(mlbot[1].val)!=ATOM)
		mlbot[1].val = error("Incorrect .o file specification",TRUE);
	if(np - mlbot != 3 || TYPE(mlbot[2].val)!=ATOM)
		mlbot[2].val = error("Incorrect entry specification for fasl"
					,TRUE);
	if(np - mlbot != 3 || TYPE(mlbot[3].val)!=ATOM || mlbot[3].val==nil)
		mlbot[3].val = error( "Bad associated atom name for fasl",TRUE);

	/*
	 * Invoke loader.
	 */
	currend = sbrk(0);
	tfile = mytemp();
	sprintf(cbuf,
		"nld -A %s -T %x -N %s -e %s -o %s",
		gstab(),
		currend,
		mlbot[1].val->pname,
		mlbot[2].val->pname,
		tfile);
	printf(cbuf); fflush(stdout);
	if(system(cbuf)!=0) {
		unlink(tfile);
		return(nil);
	}
	if(fvirgin)
		fvirgin = 0;
	else
		unlink(stabf);
	stabf = tfile;
	if((fildes = open(tfile,0))<0)
		return(nil);
	/*
	 * Read a.out header to find out how much room to
	 * allocate and attempt to do so.
	 */
	if(read(fildes,(char *)&header,sizeof(header)) <= 0) {
		close(fildes);
		return(nil);
	}
	readsize = header.a_text + header.a_data;
	totsize  = readsize + header.a_bss;
	totsize  = round(totsize,512);
	/*
	 * Fix up system indicators, typing info, etc.
	 */
	currend = (char *)csegment(int_name,totsize/4);
	
	if(readsize!=read(fildes,currend,readsize))
		return(nil);
	work = newfunct();
	work->entry = (lispval (*)())header.a_entry;
	work->discipline = lambda;
	return(mlbot[3].val->fnbnd = work);
}
#include "types.h"
#include <sys/stat.h>
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
		if(*cp==':') {
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
