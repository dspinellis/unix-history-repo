#include "global.h"
#include "gc.h"
#include <sys/types.h>
#include <sys/vtimes.h>

/* this file  is temporary and will contain routines to meter 
   the garbage collector
*/

/* gcstat - temporary routine used to report on gc statistics.
   if this causes variables to be undefined,then it should be removed
*/

extern int beginsweep,gensymcounter;
int gcstat;
int markdpcount;
int gccount;
int conssame;
int consdiff;
int consnil;


/*
 *	gcstat  - initiate and record gc statistics
 * calls:
 *  	(gcstat 0) -- initiate gc statistics by creating gc.out
 *		      and writing header.
 *	(gcstat 1) -- finish off gc statistics file by writing typetable
 *		      and closing file.
 */
lispval
Lgcstat()
{
	register lispval handy,retval;
	int nbytes;
	struct gchead hhh;

	chkarg(1,"gcstat");

	if(TYPE(handy=lbot->val) != INT)
	{	error("gcstat: non integer arg ",FALSE);
	}

	switch(handy->i)
	{
	    case 0:  if((gcstat = creat("gc.out",0644)) < 0)
		       error("cant open gc.out",FALSE);
		     hhh.version = 5;
		     hhh.lowdata = (int)beginsweep;
		     printf("writing %d bytes \n",sizeof(hhh));
		     write(gcstat,&hhh,sizeof(hhh));
		     gccount = 0;
		     return(tatom);

	    case 1:
		     /* first write out the type table */ 
		     nbytes = 0;
		     /* 0 means type table follows */
		     printf("gc's %d, writing %d bytes \n",gccount,
				  sizeof(nbytes));
		     write(gcstat,&nbytes,sizeof(nbytes));
		     write(gcstat,&typetable[((int)beginsweep>>9)+1],
				nbytes = ((int)datalim - (int)beginsweep)>>9);
		     printf("writing %d bytes \n",nbytes+sizeof(nbytes));
		     write(gcstat,&nbytes,sizeof(nbytes));
		     close(gcstat);
		     gcstat = 0;
		     return(inewint(nbytes));
	    default:
		     error("Bad value to gcstat ",TRUE);
	}
}
extern char bitmapi[];		/* a bit of a lie it is really a double array*/
char *bitmapc = bitmapi;
struct vtimes premark,presweep,alldone;
/* called in the garbage collector after the bit maps have been made 
   only if gcstat is non zero */
gcdump()
{
	int nbytes, recsize;
	/* 16 bytes/page in the bitmap */
	nbytes = (((int) datalim - (int) beginsweep) >> 9) * 16;
	recsize = nbytes + 6*sizeof(int) + 3*sizeof(struct vtimes);
	write(gcstat,&recsize,sizeof(recsize)); /* whole record size */
	write(gcstat,&premark,sizeof(premark));
	write(gcstat,&presweep,sizeof(presweep));
	write(gcstat,&alldone,sizeof(alldone));
	write(gcstat,&gensymcounter,sizeof(int));
	write(gcstat,&conssame,sizeof(int));
	write(gcstat,&consdiff,sizeof(int));
	write(gcstat,&consnil,sizeof(int));
	write(gcstat,&markdpcount,sizeof(int));
	write(gcstat,&nbytes,sizeof(nbytes));	/* bit table size */
	write(gcstat,&bitmapc[((int)beginsweep>>9) * 16],nbytes);
	printf("gc: %d, written %d bytes\n",++gccount,nbytes);
}
