
#include <stdio.h>
#include <sys/types.h>
#include <sys/buf.h>
#include <sys/drreg.h>
#include <sys/ioctl.h>

struct dr11io drio;

/*
*	DR11 loop back test
*	usage: loop [unit]
*/
main(argc,argv)
long argc;
char *argv[];
{	long fd;
	char *DR11 = "/dev/dr11";
	long unit = 0;

	fd = open(DR11,2);
	if (fd == -1) {
		fprintf(stderr,"\nloop: cannot open %s",DR11);
		perror(" : ");
		exit(1);
	}
	if (argc > 1)
		sscanf(argv[1],"%ld",&unit);
	drio.arg[0] = unit;	/* Unit no. */
	ioctl(fd,DR11STAT,(caddr_t)&drio);
	prdrio(&drio,unit);

	printf("\n ---- Perform loopback test ----");
	drio.arg[0] = unit;	/* Unit 0 */
	ioctl(fd,DR11LOOP,(caddr_t)&drio);
	printf("\n Done....");

	drio.arg[0] = unit;
	ioctl(fd,DR11STAT,(caddr_t)&drio);
	prdrio(&drio,unit);

	close(fd);
}


prdrio(drio,unit)
struct dr11io *drio;
{
	printf("\n\t------------- DR11 unit %ld Status ------------",unit);
	printf("\n\t  dr_flags  CSR   istat  idata  modvec  dmacnt  hiadr  loadr");
	printf("\n\t    %04lx    %04lx  %04lx   %04lx   %04lx    %04ld    %04lx   %04lx",
		drio->arg[0],drio->arg[1],drio->arg[2],drio->arg[3] & 0xffff,
		drio->arg[4],drio->arg[5],drio->arg[6],drio->arg[7]);
	printf("\n");
}

