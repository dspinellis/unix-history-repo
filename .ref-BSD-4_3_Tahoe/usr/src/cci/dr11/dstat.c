
#include <stdio.h>
#include <sys/types.h>
#include <sys/buf.h>
#include <sys/drreg.h>
#include <sys/ioctl.h>

struct dr11io drio;

/*
 *	usage: dstat [-l] [-t second] [unitno]
*/
main(argc,argv)
long argc;
char *argv[];
{	register long fd;
	long loop = 0;
	long cmd = 0, unit = 0;
	char *DR11 = "/dev/dr11";

	fd = open(DR11,2);
	if (fd == -1) {
		fprintf(stderr,"\ndstat: cannot open %s, ",DR11);
		perror("");
		exit(1);
	}
	if (argc > 1) { 
		if (argc > 5) {
			usage();
			exit(0);
		}
		for (argc--,argv++;argc;argc--, argv++) {
			if (argv[0][0]=='-')  {
				switch (argv[0][1]) {
					case 'l':
						cmd = 1;   /* lpback test */
						break;
					case 't':
						sscanf(argv[1],"%ld",&loop);
						argc--; argv++;
						break;
					default:
						usage();
						exit(0);
				}
			}
			else {
				sscanf(argv[0],"%ld",&unit);
			}
		}
		drio.arg[0] = unit;
	}
	if (cmd)
		lpback(fd,unit,loop);
	else
		stat(fd,unit,loop);
	close(fd);
}

stat(fd,unit,loop)
register long fd,unit,loop;
{	register long err;

again:
	drio.arg[0] = unit;
	err = ioctl(fd,DR11STAT,(caddr_t)&drio);
	if (err) {
		fprintf(stderr,"\ndstat: cannot ioctl unit %d",unit);
		perror(" ");
		exit(0);
	}
	prdrio(&drio,unit);
	if (loop > 0) {
		sleep(loop);
		goto again;
	}
}

lpback(fd,unit,loop)
register long fd,unit,loop;
{

lpagain:
	lptest(fd,unit);
	if (loop > 0) {
		sleep(loop);
		goto lpagain;
	}
}

lptest(fd,unit)
register long fd,unit;
{	register long err;

	drio.arg[0] = unit;
	err = ioctl(fd,DR11STAT,(caddr_t)&drio);
	if (err) {
		fprintf(stderr,"\ndstat: cannot ioctl unit %d",unit);
		perror(" ");
		exit(0);
	}
	prdrio(&drio,unit);

	printf("\n ---- Perform loopback test ----");
	drio.arg[0] = unit;
	ioctl(fd,DR11LOOP,(caddr_t)&drio);
	printf("\n Done....");

	drio.arg[0] = unit;
	ioctl(fd,DR11STAT,(caddr_t)&drio);
	prdrio(&drio,unit);
}


prdrio(drio,unit)
struct dr11io *drio;
long unit;
{
	printf("\n\t------------- DR11 unit %ld Status ------------",unit);
	printf("\n\t  dr_flags  CSR   istat  idata  modvec  dmacnt  hiadr  loadr");
	printf("\n\t    %04lx    %04lx  %04lx   %04lx   %04lx    %04ld    %04lx   %04lx",
		drio->arg[0],drio->arg[1],drio->arg[2],drio->arg[3] & 0xffff,
		drio->arg[4],drio->arg[5],drio->arg[6],drio->arg[7]);
	printf("\n");
}

usage()
{
	printf("\nusage: dstat [-l] [unit] [second]");
	printf("\n\t-l: do loopback test");
	printf("\n\tunit: controller no");
	printf("\n\tsecond: interval to perform command");
}
