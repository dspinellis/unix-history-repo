
/*
 *
 *	This program changes a unix textfile to a VMS text file.
 *
 *	$Locker:  $
 *
 */
#include <stdio.h>

static char *rcsid = "$Header: /na/franz/franz/vax/RCS/totxtfile.c,v 1.1 83/04/11 00:31:07 sklower Exp $";

main(argc,argv)
char *argv[];
{
	int fd;
	register FILE *f, *in;
	register c;
	/*
	 *	Open the input file
	 */
	if(argc==3) {
		if(NULL==(in = fopen(argv[1],"r"))) {
			fprintf(stderr,
				"Couldn't open %s for reading\n",argv[1]);
			exit(2);
		}
		argc--; argv++;
	} else {
		in = fdopen(0,"r");
	}
	if(argc!=2) {
		fprintf(stderr,"Usage: totxtfile <unixfile> <vmsfile>\n");
		exit(1);
	}
	/*
	 *	Open the .txt file
	 */
	if ((fd = creat(argv[1],0777,"txt")) < 0) {
		fprintf(stderr,"Couldn't open %s for writing\n",argv[1]);
		exit(3);
	}
	f = fdopen(fd,"w");
	/*
	 *	Do the copy
	 */
	for(;;) {
		c = getc(in);
		if(c==EOF) break;
		putc(c,f);
	}
	/*
	 *	Close the file
	 */
	fclose(f);
	/*
	 *	Done
	 */
	exit(0);
}
