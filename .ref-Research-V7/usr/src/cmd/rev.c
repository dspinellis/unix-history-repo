#include <stdio.h>

/* reverse lines of a file */

#define N 256
char line[N];
FILE *input;

main(argc,argv)
char **argv;
{
	register i,c;
	input = stdin;
	do {
		if(argc>1) {
			if((input=fopen(argv[1],"r"))==NULL) {
				fprintf(stderr,"rev: cannot open %s\n",
					argv[1]);
				exit(1);
			}
		}
		for(;;){
			for(i=0;i<N;i++) {
				line[i] = c = getc(input);
				switch(c) {
				case EOF:
					goto eof;
				default:
					continue;
				case '\n':
					break;
				}
				break;
			}
			while(--i>=0)
				putc(line[i],stdout);
			putc('\n',stdout);
		}
eof:
		fclose(input);
		argc--;
		argv++;
	} while(argc>1);
}
