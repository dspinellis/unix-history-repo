/* time command */

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>

extern int errno;
extern char *sys_errlist[];

main(argc, argv)
char **argv;
{
	struct tms buffer, obuffer;
	int status;
	register p;
	time_t before, after;

	if(argc<=1)
		exit(0);
	time(&before);
	p = fork();
	if(p == -1) {
		fprintf(stderr, "Try again.\n");
		exit(1);
	}
	if(p == 0) {
		execvp(argv[1], &argv[1]);
		fprintf(stderr, "%s: %s\n", argv[1], sys_errlist[errno]);
		exit(1);
	}
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	times(&obuffer);
	while(wait(&status) != p)
		times(&obuffer);
	time(&after);
	if((status&0377) != 0)
		fprintf(stderr,"Command terminated abnormally.\n");
	times(&buffer);
	fprintf(stderr,"\n");
	printt("real", (after-before) * 60);
	printt("user", buffer.tms_cutime - obuffer.tms_cutime);
	printt("sys ", buffer.tms_cstime - obuffer.tms_cstime);
	exit(status>>8);
}

char quant[] = { 6, 10, 10, 6, 10, 6, 10, 10, 10 };
char *pad  = "000      ";
char *sep  = "\0\0.\0:\0:\0\0";
char *nsep = "\0\0.\0 \0 \0\0";

printt(s, a)
char *s;
long a;
{
	char digit[9];
	register i;
	char c;
	int nonzero;

	for(i=0; i<9; i++) {
		digit[i] = a % quant[i];
		a /= quant[i];
	}
	fprintf(stderr,s);
	nonzero = 0;
	while(--i>0) {
		c = digit[i]!=0 ? digit[i]+'0':
		    nonzero ? '0':
		    pad[i];
		fprintf(stderr,"%c",c);
		nonzero |= digit[i];
		c = nonzero?sep[i]:nsep[i];
		fprintf(stderr,"%c",c);
	}
	fprintf(stderr,"\n");
}
