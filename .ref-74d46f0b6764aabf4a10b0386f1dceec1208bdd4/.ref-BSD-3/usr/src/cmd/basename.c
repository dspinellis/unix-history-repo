#include	"stdio.h"

main(argc, argv)
char **argv;
{
	register char *p1, *p2, *p3;

	if (argc < 2) {
		putchar('\n');
		exit(1);
	}
	p1 = argv[1];
	p2 = p1;
	while (*p1) {
		if (*p1++ == '/')
			p2 = p1;
	}
	if (argc>2) {
		for(p3=argv[2]; *p3; p3++) 
			;
		while(p1>p2 && p3>argv[2])
			if(*--p3 != *--p1)
				goto output;
		*p1 = '\0';
	}
output:
	puts(p2, stdout);
	exit(0);
}
