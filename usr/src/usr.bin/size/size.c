static char *sccsid = "@(#)size.c	4.1 (Berkeley) %G%";
/*
 * size
 */
#include	<stdio.h>
#include 	<a.out.h>

main(argc, argv)
char **argv;
{
	struct exec buf;
	long sum;
	int gorp,i;
	FILE *f;

	if (argc==1) {
		*argv = "a.out";
		argc++;
		--argv;
	}
	gorp = argc;
	while(--argc) {
		++argv;
		if ((f = fopen(*argv, "r"))==NULL) {
			printf("size: %s not found\n", *argv);
			continue;
		}
		fread((char *)&buf, sizeof(buf), 1, f);
		if (N_BADMAG(buf)) {
			printf("size: %s not an object file\n", *argv);
			fclose(f);
			continue;
		}
		if (gorp>2)
			printf("%s: ", *argv);
		printf("%u+%u+%u = ", buf.a_text,buf.a_data,buf.a_bss);
		sum = (long) buf.a_text + (long) buf.a_data + (long) buf.a_bss;
		printf("%Db = 0x%Xb\n", sum, sum);
		fclose(f);
	}
}
