/* From jrs@world.std.com (Rick Sladkey) */
#include <stdio.h>
main(argc, argv)
int argc;
char **argv;
{
    FILE *fp;
    char s[128];
    fp = popen("echo hello; echo goodbye", "r");
    if (fp == NULL) {
	printf("popen failed\n");
	exit(1);
    }
    while (fgets(s, 128, fp) != NULL)
	printf("s = %s", s);
    pclose(fp);
    fp = popen("tr '[a-z]' '[A-Z]'", "w");
    if (fp == NULL) {
	printf("popen (w) failed\n");
	exit(1);
    }
    fprintf(fp, "hello\n");
    fprintf(fp, "goodbye\n");
    pclose(fp);
    exit(0);
}
