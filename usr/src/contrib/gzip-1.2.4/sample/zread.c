#include <stdio.h>

/* Trivial example of reading a gzip'ed file or gzip'ed standard input
 * using stdio functions fread(), getc(), etc... fseek() is not supported.
 * Modify according to your needs. You can easily construct the symmetric
 * zwrite program.
 *
 * Usage: zread [file[.gz]]
 * This programs assumes that gzip is somewhere in your path.
 */
int main(argc, argv)
    int argc;
    char **argv;
{
    FILE *infile;
    char cmd[256];
    char buf[BUFSIZ];
    int n;

    if (argc < 1 || argc > 2) {
	fprintf(stderr, "usage: %s [file[.gz]]\n", argv[0]);
	exit(1);
    }
    strcpy(cmd, "gzip -dc ");  /* use "gzip -c" for zwrite */
    if (argc == 2) {
	strncat(cmd, argv[1], sizeof(cmd)-strlen(cmd));
    }
    infile = popen(cmd, "r");  /* use "w" for zwrite */
    if (infile == NULL) {
	fprintf(stderr, "%s: popen('%s', 'r') failed\n", argv[0], cmd);
	exit(1);
    }
    /* Read one byte using getc: */
    n = getc(infile);
    if (n == EOF) {
	pclose(infile);
	exit(0);
    }
    putchar(n);

    /* Read the rest using fread: */
    for (;;) {
	n = fread(buf, 1, BUFSIZ, infile);
	if (n <= 0) break;
	fwrite(buf, 1, n, stdout);
    }
    if (pclose(infile) != 0) {
	fprintf(stderr, "%s: pclose failed\n", argv[0]);
	exit(1);
    }
    exit(0);
    return 0; /* just to make compiler happy */
}
