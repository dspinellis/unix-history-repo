#include "uucp.h"
#include "sgtty.h"
#include "uucpdefs.h"
#include "signal.h"
#include ".secret"
#define SEP '\t'
#define MAXC 300

/*******
 * this program reads a file whose format is
 *	field1 SEP count field2
 *   and runs crypt on field2.  Count is either blank (read to newline)
 *   or SEP followed by a count byte.
 */

main (argc,argv)
char *argv[];
{
	char str[10];
	char tl[MAXC], tr[MAXC], enc[MAXC];
	int count, i, ln, clear;
	FILE *fp1, *fp2;
	struct sgttyb sb;
	extern intrEXIT();

	signal(SIGQUIT, intrEXIT);
	signal(SIGILL, intrEXIT);
	signal(SIGTRAP, intrEXIT);
	signal(SIGIOT, intrEXIT);
	signal(SIGEMT, intrEXIT);
	signal(SIGFPE, intrEXIT);
	signal(SIGBUS, intrEXIT);
	signal(SIGSEGV, intrEXIT);
	signal(SIGSYS, intrEXIT);
	if (argc < 3)
		exit(100);
	fp1 = fopen(argv[1], "r");
	fp2 = fopen(argv[2], "w");
	if (fp1 == NULL || fp2 == NULL)
		exit(120);
	gtty(0, &sb);
	sb.sg_flags &= ~ECHO;
	stty(0, &sb);
	printf("Passwd:");
	gets(str);
	sb.sg_flags |= ECHO;
	stty(0, &sb);
	strcpy(tl, secret);
	hdpw(str);
	for (ln = 0;; ln++) {
		if (gleft(tl, fp1) == 0) {
			fclose(fp1);
			fclose(fp2);
			exit(0);
		}
		ASSERT(strlen(tl) < MAXC, "LEFT PART LONG %s", tl);
		count = gright(tr, fp1, &clear);
		ASSERT(count < MAXC, "RIGHT LONG %s", tr);
		encrypt(tr, enc, ln, count);
		fprintf(fp2, "%s%c", tl, SEP);
		if (clear)
			fprintf(fp2, "%c%c", SEP, count);
		for(i = 0; i < count; i++)
			putc(enc[i], fp2);
		putc('\n', fp2);
	}
}


/***
 *	rmlock(p)	this is a dummy called in ASSERT
 */
rmlock(p)
char *p;
{
	return;
}
cleanup(code)
int code;
{
	exit(code);
}

intrEXIT() {_exit(77);}
