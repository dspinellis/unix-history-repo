/*
 * Flt40 - 11/40 floating point speed up assembly language massager
 *
 * Jeff Schriebman UCB
 *
 * This program takes a temporary file from the C compiler
 * and massages it by adding explicit subroutine calls to the floating
 * point interpreter to prevent traps to the system.  The resulting
 * code will run faster since the system overhead of interpretation
 * is eliminated.  Note that leading tabs are not allowed on input lines.
 */

char *ops[] {
	"cfcc\t",
	"setf\t",
	"setd\t",
	"seti\t",
	"setl\t",
	"clrf\t",
	"negf\t",
	"absf\t",
	"tstf\t",
	"movf\t",
	"movif\t",
	"movfi\t",
	"movof\t",
	"movfo\t",
	"movie\t",
	"movei\t",
	"addf\t",
	"subf\t",
	"mulf\t",
	"divf\t",
	"cmpf\t",
	"modf\t",
	"ldfps\t",
	"stfps\t",
	"stst\t"
};

int bufi[259];
int bufo[259];
char op[100];
char rest[100];
char label[100];
int first;

main(argc, argv)
char **argv;
{
	register prev;
	int fp1, fp2;
	char *file1, *file2;
	char buf[512];

	if (argc != 3)
		perror("FLT40 FILEI FILEO", "");
	file1 = *++argv;
	file2 = *++argv;
	if ((fp1 = fopen(file1, bufi)) < 0)
		perror("Can't open file", file1);
	if ((fp2 = fcreat(file2, bufo)) < 0)
		perror("Can't create", file2);
	prev = 0;
	while (getline(buf) > 0) {
		split(buf);
		if (match(op)) {
			if (first == 0) {
				outstr(".globl\tfpjsr\n");
				first = 1;
			}
			if (*label || prev==0) {
				prev = 1;
				outstr(label);
				outstr("jsr\tpc,fpjsr;");
				outstr(op);
				outstr(rest);
			} else
				outstr(buf);
		} else {
			prev = 0;
			outstr(buf);
		}
	}
	fflush(bufo);
	exit(0);
}

split(s1)
char *s1;
{
	register char *aptr, *bptr, *cptr;

	bptr = 0;
	for (aptr=s1; *aptr!='\0'; aptr++)
		if (*aptr == ':')
			bptr = aptr + 1;
	cptr = label;
	if (bptr)
		for (aptr=s1; aptr!=bptr; aptr++)
			*cptr++ = *aptr;
	else
		bptr = s1;
	*cptr = 0;
	cptr = op;
	for (aptr=bptr; *aptr!='\0'; aptr++)
		if (*aptr == '\t') {
			for (aptr=bptr; *aptr!='\t'; aptr++)
				*cptr++ = *aptr;
			*cptr++ = '\t';
			bptr = ++aptr;
			break;
		}
	*cptr = '\0';
	cptr = rest;
	for (aptr=bptr; *aptr!='\0'; aptr++)
		*cptr++ = *aptr;
	*cptr++ = '\0';
}

outstr(s1)
char *s1;
{
	register char *aptr, c;

	aptr = s1;
	while ((c = *aptr++) != '\0')
		putc(c, bufo);
}

match(s1)
{
	register i, j;

	j = s1;
	for (i=0; i<(sizeof ops)/2; i++)
		if (cmp(ops[i], j))
			return(1);
	return(0);
}
getline(s1)
char *s1;
{
	register char *aptr, c;

	aptr = s1;
	while ((c = getc(bufi)) > 0) {
		*aptr++ = c;
		if (c == '\n') {
			*aptr++ = '\0';
			return(1);
		}
	}
	return(0);
}

cmp(s1, s2)
char *s1, *s2;
{
	register char *aptr, *bptr;

	aptr = s1;
	bptr = s2;
	while (*aptr == *bptr++)
		if (*aptr++ == '\0')
			return(1);
	return(0);
}

perror(s1, s2)
{
	printf("%s %s\n", s1, s2);
	exit(1);
}
