/* (c) 1979 Regents of the University of California */
#include "0x.h"
#include "opcode.h"
#include "E.h"

extern	int errno;

/*
 * Routine error is called from onemt when a runtime error occurs.
 * Its argument is the internal number of the error which occurred.
 * See Edata, Emake etc.
 */
error(perrno)
	int perrno;
{
	register int *ap, i;
	register char *cp;
	char *sep;
	int disp[20], *mydp;
	extern long stcnt;

	i = errno;
	pmflush();
	pmessage();
	errno = i;
	switch (perrno) {
		case EINTR:
			break;
		case ECHR:
			puts("Argument to chr out of range\n");
			break;
		case EDIVCHK:
			puts("Div by zero\n");
			break;
		case EFDIVCHK:
			puts("Floating divide by zero\n");
			break;
		case EFPOVFLO:
			puts("Floating point overflow\n");
			break;
		case EHALT:
/*
			nodump = 0;
*/
			puts("Call to procedure halt\n");
			break;
		case ENILPTR:
			puts("Reference through a nil pointer\n");
			break;
		case EPASTEOF:
			ferror("Tried to read past eof");
			break;
		case EREADIT:
			ferror("Attempt to read, but open for writing");
			break;
		case EWRITEIT:
			ferror("Attempt to write, but open for reading");
			break;
		case ETOODIGITS:
			puts("Too many digits in number\n");
			break;
		case ESQRT:
			puts("Negative argument to sqrt\n");
			break;
		case ESTKNEMP:
			puts("Panic: stack not empty between statements\n");
			break;
		case ESUBSCR:
			puts("Subscript out of range\n");
			break;
		case EREFINAF:
			puts("Reference to an inactive file\n");
			break;
		case EWRITE:
		case EOPEN:
		case ECREATE:
		case EREMOVE:
		case ESEEK:
			perror(file);
			break;
		case ELN:
			puts("Non-positive argument to ln\n");
			break;
		case EBADOP:
			puts("Panic: bad op code\n");
			break;
		case EBADINUM:
			puts("Bad data found on integer read\n");
			break;
		case EBADFNUM:
			puts("Bad data found on real read\n");
			break;
		case EGOTO:
			puts("Panic: active frame not found in goto\n");
			break;
		case ECASE:
			puts("Label not found in case\n");
			break;
		case EOUTOFMEM:
			puts("Ran out of memory\n");
			break;
		case EALLOC:
			puts("Panic: bad arg to alloc\n");
			break;
		case ECTTOT:
			puts("Constructed set argument exceeds set bounds\n");
			break;
		case EMODCHK:
			puts("Mod by zero\n");
			break;
		case ECLOSE:
			ferror("Close failed?");
			break;
		case EARGV:
			puts("Argument to argv out of range\n");
			break;
		case EPACK:
			puts("Bad i to pack(a,i,z)\n");
			break;
		case EUNPACK:
			puts("Bad i to unpack(z,a,i)\n");
			break;
		case ERANGE:
			puts("Value out of range\n");
			break;
		case EASRT:
			puts("Assertion failed\n");
			break;
		case EBIGGIE:
			ferror("Integer number too large");
			break;
		case ESTLIM:
			puts("Statement count limit exceeded\n");
			break;
		case ESTKOVFLO:
			puts("Runtime stack overflow\n");
			break;
		default:
			puts("Panic: unknown error\n");
	}
	if (nodump == 0) {
		for (i = 0; i < 20; i++)
			disp[i] = display[i];
		mydp = dp;
		sep = perrno == EINTR ? "\n\tInterrupted at " : "\n\tError at ";
		if (lino <= 0)
			exit(perrno);
		for(;;) {
			puts(sep);
			sep = "\tCalled by ";
			pputch('"');
			ap = *mydp;
			ap =+ 3;
			cp = *ap++;
			i = 8;
			do
				pputch(*cp++);
			while (--i && *cp != ' ');
			cp =+ i;
			puts("\"+");
			pwrite(O_WRIT2, 2, lino-cp->pint, 0);
			puts(" near line ");
			pwrite(O_WRIT2, 2, lino, 0);
			pputch('\n');
			*mydp = *ap++;
			if (mydp <= &display[1]) {
				for (i = 0; i < 20; i++)
					display[i] = disp[i];
				pmflush();
				puts("\n");
				if (discard)
					puts("Execution terminated abnormally\n");
				stmts();
				exit(perrno);
			}
			mydp = *ap++;
			ap++;
			lino = *ap++;
			if (lino <= 0)
				break;
		}
	}
	exit(perrno);
}

stmts()
{
	extern long stcnt;

	pwrite(O_WRIT4, 2, stcnt, 0);
	puts(" statement");
	if (stcnt != 1)
		puts("s");
	puts(" executed in");
	stmttime();
}

stmttime()
{
	struct {
		int utime, stime;
		long cutime, cstime; 
	} tbuf;
	long l;

	times(&tbuf);
	l = tbuf.utime;
	pwrite(O_WRIT82, (2 << 3) | 2, l / HZ + 0.005, 0, 2);
	puts(" seconds cpu time\n");
}

psexit()
{

	if (nodump == 1)
		exit(0);
	pmessage();
	if (discard)
		puts("Execution terminated\n");
	stmts();
	exit(0);
}
