#include	"stdio.h"
#include	"signal.h"
#include	"h00vars.h"
#include	"h01errs.h"

/*
 * Routine error is called from the interpreter when a runtime error occurs.
 * Its argument is the internal number of the error which occurred.
 * See Edata, Emake etc.
 */
error(errnum)
long	errnum;
{
register long i;
extern long errno;

signal(SIGINT,SIG_IGN);
signal(SIGSEGV,SIG_DFL);
signal(SIGFPE,SIG_DFL);
i = errno;
pflush();
errno = i;
fputs("\n\n",stderr);
switch (errnum) {
	case EINTR:
		break;
	case ECHR:
		fputs("Argument to chr out of range\n",stderr);
		break;
	case EASSIGN:
		fputs("Overflow during assignment conversion\n",stderr);
		break;
	case EFIXADD:
		fputs("Overflow in fixed point addition\n",stderr);
		break;
	case EFLTADD:
		fputs("Floating point addition out of range\n",stderr);
		break;
	case EFIXSUB:
		fputs("Overflow in fixed point subtraction\n",stderr);
		break;
	case EFLTSUB:
		fputs("Floating point subtraction out of range\n",stderr);
		break;
	case EFIXMUL:
		fputs("Overflow in fixed point multiplication\n",stderr);
		break;
	case EFLTMUL:
		fputs("Floating point multiplication out of range\n",stderr);
		break;
	case EFIXDIV:
		fputs("Fixed point division by zero\n",stderr);
		break;
	case EFLTDIV:
		fputs("Floating point division error\n",stderr);
		break;
	case EMODDIV:
		fputs("Fixed point modulo by zero\n",stderr);
		break;
	case EFIXNEG:
		fputs("Overflow in fixed point negation\n",stderr);
		break;
	case ESYSTEM:
		fputs("Panic: Computational error in interpreter\n",stderr);
		break;
	case EBUILTIN:
		fputs("Overflow in builtin function\n",stderr);
		break;
	case EHALT:
/*
		nodump = 0;
*/
		fputs("Call to procedure halt\n",stderr);
		break;
	case ENILPTR:
		fputs("Reference through a nil pointer\n",stderr);
		break;
	case EPASTEOF:
		fprintf(stderr,"%s: Tried to read past end of file\n",file);
		break;
	case EREADIT:
		fprintf(stderr,"%s: Attempt to read, but open for writing\n",file);
		break;
	case EWRITEIT:
		fprintf(stderr,"%s: Attempt to write, but open for reading\n",file);
		break;
	case ECLOSE:
		fprintf(stderr,"%s: Close failed\n",file);
		break;
	case ELLIMIT:
		fprintf(stderr,"%s: Line limit exceeded\n",file);
		break;
	case ESQRT:
		fputs("Negative argument to sqrt\n",stderr);
		break;
	case ESTKNEMP:
		fputs("Panic: stack not empty between statements\n",stderr);
		break;
	case ESUBSCR:
		fputs("Subscript out of range\n",stderr);
		break;
	case EREFINAF:
		fputs("Reference to an inactive file\n",stderr);
		break;
	case EWRITE:
		fputs("Could not write to ",stderr);
		perror(file);
		break;
	case EOPEN:
		fputs("Could not open ",stderr);
		perror(file);
		break;
	case ECREATE:
		fputs("Could not create ",stderr);
		perror(file);
		break;
	case EREMOVE:
		fputs("Could not remove ",stderr);
		perror(file);
		break;
	case ESEEK:
		fputs("Could not reset ",stderr);
		perror(file);
		break;
	case ENAMESIZE:
		fputs("File name too long\n",stderr);
		break;
	case ELN:
		fputs("Non-positive argument to ln\n",stderr);
		break;
	case EBADOP:
		fputs("Panic: bad op code\n",stderr);
		break;
	case EBADINUM:
		fputs("Bad data found on integer read\n",stderr);
		break;
	case EBADFNUM:
		fputs("Bad data found on real read\n",stderr);
		break;
	case EGOTO:
		fputs("Panic: active frame not found in goto\n",stderr);
		break;
	case ECASE:
		fputs("Label not found in case\n",stderr);
		break;
	case EOUTOFMEM:
		fputs("Ran out of memory\n",stderr);
		break;
	case ETRASHHEAP:
		fputs("Attempt to dispose of previously deallocated memory\n",stderr);
		break;
	case ECTTOT:
		fputs("Constructed set argument exceeds set bounds\n",stderr);
		break;
	case EARGV:
		fputs("Argument to argv out of range\n",stderr);
		break;
	case EPACK:
		fputs("Bad i to pack(a,i,z)\n",stderr);
		break;
	case EUNPACK:
		fputs("Bad i to unpack(z,a,i)\n",stderr);
		break;
	case ERANGE:
		fputs("Value out of range\n",stderr);
		break;
	case EASRT:
		fputs("Assertion failed\n",stderr);
		break;
	case ESTLIM:
		fputs("Statement count limit exceeded\n",stderr);
		break;
	case ESTKOVFLO:
		fputs("Runtime stack overflow\n",stderr);
		break;
	case EFRAMESIZE:
		fputs("Compiler restricts declarations to 32768 bytes per block\n",stderr);
		break;
	default:
		fputs("Panic: unknown error\n",stderr);
}
if (nodump == 0)
	backtrace(errnum);
psexit(errnum);
}
