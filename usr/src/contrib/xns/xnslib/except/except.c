/*
 * except.c
 *
 * Support routines for C exceptions
 *
 (c) Jeffrey Mogul	Stanford	18 February 1983
 */

#include <stdio.h>
#include "except.h"

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];

_Except_Buf *_Except_Header = 0;
int ExceptMode = 0;

raise(code, msg)
int code;
char *msg;
{
	register _Except_Buf *EBp = _Except_Header;
	
	if (EBp == 0) {	/* uncaught exception */
	    if (ExceptMode&EX_MODE_REPORT) {
	    	fprintf(stderr,"Uncaught exception: %d, %s\n",
			code, msg);
	    }
	    if (ExceptMode&EX_MODE_ABORT)
		abort();
	    else
		exit(code);
	}

	EBp->Code = code;
	EBp->Message = msg;
	
	_Except_Header = EBp->Prev;
	
	longjmp(EBp->Environ, 1);
}

raise_sys()
{
	register int errnum = errno;

	if ((errnum < 1) || (errnum >= sys_nerr)) {
	    raise(-1, "Unknown Unix error code");
	}
	else {
	    raise(errnum, sys_errlist[errnum]);
	}
}
