/*
 * except.h
 *
 * Definitions and macros for C exception mechanism
 *
 (c) Jeffrey Mogul	Stanford	18 February 1983
 */

#include <setjmp.h>

typedef struct _Except_buf_x {
	struct _Except_buf_x *Prev;	/* exception chain back-pointer */
	jmp_buf Environ;		/* saved environment */
	char *Message;			/* Human-readable cause */
	int Code;			/* Exception code */
} _Except_Buf;

extern _Except_Buf *_Except_Header;	/* global exception chain header */

/*
 * "syntax":
 *	DURING statement HANDLER statement END_HANDLER
 */

#define	_E_RESTORE	_Except_Header = Exception.Prev

#define	DURING {_Except_Buf Exception;\
		 Exception.Prev=_Except_Header;\
		 _Except_Header= &Exception;\
		 if (!setjmp(Exception.Environ)) {

#define	HANDLER	_E_RESTORE;} else

#define	END_HANDLER }

#define	E_RETURN(x) {_E_RESTORE; return(x);}

#define	E_RETURN_VOID {_E_RESTORE; return;}

#define	RERAISE	raise(Exception.Code, Exception.Message)

/*
 * Exception modes (combined with ||):
 */
#define	EX_MODE_REPORT	1	/* report uncaught errors on stderr */
#define	EX_MODE_ABORT	2	/* abort if uncaught error */

extern int ExceptMode;
