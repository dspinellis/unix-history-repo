/*
 * a test program for exception handling - cc test.c -lexcept
 */

#include <stdio.h>
#include <except.h>

#define	EX_EOF	-2
#define	EX_ZERO	-3

main()
{
	ExceptMode = EX_MODE_REPORT | EX_MODE_ABORT;

	printf("With handler\n");
	DURING
	    foo(3);
	HANDLER
	    switch (Exception.Code) {
	    	case EX_EOF:
			printf("End of input file\n");
			break;

		default:
			RERAISE;
			break;
	    }
	END_HANDLER;
	
	printf("Without handler\n");
	foo(3);
}

foo(i)
int i;
{
	char line[64];
	int x;

	while (--i) {
	    printf("Enter integer> ");
	    clearerr(stdin);
	    if (gets(line) == NULL) raise(EX_EOF, "End of File");
	    sscanf(line,"%d", &x);
	    DURING
	    	bar(x);
	    HANDLER
	       	switch (Exception.Code) {
	       	    case EX_ZERO:
		    	printf("%s\n", Exception.Message);
			E_RETURN_VOID;
		    default: RERAISE; break;
	      	}	    	
	    END_HANDLER
	}
}

bar(x)
int x;
{
	if (x == 0) raise(EX_ZERO, "Division by Zero");
	
	printf("1/%d = %f\n",x,1.0/(x+0.0));
}
