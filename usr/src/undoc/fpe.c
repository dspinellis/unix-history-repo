From research!ikeya!rob  Sun Sep 13 17:04:21 1981
/*
 * This program prints an infinite number of fpe's,
 * because the pc is not advanced on FPE's.
 * 11/750 microcode bug.
 * On 11/780, die from Operand fault, which is
 * correct.
 * Please try it on your 750 and tell me what happens.
*/
#include <signal.h>
inter(){
	printf("fpe\n");
	signal(SIGFPE, inter);
	return;
}
main(){
	double x=0.0;
	signal(SIGFPE, inter);
	printf("%f\n", 3.4/x);
}


