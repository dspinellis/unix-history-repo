# 1 "write.c"



# 1 "./SYS.h"



# 1 "/usr/include/syscall.h"








				
				




				



				
				



				
				

				
				
				
				
				
				
				
				

				
				



				



				

				
				

				
				
				

				
				


				






				


				
				
				



				



				
				







				

















				











				



				






























# 4 "./SYS.h"




# 10 "./SYS.h"





	.globl	cerror
# 4 "write.c"

err: jmp cerror;
	.globl _foobar;
	.align 2;
_foobar:
	.word 0;
	.data;
1:;
	.long 0;
	.text;
	moval 1b,r0;
	jsb mcount;
	chmk $4;
	jcs err
	ret
