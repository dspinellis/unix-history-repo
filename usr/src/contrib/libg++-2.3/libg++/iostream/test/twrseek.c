/*

When using the gcc 2.1 release marked 3/26, I have problems using
fseek() and fwrite() together.  This program (which I typed from
memory, so it may not be perfect) prints "2":
*/


#include <stdio.h>
main()
{
	FILE * fp = fopen("FOO","w+");
	printf("fseek:100:%d\n", fseek(fp,100,0));
	fwrite("a",1,1,fp);
	printf("write 1:%d\n",ftell(fp));
	printf("fseek:200:%d\n", fseek(fp,200,0));
	fwrite("a",1,1,fp);
	printf("write 1:%d\n",ftell(fp));
	printf("fseek:100:%d\n", fseek(fp,100,0));
	printf("fgetc:%d\n", fgetc(fp));
	printf("fseek:150:%d\n", fseek(fp,150,0));
	printf("fgetc:%d\n", fgetc(fp));
	return 0;
}

/*
The program does reasonable things when I use write() instead.
I can give a more concise bug report later, if you need it.

BTW, thanks for the info on the binutils. 

-Eric

*/
