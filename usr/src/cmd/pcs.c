#include <sys/param.h>
#include <sys/stat.h>
struct stat buf;

fault(a)
{	signal(a,fault);
}

main()
{
	int i; char ch;
	while (read(0,&ch,1)==1) write(1,&ch,1);
	printf("PID=%d\n",getpid());
	printf("signals\n");
	for(i=1;i<NSIG;i++) {
		printf("%d ",signal(i,1));
	}
	printf("\nfiles\n");
	for(i=0;i<NOFILE;i++) {
		printf("%c ",(fstat(i,&buf) != -1 ? 't' : '-'));
	}
	printf("\n");
}
