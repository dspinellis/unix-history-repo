#include <errno.h>

main()
{

	write(1, 0xabcd, 1024);
	perror("write");
}
