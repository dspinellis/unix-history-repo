/*	@(#)pmaptst.c	1.2 90/01/03 NFS Rev 2 Testsuite
 *	1.3 Lachman ONC Test Suite source
 */
#include <sys/types.h>
#include <netinet/in.h>

#define RPCINFO "rpcinfo -p"

#ifdef SVR3
#define PROG    ((u_long)432123)
#define VERS	((u_long)4)
#else
#define PROG    ((ulong)432123)
#define VERS	((ulong)4)
#endif
#define UPORT    2345
#define TPORT    2346

extern int pmap_set();
extern int pmap_unset();

main(argc, argv)
char *argv[];
{
	int ret, errs = 0;

	printf("portmapper set/unset test.\n");
	printf("rpcinfo before pmap_set:\n");
	system(RPCINFO);

	printf("\n--- Registering udp program %d version %d port %d...  ",
		PROG, VERS, UPORT);
	if (pmap_set(PROG, VERS, IPPROTO_UDP, UPORT))
		printf("done.\n");
	else {
		printf("failed.\n");
		errs++;
	}
	printf("rpcinfo after udp pmap_set:\n");
	system(RPCINFO);

	printf("\n--- Registering tcp program %d version %d port %d...  ",
		PROG, VERS, TPORT);
	if (pmap_set(PROG, VERS, IPPROTO_TCP, TPORT))
		printf("done.\n");
	else {
		printf("failed.\n");
		errs++;
	}
	printf("rpcinfo after tcp pmap_set:\n");
	system(RPCINFO);

	printf("\n--- Unregistering program %d version %d... ", PROG, VERS);
	if (pmap_unset(PROG, VERS))
		printf("done.\n");
	else {
		printf("failed.\n");
		errs++;
	}
	printf("rpcinfo after pmap_unset:\n");
	system(RPCINFO);
	if (!errs)
		printf("Test complete ok\n");
	exit(errs);
}
