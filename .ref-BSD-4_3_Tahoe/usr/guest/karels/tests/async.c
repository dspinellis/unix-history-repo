#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/file.h>

int old_fl;

interrupt_handler(sig)
	int sig;
{
	char buf[BUFSIZ];
	int n;
	extern errno;

	switch (n = read(0, buf, sizeof (buf))) {
	case -1:
		if (errno != EWOULDBLOCK)
			perror("read");
		break;
	case 0:
		if ((old_fl = fcntl(0, F_SETFL, 0)) == -1) {
			perror("fcntl: F_SETFL 3");
			exit(1);
		}
		exit(0);
	default:
		write(1, buf, n);
		break;
	}
}

main()
{
	int n;
	struct sigvec sv;

	sv.sv_handler = interrupt_handler;
	sv.sv_mask = 0;
	sv.sv_onstack = 0;
	if (sigvec(SIGIO, &sv, 0) == -1) {
		perror("sigvec");
		exit(1);
	}
	if ((old_fl = fcntl(0, F_GETFL, 0)) == -1) {
		perror("fcntl: F_GETFL");
		exit(1);
	}
	if (fcntl(0, F_SETFL, FNDELAY|FASYNC) == -1) {
		perror("fcntl: F_SETFL");
		exit(1);
	}
	for (;;)
		sigpause(0);
}
