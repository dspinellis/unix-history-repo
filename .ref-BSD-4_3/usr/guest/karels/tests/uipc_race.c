#include <sys/signal.h>

char foo[4096] = "junk";
char buf[4096] = "initialized data\n";

flush()
{
	write(1, buf, sizeof(buf));
	exit(1);
}

main()
{
	signal(SIGINT, flush);
	signal(SIGHUP, flush);
	signal(SIGPIPE, flush);
	pause();
}
