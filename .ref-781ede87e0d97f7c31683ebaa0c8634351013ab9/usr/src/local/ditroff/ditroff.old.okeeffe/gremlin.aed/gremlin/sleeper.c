#include <signal.h>

int lasttime = 0;
int intcount = 0;
sigquit(x)
{
	int curtime;
	signal(SIGQUIT,sigquit);
	curtime = time(0);
	if(curtime - lasttime < 10) exit();
	lasttime = curtime;
}
	
main()
{
	signal(SIGINT,SIG_IGN);
	signal(SIGQUIT,sigquit);
	chmod(ttyname(1),0666);
	system("echo tty is: ; tty");
	while(1) sleep(200000);
}
