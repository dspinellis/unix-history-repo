#include <sys/types.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/ioctl.h>

jmp_buf jb;
int zero;

gorp() {
    longjmp(jb, 0);
}

main() {
    int i;
    for(i = 0; i < 20; i++)
	    if(i != 1) close(i);
    setjmp(jb);
    ioctl(1, TIOCSTART, 0);
    ioctl(1, TIOCFLUSH, &zero);
    ioctl(1, TIOCSTOP, 0);
    write(1, "a", 1);
    signal(SIGALRM, gorp);
    alarm(1);
    close(1);
}
