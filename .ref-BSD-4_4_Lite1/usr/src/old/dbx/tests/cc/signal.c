/*
 * Test of tracebacks from signal handlers.
 */

#include <stdio.h>
#include <signal.h>

int catch(), secondcatch();

main()
{
    signal(SIGQUIT, catch);
    kill(getpid(), SIGQUIT);
    printf("back in main\n");
}

catch()
{
    printf("in catch\n");
    sigsetmask(0);
    signal(SIGQUIT, secondcatch);
    kill(getpid(), SIGQUIT);
    printf("back in catch\n");
}

secondcatch()
{
    printf("in secondcatch\n");
}
