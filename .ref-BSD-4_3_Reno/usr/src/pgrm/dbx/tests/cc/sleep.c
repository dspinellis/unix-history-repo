#include <stdio.h>

main ()
{
    char token[80];

    printf("about to sleep");
    fflush(stdout);
    sleep(2);
    endnot();
}

endnot()
{
    printf("done\n");
}
