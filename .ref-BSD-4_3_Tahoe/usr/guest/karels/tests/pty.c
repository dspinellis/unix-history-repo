/* 
 * tstpty.c - Test pty bug.
 * 
 * Author:	Spencer W. Thomas
 */

#include <stdio.h>

char tststring[] =
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n";
char sendbuf[BUFSIZ];

main()
{
    int ptcfd, ptsfd, n;

    if ( (ptcfd = open("/dev/ptyqf", 2)) < 0) { perror("ptyqf"); exit(1); }
    if ( (ptsfd = open("/dev/ttyqf", 2)) < 0) { perror("ttyqf"); exit(1); }

    if (fork() == 0)
    {
	close(ptcfd);
	while ((n = read(ptsfd, sendbuf, BUFSIZ)) > 0)
	    printf( "%d:%*.*s", n, n, n, sendbuf );
	exit(0);
    }

    strcpy( sendbuf, tststring );
    for (n=0; n<5; n++)
	strcat( sendbuf, tststring );

    printf( "Buflen = %d\n", strlen(sendbuf) );
    n = write( ptcfd, sendbuf, strlen(sendbuf) );
    printf( "Write returned %d\n", n );
    sleep(2);
    printf( "Sending newline\n" );
    write( ptcfd, "\n", 1 );
    close( ptcfd );
    wait(0);
}
