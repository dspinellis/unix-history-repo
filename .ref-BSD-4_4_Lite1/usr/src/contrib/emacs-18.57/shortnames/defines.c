#include <stdio.h>

int count;

main ()
{
    char buffer[512];

    while (gets (buffer))
      {
	printf ("#define %s zz%4.4dzz\n", buffer, count++);
      }
    return (0);
}
