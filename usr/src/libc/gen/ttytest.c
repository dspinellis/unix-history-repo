#include "stdio.h"
main() {
printf("\nttyname(fd)=%s\n", ttyname(0));
printf("\nttyname=%s\n", ttyname(stdin));
}
