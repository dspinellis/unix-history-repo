main()
{
#include <sys/ioctl.h>
ioctl(1, TIOCFLUSH, 0);
}
