#include "EXTERN.h"
#include "perl.h"
#include <stdio.h>

int userinit()
{
    install_null();	/* install device /dev/null or NUL: */
    return 0;
}
