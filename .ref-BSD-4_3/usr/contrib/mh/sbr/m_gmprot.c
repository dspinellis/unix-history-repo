/* m_gmprot.c - return the msg-protect value */

#include "../h/mh.h"
#include <stdio.h>


m_gmprot () {
    register char  *cp;

    return atooi ((cp = m_find ("msg-protect")) && *cp ? cp : msgprot);
}
