/*
 * Fake multiplexor routines to satisfy references
 * if you don't want it.
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/inode.h"
#include "../h/mx.h"

sdata(cp)
struct chan *cp;
{
}

mcttstart(tp)
struct tty *tp;
{
}

mpxchan()
{
	u.u_error = EINVAL;
}

mcstart(p, q)
struct chan *p;
caddr_t q;
{
}

scontrol(chan, s, c)
struct chan *chan;
{
}
