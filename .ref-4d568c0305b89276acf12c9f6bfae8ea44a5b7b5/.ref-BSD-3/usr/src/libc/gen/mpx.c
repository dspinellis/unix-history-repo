#include <sys/param.h>
#include <sys/tty.h>
#include <sys/mx.h>

static struct mx_args vec;
int	mpxcall();


mpx(name,mode)
char *name;
{
	if (*name) {
		vec.m_name = name;
		vec.m_arg[1] = mode;
		return(mpxcall(MPX, &vec));
	} else
		return(mpxcall(MPXN, 0));
}

chan(gr)
{
	vec.m_arg[1] = gr;
	return(mpxcall(CHAN, &vec));
}

join(fd,ch)
{
	vec.m_arg[0] = fd;
	vec.m_arg[1] = ch;
	return(mpxcall(JOIN, &vec));
}

connect(fd,ch,side)
{
	vec.m_arg[0] = fd;
	vec.m_arg[1] = ch;
	vec.m_arg[2] = side;
	return(mpxcall(CONNECT, &vec));
}

attach(sub,gp)
{
	vec.m_arg[0] = sub;
	vec.m_arg[1] = gp;
	return(mpxcall(ATTACH, &vec));
}

detach(sub,gp)
{
	vec.m_arg[0] = sub;
	vec.m_arg[1] = gp;
	return(mpxcall(DETACH, &vec));
}

extract(sub,ch,side)
{
	vec.m_arg[0] = sub;
	vec.m_arg[1] = ch;
	vec.m_arg[2] = side;
	return(mpxcall(EXTR, &vec));
}

debug(var,val)
{
	vec.m_arg[0] = var;
	vec.m_arg[1] = val;
	return(mpxcall(DEBUG, &vec));
}

npgrp(ch, gfd, pid)
{
	vec.m_arg[0] = ch;
	vec.m_arg[1] = gfd;
	vec.m_arg[2] = pid;
	return(mpxcall(NPGRP, &vec));
}
ckill(index,gp,sig)
{
	vec.m_arg[0] = index;
	vec.m_arg[1] = gp;
	vec.m_arg[2] = sig;
	return(mpxcall(CSIG, &vec));
}
