/* $RCSfile: msdos.c,v $$Revision: 4.0.1.1 $$Date: 91/06/07 11:22:37 $
 *
 *    (C) Copyright 1989, 1990 Diomidis Spinellis.
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 * $Log:	msdos.c,v $
 * Revision 4.0.1.1  91/06/07  11:22:37  lwall
 * patch4: new copyright notice
 * 
 * Revision 4.0  91/03/20  01:34:46  lwall
 * 4.0 baseline.
 * 
 * Revision 3.0.1.1  90/03/27  16:10:41  lwall
 * patch16: MSDOS support
 * 
 * Revision 1.1  90/03/18  20:32:01  dds
 * Initial revision
 *
 */

/*
 * Various Unix compatibility functions for MS-DOS.
 */

#include "EXTERN.h"
#include "perl.h"

#include <dos.h>
#include <process.h>

/*
 * Interface to the MS-DOS ioctl system call.
 * The function is encoded as follows:
 * The lowest nibble of the function code goes to AL
 * The two middle nibbles go to CL
 * The high nibble goes to CH
 *
 * The return code is -1 in the case of an error and if successful
 * for functions AL = 00, 09, 0a the value of the register DX
 * for functions AL = 02 - 08, 0e the value of the register AX
 * for functions AL = 01, 0b - 0f the number 0
 *
 * Notice that this restricts the ioctl subcodes stored in AL to 00-0f
 * In the Ralf Borwn interrupt list 90.1 there are no subcodes above AL=0f
 * so we are ok.
 * Furthermore CH is also restriced in the same area.  Where CH is used as a
 * code it always is between 00-0f.  In the case where it forms a count
 * together with CL we arbitrarily set the highest count limit to 4095.  It
 * sounds reasonable for an ioctl.
 * The other alternative would have been to use the pointer argument to
 * point the the values of CX.  The problem with this approach is that
 * of accessing wild regions when DX is used as a number and not as a
 * pointer.
 */
int
ioctl(int handle, unsigned int function, char *data)
{
	union REGS      srv;
	struct SREGS    segregs;

	srv.h.ah = 0x44;
	srv.h.al = (unsigned char)(function & 0x0F);
	srv.x.bx = handle;
	srv.x.cx = function >> 4;
	segread(&segregs);
#if ( defined(M_I86LM) || defined(M_I86CM) || defined(M_I86HM) )
	segregs.ds = FP_SEG(data);
	srv.x.dx = FP_OFF(data);
#else
	srv.x.dx = (unsigned int) data;
#endif
	intdosx(&srv, &srv, &segregs);
	if (srv.x.cflag & 1) {
		switch(srv.x.ax ){
		case 1:
			errno = EINVAL;
			break;
		case 2:
		case 3:
			errno = ENOENT;
			break;
		case 4:
			errno = EMFILE;
			break;
		case 5:
			errno = EPERM;
			break;
		case 6:
			errno = EBADF;
			break;
		case 8:
			errno = ENOMEM;
			break;
		case 0xc:
		case 0xd:
		case 0xf:
			errno = EINVAL;
			break;
		case 0x11:
			errno = EXDEV;
			break;
		case 0x12:
			errno = ENFILE;
			break;
		default:
			errno = EZERO;
			break;
		}
		return -1;
	} else {
		switch (function & 0xf) {
		case 0: case 9: case 0xa:
			return srv.x.dx;
		case 2: case 3: case 4: case 5:
		case 6: case 7: case 8: case 0xe:
			return srv.x.ax;
		case 1: case 0xb: case 0xc: case 0xd:
		case 0xf:
		default:
			return 0;
		}
	}
}


/*
 * Sleep function.
 */
void
sleep(unsigned len)
{
	time_t end;

	end = time((time_t *)0) + len;
	while (time((time_t *)0) < end)
		;
}

/*
 * Just pretend that everyone is a superuser
 */
#define ROOT_UID	0
#define ROOT_GID	0
int
getuid(void)
{
	return ROOT_UID;
}

int
geteuid(void)
{
	return ROOT_UID;
}

int
getgid(void)
{
	return ROOT_GID;
}

int
getegid(void)
{
	return ROOT_GID;
}

int
setuid(int uid)
{ return (uid==ROOT_UID?0:-1); }

int
setgid(int gid)
{ return (gid==ROOT_GID?0:-1); }

/*
 * The following code is based on the do_exec and do_aexec functions
 * in file doio.c
 */
int
do_aspawn(really,arglast)
STR *really;
int *arglast;
{
    register STR **st = stack->ary_array;
    register int sp = arglast[1];
    register int items = arglast[2] - sp;
    register char **a;
    char **argv;
    char *tmps;
    int status;

    if (items) {
	New(1101,argv, items+1, char*);
	a = argv;
	for (st += ++sp; items > 0; items--,st++) {
	    if (*st)
		*a++ = str_get(*st);
	    else
		*a++ = "";
	}
	*a = Nullch;
	if (really && *(tmps = str_get(really)))
	    status = spawnvp(P_WAIT,tmps,argv);
	else
	    status = spawnvp(P_WAIT,argv[0],argv);
	Safefree(argv);
    }
    return status;
}


int
do_spawn(cmd)
char *cmd;
{
    register char **a;
    register char *s;
    char **argv;
    char flags[10];
    int status;
    char *shell, *cmd2;

    /* save an extra exec if possible */
    if ((shell = getenv("COMSPEC")) == 0)
	shell = "\\command.com";

    /* see if there are shell metacharacters in it */
    if (strchr(cmd, '>') || strchr(cmd, '<') || strchr(cmd, '|'))
	  doshell:
	    return spawnl(P_WAIT,shell,shell,"/c",cmd,(char*)0);

    New(1102,argv, strlen(cmd) / 2 + 2, char*);

    New(1103,cmd2, strlen(cmd) + 1, char);
    strcpy(cmd2, cmd);
    a = argv;
    for (s = cmd2; *s;) {
	while (*s && isspace(*s)) s++;
	if (*s)
	    *(a++) = s;
	while (*s && !isspace(*s)) s++;
	if (*s)
	    *s++ = '\0';
    }
    *a = Nullch;
    if (argv[0])
	if ((status = spawnvp(P_WAIT,argv[0],argv)) == -1) {
	    Safefree(argv);
	    Safefree(cmd2);
	    goto doshell;
	}
    Safefree(cmd2);
    Safefree(argv);
    return status;
}
