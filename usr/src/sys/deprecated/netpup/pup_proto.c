/*	pup_proto.c	5.1	82/07/31	*/

#include "../h/param.h"
#include "../h/socket.h"
#include "../h/protosw.h"

/*
 * PUP-I protocol family: raw interface
 */
int	rpup_output();

struct protosw pupsw[] = {
{ SOCK_RAW,	PF_PUP,		0,		PR_ATOMIC|PR_ADDR,
  0,		rpup_output,	0,		0,
  raw_usrreq,
  0,		0,		0,		0,
},
};

struct domain pupdomain =
    { AF_PUP, "pup", pupsw, &pupsw[sizeof(pupsw)/sizeof(pupsw[0])] };
