/*	pup_proto.c	5.2	82/08/01	*/

#include "../h/param.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/domain.h"

/*
 * PUP-I protocol family: raw interface
 */
int	rpup_output();
extern	int raw_usrreq();

struct protosw pupsw[] = {
{ SOCK_RAW,	PF_PUP,		0,		PR_ATOMIC|PR_ADDR,
  0,		rpup_output,	0,		0,
  raw_usrreq,
  0,		0,		0,		0,
},
};

struct domain pupdomain =
    { AF_PUP, "pup", pupsw, &pupsw[sizeof(pupsw)/sizeof(pupsw[0])] };
