/* $Header: get_addrs.c 1.5 83/04/01 17:55:41 moore Exp $ */

#include "talk_ctl.h"

struct hostent *gethostbyname();
struct servent *getservbyname();

get_addrs(my_machine_name, his_machine_name)
char *my_machine_name;
char *his_machine_name;
{
    struct hostent *hp;
    struct servent *sp;

    msg.pid = getpid();

	/* look up the address of the local host */

    hp = gethostbyname(my_machine_name);

    if (hp == (struct hostent *) 0) {
	printf("This machine doesn't exist. Boy, am I confused!\n");
	exit(-1);
    }

    if (hp->h_addrtype != AF_INET) {
	printf("Protocal mix up with local machine address\n");
	exit(-1);
    }

    bcopy(hp->h_addr, (char *)&my_machine_addr, hp->h_length);

	/* if he is on the same machine, then simply copy */

    if ( bcmp((char *)&his_machine_name, (char *)&my_machine_name,
		sizeof(his_machine_name)) == 0) {
	bcopy((char *)&my_machine_addr, (char *)&his_machine_addr,
		sizeof(his_machine_name));
    } else {

	/* look up the address of the recipient's machine */

	hp = gethostbyname(his_machine_name);

	if (hp == (struct hostent *) 0 ) {
	    printf("%s is an unknown host\n", his_machine_name);
	    exit(-1);
	}

	if (hp->h_addrtype != AF_INET) {
	    printf("Protocol mix up with remote machine address\n");
	    exit(-1);
	}

	bcopy(hp->h_addr, (char *) &his_machine_addr, hp->h_length);
    }

	/* find the daemon portal */

#ifdef NTALK
    sp = getservbyname("ntalk", "udp");
#else
    sp = getservbyname("talk", "udp");
#endif

    if (strcmp(sp->s_proto, "udp") != 0) {
	printf("Protocol mix up with talk daemon\n");
	exit(-1);
    }

    if (sp == 0) {
	    p_error("This machine doesn't support a tcp talk daemon");
	    exit(-1);
    }

    daemon_port = sp->s_port;
}
