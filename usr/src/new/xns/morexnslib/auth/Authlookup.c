/* $Header: */

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* contains:
 * Auth_GetFirstAuth
 */

/* $Log:	Authlookup.c,v $
 * Revision 1.2  87/03/23  10:25:34  ed
 * Minor change.
 * 
 * Revision 1.1  87/01/05  11:50:16  ed
 * Initial revision
 * 
 */
#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <xnscourier/courier.h>
#include <xnscourier/courierconnection.h>
#include <xnscourier/except.h>
#include <sys/file.h>

#ifndef BFS
#define BFS "/usr/new/xnsbfs -a"
#endif BFS

struct ns_addr *chaddr;

CourierConnection*
Auth_GetFirstAuth()
{
	extern struct ns_addr *getXNSaddr();
 	char buf[BUFSIZ];
	CourierConnection *result;
	FILE *chfile;
	int i;

	result = (CourierConnection *) NULL;
	/* broadcast for Authentication server */
	/*  -- this could be more efficient! */
	if ((chfile = popen(BFS,"r")) != NULL) {
		while (fgets(buf, BUFSIZ, chfile) != NULL)
			if ((buf[0] != '#') &&
			    (chaddr = getXNSaddr(buf)) &&
			    (result = CourierOpen(chaddr))) {
				pclose(chfile);
				return(result);
			}
		pclose(chfile);
	}
	return(result);
}

