/**************************************************************************
 * ns_ncache.c
 * author: anant kumar
 * last modification: March 17, 1993
 *
 * implements negative caching
 */

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <syslog.h>
#include <errno.h>
#include <stdio.h>
#include <resolv.h>

#include "named.h"

#ifdef NCACHE

void
cache_n_resp(msg, msglen)
	u_char *msg;
	int msglen;
{
	register struct databuf *dp;
	HEADER *hp;
	u_char *cp;
	char dname[MAXDNAME];
	int n;
	int type, class;
	int Vcode;
	int flags;

	hp = (HEADER *)msg;
	cp = msg+sizeof(HEADER);
  
	if ((n=dn_expand(msg, msg +msglen, cp,
			 (u_char *)dname, sizeof(dname))) < 0) {
		dprintf(1, (ddt, "Query expand name failed:cache_n_resp\n"));
		hp->rcode = FORMERR;
		return;
	}
	cp += n;
	GETSHORT(type, cp);
	GETSHORT(class, cp);
	dprintf(1, (ddt,
		    "ncache: dname %s, type %d, class %d\n",
		    dname, type, class));

#ifdef VALIDATE
	Vcode = validate(dname,&from_addr,type,class,NULL,0,
			 hp->rcode==NXDOMAIN?NXDOMAIN:NOERROR_NODATA );
	if (Vcode == INVALID || Vcode == VALID_NO_CACHE) {
		/*Valid_no_cache should never occur but doesn't hurt to check*/
		return;
	}
#endif
	dp = savedata(class, type, NTTL+tt.tv_sec, NULL, 0);
	dp->d_zone = DB_Z_CACHE;
#ifdef CRED
	dp->d_cred = DB_C_AUTH; /*good credibility data*/
	dp->d_clev = 0; /*good credibility data*/
#endif
	if(hp->rcode == NXDOMAIN) {
		dp->d_rcode = NXDOMAIN;
		flags = DB_NODATA|DB_NOTAUTH|DB_NOHINTS;
	} else {
		dp->d_rcode = NOERROR_NODATA;
		flags = DB_NOTAUTH|DB_NOHINTS;
	}

	if ((n = db_update(dname,dp,dp,flags,hashtab)) < 0) {
		dprintf(1, (ddt,
			  "db_update failed return value:%d, cache_n_resp()\n",
			    n));
		free((char*)dp);
		return;
	}
	dprintf(4, (ddt,
		    "ncache succeeded: d:%s, t:%d, c:%d rcode:%d ttl:%d\n",
		    dname,type,class,dp->d_rcode, dp->d_ttl-tt.tv_sec));
	return;
}

#endif /*NCACHE*/
