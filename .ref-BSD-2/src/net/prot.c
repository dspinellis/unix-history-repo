/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
static struct packet *xptr, *gptr;
# define PACKETLENGTH (datasize + sizeof *xptr - 1)
# define ACKLENGTH (sizeof *xptr - 1)
static int bufleft;
int atime = ATIME;
int maxbread = MAXBREAD;
static char savebuf[BFS*2], retransmit;
static jmp_buf env;

/*
   one problem has been character loss on
   overloaded systems due to the daemon
   taking too long to swap in
   and losing characters.
   A high priority process of small size
   with a pipe would do the job.
*/
alarmint(){
	errno = 100;
	signal(SIGCLK,SIG_IGN);		/* alarm off */
	longjmp(env,0);			/* ugh */
	}
/* returns number of bytes written, error returns WRITEFAIL (-3) */
xwrite(inbuf,size,amt)
  char *inbuf;
{
	struct packet *rpp;
	int cnt, num, savetime;
	register char *p, *b;
	register int i;
	if(xptr == NULL)xptr = (struct packet *)calloc((PACKETLENGTH+20)/4,4);
	if(xptr == NULL){error("xptr NULL"); return(WRITEFAIL); }
	amt = amt * size;
	cnt = 0;
	retransmit = 0;
	savetime = atime;
	while(amt > 0){
		if(retransmit > maxbread){
			debug("xwrite fail");
			return(WRITEFAIL);
			}
		b = inbuf+cnt;
		num = min(datasize,amt);
		xptr->pcode = REQUEST;
		xptr->seqno = masterseqno;
		xptr->len = num;
		p = xptr->data;
		i = num;
		while(i--)*p++ = *b++;
		sendpacket(xptr);
		rpp = getpacket();
		if(rpp == NULL){
			atime += 3;		/* wait three more secs */
			retransmit++;
			dump.nretrans++;
			continue;
			}
		if(rpp->chksum != 0 || rpp->pcode != ACK
			|| rpp->seqno != xptr->seqno ){
			if(rpp->pcode == RESET){
				error("reset");
				return(WRITEFAIL);
				}
			if(rpp->seqno == 1 && rpp->pcode == REQUEST){
				error("collision");
				return(WRITEFAIL);
				}
			if(rpp->chksum != 0)
				error("chksum %d",rpp->seqno);
			else if(rpp->pcode != ACK)
				error("not ack %d %d",rpp->pcode,rpp->seqno);
			else if(rpp->seqno != xptr ->seqno)
				error("WRSQNO got %d request %d",rpp->seqno,
					xptr->seqno);
			atime += 3;
			retransmit++;
			dump.nretrans++;
			continue;
			}
		masterseqno++;
		amt -= num;
		retransmit = 0;
		cnt += num;
		}
	atime = savetime;
	return(cnt/size);
	}
/* return the number of bytes read, or error = BROKENREAD (-2) */
nread(b,size,num)
  register char *b;
{
	register char *p;
	int bcnt = 0;
    	char *q;
	register struct packet *pp;
	int n,j,cnt;
	num = num * size;
	cnt = 0;
	if(bufleft > 0){
		p = savebuf;
		cnt = n = min(bufleft,num);
		while(n--)*b++ = *p++;
		num -= cnt;
		bufleft -= cnt;
		if(bufleft > 0){
			q = savebuf;
			n = bufleft;
			while(n--)*q++ = *p++;
			}
		}
	if(num <= 0)
		return(cnt/size);
	retransmit = 0;
	for(;;){
		pp = getpacket();
		if(pp == NULL){
			if(++bcnt >= maxbread){
				debug("read timeout");
				return(BROKENREAD);
				}
			continue;
			}
		if(pp->chksum != 0){
			error("chksum %d",pp->seqno);
			retransmit++;
			continue;
			}
		if(pp->pcode & ~(REQUEST|RESET)){
			error("pcode %d %d",pp->pcode,pp->seqno);
			retransmit++;
			continue;
			}
		else if(pp->pcode == RESET)break;
		else {		/* else was a REQUEST packet, no chksum errs */
			pp->pcode = ACK;
			n = pp->len;
			pp->len = 0;
			sendpacket(pp);		/* send ACK */
			pp->len = n;
			break;
			}
		}
	retransmit = 0;
	j = n = min(num,pp->len);
	cnt += j;
	p = pp->data;
	while(n--)*b++ = *p++;
	if(pp->len > num){
		n = bufleft = pp->len - num;
		q = savebuf;
		while(n--)*q++ = *p++;
		}
	return(cnt/size);
	}
printpacket(pp,dest)
  char *dest;
  struct packet *pp; {
	char *s;
	int i;
	char c;
	dest[0] = 0;
	if(pp == NULL)return;
	if(pp->pcode == REQUEST)c='r';
	else if(pp->pcode == ACK)c = 'a';
	else if(pp->pcode == RESET)c = 'x';
	else if(pp->pcode == PURGE)c = 'p';
	else c = 'u';
	sprintf(dest,"p:%d len:%d c:%c d:", pp->seqno, pp->len, c);
	s = dest + strlen(dest);
	for(i=0; i<pp->len && pp->data[i]; i++)*s++ = pp->data[i];
	*s = 0;
	}
/*
 * A purge can always be sent -
 * the receiver totally ignores it.
 * It is used to push the packet terminator
 * down the wire in case of a crash
 * leaving the receiver half reading.
 */
sendpurge()
  {
	if(xptr == NULL)xptr = (struct packet *)calloc((PACKETLENGTH+20)/4,4);
	if(xptr == NULL){
		error("bad xptr");
		return;
		}
	xptr->pcode = PURGE;
	xptr->seqno = 0;
	xptr->len = 0;
	debug("send purge");
	sendpacket(xptr);
	}
/*
 * A reset is sent by the sender whenever he begins to send.
 * It is not acknowledged.
 * The receiver must be prepared, when he receives a reset,
 * to receive a new transmission.
 */
sendreset()
  {
	if(xptr == NULL)xptr = (struct packet *)calloc((PACKETLENGTH+20)/4,4);
	if(xptr == NULL){
		error("bad xptr");
		return;
		}
	xptr->pcode = RESET;
	xptr->seqno = 0;
	xptr->len = 0;
	debug("send reset");
	sendpacket(xptr);
	}
/*
 * Getreset returns in either of two cases:
 * 1) the read times out (return BROKENREAD)
 * 2) a reset packet is received (return 0)
 * all other packets received are ignored.
 */
getreset() {
	register struct packet *pp;
	register int bcnt = 0;
	bufleft = 0;		/* if any chars are left in buffer, flush them*/
	atime = ATIME + ((rand()>>8)%15);
	lastseqno = -1;		/* forces non-RESET pks to not be ACK-ed */
	for(;;){
		pp = getpacket();
		if(pp == NULL){
			if(++bcnt >= maxbread){
				debug("reset timeout");
				return(BROKENREAD);
				}
			continue;
			}
		if(pp->pcode == RESET){
			debug("got reset");
			addtolog(remote,"^R%c ",remote);
			lastseqno = 0;
			return(0);
			}
		}
	}
/*
 *	Just sends packet pp
 *	Calculates the chksum
 */
sendpacket(pp)
  struct packet *pp; {
	char buf[BFS*2];
	int len, n, i;
	long nt,ot;
	register char *q, *p;
	register int j;
	dump.nbytesent += pp->len;
	dump.npacksent++;
	pp->chksum = 0;
	n = 0;
	p = (char *)pp;
	len = ACKLENGTH + pp->len;
	for(j = 0; j < len; j++)n ^= *p++;
	pp->chksum = n;
	p = buf;
	q = (char *)pp;
	len = n = (len+2)/3;
	while(n--){
		*p++ = (*q & 077) + INCR;
		j =    (*q++ >> 6) &03;
		*p++ = (((*q << 2) | j) & 077) + INCR;
		j =    (*q++ >> 4) & 017;
		*p++ = (((*q << 4) | j) & 077) + INCR;
		*p++ = ((*q++ >> 2) & 077) + INCR;
		}
	*p++ = '\n';
	*p = 0;
/*	because of bugs in processing around erase and kill in v6 */
	for(p=buf; *p; p++)
		if(*p == '\\')*p = '}';
	/*
	debug("send %d %s",len*4+1,buf);
	*/
	ot = gettime();
	i = fwrite(buf,1,len*4+1, writetty);
	nt = gettime();
	dump.waittime += (nt - ot);			/* add time writing */
	/*
	debug("count %d",i);
	*/
	fflush(writetty);
	}
/*
 *	returns NULL if couldn't get a packet with correct seqno
 *	chksum not checked here
 * 	because other programs may want to interrogate checksum
 */
struct packet *getpacket() {
	char buf[BFS*2];
	int i,n,j, len, plen;
	int bcnt;
	register char *q, *p;
	long ot, nt;
	if(gptr == NULL)gptr = (struct packet *)calloc((PACKETLENGTH+20)/4,4);
	if(gptr == NULL){error("gptr NULL"); return(NULL); }
	bcnt = 0;
	errno = 0;
	setjmp(env);
	alarm(0);
	signal(SIGCLK,alarmint);
	for(;;){
		if(bcnt++ > maxbread)errno = 100;	/* give up */
		if(errno == 100){
			if(debugflg)putchar('^');
			return(NULL);
			}
		alarm(atime);
		ot = gettime();
		p = fgets(buf,BFS*2,readtty);
		alarm(0);
		nt = gettime();
		dump.waittime += (nt - ot);
		if(p == NULL){error("getpacket fails"); return(NULL); }
		plen = strlen(buf);
		/*
		debug("receive %d %s",plen,buf);
		*/
		/* remove this loop later */
		for(p=buf; *p; p++)
			if(*p == '}')*p = '\\';
		p = buf;
		q = (char *)gptr;
		n = (strlen(buf)+3) /4;
		while(n--){
			if(*p == '\n')break;
			if(*p < INCR || *p & 0200)error("bad char %o\n",*p);
			i =  *p++ - INCR;
			j =  *p++ - INCR;
			*q++ = ((j & 03) << 6) | (i & 077);
			i =  *p++ -INCR;
			*q++ = ((i & 017) << 4) | ((j >> 2) & 017);
			j =  *p++ - INCR;
			*q++ = ((j & 077) << 2) | ((i >> 4) & 03);
			}
		*q = 0;
		if(plen != ((ACKLENGTH + gptr->len + 2)/3)*4 + 1){
			error("too short %d",gptr->seqno);
			continue;
			}
		if(gptr->pcode == PURGE){
			debug("got purge");
			continue;		/* never seen */
			}
		if(gptr->pcode == RESET)
			break;
		if(gptr->seqno == lastseqno){
			if(retransmit)break;
			/* send ACK - it was lost first time thru */
			len = gptr->len;
			n = gptr->pcode;
			gptr->len = 0;
			gptr->pcode = ACK;
			sendpacket(gptr);
			gptr->len = len;
			gptr->pcode = n;
			error("sendlostack %d",lastseqno);
			break;
			}
		if(gptr->seqno == lastseqno + 1)break;
		error("Wrong seq no g: %d last: %d",gptr->seqno,
			lastseqno);
		}
	lastseqno = gptr->seqno;
	n = 0;
	len = gptr->len + ACKLENGTH;
	p = (char *)gptr;
	for(i=0; i < len; i++)n ^= *p++;
	gptr->chksum = n;
	if(n != 0)dump.ncksum++;
	dump.nbytercv += gptr->len;
	dump.npackrcv++;
	return(gptr);
}
