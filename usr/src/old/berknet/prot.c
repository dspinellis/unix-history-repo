static char sccsid[] = "@(#)prot.c	4.1	(Berkeley)	9/12/82";

/* Protocol driver, user level, Berkeley network */
/*
	This code is a little complicated because of a number of different
	protocols used.  Here is an explanation:

Level	Description

0	Normal Case (6 bit with no kernel driver support)

1	Line Discipline -- uses NETLDISP in sgtty.h and ioctl to set the
	line discipline.  At Berkeley this means avoiding interrupting on
	every character by using a Silo on a DH or DZ board, and (optionally)
	bypassing the canonicalization in the tty code by putting the charactars
	directly in a buffer.
	condition (netd.dp_bnetldis != 0)

2	8-bit TTY protocol -- implies Level 1 and inserts record separators(012)
	and escapes other occurrences of 012.  Since the driver on the other
	end must interpolate the escapes, this is an asymmetric protocol where
	the sender puts in the escapes but the receiver at the user level knows
	they have already been removed.
	condition (netd.dp_bnetldis != 0 && netd.dp_use8bit != 0)

3	8-bit Block Device protocol -- this is for a DMC-11, it writes fixed
	length blocks in both directions with no quoting.
	condition (netd.dp_bnetldis != 0 && netd.dp_usehighspeed != 0)

4	RAND 8-bit protocol -- included for completeness, is not
	correctly specified here.
	Specified by an IFDEF.

If the daemons are being simulated by pipes, then netd.dp_pipesim != 0
and each of the 4 levels (except RAND) are simulated.
In this case at level 2 (use8bit) on the receiver end it does the quoting.

Timing statistics: We estimate 300 micros for queue/dequeue and then
	20 micros per interrupt for 30 cps => 2.5% of system for 9600 Baud line

Max packet lengths=> to CSVAX with 1k buffers and 6-bit prot = 758 chars
	to Ing70 with 512 byte buffers and no NETLDISC, only 182 chars

*/
# include "defs.h"

/* global */
struct dumpstruc dump;
struct daemonparms netd;

/* local */
static int bufleft;
static char retransmit;
static jmp_buf env;
static short masterseqno, lastseqno;
/* writing packet */
static char wpack[MAXNBUF];

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
	signal(SIGALRM,SIG_IGN);		/* alarm off */
	longjmp(env,0);			/* ugh */
	}
/* returns number of bytes written, error returns WRITEFAIL (-3) */
/* inbuf is buffer of amt chars to be written */
xwrite(inbuf,amt)
  char *inbuf;
{
	register char *p, *b;
	register int i;
	int cnt, num, savetime;
	struct packet *rpp, *xptr;

	xptr = (struct packet *)wpack;
	cnt = 0;
	retransmit = 0;
	savetime = netd.dp_atime;
	while(amt > 0){
		if(retransmit > netd.dp_maxbread){
			debug("xwrite fail");
			return(WRITEFAIL);
			}
		/* format the packet to send */
		num = min(netd.dp_datasize,amt);
		/* set the length down if escapes are being used */
		if(netd.dp_use8bit)num = min(num,MAXNBUF/2);
		xptr->pcode = REQUEST;
		xptr->seqno = masterseqno;
		xptr->len = num;
		p = xptr->data;
		i = num;
		b = inbuf+cnt;
		while(i--)*p++ = *b++;
		/* send it */
		sendpacket(xptr);
		rpp = getpacket();
		if(rpp == NULL){
			netd.dp_atime += 3;	/* wait three more secs */
			retransmit++;
			dump.nretrans++;
			continue;
			}
		/* various errors */
		if(rpp->chksum != 0 || rpp->pcode != ACK
			|| rpp->seqno != xptr->seqno ){
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
			netd.dp_atime += 3;
			retransmit++;
			dump.nretrans++;
			continue;
			}
		masterseqno++;
		retransmit = 0;
		amt -= num;
		cnt += num;
		}
	netd.dp_atime = savetime;
	return(cnt);
	}
/* return the number of bytes read, or error = BROKENREAD (-2) */
nread(bptr,num)
	register char *bptr;
{
	register char *p;
	register struct packet *pp;
    	register char *q;
	int bcnt = 0;
	int n,j,cnt;
	static char savebuf[MAXNBUF];

	/* first see if theres any left from the last packet */
	cnt = 0;
	if(bufleft > 0){
		p = savebuf;
		cnt = n = min(bufleft,num);
		while(n--)*bptr++ = *p++;
		num -= cnt;
		bufleft -= cnt;
		if(bufleft > 0){
			q = savebuf;
			n = bufleft;
			while(n--)*q++ = *p++;
			}
		}
	if(num <= 0)
		return(cnt);
	/* now read a packet */
	retransmit = 0;
	for(;;){
		pp = getpacket();
		if(pp == NULL){
			if(++bcnt >= netd.dp_maxbread){
				debug("read timeout");
				return(BROKENREAD);
				}
			continue;
			}
		/* various errors */
		if(pp->chksum != 0){
			error("chksum %d",pp->seqno);
			retransmit++;
			continue;
			}
		if(pp->pcode & ~REQUEST){
			error("pcode %d %d",pp->pcode,pp->seqno);
			retransmit++;
			continue;
			}
		/* this is the normal case, so we ack it */
		else {		/* else was a REQUEST packet, no chksum errs */
			/*
			if(pp->seqno == 1)debug("^R ");
			*/
			pp->pcode = ACK;
			n = pp->len;
			pp->len = 0;
			sendpacket(pp);		/* send ACK */
			pp->len = n;
			break;
			}
		}
	/* now process this packet, bptr points to where we left off */
	retransmit = 0;
	j = n = min(num,pp->len);
	cnt += j;
	p = pp->data;
	while(n--)*bptr++ = *p++;
	if(pp->len > num){
		n = bufleft = pp->len - num;
		bptr = savebuf;
		while(n--)*bptr++ = *p++;
		}
	return(cnt);
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
	struct packet *xptr;
	xptr = (struct packet *)wpack;
	xptr->pcode = PURGE;
	xptr->seqno = 0;
	xptr->len = 0;
	debug("send purge");
	sendpacket(xptr);
	}
/* init sequence numbers */
initseqno(){
	masterseqno = 1;
	lastseqno = 0;
	bufleft = 0;		/* if any chars are left in buffer, flush them*/
	netd.dp_atime = netd.dp_oatime + ((rand()>>8)%15);
	}
/*
 *	Just sends packet pp
 *	Calculates the chksum
 */
sendpacket(pp)
  struct packet *pp; {
	register char *q, *p;
	register int j;
	char *finalp;
	static char raw[MAXNBUF];
	int len, n, i;

	/* writes the data to be sent in array raw */
	/* finalp will point to either pp or raw */
	dump.nbytesent += pp->len;
	dump.npacksent++;
	pp->chksum = 0;
	n = 0;
	p = (char *)pp;
	len = ACKLENGTH + pp->len;
	for(j = 0; j < len; j++)n ^= *p++;
	pp->chksum = n;
# ifdef SWAB
	switchem(pp);
# endif
# ifndef RAND
	if(netd.dp_usehispeed)finalp = (char *)pp;
	else if(netd.dp_use8bit){
		if(len >= MAXNBUF){
			fprintf(stderr,"Packet size too big- error\n");
			exit(1);
		}
		/* add escapes */
		p = (char *)pp;
		q = raw;
		i = len;
		len = 0;
		for(j = 0; j < i; j++){
			if(*p == '\n' || *p == '\\'){
				*q++ = '\\';
				*q++ = *p++;
				len++;
				len++;
			}
			else {
				*q++ = *p++;
				len++;
			}
		}
		*q = '\n';
		len++;
		finalp = raw;
	}
	else {
		/* now change 8-bit data to 6-bit data */
		if(((len+2)*4)/3 >= MAXNBUF){
			fprintf(stderr,"Packet size too big- error\n");
			exit(1);
			}
		p = raw;
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
		for(p=raw; *p; p++)
			if(*p == '\\')*p = '}';
		len = len * 4 + 1;
		finalp = raw;
	}
	/*
	debug("send %d <<%s>>",len,raw);
	*/
	if(netd.dp_usehispeed){
		if(len > SENDLEN)error("send length too long");
		len = SENDLEN;
		}
	if(netd.dp_pipesim) i = write(netd.dp_pwritefd,finalp,len);
	else i = write(netd.dp_linefd,finalp,len);
	dump.braw += i;
	dump.brawtot += i;
# ifdef SWAB
	switchem(pp);
# endif
# else
	/* for RAND */
	i = write(netd.dp_linefd, (char *)pp,len);
# endif
	/*
	debug("count %d",i);
	*/
	}

static int tooshort;
/*
 *	returns NULL if couldn't get a packet with correct seqno
 *	chksum not checked here
 * 	because other programs may want to interrogate checksum
 */
struct packet *getpacket() {
	register struct packet *gptr;
	register char *p;
	register int i;
	int n, bcnt, len;
	struct packet *decpacket();

	bcnt = 0;
	errno = 0;
	setjmp(env);
	alarm(0);
	signal(SIGALRM,alarmint);
	for(;;){
		if(bcnt++ > netd.dp_maxbread)errno = 100;	/* give up */
		if(errno == 100){
			if(debugflg)putchar('^');
			return(NULL);
			}
		/* decode the buffer, including 6-8 bit conv, etc. */
		gptr = decpacket();
		if(gptr == NULL){
			error("getpacket fails");
			return(NULL);
		}
		if(tooshort || gptr->len < 0 || gptr->len > MAXNBUF){
			error("too short p:%d l:%d",gptr->seqno,gptr->len);
			continue;
		}
		if(gptr->seqno == 1 && gptr->pcode != ACK){
			debug("got reset");
			addtolog(remote,"^R ");
			}
		if(gptr->pcode == PURGE){
			debug("got purge");
			continue;		/* never seen */
			}
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
		/* this is the correct case */
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
/* read in and decode packet */
/* as a side effect sets "tooshort" */
static struct packet *decpacket()
{
# ifndef RAND
	register char *p, *q;
	register int i,j;
	int n, len, ch;
	struct packet *pp;
	static char cooked[MAXNBUF], raw[MAXNBUF];

	/* read in chars to raw, if processed then return in cooked, otherwise
	return in raw */
	alarm(netd.dp_atime);
	tooshort = 0;
	if(netd.dp_pipesim){
		if(netd.dp_usehispeed)
			len = read(fileno(netd.dp_rdfile),raw,SENDLEN);
		else {
			q = raw;
			len = 0;
			for(;;){
				ch = getc(netd.dp_rdfile);
				len++;
				if(ch == '\n'){
					*q++ = '\n';
					break;
				}
				/* eat up the backslashes */
				if(ch == '\\' && netd.dp_use8bit)
					ch = getc(netd.dp_rdfile);
				*q++ = ch;
			}
			if(netd.dp_use8bit)len--;
		}
	}
	else if(netd.dp_usehispeed)
		len = read(fileno(netd.dp_rdfile),raw,SENDLEN);
	else len = read(netd.dp_linefd,raw,MAXNBUF);
	alarm(0);
	if(len == 0)fprintf(stderr,"eof pip %d\n",fileno(netd.dp_rdfile));
	if(len <= 0)return(NULL);
	raw[len] = 0;
	dump.braw += len;
	dump.brawtot += len;
	/*
	debug("receive %d <<%s>>",len,raw);
	*/
	/* if 8 bit the all we need to do is return */
	if(netd.dp_usehispeed)return((struct packet *)raw);
	if(netd.dp_use8bit){
		pp = (struct packet *)raw;
		if(len != ACKLENGTH + pp->len)tooshort = 1;
		return(pp);
	}
	/* remove this loop later */
	for(p=raw; *p; p++)
		if(*p == '}')*p = '\\';
	p = raw;
	q = cooked;
	n = (len+3) /4;
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
	pp = (struct packet *)cooked;
# ifdef SWAB
	switchem(pp);
# endif
	if(len != ((ACKLENGTH + pp->len + 2)/3)*4 + 1) tooshort = 1;
# else
	/* for RAND */
	/* not sure of the length computation */
	if(len != ACKLENGTH + gptr->len) tooshort = 1;
# endif
	return((struct packet *)cooked);
}

# ifdef SWAB
switchem(pp)
register struct packet *pp; {
	register short *p;
	p = &(pp->seqno);
	swab(p, p, 2);
	p = &(pp->len);
	swab(p, p, 2);
}
# endif
