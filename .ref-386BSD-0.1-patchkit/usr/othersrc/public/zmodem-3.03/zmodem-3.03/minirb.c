/*
 * minirb.c By Chuck Forsberg Omen Technology INC
 *        "The High Reliability Communications Software"
 *
 * A bootstrap program for Unix to receive files from computers running
 *  YMODEM Batch (Professional-YAM, PowerCom, ZCOMM, etc.).
 *
 *  Minirb uses system(3) to call stty, avoiding system dependent code.
 *   program strips CR and CPMEOF (^Z) characters (see putsec()).
 *  Please refer to rz.c for comments, etc.
 */
char * Version = "minirb 2.00 05-25-87";

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#define OK 0
#define FALSE 0
#define TRUE 1
#define ERROR (-1)
#define CAN ('X'&037)
#define SOH 1
#define STX 2
#define EOT 4
#define ACK 6
#define NAK 025
#define TIMEOUT (-2)
#define RETRYMAX 9
#define WCEOT (-10)

FILE *fout;
long Bytesleft;
int Blklen;
char secbuf[1024];
char linbuf[1024];
int Lleft=0;
jmp_buf tohere;

alrm() { longjmp(tohere, -1); }

bibi(n) {
 canit(); mode(0);
 fprintf(stderr, "minirb: caught signal %d; exiting", n);
 exit(128+n);
}

mode(n) {
 if (n) system("stty raw -echo");
 else system("stty echo -raw");
}

main() {
 mode(1);
 if (signal(SIGINT, bibi) == SIG_IGN) {
  signal(SIGINT, SIG_IGN); signal(SIGKILL, SIG_IGN);
 } else {
  signal(SIGINT, bibi); signal(SIGKILL, bibi);
 }
 printf("minirb: Now send file(s) with \042sb file ...\042 command\r\n");

 if (wcreceive()==ERROR)
  canit();
 mode(0); exit(0);
}

wcreceive() {
 for (;;) {
  if (wcrxpn(secbuf) == ERROR) break;
  if (secbuf[0]==0) return OK;
  if (procheader(secbuf)==ERROR || wcrx()==ERROR) break;
 }
 canit(); return ERROR;
}


wcrxpn(rpn) char *rpn; {
 register c;

 purgeline();
et_tu:
 sendline(NAK); Lleft=0;
 while ((c = wcgetsec(rpn, 100)) != 0) {
  if (c == WCEOT) { sendline(ACK); Lleft=0; readline(1); goto et_tu; }
  return ERROR;
 }
 sendline(ACK); return OK;
}

wcrx() {
 register int sectnum, sectcurr, sendchar, cblklen;

 sectnum=0; sendchar=NAK;
 for (;;) {
  sendline(sendchar); Lleft=0;
  sectcurr=wcgetsec(secbuf, 50);
  if (sectcurr==(sectnum+1 & 0377)) {
   sectnum++; cblklen = Bytesleft>Blklen ? Blklen:Bytesleft;
   putsec(secbuf, cblklen);
   if ((Bytesleft-=cblklen) < 0) Bytesleft = 0;
   sendchar=ACK;
  }
  else if (sectcurr==(sectnum&0377)) sendchar=ACK;
  else if (sectcurr==WCEOT) {
   if (fclose(fout)==ERROR) return ERROR;
   sendline(ACK); Lleft=0; return OK;
  }
  else if (sectcurr==ERROR) return ERROR;
  else return ERROR;
 }
}

wcgetsec(rxbuf, maxtime) char *rxbuf; int maxtime; {
 register checksum, wcj, firstch; register char *p; int sectcurr, errors;
 for (errors=0; errors<RETRYMAX; errors++) {
  if ((firstch=readline(maxtime))==STX) { Blklen=1024; goto get2; }
  if (firstch==SOH) {
   Blklen=128;
get2:
   sectcurr=readline(1); checksum=0;
   if ((sectcurr+(readline(1)))==0377) {
    for (p=rxbuf,wcj=Blklen; --wcj>=0; ) {
     if ((firstch=readline(1)) < 0) goto bilge;
     checksum += (*p++ = firstch);
    }
    if ((firstch=readline(1)) < 0) goto bilge;
    if (((checksum-firstch)&0377)==0) return sectcurr;
   }
  }
  else if (firstch==EOT) return WCEOT;
  else if (firstch==CAN) return ERROR;
bilge:
  while(readline(1)!=TIMEOUT)
   ;
  maxtime=40; sendline(NAK); Lleft=0;
 }
 canit(); return ERROR;
}

readline(timeout) int timeout; {
 register n; static char *cdq;

 if (--Lleft >= 0) return (*cdq++ & 0377);
 n = timeout/10;
 if (n < 2) n = 3;
 if (setjmp(tohere)) { Lleft = 0; return TIMEOUT; }
 signal(SIGALRM, alrm); alarm(n);
 Lleft=read(0, cdq=linbuf, 1024); alarm(0);
 if (Lleft < 1) return TIMEOUT;
 --Lleft; return (*cdq++ & 0377);
}

purgeline() { Lleft = 0; lseek(0, 0L, 2); }


procheader(name) char *name; {
 register char *p;

 Bytesleft = 2000000000L; p = name + 1 + strlen(name);
 if (*p) sscanf(p, "%ld", &Bytesleft);
 if ((fout=fopen(name, "w")) == NULL) return ERROR;
 return OK;
}

putsec(p, n) char *p; int n;
{ for (; --n>=0; ++p) if (*p != 015 && *p != 032) putc(*p, fout); }

sendline(c) { char d; d = c; write(1, &d, 1); }

char canistr[] = { 24,24,24,24,24,24,24,24,0 };

canit() { printf(canistr); Lleft=0; }

/* END of minirb.c */

