#ifndef ADS_DEBUG_H
#define ADS_DEBUG_H

#ifndef int16
#define int32	long
#define int16	short
#define int8	char
#define uint32	unsigned long
#define uint16	unsigned short
#define uint8	unsigned char
#endif

#ifndef bool
#define bool int
#endif

#ifndef true
#define true 1
#define false 0
#endif

#ifndef nil
#define nil 0
#endif

#ifndef byte
#define byte unsigned char
#endif

#ifndef min
#define min(a,b) ((a<b)?a:b)
#endif

#ifndef NODEBUG
/* extern bool Dflg; /* */
#define Dflg true

#ifdef GNU_CPP
#define STRINGIFY(x) #x
#else
#define STRINGIFY(x) "x"
#endif

#ifndef name2
#define name2(a,b) a/**/b
#endif

#define dbg(s)  if (Dflg) {fprintf(stderr,STRINGIFY(s));
#define dbgaxl(a,i) fprintf(stderr,dbgaxlstring,STRINGIFY(a),i,a[i]);
#define dbgb(s) fprintf(stderr,dbgbstring,STRINGIFY(s),s?"true":"false");
#define dbgc(s) fprintf(stderr,dbgcstring,STRINGIFY(s),s);
#define dbgd(s) fprintf(stderr,dbgdstring,STRINGIFY(s),s);
#define dbgg(s) fprintf(stderr,dbggstring,STRINGIFY(s),s);
#define dbgo(s)	fprintf(stderr,dbgostring,STRINGIFY(s),s);
#define dbgs(s) fprintf(stderr,dbgsstring,STRINGIFY(s),s);
#define dbgx(s) fprintf(stderr,dbgxstring,STRINGIFY(s),s);
#define dbgxl(w) fprintf(stderr,dbgxlstring,STRINGIFY(w),w);
#define dbgxw(w) fprintf(stderr,dbgxstring,STRINGIFY(w),(0xFFFF & w));
#define eol	fprintf(stderr,"\n"); fflush(stderr);
#define eor	eol } else {}

#define assert(x) {if (!(x)) {		\
	fprintf(stderr,"(x) failed\n");	\
	assertionFailedMacro		\
	exit(99);			\
	} }

static char dbgaxlstring[]= " %s[%d]=0x%lx";
static char dbgbstring[] = " %s=%s";
static char dbgcstring[] = " %s='%c'";
static char dbgdstring[] = " %s=%d";
static char dbggstring[] = " %s=%g";
static char dbgostring[] = " %s=%o";
static char dbgsstring[] = " %s=\"%s\"";
static char dbgxstring[] = " %s=0x%x";
static char dbgxlstring[]= " %s=0x%lx";

#else

#define dbg(s) /**/
#define dbgaxl(a,i) /**/
#define dbgb(s) /**/
#define dbgo(s)	/**/
#define dbgs(s) /**/
#define dbgg(s) /**/
#define dbgd(s) /**/
#define dbgc(s) /**/
#define dbgx(x) /**/
#define dbgxl(x) /**/
#define dbgxw(w) /**/
#define eol /**/
#define eor	/**/

#define assert(x) /**/

#endif
#endif
