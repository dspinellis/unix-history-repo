/*
 * Keymap utility, 
 * - loads a specific national keyboard mapping into the
 *   keyboard driver
 * - displays the current mapping
 * - sets/displays key repetition rate
 * - enables/disables screensaver
 *
 * Contributed to 386bsd 0.1 and later versions
 *
 *	Copyright 1992,1993 by Holger Veit
 *	May be freely used with Bill Jolitz's port of 
 *	386bsd and may be included in a 386bsd collection
 *	as long as binary and source are available and reproduce the above
 *	copyright.
 *
 *	You may freely modify this code and contribute improvements based
 *	on this code as long as you don't claim to be the original author.
 *	Commercial use of this source requires permittance of the copyright 
 *	holder. A general license for 386bsd will override this restriction.
 *
 *	Use at your own risk. The copyright holder or any person who makes
 *	this code available for the public (administrators of public archives
 *	for instance) are not responsible for any harm to hardware or software
 *	that might happen due to wrong application or program faults.
 *
 * You must have the codriver driver in the same package generated
 * into the 386bsd kernel, otherwise this program does not work.
 *
 *	@(#)keymap.c	1.1 (386bsd contribution) 24-oct-92
 */
/* Oh, this has become a hack, this could have been done better. hv */

/*
 * -hv- Holger Veit
 *-vak- Serge Vakulenko
 *
 * 29/07/92 -hv- First version
 * 24/10/92 -hv- fixes + screensaver
 * 01/12/92-vak- -T and -a flags, new output format,
 *               new keycap extensions for Meta and ShiftAltgr keys.
 * 04/27/93 -hv- extensions for special function keys
 *
 */

 /*	Usage:
  *
  *	keymap [-l] [-t+|-t-] [-d#] [-r#] [-m mapping]
  *  or
  *     keymap -T
  *  or
  *     keymap -a "keydef" keyid
  *
  *	-l	list the current key mapping
  *	-t+	enable key repetition (typematic)
  *	-t-	disable key repetion
  *	-d#	set delay after which a key is repeated (includes -t+)
  *	-r#	set rate of repetition of keys (includes -t+)
  *	-s#	set screensaver timeout (in seconds, 0 to disable)
  *	-m mapping	load a key map from the keycap file
  *	-T	key testing
  *
  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/ioctl_pc.h>
#include "pathnames.h"

extern int	kgetent(char*, char*);
extern int	kgetnum(char*);
extern int	kgetflag(char*);
extern char	*kgetstr(char*,char**);

extern	char	*optarg;
extern	int	optind;
extern	int	opterr;

int	kbd = -1;
char	*progname;
struct  kbd_ovlkey      kmap[128];

main(int argc,char *argv[]) 
{

	int	lf = 0,
		Tf = 0,
		tf = 0,
 		af = 0,
		df = 0, delay = -1,
		rf = 0, rate = -1,
		sf = 0, stime = 0,
		mf = 0;
	char	*map;
	int	c;
	
	progname = argv[0];
	
 	while ((c=getopt(argc,argv,"lTat:d:r:m:s:"))!=EOF) {
		
		switch(c) {
		case 'l':
			lf++;
			break;
		case 'T':
			Tf++;
			break;
		case 'a':
			af++;
			break;
		case 't':
			tf = *optarg;
			break;
		case 'd':
			df++;
			tf='+';
			delay = atoi(optarg);
			break;
		case 'r':
			rf++;
			tf='+';
			rate = atoi(optarg);
			break;
		case 'm':
			mf++;
			map = argv[optind-1];
			break;
		case 's':
			sf++;
			stime = atoi(optarg);
			break;
		case '?':
		default:
			usage();
		}
	}

	if ((lf+tf+df+rf+mf+sf+Tf+af)==0)
		usage();
	if (Tf && (lf+tf+df+rf+mf+sf+af)!=0)
		usage();
	if (af) {
		if ((lf+tf+df+rf+mf+sf+Tf)!=0)
			usage();
		if (optind >= argc - 1)
			usage();
	}

	/* we use /dev/kbd here, because this will block if the keymap
	 * command is tried from an xterm. Use xmodmap/xset for these
	 * functions
	 */
	if ((kbd=open(_PATH_KEYBOARD, 0)) < 0) 
		fatal("Open keyboard");

	if (Tf) keynum();
	else if (af) assign (argv[optind], argv[optind+1]);
	else {
		if (lf) loadkmap();
		if (tf || df || rf) tpm(tf,delay,rate);
		if (mf) mapkey(map);
		if (sf) saver(stime);
	}
	
	close(kbd);

	if (lf) list();

	exit(0);
}				

fatal(char *msg) 
{
	fprintf(stderr,"%s: ",progname);
	perror(msg);
	exit(1);
}

error(char *msg)
{
	fprintf(stderr,"%s: %s\n",progname,msg);
	exit(1);
}

usage() 
{
	fprintf (stderr, "Usage:\n");
	fprintf (stderr, "\t%s [-l] [-t+|-t-] [-d#] [-r#] [-s#] [-m mapping]\n",
		progname);
	fprintf (stderr, "\t  -l     list the current keyboard map\n");
	fprintf (stderr, "\t  -t[+-] enable/disable key repetition\n");
	fprintf (stderr, "\t  -d#    set key repetition delay, 0..3 * 250ms\n");
	fprintf (stderr, "\t  -r#    set key repetition rate, 0..31, 0 fastest\n");
	fprintf (stderr, "\t  -s#    set screen saver delay, in seconds\n");
	fprintf (stderr, "\t  -m ent remap keyboard according to the keycap(5) entry\n");
	fprintf (stderr, "or\n\t%s -T\n", progname);
	fprintf (stderr, "\t         test key codes\n");
	fprintf (stderr, "or\n\t%s -a \"keydef\" keyid\n", progname);
	fprintf (stderr, "\t         remap the single keyid, F1..F12, KP0..KP9, KP+..KP/, or 0xNUM\n");
	fprintf (stderr, "\t         with zero or one prefix: SHIFT-, CTRL-, ALTGR-, META\n");
	exit(1);
}

/* for regular keys */
static char *type2str[] = {
/*0*/	"none    ",
/*1*/	"shift   ",
/*2*/	"meta    ",
/*3*/	"numlock ",
/*4*/	"ctrl    ",
/*5*/	"capslock",
/*6*/	"ascii   ",
/*7*/	"scroll  ",
/*8*/	"function",
/*9*/	"keypad  ",
/*10*/	"break   ",
/*11*/	"altgr   ",
/*12*/	"shftlock",
/*13*/  "altgrlck",
};

/* convert to escseq */
char *trl(XCHAR *s)
{
#if XCHAR != u_char
ERROR! FIX ME!
#endif
	static char s1[4*16+1];
	static char  s2[5];
	int i;

	s1[0] = 0;
	for (i=0; s[i]; i++) {
		if (s[i]>' ' && s[i]<0177) {
			s2[0] = s[i];
			s2[1] = 0;
		} else if ((s[i] & 0xff) < ' ')
			sprintf(s2, "^%c", s[i]+'@');
		else
			sprintf(s2, "\\%03o", s[i] & 0xff);
		strcat(s1,s2);
	}
	return s1;
}

loadkmap()
{
	int	i;
	struct  kbd_ovlkey      *ke;

	/* read the keys */
	for (i=0, ke=kmap; i<128; ke++,i++) {		
		ke->keynum = i;
		if (ioctl(kbd,KBDGCKEY,ke)<0) 
			fatal("Get kbd code");
	}
}	
	
list()
{
	int	i;
	struct  kbd_ovlkey      *ke;
	int	has_altgr= 0,
		has_shftlock = 0;
	u_short	kt;
#if XCHAR != u_char
ERROR! FIX ME!
#endif
	char	*p;

	/* scan the saved keys */
	for (i=0, ke=kmap; i<128; ke++,i++) {		
		switch (ke->type & KBD_MASK) {
		case KBD_SHFTLOCK:
			has_shftlock = i;
			break;
		case KBD_ALTGR:
		case KBD_ALTGRLOCK:
			has_altgr = i;
			break;
		}
	}

	/* get the entry of key 0. This is a nonexisting key for some	
	 * reasons, so we can use it to hold the current keyboard name
	 */
	if (kmap[0].unshift[0])
		printf("Current mapping is: %s\n",kmap[0].unshift);

	printf("  #  type  unshift    shifted      ctrl        meta");
	if (has_altgr)
		printf("      altgr   shiftaltgr");
	printf("\n");
		
	for (i=1, ke=&kmap[1]; i<128; ke++,i++) {
		kt = ke->type;		
		if (kt) {
			printf("%3d%c %8s", i, (kt & KBD_OVERLOAD) ? '*' : ' ',
				type2str[kt & KBD_MASK]);
			printf("%c%-10s", (kt & KBD_DOCAPS) ? '~' : ' ',
				trl (ke->unshift));
			printf(" %-10s", trl (ke->shift));
			if (ke->ctrl [0])
				p = trl (ke->ctrl);
			else if ((kt&KBD_MASK) == KBD_ASCII ||
			    (kt&KBD_MASK) == KBD_FUNC)
				p = "^@";
			else
				p = "";
			printf(" %-10s", p);
			if (ke->meta [0])
				p = (char*)trl (ke->meta);
			else if ((kt&KBD_MASK) == KBD_ASCII ||
			    (kt&KBD_MASK) == KBD_FUNC) {
				XCHAR buf [KBDMAXOVLKEYSIZE+1];
				XC_strcpy (buf, ke->unshift);
				buf[0] ^= 0x80;
				p = (char*)trl (buf);
			} else
				p = "";
			if (has_altgr && ke->altgr [0]) {
				printf(" %-10s", p);
				printf("%c%-10s", (kt & KBD_DOALTCAPS) ? '~' : ' ',
					(char*)trl (ke->altgr));
				printf(" %s", trl (ke->shiftaltgr));
			} else
				printf(" %s", p);
			putchar('\n');
		}
	}
}	
	
tpm(int onoff, int delay, int rate)	
{
	int	current_tpm;
	int	new_tpm;
	int	tpm_sw;
	
	if (onoff == '-') {
		tpm_sw = KBD_REPEATOFF;
		if (ioctl(kbd,KBDSREPSW,&tpm_sw)<0)
			fatal("Set key repetition OFF");
		return;
	} 
	else if (onoff=='+') {
		tpm_sw = KBD_REPEATON;
		if (ioctl(kbd,KBDSREPSW,&tpm_sw)<0)
			fatal("Set key repetition ON");
	}
	else
		usage();

	/* do we need to set new values */
	if (delay == -1 && rate == -1) return;

	/* get current setting */
	if (ioctl(kbd,KBDGTPMAT,&current_tpm)<0)
		fatal("Get typematic value");
	
	delay = (delay == -1) ? (current_tpm & 0x60) : ((delay & 0x3)<<5);
	rate = (rate == -1) ? (current_tpm & 0x1F) : (rate & 0x1F);
	
	new_tpm = delay | rate;

	/* set the new setting */
	if (ioctl(kbd,KBDSTPMAT,&new_tpm)<0)
		fatal("Set typematic value");

	return;
}

saver(int stime)
{
	int fv;

	if ((fv=open(_PATH_VIDEO,0)) <= 0)
		fatal("Open video device");

	if (ioctl(fv,VGASBLANK,&stime)<0)
		fatal("Set timeout value");

	close(fv);
}

char		keyflag[128];

checkdupandset(k)
{
	if (keyflag[k])
		error("Duplicate key def");
	keyflag[k]=1;
}

mapkey(char *map) 
{
    struct kbd_ovlkey	key;
    char		cap[1024];
    int			i,k,n,setflag;
    char		code[5];
    char		*ap1;
    char		diac[9];
    int			m0flag=0,a0flag,c0flag,ledflag,clflag;
    XCHAR		*ap;

#define NSTANDARD       7
    struct {
	XCHAR	*addr;
	char	ch;
	char	modifier;
    } standard[] = {
	0,			'D',	KBD_NOEXT,
	&key.unshift[0],	'K',	KBD_EXT_N,
	&key.shift[0],		'S',	KBD_EXT_S,
	&key.ctrl[0],		'C',	KBD_EXT_C,
	&key.meta[0],           'M',	KBD_EXT_SK,
	&key.altgr[0],          'A',	KBD_EXT_A,
	&key.shiftaltgr[0],     'X',	KBD_EXT_CA,
    };				

#define NSPECIAL	4
    struct {
	u_short	typ;
	char	ch;
    } special[] = {
	KBD_META,	'm',
	KBD_ALTGR,	'l',
	KBD_SHIFT,	'h',
	KBD_CTL,	't'
    };

#define NLOCKKEYS	5
    struct	{
	char	*ch;
	u_short	typ;
    } lockkeys[] =
    {
	"ca",	KBD_CAPS,
	"sh",	KBD_SHFTLOCK,
	"nl",	KBD_NUM,
	"sc",	KBD_SCROLL,
	"ag",   KBD_ALTGRLOCK
    };

    /* try to find the entry */
    switch(kgetent(cap,map)) {
    case -1:
	error("Keycap database not found");
    case 0:
	error("No such keymap entry");
    }

    /* set default mapping */
    if (ioctl(kbd,KBDDEFAULT)<0) 
	error("Cannot reset to default mapping");
	
    /* load fonts */
    loadfonts();

    /* DE flag present? */
    if (kgetflag("de")) return;

    /* C0 flag */
    c0flag = kgetflag("c0");
	
    /* A0 flag */
    a0flag = kgetflag("a0");

    /* Caps LED assignments */
    ledflag = kgetnum("la");
    if (ledflag == -1)
	ledflag = 0;
    ledflag |= KBD_CAPSINIT;

    for (i=0; i<128; i++) keyflag[i] = 0;

    /* check for locking keys */
    for (k=0; k<NLOCKKEYS; k++) {
	sprintf(code,"%s",lockkeys[k].ch);
	n = kgetnum(code);
	if (n > 0) {
	    checkdupandset(n);
	    key.keynum = n;
	    key.type = lockkeys[k].typ;
	    key.shift[0] = 0;
	    key.ctrl[0] = 0;
	    key.altgr[0] = 0;
	    key.shiftaltgr[0] = 0;
	    if (ioctl(kbd,KBDSCKEY,&key)<0)
		error("Cannot set lockkey");
	}
    }

    /* check for special keys */
    for (k=0; k<NSPECIAL; k++) {
	for (i=1; i<10; i++) {
	    sprintf(code,"%c%d",special[k].ch,i);
	    n = kgetnum(code);
	    if (n >= 0) {
		checkdupandset(n);
		key.keynum = n;
		key.type = special[k].typ;
		if (ioctl(kbd,KBDSCKEY,&key)<0) 
		    error("Cannot set specialkey");
	    }
	}
    }

    /* check diacritical flags */
    for (i=0; i<9; i++) {
	sprintf(code,"p%d",i+1);
	n = kgetnum(code);		
	if (n>0) diac[i] = n;
	else diac[i] = 0;
    }

    /* handle and collect standard keys */
    for (i=1; i<128; i++) {
    	setflag = 0;
	key.keynum = i;
	if (ioctl(kbd,KBDGOKEY,&key)<0)
	    error("Cannot get key setting");

	/* XXX this will defeat the different semantic of 
	 * KBD_KP, KBD_ASCII, etc. Will be corrected some day
	 */
	key.type &= ~KBD_MASK;
	key.type |= KBD_ASCII;
	for (k=0; k<9 && diac[k]; k++)
	    if (diac[k]==i) {
		key.type |= KBD_DIACPFX;
		break;
	    }
		
	for (k=0; k<NSTANDARD; k++) {
	    sprintf(code,"%c%d",standard[k].ch,i);
	    if (k==0 && kgetflag(code)) {
		/* delete a key */	
		key.type = KBD_NONE;
		setflag = 1;
		goto setit;
	    } else
	    {	
		ap = standard[k].addr;
#if XCHAR==u_char
		if (ap1=kgetstr(code,&(char*)ap)) {
#else
ERROR! FIX ME!
#endif
		    if (strlen(ap1)>KBDMAXOVLKEYSIZE)
			error("String too long in keymap");
		    ap = standard[k].addr;

		    /* process alphalock flag */
		    if (*ap=='~') {
			if (ap==key.unshift) {
			    XC_strcpy (ap, ap+1);
			    key.type |= KBD_DOCAPS;
			} else if (ap==key.altgr) {
			    XC_strcpy(ap, ap+1);
			    key.type |= KBD_DOALTCAPS;
			}
		    /* process special hotkeys */
		    } else if (*ap=='?') {
			int modifier;
			int func;
			struct kbd_hotkey s;
			
			/* get the function */
			switch (ap[1]) {
			case '-': func = KBD_HOTKEYDELETE; break;
			case 'R': func = KBD_RESETKEY; break;
			case 'D': func = KBD_DEBUGKEY; break; 
			case 'P': func = KBD_VTYDOWN; break;
			case 'N': func = KBD_VTYUP; break;
			case 'B': func = KBD_VTY11; break;
			case 'A': func = KBD_VTY10; break;
			default:
			    if (ap[1]>='0' && ap[1]<=9)
			        func = ap[1]-'0';
			    else continue;
			}
			/* convert the layer to trigger */
			if (modifier) modifier = standard[k].modifier;
			s.key = i;
			s.modifier = modifier;
			s.function = func;
			if (ioctl(kbd,KBDSSPECF,&s) <0)
				error("KBDSSPECF ioctl failed");
			continue;
		    }
		    setflag = 1;
		}
	    }
	}
			
setit:	if (setflag) {
	    checkdupandset(i);
	    if (! key.meta [0]) {
		key.meta [0] = key.unshift [0] ^ 0x80;
		key.meta [1] = 0;
	    }
	    if (ioctl(kbd,KBDSCKEY,&key)<0) 
		error("Cannot set stdkey");
	}
    }

    /* set the behavior for unassigned keys (clear layer) */
    clflag = a0flag | (c0flag<<1) | (m0flag<<2);
    if (ioctl(kbd,KBDSCLRLYR,&clflag) < 0)
	error("KBDSCLRLYR ioctl failed");
 
    /* now mark the current keyboard setting in the 0 entry */
    key.keynum = 0;
    key.type = KBD_NONE;

    XC_strncpy(key.unshift,map,KBDMAXOVLKEYSIZE);
    key.unshift[KBDMAXOVLKEYSIZE] = 0;
    key.shift[0] = 0;
    key.ctrl[0] = 0;
    key.altgr[0] = 0;
    key.shiftaltgr[0] = 0;

    if (ioctl(kbd,KBDSCKEY,&key)<0)
	error("Cannot set keymap code");

    if (ioctl(kbd,KBDSCAPSLED,&ledflag)<0)
	error("Cannot assign LEDs");
}

loadfonts()
{
	char *s,fp[255];
	int n;
	
	s = fp;
	if (kgetstr("fn0",&s))
		if (setfont(fp,1,0)) return;
	s = fp;
	if (kgetstr("fn1",&s))
		if (setfont(fp,1,1)) return;
}

/*
 * simple program to test KEY NUMBERS 
 *
 * leave this program by hitting the ESC key twice, followed by the SPACE bar
 */

/* should be fixed on all keyboards */
#define IBMKEY_ESC	110
#define IBMKEY_SPACE	61

keynum ()
{
	int state = 0;
	char c;
	
	printf ("Key number test.\n");
	printf ("Press different combinations of keys.\n");
	printf ("Leave program with sequence ESC,ESC,SPACE (with release).\n");
	for(;;) {
		if (read (kbd, &c, 1) != 1)
			continue;
		printf ("Got key %02x %s\n", c & 0xff,
			c & 0x80 ? "(release)" : "");
		if (c & 0x80) continue;
		c &= 0x7f;
		switch (c) {
		case IBMKEY_ESC:
			if (state < 2) /* ESC */
				state++;
			else
				state = 0;
			break;
		case IBMKEY_SPACE:
			if (state == 2)
				return;
		default:
			state = 0;
			break;
		}
	}
}

#define SHIFTFLAG	0x0100
#define CTRLFLAG	0x0200
#define ALTFLAG		0x0400
#define SHIFTALTFLAG    0x0800
#define METAFLAG        0x1000

/* The code in codrv uses a double \0\0 to identify a NULL byte to return
 * from a pressed key, but we want to have nothing for this key, and
 * don't want to disable this key entirely
 */
static XCHAR notanullbyte[] = { 0,-1,0 };
#define NOTANULLBYTE	notanullbyte

char *parse_bsl(s)
	char *s;
{
	char *p,*q;
	int n,i;
	static char buf [16];
	
	q = buf;
	for (p=s; *p; p++) {
		if (*p != '\\') 
			*q++ = *p;
		else {	/* start parsing backslash */
			p++;
			switch (*p) {
			case 'n':					
				n = '\n';
				break;
			case 't':
				n = '\t';
				break;
			case 'f':
				n = '\f';
				break;
			case 'b':
				n = '\b';
				break;
			case '\\':
				n = '\\';
				break;
			case '\'':
				n = '\'';
				break;
			case '\"':
				n = '\"';
				break;
			/* could add some more here */

			default:
				for (i=n=0; i<3; i++,p++) {
					if (*p>='0' && *p<='9') {
						n *= 8;
						n += *p - '0';
					} else break;
				}
				--p;
			}
			*q++ = n;
		}
	}
	*q = 0;
	return (buf);
}				

assign(keydef,keycode)
	char *keydef;
	char *keycode;
{
	int 			keynum;
	int 			defflag=0,delflag=0;
	struct kbd_ovlkey	keynow,keyorg;

	if (strlen(keydef) > 15) {
		fprintf(stderr,"%s: String \"%s\" too long\n",
			progname,keydef);
		exit(1);
	}

	keynum = def2num(keycode);
	if (keynum<0) {
		fprintf(stderr,"%s: No such key: \"%s\"\n",
			progname,keycode);
		exit(1);
	}

	/* check whether the key should be deleted or returned to default */
	if (!strcmp(keydef,"DEFAULT")) defflag = 1;
	else if (!strcmp(keydef,"DELETE")) delflag = 1;

	keydef = parse_bsl(keydef);

	keynow.keynum = keyorg.keynum = keynum & 0x7F;
	if (ioctl(kbd,KBDGCKEY,&keynow)<0 ||
	    ioctl(kbd,KBDGOKEY,&keyorg)<0) {
		perror("KBDG*KEY");
		exit(1);
	}

	switch(keynum&0x7F00) {
	case SHIFTFLAG:
		if (delflag)
			XC_strncpy(keynow.shift,NOTANULLBYTE,3);
		else if (defflag) 
			XC_strcpy(keynow.shift,keyorg.shift);
		else
			XC_strcpy(keynow.shift,keydef);
		break;
	case METAFLAG:
		if (delflag)
			XC_strncpy(keynow.meta,NOTANULLBYTE,3);
		else if (defflag)
			XC_strcpy(keynow.meta,keyorg.meta);
		else
			XC_strcpy(keynow.meta,keydef);
		break;
	case CTRLFLAG:
		if (delflag)
			XC_strncpy(keynow.ctrl,NOTANULLBYTE,3);
		else if (defflag)
			XC_strcpy(keynow.ctrl,keyorg.ctrl);
		else
			XC_strcpy(keynow.ctrl,keydef);
		break;
	case ALTFLAG:
		if (delflag)
			XC_strncpy(keynow.altgr,NOTANULLBYTE,3);
		else if (defflag) 
			XC_strcpy(keynow.altgr,keyorg.altgr);
		else
			XC_strcpy(keynow.altgr,keydef);
		break;
	case SHIFTALTFLAG:
		if (delflag)
			XC_strncpy(keynow.shiftaltgr,NOTANULLBYTE,3);
		else if (defflag)
			XC_strcpy(keynow.shiftaltgr,keyorg.shiftaltgr);
		else
			XC_strcpy(keynow.shiftaltgr,keydef);
		break;
	case 0:
		if (delflag)
			XC_strncpy(keynow.unshift,NOTANULLBYTE,3);
		else if (defflag) 
			XC_strcpy(keynow.unshift,keyorg.unshift);
		else
			XC_strcpy(keynow.unshift,keydef);
	}

	if (ioctl(kbd,KBDSCKEY,&keynow)<0) {
		perror("KBDSCKEY");
		exit(1);
	}
}

struct xlattable {
	char	*str;
	int	num;
} tbl [] = {
	/* std. */      /* X11 naming */
	"f1",           112,
	"f2",           113,
	"f3",           114,
	"f4",           115,
	"f5",           116,
	"f6",           117,
	"f7",           118,
	"f8",           119,
	"f9",           120,
	"f10",          121,
	"f11",          122,
	"f12",          123,
	"kp0",          99,             "kp_0",         99,
	"kp1",          93,             "kp_1",         93,
	"kp2",          98,             "kp_2",         98,
	"kp3",          103,            "kp_3",         103,
	"kp4",          92,             "kp_4",         92,
	"kp5",          97,             "kp_5",         97,
	"kp6",          102,            "kp_6",         102,
	"kp7",          91,             "kp_7",         91,
	"kp8",          96,             "kp_8",         96,
	"kp9",          101,            "kp_9",         101,
	"kp.",          104,            "kp_decimal",   104,
	"kp+",          106,            "kp_add",       106,
	"kp-",          105,            "kp_subtract",  105,
	"kp*",          100,            "kp_multiply",  100,
	"kp/",          95,             "kp_divide",    95,
	"shift",	SHIFTFLAG,
	"ctrl",		CTRLFLAG,
	"meta",         METAFLAG,
	"altgr",	ALTFLAG,
	"shiftaltgr",   SHIFTALTFLAG,

	/* and some misspellings: */
	"sh",		SHIFTFLAG,
	"ctl",		CTRLFLAG,
	"^",		CTRLFLAG,
	"alt",		ALTFLAG,
	"shalt",        SHIFTALTFLAG,
	"meta",		ALTFLAG,
	"shmeta",       SHIFTALTFLAG,

	/* must be last */
	0,		0,
};

int def2num(keycode)
	char *keycode;
{
	int	i,k;
	char 	*token,*strtok(),*ep;
	int	keynum = 0;
	int	flag1 = 0,flag2 = 0;

	/* look for '-' */
	token = strtok(keycode,"-");
	while (token) {
		k = strtol(token,&ep,0);
		if (ep!=token) { /* got a number */
			keynum |= k;
			flag2++;
			break;
		}
		/* reserved word? */
		for (i=0; tbl[i].str; i++) {
			if (!strcasecmp(keycode,tbl[i].str)) {
				keynum |= tbl[i].num;
				if (tbl[i].num < 128) flag2++;
				else flag1++;
			}
		}
		token=strtok(NULL,"-");
	}		
	
	/* invalid */
	if (flag1 > 1 || flag2 != 1) return -1;
	if ((keynum & 0x7F)==0) return -1;

	return keynum;
}

