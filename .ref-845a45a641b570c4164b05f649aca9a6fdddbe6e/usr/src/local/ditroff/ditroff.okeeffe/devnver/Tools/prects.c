#ifndef lint
static char sccsid[]="@(#)prects.c	1.1	(CWI)	87/07/16";
#endif lint
/*
 * read the fontdir/char.rle files from cdata output to produce the 
 * rectangular data for a font.
 */

#include <sys/types.h>
#include <sys/dir.h>
#include <sys/file.h>
#include <stdio.h>
#include <strings.h>

#include "../defs.h"
#include "huff7.h"
#include "huff14.h"
#include "Ptable.h"

extern long lseek();

extern Word	huff7[];
extern Word	huff14[];
extern Byte	Ptable[];

/*
 * bit map will be at most 1024 by 1024 points
 *
 * The right answer would be to compact the data in 
 * bytes, but I'm just going to hack this... (DD)
 */

#define MAXnat	1024
#define MAX	128
typedef unsigned short nat;

Byte bitmap[MAX][MAXnat], 		/* `real' binary data		*/
     newbitmap[MAXnat][ MAXnat];	/* expanded data in chars	*/
Byte sqmap[MAXnat][MAXnat], 
     prmap[MAXnat][MAXnat], 
     uline[MAXnat];

FILE *fd;
int eoc = 0;	/* signals end of a character */

/*
 * storage of 3 previous scan lines plus current working scan line
 * 6 extra points (always of) for begin of end of scanline for predict()
 */

Byte a[1030], b[1030], c[1030], d[1030];
	/* pointers to the scanline storage */
Byte *sl0 = &a[3], *sl1 = &b[3], *sl2 = &c[3], *sl3 = &d[3];

#define EVER	;;

/*
 * Opcodes for the huffman decoder
 */

#define OPCODE	0300
#define FINAL	0
#define POINT	0200
#define SUFFIX	0100

/*
 * address bits for pointer (huffman decode)
 */

#define ADDRS	077
#define SUFDAT	017

/*
 * Run length value types
 */

#define DUMP	1
#define X0	2
#define Y0	3
#define LMAX	4
#define RUNL	5

#define Charmax 128

struct Header head;
struct Chars Char[Charmax];
struct Rect rect;

struct Rect buf[BUFSIZ];		/* global buffer to hold rects */
struct Rect *bob = buf;			/* pointer to start of buffer */
struct Rect *eob = &buf[BUFSIZ-1];	/* pointer to end of buffer */
struct Rect *curp = buf;		/* current buffer pointer */
long offset;				/* offset in file */


/*
 * We start with significant bit
 */

#define BITMASK	0100000L

#define dbprintf if(debug)printf
int debug = 0;


#define MAXFNAME 30
char filename[MAXFNAME];
int fdo; 			/* file descriptor for output file */

main(argc, argv)
int argc;
char **argv;
{
	int i, j;
	DIR *dir;
	char  outfile[BUFSIZ];	/* name for output file */
	struct direct *dirbuf;
	char *file, *directory;
	Word *type, *gethuff();
	int k;  		/* character we are on */
	char *p;

	argv++;
	while( --argc && *argv[0] == '-') {
		
		switch((*argv)[1]) {
			case 'd':
				debug++;
				break;
			case 'o':
				sprintf(outfile,"%s", (*argv)+2);
				break;
			default:
				error("Unknown option %s", *argv);
		}
		argv++;
	}

	if(argc < 1)
		error("No data specified");

	while(argc--) {
		directory = rindex(*argv, '/');
		if( directory == (char *)0)
			directory = *argv;
		else
			directory++;

		if((dir = opendir(*argv)) == NULL)
			error("Can't open directory %s", *argv);
		argv++;
		if(sscanf(directory, "%d-%d", &j, &i) == 0)
			error("scanf error");
		type = gethuff(i);
		if(outfile[0] == NULL)
			sprintf(outfile,"%s.rect", directory);
		dbprintf("Output to file %s\n", outfile );
		if((fdo = open(outfile,O_WRONLY | O_CREAT | O_TRUNC, 0644)) == -1)
			error("open error %s\n", outfile);
		head.magic = MAGIC;
		bcopy(directory, head.name,strlen(directory));
		headit();
		setchars();
		setoffset();
		for(dirbuf = readdir(dir); dirbuf != NULL; dirbuf = readdir(dir) ) {
			if(strcmp((file = dirbuf->d_name), ".") == 0 ||
				strcmp(file, "..") == 0)
				continue;
			sprintf(filename,"%s/%s",directory,file);
			p = file;
			while(*p++)
				;
			p -= 5;
			if(strcmp(p, ".rle") != 0){
				fprintf(stderr, "strange file %s, skipped\n",  filename);
				continue;
			}
			sscanf(file, "%o", &k);
			if(k >  Charmax) {
				fprintf(stderr,"Wierd Character  %s\n", filename);
				continue;
			}
			chardecode(type, k-1);
			cleanup();
		}
		flusho();
		setchars();
	}
}


/*
 * gethuff:
 *	get the huff value from the directory name.
 */

Word *
gethuff(mcode)
int mcode;
{
	Word *huff;

	switch(mcode) {
		case MSC1:
		case MSC2:
			huff = huff7;
			break;
		case MSC3:
			huff = huff14;
			break;
		default:
			error("Unknown master code %#o\n", mcode);
	}
	return huff;
}


/*
 * chardecode:
 *	decode the encode character date in gcd of
 *	pointsize code mcode
 */

int X, Y, Lmax;	/* the offsets */
int curx, cury;
int endline;

chardecode(huff, charno)
int charno; Word *huff; 
{
	int runl;

	(void) getbit(1);	/* reset the getbit routine */

	curx = Char[charno].Relwidth =  getnbits(8);	/* ignore the first 8 bits */
	curx = X = Char[charno].XO = huffdecode(huff);
	cury = Y = Char[charno].YO = huffdecode(huff);
	Lmax = Char[charno].Lmax = huffdecode(huff);

	/*
	 * Lmax 18 means 17 dots, so y should go from
	 * Y to Lmax -1 ????
	 */
	endline = Y + Lmax - 1 ;

	while(!eoc) {	
		for( cury = Y ; cury <= endline; ) {
			runl = huffdecode(huff);
			if(!runl) {		/* End of Line */
				predict(cury, endline - cury, 1);
				cury = endline;
				break;
			}
			else {
				predict(cury, runl, 0);
				cury += runl;
				if(cury >= endline)
					break;
			}
		}
		cury =  Y ;
		storescanline(curx, cury, endline);
		swapscanp();
		curx++;
	}
	Char[charno].X = curx - X ;
	Char[charno].Y = (Lmax + Y - 2) - Y;
	Char[charno].offset = offset;
	setbitmap(X, Y, curx, Lmax + Y - 2);
	maxsq();
	prune();
	Char[charno].Nstructs =	combi();
	dbprintf("The next offset is %ld\n", offset);	
	dbprintf("@ End of character data (%d)\n", charno);
}


/*
 * huffdecode
 *
 * returns the runlength of the Character Generation Data
 * using huffman decode table huff.
 */

huffdecode(huff)
Word *huff;
{
	register Word data = 0;
	register tmp;
	register int suffix;

	for(EVER) {
		switch(data & OPCODE) {
			case FINAL:
				if(data == 0) {
					tmp = (*huff | getbit(0)) & ADDRS;
					data = *(huff + tmp);
					if(data == 0 )
						return(0);
				} else {
					tmp = data & ~FINAL;
					return(tmp);
				}
				break;
			case POINT:
				tmp =  (data | getbit(0)) & ADDRS;
				data = *(huff + tmp);
				break;
			case SUFFIX:
				tmp = data & SUFDAT;
				suffix = getnbits(tmp);
				if(!suffix)
					eoc++;
				return(suffix);
			default:
				error("Unknown opcode %#o\n", data);
		}
	}
}


/*
 * get the value of n bits from the gcd
 */

getnbits(n)
int n;
{
	register int i;
	register int j;
	unsigned int value = 0;

	for(i = n; i > 0; i-- ) {
		j = getbit(0);
		value = (value << 1) | j;
		if( i > sizeof(value) * 8)
			error("Overflow in getnbits(%d)\n", i);
	}
	return(value);
}


/*
 * return a bit from the character generation data
 *
 * initialise when argument is set
 */

getbit(init)
int init;
{
	static bitno;
	static unsigned int mask;
	static unsigned int n;
	register int bit;
	register int k;

	if(init) {
		bitno = 1;
		if((fd = fopen(filename, "r")) == NULL )
			error("Cannot open %s", filename);
		return 0;
	} else {
		if( (bitno - 1) % 16 == 0) {
			bitno = 1;
			if(( k = fread( (char *)&n, sizeof(Word), 1, fd)) != 1)
				error("Read error %d", k);
			mask = BITMASK;
		}
	}

	bit = n & mask;
	bitno++;
	mask = mask >> 1;
	if(bit) {
		return(1);
	} else {
		return(0);
	}
}


/*
 * predict:
 * predicts the dot on position x, y, over a runlength r.
 * if 3th argument is set, don't generate exception point.
 */

#define P8192	020000
#define P4096	010000
#define P2048	004000
#define P1024	002000
#define P0512	001000
#define P0256	000400
#define P0128	000200
#define P0064	000100
#define P0032	000040
#define P0016	000020
#define P0008	000010
#define P0004	000004
#define P0002	000002
#define P0001	000001	

#define	ON	1
#define OFF	0


predict(y, r, e)
register int y;
int e, r;
{
	unsigned int same = 0;
	unsigned register int mask = 0;
	unsigned register int state = 0;
	unsigned register int i;
	unsigned int prev = 0, new = 0, except = 0;
	extern unsigned int getmask();

	i = r;
	do {
		state = except = prev = 0;
		mask = getmask(y);
		if(mask & P8192) {
			mask ^= 017777;
			prev = 1;
		}
		mask &= 017777;
		same = getdot(mask);
		if( i == 1 && e == 0) {	/* exception point */
			except = 1;
		}
		state = except;
		state |= prev << 1;
		state |= same << 2;
		switch(state & 07) {
			case 0:
			case 3:
			case 5:
			case 6:
				new = ON;
				break;
			case 1:
			case 2:
			case 4:
			case 7:
				new = OFF;
				break;
			default:
				error("Unexpected state %#o\n", state);
		}
		storedot( new, y );
		y++;
	} while (--i);
}

/*
 * find wether the dot should be the same or not
 */

#define PMASK	017774
#define TWOBIT	03


getdot(value)
unsigned int value;
{
	register int tmp, i, j, k;

	i = (value & PMASK) >> 2;
	j = value & TWOBIT;
	if(i > sizeof(Ptable))
		error("Prom adressing error");

	tmp = Ptable[i];
	k = (tmp >> j) & 1;
	return k;
}


/*
 * store point in current working area
 */

storedot( dot, y)
register unsigned int dot;
register int y;
{
	if(y > Lmax + 2 + Y )
		error("Out of range in store dot, y = %d", y);

	if(y == endline -1)
		return;
	sl0[y] = dot;
}


/*
 * construct the predict mask for position x, y
 */

unsigned int
getmask(y)
register int y;
{
	register unsigned int mask = 0;

	if( y < 3 || y > 1029)
		error("Out of range in getmask, y = %d\n", y);

	if( sl3[y+2] )		/* PROM 1    */
		mask |= P0001;
	if( sl3[y+1] )
		mask |= P0002;
	if( sl3[y-1] )		/* PROM 4    */
		mask |= P0004;
	if( sl3[y-2] )		/* PROM 8    */
		mask |= P0008;
	if( sl2[y+3] )		/* PROM 16   */
		mask |= P0016;
	if( sl2[y+1] )		/* PROM 32   */
		mask |= P0032;
	if( sl2[y-1] )		/* PROM 64   */
		mask |= P0064;
	if( sl2[y-3] )		/* PROM 128  */
		mask |= P0128;
	if( sl1[y+2] )		/* PROM 256  */
		mask |= P0256;
	if( sl1[y+1] )		/* PROM 512  */
		mask |= P0512;
	if( sl1[ y ] )		/* PROM 1024 */
		mask |= P1024;
	if( sl1[y-1] )		/* PROM 2048 */
		mask |= P2048;
	if( sl1[y-3] )		/* PROM 4096 */
		mask |= P4096;
	if( sl0[y-1] )		/* PROM 8192 */
		mask |= P8192;
	return(mask);
}


/*
 * swap the scan line buffers
 */

swapscanp()
{
	register Byte *sav;

	/*
	 * swap the buffers
	 */
	sav = sl3;
	sl3 = sl2;
	sl2 = sl1;
	sl1 = sl0;
	sl0 = sav;

}


cleanup()
{
	register int i;
	register int j;

	for( i = 0; i < 1030; i++)
		a[i] = b[i] = c[i] = d[i] = 0;
	sl0 = &a[3]; 
	sl1 = &b[3];
	sl2 = &c[3];
	sl3 = &d[3];
	for( i = 0; i < MAXnat; i++)
		for( j = 0; j < MAXnat; j++) {
			newbitmap[j][i] = 0;
			sqmap[j][i] = 0;
			prmap[j][i] = 0;
			uline[j] = 0;
		}
	for( i = 0; i < MAXnat; i++)
		for (j = 0; j < MAX; j++)
			bitmap[j][i] = 0;
	eoc = 0;
	(void) fclose(fd);
}


/*
 * store the curent scan line in the bitmap
 *
 * bit clumsy, we could just as well dump everyscan line each time
 * but, as said before, we don't know what to do with the bitmap...
 *
 */

storescanline(x, y, toy)
register int x, y, toy;
{
	register int m, n, i;

	m = x / 8;
	n = x % 8;
	if(m > MAX)
		error("bit map overflow for x (%d)\n", m);

	if(toy >= MAXnat)
		error("Bitmap overflow");
	for( i = y; i < toy; i++)
		if(sl0[i])
			bitmap[m][i] |= (1 << n);
}

short width, height;

#define For_v for(v=0; v < height; v++)
#define For_h for(h=0; h < width; h++)


/*
 * print the bit map
 */

setbitmap(fromx, fromy, tox, toy)
int fromx, fromy, tox, toy;
{
	register int m, n;
	register int x, y;
	nat v, h;	

	width = tox - fromx;
	height = toy - fromy;
	if (width > MAXnat || height > MAXnat) {
		error("*** X or Y is too large (%d %d)\n", width, height);
	}

	dbprintf("# Rectangle map of character %s\n", filename);
	dbprintf("%% X %d Y %d\n", width, height);

	for(v= 0, y = toy - 1; y >= fromy; v++, y--) {
		for( h=0, x = fromx; x < tox; h++, x++) {
			m = x / 8;
			n = x % 8;
			if((bitmap[m][y]  >> n ) & 1)
				newbitmap[v][h] = 1;
			else
				newbitmap[v][h] = 0;
		}
	}
}



maxsq() 
{
	register nat v, h, m;
	nat uleft, up, left;
	For_h 
		uline[h]= 0;
	For_v {
		uleft= left= 0;
		For_h {
			up= uline[h];
			if (newbitmap[v][h]) {
				m= uleft;
				if (up < m) m= up;
				if (left < m) m= left;
				sqmap[v][h]= ++m;
			} else
				sqmap[v][h]= m= 0;
			uleft= up;
			uline[h]= left= m;
		}
		sqmap[v][h]= 0;
	}
}


prune() 
{
	register nat v, h, m;
	nat vv, hh;
	For_v {
		For_h {
			m= sqmap[v][h];
			for (vv= v; m && vv <= v+1 && vv < height; vv++)
			for (hh= h; m && hh <= h+1 && hh < width; hh++)
				if (sqmap[vv][hh] > m) m= 0;
			prmap[v][h]= m;
		}
	}
}


combi()
{
	register nat v, h, m, p=0, hh;
	int rects = 0;	/* track number of structures written */

	For_v {
		p=m= 0;
		for (h= 0; h <= width; h++) {
			if (h == width || prmap[v][h] != m) {
				/* Don't pay attention to "singletons" (h-p == 1) */
				if (m && h-p > 1) {
					rects++;
					rect.yO = v-m+1;
					rect.y = m;
					rect.xO = p-m+1;
					rect.x = h-1-p+m;
					oput(rect);
					dbprintf("> V@%3d|%3d*H@%3d|%3d\n",  v-m+1, m, p-m+1, h-1-p+m);
					/* Mark squares as accounted for */
					for (hh= p; hh < h; hh++) sqmap[v][hh]= 0;
				}
				if (h < width) m= prmap[v][p= h];
			}
		}
	}
	for (h = 0; h <= width; h++) {
		p=m= 0;
		for (v= 0; v <= height; v++) {
			if (v == height || prmap[v][h] != m) {
				/* Pay attention to unaccounted-for "singletons" */
				if (m && (v-p > 1 || sqmap[v-1][h])) {
					rects++;
					rect.yO = p-m+1;
					rect.y = v-1-p+m;
					rect.xO = h-m+1;
					rect.x = m;
					oput(rect);
					dbprintf("> V@%3d|%3d*H@%3d|%3d\n", p-m+1, v-1-p+m, h-m+1, m);
				}
				if (v < height) m= prmap[p= v][h];
			}
		}
	}
	dbprintf("<\n");
	dbprintf("The Nstructs should be %d\n", rects);
	return(rects);
}

headit()
{
	if(lseek(fdo, (long)0, 0) == -1)
		error("seek error in head routine");
	if(write(fdo, (char *) &head, sizeof(struct Header)) != sizeof(struct Header))
		error("write error in head routine");
}
setchars()
{
	if(lseek(fdo, (long)(sizeof(struct Header)), 0) == -1)
		error("seek error in setchars routine");
	if (write(fdo, (char *)Char, Charmax * sizeof(struct Chars)) != Charmax * sizeof(struct Chars))
		error("Write error in setchars routine");
}


/* output a rect struct */

oput(r)
struct Rect r;
{
	*curp++ = r;
	if(curp > eob) {
		flusho();
		curp = bob;
	}
	offset += sizeof(struct Rect);
}

/* flush the buffer holding the rectangles */

flusho()  
{ 

	if ( write(fdo, (char *)bob, (int)(curp - bob) * sizeof(struct Rect)) != 
			(int)(curp - bob)*sizeof(struct Rect))
		error("Write error in flusho routine");
}

setoffset()
{
	offset = sizeof(struct Header) + Charmax * sizeof(struct Chars);
}
