/********************************************************************
 *
 * Harris - Emulator specific font stuff: bitmap files.
 *
 * @(#)font2.c	1.2 (CWI) 87/07/10
 *
 *******************************************************************/
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>
#include "the.h"
#include "defs.h"		/* harris font definitions */


#define	BYTE	8


struct chardata {
	short charno;		/* character number	*/
	int XO, YO;		/* origin of char rel. to troff positions */
	int width, height;	/* scaled size in bits  */
	char *cbitmap;		/* bitmap of character  */
	short nbytes;		/* length of bitmap     */
	struct chardata *nextchar;
};

struct fontadmin {
	short physical;
	short size;
	struct chardata *ch[128+1];	/* we start at index 1 */
	struct fontadmin *f;
};



private struct	fontadmin *currentfnt;
private struct	fontadmin firstf;

extern short	*pstab;
extern int	size;
extern char	buffer[];
extern int	hpos, vpos, vorigin;
extern int	virtRES;
extern int	debug, dbg;
extern int	slant;
extern double	slantoffset;

extern int	errno;


char   *getfile();

struct fontadmin *findfont();
struct fontadmin *falloc();

extern char 	*malloc();




initfonts()
{
	currentfnt = (struct fontadmin *) 0;	/* zero it out to start! */
	firstf.physical = 0;
	firstf.size = 0;
}



/* 
 * putcode: get the necessary character data and write it.
 *
 * This function should be in put.c, but it knows too much
 * about the internal structure of the fonts, so it
 * is here.
 */

int	M[] = {	0x00000000, 0x01010101, 0x03030303, 0x07070707,
		0x0f0f0f0f, 0x1f1f1f1f, 0x3f3f3f3f, 0x7f7f7f7f, 0xffffffff };
int	N[] = { 0xffffffff, 0xfefefefe, 0xfcfcfcfc, 0xf8f8f8f8,
		0xf0f0f0f0, 0xe0e0e0e0, 0xc0c0c0c0, 0x80808080, 0x0 };
int	strip[] = { 0x00000000, 0x000000ff, 0x0000ffff, 0x00ffffff, 0xffffffff  };


putcode(code,f)
int code; int f;			/* character to print 			*/
{
	struct chardata *foundc;	/* pointer to found character 		*/
	char *scanp;			/* position to place character in buffer */
	char *sc;			/* real position to place char in buffer */
	char *cm;
	int nvpos;			/* new vertical position 		*/
	int nhpos;			/* new horizontal position 		*/
	int nhbyte;			/* byte index of horizontal position	*/
	int offbit, off8;		/* bit offset from byte index of hor. pos.*/
	double sloffset;		/* amount of offset due to slant	*/
	int bwidth, cnt;		/* width of bitmap in bytes 		*/
	int h;				/* height count		 		*/
	register unsigned int fontdata;
	unsigned char *to, *from;
	unsigned int n;
	int scan_inc;

	if(currentfnt->physical != f || currentfnt->size != size)   {
		currentfnt = findfont(f);
	}
	foundc = currentfnt->ch[code];
	if( foundc == (struct chardata *)0 )
		return;

	nvpos  = PHYS(vpos) - vorigin - foundc->height + foundc->YO;
	nhpos  = PHYS(hpos) + foundc->XO;
	bwidth = (foundc->width + 7)/BYTE;
	cm     = foundc->cbitmap;

	if((nvpos >= NLINES) || (nvpos + foundc->height <= 0))
		return;
	if( nhpos < 0 )			/* ignore chars left of left margin */
		return;

	/* the following is an ugly optimization: since slant will usually
	 * be zero, and we can save a whole lot of time if that is so,
	 * have a duplicate (faster) loop for that case (DD)
	 */

	if( slant == 0 ) {
	
		nhbyte = nhpos / BYTE;
		offbit = nhpos % BYTE;
		off8 = 8 - offbit;
		scanp = buf0p + nvpos * BYTES_PER_LINE + nhbyte;
		scan_inc = BYTES_PER_LINE - bwidth;

		for(h = 0; h < foundc->height; h++) {

			if (scanp + bwidth >= BUFBOTTOM)
				break;

			cnt = bwidth;
			if (scanp >= BUFTOP) {
				do {
					fontdata = *(unsigned *)cm;
					cm += 4;
					if( cnt < 4 )
						fontdata &= strip[cnt];
					*(unsigned int *)scanp |= (fontdata >> offbit) & M[off8];
					scanp++;	/* _char_ increment */
					*(unsigned int *)scanp |= (fontdata << off8) & N[off8];
					scanp += 3;	/* to next _word_ */
					cnt -= 4;
				} while( cnt > 0 );
			}
			scanp += scan_inc + cnt;
			cm += cnt;
		}
	} else {	/* we have to add in a slant offset for each row */
	
		scanp = buf0p + nvpos * BYTES_PER_LINE;
		sloffset = slantoffset * (foundc->height - foundc->YO);

		for( h = 0;
			h < foundc->height; 
			h++, scanp += BYTES_PER_LINE, sloffset -= slantoffset ){
		
			nhbyte = (nhpos + (int)sloffset) / BYTE;
			offbit = (nhpos + (int)sloffset) % BYTE;
			off8 = 8 - offbit;
			sc = scanp + nhbyte;

			if( sc <= BUFTOP )
				continue;

			if( sc + bwidth >= BUFBOTTOM )
				break;

			to = (unsigned char *)sc;
			from = (unsigned char *)&cm[ h*bwidth ];
			cnt = bwidth;
			while (cnt--) {
				n = *from++;
				*to++ |= n >> offbit;
				*to |= n << off8;
			}
		}
	}
}





/*
 *  findfont(f): Try to find font f (in current size).
 *  If the font in this size is not already loaded, try to load it.
 */


struct fontadmin *
findfont(f)
int f;
{
	struct fontadmin *p = &firstf;
	char file[BUFSIZ], *fbuf;


	DBGPRINT(0, ("Searching for %d size %d\n", f, size ));
	/* 
	 * search list for desired font and size
	 */
	
	while( p->physical != 0 )  {

		if( p->physical == f && p->size == size )
			return(p);
#ifdef DEBUGABLE
		else if(p->physical > 10000 || p->physical < 10)
			error(FATAL, "odd font: %d!\n",p->physical);
#endif
		else
			p = p->f;
	}

	
	/* 
	 * not in list, so make it
	 */

	p->physical = f;
	p->size = size;
	p->f = falloc();
	p->f->physical = 0;


	sprintf(file,"%s/%d/%d", BITS, f, pstab[ size ] );
	fbuf = getfile(file);	/* getfile exits if unsuccessful */

	DBGPRINT(0, ("Loading bitmaps for font %d size %d\n", f, size));
	loadbits( fbuf, p->ch );

	return( p );
}



/*
 *  loadbits: reconstruct the linked char list from buf,
 *  and put it into the given array.
 */


loadbits( buf, cha )
char *buf;
struct chardata *cha[];
{
	struct chardata *firstc, *ch;
	unsigned  offset = 0;
	int i;

	ch = firstc = (struct chardata *)buf;

	/* note we employ a trick here: we know that all
	 * the addresses (->nextchar and ->cbitmap) are
	 * garbage, yet still the list must have been
	 * terminated with ch->nextchar == 0, so we can
	 * still tell where it was supposed to end.
	 */

	while( ch->nextchar != (struct chardata *)0 )
	{
		offset += sizeof( struct chardata );
		ch->cbitmap  = (char *)(buf + offset);
		offset += ch->nbytes;
		ch->nextchar = (struct chardata *)(buf + offset);

		ch = ch->nextchar;
	}
	/* clean up last one */
	offset += sizeof( struct chardata );
	ch->cbitmap = (char *)(buf + offset);

	/* 
	 *  now go through and fill up the array
	 */
	ch = firstc;
	for( i=1; i<=128; i++ )
	{
		if( i == ch->charno )
		{
			cha[i] = ch;
			ch = ch->nextchar;
		}
		else
			cha[i] = 0;
	}
}



struct fontadmin *
falloc()
{
	register char *p;

	if(( p = malloc(sizeof(struct fontadmin))) == (char *) 0)
		error(FATAL, "Falloc");
	return((struct fontadmin *) p);
}




/*
 *  get the contents of the file, and return a pointer to them.
 *  All errors result in death.
 *
 */


char *
getfile(file)
char *file;
{
	struct	stat statbuf; 
	char	*filebuf, *malloc();
	int	fdi;

	if( stat(file, &statbuf) == -1 )
		error(FATAL, "Can't get file status: %s (error %d)", file, errno);

	if((statbuf.st_mode & S_IFMT) != S_IFREG)
		error(FATAL, "File %s not a regular file", file);

	if((fdi = open(file, O_RDONLY)) < 0 )
		error(FATAL, "Can not open %s (error %d)", file, errno );

	if((filebuf = malloc( (unsigned int)statbuf.st_size )) == (char *)0)
		error(FATAL, "Not enough room for file %s!", file);

	if(read(fdi, filebuf, (int)statbuf.st_size) != statbuf.st_size)
		error(FATAL, "Read error on %s", file);

	(void) close( fdi );
	return( filebuf );
}
