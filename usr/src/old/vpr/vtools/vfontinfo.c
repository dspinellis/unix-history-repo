static	char *sccsid = "@(#)vfontinfo.c	4.2 (Berkeley) 81/02/28";
/* Font Information for VCat-style fonts
 *      AJH  4/79
 *
 *	Modified to print Ascii chars 1/80 by Mark Horton
 *	Modified to use ,'| 1/81 by Mark Horton using an idea
 *		from Eric Scott of CalTech.
 */
#include <stdio.h>
#include <ctype.h>
#include <vfont.h>

struct header FontHeader;
struct dispatch disptable[256] ;

char	IName[100];
char *	rdchar();
long	fbase;

char	defascii[256];
char	*charswanted = defascii;
int	verbose;
char	charbits[4000];
int	H, W, WB;

main(argc,argv)
int argc;
char **argv;

{
	int FID,i,j;

	if (argc > 1 && argv[1][0] == '-') {
		switch(argv[1][1]) {
		case 'v':
			verbose++;
			break;
		default:
			printf("Bad flag: %s\n", argv[1]);
		}
		argc--; argv++;
	}
	if (argc < 2) {
		fprintf(stderr,"Usage: %s filename", argv[0]);
		exit(2);
	}

	for (i=0; i<128; i++)
		defascii[i] = i;
	if (argc >= 3)
		charswanted = argv[2];

	sprintf(IName,"/usr/lib/vfont/%s",argv[1]);
	if ((FID = open(argv[1],0)) < 0)
		if ((FID = open(IName,0)) < 0) { 
			printf("Can't find %s\n",argv[1]);
			exit(8); 
		};

	if (read(FID,&FontHeader,sizeof FontHeader) != sizeof FontHeader)
		error("Bad header in Font file.");

	if (read(FID,&disptable[0],sizeof disptable) != sizeof disptable)
		error("Bad dispatch table in Font file");

	fbase = sizeof FontHeader + sizeof disptable;

	if (FontHeader.magic != 0436)
	printf("Magic number %o wrong\n", FontHeader.magic);
	printf("Font %s, ",argv[1]);
	printf("raster size %d, ",FontHeader.size);
	printf("max width %d, max height %d, xtend %d\n",
		FontHeader.maxx, FontHeader.maxy,FontHeader.xtend);
	if (!verbose)
		printf("\n ASCII     offset    size  left    right   up     down    width \n");

	for (i=0; i<256; i++) {
		j = charswanted[i];
		if (i>0 && j==0)
			break;
		if (disptable[j].nbytes != 0) {
			printf(!verbose ?
				"  %3o %2s     %4d   %4d   %4d   %4d   %4d   %4d   %5d\n" :
				"  %3o %2s  a=%d, n=%d, l=%d, r=%d, u=%d, d=%d, w=%d\n",
				j, rdchar(j),
				disptable[j].addr,
				disptable[j].nbytes,
				disptable[j].left,
				disptable[j].right,
				disptable[j].up,
				disptable[j].down,
				disptable[j].width);
			if (verbose) {
				int len = disptable[j].nbytes;
				int k, l, last;

				lseek(FID, fbase+disptable[j].addr, 0);
				read(FID, charbits, len);
				H = (disptable[j].up) + (disptable[j].down);
				W = (disptable[j].left) + (disptable[j].right);
				WB = (W+7)/8;
				for (k=0; k<H; k+=2) {
					for (last=W-1; last >= 0; last--)
						if (fbit(k, last))
							break;
					for (l=0; l<=last; l++) {
						printf("%c", " ',|"[fbit(k,l)+2*fbit(k+1,l)]);
					}
					printf("\n");
				}
				printf("\n");
			}
		}
	};
}

error(string)
char *string;

{ 
	printf("\nvfontinfo: %s\n",string);
	exit(8);
};

char *rdchar(c)
char c;
{
	static char ret[3];
	ret[0] = isprint(c) ? ' ' : '^';
	ret[1] = isprint(c) ?  c  : c^0100;
	ret[2] = 0;
	return (ret);
}

int
fbit(row, col)
int row, col;
{
	int thisbyte, thisbit, ret;

	if (row >= H)
		return 0;
	thisbyte = charbits[row*WB + (col>>3)] & 0xff;
	thisbit = 0x80 >> (col&7);
	ret = thisbyte & thisbit;
	return (ret != 0);
}
