
/* gprint.c-
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *      This file contains the main and file system dependent routines
 * for producing hard copy from gremlin files.  It is extensively modified
 * from the vplot source.
 */

#include "gprint.h"
#include "grem2.h"
#include <signal.h>
#include <vfont.h>


#define VDAEMON        "/usr/lib/vad"
#define WDAEMON        "/usr/lib/vpd"

#define NB    1024        /* Number of blocks in virtual memory */
#define BSIZ    512        /* Size of blocks */
#define    SETSTATE    (('v'<<8)+1)

extern char *mktemp();
extern char *getname(); /* get login name or user name */

/* imports */
extern HGtline(), HGArc(), HGPutText(), HGMove(), HGSetFont();
extern HGSetBrush(), HGInitFont(), HGPrintElt();
extern int style[], thick[];
extern char *tfont[], *tsize[];
 
/* database imports */

extern ELT *DBInit(), *DBRead();
extern POINT *PTInit(), *PTMakePoint();

int     linethickness = 0;       /* brush styles */
int    linmod    = SOLID;
char    chrtab[][16];
char    blocks    [NB][BSIZ];
int    lastx;
int    lasty;
int    angle, startx, starty, endx, endy;
double     scale = 4.0;     /* Variables used to map gremlin screen */
double    topx;             /* coordinates into output device coordinates */
double    topy;
double    botx;
double    boty;
int    centx = 0;
int    centy = 0;
double    delx;
double    dely;
double    del;

char *vspoolr = "/usr/spool/vad/dfxXXXXXX";
char *wspoolr = "/usr/spool/vpd/dfxXXXXXX";
char *vfilen = "/usr/spool/vad/tmpXXXXXX";
char *wfilen = "/usr/spool/vad/tmpXXXXXX";

int DevRange = 1536;        /* Bits per line for output device */

struct    buf 
{
    int    bno;
    char    *block;
};
struct    buf    bufs;

int dirty[NB];         /* marks if a block has been written into */
int    in, out;
char picture[64];
char *run = "a";     /* gives uniqueness for multiple pictures */
char device = 'V';
char *banner = "gremlin";

/* variables used to print from font file */
int Orientation;
int cfont = 0;
int csize = 0;
struct header header;
struct dispatch dispatch[256];
char *bits = NULL;
char *fontdir = "/usr/lib/vfont/";

main(argc, argv)
int argc;
char *argv[];
{
    FILE *fp, *fopen();
    ELT *PICTURE, *e;
    POINT *p1, pos;
    char *file[50], sw, string[10], *arg;
    char c, string1[50], string2[50], string3[50], string4[50], 
            string5[50], string6[50], string7[50], string8[50]; 
    extern int onintr();
    float mult;
    int WriteRaster = FALSE;
    int i, j, gfil = 0;
    int b, brsh;
    int k;

    /* Parse the command line. */

    argc -= 1;  argv++;                /* Skip program name. */
    while (argc > 0)
    {
        argc -= 1;
        arg = *argv++;
        if (arg[0] != '-') file[gfil++] = arg;
        else
        {
            sw = *++arg;
            switch (sw)
            {
            case 'W':       /* Print to wide (versatec) device */
                  device = 'W';
                  DevRange = 2047;
                  break;
            case 'V':       /* Print to narrow (varian) device */
                  device = 'V';
                  DevRange = 1536;
                  break;
            case '1':      /* select size 1 */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  tsize[0] = arg;
                  break;
            case '2':      /* select size 2 */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  tsize[1] = arg;
                  break;
            case '3':      /* select size 3 */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  tsize[2] = arg;
                  break;
            case '4':      /* select size 4 */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  tsize[3] = arg;
                  break;
            case 'R':      /* select Roman font */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  tfont[0] = arg;
                  break;
            case 'I':     /* select italics font */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  tfont[1] = arg;
                  break;
            case 'B':     /* select bold font */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  tfont[2] = arg;
                  break;
            case 'S':     /* select special font */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  tfont[3] = arg;
                  break;
            case 'N':     /* select narrow brush width */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  (void) sscanf(arg, "%d", &brsh);
                  thick[0] = thick[1] = thick[3] = thick[4] = brsh;
                  break;
            case 'T':     /* select thick brush width */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  (void) sscanf(arg, "%d", &brsh);
                  thick[2] = brsh;
                  break;
            case 'M':     /* select medium brush width */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  (void) sscanf(arg, "%d", &brsh);
                  thick[5] = brsh;
                  break;
            case 't':     /* send raster to standard output */
                  WriteRaster = TRUE;
                  break;
            case 'x':     /* select scale */
                  if (*++arg == '\0')
                  if (argc-- > 0) arg = *argv++;
                  sscanf(arg,"%f",&mult);
                  scale *= mult;
                  break;
            case 'p':     /* prompt for font and size parameters */
                  printf("Roman font name? (%s): ", tfont[0]);
                  gets(string1);
                  if (*string1 != '\0') tfont[0] = string1;
                  printf("Italic font name? (%s): ", tfont[1]);
                  gets(string2);
                  if (*string2 != '\0') tfont[1] = string2;
                  printf("Bold font name? (%s): ", tfont[2]);
                  gets(string3);
                  if (*string3 != '\0') tfont[2] = string3;
                  printf("Special font name? (%s): ", tfont[3]);
                  gets(string4);
                  if (*string4 != '\0') tfont[3] = string4;
                  printf("font size 1? (%s): ", tsize[0]);
                  gets(string5);
                  if (*string5 != '\0') tsize[0] = string5;
                  printf("font size 2? (%s): ", tsize[1]);
                  gets(string6);
                  if (*string6 != '\0') tsize[1] = string6;
                  printf("font size 3? (%s): ", tsize[2]);
                  gets(string7);
                  if (*string7 != '\0') tsize[2] = string7;
                  printf("font size 4? (%s): ", tsize[3]);
                  gets(string8);
                  if (*string8 != '\0') tsize[3] = string8;
                  printf("narrow brush size? (%d): ", thick[0]);
                  gets(string);
                  if (*string != '\0') 
                  {
                       sscanf(string, "%d", &brsh);
                       thick[0] = thick[1] = thick[3] = thick[4] = brsh;
                  }
                  printf("medium brush size? (%d): ", thick[5]);
                  gets(string);
                  if (*string != '\0') 
                  {
                       sscanf(string, "%d", &brsh);
                       thick[5] = brsh;
                  }
                  printf("thick brush size? (%d): ", thick[2]);
                  gets(string);
                  if (*string != '\0') 
                  {
                       sscanf(string, "%d", &brsh);
                       thick[2] = brsh;
                  }
                  break;
            default:
                  (void) printf("unknown switch: %c", sw);
            }
        }
    }

    /* init constants for scaling */
    topx = topy = DevRange;
    botx = boty = 0;
    delx = dely = del = DevRange;
    centx = (DevRange - mapx(topx/scale))/2;
    centy = mapy(topy/scale)/2;
    signal(SIGTERM, onintr);
    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
        signal(SIGINT, onintr);
    if (gfil == 0)   /* no filename, use standard input */
    {
        file[0] = "stdin";
        ++gfil;
    }
    for (j=0; j<gfil; ++j)
    {
        if (strcmp(file[j], "stdin") != 0) fp = fopen(file[j],"r");
        else  fp = stdin;
        if (fp == NULL)
        {
            fprintf(stderr, "can't open %s",file[j]);
            continue;
        }
        PICTURE = DBRead(fp,&Orientation, &pos);    /* read picture file */
        if (DBNullelt(PICTURE)) continue;

        banner = file[j];
        picture[0] = '\0';    /* picture equals null string */
        strcat(picture,mktemp("/usr/tmp/#rastXXXXX"));
        strcat(picture, run);
        (*run)++;
        bufs.bno = -1;      /* no current block */
        for (i=0; i<NB; i++) 
        {
            dirty[i] = FALSE;     /* no blocks written yet */
            for (k=0; k<BSIZ; ++k)     /* Zero out memory area for picture */
                  blocks[i][k] = 0;
        }
        out = creat(picture, 0666);
        in = open(picture, 0);
        zseek(out, NB);               /* seek to end of file and write */
        write(out, blocks[0], BSIZ);  /* intervening hole will be 0-filled */

        e = PICTURE;
        while (!DBNullelt(e))
        {
               HGPrintElt(e);     /* traverse picture, printing elements */
               e = DBNextElt(e);
        }

        for (i=0; i<NB; i++) 
        {
            if (WriteRaster == TRUE)   /* put picture to standard output */
            {
                for (j=0; j<BSIZ; ++j)
                    putchar(blocks[i][j]);
            }
            if (dirty[i] == TRUE)      /* write out only those blocks which */
            {                          /* which have been written into, the */
                zseek(out,i);          /* rest will default to be zeroes    */
                write(out, blocks[i], BSIZ);
            }
        }
        fclose(fp);
        close(out);
        if (WriteRaster == FALSE) 
            putpict(picture);
        close(in);
        close(out);
    }  /* end for j */
    exit(0);
}  /* end main */

putpict(picture)
char *picture;
{
    FILE *temp;
    char *fileName,*spoolerName;

    if((temp = fopen(picture,"r")) == NULL)   /* picture image doesnt exist */
    {
        perror(picture);
        exit(1);
    }
    fclose(temp);
    if(device == 'V')     /* writing to varian */
    {
        fileName = (char *) malloc(strlen(vfilen) + 1);
        strcpy(fileName,vfilen);
        fileName = mktemp(fileName);
        spoolerName = (char *) malloc(strlen(vspoolr) + 1);
        strcpy(spoolerName,vspoolr);
        spoolerName = mktemp(spoolerName);
    }
    else                 /* writing to versatec */
    {
        fileName = (char *) malloc(strlen(wfilen) + 1);
        strcpy(fileName,wfilen);
        fileName = mktemp(fileName);
        spoolerName = (char *) malloc(strlen(wspoolr) + 1);
        strcpy(spoolerName,wspoolr);
        spoolerName = mktemp(spoolerName);
    }
    if((temp = fopen(fileName,"w")) == NULL) 
    {
        fprintf(stderr,"Can't make temporary spooling file\n");
        exit(1);
    }
 
           /* write file and command information to printer daemon */

    fprintf(temp,"L%s\n",getname());  /* login name (for banner) */
    fprintf(temp,"B%s\n",banner);      /* gremlin file name (for banner) */
    fprintf(temp,"V%s\n",picture);     /* raster image file name  */
    fprintf(temp,"U%s\n",picture);     /* unlink file when through */
    fclose(temp);
    if(link(fileName,spoolerName) != 0) 
    {
        perror(spoolerName);
        exit(2);
    }
    unlink(fileName);

            /* Spawn daemon process */

    if(vfork() == 0) 
    {
        if(device == 'V')
            execl(VDAEMON,VDAEMON);
        else
            execl(WDAEMON,WDAEMON);
    }
}

getblk(b)
int b;
{
    if(b < 0 || NB <= b)   /* bad block number */
    {
        fprintf(stderr,"vplot: internal error, b out of range in getblk\n");
        abort();
    }
    dirty[b] = TRUE;    /* assume that present block has been written into */
    bufs.bno = b;
    bufs.block = blocks[b];  /* get new block to write */
}

onintr()
{
    exit(1);
}

point(x, y)
int x, y;
{
    int bno;

    bno = ((x&03700)>>6) + ((y&03700)>>1);  /* calculate block number */
    if (bno != bufs.bno)     /* get appropriate block if necessary */
    {
        if (bno < 0 || bno >= 1024)
            return;
        getblk(bno);
    }
    bufs.block[((y&077)<<3)+((x>>3)&07)] |= 1 << (7-(x&07));  /* set bit */
}

zseek(a, b)
{
    return(lseek(a, (long)b*512, 0));
}
