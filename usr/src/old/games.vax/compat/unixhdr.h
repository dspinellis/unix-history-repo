/*
 * 	unixhdr.h	4.1	82/05/12
 */
#define	MAGIC1	0407
#define	MAGIC2	0410
#define	MAGIC3	0411
#define	MAGIC4	0405
struct {
	unsigned short magic;
	unsigned short textsize;
	unsigned short datasize;
	unsigned short bsssize;
	unsigned short symsize;
	unsigned short entry;
	unsigned short dummy;
	unsigned short relflag;
} header;
