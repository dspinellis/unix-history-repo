
#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "saio.h"

/* Standalone program to test a disk driver by reading every sector on
 * the disk in chunks of CHUNK.
 */

#define CHUNK	32
#define SECTSIZ	512

char diskname[] = "xx(00,0)";

main()
{
	char buf[50], buffer[CHUNK*SECTSIZ];
	int unit,fd,chunk,j;
	struct st st;
	register i;

	printf("Testprogram for stand-alone hp or up driver\n\n");
askunit:
	printf("Enter disk name [ type(adapter,unit), e.g, hp(1,3) ] > ");
	gets(buf);
	unit = (*(buf+3) - '0')*8 + *(buf+5)-'0';
	diskname[0] = *buf;
	diskname[1] = *(buf+1);
	diskname[3] = '0' + unit/10;
	diskname[4] = '0' + unit%10;
	if ((fd=open(diskname,0)) < 0) {
		goto askunit;
	}
	ioctl(fd,SAIODEVDATA,&st);
	printf("Device data: #cylinders=%d, #tracks=%d, #sectors=%d\n",
		st->ncyl, st->ntrak, st->nsect);
	chunk = st.nsect*SECTSIZ;
	printf("Testing %s, chunk size is %d bytes\n",buf, chunk);
	printf("Start ...Make sure %s is online\n",buf);
	lseek(fd,0,0);
	for (i=0;i < st.ncyl*st.ntrak; i++) {
/*		for (j=8;j<st.ntrak+8;j++) {
			lseek(fd,(i*st.nspc+((j%st.ntrak)*st.nsect))*SECTSIZ,0);
*/
			read(fd,buffer, chunk);
/*		}					*/
		if (i%(st.ntrak*5) == 0) printf("%d\r",i/st.ntrak);
	}
	goto askunit;
}
