
#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "saio.h"

/* Standalone program to test a disk driver by reading every sector on
 * the disk in chunks of CHUNK.
 */

#define CHUNK	32
#define SECTSIZ	512

main()
{
	char buf[50], buffer[CHUNK*SECTSIZ];
	int unit,fd,chunk,j;
	struct st st;
	register i;

	printf("Testprogram for stand-alone hp or up driver\n");
askunit:
	printf("Enter device name (e.g, hp(0,0) ) or q to quit >");
	gets(buf);
	unit = (int)*(buf+3) - '0';
	if (unit <0 || unit > 3 ) {
		printf("unit number out of range\n");
		goto askunit;
	}
	if ((fd=open(buf,0)) < 0) {
	     printf("Can't open %s \n",buf);
		goto askunit;
	}
	ioctl(fd,SAIODEVDATA,&st);

	chunk = st.nsect*SECTSIZ;
	printf("Testing %s, chunk size is %d\n",buf, chunk);
	printf("Start ...Make sure %s is online\n",buf);
	lseek(fd,0,0);
	for (i=0;i < st.ncyl*st.ntrak; i++) {
/*		for (j=8;j<st.ntrak+8;j++) {
			lseek(fd,(i*st.nspc+((j%st.ntrak)*st.nsect))*SECTSIZ,0);
*/
			read(fd,buffer, chunk);
/*		}					*/
		if (i%st.ntrak == 0) printf("%d\r",i/st.ntrak);
	}
	goto askunit;
}
