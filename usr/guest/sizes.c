#include <stdio.h>
#include <sys/param.h>
#include <sys/filsys.h>
#include <sys/ino.h>
#include <sys/inode.h>

#define	N	6
struct filsys filsys;
int counts[10000];
int blocks;
int totsz[N+1] = { 1,512,1024,2048,4096,1024,512 };
int indsz[N+1] = { 0,512*512/4,1024*1024/4, 2048*2048/4, 4096*4096/4,1024*1024/4,512*512/4};
int cnt1,cnt8, cnt51, cnt58;

main(argc, argv)
	char **argv;
{
	char inobuf[BSIZE*60];
	ino_t inum, tino;
	register struct dinode *dp;
	int nleft, f;
	int tot[N+1];

	argc--, argv++;
	if (argc==0)
		fprintf(stderr, "sizes filsys\n"), exit(1);
	f = open(*argv, 0);
	if (f < 0)
		perror(*argv), exit(1);
	lseek(f, BSIZE, 0);
	read(f, &filsys, sizeof (struct filsys));
	lseek(f, 2*BSIZE, 0);
	tino = (filsys.s_isize-2) * (BSIZE / sizeof (struct dinode)) + 1;
	printf("tino %d\n", tino);
	nleft = 0;
	for (inum = 1; inum < tino; inum++) {
		if (nleft == 0) {
			read(f, inobuf, sizeof (inobuf));
			nleft = (BSIZE * 60) / sizeof (struct dinode);
			dp = (struct dinode *)&inobuf;
		} else {
			--nleft;
			dp++;
		}
		if (dp->di_mode) {
			register int i;
			blocks = dp->di_size / 512;
			for (i = 0; i < N-1; i++) {
				tot[i] += (dp->di_size+totsz[i]-1)/totsz[i];
				if (i && dp->di_size > totsz[i] * 10)
					tot[i] += (dp->di_size-totsz[i]*10+indsz[i]-1)/indsz[i];
			}
			if (dp->di_size < 24*1024) {
				tot[N] += (dp->di_size+511)/512;
				cnt51 += (dp->di_size+511)/512;
				if (dp->di_size > totsz[i] * 8)
					tot[N] += (dp->di_size-totsz[N]*8+indsz[N]-1)/indsz[N];
			} else {
				tot[N] += 8*((dp->di_size+4095)/4096) +
					(dp->di_size-totsz[N]*8+indsz[N]-1)/indsz[N];
				cnt58 += ((dp->di_size+4095)/4096)*8;
				cnt58 -= 6;
				cnt51 += 48;
			}
			if (dp->di_size < 24*1024) {
				tot[N-1] += (dp->di_size+1023)/1024;
				cnt1 += (dp->di_size+1023)/1024;
				if (dp->di_size > totsz[i] * 8)
					tot[N-1] += (dp->di_size-totsz[N-1]*8+indsz[N-1]-1)/indsz[N-1];
			} else {
				tot[N-1] += 8*((dp->di_size+8191)/8192) +
					(dp->di_size-totsz[N-1]*8+indsz[N-1]-1)/indsz[N-1];
				cnt8 += ((dp->di_size+8191)/8192)*8;
				cnt8 -= 3;
				cnt1 += 24;
			}
			if (blocks >= 0 && blocks < 10000)
				counts[blocks]++;
			else
				printf("lost inum %d size %d\n", inum, dp->di_size);
		}
	}
	for (blocks = 0; blocks < 10000; blocks++)
		if (counts[blocks])
			printf("%d\t%d\n", blocks, counts[blocks]);
	printf("size\tspace\t\tratio\tblocks\n");
	for (blocks = 0; blocks < N+1; blocks++)
		printf("%d\t%d\t%7.4f\t%d\n", totsz[blocks], 
			tot[blocks]*totsz[blocks],
			((float)tot[blocks])*totsz[blocks]/(float)tot[0],
			tot[blocks]);
	printf("cnt1=%d, cnt8=%d\n", cnt1, cnt8);
	printf("cnt51=%d, cnt58=%d\n", cnt51, cnt58);
}
