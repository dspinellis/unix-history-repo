# define PRINT 0
# define DISK 3
# define other(i,j) (6-(i+j))
int num[4];
long cnt;
main(argc,argv)
  char **argv; {
	int disk;
	disk  = DISK;
	if(argc > 1)disk = atoi(argv[1]);
	num[1] = disk;
	if(PRINT)printf("Start %d on A\n",disk);
	mov(disk,1,3);
	printf("For %d, %ld moves\n",disk,cnt);
	}
mov(n,f,t){
	int o;
	if(n == 1){
		num[f]--;
		num[t]++;
		if(PRINT)printf("Move from %d to %d, result: A:%d B:%d C%d\n",
			f,t,num[1],num[2],num[3]);
		cnt++;
		return;
		}
	o = other(f,t);
	mov(n-1,f,o);
	mov(1,f,t);
	mov(n-1,o,t);
	return;
	}
