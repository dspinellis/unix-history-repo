#define	SEQ_LT(a,b)	(((short)((a)-(b))) < 0)
#define	SSEQ_LT(a,b)	((util = (short)((a)-(b))), util < 0)
short util;

main() {
	unsigned short a , b;
	short c , d;
	char buf[512], s1[512], s2[512];

	for (;;) {
		scanf("%s %s", s1, s2);
		a = c = atoi(s1);
		b = d = atoi(s2);

		printf("unsigned: %x %c %x\n",
			a, a < b ? '<' : '>',  b);
		printf("signed: %x %c %x\n",
			c, c < d ? '<' : '>',  d);
		printf("SEQ_LT: %x %c %x\n",
			a, SEQ_LT(a, b) ? '<' : '>',  b);
		printf("SSEQ_LT: %x %c %x",
			a, SSEQ_LT(a, b) ? '<' : '>',  b);
		printf(", util %x\n", util);
	}
}
