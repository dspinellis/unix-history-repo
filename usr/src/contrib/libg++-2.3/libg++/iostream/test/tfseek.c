/*

anyhow, i was elated when my small test worked! but then was dismayed when
my x11 stuff still failed [this is after a make clean and remake total].
after a little digging i found it was still an fseek problem, but with
a twist. too bad the testing wasn't up to snuff... here is a test which
shows the problem. the #if 0 section was put in so i could upload the
same random byte file from linux, just in case... it worked on the sun
in either case. here are the results

sunos:
seeking to 10 (9 bytes)...
seeking to 1125 (1114 bytes)...
seeking to 2999 (1873 bytes)...

linux:
initializing...
writing 3000 bytes as test to FOO
seeking to 10 (9 bytes)...
seeking to 1125 (1114 bytes)...
ftell returned 101, not 1125
seeking to 2999 (1873 bytes)...
ftell returned 951, not 2999
wrong byte at 2999 [140 vs 228]

ahh... the joys of debugging :-)
zorst
[reply to obz@sisd.kodak.com]

--cut here--
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

main(argc, argv)
     int argc; char **argv;
{
	int seek_kind = SEEK_CUR;
	int use_stdout = 1;
	FILE *fp ;
	char buf[3000];
	int i, j, k;
	static int seektbl[] = { 0, 10, 1125, 2999 };
	int nseektbl = sizeof(seektbl) / sizeof(seektbl[0]);

	while (*++argv) {
	    if (strcmp(*argv, "SEEK_SET") == 0)
		seek_kind = SEEK_SET;
	    else if (strcmp(*argv, "SEEK_CUR") == 0)
		seek_kind = SEEK_CUR;
	    else if (strcmp(*argv, "fopen") == 0)
		use_stdout = 0;
	    else if (strcmp(*argv, "freopen") == 0)
		use_stdout = 1;
	    else {
		fprintf(stderr, "Unknown option %s\n", *argv);
		return -1;
	    }
		
	}
	fprintf(stderr, "Using seek_kind: %d\n", seek_kind);	
	if (use_stdout)
	    fp = stdout;

#if 1
	fprintf (stderr, "initializing...\n");
	for (i = 0; i < sizeof(buf); ++i)
		buf[i] = i & 0xfe;	/* avoid -1 */
	for (i = 0; i < sizeof(buf); ++i)
	{
		j = rand() % sizeof(buf);
		k = buf[i];
		buf[i] = buf[j];
		buf[j] = k;
	}

	fprintf (stderr, "writing %d bytes as test to FOO\n", sizeof(buf));
	if (use_stdout)
		fp = freopen("FOO", "w", stdout);
	else
		fp = fopen("FOO", "w");
	if (fp == NULL)
	{
		fprintf (stderr, "can't open FOO for writing\n");
		return -1;
	}
	if (fwrite(buf, 1, sizeof(buf), fp) != sizeof(buf))
	{
		fprintf (stderr, "failed to write\n");
		return -1;
	}
#else
	if (use_stdout)
		fp = freopen("FOO", "r", stdout);
	else
		fp = fopen("FOO", "r");
	if (fp == NULL)
	{
		fprintf (stderr, "can't open FOO for initial read\n");
		return -1;
	}
	if (fread(buf, 1, sizeof(buf), fp) != sizeof(buf))
	{
		fprintf (stderr, "failed to initial read\n");
		return -1;
	}
#endif
	if (use_stdout)
		fp = freopen("FOO", "r", stdout);
	else
	{
		fclose(fp);
		fp = fopen("FOO", "r");
	}
	if (fp == NULL)
	{
		fprintf (stderr, "can't open FOO for reading\n");
		return -1;
	}

	i = getc(fp) & 0xff;
	if (i != buf[0] & 0xff)
		fprintf (stderr, "wrong first byte!\n");
	for (i = 1; i < nseektbl; ++i)
	{
		if (seek_kind == SEEK_CUR)
			j = seektbl[i] - seektbl[i-1] - 1;
		else
			j = seektbl[i];
		fprintf (stderr, "seeking to %d (%d bytes)...\n", seektbl[i], j);
		fprintf (stderr, "we are at: %d\n", ftell (fp));
		if (fseek(fp, j, seek_kind) < 0)
		{
			fprintf (stderr, "can't seek to %d\n", seektbl[i]);
			return -1;
		}
		j = ftell(fp);
		if (j != seektbl[i])
			fprintf (stderr, "ftell returned %d, not %d\n", j, seektbl[i]);

		j = getc(fp) & 0xff;
		k = buf[seektbl[i]] & 0xff;
		if (j != k)
		{
			fprintf (stderr, "wrong byte at %d [%d vs %d]\n",
				seektbl[i], j, k);
			return -1;
		}
	}
	return 0;
}
