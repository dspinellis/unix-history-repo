#include <stdio.h>

int tst3()
{
        FILE *fp;
        int c;

        if ((fp = fopen("FOO", "w")) == NULL)
        {
                fprintf(stderr, "can't open for writing\n");
                return -1;
        }
        fprintf(fp, "ABCDEFG");
        fclose(fp);

        if ((fp = fopen("FOO", "r")) == NULL)
        {
                fprintf(stderr, "can't open for reading\n");
                return -1;
        }
        c = getc(fp);
        if (c != 'A')
        {
                fprintf(stderr, "wrong on first read!\n");
                return -1;
        }
        rewind(fp);
        c = getc(fp);
        if (c != 'A') {
                fprintf(stderr, "wrong on second read (after rewind)!\n");
                fprintf(stderr, "A=%c!\n", c);
                return -1;
        }
        fseek(fp, 2, 1);
        c = getc(fp);
        if (c != 'D') {
                fprintf(stderr, "wrong on third read (after seek)!\n");
                fprintf(stderr, "D=%c!\n", c);
                return -1;
        }
	return 0;
}
main()
{
        return tst3();
}
