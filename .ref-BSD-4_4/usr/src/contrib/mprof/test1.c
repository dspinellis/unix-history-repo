#include	<stdio.h>

struct tstruct {
    char	c;
    int		i;
    float	f;
};

extern	char	*malloc();
extern	char	*valloc();
extern	char	*realloc();
extern	char	*calloc();
extern	char	*memalign();
extern  long	random();

int
main()
{
    int		i;
    char	*c;
/*    
    extern int keeping_leaks;
    keeping_leaks = 0;
*/
    for (i = 0; i < 50; i++) {
	c = memalign(32, 105);
	if (i == 0)  {
	    printf("memalign c = 0x%x\n", (int) c);
	}
	c = valloc(1345);
	if (i == 0) {
	    printf("valloc c = 0x%x\n", (int) c);
	}
	c = malloc(sizeof(struct tstruct));
	c = realloc(c, 2 * sizeof(struct tstruct));
	free(c);
	c = calloc(1, sizeof(struct tstruct));
	c = realloc(c, sizeof(struct tstruct) / 2);
	free(c);
	c = malloc(random() % 57);
	free(c);
	c = calloc(1, random() % 57);
	if (i % 2) {
	    free(c);
	}
    }
    exit(0);
}
  
