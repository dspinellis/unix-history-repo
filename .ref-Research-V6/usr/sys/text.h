/*
 * Text structure.
 * One allocated per pure
 * procedure on swap device.
 * Manipulated by text.c
 */
struct text
{
	int	x_daddr;	/* disk address of segment */
	int	x_caddr;	/* core address, if loaded */
	int	x_size;		/* size (*64) */
	int	*x_iptr;	/* inode of prototype */
	char	x_count;	/* reference count */
	char	x_ccount;	/* number of loaded references */
} text[NTEXT];
