/* not used - just to look at I guess */

#define FRAMOFFSET (-8)		/* FP points to frame + 8 */

struct machframe
       {
	lispval (*pc)();
	short mask;
	short removed;
	struct machframe *fp;
	lispval *arg[2];
};
