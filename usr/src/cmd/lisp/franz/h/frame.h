/* sccs id  @(#)frame.h	35.2  7/2/81  */
/* 
 * frame.h :: evaluation frame.
 */

/* classes of frames: */
#define F_PROG  	1
#define F_CATCH 	2
#define F_RESET 	3
#define F_EVAL		4
#define F_FUNCALL	5

/* classes of things thrown up */
#define C_INITIAL	0
#define C_GO    	1
#define C_RET   	2
#define C_THROW 	3
#define C_RESET 	4
#define C_FRETURN	5


/* the evaluation frame sits on the C runtime stack.  the global variable errp
   points to the newest frame. The base of the frame points in the middle
   of the frame, but in such a way that above the frame base the contents
   are the same for all implementation, and below it there are different
   saved registers for each machine. 
*/

struct frame 
{
    struct argent *svlbot, *svnp;
    struct nament *svbnp;
    struct frame *olderrp;
    lispval retaddr;
    long class;
    lispval larg1;	/* optional */
    lispval larg2;	/* optional */
};

extern struct frame *errp,*Pushframe();
