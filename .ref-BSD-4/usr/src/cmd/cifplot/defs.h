#define InBufSize	128	/* Initial allocation for in_buf */
#define FStackSize 	6	/* Maximum size of input file Stack
				 * Used for included files         */
#define TableSize	256	/* Size of the hash table for
				 * symbol lookup		*/
#define InstanceTable	256	/* Size of the hash table to store
				 * the inactive items.		*/
#define UTBLSIZE	256	/* Size of hash table for unactive
				 * elements */
#define NUM_LAYERS	8	/* Initial size of array holding
				 * the patterns			*/
#define NO_PAT_LINE	8	/* Number of lines in the stipple
				 * pattern			*/
#define OUT_BUF_SIZE	32	/* Number of scan lines sent to plotter 
				 * at a time			*/
		
/****************************************************************/

#define NOPLOT		0
#define VARIAN		1
#define VERSATEC	2
#define LINEPRINTER	3
#define VSPOOL		4
#define HP2648A		5

#define INTERNAL	1	/* Internal Error		*/
#define RUNTIME		2	/* Run Time Error		*/
#define FATAL		3	/* Fatal Error(abortion delayed)*/
#define RECOVERABLE	4	/* Recoverable Error		*/
#define WARNING		5	/* Warning Only			*/

#define NIL 	0		/* NIL pointer 			*/
#define DEBUG 	if(debug) printf
#define ADEBUG 	1		/* Debugging flag		*/

#define infinity	0x7FFFFFFF
#define INFINITY	0x7FFFFFFF

typedef float real;

#define elseif		else if
#define ROUND(x)	((int) (x+0.5))
#define TRUNC(x)	((int) (x))
#define ABS(x)		( (x) > 0 ? (x) : -(x))
#define ALLOC(x)	(x *) alloc(sizeof(x))
#define SWAP(x,y,t)	t=x; x=y; y=t
#define MIN(x,y)	( (x) < (y) ? (x) : (y) )
#define MAX(x,y)	( (x) > (y) ? (x) : (y) )
#define CONVERT(x)  	((int) (((x) - Window.ymin) * ConvertFactor))
#define REVCONVERT(x)   ((real) (((x)/ConvertFactor) + Window.ymin))
#define IMPORT		extern
#define FORWARD		extern
