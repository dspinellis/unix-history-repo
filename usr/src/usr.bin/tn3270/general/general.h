/*
 * Some general definitions.
 */


#define	numberof(x)	(sizeof x/sizeof x[0])
#define	highestof(x)	(numberof(x)-1)

#define	ClearStructure(x)	bzero((char *)&x, sizeof x)
#define	ClearArray(x)		bzero((char *)x, sizeof x)
