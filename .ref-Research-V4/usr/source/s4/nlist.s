/ C library -- nlist

/ struct {
/	char name[8];
/	int	type;
/	int	value;
/ } list[];
/ terminated by a null name.
/ nlist(file, list);

.globl	_nlist, savr5, nlist

.data
_nlist:
	mov	r5,-(sp)
	mov	sp,savr5
	mov	4(sp),0f
	mov	6(sp),0f+2
	jsr	r5,nlist; 0:..; ..
	mov	(sp)+,r5
	rts	pc

