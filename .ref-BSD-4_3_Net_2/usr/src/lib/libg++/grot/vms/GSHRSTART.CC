#include <stdio.h>
/*
    pointer to void function
*/
typedef	void		(*vpf)();

#define I_Psect_Name_Pre	__gxx_init_0_shr
#define D_Psect_Name_Pre	__gxx_clean_0_shr
#ifdef vax11c
#define	I_Psect_Name		__gxx_init_1
#define	D_Psect_Name		__gxx_clean_1
#else
#define	I_Psect_Name		$$PsectAttributes_NOOVR$$__gxx_init_1
#define	D_Psect_Name		$$PsectAttributes_NOOVR$$__gxx_clean_1
#endif
#define	I_Psect_Name_End	__gxx_init_2_shr
#define	D_Psect_Name_End	__gxx_clean_2_shr

/*
    PSECT just before _init_1
*/
struct {
    vpf		functs[1];
} I_Psect_Name_Pre;

/*
    PSECT with all the global constructors
*/
#ifdef vax11c
globaldef {"__gxx_init_1"}
#endif
struct {
    vpf		functs[1];
} I_Psect_Name;

/*
    PSECT located immediately after the above psect
*/
struct {
    int		dummy[1];
} I_Psect_Name_End;

/*
    PSECT just before _clean_1
*/
struct {
    vpf		functs[1];
} D_Psect_Name_Pre;

/*
    PSECT with all the global destructors
*/
#ifdef vax11c
globaldef {"__gxx_clean_1"}
#endif
struct {
    vpf		functs[1];
} D_Psect_Name;

/*
    PSECT located immediately after the above psect
*/
struct {
    int		dummy[1];
} D_Psect_Name_End;

static void		__end_gxx_shr();

//force the correct name
extern void _gxx_vms_startup_2_shr() asm("__gxx_vms_startup_2_shr");

extern void atexit(...) asm("_atexit");	//to shut up the compiler

void
_gxx_vms_startup_2_shr()
{
    /*
    	Call all the global constructors for the shared library
    */
    int			nConstructors;
    int			i;
    vpf			*f;
    nConstructors = (int)&I_Psect_Name_End - (int)&I_Psect_Name_Pre;
    nConstructors /= sizeof(vpf);
    if (nConstructors > 0) {
    	for (i=0,  f = (vpf *)&I_Psect_Name_Pre; i<nConstructors; i++, f++) {
    	    if (*f)		/* There are some null entries around */
    	        (*f)();
    	}
    }

    /*
    	Register the exit handler for the global destructors
    */
    atexit(__end_gxx_shr);
}

static void
__end_gxx_shr()
{
    /*
    	Call all the global destructors
    */
    int			nDestructors;
    int			i;
    vpf			*f;

    nDestructors = (int)&D_Psect_Name_End - (int)&D_Psect_Name_Pre;
    nDestructors /= sizeof(vpf);
    if (nDestructors > 0) {
    	for (i=0,  f = (vpf *)&D_Psect_Name_Pre; i<nDestructors; i++, f++) {
    	    if (*f)		/* There are some null entries around */
    	        (*f)();
    	}
    }
}
