/*-
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * Copyright (c) 1988, 1989 by Adam de Boor
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 */

char **brk_string(), *emalloc(), *str_concat();

ReturnStatus	Arch_ParseArchive ();
void	Arch_Touch ();
void	Arch_TouchLib ();
int	Arch_MTime ();
int	Arch_MemMTime ();
void	Arch_FindLib ();
Boolean	Arch_LibOODate ();
void	Arch_Init ();
void	Compat_Run();
void	Dir_Init ();
Boolean	Dir_HasWildcards ();
void	Dir_Expand ();
char *	Dir_FindFile ();
int	Dir_MTime ();
void	Dir_AddDir ();
ClientData	Dir_CopyDir ();
char *	Dir_MakeFlags ();
void	Dir_Destroy ();
void	Dir_ClearPath ();
void	Dir_Concat ();
int	Make_TimeStamp ();
Boolean	Make_OODate ();
int	Make_HandleUse ();
void	Make_Update ();
void	Make_DoAllVar ();
Boolean	Make_Run ();
void	Job_Touch ();
Boolean	Job_CheckCommands ();
void	Job_CatchChildren ();
void	Job_CatchOutput ();
void	Job_Make ();
void	Job_Init ();
Boolean	Job_Full ();
Boolean	Job_Empty ();
ReturnStatus	Job_ParseShell ();
int	Job_End ();
void	Job_Wait();
void	Job_AbortAll ();
void	Main_ParseArgLine ();
void	Error ();
void	Fatal ();
void	Punt ();
void	DieHorribly ();
void	Finish ();
void	Parse_Error ();
Boolean	Parse_IsVar ();
void	Parse_DoVar ();
void	Parse_AddIncludeDir ();
void	Parse_File();
Lst	Parse_MainName();
void	Suff_ClearSuffixes ();
Boolean	Suff_IsTransform ();
GNode *	Suff_AddTransform ();
void	Suff_AddSuffix ();
int	Suff_EndTransform ();
Lst	Suff_GetPath ();
void	Suff_DoPaths();
void	Suff_AddInclude ();
void	Suff_AddLib ();
void	Suff_FindDeps ();
void	Suff_SetNull();
void	Suff_Init ();
void	Targ_Init ();
GNode *	Targ_NewGN ();
GNode *	Targ_FindNode ();
Lst	Targ_FindList ();
Boolean	Targ_Ignore ();
Boolean	Targ_Silent ();
Boolean	Targ_Precious ();
void	Targ_SetMain ();
int	Targ_PrintCmd ();
char *	Targ_FmtTime ();
void	Targ_PrintType ();
char *	Str_Concat ();
int	Str_Match();
void	Var_Delete();
void	Var_Set ();
void	Var_Append ();
Boolean	Var_Exists();
char *	Var_Value ();
char *	Var_Parse ();
char *	Var_Subst ();
char *	Var_GetTail();
char *	Var_GetHead();
void	Var_Init ();
char *	Str_FindSubstring();
