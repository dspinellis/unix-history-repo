/*-
 * Copyright (c) 1988, 1989, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nonints.h	8.2 (Berkeley) %G%
 */

char **brk_string(), *emalloc(), *str_concat();

char	*Dir_FindFile();
char	*Dir_MakeFlags();
char	*Str_Concat();
char	*Str_FindSubstring();
GNode	*Suff_AddTransform();
GNode	*Targ_FindNode();
char	*Targ_FmtTime();
void	 enomem __P((void));
GNode	*Targ_NewGN();
char	*Var_GetHead();
char	*Var_GetTail();
char	*Var_Parse();
char	*Var_Subst();
char	*Var_Value();
void	 Arch_FindLib();
void	 Arch_Init();
Boolean	 Arch_LibOODate();
int	 Arch_MTime();
int	 Arch_MemMTime();
ReturnStatus	 Arch_ParseArchive();
void	 Arch_Touch();
void	 Arch_TouchLib();
void	 Compat_Run();
void	 DieHorribly();
void	 Dir_AddDir();
void	 Dir_ClearPath();
void	 Dir_Concat();
ClientData	 Dir_CopyDir();
void	 Dir_Destroy();
void	 Dir_Expand();
Boolean	 Dir_HasWildcards();
void	 Dir_Init();
int	 Dir_MTime();
void	 Error __P((const char *, ...));
void	 Fatal __P((const char *, ...));
void	 Finish();
void	 Job_AbortAll();
void	 Job_CatchChildren();
void	 Job_CatchOutput();
Boolean	 Job_CheckCommands();
Boolean	 Job_Empty();
int	 Job_End();
Boolean	 Job_Full();
void	 Job_Init();
void	 Job_Make();
ReturnStatus	 Job_ParseShell();
void	 Job_Touch();
void	 Job_Wait();
void	 Main_ParseArgLine();
void	 Make_DoAllVar();
int	 Make_HandleUse();
Boolean	 Make_OODate();
Boolean	 Make_Run();
int	 Make_TimeStamp();
void	 Make_Update();
void	 Parse_AddIncludeDir();
void	 Parse_DoVar();
void	 Parse_Error __P((int, const char *, ...));
void	 Parse_File();
Boolean	 Parse_IsVar();
Lst	 Parse_MainName();
void	 Punt __P((const char *, ...));
int	 Str_Match();
char	*Str_SYSVMatch __P((char *, char *, int *len));
void	 Str_SYSVSubst __P((Buffer, char *, char *, int));
void	 Suff_AddInclude();
void	 Suff_AddLib();
void	 Suff_AddSuffix();
void	 Suff_ClearSuffixes();
void	 Suff_DoPaths();
int	 Suff_EndTransform();
void	 Suff_FindDeps();
Lst	 Suff_GetPath();
void	 Suff_Init();
Boolean	 Suff_IsTransform();
void	 Suff_SetNull();
Lst	 Targ_FindList();
Boolean	 Targ_Ignore();
void	 Targ_Init();
Boolean	 Targ_Precious();
int	 Targ_PrintCmd();
void	 Targ_PrintType();
void	 Targ_SetMain();
Boolean	 Targ_Silent();
void	 Var_Append();
void	 Var_Delete();
Boolean	 Var_Exists();
void	 Var_Init();
void	 Var_Set();
