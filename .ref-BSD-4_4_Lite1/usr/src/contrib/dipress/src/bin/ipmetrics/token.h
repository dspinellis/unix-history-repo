/*
 * Copyright (c) 1983     Lee Moore
 */

struct TokenState {
	FILE *Input;
	int LastTokenInLine;
	int NotEndOfFile;
	int CurChar };

struct TokenState *InitTokenStream();
