/* Automatically generated by Kelbt from "rlparse.kh".
 *
 * Parts of this file are copied from Kelbt source covered by the GNU
 * GPL. As a special exception, you may use the parts of this file copied
 * from Kelbt source without restriction. The remainder is derived from
 * "rlparse.kh" and inherits the copyright status of that file.
 */

#line 1 "rlparse.kh"
/*
 *  Copyright 2001-2007 Adrian Thurston <thurston@cs.queensu.ca>
 */

/*  This file is part of Ragel.
 *
 *  Ragel is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 * 
 *  Ragel is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with Ragel; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 
 */

#ifndef RLPARSE_H
#define RLPARSE_H

#include <iostream>
#include "avltree.h"
#include "parsedata.h"


/* Import scanner tokens. */
#define IMP_Word 128
#define IMP_Literal 129
#define IMP_UInt 130
#define IMP_Define 131


struct Parser
{
#line 91 "rlparse.kh"


	#line 52 "rlparse.h"
	struct Parser_Block *block;
	struct Parser_LangEl *freshEl;
	int freshPos;
	struct Parser_LangEl *pool;
	int numRetry;
	int numNodes;
	struct Parser_LangEl *stackTop;
	struct Parser_LangEl *lastFinal;
	int errCount;
	int curs;
#line 94 "rlparse.kh"

	void init();
	int parseLangEl( int type, const Token *token );

	Parser( char *fileName, char *sectionName, InputLoc &sectionLoc )
		: sectionName(sectionName)
	{
		pd = new ParseData( fileName, sectionName, sectionLoc );
		exportContext.append( false );
	}

	int token( InputLoc &loc, int tokId, char *tokstart, int toklen );
	void tryMachineDef( InputLoc &loc, char *name, 
		JoinOrLm *joinOrLm, bool isInstance );

	/* Report an error encountered by the parser. */
	ostream &parse_error( int tokId, Token &token );

	ParseData *pd;

	/* The name of the root section, this does not change during an include. */
	char *sectionName;

	NameRef nameRef;
	NameRefList nameRefList;

	Vector<bool> exportContext;
};

#line 93 "rlparse.h"
#define TK_Word 128
#define TK_Literal 129
#define TK_Number 130
#define TK_EndSection 131
#define TK_UInt 132
#define TK_Hex 133
#define TK_DotDot 134
#define TK_ColonGt 135
#define TK_ColonGtGt 136
#define TK_LtColon 137
#define TK_Arrow 138
#define TK_DoubleArrow 139
#define TK_StarStar 140
#define TK_ColonEquals 141
#define TK_NameSep 142
#define TK_BarStar 143
#define TK_DashDash 144
#define TK_StartCond 145
#define TK_AllCond 146
#define TK_LeavingCond 147
#define TK_Middle 148
#define TK_StartGblError 149
#define TK_AllGblError 150
#define TK_FinalGblError 151
#define TK_NotFinalGblError 152
#define TK_NotStartGblError 153
#define TK_MiddleGblError 154
#define TK_StartLocalError 155
#define TK_AllLocalError 156
#define TK_FinalLocalError 157
#define TK_NotFinalLocalError 158
#define TK_NotStartLocalError 159
#define TK_MiddleLocalError 160
#define TK_StartEOF 161
#define TK_AllEOF 162
#define TK_FinalEOF 163
#define TK_NotFinalEOF 164
#define TK_NotStartEOF 165
#define TK_MiddleEOF 166
#define TK_StartToState 167
#define TK_AllToState 168
#define TK_FinalToState 169
#define TK_NotFinalToState 170
#define TK_NotStartToState 171
#define TK_MiddleToState 172
#define TK_StartFromState 173
#define TK_AllFromState 174
#define TK_FinalFromState 175
#define TK_NotFinalFromState 176
#define TK_NotStartFromState 177
#define TK_MiddleFromState 178
#define RE_Slash 179
#define RE_SqOpen 180
#define RE_SqOpenNeg 181
#define RE_SqClose 182
#define RE_Dot 183
#define RE_Star 184
#define RE_Dash 185
#define RE_Char 186
#define IL_WhiteSpace 187
#define IL_Comment 188
#define IL_Literal 189
#define IL_Symbol 190
#define KW_Machine 191
#define KW_Include 192
#define KW_Import 193
#define KW_Write 194
#define KW_Action 195
#define KW_AlphType 196
#define KW_Range 197
#define KW_GetKey 198
#define KW_InWhen 199
#define KW_When 200
#define KW_OutWhen 201
#define KW_Eof 202
#define KW_Err 203
#define KW_Lerr 204
#define KW_To 205
#define KW_From 206
#define KW_Export 207
#define KW_PrePush 208
#define KW_PostPop 209
#define KW_Break 210
#define KW_Exec 211
#define KW_Hold 212
#define KW_PChar 213
#define KW_Char 214
#define KW_Goto 215
#define KW_Call 216
#define KW_Ret 217
#define KW_CurState 218
#define KW_TargState 219
#define KW_Entry 220
#define KW_Next 221
#define KW_Variable 222
#define KW_Access 223
#define _eof 224

#line 124 "rlparse.kh"

#endif