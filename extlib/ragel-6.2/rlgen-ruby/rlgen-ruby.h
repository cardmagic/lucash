/*
 *  2007 Victor Hugo Borja <vic@rubyforge.org>
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

#ifndef _RLGEN_RUBY_H
#define _RLGEN_RUBY_H

#include <iostream>
#include "config.h"

#define PROGNAME "rlgen-ruby"

/* Target implementation */
enum RubyImplEnum
{
  MRI,
  Rubinius
};

extern RubyImplEnum rubyImpl;

/* Target output style. */
enum CodeStyleEnum
{
	GenTables,
	GenFTables,
	GenFlat,
	GenFFlat,
	GenGoto,
	GenFGoto,
	GenIpGoto,
	GenSplit

};

extern CodeStyleEnum codeStyle;

extern int gblErrorCount;
extern char machineMain[];

/* Options. */
extern int numSplitPartitions;
extern bool noLineDirectives;

std::ostream &error();

/*
 * Local Variables:
 * mode: c++
 * indent-tabs-mode: 1
 * c-file-style: "bsd"
 * End:
 */

#endif /* _RLGEN_RUBY_H */
