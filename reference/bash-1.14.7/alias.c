/* alias.c -- Not a full alias, but just the kind that we use in the
   shell.  Csh style alias is somewhere else (`over there, in a box'). */

/* Copyright (C) 1987,1991 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   Bash is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with Bash; see the file COPYING.  If not, write to the Free
   Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include "bashansi.h"
#include "config.h"
#include "command.h"
#include "general.h"
#include "hash.h"
#include "alias.h"

static int qsort_alias_compare ();

/* Non-zero means expand all words on the line.  Otherwise, expand
   after first expansion if the expansion ends in a space. */
int alias_expand_all = 0;

/* The list of aliases that we have. */
HASH_TABLE *aliases = (HASH_TABLE *)NULL;

void
initialize_aliases ()
{
  if (!aliases)
    aliases = make_hash_table (0);
}

/* Scan the list of aliases looking for one with NAME.  Return NULL
   if the alias doesn't exist, else a pointer to the assoc. */
ASSOC *
find_alias (name)
     char *name;
{
  BUCKET_CONTENTS *al;

  if (!aliases)
    return ((ASSOC *)NULL);
  else
    al = find_hash_item (name, aliases);

  if (al)
    return ((ASSOC *)al->data);
  else
    return ((ASSOC *)NULL);
}

/* Return the value of the alias for NAME, or NULL if there is none. */
char *
get_alias_value (name)
     char *name;
{
  ASSOC *alias = find_alias (name);
  if (alias)
    return (alias->value);
  else
    return ((char *)NULL);
}

/* Make a new alias from NAME and VALUE.  If NAME can be found,
   then replace its value. */
void
add_alias (name, value)
     char *name, *value;
{
  ASSOC *temp = (ASSOC *)NULL;

  if (!aliases)
    initialize_aliases ();
  else
    temp = find_alias (name);

  if (temp)
    {
      free (temp->value);
      temp->value = savestring (value);
    }
  else
    {
      BUCKET_CONTENTS *elt;

      temp = (ASSOC *)xmalloc (sizeof (ASSOC));
      temp->name = savestring (name);
      temp->value = savestring (value);

      elt = add_hash_item (savestring (name), aliases);
      elt->data = (char *)temp;
    }
}

/* Remove the alias with name NAME from the alias table.  Returns
   the number of aliases left in the table, or -1 if the alias didn't
   exist. */
int
remove_alias (name)
     char *name;
{
  BUCKET_CONTENTS *elt;

  if (!aliases)
    return (-1);

  elt = remove_hash_item (name, aliases);
  if (elt)
    {
      ASSOC *t;

      t = (ASSOC *)elt->data;
      free (t->name);
      free (t->value);
      free (elt->key);		/* alias name */
      free (t);

      return (aliases->nentries);
    }
  return (-1);
}

/* Delete a hash bucket chain of aliases. */
static void
delete_alias_list (alias_list)
     BUCKET_CONTENTS *alias_list;
{
  register BUCKET_CONTENTS *bp, *temp;
  register ASSOC *a;

  for (bp = alias_list; bp; )
    {
      temp = bp->next;
      a = (ASSOC *)bp->data;
      free (a->value);
      free (a->name);
      free (bp->data);
      free (bp->key);
      free (bp);
      bp = temp;
    }
}

/* Delete all aliases. */
void
delete_all_aliases ()
{
  register int i;

  if (!aliases)
    return;

  for (i = 0; i < aliases->nbuckets; i++)
    {
      register BUCKET_CONTENTS *bp;

      bp = get_hash_bucket (i, aliases);
      delete_alias_list (bp);
    }
  free (aliases);
  aliases = (HASH_TABLE *)NULL;
}

/* Return an array of aliases that satisfy the conditions tested by FUNCTION.
   If FUNCTION is NULL, return all aliases. */
static ASSOC **
map_over_aliases (function)
     Function *function;
{
  register int i;
  register BUCKET_CONTENTS *tlist;
  ASSOC *alias, **list = (ASSOC **)NULL;
  int list_index = 0, list_size = 0;

  for (i = 0; i < aliases->nbuckets; i++)
    {
      tlist = get_hash_bucket (i, aliases);

      while (tlist)
	{
	  alias = (ASSOC *)tlist->data;

	  if (!function || (*function) (alias))
	    {
	      if (list_index + 1 >= list_size)
		list = (ASSOC **)
		  xrealloc ((char *)list, (list_size += 20) * sizeof (ASSOC *));

	      list[list_index++] = alias;
	      list[list_index] = (ASSOC *)NULL;
	    }
	  tlist = tlist->next;
	}
    }
  return (list);
}

static void
sort_aliases (array)
     ASSOC **array;
{
  qsort (array, array_len ((char **)array), sizeof (ASSOC *), qsort_alias_compare);
}

static int
qsort_alias_compare (as1, as2)
     ASSOC **as1, **as2;
{
  int result;
  
  if ((result = (*as1)->name[0] - (*as2)->name[0]) == 0)
    result = strcmp ((*as1)->name, (*as2)->name);

  return (result);
}
        
/* Return a sorted list of all defined aliases */     
ASSOC **
all_aliases ()
{
  ASSOC **list;

  if (!aliases)
    return ((ASSOC **)NULL);

  list = map_over_aliases ((Function *)NULL);
  if (list)
    sort_aliases (list);
  return (list);
}

char *
alias_expand_word (s)
     char *s;
{
  ASSOC *r = find_alias (s);

  if (r)
    return (savestring (r->value));
  else
    return ((char *)NULL);
}

/* Return non-zero if CHARACTER is a member of the class of characters
   that are self-delimiting in the shell (this really means that these
   characters delimit tokens). */
#define self_delimiting(character) (member ((character), " \t\n\r;|&()"))

/* Return non-zero if CHARACTER is a member of the class of characters
   that delimit commands in the shell. */
#define command_separator(character) (member ((character), "\r\n;|&("))

/* If this is 1, we are checking the next token read for alias expansion
   because it is the first word in a command. */
static int command_word;

/* This is for skipping quoted strings in alias expansions. */
#define quote_char(c)  (((c) == '\'') || ((c) == '"'))

/* Consume a quoted string from STRING, starting at string[START] (so
   string[START] is the opening quote character), and return the index
   of the closing quote character matching the opening quote character.
   This handles single matching pairs of unquoted quotes; it could afford
   to be a little smarter... This skips words between balanced pairs of
   quotes, words where the first character is quoted with a `\', and other
   backslash-escaped characters. */

static int
skipquotes (string, start)
     char *string;
     int start;
{
  register int i;
  int delimiter = string[start];

  /* i starts at START + 1 because string[START] is the opening quote
     character. */
  for (i = start + 1 ; string[i] ; i++)
    {
      if (string[i] == '\\')
	{
	  i++;		/* skip backslash-quoted quote characters, too */
	  continue;
	}

      if (string[i] == delimiter)
	return i;
    }
  return (i);
}

/* Skip the white space and any quoted characters in STRING, starting at
   START.  Return the new index into STRING, after zero or more characters
   have been skipped. */
static int
skipws (string, start)
     char *string;
     int start;
{
  register int i = 0;
  int pass_next, backslash_quoted_word, peekc;

  /* skip quoted strings, in ' or ", and words in which a character is quoted
     with a `\'. */
  backslash_quoted_word = pass_next = 0;

  /* Skip leading whitespace (or separator characters), and quoted words.
     But save it in the output.  */

  for (i = start; string[i]; i++)
    {
      if (pass_next)
	{
	  pass_next = 0;
	  continue;
	}

      if (whitespace (string[i]))
	{
	  backslash_quoted_word = 0; /* we are no longer in a backslash-quoted word */
	  continue;
	}

      if (string[i] == '\\')
	{
	  peekc = string[i+1];
	  if (isletter (peekc))
	    backslash_quoted_word++;	/* this is a backslash-quoted word */
	  else
	    pass_next++;
	  continue;
	}

      /* This only handles single pairs of non-escaped quotes.  This
	 overloads backslash_quoted_word to also mean that a word like
	 ""f is being scanned, so that the quotes will inhibit any expansion
	 of the word. */
      if (quote_char(string[i]))
	{
	  i = skipquotes (string, i);
	  /* This could be a line that contains a single quote character,
	     in which case skipquotes () terminates with string[i] == '\0'
	     (the end of the string).  Check for that here. */
	  if (string[i] == '\0')
	    break;

	  peekc = string[i + 1];
	  if (isletter (peekc))
	    backslash_quoted_word++;
	  continue;
	}

      /* If we're in the middle of some kind of quoted word, let it
	 pass through. */
      if (backslash_quoted_word)
	continue;

      /* If this character is a shell command separator, then set a hint for
	 alias_expand that the next token is the first word in a command. */

      if (command_separator (string[i]))
	{
	  command_word++;
	  continue;
	}
      break;
    }
  return (i);
}

/* Characters that may appear in a token.  Basically, anything except white
   space and a token separator. */
#define token_char(c)	(!((whitespace (string[i]) || self_delimiting (string[i]))))

/* Read from START in STRING until the next separator character, and return
   the index of that separator.  Skip backslash-quoted characters.  Call
   skipquotes () for quoted strings in the middle or at the end of tokens,
   so all characters show up (e.g. foo'' and foo""bar) */
static int
rd_token (string, start)
     char *string; 
     int start;
{
  register int i;

  /* From here to next separator character is a token. */
  for (i = start; string[i] && token_char (string[i]); i++)
    {
      if (string[i] == '\\')
	{
	  i++;	/* skip backslash-escaped character */
	  continue;
	}

      /* If this character is a quote character, we want to call skipquotes
	 to get the whole quoted portion as part of this word.  That word
	 will not generally match an alias, even if te unquoted word would
	 have.  The presence of the quotes in the token serves then to 
	 inhibit expansion. */
      if (quote_char (string[i]))
	{
	  i = skipquotes (string, i);
	  /* Now string[i] is the matching quote character, and the
	     quoted portion of the token has been scanned. */
	  continue;
	}
    }
  return (i);
}

/* Return a new line, with any aliases substituted. */
char *
alias_expand (string)
     char *string;
{
  int line_len = 1 + strlen (string);
  char *line = (char *)xmalloc (line_len);
  register int i, j, start;
  char *token = xmalloc (line_len);
  int tl, real_start, expand_next, expand_this_token;
  ASSOC *alias;

  line[0] = i = 0;
  expand_next = 0;
  command_word = 1; /* initialized to expand the first word on the line */

  /* Each time through the loop we find the next word in line.  If it
     has an alias, substitute
     the alias value.  If the value ends in ` ', then try again
     with the next word.  Else, if there is no value, or if
     the value does not end in space, we are done. */

  for (;;)
    {

      token[0] = 0;
      start = i;

      /* Skip white space and quoted characters */
      i = skipws (string, start);

      if (start == i && string[i] == '\0')
	{
	  free (token);
	  return (line);
	}

      /* copy the just-skipped characters into the output string,
	 expanding it if there is not enough room. */
      j = strlen (line);
      tl = i - start;	/* number of characters just skipped */
      if (1 + j + tl >= line_len)
	line = (char *)xrealloc (line, line_len += (50 + tl));
      strncpy (line + j, string + start, tl);
      line[j + tl] = '\0';

      real_start = i;

      command_word = command_word || (command_separator (string[i]));
      expand_this_token = (command_word || expand_next);
      expand_next = 0;

      /* Read the next token, and copy it into TOKEN. */
      start = i;
      i = rd_token (string, start);

      tl = i - start;	/* token length */

      /* If tl == 0, but we're not at the end of the string, then we have a
	 single-character token, probably a delimiter */
      if (tl == 0 && string[i] != '\0')
	{
	  tl = 1;
	  i++;		/* move past it */
	}

      strncpy (token, string + start, tl);
      token [tl] = '\0';

      /* If there is a backslash-escaped character quoted in TOKEN,
	 then we don't do alias expansion.  This should check for all
	 other quoting characters, too. */
      if (strchr (token, '\\'))
	expand_this_token = 0;

      /* If we should be expanding here, if we are expanding all words, or if
	 we are in a location in the string where an expansion is supposed to
	 take place, see if this word has a substitution.  If it does, then do
	 the expansion.  Note that we defer the alias value lookup until we
	 are sure we are expanding this token. */

      if ((token[0]) &&
	  (expand_this_token || alias_expand_all) &&
	  (alias = find_alias (token)))
	{
	  char *v = alias->value;
	  int l = strlen (v);
      
	  /* +3 because we possibly add one more character below. */
	  if ((l + 3) > line_len - (int)strlen (line))
	    line = (char *)xrealloc (line, line_len += (50 + l));

	  strcat (line, v);

	  if ((expand_this_token && l && whitespace (v[l - 1])) ||
	      alias_expand_all)
	    expand_next = 1;
	}
      else
	{
	  int ll = strlen (line);
	  int tlen = i - real_start; /* tlen == strlen(token) */

	  if (ll + tlen + 2 > line_len)
	    line = (char *)xrealloc (line, line_len += 50 + ll + tlen);

	  strncpy (line + ll, string + real_start, tlen);
	  line[ll + tlen] = '\0';
	}
      command_word = 0;
    }
}
