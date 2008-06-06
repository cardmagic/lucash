/** \file parser.h
	The fish parser. 	
*/

#ifndef FISH_PARSER_H
#define FISH_PARSER_H

#include <wchar.h>

#include "proc.h"
#include "util.h"
#include "parser.h"
#include "event.h"

#define PARSER_TEST_ERROR 1
#define PARSER_TEST_INCOMPLETE 2

/**
   event_block_t represents a block on events of the specified type
*/
typedef struct event_block
{
	/**
	   The types of events to block. This is interpreted as a bitset
	   whete the value is 1 for every bit corresponding to a blocked
	   event type. For example, if EVENT_VARIABLE type events should
	   be blocked, (type & 1<<EVENT_BLOCKED) should be set. 

	   Note that EVENT_ANY can be used to specify any event.
	*/
	int type;
	
	/**
	   The next event_block struct
	*/
	struct event_block *next;
}
	event_block_t;



/**
   block_t represents a block of commands. 
*/
typedef struct block
{
	int type; /**< Type of block. Can be one of WHILE, FOR, IF and FUNCTION */
	int skip; /**< Whether execution of the commands in this block should be skipped */
	int tok_pos; /**< The start index of the block */
	int had_command; /**< Set to non-zero once a command has been executed in this block */
	
	/**
	   Status for the current loop block. Can be any of the values from the loop_status enum.
	*/
	int loop_status;

	/**
	   The job that is currently evaluated in the specified block.
	*/
	job_t *job;

	/**
	   Block type-specific data
	*/
	void *data;
	
	/**
	   First block type specific variable
	*/
	union 
	{
		int while_state;  /**< True if the loop condition has not yet been evaluated*/
		wchar_t *for_variable; /**< Name of the variable to loop over */
		int if_state; /**< The state of the if block, can be one of IF_STATE_UNTESTED, IF_STATE_FALSE, IF_STATE_TRUE */
		wchar_t *switch_value; /**< The value to test in a switch block */
		const wchar_t *source_dest; /**< The name of the file to source*/
		event_t *event; /**<The event that triggered this block */		
		wchar_t *function_call_name;
	} param1;

	/**
	   Second block type specific variable
	*/
	union
	{
		array_list_t for_vars; /**< List of values for a for block */	
		int switch_taken; /**< Whether a switch match has already been found */
		process_t *function_call_process;		/**< The process representing this function call */
	} param2;


	/**
	   Name of file that created this block
	*/
	const wchar_t *src_filename;
	
	/**
	   Line number where this block was created
	*/
	int src_lineno;
	
	/**
	   Some naming confusion. This is a pointer to the first element in the list of all event blocks.
	*/
	event_block_t *first_event_block;
	
    /**
	   Next outer block 
	*/
	struct block *outer; 
} block_t;

/** 
	Types of blocks
*/
enum block_type
{
	WHILE, /**< While loop block */
	FOR,  /**< For loop block */
	IF, /**< If block */
	FUNCTION_DEF, /**< Function definition block */
	FUNCTION_CALL, /**< Function invocation block */
	FUNCTION_CALL_NO_SHADOW, /**< Function invocation block with no variable shadowing */
	SWITCH, /**< Switch block */
	FAKE, /**< Fake block */
	SUBST, /**< Command substitution scope */
	TOP, /**< Outermost block */
	BEGIN, /**< Unconditional block */
	SOURCE, /**< Block created by the . (source) builtin */
	EVENT, /**< Block created on event notifier invocation */
	BREAKPOINT, /**< Breakpoint block */
}
;

/**
   Possible states for a loop
*/
enum loop_status 
{
	LOOP_NORMAL, /**< Current loop block executed as normal */
	LOOP_BREAK, /**< Current loop block should be removed */
	LOOP_CONTINUE, /**< Current loop block should be skipped */
};


/**
   Possible states for a while block
*/
enum while_status
{
	WHILE_TEST_FIRST, /**< This is the first command of the first lap of a while loop */
	WHILE_TEST_AGAIN, /**< This is not the first lap of the while loop, but it is the first command of the loop */
	WHILE_TESTED, /**< This is not the first command in the loop */
}
;



/**
   Errors that can be generated by the parser
*/
enum parser_error 
{
	/**
	   No error
	*/
	NO_ERR=0,
	/**
	   An error in the syntax 
	*/
	SYNTAX_ERROR,
	/**
	   Error occured while evaluating commands
	*/
	EVAL_ERROR,
	/**
	   Error while evaluating cmdsubst
	*/
	CMDSUBST_ERROR,
}
;

/** The current innermost block */
extern block_t *current_block;

/** Global event blocks */
extern event_block_t *global_event_block;

/**
   Current block level io redirections 
*/
extern io_data_t *block_io;

/**
  Evaluate the expressions contained in cmd.

  \param cmd the string to evaluate
  \param io io redirections to perform on all started jobs
  \param block_type The type of block to push on the block stack

  \return 0 on success, 1 otherwise
*/
int eval( const wchar_t *cmd, io_data_t *io, int block_type );

/**
  Evaluate line as a list of parameters, i.e. tokenize it and perform parameter expansion and cmdsubst execution on the tokens.
  The output is inserted into output, and should be freed by the caller.

  \param line Line to evaluate
  \param output List to insert output to
*/
int eval_args( const wchar_t *line,
				array_list_t *output );

/**
   Sets the current evaluation error. This function should only be used by libraries that are called by 

   \param ec The new error code
   \param p The character offset at which the error occured
   \param str The printf-style error message filter
*/
void error( int ec, int p, const wchar_t *str, ... );


/**
   Returns a string describing the current parser pisition in the format 'FILENAME (line LINE_NUMBER): LINE'.
   Example: 

   init.fish (line 127): ls|grep pancake
*/
wchar_t *parser_current_line();

/**
   Returns the current line number
*/
int parser_get_lineno();

/**
   Returns the current position in the latest string of the tokenizer.
*/
int parser_get_pos();

/**
   Returns the position where the current job started in the latest string of the tokenizer.
*/
int parser_get_job_pos();

/**
   Set the current position in the latest string of the tokenizer.
*/
void parser_set_pos( int p);

/**
   Get the string currently parsed
*/
const wchar_t *parser_get_buffer();

/**
   Create block of specified type
*/
void parser_push_block( int type);

/**
   Remove the outermost block namespace
*/
void parser_pop_block();

/**
   Return a description of the given blocktype
*/
const wchar_t *parser_get_block_desc( int block );


/**
   Test if the specified string can be parsed, or if more bytes need
   to be read first. The result will have the PARSER_TEST_ERROR bit
   set if there is a syntax error in the code, and the
   PARSER_TEST_INCOMPLETE bit set if the code contains unclosed
   blocks.

   \param buff the text buffer to test
   \param block_level if non-null, the block nesting level will be filled out into this array
   \param out if non-null, any errors in the command will be filled out into this buffer
   \param prefix the prefix string to prepend to each error message written to the \c out buffer
*/
int parser_test( const wchar_t * buff, int *block_level, string_buffer_t *out, const wchar_t *prefix );

/**
   Test if the specified string can be parsed as an argument list,
   e.g. sent to eval_args.  The result has the first bit set if the
   string contains errors, and the second bit is set if the string
   contains an unclosed block.
*/
int parser_test_args( const wchar_t * buff, string_buffer_t *out, const wchar_t *prefix );

/**
   Tell the parser that the specified function may not be run if not
   inside of a conditional block. This is to remove some possibilities
   of infinite recursion.
*/
void parser_forbid_function( wchar_t *function );
/**
   Undo last call to parser_forbid_function().
*/
void parser_allow_function();

/**
   Initialize static parser data
*/
void parser_init();

/**
   Destroy static parser data
*/
void parser_destroy();

/**
   This function checks if the specified string is a help option. 

   \param s the string to test
   \param min_match is the minimum number of characters that must match in a long style option, i.e. the longest common prefix between --help and any other option. If less than 3, 3 will be assumed.
*/
int parser_is_help( wchar_t *s, int min_match );

/**
   Returns the file currently evaluated by the parser. This can be
   different than reader_current_filename, e.g. if we are evaulating a
   function defined in a different file than the one curently read.
*/
const wchar_t *parser_current_filename();

/**
   Write a stack trace starting at the specified block to the specified string_buffer_t
*/
void parser_stack_trace( block_t *b, string_buffer_t *buff);

int parser_get_block_type( const wchar_t *cmd );
const wchar_t *parser_get_block_command( int type );


#endif