;;; -*-Emacs-Lisp-*- General command interpreter in a window stuff
;;; Copyright Olin Shivers (1988).
;;; See file COPYING

;;; The changelog is at the end of file.

;;; Please send me bug reports, bug fixes, and extensions, so that I can
;;; merge them into the master source.
;;;     - Olin Shivers (shivers@cs.cmu.edu)

;;; This file defines a a shell-in-a-buffer package (cmushell mode) built on
;;; top of comint mode.  Cmushell mode is similar to, and intended to replace,
;;; its counterpart in the standard gnu emacs release. This replacement is
;;; more featureful, robust, and uniform than the released version.

;;; Since this mode is built on top of the general command-interpreter-in-
;;; a-buffer mode (comint mode), it shares a common base functionality, 
;;; and a common set of bindings, with all modes derived from comint mode.
;;; This makes these modes easier to use.

;;; For documentation on the functionality provided by comint mode, and
;;; the hooks available for customising it, see the file comint.el.
;;; For further information on cmushell mode, see the comments below.

;;; Needs fixin:
;;; When sending text from a source file to a subprocess, the process-mark can 
;;; move off the window, so you can lose sight of the process interactions.
;;; Maybe I should ensure the process mark is in the window when I send
;;; text to the process? Switch selectable?

(require 'comint)
(provide 'cmushell)

;; YOUR .EMACS FILE
;;=============================================================================
;; Some suggestions for your .emacs file.
;;
;; ; If cmushell lives in some non-standard directory, you must tell emacs
;; ; where to get it. This may or may not be necessary.
;; (setq load-path (cons (expand-file-name "~jones/lib/emacs") load-path))
;;
;; ; Autoload cmushell from file cmushell.el
;; (autoload 'cmushell "cmushell"
;;           "Run an inferior shell process."
;;           t)
;;
;; ; Define C-c C-t to run my favorite command in cmushell mode:
;; (setq cmushell-load-hook
;;       '((lambda () 
;;           (define-key cmushell-mode-map "\C-c\C-t" 'favorite-cmd))))


;;; Brief Command Documentation:
;;;============================================================================
;;; Comint Mode Commands: (common to cmushell and all comint-derived modes)
;;;
;;; m-p	    comint-previous-input    	    Cycle backwards in input history
;;; m-n	    comint-next-input  	    	    Cycle forwards
;;; c-c r   comint-previous-input-matching  Search backwards in input history
;;; return  comint-send-input
;;; c-a     comint-bol                      Beginning of line; skip prompt.
;;; c-d	    comint-delchar-or-maybe-eof	    Delete char unless at end of buff.
;;; c-c c-u comint-kill-input	    	    ^u
;;; c-c c-w backward-kill-word    	    ^w
;;; c-c c-c comint-interrupt-subjob 	    ^c
;;; c-c c-z comint-stop-subjob	    	    ^z
;;; c-c c-\ comint-quit-subjob	    	    ^\
;;; c-c c-o comint-kill-output		    Delete last batch of process output
;;; c-c c-r comint-show-output		    Show last batch of process output
;;;         send-invisible                  Read line w/o echo & send to proc
;;;         comint-continue-subjob	    Useful if you accidentally suspend
;;;					        top-level job.
;;; comint-mode-hook is the comint mode hook.

;;; Shell Mode Commands:
;;;         cmushell			    Fires up the shell process.
;;; tab     comint-dynamic-complete	    Complete a partial file name
;;; m-?     comint-dynamic-list-completions List completions in help buffer
;;; 	    dirs    			    Resync the buffer's dir stack.
;;; 	    dirtrack-toggle                 Turn dir tracking on/off.
;;;
;;; The cmushell mode hook is cmushell-mode-hook
;;; The cmushell-load-hook is run after this file is loaded.
;;; comint-prompt-regexp is initialised to shell-prompt-pattern, for backwards
;;; compatibility.

;;; Read the rest of this file for more information.

;;; SHELL.EL COMPATIBILITY
;;;============================================================================
;;; In brief: this package should have no trouble coexisting with shell.el.
;;; 
;;; Most customising variables -- e.g., explicit-shell-file-name -- are the
;;; same, so the users shouldn't have much trouble. Hooks have different
;;; names, however, so you can customise shell mode differently from cmushell
;;; mode. You basically just have to remember to type M-x cmushell instead of
;;; M-x shell.
;;; 
;;; It would be nice if this file was completely plug-compatible with the old
;;; shell package -- if you could just name this file shell.el, and have it
;;; transparently replace the old one. But you can't.  Several other packages
;;; (tex-mode, background, dbx, gdb, kermit, monkey, prolog, telnet) are also
;;; clients of shell mode. These packages assume detailed knowledge of shell
;;; mode internals in ways that are incompatible with cmushell mode (mostly
;;; because of cmushell mode's greater functionality).  So, unless we are
;;; willing to port all of these packages, we can't have this file be a
;;; complete replacement for shell.el -- that is, we can't name this file
;;; shell.el, and its main entry point (shell), because dbx.el will break
;;; when it loads it in and tries to use it.
;;; 
;;; There are two ways to fix this. One: rewrite these other modes to use the
;;; new package. This is a win, but can't be assumed. The other, backwards
;;; compatible route, is to make this package non-conflict with shell.el, so
;;; both files can be loaded in at the same time. And *that* is why some
;;; functions and variables have different names: (cmushell),
;;; cmushell-mode-map, that sort of thing. All the names have been carefully
;;; chosen so that shell.el and cmushell.el won't tromp on each other.

;;; Customisation and Buffer Variables
;;; ===========================================================================
;;; 

;In loaddefs.el now.
;(defconst shell-prompt-pattern
;  "^[^#$%>]*[#$%>] *"
;  "*Regexp used by Newline command to match subshell prompts.
;;; Change the doc string for shell-prompt-pattern:
(put 'shell-prompt-pattern 'variable-documentation
  "Regexp to match prompts in the inferior shell.
Defaults to \"^[^#$%>]*[#$%>] *\", which works pretty well.
This variable is used to initialise comint-prompt-regexp in the 
shell buffer.

This is a fine thing to set in your .emacs file.")

(defvar shell-popd-regexp "popd"
  "*Regexp to match subshell commands equivalent to popd.")

(defvar shell-pushd-regexp "pushd"
  "*Regexp to match subshell commands equivalent to pushd.")

(defvar shell-cd-regexp "cd"
  "*Regexp to match subshell commands equivalent to cd.")

(defvar explicit-shell-file-name nil
  "*If non-nil, is file name to use for explicitly requested inferior shell.")

(defvar explicit-csh-args
  (if (eq system-type 'hpux)
      ;; -T persuades HP's csh not to think it is smarter
      ;; than us about what terminal modes to use.
      '("-i" "-T")
    '("-i"))
  "*Args passed to inferior shell by M-x cmushell, if the shell is csh.
Value is a list of strings, which may be nil.")

;;; All the above vars aren't prefixed "cmushell-" to make them
;;; backwards compatible w/shell.el and old .emacs files.

(defvar cmushell-dirstack nil
  "List of directories saved by pushd in this buffer's shell.")

(defvar cmushell-dirstack-query "dirs"
  "Command used by shell-resync-dirlist to query shell.")

(defvar cmushell-mode-map '())
(cond ((not cmushell-mode-map)
       (setq cmushell-mode-map (full-copy-sparse-keymap comint-mode-map))
       (define-key cmushell-mode-map "\t" 'comint-dynamic-complete)
       (define-key cmushell-mode-map "\M-?"  'comint-dynamic-list-completions)))

(defvar cmushell-mode-hook '()
  "*Hook for customising cmushell mode")


;;; Basic Procedures
;;; ===========================================================================
;;;

(defun cmushell-mode ()
  "Major mode for interacting with an inferior shell.
Return after the end of the process' output sends the text from the 
    end of process to the end of the current line.
Return before end of process output copies rest of line to end (skipping
    the prompt) and sends it.
M-x send-invisible reads a line of text without echoing it, and sends it to
    the shell.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

cd, pushd and popd commands given to the shell are watched by Emacs to keep
this buffer's default directory the same as the shell's working directory.
M-x dirs queries the shell and resyncs Emacs' idea of what the current 
    directory stack is.
M-x dirtrack-toggle turns directory tracking on and off.

\\{cmushell-mode-map}
Customisation: Entry to this mode runs the hooks on comint-mode-hook and
cmushell-mode-hook (in that order).

Variables shell-cd-regexp, shell-pushd-regexp and shell-popd-regexp are used
to match their respective commands."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp shell-prompt-pattern)
  (setq major-mode 'cmushell-mode)
  (setq mode-name "CMU shell")
  (use-local-map cmushell-mode-map)
  (make-local-variable 'cmushell-dirstack)
  (setq cmushell-dirstack nil)
  (make-local-variable 'cmushell-dirtrackp)
  (setq cmushell-dirtrackp t)
  (setq comint-input-sentinel 'cmushell-directory-tracker)
  (run-hooks 'cmushell-mode-hook))


(defun cmushell ()
  "Run an inferior shell, with I/O through buffer *cmushell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, 
 just switch to buffer *cmushell*.
Program used comes from variable explicit-shell-file-name,
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file ~/.emacs_SHELLNAME exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in cmushell-mode, giving commands for sending input
and controlling the subjobs of the shell.  See cmushell-mode.
See also variable shell-prompt-pattern.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-arguments'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive)
  (let ((buffer-name (concat "*" cmushell-buffer-base-name "*")))
    (cond ((not (comint-check-proc buffer-name))
	   (let* ((prog (or explicit-shell-file-name
			    (getenv "ESHELL")
			    (getenv "SHELL")
			    "/bin/sh"))		     
		  (name (file-name-nondirectory prog))
		  (startfile (concat "~/.emacs_" name))
		  (xargs-name (intern-soft (concat "explicit-" name "-args"))))
	     (set-buffer (apply 'make-comint cmushell-buffer-base-name prog
				(if (file-exists-p startfile) startfile)
				(if (and xargs-name (boundp xargs-name))
				    (symbol-value xargs-name)
				    '("-i"))))
	     (cmushell-mode))))
    (switch-to-buffer buffer-name)))

(defvar cmushell-buffer-base-name "cmushell"
  "Name of buffer in which CMU shell is to run.")


;;; Directory tracking
;;; ===========================================================================
;;; This code provides the cmushell mode input sentinel
;;;     CMUSHELL-DIRECTORY-TRACKER
;;; that tracks cd, pushd, and popd commands issued to the shell, and
;;; changes the current directory of the shell buffer accordingly.
;;;
;;; This is basically a fragile hack, although it's more accurate than
;;; the released version in shell.el. It has the following failings:
;;; 1. It doesn't know about the cdpath shell variable.
;;; 2. It only spots the first command in a command sequence. E.g., it will
;;;    miss the cd in "ls; cd foo"
;;; 3. More generally, any complex command (like ";" sequencing) is going to
;;;    throw it. Otherwise, you'd have to build an entire shell interpreter in
;;;    emacs lisp.  Failing that, there's no way to catch shell commands where
;;;    cd's are buried inside conditional expressions, aliases, and so forth.
;;;
;;; The whole approach is a crock. Shell aliases mess it up. File sourcing
;;; messes it up. You run other processes under the shell; these each have
;;; separate working directories, and some have commands for manipulating
;;; their w.d.'s (e.g., the lcd command in ftp). Some of these programs have
;;; commands that do *not* affect the current w.d. at all, but look like they
;;; do (e.g., the cd command in ftp).  In shells that allow you job
;;; control, you can switch between jobs, all having different w.d.'s. So
;;; simply saying %3 can shift your w.d..
;;;
;;; The solution is to relax, not stress out about it, and settle for
;;; a hack that works pretty well in typical circumstances. Remember
;;; that a half-assed solution is more in keeping with the spirit of Unix, 
;;; anyway. Blech.
;;;
;;; One good hack not implemented here for users of programmable shells
;;; is to program up the shell w.d. manipulation commands to output
;;; a coded command sequence to the tty. Something like
;;;     ESC | <cwd> |
;;; where <cwd> is the new current working directory. Then trash the
;;; directory tracking machinery currently used in this package, and
;;; replace it with a process filter that watches for and strips out
;;; these messages.

;;; REGEXP is a regular expression. STR is a string. START is a fixnum.
;;; Returns T if REGEXP matches STR where the match is anchored to start
;;; at position START in STR. Sort of like LOOKING-AT for strings.
(defun cmushell-front-match (regexp str start)
  (eq start (string-match regexp str start)))

(defun cmushell-directory-tracker (str)
  "Tracks cd, pushd and popd commands issued to the shell.
This function is called on each input passed to the shell.
It watches for cd, pushd and popd commands and sets the buffer's
default directory to track these commands.

You may toggle this tracking on and off with M-x dirtrack-toggle.
If emacs gets confused, you can resync with the shell with M-x dirs.

See variables shell-cd-regexp, shell-pushd-regexp, and shell-popd-regexp.
Environment variables are expanded, see function substitute-in-file-name."
  (condition-case err
    (cond (cmushell-dirtrackp
	   (string-match "^\\s *" str) ; skip whitespace
	   (let ((bos (match-end 0))
		 (x nil))
	     (cond ((setq x (cmushell-match-cmd-w/optional-arg shell-popd-regexp
							       str bos))
		    (cmushell-process-popd (substitute-in-file-name x)))
		   ((setq x (cmushell-match-cmd-w/optional-arg shell-pushd-regexp
							       str bos))
		    (cmushell-process-pushd (substitute-in-file-name x)))
		   ((setq x (cmushell-match-cmd-w/optional-arg shell-cd-regexp
							       str bos))
		    (cmushell-process-cd (substitute-in-file-name x)))))))
    (error (message (car (cdr err))))))


;;; Try to match regexp CMD to string, anchored at position START.
;;; CMD may be followed by a single argument. If a match, then return
;;; the argument, if there is one, or the empty string if not. If
;;; no match, return nil.

(defun cmushell-match-cmd-w/optional-arg (cmd str start)
  (and (cmushell-front-match cmd str start)
       (let ((eoc (match-end 0))) ; end of command
	 (cond ((cmushell-front-match "\\s *\\(\;\\|$\\)" str eoc)
		"")			; no arg
	       ((cmushell-front-match "\\s +\\([^ \t\;]+\\)\\s *\\(\;\\|$\\)"
				      str eoc)
		(substring str (match-beginning 1) (match-end 1))) ; arg
	       (t nil))))) ; something else.
;;; The first regexp is [optional whitespace, (";" or the end of string)].
;;; The second regexp is [whitespace, (an arg), optional whitespace,
;;;     (";" or end of string)].


;;; popd [+n]
(defun cmushell-process-popd (arg)
  (let ((num (if (zerop (length arg)) 0 ; no arg means +0
		 (cmushell-extract-num arg))))
    (if (and num (< num (length cmushell-dirstack)))
	(if (= num 0) ; condition-case because the CD could lose.
	    (condition-case nil (progn (cd (car cmushell-dirstack))
				       (setq cmushell-dirstack
					     (cdr cmushell-dirstack))
				       (cmushell-dirstack-message))
	      (error (message "Couldn't cd.")))
	    (let* ((ds (cons nil cmushell-dirstack))
		   (cell (nthcdr (- num 1) ds)))
	      (rplacd cell (cdr (cdr cell)))
	      (setq cmushell-dirstack (cdr ds))
	      (cmushell-dirstack-message)))
	(message "Bad popd."))))


;;; cd [dir]
(defun cmushell-process-cd (arg)
  (condition-case nil (progn (cd (if (zerop (length arg)) (getenv "HOME")
				     arg))
			     (cmushell-dirstack-message))
	   (error (message "Couldn't cd."))))


;;; pushd [+n | dir]
(defun cmushell-process-pushd (arg)
  (if (zerop (length arg))
      ;; no arg -- swap pwd and car of shell stack
      (condition-case nil (if cmushell-dirstack
			      (let ((old default-directory))
				(cd (car cmushell-dirstack))
				(setq cmushell-dirstack
				      (cons old (cdr cmushell-dirstack)))
				(cmushell-dirstack-message))
			      (message "Directory stack empty."))
	(message "Couldn't cd."))

      (let ((num (cmushell-extract-num arg)))
	(if num				; pushd +n
	    (if (> num (length cmushell-dirstack))
		(message "Directory stack not that deep.")
		(let* ((ds (cons default-directory cmushell-dirstack))
		       (dslen (length ds))
		       (front (nthcdr num ds))
		       (back (reverse (nthcdr (- dslen num) (reverse ds))))
		       (new-ds (append front back)))
		  (condition-case nil
		      (progn (cd (car new-ds))
			     (setq cmushell-dirstack (cdr new-ds))
			     (cmushell-dirstack-message))
		    (error (message "Couldn't cd.")))))
	       
	    ;; pushd <dir>
	    (let ((old-wd default-directory))
	      (condition-case nil
		  (progn (cd arg)
			 (setq cmushell-dirstack
			       (cons old-wd cmushell-dirstack))
			 (cmushell-dirstack-message))
		(error (message "Couldn't cd."))))))))

;; If STR is of the form +n, for n>0, return n. Otherwise, nil.
(defun cmushell-extract-num (str)
  (and (string-match "^\\+[1-9][0-9]*$" str)
       (string-to-int str)))


(defun cmushell-dirtrack-toggle ()
  "Turn directory tracking on and off in a cmushell buffer."
  (interactive)
  (setq cmushell-dirtrackp (not cmushell-dirtrackp))
  (message "directory tracking %s."
	   (if cmushell-dirtrackp "ON" "OFF")))

;;; For your typing convenience:
(fset 'dirtrack-toggle 'cmushell-dirtrack-toggle)


(defun cmushell-resync-dirs ()
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to 
cmushell-dirstack-query (default \"dirs\"), reads the next
line output and parses it to form the new directory stack.
DON'T issue this command unless the buffer is at a shell prompt.
Also, note that if some other subprocess decides to do output
immediately after the query, its output will be taken as the
new directory stack -- you lose. If this happens, just do the
command again."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc)))
    (goto-char pmark)
    (insert cmushell-dirstack-query) (insert "\n")
    (sit-for 0) ; force redisplay
    (comint-send-string proc cmushell-dirstack-query) 
    (comint-send-string proc "\n")
    (set-marker pmark (point))
    (let ((pt (point))) ; wait for 1 line
      ;; This extra newline prevents the user's pending input from spoofing us.
      (insert "\n") (backward-char 1)
      (while (not (looking-at ".+\n"))
	(accept-process-output proc)
	(goto-char pt)))
    (goto-char pmark) (delete-char 1) ; remove the extra newline
    ;; That's the dirlist. grab it & parse it.
    (let* ((dl (buffer-substring (match-beginning 0) (- (match-end 0) 1)))
	   (dl-len (length dl))
	   (ds '())			; new dir stack
	   (i 0))
      (while (< i dl-len)
	;; regexp = optional whitespace, (non-whitespace), optional whitespace
	(string-match "\\s *\\(\\S +\\)\\s *" dl i) ; pick off next dir
	(setq ds (cons (substring dl (match-beginning 1) (match-end 1))
		       ds))
	(setq i (match-end 0)))
      (let ((ds (reverse ds)))
	(condition-case nil
	    (progn (cd (car ds))
		   (setq cmushell-dirstack (cdr ds))
		   (cmushell-dirstack-message))
	  (error (message "Couldn't cd.")))))))

;;; For your typing convenience:
(fset 'dirs 'cmushell-resync-dirs)


;;; Show the current dirstack on the message line.
;;; Pretty up dirs a bit by changing "/usr/jqr/foo" to "~/foo".
;;; (This isn't necessary if the dirlisting is generated with a simple "dirs".)
;;; All the commands that mung the buffer's dirstack finish by calling
;;; this guy.
(defun cmushell-dirstack-message ()
  (let ((msg "")
	(ds (cons default-directory cmushell-dirstack)))
    (while ds
      (let ((dir (car ds)))
	(if (string-match (format "^%s\\(/\\|$\\)" (getenv "HOME")) dir)
	    (setq dir (concat "~/" (substring dir (match-end 0)))))
	(if (string-equal dir "~/") (setq dir "~"))
	(setq msg (concat msg dir " "))
	(setq ds (cdr ds))))
    (message msg)))



;;; Interfacing to client packages (and converting them)
;;;============================================================================
;;; Several gnu packages (tex-mode, background, dbx, gdb, kermit, prolog, 
;;; telnet are some) use the shell package as clients. Most of them would
;;; be better off using the comint package directly, but they predate it.
;;; The catch is that most of these packages (dbx, gdb, prolog, telnet)
;;; assume total knowledge of all the local variables that shell mode
;;; functions depend on. So they (kill-all-local-variables), then create
;;; the few local variables that shell.el functions depend on. Alas,
;;; cmushell.el functions depend on a different set of vars (for example,
;;; the input history ring is a local variable in cmushell.el's shell mode,
;;; whereas there is no input history ring in shell.el's shell mode).
;;; So we have a situation where the greater functionality of cmushell.el
;;; is biting us -- you can't just replace shell will cmushell.
;;;
;;; Altering these packages to use comint mode directly should *greatly*
;;; improve their functionality, and is actually pretty easy. It's
;;; mostly a matter of renaming a few variable names. See comint.el for more.
;;;     -Olin



;;; Do the user's customisation...
;;;===============================
(defvar cmushell-load-hook nil
  "This hook is run when cmushell is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'cmushell-load-hook)

;;; Change Log
;;; ===========================================================================
;;; Olin 8/88
;;; Created.
;;;
;;; Olin 5/26/90
;;; - Split cmulisp and cmushell modes into separate files. 
;;;   Not only is this a good idea, it's apparently the way it'll be rel 19.
;;; - Souped up the directory tracking; it now can handle pushd, pushd +n, 
;;;   and popd +n.
;;; - Added cmushell-dirtrack-toggle command to toggle the directory
;;;   tracking that cmushell tries to do. This is useful, for example,
;;;   when you are running ftp -- it prevents the ftp "cd" command from
;;;   spoofing the tracking machinery. This command is also named 
;;;   dirtrack-toggle, so you need only type M-x dirtrack to run it.
;;; - Added cmushell-resync-dirs command. This queries the shell
;;;   for the current directory stack, and resets the buffer's stack
;;;   accordingly. This command is also named dirs, so you need only type
;;;   M-x dirs to run it.
;;; - Bits of the new directory tracking code were adapted from source
;;;   contributed by Vince Broman, Jeff Peck, and Barry Warsaw.
;;; - See also the improvements made to comint.el at the same time.
;;; - Renamed several variables. Mostly this comprised changing "shell"
;;;   to "cmushell" in the names. The only variables that are not prefixed
;;;   with "cmushell-" are the ones that are common with shell.el:
;;;       explicit-shell-file-name shell-prompt-pattern explicit-csh-args 
;;;       and shell-cd/popd/pushd-regexp
;;;   The variables and functions that were changed to have "cmushell-" 
;;;   prefixes are:
;;;       shell-directory-stack (v), shell-directory-tracker (f)
;;;   This should not affect users, only elisp hackers. Hopefully
;;;   one day shell.el will just go away, and we can drop all this
;;;   "cmushell" bullshit.
;;; - Upgraded process sends to use comint-send-string instead of
;;;   process-send-string.
;;;
;;; Olin 6/14/90
;;; - If your shell is named <shellname>, and a variable named
;;;   explicit-<shellname>-args exists, cmushell is supposed
;;;   to use its value as the arglist to the shell invocation.
;;;   E.g., if you define explicit-csh-args to be 
;;;   ("-ifx"), then when cmushell cranks up a csh, it execs it
;;;   as "csh -ifx". This is what is documented. What has actually
;;;   been the case is that the variable checked is
;;;   explicit-<shellname>-arguments, not explicit-<shellname>-args.
;;;   The documentation has been changed to conform to the code (for
;;;   backwards compatibility with shell.el). This bug is inherited from
;;;   the same bug in shell.el.
;;;   This bug reported by Stephen Anderson.
;;;
;;; Olin 9/5/90
;;; - Arguments to cd, popd, and pushd now have their env vars expanded
;;;   out by the tracking machinery. So if you say "cd $SRCDIR/funs", the
;;;   $SRCDIR var will be replaced by its value *in emacs' process
;;;   environment*. If this is different from the shell's binding of the
;;;   variable, you lose.  Several users needed this feature, fragile
;;;   though it may be.  The fix was contributed by sk@thp.Uni-Koeln.DE.
;;;
;;; Olin 3/12/91
;;; - Moved comint-dynamic-complete (filename completion) from M-tab to tab.
