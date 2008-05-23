
if test (uname) = Darwin
	function prompt_pwd --description "Print the current working directory, shortend to fit the prompt"
		if test "$PWD" != "$HOME"
			printf "%s" (echo $PWD|sed -e 's|/private||' -e "s|^$HOME|~|" -e 's-/\([^/]\)\([^/]*\)-/\1-g')
			echo $PWD|sed -e 's-.*/[^/]\([^/]*$\)-\1-'
		else
			echo '~'
		end
	end
else
	function prompt_pwd --description "Print the current working directory, shortend to fit the prompt"
		switch "$PWD" 
			case "$HOME"
			echo '~'

			case '*'
			printf "%s" (echo $PWD|sed -e "s|^$HOME|~|" -e 's-/\([^/]\)\([^/]*\)-/\1-g')
			echo $PWD|sed -n -e 's-.*/.\([^/]*\)-\1-p'
		end
	end
end

