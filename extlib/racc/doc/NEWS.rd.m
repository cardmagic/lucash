= NEWS

== 1.4.5 (2005-11-21)
j
  * [FEATURE CHANGE] --no-extensions ���ץ�������
  * [fix] racc �ѥå������Τߤ� -E ��Ȥ���褦�˽���
  * [fix] --no-omit-actions ��ư��Ƥ��ʤ��ä��Τ���
  * setup.rb 3.4.1.
e
  * [FEATURE CHANGE] --no-extensions option was removed.
  * [fix] racc command should not depend on `raccrt' package.
  * [fix] --no-omit-actions did not work.
  * setup.rb 3.4.1.
.
== 1.4.4 (2003-10-12)
j
  * Ruby 1.8.0 ���б������꡼�������Τ��ѹ��Ϥʤ�
  * -all �ѥå������� strscan, amstd ��Ʊ������Τ��᤿
  * setup.rb 3.2.1
e
  * document changed.
  * -all packages does not include amstd and strscan.
  * setup.rb 3.2.1.
.

== 1.4.3 (2002-11-14)
j
  * [fix] ruby 1.8 �ηٹ��ä���
e
  * [fix] reduce ruby 1.8 warnings.
.

== 1.4.2 (2002-01-29)
j
  * [new] ���������ץ���� --no-extentions
e
  * [new] new option --no-extentions
.

== 1.4.1 (2001-12-02)
j
  * amstd ���¸�ˤʤä� (������ -all �ѥå������إХ�ɥ�Ϸ�³)
  * y2racc racc2y �� 1.4 �б��ˤ���
e
  * now Racc does not depend on amstd library.
  * update y2racc and racc2y for racc 1.4.1
.

== 1.4.0 (2001-11-30)
j
  * ��󥿥���� Ruby �� CVS �����줿�Τˤ��碌�ƥޥ��ʡ��С�����󥢥å�
  * RaccParser, RaccScanner �� GrammarFileParser, GrammarFileScanner
  * �ϥ��� typo ���� (grammer �� grammar)
e
  * minor version up for checking in runtime library into ruby CVS repositry.
  * RaccParser, RaccScanner -&gt; GrammarFileParser, GrammarFileScanner
  * modify typo (grammer -&gt; grammar)
.

== 1.3.12 (2001-11-22)
j
  * ���󥹥ȡ���ΥХ����� (thanks Tanaka Akira)
  * ����������������ɽ���� % ʸ���󡢥����Х��ѿ��θ��Ф���夵����
e
  * modify installer bug (thanks Tanaka Akira)
  * enhance regexp/%-strings/gvar detection in action block
.

== 1.3.11 (2001-08-28)
j
  * ������������ $' $` $/ �ʤɤ��������������
e
  * modify scan error on $' $` $/ etc.
.

== 1.3.10 (2001-08-12)
j
  * cparse.c �Υץ�ȥ����װ㤤��ľ����
e
  * modify prototype missmatch in cparse.c
.

== 1.3.9 (2001-04-07)
j
  * Ruby 1.4 ��(�Ƥ�)�б�����
e
  * support Ruby 1.4 again.
.

== 1.3.8 (2001-03-17)
j
  * �ѡ������顼�λ��˵���̾����Ϥ���褦�ˤ���
  * Racc::Parser#token_to_s
e
  * output symbol name when error
  * Racc::Parser#token_to_str
.

== 1.3.7 (2001-02-04)
j
  * ����ץ�����䤷��
e
  * allow nil for EndOfInput (experimental)
  * more sample grammar files
.

== 1.3.6 (2001-01-22)
j
  * cparse �������ƥ��å���󥯤���Ƥ�ư���褦�ˤ���
e
  * modify cparse.so for static link
.

== 1.3.5 (2001-01-18)
j
  * % ʸ����Υ�����󤬥Х��äƤ�
  * ������̿�� expect
e
  * %-string scanning was wrong
  * new directive "expect"
.

== 1.3.4 (2001-01-11)
j
  * cparse: ����ܥ�Υ����ץ����å������줿
  * cparse: depend ��ä���
  * cparse: rb_iterate ��� GC �������Х�����
e
  * cparse: add type checks
  * cparse: rm depend
  * cparse: does not pass non-VALUE object to rb_iterate()
.

== 1.3.3 (2000-12-25)
j
  * �����ͥ졼������̿Ū�ʥХ���1.3.1 ���麮�� (format.rb)
  * racc --runtime-version
e
  * <em>critical bug</em> in generator (from 1.3.1)
  * racc --runtime-version
.

== 1.3.2 (2000-12-21)
j
  * -E �����Ԥ���Τ�ľ����
  * ���� strscan ��Ʊ�� (y2racc/racc2y ��ɬ��)
e
  * bug with racc -E
  * package strscan togather (again)
.

== 1.3.1 (2000-12-17)
j
  * ����ɽ���η����֤�����ξ�¤�ưŪ�˷��ꤹ�� (RE_DUP_MAX)
  * �ѡ����롼���󤬾�� Ruby �ǤˤʤäƤ��� (�ä�˺��)
e
  * dynamically determine RE_DUP_MAX
  * ruby version routine was used always
.

== 1.3.0 (2000-11-30)
j
  * ������ʤ��� yield �ǥȡ�������Ϥ���褦�ˤʤä�
e
  * can yield(sym,val) from scanner (Parser#yyparse)
.

== 1.2.6 (2000-11-28)
j
  * class M::C �������
e
  * class M::C
.

== 1.2.5 (2000-11-20)
j
  * ���ץ���������ư����ߴ����ץ����� -h -f -p -i -n -c -A
  * ��󥰥��ץ����򥵥ݡ���
  * y2racc, racc2y �ϥǥե���Ȥǥ���������Ĥ��褦�ˤ���
e
  * big changes in option; -h -f -p -i -n -c -A are incompatible
  * support long options
  * y2racc, racc2y leaves actions as default
.

== 1.2.4 (2000-09-13)
j
  * ���󥹥ȡ���ȥɥ�����Ȥ򹹿�
e
  * updates installer and documents
.

== 1.2.3 (2000-08-14)
j
  * �Ȥ��ʤ���§����ü�������� (������)
  * S/R conflict �λ� nonassoc �ǲ�褹��ʤ�Х��顼
e
  * output useless rules and nonterminals (version 2)
  * nonassoc makes error (never shift/reduce)
.

== 1.2.2 (2000-08-12)
j
  * �������ѹ�
e
  * internal changes
.

== 1.2.1 (2000-08-05)
j
  * yacc �Ȥ��Ѵ����ޥ�� racc2y��y2racc ��ź��
e
  * racc2y, y2racc
.

== 1.2.0 (2000-08-02)
j
  * ���ɤߥ��르�ꥺ��� bison �Τ�Τ��ѹ�
e
  * uses bison's lookahead algorithm
.

== 1.1.6 (2000-07-25)
j
  * �����ʥ������ options �Ȥ��ΰ��� no_result_var
e
  * new keyword "options" and its parameter "no_result_var"
.

== 1.1.5 (2000-07-21)
j
  * [����] token �� convert ���ѹ�
  * �ֿ����ʡץ������ token (��ü��������)
e
  * [IMPORTANT] change keyword "token" to "convert"
  * NEW keyword "token" for token declearation
.

== 1.1.4 (2000-07-13)
j
  * ����ץ뤬�Х��äƤ�
e
  * update installer
  * samples had bugs
.

== 1.1.3 (2000-06-30)
j
  * �����������θƤӽФ����ά���ʤ��褦�ˤ��륪�ץ���� -a
e
  * new option -a; does not omit void action call
.

== 1.1.2 (2000-06-29)
j
  * ������ʤ� strscan ��Ȥ�ʤ��褦�ˤ���
  * ScanError -&gt; Racc::ScanError, ParseError -&gt; Racc::ParseError
  * ���顼��å������򶯲�
e
  * now racc does not use strscan.so
  * ScanError -&gt; Racc::ScanError, ParseError -&gt; Racc::ParseError
  * more friendly error messages
.

== 1.1.1 (2000-06-15)
j
  * require�ߥ� (thanks Tosh����)
  * -v ��Ĥ����conflict����𤵤�ʤ��ʤäƤ���
e
  * require miss
  * conflicts were not reported with -v
.

== 1.1.0 (2000-06-12)
j
  * ������ ��������ɽ�������르�ꥺ��
e
  * use other algolithm for generating state table
.

== 1.0.4 (2000-06-04)
j
  * S/R conflict ��������� .output ���Ϥ������Х����� (Tosh ��������)
  * �Ȥ��ʤ���ü���桦��§��ɽ��
e
  * S/R conflict & -v flag causes unexpected exception (reported by Tosh)
  * output useless nonterminals/rules
.

== 1.0.3 (2000-06-03)
j
  * filter -&gt; collect!
e
  * use Array#collect! instead of #filter.
.

== 1.0.2 (2000-05-16)
j
  * ���󥹥ȡ���򥢥åץǡ���
e
  * update installer (setup.rb)
.

== 1.0.1 (2000-05-12)
j
  * state.rb:  ���ɤߥ롼��������äȤ�����®�� && �ɲåǥХå�
  * �����ɤ��������������ɽ�����Τ������Υե�����ˤĤ�����
  * amstd ���åץǡ��� (1.7.0)
e
  * state.rb:  faster lookahead & debug lalr code
  * refine code
  * update amstd package (1.7.0)
.

== 1.0.0 (2000-05-06)
j
  * �С������ 1.0
e
  * version 1.0
.

== 0.14.6 (2000-05-05)
j
  * �ǥХå����Ϥ�ܺ٤ˤ���
e
  * much more debug output
.

== 0.14.5 (2000-05-01)
j
  * ���󥹥ȡ���� ruby 1.4.4 �Ϥο������ѥ����б�������
e
.

== 0.14.4 (2000-04-09)
j
  * �ѡ����������︺(Racc_arg �ˤޤȤ᤿)
  * state ��������̯�˹�®��(������ʸ������Ѵ�)
e
  * Racc_* are included in Racc_arg
  * faster state generation (a little)
.

== 0.14.3 (2000-04-04)
j
  * cparse �� SYM2ID �� ID2SYM �Υ����å���ʬΥ (thanks ��������)
e
  * check both of SYM2ID and ID2SYM (thanks Katsuyuki Komatsu)
.

== 0.14.2 (2000-04-03)
j
  * ����ܤ� class ���ѡ������顼�ˤʤäƤ��� (thanks ���Ĥ���)
  * �������ե饰 racc -V
e
  * "class" on first line causes parse error (thanks Yoshiki Wada)
  * new option "racc -V"
.

== 0.14.1 (2000-03-31)
j
e
.

== 0.14.0 (2000-03-21)
j
  * ��®�ơ��֥�����
  * ���Ū�˥ե�����̾/���ֹ���Ѵ����᤿(Ruby�ΥХ��Τ��ᡣ)
e
  * implement "fast" table (same to bison)
  * stop line no. conversion temporaliry because of ruby bug
.

== 0.13.1 (2000-03-21)
j
  * --version --copyright �ʤɤ����ޤ�Ư���Ƥʤ��ä� (thanks �դʤФ���)
e
  * racc --version --copyright did not work (thanks Tadayoshi Funaba)
.

== 0.13.0 (2000-03-20)
j
  * yyerror/yyerrok/yyaccept �����
e
  * implement yyerror/yyerrok/yyaccept
.

== 0.12.2 (2000-03-19)
j
  * -E �ե饰���Х��äƤ� (thanks �դʤФ���)
e
  * -E flag had bug
.

== 0.12.1 (2000-03-16)
j
  * �ǥե���ȥ��������η���������äȽ���(�����ᤷ������)
e
  * modify the way to decide default action
.

== 0.12.0 (2000-03-15)
j
  * ������ LALR ������������٤��ʤä��Τ� SLR ��ʻ�Ѥ���褦�ˤ������������硣
e
  * implement real LALR
  * use both SLR and LALR to resolve conflicts
.

== 0.11.3 (2000-03-09)
j
  * ��������ɽ�����ΥХ��ν������ޤ��Ť��ä���������̤ΥХ��⤢��褦����
e
  * modify lookahead routine again
.

== 0.11.2 (2000-03-09)
j
  * cparse �� Symbol ���б��Ǥ��Ƥʤ��ä�
e
  * bug in lookahead routine
  * modify cparse.so for Symbol class of ruby 1.5
.

== 0.11.1 (2000-03-08)
j
  * ruby 1.5 �� Symbol ���б�
  * strscan ��ǿ���
e
  * modify for Symbol
  * update strscan
.

== 0.11.0 (2000-02-19)
j
  * �㳰�ΤȤ������Υե�����ι��ֹ椬�Ф�褦�ˤ���
e
  * if error is occured in action, ruby print line number of grammar file
.

== 0.10.9 (2000-01-19)
j
  * ���åȥ��å���ˡ�ʤɺ٤����ѹ�
e
  * change package/setup
.

== 0.10.8 (2000-01-03)
j
  * ˺��Ƥ��ޤä����ɤ��������󥹥ȡ���ط��ν���
  * (1/17 repacked) �ɥ�����Ȥ��ɲäȽ���
e
  * (1-17 re-packed) add/modify documents
.

== 0.10.7 (2000-01-03)
j
  * setup.rb compile.rb amstd/inst �ʤɤΥХ�����
e
  * modify setup.rb, compile.rb, amstd/inst. (thanks: Koji Arai)
.

== 0.10.6 (1999-12-24)
j
  * racc -e ruby �ǥǥե���ȥѥ������
  * ���Υ��������θƤӤ����Ͼ�ά����褦�ˤ���
e
  * racc -e ruby
  * omit void action call
.

== 0.10.5 (1999-12-21)
j
  * ��ᤳ�ߥ��������μ����������ޤ����Х��äƤ�
  * setup.rb �� inst.rb ���Ѳ����ɽ����Ƥʤ��ä�
  * calc.y calc2.y �� 0.10 �Ѥ˽���
e
  * critical bug in embedded action implement
  * bug in setup.rb
  * modify calc[2].y for 0.10
.

== 0.10.4 (1999-12-19)
j
  * ���顼�����⡼�ɤ����
  * racc -E ��ñ�Τ�ư���ѡ���������
  * Racc �� class ���� module �ˤʤä�
e
  * support error recover ('error' token)
  * can embed runtime by "racc -E"
  * Racc is module
.

== 0.10.3 (1999-12-01)
j
  * ��ᤳ�ߥ��������򥵥ݡ���
  * .output �ν������Ƥ˥Х������ä��Τ���
e
  * support embedded action
  * modify .output bug
.

== 0.10.2 (1999-11-27)
j
  * �ɥ�����Ȥ������ȹ���
  * libracc.rb ��ʬ��
e
  * update document
  * separate libracc.rb
.

== 0.10.1 (1999-11-19)
j
  * C �ǥ�󥿥����񤭤ʤ�����
  * next_token �� false ���֤�����⤦�ɤߤ��ޤʤ�
  * ��������󤬥ȡ�����ˤ�餺��ޤ�Ȥ��� next_token ��ƤФʤ�
  * $end �ѻ�
  * LALRactionTable
e
  * rewrite runtime routine in C
  * once next_token returns [false, *], not call next_token
  * action is only default, not call next_token
  * $end is obsolute
  * LALRactionTable
.

== 0.10.0 (1999-11-06)
j
  * next_* �� next_token �˰��ܲ���peep_token �ѻ�
  * @__debug__ -&lt; @yydebug �ʤ��ѿ�̾�������ѹ�
  * ʸˡ�ե�����ι�¤�� class...rule...end ���Ѥ�ä�
  * �����Υ����ɤ�쿷����®��
  * strscan ��ʻ��
  * �饤�֥��� racc/ �ǥ��쥯�ȥ�˰�ư
e
  * next_value, peep_token is obsolute
  * @__debug__ -&gt; @yydebug
  * class...rule...end
  * refine libracc.rb
  * unify strscan library
  * *.rb are installed in lib/ruby/VERSION/racc/
.

== 0.9.5 (1999-10-03)
j
  * 0.9.4 ���ѹ����������Х��äƤ�
  * $end ���̤�ʤ��ä��Τ���
  * __show_stack__ �ΰ�������äƤ�
e
  * too few arguments for __show_stack__
  * could not scan $end
  * typo in d.format.rb
.

== 0.9.4 (1999-09-??)
j
  * Parser::Reporter ��ʤ����ƥ᥽�åɤ��ᤷ��
  * d.format.rb �������
e
.

== 0.9.3 (1999-09-03)
j
  * racc.rb -> racc
e
.

== 0.9.2 (1999-06-26)
j
  * strscan����
e
.

== 0.9.1 (1999-06-08)
j
  * ����������������ɽ�����б� ( /= �ˤ���դ�)
  * ������������ # �����Ȥ��б�
e
.

== 0.9.0 (1999-06-03)
j
  * ���������� { } �����ˤ���
  * �桼���������ɤ� '----' ��Ȥ������ˤ���
e
.

== 0.8.11 (?)
j
  * -g �ν��Ϥ�狼��䤹������
e
.

== 0.8.10 (?)
j
  * ��������󤫤�return�Ǥ���褦�ˤ���
e
.

== 0.8.9 (1999-03-21)
j
  * -g + @__debug__��Ĥ��ä��ǥХå���å��������
  * ���顼ȯ�����ΥХ�����
  * TOKEN_TO_S_TABLE���ղä���褦�ˤ���
e
.

== 0.8.8 (1999-03-20)
j
  * 100�����٤ι�®��
  * default�ȡ������ä���
  * �ǥХå��ѥ���������Ϥ��륪�ץ����-g�򤯤廊��
  * user_initialize���ѻߤ������̤�initialize��Ȥ���褦�ˤ���
  * parse_initialize/finalize,parse�᥽�åɤ��ѻ�
  * next_token,next_value,peep_token�Υǥե���Ȥ��ѻ�
  * %prec��Ʊ���ε�ǽ��ä���
e
.

== 0.8.7 (1999-03-01)
j
  * ������¤���������Ѳ�
  * �ޥ˥奢�뤬HTML�ˤʤä�
e
.

== 0.8.0 (1999-01-16)
j
  * ʸˡ���֥�å������Ѳ�
e
.

== 0.5.0 (1999-01-07)
j
  * �黻��ͥ���̤��������줿�褦��
  * �������ȵ�§���������줿�褦��
  * �ȡ������ͤ��ִ����������줿�褦��(�����̿Ū�ʥХ�ȯ��)
e
.

== 0.1.0 (1999-01-01)
j
  * �Ȥˤ���ư���褦�ˤʤä�
e
.
