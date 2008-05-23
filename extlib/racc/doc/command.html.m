j
<h1>Racc���ޥ�ɥ�ե����</h1>
e
<h1>Racc Command Reference</h1>
.
<p>
racc [-o<var>filename</var>] [--output-file=<var>filename</var>]
     [-e<var>rubypath</var>] [--embedded=<var>rubypath</var>]
     [-v] [--verbose]
     [-O<var>filename</var>] [--log-file=<var>filename</var>]
     [-g] [--debug]
     [-E] [--embedded]
     [-l] [--no-line-convert]
     [-c] [--line-convert-all]
     [-a] [--no-omit-actions]
     [-C] [--check-only]
     [-S] [--output-status]
     [--version] [--copyright] [--help] <var>grammarfile</var>
</p>

<dl>
<dt><var>filename</var>
<dd>
j
Racc��ʸˡ�ե��������ꤷ�ޤ�����ĥ�Ҥˤ��ä����¤Ϥ���ޤ���
e
Racc grammar file. Any extention is permitted.
.
</dd>
<dt>-o<var>outfile</var>, --output-file=<var>outfile</var>
<dd>
j
�������륯�饹�򤫤�����ե�����̾����ꤷ�ޤ����ǥե���Ȥ�<filename>.tab.rb�Ǥ���
e
A filename for output. default is &lt;filename&gt;.tab.rb
.
</dd>
<dt>-O<var>filename</var>, --log-file=<var>filename</var>
<dd>
j
-v ���ץ�����Ĥ�����������������ե������̾����
<var>filename</var> ���ѹ����ޤ���
�ǥե���Ȥ� <var>filename</var>.output �Ǥ���
e
Place logging output in file <var>filename</var>.
Default log file name is <var>filename</var>.output.
.
</dd>
<dt>-e<var>rubypath</var>, --executable=<var>rubypath</var>
<dd>
j
�¹Բ�ǽ�ե�������������ޤ���<var>rubypath</var>�� Ruby ���ΤΥѥ��Ǥ���
<var>rubypath</var>��ñ�� 'ruby' �ˤ������ˤ� Racc ��ư��Ƥ���
Ruby �Υѥ�����Ѥ��ޤ���
e
output executable file(mode 755). <var>path</var> is a path of ruby interpreter.
.
</dd>
<dt>-v, --verbose
<dd>
j
�ե����� "filename".output �˾ܺ٤ʲ��Ͼ������Ϥ��ޤ���
e
verbose mode. create &lt;filename&gt;.output file, like yacc's y.output file.
.
</dd>
<dt>-g, --debug
<dd>
j
���Ϥ��륳���ɤ˥ǥХå��ѥ����ɤ�ä��ޤ���-g ��Ĥ������������ѡ�����
@yydebug �� true �˥��åȤ���ȡ��ǥХå��ѤΥ����ɤ����Ϥ���ޤ���<br>
-g ��Ĥ�������Ǥϲ��⤪����ޤ���Τ���դ��Ƥ���������
e
add debug code to parser class. To display debuggin information,
use this '-g' option and set @yydebug true in parser class.
.
</dd>
<dt>-E, --embedded
<dd>
j
��󥿥���롼����򤹤٤ƴޤ�������ɤ��������ޤ���
�Ĥޤꡢ���Υ��ץ�����Ĥ����������������ɤ� Ruby ���������ư���ޤ���
e
Output parser which doesn't need runtime files (racc/parser.rb).
.
</dd>
<dt>-C, --check-only
<dd>
j
(ʸˡ�ե������) ʸˡ�Υ����å������򤷤ƽ�λ���ޤ���
e
Check syntax of racc grammer file and quit.
.
</dd>
<dt>-S, --output-status
<dd>
j
�ʹԾ����������𤷤ޤ���
e
Print messages time to time while compiling.
.
</dd>
<dt>-l, --no-line-convert
<dd>
j
<p>
Ruby �Ǥ��㳰��ȯ���������Υե�����̾����ֹ��ɽ�����Ƥ���ޤ�����
Racc �����������ѡ����ϡ��ǥե���ȤǤϤ��ξ��Υե�����̾�����ֹ��
ʸˡ�ե�����ǤΤ�Τ��֤������ޤ������Υե饰�Ϥ��ε�ǽ�򥪥դˤ��ޤ���
</p>
<p>
ruby 1.4.3 �����ΥС������ǤϥХ��Τ��������λ��Ȥ˼��Ԥ���
��礬����Τǡ�������Ȥ˴ؤ��Ƥʤˤ������������Ȥ������ä��餳�Υե饰��
��ƤߤƤ���������
</p>
e
turns off line number converting.
.
</dd>
<dt>-c, --line-convert-all
<dd>
j
���������� inner �˲ä� header footer �ι��ֹ���Ѵ����ޤ���
header �� footer ���Ĥʤ��äƤ���褦�ʾ��ˤϻȤ�ʤ��Ǥ���������
e
Convert line number of actions, inner, header and footer.
.
<dt>-a, --no-omit-actions
<dd>
j
���ƤΥ����������б�����᥽�å�����ȸƤӽФ���Ԥ��ޤ���
�㤨��������󤬾�ά����Ƥ��Ƥ���Υ᥽�åɤ��������ޤ���
e
Call all actions, even if an action is empty.
.
</dd>
<dt>--version
<dd>
j
Racc �ΥС���������Ϥ��ƽ�λ���ޤ���
e
print Racc version and quit.
.
</dd>
<dt>--copyright
<dd>
j
���ɽ������Ϥ��ƽ�λ���ޤ���
e
Print copyright and quit.
.
<dt>--help
<dd>
j
���ץ����δ�ñ����������Ϥ��ƽ�λ���ޤ���
e
Print usage and quit.
.
</dd>
</dl>
