j
<h1>Racc �λȤ���</h1>
<p>
Racc ��ʸˡ��§���� Ruby �ǽ񤫤줿�ѡ�������������ѡ��������ͥ졼���Ǥ���
�ѡ����������르�ꥺ��ˤ� yacc �ʤɤ�Ʊ�� LALR(1) ����Ѥ��Ƥ��ޤ���
</p>
<p>
yacc ���ΤäƤ���ͤϵ���ˡ�ΰ㤤�����狼��лȤ���Ȼפ��ޤ���
yacc ���Τ�ʤ��ͤ�
������Ruby �� 256 �ܻȤ�������� ̵ƻ�ԡ�(������Ϻ����ASCII)
�ʤɤ���ɤ��Ƥ��������Τ��褤���Ȼפ��ޤ���
¾�� UNIX ���ޥ�ɤʤɤȤϰۤʤꡢ
�����ʤ�Ȥ������� Racc �����򤹤�ΤϤ��ʤ꺤��Ǥ���
</p>

<h2>Racc �ȤϤʤˤ�</h2>
<p>
Racc ��ʸˡ���������ġ���Ǥ���
ʸ����Ϥ�����ʸ������ǡ�����ԥ塼���ˤȤäƤϰ�̣������ޤ���
�������ʹ֤Ϥ���ʸ���������ˤʤˤ���̣�򸫽Ф����Ȥ��Ǥ��ޤ���
����ԥ塼���ˤ⤽�Τ褦�ʤ��Ȥ���ʬŪ�ˤǤ⡢������줿�������Ǥ��礦��
Racc �Ϥ��μ������򤷤Ƥ���ޤ��������ʼ�ư���ǤϤ���ޤ��󤬡�
�ʹ֤������������ڤ��˴�ñ�ˤʤ�ޤ���
</p>
<p>
Racc ����ư�����Ƥ������ʬ�Ȥϡ�ʸ����δޤ�ֹ�¤�פν����Ǥ���
���Ȥ��� Ruby �� if ʸ��ͤ��Ƥߤ�ȡ����Τ褦���꼰���Ǥ��ޤ���
</p>
<pre>
if ��Ｐ [then]
  ʸ
  ��
[elsif ��Ｐ [then]
  ʸ
  ��]
[else
  ʸ
  ��]
end
</pre>
<p>
if ʸ�Ǥ� if �Ȥ���ñ�줬�ǽ�ˤʤ��ƤϤʤ餺��
elsif ��� else �������ˤʤ��ƤϤ����ޤ���
���Τ褦�����֤δط� (��¤) ����Racc �����������оݤǤ���
</p>
<p>
������Racc �ǽ����Ǥ��ʤ��ΤϤɤ��������ȤǤ��礦��������ϡ����Ȥ���
if �ξ�Ｐ�ˤ�������ʬ���֤ʤ�Ǥ��뤫�פȤ������ȤǤ����Ĥޤꡢ���
���� if �ξ����Ȥ������ȤǤ�������ϡ����ä��Ǿ��Ȥ��ư��������ɤ�
�񤤤Ƥ��ʤ��Ȥ����ޤ���
</p>
<p>
�ȸ��äƤ⡢�狼��ˤ����Ǥ��礦�������������Ū�ʤ�Τϼºݤˤ����ä�
�ߤ�Τ����֤Ǥ���
</p>

<h2>�ºݤ���</h2>
<p>
�ºݤ� Racc ��ɤΤ褦�˻Ȥ����Ȥ����ä򤷤ޤ���Racc �ˤ��ȼ��Υ�����
�����ɤߤ����ʤ�Τ����äơ�������˽����������ֹ�¤�פ򵭽Ҥ��Ƥ�����
�������Υ������ե�������ʸˡ�ե�����פȸƤ֤��Ȥˤ��ޤ��礦������ʸ
ˡ�ե������̾���� parse.y �Ȳ��ꤹ��ȡ����ޥ�ɥ饤�󤫤�ʲ��Τ褦
���Ǥ�����С����ι�¤��������뤿��Υ��饹��ޤ���ե����뤬������
����
</p>
<pre>
$ racc parse.y
</pre>
<p>
���������ե�����ϥǥե���ȤǤ� "�ե�����̾.tab.rb" �Ǥ���¾��̾��
�ˤ������ʤ顢-o ���ץ������ѹ��Ǥ��ޤ���
</p>
<pre>
$ racc parse.y -o myparser.rb
</pre>
<p>
���Τ褦�ˤ��ƺ�ä����饹���ޤ��Ϥ��Τ褦�ʽ�����ô������ѡ��ȡ�
�Τ��Ȥϥѡ��� (parser) �ȸƤ֤��ȤˤʤäƤ��ޤ������Ϥ����ġ�
�Ȥ������餤��Ŭ���ˤȤ館�Ƥ���������
</p>

<h2>ʸˡ�ե�������</h2>
<p>
Racc ��ʸˡ�ե����뤫�� Ruby �Υ��饹����������ġ�����ȸ����ޤ�����
���Υ��饹������ Racc::Parser �β��̥��饹�ǡ�̾����ʸˡ�ե��������
���ꤷ�ޤ����ʲ��������˽񤯤٤����Ȥ��֤ʤ�ʤΤ��פ��������ޤ���
�����Ǥ����Ƥ˽������֤��Τǡ�ʸˡ�ե����뼫�Τ�ʸˡ�ξܺ٤�
<a href="grammar.html">ʸˡ��ե����</a>�򸫤Ƥ���������
</p>

<h3>ʸˡ</h3>
<p>
�ޤ��ϡ����Τγ����Ǥ���
</p>
<pre>
class MyParser
rule

  if_stmt: IF expr then stmt_list elsif else END

  then   : THEN
         |

  elsif  :
         | ELSIF stmt_list

  else   :
         | ELSE stmt_list

  expr   : NUMBER
         | IDENT
         | STRING

  stmt_list : �դˤ�դˤ�

end
</pre>
<p>
Ruby ������ץȤΤ褦�� class �ǥѡ������饹̾����ꤷ��rule ... end 
�δ֤˥ѡ����˲��Ϥ�������ʸˡ�򵭽Ҥ��ޤ���
</p>
<p>
ʸˡ�ϡ�������¤ӤǤ�ä�ɽ���ޤ���rule ... end �δ֤ˤ��륳���ȥС�
�ʳ��Τ�Ρ�if_stmt IF expr then �ʤɤ����ơֵ���פǤ��������ƥ����
�����ܸ�Ǹ����֡��ϡߡߤ��פΡ֤ϡפߤ����ʤ��ǡ����κ��ε��椬����
��������Ʊ����Τ�ؤ����Ȥ����դ���������ޤ����ޤ����С��ϡ֤ޤ��ϡ�
���̣���ޤ�������ȡ�ñ��˥����κ��ε���Τ��Ȥ��ա������դȤ�
�����ޤ����ʲ��Ϥ�����Τۤ���Ȥä��������ޤ��礦��
</p>
<p>
������դ�ɬ�פ�����Ҥ٤ޤ����ޤ���then �Ρ��С��Τ��Ȥ���� (��§) ��
���Ƥ��������������ˤϲ���񤤤Ƥ��ʤ��Τǡ�����Ϥ����̤��̵�פǤ���
�Ƥ⤤�����Ȥ������Ȥ�ɽ���Ƥ��ޤ����Ĥޤꡢthen �ϵ��� THEN ��Ĥ���
�ޤ��Ϥʤˤ�ʤ�(��ά����)�Ǥ褤���Ȥ������ȤǤ������� then �ϼºݤ� 
Ruby �Υ����������ɤˤ��� then �Ȥ��ڤ�Υ���ƹͤ��ޤ��礦
(����ϼ¤���ʸ���ε��� THEN ��ɽ���Ƥ��ޤ�)��
</p>
<p>
���ơ�������ֵ���פȤ�����Τ��ʤ�ʤΤ��񤭤ޤ��礦��
���������֤��ä򤷤ʤ��Ȥ����ʤ��Τǡ��ޤ���ʹ���Ƥ��Ƥ���������
����ʸ�Ϥκǽ�ˡ��ѡ����Ȥ�ʸ�����󤫤鹽¤�򸫽Ф���ʬ���ȸ����ޤ�����
������ʸ�����󤫤餤���ʤ깽¤��õ���Τ����ݤʤΤǡ��ºݤˤϤޤ�
ʸ�������ñ������ʬ�䤷�ޤ������λ����ǥ��ڡ����䥳���ȤϼΤƤ�
���ޤ����ʹߤϽ��˥ץ����ΰ�����ʤ���ʬ���������ˤ��ޤ���
���Ȥ���ʸ��������Ϥ����Τ褦���ä��Ȥ���ȡ�
</p>
<pre>
if flag then   # item found.
  puts 'ok'
end
</pre>
<p>
ñ�����ϼ��Τ褦�ˤʤ�ޤ���
</p>
<pre>
if flag then puts 'ok' end
</pre>
<p>
�����ǡ����פ�ɬ�פǤ����ɤ���� flag �ϥ������ѿ�̾���Ȼפ��ޤ�����
�ѿ�̾�Ȥ����Τ�¾�ˤ⤤������ޤ���������̾���� i ������ a ����
���� vvvvvvvvvvvv ���������ֹ�¤�פ�Ʊ���Ǥ����Ĥޤ�Ʊ�������򤵤��
�٤��Ǥ����ѿ� a ��񤱤���ʤ� b ��񤱤ʤ��ƤϤ����ޤ��󡣤��ä���
���Ū��Ʊ��̾�����ɤ�Ǥ⤤������󡣤Ȥ������Ȥǡ�����ñ������ʲ�
�Τ褦���ɤߤ����ޤ��礦��
</p>
<pre>
IF IDENT THEN IDENT STRING END
</pre>
<p>
���줬�ֵ���פ���Ǥ����ѡ����ǤϤ��ε�����Τۤ��򰷤�����¤���դ�
�Ƥ����ޤ���
</p>
<p>
����˵���ˤĤ��Ƹ��Ƥ����ޤ��礦��
�����������ʬ�����ޤ����ֺ��դˤ��뵭��פȡ֤ʤ�����פǤ���
���դˤ��뵭��ϡ���ü�׵���ȸ����ޤ����ʤ��ۤ��ϡֽ�ü�׵����
�����ޤ����ǽ����ǤϽ�ü����Ϥ��٤���ʸ������ü����Ͼ�ʸ����
�񤤤Ƥ���Τǡ��⤦������ä����ʸˡ�򸫤Ƥ���������
</p>
<p>
�ʤ����ζ�ʬ�����פ��ȸ����ȡ����Ϥε�����Ϥ��٤ƽ�ü���������Ǥ���
��������ü����ϥѡ�������Ǥ�������ü������󤫤�ֺ������פ��Ȥ�
��äƻϤ��¸�ߤ��ޤ����㤨�м��ε�§��⤦���ٸ��Ƥ���������
</p>
<pre>
  expr   : NUMBER
         | IDENT
         | STRING
</pre>
<p>
expr �� NUMBER �� IDENT �� STRING ���ȸ��äƤ��ޤ����դ˸����ȡ�
IDENT �� expr �ˡ֤ʤ뤳�Ȥ��Ǥ��ޤ��ס�ʸˡ�� expr ��¸�ߤǤ���
���� IDENT �����ȡ������ expr �ˤʤ�ޤ����㤨�� if �ξ�Ｐ��
��ʬ�� expr �Ǥ����顢������ IDENT ������� expr �ˤʤ�ޤ�������
�褦��ʸˡŪ�ˡ��礭���׵�����äƤ��äơ��ǽ�Ū�˰�Ĥˤʤ�ȡ�
�������Ϥ�ʸˡ���������Ƥ��뤳�Ȥˤʤ�ޤ����ºݤˤ��ä������Ϥ�
��Ƥߤޤ��礦�����ϤϤ����Ǥ�����
</p>
<pre>
IF IDENT THEN IDENT STRING END
</pre>
<p>
�ޤ���IDENT �� expr �ˤʤ�ޤ���
</p>
<pre>
IF expr THEN IDENT STRING END
</pre>
<p>
���� THEN �� then �ˤʤ�ޤ���
</p>
<pre>
IF expr then IDENT STRING END
</pre>
<p>
IDENT STRING ���᥽�åɥ�����ˤʤ�ޤ�����������Ϥ����ۤɤ���ˤ�
�ʤ��Ǥ������¤Ͼ�ά����Ƥ������ȹͤ��Ƥ��������������Ƥ������
������Фơ��ǽ�Ū�ˤ� stmt_list (ʸ�Υꥹ��)�ˤʤ�ޤ���
</p>
<pre>
IF expr then stmt_list END
</pre>
<p>
elsif �� else �Ͼ�ά�Ǥ��롢�Ĥޤ�̵���������Ǥ��ޤ���
</p>
<pre>
IF expr then stmt_list elsif else END
</pre>
<p>
�Ǹ�� if_stmt ����ޤ���
</p>
<pre>
if_stmt
</pre>
<p>
�Ȥ������ȤǤҤȤĤˤʤ�ޤ�����
�Ĥޤꤳ�����Ϥ�ʸˡŪ���������Ȥ������Ȥ��狼��ޤ�����
</p>

<h3>���������</h3>
<p>
�����ޤǤ����Ϥ�ʸˡ�����������ɤ������ǧ������ˡ�Ϥ狼��ޤ�������
��������ǤϤʤ�ˤ�ʤ�ޤ��󡣺ǽ�����������褦�ˡ������ޤǤǤ�
��¤�������������ǡ��ץ����ϡְ�̣�פ�����Ǥ��ޤ��󡣤����Ƥ���
��ʬ�� Racc �Ǥϼ�ư�����Ǥ��ʤ��Τǡ��ʹ֤��񤯡��Ȥ�����ޤ�����
�����񤯤Τ��ʲ�����������֥��������פȤ�����ʬ�Ǥ���
</p>
<p>
����ǡ�������󤬤��������礭��ñ�̤ˤޤȤ���Ƥ��������򸫤ޤ�����
���ΤޤȤ����ˡ�Ʊ���ˤʤˤ����餻�뤳�Ȥ��Ǥ��ޤ������줬
���������Ǥ������������ϡ�ʸˡ�ե�����ǰʲ��Τ褦�˽񤭤ޤ���
</p>
<pre>
class MyParser
rule

  if_stmt: IF expr then stmt_list elsif else END
             { puts 'if_stmt found' }

  then   : THEN
             { puts 'then found' }
         |
             { puts 'then is omitted' }

  elsif  :
             { puts 'elsif is omitted' }
         | ELSIF stmt_list
             { puts 'elsif found' }

  else   :
             { puts 'else omitted' }
         | ELSE stmt_list
             { puts 'else found' }

  expr   : NUMBER
             { puts 'expr found (NUMBER)' }
         | IDENT
             { puts 'expr found (IDENT)' }
         | STRING
             { puts 'expr found (STRING)' }

  stmt_list : �դˤ�դˤ�

end
</pre>
<p>
���ƤΤȤ��ꡢ��§�Τ��Ȥ� { �� } �ǰϤ�ǽ񤭤ޤ���
���������ˤϤ������������ʤ褦�� Ruby ������ץȤ��񤱤ޤ���
</p>
<p>
(�����ᡢ̤��)
</p>
<hr>

<p>
yacc �Ǥ� <code>$$</code> �� Racc �Ǥϥ������ѿ� <code>result</code>
�ǡ�<code>$1,$2...</code> ������ <var>val</var>�Ǥ���
<code>result</code> �� <code>val[0]</code> ($1) ���ͤ˽�������졢
����������ȴ�����Ȥ��� <code>result</code> ���ͤ������ͤˤʤ�ޤ���
Racc �Ǥϥ����������� <code>return</code> �ϥ�������󤫤�ȴ��������ǡ�
�ѡ������ΤϽ����ޤ��󡣥���������椫��ѡ�����λ����ˤϡ�
�᥽�å� <code>yyaccept</code> ��ȤäƤ���������
</p>
<p>
�黻�Ҥ�ͥ���̡��������ȥ롼��ʤɤ� yacc �ΰ���Ū�ʵ�ǽ���Ѱդ����
���ޤ���������������⾯��ʸˡ���㤤�ޤ���
</p>
<p>
yacc �Ǥ��������줿�����ɤ�ľ��ž�̤���륳���ɤ�����ޤ�����
Racc �Ǥ�Ʊ���褦�ˡ��桼������Υ����ɤ��񤱤ޤ���
Racc �Ǥϥ��饹����������Τǡ����饹�������/��/��λ��Ľ꤬����ޤ���
Racc �ǤϤ����夫����֤� header inner footer �ȸƤ�Ǥ��ޤ���
</p>

<h3>�桼�����Ѱդ��٤�������</h3>
<p>
�ѡ����Υ���ȥ�ݥ���ȤȤʤ�᥽�åɤ���Ĥ���ޤ����ҤȤĤ� 
<code>do_parse</code>�ǡ�������ϥȡ������ 
<code>Parser#next_token</code> �������ޤ����⤦�ҤȤĤ� 
<code>yyparse</code> �ǡ�������ϥ�����ʤ��� <code>yield</code> ����
�뤳�Ȥˤ�äƥȡ���������ޤ����桼��¦�ǤϤ��Τɤ��餫(ξ���Ǥ⤤��
����)��ư�����ñ�ʥ᥽�åɤ� inner �˽񤤤Ƥ��������������᥽�å�
�ΰ����ʤɡ��ܤ������Ȥϥ�ե���󥹤򸫤Ƥ���������
</p>
<ul>
<li><a href="parser.html#Racc%3a%3aParser-do_parse">do_parse</a>
<li><a href="parser.html#Racc%3a%3aParser-yyparse">yyparse</a>
</ul>
<p>
�ɤ���Υ᥽�åɤˤⶦ�̤ʤΤϥȡ�����η����Ǥ���ɬ���ȡ����󥷥�ܥ�
�Ȥ����ͤ������Ǥ����������֤��褦�ˤ��ޤ����ޤ�������󤬽�λ���ơ�
�⤦�����Τ��ʤ����� <code>[false,<var>�ʤˤ�</var>]</code> ���֤�
�Ƥ�������������ϰ���֤��н�ʬ�Ǥ� (�դˡ�<code>yyparse</code> ���
���������ʾ� <code>yield</code> ���ƤϤ����ʤ�)��
</p>
<p>
�ѡ������̤�ʸ��������ˤ����Ȥ����ΤǤϤ���ޤ��󤬡��º�����Ȥ�
�ơ��ѡ���������̤ǤϤ����Ƥ�ʸ����Υ�����ʤȥ��åȤǻȤ����Ȥ�¿
���Ǥ��礦��Ruby �ʤ饹����ʤ��餤�ھ��Ǻ��ޤ�������®�ʥ�����ʤ�
�ʤ�ȼ¤��񤷤��ä��ꤷ�ޤ��������ǹ�®�ʥ�����ʤ�������뤿��Υ饤
�֥����äƤ��ޤ����ܤ�����
<a href="#WritingScanner">�֥�����ʤ���פι�</a>�򸫤Ƥ���������
</p>
<p>
Racc �ˤ� error �ȡ������Ȥä����顼������ǽ�⤢��ޤ���yacc ��
<code>yyerror()</code> �� Racc �Ǥ�
<a href="parser.html#Racc%3a%3aParser-on_error"><code>Racc::Parser#on_error</code></a>
�ǡ����顼���������ȡ�����Ȥ����͡��ͥ����å����λ��Ĥΰ�����Ȥ�ޤ���
<code>on_error</code> �Υǥե���Ȥμ������㳰
<code>Racc::ParseError</code> ��ȯ�����ޤ���
</p>
<p>
�桼���������������ǥѡ������顼��ȯ���������ϡ��᥽�å�
<a href="parser.html#Racc%3a%3aParser-yyerror"><code>yyerror</code></a>
��Ƥ٤Хѡ��������顼�����⡼�ɤ�����ޤ���
���������ΤȤ� <code>on_error</code>�ϸƤФ�ޤ���
</p>

<h3>�ѡ�������������</h3>
<p>
�����������Ф��������񤱤�Ȼפ��ޤ������Ȥϡ��ǽ�˼�������ˡ��ʸˡ
�ե�������������Ruby ������ץȤ����ޤ���
</p>
<p>
���ޤ������Ф����ΤǤ������礭����Τ��Ⱥǽ餫��Ϥ��ޤ������ʤ��Ǥ���
����racc �� -g ���ץ�����Ĥ��ƥ���ѥ��뤷��@yydebug �� true �ˤ���
�ȥǥХå��Ѥν��Ϥ������ޤ����ǥХå����Ϥϥѡ����� @racc_debug_out 
�˽��Ϥ���ޤ�(�ǥե���Ȥ� stderr)���ޤ���racc �� -v ���ץ�����Ĥ�
��ȡ���������ɽ���ɤߤ䤹�����ǽ��Ϥ����ե�����(*.output)�������ޤ���
�ɤ����ǥХå��λ��ͤˤʤ�Ǥ��礦��
</p>


<h2>��ä��ѡ��������ۤ���</h2>
<p>
Racc �����������ѡ�����ư����˥�󥿥���롼����ɬ�פǤ���
����Ū�ˤ� parser.rb �� cparse.so �Ǥ���
������ cparse.so ��ñ�˥ѡ������®�����뤿��Υ饤�֥��ʤΤ�
ɬ�ܤǤϤ���ޤ��󡣤ʤ��Ƥ�ư���ޤ���
</p>
<p>
�ޤ� Ruby 1.8.0 �ʹߤˤϤ��Υ�󥿥��बɸ��ź�դ���Ƥ���Τǡ�
Ruby 1.8 ������Ķ��ʤ�Х�󥿥���ˤĤ��ƹ�θ����ɬ�פϤ���ޤ���
Racc 1.4.x �Υ�󥿥���� Ruby 1.8 ��ź�դ���Ƥ����󥿥����
�����ߴ��Ǥ���
</p>
<p>
����� Ruby 1.8 ����Ǥ��ʤ����Ǥ��� 
Racc ��桼���ߤ�ʤ˥��󥹥ȡ��뤷�Ƥ�餦�Τ��Ĥμ�Ǥ�����
����Ǥ��Կ��ڤǤ���������Racc �Ǥϲ�������Ѱդ��ޤ�����
</p>
<p>
racc �� -E ���ץ�����Ĥ��ƥ���ѥ��뤹��ȡ�
�ѡ����� racc/parser.rb ����Τ����ե��������ϤǤ��ޤ���
����ʤ�Хե�����ϰ�Ĥ����ʤΤǴ�ñ�˰����ޤ���
racc/parser.rb �ϵ���Ū�� require �����褦�ʰ����ˤʤ�Τǡ�
���η����Υѡ�����ʣ�����ä��Ȥ��Ƥ⥯�饹��᥽�åɤ����ͤ��뤳�Ȥ⤢��ޤ���
������ -E ��Ȥä����� cparse.so ���Ȥ��ޤ���Τǡ�
ɬ��Ū�˥ѡ�����®�٤�����ޤ���
</p>


<h2><a name="WritingScanner">���ޤ��� ������ʤ��</a></h2>
<p>
�ѡ�����Ȥ��Ȥ��ϡ������Ƥ�ʸ�����ȡ�������ڤ�櫓�Ƥ���륹�����
��ɬ�פˤʤ�ޤ����������¤� Ruby ��ʸ����κǽ餫��ȡ�������ڤ�櫓
�Ƥ����Ȥ�����Ȥ����ޤ����դǤϤ���ޤ���
���Τ˸����ȡ���ñ�ˤǤ���ΤǤ���������ʤ�Υ����С��إåɤ�������ޤ���
</p>
<p>
���Υ����С��إåɤ���򤷤Ĥġ�
��ڤ˥�����ʤ����褦�� strscan �Ȥ����ѥå���������ޤ�����
Ruby 1.8 �ʹߤˤ�ɸ��ź�դ���Ƥ��ޤ�����
<a href="http://i.loveruby.net/ja/">ɮ�ԤΥۡ���ڡ���</a>�ˤ�
ñ�Υѥå�����������ޤ���
</p>
e
<h1>Usage</h1>

<h2>Generating Parser Using Racc</h2>
<p>
To compile Racc grammar file, simply type:
</p>
<pre>
$ racc parse.y
</pre>
<p>
This creates ruby script file "parse.tab.y". -o option changes this.
</p>

<h2>Writing Racc Grammer File</h2>
<p>
If you want your own parser, you have to write grammar file.
A grammar file contains name of parser class, grammar the parser can parse,
user code, and any.<br>
When writing grammar file, yacc's knowledge is helpful.
If you have not use yacc, also racc is too difficult.
</p>
<p>
Here's example of Racc grammar file.
</p>
<pre>
class Calcparser
rule
  target: exp { print val[0] }

  exp: exp '+' exp
     | exp '*' exp
     | '(' exp ')'
     | NUMBER
end
</pre>
<p>
Racc grammar file is resembles to yacc file.
But (of cource), action is Ruby code. yacc's $$ is 'result', $0, $1... is
an array 'val', $-1, $-2... is an array '_values'.
</p>
<p>
Then you must prepare parse entry method. There's two types of
racc's parse method, 
<a href="parser.html#Racc%3a%3aParser-do_parse"><code>do_parse</code></a> and
<a href="parser.html#Racc%3a%3aParser-yyparse"><code>yyparse</code></a>.
</p>
<p>
"do_parse()" is simple. it is yyparse() of yacc, and "next_token()" is
yylex(). This method must returns an array like [TOKENSYMBOL, ITS_VALUE].
EOF is [false, false].
(token symbol is ruby symbol (got by String#intern) as default.
 If you want to change this, see <a href="grammar.html#token">grammar reference</a>.
</p>
<p>
"yyparse()" is little complecated, but useful. It does not use "next_token()",
it gets tokens from any iterator. For example, "yyparse(obj, :scan)" causes
calling obj#scan, and you can return tokens by yielding them from obj#scan.
</p>
<p>
When debugging, "-v" or/and "-g" option is helpful.
"-v" causes creating verbose log file (.output).
"-g" causes creating "Verbose Parser".
Verbose Parser prints internal status when parsing.
But it is <em>not</em> automatic.
You must use -g option and set @yydebug true to get output.
-g option only creates verbose parser.
</p>

<h3>re-distributing Racc runtime</h3>
<p>
A parser, which is created by Racc, requires Racc runtime module;
racc/parser.rb.
</p>
<p>
Ruby 1.8.x comes with racc runtime module,
you need NOT distribute racc runtime files.
</p>
<p>
If you want to run your parsers on ruby 1.6,
you need re-distribute racc runtime module with your parser.
It can be done by using '-E' option:
<pre>
$ racc -E -omyparser.rb myparser.y
</pre>
<p>
This command creates myparser.rb which `includes' racc runtime.
Only you must do is to distribute your parser file (myparser.rb).
</p>
<p>
Note: parser.rb is LGPL, but your parser is not.
Your own parser is completely yours.
</p>
.
