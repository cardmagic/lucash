j
= ��§�ե�����ʸˡ��ե����

== ʸˡ�˴ؤ������С������Ȥ���ߴ�

  * (1.2.5) �桼���������ɤ�Ϣ�뤹����������ե��������
            ��ᤳ��Ǥ��륳���ɤ����Ϣ�뤷�ޤ���
  * (1.1.6) �������ǥ��쥯�ƥ��� options ���ɲä���ޤ�����
  * (1.1.5) ͽ��� token �ΰ�̣���ѹ��ˤʤ�ޤ�����
  * (0.14) �롼��κǸ�Υ��ߥ���󤬾�ά��ǽ�ˤʤ�ޤ�����
           �ޤ���token prechigh �ʤɤ�ͽ���Ǥʤ��ʤ�ޤ�����
  * (10.2) prepare �� header �� driver �� footer �ˤʤ�ޤ�����
           ���Ϥ��ΤޤޤǤ�Ȥ��ޤ�����2.0 ������б����ޤ���
  * (0.10) class ���б����� end ���ʤ��ʤ�ޤ�����
  * (0.9) ���������Υԥꥪ����������� { �� } �ǰϤ�褦�ˤ��ޤ�����

== ���Τι�¤

�ȥåץ�٥�ϡ���§���ȥ桼��������������ʬ�����ޤ���
�桼�������������ϥ��饹����θ����ʤ���Ф����ޤ���

=== ������

ʸˡ�ե�����ˤϡ������㳰������ơ��ۤȤ�ɤɤ��ˤǤ⥳���Ȥ�
�񤯤��Ȥ��Ǥ��ޤ��������Ȥϡ�Ruby�� #.....(����) ��������ȡ�
C�� /*......*/ ���������Ȥ����Ȥ��Ǥ��ޤ���

=== ��§��

��§���ϰʲ��Τ褦�ʷ��򤷤Ƥ��ޤ���
--
class ���饹̾ [< �����ѡ����饹]
  [�黻�ҽ��]
  [�ȡ��������]
  [���ץ����]
  [expect]
  [�ȡ����󥷥�ܥ��ͤ�������]
  [�������ȵ�§]
rule
  ʸˡ����
--
"���饹̾"�Ϥ������������ѡ������饹��̾���Ǥ���
����Ϥ��Τޤ�Ruby�Υ��饹̾�ˤʤ�ޤ���

�ޤ� M::C �Τ褦�ˡ�::�פ�Ȥä�̾����Ȥ��ȡ����饹�����
�⥸�塼�� M ����˥ͥ��Ȥ����ޤ����Ĥޤ� class M::C �ʤ��
--
module M
  class C < Racc::Parser
    ������
  end
end
--
�Τ褦�˽��Ϥ��ޤ���

����ˡ�Ruby ��Ʊ����ʸ�ǥ����ѡ����饹�����Ǥ��ޤ���
���������λ���򤹤�ȥѡ�����ư��˽���ʱƶ���Ϳ����Τǡ�
�ä�ɬ�פ��ʤ��¤���ꤷ�ƤϤ����ޤ��󡣤���Ͼ���γ�ĥ��
������Ѱդ�����Τǡ����߻��ꤹ��ɬ�����Ϥ��ޤꤢ��ޤ���

=== ʸˡ�ε���

racc ����������ѡ���������Ǥ���ʸˡ�򵭽Ҥ��ޤ���
ʸˡ�ϡ�ͽ��� rule �� end �δ֤ˡ��ʲ��Τ褦�ʽ񼰤ǽ񤭤ޤ���
--
�ȡ�����: �ȡ�������¤� ���������

�ȡ�����: �ȡ�������¤� ���������
        | �ȡ�������¤� ���������
        | �ȡ�������¤� ���������
             (ɬ�פʤ���Ʊ���褦�ˤĤŤ���)
--
���������� { } �ǰϤߤޤ������������Ǥ� Ruby ��ʸ�ϤۤȤ��
�Ȥ��ޤ������������������б��Ǥ����б����Ƥ��ʤ���Τϰʲ��ΤȤ��ꡣ

  * �ҥ��ɥ������
  * =begin ... =end ��������
  * ���ڡ����ǻϤޤ�����ɽ��
  * �����ޤ�� % �α黻�����̤˱黻�ҤΤޤ��˥��ڡ���������Ƥ��������ʤ�

���Τ�����˴ؤ��Ƥϴ������б��Ϥޤ�̵���Ǥ����������Ƥ���������

���դ���($$)�ϡ����ץ����ˤ�ä��֤����������ޤ����ޤ��ǥե���ȤǤ�
�������ѿ� result (���Υǥե�����ͤ� val[0])�� �����ͤ�ɽ�������������
�֥�å���ȴ�������� result ���ͤ������ͤˤʤ�ޤ����ޤ�������Ū�� return
���֤������⤳���ͤˤʤ�ޤ���������options �� no_result_var ����ꤷ��
��硢�����ͤϥ��������֥�å��κǸ��ʸ���ͤˤʤ�ޤ� (Ruby �Υ᥽�åɤ�
Ʊ��)��

�ɤ���ξ��Ǥ⥢�������Ͼ�ά�Ǥ�����ά�������κ����ͤϾ�� val[0] �Ǥ���

�ʲ���ʸˡ���Ҥ����Τ���򤷤ᤷ�ޤ���
--
rule
  goal: def ruls source
        {
          result = val
        }

  def : /* none */
        {
          result = []
        }
      | def startdesig
        {
          result[0] = val[1]
        }
      | def
          precrule   # ����Ͼ�ιԤ�³��
        {
          result[1] = val[1]
        }
(ά)
--
�����������Ǥ����̤ʰ�̣���ä��ѿ��������Ĥ��Ȥ��ޤ���
���Τ褦���ѿ���ʲ��˼����ޤ�����̤���� yacc �Ǥ�ɽ���Ǥ���

  * result ($$)

���դ��͡�����ͤ� val[0] �Ǥ���

  * val ($1,$2,$3��)

���դε�����ͤ�����Ruby ������ʤΤ���������ǥå����ϥ�����Ϥޤ�ޤ���
����������������ΤǼ�ͳ���ѹ�������ΤƤ��ꤷ�ƹ����ޤ���

  * _values (...,$-2,$-1,$0)

�ͥ����å���Racc �������ȤäƤ��륪�֥������Ȥ����Τޤ��Ϥ���ޤ���
�����ѿ��ΰ�̣���狼��Ͱʳ���<em>���Ф�</em>�ѹ����ƤϤ����ޤ���

�ޤ��������������̤ʷ����ˡ���ᤳ�ߥ��������Ȥ�����Τ�����ޤ���
����ϥȡ������������ι����ʤȤ���˵��Ҥ��뤳�Ȥ��Ǥ��ޤ���
�ʲ�����ᤳ�ߥ�����������򼨤��ޤ���
--
target: A B { puts 'test test' } C D { normal action }
--
���Τ褦�˵��Ҥ���� A B �򸡽Ф��������� puts ���¹Ԥ���ޤ���
�ޤ�����ᤳ�ߥ��������Ϥ��켫�Τ��ͤ�����ޤ����Ĥޤꡢ�ʲ�����ˤ�����
--
target: A { result = 1 } B { p val[1] }
--
�Ǹ�ˤ��� p val[1] ����ᤳ�ߥ����������� 1 ��ɽ�����ޤ���
B ���ͤǤϤ���ޤ���

��̣Ū�ˤϡ���ᤳ�ߥ��������϶��ε�§�������ü������ɲä��뤳�Ȥ�
����Ʊ��Ư���򤷤ޤ����Ĥޤꡢ�����ϼ��Υ����ɤȴ�����Ʊ����̣�Ǥ���
--
target  : A nonterm B { p val[1] }
nonterm : /* ���ε�§ */ { result = 1 }
--

=== �黻��ͥ����

����ȡ������ǥ��եȡ��Ը����ͤ������ä��Ȥ������Υȡ������
�黻��ͥ���̤����ꤷ�Ƥ���Ⱦ��ͤ��äǤ����礬����ޤ���
���Τ褦�ʤ�ΤȤ����ä�ͭ̾�ʤΤϿ����α黻�Ҥ� if...else ��ʸ�Ǥ���

ͥ���̤ǲ��Ǥ���ʸˡ�ϡ����ޤ�ʸˡ�򤯤ߤ����Ƥ���
ͥ���̤ʤ��Ǥ�Ʊ�����̤����뤳�Ȥ��Ǥ��ޤ��������������Ƥ���
����ͥ���̤����ꤷ�Ʋ�褹��ۤ���ʸˡ���ñ�ˤǤ��ޤ���

���եȡ��Ը����ͤ������ä��Ȥ���Racc �Ϥޤ����ε�§�˽�̤�����
����Ƥ��뤫Ĵ�٤ޤ�����§�ν�̤ϡ����ε�§�ǰ��֤�����ˤ���
��ü�ȡ������ͥ���̤Ǥ������Ȥ���
--
target: TERM_A nonterm_a TERM_B nonterm_b
--
�Τ褦�ʵ�§�ν�̤�TERM_B��ͥ���̤ˤʤ�ޤ����⤷TERM_B��
ͥ���̤����ꤵ��Ƥ��ʤ��ä��顢ͥ���̤Ǿ��ͤ��褹�뤳�Ȥ�
�Ǥ��ʤ���Ƚ�Ǥ�����Shift/Reduce conflict�פ���𤷤ޤ���

�黻�Ҥ�ͥ���̤ϤĤ��Τ褦�˽񤤤�������ޤ���
--
prechigh
  nonassoc PLUSPLUS
  left     MULTI DEVIDE
  left     PLUS MINUS
  right    '='
preclow
--
prechigh �˶ᤤ�Ԥˤ���ۤ�ͥ���̤ι⤤�ȡ�����Ǥ����岼��ޤ뤴��
�������ޤˤ��� preclow...prechigh �ν��֤˽񤯤��Ȥ�Ǥ��ޤ���left
�ʤɤ�ɬ���Ԥκǽ�ˤʤ���Ф����ޤ���

left right nonassoc �Ϥ��줾��ַ�����פ�ɽ���ޤ���������ˤ�äơ�
Ʊ����̤α黻�Ҥε�§�����ͤ������˥��եȴԸ��Τɤ����Ȥ뤫��
��ޤ�ޤ������Ȥ���
--
a - b - c
--
��
--
(a - b) - c
--
�ˤʤ�Τ������ (left) �Ǥ�����§�黻�����̤���Ǥ���
����
--
a - (b - c)
--
�ˤʤ�Τ������ (right) �Ǥ��������Υ������Ȥ����� right �Ǥ���
�ޤ����Τ褦�˱黻�Ҥ��Ťʤ�Τϥ��顼�Ǥ����硢���� (nonassoc) �Ǥ���
C ����� ++ ��ñ��Υޥ��ʥ��ʤɤ�����ˤ�����ޤ���

�Ȥ���ǡ����������Ȥ����̾�ϴԸ����뵬§�κǸ�Υȡ����󤬽�̤�
����ΤǤ��������뵬§�˸¤äƤ��Υȡ�����Ȥϰ㤦��̤ˤ��������Ȥ�
����ޤ����㤨�����ȿž�Υޥ��ʥ��ϰ������Υޥ��ʥ�����̤�⤯
���ʤ��Ȥ����ޤ��󡣤��Τ褦�ʾ�� yacc �Ǥ� %prec ��Ȥ��ޤ���
racc �Ǥϥ������뵭���Ȥä�Ʊ�����Ȥ�Ǥ��ޤ���
--
prechigh
  nonassoc UMINUS
  left '*' '/'
  left '+' '-'
preclow
(ά)
exp: exp '*' exp
   | exp '-' exp
   | '-' exp     = UMINUS    # ����������̤�夲��
--
���Τ褦�˵��Ҥ���ȡ�'-' exp �ε�§�ν�̤� UMINUS �ν�̤ˤʤ�ޤ���
�������뤳�Ȥ����ȿž�� '-' �� '*' �����̤��⤯�ʤ�Τǡ�
�տޤɤ���ˤʤ�ޤ���

=== �ȡ��������

�ȡ�����(��ü����)�ΤĤŤ��ְ㤨��Ȥ����ΤϤ褯���뤳�ȤǤ�����
ȯ������ΤϤʤ��ʤ��񤷤���ΤǤ���1.1.5 ����ϥȡ����������Ū��
������뤳�Ȥǡ�����ˤʤ��ȡ����� / ����ˤ�������ȡ�������Ф���
�ٹ𤬽Ф�褦�ˤʤ�ޤ�����yacc �� %token �Ȼ��Ƥ��ޤ�������ΰ㤤��
racc �Ǥ�ɬ�ܤǤϤʤ��������⥨�顼�ˤʤ餺�ٹ�������Ȥ������Ǥ���

�ȡ���������ϰʲ��Τ褦�˽񤭤ޤ���
--
token A B C D
        E F G H
--
�ȡ�����Υꥹ�Ȥ�ʣ���Ԥˤ錄�äƽ񤱤뤳�Ȥ����ܤ��Ƥ���������
racc �Ǥϰ��̤ˡ�ͽ���פϹԤ���Ƭ���褿������ͽ���Ȥߤʤ����Τ�
prechigh �ʤɤ⥷��ܥ�Ȥ��ƻȤ��ޤ�����������ʥ����ͳ���� end ������
�ɤ���äƤ�ͽ���ˤʤäƤ��ޤ��ޤ���

=== ���ץ����

racc �Υ��ޥ�ɥ饤�󥪥ץ����ΰ�����ե�������˥ǥե������
�Ȥ��Ƶ��Ҥ��뤳�Ȥ��Ǥ��ޤ���
--
options ���ץ���� ���ץ���� ��
--
���ߤ����ǻȤ���Τ�

  * omit_action_call

���Υ��������ƤӽФ����ά����

  * result_var

�ѿ� result ��Ȥ�

�Ǥ���
���줾�� no_ ��Ƭ�ˤĤ��뤳�Ȥǰ�̣��ȿž�Ǥ��ޤ���

=== expect

���Ѥˤʤ�ѡ����Ϥ����Ƥ�̵���� shift/reduce conflict ��ޤߤޤ���
������ʸˡ�ե������񤤤��ܿͤϤ�����ΤäƤ��뤫�餤���Ǥ�����
�桼����ʸˡ�ե����������������ˡ�conflict�פ�ɽ�����줿��
�԰¤˻פ��Ǥ��礦�����Τ褦�ʾ�硢�ʲ��Τ褦�˽񤤤Ƥ�����
shift/reduce conflict �Υ�å������������Ǥ��ޤ���
--
expect 3
--
���ξ�� shift/reduce conflict �ϤԤä��껰�ĤǤʤ���Ф����ޤ���
���ĤǤʤ����Ϥ�Ϥ�ɽ�����Фޤ� (����Ǥ�Фޤ�)��
�ޤ� reduce/reduce conflict ��ɽ���������Ǥ��ޤ���

=== �ȡ����󥷥�ܥ��ͤ��ѹ�

�ȡ����󥷥�ܥ��ɽ���ͤϡ��ǥե���ȤǤ�

  * ʸˡ�桢������Ǥ����ޤ�Ƥ��ʤ���� (RULE�Ȥ�XEND�Ȥ�)
    ������̾����ʸ����� intern ���������륷��ܥ� (1.4 �Ǥ� Fixnum)
  * ������Ǥ����ޤ�Ƥ�����(':'�Ȥ�'.'�Ȥ�)
    ������ʸ���󤽤Τޤ�

�ȤʤäƤ��ޤ��������Ȥ���¾�η����Υ�����ʤ����Ǥ�¸�ߤ�����ʤɤϡ�
����ˤ��碌�ʤ���Фʤ餺�����ΤޤޤǤ����ؤǤ������Τ褦�ʾ��ˤϡ�
convert ���ä��뤳�Ȥǡ��ȡ����󥷥�ܥ��ɽ���ͤ��Ѥ��뤳�Ȥ��Ǥ��ޤ���
�ʲ���������Ǥ���
--
convert
  PLUS 'PlusClass'      #�� PlusClass
  MIN  'MinusClass'     #�� MinusClass
end
--
�ǥե���ȤǤϥȡ����󥷥�ܥ� PLUS ���Ф��Ƥϥȡ����󥷥�ܥ��ͤ�
:PLUS �Ǥ�������Τ褦�ʵ��Ҥ�������� PlusClass �ˤʤ�ޤ���
�Ѵ�����ͤ� false��nil �ʳ��ʤ�ʤ�Ǥ�Ȥ��ޤ���

�Ѵ�����ͤȤ���ʸ�����Ȥ��Ȥ��ϡ����Τ褦�˰������Ťͤ�ɬ�פ�����ޤ���
--
convert
  PLUS '"plus"'       #�� "plus"
end
--
�ޤ�����'�פ�ȤäƤ��������줿 Ruby �Υ����ɾ�Ǥϡ�"�פˤʤ�Τ�
��դ��Ƥ����������Хå�����å���ˤ�륯�����Ȥ�ͭ���Ǥ������Хå�
����å���Ͼä����ˤ��Τޤ޻Ĥ�ޤ���
--
PLUS '"plus\n"'          #�� "plus\n"
MIN  "\"minus#{val}\""   #�� \"minus#{val}\"
--

=== �������ȵ�§

�ѡ�����Ĥ��뤿��ˤϡ��ɤε�§���ֺǽ�Ρ׵�§�����Ȥ������Ȥ� Racc �ˤ�������
���ʤ���Ф����ޤ��󡣤��������Ū�˽񤯤Τ��������ȵ�§�Ǥ����������ȵ�§��
���Τ褦�˽񤭤ޤ���
--
start real_target
--
start �ϹԤκǽ�ˤ��ʤ���Ф����ޤ��󡣤��Τ褦�˽񤯤ȡ��ե������
���ֺǽ�˽ФƤ��� real_target �ε�§�򥹥����ȵ�§�Ȥ��ƻȤ��ޤ���
��ά�������ϡ��ե�����κǽ�ε�§���������ȵ�§�ˤʤ�ޤ������̤�
�ǽ�ε�§����־�ˤ����ۤ����񤭤䤹�����狼��䤹���ʤ�ޤ����顢
���ε�ˡ�Ϥ��ޤ�Ĥ���ɬ�פϤʤ��Ǥ��礦��

=== �桼������������

�桼���������ɤϡ��ѡ������饹���񤭤��ޤ��ե�����ˡ�
����������¾�ˤ⥳���ɤ�ޤ᤿�����˻Ȥ��ޤ������Τ褦�ʤ�Τ�
�񤭤��ޤ����˱����ƻ���¸�ߤ����ѡ������饹�����������
header�����饹�������(����Ƭ)�� inner������θ夬 footer �Ǥ���
�桼�������ɤȤ��ƽ񤤤���Τ��������ä����ˤ��Τޤ�Ϣ�뤵��ޤ���

�桼�������������ν񼰤ϰʲ����̤�Ǥ���
--
---- ���̻�
  ruby ��ʸ
  ruby ��ʸ
  ruby ��ʸ

---- ���̻�
  ruby ��ʸ
     :
--
�Ԥ���Ƭ����ͤİʾ�Ϣ³������-��(�ޥ��ʥ�)������ȥ桼���������ɤ�
�ߤʤ���ޤ������̻Ҥϰ�Ĥ�ñ��ǡ����Τ��Ȥˤϡ�=�װʳ��ʤ鲿��
�񤤤Ƥ⤫�ޤ��ޤ���
e
= Racc Grammar File Reference

== Global Structure

== Class Block and User Code Block

There's two block on toplevel.
one is 'class' block, another is 'user code' block. 'user code' block MUST
places after 'class' block.

== Comment

You can insert comment about all places. Two style comment can be used,
Ruby style (#.....) and C style (/*......*/) .

== Class Block

The class block is formed like this:
--
class CLASS_NAME
  [precedance table]
  [token declearations]
  [expected number of S/R conflict]
  [options]
  [semantic value convertion]
  [start rule]
rule
  GRAMMARS
--
CLASS_NAME is a name of parser class.
This is the name of generating parser class.

If CLASS_NAME includes '::', Racc outputs module clause.
For example, writing "class M::C" causes creating the code bellow:
--
module M
  class C
    :
    :
  end
end
--

== Grammar Block

The grammar block discripts grammar which is able
to be understood by parser.  Syntax is:
--
(token): (token) (token) (token).... (action)

(token): (token) (token) (token).... (action)
       | (token) (token) (token).... (action)
       | (token) (token) (token).... (action)
--
(action) is an action which is executed when its (token)s are found.
(action) is a ruby code block, which is surrounded by braces:
--
{ print val[0]
  puts val[1] }
--
Note that you cannot use '%' string, here document, '%r' regexp in action.

Actions can be omitted.
When it is omitted, '' (empty string) is used.

A return value of action is a value of left side value ($$).
It is value of result, or returned value by "return" statement.

Here is an example of whole grammar block.
--
rule
  goal: definition ruls source { result = val }

  definition: /* none */   { result = [] }
    | definition startdesig  { result[0] = val[1] }
    | definition
             precrule   # this line continue from upper line
      {
        result[1] = val[1]
      }

  startdesig: START TOKEN
--
You can use following special local variables in action.

  * result ($$)

The value of left-hand side (lhs). A default value is val[0].

  * val ($1,$2,$3...)

An array of value of right-hand side (rhs).

  * _values (...$-2,$-1,$0)

A stack of values.
DO NOT MODIFY this stack unless you know what you are doing.

== Operator Precedance

This function is equal to '%prec' in yacc.
To designate this block:
--
prechigh
  nonassoc '++'
  left     '*' '/'
  left     '+' '-'
  right    '='
preclow
--
`right' is yacc's %right, `left' is yacc's %left.

`=' + (symbol) means yacc's %prec:
--
prechigh
  nonassoc UMINUS
  left '*' '/'
  left '+' '-'
preclow

rule
  exp: exp '*' exp
     | exp '-' exp
     | '-' exp       =UMINUS   # equals to "%prec UMINUS"
         :
         :
--

== expect

Racc has bison's "expect" directive.
--
# Example

class MyParser
rule
  expect 3
    :
    :
--
This directive declears "expected" number of shift/reduce conflict.
If "expected" number is equal to real number of conflicts,
racc does not print confliction warning message.

== Declaring Tokens

By declaring tokens, you can avoid many meanless bugs.
If decleared token does not exist/existing token does not decleared,
Racc output warnings.  Declearation syntax is:
--
token TOKEN_NAME AND_IS_THIS
      ALSO_THIS_IS AGAIN_AND_AGAIN THIS_IS_LAST
--

== Options

You can write options for racc command in your racc file.
--
options OPTION OPTION ...
--
Options are:

  * omit_action_call

omit empty action call or not.

  * result_var

use/does not use local variable "result"

You can use 'no_' prefix to invert its meanings.

== Converting Token Symbol

Token symbols are, as default,

  * naked token string in racc file (TOK, XFILE, this_is_token, ...)
    --&gt; symbol (:TOK, :XFILE, :this_is_token, ...)
  * quoted string (':', '.', '(', ...)
    --&gt; same string (':', '.', '(', ...)

You can change this default by "convert" block.
Here is an example:
--
convert
  PLUS 'PlusClass'      # We use PlusClass for symbol of `PLUS'
  MIN  'MinusClass'     # We use MinusClass for symbol of `MIN'
end
--
We can use almost all ruby value can be used by token symbol,
except 'false' and 'nil'.  These are causes unexpected parse error.

If you want to use String as token symbol, special care is required.
For example:
--
convert
  class '"cls"'            # in code, "cls"
  PLUS '"plus\n"'          # in code, "plus\n"
  MIN  "\"minus#{val}\""   # in code, \"minus#{val}\"
end
--

== Start Rule

'%start' in yacc. This changes start rule.
--
start real_target
--
This statement will not be used forever, I think.

== User Code Block

"User Code Block" is a Ruby source code which is copied to output.
There are three user code block, "header" "inner" and "footer".

Format of user code is like this:
--
---- header
  ruby statement
  ruby statement
  ruby statement

---- inner
  ruby statement
     :
     :
--
If four '-' exist on line head,
racc treat it as beginning of user code block.
A name of user code must be one word.
.
