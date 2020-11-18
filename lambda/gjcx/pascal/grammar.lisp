;;; -*- Mode:LISP; Package:PASCAL; Readtable:CL; Fonts:(CPTFONTB); Base:10 -*-

(define-grammar "pascal" "for pascal"

  (<program>
    PROGRAM <identifier> ( \( <id-list> \) ) \; <block> \.)

  (<id-list>
    <identifier> (k* \, <identifier>))

  (<block>
    (<block-label>) (<block-const>) (<block-type>) (<block-var>) (<block-procs>) <block-begin>)

  (<block>
    FORWARD)

  (<block-label>
    LABEL <unsigned-integer> (k* \, <unsigned-integer>) \;)

  (<unsigned-integer>
    <number>)

  (<block-const>
    CONST <id-equals-const-list>)

  (<id-equals-const-list>
    <identifier> = <constant> \; (k* <identifier> = <constant> \;))

  (<block-type>
    TYPE  <id-equals-type-list>)

  (<id-equals-type-list>
    <identifier> = <type> \; (k* <identifier> = <type> \;))

  (<block-var>
    VAR  <idlist-colon-type-list>)

  (<idlist-colon-type-list>
    <id-list> \: <type> \; (k* <id-list> \: <type> \;))

  (<block-procs>
    <proc-or-fn> \; (k* <proc-or-fn> \;))

  (<proc-or-fn>
    <procedure>)

  (<proc-or-fn>
    <function>)

  (<procedure>
    PROCEDURE <identifier> (\( <param-list> \)) \; <block>)

  (<function>
    FUNCTION <identifier> (\( <param-list> \)) \: <identifier> \; <block>)

  (<block-begin>
    BEGIN (<statement-list>) END)

  (<statement-list>
    <l-statement> (k* \; <l-statement>))

  (<param-list>
    <param> (k* \; <param>))

  (<param>
    (VAR) <id-list> \: <identifier>)

  (<param>
    PROCEDURE <identifier> \( <param-list> \))

  (<param>
    FUNCTION <identifier> \( <param-list> \) \: <identifier>)

  (<type>
    <simple-type>)

  (<type>
    ^ <identifier>)

  (<type>
    (PACKED) SET OF <simple-type>)

  (<type>
    (PACKED) ARRAY [ <simple-type-list> ] OF <type>)

  (<simple-type-list>
    <simple-type> (\, <simple-type-list>))

  (<type>
    PACKED RECORD <field-list> END)

  (<type>
    PACKED FILE OF <type>)

  (<type>
    RECORD <field-list> END)

  (<type>
    FILE (OF <type>))

  (<type>
    STRING [ <unsigned-integer> ])

  (<field-list>
    <simple-field-list> (\; <case-field-list>))

  (<field-list>
    <case-field-list>)

  (<simple-field-list>
    <id-list> \: <type> (\; <simple-field-list>))

  (<case-field-list>
    CASE (<identifier> \:) <identifier> OF <type-case-fields>)

  (<type-case-fields>
    <constant-list> \: \( <field-list> \) (\; <type-case-fields>))

  (<constant-list>
    <constant> (k* \, <constant>))

  (<simple-type>
    <identifier>)

  (<simple-type>
    \( <id-list> \))

  (<simple-type>
    <constant> \.\. <constant>)

  (<l-statement>
    <unsigned-integer> \: <statement>)

  (<l-statement>
    <statement>)

  (<statement>
    <variable> \:= <expression>)

  (<statement>
    <identifier> (\( <expression-list> \)))

  (<statement>
    BEGIN (<statement-list>) (\;) END)

  (<statement>
    IF <expression> THEN <statement> (\;) (ELSE <statement>))

  (<statement>
    REPEAT <statement-list> (\;) UNTIL <expression>)

  (<statement>
    WHILE <expression> DO <statement>)

  (<statement>
    FOR <identifier> \:= <expression> <to-downto> <expression> DO <statement>)

  (<to-downto>
    TO)

  (<to-downto>
    DOWNTO)

  (<statement>
    CASE <expression> OF <case-clause-list> (\;) END)

  (<case-clause-list>
    <constant-list> \: <statement> (k* \; <constant-list> \: <statement>))

  (<statement>
    WITH <variable-list> DO <statement>)

  (<variable-list>
    <variable> (k* \, <variable>))

  (<statement>
    GOTO <unsigned-integer>)

  (<expression>
    <simple-expression> (<logical-op> <simple-expression>))

  (<logical-op>
    <)

  (<logical-op>
    <=)

  (<logical-op>
    =)

  (<logical-op>
    <>)

  (<logical-op>
    >=)

  (<logical-op>
    >)

  (<logical-op>
    IN)

  (<simple-expression>
    <term-sequence>)

  (<term-sequence>
    (<plus-minus>) <term> (<plus-minus-or> <term-sequence>))

  (<plus-minus>
    +)

  (<plus-minus>
    -)

  (<plus-minus-or>
    +)

  (<plus-minus-or>
    -)

  (<plus-minus-or>
    OR)

  (<term>
    <factor-sequence>)

  (<factor-sequence>
    <factor> (<star-slash-div-mod-and> <factor-sequence>))

  (<star-slash-div-mod-and>
    *)

  (<star-slash-div-mod-and>
    /)

  (<star-slash-div-mod-and>
    DIV)

  (<star-slash-div-mod-and>
    MOD)

  (<star-slash-div-mod-and>
    AND)

  (<factor>
    <unsigned-constant>)

  (<factor>
    <variable>)

  (<factor>
    <identifier> \( <expression-list> \))

  (<expression-list>
    <expression> (k* \, <expression>))

  (<factor>
    \( <expression> \))

  (<factor>
    NOT <FACTOR>)

  (<factor>
    [ <factor-expression-list> ])

  (<factor-expression-list>
    <expression> (\.\. <expression>) (k* \, <expression> (\.\. <expression>)))

  (<variable>
    <variable> <variable-suffix>)

  (<variable>
    <identifier>)

  (<variable-suffix>
    [ <expression-list> ])

  (<variable-suffix>
    \. <identifier>)

  (<variable-suffix>
    ^)

  (<constant>
    (<plus-minus>) <identifier>)

  (<constant>
    (<plus-minus>) <unsigned-number>)

  (<unsigned-number>
    <number>)

  (<constant>
    <string>)

  (<unsigned-constant>
    <unsigned-number>)

  (<unsigned-constant>
    NIL)

  (<unsigned-constant>
    <string>)

  )
