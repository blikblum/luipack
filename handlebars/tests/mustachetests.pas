unit MustacheTests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, HandlebarsTestCase;

type
  TCommentsTests = class(THandlebarsTestCase)
  published
    procedure TestInline;
    procedure TestMultiline;
    procedure TestStandalone;
    procedure TestIndentedStandalone;
    procedure TestStandaloneLineEndings;
    procedure TestStandaloneWithoutPreviousLine;
    procedure TestStandaloneWithoutNewline;
    procedure TestMultilineStandalone;
    procedure TestIndentedMultilineStandalone;
    procedure TestIndentedInline;
    procedure TestSurroundingWhitespace;
  end;

  TDelimitersTests = class(THandlebarsTestCase)
  published
    procedure TestPairBehavior;
    procedure TestSpecialCharacters;
    procedure TestSections;
    procedure TestInvertedSections;
    procedure TestPartialInheritence;
    procedure TestPostPartialBehavior;
    procedure TestSurroundingWhitespace;
    procedure TestOutlyingWhitespaceInline;
    procedure TestStandaloneTag;
    procedure TestIndentedStandaloneTag;
    procedure TestStandaloneLineEndings;
    procedure TestStandaloneWithoutPreviousLine;
    procedure TestStandaloneWithoutNewline;
    procedure TestPairwithPadding;
  end;

  TInterpolationTests = class(THandlebarsTestCase)
  published
    procedure TestNoInterpolation;
    procedure TestBasicInterpolation;
    procedure TestHTMLEscaping;
    procedure TestTripleMustache;
    procedure TestAmpersand;
    procedure TestBasicIntegerInterpolation;
    procedure TestTripleMustacheIntegerInterpolation;
    procedure TestAmpersandIntegerInterpolation;
    procedure TestBasicDecimalInterpolation;
    procedure TestTripleMustacheDecimalInterpolation;
    procedure TestAmpersandDecimalInterpolation;
    procedure TestBasicContextMissInterpolation;
    procedure TestTripleMustacheContextMissInterpolation;
    procedure TestAmpersandContextMissInterpolation;
    procedure TestDottedNamesBasicInterpolation;
    procedure TestDottedNamesTripleMustacheInterpolation;
    procedure TestDottedNamesAmpersandInterpolation;
    procedure TestDottedNamesArbitraryDepth;
    procedure TestDottedNamesBrokenChains;
    procedure TestDottedNamesBrokenChainResolution;
    procedure TestDottedNamesInitialResolution;
    procedure TestInterpolationSurroundingWhitespace;
    procedure TestTripleMustacheSurroundingWhitespace;
    procedure TestAmpersandSurroundingWhitespace;
    procedure TestInterpolationStandalone;
    procedure TestTripleMustacheStandalone;
    procedure TestAmpersandStandalone;
    procedure TestInterpolationWithPadding;
    procedure TestTripleMustacheWithPadding;
    procedure TestAmpersandWithPadding;
  end;

  TInvertedTests = class(THandlebarsTestCase)
  published
    procedure TestFalsey;
    procedure TestTruthy;
    procedure TestContext;
    procedure TestList;
    procedure TestEmptyList;
    procedure TestDoubled;
    procedure TestNestedFalsey;
    procedure TestNestedTruthy;
    procedure TestContextMisses;
    procedure TestDottedNamesTruthy;
    procedure TestDottedNamesFalsey;
    procedure TestDottedNamesBrokenChains;
    procedure TestSurroundingWhitespace;
    procedure TestInternalWhitespace;
    procedure TestIndentedInlineSections;
    procedure TestStandaloneLines;
    procedure TestStandaloneIndentedLines;
    procedure TestStandaloneLineEndings;
    procedure TestStandaloneWithoutPreviousLine;
    procedure TestStandaloneWithoutNewline;
    procedure TestPadding;
  end;

  TPartialsTests = class(THandlebarsTestCase)
  published
    procedure TestBasicBehavior;
    procedure TestFailedLookup;
    procedure TestContext;
    procedure TestRecursion;
    procedure TestSurroundingWhitespace;
    procedure TestInlineIndentation;
    procedure TestStandaloneLineEndings;
    procedure TestStandaloneWithoutPreviousLine;
    procedure TestStandaloneWithoutNewline;
    procedure TestStandaloneIndentation;
    procedure TestPaddingWhitespace;
  end;

  TSectionsTests = class(THandlebarsTestCase)
  published
    procedure TestTruthy;
    procedure TestFalsey;
    procedure TestContext;
    procedure TestDeeplyNestedContexts;
    procedure TestList;
    procedure TestEmptyList;
    procedure TestDoubled;
    procedure TestNestedTruthy;
    procedure TestNestedFalsey;
    procedure TestContextMisses;
    procedure TestImplicitIteratorString;
    procedure TestImplicitIteratorInteger;
    procedure TestImplicitIteratorDecimal;
    procedure TestDottedNamesTruthy;
    procedure TestDottedNamesFalsey;
    procedure TestDottedNamesBrokenChains;
    procedure TestSurroundingWhitespace;
    procedure TestInternalWhitespace;
    procedure TestIndentedInlineSections;
    procedure TestStandaloneLines;
    procedure TestIndentedStandaloneLines;
    procedure TestStandaloneLineEndings;
    procedure TestStandaloneWithoutPreviousLine;
    procedure TestStandaloneWithoutNewline;
    procedure TestPadding;
  end;

  TLambdasTests = class(THandlebarsTestCase)
  published
    procedure TestInterpolation;
    procedure TestInterpolationExpansion;
    procedure TestInterpolationAlternateDelimiters;
    procedure TestInterpolationMultipleCalls;
    procedure TestEscaping;
    procedure TestSection;
    procedure TestSectionExpansion;
    procedure TestSectionAlternateDelimiters;
    procedure TestSectionMultipleCalls;
    procedure TestInvertedSection;
  end;

implementation

procedure TCommentsTests.TestInline;
begin
  CheckRender('12345{{! Comment Block! }}67890','{}','1234567890')
end;

procedure TCommentsTests.TestMultiline;
begin
  CheckRender('12345{{!'+ LineEnding +'  This is a'+ LineEnding +'  multi-line comment...'+ LineEnding +'}}67890'+ LineEnding +'','{}','1234567890'+ LineEnding +'')
end;

procedure TCommentsTests.TestStandalone;
begin
  CheckRender('Begin.'+ LineEnding +'{{! Comment Block! }}'+ LineEnding +'End.'+ LineEnding +'','{}','Begin.'+ LineEnding +'End.'+ LineEnding +'')
end;

procedure TCommentsTests.TestIndentedStandalone;
begin
  CheckRender('Begin.'+ LineEnding +'  {{! Indented Comment Block! }}'+ LineEnding +'End.'+ LineEnding +'','{}','Begin.'+ LineEnding +'End.'+ LineEnding +'')
end;

procedure TCommentsTests.TestStandaloneLineEndings;
begin
  CheckRender('|'+ LineEnding +'{{! Standalone Comment }}'+ LineEnding +'|','{}','|'+ LineEnding +'|')
end;

procedure TCommentsTests.TestStandaloneWithoutPreviousLine;
begin
  CheckRender('  {{! I''m Still Standalone }}'+ LineEnding +'!','{}','!')
end;

procedure TCommentsTests.TestStandaloneWithoutNewline;
begin
  CheckRender('!'+ LineEnding +'  {{! I''m Still Standalone }}','{}','!'+ LineEnding +'')
end;

procedure TCommentsTests.TestMultilineStandalone;
begin
  CheckRender('Begin.'+ LineEnding +'{{!'+ LineEnding +'Something''s going on here...'+ LineEnding +'}}'+ LineEnding +'End.'+ LineEnding +'','{}','Begin.'+ LineEnding +'End.'+ LineEnding +'')
end;

procedure TCommentsTests.TestIndentedMultilineStandalone;
begin
  CheckRender('Begin.'+ LineEnding +'  {{!'+ LineEnding +'    Something''s going on here...'+ LineEnding +'  }}'+ LineEnding +'End.'+ LineEnding +'','{}','Begin.'+ LineEnding +'End.'+ LineEnding +'')
end;

procedure TCommentsTests.TestIndentedInline;
begin
  CheckRender('  12 {{! 34 }}'+ LineEnding +'','{}','  12 '+ LineEnding +'')
end;

procedure TCommentsTests.TestSurroundingWhitespace;
begin
  CheckRender('12345 {{! Comment Block! }} 67890','{}','12345  67890')
end;


procedure TDelimitersTests.TestPairBehavior;
begin
  CheckRender('{{=<% %>=}}(<%text%>)','{ "text" : "Hey!" }','(Hey!)')
end;

procedure TDelimitersTests.TestSpecialCharacters;
begin
  CheckRender('({{=[ ]=}}[text])','{ "text" : "It worked!" }','(It worked!)')
end;

procedure TDelimitersTests.TestSections;
begin
  CheckRender('['+ LineEnding +'{{#section}}'+ LineEnding +'  {{data}}'+ LineEnding +'  |data|'+ LineEnding +'{{/section}}'+ LineEnding +''+ LineEnding +'{{= | | =}}'+ LineEnding +'|#section|'+ LineEnding +'  {{data}}'+ LineEnding +'  |data|'+ LineEnding +'|/section|'+ LineEnding +']'+ LineEnding +'','{ "section" : true, "data" : "I got interpolated." }','['+ LineEnding +'  I got interpolated.'+ LineEnding +'  |data|'+ LineEnding +''+ LineEnding +'  {{data}}'+ LineEnding +'  I got interpolated.'+ LineEnding +']'+ LineEnding +'')
end;

procedure TDelimitersTests.TestInvertedSections;
begin
  CheckRender('['+ LineEnding +'{{^section}}'+ LineEnding +'  {{data}}'+ LineEnding +'  |data|'+ LineEnding +'{{/section}}'+ LineEnding +''+ LineEnding +'{{= | | =}}'+ LineEnding +'|^section|'+ LineEnding +'  {{data}}'+ LineEnding +'  |data|'+ LineEnding +'|/section|'+ LineEnding +']'+ LineEnding +'','{ "section" : false, "data" : "I got interpolated." }','['+ LineEnding +'  I got interpolated.'+ LineEnding +'  |data|'+ LineEnding +''+ LineEnding +'  {{data}}'+ LineEnding +'  I got interpolated.'+ LineEnding +']'+ LineEnding +'')
end;

procedure TDelimitersTests.TestPartialInheritence;
begin
  CheckRender('[ {{>include}} ]'+ LineEnding +'{{= | | =}}'+ LineEnding +'[ |>include| ]'+ LineEnding +'','{ "value" : "yes" }','[ .yes. ]'+ LineEnding +'[ .yes. ]'+ LineEnding +'')
end;

procedure TDelimitersTests.TestPostPartialBehavior;
begin
  CheckRender('[ {{>include}} ]'+ LineEnding +'[ .{{value}}.  .|value|. ]'+ LineEnding +'','{ "value" : "yes" }','[ .yes.  .yes. ]'+ LineEnding +'[ .yes.  .|value|. ]'+ LineEnding +'')
end;

procedure TDelimitersTests.TestSurroundingWhitespace;
begin
  CheckRender('| {{=@ @=}} |','{}','|  |')
end;

procedure TDelimitersTests.TestOutlyingWhitespaceInline;
begin
  CheckRender(' | {{=@ @=}}'+ LineEnding +'','{}',' | '+ LineEnding +'')
end;

procedure TDelimitersTests.TestStandaloneTag;
begin
  CheckRender('Begin.'+ LineEnding +'{{=@ @=}}'+ LineEnding +'End.'+ LineEnding +'','{}','Begin.'+ LineEnding +'End.'+ LineEnding +'')
end;

procedure TDelimitersTests.TestIndentedStandaloneTag;
begin
  CheckRender('Begin.'+ LineEnding +'  {{=@ @=}}'+ LineEnding +'End.'+ LineEnding +'','{}','Begin.'+ LineEnding +'End.'+ LineEnding +'')
end;

procedure TDelimitersTests.TestStandaloneLineEndings;
begin
  CheckRender('|'+ LineEnding +'{{= @ @ =}}'+ LineEnding +'|','{}','|'+ LineEnding +'|')
end;

procedure TDelimitersTests.TestStandaloneWithoutPreviousLine;
begin
  CheckRender('  {{=@ @=}}'+ LineEnding +'=','{}','=')
end;

procedure TDelimitersTests.TestStandaloneWithoutNewline;
begin
  CheckRender('='+ LineEnding +'  {{=@ @=}}','{}','='+ LineEnding +'')
end;

procedure TDelimitersTests.TestPairwithPadding;
begin
  CheckRender('|{{= @   @ =}}|','{}','||')
end;


procedure TInterpolationTests.TestNoInterpolation;
begin
  CheckRender('Hello from {Mustache}!'+ LineEnding +'','{}','Hello from {Mustache}!'+ LineEnding +'')
end;

procedure TInterpolationTests.TestBasicInterpolation;
begin
  CheckRender('Hello, {{subject}}!'+ LineEnding +'','{ "subject" : "world" }','Hello, world!'+ LineEnding +'')
end;

procedure TInterpolationTests.TestHTMLEscaping;
begin
  CheckRender('These characters should be HTML escaped: {{forbidden}}'+ LineEnding +'','{ "forbidden" : "& \" < >" }','These characters should be HTML escaped: &amp; &quot; &lt; &gt;'+ LineEnding +'')
end;

procedure TInterpolationTests.TestTripleMustache;
begin
  CheckRender('These characters should not be HTML escaped: {{{forbidden}}}'+ LineEnding +'','{ "forbidden" : "& \" < >" }','These characters should not be HTML escaped: & " < >'+ LineEnding +'')
end;

procedure TInterpolationTests.TestAmpersand;
begin
  CheckRender('These characters should not be HTML escaped: {{&forbidden}}'+ LineEnding +'','{ "forbidden" : "& \" < >" }','These characters should not be HTML escaped: & " < >'+ LineEnding +'')
end;

procedure TInterpolationTests.TestBasicIntegerInterpolation;
begin
  CheckRender('"{{mph}} miles an hour!"','{ "mph" : 85 }','"85 miles an hour!"')
end;

procedure TInterpolationTests.TestTripleMustacheIntegerInterpolation;
begin
  CheckRender('"{{{mph}}} miles an hour!"','{ "mph" : 85 }','"85 miles an hour!"')
end;

procedure TInterpolationTests.TestAmpersandIntegerInterpolation;
begin
  CheckRender('"{{&mph}} miles an hour!"','{ "mph" : 85 }','"85 miles an hour!"')
end;

procedure TInterpolationTests.TestBasicDecimalInterpolation;
begin
  CheckRender('"{{power}} jiggawatts!"','{ "power" : 1.21000000000000E+000 }','"1.21 jiggawatts!"')
end;

procedure TInterpolationTests.TestTripleMustacheDecimalInterpolation;
begin
  CheckRender('"{{{power}}} jiggawatts!"','{ "power" : 1.21000000000000E+000 }','"1.21 jiggawatts!"')
end;

procedure TInterpolationTests.TestAmpersandDecimalInterpolation;
begin
  CheckRender('"{{&power}} jiggawatts!"','{ "power" : 1.21000000000000E+000 }','"1.21 jiggawatts!"')
end;

procedure TInterpolationTests.TestBasicContextMissInterpolation;
begin
  CheckRender('I ({{cannot}}) be seen!','{}','I () be seen!')
end;

procedure TInterpolationTests.TestTripleMustacheContextMissInterpolation;
begin
  CheckRender('I ({{{cannot}}}) be seen!','{}','I () be seen!')
end;

procedure TInterpolationTests.TestAmpersandContextMissInterpolation;
begin
  CheckRender('I ({{&cannot}}) be seen!','{}','I () be seen!')
end;

procedure TInterpolationTests.TestDottedNamesBasicInterpolation;
begin
  CheckRender('"{{person.name}}" == "{{#person}}{{name}}{{/person}}"','{ "person" : { "name" : "Joe" } }','"Joe" == "Joe"')
end;

procedure TInterpolationTests.TestDottedNamesTripleMustacheInterpolation;
begin
  CheckRender('"{{{person.name}}}" == "{{#person}}{{{name}}}{{/person}}"','{ "person" : { "name" : "Joe" } }','"Joe" == "Joe"')
end;

procedure TInterpolationTests.TestDottedNamesAmpersandInterpolation;
begin
  CheckRender('"{{&person.name}}" == "{{#person}}{{&name}}{{/person}}"','{ "person" : { "name" : "Joe" } }','"Joe" == "Joe"')
end;

procedure TInterpolationTests.TestDottedNamesArbitraryDepth;
begin
  CheckRender('"{{a.b.c.d.e.name}}" == "Phil"','{ "a" : { "b" : { "c" : { "d" : { "e" : { "name" : "Phil" } } } } } }','"Phil" == "Phil"')
end;

procedure TInterpolationTests.TestDottedNamesBrokenChains;
begin
  CheckRender('"{{a.b.c}}" == ""','{ "a" : {} }','"" == ""')
end;

procedure TInterpolationTests.TestDottedNamesBrokenChainResolution;
begin
  CheckRender('"{{a.b.c.name}}" == ""','{ "a" : { "b" : {} }, "c" : { "name" : "Jim" } }','"" == ""')
end;

procedure TInterpolationTests.TestDottedNamesInitialResolution;
begin
  CheckRender('"{{#a}}{{b.c.d.e.name}}{{/a}}" == "Phil"','{ "a" : { "b" : { "c" : { "d" : { "e" : { "name" : "Phil" } } } } }, "b" : { "c" : { "d" : { "e" : { "name" : "Wrong" } } } } }','"Phil" == "Phil"')
end;

procedure TInterpolationTests.TestInterpolationSurroundingWhitespace;
begin
  CheckRender('| {{string}} |','{ "string" : "---" }','| --- |')
end;

procedure TInterpolationTests.TestTripleMustacheSurroundingWhitespace;
begin
  CheckRender('| {{{string}}} |','{ "string" : "---" }','| --- |')
end;

procedure TInterpolationTests.TestAmpersandSurroundingWhitespace;
begin
  CheckRender('| {{&string}} |','{ "string" : "---" }','| --- |')
end;

procedure TInterpolationTests.TestInterpolationStandalone;
begin
  CheckRender('  {{string}}'+ LineEnding +'','{ "string" : "---" }','  ---'+ LineEnding +'')
end;

procedure TInterpolationTests.TestTripleMustacheStandalone;
begin
  CheckRender('  {{{string}}}'+ LineEnding +'','{ "string" : "---" }','  ---'+ LineEnding +'')
end;

procedure TInterpolationTests.TestAmpersandStandalone;
begin
  CheckRender('  {{&string}}'+ LineEnding +'','{ "string" : "---" }','  ---'+ LineEnding +'')
end;

procedure TInterpolationTests.TestInterpolationWithPadding;
begin
  CheckRender('|{{ string }}|','{ "string" : "---" }','|---|')
end;

procedure TInterpolationTests.TestTripleMustacheWithPadding;
begin
  CheckRender('|{{{ string }}}|','{ "string" : "---" }','|---|')
end;

procedure TInterpolationTests.TestAmpersandWithPadding;
begin
  CheckRender('|{{& string }}|','{ "string" : "---" }','|---|')
end;


procedure TInvertedTests.TestFalsey;
begin
  CheckRender('"{{^boolean}}This should be rendered.{{/boolean}}"','{ "boolean" : false }','"This should be rendered."')
end;

procedure TInvertedTests.TestTruthy;
begin
  CheckRender('"{{^boolean}}This should not be rendered.{{/boolean}}"','{ "boolean" : true }','""')
end;

procedure TInvertedTests.TestContext;
begin
  CheckRender('"{{^context}}Hi {{name}}.{{/context}}"','{ "context" : { "name" : "Joe" } }','""')
end;

procedure TInvertedTests.TestList;
begin
  CheckRender('"{{^list}}{{n}}{{/list}}"','{ "list" : [{ "n" : 1 }, { "n" : 2 }, { "n" : 3 }] }','""')
end;

procedure TInvertedTests.TestEmptyList;
begin
  CheckRender('"{{^list}}Yay lists!{{/list}}"','{ "list" : [] }','"Yay lists!"')
end;

procedure TInvertedTests.TestDoubled;
begin
  CheckRender('{{^bool}}'+ LineEnding +'* first'+ LineEnding +'{{/bool}}'+ LineEnding +'* {{two}}'+ LineEnding +'{{^bool}}'+ LineEnding +'* third'+ LineEnding +'{{/bool}}'+ LineEnding +'','{ "two" : "second", "bool" : false }','* first'+ LineEnding +'* second'+ LineEnding +'* third'+ LineEnding +'')
end;

procedure TInvertedTests.TestNestedFalsey;
begin
  CheckRender('| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |','{ "bool" : false }','| A B C D E |')
end;

procedure TInvertedTests.TestNestedTruthy;
begin
  CheckRender('| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |','{ "bool" : true }','| A  E |')
end;

procedure TInvertedTests.TestContextMisses;
begin
  CheckRender('[{{^missing}}Cannot find key ''missing''!{{/missing}}]','{}','[Cannot find key ''missing''!]')
end;

procedure TInvertedTests.TestDottedNamesTruthy;
begin
  CheckRender('"{{^a.b.c}}Not Here{{/a.b.c}}" == ""','{ "a" : { "b" : { "c" : true } } }','"" == ""')
end;

procedure TInvertedTests.TestDottedNamesFalsey;
begin
  CheckRender('"{{^a.b.c}}Not Here{{/a.b.c}}" == "Not Here"','{ "a" : { "b" : { "c" : false } } }','"Not Here" == "Not Here"')
end;

procedure TInvertedTests.TestDottedNamesBrokenChains;
begin
  CheckRender('"{{^a.b.c}}Not Here{{/a.b.c}}" == "Not Here"','{ "a" : {} }','"Not Here" == "Not Here"')
end;

procedure TInvertedTests.TestSurroundingWhitespace;
begin
  CheckRender(' | {{^boolean}}	|	{{/boolean}} | '+ LineEnding +'','{ "boolean" : false }',' | 	|	 | '+ LineEnding +'')
end;

procedure TInvertedTests.TestInternalWhitespace;
begin
  CheckRender(' | {{^boolean}} {{! Important Whitespace }}'+ LineEnding +' {{/boolean}} | '+ LineEnding +'','{ "boolean" : false }',' |  '+ LineEnding +'  | '+ LineEnding +'')
end;

procedure TInvertedTests.TestIndentedInlineSections;
begin
  CheckRender(' {{^boolean}}NO{{/boolean}}'+ LineEnding +' {{^boolean}}WAY{{/boolean}}'+ LineEnding +'','{ "boolean" : false }',' NO'+ LineEnding +' WAY'+ LineEnding +'')
end;

procedure TInvertedTests.TestStandaloneLines;
begin
  CheckRender('| This Is'+ LineEnding +'{{^boolean}}'+ LineEnding +'|'+ LineEnding +'{{/boolean}}'+ LineEnding +'| A Line'+ LineEnding +'','{ "boolean" : false }','| This Is'+ LineEnding +'|'+ LineEnding +'| A Line'+ LineEnding +'')
end;

procedure TInvertedTests.TestStandaloneIndentedLines;
begin
  CheckRender('| This Is'+ LineEnding +'  {{^boolean}}'+ LineEnding +'|'+ LineEnding +'  {{/boolean}}'+ LineEnding +'| A Line'+ LineEnding +'','{ "boolean" : false }','| This Is'+ LineEnding +'|'+ LineEnding +'| A Line'+ LineEnding +'')
end;

procedure TInvertedTests.TestStandaloneLineEndings;
begin
  CheckRender('|'+ LineEnding +'{{^boolean}}'+ LineEnding +'{{/boolean}}'+ LineEnding +'|','{ "boolean" : false }','|'+ LineEnding +'|')
end;

procedure TInvertedTests.TestStandaloneWithoutPreviousLine;
begin
  CheckRender('  {{^boolean}}'+ LineEnding +'^{{/boolean}}'+ LineEnding +'/','{ "boolean" : false }','^'+ LineEnding +'/')
end;

procedure TInvertedTests.TestStandaloneWithoutNewline;
begin
  CheckRender('^{{^boolean}}'+ LineEnding +'/'+ LineEnding +'  {{/boolean}}','{ "boolean" : false }','^'+ LineEnding +'/'+ LineEnding +'')
end;

procedure TInvertedTests.TestPadding;
begin
  CheckRender('|{{^ boolean }}={{/ boolean }}|','{ "boolean" : false }','|=|')
end;


procedure TPartialsTests.TestBasicBehavior;
begin
  CheckRender('"{{>text}}"','{}','"from partial"')
end;

procedure TPartialsTests.TestFailedLookup;
begin
  CheckRender('"{{>text}}"','{}','""')
end;

procedure TPartialsTests.TestContext;
begin
  CheckRender('"{{>partial}}"','{ "text" : "content" }','"*content*"')
end;

procedure TPartialsTests.TestRecursion;
begin
  CheckRender('{{>node}}','{ "content" : "X", "nodes" : [{ "content" : "Y", "nodes" : [] }] }','X<Y<>>')
end;

procedure TPartialsTests.TestSurroundingWhitespace;
begin
  CheckRender('| {{>partial}} |','{}','| 	|	 |')
end;

procedure TPartialsTests.TestInlineIndentation;
begin
  CheckRender('  {{data}}  {{> partial}}'+ LineEnding +'','{ "data" : "|" }','  |  >'+ LineEnding +'>'+ LineEnding +'')
end;

procedure TPartialsTests.TestStandaloneLineEndings;
begin
  CheckRender('|'+ LineEnding +'{{>partial}}'+ LineEnding +'|','{}','|'+ LineEnding +'>|')
end;

procedure TPartialsTests.TestStandaloneWithoutPreviousLine;
begin
  CheckRender('  {{>partial}}'+ LineEnding +'>','{}','  >'+ LineEnding +'  >>')
end;

procedure TPartialsTests.TestStandaloneWithoutNewline;
begin
  CheckRender('>'+ LineEnding +'  {{>partial}}','{}','>'+ LineEnding +'  >'+ LineEnding +'  >')
end;

procedure TPartialsTests.TestStandaloneIndentation;
begin
  CheckRender('\'+ LineEnding +' {{>partial}}'+ LineEnding +'/'+ LineEnding +'','{ "content" : "<\n->" }','\'+ LineEnding +' |'+ LineEnding +' <'+ LineEnding +'->'+ LineEnding +' |'+ LineEnding +'/'+ LineEnding +'')
end;

procedure TPartialsTests.TestPaddingWhitespace;
begin
  CheckRender('|{{> partial }}|','{ "boolean" : true }','|[]|')
end;


procedure TSectionsTests.TestTruthy;
begin
  CheckRender('"{{#boolean}}This should be rendered.{{/boolean}}"','{ "boolean" : true }','"This should be rendered."')
end;

procedure TSectionsTests.TestFalsey;
begin
  CheckRender('"{{#boolean}}This should not be rendered.{{/boolean}}"','{ "boolean" : false }','""')
end;

procedure TSectionsTests.TestContext;
begin
  CheckRender('"{{#context}}Hi {{name}}.{{/context}}"','{ "context" : { "name" : "Joe" } }','"Hi Joe."')
end;

procedure TSectionsTests.TestDeeplyNestedContexts;
begin
  CheckRender('{{#a}}'+ LineEnding +'{{one}}'+ LineEnding +'{{#b}}'+ LineEnding +'{{one}}{{two}}{{one}}'+ LineEnding +'{{#c}}'+ LineEnding +'{{one}}{{two}}{{three}}{{two}}{{one}}'+ LineEnding +'{{#d}}'+ LineEnding +'{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}'+ LineEnding +'{{#e}}'+ LineEnding +'{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}'+ LineEnding +'{{/e}}'+ LineEnding +'{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}'+ LineEnding +'{{/d}}'+ LineEnding +'{{one}}{{two}}{{three}}{{two}}{{one}}'+ LineEnding +'{{/c}}'+ LineEnding +'{{one}}{{two}}{{one}}'+ LineEnding +'{{/b}}'+ LineEnding +'{{one}}'+ LineEnding +'{{/a}}'+ LineEnding +'','{ "a" : { "one" : 1 }, "b" : { "two" : 2 }, "c" : { "three" : 3 }, "d" : { "four" : 4 }, "e" : { "five" : 5 } }','1'+ LineEnding +'121'+ LineEnding +'12321'+ LineEnding +'1234321'+ LineEnding +'123454321'+ LineEnding +'1234321'+ LineEnding +'12321'+ LineEnding +'121'+ LineEnding +'1'+ LineEnding +'')
end;

procedure TSectionsTests.TestList;
begin
  CheckRender('"{{#list}}{{item}}{{/list}}"','{ "list" : [{ "item" : 1 }, { "item" : 2 }, { "item" : 3 }] }','"123"')
end;

procedure TSectionsTests.TestEmptyList;
begin
  CheckRender('"{{#list}}Yay lists!{{/list}}"','{ "list" : [] }','""')
end;

procedure TSectionsTests.TestDoubled;
begin
  CheckRender('{{#bool}}'+ LineEnding +'* first'+ LineEnding +'{{/bool}}'+ LineEnding +'* {{two}}'+ LineEnding +'{{#bool}}'+ LineEnding +'* third'+ LineEnding +'{{/bool}}'+ LineEnding +'','{ "two" : "second", "bool" : true }','* first'+ LineEnding +'* second'+ LineEnding +'* third'+ LineEnding +'')
end;

procedure TSectionsTests.TestNestedTruthy;
begin
  CheckRender('| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |','{ "bool" : true }','| A B C D E |')
end;

procedure TSectionsTests.TestNestedFalsey;
begin
  CheckRender('| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |','{ "bool" : false }','| A  E |')
end;

procedure TSectionsTests.TestContextMisses;
begin
  CheckRender('[{{#missing}}Found key ''missing''!{{/missing}}]','{}','[]')
end;

procedure TSectionsTests.TestImplicitIteratorString;
begin
  CheckRender('"{{#list}}({{.}}){{/list}}"','{ "list" : ["a", "b", "c", "d", "e"] }','"(a)(b)(c)(d)(e)"')
end;

procedure TSectionsTests.TestImplicitIteratorInteger;
begin
  CheckRender('"{{#list}}({{.}}){{/list}}"','{ "list" : [1, 2, 3, 4, 5] }','"(1)(2)(3)(4)(5)"')
end;

procedure TSectionsTests.TestImplicitIteratorDecimal;
begin
  CheckRender('"{{#list}}({{.}}){{/list}}"','{ "list" : [1.10000000000000E+000, 2.20000000000000E+000, 3.30000000000000E+000, 4.40000000000000E+000, 5.50000000000000E+000] }','"(1.1)(2.2)(3.3)(4.4)(5.5)"')
end;

procedure TSectionsTests.TestDottedNamesTruthy;
begin
  CheckRender('"{{#a.b.c}}Here{{/a.b.c}}" == "Here"','{ "a" : { "b" : { "c" : true } } }','"Here" == "Here"')
end;

procedure TSectionsTests.TestDottedNamesFalsey;
begin
  CheckRender('"{{#a.b.c}}Here{{/a.b.c}}" == ""','{ "a" : { "b" : { "c" : false } } }','"" == ""')
end;

procedure TSectionsTests.TestDottedNamesBrokenChains;
begin
  CheckRender('"{{#a.b.c}}Here{{/a.b.c}}" == ""','{ "a" : {} }','"" == ""')
end;

procedure TSectionsTests.TestSurroundingWhitespace;
begin
  CheckRender(' | {{#boolean}}	|	{{/boolean}} | '+ LineEnding +'','{ "boolean" : true }',' | 	|	 | '+ LineEnding +'')
end;

procedure TSectionsTests.TestInternalWhitespace;
begin
  CheckRender(' | {{#boolean}} {{! Important Whitespace }}'+ LineEnding +' {{/boolean}} | '+ LineEnding +'','{ "boolean" : true }',' |  '+ LineEnding +'  | '+ LineEnding +'')
end;

procedure TSectionsTests.TestIndentedInlineSections;
begin
  CheckRender(' {{#boolean}}YES{{/boolean}}'+ LineEnding +' {{#boolean}}GOOD{{/boolean}}'+ LineEnding +'','{ "boolean" : true }',' YES'+ LineEnding +' GOOD'+ LineEnding +'')
end;

procedure TSectionsTests.TestStandaloneLines;
begin
  CheckRender('| This Is'+ LineEnding +'{{#boolean}}'+ LineEnding +'|'+ LineEnding +'{{/boolean}}'+ LineEnding +'| A Line'+ LineEnding +'','{ "boolean" : true }','| This Is'+ LineEnding +'|'+ LineEnding +'| A Line'+ LineEnding +'')
end;

procedure TSectionsTests.TestIndentedStandaloneLines;
begin
  CheckRender('| This Is'+ LineEnding +'  {{#boolean}}'+ LineEnding +'|'+ LineEnding +'  {{/boolean}}'+ LineEnding +'| A Line'+ LineEnding +'','{ "boolean" : true }','| This Is'+ LineEnding +'|'+ LineEnding +'| A Line'+ LineEnding +'')
end;

procedure TSectionsTests.TestStandaloneLineEndings;
begin
  CheckRender('|'+ LineEnding +'{{#boolean}}'+ LineEnding +'{{/boolean}}'+ LineEnding +'|','{ "boolean" : true }','|'+ LineEnding +'|')
end;

procedure TSectionsTests.TestStandaloneWithoutPreviousLine;
begin
  CheckRender('  {{#boolean}}'+ LineEnding +'#{{/boolean}}'+ LineEnding +'/','{ "boolean" : true }','#'+ LineEnding +'/')
end;

procedure TSectionsTests.TestStandaloneWithoutNewline;
begin
  CheckRender('#{{#boolean}}'+ LineEnding +'/'+ LineEnding +'  {{/boolean}}','{ "boolean" : true }','#'+ LineEnding +'/'+ LineEnding +'')
end;

procedure TSectionsTests.TestPadding;
begin
  CheckRender('|{{# boolean }}={{/ boolean }}|','{ "boolean" : true }','|=|')
end;


procedure TLambdasTests.TestInterpolation;
begin
  CheckRender('Hello, {{lambda}}!','{ "lambda" : { "php" : "return \"world\";", "clojure" : "(fn [] \"world\")", "__tag__" : "code", "perl" : "sub { \"world\" }", "python" : "lambda: \"world\"", "ruby" : "proc { \"world\" }", "js" : "function() { return \"world\" }" } }','Hello, world!')
end;

procedure TLambdasTests.TestInterpolationExpansion;
begin
  CheckRender('Hello, {{lambda}}!','{ "planet" : "world", "lambda" : { "php" : "return \"{{planet}}\";", "clojure" : "(fn [] \"{{planet}}\")", "__tag__" : "code", "perl" : "sub { \"{{planet}}\" }", "python" : "lambda: \"{{planet}}\"", "ruby" : "proc { \"{{planet}}\" }", "js" : "function() { return \"{{planet}}\" }" } }','Hello, world!')
end;

procedure TLambdasTests.TestInterpolationAlternateDelimiters;
begin
  CheckRender('{{= | | =}}'+ LineEnding +'Hello, (|&lambda|)!','{ "planet" : "world", "lambda" : { "php" : "return \"|planet| => {{planet}}\";", "clojure" : "(fn [] \"|planet| => {{planet}}\")", "__tag__" : "code", "perl" : "sub { \"|planet| => {{planet}}\" }", "python" : "lambda: \"|planet| => {{planet}}\"", "ruby" : "proc { \"|planet| => {{planet}}\" }", "js" : "function() { return \"|planet| => {{planet}}\" }" } }','Hello, (|planet| => world)!')
end;

procedure TLambdasTests.TestInterpolationMultipleCalls;
begin
  CheckRender('{{lambda}} == {{{lambda}}} == {{lambda}}','{ "lambda" : { "php" : "global $calls; return ++$calls;", "clojure" : "(def g (atom 0)) (fn [] (swap! g inc))", "__tag__" : "code", "perl" : "sub { no strict; $calls += 1 }", "python" : "lambda: globals().update(calls=globals().get(\"calls\",0)+1) or calls", "ruby" : "proc { $calls ||= 0; $calls += 1 }", "js" : "function() { return (g=(function(){return this})()).calls=(g.calls||0)+1 }" } }','1 == 2 == 3')
end;

procedure TLambdasTests.TestEscaping;
begin
  CheckRender('<{{lambda}}{{{lambda}}}','{ "lambda" : { "php" : "return \">\";", "clojure" : "(fn [] \">\")", "__tag__" : "code", "perl" : "sub { \">\" }", "python" : "lambda: \">\"", "ruby" : "proc { \">\" }", "js" : "function() { return \">\" }" } }','<&gt;>')
end;

procedure TLambdasTests.TestSection;
begin
  CheckRender('<{{#lambda}}{{x}}{{/lambda}}>','{ "x" : "Error!", "lambda" : { "php" : "return ($text == \"{{x}}\") ? \"yes\" : \"no\";", "clojure" : "(fn [text] (if (= text \"{{x}}\") \"yes\" \"no\"))", "__tag__" : "code", "perl" : "sub { $_[0] eq \"{{x}}\" ? \"yes\" : \"no\" }", "python" : "lambda text: text == \"{{x}}\" and \"yes\" or \"no\"", "ruby" : "proc { |text| text == \"{{x}}\" ? \"yes\" : \"no\" }", "js" : "function(txt) { return (txt == \"{{x}}\" ? \"yes\" : \"no\") }" } }','<yes>')
end;

procedure TLambdasTests.TestSectionExpansion;
begin
  CheckRender('<{{#lambda}}-{{/lambda}}>','{ "planet" : "Earth", "lambda" : { "php" : "return $text . \"{{planet}}\" . $text;", "clojure" : "(fn [text] (str text \"{{planet}}\" text))", "__tag__" : "code", "perl" : "sub { $_[0] . \"{{planet}}\" . $_[0] }", "python" : "lambda text: \"%s{{planet}}%s\" % (text, text)", "ruby" : "proc { |text| \"#{text}{{planet}}#{text}\" }", "js" : "function(txt) { return txt + \"{{planet}}\" + txt }" } }','<-Earth->')
end;

procedure TLambdasTests.TestSectionAlternateDelimiters;
begin
  CheckRender('{{= | | =}}<|#lambda|-|/lambda|>','{ "planet" : "Earth", "lambda" : { "php" : "return $text . \"{{planet}} => |planet|\" . $text;", "clojure" : "(fn [text] (str text \"{{planet}} => |planet|\" text))", "__tag__" : "code", "perl" : "sub { $_[0] . \"{{planet}} => |planet|\" . $_[0] }", "python" : "lambda text: \"%s{{planet}} => |planet|%s\" % (text, text)", "ruby" : "proc { |text| \"#{text}{{planet}} => |planet|#{text}\" }", "js" : "function(txt) { return txt + \"{{planet}} => |planet|\" + txt }" } }','<-{{planet}} => Earth->')
end;

procedure TLambdasTests.TestSectionMultipleCalls;
begin
  CheckRender('{{#lambda}}FILE{{/lambda}} != {{#lambda}}LINE{{/lambda}}','{ "lambda" : { "php" : "return \"__\" . $text . \"__\";", "clojure" : "(fn [text] (str \"__\" text \"__\"))", "__tag__" : "code", "perl" : "sub { \"__\" . $_[0] . \"__\" }", "python" : "lambda text: \"__%s__\" % (text)", "ruby" : "proc { |text| \"__#{text}__\" }", "js" : "function(txt) { return \"__\" + txt + \"__\" }" } }','__FILE__ != __LINE__')
end;

procedure TLambdasTests.TestInvertedSection;
begin
  CheckRender('<{{^lambda}}{{static}}{{/lambda}}>','{ "static" : "static", "lambda" : { "php" : "return false;", "clojure" : "(fn [text] false)", "__tag__" : "code", "perl" : "sub { 0 }", "python" : "lambda text: 0", "ruby" : "proc { |text| false }", "js" : "function(txt) { return false }" } }','<>')
end;

initialization
  RegisterTest('Mustache', TCommentsTests.Suite);
  RegisterTest('Mustache', TDelimitersTests.Suite);
  RegisterTest('Mustache', TInterpolationTests.Suite);
  RegisterTest('Mustache', TInvertedTests.Suite);
  RegisterTest('Mustache', TPartialsTests.Suite);
  RegisterTest('Mustache', TSectionsTests.Suite);
  RegisterTest('Mustache', TLambdasTests.Suite);

end.
