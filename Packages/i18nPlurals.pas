{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implememnts classes and functions to determine how languages
/// handle plurals of nouns or unit expressions.
unit i18nPlurals;

{$I DELPHIAREA.INC}

interface

uses
  Windows, SysUtils, Classes, i18nHashList, i18nCExpr;

type

  {$region 'xmldoc'}
  /// <summary>
  /// EPluralFormsError is the exception class for problems that occur while using
  /// an instance of <see cref="TPluralForms"/> class.</summary>
  /// <remarks>
  /// EPluralFormsError represents exceptions that occur when trying to use a plural
  /// rule. These exceptions include:
  ///
  /// <list type="bullet">
  ///   <item>Attempts to use a plural rule that is not in a correct syntax.</item>
  ///   <item>Specifying an invalid value for the number of plurals.</item>
  ///   <item>Using a variable other than <c>n</c> in a formula of the plural rule.</item>
  /// </list>
  ///
  /// </remarks>
  /// <seealso cref="TPluralForms"/>
  {$endregion}
  EPluralFormsError = class(Exception);

  {$region 'xmldoc'}
  /// <summary>
  /// TPluralForms manages the way a language handles plurals of nouns or unit
  /// expressions.</summary>
  /// <remarks>
  /// Plural forms are grammatical variants depending on a number. Some languages
  /// have only one form. Some other have two forms, called singular and plural.
  /// Other languages have three forms, called singular, dual and plural. There
  /// are also languages with up to six forms.
  ///
  /// Use TPluralForms to select the proper plural form from a list of plural forms
  /// that a langauge may have. The selection is handled by the rules of the language
  /// and a specified number.
  ///
  /// TPluralForms not only has a method to select the right plural form but also has
  /// properties and methods to specify the rules of the language to handle the selection.
  ///
  /// TPluralForms privides two ways to define the language's rules for selecting a plural
  /// form:
  ///
  /// <list type="bullet">
  ///   <item>
  ///   Designating either the locale or the international language code of the target
  ///   language. TPluralForms knows the plural rules of the most languages.</item>
  ///   <item>
  ///   Specifying the number of plural forms for the target language, and supplying a
  ///   formula to dictate its rules.</item>
  /// </list>
  ///
  /// </remarks>
  {$endregion}
  TPluralForms = class(TObject)
  private
    fNumOfPlurals: Integer;
    fFormula: TCExpression;
    Cache: TKeyLookup<Integer,Integer>;
    function GetRule: String;
    procedure SetRule(const Value: String);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.</summary>
    /// <param name="ARule">
    /// The plural rule to initialie the instance.</param>
    /// <seealso cref="Rule"/>
    {$endregion}
    constructor Create(const ARule: string);
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance of the class and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the information about the plural form selection of a specified language.</summary>
    /// <param name="Locale">
    /// The locale or the two-character international language code of the target language.</param>
    /// <param name="DefaultRule">
    /// The plural rule that will be returned if the rule of the specified locale/language
    /// is unknown.</param>
    /// <returns>
    /// If the plural rule of the language given by <paramref name="Locale"/> is known,
    /// returns it. Otherwise, returns the rule given by <paramref name="DefaultRule"/>.</returns>
    /// <seealso cref="Rule"/>
    {$endregion}
    class function RuleOf(const Locale: String; const DefaultRule: String = ''): String; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Parses a specified plural rule and extracts the number of plural forms plus
    /// the furmula for selecting a plural form.</summary>
    /// <param name="ARule">
    /// The plural rule to parse.</param>
    /// <param name="ANumOfPlurals">
    /// Returns the number plural forms.</param>
    /// <param name="AFormula">
    /// Returns the formula for selecting a plural form as an expression in C language
    /// syntax.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the rule is parsed successfully. Otherwise,
    /// returns <see langword="false"/>.</returns>
    /// <seealso cref="Apply"/>
    /// <seealso cref="Rule"/>
    {$endregion}
    class function ParseRule(const ARule: String; out ANumOfPlurals: Integer; out AFormula: String): Boolean; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets a custom rule for selecting the plural forms by specifying the number
    /// of plural forms in a target language and providing a formula that evaluates
    /// index of the proper plural from based on variable <c>n</c>.</summary>
    /// <param name="ANumOfPlurals">
    /// The number of plural forms for the target language.</param>
    /// <param name="AFormula">
    /// The expression in C language syntax that returns index of the right plural
    /// form based on variable <c>n</c>. See <see cref="Rule"/> property for more
    /// information.</param>
    /// <exception cref="EPluralFormsError">
    /// Occurs when value of either of the parameters is not acceptable.</exception>
    /// <exception cref="ECExpressionError">
    /// Occurs when the expression specified by <paramref name="AFormula"/> has
    /// a syntax error.</exception>
    /// <seealso cref="ParseRule"/>
    /// <seealso cref="Rule"/>
    {$endregion}
    procedure Apply(ANumOfPlurals: Integer; const AFormula: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Extracts the number of plurals from a specified plural rule.</summary>
    /// <param name="ARule">
    /// The plural rule to extract the number of plurals from it.</param>
    /// <returns>
    /// Returns the number of plurals indicated by the specified plural rule.</returns>
    /// <exception cref="EPluralFormsError">
    /// Occurs when the rule is not in correct syntax.</exception>
    {$endregion}
    class function ExtractNumOfPlurals(const ARule: String): Integer; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds the index of the right plural form for expressing a specified number
    /// of nouns or unuts.</summary>
    /// <param name="N">
    /// The number of nouns or units to express.</param>
    /// <returns>
    /// The zero-based index of the proper plural form in a list of plural forms.</returns>
    {$endregion}
    function IndexOf(N: Integer): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the plural rule of the target language as a string.</summary>
    /// <remarks>
    /// The Rule property provides the information about the plural form selection. For
    /// example, the plural rule for English language looks like this:
    /// <code>
    /// nplurals=2; plural=(n == 1) ? 0 : 1;
    /// </code>
    /// The <c>nplurals</c> value must be a decimal number which specifies how many
    /// different plural forms exist for this language. The string following <c>plural</c>
    /// is an expression which is using the C language syntax. Exceptions are that numbers
    /// must be decimal, and the only variable allowed is <c>n</c>. Spaces are allowed in
    /// the expression. This expression will be evaluated whenever <see cref="IndexOf"/>
    /// method is called. The absolute of numeric value passed to this method is then
    /// assigned to the variable <c>n</c> in the expression. The resulting value then
    /// must be greater than or equal to zero and smaller than the value given as the
    /// value of <c>nplurals</c>.</remarks>
    /// <exception cref="EPluralFormsError">
    /// Occurs when the rule is not in correct syntax or indicates an unacceptable value
    /// or expression.</exception>
    /// <exception cref="ECExpressionError">
    /// Occurs when the expression part of the rule has a syntax error.</exception>
    /// <seealso cref="NumOfPlurals"/>
    /// <seealso cref="TCExpression"/>
    {$endregion}
    property Rule: String read GetRule write SetRule;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of plural forms for the current target language.</summary>
    /// <seealso cref="Rule"/>
    {$endregion}
    property NumOfPlurals: Integer read fNumOfPlurals;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TCExpression"/> object that evaluates the plural rules
    /// for the current target language.</summary>
    /// <seealso cref="Rule"/>
    /// <seealso cref="TCExpression"/>
    {$endregion}
    property Formula: TCExpression read fFormula;
  end;

implementation

uses
  i18nCore, Types;

resourcestring
  SInvalidPluralRule = 'Invalid plural rule'#13#10'%s';
  SInvalidPluralNum  = 'The number of plurals (%d) must be an integer value greater that or equal to one';
  SInvalidPluralVar  = 'The only allowed variable in a plural formula is ''n''';

type
  TLocalePluralRuleTableEntry = record
    Locale: String;
    PluralRule: String;
  end;

const
  // Sorted list of Locale/Language -> Prural Rule
  LocalePluralRuleTable: array[1..125] of TLocalePluralRuleTableEntry = (
    (Locale: 'ach';   PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'af';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ak';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'am';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'an';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ar';    PluralRule: 'nplurals=6; plural=n==0 ? 0 : n==1 ? 1 : n==2 ? 2 : n%100>=3 && n%100<=10 ? 3 : n%100>=11 ? 4 : 5;'),
    (Locale: 'arn';   PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'ast';   PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ay';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'az';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'be';    PluralRule: 'nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;'),
    (Locale: 'bg';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'bn';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'bo';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'br';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'bs';    PluralRule: 'nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;'),
    (Locale: 'ca';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'cgg';   PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'cs';    PluralRule: 'nplurals=3; plural=n==1 ? 0 : n>=2 && n<=4 ? 1 : 2;'),
    (Locale: 'csb';   PluralRule: 'nplurals=3; plural=n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;'),
    (Locale: 'cy';    PluralRule: 'nplurals=4; plural=n==1 ? 0 : n==2 ? 1 : n!=8 && n!=11 ? 2 : 3;'),
    (Locale: 'da';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'de';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'dz';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'el';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'en';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'eo';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'es';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'et';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'eu';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'fa';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'fi';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'fil';   PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'fo';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'fr';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'fur';   PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'fy';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ga';    PluralRule: 'nplurals=5; plural=n==1 ? 0 : n==2 ? 1 : n<7 ? 2 : n<11 ? 3 : 4;'),
    (Locale: 'gd';    PluralRule: 'nplurals=4; plural=n==1 || n==11 ? 0 : n==2 || n==12 ? 1 : n>2 && n<20 ? 2 : 3;'),
    (Locale: 'gl';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'gu';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'gun';   PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'ha';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'he';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'hi';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'hy';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'hr';    PluralRule: 'nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;'),
    (Locale: 'hu';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ia';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'id';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'is';    PluralRule: 'nplurals=2; plural=n%10!=1 || n%100==11;'),
    (Locale: 'it';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ja';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'jv';    PluralRule: 'nplurals=2; plural=n!=0;'),
    (Locale: 'ka';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'kk';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'km';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'kn';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ko';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'ku';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'kw';    PluralRule: 'nplurals=4; plural=n==1 ? 0 : n==2 ? 1 : n==3 ? 2 : 3;'),
    (Locale: 'ky';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'lb';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ln';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'lo';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'lt';    PluralRule: 'nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && (n%100<10 || n%100>=20) ? 1 : 2;'),
    (Locale: 'lv';    PluralRule: 'nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n!=0 ? 1 : 2;'),
    (Locale: 'mai';   PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'mfe';   PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'mg';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'mi';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'mk';    PluralRule: 'nplurals=2; plural=n==1 || n%10==1 ? 0 : 1;'),
    (Locale: 'ml';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'mn';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'mr';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ms';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'mt';    PluralRule: 'nplurals=4; plural=n==1 ? 0 : n==0 || (n%100>1 && n%100<11) ? 1 : n%100>10 && n%100<20 ? 2 : 3;'),
    (Locale: 'nah';   PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'nap';   PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'nb';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ne';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'nl';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'se';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'nn';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'no';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'nso';   PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'oc';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'or';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ps';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'pa';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'pap';   PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'pl';    PluralRule: 'nplurals=3; plural=n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;'),
    (Locale: 'pms';   PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'pt';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'pt-BR'; PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'rm';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ro';    PluralRule: 'nplurals=3; plural=n==1 ? 0 : n==0 || (n%100 > 0 && n%100 < 20) ? 1 : 2;'),
    (Locale: 'ru';    PluralRule: 'nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;'),
    (Locale: 'sco';   PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'si';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'sk';    PluralRule: 'nplurals=3; plural=n==1 ? 0 : n>=2 && n<=4 ? 1 : 2;'),
    (Locale: 'sl';    PluralRule: 'nplurals=4; plural=n%100==1 ? 1 : n%100==2 ? 2 : n%100==3 || n%100==4 ? 3 : 0;'),
    (Locale: 'so';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'son';   PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'sq';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'sr';    PluralRule: 'nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;'),
    (Locale: 'su';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'sw';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'sv';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'ta';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'te';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'tg';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'ti';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'th';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'tk';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'tr';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'tt';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'ug';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'uk';    PluralRule: 'nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;'),
    (Locale: 'ur';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'uz';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'vi';    PluralRule: 'nplurals=1; plural=0;'),
    (Locale: 'wa';    PluralRule: 'nplurals=2; plural=n>1;'),
    (Locale: 'yo';    PluralRule: 'nplurals=2; plural=n!=1;'),
    (Locale: 'zh';    PluralRule: 'nplurals=1; plural=0;'));

{ TPluralForms }

constructor TPluralForms.Create(const ARule: String);
begin
  Cache := TKeyLookup<Integer,Integer>.Create(32);
  SetRule(ARule);
end;

destructor TPluralForms.Destroy;
begin
  Cache.Free;
  if Assigned(fFormula) then
    fFormula.Free;
  inherited Destroy;
end;

function TPluralForms.GetRule: String;
begin
  Result := Format('nplurals=%d; plural=%s;', [NumOfPlurals, Formula.Source]);
end;

procedure TPluralForms.SetRule(const Value: String);
var
  LNumOfPlurals: Integer;
  LFormula: String;
begin
  if not ParseRule(Value, LNumOfPlurals, LFormula) then
    raise EPluralFormsError.CreateResFmt(@SInvalidPluralRule, [Value]);
  Apply(LNumOfPlurals, LFormula);
end;

class function TPluralForms.RuleOf(const Locale: String; const DefaultRule: String): String;
var
  L, H, M: Integer;
  C: Integer;
begin
  Result := DefaultRule;
  L := Low(LocalePluralRuleTable);
  H := High(LocalePluralRuleTable);
  repeat
    M := (L + H) shr 1;
    C := CompareText(LocalePluralRuleTable[M].Locale, Locale);
    if C < 0 then
      L := M + 1
    else if C > 0 then
      H := M - 1
    else
    begin
      Result := LocalePluralRuleTable[M].PluralRule;
      Exit;
    end;
  until L > H;
  if SameLanguage(LocalePluralRuleTable[M].Locale, Locale) then
    Result := LocalePluralRuleTable[M].PluralRule
  else if M > Low(LocalePluralRuleTable) then
  begin
    Dec(M);
    if SameLanguage(LocalePluralRuleTable[M].Locale, Locale) then
      Result := LocalePluralRuleTable[M].PluralRule;
  end
end;

class function TPluralForms.ExtractNumOfPlurals(const ARule: String): Integer;
var
  NotUsed: String;
begin
  if not ParseRule(ARule, Result, NotUsed) then
    raise EPluralFormsError.CreateResFmt(@SInvalidPluralRule, [ARule]);
end;

class function TPluralForms.ParseRule(const ARule: String;
  out ANumOfPlurals: Integer; out AFormula: String): Boolean;
const
  Pattern = 'nplurals=#; plural=*;';
var
  P, S, E: PChar;
begin
  Result := False;
  ANumOfPlurals := 0;
  AFormula := '';
  P := PChar(Pattern);
  S := PChar(ARule);
  while CharInSet(S^, [' ', ^I]) do
    Inc(S);
  while S^ = P^ do
  begin
    Inc(S);
    Inc(P);
  end;
  while CharInSet(S^, [' ', ^I]) do
    Inc(S);
  if (S^ = '=') and (P^ = '=') then
  begin
    Inc(S);
    while CharInSet(S^, [' ', ^I]) do
      Inc(S);
    Inc(P);
  end;
  if P^ <> '#' then
    Exit;
  Inc(P);
  if not CharInSet(S^, ['0'..'9']) then
    Exit;
  repeat
    ANumOfPlurals := ANumOfPlurals * 10 + Ord(S^) - Ord('0');
    Inc(S);
  until not CharInSet(S^, ['0'..'9']);
  while CharInSet(S^, [' ', ^I]) do
    Inc(S);
  if (S^ = ';') and (P^ = ';') then
  begin
    Inc(S);
    while CharInSet(S^, [' ', ^I]) do
      Inc(S);
    Inc(P, 2);
  end;
  while S^ = P^ do
  begin
    Inc(S);
    Inc(P);
  end;
  while CharInSet(S^, [' ', ^I]) do
    Inc(S);
  if (S^ = '=') and (P^ = '=') then
  begin
    Inc(S);
    while CharInSet(S^, [' ', ^I]) do
      Inc(S);
    Inc(P);
  end;
  if P^ <> '*' then
    Exit;
  E := StrScan(S, ';');
  if E <> nil then
  begin
    repeat
      Dec(E);
    until (S < E) or not CharInSet(E^, [' ', ^I]);
    SetString(AFormula, S, E - S + 1)
  end
  else
    AFormula := Trim(S);
  Result := (AFormula <> '');
end;

procedure TPluralForms.Apply(ANumOfPlurals: Integer; const AFormula: String);
var
  NewFormula: TCExpression;
begin
  if (NumOfPlurals = ANumOfPlurals) and Assigned(Formula) and (Formula.Source = AFormula) then
    Exit;
  if ANumOfPlurals < 1 then
    raise EPluralFormsError.CreateResFmt(@SInvalidPluralNum, [ANumOfPlurals]);
  NewFormula := TCExpression.Create(AFormula);
  if (NewFormula.VarCount > 1) or ((NewFormula.VarCount = 1) and (NewFormula.Vars['n'].RefCount = 0)) then
  begin
    NewFormula.Free;
    raise EPluralFormsError.CreateRes(@SInvalidPluralVar);
  end;
  if Assigned(fFormula) then
    fFormula.Free;
  fFormula := NewFormula;
  fNumOfPlurals := ANumOfPlurals;
  Cache.Clear;
end;

function TPluralForms.IndexOf(N: Integer): Integer;
begin
  if N < 0 then
    N := -N;
  if not Cache.Retrieve(N, Result) then
  begin
    Formula.Vars['n'].Value := N;
    Result := Formula.Evaluate;
    Cache.Add(N, Result);
  end;
end;

end.
