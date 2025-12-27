{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  Internationalization and Localization for Delphi                            }
{                                                                              }
{  Copyright (c) Kambiz Khojasteh                                              }
{  https://github.com/khojasteh/i18n                                           }
{                                                                              }
{------------------------------------------------------------------------------}

/// <summary>
/// This unit implements some utility functions.
/// </summary>
unit i18nUtils;

{$I DELPHIAREA.INC}

interface

uses
  Windows, SysUtils, Classes, Variants, i18nCore, i18nUnicode;

{$region 'xmldoc'}
/// <summary>
/// Extracts the component name from a specified property path.
/// </summary>
/// <param name="PropertyPath">
/// The full path of the property.
/// </param>
/// <returns>
/// The name of component that owns the property.
/// </returns>
{$endregion}
function ExtractComponentName(const PropertyPath: String): String;

{$region 'xmldoc'}
/// <summary>
/// Expands environment-variable strings and replaces them with their defined
/// values.
/// </summary>
/// <param name="Str">
/// The string that might contain references to environment-variable strings of
/// the form: <c>%variableName%</c>.</param>
/// <returns>
/// The string with its environment variables replaced with the current
/// values of those environment variables.
/// </returns>
{$endregion}
function ExpandEnvStr(const Str: String): String;

{$region 'xmldoc'}
/// <summary>
/// Escapes all whitespace characters, except blank, in a specified string
/// using backslash escapes.
/// </summary>
/// <param name="Str">
/// The string to be escaped.
/// </param>
/// <returns>
/// The escaped string.
/// </returns>
/// <seealso cref="UnescapeString"/>
{$endregion}
function EscapeString(const Str: String): String;

{$region 'xmldoc'}
/// <summary>
/// Unescapes a specified string that was previously escaped using
/// <see cref="EscapeString"/> function.
/// </summary>
/// <param name="Str">
/// The source string.
/// </param>
/// <returns>
/// The string as normal.
/// </returns>
/// <exception cref="EConvertError">
/// Occurs if the source string contains invalid escaped characters.
/// </exception>
/// <seealso cref="EscapeString"/>
{$endregion}
function UnescapeString(const Str: String): String;

{$region 'xmldoc'}
/// <summary>
/// Calculates Damerau-Levenshtein distance between two specified strings.
/// </summary>
/// <remarks>
/// <para>
/// The Damerau-Levenshtein distance (named after Frederick J. Damerau and Vladimir I.
/// Levenshtein) is a "distance" (string metric) between two strings, i.e., finite
/// sequence of symbols, given by counting the minimum number of operations needed
/// to transform one string into the other, where an operation is defined as an
/// insertion, deletion, or substitution of a single character, or a transposition
/// of two adjacent characters.
/// </para>
/// </remarks>
/// <param name="Str1">
/// The first string.
/// </param>
/// <param name="Str2">
/// The second string.
/// </param>
/// <returns>
/// The Damerau-Levenshtein distance.
/// </returns>
/// <seealso cref="StringSimilarityRatio"/>
/// <seealso cref="TextSimilarityRatio"/>
{$endregion}
function DamerauLevenshteinDistance(const Str1, Str2: String): Integer;

{$region 'xmldoc'}
/// <summary>
/// Calculates the ratio that determines how much two specified strings are
/// similar. The comparison is case sensitive.
/// </summary>
/// <param name="Str1">
/// The first string.
/// </param>
/// <param name="Str2">
/// The second string.
/// </param>
/// <returns>
/// The similarity ratio in range 0 to 1, where 0 means not similar at all and
/// 1 means identical.
/// </returns>
/// <seealso cref="DamerauLevenshteinDistance"/>
/// <seealso cref="TextSimilarityRatio"/>
{$endregion}
function StringSimilarityRatio(const Str1, Str2: String): Double;

{$region 'xmldoc'}
/// <summary>
/// Calculates the ratio that determines how much two specified strings are
/// similar. The comparison is case insensitive.
/// </summary>
/// <param name="Str1">
/// The first string.
/// </param>
/// <param name="Str2">
/// The second string.
/// </param>
/// <returns>
/// The similarity ratio in range 0 to 1, where 0 means not similar at all and
/// 1 means identical (ignoring case of letters).
/// </returns>
/// <seealso cref="DamerauLevenshteinDistance"/>
/// <seealso cref="StringSimilarityRatio"/>
{$endregion}
function TextSimilarityRatio(const Str1, Str2: String): Double;

{$region 'xmldoc'}
/// <summary>
/// Substitutes the native digits of a source language with native digits of
/// another language, in a specified string.
/// </summary>
/// <param name="Str">
/// The string to substitute its digits.
/// </param>
/// <param name="FromCulture">
/// The <see cref="TCultureInfo"/> object that specifies the source language.
/// </param>
/// <param name="ToCulture">
/// The <see cref="TCultureInfo"/> object that specifies the target language.
/// </param>
/// <returns>
/// The string that its digits are substituted.
/// </returns>
{$endregion}
function TranslateDigits(const Str: String; FromCulture, ToCulture: TCultureInfo): String;

{$region 'xmldoc'}
/// <summary>
/// Converts a specified value using a format string.
/// </summary>
/// <remarks>
/// <para>
/// FormatValue formats the value given by <paramref name="Value"/> using the format
/// specifier given by <paramref name="Fmt"/>.
/// </para>
/// <para>
/// The format specifiers for datetime values and numeric values respectively correspond
/// to the Fmt parameters of <see cref="TCalendar.Format"/> and <see cref="TCultureInfo.FormatNumber"/>
/// methods. In addition, there are some extra format specifiers for numeric values:
/// </para>
///
/// <list type="table">
///   <listheader>
///     <term>Format Specifier</term>
///     <description>Name</description>
///     <description>Description</description>
///   </listheader>
///   <item>
///     <term>C or c</term>
///     <description>Currency</description>
///     <description>
///     <para>
///     The number is converted to a string that represents a currency amount. The conversion is
///     controlled by the currency format information of the <paramref name="Culture"/> parameter
///     or the current locale's settings.
///     </para>
///     <para>
///     The precision specifier indicates the desired number of decimal places. If the precision
///     specifier is omitted, the default currency precision given by the <paramref name="Culture"/>
///     parameter or the current locale's settings is used.
///     </para>
///     <example>
///     The following example formats a floating point value with the currency format specifier.
///     <code>
///     value := 12345.6789;
///     a := FormatValue('C', value);  // $12345.68
///     b := FormatValue('C3', value); // $12345.679
///     </code>
///     </example>
///     </description>
///   </item>
///   <item>
///     <term>D or d</term>
///     <description>Decimal</description>
///     <description>
///     <para>
///     This format is supported only for integral types. The number is converted to a string of
///     decimal digits (0-9), prefixed by a minus sign if the number is negative.
///     </para>
///     <para>
///     The precision specifier indicates the minimum number of digits desired in the resulting
///     string. If required, the number is padded with zeros to its left to produce the number
///     of digits given by the precision specifier.
///     </para>
///     <example>
///     The following example formats an integer value with the decimal format specifier.
///     <code>
///     value := 12345;
///     a := FormatValue('D', value);  // 12345
///     b := FormatValue('D8', value); // 00012345
///
///     value := -12345;
///     c := FormatValue('D', value);  // -12345
///     d := FormatValue('D8', value); // -00012345
///     </code>
///     </example>
///     </description>
///   </item>
///   <item>
///     <term>E or e</term>
///     <description>Scientific (Exponential)</description>
///     <description>
///     <para>
///     The number is converted to a string of the form "-d.ddd...E+ddd" or "-d.ddd...e+ddd",
///     where each 'd' indicates a digit (0-9). The string starts with a minus sign if the number
///     is negative. One digit always precedes the decimal point.
///     </para>
///     <para>
///     The precision specifier indicates the desired number of digits after the decimal point. If
///     the precision specifier is omitted, a default of six digits after the decimal point is used.
///     </para>
///     <para>
///     The case of the format specifier indicates whether to prefix the exponent with an 'E' or an
///     'e'. The exponent always consists of a plus or minus sign and a minimum of three digits. The
///     exponent is padded with zeros to meet this minimum, if required.
///     </para>
///     <example>
///     The following example formats a floating point value with the scientific format specifier.
///     <code>
///     value := 12345.6789;
///     a := FormatValue('E', value);   // 1.234568E+004
///     b := FormatValue('E10', value); // 1.2345678900E+004
///     c := FormatValue('e4', value);  // 1.2346e+004
///     </code>
///     </example>
///     </description>
///   </item>
///   <item>
///     <term>F or f</term>
///     <description>Fixed-point</description>
///     <description>
///     <para>
///     The number is converted to a string of the form "-ddd.ddd..." where each 'd' indicates a
///     digit (0-9). The string starts with a minus sign if the number is negative.
///     </para>
///     <para>
///     The precision specifier indicates the desired number of decimal places. If the precision
///     specifier is omitted, the default numeric precision is given by the <paramref name="Culture"/>
///     parameter or the current locale's settings.
///     </para>
///     <example>
///     The following example formats a floating point value with the fixed-point format specifier.
///     <code>
///     value := 12345.6789;
///     a := FormatValue('F', value);   // 12345.68
///     b := FormatValue('F3', value);  // 12345.679
///     c := FormatValue('F0', value);  // 12346
///     </code>
///     </example>
///     </description>
///   </item>
///   <item>
///     <term>G or g</term>
///     <description>General</description>
///     <description>
///     <para>
///     The number is converted to the most compact of either fixed-point or scientific notation,
///     depending on the type of the number and whether a precision specifier is present. If the
///     precision specifier is omitted or zero, the type of the number determines the default
///     precision.
///     </para>
///     <para>
///     Fixed-point notation is used if the exponent that would result from expressing the number
///     in scientific notation is greater than -5 and less than the precision specifier; otherwise,
///     scientific notation is used. The result contains a decimal point if required and trailing
///     zeroes are omitted. If the precision specifier is present and the number of significant
///     digits in the result exceeds the specified precision, then the excess trailing digits are
///     removed by rounding.
///     </para>
///     <para>
///     The exception to the preceding rule is if the number is a decimal and the precision specifier
///     is omitted. In that case, fixed-point notation is always used and trailing zeroes are preserved.
///     </para>
///     <para>
///     If scientific notation is used, the exponent in the result is prefixed with 'E' if the format
///     specifier is 'G', or 'e' if the format specifier is 'g'.
///     </para>
///     <example>
///     The following example formats assorted integer and floating point values with the general
///     format specifier.
///     <code>
///     value := 12345.6789;
///     a := FormatValue('G', value);   // 12345.6789
///     b := FormatValue('G7', value);  // 12345.68
///
///     value := 0.0000023;
///     c := FormatValue('G', value);   // 2.3E-006
///
///     value := 0.0023;
///     d := FormatValue('G', value);   // 0.0023
///
///     value := 1234;
///     e := FormatValue('G2', value);  // 1.2E003
///
///     value := PI;
///     f := FormatValue('G5', value);  // 3.1416
///     </code>
///     </example>
///     </description>
///   </item>
///   <item>
///     <term>N or n</term>
///     <description>Number</description>
///     <description>
///     <para>
///     The number is converted to a string of the form "-d,ddd,ddd.ddd...", where '-' indicates a negative
///     number symbol if required, 'd' indicates a digit (0-9), ',' indicates a thousand separator between
///     number groups, and '.' indicates a decimal point symbol. The actual negative number pattern, number
///     group size, thousand separator, and decimal separator are specified by the <paramref name="Culture"/>
///     parameter or the current locale's settings.
///     </para>
///     <para>
///     The precision specifier indicates the desired number of decimal places. If the precision specifier
///     is omitted, the default numeric precision is given by the <paramref name="Culture"/> parameter
///     or the current locale's settings.
///     </para>
///     <example>
///     The following example formats a floating point and an integer value with the number format specifier.
///     <code>
///     value := -12345.6789;
///     a := FormatValue('N', value);   // -12,345.68
///     b := FormatValue('N1', value);  // -12,345.7
///
///     value := 123456789;
///     c := FormatValue('N', value);   // 123,456,789.00
///     d := FormatValue('N0', value);  // 123,456,789
///     </code>
///     </example>
///     </description>
///   </item>
///   <item>
///     <term>P or p</term>
///     <description>Percent</description>
///     <description>
///     <para>
///     The number is converted to a string that represents a percent. The converted number is multiplied
///     by 100 in order to be presented as a percentage. The percent sign and its location is determined
///     by the <paramref name="Culture"/> parameter or the current locale's settings.
///     </para>
///     <para>
///     The precision specifier indicates the desired number of decimal places. If the precision specifier
///     is omitted, the default numeric precision given by the <paramref name="Culture"/> parameter or the
///     current locale's settings is used.
///     </para>
///     <example>
///     The following example formats a floating point value with the percent format specifier.
///     <code>
///     value := 0.2468013;
///     a := FormatValue('P', value);   // 24.68%
///     b := FormatValue('P1', value);  // 24.7%
///     </code>
///     </example>
///     </description>
///   </item>
///   <item>
///     <term>X or x</term>
///     <description>Hexadecimal</description>
///     <description>
///     <para>
///     This format is supported only for integer values. The number is converted to a string of hexadecimal
///     digits. The case of the format specifier indicates whether to use uppercase or lowercase characters
///     for the hexadecimal digits greater than 9. For example, use 'X' to produce "ABCDEF", and 'x' to produce
///     "abcdef".
///     </para>
///     <para>
///     The precision specifier indicates the minimum number of digits desired in the resulting string. If
///     required, the number is padded with zeros to its left to produce the number of digits given by the
///     precision specifier.
///     </para>
///     <example>
///     The following example formats various integer values with the hexadecimal format specifier.
///     <code>
///     value := $2045e;
///     a := FormatValue('X', value);    // 2045E
///     b := FormatValue('X8', value);   // 0002045E
///
///     value := 123456789;
///     c := FormatValue('X', value);    // 75BCD15
///     d := FormatValue('X2', value);   // 75BCD15
///     e := FormatValue('x10', value);  // 00075bcd15
///     </code>
///     </example>
///     </description>
///   </item>
///   <item>
///     <term>B or b</term>
///     <description>Binary</description>
///     <description>
///     <para>
///     This format is supported only for integer values. The number is converted to a string of binary
///     digits.
///     </para>
///     <para>
///     The precision specifier indicates the minimum number of digits desired in the resulting string.
///     If required, the number is padded with zeros to its left to produce the number of digits given
///     by the precision specifier.
///     </para>
///     <example>
///     The following example formats various integer values with the binary format specifier.
///     <code>
///     value := $A6;
///     a := FormatValue('B', value);    // 10100110
///     b := FormatValue('B12', value);  // 000010100110
///
///     value := 123;
///     c := FormatValue('B', value);    // 1111011
///     d := FormatValue('B4', value);   // 1111011
///     e := FormatValue('B10', value);  // 0001111011
///     </code>
///     </example>
///     </description>
///   </item>
/// </list>
/// <para>
/// NOTE: FormatValue always uses the native calendar system of the specified
/// culture to format a date value.
/// </para>
/// </remarks>
/// <param name="Fmt">
/// The format string. See <see cref="FormatCS"/> for details.
/// </param>
/// <param name="Value">
/// The value to be formatted.
/// </param>
/// <param name="Culture">
/// The optional <see cref="TCultureInfo"/> object that provides the locale specific
/// format settings.
/// </param>
/// <param name="UseNativeDigits">
/// Indicates whether the numbers should be represented in the native digits of the
/// specified <paramref name="Culture"/>.
/// </param>
/// <returns>
/// The formatted value as a string.
/// </returns>
/// <seealso cref="FormatCS"/>
{$endregion}
function FormatValue(const Fmt: String; const Value: Variant; Culture: TCultureInfo = nil;
  UseNativeDigits: Boolean = True): String;

{$region 'xmldoc'}
/// <summary>
/// Returns a formatted string assembled from a format string in C# language style
/// and an array of arguments.
/// </summary>
/// <remarks>
/// <para>
/// FormatCS returns a string produced according to the formatting string <paramref name="Fmt"/>.
/// </para>
/// <para>
/// The <paramref name="Fmt"/> parameter consists of zero or more runs of text intermixed
/// with zero or more indexed placeholders, called format items, that correspond to a value
/// in the parameter list of this function. The formatting process replaces each format item
/// with the text representation of the value of the corresponding value.
/// </para>
/// <para>
/// The syntax of a format item is <c>{index[,alignment][:specifier]}</c>, which specifies
/// a mandatory index, the optional length and alignment of the formatted text, and an optional
/// string of format specifier characters that govern how the value of the corresponding argument
/// is formatted. The components of a format item are:
/// </para>
///
/// <list type="table">
///   <listheader>
///     <term>Format Component</term>
///     <description>Description</description>
///   </listheader>
///   <item>
///     <term>index</term>
///     <description>
///     A zero-based integer that indicates which element in a list of objects to format.
///     </description>
///   </item>
///   <item>
///     <term>alignment</term>
///     <description>
///     An optional integer indicating the minimum width of the region to contain the formatted
///     value. If the length of the formatted value is less than alignment, then the region is
///     padded with spaces. If alignment is negative, the formatted value is left justified in
///     the region; if alignment is positive, the formatted value is right justified. If alignment
///     is not specified, the length of the region is the length of the formatted value. The comma
///     is required if alignment is specified.
///     </description>
///   </item>
///   <item>
///     <term>specifier</term>
///     <description>
///     An optional string of format specifiers. The format specifier for datetime values and numeric
///     values respectively correspond to the Fmt parameters of <see cref="TCalendar.Format"/>
///     and <see cref="TCultureInfo.FormatNumber"/> methods. In addition, there are some extra format
///     specifiers for numeric values. See <see cref="FormatValue"/> for details.
///     </description>
///   </item>
/// </list>
/// <para>
/// The leading and trailing brace characters, '{' and '}', are required. To specify a single
/// literal brace character in format, specify two leading or trailing brace characters; that
/// is, "{{" or "}}".
/// </para>
/// <para>
/// If FormatCS cannot expand a format item, it will include the format item in the string
/// as untouched.
/// </para>
/// <para>
/// NOTE: FormatCS always uses the native digits and calendar system of the specified
/// culture to format values.
/// </para>
/// </remarks>
/// <param name="Fmt">
/// The format string.
/// </param>
/// <param name="Args">
/// The array of arguments to apply to the format items in the format string.
/// </param>
/// <param name="Culture">
/// The optional <see cref="TCultureInfo"/> object that provides the locale specific
/// formatting information.
/// </param>
/// <returns>
/// The formatted string.
/// </returns>
/// <seealso cref="FormatValue"/>
{$endregion}
function FormatCS(const Fmt: String; const Args: array of Variant; Culture: TCultureInfo = nil): String;

{$region 'xmldoc'}
/// <summary>
/// Maps a specified string in a specified code page to a Unicode string.
/// </summary>
/// <param name="CodePage">
/// The code page of the source string.
/// </param>
/// <param name="Str">
/// The source string.
/// </param>
/// <returns>
/// The Unicode string.
/// </returns>
/// <seealso cref="UnicodeToCodePage"/>
{$endregion}
function CodePageToUnicode(CodePage: Integer; const Str: RawByteString): UnicodeString;

{$region 'xmldoc'}
/// <summary>
/// Maps a specified Unicode string to a string in a specified code page.
/// </summary>
/// <param name="CodePage">
/// The code page of the result string.
/// </param>
/// <param name="Str">
/// The source Unicode string.
/// </param>
/// <returns>
/// The string in the code page.
/// </returns>
/// <seealso cref="CodePageToUnicode"/>
{$endregion}
function UnicodeToCodePage(CodePage: Integer; const Str: UnicodeString): RawByteString;

{$region 'xmldoc'}
/// <summary>
/// Provides a list of scripts used in a specified Unicode string.
/// </summary>
/// <param name="Str">
/// The Unicode string.
/// </param>
/// <param name="AllowInheritedCommon">
/// Indicates whether to retrieve "Qaii" (INHERITED) and "Zyyy" (COMMON) script
/// information.
/// </param>
/// <returns>
/// The set of scripts used in the Unicode string.
/// </returns>
{$endregion}
function StringScripts(const Str: String; AllowInheritedCommon: Boolean = False): TUnicodeScripts;

{$region 'xmldoc'}
/// <summary>
/// Indicates whether a specified string represents a GUID.
/// </summary>
/// <param name="Str">
/// The string to examine.
/// </param>
/// <returns>
/// Returns <see langword="true"/> if the string represents a GUID, otherwise
/// returns <see langword="false"/>.
/// </returns>
{$endregion}
function IsStringGUID(const Str: String): Boolean;

{$region 'xmldoc'}
/// <summary>
/// Indicates whether a specified string contains translatable phrases.
/// </summary>
/// <remarks>
/// <para>
/// A string is considered translatable if it contains non-whitespace characters
/// and does not represent a GUID.
/// </para>
/// </remarks>
/// <param name="Str">
/// The string to examine.
/// </param>
/// <returns>
/// Returns <see langword="true"/> if the string is translatable, otherwise returns
/// <see langword="false"/>.
/// </returns>
{$endregion}
function IsStringTranslatable(const Str: String): Boolean;

implementation

uses
  Types, Math, i18nWinNLS;

resourcestring
  SInvalidString = 'Invalid escaped character in the string';

function ExtractComponentName(const PropertyPath: String): String;
var
  I: Integer;
begin
  I := Pos('.', PropertyPath);
  if I = 0 then
    I := Pos('[', PropertyPath);
  if I <> 0 then
    Result := Copy(PropertyPath, 1, I - 1)
  else
    Result := '';
end;

function ExpandEnvStr(const Str: String): String;
var
  Size: Cardinal;
begin
  Size := 2048;
  SetString(Result, nil, Size);
  Size := ExpandEnvironmentStrings(PChar(Str), PChar(Result), Size);
  if Size > Cardinal(Length(Result)) then
  begin
    SetString(Result, nil, Size);
    Size := ExpandEnvironmentStrings(PChar(Str), PChar(Result), Size);
  end;
  if Size > 0 then
    SetLength(Result, Size - 1)
  else
    Result := '';
end;

function EscapeString(const Str: String): String;
const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';
  Chars8to13: array[#$8..#$D] of Char = 'btnvfr';
var
  I: Integer;
  R: PChar;
  C, Esc: Char;
begin
  {$IFNDEF COMPILER_XE5_UP}
  Esc := #0; // to prevent compiler warning
  {$ENDIF}
  SetString(Result, nil, 2 * Length(Str));
  R := PChar(Result) - 1;
  for I := 1 to Length(Str) do
  begin
    Inc(R);
    C := Str[I];
    case C of
      #$8..#$D: Esc := Chars8to13[C];    // -> \b \t \n \v \f \r
           #$0: Esc := '0';              // -> \0
           '\': Esc := '\';              // -> \\
    else
      if C < #$20 then                   // -> \xHH
        Esc := 'x'
      else
      begin
        R^ := C;
        Continue;
      end;
    end;
    R^ := '\';
    Inc(R);
    R^ := Esc;
    if Esc = 'x' then
    begin
      Inc(R);
      R^ := HexDigits[Ord(C) shr 4];
      Inc(R);
      R^ := HexDigits[Ord(C) and $000F];
    end;
  end;
  SetLength(Result, R - PChar(Result) + 1);
end;

function UnescapeString(const Str: String): String;

  function GetEscapedChar(var Index: Integer; MaxLen: Integer): Char;
  var
    Count: Integer;
    Value: Word;
  begin
    Value := 0;
    Count := 0;
    while (Count < MaxLen) and (Index < Length(Str)) do
    begin
      Inc(Index);
      case Str[Index] of
        '0'..'9': Value := Value * 16 + Ord(Str[Index]) - Ord('0');
        'A'..'F': Value := Value * 16 + 10 + Ord(Str[Index]) - Ord('A');
        'a'..'f': Value := Value * 16 + 10 + Ord(Str[Index]) - Ord('a');
      else
        Break;
      end;
      Inc(Count);
    end;
    if Count = 0 then
      EConvertError.Create(SInvalidString);
    Result := Char(Value);
  end;

var
  I, Len: Integer;
  R: PChar;
begin
  Len := Length(Str);
  SetString(Result, nil, Len);
  R := PChar(Result);
  I := 1;
  while I <= Len do
  begin
    R^ := Str[I];
    if Str[I] = '\' then
    begin
      Inc(I);
      if I > Len then
        Break;
      case Str[I] of
        'r': R^ := #$D;  // carriage return
        'f': R^ := #$C;  // form feed
        'v': R^ := #$B;  // vertical tab
        'n': R^ := #$A;  // line feed
        't': R^ := #$9;  // horizontal tab
        'b': R^ := #$8;  // backspace
        '0': R^ := #$0;  // null character
        'x': R^ := GetEscapedChar(I, 2);
        'u': R^ := GetEscapedChar(I, 4);
      end;
    end;
    Inc(R);
    Inc(I);
  end;
  SetLength(Result, R - PChar(Result));
end;

function DamerauLevenshteinDistance(const Str1, Str2: String): Integer;

  function Min(const A, B, C: Integer): Integer; inline;
  begin
    Result := A;
    if B < Result then
      Result := B;
    if C < Result then
      Result := C;
  end;

var
  LenStr1, LenStr2: Integer;
  I, J, Cost, PrevCost: Integer;
  pStr1, pStr2, S1, S2: PChar;
  D: PIntegerArray;
begin
  LenStr1 := Length(Str1);
  LenStr2 := Length(Str2);
  // to save some space, make sure the second index points to the shorter string
  if LenStr1 < LenStr2 then
  begin
    I := LenStr1;
    LenStr1 := LenStr2;
    LenStr2 := I;
    pStr1 := PChar(Str2);
    pStr2 := PChar(Str1);
  end
  else
  begin
    pStr1 := PChar(Str1);
    pStr2 := PChar(Str2);
  end;
  // skip leading matches
  while (LenStr2 <> 0) and (pStr1^ = pStr2^) do
  begin
    Inc(pStr1);
    Inc(pStr2);
    Dec(LenStr1);
    Dec(LenStr2);
  end;
  // skip trailing matches
  while (LenStr2 <> 0) and ((pStr1+LenStr1-1)^ = (pStr2+LenStr2-1)^) do
  begin
    Dec(LenStr1);
    Dec(LenStr2);
  end;
  // when one string is empty, length of the other is the distance
  if LenStr2 = 0 then
  begin
    Result := LenStr1;
    Exit;
  end;
  // calculate the edit distance
  D := AllocMem((LenStr2+1) * SizeOf(Integer));
  S1 := pStr1;
  for I := 1 to LenStr1 do
  begin
    PrevCost := I-1;
    Cost := I;
    S2 := pStr2;
    for J := 1 to LenStr2 do
    begin
      if (S1^ = S2^) or ((I > 1) and (J > 1) and (S1^ = (S2-1)^) and (S2^ = (S1-1)^)) then
        Cost := PrevCost
      else
        Cost := 1 + Min(Cost, PrevCost, D[J]);
      PrevCost := D[J];
      D[J] := Cost;
      Inc(S2);
    end;
    Inc(S1);
  end;
  Result := D[LenStr2];
  FreeMem(D);
end;

function StringSimilarityRatio(const Str1, Str2: String): Double;
var
  MaxLen: Integer;
  Distance: Integer;
begin
  Result := 1.0;
  if Length(Str1) > Length(Str2) then
    MaxLen := Length(Str1)
  else
    MaxLen := Length(Str2);
  if MaxLen <> 0 then
  begin
    Distance := DamerauLevenshteinDistance(Str1, Str2);
    Result := Result - (Distance / MaxLen);
  end;
end;

function TextSimilarityRatio(const Str1, Str2: String): Double;
var
  MaxLen: Integer;
  Distance: Integer;
begin
  Result := 1.0;
  if Length(Str1) > Length(Str2) then
    MaxLen := Length(Str1)
  else
    MaxLen := Length(Str2);
  if MaxLen <> 0 then
  begin
    Distance := DamerauLevenshteinDistance(LowerCase(Str1), LowerCase(Str2));
    Result := Result - (Distance / MaxLen);
  end;
end;

function TranslateDigits(const Str: String;
  FromCulture, ToCulture: TCultureInfo): String;
var
  S: PChar;
  FromZero, ToZero: Char;
begin
  Result := Str;
  FromZero := FromCulture.NativeDigits[1];
  ToZero := ToCulture.NativeDigits[1];
  if FromZero = ToZero then Exit;
  S := PChar(Result);
  while S^ <> #0 do
  begin
    if Cardinal(Ord(S^) - Ord(FromZero)) <= 9 then
      S^ := Chr(Ord(ToZero) + Ord(S^) - Ord(FromZero));
    Inc(S);
  end;
end;

function IntToBin(Value: Cardinal; Len: Integer): String;
begin
  Result := '';
  while Value <> 0 do
  begin
    if LongBool(Value and 1) then
      Result := '1' + Result
    else
      Result := '0' + Result;
    Value := Value shr 1;
  end;
  if Len > Length(Result) then
    Result := StringOfChar('0', Len - Length(Result)) + Result;
end;

function FormatValue(const Fmt: String; const Value: Variant;
  Culture: TCultureInfo; UseNativeDigits: Boolean): String; overload;
var
  M: Char;
  FmtStr: String;
  FloatValue: Extended;
  IntegerValue: Integer;
  Precision: Integer;
  IsFloat, NeedsPrec, NeedsLen: Boolean;
begin
  if VarIsStr(Value) and ((Fmt = '') or not TryStrToFloat(Value, FloatValue)) then
  begin
    Result := VarToStr(Value);
    Exit;
  end;
  if Length(Fmt) <> 0 then
    M := Fmt[1]
  else if VarType(Value) <> varDate then
    M := 'G'
  else
    M := '*';
  FmtStr := '';
  FloatValue := 0.0;
  IntegerValue := 0;
  Precision := 0;
  NeedsPrec := False;
  NeedsLen := False;
  IsFloat := False;
  if Length(Fmt) > 1 then
    NeedsPrec := TryStrToInt(Copy(Fmt, 2, Length(Fmt) - 1), Precision);
  case M of
    'C', 'c':
    begin
      IsFloat := True;
      FloatValue := Value;
      FmtStr := 'm';
    end;
    'G', 'g':
    begin
      IsFloat := True;
      FloatValue := Value;
      FmtStr := 'g';
    end;
    'E', 'e':
    begin
      IsFloat := True;
      FloatValue := Value;
      FmtStr := 'e';
      if not NeedsPrec then
      begin
        NeedsPrec := True;
        Precision := 7;
      end
      else
        Inc(Precision);
    end;
    'F', 'f':
    begin
      IsFloat := True;
      FloatValue := Value;
      FmtStr := 'f';
    end;
    'N', 'n':
    begin
      IsFloat := True;
      FloatValue := Value;
      FmtStr := 'n';
    end;
    'P', 'p':
    begin
      if not NeedsPrec then
      begin
        NeedsPrec := True;
        Precision := 2;
      end;
      if Assigned(Culture) then
      begin
        Result := Culture.FormatPercent(Value, Precision, UseNativeDigits);
        Exit;
      end;
      IsFloat := True;
      FloatValue := Value * 100.0;
      FmtStr := 'f%%';
    end;
    'D', 'd':
    begin
      IntegerValue := Value;
      FmtStr := 'd';
      NeedsLen := NeedsPrec;
    end;
    'X', 'x':
    begin
      Result := IntToHex(Value, Precision);
      if M = 'x' then
        Result := LowerCase(Result);
      Exit;
    end;
    'B', 'b':
    begin
      Result := IntToBin(Value, Precision);
      Exit;
    end;
  else
    if Assigned(Culture) then
      if VarType(Value) = varDate then
        Result := Culture.FormatDateTime(Fmt, VarToDateTime(Value), UseNativeDigits)
      else
        Result := Culture.FormatNumber(Fmt, Value, UseNativeDigits)
    else
      if VarType(Value) = varDate then
        Result := DefaultCalendar.Format(Fmt, VarToDateTime(Value))
      else
        Result := SysUtils.FormatFloat(Fmt, Value);
    Exit;
  end;
  if NeedsPrec then
    FmtStr := '.' + IntToStr(Precision) + FmtStr;
  if NeedsLen then
    FmtStr := IntToStr(Precision) + FmtStr;
  FmtStr := '%' + FmtStr;
  if Assigned(Culture) then
    if IsFloat then
      Result := Culture.Format(FmtStr, [FloatValue], UseNativeDigits)
    else
      Result := Culture.Format(FmtStr, [IntegerValue], UseNativeDigits)
  else
    if IsFloat then
      Result := SysUtils.Format(FmtStr, [FloatValue])
    else
      Result := SysUtils.Format(FmtStr, [IntegerValue]);
  if (M = 'g') or (M = 'e') then
    Result := LowerCase(Result);
end;

function FormatCS(const Fmt: String; const Args: array of Variant;
  Culture: TCultureInfo): String;
var
  Buffer: String;
  BufferPos: Integer;

  procedure Append(P: PChar; Count: Integer); overload;
  begin
    if Count <> 0 then
    begin
      if BufferPos + Count > Length(Buffer) then
        SetLength(Buffer, BufferPos + Max(512, Count));
      Move(P^, Buffer[BufferPos + 1], Count * SizeOf(Char));
      Inc(BufferPos, Count);
    end;
  end;

  procedure Append(C: Char; Count: Integer); overload;
  begin
    if Count <> 0 then
    begin
      if BufferPos + Count > Length(Buffer) then
        SetLength(Buffer, BufferPos + Max(512, Count));
      repeat
        Inc(BufferPos);
        Buffer[BufferPos] := C;
        Dec(Count);
      until Count = 0;
    end;
  end;

  procedure Append(const Str: String); overload;
  begin
    Append(PChar(Str), Length(Str));
  end;

  function AppendItem(const Item: String; UseNativeDigits: Boolean): Boolean;
  var
    IndexStr: String;
    AlignStr: String;
    Specifier: String;
    Index, Align: Integer;
    FormattedItem: String;
    P: Integer;
  begin
    Result := False;
    P := Pos(':', Item);
    if P > 0 then
    begin
      IndexStr := Copy(Item, 1, P - 1);
      Specifier := Copy(Item, P + 1, Length(Item) - P);
    end
    else
    begin
      IndexStr := Item;
      Specifier := '';
    end;
    P := Pos(',', IndexStr);
    if P > 0 then
    begin
      AlignStr := Copy(IndexStr, P + 1, Length(IndexStr) - P);
      Delete(IndexStr, P, Length(IndexStr) - P + 1);
    end
    else
      AlignStr := '';
    Align := 0;
    if TryStrToInt(IndexStr, Index) and
       (Index >= 0) and (Index < Length(Args)) and
       ((AlignStr = '') or TryStrToInt(AlignStr, Align)) then
    begin
      FormattedItem := FormatValue(Specifier, Args[Index], Culture, UseNativeDigits);
      if (AlignStr <> '') and (Abs(Align) > Length(FormattedItem)) then
      begin
        if Align < 0 then
          Append(' ', -Align - Length(FormattedItem));
        Append(FormattedItem);
        if Align > 0 then
          Append(' ', Align - Length(FormattedItem));
      end
      else
        Append(FormattedItem);
      Result := True;
    end;
  end;

var
  FmtItem: String;
  P1, P2: PChar;
  UseNativeDigits: Boolean;
begin
  Buffer := '';
  BufferPos := 0;
  UseNativeDigits := True;
  P2 := PChar(Fmt);
  while P2^ <> #0 do
  begin
    P1 := P2;
    while (P2^ <> #0) and (P2^ <> '{') and (P2^ <> '}') do
    begin
      case P2^ of
        UCC_NODS: UseNativeDigits := False;
        UCC_NADS: UseNativeDigits := True;
      end;
      Inc(P2);
    end;
    Append(P1, P2 - P1);
    if P2^ <> #0 then
    begin
      if P2^ = (P2 + 1)^ then
      begin
        Append(P2, 1);
        Inc(P2, 2);
      end
      else if P2^ = '{' then
      begin
        Inc(P2);
        P1 := P2;
        while P2^ <> #0 do
        begin
          if P2^ = '}' then
          begin
            if P2^ = (P2 + 1)^ then
              Inc(P2)
            else
              Break;
          end;
          Inc(P2);
        end;
        if P2^ = '}' then
        begin
          SetString(FmtItem, P1, P2 - P1);
          Inc(P2);
          FmtItem := StringReplace(FmtItem, '{{', '{', [rfReplaceAll]);
          FmtItem := StringReplace(FmtItem, '}}', '}', [rfReplaceAll]);
          if not AppendItem(FmtItem, UseNativeDigits) then
            Append(FmtItem);
        end
        else
          Append(P1 - 1, P2 - P1 + 1);
      end
      else // if P2^ = '}' then
      begin
        Append(P2, 1);
        Inc(P2);
      end;
    end;
  end;
  SetLength(Buffer, BufferPos);
  Result := Buffer;
end;

function CodePageToUnicode(CodePage: Integer;
  const Str: RawByteString): UnicodeString;
var
  NumOfChars: Integer;
begin
  Result := '';
  NumOfChars := MultiByteToWideChar(CodePage, 0, PAnsiChar(Str), Length(Str), nil, 0);
  if NumOfChars <> 0 then
  begin
    SetString(Result, nil, NumOfChars);
    MultiByteToWideChar(CodePage, 0, PAnsiChar(Str), Length(Str), PWideChar(Result), NumOfChars);
  end;
end;

function UnicodeToCodePage(CodePage: Integer;
  const Str: UnicodeString): RawByteString;
var
  NumOfChars: Integer;
begin
  Result := '';
  NumOfChars := WideCharToMultiByte(CodePage, 0, PWideChar(Str), Length(Str), nil, 0, nil, nil);
  if NumOfChars <> 0 then
  begin
    SetString(Result, nil, NumOfChars);
    WideCharToMultiByte(CodePage, 0, PWideChar(Str), Length(Str), PAnsiChar(Result), NumOfChars, nil, nil);
  end;
end;

function StringScripts(const Str: String;
  AllowInheritedCommon: Boolean): TUnicodeScripts;
var
  ScriptCodes: String;
begin
  ScriptCodes := GetStringScripts(Str, AllowInheritedCommon);
  Result := ScriptCodesToScripts(ScriptCodes);
end;

function IsStringGUID(const Str: String): Boolean;

  function IsHexNumber(S: PChar; Count: Integer): Boolean;
  begin
    while (Count > 0) and CharInSet(S^, ['0'..'9', 'a'..'f', 'A'..'F']) do
    begin
      Inc(S);
      Dec(Count);
    end;
    Result := (Count = 0);
  end;

begin
  Result := (Length(Str) = 38)
        and (Str[1] = '{') and (Str[38] = '}')
        and (Str[10] = '-') and (Str[15] = '-')
        and (Str[20] = '-') and (Str[25] = '-')
        and IsHexNumber(@Str[2], 8) and IsHexNumber(@Str[11], 4)
        and IsHexNumber(@Str[16], 4) and IsHexNumber(@Str[21], 4)
        and IsHexNumber(@Str[26], 12);
end;

function IsStringTranslatable(const Str: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(Str) do
    if Ord(Str[I]) > 32 then
    begin
      Result := not IsStringGUID(Str);
      Exit;
    end;
end;

end.
