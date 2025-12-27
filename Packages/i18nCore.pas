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
/// This unit implements classes and functions to collect information about cultures
/// (locales), territories (countries/regions), and currencies. It also provides
/// methods to perform culture-specific operations such as formatting and parsing
/// of numbers and date-time values, digit substitution, string verification,
/// and code page conversions.
/// </summary>
unit i18nCore;

{$I DELPHIAREA.INC}

{-$DEFINE EXCLUDE_TERRITORIES_WITH_NO_CULTURE_INFO}
{$DEFINE EXCLUDE_TERRITORIES_WITH_X_CODE}

interface

uses
  Windows, SysUtils, Classes, i18nHashList, i18nUnicode, i18nPlurals,
  i18nCalendar, i18nCalendars, i18nWinNLS;

const
  {$region 'xmldoc'}
  /// <summary>
  /// Specifies the start range of custom primary language identifiers.
  /// </summary>
  {$endregion}
  CUSTOM_LANG = $0200;
  {$region 'xmldoc'}
  /// <summary>
  /// Specifies the end range of custom primary language identifiers.
  /// </summary>
  {$endregion}
  CUSTOM_LANG_LAST = $03FF;
  {$region 'xmldoc'}
  /// <summary>
  /// Specifies the start range of custom secondary language identifiers.
  /// </summary>
  {$endregion}
  CUSTOM_SUBLANG = $20;
  {$region 'xmldoc'}
  /// <summary>
  /// Specifies the end range of custom secondary language identifiers.
  /// </summary>
  {$endregion}
  CUSTOM_SUBLANG_LAST = $3F;

const
  {$region 'xmldoc'}
  /// <summary>
  /// Lists nominal digits as a string of characters.
  /// </summary>
  {$endregion}
  NOMINAL_DIGITS = '0123456789';

type

  TCultureInfo = class;
  TReadonlyCultureList = class;
  TCultureList = class;
  TTerritoryInfo = class;
  TReadonlyTerritoryList = class;
  TTerritoryList = class;
  TCurrencyInfo = class;
  TReadonlyCurrencyList = class;
  TCurrencyList = class;

  {$region 'xmldoc'}
  /// <summary>
  /// This data type represents a pointer to <see cref="TFormatSettings"/> record.
  /// </summary>
  {$endregion}
  PFormatSettings = ^TFormatSettings;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the type of code pages.
  /// </summary>
  {$endregion}
  TCodePageType = (
    {$region 'xmldoc'}
    /// OEM code page
    {$endregion}
    cpOEM,
    {$region 'xmldoc'}
    /// ANSI code page
    {$endregion}
    cpANSI,
    {$region 'xmldoc'}
    /// MAC code page
    {$endregion}
    cpMAC
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the measurement systems.
  /// </summary>
  {$endregion}
  TMeasurementSystem = (
    {$region 'xmldoc'}
    /// S.I. system
    {$endregion}
    msMetric,
    {$region 'xmldoc'}
    /// U.S. system
    {$endregion}
    msUS
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the reading direction of languages.
  /// </summary>
  {$endregion}
  TReadingLayout = (
    {$region 'xmldoc'}
    /// Read from left to right, as for the English (United States) locale.
    {$endregion}
    rlLeftToRight,
    {$region 'xmldoc'}
    /// Read from right to left, as for Arabic locales.
    {$endregion}
    rlRightToLeft,
    {$region 'xmldoc'}
    /// Read vertically from top to bottom with columns to the right, and read
    /// from right to left, as for the Japanese (Japan) locale.
    {$endregion}
    rlTopToBottomColRightToLeft,
    {$region 'xmldoc'}
    /// Read vertically from top to bottom with columns proceeding to the right,
    /// as for the Mongolian (Mongolian) locale.
    {$endregion}
    rlTopToBottomColLeftToRight
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the possible display names for
  /// <see cref="TCultureInfo"/> objects.
  /// </summary>
  {$endregion}
  TCultureDisplayName = (
    {$region 'xmldoc'}
    /// Nothing
    {$endregion}
    cnNone,
    {$region 'xmldoc'}
    /// Locale name, a multi-part tag to uniquely identify the locale.
    {$endregion}
    cnLocale,
    {$region 'xmldoc'}
    /// Two characters of the international language code specified in ISO 639-1.
    {$endregion}
    cnLanguage,
    {$region 'xmldoc'}
    /// Three characters of the international language code specified in ISO 639-2/T.
    {$endregion}
    cnLanguage3,
    {$region 'xmldoc'}
    /// Full localized (in Windows user's language) name of the locale, for example,
    /// Deutsch (Deutschland) for German (Germany).
    {$endregion}
    cnLocalizedDisplayName,
    {$region 'xmldoc'}
    /// Display name of the locale in English. Usually the display name consists of
    /// the language and the country/region, for example, German (Germany) for
    /// Deutsch (Deutschland).
    {$endregion}
    cnEnglishDisplayName,
    {$region 'xmldoc'}
    /// Display name of the locale in its native language, for example, Deutsch
    /// (Deutschland) for the locale German (Germany).
    {$endregion}
    cnNativeDisplayName,
    {$region 'xmldoc'}
    /// Full localized (in Windows user's language) name of the language, for example,
    /// Deutsch representing German.
    {$endregion}
    cnLocalizedLanguageName,
    {$region 'xmldoc'}
    /// English name of the language, for example, German for Deutsch, from
    /// International ISO Standard 639.
    {$endregion}
    cnEnglishLanguageName,
    {$region 'xmldoc'}
    /// Native name of the language, for example, Deutsch for German (Germany).
    {$endregion}
    cnNativeLanguageName,
    {$region 'xmldoc'}
    /// Native name of the country/region associated with the locale, in the native
    /// language of the locale.
    {$endregion}
    cnNativeCountryName,
    {$region 'xmldoc'}
    /// Native name of the currency associated with the locale, in the native language
    /// of the locale.
    {$endregion}
    cnNativeCurrencyName
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This class represents a culture (locale).
  /// </summary>
  /// <remarks>
  /// Each instance of TCultureInfo class represents a specific locale. This class has
  /// properties and methods to query information about the locale, and perform the
  /// locale specific tasks.
  ///
  /// NOTE: Do not create or destroy an instance of TCultureInfo class. Use <see cref="CultureOf"/>
  /// global function to get TCultureInfo instance of a locale.
  /// </remarks>
  /// <seealso cref="CultureOf"/>
  /// <seealso cref="World.Cultures"/>
  {$endregion}
  TCultureInfo = class sealed(TObject)
  private
    fLocaleID: LCID;
    fGeoID: GEOID;
    fLocale: String;
    fLanguage2: String;
    fLanguage3: String;
    fScriptCodes: String;
    fNativeDigits: String;
    fNativeCalendarType: TCalendarClass;
    fCalendarTypes: TCalendarClassList;
    fScripts: TUnicodeScripts;
    fReadingLayout: TReadingLayout;
    fMeasurementSystem: TMeasurementSystem;
    fDefaultCodePages: array[TCodePageType] of Integer;
    fNativeCalendar: TCalendar;
    fLocalizedDisplayName: String;
    fEnglishDisplayName: String;
    fNativeDisplayName: String;
    fLocalizedLanguageName: String;
    fEnglishLanguageName: String;
    fNativeLanguageName: String;
    fNativeCountryName: String;
    fNativeCurrencyName: String;
    fPercentNegative: String;
    fPercentPositive: String;
    fPluralRule: String;
    fCountry: TTerritoryInfo;
    fCurrency: TCurrencyInfo;
    fFormatSettings: PFormatSettings;
    function GetLangID: LANGID; inline;
    function GetPrimaryLangID: LANGID; inline;
    function GetSubLangID: LANGID; inline;
    function GetBiDiMode: TBiDiMode; inline;
    function GetDefaultCodePages(CodePageType: TCodePageType): Integer;
    function GetDisplayNames(DisplayName: TCultureDisplayName): String;
    function GetFormatSettings: PFormatSettings;
    function GetNativeCalendar: TCalendar;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for the locale specified by its identifier.
    /// </summary>
    /// <param name="ALocaleID">
    /// The identifier of the locale.
    /// </param>
    {$endregion}
    constructor Create(ALocaleID: LCID); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for the locale specified by its name.
    /// </summary>
    /// <param name="ALocale">
    /// The name of the locale.
    /// </param>
    {$endregion}
    constructor Create(const ALocale: String); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the object and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a string that its embedded nominal digits (0-9) are substituted with
    /// their corresponding digits in this culture.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This method substitutes the nominal digits in the string specified
    /// by <paramref name="Str"/> to their corresponding native digits of this culture.
    /// </para>
    /// <para>
    /// If the argument passed as <paramref name="IgnoreUCC"/> parameter is false, the
    /// method will not substitute nominal digits that are marked with NODS (Nominal digit
    /// shapes) Unicode control character, otherwise all nominal digits will be substituted.
    /// </para>
    /// </remarks>
    /// <param name="Str">
    /// The string to convert.
    /// </param>
    /// <param name="IgnoreUCCs">
    /// Indicates whether ignore Unicode control characters.
    /// </param>
    /// <returns>
    /// The string that its digits are represented in native digits.
    /// </returns>
    /// <seealso cref="NominalDigitsToNative"/>
    /// <seealso cref="FreezeDigits"/>
    /// <seealso cref="UnfreezeDigits"/>
    /// <see cref="UCC_NODS"/>
    {$endregion}
    function NominalDigitsToNative(const Str: String; IgnoreUCCs: Boolean = False): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a string that its embedded native digits are substituted with their
    /// corresponding nominal digits (0-9).
    /// </summary>
    /// <remarks>
    /// <para>
    /// This method substitutes the native digits of this culture in the
    /// string specified by <paramref name="Str"/> to their corresponding nominal digits.
    /// </para>
    /// <para>
    /// If the argument passed as <paramref name="IgnoreUCC"/> parameter is false, the
    /// method will not substitute native digits that are marked with NADS (Native digit
    /// shapes substitution) Unicode control character, otherwise all native digits will
    /// be substituted.
    /// </para>
    /// </remarks>
    /// <param name="Str">
    /// The string to convert.
    /// </param>
    /// <param name="IgnoreUCCs">
    /// Indicates whether ignore Unicode control characters.
    /// </param>
    /// <returns>
    /// The string that its digits are represented in nominal digits.
    /// </returns>
    /// <seealso cref="NativeDigitsToNominal"/>
    /// <seealso cref="FreezeDigits"/>
    /// <seealso cref="UnfreezeDigits"/>
    /// <see cref="UCC_NADS"/>
    {$endregion}
    function NativeDigitsToNominal(const Str: String; IgnoreUCCs: Boolean = False): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a string that its embedded digits will always be displayed as either native
    /// or nominal, regardless of Windows settings.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Based on the user's selection (configurable via Windows control panel), Windows may
    /// display nominal digits (0-9) in different digit shapes.
    /// </para>
    /// <para>
    /// FreezeDigits inserts appropriate Unicode control character in front of the string
    /// to prevent automatic digit substitution by Windows.
    /// </para>
    /// <para>
    /// If argument passed as <paramref name="NativeDigits"/> is true, FreezeDigits will
    /// also substitute nominal digits with native digits.
    /// </para>
    /// </remarks>
    /// <param name="Str">
    /// The string to convert.
    /// </param>
    /// <param name="UseNativeDigits">
    /// Indicates whether to use native digits or nominal digits.
    /// </param>
    /// <returns>
    /// The string that its digits are fixed as either nominal or native.
    /// </returns>
    /// <seealso cref="UnfreezeDigits"/>
    /// <seealso cref="NominalDigitsToNative"/>
    /// <seealso cref="NativeDigitsToNominal"/>
    {$endregion}
    function FreezeDigits(const Str: String; UseNativeDigits: Boolean): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a string that its embedded digits can be displayed automatically as either
    /// native or nominal by Windows.
    /// </summary>
    /// <remarks>
    /// <para>
    /// UnfreezeDigits reverts changes made by <see cref="FreezeDigits"/> on the string
    /// specified by the <paramref name="Str"/> parameter.
    /// </para>
    /// <para>
    /// That means, if the string specified by the <paramref name="Str"/> parameter has
    /// NODS (Nominal digit shapes) Unicode control character in front, the control
    /// character will be removed and its native digits will be substituted with their
    /// corresponding nominal digits.
    /// </para>
    /// </remarks>
    /// <param name="Str">
    /// The string to convert.
    /// </param>
    /// <returns>
    /// The string that its digits can be displayed as either native or nominal.
    /// </returns>
    /// <seealso cref="FreezeDigits"/>
    {$endregion}
    function UnfreezeDigits(const Str: String): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a formatted string assembled from a format string and an array of
    /// arguments.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Format method acts like Format function of Delphi, except that it uses this
    /// culture's preferences for formatting the values.
    /// </para>
    /// <para>
    /// Optionally, this method can represent numbers in native digits.
    /// </para>
    /// </remarks>
    /// <param name="Fmt">
    /// The format string as in used by Delphi's standard Format function.
    /// </param>
    /// <param name="Args">
    /// The array of arguments to apply to the format specifiers in the format string.
    /// </param>
    /// <param name="UseNativeDigits">
    /// Indicates whether to use native digits or nominal digits for numbers.
    /// </param>
    /// <returns>
    /// The formatted string.
    /// </returns>
    {$endregion}
    function Format(const Fmt: String; const Args: array of const;
      UseNativeDigits: Boolean = True): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Formats a floating point value.
    /// </summary>
    /// <remarks>
    /// <para>
    /// FormatNumber method acts like FormatFloat function of Delphi, except that it
    /// uses this culture's preferences for formatting the number.
    /// </para>
    /// <para>
    /// Optionally, this method can represent the number in native digits.
    /// </para>
    /// </remarks>
    /// <param name="Fmt">
    /// The format string.
    /// </param>
    /// <param name="Value">
    /// The floating point value to format.
    /// </param>
    /// <param name="UseNativeDigits">
    /// Indicates whether to use native digits or nominal digits for the number.
    /// </param>
    /// <returns>
    /// The formatted string.
    /// </returns>
    {$endregion}
    function FormatNumber(const Fmt: String; const Value: Extended;
      UseNativeDigits: Boolean = True): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Formats a floating point value as a percentage.
    /// </summary>
    /// <remarks>
    /// <para>
    /// FormatPercent represents a floating point value as a percentage using the
    /// percentage format of this culture.
    /// </para>
    /// <para>
    /// Optionally, this method can represent the percentage in native digits.
    /// </para>
    /// </remarks>
    /// <param name="Value">
    /// The floating point value to format.
    /// </param>
    /// <param name="Decimals">
    /// The number of decimals.
    /// </param>
    /// <param name="UseNativeDigits">
    /// Indicates whether to use native digits or nominal digits for the percentage.
    /// </param>
    /// <returns>
    /// The formatted string.
    /// </returns>
    {$endregion}
    function FormatPercent(const Value: Extended; Decimals: Integer = 2;
      UseNativeDigits: Boolean = True): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Formats a <see cref="TDateTime"/> value.
    /// </summary>
    /// <remarks>
    /// <para>
    /// FormatDateTime formats the <see cref="TDateTime"/> value given by <paramref name="Value"/>
    /// using the format given by <paramref name="Fmt"/>.
    /// </para>
    /// <para>
    /// FormatDateTime uses <see cref="NativeCalendar"/> object to extract components of
    /// the specified <see cref="TDateTime"/> value.
    /// </para>
    /// <para>
    /// Optionally, this method can represent the numbers in native digits.
    /// </para>
    /// </remarks>
    /// <param name="Fmt">
    /// The format string. See the <see cref="TCalendar.Format"/> method for details.
    /// </param>
    /// <param name="Value">
    /// The <see cref="TDateTime"/> value to format.
    /// </param>
    /// <param name="UseNativeDigits">
    /// Indicates whether to use native digits or nominal digits for the numbers.
    /// </param>
    /// <returns>
    /// The formatted date-time string.
    /// </returns>
    /// <seealso cref="NativeCalendar"/>
    /// <seealso cref="TryParseDateTime"/>
    /// <seealso cref="ParseDateTime"/>
    {$endregion}
    function FormatDateTime(const Fmt: String; const Value: TDateTime;
      UseNativeDigits: Boolean = True): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string to a <see cref="TDateTime"/> value using a specified format
    /// string.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The ParseDateTime method scans the string given by <paramref name="Str"/>
    /// using the format string given by <paramref name="Fmt"/> to extract its
    /// date and time values.
    /// </para>
    /// <para>
    /// ParseDateTime uses the calendar object specified by <see cref="NativeCalendar"/>
    /// property to extract the date and time components from the input string.
    /// </para>
    /// </remarks>
    /// <param name="Fmt">
    /// The format string. See the <see cref="TCalendar.Format"/> method for details.
    /// </param>
    /// <param name="Str">
    /// The date and time value as a string.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.
    /// </returns>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="TryParseDateTime"/>
    /// <seealso cref="FormatDateTime"/>
    {$endregion}
    function ParseDateTime(const Fmt: String; const Str: String): TDateTime;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string to a <see cref="TDateTime"/> value using a specified format
    /// string.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The TryParseDateTime method scans the string given by <paramref name="Str"/>
    /// using the format string given by <paramref name="Fmt"/> to extract its
    /// date and time values.
    /// </para>
    /// <para>
    /// TryParseDateTime uses the calendar object specified by <see cref="NativeCalendar"/>
    /// property to extract the date and time components of the input string.
    /// </para>
    /// </remarks>
    /// <param name="Fmt">
    /// The format string. See the <see cref="TCalendar.Format"/> method for details.
    /// </param>
    /// <param name="Str">
    /// The date and time value as a string.
    /// </param>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion is succeeded, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="ParseDateTime"/>
    /// <seealso cref="FormatDateTime"/>
    {$endregion}
    function TryParseDateTime(const Fmt: String; const Str: String;
      var DateTime: TDateTime): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Verifies whether a specified string used the scripts of this culture.
    /// </summary>
    /// <param name="Str">
    /// The string to examine.
    /// </param>
    /// <param name="AllowLatin">
    /// Indicates whether the Latin script is allowed anyway.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the string uses scripts of this culture,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="ScriptCodes"/>
    /// <seealso cref="Scripts"/>
    {$endregion}
    function VerifyString(const Str: String; AllowLatin: Boolean = True): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string in default OEM code page of this culture's language to Unicode.
    /// </summary>
    /// <param name="Str">
    /// The OEM string.
    /// </param>
    /// <returns>
    /// The Unicode string.
    /// </returns>
    /// <seealso cref="UnicodeToOem"/>
    /// <seealso cref="AnsiToUnicode"/>
    /// <seealso cref="MacToUnicode"/>
    /// <seealso cref="DefaultCodePages"/>
    {$endregion}
    function OemToUnicode(const Str: RawByteString): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string from Unicode to default OEM code page of this culture's language.
    /// </summary>
    /// <param name="Str">
    /// The Unicode string
    /// </param>
    /// <returns>
    /// The OEM string.
    /// </returns>
    /// <seealso cref="OemToUnicode"/>
    /// <seealso cref="UnicodeToAnsi"/>
    /// <seealso cref="UnicodeToMac"/>
    /// <seealso cref="DefaultCodePages"/>
    {$endregion}
    function UnicodeToOem(const Str: String): RawByteString;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string in default ANSI code page of this culture's language to Unicode.
    /// </summary>
    /// <param name="Str">
    /// The ANSI string.
    /// </param>
    /// <returns>
    /// The Unicode string.
    /// </returns>
    /// <seealso cref="UnicodeToAnsi"/>
    /// <seealso cref="OemToUnicode"/>
    /// <seealso cref="MacToUnicode"/>
    /// <seealso cref="DefaultCodePages"/>
    {$endregion}
    function AnsiToUnicode(const Str: RawByteString): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string from Unicode to default ANSI code page of this culture's language.
    /// </summary>
    /// <param name="Str">
    /// The Unicode string
    /// </param>
    /// <returns>
    /// The ANSI string.
    /// </returns>
    /// <seealso cref="AnsiToUnicode"/>
    /// <seealso cref="UnicodeToOem"/>
    /// <seealso cref="UnicodeToMac"/>
    /// <seealso cref="DefaultCodePages"/>
    {$endregion}
    function UnicodeToAnsi(const Str: String): RawByteString;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string in default MAC code page of this culture's language to Unicode.
    /// </summary>
    /// <param name="Str">
    /// The MAC string.
    /// </param>
    /// <returns>
    /// The Unicode string.
    /// </returns>
    /// <seealso cref="UnicodeToMac"/>
    /// <seealso cref="AnsiToUnicode"/>
    /// <seealso cref="OemToUnicode"/>
    /// <seealso cref="DefaultCodePages"/>
    {$endregion}
    function MacToUnicode(const Str: RawByteString): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string from Unicode to default MAC code page of this culture's language.
    /// </summary>
    /// <param name="Str">
    /// The Unicode string
    /// </param>
    /// <returns>
    /// The MAC string.
    /// </returns>
    /// <seealso cref="MacToUnicode"/>
    /// <seealso cref="UnicodeToAnsi"/>
    /// <seealso cref="UnicodeToOem"/>
    /// <seealso cref="DefaultCodePages"/>
    {$endregion}
    function UnicodeToMac(const Str: String): RawByteString;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the adjusted DrawText flags based on the language direction of this
    /// culture.
    /// </summary>
    /// <param name="Flags">
    /// The DrawText flags to adjust.
    /// </param>
    /// <returns>
    /// The adjusted DrawText flags.
    /// </returns>
    /// <seealso cref="DrawTextBiDiModeFlagsReadingOnly"/>
    {$endregion}
    function DrawTextBiDiModeFlags(Flags: Cardinal): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the DrawText reading order flag based on the language direction of
    /// this culture.
    /// </summary>
    /// <returns>
    /// The DrawText reading order flag.
    /// </returns>
    /// <seealso cref="DrawTextBiDiModeFlags"/>
    {$endregion}
    function DrawTextBiDiModeFlagsReadingOnly: Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified culture object has the same primary language
    /// as this culture.
    /// </summary>
    /// <param name="Culture">
    /// The culture object to examined.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if primary languages are identical, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsDialectOf(Culture: TCultureInfo): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified locale name represents the primary language
    /// of this culture.
    /// </summary>
    /// <param name="Locale">
    /// The locale name to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if primary languages are identical, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsDialectOf(const Locale: String): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified language identifier represents the primary
    /// language of this culture.
    /// </summary>
    /// <param name="LangID">
    /// The language identifier to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if primary languages are identical, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsDialectOf(LangID: LANGID): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the nominal digits (0-9) are the native digits of this culture.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the nominal digits are the native digits of this
    /// culture, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <see cref="NativeDigits"/>
    {$endregion}
    function IsUsingNominalDigits: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the Gregorian calendar in Common Era (C.E.) is the native calendar
    /// of this culture.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the Gregorian calendar in Common Era is the native
    /// calendar of this culture, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <see cref="NativeCalendar"/>
    {$endregion}
    function IsUsingCommonEraCalendar: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the language direction of this culture is from right to left.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the language direction of this culture
    /// is from right to left, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="ReadingLayout"/>
    {$endregion}
    function IsRightToLeft: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the language represented by this culture is the default
    /// dialect (sub-language) of its main language (primary language).
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if this culture's language is the default
    /// dialect of its primary language, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsDefault: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the locale identifier of the culture.
    /// </summary>
    {$endregion}
    property LocaleID: LCID read fLocaleID;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the language identifier of the culture.
    /// </summary>
    {$endregion}
    property LangID: LANGID read GetLangID;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the primary language identifier of the culture.
    /// </summary>
    {$endregion}
    property PrimaryLangID: LANGID read GetPrimaryLangID;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the sub-language identifier of the culture.
    /// </summary>
    {$endregion}
    property SubLangID: LANGID read GetSubLangID;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the geographical identifier of the culture.
    /// </summary>
    {$endregion}
    property GeoID: GEOID read fGeoID;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the locale name of the culture.
    /// </summary>
    {$endregion}
    property Locale: String read fLocale;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the two-character ISO 639-1 international language code of the culture.
    /// </summary>
    {$endregion}
    property Language2: String read fLanguage2;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the three-character ISO 639-2/T international language code of the culture.
    /// </summary>
    {$endregion}
    property Language3: String read fLanguage3;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets country information of the culture.
    /// </summary>
    {$endregion}
    property Country: TTerritoryInfo read fCountry;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets currency information of the culture.
    /// </summary>
    {$endregion}
    property Currency: TCurrencyInfo read fCurrency;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the four-character ISO 15924 script codes of the culture separated by
    /// semicolon.
    /// </summary>
    {$endregion}
    property ScriptCodes: String read fScriptCodes;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the set of culture's scripts.
    /// </summary>
    {$endregion}
    property Scripts: TUnicodeScripts read fScripts;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the default measurement system used by the culture.
    /// </summary>
    {$endregion}
    property MeasurementSystem: TMeasurementSystem read fMeasurementSystem;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the native digits of the culture starting from 0 to 9.
    /// </summary>
    {$endregion}
    property NativeDigits: String read fNativeDigits;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TCalendar"/> class that manages the default calendar system
    /// used by the culture.
    /// </summary>
    {$endregion}
    property NativeCalendarType: TCalendarClass read fNativeCalendarType;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the native calendar used by the culture.
    /// </summary>
    {$endregion}
    property NativeCalendar: TCalendar read GetNativeCalendar;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the reading order of the scripts in the culture.
    /// </summary>
    {$endregion}
    property ReadingLayout: TReadingLayout read fReadingLayout;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the TBiDiMode value of the culture's reading order.
    /// </summary>
    {$endregion}
    property BiDiMode: TBiDiMode read GetBiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the language's plural rule that determines how the language handles
    /// plurals of nouns or unit expressions.
    /// </summary>
    /// <remarks>
    /// <para>
    /// NOTE: During development of the i18n package, I could not find the plural
    /// rule of some languages. For those languages, the PluralRule property will
    /// return a default rule that is 'nplurals=1; plural=0; ASSUMED'. So, you can
    /// check for existence of word 'ASSUMED' in a plural rule returned by this
    /// property to find out whether the rule is correct or is just a default
    /// assumption.
    /// </para>
    /// </remarks>
    /// <seealso cref="TPluralForms"/>
    {$endregion}
    property PluralRule: String read fPluralRule;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the localized (in Windows user's language) name of the culture.
    /// </summary>
    {$endregion}
    property LocalizedDisplayName: String read fLocalizedDisplayName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the English name of the culture.
    /// </summary>
    {$endregion}
    property EnglishDisplayName: String read fEnglishDisplayName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the native name of the culture.
    /// </summary>
    {$endregion}
    property NativeDisplayName: String read fNativeDisplayName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the localized (in Windows user's language) name of the culture's language.
    /// </summary>
    {$endregion}
    property LocalizedLanguageName: String read fLocalizedLanguageName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the English name of the culture's language.
    /// </summary>
    {$endregion}
    property EnglishLanguageName: String read fEnglishLanguageName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the native name of the culture's language.
    /// </summary>
    {$endregion}
    property NativeLanguageName: String read fNativeLanguageName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the native name of the culture's country.
    /// </summary>
    {$endregion}
    property NativeCountryName: String read fNativeCountryName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the native name of the culture's currency.
    /// </summary>
    {$endregion}
    property NativeCurrencyName: String read fNativeCurrencyName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets pointer to the culture's format settings.
    /// </summary>
    {$endregion}
    property FormatSettings: PFormatSettings read GetFormatSettings;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the default ANSI, OEM, and MAC codepages of the culture.
    /// </summary>
    {$endregion}
    property DefaultCodePages[CodePageType: TCodePageType]: Integer read GetDefaultCodePages;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists display names of the culture.
    /// </summary>
    {$endregion}
    property DisplayNames[DisplayName: TCultureDisplayName]: String read GetDisplayNames;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCultureEnumerator enumerates through the <see cref="TCultureInfo"/> objects
  /// of an instance of <see cref="TReadonlyCultureList"/> or its descendent class.
  /// </summary>
  {$endregion}
  TCultureEnumerator = class(TListEnumerator)
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for a specified list.
    /// </summary>
    /// <param name="list">
    /// The list to enumerate its items.
    /// </param>
    {$endregion}
    constructor Create(List: TReadonlyCultureList);
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the current <see cref="TCultureInfo"/> object of the list being
    /// enumerated.
    /// </summary>
    /// <returns>
    /// Returns the current <see cref="TCultureInfo"/> object.
    /// </returns>
    {$endregion}
    function GetCurrent: TCultureInfo; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the current <see cref="TCultureInfo"/> object.
    /// </summary>
    {$endregion}
    property Current: TCultureInfo read GetCurrent;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class maintains a read-only list of <see cref="TCultureInfo"/> objects.
  /// </summary>
  /// <remarks>
  /// TReadonlyCultureList maintains a read-only list of <see cref="TCultureInfo"/>
  /// objects, and provides properties and methods to locate and access the objects.
  /// The objects in the list are hashed and can be located quickly.
  /// </remarks>
  /// <see cref="TCultureList"/>
  {$endregion}
  TReadonlyCultureList = class(TPersistent)
  private
    Lookup_LocaleID: TKeyLookup<LCID,Integer>;
    Lookup_Locale: TKeyLookup<String,Integer>;
    IsLookupTableValid_LocaleID: Boolean;
    IsLookupTableValid_Locale: Boolean;
    function GetCount: Integer; inline;
    function GetItems(Index: Integer): TCultureInfo; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Provides direct access to the individual <see cref="TCultureInfo"/> objects.
    /// </summary>
    {$endregion}
    List: TList;
    {$region 'xmldoc'}
    /// <summary>
    /// Attaches a specified <see cref="TCultureInfo"/> object with a specified index
    /// in the list.
    /// </summary>
    /// <remarks>
    /// <para>
    /// NOTE: This method does not add the object to the list, it only creates its lookup
    /// entries for all the lookup tables.
    /// </para>
    /// </remarks>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object to add to the lookup tables.
    /// </param>
    /// <param name="Index">
    /// The index of the <see cref="TCultureInfo"/> object in the list.
    /// </param>
    /// <seealso cref="RemoveLookupEntries"/>
    {$endregion}
    procedure AddLookupEntries(Culture: TCultureInfo; Index: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Detaches a specified <see cref="TCultureInfo"/> object from the previously attached
    /// index.
    /// </summary>
    /// <remarks>
    /// <para>
    /// NOTE: This method does not remove the object from the list, it only deletes its
    /// lookup entries from all lookup tables.
    /// </para>
    /// </remarks>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object to remove from the lookup tables.
    /// </param>
    /// <seealso cref="AddLookupEntries"/>
    {$endregion}
    procedure RemoveLookupEntries(Culture: TCultureInfo);
    {$region 'xmldoc'}
    /// <summary>
    /// Clears all entries from all lookup tables.
    /// </summary>
    {$endregion}
    procedure ClearLookupTables;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules rebuilding of all lookup tables.
    /// </summary>
    {$endregion}
    procedure InvalidateLookupTables; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Validates lookup table for <see cref="TCultureInfo.LocaleID"/> values.
    /// </summary>
    {$endregion}
    procedure ValidateLookupTable_LocaleID;
    {$region 'xmldoc'}
    /// <summary>
    /// Validates lookup table for <see cref="TCultureInfo.Locale"/> values.
    /// </summary>
    {$endregion}
    procedure ValidateLookupTable_Locale;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TCultureEnumerator"/> reference, which enumerates
    /// all <see cref="TCultureInfo"/> objects in the list.
    /// </summary>
    /// <returns>
    /// Returns a reference to a <see cref="TCultureEnumerator"/> instance.
    /// </returns>
    {$endregion}
    function GetEnumerator: TCultureEnumerator;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the first <see cref="TCultureInfo"/> object in the list.
    /// </summary>
    /// <returns>
    /// The first <see cref="TCultureInfo"/> object in the list.
    /// </returns>
    /// <seealso cref="Last"/>
    {$endregion}
    function First: TCultureInfo;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the last <see cref="TCultureInfo"/> object in the list.
    /// </summary>
    /// <returns>
    /// The last <see cref="TCultureInfo"/> object in the list.
    /// </returns>
    /// <seealso cref="First"/>
    {$endregion}
    function Last: TCultureInfo;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies <see cref="TCultureInfo"/> objects in the list to another object.
    /// </summary>
    /// <param name="Dest">
    /// The destination object.
    /// </param>
    /// <seealso cref="AssignLabelsTo"/>
    {$endregion}
    procedure AssignTo(Dest: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies display name of <see cref="TCultureInfo"/> objects in the list to
    /// a <see cref="TStrings"/> object.
    /// </summary>
    /// <param name="Dest">
    /// The destination <see cref="TStrings"/> object.
    /// </param>
    /// <param name="DisplayName">
    /// Determines which <see cref="TCultureInfo.DisplayNames"/> value should be used.
    /// </param>
    /// <seealso cref="AssignTo"/>
    /// <seealso cref="TCultureInfo.DisplayNames"/>
    {$endregion}
    procedure AssignLabelsTo(Dest: TStrings; DisplayName: TCultureDisplayName);
    {$region 'xmldoc'}
    /// <summary>
    /// Locates a <see cref="TCultureInfo"/> object in the list by its locale identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    /// <returns>
    /// The <see cref="TCultureInfo"/> object with the specified locale identifier
    /// or <see langword="nil"/> if the object is not found.
    /// </returns>
    /// <seealso cref="FindNearest"/>
    {$endregion}
    function Find(LocaleID: LCID): TCultureInfo; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Locates a <see cref="TCultureInfo"/> object in the list by its locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <returns>
    /// The <see cref="TCultureInfo"/> object with the specified locale name
    /// or <see langword="nil"/> if the object is not found.
    /// </returns>
    /// <seealso cref="FindNearest"/>
    {$endregion}
    function Find(const Locale: String): TCultureInfo; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Locates a <see cref="TCultureInfo"/> object in the list, which is either
    /// identical to or a dialect of a specified <see cref="TCultureInfo"/> object.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object to be located.
    /// </param>
    /// <returns>
    /// The matched <see cref="TCultureInfo"/> object or <see langword="nil"/> if
    /// the object is not found.
    /// </returns>
    /// <seealso cref="Find"/>
    {$endregion}
    function FindNearest(Culture: TCultureInfo): TCultureInfo; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Locates a <see cref="TCultureInfo"/> object in the list, which is either
    /// identical to or a dialect of a specified locale identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    /// <returns>
    /// The matched <see cref="TCultureInfo"/> object or <see langword="nil"/> if
    /// the object is not found.
    /// </returns>
    /// <seealso cref="Find"/>
    {$endregion}
    function FindNearest(LocaleID: LCID): TCultureInfo; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Locates a <see cref="TCultureInfo"/> object in the list, which is either
    /// identical to or a dialect of a specified locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <returns>
    /// The matched <see cref="TCultureInfo"/> object or <see langword="nil"/> if
    /// the object is not found.
    /// </returns>
    /// <seealso cref="Find"/>
    {$endregion}
    function FindNearest(const Locale: String): TCultureInfo; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a specified <see cref="TCultureInfo"/> object in the list.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object to locate.
    /// </param>
    /// <returns>
    /// The index of <see cref="TCultureInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOfName"/>
    /// <seealso cref="NearestIndexOf"/>
    {$endregion}
    function IndexOf(Culture: TCultureInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TCultureInfo"/> object in the list by its locale
    /// identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    /// <returns>
    /// The index of <see cref="TCultureInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOfName"/>
    /// <seealso cref="NearestIndexOf"/>
    {$endregion}
    function IndexOf(LocaleID: LCID): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TCultureInfo"/> object in the list by its locale
    /// name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <returns>
    /// The index of <see cref="TCultureInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOfName"/>
    /// <seealso cref="NearestIndexOf"/>
    {$endregion}
    function IndexOf(const Locale: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TCultureInfo"/> object in the list by its display name.
    /// </summary>
    /// <param name="AName">
    /// The display name of the target object.
    /// </param>
    /// <param name="DisplayName">
    /// Indicates which <see cref="TCultureInfo.DisplayNames"/> value should be used.
    /// </param>
    /// <returns>
    /// The index of <see cref="TCultureInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOf"/>
    {$endregion}
    function IndexOfName(const AName: String; DisplayName: TCultureDisplayName): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TCultureInfo"/> object in the list, which is either
    /// identical to or a dialect of a specified <see cref="TCultureInfo"/> object.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object to locate.
    /// </param>
    /// <returns>
    /// The index of the matched <see cref="TCultureInfo"/> object or -1 if the
    /// object is not found.
    /// </returns>
    /// <seealso cref="IndexOf"/>
    {$endregion}
    function NearestIndexOf(Culture: TCultureInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TCultureInfo"/> object in the list, which is either
    /// identical to or a dialect of a specified locale identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    /// <returns>
    /// The index of the matched <see cref="TCultureInfo"/> object or -1 if the
    /// object is not found.
    /// </returns>
    /// <seealso cref="IndexOf"/>
    {$endregion}
    function NearestIndexOf(LocaleID: LCID): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TCultureInfo"/> object in the list, which is either
    /// identical to or a dialect of a specified locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <returns>
    /// The index of the matched <see cref="TCultureInfo"/> object or -1 if the
    /// object is not found.
    /// </returns>
    /// <seealso cref="IndexOf"/>
    {$endregion}
    function NearestIndexOf(const Locale: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified <see cref="TCultureInfo"/> object is in the list.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TCultureInfo"/> object is in the
    /// list, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Exists(Culture: TCultureInfo): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a <see cref="TCultureInfo"/> object with a specified locale
    /// identifier is in the list.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if a <see cref="TCultureInfo"/> object with the
    /// specified locale identifier is in the list, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Exists(LocaleID: LCID): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a <see cref="TCultureInfo"/> object with a specified locale
    /// name is in the list.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if a <see cref="TCultureInfo"/> object with the
    /// specified locale name is in the list, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Exists(const Locale: String): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of <see cref="TCultureInfo"/> objects in the list.
    /// </summary>
    {$endregion}
    property Count: Integer read GetCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCultureInfo"/> objects in the list.
    /// </summary>
    {$endregion}
    property Items[Index: Integer]: TCultureInfo read GetItems; default;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class maintains a list of <see cref="TCultureInfo"/> objects.
  /// </summary>
  /// <remarks>
  /// Use TCultureList to store and maintain a list of <see cref="TCultureInfo"/>
  /// objects. TCultureList provides properties and methods to add, delete, rearrange,
  /// locate, access, and sort <see cref="TCultureInfo"/> objects.
  /// </remarks>
  {$endregion}
  TCultureList = class(TReadonlyCultureList)
  private
    fDuplicates: TDuplicates;
    fUpdateCount: Integer;
    fOnChange: TNotifyEvent;
    fOnChanging: TNotifyEvent;
    function GetCapacity: Integer; inline;
    procedure SetCapacity(Value: Integer);
    procedure ReadLocales(Reader: TReader);
    procedure WriteLocales(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="Items"/> property as if it were published.
    /// </summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the list.
    /// </param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChanging"/> event.
    /// </summary>
    /// <seealso cref="Change"/>
    {$endregion}
    procedure Changing; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChange"/> event.
    /// </summary>
    /// <seealso cref="Changing"/>
    {$endregion}
    procedure Change; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the update level.
    /// </summary>
    {$endregion}
    property UpdateCount: Integer read fUpdateCount;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents generating of the <see cref="OnChanging"/> and <see cref="OnChange"/>
    /// events until the <see cref="EndUpdate"/> method is called.
    /// </summary>
    /// <seealso cref="EndUpdate"/>
    {$endregion}
    procedure BeginUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Reenables <see cref="OnChanging"/> and <see cref="OnChange"/> events generation
    /// that was turned off with the <see cref="BeginUpdate"/> method.
    /// </summary>
    /// <seealso cref="BeginUpdate"/>
    {$endregion}
    procedure EndUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes all items from the list.
    /// </summary>
    {$endregion}
    procedure Clear;
    {$region 'xmldoc'}
    /// <summary>
    /// Sorts the items in the list by a specified display name.
    /// </summary>
    /// <param name="OrderBy">
    /// Determines which <see cref="TCultureInfo.DisplayNames"/> value should the
    /// list be sorted by.
    /// </param>
    {$endregion}
    procedure Sort(OrderBy: TCultureDisplayName);
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the list of <see cref="TCultureInfo"/> objects from another object.
    /// </summary>
    /// <param name="Source">
    /// The source list.
    /// </param>
    /// <seealso cref="Apply"/>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies <see cref="TCultureInfo"/> objects of one list to another.
    /// </summary>
    /// <remarks>
    /// Call Apply to assign the elements of another list to this one. Assign combines
    /// the source list with this one using the logical operator specified by the
    /// <paramref name="Op"/> parameter.
    ///
    /// If the <paramref name="ListB"/> parameter is specified or not <see langword="nil"/>,
    /// then Apply first replaces all the elements of this list with those in <paramref name="ListA"/>,
    /// and then merges <paramref name="ListB"/> into this list using the operator specified
    /// by <paramref name="Op"/>.
    ///
    /// These operators are acceptable in Apply method:
    ///
    /// <list type="table">
    ///   <listheader>
    ///     <term>Operator</term>
    ///     <description>Description</description>
    ///   </listheader>
    ///   <item>
    ///     <term>laCopy</term>
    ///     <description>Destination becomes a copy of the source</description>
    ///   </item>
    ///   <item>
    ///     <term>laAnd</term>
    ///     <description>Intersection of the two lists</description>
    ///   </item>
    ///   <item>
    ///     <term>laOr</term>
    ///     <description>Union of the two lists</description>
    ///   </item>
    ///   <item>
    ///     <term>laXor</term>
    ///     <description>Only those not in both lists</description>
    ///   </item>
    ///   <item>
    ///     <term>laSrcUnique</term>
    ///     <description>Only those unique to source (same as laAnd followed by laXor)</description>
    ///   </item>
    ///   <item>
    ///     <term>laDestUnique</term>
    ///     <description>Only those unique to destination (same as laOr followed by laXor)</description>
    ///   </item>
    /// </list>
    ///
    /// </remarks>
    /// <param name="ListA">
    /// The source list.
    /// </param>
    /// <param name="Op">
    /// The operator that indicates how the two lists should be merged.
    /// </param>
    /// <param name="ListB">
    /// The optional and aditional source list.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure Apply(ListA: TReadonlyCultureList; Op: TListAssignOp = laCopy;
      ListB: TReadonlyCultureList = nil);
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a specified <see cref="TCultureInfo"/> object to the list.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object to add.
    /// </param>
    /// <returns>
    /// The index of new <see cref="TCultureInfo"/> object in the list.
    /// </returns>
    {$endregion}
    function Add(Culture: TCultureInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a <see cref="TCultureInfo"/> object to the list by its locale identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    /// <returns>
    /// The index of new <see cref="TCultureInfo"/> object in the list.
    /// </returns>
    {$endregion}
    function Add(LocaleID: LCID): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a <see cref="TCultureInfo"/> object to the list by its locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <returns>
    /// The index of new <see cref="TCultureInfo"/> object in the list.
    /// </returns>
    {$endregion}
    function Add(const Locale: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a specified <see cref="TCultureInfo"/> object from the list.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object to remove.
    /// </param>
    /// <returns>
    /// The index of the removed <see cref="TCultureInfo"/> object, or -1 if the object
    /// is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Remove(Culture: TCultureInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a <see cref="TCultureInfo"/> object from the list by its locale identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    /// <returns>
    /// The index of the removed <see cref="TCultureInfo"/> object, or -1 if the object
    /// is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Remove(LocaleID: LCID): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a <see cref="TCultureInfo"/> object from the list by its locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <returns>
    /// The index of the removed <see cref="TCultureInfo"/> object, or -1 if the object
    /// is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Remove(const Locale: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a <see cref="TCultureInfo"/> object at a specified index of the list.
    /// </summary>
    /// <param name="Index">
    /// The index of <see cref="TCultureInfo"/> object to removed.
    /// </param>
    /// <seealso cref="Remove"/>
    {$endregion}
    procedure Delete(Index: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of <see cref="TCultureInfo"/> objects the TCultureList object can hold.
    /// </summary>
    {$endregion}
    property Capacity: Integer read GetCapacity write SetCapacity;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether duplicate <see cref="TCultureInfo"/> objects can be added to the list.
    /// </summary>
    {$endregion}
    property Duplicates: TDuplicates read fDuplicates write fDuplicates;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately after the list of <see cref="TCultureInfo"/> objects changes.
    /// </summary>
    {$endregion}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately before the list of <see cref="TCultureInfo"/> objects changes.
    /// </summary>
    {$endregion}
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the possible display names for
  /// <see cref="TTerritoryInfo"/> objects.
  /// </summary>
  {$endregion}
  TTerritoryDisplayName = (
    {$region 'xmldoc'}
    /// Nothing
    {$endregion}
    tnNone,
    {$region 'xmldoc'}
    /// Two characters of the international country code specified in ISO 3166-2.
    {$endregion}
    tnCode2,
    {$region 'xmldoc'}
    /// Three characters of the international country code specified in ISO 3166-3.
    {$endregion}
    tnCode3,
    {$region 'xmldoc'}
    /// The friendly name of the nation, for example, Germany.
    {$endregion}
    tnFriendlyName,
    {$region 'xmldoc'}
    /// The official name of the nation, for example, Federal Republic of Germany.
    {$endregion}
    tnOfficialName,
    {$region 'xmldoc'}
    /// Full localized (in Windows user's language) name of the country/region, for
    /// example, Deutschland for Germany.
    {$endregion}
    tnLocalizedName,
    {$region 'xmldoc'}
    /// English name of the country/region, for example, Germany for Deutschland.
    {$endregion}
    tnEnglishName,
    {$region 'xmldoc'}
    /// Native name of the country/region, for example, Deutschland for Germany.
    {$endregion}
    tnNativeName
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This class represents a country/region.
  /// </summary>
  /// <remarks>
  /// Each instance of TTerritoryInfo class represents a specific geographical location,
  /// and has properties and methods to query information about it.
  ///
  /// NOTE: Do not create or destroy an instance of TTerritoryInfo class. Use <see cref="TerritoryOf"/>
  /// global function to get TTerritoryInfo instance of a geographical location.
  /// </remarks>
  /// <seealso cref="TerritoryOf"/>
  /// <seealso cref="World.Territories"/>
  {$endregion}
  TTerritoryInfo = class sealed(TObject)
  private
    fGeoID: GEOID;
    fCode2: String;
    fCode3: String;
    fLatitude: Double;
    fLongitude: Double;
    fFriendlyName: String;
    fOfficialName: String;
    fLocalizedName: String;
    fEnglishName: String;
    fCurrencies: TCurrencyList;
    fCultures: TCultureList;
    function GetNativeName: String;
    function GetCultures: TReadonlyCultureList; inline;
    function GetCurrencies: TReadonlyCurrencyList; inline;
    function GetDisplayNames(DisplayName: TTerritoryDisplayName): String;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Associates the specified <see cref="TCultureInfo"/> and <see cref="TCurrencyInfo"/>
    /// objects to this TTerritoryInfo object.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object.
    /// </param>
    /// <param name="Currency">
    /// The <see cref="TCurrencyInfo"/> object.
    /// </param>
    {$endregion}
    procedure Associate(Culture: TCultureInfo; Currency: TCurrencyInfo);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the object for a specified geographical identifier.
    /// </summary>
    /// <param name="AGeoID">
    /// The geographical identifier.
    /// </param>
    /// <seealso cref="CreateByLocaleID"/>
    /// <seealso cref="CreateByLocale"/>
    {$endregion}
    constructor Create(AGeoID: GEOID);
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the object for a specified locale identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    /// <param name="Dummy">
    /// This parameter is for compatibility with c++.
    /// </param>
    /// <seealso cref="Create"/>
    /// <seealso cref="CreateByLocale"/>
    {$endregion}
    constructor CreateByLocaleID(LocaleID: LCID; Dummy: Integer = 0);
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the object for a specified locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <seealso cref="Create"/>
    /// <seealso cref="CreateByLocaleID"/>
    {$endregion}
    constructor CreateByLocale(const Locale: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the object and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the geographical identifier of the country/region.
    /// </summary>
    {$endregion}
    property GeoID: GEOID read fGeoID;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the two-character ISO 3166-2 international country code.
    /// </summary>
    {$endregion}
    property Code2: String read fCode2;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the three-character ISO 3166-3 international country code.
    /// </summary>
    {$endregion}
    property Code3: String read fCode3;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the geographical latitude of the territory's capital, in degrees.
    /// </summary>
    {$endregion}
    property Latitude: Double read fLatitude;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the geographical longitude of the territory's capital, in degrees.
    /// </summary>
    {$endregion}
    property Longitude: Double read fLongitude;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the friendly name of the nation.
    /// </summary>
    {$endregion}
    property FriendlyName: String read fFriendlyName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the official name of the nation.
    /// </summary>
    {$endregion}
    property OfficialName: String read fOfficialName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the localized (in Windows user's language) name of the country/region.
    /// </summary>
    {$endregion}
    property LocalizedName: String read fLocalizedName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the English name of the country/region.
    /// </summary>
    {$endregion}
    property EnglishName: String read fEnglishName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the Native name of the country/region.
    /// </summary>
    {$endregion}
    property NativeName: String read GetNativeName;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists different cultures in this country/region.
    /// </summary>
    {$endregion}
    property Cultures: TReadonlyCultureList read GetCultures;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists authoritative currencies in this country/region.
    /// </summary>
    {$endregion}
    property Currencies: TReadonlyCurrencyList read GetCurrencies;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists display names of the country/region.
    /// </summary>
    {$endregion}
    property DisplayNames[DisplayName: TTerritoryDisplayName]: String read GetDisplayNames;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TTerritoryEnumerator enumerates through the <see cref="TTerritoryInfo"/> objects
  /// of an instance of <see cref="TReadonlyTerritoryList"/> or its descendent class.
  /// </summary>
  {$endregion}
  TTerritoryEnumerator = class(TListEnumerator)
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for a specified list.
    /// </summary>
    /// <param name="list">
    /// The list to enumerate its items.
    /// </param>
    {$endregion}
    constructor Create(List: TReadonlyTerritoryList);
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the current <see cref="TTerritoryInfo"/> object of the list being
    /// enumerated.
    /// </summary>
    /// <returns>
    /// Returns the current <see cref="TTerritoryInfo"/> object.
    /// </returns>
    {$endregion}
    function GetCurrent: TTerritoryInfo; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the current <see cref="TTerritoryInfo"/> object.
    /// </summary>
    {$endregion}
    property Current: TTerritoryInfo read GetCurrent;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class maintains a read-only list of <see cref="TTerritoryInfo"/> objects.
  /// </summary>
  /// <remarks>
  /// TReadonlyTerritoryList maintains a read-only list of <see cref="TTerritoryInfo"/>
  /// objects, and provides properties and methods to locate and access the objects.
  /// The objects in the list are hashed and can be located quickly.
  /// </remarks>
  /// <see cref="TTerritoryList"/>
  {$endregion}
  TReadonlyTerritoryList = class(TPersistent)
  private
    LookupTable_GeoID: TKeyLookup<GEOID,Integer>;
    LookupTable_Code2: TKeyLookup<String,Integer>;
    LookupTable_Code3: TKeyLookup<String,Integer>;
    IsLookupTableValid_GeoID: Boolean;
    IsLookupTableValid_Code2: Boolean;
    IsLookupTableValid_Code3: Boolean;
    function GetCount: Integer; inline;
    function GetItems(Index: Integer): TTerritoryInfo; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Provides direct access to the individual <see cref="TTerritoryInfo"/> objects.
    /// </summary>
    {$endregion}
    List: TList;
    {$region 'xmldoc'}
    /// <summary>
    /// Attaches a specified <see cref="TTerritoryInfo"/> object with a specified index
    /// in the list.
    /// </summary>
    /// <remarks>
    /// <para>
    /// NOTE: This method does not add the object to the list, it only creates its lookup
    /// entries for all the lookup tables.
    /// </para>
    /// </remarks>
    /// <param name="Territory">
    /// The <see cref="TTerritoryInfo"/> object to add to the lookup tables.
    /// </param>
    /// <param name="Index">
    /// The index of the <see cref="TTerritoryInfo"/> object in the list.
    /// </param>
    /// <seealso cref="RemoveLookupEntries"/>
    {$endregion}
    procedure AddLookupEntries(Territory: TTerritoryInfo; Index: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Detaches a specified <see cref="TTerritoryInfo"/> object from the previously attached
    /// index.
    /// </summary>
    /// <remarks>
    /// <para>
    /// NOTE: This method does not remove the object from the list, it only deletes its
    /// lookup entries from all lookup tables.
    /// </para>
    /// </remarks>
    /// <param name="Territory">
    /// The <see cref="TTerritoryInfo"/> object to remove from the lookup tables.
    /// </param>
    /// <seealso cref="AddLookupEntries"/>
    {$endregion}
    procedure RemoveLookupEntries(Territory: TTerritoryInfo);
    {$region 'xmldoc'}
    /// <summary>
    /// Clears all entries from all lookup tables.
    /// </summary>
    {$endregion}
    procedure ClearLookupTables;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules rebuilding of all lookup tables.
    /// </summary>
    {$endregion}
    procedure InvalidateLookupTables; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Validates lookup table for <see cref="TTerritoryInfo.GeoID"/> values.
    /// </summary>
    {$endregion}
    procedure ValidateLookupTable_GeoID;
    {$region 'xmldoc'}
    /// <summary>
    /// Validates lookup table for <see cref="TTerritoryInfo.Code2"/> values.
    /// </summary>
    {$endregion}
    procedure ValidateLookupTable_Code2;
    {$region 'xmldoc'}
    /// <summary>
    /// Validates lookup table for <see cref="TTerritoryInfo.Code3"/> values.
    /// </summary>
    {$endregion}
    procedure ValidateLookupTable_Code3;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TTerritoryEnumerator"/> reference, which enumerates
    /// all <see cref="TTerritoryInfo"/> objects in the list.
    /// </summary>
    /// <returns>
    /// Returns an instance of <see cref="TTerritoryEnumerator"/> class.
    /// </returns>
    {$endregion}
    function GetEnumerator: TTerritoryEnumerator;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the first <see cref="TTerritoryInfo"/> object in the list.
    /// </summary>
    /// <returns>
    /// The first <see cref="TTerritoryInfo"/> object in the list.
    /// </returns>
    /// <seealso cref="Last"/>
    {$endregion}
    function First: TTerritoryInfo;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the last <see cref="TTerritoryInfo"/> object in the list.
    /// </summary>
    /// <returns>
    /// The last <see cref="TTerritoryInfo"/> object in the list.
    /// </returns>
    /// <seealso cref="First"/>
    {$endregion}
    function Last: TTerritoryInfo;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies <see cref="TTerritoryInfo"/> objects in the list to another object.
    /// </summary>
    /// <param name="Dest">
    /// The destination object.
    /// </param>
    /// <seealso cref="AssignLabelsTo"/>
    {$endregion}
    procedure AssignTo(Dest: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies display name of <see cref="TTerritoryInfo"/> objects in the list to
    /// a <see cref="TStrings"/> object.
    /// </summary>
    /// <param name="Dest">
    /// The destination <see cref="TStrings"/> object.
    /// </param>
    /// <param name="DisplayName">
    /// Determines which <see cref="TTerritoryInfo.DisplayNames"/> value should be used.
    /// </param>
    /// <seealso cref="AssignTo"/>
    /// <seealso cref="TTerritoryInfo.DisplayNames"/>
    {$endregion}
    procedure AssignLabelsTo(Dest: TStrings; DisplayName: TTerritoryDisplayName);
    {$region 'xmldoc'}
    /// <summary>
    /// Locates a <see cref="TTerritoryInfo"/> object in the list by its geographical identifier.
    /// </summary>
    /// <param name="GeoID">
    /// The geographical identifier.
    /// </param>
    /// <returns>
    /// The <see cref="TTerritoryInfo"/> object with the specified geographical identifier
    /// or <see langword="nil"/> if the object is not found.
    /// </returns>
    {$endregion}
    function Find(GeoID: GEOID): TTerritoryInfo; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Locates a <see cref="TTerritoryInfo"/> object in the list by its two or three
    /// characters international country/region code.
    /// </summary>
    /// <param name="Code">
    /// The two or three characters international code.
    /// </param>
    /// <returns>
    /// The <see cref="TTerritoryInfo"/> object with the specified international code
    /// or <see langword="nil"/> if the object is not found.
    /// </returns>
    {$endregion}
    function Find(const Code: String): TTerritoryInfo; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a specified <see cref="TTerritoryInfo"/> object in the list.
    /// </summary>
    /// <param name="Territory">
    /// The <see cref="TTerritoryInfo"/> object to locate.
    /// </param>
    /// <returns>
    /// The index of <see cref="TTerritoryInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOfName"/>
    {$endregion}
    function IndexOf(Territory: TTerritoryInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TTerritoryInfo"/> object in the list by its geographical
    /// identifier.
    /// </summary>
    /// <param name="GeoID">
    /// The geographical identifier.
    /// </param>
    /// <returns>
    /// The index of <see cref="TTerritoryInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOfName"/>
    {$endregion}
    function IndexOf(GeoID: GEOID): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TTerritoryInfo"/> object in the list by its two or three
    /// characters international code.
    /// </summary>
    /// <param name="Code">
    /// The two or three characters international code.
    /// </param>
    /// <returns>
    /// The index of <see cref="TTerritoryInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOfName"/>
    {$endregion}
    function IndexOf(const Code: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TTerritoryInfo"/> object in the list by its display name.
    /// </summary>
    /// <param name="AName">
    /// The display name of the target object.
    /// </param>
    /// <param name="DisplayName">
    /// Indicates which <see cref="TTerritoryInfo.DisplayNames"/> value should be used.
    /// </param>
    /// <returns>
    /// The index of <see cref="TTerritoryInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOf"/>
    {$endregion}
    function IndexOfName(const AName: String; DisplayName: TTerritoryDisplayName): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified <see cref="TTerritoryInfo"/> object is in the list.
    /// </summary>
    /// <param name="Territory">
    /// The <see cref="TTerritoryInfo"/> object to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TTerritoryInfo"/> object is in the
    /// list, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Exists(Territory: TTerritoryInfo): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a <see cref="TTerritoryInfo"/> object with a specified geographical
    /// identifier is in the list.
    /// </summary>
    /// <param name="GeoID">
    /// The geographical identifier.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if a <see cref="TTerritoryInfo"/> object with the
    /// specified geographical identifier is in the list, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Exists(GeoID: GEOID): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a <see cref="TTerritoryInfo"/> object with a specified two or three
    /// characters international code is in the list.
    /// </summary>
    /// <param name="Code">
    /// The two or three characters international code.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if a <see cref="TTerritoryInfo"/> object with the
    /// specified international code is in the list, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Exists(const Code: String): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of <see cref="TTerritoryInfo"/> objects in the list.
    /// </summary>
    {$endregion}
    property Count: Integer read GetCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TTerritoryInfo"/> objects in the list.
    /// </summary>
    {$endregion}
    property Items[Index: Integer]: TTerritoryInfo read GetItems; default;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class maintains a list of <see cref="TTerritoryInfo"/> objects.
  /// </summary>
  /// <remarks>
  /// Use TTerritoryList to store and maintain a list of <see cref="TTerritoryInfo"/>
  /// objects. TTerritoryList provides properties and methods to add, delete, rearrange,
  /// locate, access, and sort <see cref="TTerritoryInfo"/> objects.
  /// </remarks>
  {$endregion}
  TTerritoryList = class(TReadonlyTerritoryList)
  private
    fDuplicates: TDuplicates;
    fUpdateCount: Integer;
    fOnChanging: TNotifyEvent;
    fOnChange: TNotifyEvent;
    function GetCapacity: Integer; inline;
    procedure SetCapacity(Value: Integer);
    procedure ReadTerritory(Reader: TReader);
    procedure WriteTerritory(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="Items"/> property as if it were published.
    /// </summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the list.
    /// </param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChanging"/> event.
    /// </summary>
    /// <seealso cref="Change"/>
    {$endregion}
    procedure Changing; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChange"/> event.
    /// </summary>
    /// <seealso cref="Changing"/>
    {$endregion}
    procedure Change; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the update level.
    /// </summary>
    {$endregion}
    property UpdateCount: Integer read fUpdateCount;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents generating of the <see cref="OnChanging"/> and <see cref="OnChange"/>
    /// events until the <see cref="EndUpdate"/> method is called.
    /// </summary>
    /// <seealso cref="EndUpdate"/>
    {$endregion}
    procedure BeginUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Reenables <see cref="OnChanging"/> and <see cref="OnChange"/> events generation
    /// that was turned off with the <see cref="BeginUpdate"/> method.
    /// </summary>
    /// <seealso cref="BeginUpdate"/>
    {$endregion}
    procedure EndUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes all items from the list.
    /// </summary>
    {$endregion}
    procedure Clear;
    {$region 'xmldoc'}
    /// <summary>
    /// Sorts the items in the list by a specified display name.
    /// </summary>
    /// <param name="OrderBy">
    /// Determines which <see cref="TTerritoryInfo.DisplayNames"/> value should the
    /// list be sorted by.
    /// </param>
    {$endregion}
    procedure Sort(OrderBy: TTerritoryDisplayName);
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the list of <see cref="TTerritoryInfo"/> objects from another object.
    /// </summary>
    /// <param name="Source">
    /// The source list.
    /// </param>
    /// <seealso cref="Apply"/>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies <see cref="TTerritoryInfo"/> objects of one list to another.
    /// </summary>
    /// <remarks>
    /// Call Apply to assign the elements of another list to this one. Assign combines
    /// the source list with this one using the logical operator specified by the
    /// <paramref name="Op"/> parameter.
    ///
    /// If the <paramref name="ListB"/> parameter is specified or not <see langword="nil"/>,
    /// then Apply first replaces all the elements of this list with those in <paramref name="ListA"/>,
    /// and then merges <paramref name="ListB"/> into this list using the operator specified
    /// by <paramref name="Op"/>.
    ///
    /// These operators are acceptable in Apply method:
    ///
    /// <list type="table">
    ///   <listheader>
    ///     <term>Operator</term>
    ///     <description>Description</description>
    ///   </listheader>
    ///   <item>
    ///     <term>laCopy</term>
    ///     <description>Destination becomes a copy of the source</description>
    ///   </item>
    ///   <item>
    ///     <term>laAnd</term>
    ///     <description>Intersection of the two lists</description>
    ///   </item>
    ///   <item>
    ///     <term>laOr</term>
    ///     <description>Union of the two lists</description>
    ///   </item>
    ///   <item>
    ///     <term>laXor</term>
    ///     <description>Only those not in both lists</description>
    ///   </item>
    ///   <item>
    ///     <term>laSrcUnique</term>
    ///     <description>Only those unique to source (same as laAnd followed by laXor)</description>
    ///   </item>
    ///   <item>
    ///     <term>laDestUnique</term>
    ///     <description>Only those unique to destination (same as laOr followed by laXor)</description>
    ///   </item>
    /// </list>
    ///
    /// </remarks>
    /// <param name="ListA">
    /// The source list.
    /// </param>
    /// <param name="Op">
    /// The operator that indicates how the two lists should be merged.
    /// </param>
    /// <param name="ListB">
    /// The optional and aditional source list.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure Apply(ListA: TReadonlyTerritoryList; Op: TListAssignOp = laCopy;
      ListB: TReadonlyTerritoryList = nil);
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a specified <see cref="TTerritoryInfo"/> object to the list.
    /// </summary>
    /// <param name="Territory">
    /// The <see cref="TTerritoryInfo"/> object to add.
    /// </param>
    /// <returns>
    /// The index of new <see cref="TTerritoryInfo"/> object in the list.
    /// </returns>
    {$endregion}
    function Add(Territory: TTerritoryInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a <see cref="TTerritoryInfo"/> object to the list by its geographical identifier.
    /// </summary>
    /// <param name="GeoID">
    /// The geographical identifier.
    /// </param>
    /// <returns>
    /// The index of new <see cref="TTerritoryInfo"/> object in the list.
    /// </returns>
    {$endregion}
    function Add(GeoID: GEOID): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a <see cref="TTerritoryInfo"/> object to the list by its two or three
    /// character international code.
    /// </summary>
    /// <param name="Code">
    /// The two or three character international code.
    /// </param>
    /// <returns>
    /// The index of new <see cref="TTerritoryInfo"/> object in the list.
    /// </returns>
    {$endregion}
    function Add(const Code: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a specified <see cref="TTerritoryInfo"/> object from the list.
    /// </summary>
    /// <param name="Territory">
    /// The <see cref="TTerritoryInfo"/> object to remove.
    /// </param>
    /// <returns>
    /// The index of the removed <see cref="TTerritoryInfo"/> object, or -1 if the object
    /// is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Remove(Territory: TTerritoryInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a <see cref="TTerritoryInfo"/> object from the list by its geographical
    /// identifier.
    /// </summary>
    /// <param name="GeoID">
    /// The geographical identifier.
    /// </param>
    /// <returns>
    /// The index of the removed <see cref="TTerritoryInfo"/> object, or -1 if the object
    /// is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Remove(GeoID: GEOID): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a <see cref="TTerritoryInfo"/> object from the list by its two or three
    /// characters international code.
    /// </summary>
    /// <param name="Code">
    /// The two or three character sinternational code.
    /// </param>
    /// <returns>
    /// The index of the removed <see cref="TTerritoryInfo"/> object, or -1 if the object
    /// is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Remove(const Code: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a <see cref="TTerritoryInfo"/> object at a specified index of the list.
    /// </summary>
    /// <param name="Index">
    /// The index of <see cref="TTerritoryInfo"/> object to remove.
    /// </param>
    /// <seealso cref="Remove"/>
    {$endregion}
    procedure Delete(Index: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of <see cref="TTerritoryInfo"/> objects the TTerritoryList object can hold.
    /// </summary>
    {$endregion}
    property Capacity: Integer read GetCapacity write SetCapacity;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether duplicate <see cref="TTerritoryInfo"/> objects can be added to the list.
    /// </summary>
    {$endregion}
    property Duplicates: TDuplicates read fDuplicates write fDuplicates;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately after the list of <see cref="TTerritoryInfo"/> objects changes.
    /// </summary>
    {$endregion}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately before the list of <see cref="TTerritoryInfo"/> objects changes.
    /// </summary>
    {$endregion}
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the possible display names for
  /// <see cref="TCurrencyInfo"/> objects.
  /// </summary>
  {$endregion}
  TCurrencyDisplayName = (
    {$region 'xmldoc'}
    /// Nothing
    {$endregion}
    crnNone,
    {$region 'xmldoc'}
    /// Three characters of the international monetary symbol specified in ISO 4217.
    {$endregion}
    crnIntlSymbol,
    {$region 'xmldoc'}
    /// The full English name of the currency.
    {$endregion}
    crnEnglishName,
    {$region 'xmldoc'}
    /// The native name of the currency.
    {$endregion}
    crnNativeName
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This class represents a currency.
  /// </summary>
  /// <remarks>
  /// Each instance of TCurrencyInfo class represents a specific currency and has properties
  /// and methods to query information about it.
  ///
  /// NOTE: Do not create or destroy an instance of TCurrencyInfo class. Use <see cref="CurrencyOf"/>
  /// global function to get TCurrencyInfo instance of a currency.
  /// </remarks>
  /// <seealso cref="CurrencyOf"/>
  /// <seealso cref="World.Currencies"/>
  {$endregion}
  TCurrencyInfo = class sealed(TObject)
  private
    fIntlSymbol: String;
    fLocalSymbol: String;
    fEnglishName: String;
    fCultures: TCultureList;
    fCountries: TTerritoryList;
    fOriginCountry: TTerritoryInfo;
    function GetNativeName: String; inline;
    function GetCultures: TReadonlyCultureList; inline;
    function GetCountries: TReadonlyTerritoryList; inline;
    function GetDisplayNames(DisplayName: TCurrencyDisplayName): String;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Associates the specified <see cref="TCultureInfo"/> and <see cref="TTerritoryInfo"/>
    /// objects to this TCurrencyInfo object.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object.
    /// </param>
    /// <param name="Territory">
    /// The <see cref="TTerritoryInfo"/> object.
    /// </param>
    {$endregion}
    procedure Associate(Culture: TCultureInfo; Territory: TTerritoryInfo);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the object for a specified locale identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    {$endregion}
    constructor Create(LocaleID: LCID); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the object for a specified locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    {$endregion}
    constructor Create(const Locale: String); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroyes the object and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies the three-character ISO 4217 international monetary symbol of
    /// the currency.
    /// </summary>
    {$endregion}
    property IntlSymbol: String read fIntlSymbol;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the local monetary symbol of the currency.
    /// </summary>
    {$endregion}
    property LocalSymbol: String read fLocalSymbol;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the English name of the currency.
    /// </summary>
    {$endregion}
    property EnglishName: String read fEnglishName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the native name of the currency.
    /// </summary>
    {$endregion}
    property NativeName: String read GetNativeName;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists cultures that this currency is authoritative in.
    /// </summary>
    {$endregion}
    property Cultures: TReadonlyCultureList read GetCultures;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists countries/regions that this currency is authoritative in.
    /// </summary>
    {$endregion}
    property Countries: TReadonlyTerritoryList read GetCountries;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the main country/region of the currency. The value of this property
    /// can be <see langword="nil"/>. For example, Euro has no main country.
    /// </summary>
    {$endregion}
    property OriginCountry: TTerritoryInfo read fOriginCountry;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists display names of the currency.
    /// </summary>
    {$endregion}
    property DisplayNames[DisplayName: TCurrencyDisplayName]: String read GetDisplayNames;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCurrencyEnumerator enumerates through the <see cref="TCurrencyInfo"/> objects
  /// of an instance of <see cref="TReadonlyCurrencyList"/> or its descendent class.
  /// </summary>
  {$endregion}
  TCurrencyEnumerator = class(TListEnumerator)
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for a specified list.
    /// </summary>
    /// <param name="list">
    /// The list to enumerate its items.
    /// </param>
    {$endregion}
    constructor Create(List: TReadonlyCurrencyList);
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the current <see cref="TCurrencyInfo"/> object of the list being
    /// enumerated.
    /// </summary>
    /// <returns>
    /// Returns the current <see cref="TCurrencyInfo"/> object.
    /// </returns>
    {$endregion}
    function GetCurrent: TCurrencyInfo; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the current <see cref="TCurrencyInfo"/> object.
    /// </summary>
    {$endregion}
    property Current: TCurrencyInfo read GetCurrent;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class maintains a read-only list of <see cref="TCurrencyInfo"/> objects.
  /// </summary>
  /// <remarks>
  /// TReadonlyCurrencyList maintains a read-only list of <see cref="TCurrencyInfo"/>
  /// objects, and provides properties and methods to locate and access the objects.
  /// The objects in the list are hashed and can be located quickly.
  /// </remarks>
  /// <see cref="TCurrencyList"/>
  {$endregion}
  TReadonlyCurrencyList = class(TPersistent)
  private
    LookupTable_IntlSymbol: TKeyLookup<String,Integer>;
    IsLookupTableValid_IntlSymbol: Boolean;
    function GetCount: Integer; inline;
    function GetItems(Index: Integer): TCurrencyInfo; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Provides direct accress to the individual <see cref="TCurrencyInfo"/> objects.
    /// </summary>
    {$endregion}
    List: TList;
    {$region 'xmldoc'}
    /// <summary>
    /// Attaches a specified <see cref="TCurrencyInfo"/> object with a specified index
    /// in the list.
    ///
    /// NOTE: This method does not add the object to the list, it only creats its lookup
    /// entries for all the lookup tables.
    /// </summary>
    /// <param name="Currency">
    /// The <see cref="TCurrencyInfo"/> object to add to the lookup tables.
    /// </param>
    /// <param name="Index">
    /// The index of the <see cref="TCurrencyInfo"/> object in the list.
    /// </param>
    /// <seealso cref="RemoveLookupEntries"/>
    {$endregion}
    procedure AddLookupEntries(Currency: TCurrencyInfo; Index: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Detaches a specified <see cref="TCurrencyInfo"/> object from the previosuly
    /// attached index.
    ///
    /// NOTE: This method does not remove the object from the list, it only deletes its
    /// lookup entries from all lookup tables.
    /// </summary>
    /// <param name="Currency">
    /// The <see cref="TCurrencyInfo"/> object to remove from the lookup tables.
    /// </param>
    /// <seealso cref="AddLookupEntries"/>
    {$endregion}
    procedure RemoveLookupEntries(Currency: TCurrencyInfo);
    {$region 'xmldoc'}
    /// <summary>
    /// Clears all entries from all lookup tables.
    /// </summary>
    {$endregion}
    procedure ClearLookupTables;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules rebuilding of all lookup tables.
    /// </summary>
    {$endregion}
    procedure InvalidateLookupTables; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Validates lookup table for <see cref="TCurrencyInfo.IntlSymbol"/> values.
    /// </summary>
    {$endregion}
    procedure ValidateLookupTable_IntlSymbol;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creats an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TCurrencyEnumerator"/> reference, which enumerates
    /// all <see cref="TCurrencyInfo"/> objects in the list.
    /// </summary>
    /// <returns>
    /// Returns an instance of <see cref="TCurrencyEnumerator"/> class.
    /// </returns>
    {$endregion}
    function GetEnumerator: TCurrencyEnumerator;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the first <see cref="TCurrencyInfo"/> object in the list.
    /// </summary>
    /// <returns>
    /// The first <see cref="TCurrencyInfo"/> object in the list.
    /// </returns>
    /// <seealso cref="Last"/>
    {$endregion}
    function First: TCurrencyInfo;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the last <see cref="TCurrencyInfo"/> object in the list.
    /// </summary>
    /// <returns>
    /// The last <see cref="TCurrencyInfo"/> object in the list.
    /// </returns>
    /// <seealso cref="First"/>
    {$endregion}
    function Last: TCurrencyInfo;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies <see cref="TCurrencyInfo"/> objects in the list to another object.
    /// </summary>
    /// <param name="Dest">
    /// The destination object.
    /// </param>
    /// <seealso cref="AssignLabelsTo"/>
    {$endregion}
    procedure AssignTo(Dest: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies display name of <see cref="TCurrencyInfo"/> objects in the list to
    /// a <see cref="TStrings"/> object.
    /// </summary>
    /// <param name="Dest">
    /// The destination <see cref="TStrings"/> object.
    /// </param>
    /// <param name="DisplayName">
    /// Determines which <see cref="TCurrencyInfo.DisplayNames"/> value should be used.
    /// </param>
    /// <seealso cref="AssignTo"/>
    /// <seealso cref="TCurrencyInfo.DisplayNames"/>
    {$endregion}
    procedure AssignLabelsTo(Dest: TStrings; DisplayName: TCurrencyDisplayName);
    {$region 'xmldoc'}
    /// <summary>
    /// Locates a <see cref="TCurrencyInfo"/> object in the list by its three characters
    /// international monetary symbol.
    /// </summary>
    /// <param name="IntlSymbol">
    /// The international monetary sumbol.
    /// </param>
    /// <returns>
    /// The <see cref="TCurrencyInfo"/> object with the specified international monetary
    /// sumbol or <see langword="nil"/> if the object is not found.
    /// </returns>
    {$endregion}
    function Find(const IntlSymbol: String): TCurrencyInfo;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a specified <see cref="TCurrencyInfo"/> object in the list.
    /// </summary>
    /// <param name="Currency">
    /// The <see cref="TCurrencyInfo"/> object to locate.
    /// </param>
    /// <returns>
    /// The index of <see cref="TCurrencyInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOfName"/>
    {$endregion}
    function IndexOf(Currency: TCurrencyInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TCurrencyInfo"/> object in the list by its three
    /// characters international monetary symbol.
    /// </summary>
    /// <param name="IntlSymbol">
    /// The international monetary symbol.
    /// </param>
    /// <returns>
    /// The index of <see cref="TCurrencyInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOfName"/>
    {$endregion}
    function IndexOf(const IntlSymbol: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a <see cref="TCurrencyInfo"/> object in the list by its display name.
    /// </summary>
    /// <param name="AName">
    /// The display name of the target object.
    /// </param>
    /// <param name="DisplayName">
    /// Indicates which <see cref="TCurrencyInfo.DisplayNames"/> value should be used.
    /// </param>
    /// <returns>
    /// The index of <see cref="TCurrencyInfo"/> object or -1 if the object is not found.
    /// </returns>
    /// <seealso cref="IndexOf"/>
    {$endregion}
    function IndexOfName(const AName: String; DisplayName: TCurrencyDisplayName): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified <see cref="TCurrencyInfo"/> object is in the list.
    /// </summary>
    /// <param name="Currency">
    /// The <see cref="TCurrencyInfo"/> object to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TCurrencyInfo"/> object is in the
    /// list, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Exists(Currency: TCurrencyInfo): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a <see cref="TCurrencyInfo"/> object with a specified three characters
    /// international monetary symbol is in the list.
    /// </summary>
    /// <param name="IntlSymbol">
    /// The international monetary symbol.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if a <see cref="TCurrencyInfo"/> object with the
    /// specified international monetary symbol is in the list, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Exists(const IntlSymbol: String): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of <see cref="TCurrencyInfo"/> objects in the list.
    /// </summary>
    {$endregion}
    property Count: Integer read GetCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCurrencyInfo"/> objects in the list.
    /// </summary>
    {$endregion}
    property Items[Index: Integer]: TCurrencyInfo read GetItems; default;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class maintains a list of <see cref="TCurrencyInfo"/> objects.
  /// </summary>
  /// <remarks>
  /// Use TCurrencyList to store and maintain a list of <see cref="TCurrencyInfo"/>
  /// objects. TCurrencyList provides properties and methods to add, delete, rearrange,
  /// locate, access, and sort <see cref="TCurrencyInfo"/> objects.
  /// </remarks>
  {$endregion}
  TCurrencyList = class(TReadonlyCurrencyList)
  private
    fDuplicates: TDuplicates;
    fUpdateCount: Integer;
    fOnChanging: TNotifyEvent;
    fOnChange: TNotifyEvent;
    function GetCapacity: Integer; inline;
    procedure SetCapacity(Value: Integer);
    procedure ReadCurrencies(Reader: TReader);
    procedure WriteCurrencies(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="Items"/> property as if it were published.
    /// </summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the list.
    /// </param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChanging"/> event.
    /// </summary>
    /// <seealso cref="Change"/>
    {$endregion}
    procedure Changing; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChange"/> event.
    /// </summary>
    /// <seealso cref="Changing"/>
    {$endregion}
    procedure Change; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the update level.
    /// </summary>
    {$endregion}
    property UpdateCount: Integer read fUpdateCount;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents generating of the <see cref="OnChanging"/> and <see cref="OnChange"/>
    /// events until the <see cref="EndUpdate"/> method is called.
    /// </summary>
    /// <seealso cref="EndUpdate"/>
    {$endregion}
    procedure BeginUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Reenables <see cref="OnChanging"/> and <see cref="OnChange"/> events generation
    /// that was turned off with the <see cref="BeginUpdate"/> method.
    /// </summary>
    /// <seealso cref="BeginUpdate"/>
    {$endregion}
    procedure EndUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes all items from the list.
    /// </summary>
    {$endregion}
    procedure Clear;
    {$region 'xmldoc'}
    /// <summary>
    /// Sorts the items in the list by a specified display name.
    /// </summary>
    /// <param name="OrderBy">
    /// Determines which <see cref="TCurrencyInfo.DisplayNames"/> value should the
    /// list be sorted by.
    /// </param>
    {$endregion}
    procedure Sort(OrderBy: TCurrencyDisplayName);
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the list of <see cref="TCurrencyInfo"/> objects from another object.
    /// </summary>
    /// <param name="Source">
    /// The source list.
    /// </param>
    /// <seealso cref="Apply"/>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies <see cref="TCurrencyInfo"/> objects of one list to another.
    /// </summary>
    /// <remarks>
    /// Call Apply to assign the elements of another list to this one. Assign combines
    /// the source list with this one using the logical operator specified by the
    /// <paramref name="Op"/> parameter.
    ///
    /// If the <paramref name="ListB"/> parameter is specified or not <see langword="nil"/>,
    /// then Apply first replaces all the elements of this list with those in <paramref name="ListA"/>,
    /// and then merges <paramref name="ListB"/> into this list using the operator specified
    /// by <paramref name="Op"/>.
    ///
    /// These operators are acceptable in Apply method:
    ///
    /// <list type="table">
    ///   <listheader>
    ///     <term>Operator</term>
    ///     <description>Description</description>
    ///   </listheader>
    ///   <item>
    ///     <term>laCopy</term>
    ///     <description>Destination becomes a copy of the source</description>
    ///   </item>
    ///   <item>
    ///     <term>laAnd</term>
    ///     <description>Intersection of the two lists</description>
    ///   </item>
    ///   <item>
    ///     <term>laOr</term>
    ///     <description>Union of the two lists</description>
    ///   </item>
    ///   <item>
    ///     <term>laXor</term>
    ///     <description>Only those not in both lists</description>
    ///   </item>
    ///   <item>
    ///     <term>laSrcUnique</term>
    ///     <description>Only those unique to source (same as laAnd followed by laXor)</description>
    ///   </item>
    ///   <item>
    ///     <term>laDestUnique</term>
    ///     <description>Only those unique to destination (same as laOr followed by laXor)</description>
    ///   </item>
    /// </list>
    ///
    /// </remarks>
    /// <param name="ListA">
    /// The source list.
    /// </param>
    /// <param name="Op">
    /// The operator that indicates how the two lists should be merged.
    /// </param>
    /// <param name="ListB">
    /// The optional and aditional source list.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure Apply(ListA: TReadonlyCurrencyList; Op: TListAssignOp = laCopy;
      ListB: TReadonlyCurrencyList = nil);
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a specified <see cref="TCurrencyInfo"/> object to the list.
    /// </summary>
    /// <param name="Currency">
    /// The <see cref="TCurrencyInfo"/> object to add.
    /// </param>
    /// <returns>
    /// The index of new <see cref="TCurrencyInfo"/> object in the list.
    /// </returns>
    {$endregion}
    function Add(Currency: TCurrencyInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a <see cref="TCurrencyInfo"/> object to the list by its three characters
    /// international monetary symbol.
    /// </summary>
    /// <param name="IntlSymbol">
    /// The international monetary symbol.
    /// </param>
    /// <returns>
    /// The index of new <see cref="TCurrencyInfo"/> object in the list.
    /// </returns>
    {$endregion}
    function Add(const IntlSymbol: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a specified <see cref="TCurrencyInfo"/> object from the list.
    /// </summary>
    /// <param name="Currency">
    /// The <see cref="TCurrencyInfo"/> object to remove.
    /// </param>
    /// <returns>
    /// The index of the removed <see cref="TCurrencyInfo"/> object, or -1 if the object
    /// is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Remove(Currency: TCurrencyInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a <see cref="TCurrencyInfo"/> object from the list by its three characters
    /// international monetry symbol.
    /// </summary>
    /// <param name="IntlSymbol">
    /// The international monetry symbol.
    /// </param>
    /// <returns>
    /// The index of the removed <see cref="TCurrencyInfo"/> object, or -1 if the object
    /// is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Remove(const IntlSymbol: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a <see cref="TCurrencyInfo"/> object at a specified index of the list.
    /// </summary>
    /// <param name="Index">
    /// The index of <see cref="TCurrencyInfo"/> object to remove.
    /// </param>
    /// <seealso cref="Remove"/>
    {$endregion}
    procedure Delete(Index: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of <see cref="TCurrencyInfo"/> objects the TCurrencyList object can hold.
    /// </summary>
    {$endregion}
    property Capacity: Integer read GetCapacity write SetCapacity;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether duplicate <see cref="TCurrencyInfo"/> objects can be added to the list.
    /// </summary>
    {$endregion}
    property Duplicates: TDuplicates read fDuplicates write fDuplicates;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately after the list of <see cref="TCurrencyInfo"/> objects changes.
    /// </summary>
    {$endregion}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately before the list of <see cref="TCurrencyInfo"/> objects changes.
    /// </summary>
    {$endregion}
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// World class provides information about all cultures, territories, and
  /// currencies.
  /// </summary>
  /// <remarks>
  /// World class provides properties to locate and access all available
  /// <see cref="TCultureInfo"/>, <see cref="TTerritoryInfo"/>, and
  /// <see cref="TCurrencyInfo"/> objects.
  /// </remarks>
  {$endregion}
  World = class
  private
    class var Changing: Boolean;
    class var CultureList: TCultureList;
    class var TerritoryList: TTerritoryList;
    class var CurrencyList: TCurrencyList;
    class procedure Startup; static;
    class procedure Cleanup; static;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the read-only list of all available <see cref="TCultureInfo"/> objects.
    /// </summary>
    /// <returns>
    /// The read-only list of all available <see cref="TCultureInfo"/> objects.
    /// </returns>
    {$endregion}
    class function Cultures: TReadonlyCultureList; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the read-only list of all available <see cref="TTerritoryInfo"/> objects.
    /// </summary>
    /// <returns>
    /// The read-only list of all available <see cref="TTerritoryInfo"/> objects.
    /// </returns>
    {$endregion}
    class function Territories: TReadonlyTerritoryList; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the read-only list of all available <see cref="TCurrencyInfo"/> objects.
    /// </summary>
    /// <returns>
    /// The read-only list of all available <see cref="TCurrencyInfo"/> objects.
    /// </returns>
    {$endregion}
    class function Currencies: TReadonlyCurrencyList; static;
  end;

{$region 'xmldoc'}
/// <summary>
/// Indicates whether the i18n package functionality is available for the current
/// version of Windows.
/// </summary>
/// <remarks>
/// The i18n package is only supported by Windows XP and newer versions of Windows.
/// Use i18nAvailable to determine whether the current version of Windows can support
/// the i18n package or not.
/// </remarks>
/// <returns>
/// Returns <see langword="true"/> if the i18n package can be used, otherwise returns
/// <see langword="false"/>.
/// </returns>
{$endregion}
function i18nAvailable: Boolean; inline;

{$region 'xmldoc'}
/// <summary>
/// Returns the <see cref="TCultureInfo"/> object that represents the application's
/// default locale.
/// </summary>
/// <returns>
/// The <see cref="TCultureInfo"/> object.
/// </returns>
/// <seealso cref="GetSystemDefaultCulture"/>
/// <seealso cref="GetUserDefaultCulture"/>
/// <seealso cref="GetUserDefaultUICulture"/>
{$endregion}
function GetApplicationDefaultCulture: TCultureInfo;

{$region 'xmldoc'}
/// <summary>
/// Returns the <see cref="TCultureInfo"/> object that represents the system
/// default locale.
/// </summary>
/// <returns>
/// The <see cref="TCultureInfo"/> object.
/// </returns>
/// <seealso cref="GetUserDefaultCulture"/>
/// <seealso cref="GetUserDefaultUICulture"/>
{$endregion}
function GetSystemDefaultCulture: TCultureInfo; inline;

{$region 'xmldoc'}
/// <summary>
/// Returns the <see cref="TCultureInfo"/> object that represents the user's
/// default locale.
/// </summary>
/// <returns>
/// The <see cref="TCultureInfo"/> object.
/// </returns>
/// <seealso cref="GetUserDefaultUICulture"/>
/// <seealso cref="GetSystemDefaultCulture"/>
{$endregion}
function GetUserDefaultCulture: TCultureInfo; inline;

{$region 'xmldoc'}
/// <summary>
/// Returns the <see cref="TCultureInfo"/> object that represents the user's
/// default user interface locale.
/// </summary>
/// <returns>
/// The <see cref="TCultureInfo"/> object.
/// </returns>
/// <seealso cref="GetUserDefaultCulture"/>
/// <seealso cref="GetSystemDefaultCulture"/>
{$endregion}
function GetUserDefaultUICulture: TCultureInfo;

{$region 'xmldoc'}
/// <summary>
/// Returns the <see cref="TTerritoryInfo"/> object that represents the geographical
/// location of the user.
/// </summary>
/// <returns>
/// The <see cref="TTerritoryInfo"/> object.
/// </returns>
{$endregion}
function GetUserTerritory: TTerritoryInfo; inline;

{$region 'xmldoc'}
/// <summary>
/// Returns the <see cref="TCultureInfo"/> object that represents a locale specified
/// by its identifier.
/// </summary>
/// <remarks>
/// <para>
/// Use CultureOf to get information about a locale specified by its identifier.
/// </para>
/// <para>
/// When the argument passed as <paramref name="Exact"/> parameter is <see langword="true"/>,
/// the returned <see cref="TCultureInfo"/> object has the same locale as the locale
/// specified by the <paramref name="LocaleID"/> parameter.
/// </para>
/// <para>
/// When the argument passed as <paramref name="Exact"/> parameter is <see langword="false"/>,
/// the returned <see cref="TCultureInfo"/> object is the nearest locale to the locale
/// specified by the <paramref name="LocaleID"/> parameter if the exact match is not found.
/// </para>
/// </remarks>
/// <param name="LocaleID">
/// The locale identifier.
/// </param>
/// <param name="Exact">
/// Indicates whether a dialect of the locale can be a match.
/// </param>
/// <returns>
/// The <see cref="TCultureInfo"/> object or <see langword="nil"/> if the locale
/// identifier is undefined.
/// </returns>
{$endregion}
function CultureOf(LocaleID: LCID; Exact: Boolean = True): TCultureInfo; overload; inline;

{$region 'xmldoc'}
/// <summary>
/// Returns the <see cref="TCultureInfo"/> object that represents a locale specified
/// by its name.
/// </summary>
/// <remarks>
/// <para>
/// Use CultureOf to get information about a locale specified by its name.
/// </para>
/// <para>
/// When the argument passed as <paramref name="Exact"/> parameter is <see langword="true"/>,
/// the returned <see cref="TCultureInfo"/> object has the same locale as the locale
/// specified by the <paramref name="Locale"/> parameter.
/// </para>
/// <para>
/// When the argument passed as <paramref name="Exact"/> parameter is <see langword="false"/>,
/// the returned <see cref="TCultureInfo"/> object is the nearest locale to the locale
/// specified by the <paramref name="Locale"/> parameter if the exact match is not found.
/// </para>
/// </remarks>
/// <param name="Locale">
/// The locale name.
/// </param>
/// <param name="Exact">
/// Indicates whether a dialect of the locale can be a match.
/// </param>
/// <returns>
/// The <see cref="TCultureInfo"/> object or <see langword="nil"/> if the locale
/// identifier is undefined.
/// </returns>
{$endregion}
function CultureOf(const Locale: String; Exact: Boolean = True): TCultureInfo; overload; inline;

{$region 'xmldoc'}
/// <summary>
/// Returns the <see cref="TTerritoryInfo"/> object that represents a country/region
/// specified by its geographical identifier.
/// </summary>
/// <param name="GeoID">
/// The geographical identifier.
/// </param>
/// <returns>
/// The <see cref="TTerritoryInfo"/> object or <see langword="nil"/> if the geographical
/// identifier is undefined.
/// </returns>
{$endregion}
function TerritoryOf(GeoID: GEOID): TTerritoryInfo; overload; inline;

{$region 'xmldoc'}
/// <summary>
/// Returns the <see cref="TTerritoryInfo"/> object that represents a country/region
/// specified by its two or three characters international code.
/// </summary>
/// <param name="Code">
/// The two or three characters of the international code.
/// </param>
/// <returns>
/// The <see cref="TTerritoryInfo"/> object or <see langword="nil"/> if the international
/// code is undefined.
/// </returns>
{$endregion}
function TerritoryOf(const Code: String): TTerritoryInfo; overload; inline;

{$region 'xmldoc'}
/// <summary>
/// Returns the <see cref="TCurrencyInfo"/> object that represents a currency specified
/// by its international monetary symbol.
/// </summary>
/// <param name="IntlSymbol">
/// The three characters of the international monetary symbol.
/// </param>
/// <returns>
/// The <see cref="TCurrencyInfo"/> object or <see langword="nil"/> if the international
/// monetary symbol is undefined.
/// </returns>
{$endregion}
function CurrencyOf(const IntlSymbol: String): TCurrencyInfo; inline;

{$region 'xmldoc'}
/// <summary>
/// Indicates whether two locale names represent the same primary language.
/// </summary>
/// <param name="Locale1">
/// The first locale name.
/// </param>
/// <param name="Locale2">
/// The second locale name.
/// </param>
/// <returns>
/// Returns <see langword="true"/> if both locales represent the same primary
/// language, otherwise returns <see langword="false"/>.
/// </returns>
/// <seealso cref="SameCountry"/>
{$endregion}
function SameLanguage(const Locale1, Locale2: String): Boolean;

{$region 'xmldoc'}
/// <summary>
/// Indicates whether two locale names represent the same country.
/// </summary>
/// <param name="Locale1">
/// The first locale name.
/// </param>
/// <param name="Locale2">
/// The second locale name.
/// </param>
/// <returns>
/// Returns <see langword="true"/> if both locales represent the same country,
/// otherwise returns <see langword="false"/>.
/// </returns>
/// <seealso cref="SameLanguage"/>
{$endregion}
function SameCountry(const Locale1, Locale2: String): Boolean;

{$region 'xmldoc'}
/// <summary>
/// Call DefaultCalendar to get the <see cref="TCalendar"/> object that indicates
/// the default calendar of the application.
/// </summary>
/// <returns>
/// Returns a <see cref="TCalendar"/> object.
/// </returns>
/// <seealso cref="ChangeDefaultCalendarType"/>
{$endregion}
function DefaultCalendar: TCalendar;

{$region 'xmldoc'}
/// <summary>
/// Call ChangeDefaultCalendarType to change the default calendar system of the
/// application.
/// </summary>
/// <param name="CalendarClass">
/// The <see cref="TCalendar"/> class that represents the new calendar system.
/// </param>
/// <seealso cref="DefaultCalendar"/>
{$endregion}
procedure ChangeDefaultCalendarType(CalendarClass: TCalendarClass);

implementation

uses
  Types, i18nUtils, i18nCalGregorian;

resourcestring
  SInstanceCreateError  = 'You cannot create an instance of %s class.';
  SInstanceDestroyError = 'You cannot destroy an instance of %s class.';
  SDuplicateItemError   = 'List does not allow duplicates (%u)';

{ Helper Functions }

var
  TheDefaultCalendar: TCalendar = nil;
  TheDefaultCalendarClass: TCalendarClass = TGregorianCalendar;

function i18nAvailable: Boolean;
begin
  Result := (WinVer >= $0501); // Windows XP or Later
end;

function DefaultCalendar: TCalendar;
begin
  if not Assigned(TheDefaultCalendar) then
    TheDefaultCalendar := TheDefaultCalendarClass.Create(LOCALE_USER_DEFAULT);
  Result := TheDefaultCalendar;
end;

procedure ChangeDefaultCalendarType(CalendarClass: TCalendarClass);
begin
  if TheDefaultCalendarClass <> CalendarClass then
  begin
    TheDefaultCalendarClass := CalendarClass;
    if Assigned(TheDefaultCalendar) then
      FreeAndNil(TheDefaultCalendar);
  end;
end;

function GetApplicationDefaultCulture: TCultureInfo;
var
  AppFileName: String;
  VerInfoSize, Dummy: Cardinal;
  VerInfo, Trans: Pointer;
begin
  Result := nil;
  AppFileName := ParamStr(0);
  VerInfoSize := GetFileVersioninfoSize(PChar(AppFileName), Dummy);
  if VerInfoSize <> 0 then
  begin
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(PChar(AppFileName)), Dummy, VerInfoSize, VerInfo) and
         VerQueryValue(VerInfo, '\VarFileInfo\Translation', Trans, Dummy)
      then
        Result := CultureOf(LoWord(PDWORD(Trans)^));
    finally
      FreeMem(VerInfo);
    end;
  end;
  if not Assigned(Result) then
    Result := CultureOf('en-US');
end;

function GetSystemDefaultCulture: TCultureInfo;
begin
  Result := World.Cultures.Find(GetSystemDefaultLCID);
end;

function GetUserDefaultCulture: TCultureInfo;
begin
  Result := World.Cultures.Find(GetUserDefaultLCID);
end;

{$INLINE OFF}
function GetUserDefaultUICulture: TCultureInfo;
begin
  Result := World.Cultures.Find(GetUserDefaultUILanguage);
end;
{$INLINE AUTO}

function GetUserTerritory: TTerritoryInfo;
begin
  Result := World.Territories.Find(GetUserGeoID(GEOCLASS_NATION));
end;

function CultureOf(LocaleID: LCID; Exact: Boolean): TCultureInfo;
begin
  if Exact then
    Result := World.Cultures.Find(LocaleID)
  else
    Result := World.Cultures.FindNearest(LocaleID);
end;

function CultureOf(const Locale: String; Exact: Boolean): TCultureInfo;
begin
  if Exact then
    Result := World.Cultures.Find(Locale)
  else
    Result := World.Cultures.FindNearest(Locale);
end;

function TerritoryOf(GeoID: GEOID): TTerritoryInfo;
begin
  Result := World.Territories.Find(GeoID);
end;

function TerritoryOf(const Code: String): TTerritoryInfo;
begin
  Result := World.Territories.Find(Code);
end;

function CurrencyOf(const IntlSymbol: String): TCurrencyInfo;
begin
  Result := World.Currencies.Find(IntlSymbol);
end;

function SameLanguage(const Locale1, Locale2: String): Boolean;
var
  S1, S2: PChar;
begin
  Result := False;
  S1 := PChar(Locale1);
  S2 := PChar(Locale2);
  while (S1^ <> #0) and (S1^ <> '-') do
  begin
    if UpCase(S1^) <> UpCase(S2^) then
      Exit;
    Inc(S1);
    Inc(S2);
  end;
  Result := (S2^ = #0) or (S2^ = '-');
end;

function SameCountry(const Locale1, Locale2: String): Boolean;
var
  P1, P2: PChar;
begin
  P1 := StrRScan(PChar(Locale1), '-');
  P2 := StrRScan(PChar(Locale2), '-');
  Result := (P1 <> nil) and (P2 <> nil) and (StrIComp(P1, P2) = 0);
end;

function IsRightToLeftLanguage(LocaleID: LCID): Boolean;
var
  I: Integer;
  BufferA: array [128 .. 255] of AnsiChar;
  BufferW: array [128 .. 256] of WideChar;
begin
  Result := False;
  for I := Low(BufferA) to High(BufferA) do
    BufferA[I] := AnsiChar(I);
  GetStringTypeExA(LocaleID, CT_CTYPE2, @BufferA,
    High(BufferA) - Low(BufferA) + 1, BufferW);
  for I := Low(BufferA) to High(BufferA) do
    if Ord(BufferW[I]) = C2_RIGHTTOLEFT then
    begin
      Result := True;
      Exit;
    end;
end;

{ TCulture }

constructor TCultureInfo.Create(ALocaleID: LCID);
var
  CurrencySymbol: String;
  PercentSign: String;
begin
  if not World.Changing then
    raise Exception.CreateResFmt(@SInstanceCreateError, [ClassName]);
  fLocaleID := ALocaleID;
  fLocale := LCIDToLocaleName(LocaleID);
  fLanguage2 := GetLocaleStr(LocaleID, LOCALE_SISO639LANGNAME, '');
  fLanguage3 := GetLocaleStr(LocaleID, LOCALE_SISO639LANGNAME2, '');
  fGeoID := GetLocaleInt(LocaleID, LOCALE_IGEOID, 0);
  fLocalizedDisplayName := GetLocaleStr(LocaleID, LOCALE_SLOCALIZEDDISPLAYNAME, '');
  fEnglishDisplayName := GetLocaleStr(LocaleID, LOCALE_SENGLISHDISPLAYNAME, '');
  fNativeDisplayName := GetLocaleStr(LocaleID, LOCALE_SNATIVEDISPLAYNAME, '');
  fLocalizedLanguageName := GetLocaleStr(LocaleID, LOCALE_SLOCALIZEDLANGUAGENAME, '');
  fEnglishLanguageName := GetLocaleStr(LocaleID, LOCALE_SENGLISHLANGUAGENAME, '');
  fNativeLanguageName := GetLocaleStr(LocaleID, LOCALE_SNATIVELANGUAGENAME, '');
  fNativeCountryName := GetLocaleStr(LocaleID, LOCALE_SNATIVECOUNTRYNAME, '');
  fNativeCurrencyName := GetLocaleStr(LocaleID, LOCALE_SNATIVECURRNAME, '');
  fScriptCodes := GetLocaleScripts(Locale);
  fScripts := ScriptCodesToScripts(ScriptCodes);
  case GetLocaleInt(LocaleID, LOCALE_IREADINGLAYOUT, -1) of
    0: fReadingLayout := rlLeftToRight;
    1: fReadingLayout := rlRightToLeft;
    2: fReadingLayout := rlTopToBottomColRightToLeft;
    3: fReadingLayout := rlTopToBottomColLeftToRight;
  else if IsRightToLeftLanguage(LocaleID) then
    fReadingLayout := rlRightToLeft;
  end;
  if GetLocaleInt(LocaleID, LOCALE_IMEASURE, 0) = 0 then
    fMeasurementSystem := msMetric
  else
    fMeasurementSystem := msUS;
  fDefaultCodePages[cpOEM] := GetLocaleInt(LocaleID, LOCALE_IDEFAULTCODEPAGE, 0);
  fDefaultCodePages[cpANSI] := GetLocaleInt(LocaleID, LOCALE_IDEFAULTANSICODEPAGE, 0);
  fDefaultCodePages[cpMAC] := GetLocaleInt(LocaleID, LOCALE_IDEFAULTMACCODEPAGE, 0);
  fNativeDigits := GetLocaleStr(LocaleID, LOCALE_SNATIVEDIGITS, NOMINAL_DIGITS);
  fNativeCalendarType := CalendarTypes.ByLocale(Locale);
  if not Assigned(fNativeCalendarType) then
    fNativeCalendarType := TGregorianCalendar;
  if GeoID <> 0 then
  begin
    fCountry := World.TerritoryList.Find(GeoID);
    if not Assigned(fCountry) then
    begin
      fCountry := TTerritoryInfo.CreateByLocaleID(LocaleID);
      World.TerritoryList.Add(fCountry);
    end;
  end;
  CurrencySymbol := GetLocaleStr(LocaleID, LOCALE_SINTLSYMBOL, '');
  if CurrencySymbol <> '' then
  begin
    fCurrency := World.CurrencyList.Find(CurrencySymbol);
    if not Assigned(fCurrency) then
    begin
      fCurrency := TCurrencyInfo.Create(LocaleID);
      World.CurrencyList.Add(fCurrency);
    end;
  end;
  if Assigned(fCountry) then
    fCountry.Associate(Self, fCurrency);
  if Assigned(fCurrency) then
    fCurrency.Associate(Self, fCountry);
  case GetLocaleInt(LocaleID, LOCALE_IPOSITIVEPERCENT, 0) of
    0: fPercentPositive := '# %';
    1: fPercentPositive := '#%';
    2: fPercentPositive := '%#';
    3: fPercentPositive := '% #';
  end;
  case GetLocaleInt(LocaleID, LOCALE_INEGATIVEPERCENT, 0) of
    0: fPercentNegative := '-# %';
    1: fPercentNegative := '-#%';
    2: fPercentNegative := '-%#';
    3: fPercentNegative := '%-#';
    4: fPercentNegative := '%#-';
    5: fPercentNegative := '#-%';
    6: fPercentNegative := '#%-';
    7: fPercentNegative := '-% #';
    8: fPercentNegative := '# %-';
    9: fPercentNegative := '% #-';
   10: fPercentNegative := '% -#';
   11: fPercentNegative := '#- %';
  end;
  PercentSign := GetLocaleStr(LocaleID, LOCALE_SPERCENT, '%');
  if PercentSign <> '%' then
  begin
    fPercentPositive := StringReplace(fPercentPositive, '%', PercentSign, []);
    fPercentNegative := StringReplace(fPercentNegative, '%', PercentSign, []);
  end;
  fPluralRule := TPluralForms.RuleOf(Locale, 'nplurals=1; plural=0; ASSUMED');
end;

constructor TCultureInfo.Create(const ALocale: String);
begin
  Create(LocaleNameToLCID(ALocale));
end;

destructor TCultureInfo.Destroy;
begin
  if not World.Changing then
    raise Exception.CreateResFmt(@SInstanceDestroyError, [ClassName]);
  if Assigned(fFormatSettings) then
    Dispose(fFormatSettings);
  if Assigned(fNativeCalendar) then
    fNativeCalendar.Free;
  if Assigned(fCalendarTypes) then
    fCalendarTypes.Free;
  inherited Destroy;
end;

function TCultureInfo.GetDisplayNames(DisplayName: TCultureDisplayName): String;
begin
  case DisplayName of
    cnLocale:
      Result := Locale;
    cnLanguage:
      Result := Language2;
    cnLanguage3:
      Result := Language3;
    cnLocalizedDisplayName:
      Result := LocalizedDisplayName;
    cnEnglishDisplayName:
      Result := EnglishDisplayName;
    cnNativeDisplayName:
      Result := NativeDisplayName;
    cnLocalizedLanguageName:
      Result := LocalizedLanguageName;
    cnEnglishLanguageName:
      Result := EnglishLanguageName;
    cnNativeLanguageName:
      Result := NativeLanguageName;
    cnNativeCountryName:
      Result := NativeCountryName;
    cnNativeCurrencyName:
      Result := NativeCurrencyName;
  else
    Result := '';
  end;
end;

function TCultureInfo.GetDefaultCodePages(CodePageType: TCodePageType): Integer;
begin
  Result := fDefaultCodePages[CodePageType];
end;

function TCultureInfo.GetLangID: LANGID;
begin
  Result := Windows.LANGID(LocaleID);
end;

function TCultureInfo.GetPrimaryLangID: LANGID;
begin
  Result := Windows.PRIMARYLANGID(LocaleID);
end;

function TCultureInfo.GetSubLangID: LANGID;
begin
  Result := Windows.SUBLANGID(LocaleID);
end;

function TCultureInfo.GetBiDiMode: TBiDiMode;
begin
  if IsRightToLeft then
    Result := bdRightToLeft
  else
    Result := bdLeftToRight;
end;

function TCultureInfo.GetFormatSettings: PFormatSettings;
begin
  if not Assigned(fFormatSettings) then
  begin
    New(fFormatSettings);
    {$IFDEF COMPILER_XE2_UP}
    fFormatSettings^ := TFormatSettings.Create(LocaleID);
    {$ELSE}
    GetLocaleFormatSettings(LocaleID, fFormatSettings^);
    {$ENDIF}
  end;
  Result := fFormatSettings;
end;

function TCultureInfo.GetNativeCalendar: TCalendar;
begin
  if not Assigned(fNativeCalendar) then
    fNativeCalendar := NativeCalendarType.Create(Locale);
  Result := fNativeCalendar;
end;

function TCultureInfo.IsRightToLeft: Boolean;
begin
  Result := (ReadingLayout = rlRightToLeft);
end;

function TCultureInfo.IsDefault: Boolean;
begin
  Result := (SubLangID = SUBLANG_DEFAULT);
end;

function TCultureInfo.IsDialectOf(Culture: TCultureInfo): Boolean;
begin
  Result := (Culture.PrimaryLangID = PrimaryLangID);
end;

function TCultureInfo.IsDialectOf(const Locale: String): Boolean;
begin
  Result := (Pos(Language2, LowerCase(Locale)) = 1) and
    ((Length(Language2) = Length(Locale)) or
     (Locale[Length(Language2) + 1] = '-'));
end;

function TCultureInfo.IsDialectOf(LangID: LANGID): Boolean;
begin
  Result := (Windows.PRIMARYLANGID(LangID) = PrimaryLangID);
end;

function TCultureInfo.IsUsingNominalDigits: Boolean;
begin
  Result := (Pos('0', NativeDigits) = 1);
end;

function TCultureInfo.IsUsingCommonEraCalendar: Boolean;
begin
  Result := (NativeCalendarType = TGregorianCalendar);
end;

function TCultureInfo.DrawTextBiDiModeFlags(Flags: Cardinal): Integer;
begin
  Result := Flags;
  if IsRightToLeft then
  begin
    if (Result and DT_RIGHT) = DT_RIGHT then
      Result := Result and not DT_RIGHT
    else if (Result and DT_CENTER) <> DT_CENTER then
      Result := Result or DT_RIGHT;
  end;
  Result := Result or DrawTextBiDiModeFlagsReadingOnly;
end;

function TCultureInfo.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  if IsRightToLeft then
    Result := DT_RTLREADING
  else
    Result := 0;
end;

function TCultureInfo.VerifyString(const Str: String; AllowLatin: Boolean): Boolean;
begin
  Result := VerifyScripts(ScriptCodes, GetStringScripts(Str));
end;

function TCultureInfo.OemToUnicode(const Str: RawByteString): String;
begin
  Result := CodePageToUnicode(DefaultCodePages[cpOEM], Str);
end;

function TCultureInfo.UnicodeToOem(const Str: String): RawByteString;
begin
  Result := UnicodeToCodePage(DefaultCodePages[cpOEM], Str);
end;

function TCultureInfo.AnsiToUnicode(const Str: RawByteString): String;
begin
  Result := CodePageToUnicode(DefaultCodePages[cpANSI], Str);
end;

function TCultureInfo.UnicodeToAnsi(const Str: String): RawByteString;
begin
  Result := UnicodeToCodePage(DefaultCodePages[cpANSI], Str);
end;

function TCultureInfo.MacToUnicode(const Str: RawByteString): String;
begin
  Result := CodePageToUnicode(DefaultCodePages[cpMAC], Str);
end;

function TCultureInfo.UnicodeToMac(const Str: String): RawByteString;
begin
  Result := UnicodeToCodePage(DefaultCodePages[cpMAC], Str);
end;

function TCultureInfo.NominalDigitsToNative(const Str: String;
  IgnoreUCCs: Boolean): String;
var
  S: PChar;
  Zero: Char;
  CanChange: Boolean;
begin
  Result := Str;
  Zero := PChar(NativeDigits)^;
  if Zero <> '0' then
  begin
    CanChange := True;
    S := PChar(Result);
    while S^ <> #0 do
    begin
      if S^ = UCC_NODS then
        CanChange := IgnoreUCCs
      else if S^ = UCC_NADS then
        CanChange := True
      else if CanChange and (S^ >= '0') and (S^ <= '9') then
        S^ := Chr(Ord(Zero) + Ord(S^) - Ord('0'));
      Inc(S);
    end;
  end;
end;

function TCultureInfo.NativeDigitsToNominal(const Str: String;
  IgnoreUCCs: Boolean): String;
var
  S: PChar;
  Zero: Char;
  CanChange: Boolean;
begin
  Result := Str;
  Zero := PChar(NativeDigits)^;
  if Zero <> '0' then
  begin
    CanChange := True;
    S := PChar(Result);
    while S^ <> #0 do
    begin
      if S^ = UCC_NADS then
        CanChange := IgnoreUCCs
      else if S^ = UCC_NODS then
        CanChange := True
      else if CanChange and (Cardinal(Ord(S^) - Ord(Zero)) <= 9) then
        S^ := Chr(Ord('0') + Ord(S^) - Ord(Zero));
      Inc(S);
    end;
  end;
end;

function TCultureInfo.FreezeDigits(const Str: String; UseNativeDigits: Boolean): String;
begin
  if UseNativeDigits then
    Result := UCC_NODS + NominalDigitsToNative(Str, False)
  else
    Result := UCC_NODS + Str;
end;

function TCultureInfo.UnfreezeDigits(const Str: String): String;
begin
  Result := Str;
  if PChar(Result)^ = UCC_NODS then
  begin
    Delete(Result, 1, 1);
    Result := NativeDigitsToNominal(Result, False);
  end;
end;

function TCultureInfo.Format(const Fmt: String; const Args: array of const;
  UseNativeDigits: Boolean): String;
begin
  Result := FreezeDigits(SysUtils.Format(Fmt, Args, FormatSettings^), UseNativeDigits);
end;

function TCultureInfo.FormatNumber(const Fmt: String; const Value: Extended;
  UseNativeDigits: Boolean): String;
begin
  Result := FreezeDigits(SysUtils.FormatFloat(Fmt, Value, FormatSettings^), UseNativeDigits);
end;

function TCultureInfo.FormatPercent(const Value: Extended; Decimals: Integer;
  UseNativeDigits: Boolean): String;
var
  Str, Fmt: String;
begin
  if Decimals < 0 then
    Fmt := '#,##0.' + StringOfChar('0', Abs(Decimals))
  else
    Fmt := '#,##0.' + StringOfChar('#', Decimals);
  Str := FormatNumber(Fmt, Abs(Value * 100.0), UseNativeDigits);
  if Value < 0 then
    Result := StringReplace(fPercentNegative, '#', Str, [])
  else
    Result := StringReplace(fPercentPositive, '#', Str, []);
end;

function TCultureInfo.FormatDateTime(const Fmt: String; const Value: TDateTime;
  UseNativeDigits: Boolean): String;
begin
  Result := FreezeDigits(NativeCalendar.Format(Fmt, Value), UseNativeDigits);
end;

function TCultureInfo.ParseDateTime(const Fmt: String;
  const Str: String): TDateTime;
begin
  Result := NativeCalendar.Parse(Fmt, UnfreezeDigits(Str));
end;

function TCultureInfo.TryParseDateTime(const Fmt: String; const Str: String;
  var DateTime: TDateTime): Boolean;
begin
  Result := NativeCalendar.TryParse(Fmt, UnfreezeDigits(Str), DateTime);
end;

{ TCultureEnumerator }

constructor TCultureEnumerator.Create(List: TReadonlyCultureList);
begin
  inherited Create(List.List);
end;

function TCultureEnumerator.GetCurrent: TCultureInfo;
begin
  Result := TCultureInfo(inherited GetCurrent);
end;

{ TReadonlyCultureList }

constructor TReadonlyCultureList.Create;
begin
  List := TList.Create;
end;

destructor TReadonlyCultureList.Destroy;
begin
  List.Free;
  if Assigned(Lookup_LocaleID) then
    Lookup_LocaleID.Free;
  if Assigned(Lookup_Locale) then
    Lookup_Locale.Free;
  inherited;
end;

function TReadonlyCultureList.GetCount: Integer;
begin
  Result := List.Count;
end;

function TReadonlyCultureList.GetItems(Index: Integer): TCultureInfo;
begin
  Result := TCultureInfo(List[Index]);
end;

function TReadonlyCultureList.GetEnumerator: TCultureEnumerator;
begin
  Result := TCultureEnumerator.Create(Self);
end;

procedure TReadonlyCultureList.AddLookupEntries(Culture: TCultureInfo; Index: Integer);
begin
  if IsLookupTableValid_LocaleID then
    Lookup_LocaleID.Add(Culture.LocaleID, Index);
  if IsLookupTableValid_Locale then
    Lookup_Locale.Add(AnsiLowerCase(Culture.Locale), Index);
end;

procedure TReadonlyCultureList.RemoveLookupEntries(Culture: TCultureInfo);
begin
  if IsLookupTableValid_LocaleID then
    Lookup_LocaleID.Remove(Culture.LocaleID);
  if IsLookupTableValid_Locale then
    Lookup_Locale.Remove(AnsiLowerCase(Culture.Locale));
end;

procedure TReadonlyCultureList.ClearLookupTables;
begin
  if Assigned(Lookup_LocaleID) then
  begin
    Lookup_LocaleID.Clear;
    IsLookupTableValid_LocaleID := False;
  end;
  if Assigned(Lookup_Locale) then
  begin
    Lookup_Locale.Clear;
    IsLookupTableValid_Locale := False;
  end;
end;

procedure TReadonlyCultureList.InvalidateLookupTables;
begin
  IsLookupTableValid_LocaleID := False;
  IsLookupTableValid_Locale := False;
end;

procedure TReadonlyCultureList.ValidateLookupTable_LocaleID;
var
  I: Integer;
begin
  if not IsLookupTableValid_LocaleID then
  begin
    if Assigned(Lookup_LocaleID) then
      Lookup_LocaleID.Clear
    else
      Lookup_LocaleID := TKeyLookup<LCID,Integer>.Create;
    for I := 0 to Count - 1 do
      Lookup_LocaleID.Add(Items[I].LocaleID, I);
    IsLookupTableValid_LocaleID := True;
  end;
end;

procedure TReadonlyCultureList.ValidateLookupTable_Locale;
var
  I: Integer;
begin
  if not IsLookupTableValid_Locale then
  begin
    if Assigned(Lookup_Locale) then
      Lookup_Locale.Clear
    else
      Lookup_Locale := TKeyLookup<String,Integer>.Create;
    for I := 0 to Count - 1 do
      Lookup_Locale.Add(LowerCase(Items[I].Locale), I);
    IsLookupTableValid_Locale := True;
  end;
end;

function TReadonlyCultureList.First: TCultureInfo;
begin
  if Count <> 0 then
    Result := Items[0]
  else
    Result := nil;
end;

function TReadonlyCultureList.Last: TCultureInfo;
begin
  if Count <> 0 then
    Result := Items[Count - 1]
  else
    Result := nil;
end;

procedure TReadonlyCultureList.AssignTo(Dest: TPersistent);
var
  Strings: TStrings absolute Dest;
  I: Integer;
begin
  if Dest is TStrings then
  begin
    Strings.BeginUpdate;
    try
      Strings.Clear;
      for I := 0 to Count - 1 do
        Strings.AddObject(Items[I].Locale, Items[I]);
    finally
      Strings.EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TReadonlyCultureList.AssignLabelsTo(Dest: TStrings;
  DisplayName: TCultureDisplayName);
var
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for I := 0 to Count - 1 do
      Dest.AddObject(Items[I].DisplayNames[DisplayName], Items[I]);
  finally
    Dest.EndUpdate;
  end;
end;

function TReadonlyCultureList.Find(LocaleID: LCID): TCultureInfo;
var
  Index: Integer;
begin
  if not IsLookupTableValid_LocaleID then
    ValidateLookupTable_LocaleID;
  if Lookup_LocaleID.Retrieve(LocaleID, Index) then
    Result := Items[Index]
  else
    Result := nil;
end;

function TReadonlyCultureList.Find(const Locale: String): TCultureInfo;
var
  Index: Integer;
begin
  if not IsLookupTableValid_Locale then
    ValidateLookupTable_Locale;
  if Lookup_Locale.Retrieve(LowerCase(Locale), Index) then
    Result := Items[Index]
  else
    Result := nil;
end;

function TReadonlyCultureList.FindNearest(Culture: TCultureInfo): TCultureInfo;
var
  Index: Integer;
begin
  Index := NearestIndexOf(Culture);
  if Index >= 0 then
    Result := Items[Index]
  else
    Result := nil;
end;

function TReadonlyCultureList.FindNearest(LocaleID: LCID): TCultureInfo;
var
  Index: Integer;
begin
  Index := NearestIndexOf(LocaleID);
  if Index >= 0 then
    Result := Items[Index]
  else
    Result := nil;
end;

function TReadonlyCultureList.FindNearest(const Locale: String): TCultureInfo;
var
  Index: Integer;
begin
  Index := NearestIndexOf(Locale);
  if Index >= 0 then
    Result := Items[Index]
  else
    Result := nil;
end;

function TReadonlyCultureList.IndexOf(Culture: TCultureInfo): Integer;
begin
  if Assigned(Culture) then
    Result := IndexOf(Culture.LocaleID)
  else
    Result := -1;
end;

function TReadonlyCultureList.IndexOf(LocaleID: LCID): Integer;
begin
  if not IsLookupTableValid_LocaleID then
    ValidateLookupTable_LocaleID;
  if not Lookup_LocaleID.Retrieve(LocaleID, Result) then
    Result := -1;
end;

function TReadonlyCultureList.IndexOf(const Locale: String): Integer;
begin
  if not IsLookupTableValid_Locale then
    ValidateLookupTable_Locale;
  if not Lookup_Locale.Retrieve(LowerCase(Locale), Result) then
    Result := -1;
end;

function TReadonlyCultureList.IndexOfName(const AName: String;
  DisplayName: TCultureDisplayName): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(Items[I].DisplayNames[DisplayName], AName) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TReadonlyCultureList.NearestIndexOf(Culture: TCultureInfo): Integer;
begin
  if Assigned(Culture) then
    Result := NearestIndexOf(Culture.LocaleID)
  else
    Result := -1;
end;

function TReadonlyCultureList.NearestIndexOf(LocaleID: LCID): Integer;
var
  PriID, SubID: LANGID;
  I: Integer;
begin
  Result := IndexOf(LocaleID);
  if Result >= 0 then Exit;
  PriID := Windows.PRIMARYLANGID(LocaleID);
  if PriID = 0 then Exit;
  SubID := High(LANGID);
  for I := 0 to Count - 1 do
    with Items[I] do
    begin
      if PrimaryLangID <> PriID then
        Continue;
      if IsDefault then
      begin
        Result := I;
        Exit;
      end;
      if (Result < 0) or (SubLangID < SubID) then
      begin
        Result := I;
        SubID := SubLangID;
      end;
    end;
end;

function TReadonlyCultureList.NearestIndexOf(const Locale: String): Integer;
var
  Lang: String;
  SubID: LANGID;
  I: Integer;
begin
  Result := IndexOf(Locale);
  if Result >= 0 then Exit;
  I := Pos('-', Locale);
  if I <> 0 then
    Lang := LowerCase(Copy(Locale, 1, I - 1))
  else
    Lang := LowerCase(Locale);
  if Lang = '' then Exit;
  SubID := High(LANGID);
  for I := 0 to Count - 1 do
    with Items[I] do
    begin
      if Language2 <> Lang then
        Continue;
      if IsDefault then
      begin
        Result := I;
        Exit;
      end;
      if (Result < 0) or (SubLangID < SubID) then
      begin
        Result := I;
        SubID := SubLangID;
      end;
    end;
end;

function TReadonlyCultureList.Exists(Culture: TCultureInfo): Boolean;
begin
  Result := Assigned(Culture) and Exists(Culture.LocaleID);
end;

function TReadonlyCultureList.Exists(LocaleID: LCID): Boolean;
begin
  if not IsLookupTableValid_LocaleID then
    ValidateLookupTable_LocaleID;
  Result := Lookup_LocaleID.Exists(LocaleID);
end;

function TReadonlyCultureList.Exists(const Locale: String): Boolean;
begin
  if not IsLookupTableValid_Locale then
    ValidateLookupTable_Locale;
  Result := Lookup_Locale.Exists(LowerCase(Locale));
end;

{ TCultureList }

function TCultureList.GetCapacity: Integer;
begin
  Result := List.Capacity;
end;

procedure TCultureList.SetCapacity(Value: Integer);
begin
  List.Capacity := Value;
  if Value < Count then
    InvalidateLookupTables;
end;

procedure TCultureList.ReadLocales(Reader: TReader);
var
  Locale: String;
begin
  BeginUpdate;
  try
    Clear;
    Reader.ReadListBegin;
    while not Reader.EndOfList do
    begin
      Locale := Reader.ReadString;
      if Locale = '*' then
      begin
        Assign(World.Cultures);
        Break;
      end;
      Add(Locale);
    end;
    Reader.ReadListEnd;
  finally
    EndUpdate;
  end;
end;

procedure TCultureList.WriteLocales(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  if (Duplicates <> dupAccept) and (Count = World.Cultures.Count) then
    Writer.WriteString('*') // all cultures
  else
    for I := 0 to Count - 1 do
      Writer.WriteString(Items[I].Locale);
  Writer.WriteListEnd;
end;

procedure TCultureList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Cultures', ReadLocales, WriteLocales, Count <> 0);
end;

procedure TCultureList.Change;
begin
  if (UpdateCount = 0) and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCultureList.Changing;
begin
  if (UpdateCount = 0) and Assigned(OnChanging) then
    OnChanging(Self);
end;

procedure TCultureList.BeginUpdate;
begin
  if UpdateCount = 0 then
    Changing;
  Inc(fUpdateCount);
end;

procedure TCultureList.EndUpdate;
begin
  Dec(fUpdateCount);
  if UpdateCount = 0 then
    Change;
end;

procedure TCultureList.Clear;
begin
  if Count <> 0 then
  begin
    Changing;
    try
      List.Clear;
      ClearLookupTables;
    finally
      Change;
    end;
  end;
end;

procedure TCultureList.Sort(OrderBy: TCultureDisplayName);

  procedure QuickSort(L, R: Integer);
  var
    I, J, P: Integer;
    Pivot: String;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        Pivot := Items[P].DisplayNames[OrderBy];
        while AnsiCompareText(Items[I].DisplayNames[OrderBy], Pivot) < 0 do
          Inc(I);
        while AnsiCompareText(Items[J].DisplayNames[OrderBy], Pivot) > 0 do
          Dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            List.Exchange(I, J);
            InvalidateLookupTables;
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if (OrderBy <> cnNone) and (Count > 1) then
  begin
    Changing;
    try
      QuickSort(0, Count - 1);
    finally
      Change;
    end;
  end;
end;

procedure TCultureList.Assign(Source: TPersistent);
begin
  if Source is TReadonlyCultureList then
    Apply(TReadonlyCultureList(Source), laCopy)
  else
    inherited Assign(Source);
end;

procedure TCultureList.Apply(ListA: TReadonlyCultureList;
  Op: TListAssignOp; ListB: TReadonlyCultureList);
begin
  Changing;
  try
    if Assigned(ListB) then
      List.Assign(ListA.List, Op, ListB.List)
    else
      List.Assign(ListA.List, Op, nil);
    InvalidateLookupTables;
  finally
    Change;
  end;
end;

function TCultureList.Add(Culture: TCultureInfo): Integer;
begin
  Result := -1;
  if Assigned(Culture) then
  begin
    if Duplicates <> dupAccept then
    begin
      Result := IndexOf(Culture);
      if Result >= 0 then
      begin
        if Duplicates = dupError then
          raise EListError.CreateResFmt(@SDuplicateItemError, [Result]);
        Exit;
      end;
    end;
    Changing;
    try
      Result := List.Add(Culture);
      AddLookupEntries(Culture, Result);
    finally
      Change;
    end;
  end;
end;

function TCultureList.Add(LocaleID: LCID): Integer;
begin
  Result := Add(World.Cultures.Find(LocaleID));
end;

function TCultureList.Add(const Locale: String): Integer;
begin
  Result := Add(World.Cultures.Find(Locale));
end;

function TCultureList.Remove(Culture: TCultureInfo): Integer;
begin
  Result := IndexOf(Culture);
  if Result >= 0 then
    Delete(Result);
end;

function TCultureList.Remove(LocaleID: LCID): Integer;
begin
  Result := IndexOf(LocaleID);
  if Result >= 0 then
    Delete(Result);
end;

function TCultureList.Remove(const Locale: String): Integer;
begin
  Result := IndexOf(Locale);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCultureList.Delete(Index: Integer);
begin
  Changing;
  try
    RemoveLookupEntries(Items[Index]);
    List.Delete(Index);
  finally
    Change;
  end;
end;

{ TTerritory }

constructor TTerritoryInfo.Create(AGeoID: GEOID);
begin
  if not World.Changing then
    raise Exception.CreateResFmt(@SInstanceCreateError, [ClassName]);
  fCultures := TCultureList.Create;
  fCurrencies := TCurrencyList.Create;
  fGeoID := AGeoID;
  fCode2 := GetGeoStr(GeoID, GEO_ISO2, '');
  fCode3 := GetGeoStr(GeoID, GEO_ISO3, '');
  fLatitude := StrToFloatDef(GetGeoStr(GeoID, GEO_LATITUDE, '0'), 0);
  fLongitude := StrToFloatDef(GetGeoStr(GeoID, GEO_LONGITUDE, '0'), 0);
  fFriendlyName := GetGeoStr(GeoID, GEO_FRIENDLYNAME, '');
  fOfficialName := GetGeoStr(GeoID, GEO_OFFICIALNAME, '');
  fLocalizedName := FriendlyName;
  fEnglishName := FriendlyName;
end;

constructor TTerritoryInfo.CreateByLocaleID(LocaleID: LCID; Dummy: Integer);
begin
  Create(GetLocaleInt(LocaleID, LOCALE_IGEOID, 0));
  fLocalizedName := GetLocaleStr(LocaleID, LOCALE_SLOCALIZEDCOUNTRYNAME, '');
  fEnglishName := GetLocaleStr(LocaleID, LOCALE_SENGLISHCOUNTRYNAME, '');
end;

constructor TTerritoryInfo.CreateByLocale(const Locale: String);
begin
  CreateByLocaleID(LocaleNameToLCID(Locale));
end;

destructor TTerritoryInfo.Destroy;
begin
  if not World.Changing then
    raise Exception.CreateResFmt(@SInstanceDestroyError, [ClassName]);
  fCultures.Free;
  fCurrencies.Free;
  inherited Destroy;
end;

function TTerritoryInfo.GetDisplayNames(DisplayName: TTerritoryDisplayName): String;
begin
  case DisplayName of
    tnCode2:
      Result := Code2;
    tnCode3:
      Result := Code3;
    tnFriendlyName:
      Result := FriendlyName;
    tnOfficialName:
      Result := OfficialName;
    tnLocalizedName:
      Result := LocalizedName;
    tnEnglishName:
      Result := EnglishName;
    tnNativeName:
      Result := NativeName;
  else
    Result := '';
  end;
end;

function TTerritoryInfo.GetNativeName: String;
begin
  if Cultures.Count <> 0 then
    Result := Cultures[0].NativeCountryName
  else
    Result := '';
end;

function TTerritoryInfo.GetCurrencies: TReadonlyCurrencyList;
begin
  Result := fCurrencies;
end;

function TTerritoryInfo.GetCultures: TReadonlyCultureList;
begin
  Result := fCultures;
end;

procedure TTerritoryInfo.Associate(Culture: TCultureInfo; Currency: TCurrencyInfo);
begin
  if Assigned(Culture) and not fCultures.Exists(Culture) then
    fCultures.Add(Culture);
  if Assigned(Currency) and not fCurrencies.Exists(Currency) then
    fCurrencies.Add(Currency);
end;

{ TTerritoryEnumerator }

constructor TTerritoryEnumerator.Create(List: TReadonlyTerritoryList);
begin
  inherited Create(List.List);
end;

function TTerritoryEnumerator.GetCurrent: TTerritoryInfo;
begin
  Result := TTerritoryInfo(inherited GetCurrent);
end;

{ TReadonlyTerritoryList }

constructor TReadonlyTerritoryList.Create;
begin
  List := TList.Create;
end;

destructor TReadonlyTerritoryList.Destroy;
begin
  List.Free;
  if Assigned(LookupTable_GeoID) then
    LookupTable_GeoID.Free;
  if Assigned(LookupTable_Code2) then
    LookupTable_Code2.Free;
  if Assigned(LookupTable_Code3) then
    LookupTable_Code3.Free;
  inherited Destroy;
end;

function TReadonlyTerritoryList.GetCount;
begin
  Result := List.Count;
end;

function TReadonlyTerritoryList.GetItems(Index: Integer): TTerritoryInfo;
begin
  Result := TTerritoryInfo(List[Index]);
end;

function TReadonlyTerritoryList.GetEnumerator: TTerritoryEnumerator;
begin
  Result := TTerritoryEnumerator.Create(Self);
end;

procedure TReadonlyTerritoryList.AddLookupEntries(Territory: TTerritoryInfo; Index: Integer);
begin
  if IsLookupTableValid_GeoID then
    LookupTable_GeoID.Add(Territory.GeoID, Index);
  if IsLookupTableValid_Code2 then
    LookupTable_Code2.Add(Territory.Code2, Index);
  if IsLookupTableValid_Code3 then
    LookupTable_Code3.Add(Territory.Code3, Index);
end;

procedure TReadonlyTerritoryList.RemoveLookupEntries(Territory: TTerritoryInfo);
begin
  if IsLookupTableValid_GeoID then
    LookupTable_GeoID.Remove(Territory.GeoID);
  if IsLookupTableValid_Code2 then
    LookupTable_Code2.Remove(Territory.Code2);
  if IsLookupTableValid_Code3 then
    LookupTable_Code3.Remove(Territory.Code3);
end;

procedure TReadonlyTerritoryList.ClearLookupTables;
begin
  if Assigned(LookupTable_GeoID) then
  begin
    LookupTable_GeoID.Clear;
    IsLookupTableValid_GeoID := False;
  end;
  if Assigned(LookupTable_Code2) then
  begin
    LookupTable_Code2.Clear;
    IsLookupTableValid_Code2 := False;
  end;
  if Assigned(LookupTable_Code3) then
  begin
    LookupTable_Code3.Clear;
    IsLookupTableValid_Code3 := False;
  end;
end;

procedure TReadonlyTerritoryList.InvalidateLookupTables;
begin
  IsLookupTableValid_GeoID := False;
  IsLookupTableValid_Code2 := False;
  IsLookupTableValid_Code3 := False;
end;

procedure TReadonlyTerritoryList.ValidateLookupTable_GeoID;
var
  I: Integer;
begin
  if not IsLookupTableValid_GeoID then
  begin
    if Assigned(LookupTable_GeoID) then
      LookupTable_GeoID.Clear
    else
      LookupTable_GeoID := TKeyLookup<GEOID,Integer>.Create;
    for I := 0 to Count - 1 do
      LookupTable_GeoID.Add(Items[I].GeoID, I);
    IsLookupTableValid_GeoID := True;
  end;
end;

procedure TReadonlyTerritoryList.ValidateLookupTable_Code2;
var
  I: Integer;
begin
  if not IsLookupTableValid_Code2 then
  begin
    if Assigned(LookupTable_Code2) then
      LookupTable_Code2.Clear
    else
      LookupTable_Code2 := TKeyLookup<String,Integer>.Create;
    for I := 0 to Count - 1 do
      LookupTable_Code2.Add(Items[I].Code2, I);
    IsLookupTableValid_Code2 := True;
  end;
end;

procedure TReadonlyTerritoryList.ValidateLookupTable_Code3;
var
  I: Integer;
begin
  if not IsLookupTableValid_Code3 then
  begin
    if Assigned(LookupTable_Code3) then
      LookupTable_Code3.Clear
    else
      LookupTable_Code3 := TKeyLookup<String,Integer>.Create;
    for I := 0 to Count - 1 do
      LookupTable_Code3.Add(Items[I].Code3, I);
    IsLookupTableValid_Code3 := True;
  end;
end;

function TReadonlyTerritoryList.First: TTerritoryInfo;
begin
  if Count <> 0 then
    Result := Items[0]
  else
    Result := nil;
end;

function TReadonlyTerritoryList.Last: TTerritoryInfo;
begin
  if Count <> 0 then
    Result := Items[Count - 1]
  else
    Result := nil;
end;

procedure TReadonlyTerritoryList.AssignTo(Dest: TPersistent);
var
  Strings: TStrings absolute Dest;
  I: Integer;
begin
  if Dest is TStrings then
  begin
    Strings.BeginUpdate;
    try
      Strings.Clear;
      for I := 0 to Count - 1 do
        Strings.AddObject(Items[I].Code2, Items[I]);
    finally
      Strings.EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TReadonlyTerritoryList.AssignLabelsTo(Dest: TStrings;
  DisplayName: TTerritoryDisplayName);
var
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for I := 0 to Count - 1 do
      Dest.AddObject(Items[I].DisplayNames[DisplayName], Items[I]);
  finally
    Dest.EndUpdate;
  end;
end;

function TReadonlyTerritoryList.Find(GeoID: GEOID): TTerritoryInfo;
var
  Index: Integer;
begin
  if not IsLookupTableValid_GeoID then
    ValidateLookupTable_GeoID;
  if LookupTable_GeoID.Retrieve(GeoID, Index) then
    Result := Items[Index]
  else
    Result := nil;
end;

function TReadonlyTerritoryList.Find(const Code: String): TTerritoryInfo;
var
  Index: Integer;
begin
  Result := nil;
  case Length(Code) of
    2:
    begin
      if not IsLookupTableValid_Code2 then
        ValidateLookupTable_Code2;
      if LookupTable_Code2.Retrieve(UpperCase(Code), Index) then
        Result := Items[Index];
    end;
    3:
    begin
      if not IsLookupTableValid_Code3 then
        ValidateLookupTable_Code3;
      if LookupTable_Code3.Retrieve(UpperCase(Code), Index) then
        Result := Items[Index];
    end;
  end;
end;

function TReadonlyTerritoryList.IndexOf(Territory: TTerritoryInfo): Integer;
begin
  if Assigned(Territory) then
    Result := IndexOf(Territory.GeoID)
  else
    Result := -1;
end;

function TReadonlyTerritoryList.IndexOf(GeoID: GEOID): Integer;
begin
  if not IsLookupTableValid_GeoID then
    ValidateLookupTable_GeoID;
  if not LookupTable_GeoID.Retrieve(GeoID, Result) then
    Result := -1;
end;

function TReadonlyTerritoryList.IndexOf(const Code: String): Integer;
begin
  case Length(Code) of
    2:
    begin
      if not IsLookupTableValid_Code2 then
        ValidateLookupTable_Code2;
      if not LookupTable_Code2.Retrieve(UpperCase(Code), Result) then
        Result := -1;
    end;
    3:
    begin
      if not IsLookupTableValid_Code3 then
        ValidateLookupTable_Code3;
      if not LookupTable_Code3.Retrieve(UpperCase(Code), Result) then
        Result := -1;
    end;
  else
    Result := -1;
  end;
end;

function TReadonlyTerritoryList.IndexOfName(const AName: String;
  DisplayName: TTerritoryDisplayName): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(Items[I].DisplayNames[DisplayName], AName) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TReadonlyTerritoryList.Exists(Territory: TTerritoryInfo): Boolean;
begin
  Result := Assigned(Territory) and Exists(Territory.GeoID);
end;

function TReadonlyTerritoryList.Exists(GeoID: GEOID): Boolean;
begin
  if not IsLookupTableValid_GeoID then
    ValidateLookupTable_GeoID;
  Result := LookupTable_GeoID.Exists(GeoID);
end;

function TReadonlyTerritoryList.Exists(const Code: String): Boolean;
begin
  case Length(Code) of
    2:
    begin
      if not IsLookupTableValid_Code2 then
        ValidateLookupTable_Code2;
      Result := LookupTable_Code2.Exists(UpperCase(Code));
    end;
    3:
    begin
      if not IsLookupTableValid_Code3 then
        ValidateLookupTable_Code3;
      Result := LookupTable_Code3.Exists(UpperCase(Code));
    end;
  else
    Result := False;
  end;
end;

{ TTerritoryList }

function TTerritoryList.GetCapacity: Integer;
begin
  Result := List.Capacity;
end;

procedure TTerritoryList.SetCapacity(Value: Integer);
begin
  List.Capacity := Value;
  if Value < Count then
    InvalidateLookupTables;
end;

procedure TTerritoryList.ReadTerritory(Reader: TReader);
var
  Code2: String;
begin
  BeginUpdate;
  try
    Clear;
    Reader.ReadListBegin;
    while not Reader.EndOfList do
    begin
      Code2 := Reader.ReadString;
      if Code2 = '*' then
      begin
        Assign(World.Territories);
        Break;
      end;
      Add(Code2);
    end;
    Reader.ReadListEnd;
  finally
    EndUpdate;
  end;
end;

procedure TTerritoryList.WriteTerritory(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  if (Duplicates <> dupAccept) and (Count = World.Territories.Count) then
    Writer.WriteString('*') // all territories
  else
    for I := 0 to Count - 1 do
      Writer.WriteString(Items[I].Code2);
  Writer.WriteListEnd;
end;

procedure TTerritoryList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Territories', ReadTerritory, WriteTerritory, Count <> 0);
end;

procedure TTerritoryList.Changing;
begin
  if (UpdateCount = 0) and Assigned(OnChanging) then
    OnChanging(Self);
end;

procedure TTerritoryList.Change;
begin
  if (UpdateCount = 0) and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TTerritoryList.BeginUpdate;
begin
  if UpdateCount = 0 then
    Changing;
  Inc(fUpdateCount);
end;

procedure TTerritoryList.EndUpdate;
begin
  Dec(fUpdateCount);
  if UpdateCount = 0 then
    Change;
end;

procedure TTerritoryList.Clear;
begin
  if Count <> 0 then
  begin
    Changing;
    try
      List.Clear;
      ClearLookupTables;
    finally
      Change;
    end;
  end;
end;

procedure TTerritoryList.Sort(OrderBy: TTerritoryDisplayName);

  procedure QuickSort(L, R: Integer);
  var
    I, J, P: Integer;
    Pivot: String;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        Pivot := Items[P].DisplayNames[OrderBy];
        while AnsiCompareText(Items[I].DisplayNames[OrderBy], Pivot) < 0 do
          Inc(I);
        while AnsiCompareText(Items[J].DisplayNames[OrderBy], Pivot) > 0 do
          Dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            List.Exchange(I, J);
            InvalidateLookupTables;
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if (OrderBy <> tnNone) and (Count > 1) then
  begin
    Changing;
    try
      QuickSort(0, Count - 1);
    finally
      Change;
    end;
  end;
end;

procedure TTerritoryList.Assign(Source: TPersistent);
begin
  if Source is TReadonlyTerritoryList then
    Apply(TReadonlyTerritoryList(Source), laCopy)
  else
    inherited Assign(Source);
end;

procedure TTerritoryList.Apply(ListA: TReadonlyTerritoryList; Op: TListAssignOp;
  ListB: TReadonlyTerritoryList);
begin
  Changing;
  try
    if Assigned(ListB) then
      List.Assign(ListA.List, Op, ListB.List)
    else
      List.Assign(ListA.List, Op, nil);
    InvalidateLookupTables;
  finally
    Change;
  end;
end;

function TTerritoryList.Add(Territory: TTerritoryInfo): Integer;
begin
  if Assigned(Territory) then
  begin
    if Duplicates <> dupAccept then
    begin
      Result := IndexOf(Territory);
      if Result >= 0 then
      begin
        if Duplicates = dupError then
          raise EListError.CreateResFmt(@SDuplicateItemError, [Result]);
        Exit;
      end;
    end;
    Changing;
    try
      Result := List.Add(Territory);
      AddLookupEntries(Territory, Result);
    finally
      Change;
    end;
  end
  else
    Result := -1;
end;

function TTerritoryList.Add(GeoID: GEOID): Integer;
begin
  Result := Add(World.Territories.Find(GeoID));
end;

function TTerritoryList.Add(const Code: String): Integer;
begin
  Result := Add(World.Territories.Find(Code));
end;

function TTerritoryList.Remove(Territory: TTerritoryInfo): Integer;
begin
  Result := IndexOf(Territory);
  if Result >= 0 then
    Delete(Result);
end;

function TTerritoryList.Remove(GeoID: GEOID): Integer;
begin
  Result := IndexOf(GeoID);
  if Result >= 0 then
    Delete(Result);
end;

function TTerritoryList.Remove(const Code: String): Integer;
begin
  Result := IndexOf(Code);
  if Result >= 0 then
    Delete(Result);
end;

procedure TTerritoryList.Delete(Index: Integer);
begin
  Changing;
  try
    RemoveLookupEntries(Items[Index]);
    List.Delete(Index);
  finally
    Change;
  end;
end;

{ TCurrency }

constructor TCurrencyInfo.Create(LocaleID: LCID);
begin
  if not World.Changing then
    raise Exception.CreateResFmt(@SInstanceCreateError, [ClassName]);
  fCultures := TCultureList.Create;
  fCountries := TTerritoryList.Create;
  fIntlSymbol := GetLocaleStr(LocaleID, LOCALE_SINTLSYMBOL, '');
  fLocalSymbol := GetLocaleStr(LocaleID, LOCALE_SCURRENCY, '');
  fEnglishName := GetLocaleStr(LocaleID, LOCALE_SENGCURRNAME, '');
end;

constructor TCurrencyInfo.Create(const Locale: String);
begin
  Create(LocaleNameToLCID(Locale));
end;

destructor TCurrencyInfo.Destroy;
begin
  if not World.Changing then
    raise Exception.CreateResFmt(@SInstanceDestroyError, [ClassName]);
  fCultures.Free;
  fCountries.Free;
  inherited Destroy;
end;

function TCurrencyInfo.GetDisplayNames(DisplayName: TCurrencyDisplayName): String;
begin
  case DisplayName of
    crnIntlSymbol:
      Result := IntlSymbol;
    crnEnglishName:
      Result := EnglishName;
    crnNativeName:
      Result := NativeName;
  else
    Result := '';
  end;
end;

function TCurrencyInfo.GetNativeName: String;
begin
  if Assigned(OriginCountry) then
    Result := OriginCountry.Cultures[0].NativeCurrencyName
  else
    Result := '';
end;

function TCurrencyInfo.GetCultures: TReadonlyCultureList;
begin
  Result := fCultures;
end;

function TCurrencyInfo.GetCountries: TReadonlyTerritoryList;
begin
  Result := fCountries;
end;

procedure TCurrencyInfo.Associate(Culture: TCultureInfo; Territory: TTerritoryInfo);
begin
  if Assigned(Culture) and not fCultures.Exists(Culture) then
    fCultures.Add(Culture);
  if Assigned(Territory) and not fCountries.Exists(Territory) then
  begin
    fCountries.Add(Territory);
    if Assigned(fOriginCountry) and (Pos(fOriginCountry.Code2, IntlSymbol) = 0) then
      fOriginCountry := nil;
    if (fCountries.Count = 1) or (Pos(Territory.Code2, IntlSymbol) <> 0) then
      fOriginCountry := Territory;
  end;
end;

{ TCurrencyEnumerator }

constructor TCurrencyEnumerator.Create(List: TReadonlyCurrencyList);
begin
  inherited Create(List.List);
end;

function TCurrencyEnumerator.GetCurrent: TCurrencyInfo;
begin
  Result := TCurrencyInfo(inherited GetCurrent);
end;

{ TReadonlyCurrencyList }

constructor TReadonlyCurrencyList.Create;
begin
  List := TList.Create;
end;

destructor TReadonlyCurrencyList.Destroy;
begin
  List.Free;
  if Assigned(LookupTable_IntlSymbol) then
    LookupTable_IntlSymbol.Free;
  inherited Destroy;
end;

function TReadonlyCurrencyList.GetCount: Integer;
begin
  Result := List.Count;
end;

function TReadonlyCurrencyList.GetItems(Index: Integer): TCurrencyInfo;
begin
  Result := TCurrencyInfo(List[Index]);
end;

function TReadonlyCurrencyList.GetEnumerator: TCurrencyEnumerator;
begin
  Result := TCurrencyEnumerator.Create(Self);
end;

procedure TReadonlyCurrencyList.AddLookupEntries(Currency: TCurrencyInfo; Index: Integer);
begin
  if IsLookupTableValid_IntlSymbol then
    LookupTable_IntlSymbol.Add(Currency.IntlSymbol, Index);
end;

procedure TReadonlyCurrencyList.RemoveLookupEntries(Currency: TCurrencyInfo);
begin
  if IsLookupTableValid_IntlSymbol then
    LookupTable_IntlSymbol.Remove(Currency.IntlSymbol);
end;

procedure TReadonlyCurrencyList.ClearLookupTables;
begin
  if Assigned(LookupTable_IntlSymbol) then
  begin
    LookupTable_IntlSymbol.Clear;
    IsLookupTableValid_IntlSymbol := False;
  end;
end;

procedure TReadonlyCurrencyList.InvalidateLookupTables;
begin
  IsLookupTableValid_IntlSymbol := False;
end;

procedure TReadonlyCurrencyList.ValidateLookupTable_IntlSymbol;
var
  I: Integer;
begin
  if not IsLookupTableValid_IntlSymbol then
  begin
    if Assigned(LookupTable_IntlSymbol) then
      LookupTable_IntlSymbol.Clear
    else
      LookupTable_IntlSymbol := TKeyLookup<String,Integer>.Create;
    for I := 0 to Count - 1 do
      LookupTable_IntlSymbol.Add(Items[I].IntlSymbol, I);
    IsLookupTableValid_IntlSymbol := True;
  end;
end;

function TReadonlyCurrencyList.First: TCurrencyInfo;
begin
  if Count <> 0 then
    Result := Items[0]
  else
    Result := nil;
end;

function TReadonlyCurrencyList.Last: TCurrencyInfo;
begin
  if Count <> 0 then
    Result := Items[Count - 1]
  else
    Result := nil;
end;

procedure TReadonlyCurrencyList.AssignTo(Dest: TPersistent);
var
  Strings: TStrings absolute Dest;
  I: Integer;
begin
  if Dest is TStrings then
  begin
    Strings.BeginUpdate;
    try
      Strings.Clear;
      for I := 0 to Count - 1 do
        Strings.AddObject(Items[I].IntlSymbol, Items[I]);
    finally
      Strings.EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TReadonlyCurrencyList.AssignLabelsTo(Dest: TStrings;
  DisplayName: TCurrencyDisplayName);
var
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for I := 0 to Count - 1 do
      Dest.AddObject(Items[I].DisplayNames[DisplayName], Items[I]);
  finally
    Dest.EndUpdate;
  end;
end;

function TReadonlyCurrencyList.Find(const IntlSymbol: String): TCurrencyInfo;
var
  Index: Integer;
begin
  if not IsLookupTableValid_IntlSymbol then
    ValidateLookupTable_IntlSymbol;
  if LookupTable_IntlSymbol.Retrieve(UpperCase(IntlSymbol), Index) then
    Result := Items[Index]
  else
    Result := nil;
end;

function TReadonlyCurrencyList.IndexOf(Currency: TCurrencyInfo): Integer;
begin
  if Assigned(Currency) then
    Result := IndexOf(Currency.IntlSymbol)
  else
    Result := -1;
end;

function TReadonlyCurrencyList.IndexOf(const IntlSymbol: String): Integer;
begin
  if not IsLookupTableValid_IntlSymbol then
    ValidateLookupTable_IntlSymbol;
  if not LookupTable_IntlSymbol.Retrieve(UpperCase(IntlSymbol), Result) then
    Result := -1;
end;

function TReadonlyCurrencyList.IndexOfName(const AName: String;
  DisplayName: TCurrencyDisplayName): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(Items[I].DisplayNames[DisplayName], AName) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TReadonlyCurrencyList.Exists(Currency: TCurrencyInfo): Boolean;
begin
  Result := Assigned(Currency) and Exists(Currency.IntlSymbol);
end;

function TReadonlyCurrencyList.Exists(const IntlSymbol: String): Boolean;
begin
  if not IsLookupTableValid_IntlSymbol then
    ValidateLookupTable_IntlSymbol;
  Result := LookupTable_IntlSymbol.Exists(UpperCase(IntlSymbol));
end;

{ TCurrencyList }

function TCurrencyList.GetCapacity: Integer;
begin
  Result := List.Capacity;
end;

procedure TCurrencyList.SetCapacity(Value: Integer);
begin
  List.Capacity := Value;
  if Value < Count then
    InvalidateLookupTables;
end;

procedure TCurrencyList.ReadCurrencies(Reader: TReader);
var
  IntlSymbol: String;
begin
  BeginUpdate;
  try
    Clear;
    Reader.ReadListBegin;
    while not Reader.EndOfList do
    begin
      IntlSymbol := Reader.ReadString;
      if IntlSymbol = '*' then
      begin
        Assign(World.Currencies);
        Break;
      end;
      Add(IntlSymbol);
    end;
    Reader.ReadListEnd;
  finally
    EndUpdate;
  end;
end;

procedure TCurrencyList.WriteCurrencies(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  if (Duplicates <> dupAccept) and (Count = World.Currencies.Count) then
    Writer.WriteString('*') // all currencies
  else
    for I := 0 to Count - 1 do
      Writer.WriteString(Items[I].IntlSymbol);
  Writer.WriteListEnd;
end;

procedure TCurrencyList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Currencies', ReadCurrencies, WriteCurrencies, Count <> 0);
end;

procedure TCurrencyList.Changing;
begin
  if (UpdateCount = 0) and Assigned(OnChanging) then
    OnChanging(Self);
end;

procedure TCurrencyList.Change;
begin
  if (UpdateCount = 0) and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCurrencyList.BeginUpdate;
begin
  if UpdateCount = 0 then
    Changing;
  Inc(fUpdateCount);
end;

procedure TCurrencyList.EndUpdate;
begin
  Dec(fUpdateCount);
  if UpdateCount = 0 then
    Change;
end;

procedure TCurrencyList.Clear;
begin
  if Count <> 0 then
  begin
    Changing;
    try
      List.Clear;
      ClearLookupTables;
    finally
      Change;
    end;
  end;
end;

procedure TCurrencyList.Sort(OrderBy: TCurrencyDisplayName);

  procedure QuickSort(L, R: Integer);
  var
    I, J, P: Integer;
    Pivot: String;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        Pivot := Items[P].DisplayNames[OrderBy];
        while AnsiCompareText(Items[I].DisplayNames[OrderBy], Pivot) < 0 do
          Inc(I);
        while AnsiCompareText(Items[J].DisplayNames[OrderBy], Pivot) > 0 do
          Dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            List.Exchange(I, J);
            InvalidateLookupTables;
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if (OrderBy <> crnNone) and (Count > 1) then
  begin
    Changing;
    try
      QuickSort(0, Count - 1);
    finally
      Change;
    end;
  end;
end;

procedure TCurrencyList.Assign(Source: TPersistent);
begin
  if Source is TReadonlyCurrencyList then
    Apply(TReadonlyCurrencyList(Source), laCopy)
  else
    inherited Assign(Source);
end;

procedure TCurrencyList.Apply(ListA: TReadonlyCurrencyList;
  Op: TListAssignOp; ListB: TReadonlyCurrencyList);
begin
  Changing;
  try
    if Assigned(ListB) then
      List.Assign(ListA.List, Op, ListB.List)
    else
      List.Assign(ListA.List, Op, nil);
    InvalidateLookupTables;
  finally
    Change;
  end;
end;

function TCurrencyList.Add(Currency: TCurrencyInfo): Integer;
begin
  if Assigned(Currency) then
  begin
    if Duplicates <> dupAccept then
    begin
      Result := IndexOf(Currency);
      if Result >= 0 then
      begin
        if Duplicates = dupError then
          raise EListError.CreateResFmt(@SDuplicateItemError, [Result]);
        Exit;
      end;
    end;
    Changing;
    try
      Result := List.Add(Currency);
      AddLookupEntries(Currency, Result);
    finally
      Change;
    end;
  end
  else
    Result := -1;
end;

function TCurrencyList.Add(const IntlSymbol: String): Integer;
begin
  Result := Add(World.Currencies.Find(IntlSymbol));
end;

function TCurrencyList.Remove(Currency: TCurrencyInfo): Integer;
begin
  Result := IndexOf(Currency);
  if Result >= 0 then
    Delete(Result);
end;

function TCurrencyList.Remove(const IntlSymbol: String): Integer;
begin
  Result := IndexOf(IntlSymbol);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCurrencyList.Delete(Index: Integer);
begin
  Changing;
  try
    RemoveLookupEntries(Items[Index]);
    List.Delete(Index);
  finally
    Change;
  end;
end;

{ World }

function LocaleEnumCallbackEx(lpLocaleString: PChar; dwFlags: DWORD;
  lParam: LPARAM): Integer; stdcall;
begin
  World.CultureList.Add(TCultureInfo.Create(lpLocaleString));
  Result := 1;
end;

function LocaleEnumCallback(lpLocaleString: PChar): Integer; stdcall;
var
  LocaleID: LCID;
begin
  LocaleID := StrToIntDef('$' + String(lpLocaleString), 0);
  if LocaleID <> 0 then
    World.CultureList.Add(TCultureInfo.Create(LocaleID));
  Result := 1;
end;

function GeoEnumCallBack(GeoID: GEOID): Integer; stdcall;
begin
  if not World.TerritoryList.Exists(GeoID) then
  begin
    {$IFDEF EXCLUDE_TERRITORIES_WITH_X_CODE}
    if Pos('X', GetGeoStr(GeoID, GEO_ISO3, 'X')) <> 1 then
    {$ENDIF}
    World.TerritoryList.Add(TTerritoryInfo.Create(GeoID));
  end;
  Result := 1;
end;

class procedure World.Startup;
begin
  CultureList := TCultureList.Create;
  TerritoryList := TTerritoryList.Create;
  CurrencyList := TCurrencyList.Create;
  CultureList.Capacity := 300;
  TerritoryList.Capacity := 300;
  CurrencyList.Capacity := 300;
  Changing := True;
  try
    EnumSystemLocales(@LocaleEnumCallback, LCID_SUPPORTED);
    {$IFNDEF EXCLUDE_TERRITORIES_WITH_NO_CULTURE_INFO}
    EnumSystemGeoID(GEOCLASS_NATION, 0, @GeoEnumCallBack);
    {$ENDIF}
  finally
    Changing := False;
  end;
  CultureList.List.Capacity := CultureList.Count;
  TerritoryList.List.Capacity := TerritoryList.Count;
  CurrencyList.List.Capacity := CurrencyList.Count;
end;

class procedure World.Cleanup;
var
  I: Integer;
begin
  Changing := True;
  try
    if Assigned(CultureList) then
    begin
      for I := 0 to CultureList.Count - 1 do
        CultureList[I].Free;
      FreeAndNil(CultureList);
    end;
    if Assigned(TerritoryList) then
    begin
      for I := 0 to TerritoryList.Count - 1 do
        TerritoryList[I].Free;
      FreeAndNil(TerritoryList);
    end;
    if Assigned(CurrencyList) then
    begin
      for I := 0 to CurrencyList.Count - 1 do
        CurrencyList[I].Free;
      FreeAndNil(CurrencyList);
    end;
  finally
    Changing := False;
  end;
end;

class function World.Cultures: TReadonlyCultureList;
begin
  if not Assigned(CultureList) then
    Startup;
  Result := CultureList;
end;

class function World.Territories: TReadonlyTerritoryList;
begin
  if not Assigned(TerritoryList) then
    Startup;
  Result := TerritoryList;
end;

class function World.Currencies: TReadonlyCurrencyList;
begin
  if not Assigned(CultureList) then
    Startup;
  Result := CurrencyList;
end;

initialization

finalization
  if Assigned(TheDefaultCalendar) then
    FreeAndNil(TheDefaultCalendar);
  World.Cleanup;
end.


