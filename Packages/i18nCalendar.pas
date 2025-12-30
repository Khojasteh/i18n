{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  Internationalization and Localization for Delphi                            }
{                                                                              }
{  Copyright (c) Kambiz Khojasteh                                              }
{  https://github.com/khojasteh/i18n                                           }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit contains base classes for calendar systems and date-time
/// manipulation.
unit i18nCalendar;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, Classes;

type

  TCalendar = class;
  TCalendarSettings = class;

  {$region 'xmldoc'}
  /// <summary>
  /// This is a class reference for the <see cref="TCalendar"/> class or for one of
  /// its descendants.
  /// </summary>
  /// <remarks>
  /// TCalendarClass is the metaclass for <see cref="TCalendar"/>. Its value
  /// is the class reference for <see cref="TCalendar"/> or for one of its
  /// descendants.
  /// </remarks>
  {$endregion}
  TCalendarClass = class of TCalendar;

  {$region 'xmldoc'}
  /// <summary>
  /// This is a class reference for the <see cref="TCalendarSettings"/> class or for
  /// one of its descendants.
  /// </summary>
  /// <remarks>
  /// TCalendarSettingsClass is the metaclass for <see cref="TCalendarSettings"/>.
  /// Its value is the class reference for <see cref="TCalendarSettings"/> or for
  /// one of its descendants.
  /// </remarks>
  {$endregion}
  TCalendarSettingsClass = class of TCalendarSettings;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies days of the week.
  /// </summary>
  {$endregion}
  TDayOfWeek = (Monday = 1, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

  {$region 'xmldoc'}
  /// <summary>
  /// This data type represents a set of days of the week.
  /// </summary>
  {$endregion}
  TDaysOfWeek = set of TDayOfWeek;


  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the rules that determine which week is the
  /// first week of a year.
  /// </summary>
  {$endregion}
  TWeekRule = (
    {$region 'xmldoc'}
    /// The first week of the year starts on the first day of the year and
    /// ends before the following designated first day of the week.
    {$endregion}
    wrFirstDayWeek,
    {$region 'xmldoc'}
    /// The first week of the year is the first week with four or more days
    /// before the designated first day of the week (if Monday is specified
    /// as the first day of the week, this rule complies with ISO 8601).
    {$endregion}
    wrFirstFourDayWeek,
    {$region 'xmldoc'}
    /// The first week of the year begins on the first occurrence of the
    /// designated first day of the week on or after the first day of the year.
    {$endregion}
    wrFullWeek
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the algorithm types of the calendar systems.
  /// </summary>
  {$endregion}
  TCalendarKind = (
    {$region 'xmldoc'}
    /// The calendar type is unknown.
    {$endregion}
    ckUnknown,
    {$region 'xmldoc'}
    /// The calendar is based on the solar year and seasons.
    {$endregion}
    ckSolar,
    {$region 'xmldoc'}
    /// The calendar is based on the path of the moon.
    {$endregion}
    ckLunar,
    {$region 'xmldoc'}
    /// The calendar indicates both the moon phase and the time of the solar year.
    {$endregion}
    ckLunisolar
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This data type represents a date as a four-byte (DWORD) value.
  /// </summary>
  {$endregion}
  TDateYMD = packed record
    {$region 'xmldoc'}
    /// Year in range -32768 to 32767
    {$endregion}
    Year: SmallInt;
    {$region 'xmldoc'}
    /// Month of the year
    {$endregion}
    Month: Byte;
    {$region 'xmldoc'}
    /// Day of the month
    {$endregion}
    Day: Byte;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class is the base class for classes that provide locale-specific settings
  /// for a calendar.
  /// </summary>
  /// <remarks>
  /// TCalendarSettings is an abstract class that cannot be instantiated. Descendant
  /// classes override many of the methods of the TCalendarSettings class to provide
  /// calendar-specific information.
  /// </remarks>
  {$endregion}
  TCalendarSettings = class abstract(TPersistent)
  private
    fLocale: String;
    fCalendarName: String;
    fEraNames: array of String;
    fShortEraNames: array of String;
    fMonthNames: array of String;
    fShortMonthNames: array of String;
    fGenitiveMonthNames: array of String;
    fDayNames: array[TDayOfWeek] of String;
    fShortDayNames: array[TDayOfWeek] of String;
    fShortestDayNames: array[TDayOfWeek] of String;
    fLongDateFormat: String;
    fShortDateFormat: String;
    fYearMonthFormat: String;
    fMonthDayFormat: String;
    fYearFormat: String;
    fTwoDigitYearMax: Word;
    fWeekRule: TWeekRule;
    fFirstDayOfWeek: TDayOfWeek;
    fDateSeparator: String;
    fTimeSeparator: String;
    fTimeAMString: String;
    fTimePMString: String;
    fShortTimeFormat: String;
    fLongTimeFormat: String;
    fUpdateCount: Integer;
    fUpdateAffected: Boolean;
    fOnChange: TNotifyEvent;
    procedure SetCalendarName(const Value: String);
    function GetEraCount: Integer; inline;
    function GetEraNames(Era: Integer): String; inline;
    procedure SetEraNames(Era: Integer; const Value: String);
    function GetShortEraNames(Era: Integer): String; inline;
    procedure SetShortEraNames(Era: Integer; const Value: String);
    function GetMonthCount: Integer; inline;
    function GetMonthNames(Month: Integer): String; inline;
    procedure SetMonthNames(Month: Integer; const Value: String);
    function GetShortMonthNames(Month: Integer): String; inline;
    procedure SetShortMonthNames(Month: Integer; const Value: String);
    function GetGenitiveMonthNames(Month: Integer): String; inline;
    procedure SetGenitiveMonthNames(Month: Integer; const Value: String);
    function GetDayNames(DoW: TDayOfWeek): String; inline;
    procedure SetDayNames(DoW: TDayOfWeek; const Value: String);
    function GetShortDayNames(DoW: TDayOfWeek): String; inline;
    procedure SetShortDayNames(DoW: TDayOfWeek; const Value: String);
    function GetShortestDayNames(DoW: TDayOfWeek): String; inline;
    procedure SetShortestDayNames(DoW: TDayOfWeek; const Value: String);
    procedure SetLongDateFormat(const Value: String);
    procedure SetShortDateFormat(const Value: String);
    procedure SetYearMonthFormat(const Value: String);
    procedure SetMonthDayFormat(const Value: String);
    procedure SetYearFormat(const Value: String);
    procedure SetTwoDigitYearMax(Value: Word);
    procedure SetWeekRule(Value: TWeekRule);
    procedure SetFirstDayOfWeek(Value: TDayOfWeek);
    procedure SetDateSeparator(const Value: String);
    procedure SetTimeSeparator(const Value: String);
    procedure SetTimeAMString(const Value: String);
    procedure SetTimePMString(const Value: String);
    procedure SetShortTimeFormat(const Value: String);
    procedure SetLongTimeFormat(const Value: String);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Performs a case-insensitive search in the <paramref name="Names"/> array
    /// to find the block of characters given by the <paramref name="S"/> parameter.
    /// </summary>
    /// <param name="S">
    /// The search string.
    /// </param>
    /// <param name="Names">
    /// The array of names to search.
    /// </param>
    /// <returns>
    /// The position of <paramref name="S"/> in the <paramref name="Names"/>
    /// array (positioning from one), or zero if <paramref name="S"/> is not found.
    /// </returns>
    /// <remarks>
    /// If the search succeeds, the <paramref name="S"/> pointer will be advanced
    /// by the length of the matched name.
    /// </remarks>
    {$endregion}
    function ScanNames(var S: PChar; Names: array of String): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches for the block of characters given by the <paramref name="S"/> parameter
    /// within the <see cref="EraNames"/> property. The search is case-insensitive.
    /// </summary>
    /// <param name="S">
    /// The search string.
    /// </param>
    /// <returns>
    /// Index of <paramref name="S"/> in the <see cref="EraNames"/> property,
    /// or zero if <paramref name="S"/> is not found.
    /// </returns>
    /// <remarks>
    /// If the search succeeds, the <paramref name="S"/> pointer will be advanced
    /// by the length of the matched name.
    /// </remarks>
    {$endregion}
    function ScanEraNames(var S: PChar): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches for the block of characters given by the <paramref name="S"/> parameter
    /// within the <see cref="ShortEraNames"/> property. The search is case-insensitive.
    /// </summary>
    /// <param name="S">
    /// The search string.
    /// </param>
    /// <returns>
    /// Index of <paramref name="S"/> in the <see cref="ShortEraNames"/>
    /// property, or zero if <paramref name="S"/> is not found.
    /// </returns>
    /// <remarks>
    /// If the search succeeds, the <paramref name="S"/> pointer will be advanced
    /// by the length of the matched name.
    /// </remarks>
    {$endregion}
    function ScanShortEraNames(var S: PChar): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches for the block of characters given by the <paramref name="S"/> parameter
    /// within the <see cref="MonthNames"/> property. The search is case-insensitive.
    /// </summary>
    /// <param name="S">
    /// The search string.
    /// </param>
    /// <returns>
    /// Index of <paramref name="S"/> in the <see cref="MonthNames"/> property,
    /// or zero if <paramref name="S"/> is not found.
    /// </returns>
    /// <remarks>
    /// If the search succeeds, the <paramref name="S"/> pointer will be advanced
    /// by the length of the matched name.
    /// </remarks>
    {$endregion}
    function ScanMonthNames(var S: PChar): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches for the block of characters given by the <paramref name="S"/> parameter
    /// within the <see cref="ShortMonthNames"/> property. The search is case-insensitive.
    /// </summary>
    /// <param name="S">
    /// The search string.
    /// </param>
    /// <returns>
    /// Index of <paramref name="S"/> in the <see cref="ShortMonthNames"/> property,
    /// or zero if <paramref name="S"/> is not found.
    /// </returns>
    /// <remarks>
    /// If the search succeeds, the <paramref name="S"/> pointer will be advanced
    /// by the length of the matched name.
    /// </remarks>
    {$endregion}
    function ScanShortMonthNames(var S: PChar): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches for the block of characters given by the <paramref name="S"/> parameter
    /// within the <see cref="GenitiveMonthNames"/> property. The search is case-insensitive.
    /// </summary>
    /// <param name="S">
    /// The search string.
    /// </param>
    /// <returns>
    /// Index of <paramref name="S"/> in the <see cref="GenitiveMonthNames"/>
    /// property, or zero if <paramref name="S"/> is not found.
    /// </returns>
    /// <remarks>
    /// If the search succeeds, the <paramref name="S"/> pointer will be advanced
    /// by the length of the matched name.
    /// </remarks>
    {$endregion}
    function ScanGenitiveMonthNames(var S: PChar): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches for the block of characters given by the <paramref name="S"/> parameter
    /// within the <see cref="DayNames"/> property. The search is case-insensitive.
    /// </summary>
    /// <param name="S">
    /// The search string.
    /// </param>
    /// <returns>
    /// Index of <paramref name="S"/> in the <see cref="DayNames"/> property,
    /// or zero if <paramref name="S"/> is not found.
    /// </returns>
    /// <remarks>
    /// If the search succeeds, the <paramref name="S"/> pointer will be advanced
    /// by the length of the matched name.
    /// </remarks>
    {$endregion}
    function ScanDayNames(var S: PChar): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches for the block of characters given by the <paramref name="S"/> parameter
    /// within the <see cref="ShortDayNames"/> property. The search is case-insensitive.
    /// </summary>
    /// <param name="S">
    /// The search string.
    /// </param>
    /// <returns>
    /// Index of <paramref name="S"/> in the <see cref="ShortDayNames"/> property,
    /// or zero if <paramref name="S"/> is not found.
    /// </returns>
    /// <remarks>
    /// If the search succeeds, the <paramref name="S"/> pointer will be advanced
    /// by the length of the matched name.
    /// </remarks>
    {$endregion}
    function ScanShortDayNames(var S: PChar): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches for the block of characters given by the <paramref name="S"/> parameter
    /// within the <see cref="ShortestDayNames"/> property. The search is case-insensitive.
    /// </summary>
    /// <param name="S">
    /// The search string.
    /// </param>
    /// <returns>
    /// Index of <paramref name="S"/> in the <see cref="ShortestDayNames"/> property,
    /// or zero if <paramref name="S"/> is not found.
    /// </returns>
    /// <remarks>
    /// If the search succeeds, the <paramref name="S"/> pointer will be advanced
    /// by the length of the matched name.
    /// </remarks>
    {$endregion}
    function ScanShortestDayNames(var S: PChar): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches for the block of characters given by the <paramref name="S"/> parameter
    /// within the <see cref="TimeAMString"/> and <see cref="TimePMString"/> properties.
    /// The search is case-insensitive.
    /// </summary>
    /// <param name="S">
    /// The search string.
    /// </param>
    /// <returns>
    /// If string <paramref name="S"/> is identical to the value of <see cref="TimeAMString"/> or
    /// <see cref="TimePMString"/> properties, respectively returns one or two. If the string
    /// is not identical to either of the properties, returns zero.
    /// </returns>
    /// <remarks>
    /// If the search succeeds, the <paramref name="S"/> pointer will be advanced
    /// by the length of the matched name.
    /// </remarks>
    {$endregion}
    function ScanAMPM(var S: PChar): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves a piece of information for a locale by looking in the Windows calendar
    /// information. If the retrieval from Windows calendar information fails, then tries
    /// to retrieve the information from the Windows locale information.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <param name="CalID">
    /// The identifier of calendar system.
    /// </param>
    /// <param name="LType">
    /// The identifier of locale information to retrieve.
    /// </param>
    /// <param name="CType">
    /// The identifier of calendar information to retrieve.
    /// </param>
    /// <param name="Default">
    /// The default value of the information if the function failed.
    /// </param>
    /// <returns>
    /// The locale or calendar information.
    /// </returns>
    {$endregion}
    function SafeGetInfoStr(const Locale: String; CalID: Cardinal;
      LType, CType: Cardinal; const Default: String): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a given month number to its index in the Windows calendar
    /// information.
    /// </summary>
    /// <param name="Month">
    /// The month number.
    /// </param>
    /// <returns>
    /// The zero-based index of the given month number in the Windows calendar
    /// information.
    /// </returns>
    {$endregion}
    function GetMonthInfoIndex(Month: Integer): Integer; virtual;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChange"/> event.
    /// </summary>
    {$endregion}
    procedure DoChange; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="EraNames"/> and <see cref="ShortEraNames"/> properties for
    /// the specified era based on the given locale and calendar identifier.
    /// </summary>
    /// <param name="Era">
    /// The era whose name properties should be set.
    /// </param>
    /// <param name="Locale">
    /// The locale of the name.
    /// </param>
    /// <param name="CalendarID">
    /// The identifier of calendar system.
    /// </param>
    {$endregion}
    procedure PrepareEraName(Era: Integer; const Locale: String; CalendarID: Cardinal); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="MonthNames"/>, <see cref="ShortMonthNames"/> and
    /// <see cref="GenitiveMonthNames"/> properties based on the given locale and
    /// calendar identifier.
    /// </summary>
    /// <param name="Locale">
    /// The locale of the names.
    /// </param>
    /// <param name="CalendarID">
    /// The identifier of calendar system.
    /// </param>
    {$endregion}
    procedure PrepareMonthNames(const Locale: String; CalendarID: Cardinal); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="DayNames"/>, <see cref="ShortDayNames"/> and
    /// <see cref="ShortestDayNames"/> properties based on the given locale and
    /// calendar identifier.
    /// </summary>
    /// <param name="Locale">
    /// The locale of the names.
    /// </param>
    /// <param name="CalendarID">
    /// The identifier of calendar system.
    /// </param>
    {$endregion}
    procedure PrepareDayNames(const Locale: String; CalendarID: Cardinal); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="LongDateFormat"/>, <see cref="ShortDateFormat"/>,
    /// <see cref="YearMonthFormat"/>, <see cref="MonthDayFormat"/> and
    /// <see cref="YearFormat"/> properties based on the given locale and calendar
    /// identifier.
    /// </summary>
    /// <param name="Locale">
    /// The locale of the formats.
    /// </param>
    /// <param name="CalendarID">
    /// The identifier of calendar system.
    /// </param>
    {$endregion}
    procedure PrepareDateFormats(const Locale: String; CalendarID: Cardinal); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="LongTimeFormat"/>, <see cref="ShortTimeFormat"/>,
    /// <see cref="TimeAMString"/>, <see cref="TimePMString"/>,
    /// <see cref="TimeSeparator"/> and <see cref="DateSeparator"/> properties
    /// based on the given locale.
    /// </summary>
    /// <param name="Locale">
    /// The locale of the formats.
    /// </param>
    {$endregion}
    procedure PrepareTimeFormats(const Locale: String); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="FirstDayOfWeek"/> and <see cref="WeekRule"/> properties
    /// based on the given locale.
    /// </summary>
    /// <param name="Locale">
    /// The locale for the week information.
    /// </param>
    {$endregion}
    procedure PrepareWeekParams(const Locale: String); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets all writable properties based on the given locale and calendar
    /// identifier.
    /// </summary>
    /// <param name="Locale">
    /// The locale of the settings.
    /// </param>
    /// <param name="CalendarID">
    /// The identifier of calendar system.
    /// </param>
    {$endregion}
    procedure PrepareSettings(const Locale: String; CalendarID: Cardinal); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Loads all the writable properties from a stream.
    /// </summary>
    /// <param name="Reader">
    /// The reader object connected to the stream.
    /// </param>
    /// <seealso cref="WriteSettings"/>
    {$endregion}
    procedure ReadSettings(Reader: TReader); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Saves all the writable properties to a stream.
    /// </summary>
    /// <param name="Writer">
    /// The writer object connected to the stream.
    /// </param>
    /// <seealso cref="ReadSettings"/>
    {$endregion}
    procedure WriteSettings(Writer: TWriter); virtual;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class without setting any value for the locale
    /// specific properties.
    /// </summary>
    {$endregion}
    constructor Create; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class, and collects the values of the properties based
    /// on the given locale identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    {$endregion}
    constructor Create(LocaleID: Cardinal); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class, and collects the values of the properties based
    /// on the given locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    {$endregion}
    constructor Create(const Locale: String); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that these settings are provided for.
    /// </summary>
    /// <returns>
    /// The <see cref="TCalendar"/> class or one of its descendants.
    /// </returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Collects the values of the properties based on the given locale identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    {$endregion}
    procedure Prepare(LocaleID: Cardinal); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Collects the values of the properties based on the given locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    {$endregion}
    procedure Prepare(const Locale: String); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Loads the settings from a stream. The stream must be created previously
    /// by the same <see cref="TCalendarSettings"/> class.
    /// </summary>
    /// <param name="Stream">
    /// The stream containing the previously saved settings.
    /// </param>
    /// <seealso cref="SaveToStream"/>
    {$endregion}
    procedure LoadFromStream(Stream: TStream);
    {$region 'xmldoc'}
    /// <summary>
    /// Saves the settings to a stream.
    /// </summary>
    /// <param name="Stream">
    /// The stream to store the settings.
    /// </param>
    /// <seealso cref="LoadFromStream"/>
    {$endregion}
    procedure SaveToStream(Stream: TStream);
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the contents of another object to the current one.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents generation of the <see cref="OnChange"/> event until the
    /// <see cref="EndUpdate"/> method is called.
    /// </summary>
    /// <seealso cref="EndUpdate"/>
    {$endregion}
    procedure BeginUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Reenables <see cref="OnChange"/> event generation that was turned off
    /// with the <see cref="BeginUpdate"/> method.
    /// </summary>
    /// <seealso cref="BeginUpdate"/>
    {$endregion}
    procedure EndUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the locale name of the current settings.
    /// </summary>
    {$endregion}
    property Locale: String read fLocale;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the native name of the calendar in the current locale.
    /// </summary>
    {$endregion}
    property CalendarName: String read fCalendarName write SetCalendarName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of eras supported by the calendar.
    /// </summary>
    {$endregion}
    property EraCount: Integer read GetEraCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the full name of each calendar's eras in the current locale.
    /// </summary>
    {$endregion}
    property EraNames[Era: Integer]: String read GetEraNames write SetEraNames;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the short name of each calendar's eras in the current locale.
    /// </summary>
    {$endregion}
    property ShortEraNames[Era: Integer]: String read GetShortEraNames write SetShortEraNames;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the maximum number of months in the calendar in the current locale.
    /// </summary>
    {$endregion}
    property MonthCount: Integer read GetMonthCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the full name of each month of the calendar in the current locale.
    /// </summary>
    {$endregion}
    property MonthNames[Month: Integer]: string read GetMonthNames write SetMonthNames;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the short name of each month of the calendar in the current locale.
    /// </summary>
    {$endregion}
    property ShortMonthNames[Month: Integer]: string read GetShortMonthNames write SetShortMonthNames;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the genitive name of each month of the calendar in the current locale.
    /// </summary>
    {$endregion}
    property GenitiveMonthNames[Month: Integer]: string read GetGenitiveMonthNames write SetGenitiveMonthNames;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the full name of each day of week in the current locale.
    /// </summary>
    {$endregion}
    property DayNames[DoW: TDayOfWeek]: String read GetDayNames write SetDayNames;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the short name of each day of week in the current locale.
    /// </summary>
    {$endregion}
    property ShortDayNames[DoW: TDayOfWeek]: String read GetShortDayNames write SetShortDayNames;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the shortest name of each day of week in the current locale.
    /// </summary>
    {$endregion}
    property ShortestDayNames[DoW: TDayOfWeek]: String read GetShortestDayNames write SetShortestDayNames;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the long date format in the current locale.
    /// </summary>
    {$endregion}
    property LongDateFormat: String read fLongDateFormat write SetLongDateFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the short date format in the current locale.
    /// </summary>
    {$endregion}
    property ShortDateFormat: String read fShortDateFormat write SetShortDateFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the year-month format in the current locale.
    /// </summary>
    {$endregion}
    property YearMonthFormat: String read fYearMonthFormat write SetYearMonthFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the month-day format in the current locale.
    /// </summary>
    {$endregion}
    property MonthDayFormat: String read fMonthDayFormat write SetMonthDayFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the year format in the current locale.
    /// </summary>
    {$endregion}
    property YearFormat: String read fYearFormat write SetYearFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the last year of a 100-year range that can be represented by a 2-digit year.
    /// </summary>
    {$endregion}
    property TwoDigitYearMax: Word read fTwoDigitYearMax write SetTwoDigitYearMax;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the rule for determining the first week of the year for the current locale.
    /// </summary>
    {$endregion}
    property WeekRule: TWeekRule read fWeekRule write SetWeekRule;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the starting day of a week for the current locale.
    /// </summary>
    {$endregion}
    property FirstDayOfWeek: TDayOfWeek read fFirstDayOfWeek write SetFirstDayOfWeek;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the string that separates the components of a date, that is, the year, month, and day.
    /// </summary>
    {$endregion}
    property DateSeparator: String read fDateSeparator write SetDateSeparator;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the string that separates the components of time, that is, the hour, minutes, and seconds.
    /// </summary>
    {$endregion}
    property TimeSeparator: String read fTimeSeparator write SetTimeSeparator;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the string designator for hours that are "ante meridiem" (before noon).
    /// </summary>
    {$endregion}
    property TimeAMString: String read fTimeAMString write SetTimeAMString;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the string designator for hours that are "post meridiem" (after noon).
    /// </summary>
    {$endregion}
    property TimePMString: String read fTimePMString write SetTimePMString;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the long time format in the current locale.
    /// </summary>
    {$endregion}
    property ShortTimeFormat: String read fShortTimeFormat write SetShortTimeFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the short time format in the current locale.
    /// </summary>
    {$endregion}
    property LongTimeFormat: String read fLongTimeFormat write SetLongTimeFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when there is a change in any of the properties.
    /// </summary>
    {$endregion}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class is the base class for classes that encapsulate a calendar system.
  /// </summary>
  /// <remarks>
  /// TCalendar is an abstract class that cannot be instantiated. This class provides
  /// a uniform interface for managing date and time. Descendant classes override many
  /// of the methods of <see cref="TCalendar"/> class to provide calendar specific functionality.
  /// </remarks>
  {$endregion}
  TCalendar = class abstract(TPersistent)
  private
    fSettings: TCalendarSettings;
    fDefaultEra: Integer;
    fOnChange: TNotifyEvent;
    procedure SetSettings(Value: TCalendarSettings); inline;
    procedure SetDefaultEra(Value: Integer);
    procedure SettingsChanged(Sender: TObject);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Call this method when an Era is not in the valid range to raise an EEncodingError
    /// exception.
    /// </summary>
    /// <param name="Era">
    /// The era, which caused the error.
    /// </param>
    {$endregion}
    class procedure EraError(Era: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Call this method when a year is not in the valid range to raise an EEncodingError
    /// exception.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era, which caused the error.
    /// </param>
    {$endregion}
    class procedure YearError(Era, Year: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Call this method when a month is not in the valid range to raise an EEncodingError
    /// exception.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year, which caused the error.
    /// </param>
    {$endregion}
    class procedure MonthError(Era, Year, Month: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Call this method when a day is not in the valid range to raise an EEncodingError
    /// exception.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month, which caused the error.
    /// </param>
    {$endregion}
    class procedure DayError(Era, Year, Month, Day: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Call this method when a numeral representation of the day of the week is not
    /// in range 1 to 7 to raise an EConvertError exception.
    /// </summary>
    /// <param name="DayNum">
    /// The numeral representation of the day of the week.
    /// </param>
    {$endregion}
    class procedure DayNumError(DayNum: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Call this method when a <see cref="TDateTime"/> value is not in the valid range
    /// to raise an EConvertError exception.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value, which caused the error.
    /// </param>
    {$endregion}
    class procedure DateError(const DateTime: TDateTime);
    {$region 'xmldoc'}
    /// <summary>
    /// Call this method when a day of year is not in the valid range to raise an
    /// EEncodingError exception.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year, which caused the error.
    /// </param>
    {$endregion}
    class procedure DateDayError(Era, Year, DayOfYear: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Call this method when a week of year is not in the valid range to raise
    /// an EEncodingError exception.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Week">
    /// The week number of the year, which caused the error.
    /// </param>
    /// <param name="DoW">
    /// The day of the week, which caused the error.
    /// </param>
    {$endregion}
    class procedure DateWeekError(Era, Year, Week: Integer; DoW: TDayOfWeek);
    {$region 'xmldoc'}
    /// <summary>
    /// Call this method when conversion of a year from one era to another fails
    /// to raise an EConvertError exception.
    /// </summary>
    /// <param name="Year">
    /// The year that was going to convert.
    /// </param>
    /// <param name="FromEra">
    /// The source era.
    /// </param>
    /// <param name="ToEra">
    /// The target era.
    /// </param>
    {$endregion}
    class procedure ConvertEraError(Year, FromEra, ToEra: Integer);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Divides two integers, returning both quotient and remainder.
    /// </summary>
    /// <param name="Numerator">
    /// The numerator.
    /// </param>
    /// <param name="Denominator">
    /// The denominator.
    /// </param>
    /// <param name="Quotient">
    /// The quotient of the division.
    /// </param>
    /// <param name="Reminder">
    /// The remainder of the division.
    /// </param>
    /// <seealso cref="Modulus"/>
    {$endregion}
    class procedure Divide(Numerator, Denominator: Integer; out Quotient, Reminder: Integer); inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns remainder of an integer division.
    /// </summary>
    /// <param name="Numerator">
    /// The numerator.
    /// </param>
    /// <param name="Denominator">
    /// The denominator.
    /// </param>
    /// <returns>
    /// The remainder of the division.
    /// </returns>
    /// <seealso cref="Divide"/>
    {$endregion}
    class function Modulus(Numerator, Denominator: Integer): Integer; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// The starting epoch of <see cref="TDateTime"/> values in Julian days.
    /// </summary>
    {$endregion}
    const DATETIME_EPOCH = 2415018.5;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChange"/> event.
    /// </summary>
    {$endregion}
    procedure DoChange; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a date represented by its year, month and day components to its
    /// corresponding Julian day. The year should be in the calendar's base era.
    /// </summary>
    /// <param name="Year">
    /// The year in calendar's base era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Julian day of the specified date.
    /// </returns>
    /// <seealso cref="FromJulianDay"/>
    /// <seealso cref="BaseEra"/>
    {$endregion}
    function ToJulianDay(Year, Month, Day: Integer): Extended; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a date expressed in Julian day, to its year, month, and day components.
    /// The result year is in the calendar's base era.
    /// </summary>
    /// <param name="JD">
    /// The date expressed in Julian day.
    /// </param>
    /// <param name="Year">
    /// The year in calendar's base era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the function succeeds, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="ToJulianDay"/>
    /// <seealso cref="BaseEra"/>
    {$endregion}
    function FromJulianDay(const JD: Extended; out Year, Month, Day: Integer): Boolean; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Adjusts a Julian day before or after being converted to or from a date in a
    /// calendar system.
    /// </summary>
    /// <param name="JD">
    /// The date expressed in Julian day.
    /// </param>
    /// <param name="Backward">
    /// Indicates the direction of adjustment. If <see langword="true"/>, the Julian day
    /// is about to be converted to a date. If <see langword="false"/>, the Julian day
    /// is the result of converting a date.
    /// </param>
    /// <returns>
    /// Returns the adjusted Julian day.
    /// </returns>
    {$endregion}
    function AdjustJulianDay(const JD: Extended; Backward: Boolean): Extended; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the era that is used for internal calculations of the calendar.
    /// </summary>
    /// <returns>
    /// The base era of the calendar.
    /// </returns>
    {$endregion}
    function BaseEra: Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the era that is more suitable for expressing a year in the calendar's
    /// base era.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's base era.
    /// </param>
    /// <returns>
    /// The best era to express the specified year.
    /// </returns>
    /// <seealso cref="BaseEra"/>
    {$endregion}
    function BestEraOfBaseEraYear(Year: Integer): Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a year to zero-based year. Most calendar systems do not have year
    /// zero, but the TCalendar class uses zero-based year in its internal calculations.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// The year as zero-based for internal calculations.
    /// </returns>
    /// <remarks>
    /// It is safe to call this method for a calendar system that year zero is
    /// a valid year for it.
    /// </remarks>
    /// <seealso cref="FromZeroBase"/>
    {$endregion}
    function ToZeroBase(Era, Year: Integer): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a year previously converted to zero-based year by using the
    /// <see cref="ToZeroBase"/> method.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// The year as expected by the calendar system.
    /// </returns>
    /// <remarks>
    /// It is safe to call this method for a calendar system that year zero is
    /// a valid year for it.
    /// </remarks>
    /// <seealso cref="ToZeroBase"/>
    {$endregion}
    function FromZeroBase(Era, Year: Integer): Integer; inline;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Represents the number of days in a week.
    /// </summary>
    {$endregion}
    const DaysPerWeek = 7;
    {$region 'xmldoc'}
    /// <summary>
    /// Represents one day.
    /// </summary>
    {$endregion}
    const OneDay = 1.0;
    {$region 'xmldoc'}
    /// <summary>
    /// Represents one hour as fraction of a day.
    /// </summary>
    {$endregion}
    const OneHour = OneDay / HoursPerDay;
    {$region 'xmldoc'}
    /// <summary>
    /// Represents one minute as fraction of a day.
    /// </summary>
    {$endregion}
    const OneMinute = OneDay / MinsPerDay;
    {$region 'xmldoc'}
    /// <summary>
    /// Represents one second as fraction of a day.
    /// </summary>
    {$endregion}
    const OneSecond = OneDay / SecsPerDay;
    {$region 'xmldoc'}
    /// <summary>
    /// Represents one millisecond as fraction of a day.
    /// </summary>
    {$endregion}
    const OneMillisecond = OneDay / MSecsPerDay;
    {$region 'xmldoc'}
    /// <summary>
    /// Represents one week.
    /// </summary>
    {$endregion}
    const OneWeek = OneDay * DaysPerWeek;
    {$region 'xmldoc'}
    /// <summary>
    /// Represents no date or an invalid date value.
    /// </summary>
    {$endregion}
    const NoDate: TDateTime = -0.5;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class without initializing the locale-specific
    /// settings.
    /// </summary>
    {$endregion}
    constructor Create; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class, and initializes the locale-specific settings
    /// based on the given locale identifier.
    /// </summary>
    /// <param name="LocaleID">
    /// The locale identifier.
    /// </param>
    {$endregion}
    constructor Create(LocaleID: Integer); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class, and initializes the locale-specific settings
    /// based on the given locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    {$endregion}
    constructor Create(const Locale: String); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the contents of another object to the current one.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the contents of this object to another object.
    /// </summary>
    /// <param name="Dest">
    /// The target object.
    /// </param>
    {$endregion}
    procedure AssignTo(Dest: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates and returns a copy of the object, with the same class and with all the
    /// properties having the same values.
    /// </summary>
    /// <returns>
    /// The newly created <see cref="TCalendar"/> object.
    /// </returns>
    {$endregion}
    function Clone: TCalendar; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the locale-specific settings of the calendar.
    /// </summary>
    {$endregion}
    property Settings: TCalendarSettings read fSettings write SetSettings;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the default era of the calendar.
    /// </summary>
    {$endregion}
    property DefaultEra: Integer read fDefaultEra write SetDefaultEra;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when any of the calendar settings is changed.
    /// </summary>
    {$endregion}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the friendly name of the calendar system. This name is automatically
    /// made from the class name.
    /// </summary>
    /// <returns>
    /// The friendly name of the calendar system.
    /// </returns>
    {$endregion}
    class function CalendarName: String;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the unique identifier of the calendar.
    /// </summary>
    /// <returns>
    /// The unique identifier of the calendar.
    /// </returns>
    {$endregion}
    class function CalendarID: Cardinal; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the algorithm type of the calendar system.
    /// </summary>
    /// <returns>
    /// The algorithm type of the calendar system.
    /// </returns>
    {$endregion}
    class function CalendarKind: TCalendarKind; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TCalendarSettings"/> class that provides locale
    /// specific settings for the calendar.
    /// </summary>
    /// <returns>
    /// The <see cref="TCalendarSettings"/> class or one of its descendents.
    /// </returns>
    {$endregion}
    class function SettingsClass: TCalendarSettingsClass; virtual;
  { Min/Max Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of eras that the calendar class supports.
    /// </summary>
    /// <returns>
    /// The maximum number of eras.
    /// </returns>
    {$endregion}
    class function MaxEra: Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the maximum number of months that the calendar may have in a year.
    /// </summary>
    /// <returns>
    /// The maximum number of months per year.
    /// </returns>
    {$endregion}
    class function MaxMonthsPerYear: Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the minimum <see cref="TDateTime"/> value that can be managed by
    /// the calendar.
    /// </summary>
    /// <returns>
    /// The minimum supported <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="MaxSupportedDateTime"/>
    {$endregion}
    class function MinSupportedDateTime: TDateTime; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the maximum <see cref="TDateTime"/> value that can be managed by
    /// the calendar.
    /// </summary>
    /// <returns>
    /// The maximum supported <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="MinSupportedDateTime"/>
    {$endregion}
    class function MaxSupportedDateTime: TDateTime; virtual;
  { Miscellaneous Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the current date.
    /// </summary>
    /// <returns>
    /// The date of today.
    /// </returns>
    /// <seealso cref="Yesterday"/>
    /// <seealso cref="Tomorrow"/>
    {$endregion}
    class function Today: TDate; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the preceding day.
    /// </summary>
    /// <returns>
    /// The date of yesterday.
    /// </returns>
    /// <seealso cref="Today"/>
    /// <seealso cref="Tomorrow"/>
    {$endregion}
    class function Yesterday: TDate; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the following day.
    /// </summary>
    /// <returns>
    /// The date of tomorrow.
    /// </returns>
    /// <seealso cref="Today"/>
    /// <seealso cref="Yesterday"/>
    {$endregion}
    class function Tomorrow: TDate; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the first month of the year for the calendar.
    /// </summary>
    /// <returns>
    /// The first month of the year.
    /// </returns>
    {$endregion}
    class function FirstMonthOfYear: Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days from beginning of a specified year to beginning
    /// of a specified month.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// The number of days between start of the year and start of the month.
    /// </returns>
    {$endregion}
    function DaysToMonth(Era, Year, Month: Integer): Integer; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days from beginning of a specified year to beginning
    /// of a specified month.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in thecurrent calendar era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// The number of days between start of the year and start of the month.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function DaysToMonth(Year, Month: Integer): Integer; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days from beginning of the year to beginning of the month
    /// of a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The target date.
    /// </param>
    /// <returns>
    /// The number of days between start of the year and start of the month.
    /// </returns>
    {$endregion}
    function DaysToMonth(const DateTime: TDateTime): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days in a specified month of a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// The number of days in the month.
    /// </returns>
    {$endregion}
    function DaysInMonth(Era, Year, Month: Integer): Integer; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days in a specified month of a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// The number of days in the month.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function DaysInMonth(Year, Month: Integer): Integer; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days in the month of a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The target date.
    /// </param>
    /// <returns>
    /// The number of days in the month.
    /// </returns>
    {$endregion}
    function DaysInMonth(const DateTime: TDateTime): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days in a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// The number of days in the year.
    /// </returns>
    {$endregion}
    function DaysInYear(Era, Year: Integer): Integer; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days in a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <returns>
    /// The number of days in the year.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function DaysInYear(Year: Integer): Integer; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days in the year of a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The target date.
    /// </param>
    /// <returns>
    /// The number of days in the year.
    /// </returns>
    {$endregion}
    function DaysInYear(const DateTime: TDateTime): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of months in a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// The number of months in the year.
    /// </returns>
    {$endregion}
    function MonthsInYear(Era, Year: Integer): Integer; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of months in a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <returns>
    /// The number of months in the year.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function MonthsInYear(Year: Integer): Integer; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of months in the year of a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The target date.
    /// </param>
    /// <returns>
    /// The number of months in the year.
    /// </returns>
    {$endregion}
    function MonthsInYear(const DateTime: TDateTime): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of weeks in a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// The number of weeks in the year.
    /// </returns>
    {$endregion}
    function WeeksInYear(Era, Year: Integer): Integer; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of weeks in a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <returns>
    /// The number of weeks in the year.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function WeeksInYear(Year: Integer): Integer; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of weeks in the year of a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The target date.
    /// </param>
    /// <returns>
    /// The number of weeks in the year.
    /// </returns>
    {$endregion}
    function WeeksInYear(const DateTime: TDateTime): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of whole weeks in a specified month of a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// The number of whole weeks in the month.
    /// </returns>
    {$endregion}
    function WeeksInMonth(Era, Year, Month: Integer): Integer; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of whole weeks in a specified month of a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// The number of whole weeks in the month.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function WeeksInMonth(Year, Month: Integer): Integer; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of whole weeks in the month of a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The target date.
    /// </param>
    /// <returns>
    /// The number of whole weeks in the month.
    /// </returns>
    {$endregion}
    function WeeksInMonth(const DateTime: TDateTime): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds month and day of the month for a specified day of a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the function is succeeded, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DayOfYearToDayOfMonth(Era, Year, DayOfYear: Integer; var Month, Day: Integer): Boolean; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds month and day of the month for a specified day of a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the function is succeeded, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function DayOfYearToDayOfMonth(Year, DayOfYear: Integer; var Month, Day: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds day of year for a date specified by its era, year, month, and day
    /// components.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the function is succeeded, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DayOfMonthToDayOfYear(Era, Year, Month, Day: Integer; out DayOfYear: Integer): Boolean; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds day of year for a date specified by its year, month, and day
    /// components.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the function is succeeded, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function DayOfMonthToDayOfYear(Year, Month, Day: Integer; out DayOfYear: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a year from one era to another.
    /// </summary>
    /// <param name="Year">
    /// The year to convert.
    /// </param>
    /// <param name="FromEra">
    /// The source era.
    /// </param>
    /// <param name="ToEra">
    /// The target era.
    /// </param>
    /// <returns>
    /// The year expressed in the target era.
    /// </returns>
    {$endregion}
    function ConvertYear(Year, FromEra, ToEra: Integer): Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a two-digit year to four-digit.
    /// </summary>
    /// <param name="Year">
    /// The two-digit year.
    /// </param>
    /// <returns>
    /// The four-digit year.
    /// </returns>
    {$endregion}
    function ToFourDigitYear(Year: Integer): Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the year zero is a valid year for an era.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the era can have year zero, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function CanHaveZeroYear(Era: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a year by a specified number of years, and converts era of
    /// the year as well.
    /// </summary>
    /// <param name="Year">
    /// The year to increment.
    /// </param>
    /// <param name="NumberOfYears">
    /// The number of years to add.
    /// </param>
    /// <param name="FromEra">
    /// The era of the input year.
    /// </param>
    /// <param name="ToEra">
    /// The era of the output year.
    /// </param>
    /// <returns>
    /// The year incremented by the number of years.
    /// </returns>
    /// <remarks>
    /// Use this method to safely increment/decrement year regardless of whether
    /// year zero is a valid year for the source and target eras or not.
    /// </remarks>
    {$endregion}
    function OffsetYear(Year, NumberOfYears: Integer; FromEra, ToEra: Integer): Integer; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a year by a specified number of years.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="NumberOfYears">
    /// The number of years to add.
    /// </param>
    /// <returns>
    /// The year incremented by the number of years.
    /// </returns>
    /// <remarks>
    /// Use this method to safely increment/decrement year regardless of whether
    /// year zero is a valid year for the source and target eras or not.
    /// </remarks>
    {$endregion}
    function OffsetYear(Era, Year, NumberOfYears: Integer): Integer; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a year by a specified number of years.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="NumberOfYears">
    /// The number of years to add.
    /// </param>
    /// <returns>
    /// The year incremented by the number of years.
    /// </returns>
    /// <remarks>
    /// Use this method to safely increment/decrement year regardless of whether
    /// year zero is a valid year for the source and target eras or not.
    /// </remarks>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function OffsetYear(Year, NumberOfYears: Integer): Integer; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds the amount of days and fraction of a day to a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <remarks>
    /// The OffsetDateTime method will offset a <see cref="TDateTime"/> value by considering the
    /// following rules:
    /// <list type="bullet">
    ///   <item>
    ///   The fraction part in a <see cref="TDateTime"/> value is always a positive value even
    ///   when the whole value is negative.</item>
    ///   <item>
    ///   There is no <see cref="TDateTime"/> value in range (-1, 0).</item>
    /// </list>
    /// </remarks>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Delta">
    /// The amount of days (or fraction of a day) to add.
    /// </param>
    /// <returns>
    /// The new <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    class function OffsetDateTime(const DateTime: TDateTime; const Delta: Double): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the whole days between two specified <see cref="TDateTime"/> values.
    /// </summary>
    /// <param name="DateTime1">
    /// The first date.
    /// </param>
    /// <param name="DateTime2">
    /// The second date.
    /// </param>
    /// <returns>
    /// The whole days between two dates.
    /// </returns>
    {$endregion}
    class function DaysBetween(DateTime1, DateTime2: TDateTime): Integer; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days between two specified days of week.
    /// </summary>
    /// <param name="DoW1">
    /// The first day of week.
    /// </param>
    /// <param name="DoW2">
    /// The second day of week.
    /// </param>
    /// <returns>
    /// The number of whole days between two days of week.
    /// </returns>
    {$endregion}
    class function DaysOfWeekBetween(DoW1, DoW2: TDayOfWeek): Integer; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a specified TDayOfWeek value to its numeral representation,
    /// where the first day of the week is 1 and the last day of the week is 7.
    /// </summary>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <returns>
    /// The numeral representation of the day of the week in range 1 to 7.
    /// </returns>
    /// <remarks>
    /// The returned value is locale dependent.
    /// </remarks>
    /// <seealso cref="DayNumToDayOfWeek"/>
    {$endregion}
    function DayOfWeekToDayNum(DoW: TDayOfWeek): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a number in range 1 to 7 that represents the day of the week to
    /// a TDayOfWeek value.
    /// </summary>
    /// <param name="DayNum">
    /// The numeral representation of the day of the week in range 1 to 7, where
    /// the first day of the week is 1 and the last day of the week is 7.
    /// </param>
    /// <returns>
    /// The day of the week.
    /// </returns>
    /// <remarks>
    /// The input value is locale dependent.
    /// </remarks>
    /// <seealso cref="DayOfWeekToDayNum"/>
    {$endregion}
    function DayNumToDayOfWeek(DayNum: Integer): TDayOfWeek;
  { Is Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified <see cref="TDateTime"/> value is invalid.
    /// </summary>
    /// <param name="DateTime">
    /// The datatime value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is invalid,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsValidDate"/>
    {$endregion}
    class function IsNoDate(const DateTime: TDateTime): Boolean; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified <see cref="TDateTime"/> value is valid and supported
    /// by the calendar.
    /// </summary>
    /// <param name="DateTime">
    /// The data to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is valid and
    /// in the supported range of the calendar, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsValidDate(const DateTime: TDateTime): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified day in a specified year is valid.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is valid,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsValidDateDay(Era, Year, DayOfYear: Integer): Boolean; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified day in a specified year is valid.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is valid,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function IsValidDateDay(Year, DayOfYear: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified day of a specified week in a speified year is
    /// valid.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Week">
    /// The week number of the year.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is valid,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsValidDateWeek(Era, Year, Week: Integer; DoW: TDayOfWeek): Boolean; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified day of a specified week in a speified year is
    /// valid.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Week">
    /// The week number of the year.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is valid,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function IsValidDateWeek(Year, Week: Integer; DoW: TDayOfWeek): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a era is valid.
    /// </summary>
    /// <param name="Era">
    /// The era to check.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the era is valid, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsValidEra(Era: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified year in a specified era is valid.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the era and year pair is valid, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsValidYear(Era, Year: Integer): Boolean; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified year is valid.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the year is valid, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function IsValidYear(Year: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified month in a specified year is valid.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the era, year and month triple is valid,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsValidMonth(Era, Year, Month: Integer): Boolean; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified month in a specified year is valid.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the year and month pair is valid, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function IsValidMonth(Year, Month: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified day of a specified month in a specified year
    /// is valid.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the date is valid, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsValidDay(Era, Year, Month, Day: Integer): Boolean; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified day of a specified month in a specified year
    /// is valid.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the date is valid, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function IsValidDay(Year, Month, Day: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified year in a specified era is a leap year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the year is a leap year, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsLeapYear(Era, Year: Integer): Boolean; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified year is a leap year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the year is a leap year, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function IsLeapYear(Year: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified <see cref="TDateTime"/> value is within a leap year.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is in a leap
    /// year, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsInLeapYear(const DateTime: TDateTime): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Checks whether a specified <see cref="TDateTime"/> value is within the given date
    /// range (inclusive).
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value to examine.
    /// </param>
    /// <param name="RangeStart">
    /// The <see cref="TDateTime"/> value that specifies start of the range.
    /// </param>
    /// <param name="RangeEnd">
    /// The <see cref="TDateTime"/> value that specifies end of the range.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the datetime is within the range, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    class function IsInRange(const DateTime, RangeStart, RangeEnd: TDateTime): Boolean; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified <see cref="TDateTime"/> value is within the
    /// current era.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is within
    /// the current era, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsThisEra(const DateTime: TDateTime): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified era is the current era.
    /// </summary>
    /// <param name="Era">
    /// The era to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the era is the current era, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsThisEra(Era: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified <see cref="TDateTime"/> value is within the
    /// current year.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is within
    /// the current year, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsThisYear(const DateTime: TDateTime): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified year in a specified era represents the current year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the era and year pair represents the
    /// current year, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsThisYear(Era, Year: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified year is the current year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the year is the current year, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function IsThisYear(Year: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified <see cref="TDateTime"/> value is within the
    /// current month.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is within
    /// the current month, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsThisMonth(const DateTime: TDateTime): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified month in a specified year represents the
    /// current month.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the era, year, and month triple represents
    /// the current month, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsThisMonth(Era, Year, Month: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified month in a specified year represents the
    /// current month.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the year, and month pair represents the
    /// current month, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function IsThisMonth(Year, Month: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified month is the current month.
    /// </summary>
    /// <param name="Month">
    /// The month to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the month is the current month, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsThisMonth(Month: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified <see cref="TDateTime"/> value is within the
    /// current week.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is within
    ///  the current week, otherwise returns <see langword="false"/>.
    ///  </returns>
    {$endregion}
    function IsThisWeek(const DateTime: TDateTime): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified <see cref="TDateTime"/> value represents the
    /// current date.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value represents
    /// the current date, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    class function IsToday(const DateTime: TDateTime): Boolean; overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether a specified day of week is the today's day of week.
    /// </summary>
    /// <param name="DoW">
    /// The day of week to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the day of week is the today's day of week,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    class function IsToday(DoW: TDayOfWeek): Boolean; overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the time portion of a specified <see cref="TDateTime"/>
    /// value occurs after noon.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the time is  after noon, and returns
    /// <see langword="false"/> if the time is before noon.
    /// </returns>
    {$endregion}
    class function IsPM(const DateTime: TDateTime): Boolean; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// CHeckes whether two specified <see cref="TDateTime"/> values are equal.
    /// </summary>
    /// <param name="DateTime1">
    /// The first <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DateTime2">
    /// The second <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the two <see cref="TDateTime"/> values are
    /// equal, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    class function IsSame(const DateTime1, DateTime2: TDateTime): Boolean; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether two specified <see cref="TDateTime"/> values are within the
    /// same era.
    /// </summary>
    /// <param name="DateTime1">
    /// The first <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DateTime2">
    /// The second <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the two <see cref="TDateTime"/> values are
    /// within the same era, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsSameEra(const DateTime1, DateTime2: TDateTime): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether two specified <see cref="TDateTime"/> values are within the
    /// same year.
    /// </summary>
    /// <param name="DateTime1">
    /// The first <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DateTime2">
    /// The second <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the two <see cref="TDateTime"/> values are
    /// within the same year, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsSameYear(const DateTime1, DateTime2: TDateTime): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether two specified <see cref="TDateTime"/> values are within the
    /// same month.
    /// </summary>
    /// <param name="DateTime1">
    /// The first <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DateTime2">
    /// The second <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the two <see cref="TDateTime"/> values are
    /// within the same month, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsSameMonth(const DateTime1, DateTime2: TDateTime): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether two specified <see cref="TDateTime"/> values are within the
    /// same week.
    /// </summary>
    /// <param name="DateTime1">
    /// The first <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DateTime2">
    /// The second <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the two <see cref="TDateTime"/> values are
    /// within the same week, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsSameWeek(const DateTime1, DateTime2: TDateTime): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes whether two specified <see cref="TDateTime"/> values reperesent
    /// the same day.
    /// </summary>
    /// <param name="DateTime1">
    /// The first <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DateTime2">
    /// The second <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the two <see cref="TDateTime"/> values
    /// reperesent the same day, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    class function IsSameDay(const DateTime1, DateTime2: TDateTime): Boolean; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Compares two specified <see cref="TDateTime"/> values.
    /// </summary>
    /// <param name="DateTime1">
    /// The first <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DateTime2">
    /// The second <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns -1 if <paramref name="DateTime1"/> is less than <paramref name="DateTime2"/>,
    /// returns zero if <paramref name="DateTime1"/> is equal to <paramref name="DateTime2"/>,
    /// and returns 1 if <paramref name="DateTime1"/> is greater than <paramref name="DateTime2"/>.</returns>
    {$endregion}
    class function Compare(const DateTime1, DateTime2: TDateTime): Integer; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Compares the date portion of two specified <see cref="TDateTime"/> values.
    /// </summary>
    /// <param name="DateTime1">
    /// The first <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DateTime2">
    /// The second <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns -1 if date of <paramref name="DateTime1"/> is less than date of
    /// <paramref name="DateTime2"/>, returns zero if date of <paramref name="DateTime1"/>
    /// is equal to date of <paramref name="DateTime2"/>, and returns 1 if date of
    /// <paramref name="DateTime1"/> is greater than date of <paramref name="DateTime2"/>.</returns>
    {$endregion}
    class function CompareDate(const DateTime1, DateTime2: TDateTime): Integer; overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Compares the time portion of two specified <see cref="TDateTime"/> values.
    /// </summary>
    /// <param name="DateTime1">
    /// The first <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DateTime2">
    /// The second <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns -1 if time of <paramref name="DateTime1"/> is less than time of
    /// <paramref name="DateTime2"/>, returns zero if time of <paramref name="DateTime1"/>
    /// is equal to time of <paramref name="DateTime2"/>, and returns 1 if time of
    /// <paramref name="DateTime1"/> is greater than time of <paramref name="DateTime2"/>.</returns>
    {$endregion}
    class function CompareTime(const DateTime1, DateTime2: TDateTime): Integer; overload; static; inline;
  { Encode/Decode Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates a <see cref="TDateTime"/> value that represents a specified day
    /// of a specified week in a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Week">
    /// The week of the year.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <param name="DateTime">
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="EncodeDateWeek"/>
    {$endregion}
    function TryEncodeDateWeek(Era, Year, Week: Integer; DoW: TDayOfWeek; out DateTime: TDateTime): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates a <see cref="TDateTime"/> value that represents a specified day
    /// of a specified week in a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Week">
    /// The week of the year.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <param name="DateTime">
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="EncodeDateWeek"/>
    {$endregion}
    function TryEncodeDateWeek(Year, Week: Integer; DoW: TDayOfWeek; out DateTime: TDateTime): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TDateTime"/> value that represents a specified day
    /// of a specified week in a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Week">
    /// The week of the year.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.
    /// </returns>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="TryEncodeDateWeek"/>
    {$endregion}
    function EncodeDateWeek(Era, Year, Week: Integer; DoW: TDayOfWeek): TDateTime; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TDateTime"/> value that represents a specified day
    /// of a specified week in a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Week">
    /// The week of the year.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.
    /// </returns>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="TryEncodeDateWeek"/>
    {$endregion}
    function EncodeDateWeek(Year, Week: Integer; DoW: TDayOfWeek): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the era, year of the era, week of the year, and day of the week
    /// for a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Week">
    /// The week of the year.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DecodeDateWeek"/>
    {$endregion}
    function TryDecodeDateWeek(const DateTime: TDateTime; out Era, Year, Week: Integer; out DoW: TDayOfWeek): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the year, week of the year, and day of the week for a specified
    /// <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Week">
    /// The week of the year.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="DecodeDateWeek"/>
    {$endregion}
    function TryDecodeDateWeek(const DateTime: TDateTime; out Year, Week: Integer; out DoW: TDayOfWeek): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the era, year of the era, week of the year, and day of the week
    /// for a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Week">
    /// The week of the year.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="TryDecodeDateWeek"/>
    {$endregion}
    procedure DecodeDateWeek(const DateTime: TDateTime; out Era, Year, Week: Integer; out DoW: TDayOfWeek); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the year, week of the year, and day of the week for a specified
    /// <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Week">
    /// The week of the year.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="TryDecodeDateWeek"/>
    {$endregion}
    procedure DecodeDateWeek(const DateTime: TDateTime; out Year, Week: Integer; out DoW: TDayOfWeek); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the <see cref="TDateTime"/> value that represents a specified
    /// day of the year for a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <param name="DateTime">
    /// The TDateTimeValue.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="EncodeDateDay"/>
    {$endregion}
    function TryEncodeDateDay(Era, Year, DayOfYear: Integer; out DateTime: TDateTime): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the <see cref="TDateTime"/> value that represents a specified day
    /// of the year for a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <param name="DateTime">
    /// The TDateTimeValue.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="EncodeDateDay"/>
    {$endregion}
    function TryEncodeDateDay(Year, DayOfYear: Integer; out DateTime: TDateTime): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TDateTime"/> value that represents a specified day
    /// of the year for a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.
    /// </returns>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="TryEncodeDateDay"/>
    {$endregion}
    function EncodeDateDay(Era, Year, DayOfYear: Integer): TDateTime; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TDateTime"/> value that represents a specified day
    /// of the year for a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.
    /// </returns>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="TryEncodeDateDay"/>
    {$endregion}
    function EncodeDateDay(Year, DayOfYear: Integer): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the year and day of the year for a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DecodeDateDay"/>
    {$endregion}
    function TryDecodeDateDay(const DateTime: TDateTime; out Era, Year, DayOfYear: Integer): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the year and day of the year for a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="DecodeDateDay"/>
    {$endregion}
    function TryDecodeDateDay(const DateTime: TDateTime; out Year, DayOfYear: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the year and day of the year for a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="TryDecodeDateDay"/>
    {$endregion}
    procedure DecodeDateDay(const DateTime: TDateTime; out Era, Year, DayOfYear: Integer); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the year and day of the year for a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="DayOfYear">
    /// The day of the year.
    /// </param>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="TryDecodeDateDay"/>
    {$endregion}
    procedure DecodeDateDay(const DateTime: TDateTime; out Year, DayOfYear: Integer); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the <see cref="TDateTime"/> value that represents a specified
    /// era, year, month, and day.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="EncodeDate"/>
    {$endregion}
    function TryEncodeDate(Era, Year, Month, Day: Integer; out DateTime: TDateTime): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the <see cref="TDateTime"/> value that represents a specified
    /// year, month, and day.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="EncodeDate"/>
    {$endregion}
    function TryEncodeDate(Year, Month, Day: Integer; out DateTime: TDateTime): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TDateTime"/> value that represents a specified era,
    /// year, month, and day.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.
    /// </returns>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="TryEncodeDate"/>
    {$endregion}
    function EncodeDate(Era, Year, Month, Day: Integer): TDateTime; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TDateTime"/> value that represents a specified year,
    /// month, and day.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.
    /// </returns>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="TryEncodeDate"/>
    {$endregion}
    function EncodeDate(Year, Month, Day: Integer): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the era, year, month, and day values for a specified <see cref="TDateTime"/>
    /// value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DecodeDate"/>
    {$endregion}
    function TryDecodeDate(const DateTime: TDateTime; out Era, Year, Month, Day: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the year, month, and day values for a specified <see cref="TDateTime"/>
    /// value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="DecodeDate"/>
    {$endregion}
    function TryDecodeDate(const DateTime: TDateTime; out Year, Month, Day: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the era, year, month, and day values for a specified <see cref="TDateTime"/>
    /// value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="TryDecodeDate"/>
    {$endregion}
    procedure DecodeDate(const DateTime: TDateTime; out Era, Year, Month, Day: Integer); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the year, month, and day values for a specified <see cref="TDateTime"/>
    /// value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="DefaultEra"/>
    /// <seealso cref="TryDecodeDate"/>
    {$endregion}
    procedure DecodeDate(const DateTime: TDateTime; out Year, Month, Day: Integer); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the Julian day for a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The Julian day.
    /// </returns>
    /// <seealso cref="JulianDayToDateTime"/>
    {$endregion}
    class function DateTimeToJulianDay(const DateTime: TDateTime): Extended; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TDateTime"/> value for a specified Julian day.
    /// </summary>
    /// <param name="JulianDate">
    /// The Julian day.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="DateTimeToJulianDay"/>
    {$endregion}
    class function JulianDayToDateTime(const JulianDate: Extended): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value, which is the result of combining
    /// a specified date with a specified time.
    /// </summary>
    /// <param name="Date">
    /// The <see cref="TDate"/> value.
    /// </param>
    /// <param name="Time">
    /// The <see cref="TTime"/> value.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    class function CombineDateTime(const Date: TDate; const Time: TTime): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the universal time coosrdinate of a specified local time.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value in local time coordinate.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value in universal time coordinate.
    /// </returns>
    /// <seealso cref="UniversalToLocalDateTime"/>
    {$endregion}
    class function LocalToUniversalDateTime(const DateTime: TDateTime): TDateTime; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the local time of a specified universal time coosrdinate.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value in universal time coordinate.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value in local time coordinate.
    /// </returns>
    /// <seealso cref="LocalToUniversalDateTime"/>
    {$endregion}
    class function UniversalToLocalDateTime(const DateTime: TDateTime): TDateTime; static;
  { Pick-a-Field Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Strips the time portion from a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The date portion of the <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="TimeOf"/>
    {$endregion}
    class function DateOf(const DateTime: TDateTime): TDate; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the era represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The era represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    function EraOf(const DateTime: TDateTime): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the year represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The year represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    function YearOf(const DateTime: TDateTime): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the month of the year represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The month of the year represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    function MonthOfYear(const DateTime: TDateTime): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the week of the year represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The week of the year represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    function WeekOfYear(const DateTime: TDateTime): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the month of the month represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The week of the month represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    function WeekOfMonth(const DateTime: TDateTime): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the day of the year represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The day of the year represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    function DayOfYear(const DateTime: TDateTime): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the day of the month represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The day of the month represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    function DayOfMonth(const DateTime: TDateTime): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the day of the week represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The day of the week represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    class function DayOfWeek(const DateTime: TDateTime): TDayOfWeek; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Strips the date portion from a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The time portion of the <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="DateOf"/>
    {$endregion}
    class function TimeOf(const DateTime: TDateTime): TTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the hour of the day represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The hour of the day represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    class function HourOfDay(const DateTime: TDateTime): Integer; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of minutes between a specified <see cref="TDateTime"/>
    /// value and midnight of the same day.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The minute of the day represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    class function MinuteOfDay(const DateTime: TDateTime): Integer; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the minute of the hour represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The minute of the hour represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    class function MinuteOfHour(const DateTime: TDateTime): Integer; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of seconds between a specified <see cref="TDateTime"/>
    /// value and midnight of the same day.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The second of the day represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    class function SecondOfDay(const DateTime: TDateTime): Integer; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the second of the minute represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The second of the minute represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    class function SecondOfMinute(const DateTime: TDateTime): Integer; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of milliseconds between a specified <see cref="TDateTime"/>
    /// value and midnight of the same day.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The millisecond of the day represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    class function MillisecondOfDay(const DateTime: TDateTime): Integer; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the millisecond of the second represented by a <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The millisecond of the second represented by the <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    class function MillisecondOfSecond(const DateTime: TDateTime): Integer; static; inline;
  { Start of a Field Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of a specified era.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfEra"/>
    {$endregion}
    function StartOfEra(Era: Integer): TDateTime; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfYear"/>
    {$endregion}
    function StartOfYear(Era, Year: Integer): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfYear"/>
    {$endregion}
    function StartOfYear(Year: Integer): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of the year identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfYear"/>
    {$endregion}
    function StartOfYear(const DateTime: TDateTime): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of a specified month in a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfMonth"/>
    {$endregion}
    function StartOfMonth(Era, Year, Month: Integer): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of a specified month in a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfMonth"/>
    {$endregion}
    function StartOfMonth(Year, Month: Integer): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of the month identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfMonth"/>
    {$endregion}
    function StartOfMonth(const DateTime: TDateTime): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of the week identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfWeek"/>
    {$endregion}
    function StartOfWeek(const DateTime: TDateTime): TDateTime; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of the day identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfDay"/>
    {$endregion}
    class function StartOfDay(const DateTime: TDateTime): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of the hour identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfHour"/>
    {$endregion}
    class function StartOfHour(const DateTime: TDateTime): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of the minute identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfMinute"/>
    {$endregion}
    class function StartOfMinute(const DateTime: TDateTime): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of the second identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.
    /// </returns>
    /// <seealso cref="EndOfSecond"/>
    {$endregion}
    class function StartOfSecond(const DateTime: TDateTime): TDateTime; static; inline;
  { End of a Field Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of a specified era.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfEra"/>
    {$endregion}
    function EndOfEra(Era: Integer): TDateTime; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfYear"/>
    {$endregion}
    function EndOfYear(Era, Year: Integer): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfYear"/>
    {$endregion}
    function EndOfYear(Year: Integer): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of the year identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfYear"/>
    {$endregion}
    function EndOfYear(const DateTime: TDateTime): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of a specified month in a specified year.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year of the era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfMonth"/>
    {$endregion}
    function EndOfMonth(Era, Year, Month: Integer): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of a specified month in a specified year.
    /// </summary>
    /// <param name="Year">
    /// The year expressed in the calendar's default era.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfMonth"/>
    {$endregion}
    function EndOfMonth(Year, Month: Integer): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of the month identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfMonth"/>
    {$endregion}
    function EndOfMonth(const DateTime: TDateTime): TDateTime; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of the week identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfWeek"/>
    {$endregion}
    function EndOfWeek(const DateTime: TDateTime): TDateTime; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of the day identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfDay"/>
    {$endregion}
    class function EndOfDay(const DateTime: TDateTime): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of the hour identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfHour"/>
    {$endregion}
    class function EndOfHour(const DateTime: TDateTime): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of the minute identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfMinute"/>
    {$endregion}
    class function EndOfMinute(const DateTime: TDateTime): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last expressible
    /// moment of the second identified by a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last moment.
    /// </returns>
    /// <seealso cref="StartOfSecond"/>
    {$endregion}
    class function EndOfSecond(const DateTime: TDateTime): TDateTime; static; inline;
  { Previous of a Field Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted back by a specified number of years.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfYears">
    /// The number of years.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="NextYear"/>
    /// <seealso cref="IncYear"/>
    /// <seealso cref="DecYear"/>
    {$endregion}
    function PrevYear(const DateTime: TDateTime; NumberOfYears: Integer = 1): TDateTime; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted back by a specified number of months.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMonths">
    /// The number of months.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="NextMonth"/>
    /// <seealso cref="IncMonth"/>
    /// <seealso cref="DecMonth"/>
    {$endregion}
    function PrevMonth(const DateTime: TDateTime; NumberOfMonths: Integer = 1): TDateTime; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted back by a specified number of weeks.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfWeeks">
    /// The number of weeks.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="NextWeek"/>
    /// <seealso cref="IncWeek"/>
    /// <seealso cref="DecWeek"/>
    {$endregion}
    class function PrevWeek(const DateTime: TDateTime; NumberOfWeeks: Integer = 1): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted back by a specified number of days.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfDays">
    /// The number of days.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="NextDay"/>
    /// <seealso cref="IncDay"/>
    /// <seealso cref="DecDay"/>
    {$endregion}
    class function PrevDay(const DateTime: TDateTime; NumberOfDays: Integer = 1): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the last day of the
    /// week of a specified day in a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the last day of the week.
    /// </returns>
    /// <seealso cref="NextDayOfWeek"/>
    {$endregion}
    function PrevDayOfWeek(const DateTime: TDateTime; DoW: TDayOfWeek): TDateTime;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted back by a specified number of hours.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfHours">
    /// The number of hours.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="NextHour"/>
    /// <seealso cref="IncHour"/>
    /// <seealso cref="DecHour"/>
    {$endregion}
    class function PrevHour(const DateTime: TDateTime; NumberOfHours: Integer = 1): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted back by a specified number of minutes.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMinutes">
    /// The number of minutes.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="NextMinute"/>
    /// <seealso cref="IncMinute"/>
    /// <seealso cref="DecMinute"/>
    {$endregion}
    class function PrevMinute(const DateTime: TDateTime; NumberOfMinutes: Integer = 1): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted back by a specified number of seconds.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfSeconds">
    /// The number of seconds.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="NextSecond"/>
    /// <seealso cref="IncSecond"/>
    /// <seealso cref="DecSecond"/>
    {$endregion}
    class function PrevSecond(const DateTime: TDateTime; NumberOfSeconds: Integer = 1): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted back by a specified number of milliseconds.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMilliseconds">
    /// The number of milliseconds.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="NextMillisecond"/>
    /// <seealso cref="IncMillisecond"/>
    /// <seealso cref="DecMillisecond"/>
    {$endregion}
    class function PrevMillisecond(const DateTime: TDateTime; NumberOfMilliseconds: Integer = 1): TDateTime; static; inline;
  { Next of a Field Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted forward by a specified number of years.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfYears">
    /// The number of years.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="PrevYear"/>
    /// <seealso cref="IncYear"/>
    /// <seealso cref="DecYear"/>
    {$endregion}
    function NextYear(const DateTime: TDateTime; NumberOfYears: Integer = 1): TDateTime;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted forward by a specified number of months.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMonths">
    /// The number of months.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="PrevMonth"/>
    /// <seealso cref="IncMonth"/>
    /// <seealso cref="DecMonth"/>
    {$endregion}
    function NextMonth(const DateTime: TDateTime; NumberOfMonths: Integer = 1): TDateTime;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted forward by a specified number of weeks.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfWeeks">
    /// The number of weeks.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="PrevWeek"/>
    /// <seealso cref="IncWeek"/>
    /// <seealso cref="DecWeek"/>
    {$endregion}
    class function NextWeek(const DateTime: TDateTime; NumberOfWeeks: Integer = 1): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted forward by a specified number of days.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfDays">
    /// The number of days.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="PrevDay"/>
    /// <seealso cref="IncDay"/>
    /// <seealso cref="DecDay"/>
    {$endregion}
    class function NextDay(const DateTime: TDateTime; NumberOfDays: Integer = 1): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the next day of the
    /// week of a specified day in a specified <see cref="TDateTime"/> value.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="DoW">
    /// The day of the week.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the next day of the week.
    /// </returns>
    /// <seealso cref="PrevDayOfWeek"/>
    {$endregion}
    function NextDayOfWeek(const DateTime: TDateTime; DoW: TDayOfWeek): TDateTime;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted forward by a specified number of hours.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfHours">
    /// The number of hours.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="PrevHour"/>
    /// <seealso cref="IncHour"/>
    /// <seealso cref="DecHour"/>
    {$endregion}
    class function NextHour(const DateTime: TDateTime; NumberOfHours: Integer = 1): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted forward by a specified number of minutes.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMinutes">
    /// The number of minutes.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="PrevMinute"/>
    /// <seealso cref="IncMinute"/>
    /// <seealso cref="DecMinute"/>
    {$endregion}
    class function NextMinute(const DateTime: TDateTime; NumberOfMinutes: Integer = 1): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted forward by a specified number of seconds.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfSeconds">
    /// The number of seconds.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="PrevSecond"/>
    /// <seealso cref="IncSecond"/>
    /// <seealso cref="DecSecond"/>
    {$endregion}
    class function NextSecond(const DateTime: TDateTime; NumberOfSeconds: Integer = 1): TDateTime; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value shifted forward by a specified number of milliseconds.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMilliseconds">
    /// The number of milliseconds.
    /// </param>
    /// <returns>
    /// The shifted <see cref="TDateTime"/> value.
    /// </returns>
    /// <seealso cref="PrevMillisecond"/>
    /// <seealso cref="IncMillisecond"/>
    /// <seealso cref="DecMillisecond"/>
    {$endregion}
    class function NextMillisecond(const DateTime: TDateTime; NumberOfMilliseconds: Integer = 1): TDateTime; static; inline;
  { Increment Field Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDateTime"/> value by a specified number of years.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfYears">
    /// The number of years.
    /// </param>
    /// <seealso cref="DecYear"/>
    /// <seealso cref="NextYear"/>
    /// <seealso cref="PrevYear"/>
    {$endregion}
    procedure IncYear(var DateTime: TDateTime; NumberOfYears: Integer = 1); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDate"/> value by a specified number of years.
    /// </summary>
    /// <param name="Date">
    /// The <see cref="TDate"/> value.
    /// </param>
    /// <param name="NumberOfYears">
    /// The number of years.
    /// </param>
    /// <seealso cref="DecYear"/>
    /// <seealso cref="NextYear"/>
    /// <seealso cref="PrevYear"/>
    {$endregion}
    procedure IncYear(var Date: TDate; NumberOfYears: Integer = 1); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDateTime"/> value by a specified number of months.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMonths">
    /// The number of months.
    /// </param>
    /// <seealso cref="DecMonth"/>
    /// <seealso cref="NextMonth"/>
    /// <seealso cref="PrevMonth"/>
    {$endregion}
    procedure IncMonth(var DateTime: TDateTime; NumberOfMonths: Integer = 1); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDate"/> value by a specified number of months.
    /// </summary>
    /// <param name="Date">
    /// The <see cref="TDate"/> value.
    /// </param>
    /// <param name="NumberOfMonths">
    /// The number of months.
    /// </param>
    /// <seealso cref="DecMonth"/>
    /// <seealso cref="NextMonth"/>
    /// <seealso cref="PrevMonth"/>
    {$endregion}
    procedure IncMonth(var Date: TDate; NumberOfMonths: Integer = 1); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDateTime"/> value by a specified number of weeks.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfWeeks">
    /// The number of weeks.
    /// </param>
    /// <seealso cref="DecWeek"/>
    /// <seealso cref="NextWeek"/>
    /// <seealso cref="PrevWeek"/>
    {$endregion}
    class procedure IncWeek(var DateTime: TDateTime; NumberOfWeeks: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDate"/> value by a specified number of weeks.
    /// </summary>
    /// <param name="Date">
    /// The <see cref="TDate"/> value.
    /// </param>
    /// <param name="NumberOfWeeks">
    /// The number of weeks.
    /// </param>
    /// <seealso cref="DecWeek"/>
    /// <seealso cref="NextWeek"/>
    /// <seealso cref="PrevWeek"/>
    {$endregion}
    class procedure IncWeek(var Date: TDate; NumberOfWeeks: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDateTime"/> value by a specified number of days.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfDays">
    /// The number of days.
    /// </param>
    /// <seealso cref="DecDay"/>
    /// <seealso cref="NextDay"/>
    /// <seealso cref="PrevDay"/>
    {$endregion}
    class procedure IncDay(var DateTime: TDateTime; NumberOfDays: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDate"/> value by a specified number of days.
    /// </summary>
    /// <param name="Date">
    /// The <see cref="TDate"/> value.
    /// </param>
    /// <param name="NumberOfDays">
    /// The number of days.
    /// </param>
    /// <seealso cref="DecDay"/>
    /// <seealso cref="NextDay"/>
    /// <seealso cref="PrevDay"/>
    {$endregion}
    class procedure IncDay(var Date: TDate; NumberOfDays: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDateTime"/> value by a specified number of hours.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfHours">
    /// The number of hours.
    /// </param>
    /// <seealso cref="DecHour"/>
    /// <seealso cref="NextHour"/>
    /// <seealso cref="PrevHour"/>
    {$endregion}
    class procedure IncHour(var DateTime: TDateTime; NumberOfHours: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TTime"/> value by a specified number of hours.
    /// </summary>
    /// <param name="Time">
    /// The <see cref="TTime"/> value.
    /// </param>
    /// <param name="NumberOfHours">
    /// The number of hours.
    /// </param>
    /// <seealso cref="DecHour"/>
    /// <seealso cref="NextHour"/>
    /// <seealso cref="PrevHour"/>
    {$endregion}
    class procedure IncHour(var Time: TTime; NumberOfHours: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDateTime"/> value by a specified number of minutes.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMinutes">
    /// The number of minutes.
    /// </param>
    /// <seealso cref="DecMinute"/>
    /// <seealso cref="NextMinute"/>
    /// <seealso cref="PrevMinute"/>
    {$endregion}
    class procedure IncMinute(var DateTime: TDateTime; NumberOfMinutes: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TTime"/> value by a specified number of minutes.
    /// </summary>
    /// <param name="Time">
    /// The <see cref="TTime"/> value.
    /// </param>
    /// <param name="NumberOfMinutes">
    /// The number of minutes.
    /// </param>
    /// <seealso cref="DecMinute"/>
    /// <seealso cref="NextMinute"/>
    /// <seealso cref="PrevMinute"/>
    {$endregion}
    class procedure IncMinute(var Time: TTime; NumberOfMinutes: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDateTime"/> value by a specified number of seconds.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfSeconds">
    /// The number of seconds.
    /// </param>
    /// <seealso cref="DecSecond"/>
    /// <seealso cref="NextSecond"/>
    /// <seealso cref="PrevSecond"/>
    {$endregion}
    class procedure IncSecond(var DateTime: TDateTime; NumberOfSeconds: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TTime"/> value by a specified number of seconds.
    /// </summary>
    /// <param name="Time">
    /// The <see cref="TTime"/> value.
    /// </param>
    /// <param name="NumberOfSeconds">
    /// The number of seconds.
    /// </param>
    /// <seealso cref="DecSecond"/>
    /// <seealso cref="NextSecond"/>
    /// <seealso cref="PrevSecond"/>
    {$endregion}
    class procedure IncSecond(var Time: TTime; NumberOfSeconds: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDateTime"/> value by a specified number of milliseconds.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMilliseconds">
    /// The number of milliseconds.
    /// </param>
    /// <seealso cref="DecMillisecond"/>
    /// <seealso cref="NextMillisecond"/>
    /// <seealso cref="PrevMillisecond"/>
    {$endregion}
    class procedure IncMillisecond(var DateTime: TDateTime; NumberOfMilliseconds: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TTime"/> value by a specified number of milliseconds.
    /// </summary>
    /// <param name="Time">
    /// The <see cref="TTime"/> value.
    /// </param>
    /// <param name="NumberOfMilliseconds">
    /// The number of milliseconds.
    /// </param>
    /// <seealso cref="DecMillisecond"/>
    /// <seealso cref="NextMillisecond"/>
    /// <seealso cref="PrevMillisecond"/>
    {$endregion}
    class procedure IncMillisecond(var Time: TTime; NumberOfMilliseconds: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Increments a <see cref="TDateTime"/> value by a specified number of days
    /// including fraction of a day.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Delta">
    /// The number of days including fraction of a day.
    /// </param>
    /// <seealso cref="DecDateTime"/>
    {$endregion}
    class procedure IncDateTime(var DateTime: TDateTime; const Delta: Double); static; inline;
  { Decrement Field procedures }
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDateTime"/> value by a specified number of years.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfYears">
    /// The number of years.
    /// </param>
    /// <seealso cref="IncYear"/>
    /// <seealso cref="NextYear"/>
    /// <seealso cref="PrevYear"/>
    {$endregion}
    procedure DecYear(var DateTime: TDateTime; NumberOfYears: Integer = 1); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDate"/> value by a specified number of years.
    /// </summary>
    /// <param name="Date">
    /// The <see cref="TDate"/> value.
    /// </param>
    /// <param name="NumberOfYears">
    /// The number of years.
    /// </param>
    /// <seealso cref="IncYear"/>
    /// <seealso cref="NextYear"/>
    /// <seealso cref="PrevYear"/>
    {$endregion}
    procedure DecYear(var Date: TDate; NumberOfYears: Integer = 1); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDateTime"/> value by a specified number of months.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMonths">
    /// The number of months.
    /// </param>
    /// <seealso cref="IncMonth"/>
    /// <seealso cref="NextMonth"/>
    /// <seealso cref="PrevMonth"/>
    {$endregion}
    procedure DecMonth(var DateTime: TDateTime; NumberOfMonths: Integer = 1); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDate"/> value by a specified number of months.
    /// </summary>
    /// <param name="Date">
    /// The <see cref="TDate"/> value.
    /// </param>
    /// <param name="NumberOfMonths">
    /// The number of months.
    /// </param>
    /// <seealso cref="IncMonth"/>
    /// <seealso cref="NextMonth"/>
    /// <seealso cref="PrevMonth"/>
    {$endregion}
    procedure DecMonth(var Date: TDate; NumberOfMonths: Integer = 1); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDateTime"/> value by a specified number of weeks.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfWeeks">
    /// The number of weeks.
    /// </param>
    /// <seealso cref="IncWeek"/>
    /// <seealso cref="NextWeek"/>
    /// <seealso cref="PrevWeek"/>
    {$endregion}
    class procedure DecWeek(var DateTime: TDateTime; NumberOfWeeks: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDate"/> value by a specified number of weeks.
    /// </summary>
    /// <param name="Date">
    /// The <see cref="TDate"/> value.
    /// </param>
    /// <param name="NumberOfWeeks">
    /// The number of weeks.
    /// </param>
    /// <seealso cref="IncWeek"/>
    /// <seealso cref="NextWeek"/>
    /// <seealso cref="PrevWeek"/>
    {$endregion}
    class procedure DecWeek(var Date: TDate; NumberOfWeeks: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDateTime"/> value by a specified number of days.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfDays">
    /// The number of days.
    /// </param>
    /// <seealso cref="IncDay"/>
    /// <seealso cref="NextDay"/>
    /// <seealso cref="PrevDay"/>
    {$endregion}
    class procedure DecDay(var DateTime: TDateTime; NumberOfDays: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDate"/> value by a specified number of days.
    /// </summary>
    /// <param name="Date">
    /// The <see cref="TDate"/> value.
    /// </param>
    /// <param name="NumberOfDays">
    /// The number of days.
    /// </param>
    /// <seealso cref="IncDay"/>
    /// <seealso cref="NextDay"/>
    /// <seealso cref="PrevDay"/>
    {$endregion}
    class procedure DecDay(var Date: TDate; NumberOfDays: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDateTime"/> value by a specified number of hours.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfHours">
    /// The number of hours.
    /// </param>
    /// <seealso cref="IncHour"/>
    /// <seealso cref="NextHour"/>
    /// <seealso cref="PrevHour"/>
    {$endregion}
    class procedure DecHour(var DateTime: TDateTime; NumberOfHours: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TTime"/> value by a specified number of hours.
    /// </summary>
    /// <param name="Time">
    /// The <see cref="TTime"/> value.
    /// </param>
    /// <param name="NumberOfHours">
    /// The number of hours.
    /// </param>
    /// <seealso cref="IncHour"/>
    /// <seealso cref="NextHour"/>
    /// <seealso cref="PrevHour"/>
    {$endregion}
    class procedure DecHour(var Time: TTime; NumberOfHours: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDateTime"/> value by a specified number of minutes.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMinutes">
    /// The number of minutes.
    /// </param>
    /// <seealso cref="IncMinute"/>
    /// <seealso cref="NextMinute"/>
    /// <seealso cref="PrevMinute"/>
    {$endregion}
    class procedure DecMinute(var DateTime: TDateTime; NumberOfMinutes: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TTime"/> value by a specified number of minutes.
    /// </summary>
    /// <param name="Time">
    /// The <see cref="TTime"/> value.
    /// </param>
    /// <param name="NumberOfMinutes">
    /// The number of minutes.
    /// </param>
    /// <seealso cref="IncMinute"/>
    /// <seealso cref="NextMinute"/>
    /// <seealso cref="PrevMinute"/>
    {$endregion}
    class procedure DecMinute(var Time: TTime; NumberOfMinutes: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDateTime"/> value by a specified number of seconds.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfSeconds">
    /// The number of seconds.
    /// </param>
    /// <seealso cref="IncSecond"/>
    /// <seealso cref="NextSecond"/>
    /// <seealso cref="PrevSecond"/>
    {$endregion}
    class procedure DecSecond(var DateTime: TDateTime; NumberOfSeconds: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TTime"/> value by a specified number of seconds.
    /// </summary>
    /// <param name="Time">
    /// The <see cref="TTime"/> value.
    /// </param>
    /// <param name="NumberOfSeconds">
    /// The number of seconds.
    /// </param>
    /// <seealso cref="IncSecond"/>
    /// <seealso cref="NextSecond"/>
    /// <seealso cref="PrevSecond"/>
    {$endregion}
    class procedure DecSecond(var Time: TTime; NumberOfSeconds: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDateTime"/> value by a specified number of milliseconds.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="NumberOfMilliseconds">
    /// The number of milliseconds.
    /// </param>
    /// <seealso cref="IncMillisecond"/>
    /// <seealso cref="NextMillisecond"/>
    /// <seealso cref="PrevMillisecond"/>
    {$endregion}
    class procedure DecMillisecond(var DateTime: TDateTime; NumberOfMilliseconds: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TTime"/> value by a specified number of milliseconds.
    /// </summary>
    /// <param name="Time">
    /// The <see cref="TTime"/> value.
    /// </param>
    /// <param name="NumberOfMilliseconds">
    /// The number of milliseconds.
    /// </param>
    /// <seealso cref="IncMillisecond"/>
    /// <seealso cref="NextMillisecond"/>
    /// <seealso cref="PrevMillisecond"/>
    {$endregion}
    class procedure DecMillisecond(var Time: TTime; NumberOfMilliseconds: Integer = 1); overload; static; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Decrements a <see cref="TDateTime"/> value by a specified number of days
    /// including fraction of a day.
    /// </summary>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <param name="Delta">
    /// The number of days including fraction of a day.
    /// </param>
    /// <seealso cref="IncDateTime"/>
    {$endregion}
    class procedure DecDateTime(var DateTime: TDateTime; const Delta: Double); static; inline;
  { String Conversion Functions }
    {$region 'xmldoc'}
    /// <summary>
    /// Formats a <see cref="TDateTime"/> value.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The Format method formats the <see cref="TDateTime"/> value given by
    /// <paramref name="DateTime"/> using the format string given by <paramref name="FmtStr"/>.
    /// </para>
    /// <para>
    /// The format string controls the formatting of date and time, and is composed from
    /// specifiers that represent values to insert into the formatted string. Some
    /// specifiers (such as "d"), simply format numbers or strings. Other specifiers
    /// (such as "/") refer to the locale-specific strings from <see cref="Settings"/>
    /// property.
    /// </para>
    /// <para>
    /// In the following table, specifiers are given in lower case. Case is ignored in
    /// formats, except for the "am/pm" and "a/p" specifiers.
    /// </para>
    /// <list type="table">
    ///   <listheader>
    ///     <term>Specifier</term>
    ///     <description>Meaning</description>
    ///   </listheader>
    ///   <item>
    ///     <term>c</term>
    ///     <description>
    ///     The date using the format given by the <see cref="Settings.ShortDateFormat"/>
    ///     property, followed by the time using the format given by the
    ///     <see cref="Settings.LongTimeFormat"/> property. The time is not displayed
    ///     if the date-time value indicates midnight precisely.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>q</term>
    ///     <description>
    ///     The date and time using '<c>yyyy-mm-dd hh:nn:ss</c>' format.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>u</term>
    ///     <description>
    ///     The date and time using '<c>yyyy-mm-dd hh:nn:ss</c>' format in universal
    ///     time coordinate.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>g</term>
    ///     <description>
    ///     The period/era as an abbreviation.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>gg</term>
    ///     <description>
    ///     The period/era as a full name.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>yy</term>
    ///     <description>
    ///     The year as a two-digit number.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>yyyy</term>
    ///     <description>
    ///     The year as a four-digit number.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>m</term>
    ///     <description>
    ///     The month as a number without a leading zero. If the <c>m</c> specifier
    ///     immediately follows an <c>h</c> or <c>hh</c> specifier, the minute rather
    ///     than the month is displayed.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>mm</term>
    ///     <description>
    ///     The month as a tw-digit number with a leading zero. If the <c>mm</c>
    ///     specifier immediately follows an <c>h</c> or <c>hh</c>  specifier, the
    ///     minute rather than the month is displayed.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>mmm</term>
    ///     <description>
    ///     The month as an abbreviation using the strings given by the
    ///     <see cref="Settings.ShortMonthNames"/> property.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>mmmm</term>
    ///     <description>
    ///     The month as a full name using the strings given by either the
    ///     <see cref="Settings.MonthNames"/> or <see cref="Settings.GenitiveMonthNames"/>
    ///     properties depends on the context.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>d</term>
    ///     <description>
    ///     The day as a number without a leading zero.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>dd</term>
    ///     <description>
    ///     The day as a two-digit number with a leading zero.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>ddd</term>
    ///     <description>
    ///     The day of the week as an abbreviation using the strings given by
    ///     the <see cref="Settings.ShortDayNames"/> property.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>dddd</term>
    ///     <description>
    ///     The day of the week as a full name using the strings given by the
    ///     <see cref="Settings.DayNames"/> property.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>ddddd</term>
    ///     <description>
    ///     The date using the format given by the <see cref="Settings.ShortDateFormat"/>
    ///     property.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>dddddd</term>
    ///     <description>
    ///     The date using the format given by the <see cref="Settings.LongDateFormat"/>
    ///     property.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>i</term>
    ///     <description>
    ///     The day of the year without a leading zero.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>iii</term>
    ///     <description>
    ///     The day of the year with a leading zero.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>rr</term>
    ///     <description>
    ///     The week number year as a two-digit number. This has the same value
    ///     as <c>yy</c> specifier, except that if the week number belongs to the
    ///     previous or next year, that year is used instead.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>rrrr</term>
    ///     <description>
    ///     The week number year as a four-digit number. This has the same
    ///     value as <c>yyyy</c> specifier, except that if the week number belongs
    ///     to the previous or next year, that year is used instead.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>w</term>
    ///     <description>
    ///     The week of the year without a leading zero.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>ww</term>
    ///     <description>
    ///     The week of the year with a leading zero.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>o</term>
    ///     <description>
    ///     The day of the week as a number, where 1 represents the first day
    ///     of the week and 7 represents the last day of the week.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>h</term>
    ///     <description>
    ///     The hour without a leading zero (0-23).
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>hh</term>
    ///     <description>
    ///     The hour with a leading zero (00-23).
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>n</term>
    ///     <description>
    ///     The minute without a leading zero (0-59).
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>nn</term>
    ///     <description>
    ///     The minute with a leading zero (00-59).
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>s</term>
    ///     <description>
    ///     The second without a leading zero (0-59).
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>ss</term>
    ///     <description>
    ///     The second with a leading zero (00-59).
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>z</term>
    ///     <description>
    ///     The millisecond without a leading zero (0-999).
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>zzz</term>
    ///     <description>
    ///     The millisecond with a leading zero (000-999).
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>t</term>
    ///     <description>
    ///     The time using the format given by the <see cref="Settings.ShortTimeFormat"/>
    ///     property.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>tt</term>
    ///     <description>
    ///     The time using the format given by the <see cref="Settings.LongTimeFormat"/>
    ///     property.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>am/pm</term>
    ///     <description>
    ///     Uses the 12-hour clock for the preceding <c>h</c> or <c>hh</c> specifier,
    ///     and displays 'am' for any hour before noon, and 'pm' for any hour after
    ///     noon. The am/pm specifier can use lower, upper, or mixed case, and the
    ///     result is displayed accordingly.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>a/p</term>
    ///     <description>
    ///     Uses the 12-hour clock for the preceding <c>h</c> or <c>hh</c> specifier,
    ///     and displays 'a' for any hour before noon, and 'p' for any hour after
    ///     noon. The a/p specifier can use lower, upper, or mixed case, and the
    ///     result is displayed accordingly.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>ampm</term>
    ///     <description>
    ///     Uses the 12-hour clock for the preceding <c>h</c> or <c>hh</c> specifier,
    ///     and displays the contents of the <see cref="Settings.TimeAMString"/>
    ///     property for any hour before noon, and the contents of the
    ///     <see cref="Settings.TimePMString"/> property for any hour after noon.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>/</term>
    ///     <description>
    ///     The date separator given by the <see cref="Settings.DateSeparator"/>
    ///     property.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>:</term>
    ///     <description>
    ///     The time separator given by the <see cref="Settings.TimeSeparator"/>
    ///     property.
    ///     </description>
    ///   </item>
    ///   <item>
    ///     <term>'xx'/"xx"</term>
    ///     <description>
    ///     Characters enclosed in single or double quotes are displayed as-is,
    ///     and do not affect formatting.
    ///     </description>
    ///   </item>
    /// </list>
    /// <para>
    /// If the string specified by the <paramref name="FmtStr"/> parameter is empty,
    /// the <see cref="TDateTime"/> value is formatted as if a 'c' format specifier
    /// had been given.
    /// </para>
    /// </remarks>
    /// <param name="FmtStr">
    /// The format string.
    /// </param>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The formatted <see cref="TDateTime"/> value as a string.
    /// </returns>
    /// <seealso cref="Settings"/>
    /// <seealso cref="Parse"/>
    /// <seealso cref="TryParse"/>
    {$endregion}
    function Format(const FmtStr: String; const DateTime: TDateTime): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string to a <see cref="TDateTime"/> value using a specified
    /// format string.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The Parse method scans the string given by <paramref name="DateTimeStr"/>
    /// using the format string given by <paramref name="FmtStr"/> to extract its
    /// date and time values. See <see cref="Format"/> method for more information
    /// about the format string.
    /// </para>
    /// <para>
    /// If the string specified by the <paramref name="FmtStr"/> parameter is empty,
    /// it is assumed to be a 'c' format specifier had been given.
    /// </para>
    /// </remarks>
    /// <param name="FmtStr">
    /// The format string. See <see cref="Format"/> method for details.
    /// </param>
    /// <param name="DateTimeStr">
    /// The date and time value as a string.
    /// </param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.
    /// </returns>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.
    /// </exception>
    /// <seealso cref="TryParse"/>
    /// <seealso cref="Format"/>
    {$endregion}
    function Parse(const FmtStr: String; const DateTimeStr: String): TDateTime; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string to a <see cref="TDateTime"/> value using a specified
    /// format string.
    /// </summary>
    /// <remarks>
    /// The TryParse method scans the string given by <paramref name="DateTimeStr"/>
    /// using the format string given by <paramref name="FmtStr"/> to extract its
    /// date and time values. See <see cref="Format"/> method for more information
    /// about the format string.
    /// <para>
    /// If the string specified by the <paramref name="FmtStr"/> parameter is empty,
    /// it is assumed that a 'c' format specifier had been given.
    /// </para>
    /// </remarks>
    /// <param name="FmtStr">
    /// The format string. See <see cref="Format"/> method for details.
    /// </param>
    /// <param name="DateTimeStr">
    /// The date and time value as a string.
    /// </param>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion is succeeded, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Parse"/>
    /// <seealso cref="Format"/>
    {$endregion}
    function TryParse(const FmtStr: String; const DateTimeStr: String; var DateTime: TDateTime): Boolean;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCalendarClassList maintains a list of <see cref="TCalendar"/> classes.
  /// </summary>
  /// <remarks>
  /// Use TCalendarClassList to store and maintain a list of <see cref="TCalendar"/>
  /// classes. TCalendarClassList provides properties and methods to add, delete,
  /// rearrange, locate, access, and sort <see cref="TCalendar"/> classes.
  /// </remarks>
  {$endregion}
  TCalendarClassList = class(TList)
  private
    function GetItems(Index: Integer): TCalendarClass; inline;
    procedure SetItems(Index: Integer; Value: TCalendarClass);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the first <see cref="TCalendar"/> class in the <see cref="Items"/>
    /// array.
    /// </summary>
    /// <returns>
    /// The first item in the list.
    /// </returns>
    /// <seealso cref="Last"/>
    {$endregion}
    function First: TCalendarClass; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the last <see cref="TCalendar"/> class in the <see cref="Items"/>
    /// array.
    /// </summary>
    /// <returns>
    /// The last item in the list.
    /// </returns>
    /// <seealso cref="First"/>
    {$endregion}
    function Last: TCalendarClass; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Inserts the <paramref name="Item"/> parameter at the end of the <see cref="Items"/>
    /// array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TCalendar"/> class to add.
    /// </param>
    /// <returns>
    /// The index of the new item.
    /// </returns>
    /// <seealso cref="Insert"/>
    {$endregion}
    function Add(Item: TCalendarClass): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Inserts the <paramref name="Item"/> parameter in the <see cref="Items"/> array,
    /// at the position specified by <paramref name="Index"/>.
    /// </summary>
    /// <param name="Index">
    /// Position of the item in the <see cref="Items"/> array.
    /// </param>
    /// <param name="Item">
    /// The <see cref="TCalendar"/> class to insert.
    /// </param>
    /// <seealso cref="Add"/>
    {$endregion}
    procedure Insert(Index: Integer; Item: TCalendarClass); inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes the first reference to the <paramref name="Item"/> parameter from
    /// the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TCalendar"/> class to remove.
    /// </param>
    /// <returns>
    /// The index of the removed item, or -1 if the item is not found.
    /// </returns>
    /// <seealso cref="RemoveItem"/>
    {$endregion}
    function Remove(Item: TCalendarClass): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes the first or last reference to the <paramref name="Item"/> parameter
    /// from the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TCalendar"/> class to remove.
    /// </param>
    /// <param name="Direction">
    /// The direction of search for locating the item.
    /// </param>
    /// <returns>
    /// The index of the removed item, or -1 if the item is not found.
    /// </returns>
    /// <seealso cref="Remove"/>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    function RemoveItem(Item: TCalendarClass; Direction: TList.TDirection): Integer;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes the first reference to the <paramref name="Item"/> parameter from
    /// the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TCalendar"/> class to extract.
    /// </param>
    /// <returns>
    /// The extracted item, or <see langword="nil"/> if the item is not found.
    /// </returns>
    /// <seealso cref="ExtractItem"/>
    {$endregion}
    function Extract(Item: TCalendarClass): TCalendarClass; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes the first or last reference to the <paramref name="Item"/> parameter
    /// from the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TCalendar"/> class to extract.
    /// </param>
    /// <param name="Direction">
    /// The direction of search for locating the item.
    /// </param>
    /// <returns>
    /// The extracted item, or <see langword="nil"/> if the item is not found.
    /// </returns>
    /// <seealso cref="Extract"/>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    function ExtractItem(Item: TCalendarClass; Direction: TList.TDirection): TCalendarClass; inline;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Returns index of the first reference to the <paramref name="Item"/> parameter
    /// in the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TCalendar"/> class to locate.
    /// </param>
    /// <returns>
    /// The index of item, or -1 if the item is not found.
    /// </returns>
    /// <seealso cref="IndexOfItem"/>
    {$endregion}
    function IndexOf(Item: TCalendarClass): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns index of the first or last reference to the <paramref name="Item"/>
    /// parameter in the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TCalendar"/> class to locate.
    /// </param>
    /// <param name="Direction">
    /// The direction of search for locating the item.
    /// </param>
    /// <returns>
    /// The index of item, or -1 if the item is not found.
    /// </returns>
    /// <seealso cref="IndexOf"/>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    function IndexOfItem(Item: TCalendarClass; Direction: TList.TDirection): Integer; inline;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCalendar"/> classes.
    /// </summary>
    {$endregion}
    property Items[Index: Integer]: TCalendarClass read GetItems write SetItems; default;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// CalendarTypes maintains a list of registered <see cref="TCalendar"/> classes.
  /// </summary>
  /// <remarks>
  /// Use CalendarTypes to register or unregister a <see cref="TCalendar"/> class.
  /// Also, CalendarTypes provides properties and methods to locate. and access the
  /// registered <see cref="TCalendar"/> classes by their identifier, name, or
  /// associated locale names.
  /// </remarks>
  {$endregion}
  CalendarTypes = class
  private
    class var CalendarList: TStringList;
    class procedure Cleanup;
  protected
    type
      {$region 'xmldoc'}
      /// <summary>
      /// TCalendarRegInfo associates a <see cref="TCalendar"/> class with a set of locales.
      /// </summary>
      {$endregion}
      TCalendarRegInfo = class(TObject)
      private
        fCalendarClass: TCalendarClass;
        fLocales: TStringList;
      public
        {$region 'xmldoc'}
        /// <summary>
        /// Creates an instance of the class.
        /// </summary>
        /// <param name="ACalendarClass">
        /// The <see cref="TCalendar"/> class that is referenced by the object.
        /// </param>
        {$endregion}
        constructor Create(ACalendarClass: TCalendarClass);
        {$region 'xmldoc'}
        /// <summary>
        /// Destroys the instance and releases its allocated momory.
        /// </summary>
        {$endregion}
        destructor Destroy; override;
        {$region 'xmldoc'}
        /// <summary>
        /// Links a set of locale names to the <see cref="TCalendar"/> class
        /// referenced by the <see cref="CalendarClass"/> property.
        /// </summary>
        /// <param name="Locales">
        /// The array of locale names.
        /// </param>
        /// <seealso cref="Dissociate"/>
        {$endregion}
        procedure Associate(const Locales: array of String);
        {$region 'xmldoc'}
        /// <summary>
        /// Unlinks a set of locale names from the <see cref="TCalendar"/> class
        /// referenced by the <see cref="CalendarClass"/> property.
        /// </summary>
        /// <param name="Locales">
        /// The array of locale names.
        /// </param>
        /// <seealso cref="Associate"/>
        {$endregion}
        procedure Dissociate(const Locales: array of String);
        {$region 'xmldoc'}
        /// <summary>
        /// Lists the locale names, which are linked to the <see cref="TCalendar"/>
        /// class referenced by the <see cref="CalendarClass"/> property.
        /// </summary>
        {$endregion}
        property AssociatedLocales: TStringList read fLocales;
        {$region 'xmldoc'}
        /// <summary>
        /// Gets the <see cref="TCalendar"/> class referenced by the object.
        /// </summary>
        {$endregion}
        property CalendarClass: TCalendarClass read fCalendarClass;
      end;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Registers a <see cref="TCalendar"/> class for all locales.
    /// </summary>
    /// <param name="CalendarClass">
    /// The <see cref="TCalendar"/> class to register.
    /// </param>
    /// <seealso cref="Unregister"/>
    {$endregion}
    class procedure Register(CalendarClass: TCalendarClass); overload; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Registers a <see cref="TCalendar"/> class for a list of specific locales.
    /// </summary>
    /// <param name="CalendarClass">
    /// The <see cref="TCalendar"/> class to register.
    /// </param>
    /// <param name="Locales">
    /// The array of locale names.
    /// </param>
    /// <seealso cref="Unregister"/>
    {$endregion}
    class procedure Register(CalendarClass: TCalendarClass;
      const Locales: array of String); overload; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Unregisters a previously registered <see cref="TCalendar"/> class for all locales.
    /// </summary>
    /// <param name="CalendarClass">
    /// The <see cref="TCalendar"/> class to unregister.
    /// </param>
    /// <seealso cref="Register"/>
    {$endregion}
    class procedure Unregister(CalendarClass: TCalendarClass); overload; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Unregisters a previously registered <see cref="TCalendar"/> class, only for
    /// a list of specific locales.
    /// </summary>
    /// <param name="CalendarClass">
    /// The <see cref="TCalendar"/> class to unregister.
    /// </param>
    /// <param name="Locales">
    /// The array of locale names.
    /// </param>
    /// <seealso cref="Register"/>
    {$endregion}
    class procedure Unregister(CalendarClass: TCalendarClass;
      const Locales: array of String); overload; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the minimum <see cref="TDateTime"/> value that is supported by all
    /// registered <see cref="TCalendar"/> classes.
    /// </summary>
    /// <returns>
    /// The smallest <see cref="TDateTime"/> value that is supported by all the
    /// registered <see cref="TCalendar"/> classes.
    /// </returns>
    {$endregion}
    class function MinSupportedDateTime: TDateTime; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the maximum <see cref="TDateTime"/> value that is supported by all registered
    /// <see cref="TCalendar"/> classes.
    /// </summary>
    /// <returns>
    /// The largest <see cref="TDateTime"/> value that is supported by all the
    /// registered <see cref="TCalendar"/> classes.
    /// </returns>
    {$endregion}
    class function MaxSupportedDateTime: TDateTime; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of registered <see cref="TCalendar"/> classes.
    /// </summary>
    /// <returns>
    /// The number of registered <see cref="TCalendar"/> classes.
    /// </returns>
    {$endregion}
    class function Count: Integer; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets a registered <see cref="TCalendar"/> classe by its index.
    /// </summary>
    /// <returns>
    /// The <see cref="TCalendar"/> class at the specified index, or <see langword="nil"/>
    /// if the index is out of range.
    /// </returns>
    /// <seealso cref="ByID"/>
    /// <seealso cref="ByName"/>
    /// <seealso cref="ByLocale"/>
    {$endregion}
    class function ByIndex(Index: Integer): TCalendarClass; inline; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets a <see cref="TCalendar"/> class by its unique identifier.
    /// </summary>
    /// <param name="ID">
    /// The unique identifier of the calendar.
    /// </param>
    /// <returns>
    /// The <see cref="TCalendar"/> class with the specified identifier, or
    /// <see langword="nil"/> if the identifier is unknown.
    /// </returns>
    /// <seealso cref="ByIndex"/>
    /// <seealso cref="ByName"/>
    /// <seealso cref="ByLocale"/>
    {$endregion}
    class function ByID(ID: Cardinal): TCalendarClass; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets a <see cref="TCalendar"/> class by its unique name.
    /// </summary>
    /// <param name="Name">
    /// The unique name of the calendar.
    /// </param>
    /// <returns>
    /// The <see cref="TCalendar"/> class with the specified name, or <see langword="nil"/>
    /// if the name is unknown.
    /// </returns>
    /// <seealso cref="ByIndex"/>
    /// <seealso cref="ByID"/>
    /// <seealso cref="ByLocale"/>
    {$endregion}
    class function ByName(const Name: String): TCalendarClass; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets a <see cref="TCalendar"/> class by its associated locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <returns>
    /// The <see cref="TCalendar"/> class that is associated with the locale, or
    /// <see langword="nil"/> if no calendar class is associated with the locale.
    /// </returns>
    /// <seealso cref="ByIndex"/>
    /// <seealso cref="ByID"/>
    /// <seealso cref="ByLocale"/>
    {$endregion}
    class function ByLocale(const Locale: String): TCalendarClass; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Collects the list of <see cref="TCalendar"/> classes, which are associated
    /// with a specified locale name.
    /// </summary>
    /// <param name="Locale">
    /// The locale name.
    /// </param>
    /// <param name="CalendarClasses">
    /// The list of matched <see cref="TCalendar"/> classes.
    /// </param>
    {$endregion}
    class procedure GetCalendersOfLocale(const Locale: String; CalendarClasses: TCalendarClassList); static;
  end;

implementation

uses
  Math, Types, SysConst, RTLConsts, Windows, i18nWinNLS, i18nCore, i18nUnicode;

resourcestring
  SStreamReadError        = 'Incompatible calendar settings';
  SInvalidEraError        = '%d is not a valid era in %s';
  SInvalidYearError       = '%d is not a valid year in %s (Era = %d)';
  SInvalidMonthError      = '%d/%d is not a valid year/month in %s (Era = %d)';
  SInvalidDayError        = '%d/%d/%d is not a valid year/month/day in %s (Era = %d)';
  SInvalidDayNumError     = 'The numeric value of a day of the week (%d) must be in range 1 to 7.';
  SInvalidDateError       = 'The Gregorian date (%s) is not supported by %s';
  SInvalidDateDayError    = '(%d, %d) is not a valid DateDay pair in %s (Era = %d)';
  SInvalidDateWeekError   = '(%d, %d, %d) is not a valid DateWeek triplet in %s (Era = %d)';
  SInvalidConvertEraError = 'Year %d cannot be converted from era %d to era %d in %s';

{ TCalendarSettings }

constructor TCalendarSettings.Create;
begin
  SetLength(fEraNames, CalendarClass.MaxEra);
  SetLength(fShortEraNames, CalendarClass.MaxEra);
  SetLength(fMonthNames, CalendarClass.MaxMonthsPerYear);
  SetLength(fShortMonthNames, CalendarClass.MaxMonthsPerYear);
  SetLength(fGenitiveMonthNames, CalendarClass.MaxMonthsPerYear);
end;

constructor TCalendarSettings.Create(LocaleID: Cardinal);
begin
  Create;
  Prepare(LocaleID);
end;

constructor TCalendarSettings.Create(const Locale: String);
begin
  Create;
  Prepare(Locale);
end;

destructor TCalendarSettings.Destroy;
begin
  fEraNames := nil;
  fShortEraNames := nil;
  fMonthNames := nil;
  fShortMonthNames := nil;
  fGenitiveMonthNames := nil;
  inherited Destroy;
end;

procedure TCalendarSettings.SetCalendarName(const Value: String);
begin
  if CalendarName <> Value then
  begin
    fCalendarName := Value;
    DoChange;
  end;
end;

function TCalendarSettings.GetEraCount: Integer;
begin
  Result := Length(fEraNames);
end;

function TCalendarSettings.GetEraNames(Era: Integer): String;
begin
  Result := fEraNames[Era - 1];
end;

procedure TCalendarSettings.SetEraNames(Era: Integer; const Value: String);
begin
  if EraNames[Era] <> Value then
  begin
    fEraNames[Era - 1] := Value;
    DoChange;
  end;
end;

function TCalendarSettings.GetShortEraNames(Era: Integer): String;
begin
  Result := fShortEraNames[Era - 1];
end;

procedure TCalendarSettings.SetShortEraNames(Era: Integer; const Value: String);
begin
  if ShortEraNames[Era] <> Value then
  begin
    fShortEraNames[Era - 1] := Value;
    DoChange;
  end;
end;

function TCalendarSettings.GetMonthCount: Integer;
begin
  Result := Length(fMonthNames);
end;

function TCalendarSettings.GetMonthNames(Month: Integer): String;
begin
  Result := fMonthNames[Month - 1];
end;

procedure TCalendarSettings.SetMonthNames(Month: Integer; const Value: String);
begin
  if MonthNames[Month] <> Value then
  begin
    fMonthNames[Month - 1] := Value;
    DoChange;
  end;
end;

function TCalendarSettings.GetShortMonthNames(Month: Integer): String;
begin
  Result := fShortMonthNames[Month - 1];
end;

procedure TCalendarSettings.SetShortMonthNames(Month: Integer; const Value: String);
begin
  if ShortMonthNames[Month] <> Value then
  begin
    fShortMonthNames[Month - 1] := Value;
    DoChange;
  end;
end;

function TCalendarSettings.GetGenitiveMonthNames(Month: Integer): String;
begin
  Result := fGenitiveMonthNames[Month - 1];
end;

procedure TCalendarSettings.SetGenitiveMonthNames(Month: Integer; const Value: String);
begin
  if GenitiveMonthNames[Month] <> Value then
  begin
    fGenitiveMonthNames[Month - 1] := Value;
    DoChange;
  end;
end;

function TCalendarSettings.GetDayNames(DoW: TDayOfWeek): String;
begin
  Result := fDayNames[DoW];
end;

procedure TCalendarSettings.SetDayNames(DoW: TDayOfWeek; const Value: String);
begin
  if DayNames[DoW] <> Value then
  begin
    fDayNames[DoW] := Value;
    DoChange;
  end;
end;

function TCalendarSettings.GetShortDayNames(DoW: TDayOfWeek): String;
begin
  Result := fShortDayNames[DoW];
end;

procedure TCalendarSettings.SetShortDayNames(DoW: TDayOfWeek; const Value: String);
begin
  if ShortDayNames[DoW] <> Value then
  begin
    fShortDayNames[DoW] := Value;
    DoChange;
  end;
end;

function TCalendarSettings.GetShortestDayNames(DoW: TDayOfWeek): String;
begin
  Result := fShortestDayNames[DoW];
end;

procedure TCalendarSettings.SetShortestDayNames(DoW: TDayOfWeek; const Value: String);
begin
  if ShortestDayNames[DoW] <> Value then
  begin
    fShortestDayNames[DoW] := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetLongDateFormat(const Value: String);
begin
  if LongDateFormat <> Value then
  begin
    fLongDateFormat := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetShortDateFormat(const Value: String);
begin
  if ShortDateFormat <> Value then
  begin
    fShortDateFormat := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetYearMonthFormat(const Value: String);
begin
  if YearMonthFormat <> Value then
  begin
    fYearMonthFormat := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetMonthDayFormat(const Value: String);
begin
  if MonthDayFormat <> Value then
  begin
    fMonthDayFormat := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetYearFormat(const Value: String);
begin
  if YearFormat <> Value then
  begin
    fYearFormat := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetTwoDigitYearMax(Value: Word);
begin
  if TwoDigitYearMax <> Value then
  begin
    fTwoDigitYearMax := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetWeekRule(Value: TWeekRule);
begin
  if WeekRule <> Value then
  begin
    fWeekRule := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetFirstDayOfWeek(Value: TDayOfWeek);
begin
  if FirstDayOfWeek <> Value then
  begin
    fFirstDayOfWeek := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetDateSeparator(const Value: String);
begin
  if DateSeparator <> Value then
  begin
    fDateSeparator := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetTimeSeparator(const Value: String);
begin
  if TimeSeparator <> Value then
  begin
    fTimeSeparator := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetTimeAMString(const Value: String);
begin
  if TimeAMString <> Value then
  begin
    fTimeAMString := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetTimePMString(const Value: String);
begin
  if fTimePMString <> Value then
  begin
    fTimePMString := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetShortTimeFormat(const Value: String);
begin
  if ShortTimeFormat <> Value then
  begin
    fShortTimeFormat := Value;
    DoChange;
  end;
end;

procedure TCalendarSettings.SetLongTimeFormat(const Value: String);
begin
  if LongTimeFormat <> Value then
  begin
    fLongTimeFormat := Value;
    DoChange;
  end;
end;

function TCalendarSettings.ScanNames(var S: PChar; Names: array of String): Integer;
var
  I, Len, MaxMatchedLen: Integer;
begin
  Result := 0;
  MaxMatchedLen := 0;
  for I := Low(Names) to High(Names) do
  begin
    Len := Length(Names[I]);
    if (AnsiStrLIComp(S, PChar(Names[I]), Len) = 0) then
    begin
      if Len > MaxMatchedLen then
      begin
        MaxMatchedLen := Len;
        Result := I + 1;
      end;
    end;
  end;
  Inc(S, MaxMatchedLen);
end;

function TCalendarSettings.ScanEraNames(var S: PChar): Integer;
begin
  Result := ScanNames(S, fEraNames);
end;

function TCalendarSettings.ScanShortEraNames(var S: PChar): Integer;
begin
  Result := ScanNames(S, fShortEraNames);
end;

function TCalendarSettings.ScanMonthNames(var S: PChar): Integer;
begin
  Result := ScanNames(S, fMonthNames);
end;

function TCalendarSettings.ScanShortMonthNames(var S: PChar): Integer;
begin
  Result := ScanNames(S, fShortMonthNames);
end;

function TCalendarSettings.ScanGenitiveMonthNames(var S: PChar): Integer;
begin
  Result := ScanNames(S, fGenitiveMonthNames);
end;

function TCalendarSettings.ScanDayNames(var S: PChar): Integer;
begin
  Result := ScanNames(S, fDayNames);
end;

function TCalendarSettings.ScanShortDayNames(var S: PChar): Integer;
begin
  Result := ScanNames(S, fShortDayNames);
end;

function TCalendarSettings.ScanShortestDayNames(var S: PChar): Integer;
begin
  Result := ScanNames(S, fShortestDayNames);
end;

function TCalendarSettings.ScanAMPM(var S: PChar): Integer;
begin
  Result := ScanNames(S, [TimeAMString, TimePMString]);
end;

function TCalendarSettings.SafeGetInfoStr(const Locale: String; CalID: Cardinal;
  LType, CType: Cardinal; const Default: String): String;
begin
  if CalID <> 0 then
  begin
    Result := GetCalendarStr(Locale, CalID, CType, '');
    if Result <> '' then Exit;
  end;
  Result := GetLocaleStr(Locale, LType, Default);
end;

function TCalendarSettings.GetMonthInfoIndex(Month: Integer): Integer;
begin
  Result := Month - 1;
end;

procedure TCalendarSettings.DoChange;
begin
  if fUpdateCount <> 0 then
    fUpdateAffected := True
  else if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCalendarSettings.Prepare(LocaleID: Cardinal);
begin
  Prepare(LCIDToLocaleName(LocaleID));
end;

procedure TCalendarSettings.Prepare(const Locale: String);
begin
  fLocale := Locale;
  BeginUpdate;
  try
    PrepareSettings(Locale, CalendarClass.CalendarID);
  finally
    EndUpdate;
  end;
end;

procedure TCalendarSettings.LoadFromStream(Stream: TStream);
var
  Reader: TReader;
  SourceClassName: String;
begin
  BeginUpdate;
  try
    Reader := TReader.Create(Stream, 1024);
    try
      SourceClassName := Reader.ReadString;
      if not SameText(SourceClassName, CalendarClass.ClassName) then
        EStreamError.CreateRes(@SStreamReadError);
      fLocale := Reader.ReadString;
      ReadSettings(Reader);
    finally
      Reader.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCalendarSettings.SaveToStream(Stream: TStream);
var
  Writer: TWriter;
begin
  Writer := TWriter.Create(Stream, 1024);
  try
    Writer.WriteString(CalendarClass.ClassName);
    Writer.WriteString(Locale);
    WriteSettings(Writer);
    Writer.FlushBuffer;
  finally
    Writer.Free;
  end;
end;

procedure TCalendarSettings.BeginUpdate;
begin
  if fUpdateCount = 0 then
    fUpdateAffected := False;
  Inc(fUpdateCount);
end;

procedure TCalendarSettings.EndUpdate;
begin
  Dec(fUpdateCount);
  if (fUpdateCount = 0) and fUpdateAffected then
    DoChange;
end;

procedure TCalendarSettings.Assign(Source: TPersistent);
var
  I: Integer;
  Day: TDayOfWeek;
begin
  if (Source is TCalendarSettings) and
     (CalendarClass = TCalendarSettings(Source).CalendarClass) then
  begin
    BeginUpdate;
    try
      WeekRule := TCalendarSettings(Source).WeekRule;
      FirstDayOfWeek := TCalendarSettings(Source).FirstDayOfWeek;
      TwoDigitYearMax := TCalendarSettings(Source).TwoDigitYearMax;
      CalendarName := TCalendarSettings(Source).CalendarName;
      for I := 1 to EraCount do
      begin
        EraNames[I] := TCalendarSettings(Source).EraNames[I];
        ShortEraNames[I] := TCalendarSettings(Source).ShortEraNames[I];
      end;
      for I := 1 to MonthCount do
      begin
        MonthNames[I] := TCalendarSettings(Source).MonthNames[I];
        ShortMonthNames[I] := TCalendarSettings(Source).ShortMonthNames[I];
        GenitiveMonthNames[I] := TCalendarSettings(Source).GenitiveMonthNames[I];
      end;
      for Day := Low(TDayOfWeek) to High(TDayOfWeek) do
      begin
        DayNames[Day] := TCalendarSettings(Source).DayNames[Day];
        ShortDayNames[Day] := TCalendarSettings(Source).ShortDayNames[Day];
        ShortestDayNames[Day] := TCalendarSettings(Source).ShortestDayNames[Day];
      end;
      LongDateFormat := TCalendarSettings(Source).LongDateFormat;
      ShortDateFormat := TCalendarSettings(Source).ShortDateFormat;
      YearMonthFormat := TCalendarSettings(Source).YearMonthFormat;
      MonthDayFormat := TCalendarSettings(Source).MonthDayFormat;
      YearFormat := TCalendarSettings(Source).YearFormat;
      ShortTimeFormat := TCalendarSettings(Source).ShortTimeFormat;
      LongTimeFormat := TCalendarSettings(Source).LongTimeFormat;
      DateSeparator := TCalendarSettings(Source).DateSeparator;
      TimeSeparator := TCalendarSettings(Source).TimeSeparator;
      TimeAMString := TCalendarSettings(Source).TimeAMString;
      TimePMString := TCalendarSettings(Source).TimePMString;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TCalendarSettings.PrepareEraName(Era: Integer; const Locale: String;
  CalendarID: Cardinal);
var
  Temp: String;
begin
  Temp := GetCalendarStr(Locale, CalendarID, CAL_SERASTRING, '');
  SetLength(Temp, StrLen(PChar(Temp)));
  EraNames[Era] := Temp;
  Temp := GetCalendarStr(Locale, CalendarID, CAL_SABBREVERASTRING, '');
  SetLength(Temp, StrLen(PChar(Temp)));
  ShortEraNames[Era] := Temp;
end;

procedure TCalendarSettings.PrepareMonthNames(const Locale: String;
  CalendarID: Cardinal);
var
  M, I: Integer;
begin
  for M := 1 to MonthCount do
  begin
    I := GetMonthInfoIndex(M);
    MonthNames[M] := SafeGetInfoStr(Locale, CalendarID,
      LOCALE_SMONTHNAME1 + I, CAL_SMONTHNAME1 + I, '');
    ShortMonthNames[M] := SafeGetInfoStr(Locale, CalendarID,
      LOCALE_SABBREVMONTHNAME1 + I, CAL_SABBREVMONTHNAME1 + I, '');
    GenitiveMonthNames[M] := SafeGetInfoStr(Locale, CalendarID,
      LOCALE_RETURN_GENITIVE_NAMES or CAL_SMONTHNAME1 + I,
      CAL_RETURN_GENITIVE_NAMES or CAL_SMONTHNAME1 + I, MonthNames[M]);
  end;
end;

procedure TCalendarSettings.PrepareDayNames(const Locale: String;
  CalendarID: Cardinal);
var
  Day: TDayOfWeek;
  I: Integer;
begin
  for Day := Low(TDayOfWeek) to High(TDayOfWeek) do
  begin
    I := Ord(Day) - Ord(Monday);
    DayNames[Day] := SafeGetInfoStr(Locale, CalendarID,
      LOCALE_SDAYNAME1 + I, CAL_SDAYNAME1 + I, '');
    ShortDayNames[Day] := SafeGetInfoStr(Locale, CalendarID,
      LOCALE_SABBREVDAYNAME1 + I, CAL_SABBREVDAYNAME1 + I, '');
    ShortestDayNames[Day] := SafeGetInfoStr(Locale, CalendarID,
      LOCALE_SSHORTESTDAYNAME1 + I, CAL_SSHORTESTDAYNAME1 + I, ShortDayNames[Day]);
  end;
end;

procedure TCalendarSettings.PrepareDateFormats(const Locale: String;
  CalendarID: Cardinal);
const
  ArabicComma = #$60C;
var
  TranslateComma: Boolean;
  Format: String;
begin
  TranslateComma := (Pos('Arab;', GetLocaleScripts(Locale)) <> 0);
  Format := SafeGetInfoStr(Locale, CalendarID,
    LOCALE_SLONGDATE, CAL_SLONGDATE, 'dddd, MMMM d, y');
  if TranslateComma then
    LongDateFormat := StringReplace(Format, ',', ArabicComma, [rfReplaceAll])
  else
    LongDateFormat := Format;
  Format := SafeGetInfoStr(Locale, CalendarID,
    LOCALE_SSHORTDATE, CAL_SSHORTDATE, 'dd/MM/yyyy');
  if TranslateComma then
    ShortDateFormat := StringReplace(Format, ',', ArabicComma, [rfReplaceAll])
  else
    ShortDateFormat := Format;
  Format := SafeGetInfoStr(Locale, CalendarID,
    LOCALE_SYEARMONTH, CAL_SYEARMONTH, 'MMMM, y');
  if TranslateComma then
    YearMonthFormat := StringReplace(Format, ',', ArabicComma, [rfReplaceAll])
  else
    YearMonthFormat := Format;
  Format := SafeGetInfoStr(Locale, CalendarID,
    LOCALE_SMONTHDAY, CAL_SMONTHDAY, 'd MMMM');
  if TranslateComma then
    MonthDayFormat := StringReplace(Format, ',', ArabicComma, [rfReplaceAll])
  else
    MonthDayFormat := Format;
  YearFormat := 'y';
end;

procedure TCalendarSettings.PrepareTimeFormats(const Locale: String);
var
  TimePrefix, TimePostfix: String;
  HourFmt: String;
begin
  TimePrefix := '';
  TimePostfix := '';
  if GetLocaleInt(Locale, LOCALE_ITIME, 0) = 0 then
  begin
    if GetLocaleInt(Locale, LOCALE_ITIMEMARKPOSN, 0) = 0 then
      TimePostfix := ' AMPM'
    else
      TimePrefix := 'AMPM ';
  end;
  if GetLocaleInt(Locale, LOCALE_ITLZERO, 0) = 0 then
    HourFmt := 'h'
  else
    HourFmt := 'hh';
  DateSeparator := GetLocaleStr(Locale, LOCALE_SDATE, '/');
  TimeSeparator := GetLocaleStr(Locale, LOCALE_STIME, ':');
  TimeAMString := GetLocaleStr(Locale, LOCALE_S1159, 'am');
  TimePMString := GetLocaleStr(Locale, LOCALE_S2359, 'pm');
  ShortTimeFormat := GetLocaleStr(Locale, LOCALE_SSHORTTIME, TimePrefix + HourFmt + ':nn' + TimePostfix);
  ShortTimeFormat := StringReplace(ShortTimeFormat, 'tt', 'ampm', [rfReplaceAll]);
  LongTimeFormat := GetLocaleStr(Locale, LOCALE_STIMEFORMAT, TimePrefix + HourFmt + ':nn:ss' + TimePostfix);
  LongTimeFormat := StringReplace(LongTimeFormat, 'tt', 'ampm', [rfReplaceAll]);
end;

procedure TCalendarSettings.PrepareWeekParams(const Locale: String);
const
  FullWeekLocales: array[1..2] of String = ('et-EE', 'ur-PK');
  FirstFourWeekLocales: array[1..34] of String = ('de-AT', 'fr-BE', 'nl-BE',
    'cs-CZ', 'da-DK', 'et-EE', 'fo-FO', 'fi-FI', 'sv-FI', 'mk-MK', 'fr-FR',
    'de-DE', 'el-GR', 'is-IS', 'en-IE', 'it-IT', 'lv-LV', 'de-LI', 'de-LU',
    'fr-LU', 'nl-NL', 'nb-NO', 'nn-NO', 'pl-PL', 'pt-PT', 'ca-ES', 'es-ES',
    'es-ES_tradnl', 'eu-ES', 'sv-SE', 'de-CH', 'it-CH', 'fr-CH', 'en-GB');

  function IsMemberOf(const Locales: array of String): Boolean;
  var
    I: Integer;
  begin
    for I := Low(Locales) to High(Locales) do
      if SameText(Locale, Locales[I]) then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;

begin
  FirstDayOfWeek := TDayOfWeek(GetLocaleInt(Locale, LOCALE_IFIRSTDAYOFWEEK, 0) + 1);
  if IsMemberOf(FullWeekLocales) then
    WeekRule := wrFullWeek
  else if IsMemberOf(FirstFourWeekLocales) then
    WeekRule := wrFirstFourDayWeek
  else
    WeekRule := wrFirstDayWeek;
end;

procedure TCalendarSettings.PrepareSettings(const Locale: String;
  CalendarID: Cardinal);
var
  Era: Integer;
begin
  CalendarName := GetCalendarStr(Locale, CalendarID, CAL_SCALNAME, CalendarClass.CalendarName);
  for Era := 1 to  EraCount do
    PrepareEraName(Era, Locale, CalendarID);
  PrepareMonthNames(Locale, CalendarID);
  PrepareDayNames(Locale, CalendarID);
  PrepareDateFormats(Locale, CalendarID);
  PrepareDateFormats(Locale, CalendarID);
  PrepareTimeFormats(Locale);
  PrepareWeekParams(Locale);
  TwoDigitYearMax := GetCalendarInt(Locale, CalendarID, CAL_ITWODIGITYEARMAX, 99);
end;

procedure TCalendarSettings.ReadSettings(Reader: TReader);
var
  I: Integer;
  Day: TDayOfWeek;
begin
  WeekRule := TWeekRule(Reader.ReadInteger);
  FirstDayOfWeek := TDayOfWeek(Reader.ReadInteger);
  TwoDigitYearMax := Reader.ReadInteger;
  CalendarName := Reader.ReadString;
  for I := 1 to EraCount do
  begin
    EraNames[I] := Reader.ReadString;
    ShortEraNames[I] := Reader.ReadString;
  end;
  for I := 1 to MonthCount do
  begin
    MonthNames[I] := Reader.ReadString;
    ShortMonthNames[I] := Reader.ReadString;
    GenitiveMonthNames[I] := Reader.ReadString;
  end;
  for Day := Low(TDayOfWeek) to High(TDayOfWeek) do
  begin
    DayNames[Day] := Reader.ReadString;
    ShortDayNames[Day] := Reader.ReadString;
    ShortestDayNames[Day] := Reader.ReadString;
  end;
  LongDateFormat := Reader.ReadString;
  ShortDateFormat := Reader.ReadString;
  YearMonthFormat := Reader.ReadString;
  MonthDayFormat := Reader.ReadString;
  YearFormat := Reader.ReadString;
  ShortTimeFormat := Reader.ReadString;
  LongTimeFormat := Reader.ReadString;
  DateSeparator := Reader.ReadString;
  TimeSeparator := Reader.ReadString;
  TimeAMString := Reader.ReadString;
  TimePMString := Reader.ReadString;
end;

procedure TCalendarSettings.WriteSettings(Writer: TWriter);
var
  I: Integer;
  Day: TDayOfWeek;
begin
  Writer.WriteInteger(Ord(WeekRule));
  Writer.WriteInteger(Ord(FirstDayOfWeek));
  Writer.WriteInteger(TwoDigitYearMax);
  Writer.WriteString(CalendarName);
  for I := 1 to EraCount do
  begin
    Writer.WriteString(EraNames[I]);
    Writer.WriteString(ShortEraNames[I]);
  end;
  for I := 1 to MonthCount do
  begin
    Writer.WriteString(MonthNames[I]);
    Writer.WriteString(ShortMonthNames[I]);
    Writer.WriteString(GenitiveMonthNames[I]);
  end;
  for Day := Low(TDayOfWeek) to High(TDayOfWeek) do
  begin
    Writer.WriteString(DayNames[Day]);
    Writer.WriteString(ShortDayNames[Day]);
    Writer.WriteString(ShortestDayNames[Day]);
  end;
  Writer.WriteString(LongDateFormat);
  Writer.WriteString(ShortDateFormat);
  Writer.WriteString(YearMonthFormat);
  Writer.WriteString(MonthDayFormat);
  Writer.WriteString(YearFormat);
  Writer.WriteString(ShortTimeFormat);
  Writer.WriteString(LongTimeFormat);
  Writer.WriteString(DateSeparator);
  Writer.WriteString(TimeSeparator);
  Writer.WriteString(TimeAMString);
  Writer.WriteString(TimePMString);
end;

{ TCalendar }

constructor TCalendar.Create;
begin
  fSettings := SettingsClass.Create;
  fSettings.OnChange := SettingsChanged;
  fDefaultEra := MaxEra;
end;

constructor TCalendar.Create(LocaleID: Integer);
begin
  Create;
  Settings.Prepare(LocaleID);
end;

constructor TCalendar.Create(const Locale: String);
begin
  Create;
  Settings.Prepare(Locale);
end;

destructor TCalendar.Destroy;
begin
  fSettings.Free;
  inherited Destroy;
end;

procedure TCalendar.SettingsChanged(Sender: TObject);
begin
  DoChange;
end;

procedure TCalendar.Assign(Source: TPersistent);
begin
  if Source is TCalendar then
  begin
    DefaultEra := TCalendar(Source).DefaultEra;
    Settings.Assign(TCalendar(Source).Settings);
  end
  else if Source is TCalendarSettings then
    Settings.Assign(Source)
  else
    inherited Assign(Source);
end;

procedure TCalendar.AssignTo(Dest: TPersistent);
begin
  if Dest is TCalendar then
    TCalendar(Dest).Assign(Self)
  else if Dest is TCalendarSettings then
    TCalendarSettings(Dest).Assign(Self.Settings)
  else
    inherited AssignTo(Dest);
end;

function TCalendar.Clone: TCalendar;
begin
  Result := TCalendarClass(ClassType).Create;
  Result.Assign(Self);
end;

procedure TCalendar.SetSettings(Value: TCalendarSettings);
begin
  Settings.Assign(Value);
end;

procedure TCalendar.SetDefaultEra(Value: Integer);
begin
  if DefaultEra <> Value then
  begin
    if not IsValidEra(Value) then
      EraError(Value);
    fDefaultEra := Value;
    DoChange;
  end;
end;

class function TCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := TCalendarSettings;
end;

procedure TCalendar.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

class procedure TCalendar.EraError(Era: Integer);
begin
  raise EEncodingError.CreateResFmt(@SInvalidEraError,
    [Era, CalendarName]);
end;

class procedure TCalendar.YearError(Era, Year: Integer);
begin
  raise EEncodingError.CreateResFmt(@SInvalidYearError,
    [Year, CalendarName, Era]);
end;

class procedure TCalendar.MonthError(Era, Year, Month: Integer);
begin
  raise EEncodingError.CreateResFmt(@SInvalidMonthError,
    [Year, Month, CalendarName, Era]);
end;

class procedure TCalendar.DayError(Era, Year, Month, Day: Integer);
begin
  raise EEncodingError.CreateResFmt(@SInvalidDayError,
    [Year, Month, Day, CalendarName, Era]);
end;

class procedure TCalendar.DayNumError(DayNum: Integer);
begin
  raise EConvertError.CreateResFmt(@SInvalidDayNumError,
    [DayNum]);
end;

class procedure TCalendar.DateError(const DateTime: TDateTime);
begin
  raise EConvertError.CreateResFmt(@SInvalidDateError,
    [DateToStr(DateTime), CalendarName]);
end;

class procedure TCalendar.DateDayError(Era, Year, DayOfYear: Integer);
begin
  raise EEncodingError.CreateResFmt(@SInvalidDateDayError,
    [Year, DayOfYear, CalendarName, Era]);
end;

class procedure TCalendar.DateWeekError(Era, Year, Week: Integer; DoW: TDayOfWeek);
begin
  raise EEncodingError.CreateResFmt(@SInvalidDateWeekError,
    [Year, Week, Ord(DoW), CalendarName, Era]);
end;

class procedure TCalendar.ConvertEraError(Year, FromEra, ToEra: Integer);
begin
  raise EComponentError.CreateResFmt(@SInvalidConvertEraError, [Year, FromEra, ToEra, CalendarName]);
end;

class procedure TCalendar.Divide(Numerator, Denominator: Integer;
  out Quotient, Reminder: Integer);
begin
  Quotient := Floor(Numerator / Denominator);
  Reminder := Numerator - (Denominator * Quotient);
end;

class function TCalendar.Modulus(Numerator, Denominator: Integer): Integer;
begin
  Result := Numerator - (Denominator * Floor(Numerator / Denominator));
end;

function TCalendar.ToZeroBase(Era, Year: Integer): Integer;
begin
  Result := Year;
  if (Year < 1) and not CanHaveZeroYear(Era) then
    Inc(Result)
end;

function TCalendar.FromZeroBase(Era, Year: Integer): Integer;
begin
  Result := Year;
  if (Year < 1) and not CanHaveZeroYear(Era) then
    Dec(Result)
end;

class function TCalendar.CalendarID: Cardinal;
begin
  Result := 0;
end;

class function TCalendar.CalendarKind: TCalendarKind;
begin
  Result := ckUnknown;
end;

class function TCalendar.CalendarName: String;
var
  I: Integer;
  NeedsSpace: Boolean;
  NeedsCloseParenthesis: Boolean;
begin
  Result := String(ClassName);
  Delete(Result, 1, 1);
  NeedsSpace := False;
  NeedsCloseParenthesis := False;
  I := 1;
  while I <= Length(Result) do
  begin
    if (Result[I] = '_') and not NeedsCloseParenthesis then
    begin
      Result[I] := ' ';
      Inc(I);
      Insert('(', Result, I);
      NeedsSpace := False;
      NeedsCloseParenthesis := True;
    end
    else if not CharInSet(Result[I], ['A'..'Z', '0'..'9']) then
      NeedsSpace := True
    else if NeedsSpace then
    begin
      Insert(' ', Result, I);
      Inc(I);
      NeedsSpace := False;
    end;
    Inc(I);
  end;
  if NeedsCloseParenthesis then
    Insert(')', Result, I);
end;

class function TCalendar.MaxEra: Integer;
begin
  Result := 1;
end;

class function TCalendar.MaxMonthsPerYear: Integer;
begin
  Result := 12;
end;

class function TCalendar.MinSupportedDateTime: TDateTime;
begin
  Result := SysUtils.MinDateTime;
end;

class function TCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := SysUtils.MaxDateTime;
end;

class function TCalendar.Today: TDate;
begin
  Result := SysUtils.Date;
end;

class function TCalendar.Yesterday: TDate;
begin
  Result := PrevDay(Today);
end;

class function TCalendar.Tomorrow: TDate;
begin
  Result := NextDay(Today);
end;

class function TCalendar.FirstMonthOfYear: Integer;
begin
  Result := 1;
end;

function TCalendar.DaysToMonth(Era, Year, Month: Integer): Integer;
begin
  if Month <> FirstMonthOfYear then
    Result := DaysBetween(StartOfMonth(Era, Year, Month), StartOfYear(Era, Year))
  else
    Result := 0;
end;

function TCalendar.DaysToMonth(Year, Month: Integer): Integer;
begin
  Result := DaysToMonth(DefaultEra, Year, Month);
end;

function TCalendar.DaysToMonth(const DateTime: TDateTime): Integer;
var
  Era, Year, Month, Day: Integer;
begin
  DecodeDate(DateTime, Era, Year, Month, Day);
  Result := DaysToMonth(Era, Year, Month);
end;

function TCalendar.DaysInMonth(Era, Year, Month: Integer): Integer;
begin
  Result := DaysBetween(EndOfMonth(Era, Year, Month), StartOfMonth(Era, Year, Month)) + 1;
end;

function TCalendar.DaysInMonth(Year, Month: Integer): Integer;
begin
  Result := DaysInMonth(DefaultEra, Year, Month);
end;

function TCalendar.DaysInMonth(const DateTime: TDateTime): Integer;
var
  Era, Year, Month, Day: Integer;
begin
  DecodeDate(DateTime, Era, Year, Month, Day);
  Result := DaysInMonth(Era, Year, Month);
end;

function TCalendar.DaysInYear(Era, Year: Integer): Integer;
begin
  Result := DaysBetween(EndOfYear(Era, Year), StartOfYear(Era, Year)) + 1;
end;

function TCalendar.DaysInYear(Year: Integer): Integer;
begin
  Result := DaysInYear(DefaultEra, Year);
end;

function TCalendar.DaysInYear(const DateTime: TDateTime): Integer;
var
  Era, Year, Month, Day: Integer;
begin
  DecodeDate(DateTime, Era, Year, Month, Day);
  Result := DaysInYear(Era, Year);
end;

function TCalendar.MonthsInYear(Era, Year: Integer): Integer;
begin
  Result := MaxMonthsPerYear;
end;

function TCalendar.MonthsInYear(Year: Integer): Integer;
begin
  Result := MonthsInYear(DefaultEra, Year);
end;

function TCalendar.MonthsInYear(const DateTime: TDateTime): Integer;
var
  Era, Year, Month, Day: Integer;
begin
  DecodeDate(DateTime, Era, Year, Month, Day);
  Result := MonthsInYear(Era, Year);
end;

function TCalendar.WeeksInYear(Era, Year: Integer): Integer;
var
  LastWeekDate: TDate;
begin
  LastWeekDate := StartOfWeek(EndOfYear(Era, Year));
  Result := WeekOfYear(LastWeekDate);
  if Result = 1 then
  begin
    DecWeek(LastWeekDate);
    Result := WeekOfYear(LastWeekDate);
  end;
end;

function TCalendar.WeeksInYear(Year: Integer): Integer;
begin
  Result := WeeksInYear(DefaultEra, Year);
end;

function TCalendar.WeeksInYear(const DateTime: TDateTime): Integer;
var
  Era, Year, Month, Day: Integer;
begin
  DecodeDate(DateTime, Era, Year, Month, Day);
  Result := WeeksInYear(Era, Year);
end;

function TCalendar.WeeksInMonth(Era, Year, Month: Integer): Integer;
begin
  Result := ((DaysInMonth(Era, Year, Month) - 1) div DaysPerWeek) + 1;
end;

function TCalendar.WeeksInMonth(Year, Month: Integer): Integer;
begin
  Result := WeeksInMonth(DefaultEra, Year, Month);
end;

function TCalendar.WeeksInMonth(const DateTime: TDateTime): Integer;
var
  Era, Year, Month, Day: Integer;
begin
  DecodeDate(DateTime, Era, Year, Month, Day);
  Result := WeeksInMonth(Era, Year, Month);
end;

function TCalendar.DayOfYearToDayOfMonth(Era, Year, DayOfYear: Integer;
  var Month, Day: Integer): Boolean;
var
  FirstMonth: Integer;
  MonthsPerYear: Integer;
  DaysPerMonth: Integer;
begin
  FirstMonth := FirstMonthOfYear;
  Day := DayOfYear;
  Month := FirstMonth;
  MonthsPerYear := MonthsInYear(Era, Year);
  DaysPerMonth := DaysInMonth(Era, Year, Month);
  while Day > DaysPerMonth do
  begin
    Dec(Day, DaysPerMonth);
    Inc(Month);
    if Month > MonthsPerYear then
    begin
      Month := 1;
      if Month = FirstMonth then
      begin
        Result := False;
        Exit;
      end;
    end;
    DaysPerMonth := DaysInMonth(Era, Year, Month);
  end;
  Result := (Day >= 1);
end;

function TCalendar.DayOfYearToDayOfMonth(Year, DayOfYear: Integer;
  var Month, Day: Integer): Boolean;
begin
  Result := DayOfYearToDayOfMonth(DefaultEra, Year, DayOfYear, Month, Day);
end;

function TCalendar.DayOfMonthToDayOfYear(Era, Year, Month, Day: Integer;
  out DayOfYear: Integer): Boolean;
begin
  if IsValidDay(Era, Year, Month, Day) then
  begin
    if Month = FirstMonthOfYear then
      DayOfYear := Day
    else
      DayOfYear := DaysToMonth(Era, Year, Month) + Day;
    Result := True;
  end
  else
    Result := False;
end;

function TCalendar.DayOfMonthToDayOfYear(Year, Month, Day: Integer;
  out DayOfYear: Integer): Boolean;
begin
  Result := DayOfMonthToDayOfYear(DefaultEra, Year, Month, Day, DayOfYear);
end;

function TCalendar.ConvertYear(Year, FromEra, ToEra: Integer): Integer;
begin
  if FromEra <> ToEra then
    ConvertEraError(Year, FromEra, ToEra);
  if not IsValidYear(FromEra, Year) then
    YearError(FromEra, Year);
  Result := Year;
end;

function TCalendar.AdjustJulianDay(const JD: Extended; Backward: Boolean): Extended;
begin
  Result := JD;
end;

function TCalendar.BaseEra: Integer;
begin
  Result := 1;
end;

function TCalendar.BestEraOfBaseEraYear(Year: Integer): Integer;
begin
  Result := DefaultEra;
end;

function TCalendar.ToFourDigitYear(Year: Integer): Integer;
begin
  Result := Year;
  if (Year > 0) and (Year < 100) then
  begin
    Inc(Result, 100 * (Settings.TwoDigitYearMax div 100));
    if Result > Settings.TwoDigitYearMax then
      Dec(Result, 100);
  end;
end;

function TCalendar.OffsetYear(Year, NumberOfYears, FromEra, ToEra: Integer): Integer;
begin
  // In most calendars the year zero is meaningless and we should take care of it.
  Result := FromZeroBase(ToEra, ToZeroBase(FromEra, Year) + NumberOfYears);
end;

function TCalendar.OffsetYear(Era, Year, NumberOfYears: Integer): Integer;
begin
  Result := OffsetYear(Year, NumberOfYears, Era, Era);
end;

function TCalendar.OffsetYear(Year, NumberOfYears: Integer): Integer;
begin
  Result := OffsetYear(DefaultEra, Year, NumberOfYears);
end;

class function TCalendar.OffsetDateTime(const DateTime: TDateTime;
  const Delta: Double): TDateTime;
begin
  // We should take in account that in TDateTime type
  // 1. The fraction part is always a positive value even when the whole value is negative
  // 2. There is no value in range (-1, 0)
  Result := JulianDayToDateTime(DateTimeToJulianDay(DateTime) + Delta);
end;

class function TCalendar.DaysBetween(DateTime1, DateTime2: TDateTime): Integer;
begin
  Result := Abs(Trunc(DateTime1) - Trunc(DateTime2));
end;

class function TCalendar.DaysOfWeekBetween(DoW1, DoW2: TDayOfWeek): Integer;
begin
  Result := (Ord(DoW1) - Ord(DoW2) + DaysPerWeek) mod DaysPerWeek;
end;

function TCalendar.DayOfWeekToDayNum(DoW: TDayOfWeek): Integer;
begin
  Result := ((DaysPerWeek - Ord(Settings.FirstDayOfWeek) + Ord(Dow)) mod DaysPerWeek) + 1;
end;

function TCalendar.DayNumToDayOfWeek(DayNum: Integer): TDayOfWeek;
begin
  if DayNum in [1..7] then
    DayNumError(DayNum);
  Result := TDayOfWeek(((Ord(Settings.FirstDayOfWeek) + DayNum - 2) mod DaysPerWeek) + 1);
end;

function TCalendar.CanHaveZeroYear(Era: Integer): Boolean;
begin
  Result := False;
end;

class function TCalendar.IsNoDate(const DateTime: TDateTime): Boolean;
begin
  Result := (DateTime > -1) and (DateTime < 0);
end;

function TCalendar.IsValidDate(const DateTime: TDateTime): Boolean;
begin
  Result := IsInRange(DateTime, MinSupportedDateTime, MaxSupportedDateTime);
end;

function TCalendar.IsValidDateDay(Era, Year, DayOfYear: Integer): Boolean;
begin
  Result := IsValidYear(Era, Year)
        and (DayOfYear >= 1) and (DayOfYear <= DaysInYear(Era, Year));
end;

function TCalendar.IsValidDateDay(Year, DayOfYear: Integer): Boolean;
begin
  Result := IsValidDateDay(DefaultEra, Year);
end;

function TCalendar.IsValidDateWeek(Era, Year, Week: Integer;
  DoW: TDayOfWeek): Boolean;
begin
  Result := IsValidYear(Era, Year)
        and (Week >= 1) and (Week <= WeeksInYear(Era, Year))
        and (Ord(DoW) in [Ord(Low(TDayOfWeek))..Ord(High(TDayOfWeek))]);
end;

function TCalendar.IsValidDateWeek(Year, Week: Integer;
  DoW: TDayOfWeek): Boolean;
begin
  Result := IsValidDateWeek(DefaultEra, Year, DoW);
end;

function TCalendar.IsValidEra(Era: Integer): Boolean;
begin
  Result := (Era = DefaultEra) or ((Era >= 1) and (Era <= MaxEra));
end;

function TCalendar.IsValidYear(Era, Year: Integer): Boolean;
begin
  Result := IsValidEra(Era) and ((Year <> 0) or CanHaveZeroYear(Era));
end;

function TCalendar.IsValidYear(Year: Integer): Boolean;
begin
  Result := IsValidYear(DefaultEra, Year);
end;

function TCalendar.IsValidMonth(Era, Year, Month: Integer): Boolean;
begin
  Result := IsValidYear(Era, Year)
        and ((Month = FirstMonthOfYear) or ((Month >= 1) and (Month <= MonthsInYear(Era, Year))));
end;

function TCalendar.IsValidMonth(Year, Month: Integer): Boolean;
begin
  Result := IsValidMonth(DefaultEra, Year, Month);
end;

function TCalendar.IsValidDay(Era, Year, Month, Day: Integer): Boolean;
begin
  Result := IsValidMonth(Era, Year, Month)
        and ((Day = 1) or ((Day >= 1) and (Day <= DaysInMonth(Era, Year, Month))));
end;

function TCalendar.IsValidDay(Year, Month, Day: Integer): Boolean;
begin
  Result := IsValidDay(DefaultEra, Year, Month)
end;

function TCalendar.IsLeapYear(Era, Year: Integer): Boolean;
begin
  if not IsValidYear(Era, Year) then
    YearError(Era, Year);
  Result := False;
end;

function TCalendar.IsLeapYear(Year: Integer): Boolean;
begin
  Result := IsLeapYear(DefaultEra, Year);
end;

function TCalendar.IsInLeapYear(const DateTime: TDateTime): Boolean;
var
  Era, Year, Month, Day: Integer;
begin
  DecodeDate(DateTime, Era, Year, Month, Day);
  Result := IsLeapYear(Era, Year);
end;

class function TCalendar.IsInRange(const DateTime, RangeStart, RangeEnd: TDateTime): Boolean;
begin
  Result := not IsNoDate(DateTime)
        and (Compare(DateTime, RangeStart) >= 0) and (Compare(DateTime, RangeEnd) <= 0);
end;

function TCalendar.IsThisEra(const DateTime: TDateTime): Boolean;
begin
  Result := IsSameEra(DateTime, Today);
end;

function TCalendar.IsThisEra(Era: Integer): Boolean;
begin
  Result := (EraOf(Today) = Era);
end;

function TCalendar.IsThisYear(const DateTime: TDateTime): Boolean;
begin
  Result := IsSameYear(DateTime, Today);
end;

function TCalendar.IsThisYear(Era, Year: Integer): Boolean;
begin
  Result := IsThisYear(ConvertYear(Year, Era, DefaultEra));
end;

function TCalendar.IsThisYear(Year: Integer): Boolean;
begin
  Result := (YearOf(Today) = Year);
end;

function TCalendar.IsThisMonth(const DateTime: TDateTime): Boolean;
begin
  Result := IsSameMonth(DateTime, Today);
end;

function TCalendar.IsThisMonth(Era, Year, Month: Integer): Boolean;
begin
  Result := IsThisMonth(ConvertYear(Year, Era, DefaultEra), Month);
end;

function TCalendar.IsThisMonth(Year, Month: Integer): Boolean;
var
  TodayYear, TodayMonth, TodayDay: Integer;
begin
  DecodeDate(Today, TodayYear, TodayMonth, TodayDay);
  Result := (TodayYear = Year) and (TodayMonth = Month);
end;

function TCalendar.IsThisMonth(Month: Integer): Boolean;
begin
  Result := (MonthOfYear(Today) = Month);
end;

function TCalendar.IsThisWeek(const DateTime: TDateTime): Boolean;
begin
  Result := IsSameWeek(DateTime, Today);
end;

class function TCalendar.IsToday(const DateTime: TDateTime): Boolean;
begin
  Result := IsSameDay(DateTime, Today);
end;

class function TCalendar.IsToday(DoW: TDayOfWeek): Boolean;
begin
  Result := (DayOfWeek(Today) = DoW);
end;

class function TCalendar.IsPM(const DateTime: TDateTime): Boolean;
begin
  Result := TimeOf(DateTime) >= 0.5;
end;

class function TCalendar.IsSame(const DateTime1, DateTime2: TDateTime): Boolean;
begin
  Result := (Abs(DateTime1 - DateTime2) < OneMilliSecond);
end;

function TCalendar.IsSameEra(const DateTime1, DateTime2: TDateTime): Boolean;
begin
  Result := (EraOf(DateTime1) = EraOf(DateTime2));
end;

function TCalendar.IsSameYear(const DateTime1, DateTime2: TDateTime): Boolean;
var
  Era1, Year1, Month1, Day1: Integer;
  Era2, Year2, Month2, Day2: Integer;
begin
  DecodeDate(DateTime1, Era1, Year1, Month1, Day1);
  DecodeDate(DateTime2, Era2, Year2, Month2, Day2);
  Result := (Era1 = Era2) and (Year1 = Year2);
end;

function TCalendar.IsSameMonth(const DateTime1, DateTime2: TDateTime): Boolean;
var
  Era1, Year1, Month1, Day1: Integer;
  Era2, Year2, Month2, Day2: Integer;
begin
  DecodeDate(DateTime1, Era1, Year1, Month1, Day1);
  DecodeDate(DateTime2, Era2, Year2, Month2, Day2);
  Result := (Era1 = Era2) and (Year1 = Year2) and (Month1 = Month2);
end;

function TCalendar.IsSameWeek(const DateTime1, DateTime2: TDateTime): Boolean;
begin
  Result := IsInRange(DateTime1, StartOfWeek(DateTime2), EndOfWeek(DateTime2));
end;

class function TCalendar.IsSameDay(const DateTime1, DateTime2: TDateTime): Boolean;
begin
  Result := (Trunc(DateTime1) = Trunc(DateTime2));
end;

class function TCalendar.Compare(const DateTime1, DateTime2: TDateTime): Integer;
begin
  if IsSame(DateTime1, DateTime2) then
    Result := 0
  else if DateTime1 < DateTime2 then
    Result := -1
  else
    Result := +1;
end;

class function TCalendar.CompareDate(const DateTime1, DateTime2: TDateTime): Integer;
begin
  Result := Compare(DateOf(DateTime1), DateOf(DateTime2));
end;

class function TCalendar.CompareTime(const DateTime1, DateTime2: TDateTime): Integer;
begin
  Result := Compare(TimeOf(DateTime1), TimeOf(DateTime2));
end;

function TCalendar.TryEncodeDateWeek(Era, Year, Week: Integer;
  DoW: TDayOfWeek; out DateTime: TDateTime): Boolean;
var
  YearStart: TDateTime;
  DayOfYear: Integer;
  Offset: Integer;
begin
  Result := False;
  if IsValidDateWeek(Era, Year, Week, DoW) then
  begin
    YearStart := StartOfYear(Era, Year);
    DayOfYear := ((Week - 1) * DaysPerWeek) + DaysOfWeekBetween(DoW, Settings.FirstDayOfWeek);
    Offset := DaysOfWeekBetween(DayOfWeek(YearStart), Settings.FirstDayOfWeek);
    case Settings.WeekRule of
      wrFirstFourDayWeek:
        if Offset >= 4 then
          Dec(Offset, DaysPerWeek);
      wrFullWeek:
        if Offset <> 0 then
          Dec(Offset, DaysPerWeek);
    end;
    DateTime := YearStart + DayOfYear - Offset;
    Result := True;
  end;
end;

function TCalendar.TryEncodeDateWeek(Year, Week: Integer;
  DoW: TDayOfWeek; out DateTime: TDateTime): Boolean;
begin
  Result := TryEncodeDateWeek(DefaultEra, Year, Week, DoW, DateTime);
end;

function TCalendar.EncodeDateWeek(Era, Year, Week: Integer;
  DoW: TDayOfWeek): TDateTime;
begin
  if not TryEncodeDateWeek(Era, Year, Week, DoW, Result) then
    DateWeekError(Era, Year, Week, DoW);
end;

function TCalendar.EncodeDateWeek(Year, Week: Integer;
  DoW: TDayOfWeek): TDateTime;
begin
  Result := EncodeDateWeek(DefaultEra, Year, Week, DoW);
end;

function TCalendar.TryDecodeDateWeek(const DateTime: TDateTime;
  out Era, Year, Week: Integer; out DoW: TDayOfWeek): Boolean;

  function DoDecode(const Date: TDateTime): Boolean;
  var
    DayOfYear: Integer;
    FirstOffset, Offset: Integer;
  begin
    Result := TryDecodeDateDay(Date, Era, Year, DayOfYear);
    if Result then
    begin
      FirstOffset := Ord(DayOfWeek(Date)) - ((DayOfYear - 1) mod DaysPerWeek);
      if Settings.WeekRule = wrFirstDayWeek then
      begin
        Offset := (FirstOffset - Ord(Settings.FirstDayOfWeek) + 2 * DaysPerWeek) mod DaysPerWeek;
        Week := ((DayOfYear + Offset - 1) div DaysPerWeek) + 1;
      end
      else
      begin
        Offset := (Ord(Settings.FirstDayOfWeek) - FirstOffset + 2 * DaysPerWeek) mod DaysPerWeek;
        if (Settings.WeekRule = wrFirstFourDayWeek) and (Offset >= 4) then
          Dec(Offset, DaysPerWeek);
        if DayOfYear > Offset then
          Week := ((DayOfYear - Offset - 1) div DaysPerWeek) + 1
        else
          Result := DoDecode(Date - DayOfYear);
      end;
    end;
  end;

begin
  DoW := DayOfWeek(DateTime);
  if (Settings.WeekRule = wrFirstFourDayWeek) and
     (DaysOfWeekBetween(DoW, Settings.FirstDayOfWeek) <= 2)
  then
    Result := DoDecode(DateTime + 3)
  else
    Result := DoDecode(DateTime);
end;

function TCalendar.TryDecodeDateWeek(const DateTime: TDateTime;
  out Year, Week: Integer; out DoW: TDayOfWeek): Boolean;
var
  Era: Integer;
begin
  Result := TryDecodeDateWeek(DateTime, Era, Year, Week, DoW);
  if Result and (Era <> DefaultEra) then
    Year := ConvertYear(Year, Era, DefaultEra);
end;

procedure TCalendar.DecodeDateWeek(const DateTime: TDateTime;
  out Era, Year, Week: Integer; out DoW: TDayOfWeek);
begin
  if not TryDecodeDateWeek(DateTime, Era, Year, Week, DoW) then
    DateError(DateTime);
end;

procedure TCalendar.DecodeDateWeek(const DateTime: TDateTime;
  out Year, Week: Integer; out DoW: TDayOfWeek);
begin
  if not TryDecodeDateWeek(DateTime, Year, Week, DoW) then
    DateError(DateTime);
end;

function TCalendar.TryEncodeDateDay(Era, Year, DayOfYear: Integer;
  out DateTime: TDateTime): Boolean;
begin
  if IsValidDateDay(Era, Year, DayOfYear) then
  begin
    DateTime := StartOfYear(Era, Year) + (DayOfYear - 1);
    Result := True;
  end
  else
    Result := False;
end;

function TCalendar.TryEncodeDateDay(Year, DayOfYear: Integer;
  out DateTime: TDateTime): Boolean;
begin
  Result := TryEncodeDateDay(DefaultEra, Year, DayOfYear, DateTime);
end;

function TCalendar.EncodeDateDay(Era, Year, DayOfYear: Integer): TDateTime;
begin
  if not TryEncodeDateDay(Era, Year, DayOfYear, Result) then
    DateDayError(Era, Year, DayOfYear);
end;

function TCalendar.EncodeDateDay(Year, DayOfYear: Integer): TDateTime;
begin
  Result := EncodeDateDay(DefaultEra, Year, DayOfYear);
end;

function TCalendar.TryDecodeDateDay(const DateTime: TDateTime;
  out Era, Year, DayOfYear: Integer): Boolean;
var
  Month, Day: Integer;
begin
  Result := TryDecodeDate(DateTime, Era, Year, Month, Day)
        and DayOfMonthToDayOfYear(Era, Year, Month, Day, DayOfYear);
end;

function TCalendar.TryDecodeDateDay(const DateTime: TDateTime;
  out Year, DayOfYear: Integer): Boolean;
var
  Era: Integer;
begin
  Result := TryDecodeDateDay(DateTime, Era, Year, DayOfYear);
  if Result and (Era <> DefaultEra) then
    Year := ConvertYear(Year, Era, DefaultEra);
end;

procedure TCalendar.DecodeDateDay(const DateTime: TDateTime;
  out Era, Year, DayOfYear: Integer);
begin
  if not TryDecodeDateDay(DateTime, Era, Year, DayOfYear) then
    DateError(DateTime);
end;

procedure TCalendar.DecodeDateDay(const DateTime: TDateTime;
  out Year, DayOfYear: Integer);
begin
  if not TryDecodeDateDay(DateTime, Year, DayOfYear) then
    DateError(DateTime);
end;

function TCalendar.TryEncodeDate(Era, Year, Month, Day: Integer;
  out DateTime: TDateTime): Boolean;
var
  JD: Extended;
begin
  Result := False;
  if IsValidDay(Era, Year, Month, Day) then
  begin
    Year := ConvertYear(Year, Era, BaseEra);
    JD := ToJulianDay(Year, Month, Day);
    JD := AdjustJulianDay(JD, True);
    DateTime := JulianDayToDateTime(JD);
    Result := True;
  end;
end;

function TCalendar.TryEncodeDate(Year, Month, Day: Integer;
  out DateTime: TDateTime): Boolean;
begin
  Result := TryEncodeDate(DefaultEra, Year, Month, Day, DateTime);
end;

function TCalendar.EncodeDate(Era, Year, Month, Day: Integer): TDateTime;
begin
  if not TryEncodeDate(Era, Year, Month, Day, Result) then
    DayError(Era, Year, Month, Day);
end;

function TCalendar.EncodeDate(Year, Month, Day: Integer): TDateTime;
begin
  Result := EncodeDate(DefaultEra, Year, Month, Day);
end;

function TCalendar.TryDecodeDate(const DateTime: TDateTime;
  out Era, Year, Month, Day: Integer): Boolean;
var
  JD: Extended;
begin
  JD := DateTimeToJulianDay(DateTime);
  JD := AdjustJulianDay(JD, False);
  Result := FromJulianDay(JD, Year, Month, Day);
  if Result then
  begin
    Era := BestEraOfBaseEraYear(Year);
    if Era <> BaseEra then
      Year := ConvertYear(Year, BaseEra, Era);
  end;
end;

function TCalendar.TryDecodeDate(const DateTime: TDateTime;
  out Year, Month, Day: Integer): Boolean;
var
  JD: Extended;
begin
  JD := DateTimeToJulianDay(DateTime);
  JD := AdjustJulianDay(JD, False);
  Result := FromJulianDay(JD, Year, Month, Day);
  if Result and (DefaultEra <> BaseEra) then
    Year := ConvertYear(Year, BaseEra, DefaultEra);
end;

procedure TCalendar.DecodeDate(const DateTime: TDateTime;
  out Era, Year, Month, Day: Integer);
begin
  if not TryDecodeDate(DateTime, Era, Year, Month, Day) then
    DateError(DateTime);
end;

procedure TCalendar.DecodeDate(const DateTime: TDateTime;
  out Year, Month, Day: Integer);
begin
  if not TryDecodeDate(DateTime, Year, Month, Day) then
    DateError(DateTime);
end;

class function TCalendar.DateTimeToJulianDay(const DateTime: TDateTime): Extended;
begin
  Result := DateTime + DATETIME_EPOCH;
  // We should take in account that in TDateTime type
  // 1. The fraction part is always a positive value even when the whole value is negative
  // 2. There is no value in range (-1, 0)
  if DateTime < 0 then
    Result := Result - 2 * Frac(DateTime);
end;

class function TCalendar.JulianDayToDateTime(const JulianDate: Extended): TDateTime;
var
  F: Double;
begin
  Result := JulianDate - DATETIME_EPOCH;
  // We should take in account that in TDateTime type
  // 1. The fraction part is always a positive value even when the whole value is negative
  // 2. There is no value in range (-1, 0)
  if Result < 0 then
  begin
    F := Frac(Result);
    if F <> 0 then
      Result := Result - 2 * (F + 1);
  end;
end;

class function TCalendar.CombineDateTime(const Date: TDate; const Time: TTime): TDateTime;
begin
  if Date < 0 then
    Result := Date - Time
  else
    Result := Date + Time;
end;

class function TCalendar.LocalToUniversalDateTime(const DateTime: TDateTime): TDateTime;
var
  LocalSystemTime, UniversalSystemTime: TSystemTime;
  LocalFileTime, UniversalFileTime: TFileTime;
begin
  DateTimeToSystemTime(DateTime, LocalSystemTime);
  SystemTimeToFileTime(LocalSystemTime, LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, UniversalFileTime);
  FileTimeToSystemTime(UniversalFileTime, UniversalSystemTime);
  Result := SystemTimeToDateTime(UniversalSystemTime);
end;

class function TCalendar.UniversalToLocalDateTime(const DateTime: TDateTime): TDateTime;
var
  LocalTime, UniversalTime: TSystemTime;
begin
  DateTimeToSystemTime(DateTime, UniversalTime);
  SystemTimeToTzSpecificLocalTime(nil, UniversalTime, LocalTime);
  Result := SystemTimeToDateTime(LocalTime);
end;

class function TCalendar.DateOf(const DateTime: TDateTime): TDate;
begin
  Result := Trunc(DateTime);
end;

function TCalendar.EraOf(const DateTime: TDateTime): Integer;
var
  Year, Month, Day: Integer;
begin
  DecodeDate(DateTime, Result, Year, Month, Day);
end;

function TCalendar.YearOf(const DateTime: TDateTime): Integer;
var
  Month, Day: Integer;
begin
  DecodeDate(DateTime, Result, Month, Day);
end;

function TCalendar.MonthOfYear(const DateTime: TDateTime): Integer;
var
  Year, Day: Integer;
begin
  DecodeDate(DateTime, Year, Result, Day);
end;

function TCalendar.WeekOfYear(const DateTime: TDateTime): Integer;
var
  Year: Integer;
  DoW: TDayOfWeek;
begin
  DecodeDateWeek(DateTime, Year, Result, DoW);
end;

function TCalendar.WeekOfMonth(const DateTime: TDateTime): Integer;
begin
  Result := ((DayOfMonth(DateTime) - 1) div DaysPerWeek) + 1;
end;

function TCalendar.DayOfYear(const DateTime: TDateTime): Integer;
begin
  Result := DaysBetween(DateTime, StartOfYear(DateTime)) + 1;
end;

function TCalendar.DayOfMonth(const DateTime: TDateTime): Integer;
var
  Year, Month: Integer;
begin
  DecodeDate(DateTime, Year, Month, Result);
end;

class function TCalendar.DayOfWeek(const DateTime: TDateTime): TDayOfWeek;
begin
  Result := TDayOfWeek((Trunc(DateTimeToJulianDay(DateTime) + 0.5) mod DaysPerWeek) + 1);
end;

class function TCalendar.TimeOf(const DateTime: TDateTime): TTime;
begin
  Result := Abs(Frac(DateTime));
end;

class function TCalendar.HourOfDay(const DateTime: TDateTime): Integer;
begin
  Result := Trunc(TimeOf(DateTime) * HoursPerDay);
end;

class function TCalendar.MinuteOfDay(const DateTime: TDateTime): Integer;
begin
  Result := Trunc(TimeOf(DateTime) * MinsPerDay);
end;

class function TCalendar.MinuteOfHour(const DateTime: TDateTime): Integer;
begin
  Result := MinuteOfDay(DateTime) mod MinsPerHour;
end;

class function TCalendar.SecondOfDay(const DateTime: TDateTime): Integer;
begin
  Result := Trunc(TimeOf(DateTime) * SecsPerDay);
end;

class function TCalendar.SecondOfMinute(const DateTime: TDateTime): Integer;
begin
  Result := SecondOfDay(DateTime) mod SecsPerMin;
end;

class function TCalendar.MillisecondOfDay(const DateTime: TDateTime): Integer;
begin
  Result := Trunc(TimeOf(DateTime) * MSecsPerDay);
end;

class function TCalendar.MillisecondOfSecond(const DateTime: TDateTime): Integer;
begin
  Result := MillisecondOfDay(DateTime) mod MSecsPerSec;
end;

function TCalendar.StartOfEra(Era: Integer): TDateTime;
begin
  if not IsValidEra(Era) then
    EraError(Era);
  if CanHaveZeroYear(Era) then
    Result := StartOfYear(Era, 0)
  else
    Result := StartOfYear(Era, 1);
end;

function TCalendar.StartOfYear(Era, Year: Integer): TDateTime;
begin
  Result := EncodeDate(Era, Year, FirstMonthOfYear, 1);
end;

function TCalendar.StartOfYear(Year: Integer): TDateTime;
begin
  Result := StartOfYear(DefaultEra, Year);
end;

function TCalendar.StartOfYear(const DateTime: TDateTime): TDateTime;
begin
  Result := StartOfYear(YearOf(DateTime));
end;

function TCalendar.StartOfMonth(Era, Year, Month: Integer): TDateTime;
begin
  Result := EncodeDate(Era, Year, Month, 1);
end;

function TCalendar.StartOfMonth(Year, Month: Integer): TDateTime;
begin
  Result := StartOfMonth(DefaultEra, Year, Month);
end;

function TCalendar.StartOfMonth(const DateTime: TDateTime): TDateTime;
var
  Year, Month, Day: Integer;
begin
  DecodeDate(DateTime, Year, Month, Day);
  Result := DateOf(DateTime) - (Day - 1);
end;

function TCalendar.StartOfWeek(const DateTime: TDateTime): TDateTime;
begin
  Result := DateOf(DateTime) - DaysOfWeekBetween(DayOfWeek(DateTime), Settings.FirstDayOfWeek);
end;

class function TCalendar.StartOfDay(const DateTime: TDateTime): TDateTime;
begin
  Result := DateOf(DateTime);
end;

class function TCalendar.StartOfHour(const DateTime: TDateTime): TDateTime;
begin
  Result := Trunc(DateTime / OneHour) * OneHour;
end;

class function TCalendar.StartOfMinute(const DateTime: TDateTime): TDateTime;
begin
  Result := Trunc(DateTime / OneMinute) * OneMinute;
end;

class function TCalendar.StartOfSecond(const DateTime: TDateTime): TDateTime;
begin
  Result := Trunc(DateTime / OneSecond) * OneSecond;
end;

function TCalendar.EndOfEra(Era: Integer): TDateTime;
begin
  if not IsValidEra(Era) then
    EraError(Era);
  if Era < MaxEra then
    Result := PrevMillisecond(StartOfEra(Era + 1))
  else
    Result := MaxSupportedDateTime;
end;

function TCalendar.EndOfYear(Era, Year: Integer): TDateTime;
var
  NextYear: Integer;
begin
  NextYear := OffsetYear(Era, Year, +1);
  Result := PrevMillisecond(StartOfYear(Era, NextYear));
end;

function TCalendar.EndOfYear(Year: Integer): TDateTime;
begin
  Result := EndOfYear(DefaultEra, Year);
end;

function TCalendar.EndOfYear(const DateTime: TDateTime): TDateTime;
begin
  Result := EndOfYear(YearOf(DateTime));
end;

function TCalendar.EndOfMonth(Era, Year, Month: Integer): TDateTime;
var
  NextMonth: Integer;
begin
  NextMonth := (Month mod MonthsInYear(Era, Year)) + 1;
  if NextMonth = FirstMonthOfYear then
    Result := EndOfYear(Era, Year)
  else
    Result := PrevMillisecond(StartOfMonth(Era, Year, NextMonth));
end;

function TCalendar.EndOfMonth(Year, Month: Integer): TDateTime;
begin
  Result := EndOfMonth(DefaultEra, Year, Month);
end;

function TCalendar.EndOfMonth(const DateTime: TDateTime): TDateTime;
var
  Era, Year, Month, Day: Integer;
begin
  DecodeDate(DateTime, Era, Year, Month, Day);
  Result := EndOfMonth(Era, Year, Month);
end;

function TCalendar.EndOfWeek(const DateTime: TDateTime): TDateTime;
begin
  Result := PrevMillisecond(StartOfWeek(DateTime) + OneWeek);
end;

class function TCalendar.EndOfDay(const DateTime: TDateTime): TDateTime;
begin
  Result := PrevMillisecond(StartOfDay(DateTime) + OneDay);
end;

class function TCalendar.EndOfHour(const DateTime: TDateTime): TDateTime;
begin
  Result := PrevMillisecond(StartOfHour(DateTime) + OneHour);
end;

class function TCalendar.EndOfMinute(const DateTime: TDateTime): TDateTime;
begin
  Result := PrevMillisecond(StartOfMinute(DateTime) + OneMinute);
end;

class function TCalendar.EndOfSecond(const DateTime: TDateTime): TDateTime;
begin
  Result := PrevMillisecond(StartOfSecond(DateTime) + OneSecond);
end;

function TCalendar.PrevYear(const DateTime: TDateTime;
  NumberOfYears: Integer): TDateTime;
begin
  Result := NextYear(DateTime, -NumberOfYears);
end;

function TCalendar.PrevMonth(const DateTime: TDateTime;
  NumberOfMonths: Integer): TDateTime;
begin
  Result := NextMonth(DateTime, -NumberOfMonths)
end;

class function TCalendar.PrevWeek(const DateTime: TDateTime;
  NumberOfWeeks: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, -NumberOfWeeks * OneWeek);
end;

class function TCalendar.PrevDay(const DateTime: TDateTime;
  NumberOfDays: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, -NumberOfDays * OneDay);
end;

function TCalendar.PrevDayOfWeek(const DateTime: TDateTime;
  DoW: TDayOfWeek): TDateTime;
var
  Days: Integer;
begin
  Days := DaysPerWeek - DaysOfWeekBetween(DayOfWeek(DateTime), DoW);
  Result := PrevDay(DateTime, Days);
end;

class function TCalendar.PrevHour(const DateTime: TDateTime;
  NumberOfHours: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, -NumberOfHours * OneHour);
end;

class function TCalendar.PrevMinute(const DateTime: TDateTime;
  NumberOfMinutes: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, -NumberOfMinutes * OneMinute);
end;

class function TCalendar.PrevSecond(const DateTime: TDateTime;
  NumberOfSeconds: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, -NumberOfSeconds * OneSecond);
end;

class function TCalendar.PrevMillisecond(const DateTime: TDateTime;
  NumberOfMilliseconds: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, -NumberOfMilliseconds * OneMillisecond);
end;

function TCalendar.NextYear(const DateTime: TDateTime;
  NumberOfYears: Integer): TDateTime;
var
  Era, Year, Month, Day: Integer;
begin
  if NumberOfYears = 0 then
  begin
    Result := DateTime;
    Exit;
  end;
  DecodeDate(DateTime, Era, Year, Month, Day);
  Year := OffsetYear(Era, Year, NumberOfYears);
  Month := Min(Month, MonthsInYear(Era, Year));
  Day := Min(Day, DaysInMonth(Era, Year, Month));
  Result := CombineDateTime(EncodeDate(Era, Year, Month, Day), Frac(DateTime));
end;

function TCalendar.NextMonth(const DateTime: TDateTime;
  NumberOfMonths: Integer): TDateTime;
var
  Era, Year, Month, Day: Integer;
  MonthsToStartOfYear, MonthsOfThisYear: Integer;
  FirstMonth: Integer;
begin
  if NumberOfMonths = 0 then
  begin
    Result := DateTime;
    Exit;
  end;
  DecodeDate(DateTime, Era, Year, Month, Day);
  MonthsOfThisYear := MonthsInYear(Era, Year);
  FirstMonth := FirstMonthOfYear;
  if NumberOfMonths < 0 then
  begin
    if Month < FirstMonth then
      MonthsToStartOfYear := MonthsOfThisYear - (FirstMonth - Month)
    else
      MonthsToStartOfYear := Month - FirstMonth;
    if -NumberOfMonths > MonthsToStartOfYear then
    begin
      Inc(NumberOfMonths, MonthsToStartOfYear);
      Month := FirstMonth;
      repeat
        Year := OffsetYear(Era, Year, -1);
        MonthsOfThisYear := MonthsInYear(Era, Year);
        Inc(NumberOfMonths, MonthsOfThisYear);
      until NumberOfMonths >= 0;
    end;
  end
  else
  begin
    if Month >= FirstMonth then
      MonthsToStartOfYear := MonthsOfThisYear - (Month - FirstMonth)
    else
      MonthsToStartOfYear := FirstMonth - Month;
    if NumberOfMonths >= MonthsToStartOfYear then
    begin
      Month := FirstMonth;
      repeat
        Dec(NumberOfMonths, MonthsToStartOfYear);
        Year := OffsetYear(Era, Year, +1);
        MonthsToStartOfYear := MonthsInYear(Era, Year);
      until NumberOfMonths < MonthsToStartOfYear;
      MonthsOfThisYear := MonthsToStartOfYear;
    end;
  end;
  Month := ((Month + NumberOfMonths + MonthsOfThisYear - 1) mod MonthsOfThisYear) + 1;
  Day := Min(Day, DaysInMonth(Era, Year, Month));
  Result := CombineDateTime(EncodeDate(Era, Year, Month, Day), Frac(DateTime));
end;

class function TCalendar.NextWeek(const DateTime: TDateTime;
  NumberOfWeeks: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, NumberOfWeeks * OneWeek);
end;

class function TCalendar.NextDay(const DateTime: TDateTime;
  NumberOfDays: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, NumberOfDays * OneDay);
end;

function TCalendar.NextDayOfWeek(const DateTime: TDateTime;
  DoW: TDayOfWeek): TDateTime;
var
  Days: Integer;
begin
  Days := DaysOfWeekBetween(DayOfWeek(DateTime), DoW);
  if Days = 0 then
    Days := DaysPerWeek;
  Result := NextDay(DateTime, Days);
end;

class function TCalendar.NextHour(const DateTime: TDateTime;
  NumberOfHours: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, NumberOfHours * OneHour);
end;

class function TCalendar.NextMinute(const DateTime: TDateTime;
  NumberOfMinutes: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, NumberOfMinutes * OneDay);
end;

class function TCalendar.NextSecond(const DateTime: TDateTime;
  NumberOfSeconds: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, NumberOfSeconds * OneSecond);
end;

class function TCalendar.NextMillisecond(const DateTime: TDateTime;
  NumberOfMilliseconds: Integer): TDateTime;
begin
  Result := OffsetDateTime(DateTime, NumberOfMilliseconds * OneSecond);
end;

procedure TCalendar.IncYear(var DateTime: TDateTime;
  NumberOfYears: Integer);
begin
  DateTime := NextYear(DateTime, NumberOfYears);
end;

procedure TCalendar.IncYear(var Date: TDate;
  NumberOfYears: Integer);
begin
  IncYear(TDateTime(Date), NumberOfYears);
end;

procedure TCalendar.IncMonth(var DateTime: TDateTime;
  NumberOfMonths: Integer);
begin
  DateTime := NextMonth(DateTime, NumberOfMonths);
end;

procedure TCalendar.IncMonth(var Date: TDate;
  NumberOfMonths: Integer);
begin
  IncMonth(TDateTime(Date), NumberOfMonths);
end;

class procedure TCalendar.IncWeek(var DateTime: TDateTime;
  NumberOfWeeks: Integer);
begin
  IncDateTime(DateTime, NumberOfWeeks * OneWeek);
end;

class procedure TCalendar.IncWeek(var Date: TDate;
  NumberOfWeeks: Integer);
begin
  IncWeek(TDateTime(Date), NumberOfWeeks);
end;

class procedure TCalendar.IncDay(var DateTime: TDateTime;
  NumberOfDays: Integer);
begin
  IncDateTime(DateTime, NumberOfDays * OneDay);
end;

class procedure TCalendar.IncDay(var Date: TDate;
  NumberOfDays: Integer);
begin
  IncDay(TDateTime(Date), NumberOfDays);
end;

class procedure TCalendar.IncHour(var DateTime: TDateTime;
  NumberOfHours: Integer);
begin
  IncDateTime(DateTime, NumberOfHours * OneHour);
end;

class procedure TCalendar.IncHour(var Time: TTime;
  NumberOfHours: Integer);
begin
  IncHour(TDateTime(Time), NumberOfHours);
end;

class procedure TCalendar.IncMinute(var DateTime: TDateTime;
  NumberOfMinutes: Integer);
begin
  IncDateTime(DateTime, NumberOfMinutes * OneMinute);
end;

class procedure TCalendar.IncMinute(var Time: TTime;
  NumberOfMinutes: Integer);
begin
  IncMinute(TDateTime(Time), NumberOfMinutes);
end;

class procedure TCalendar.IncSecond(var DateTime: TDateTime;
  NumberOfSeconds: Integer);
begin
  IncDateTime(DateTime, NumberOfSeconds * OneSecond);
end;

class procedure TCalendar.IncSecond(var Time: TTime;
  NumberOfSeconds: Integer);
begin
  IncSecond(TDateTime(Time), NumberOfSeconds);
end;

class procedure TCalendar.IncMillisecond(var DateTime: TDateTime;
  NumberOfMilliseconds: Integer);
begin
  IncDateTime(DateTime, NumberOfMilliseconds * OneMillisecond);
end;

class procedure TCalendar.IncMillisecond(var Time: TTime;
  NumberOfMilliseconds: Integer);
begin
  IncMillisecond(TDateTime(Time), NumberOfMilliseconds);
end;

class procedure TCalendar.IncDateTime(var DateTime: TDateTime; const Delta: Double);
begin
  DateTime := OffsetDateTime(DateTime, Delta);
end;

procedure TCalendar.DecYear(var DateTime: TDateTime;
  NumberOfYears: Integer);
begin
  DateTime := PrevYear(DateTime, NumberOfYears);
end;

procedure TCalendar.DecYear(var Date: TDate;
  NumberOfYears: Integer);
begin
  DecYear(TDateTime(Date), NumberOfYears);
end;

procedure TCalendar.DecMonth(var DateTime: TDateTime;
  NumberOfMonths: Integer);
begin
  DateTime := PrevMonth(DateTime, NumberOfMonths);
end;

procedure TCalendar.DecMonth(var Date: TDate;
  NumberOfMonths: Integer);
begin
  DecMonth(TDateTime(Date), NumberOfMonths);
end;

class procedure TCalendar.DecWeek(var DateTime: TDateTime;
  NumberOfWeeks: Integer);
begin
  DecDateTime(DateTime, NumberOfWeeks * OneWeek);
end;

class procedure TCalendar.DecWeek(var Date: TDate;
  NumberOfWeeks: Integer);
begin
  DecWeek(TDateTime(Date), NumberOfWeeks);
end;

class procedure TCalendar.DecDay(var DateTime: TDateTime;
  NumberOfDays: Integer);
begin
  DecDateTime(DateTime, NumberOfDays * OneDay);
end;

class procedure TCalendar.DecDay(var Date: TDate;
  NumberOfDays: Integer);
begin
  DecDay(TDateTime(Date), NumberOfDays);
end;

class procedure TCalendar.DecHour(var DateTime: TDateTime;
  NumberOfHours: Integer);
begin
  DecDateTime(DateTime, NumberOfHours * OneHour);
end;

class procedure TCalendar.DecHour(var Time: TTime;
  NumberOfHours: Integer);
begin
  DecHour(TDateTime(Time), NumberOfHours);
end;

class procedure TCalendar.DecMinute(var DateTime: TDateTime;
  NumberOfMinutes: Integer);
begin
  DecDateTime(DateTime, NumberOfMinutes * OneMinute);
end;

class procedure TCalendar.DecMinute(var Time: TTime;
  NumberOfMinutes: Integer);
begin
  DecMinute(TDateTime(Time), NumberOfMinutes);
end;

class procedure TCalendar.DecSecond(var DateTime: TDateTime;
  NumberOfSeconds: Integer);
begin
  DecDateTime(DateTime, NumberOfSeconds * OneSecond);
end;

class procedure TCalendar.DecSecond(var Time: TTime;
  NumberOfSeconds: Integer);
begin
  DecSecond(TDateTime(Time), NumberOfSeconds);
end;

class procedure TCalendar.DecMillisecond(var DateTime: TDateTime;
  NumberOfMilliseconds: Integer);
begin
  DecDateTime(DateTime, NumberOfMilliseconds * OneMillisecond);
end;

class procedure TCalendar.DecMillisecond(var Time: TTime;
  NumberOfMilliseconds: Integer);
begin
  DecMillisecond(TDateTime(Time), NumberOfMilliseconds);
end;

class procedure TCalendar.DecDateTime(var DateTime: TDateTime; const Delta: Double);
begin
  DateTime := OffsetDateTime(DateTime, -Delta);
end;

function TCalendar.Format(const FmtStr: String;
  const DateTime: TDateTime): String;
var
  BufPos, AppendLevel: Integer;
  Buffer: array[0..$3FF] of Char;
  Era, Year, Month, Day: Integer;
  Hour, Min, Sec, MSec, H: Word;
  WeekDecoded, DateDecoded, TimeDecoded: Boolean;
  WeekYear, WeekNum: Integer;
  DoW: TDayOfWeek;

  procedure AppendChars(P: PChar; Count: Integer);
  var
    N: Integer;
  begin
    N := (SizeOf(Buffer) div SizeOf(Char)) - BufPos;
    if N > Count then
      N := Count;
    if N <> 0 then
    begin
      Move(P^, Buffer[BufPos], N * SizeOf(Char));
      Inc(BufPos, N);
    end;
  end;

  procedure AppendString(const S: String); inline;
  begin
    AppendChars(PChar(S), Length(S));
  end;

  procedure AppendNumber(Number, Digits: Integer); inline;
  begin
    AppendString(SysUtils.Format('%.*d', [Digits, Number]));
  end;

  procedure AppendFormat(Fmt: PChar; TranslateSeparators: Boolean = False);
  var
    Starter, Token, LastToken: Char;
    Use12HourClock: Boolean;
    BetweenQuotes: Boolean;
    Count: Integer;
    P: PChar;

    procedure GetCount;
    var
      P: PChar;
    begin
      P := Fmt;
      while Fmt^ = Starter do
        Inc(Fmt);
      Count := Fmt - P + 1;
    end;

    procedure GetDateWeek;
    begin
      if not WeekDecoded then
      begin
        DecodeDateWeek(DateTime, WeekYear, WeekNum, DoW);
        WeekDecoded := True;
      end;
    end;

    procedure GetDate;
    begin
      if not DateDecoded then
      begin
        DecodeDate(DateTime, Era, Year, Month, Day);
        DateDecoded := True;
      end;
    end;

    procedure GetTime;
    begin
      if not TimeDecoded then
      begin
        DecodeTime(DateTime, Hour, Min, Sec, MSec);
        TimeDecoded := True;
      end;
    end;

  begin
    if (Fmt <> nil) and (AppendLevel < 2) then
    begin
      Inc(AppendLevel);
      LastToken := ' ';
      Use12HourClock := False;
      while Fmt^ <> #0 do
      begin
        Starter := Fmt^;
        Inc(Fmt);
        Token := UpCase(Starter);
        if CharInSet(Token, ['A'..'Z']) then
        begin
          if (Token = 'M') and (LastToken = 'H') then
            Token := 'N'
          else if Token = 'E' then
            Token := 'Y';
          LastToken := Token;
        end;
        case Token of
          'G':
          begin
            GetCount;
            GetDate;
            if Count = 1 then
              AppendString(Settings.ShortEraNames[Era])
            else
              AppendString(Settings.EraNames[Era]);
          end;
          'Y':
          begin
            GetCount;
            GetDate;
            case Count of
              1: AppendNumber(Year, 1);
              2: AppendNumber(Year mod 100, 2);
            else
              AppendNumber(Year, 4);
            end;
          end;
          'M':
          begin
            GetCount;
            GetDate;
            case Count of
              1, 2: AppendNumber(Month, Count);
              3: AppendString(Settings.ShortMonthNames[Month]);
            else
              if (LastToken = 'Y') or (LastToken = 'D') then
                AppendString(Settings.GenitiveMonthNames[Month])
              else
                AppendString(Settings.MonthNames[Month]);
            end;
          end;
          'D':
          begin
            GetCount;
            if Count <= 2 then
              GetDate;
            case Count of
              1, 2: AppendNumber(Day, Count);
              3: AppendString(Settings.ShortDayNames[DayOfWeek(DateTime)]);
              4: AppendString(Settings.DayNames[DayOfWeek(DateTime)]);
              5: AppendFormat(PChar(Settings.ShortDateFormat));
            else
              AppendFormat(PChar(Settings.LongDateFormat));
            end;
          end;
          'I':
          begin
            GetCount;
            GetDate;
            AppendNumber(DayOfYear(DateTime), Count);
          end;
          'O':
          begin
            GetCount;
            AppendNumber(DayOfWeekToDayNum(DayOfWeek(DateTime)), 1);
          end;
          'W':
          begin
            GetCount;
            GetDateWeek;
            AppendNumber(WeekNum, Count)
          end;
          'R':
          begin
            GetCount;
            GetDateWeek;
            case Count of
              1: AppendNumber(WeekYear, 1);
              2: AppendNumber(WeekYear mod 100, 2);
            else
              AppendNumber(WeekYear, 4);
            end;
          end;
          'H':
          begin
            GetCount;
            GetTime;
            BetweenQuotes := False;
            P := Fmt;
            while P^ <> #0 do
            begin
              case P^ of
                'A', 'a':
                  if not BetweenQuotes then
                  begin
                    if ((AnsiStrLIComp(P, 'AM/PM', 5) = 0) or
                        (AnsiStrLIComp(P, 'A/P', 3) = 0) or
                        (AnsiStrLIComp(P, 'AMPM', 4) = 0))
                    then
                      Use12HourClock := True;
                    Break;
                  end;
                'H', 'h':
                  Break;
                '''', '"':
                  BetweenQuotes := not BetweenQuotes;
              end;
              Inc(P);
            end;
            H := Hour;
            if Use12HourClock then
              if H = 0 then
                H := 12
              else if H > 12 then
                Dec(H, 12);
            if Count > 2 then
              Count := 2;
            AppendNumber(H, Count);
          end;
          'N':
          begin
            GetCount;
            GetTime;
            if Count > 2 then
              Count := 2;
            AppendNumber(Min, Count);
          end;
          'S':
          begin
            GetCount;
            GetTime;
            if Count > 2 then
              Count := 2;
            AppendNumber(Sec, Count);
          end;
          'Z':
          begin
            GetCount;
            GetTime;
            if Count > 3 then
              Count := 3;
            AppendNumber(MSec, Count);
          end;
          'A':
          begin
            GetTime;
            P := Fmt - 1;
            if AnsiStrLIComp(P, 'AM/PM', 5) = 0 then
            begin
              if Hour >= 12 then
                Inc(P, 3);
              AppendChars(P, 2);
              Inc(Fmt, 4);
              Use12HourClock := True;
            end
            else if AnsiStrLIComp(P, 'A/P', 3) = 0 then
            begin
              if Hour >= 12 then
                Inc(P, 2);
              AppendChars(P, 1);
              Inc(Fmt, 2);
              Use12HourClock := True;
            end
            else if AnsiStrLIComp(P, 'AMPM', 4) = 0 then
            begin
              if Hour < 12 then
                AppendString(Settings.TimeAMString)
              else
                AppendString(Settings.TimePMString);
              Inc(Fmt, 3);
              Use12HourClock := True;
            end
            else
              AppendChars(@Starter, 1);
          end;
          'C':
          begin
            GetCount;
            AppendFormat(PChar(Settings.ShortDateFormat));
            GetTime;
            if (Hour <> 0) or (Min <> 0) or (Sec <> 0) then
            begin
              AppendChars(' ', 1);
              AppendFormat(PChar(Settings.LongTimeFormat));
            end;
          end;
          'T':
          begin
            GetCount;
            if Count = 1 then
              AppendFormat(PChar(Settings.ShortTimeFormat))
            else
              AppendFormat(PChar(Settings.LongTimeFormat));
          end;
          'Q':
          begin
            GetCount;
            AppendFormat('YYYY-MM-DD HH:NN:SS');
          end;
          'U':
          begin
            GetCount;
            AppendString(Format('Q', LocalToUniversalDateTime(DateTime)));
          end;
          '/':
          begin
            if TranslateSeparators then
              AppendString(Settings.DateSeparator)
            else
              AppendChars(@Starter, 1);
          end;
          ':':
          begin
            if TranslateSeparators then
              AppendString(Settings.TimeSeparator)
            else
              AppendChars(@Starter, 1);
          end;
          '"', '''':
          begin
            P := Fmt;
            while (Fmt^ <> #0) and (Fmt^ <> Starter) do
              Inc(Fmt);
            AppendChars(P, Fmt - P);
            if Fmt^ <> #0 then
              Inc(Fmt);
          end;
        else
          AppendChars(@Starter, 1);
        end;
      end;
      Dec(AppendLevel);
    end;
  end;

begin
  if IsNoDate(DateTime) then
    Result := ''
  else
  begin
    BufPos := 0;
    AppendLevel := 0;
    WeekDecoded := False;
    DateDecoded := False;
    TimeDecoded := False;
    if FmtStr <> '' then
      AppendFormat(PChar(FmtStr), True)
    else
      AppendFormat('C');
    SetString(Result, Buffer, BufPos);
  end;
end;

function TCalendar.Parse(const FmtStr: String;
  const DateTimeStr: String): TDateTime;
var
  Era, Year, Month, Day: Integer;
  Hour, Min, Sec, MSec: Word;
  WeekYear, WeekNum, DoW, DoY: Integer;
  WeekScanned, DateScanned, TimeScanned: Boolean;
  Is12Hours, IsHourPM: Boolean;
  IsUTC: Boolean;
  ScanLevel: Integer;
  S: PChar;

  function ScanNumber: Integer;
  var
    Negate: Boolean;
  begin
    Result := 0;
    if (S^ = UCC_NODS) or (S^ = UCC_NADS) then
      Inc(S);
    Negate := (S^ = '-');
    if Negate then
      Inc(S);
    while CharInSet(S^, ['0'..'9']) do
    begin
      Result := Result * 10 + (Ord(S^) - Ord('0'));
      Inc(S);
    end;
    if Negate then
      Result := -Result;
  end;

  function ScanString(Terminator: Char): String;
  var
    Start: PChar;
  begin
    Start := S;
    while (S^ <> #0) and (S^ <> Terminator) do
      Inc(S);
    SetString(Result, Start, S - Start);
  end;

  procedure ScanFormat(Fmt: PChar; TranslateSeparators: Boolean = False);
  var
    Starter, Token, LastToken: Char;
    Count: Integer;
    P: PChar;

    procedure GetCount;
    var
      P: PChar;
    begin
      P := Fmt;
      while Fmt^ = Starter do
        Inc(Fmt);
      Count := Fmt - P + 1;
    end;

  begin
    if (Fmt <> nil) and (ScanLevel < 2) then
    begin
      Inc(ScanLevel);
      LastToken := ' ';
      while (Fmt^ <> #0) and (S^ <> #0) do
      begin
        Starter := Fmt^;
        Inc(Fmt);
        Token := UpCase(Starter);
        if CharInSet(Token, ['A'..'Z']) then
        begin
          if (Token = 'M') and (LastToken = 'H') then
            Token := 'N';
          if Token = 'E' then
            Token := 'Y';
          LastToken := Token;
        end;
        case Token of
          'G':
          begin
            GetCount;
            if Count = 1 then
              Era := Settings.ScanShortEraNames(S)
            else
              Era := Settings.ScanEraNames(S)
          end;
          'Y':
          begin
            GetCount;
            Year := ScanNumber;
            if Count <= 2 then
              Year := ToFourDigitYear(Year);
            DateScanned := True;
            if not WeekScanned then
              WeekYear := Year;
          end;
          'M':
          begin
            GetCount;
            case Count of
              1, 2: Month := ScanNumber;
              3: Month := Settings.ScanShortMonthNames(S);
            else
              Month := Settings.ScanMonthNames(S);
              if Month = 0 then
                Month := Settings.ScanGenitiveMonthNames(S);
            end;
            DateScanned := True;
          end;
          'D':
          begin
            GetCount;
            case Count of
              1, 2:
              begin
                Day := ScanNumber;
                DateScanned := True;
              end;
              3: DoW := Settings.ScanShortDayNames(S);
              4: DoW := Settings.ScanDayNames(S);
              5: ScanFormat(PChar(Settings.ShortDateFormat));
            else
              ScanFormat(PChar(Settings.LongDateFormat));
            end;
          end;
          'I':
          begin
            GetCount;
            DoY := ScanNumber;
            DateScanned := True;
          end;
          'O':
          begin
            GetCount;
            DoW := Ord(DayNumToDayOfWeek(ScanNumber));
            WeekScanned := True;
          end;
          'W':
          begin
            GetCount;
            WeekNum := ScanNumber;
            WeekScanned := True;
          end;
          'R':
          begin
            GetCount;
            WeekYear := ScanNumber;
            if Count <= 2 then
              WeekYear := ToFourDigitYear(WeekYear);
            WeekScanned := True;
          end;
          'H':
          begin
            GetCount;
            Hour := ScanNumber;
            TimeScanned := True;
          end;
          'N':
          begin
            GetCount;
            Min := ScanNumber;
            TimeScanned := True;
          end;
          'S':
          begin
            GetCount;
            Sec := ScanNumber;
            TimeScanned := True;
          end;
          'Z':
          begin
            GetCount;
            MSec := ScanNumber;
            TimeScanned := True;
          end;
          'A':
          begin
            P := Fmt - 1;
            if AnsiStrLIComp(P, 'AM/PM', 5) = 0 then
            begin
              Inc(Fmt, 4);
              Is12Hours := True;
              IsHourPM := (AnsiStrLIComp(S, 'PM', 2) = 0);
              Inc(S, 2);
            end
            else if AnsiStrLIComp(P, 'A/P', 3) = 0 then
            begin
              Inc(Fmt, 2);
              Is12Hours := True;
              IsHourPM := (AnsiStrLIComp(S, 'P', 1) = 0);
              Inc(S, 1);
            end
            else if AnsiStrLIComp(P, 'AMPM', 4) = 0 then
            begin
              Inc(Fmt, 3);
              Is12Hours := True;
              IsHourPM := (Settings.ScanAMPM(S) = 2);
            end;
          end;
          'C':
          begin
            GetCount;
            ScanFormat(PChar(Settings.ShortDateFormat + ' ' + Settings.LongTimeFormat));
          end;
          'T':
          begin
            GetCount;
            if Count = 1 then
              ScanFormat(PChar(Settings.ShortTimeFormat))
            else
              ScanFormat(PChar(Settings.LongTimeFormat));
          end;
          'Q', 'U':
          begin
            GetCount;
            ScanFormat('YYYY-MM-DD HH:NN:SS');
            IsUTC := (Token = 'U');
          end;
          '/':
          begin
            if TranslateSeparators then
            begin
              P := PChar(Settings.DateSeparator);
              while (P^ <> #0) and (S^ <> #0) do
              begin
                Inc(P);
                Inc(S);
              end;
            end
            else
              Inc(S);
          end;
          ':':
          begin
            if TranslateSeparators then
            begin
              P := PChar(Settings.TimeSeparator);
              while (P^ <> #0) and (S^ <> #0) do
              begin
                Inc(P);
                Inc(S);
              end;
            end
            else
              Inc(S);
          end;
          '"', '''':
          begin
            while (Fmt^ <> #0) and (Fmt^ <> Starter) and (S^ <> #0) do
            begin
              Inc(Fmt);
              Inc(S);
            end;
            if Fmt^ <> #0 then
              Inc(Fmt);
          end;
        else
          Inc(S);
        end;
      end;
      Dec(ScanLevel);
    end;
  end;

var
  Date: TDate;
  Time: TTime;
begin
  Era := DefaultEra; Year := 0; Month := 0; Day := 0;
  Hour := 0; Min := 0; Sec := 0; MSec := 0;
  WeekYear := 0; WeekNum := 0; DoW := 0;
  WeekScanned := False;
  DateScanned := False;
  TimeScanned := False;
  Is12Hours := False;
  IsHourPM := False;
  IsUTC := False;
  S := PChar(DateTimeStr);
  ScanLevel := 0;
  if FmtStr <> '' then
    ScanFormat(PChar(FmtStr), True)
  else
    ScanFormat('C');
  if TimeScanned then
  begin
    if Is12Hours then
    begin
      if IsHourPM and (Hour < 12) then
        Hour := (Hour + 12) mod 24
      else if not IsHourPM and (Hour = 12) then
        Hour := 0;
    end;
    Time := EncodeTime(Hour, Min, Sec, MSec);
  end
  else
    Time := 0;
  if DateScanned then
  begin
    if (DoY <> 0) and ((Month = 0) or (Day = 0)) then
      Date := EncodeDateDay(Era, Year, DoY)
    else
      Date := EncodeDate(Era, Year, Month, Day);
  end
  else if WeekScanned then
    Date := EncodeDateWeek(Era, WeekYear, WeekNum, TDayOfWeek(DoW))
  else
    Date := 0;
  Result := CombineDateTime(Date, Time);
  if IsUTC then
    Result := UniversalToLocalDateTime(Result);
end;

function TCalendar.TryParse(const FmtStr: String; const DateTimeStr: String;
  var DateTime: TDateTime): Boolean;
begin
  try
    DateTime := Parse(FmtStr, DateTimeStr);
    Result := True;
  except
    Result := False;
  end;
end;

{ TCalendarClassList }

function TCalendarClassList.GetItems(Index: Integer): TCalendarClass;
begin
  Result := TCalendarClass(Get(Index));
end;

procedure TCalendarClassList.SetItems(Index: Integer; Value: TCalendarClass);
begin
  Put(Index, Value);
end;

function TCalendarClassList.First: TCalendarClass;
begin
  Result := TCalendarClass(inherited First);
end;

function TCalendarClassList.Last: TCalendarClass;
begin
  Result := TCalendarClass(inherited Last);
end;

function TCalendarClassList.Add(Item: TCalendarClass): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TCalendarClassList.Insert(Index: Integer; Item: TCalendarClass);
begin
  inherited Insert(Index, Item);
end;

function TCalendarClassList.Remove(Item: TCalendarClass): Integer;
begin
  Result := inherited Remove(Item);
end;

{$IFDEF COMPILER2010_UP}
function TCalendarClassList.RemoveItem(Item: TCalendarClass;
  Direction: TList.TDirection): Integer;
begin
  Result := inherited RemoveItem(Item, Direction);
end;
{$ENDIF}

function TCalendarClassList.Extract(Item: TCalendarClass): TCalendarClass;
begin
  Result := TCalendarClass(inherited Extract(Item));
end;

{$IFDEF COMPILER2010_UP}
function TCalendarClassList.ExtractItem(Item: TCalendarClass;
  Direction: TList.TDirection): TCalendarClass;
begin
  Result := TCalendarClass(inherited ExtractItem(Item, Direction));
end;
{$ENDIF}

function TCalendarClassList.IndexOf(Item: TCalendarClass): Integer;
begin
  Result := inherited IndexOf(Item);
end;

{$IFDEF COMPILER2010_UP}
function TCalendarClassList.IndexOfItem(Item: TCalendarClass;
  Direction: TList.TDirection): Integer;
begin
  Result := inherited IndexOfItem(Item, Direction);
end;
{$ENDIF}

{ CalendarTypes }

class procedure CalendarTypes.Cleanup;
begin
  if Assigned(CalendarList) then
    FreeAndNil(CalendarList);
end;

class procedure CalendarTypes.Register(CalendarClass: TCalendarClass);
var
  Index: Integer;
  RegInfo: TCalendarRegInfo;
begin
  if not Assigned(CalendarList) then
  begin
    CalendarList := TStringList.Create;
    CalendarList.Sorted := True;
    CalendarList.CaseSensitive := False;
    CalendarList.Duplicates := dupIgnore;
    CalendarList.OwnsObjects := True;
  end;
  Index := CalendarList.Add(CalendarClass.CalendarName);
  RegInfo := TCalendarRegInfo(CalendarList.Objects[Index]);
  if not Assigned(RegInfo) then
  begin
    RegInfo := TCalendarRegInfo.Create(CalendarClass);
    CalendarList.Objects[Index] := RegInfo;
  end;
end;

class procedure CalendarTypes.Register(CalendarClass: TCalendarClass;
  const Locales: array of String);
var
  Index: Integer;
begin
  Register(CalendarClass);
  Index := CalendarList.IndexOf(CalendarClass.CalendarName);
  TCalendarRegInfo(CalendarList.Objects[Index]).Associate(Locales);
end;

class procedure CalendarTypes.Unregister(CalendarClass: TCalendarClass);
var
  Index: Integer;
begin
  if Assigned(CalendarList) then
  begin
    Index := CalendarList.IndexOf(CalendarClass.CalendarName);
    if Index >= 0 then
      CalendarList.Delete(Index);
  end;
end;

class procedure CalendarTypes.Unregister(CalendarClass: TCalendarClass;
  const Locales: array of String);
var
  Index: Integer;
  RegInfo: TCalendarRegInfo;
begin
  if Assigned(CalendarList) then
  begin
    Index := CalendarList.IndexOf(CalendarClass.CalendarName);
    if Index >= 0 then
    begin
      RegInfo := TCalendarRegInfo(CalendarList.Objects[Index]);
      RegInfo.Dissociate(Locales);
    end;
  end;
end;

class function CalendarTypes.MinSupportedDateTime: TDateTime;
var
  I: Integer;
  CalendarClass: TCalendarClass;
begin
  Result := SysUtils.MinDateTime;
  if Assigned(CalendarList) then
    for I := CalendarList.Count - 1 downto 0 do
    begin
      CalendarClass := TCalendarRegInfo(CalendarList.Objects[I]).CalendarClass;
      if TCalendar.CompareDate(CalendarClass.MinSupportedDateTime, Result) > 0 then
        Result := CalendarClass.MinSupportedDateTime;
    end;
end;

class function CalendarTypes.MaxSupportedDateTime: TDateTime;
var
  I: Integer;
  CalendarClass: TCalendarClass;
begin
  Result := SysUtils.MaxDateTime;
  if Assigned(CalendarList) then
    for I := CalendarList.Count - 1 downto 0 do
    begin
      CalendarClass := TCalendarRegInfo(CalendarList.Objects[I]).CalendarClass;
      if TCalendar.CompareDate(CalendarClass.MaxSupportedDateTime, Result) < 0 then
        Result := CalendarClass.MaxSupportedDateTime;
    end;
end;

class function CalendarTypes.Count: Integer;
begin
  if Assigned(CalendarList) then
    Result := CalendarList.Count
  else
    Result := 0;
end;

class function CalendarTypes.ByIndex(Index: Integer): TCalendarClass;
begin
  if Assigned(CalendarList) and (Index >= 0) and (Index < CalendarList.Count) then
    Result := TCalendarRegInfo(CalendarList.Objects[Index]).CalendarClass
  else
    Result := nil;
end;

class function CalendarTypes.ByID(ID: Cardinal): TCalendarClass;
var
  I: Integer;
begin
  if Assigned(CalendarList) then
    for I := CalendarList.Count - 1 downto 0 do
    begin
      Result := TCalendarRegInfo(CalendarList.Objects[I]).CalendarClass;
      if Result.CalendarID = ID then
        Exit;
    end;
  Result := nil;
end;

class function CalendarTypes.ByName(const Name: String): TCalendarClass;
var
  I: Integer;
begin
  if Assigned(CalendarList) and CalendarList.Find(Name, I) then
    Result := TCalendarRegInfo(CalendarList.Objects[I]).CalendarClass
  else
    Result := nil;
end;

class function CalendarTypes.ByLocale(const Locale: String): TCalendarClass;
var
  CalendarID: Cardinal;
  Index, I: Integer;
  RegInfo: TCalendarRegInfo;
begin
  CalendarID := GetLocaleInt(Locale, LOCALE_ICALENDARTYPE, CAL_GREGORIAN);
  Result := ByID(CalendarID);
  for Index := 0 to Count - 1 do
  begin
    RegInfo := TCalendarRegInfo(CalendarList.Objects[Index]);
    if Assigned(RegInfo.AssociatedLocales) then
    begin
      for I := 0 to RegInfo.AssociatedLocales.Count - 1 do
        if SameLanguage(Locale, RegInfo.AssociatedLocales[I]) then
        begin
          Result := RegInfo.CalendarClass;
          if SameText(Locale, RegInfo.AssociatedLocales[I]) then
            Exit;
        end;
    end;
  end;
end;

class procedure CalendarTypes.GetCalendersOfLocale(const Locale: String;
  CalendarClasses: TCalendarClassList);
var
  Index, I: Integer;
  RegInfo: TCalendarRegInfo;
begin
  for Index := 0 to Count - 1 do
  begin
    RegInfo := TCalendarRegInfo(CalendarList.Objects[Index]);
    if not Assigned(RegInfo.AssociatedLocales) then
      CalendarClasses.Add(RegInfo.CalendarClass)
    else
    begin
      for I := 0 to RegInfo.AssociatedLocales.Count - 1 do
        if SameLanguage(Locale, RegInfo.AssociatedLocales[I]) then
        begin
          CalendarClasses.Add(RegInfo.CalendarClass);
          Break;
        end;
    end;
  end;
end;

{ CalendarTypes.TCalendarRegInfo }

constructor CalendarTypes.TCalendarRegInfo.Create(ACalendarClass: TCalendarClass);
begin
  fCalendarClass := ACalendarClass;
end;

destructor CalendarTypes.TCalendarRegInfo.Destroy;
begin
  if Assigned(fLocales) then
    fLocales.Free;
  inherited Destroy;
end;

procedure CalendarTypes.TCalendarRegInfo.Associate(const Locales: array of String);
var
  I: Integer;
begin
  if Length(Locales) = 0 then
    Exit;
  if not Assigned(fLocales) then
  begin
    fLocales := TStringList.Create;
    fLocales.Sorted := True;
    fLocales.CaseSensitive := False;
    fLocales.Duplicates := dupIgnore;
  end;
  for I := Low(Locales) to High(Locales) do
    fLocales.Add(Locales[I]);
end;

procedure CalendarTypes.TCalendarRegInfo.Dissociate(const Locales: array of String);
var
  I, Index: Integer;
begin
  if Assigned(fLocales) then
    for I := Low(Locales) to High(Locales) do
      if fLocales.Find(Locales[I], Index) then
        fLocales.Delete(Index);
end;

initialization

finalization
  CalendarTypes.Cleanup;
end.



