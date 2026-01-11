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
/// This unit implements the Jalali (or Persian) calendar.
/// </summary>
unit i18nCalJalali;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, i18nCalendar;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This class implements the Jalali (or Persian) calendar.
  /// </summary>
  /// <remarks>
  /// <para>
  /// TJalaliCalendar provides properties and methods to manipulate dates in
  /// the Jalali (or Persian) calendar.
  /// </para>
  /// <para>
  /// The Jalali calendar consists of 12 months: the first six are 31 days,
  /// the next five are 30 days, and the final month is 29 days in a common year
  /// and 30 days in a leap year.
  /// </para>
  /// <para>
  /// In the astronomical definition of the Jalali calendar, each year begins on
  /// Nowruz, determined by the vernal equinox. Leap years do not recur in a fixed
  /// arithmetic pattern; a year is a leap year when 366 days elapse between
  /// consecutive Nowruz days.
  /// </para>
  /// <para>
  /// This implementation uses astronomical calculations to determine Nowruz dates
  /// based on the vernal equinox time in Iran Standard Time.
  /// </para>
  /// <para>
  /// The Jalali calendar supports two eras:
  /// <list type="bullet">
  ///   <item>
  ///     <term>Imperial Era</term>
  ///     <description>
  ///       The era starting with the coronation of Cyrus the Great (year 1 = 559 BCE).
  ///     </description>
  ///   </item>
  ///   <item>
  ///     <term>Hijri Era</term>
  ///     <description>
  ///       The era starting with the Hijra of Prophet Muhammad (year 1 = 622 CE).
  ///     </description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TJalaliCalendar = class(TCalendar)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// The start of the Jalali calendar in Julian days (Imperial year 1).
    /// </summary>
    {$endregion}
    const JALALI_EPOCH = 1517699.5;
    {$region 'xmldoc'}
    /// <summary>
    /// The year offset between Hijri and Imperial eras in the Jalali calendar.
    /// </summary>
    {$endregion}
    const HIJRI_YEAR_OFFSET = 1180;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a Jalali date represented by its year, month and day
    /// components to its corresponding Julian day.
    /// </summary>
    /// <param name="Year">
    /// The year.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Julian day of the specified Jalali date.
    /// </returns>
    /// <seealso cref="FromJulianDay"/>
    {$endregion}
    function ToJulianDay(Year, Month, Day: Integer): Extended; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a date expressed in Julian day, to its year, month, and day
    /// components in the Jalali calendar.
    /// </summary>
    /// <param name="JD">
    /// The date expressed in Julian day.
    /// </param>
    /// <param name="Year">
    /// The year.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeds, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="ToJulianDay"/>
    {$endregion}
    function FromJulianDay(const JD: Extended; out Year, Month, Day: Integer): Boolean; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies the index of the Imperial era.
    /// </summary>
    {$endregion}
    const ImperialEra = 1;
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies the index of the Hijri era.
    /// </summary>
    {$endregion}
    const HijriEra = 2;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of eras that the calendar class supports.
    /// </summary>
    /// <returns>
    /// Returns 2.
    /// </returns>
    {$endregion}
    class function MaxEra: Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the unique identifier of the calendar.
    /// </summary>
    /// <returns>
    /// Returns <see cref="CAL_PERSIAN"/>.
    /// </returns>
    {$endregion}
    class function CalendarID: Cardinal; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the algorithm type of the calendar system.
    /// </summary>
    /// <returns>
    /// Returns <see cref="TCalendarKind.ckSolar"/>.
    /// </returns>
    {$endregion}
    class function CalendarKind: TCalendarKind; override;
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
    class function MinSupportedDateTime: TDateTime; override;
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
    class function MaxSupportedDateTime: TDateTime; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TCalendarSettings"/> class that provides locale
    /// specific settings for the calendar.
    /// </summary>
    /// <returns>
    /// Returns <see cref="TJalaliCalendarSettings"/> class.
    /// </returns>
    {$endregion}
    class function SettingsClass: TCalendarSettingsClass; override;
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
    function ConvertYear(Year, FromEra, ToEra: Integer): Integer; override;
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
    function IsValidYear(Era, Year: Integer): Boolean; override;
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
    function IsLeapYear(Era, Year: Integer): Boolean; override;
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
    /// Returns 365 for normal and 366 for leap years.
    /// </returns>
    {$endregion}
    function DaysInYear(Era, Year: Integer): Integer; override;
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
    function DaysInMonth(Era, Year, Month: Integer): Integer; override;
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
    function DaysToMonth(Era, Year, Month: Integer): Integer; override;
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
    /// <param name="YearDay">
    /// The day of the year.
    /// </param>
    /// <param name="Month">
    /// The month of the year.
    /// </param>
    /// <param name="Day">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion succeeds, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DayOfYearToDayOfMonth(Era, Year, YearDay: Integer; var Month, Day: Integer): Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the date and time of the vernal equinox for a specified era and year
    /// in Iran Standard Time.
    /// </summary>
    /// <param name="Era">
    /// The era.
    /// </param>
    /// <param name="Year">
    /// The year in the Jalali calendar.
    /// </param>
    /// <returns>
    /// The date and time of the vernal equinox in <see cref="TDateTime"/> format.
    /// </returns>
    /// <remarks>
    /// The returned <see cref="TDateTime"/> value is expressed in Tehran civil time
    /// (UTC+03:30). The astronomical algorithm provides sub-minute accuracy for
    /// equinox times across the supported date range.
    /// </remarks>
    {$endregion}
    function VernalEquinox(Era, Year: Integer): TDateTime; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the date and time of the vernal equinox for a specified year in
    /// Iran Standard Time.
    /// </summary>
    /// <param name="Year">
    /// The year in the Jalali calendar.
    /// </param>
    /// <returns>
    /// The date and time of the vernal equinox in <see cref="TDateTime"/> format.
    /// </returns>
    /// <remarks>
    /// The returned <see cref="TDateTime"/> value is expressed in Tehran civil time
    /// (UTC+03:30). The astronomical algorithm provides sub-minute accuracy for
    /// equinox times across the supported date range.
    /// </remarks>
    /// <seealso cref="DefaultEra"/>
    {$endregion}
    function VernalEquinox(Year: Integer): TDateTime; overload; inline;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class provides locale specific settings for the <see cref="TJalaliCalendar"/>
  /// class.
  /// </summary>
  /// <remarks>
  /// TJalaliCalendarSettings class collects the Jalali calendar's locale specific
  /// settings, which are required by the <see cref="TJalaliCalendar"/> class.
  /// </remarks>
  {$endregion}
  TJalaliCalendarSettings = class(TCalendarSettings)
  protected
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
    /// The identifier of the calendar system.
    /// </param>
    {$endregion}
    procedure PrepareEraName(Era: Integer; const Locale: string; CalendarID: Cardinal); override;
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
    /// The identifier of the calendar system.
    /// </param>
    {$endregion}
    procedure PrepareMonthNames(const Locale: string; CalendarID: Cardinal); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets all writable properties based on the given locale and calendar
    /// identifier.
    /// </summary>
    /// <param name="Locale">
    /// The locale of the settings.
    /// </param>
    /// <param name="CalendarID">
    /// The identifier of the calendar system.
    /// </param>
    {$endregion}
    procedure PrepareSettings(const Locale: String; CalendarID: Cardinal); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that this settings is provided for.
    /// </summary>
    /// <returns>
    /// Returns the <see cref="TJalaliCalendar"/> class.
    /// </returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; override;
  end;

implementation

uses
  Math, Types, i18nWinNLS, i18nCore, i18nAstronomy;

const
  // Year offset between Jalali and Gregorian calendars
  JALALI_TO_GREGORIAN_YEAR_OFFSET = -559;
  // Jalali calendar year range
  JALALI_MIN_YEAR = MEEUS_MIN_YEAR - JALALI_TO_GREGORIAN_YEAR_OFFSET;
  JALALI_MAX_YEAR = MEEUS_MAX_YEAR - JALALI_TO_GREGORIAN_YEAR_OFFSET;
  // Iran Standard Time offset from UTC in days (+03:30)
  IRAN_UTC_OFFSET_DAYS = 3.5 / 24.0;

var
  // Computed Nowruz Julian Day Number for each Jalali year.
  // Zero indicates that the value is not yet computed.
  NowruzCache: array[JALALI_MIN_YEAR..JALALI_MAX_YEAR] of Integer;

// Calculates the Julian Day Number for Nowruz (the start of the Jalali year)
// for a given zero-based Jalali year.
function NowruzJDN(Jy: Integer): Integer;
var
  EquinoxJD_UT: Extended;
  EquinoxJD_Tehran: Extended;
  TehranMidnightJD: Extended;
  TehranCivilDay: Integer;
  FractionOfDay: Extended;
begin
  if (Jy < JALALI_MIN_YEAR) or (Jy > JALALI_MAX_YEAR) then
  begin
    Result := 0;
    Exit;
  end;

  Result := NowruzCache[Jy];
  if Result <> 0 then Exit;

  // Astronomical equinox instant (noon-based)
  EquinoxJD_UT := VernalEquinoxJulianDay(Jy + JALALI_TO_GREGORIAN_YEAR_OFFSET);

  // Convert equinox instant to Tehran local time (still noon-based)
  EquinoxJD_Tehran := EquinoxJD_UT + IRAN_UTC_OFFSET_DAYS;

  // Shift to midnight-based civil day numbering
  TehranMidnightJD := EquinoxJD_Tehran + 0.5;

  // Extract Tehran civil day and time within that day
  TehranCivilDay := Trunc(TehranMidnightJD);
  FractionOfDay  := TehranMidnightJD - TehranCivilDay;

  // Nowruz rule:
  //   before noon  -> same civil day
  //   noon or after -> next civil day
  if FractionOfDay < 0.5 then
    Result := TehranCivilDay
  else
    Result := TehranCivilDay + 1;

  NowruzCache[Jy] := Result;
end;

// Finds the Jalali year for a given Julian Day Number.
function FindJalaliYear(JDN: Integer): Integer;
var
  Lo, Hi, Mid: Integer;
begin
  Lo := JALALI_MIN_YEAR;
  Hi := JALALI_MAX_YEAR;

  while Lo < Hi do
  begin
    Mid := (Lo + Hi + 1) div 2;
    if NowruzJDN(Mid) <= JDN then
      Lo := Mid
    else
      Hi := Mid - 1;
  end;

  Result := Lo;
end;

{ TJalaliCalendar }

class function TJalaliCalendar.CalendarID: Cardinal;
begin
  Result := CAL_PERSIAN;
end;

class function TJalaliCalendar.CalendarKind: TCalendarKind;
begin
  Result := ckSolar;
end;

class function TJalaliCalendar.MinSupportedDateTime: TDateTime;
begin
  Result := Max({1/01/01 Jalali - Imperial} -897684, inherited);
end;

class function TJalaliCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min({3558/12/29 Jalali - Imperial} 401847.99999, inherited);
end;

class function TJalaliCalendar.MaxEra: Integer;
begin
  Result := HijriEra;
end;

class function TJalaliCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := TJalaliCalendarSettings;
end;

function TJalaliCalendar.DayOfYearToDayOfMonth(Era, Year, YearDay: Integer;
  var Month, Day: Integer): Boolean;
begin
  if YearDay <= 6 * 31 then
  begin
    Month := ((YearDay - 1) div 31) + 1;
    Day := YearDay - (Month - 1) * 31;
    Result := True;
  end
  else if YearDay <= DaysInYear(Era, Year) then
  begin
    Dec(YearDay, 6 * 31);
    Month := ((YearDay - 1) div 30) + 7;
    Day := YearDay - (Month - 7) * 30;
    Result := True;
  end
  else
    Result := False;
end;

function TJalaliCalendar.ToJulianDay(Year, Month, Day: Integer): Extended;
var
  JDN: Integer;
begin
  JDN := NowruzJDN(ToZeroBase(ImperialEra, Year));
  if JDN = 0 then
    YearError(ImperialEra, Year);

  if (Day < 1) or (Day > DaysInMonth(ImperialEra, Year, Month)) then
    DayError(ImperialEra, Year, Month, Day);

  Result := JDN + DaysToMonth(ImperialEra, Year, Month) + (Day - 1) - 0.5;
end;

function TJalaliCalendar.FromJulianDay(const JD: Extended;
  out Year, Month, Day: Integer): Boolean;
var
  JDN: Integer;
  Jy, Nowruz: Integer;
  YearDay: Integer;
begin
  JDN := Trunc(JD + 0.5); // Assumed JD is in Tehran civil time

  Jy := FindJalaliYear(JDN);
  Nowruz := NowruzJDN(Jy);
  if Nowruz = 0 then
  begin
    Result := False;
    Exit;
  end;

  Year := FromZeroBase(ImperialEra, Jy);
  YearDay := (JDN - Nowruz) + 1;
  Result := DayOfYearToDayOfMonth(ImperialEra, Year, YearDay, Month, Day);
end;

function TJalaliCalendar.ConvertYear(Year, FromEra, ToEra: Integer): Integer;
begin
  if Year = 0 then
    YearError(FromEra, 0)
  else if FromEra <> ToEra then
  begin
    if FromEra = HijriEra then
      Year := OffsetYear(Year, +HIJRI_YEAR_OFFSET, FromEra, ImperialEra)
    else if FromEra <> ImperialEra then
      EraError(FromEra);
    if ToEra = HijriEra then
      Year := OffsetYear(Year, -HIJRI_YEAR_OFFSET, ImperialEra, ToEra)
    else if ToEra <> ImperialEra then
      EraError(ToEra);
  end
  else if not (FromEra in [ImperialEra, HijriEra]) then
    EraError(FromEra);
  Result := Year;
end;

function TJalaliCalendar.IsValidYear(Era, Year: Integer): Boolean;
var
  Jy: Integer;
begin
  Jy := ToZeroBase(ImperialEra, ConvertYear(Year, Era, ImperialEra));
  Result := (Jy >= JALALI_MIN_YEAR) and (Jy <= JALALI_MAX_YEAR);
end;

function TJalaliCalendar.IsLeapYear(Era, Year: Integer): Boolean;
begin
  Result := DaysInYear(Era, Year) = 366;
end;

function TJalaliCalendar.DaysInYear(Era, Year: Integer): Integer;
var
  Jy, Current, Next: Integer;
begin
  Jy := ToZeroBase(ImperialEra, ConvertYear(Year, Era, ImperialEra));

  Current := NowruzJDN(Jy);
  if Current = 0 then
    YearError(Era, Year);

  Next := NowruzJDN(Jy + 1);
  if Next = 0 then
    YearError(Era, Year);

  Result := Next - Current;
end;

function TJalaliCalendar.DaysInMonth(Era, Year, Month: Integer): Integer;
begin
  if (Month < 1) or (Month > 12) then
    MonthError(Era, Year, Month);

  if (Month = 12) and not IsLeapYear(Era, Year) then
    Result := 29
  else if Month > 6 then
    Result := 30
  else
    Result := 31;
end;

function TJalaliCalendar.DaysToMonth(Era, Year, Month: Integer): Integer;
begin
  if (Month < 1) or (Month > 12) then
    MonthError(Era, Year, Month);

  Result := Min(Month - 1, 6) + ((Month - 1) * 30);
end;

function TJalaliCalendar.VernalEquinox(Era, Year: Integer): TDateTime;
var
  Jy: Integer;
  EquinoxJD: Extended;
begin
  Jy := ToZeroBase(ImperialEra, ConvertYear(Year, Era, ImperialEra));
  if (Jy < JALALI_MIN_YEAR) or (Jy > JALALI_MAX_YEAR) then
    YearError(Era, Year);

  EquinoxJD := VernalEquinoxJulianDay(Jy + JALALI_TO_GREGORIAN_YEAR_OFFSET);
  Result := JulianDayToDateTime(EquinoxJD + IRAN_UTC_OFFSET_DAYS);
end;

function TJalaliCalendar.VernalEquinox(Year: Integer): TDateTime;
begin
  Result := VernalEquinox(DefaultEra, Year);
end;

{ TJalaliCalendarSettings }

class function TJalaliCalendarSettings.CalendarClass: TCalendarClass;
begin
  Result := TJalaliCalendar;
end;

procedure TJalaliCalendarSettings.PrepareEraName(Era: Integer;
  const Locale: string; CalendarID: Cardinal);
begin
  if Pos('Arab;', GetLocaleScripts(Locale)) = 0 then
    case Era of
      TJalaliCalendar.ImperialEra:
      begin
        EraNames[Era] := 'Imperial';
        ShortEraNames[Era] := 'IE';
      end;
      TJalaliCalendar.HijriEra:
      begin
        EraNames[Era] := 'Solar Hijri';
        ShortEraNames[Era] := 'SH';
      end;
    end
  else if SameLanguage(Locale, 'fa') or SameLanguage(Locale, 'ps') or SameLanguage(Locale, 'prs') then
    case Era of
      TJalaliCalendar.ImperialEra:
      begin
          EraNames[Era] := 'شاهنشاهی';
          ShortEraNames[Era] := 'ش';
      end;
      TJalaliCalendar.HijriEra:
      begin
          EraNames[Era] := 'هجری خورشیدی';
          ShortEraNames[Era] := 'هـ.خ';
      end;
    end
  else
    case Era of
      TJalaliCalendar.ImperialEra:
      begin
        EraNames[Era] := 'الإمبراطورية';
        ShortEraNames[Era] := 'إم';
      end;
      TJalaliCalendar.HijriEra:
      begin
        EraNames[Era] := 'الهجري الشمسي';
        ShortEraNames[Era] := 'هـ.ش';
      end;
    end;
end;

procedure TJalaliCalendarSettings.PrepareMonthNames(const Locale: string;
  CalendarID: Cardinal);
const
  PersianMonthNames: array [1 .. 12] of String = (
    'فروردین', 'اردیبهشت', 'خرداد', 'تیر', 'مرداد', 'شهریور',
    'مهر', 'آبان', 'آذر', 'دی', 'بهمن', 'اسفند');
  ArabicMonthNames: array [1 .. 12] of String = (
    'حمل', 'ثور', 'جوزا', 'سرطان', 'اسد', 'سنبله',
    'میزان', 'عقرب', 'قوس', 'جدی', 'دلو', 'حوت');
  EnglishMonthNames: array [1 .. 12] of String = (
    'Farvardin', 'Ordibehesht', 'Khordad', 'Tir', 'Mordad', 'Shahrivar',
    'Mehr', 'Aban', 'Azar', 'Dey', 'Bahman', 'Esfand');
var
  M: Integer;
begin
  if Pos('Arab;', GetLocaleScripts(Locale)) <> 0 then
  begin
    if SameText(Locale, 'fa-IR') then
      for M := 1 to 12 do
      begin
        MonthNames[M] := PersianMonthNames[M];
        ShortMonthNames[M] := PersianMonthNames[M];
        GenitiveMonthNames[M] := PersianMonthNames[M];
      end
    else
      for M := 1 to 12 do
      begin
        MonthNames[M] := ArabicMonthNames[M];
        ShortMonthNames[M] := ArabicMonthNames[M];
        GenitiveMonthNames[M] := ArabicMonthNames[M];
      end
  end
  else
  begin
    for M := 1 to 12 do
    begin
      MonthNames[M] := EnglishMonthNames[M];
      ShortMonthNames[M] := Copy(EnglishMonthNames[M], 1, 3);
      GenitiveMonthNames[M] := EnglishMonthNames[M];
    end;
  end;
end;

procedure TJalaliCalendarSettings.PrepareSettings(const Locale: String;
  CalendarID: Cardinal);
begin
  inherited PrepareSettings(Locale, CalendarID);

  TwoDigitYearMax := 2530;
  FirstDayOfWeek := Saturday;

  if SameLanguage(Locale, 'fa') or SameLanguage(Locale, 'ps') or SameLanguage(Locale, 'prs') then
    CalendarName := 'گاهشمار جلالی';

  if SameText(Locale, 'fa-IR') then
  begin
    // Workaround for Windows NLS incorrect formats
    if SameText(LongDateFormat, 'yyyy/MM/dd') then
      LongDateFormat := 'dddd d MMMM yyyy';
    if SameText(LongTimeFormat, 'hh:mm:ss ampm') then
      LongTimeFormat := 'hh:nn:ss';
    if Win32MajorVersion <= 5 then
    begin
      ShortestDayNames[Saturday]  := 'ش';
      ShortestDayNames[Sunday]    := 'ی';
      ShortestDayNames[Monday]    := 'د';
      ShortestDayNames[Tuesday]   := 'س';
      ShortestDayNames[Wednesday] := 'چ';
      ShortestDayNames[Thursday]  := 'پ';
      ShortestDayNames[Friday]    := 'ج';
    end;
  end;
end;

initialization
  CalendarTypes.Register(TJalaliCalendar, ['fa-IR', 'prs-AF', 'ps-AF']);
end.