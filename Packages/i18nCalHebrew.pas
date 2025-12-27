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
/// This unit implements the Hebrew (or Jewish) calendar.
/// </summary>
unit i18nCalHebrew;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, i18nCalendar;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This class implements the Hebrew (or Jewish) calendar.
  /// </summary>
  /// <remarks>
  /// <para>
  /// THebrewCalendar provides properties and methods to manipulate dates in
  /// the Hebrew calendar.
  /// </para>
  /// <para>
  /// The Hebrew (or Jewish) calendar attempts to simultaneously maintain alignment
  /// between the months and the seasons and synchronize the months with the Moon; it
  /// is thus deemed a "luni-solar calendar". In addition, there are constraints
  /// on which days of the week a year can begin and to shift the otherwise
  /// required extra days to prior years to keep the length of the year within the
  /// prescribed bounds. This isn't easy, and the computations required are
  /// correspondingly intricate.
  /// </para>
  /// <para>
  /// Years are classified as common (normal) or embolismic (leap) years which occur
  /// in a 19 year cycle in years 3, 6, 8, 11, 14, 17, and 19. In an embolismic
  /// (leap) year, an extra month of 29 days, "Veadar" or "Adar II", is added to the
  /// end of the year after the month "Adar", which is designated "Adar I" in such
  /// years. Further, years may be deficient, regular, or complete, having respectively
  /// 353, 354, or 355 days in a common year and 383, 384, or 385 days in embolismic
  /// years. Days are defined as beginning at sunset, and the calendar begins at sunset
  /// the night before Monday, October 7, 3761 B.C.E. in the Julian calendar, or Julian
  /// day 347995.5. Days are numbered with Sunday as day 1, through Saturday as day 7.
  /// </para>
  /// <para>
  /// The average length of a month is 29.530594 days, extremely close to the mean
  /// synodic month (time from new Moon to next new Moon) of 29.530588 days. Such is
  /// the accuracy that more than 13,800 years elapse before a single day discrepancy
  /// between the calendar's average reckoning of the start of months and the mean time
  /// of the new Moon. Alignment with the solar year is better than the Julian calendar,
  /// but inferior to the Gregorian. The average length of a year is 365.2468 days
  /// compared to the actual solar tropical year (time from equinox to equinox) of
  /// 365.24219 days, so the calendar accumulates one day of error with respect to the
  /// solar year every 216 years.
  /// </para>
  /// <para>
  /// NOTE: Both description and algorithm of this calendar are adapted from
  /// http://www.fourmilab.ch/documents/calendar/.
  /// </para>
  /// </remarks>
  {$endregion}
  THebrewCalendar = class(TCalendar)
  private
    class function ElapsedDays(Year: Integer): Integer; static;
    class function Delay(Year: Integer): Integer; static;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// The start of the Hebrew calendar in Julian days.
    /// </summary>
    {$endregion}
    const HEBREW_EPOCH = 347995.5;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a Hebrew date represented by its year, month and day
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
    /// Julian day of the specified Hebrew date.
    /// </returns>
    /// <seealso cref="FromJulianDay"/>
    {$endregion}
    function ToJulianDay(Year, Month, Day: Integer): Extended; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a date expressed in Julian day, to its year, month, and day
    /// components in the Hebrew calendar.
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
    /// Returns <see langword="true"/> if the function is succeeded, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="ToJulianDay"/>
    {$endregion}
    function FromJulianDay(JD: Extended; out Year, Month, Day: Integer): Boolean; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies index of Hebrew Era (C.E.).
    /// </summary>
    {$endregion}
    const HebrewEra = 1;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the unique identifier of the calendar.
    /// </summary>
    /// <returns>
    /// Returns <see cref="CAL_HEBREW"/>.
    /// </returns>
    {$endregion}
    class function CalendarID: Cardinal; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the algorithm type of the calendar system.
    /// </summary>
    /// <returns>
    /// Returns a <see cref="TCalendarKind"/> value of ckLunisolar.
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
    /// Returns the <see cref="TCalendarSettings"/> class that provides locale-
    /// specific settings for the calendar.
    /// </summary>
    /// <returns>
    /// Returns the <see cref="THebrewCalendarSettings"/> class.
    /// </returns>
    {$endregion}
    class function SettingsClass: TCalendarSettingsClass; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the maximum number of months that the calendar may have in a year.
    /// </summary>
    /// <returns>
    /// Returns 13.
    /// </returns>
    {$endregion}
    class function MaxMonthsPerYear: Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the first month of the year for the calendar.
    /// </summary>
    /// <returns>
    /// Returns 7 (Tishri).
    /// </returns>
    {$endregion}
    class function FirstMonthOfYear: Integer; override;
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
    /// Returns 12 for common (normal) and 13 for embolismic (leap) years.
    /// </returns>
    {$endregion}
    function MonthsInYear(Era, Year: Integer): Integer; override;
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
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class provides locale-specific settings for the <see cref="THebrewCalendar"/>
  /// class.
  /// </summary>
  /// <remarks>
  /// <para>
  /// THebrewCalendarSettings class collects the Hebrew calendar's locale-specific
  /// settings, which are required by the <see cref="THebrewCalendar"/> class.
  /// </para>
  /// </remarks>
  {$endregion}
  THebrewCalendarSettings = class(TCalendarSettings)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a given month number to its index in the Windows calendar
    /// information.
    /// </summary>
    /// <param name="Month">
    /// The month number.
    /// </param>
    /// <returns>
    /// The zero-based index of given month number in the Windows calendar
    /// information.
    /// </returns>
    {$endregion}
    function GetMonthInfoIndex(Month: Integer): Integer; override;
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
    procedure PrepareMonthNames(const Locale: string; CalendarID: Cardinal); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that these settings are provided for.
    /// </summary>
    /// <returns>
    /// Returns the <see cref="THebrewCalendar"/> class.
    /// </returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; override;
  end;

implementation

uses
  Math, Types, i18nWinNLS;

{ THebrewCalendar }

class function THebrewCalendar.CalendarID: Cardinal;
begin
  Result := CAL_HEBREW;
end;

class function THebrewCalendar.CalendarKind: TCalendarKind;
begin
  Result := ckLunisolar;
end;

class function THebrewCalendar.MaxMonthsPerYear: Integer;
begin
  Result := 13;
end;

class function THebrewCalendar.MinSupportedDateTime: TDateTime;
begin
  Result := Max({0001/07/01 Hebrew} -2067021.0, inherited);
end;

class function THebrewCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min({9999/06/29 Hebrew} 1585056.99999, inherited);
end;

class function THebrewCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := THebrewCalendarSettings;
end;

class function THebrewCalendar.FirstMonthOfYear: Integer;
begin
  Result := 7;
end;

function THebrewCalendar.IsLeapYear(Era, Year: Integer): Boolean;
begin
  Year := ConvertYear(Year, Era, HebrewEra);
  Year := ToZeroBase(HebrewEra, Year);
  Result := Modulus(((Year * 7) + 1), 19) < 7;
end;

function THebrewCalendar.MonthsInYear(Era, Year: Integer): Integer;
begin
  if IsLeapYear(Era, Year) then
    Result := 13
  else
    Result := 12;
end;

function THebrewCalendar.DaysInMonth(Era, Year, Month: Integer): Integer;
begin
  if (Month < 1) or (Month > 13) or ((Month = 13) and not IsLeapYear(Era, Year)) then
    MonthError(Era, Year, Month);
  if Month in [2, 4, 6, 10, 13] then
    Result := 29
  else if (Month = 12) and not IsLeapYear(Era, Year) then
    Result := 29
  else if (Month = 8) and ((DaysInYear(Era, Year) mod 10) <> 5) then
    Result := 29
  else if (Month = 9) and ((DaysInYear(Era, Year) mod 10) = 3) then
    Result := 29
  else
    Result := 30;
end;

class function THebrewCalendar.ElapsedDays(Year: Integer): Integer;
var
  Months, Day, Parts: Integer;
begin
  Months := Floor(((235 * Year) - 234) / 19);
  Parts := 12084 + (13753 * Months);
  Day := (Months * 29) + Floor(Parts / 25920);
  if ((3 * (Day + 1)) mod 7) < 3 then
    Inc(Day);
  Result := Day;
end;

class function THebrewCalendar.Delay(Year: Integer): Integer;
var
  Last, Present, Next: Integer;
begin
  Last := ElapsedDays(Year - 1);
  Present := ElapsedDays(Year);
  Next := ElapsedDays(Year + 1);
  if (Next - Present) = 356 then
    Result := 2
  else if (Present - Last) = 382 then
    Result := 1
  else
    Result := 0;
  Inc(Result, Present);
end;

function THebrewCalendar.ToJulianDay(Year, Month, Day: Integer): Extended;
var
  M: Integer;
begin
  Year := ToZeroBase(HebrewEra, Year);
  Result := HEBREW_EPOCH + Delay(Year) + Day + 1;
  Year := FromZeroBase(HebrewEra, Year);
  if Month < 7 then
  begin
    for M := 1 to Month - 1 do
      Result := Result + DaysInMonth(HebrewEra, Year, M);
    for M := 7 to MonthsInYear(HebrewEra, Year) do
      Result := Result + DaysInMonth(HebrewEra, Year, M);
  end
  else
  begin
    for M := 7 to Month - 1 do
      Result := Result + DaysInMonth(HebrewEra, Year, M);
  end;
end;

function THebrewCalendar.FromJulianDay(JD: Extended;
  out Year, Month, Day: Integer): Boolean;
var
  Diff: Extended;
begin
  JD := Trunc(JD - 0.5) + 0.5;
  Year := Floor(((JD - HEBREW_EPOCH) * 98496) / 35975351);
  Year := FromZeroBase(HebrewEra, Year);
  Diff := JD - ToJulianDay(Year, 7, 1);
  if Diff >= 0 then
  begin
    Inc(Year, Floor(Diff / 385));
    while JD >= ToJulianDay(Year, 7, 1) do
      Inc(Year);
  end;
  Dec(Year);
  if JD < ToJulianDay(Year, 1, 1) then
    Month := 7
  else
    Month := 1;
  while JD > ToJulianDay(Year, Month, DaysInMonth(HebrewEra, Year, Month)) do
    Inc(Month);
  Day := Trunc(JD - ToJulianDay(Year, Month, 1)) + 1;
  Result := (Day >= 0);
end;

{ THebrewCalendarSettings }

class function THebrewCalendarSettings.CalendarClass: TCalendarClass;
begin
  Result := THebrewCalendar;
end;

function THebrewCalendarSettings.GetMonthInfoIndex(Month: Integer): Integer;
begin
  Result := (Month + 6) mod 13;
end;

procedure THebrewCalendarSettings.PrepareMonthNames(const Locale: string;
  CalendarID: Cardinal);
const
  EnglishMonthNames: array[1..13] of String = (
    'Nisan', 'Iyyar', 'Sivan', 'Tammuz', 'Av', 'Elul', 'Tishri',
    'Heshvan', 'Kislev', 'Teveth', 'Shevat', 'Adar', 'Adar II');
  EnglishShortMonthNames: array[1..13] of String = (
    'NIS', 'IYA', 'SIV', 'TAM', 'AV', 'ELU', 'TIS',
    'HES', 'KIS', 'TEV', 'SHE', 'ADA', 'AD2');
var
  M: Integer;
begin
  if Pos('Hebr;', GetLocaleScripts(Locale)) <> 0 then
    inherited PrepareMonthNames(Locale, CalendarID)
  else
  begin
    for M := 1 to 13 do
    begin
      MonthNames[M] := EnglishMonthNames[M];
      ShortMonthNames[M] := EnglishShortMonthNames[M];
      GenitiveMonthNames[M] := EnglishMonthNames[M];
    end;
  end;
end;

initialization
  CalendarTypes.Register(THebrewCalendar, ['he-IL']);
end.
