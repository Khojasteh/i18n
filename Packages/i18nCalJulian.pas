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
/// This unit implements the Julian calendar.
/// </summary>
unit i18nCalJulian;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, i18nCalendar;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This class implements the Julian calendar.
  /// </summary>
  /// <remarks>
  /// <para>
  /// TJulianCalendar provides properties and methods to manipulate dates in
  /// the Julian calendar.
  /// </para>
  /// <para>
  /// The Julian calendar was proclaimed by Julius Caesar in 46 B.C. and underwent
  /// several modifications before reaching its final form in 8 C.E. The Julian
  /// calendar differs from the Gregorian only in the determination of leap years,
  /// lacking the correction for years divisible by 100 and 400 in the Gregorian
  /// calendar. In the Julian calendar, any positive year is a leap year if
  /// divisible by 4. (Negative years are leap years if the absolute value divided
  /// by 4 yields a remainder of 1.) Days are considered to begin at midnight.
  /// </para>
  /// <para>
  /// In the Julian calendar the average year has a length of 365.25 days compared
  /// to the actual solar tropical year of 365.24219878 days. The calendar thus
  /// accumulates one day of error with respect to the solar year every 128 years.
  /// Being a purely solar calendar, no attempt is made to synchronise the start of
  /// months to the phases of the Moon.
  /// </para>
  /// <para>
  /// NOTE: Both the description and algorithm of this calendar are adapted from
  /// http://www.fourmilab.ch/documents/calendar/.
  /// </para>
  /// </remarks>
  {$endregion}
  TJulianCalendar = class(TCalendar)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a Julian date represented by its year, month, and day
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
    /// The Julian day of the specified Julian date.
    /// </returns>
    /// <seealso cref="FromJulianDay" />
    {$endregion}
    function ToJulianDay(Year, Month, Day: Integer): Extended; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a date expressed in Julian day to its year, month, and day
    /// components in the Julian calendar.
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
    /// Returns <see langword="true" /> if the function is successful; otherwise,
    /// returns <see langword="false" />.
    /// </returns>
    /// <seealso cref="ToJulianDay" />
    {$endregion}
    function FromJulianDay(JD: Extended; out Year, Month, Day: Integer): Boolean; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies the index of the Julian Era.
    /// </summary>
    {$endregion}
    const JulianEra = 1;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the algorithm type of the calendar system.
    /// </summary>
    /// <returns>
    /// Returns <see cref="TCalendarKind" /> of ckSolar.
    /// </returns>
    {$endregion}
    class function CalendarKind: TCalendarKind; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the minimum <see cref="TDateTime" /> value that can be managed by
    /// the calendar.
    /// </summary>
    /// <returns>
    /// The minimum supported <see cref="TDateTime" /> value.
    /// </returns>
    /// <seealso cref="MaxSupportedDateTime" />
    {$endregion}
    class function MinSupportedDateTime: TDateTime; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the maximum <see cref="TDateTime" /> value that can be managed by
    /// the calendar.
    /// </summary>
    /// <returns>
    /// The maximum supported <see cref="TDateTime" /> value.
    /// </returns>
    /// <seealso cref="MinSupportedDateTime" />
    {$endregion}
    class function MaxSupportedDateTime: TDateTime; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TCalendarSettings" /> class that provides locale-
    /// specific settings for the calendar.
    /// </summary>
    /// <returns>
    /// Returns the <see cref="TJulianCalendarSettings" /> class.
    /// </returns>
    {$endregion}
    class function SettingsClass: TCalendarSettingsClass; override;
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
    /// Returns <see langword="true" /> if the year is a leap year; otherwise,
    /// returns <see langword="false" />.
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
    /// Returns 365 for normal years and 366 for leap years.
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
    /// Returns the number of days from the beginning of a specified year to the beginning
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
    /// The number of days between the start of the year and the start of the month.
    /// </returns>
    {$endregion}
    function DaysToMonth(Era, Year, Month: Integer): Integer; override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class provides locale-specific settings for the <see cref="TJulianCalendar" />
  /// class.
  /// </summary>
  /// <remarks>
  /// <para>
  /// The TJulianCalendarSettings class collects the Julian calendar's locale-specific
  /// settings, which are required by the <see cref="TJulianCalendar" /> class.
  /// </para>
  /// </remarks>
  {$endregion}
  TJulianCalendarSettings = class(TCalendarSettings)
  protected
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
    procedure PrepareSettings(const Locale: string; CalendarID: Cardinal); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that this settings class is provided for.
    /// </summary>
    /// <returns>
    /// Returns the <see cref="TJulianCalendar" /> class.
    /// </returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; override;
  end;

implementation

uses
  Math, Types, i18nWinNLS;

{ TJulianCalendar }

class function TJulianCalendar.CalendarKind: TCalendarKind;
begin
  Result := ckSolar;
end;

class function TJulianCalendar.MinSupportedDateTime: TDateTime;
begin
  Result := Max({0001/01/01} -693595, inherited);
end;

class function TJulianCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min({9999/12/31} 2958538.99999, inherited);
end;

class function TJulianCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := TJulianCalendarSettings;
end;

function TJulianCalendar.IsLeapYear(Era, Year: Integer): Boolean;
begin
  Year := ConvertYear(Year, Era, JulianEra);
  Year := ToZeroBase(JulianEra, Year);
  if Year > 0 then
    Result := ((Year mod 4) = 0)
  else
    Result := ((Year mod 4) = -3);
end;

function TJulianCalendar.DaysInYear(Era, Year: Integer): Integer;
begin
  if IsLeapYear(Era, Year) then
    Result := 366
  else
    Result := 365;
end;

function TJulianCalendar.DaysInMonth(Era, Year, Month: Integer): Integer;
const
  Days: array[Boolean] of TDayTable =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));
begin
  if (Month < 1) or (Month > 12) then
    MonthError(Era, Year, Month);
  Result := Days[IsLeapYear(Era, Year)][Month];
end;

function TJulianCalendar.DaysToMonth(Era, Year, Month: Integer): Integer;
const
  Days: array[Boolean] of TDayTable =
    ((0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
     (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335));
begin
  if (Month < 1) or (Month > 12) then
    MonthError(Era, Year, Month);
  Result := Days[IsLeapYear(Era, Year)][Month];
end;

function TJulianCalendar.ToJulianDay(Year, Month, Day: Integer): Extended;
begin
  Year := ToZeroBase(JulianEra, Year);
  if Month <= 2 then
  begin
    Dec(Year);
    Inc(Month, 12);
  end;
  Result := Floor(365.25 * (Year + 4716))
          + Floor(30.6001 * (Month + 1)) + Day - 1524.5;
end;

function TJulianCalendar.FromJulianDay(JD: Extended;
  out Year, Month, Day: Integer): Boolean;
var
  A, B, C, D, E: Extended;
begin
  JD := Trunc(JD - 0.5) + 0.5;
  A := JD + 0.5;
  B := A + 1524;
  C := Floor((B - 122.1) / 365.25);
  D := Floor(365.25 * C);
  E := Floor((B - D) / 30.6001);
  Day := Trunc(B - D - Floor(30.6001 * E));
  Month := Trunc(E - 1);
  if Month > 12 then
    Dec(Month, 12);
  Year := Trunc(C - 4715);
  if Month > 2 then
    Dec(Year);
  Year := FromZeroBase(JulianEra, Year);
  Result := IsValidDay(JulianEra, Year, Month, Day);
end;

{ TJulianCalendarSettings }

class function TJulianCalendarSettings.CalendarClass: TCalendarClass;
begin
  Result := TJulianCalendar;
end;

procedure TJulianCalendarSettings.PrepareSettings(const Locale: string;
  CalendarID: Cardinal);
begin
  inherited PrepareSettings(Locale, CAL_GREGORIAN);
  CalendarName := TJulianCalendar.CalendarName;
end;

initialization
  CalendarTypes.Register(TJulianCalendar);
end.
