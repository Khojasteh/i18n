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
/// This unit implements the Gregorian calendar.
/// </summary>
unit i18nCalGregorian;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, i18nCalendar;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This class implements the Gregorian calendar.
  /// </summary>
  /// <remarks>
  /// <para>
  /// TGregorianCalendar provides properties and methods to manipulate dates
  /// in the Gregorian calendar.
  /// </para>
  /// <para>
  /// The Gregorian calendar is a minor correction to the Julian calendar, proclaimed
  /// by Pope Gregory XIII. In the Julian calendar every fourth year is a leap year
  /// in which February has 29, not 28 days, but in the Gregorian, years divisible
  /// by 100 are not leap years unless they are also divisible by 400. As in the
  /// Julian calendar, days are considered to begin at midnight.
  /// </para>
  /// <para>
  /// The average length of a year in the Gregorian calendar is 365.2425 days
  /// compared to the actual solar tropical year (time from equinox to equinox)
  /// of 365.24219878 days, so the calendar accumulates one day of error with
  /// respect to the solar year about every 3300 years. As a purely solar calendar,
  /// no attempt is made to synchronize the start of months to the phases of the Moon.
  /// </para>
  /// <para>
  /// NOTE: Both description and algorithm of this calendar are adapted from
  /// http://www.fourmilab.ch/documents/calendar/.
  /// </para>
  /// </remarks>
  {$endregion}
  TGregorianCalendar = class(TCalendar)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// The start of the Gregorian calendar in Julian days.
    /// </summary>
    {$endregion}
    const GREGORIAN_EPOCH = 1721425.5;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a Gregorian date represented by its year, month and day
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
    /// Julian day of the specified Gregorian date.
    /// </returns>
    /// <seealso cref="FromJulianDay"/>
    {$endregion}
    function ToJulianDay(Year, Month, Day: Integer): Extended; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a date expressed in Julian day to its year, month, and day
    /// components in the Gregorian calendar.
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
    /// Returns <see langword="true"/> if the function succeeds, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="ToJulianDay"/>
    {$endregion}
    function FromJulianDay(const JD: Extended; out Year, Month, Day: Integer): Boolean; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies index of Common Era (C.E.).
    /// </summary>
    {$endregion}
    const CommonEra = 1;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the unique identifier of the calendar.
    /// </summary>
    /// <returns>
    /// Returns <see cref="CAL_GREGORIAN"/>.
    /// </returns>
    {$endregion}
    class function CalendarID: Cardinal; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the algorithm type of the calendar system.
    /// </summary>
    /// <returns>
    /// Returns a <see cref="TCalendarKind"/> value of ckSolar.
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
    /// Returns the <see cref="TCalendarSettings"/> class that provides locale-specific
    /// settings for the calendar.
    /// </summary>
    /// <returns>
    /// Returns <see cref="TGregorianCalendarSettings"/> class.
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
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class provides locale-specific settings for the <see cref="TGregorianCalendar"/>
  /// class.
  /// </summary>
  /// <remarks>
  /// TGregorianCalendarSettings class collects the Gregorian calendar's locale-specific
  /// settings, which are required by the <see cref="TGregorianCalendar"/> class.
  /// </remarks>
  {$endregion}
  TGregorianCalendarSettings = class(TCalendarSettings)
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that this settings is provided for.
    /// </summary>
    /// <returns>
    /// Returns <see cref="TGregorianCalendar"/> class.
    /// </returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; override;
  end;

implementation

uses
  Math, Windows, Types, i18nWinNLS;

{ TGregorianCalendar }

class function TGregorianCalendar.CalendarID: Cardinal;
begin
  Result := CAL_GREGORIAN;
end;

class function TGregorianCalendar.CalendarKind: TCalendarKind;
begin
  Result := ckSolar;
end;

class function TGregorianCalendar.MinSupportedDateTime: TDateTime;
begin
  Result := Max({0001/01/01} -693593, inherited);
end;

class function TGregorianCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min({9999/12/31} 2958465.99999, inherited);
end;

class function TGregorianCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := TGregorianCalendarSettings;
end;

function TGregorianCalendar.IsLeapYear(Era, Year: Integer): Boolean;
begin
  Year := ConvertYear(Year, Era, CommonEra);
  Year := ToZeroBase(CommonEra, Year);
  Result := ((Year mod 4) = 0) and (((Year mod 100) <> 0) or ((Year mod 400) = 0));
end;

function TGregorianCalendar.DaysInYear(Era, Year: Integer): Integer;
begin
  if IsLeapYear(Era, Year) then
    Result := 366
  else
    Result := 365;
end;

function TGregorianCalendar.DaysInMonth(Era, Year, Month: Integer): Integer;
const
  Days: array[Boolean] of TDayTable =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));
begin
  if (Month < 1) or (Month > 12) then
    MonthError(Era, Year, Month);
  Result := Days[IsLeapYear(Era, Year)][Month];
end;

function TGregorianCalendar.DaysToMonth(Era, Year, Month: Integer): Integer;
const
  Days: array[Boolean] of TDayTable =
    ((0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
     (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335));
begin
  if (Month < 1) or (Month > 12) then
    MonthError(Era, Year, Month);
  Result := Days[IsLeapYear(Era, Year)][Month];
end;

function TGregorianCalendar.ToJulianDay(Year, Month, Day: Integer): Extended;
const
  AdjustDays: array[Boolean] of Integer = (2, 1);
var
  Adjust: Integer;
begin
  if Month > 2 then
    Adjust := AdjustDays[IsLeapYear(CommonEra, Year)]
  else
    Adjust := 0;
  Year := ToZeroBase(CommonEra, Year);
  Result := GREGORIAN_EPOCH + (Year - 1) * 365
          + Floor((Year - 1) / 4) - Floor((Year - 1) / 100) + Floor((Year - 1) / 400)
          + Floor(((367 * Month) - 362) / 12) + (Day - 1) - Adjust;
end;

function TGregorianCalendar.FromJulianDay(const JD: Extended;
  out Year, Month, Day: Integer): Boolean;
var
  NormalizedJD: Extended;
  Period, QuadriCent, Cent, Quad, YearOfQuad, DayOfYear: Integer;
begin
  NormalizedJD := Trunc(JD - 0.5) + 0.5;
  Period := Trunc(NormalizedJD - GREGORIAN_EPOCH);
  Divide(Period, 146097, QuadriCent, Period);
  Divide(Period, 36524, Cent, Period);
  Divide(Period, 1461, Quad, Period);
  YearOfQuad := Floor(Period / 365);
  Year := (QuadriCent * 400) + (Cent * 100) + (Quad * 4) + YearOfQuad;
  if (Cent <> 4) and (YearOfQuad <> 4) then
    Inc(Year);
  Year := FromZeroBase(CommonEra, Year);
  DayOfYear := Trunc(NormalizedJD - ToJulianDay(Year, 1, 1)) + 1;
  Result := DayOfYearToDayOfMonth(CommonEra, Year, DayOfYear, Month, Day);
end;

{ TGregorianCalendarSettings }

class function TGregorianCalendarSettings.CalendarClass: TCalendarClass;
begin
  Result := TGregorianCalendar;
end;

initialization
  CalendarTypes.Register(TGregorianCalendar);
end.

