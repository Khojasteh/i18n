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
  /// Nowruz, determined by the vernal equinox near March. Leap years do not recur
  /// in a fixed arithmetic pattern; a year is a leap year when 366 days elapse
  /// between consecutive Nowruz days.
  /// </para>
  /// <para>
  /// This implementation determines Nowruz by computing an astronomical approximation
  /// of the vernal equinox time using Jean Meeus' polynomials (JDE in Terrestrial Time),
  /// converting to Universal Time (UT) via an approximate ΔT model, and then evaluating
  /// the instant in Iran Standard Time using a fixed UTC+03:30 offset (no historical DST).
  /// Nowruz is taken as the local civil day containing the equinox when it occurs before
  /// local noon, otherwise the following day.
  /// </para>
  /// <para>
  /// Supported range is limited to the Gregorian-year window used by the Meeus equinox
  /// approximation. The implementation supports Jalali years from -0379 to 2379 (Gregorian
  /// years -1000 to 3000). However, to keep the implementation efficient, the minimum
  /// supported Jalali year is limited to 1, and the maximum supported year is one less
  //  than 2379 (i.e., 2378) to avoid out of range calculations in some methods.
  /// </para>
  /// </remarks>
  {$endregion}
  TJalaliCalendar = class(TCalendar)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// The start of the Jalali calendar in Julian days.
    /// </summary>
    {$endregion}
    const JALALI_EPOCH = 1948320.5;
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
    /// Specifies the index of the Hijri era.
    /// </summary>
    {$endregion}
    const HijriEra = 1;
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
  Math, Types, i18nWinNLS;

const
  // Fixed Iran Standard Time offset from UTC in days (+03:30)
  IRAN_UTC_OFFSET_DAYS = 3.5 / 24.0;

  // Minimum supported Jalali year can be -0379. However, to keep the
  // implementation efficient, we limit the minimum supported year to 1.
  JALALI_MIN_YEAR = 1;

  // Maximum supported Jalali year is 2378 because DaysInYear(Y) requires
  // computing NowruzJDN(Y+1), and the equinox approximation is limited
  // to Gregorian year 3000.
  JALALI_MAX_YEAR = 2378;

  // Year offset between Jalali and Gregorian calendars
  JALALI_TO_GREGORIAN_YEAR_OFFSET = 621;

  // Meeus polynomial switch
  EQUINOX_POLY_SWITCH_YEAR = 1000;

  // Meeus time variable definition (T in Julian millennia)
  JULIAN_YEARS_PER_MILLENNIUM = 1000.0;
  EQUINOX_REF_YEAR_EARLY = 0;
  EQUINOX_REF_YEAR_LATE  = 2000;

// Calculates the difference between Terrestrial Time (TT) and Universal Time (UT)
// in seconds for a given decimal year.
function DeltaTSeconds(const YDecimal: Extended): Extended;
var
  u, t: Extended;
begin
  if YDecimal < -500 then
  begin
    u := (YDecimal - 1820.0) / 100.0;
    Result := -20.0 + 32.0 * u * u;
    Exit;
  end;

  if YDecimal < 500 then
  begin
    u := YDecimal / 100.0;
    Result := 10583.6
            - 1014.41 * u
            + 33.78311 * Sqr(u)
            - 5.952053 * Power(u, 3)
            - 0.1798452 * Power(u, 4)
            + 0.022174192 * Power(u, 5)
            + 0.0090316521 * Power(u, 6);
    Exit;
  end;

  if YDecimal < 1600 then
  begin
    u := (YDecimal - 1000.0) / 100.0;
    Result := 1574.2
            - 556.01 * u
            + 71.23472 * Sqr(u)
            + 0.319781 * Power(u, 3)
            - 0.8503463 * Power(u, 4)
            - 0.005050998 * Power(u, 5)
            + 0.0083572073 * Power(u, 6);
    Exit;
  end;

  if YDecimal < 1700 then
  begin
    t := YDecimal - 1600.0;
    Result := 120.0 - 0.9808 * t - 0.01532 * Sqr(t) + Power(t, 3) / 7129.0;
    Exit;
  end;

  if YDecimal < 1800 then
  begin
    t := YDecimal - 1700.0;
    Result := 8.83
            + 0.1603 * t
            - 0.0059285 * Sqr(t)
            + 0.00013336 * Power(t, 3)
            - Power(t, 4) / 1174000.0;
    Exit;
  end;

  if YDecimal < 1860 then
  begin
    t := YDecimal - 1800.0;
    Result := 13.72
            - 0.332447 * t
            + 0.0068612 * Sqr(t)
            + 0.0041116 * Power(t, 3)
            - 0.00037436 * Power(t, 4)
            + 0.0000121272 * Power(t, 5)
            - 0.0000001699 * Power(t, 6)
            + 0.000000000875 * Power(t, 7);
    Exit;
  end;

  if YDecimal < 1900 then
  begin
    t := YDecimal - 1860.0;
    Result := 7.62
            + 0.5737 * t
            - 0.251754 * Sqr(t)
            + 0.01680668 * Power(t, 3)
            - 0.0004473624 * Power(t, 4)
            + Power(t, 5) / 233174.0;
    Exit;
  end;

  if YDecimal < 1920 then
  begin
    t := YDecimal - 1900.0;
    Result := -2.79
            + 1.494119 * t
            - 0.0598939 * Sqr(t)
            + 0.0061966 * Power(t, 3)
            - 0.000197 * Power(t, 4);
    Exit;
  end;

  if YDecimal < 1941 then
  begin
    t := YDecimal - 1920.0;
    Result := 21.20 + 0.84493 * t - 0.076100 * Sqr(t) + 0.0020936 * Power(t, 3);
    Exit;
  end;

  if YDecimal < 1961 then
  begin
    t := YDecimal - 1950.0;
    Result := 29.07 + 0.407 * t - Sqr(t) / 233.0 + Power(t, 3) / 2547.0;
    Exit;
  end;

  if YDecimal < 1986 then
  begin
    t := YDecimal - 1975.0;
    Result := 45.45 + 1.067 * t - Sqr(t) / 260.0 - Power(t, 3) / 718.0;
    Exit;
  end;

  if YDecimal < 2005 then
  begin
    t := YDecimal - 2000.0;
    Result := 63.86
            + 0.3345 * t
            - 0.060374 * Sqr(t)
            + 0.0017275 * Power(t, 3)
            + 0.000651814 * Power(t, 4)
            + 0.00002373599 * Power(t, 5);
    Exit;
  end;

  if YDecimal < 2050 then
  begin
    t := YDecimal - 2000.0;
    Result := 62.92 + 0.32217 * t + 0.005589 * Sqr(t);
    Exit;
  end;

  if YDecimal < 2150 then
  begin
    // ΔT = -20 + 32*u^2 - 0.5628*(2150-y) :contentReference[oaicite:4]{index=4}
    u := (YDecimal - 1820.0) / 100.0;
    Result := -20.0 + 32.0 * u * u - 0.5628 * (2150.0 - YDecimal);
    Exit;
  end;

  u := (YDecimal - 1820.0) / 100.0;
  Result := -20.0 + 32.0 * u * u;
end;

// Calculates the Julian Ephemeris Day in Terrestrial Time for the vernal equinox
// in a given Gregorian year.
function VernalEquinoxJDE_TT(Gy: Integer): Extended;
const
  A: array[0..23] of Integer = (
    485, 203, 199, 182, 156, 136,  77,  74,  70,  58,  52,  50,
     45,  44,  29,  18,  17,  16,  14,  12,  12,  12,   9,   8
  );
  B: array[0..23] of Extended = (
    324.96, 337.23, 342.08,  27.85,  73.14, 171.52, 222.54, 296.72,
    243.58, 119.81, 297.17,  21.02, 247.54, 325.15,  60.93, 155.12,
    288.79, 198.04, 199.76,  95.39, 287.11, 320.81, 227.73,  15.45
  );
  C: array[0..23] of Extended = (
    1934.136, 32964.467,   20.186, 445267.112, 45036.886, 22518.443,
   65928.934,  3034.906,  9037.513,  33718.147,   150.678,  2281.226,
   29929.562, 31555.956,  4443.417,  67555.328,  4562.452, 62894.029,
   31436.921, 14577.848, 31931.756, 34777.259,  1222.114, 16859.074
  );
var
  T, JDE0, W, dLambda, S: Extended;
  i: Integer;
  Rad: Extended;
begin
  if Gy >= EQUINOX_POLY_SWITCH_YEAR then
  begin
    // Gy in [1000..3000], Meeus "late" polynomial referenced to J2000.0
    T := (Gy - EQUINOX_REF_YEAR_LATE) / JULIAN_YEARS_PER_MILLENNIUM;
    JDE0 := 2451623.80984
          + 365242.37404 * T
          + 0.05169 * Sqr(T)
          - 0.00411 * Power(T, 3)
          - 0.00057 * Power(T, 4);
  end
  else
  begin
    // Gy in [-1000..999], Meeus "early" polynomial referenced to year 0
    T := (Gy - EQUINOX_REF_YEAR_EARLY) / JULIAN_YEARS_PER_MILLENNIUM;
    JDE0 := 1721139.29189
          + 365242.13740 * T
          + 0.06134 * Sqr(T)
          + 0.00111 * Power(T, 3)
          - 0.00071 * Power(T, 4);
  end;

  S := 0.0;
  Rad := Pi / 180.0;
  for i := Low(A) to High(A) do
    S := S + A[i] * Cos((B[i] + C[i] * T) * Rad);

  W := (35999.373 * T) - 2.47;
  dLambda := 1.0 + 0.0334 * Cos(W * Rad) + 0.0007 * Cos(2.0 * W * Rad);

  Result := JDE0 + (0.00001 * S) / dLambda;
end;

// Calculates the Julian Day for the vernal equinox in Universal Time
// for a given Gregorian year.
function VernalEquinoxJD_UT(Gy: Integer): Extended; inline;
var
  JDE_TT, DT: Extended;
  YDecimal: Extended;
begin
  JDE_TT := VernalEquinoxJDE_TT(Gy);

  YDecimal := Gy + (3.0 - 0.5) / 12.0;

  DT := DeltaTSeconds(YDecimal);
  Result := JDE_TT - (DT / 86400.0);
end;

// Calculates the Julian Day for the vernal equinox in Tehran civil time
// for a given Jalali year.
function VernalEquinoxJD_TehranTime(Jy: Integer): Extended; inline;
begin
  Result := VernalEquinoxJD_UT(Jy + JALALI_TO_GREGORIAN_YEAR_OFFSET) + IRAN_UTC_OFFSET_DAYS;
end;

// Calculates the Julian Day Number for Nowruz (the start of the Jalali year)
// for a given Jalali year.
function NowruzJDN(Jy: Integer): Integer; inline;
var
  EqTehran: Extended;
  D: Integer;
begin
  EqTehran := VernalEquinoxJD_TehranTime(Jy);
  D := Trunc(EqTehran + 0.5);

  // If equinox occurs before Tehran local noon, Nowruz is that day; otherwise next day.
  if (EqTehran - (D - 0.5)) < 0.5 then
    Result := D
  else
    Result := D + 1;
end;

// Finds the Jalali year for a given Julian Day Number.
function FindJalaliYear(const JDN: Integer): Integer; inline;
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
  Result := Max({0001/01/01 Jalali} -466698, inherited);
end;

class function TJalaliCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min({2378/12/29 Jalali} 401847.99999, inherited);
end;

class function TJalaliCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := TJalaliCalendarSettings;
end;

function TJalaliCalendar.IsLeapYear(Era, Year: Integer): Boolean;
begin
  Result := DaysInYear(Era, Year) = 366;
end;

function TJalaliCalendar.DaysInYear(Era, Year: Integer): Integer;
begin
  Year := ConvertYear(Year, Era, HijriEra);
  Year := ToZeroBase(HijriEra, Year);

  if (Year < JALALI_MIN_YEAR) or (Year > JALALI_MAX_YEAR) then
    YearError(Era, Year);

  Result := NowruzJDN(Year + 1) - NowruzJDN(Year);
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
  TargetJDN: Integer;
begin
  Year := ToZeroBase(HijriEra, Year);

  if (Year < JALALI_MIN_YEAR) or (Year > JALALI_MAX_YEAR) then
    YearError(HijriEra, Year);
  if (Month < 1) or (Month > 12) then
    MonthError(HijriEra, Year, Month);
  if (Day < 1) or (Day > DaysInMonth(HijriEra, Year, Month)) then
    DayError(HijriEra, Year, Month, Day);

  TargetJDN := NowruzJDN(Year) + DaysToMonth(HijriEra, Year, Month) + (Day - 1);
  Result := TargetJDN - 0.5;
end;

function TJalaliCalendar.FromJulianDay(const JD: Extended;
  out Year, Month, Day: Integer): Boolean;
var
  JDN: Integer;
  Jy, NY, NextNY: Integer;
  YearDay: Integer;
begin
  JDN := Trunc(JD + 0.5); // Assumed JD is in Tehran civil time

  Jy := FindJalaliYear(JDN);
  if (Jy < JALALI_MIN_YEAR) or (Jy > JALALI_MAX_YEAR) then
    Exit(False);

  NY := NowruzJDN(Jy);
  NextNY := NowruzJDN(Jy + 1);

  if (JDN < NY) or (JDN >= NextNY) then
    Exit(False);

  Year := FromZeroBase(HijriEra, Jy);
  YearDay := (JDN - NY) + 1;
  Result := DayOfYearToDayOfMonth(HijriEra, Year, YearDay, Month, Day);
end;

{ TJalaliCalendarSettings }

class function TJalaliCalendarSettings.CalendarClass: TCalendarClass;
begin
  Result := TJalaliCalendar;
end;

procedure TJalaliCalendarSettings.PrepareEraName(Era: Integer;
  const Locale: string; CalendarID: Cardinal);
begin
  if (Era = TJalaliCalendar.HijriEra) and
     (Pos('Arab;', GetLocaleScripts(Locale)) <> 0) then
  begin
    EraNames[TJalaliCalendar.HijriEra] := 'هجری خورشیدی';
    ShortEraNames[TJalaliCalendar.HijriEra] := 'ﻫ';
  end
  else
    inherited PrepareEraName(Era, Locale, CalendarID);
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
  TwoDigitYearMax := 1350;
  if SameText(Locale, 'fa-IR') or
     SameText(Locale, 'prs-AF') or
     SameText(Locale, 'ps-AF') then
  begin
    CalendarName := 'گاهشمار جلالی';
    FirstDayOfWeek := Saturday;
  end;
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