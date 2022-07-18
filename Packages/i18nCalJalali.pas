{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements the Jalali (or Persian) calendar.
unit i18nCalJalali;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, i18nCalendar;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This class implements the Jalali (or Persian) calendar.</summary>
  /// <remarks>
  /// TJalaliCalendar provides properties and methods to manimuplate dates in
  /// the Jalali calendar.
  ///
  /// The Jalali (or Persian) calendar consists of 12 months, the first six of which
  /// are 31 days, the next five 30 days, and the final month 29 days in a normal year
  /// and 30 days in a leap year.
  ///
  /// Each year begins on the day in which the March equinox occurs at or after solar
  /// noon at the reference longitude for Iran Standard Time (52°30' E). Days begin
  /// at midnight in the standard time zone. There is no leap year rule; 366 day years
  /// do not recur in a regular pattern but instead occur whenever that number of days
  /// elapse between equinoxes at the reference meridian. The calendar therefore stays
  /// perfectly aligned with the seasons. No attempt is made to synchronise months with
  /// the phases of the Moon.
  ///
  /// The Jalali calendar implemented here is not the official calendar in use in Iran!
  /// Because the presented algorithm here avoids the need to determine the moment of
  /// the astronomical equinox, replacing it with a very complex leap year structure.
  /// Years are grouped into cycles which begin with four normal years after which every
  /// fourth subsequent year in the cycle is a leap year. Cycles are grouped into grand
  /// cycles of either 128 years (composed of cycles of 29, 33, 33, and 33 years) or 132
  /// years, containing cycles of of 29, 33, 33, and 37 years. A great grand cycle is
  /// consecutive 128 year grand cycles and a final 132 grand cycle, for a total of 2820
  /// composed of 21 years. The pattern of normal and leap years which began in 1925 will
  /// not repeat until the year 4745.
  ///
  /// Each 2820 year great grand cycle contains 2137 normal years of 365 days and 683 leap
  /// years of 366 days, with the average year length over the great grand cycle of
  /// 365.24219852 days. So close is this to the actual solar tropical year of 365.24219878
  /// days that this implementation accumulates an error of one day only every 3.8 million
  /// years.
  ///
  /// NOTE: Both description and algorithm of this calendar is adapted from
  /// http://www.fourmilab.ch/documents/calendar/.</remarks>
  {$endregion}
  TJalaliCalendar = class(TCalendar)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// The start of the Jalali calendar in Julian days.</summary>
    {$endregion}
    const JALALI_EPOCH = 1948320.5;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a Jalali date represented by its year, month and day
    /// components to its corresponding Julian day.</summary>
    /// <param name="Year">
    /// The year.</param>
    /// <param name="Month">
    /// The month of the year.</param>
    /// <param name="Day">
    /// The day of the month.</param>
    /// <returns>
    /// Julian day of the specified Jalali date.</returns>
    /// <seealso cref="FromJulianDay"/>
    {$endregion}
    function ToJulianDay(Year, Month, Day: Integer): Extended; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a date expressed in Julian day, to its year, month, and day
    /// components in the Jalali calendar.</summary>
    /// <param name="JD">
    /// The date expressed in Julian day.</param>
    /// <param name="Year">
    /// The year.</param>
    /// <param name="Month">
    /// The month of the year.</param>
    /// <param name="Day">
    /// The day of the month.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the function is succeeded, otherwise
    /// returns <see langword="false"/>.</returns>
    /// <seealso cref="ToJulianDay"/>
    {$endregion}
    function FromJulianDay(JD: Extended; out Year, Month, Day: Integer): Boolean; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies index of Hijri Era.</summary>
    {$endregion}
    const HijriEra = 1;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the unique identifier of the calendar.</summary>
    /// <returns>
    /// Returns <see cref="CAL_PERSIAN"/>.</returns>
    {$endregion}
    class function CalendarID: Cardinal; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the algorithm type of the calendar system.</summary>
    /// <returns>
    /// Returns <see cref="TCalendarKind"/> of ckSolar.</returns>
    {$endregion}
    class function CalendarKind: TCalendarKind; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the minimum <see cref="TDateTime"/> value that can be managed by
    /// the calendar.</summary>
    /// <returns>
    /// The minimum supported <see cref="TDateTime"/> value.</returns>
    /// <seealso cref="MaxSupportedDateTime"/>
    {$endregion}
    class function MinSupportedDateTime: TDateTime; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the maximum <see cref="TDateTime"/> value that can be managed by
    /// the calendar.</summary>
    /// <returns>
    /// The maximum supported <see cref="TDateTime"/> value.</returns>
    /// <seealso cref="MinSupportedDateTime"/>
    {$endregion}
    class function MaxSupportedDateTime: TDateTime; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TCalendarSettings"/> class that provides locale
    /// specific settings for the calendar.</summary>
    /// <returns>
    /// Returns <see cref="TJalaliCalendarSettings"/> class.</returns>
    {$endregion}
    class function SettingsClass: TCalendarSettingsClass; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified year in a specified era is a leap year.</summary>
    /// <param name="Era">
    /// The era.</param>
    /// <param name="Year">
    /// The year of the era.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the year is a leap year valid, otherwise
    /// returns <see langword="false"/>.</returns>
    {$endregion}
    function IsLeapYear(Era, Year: Integer): Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days in a specified year.</summary>
    /// <param name="Era">
    /// The era.</param>
    /// <param name="Year">
    /// The year of the era.</param>
    /// <returns>
    /// Returns 365 for notmal and 266 for leap years.</returns>
    {$endregion}
    function DaysInYear(Era, Year: Integer): Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days in a specified month of a specified year.</summary>
    /// <param name="Era">
    /// The era.</param>
    /// <param name="Year">
    /// The year of the era.</param>
    /// <param name="Month">
    /// The month of the year.</param>
    /// <returns>
    /// The number of days in the month.</returns>
    {$endregion}
    function DaysInMonth(Era, Year, Month: Integer): Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of days from beginning of a specified year to beginning
    /// of a specified month.</summary>
    /// <param name="Era">
    /// The era.</param>
    /// <param name="Year">
    /// The year of the era.</param>
    /// <param name="Month">
    /// The month of the year.</param>
    /// <returns>
    /// The number of days between start of the year and start of the month.</returns>
    {$endregion}
    function DaysToMonth(Era, Year, Month: Integer): Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds month and day of the month for a specified day of a specified year.</summary>
    /// <param name="Era">
    /// The era.</param>
    /// <param name="Year">
    /// The year of the era.</param>
    /// <param name="YearDay">
    /// The day of the year.</param>
    /// <param name="Month">
    /// The month of the year.</param>
    /// <param name="Day">
    /// The day of the month.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the function is succeeded, otherwise
    /// returns <see langword="false"/>.</returns>
    {$endregion}
    function DayOfYearToDayOfMonth(Era, Year, YearDay: Integer; var Month, Day: Integer): Boolean; override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class provides locale specific settings for the <see cref="TJalaliCalendar"/>
  /// class.</summary>
  /// <remarks>
  /// TJalaliCalendarSettings class collects the Jalali calendar's locale specific
  /// settings, which are required by the <see cref="TJalaliCalendar"/> class.</remarks>
  {$endregion}
  TJalaliCalendarSettings = class(TCalendarSettings)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="EraNames"/> and <see cref="ShortEraNames"/> properties for
    /// the specified era based on the given locale and calendar identifier.</summary>
    /// <param name="Era">
    /// The era that its name properties should be set.</param>
    /// <param name="Locale">
    /// The locale of the name.</param>
    /// <param name="CalendarID">
    /// The identifier of calendar system.</param>
    {$endregion}
    procedure PrepareEraName(Era: Integer; const Locale: string; CalendarID: Cardinal); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="MonthNames"/>, <see cref="ShortMonthNames"/> and
    /// <see cref="GenitiveMonthNames"/> properties based on the given locale and
    /// calendar identifier.</summary>
    /// <param name="Locale">
    /// The locale of the names.</param>
    /// <param name="CalendarID">
    /// The identifier of calendar system.</param>
    {$endregion}
    procedure PrepareMonthNames(const Locale: string; CalendarID: Cardinal); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets all writable properties based on the given locale and calendar
    /// identifier.</summary>
    /// <param name="Locale">
    /// The locale of the settings.</param>
    /// <param name="CalendarID">
    /// The identifier of calendar system.</param>
    {$endregion}
    procedure PrepareSettings(const Locale: String; CalendarID: Cardinal); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that this settings is provided for.</summary>
    /// <returns>
    /// Returns <see cref="TJalaliCalendar"/> class.</returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; override;
  end;

implementation

uses
  Math, Types, i18nWinNLS;

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
  Result := Max({0955/01/01 Jalali} -118257, inherited);
end;

class function TJalaliCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min({9999/12/29 Jalali} 3185357.99999, inherited);
end;

class function TJalaliCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := TJalaliCalendarSettings;
end;

function TJalaliCalendar.IsLeapYear(Era, Year: Integer): Boolean;
begin
  Year := ConvertYear(Year, Era, HijriEra);
  Year := ToZeroBase(HijriEra, Year);
  Result := ((((Modulus(Year - 474, 2820) + 474) + 38) * 682) mod 2816) < 682;
end;

function TJalaliCalendar.DaysInYear(Era, Year: Integer): Integer;
begin
  if IsLeapYear(Era, Year) then
    Result := 366
  else
    Result := 365;
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
  epBase, epYear: Integer;
begin
  Year := ToZeroBase(HijriEra, Year);
  epBase := Year - 474;
  epYear := 474 + Modulus(epBase, 2820);
  Result := Jalali_EPOCH + (epYear - 1) * 365
          + Floor(((epYear * 682) - 110) / 2816)
          + Floor(epBase / 2820) * 1029983
          + DaysToMonth(HijriEra, Year, Month) + (Day - 1);
end;

function TJalaliCalendar.FromJulianDay(JD: Extended;
  out Year, Month, Day: Integer): Boolean;
var
  Period, Cycle, cYear, yCycle, Aux1, Aux2, YearDay: Integer;
begin
  JD := Trunc(JD - 0.5) + 0.5;
  Period := Trunc(JD - ToJulianDay(475, 1, 1));
  Divide(Period, 1029983, Cycle, cYear);
  if cYear = 1029982 then
    yCycle := 2820
  else
  begin
    Divide(cYear, 366, Aux1, Aux2);
    yCycle := Floor(((2134 * Aux1) + (2816 * Aux2) + 2815) / 1028522) + Aux1 + 1;
  end;
  Year := yCycle + (2820 * Cycle) + 474;
  Year := FromZeroBase(HijriEra, Year);
  YearDay := Trunc(JD - ToJulianDay(Year, 1, 1)) + 1;
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
  begin // fix Windows mistakes if the user didn't fix them already
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

