{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements the base functionality of Hijri (or Islamic) calendar.
unit i18nCalHijri;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, Classes, i18nCalendar;

type

  {$region 'xmldoc'}
  /// <summary>
  /// THijriCalendarAdjustmentEvent is the type for event handlers that respond when
  /// a Hijri date need to be adjusted.</summary>
  /// <param name="Sender">
  /// The object that generated the event.</param>
  /// <param name="JulianDay">
  /// The Julian day that is subject of the adjustment.</param>
  /// <param name="NumberOfDays">
  /// The number of days that should be added or subteracted from the Julian day
  /// to have a correct Hijri date. The handler must provide a value for thid
  /// parameter.</param>
  {$endregion}
  THijriCalendarAdjustmentEvent = procedure(Sender: TObject;
    const JulianDay: Extended; var NumberOfDays: Integer) of Object;

  {$region 'xmldoc'}
  /// <summary>
  /// This class implements the Hijri (or Islamic) calendar.</summary>
  /// <remarks>
  /// THijriCalendar provides properties and methods to manimuplate dates in
  /// the Hijri calendar.
  ///
  /// The Hijri (or Islamic) calendar is purely lunar and consists of twelve
  /// alternating months of 30 and 29 days, with the final 29 day month extended
  /// to 30 days during leap years. Leap years follow a 30 year cycle and occur
  /// in years 1, 5, 7, 10, 13, 16, 18, 21, 24, 26, and 29. Days are considered
  /// to begin at sunset. The calendar begins on Friday, July 16th, 622 C.E. in
  /// the Julian calendar, Julian day 1948439.5, the day of Muhammad's flight
  /// from Mecca to Medina, with sunset on the preceding day reckoned as the
  /// first day of the first month of year 1 A.H. (Anno Hegirae). Days are
  /// numbered with Saturday as day 1, through Friday as day 7.
  ///
  /// Each cycle of 30 years thus contains 19 normal years of 354 days and 11
  /// leap years of 355, so the average length of a year is therefore
  /// <c>((19 x 354) + (11 x 355)) / 30 = 354.365</c> days, with a mean length
  /// of month of 1/12 this figure, or 29.53055 days, which closely approximates
  /// the mean synodic month (time from new Moon to next new Moon) of 29.530588
  /// days, with the calendar only slipping one day with respect to the Moon
  /// every 2525 years. Since the calendar is fixed to the Moon, not the solar
  /// year, the months shift with respect to the seasons, with each month beginning
  /// about 11 days earlier in each successive solar year.
  ///
  /// The calendar presented here is the most commonly used civil calendar in the
  /// Islamic world; for religious purposes months are defined to start with the
  /// first observation of the crescent of the new Moon.
  ///
  /// NOTE: Both description and algorithm of this calendar is adapted from
  /// http://www.fourmilab.ch/documents/calendar/.</remarks>
  {$endregion}
  THijriCalendar = class(TCalendar)
  private
    fAdjustDays: Integer;
    fOnAdjustment: THijriCalendarAdjustmentEvent;
    procedure SetHijriAdjust(Value: Integer);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// The start of the Hijri calendar in Julian days.</summary>
    {$endregion}
    const HIJRI_EPOCH = 1948439.5;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a Hijri date represented by its year, month and day
    /// components to its corresponding Julian day.</summary>
    /// <param name="Year">
    /// The year.</param>
    /// <param name="Month">
    /// The month of the year.</param>
    /// <param name="Day">
    /// The day of the month.</param>
    /// <returns>
    /// Julian day of the specified Hijri date.</returns>
    /// <seealso cref="FromJulianDay"/>
    {$endregion}
    function ToJulianDay(Year, Month, Day: Integer): Extended; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a date expressed in Julian day, to its year, month, and day
    /// components in the Hijri calendar.</summary>
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
    {$region 'xmldoc'}
    /// <summary>
    /// Adjusts a julian day before or after being converted to or from a Hijri date.</summary>
    /// <param name="JD">
    /// The date expressed in Julian day.</param>
    /// <param name="Backward">
    /// Indicates whether the day should be adjusted backward or forward. In fact,
    /// backward means the julian day is about to be converted to a Hijri date
    /// and forward means the julian day is result of conversion from a Hijri date.</param>
    /// <returns>
    /// Returns the adjusted julian day.</returns>
    {$endregion}
    function AdjustJulianDay(JD: Extended; Backward: Boolean): Extended; override;
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
    /// Returns <see cref="CAL_HIJRI"/>.</returns>
    {$endregion}
    class function CalendarID: Cardinal; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the algorithm type of the calendar system.</summary>
    /// <returns>
    /// Returns <see cref="TCalendarKind"/> of ckLunar.</returns>
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
    /// Returns <see cref="THijriCalendarSettings"/> class.</returns>
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
    /// Returns 354 for notmal and 355 for leap years.</returns>
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
    /// Copies the contents of another object to the current one.</summary>
    /// <param name="Source">
    /// The source object.</param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of days to adjust the Hijri date.
    /// This property is ignored when the <see cref="OnAdjustment"/> event is set.</summary>
    {$endregion}
    property AdjustDays: Integer read fAdjustDays write SetHijriAdjust default 0;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a Julian day is about to be converted to a Hijri date or vice versa.</summary>
    {$endregion}
    property OnAdjustment: THijriCalendarAdjustmentEvent read fOnAdjustment write fOnAdjustment;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class provides locale specific settings for the <see cref="THijriCalendar"/>
  /// class.</summary>
  /// <remarks>
  /// THijriCalendarSettings class collects the Hijri calendar's locale specific
  /// settings, which are required by the <see cref="THijriCalendar"/> class.</remarks>
  {$endregion}
  THijriCalendarSettings = class(TCalendarSettings)
  protected
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
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that this settings is provided for.</summary>
    /// <returns>
    /// Returns <see cref="THijriCalendar"/> class.</returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; override;
  end;

implementation

uses
  Math, Types, i18nWinNLS;

{ THijriCalendar }

class function THijriCalendar.CalendarID: Cardinal;
begin
  Result := CAL_HIJRI;
end;

class function THijriCalendar.CalendarKind: TCalendarKind;
begin
  Result := ckLunar;
end;

class function THijriCalendar.MinSupportedDateTime: TDateTime;
begin
  Result := Max({0001/01/01 Hijri} -466579.0, inherited);
end;

class function THijriCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min({9999/12/29 Hijri} 3076732.99999, inherited);
end;

class function THijriCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := THijriCalendarSettings;
end;

function THijriCalendar.IsLeapYear(Era, Year: Integer): Boolean;
begin
  Year := ConvertYear(Year, Era, HijriEra);
  Year := ToZeroBase(HijriEra, Year);
  Result := Modulus(((Year * 11) + 14), 30) < 11;
end;

function THijriCalendar.DaysInYear(Era: Integer; Year: Integer): Integer;
begin
  if IsLeapYear(Era, Year) then
    Result := 355
  else
    Result := 354;
end;

function THijriCalendar.DaysInMonth(Era: Integer; Year: Integer;
  Month: Integer): Integer;
begin
  if (Month < 1) or (Month > 12) then
    MonthError(Era, Year, Month);
  if Odd(Month) or ((Month = 12) and IsLeapYear(Era, Year)) then
    Result := 30
  else
    Result := 29;
end;

function THijriCalendar.ToJulianDay(Year, Month, Day: Integer): Extended;
begin
  Year := ToZeroBase(HijriEra, Year);
  Result := HIJRI_EPOCH +
          + (Year - 1) * 354 + Floor((3 + (11 * Year)) / 30)
          + (29 * (Month - 1)) + (Month div 2) + (Day - 1);
end;

function THijriCalendar.FromJulianDay(JD: Extended;
  out Year, Month, Day: Integer): Boolean;
var
  YearDay: Integer;
begin
  JD := Trunc(JD - 0.5) + 0.5;
  Year := Floor((30 * (JD - HIJRI_EPOCH) + 10646) / 10631);
  Year := FromZeroBase(HijriEra, Year);
  YearDay := Trunc(JD - ToJulianDay(Year, 1, 1)) + 1;
  Result := DayOfYearToDayOfMonth(HijriEra, Year, YearDay, Month, Day);
end;

function THijriCalendar.AdjustJulianDay(JD: Extended;
  Backward: Boolean): Extended;
var
  NumberOfDays: Integer;
begin
  NumberOfDays := AdjustDays;
  if Assigned(OnAdjustment) then
    OnAdjustment(Self, JD, NumberOfDays);
  if Backward then
    Result := JD - NumberOfDays
  else
    Result := JD + NumberOfDays;
end;

procedure THijriCalendar.SetHijriAdjust(Value: Integer);
begin
  if AdjustDays <> Value then
  begin
    fAdjustDays := Value;
    DoChange;
  end;
end;

procedure THijriCalendar.Assign(Source: TPersistent);
begin
  if Source is THijriCalendar then
    AdjustDays := THijriCalendar(Source).AdjustDays;
  inherited Assign(Source);
end;

{ THijriCalendarSettings }

class function THijriCalendarSettings.CalendarClass: TCalendarClass;
begin
  Result := THijriCalendar;
end;

procedure THijriCalendarSettings.PrepareMonthNames(const Locale: string;
  CalendarID: Cardinal);
const
  EnglishMonthNames: array[1..12] of String = (
    'Muharram', 'Safar', 'Rabi`al-Awwal', 'Rabi`ath-Thani',
    'Jumada l-Ula', 'Jumada t-Tania', 'Rajab', 'Sha`ban',
    'Ramadan', 'Shawwal', 'Dhu l-Qa`da', 'Dhu l-Hijja');
var
  M: Integer;
  LocaleScripts: String;
begin
  LocaleScripts := GetLocaleScripts(Locale);
  if (Pos('Arab;', LocaleScripts) <> 0) or (Pos('Thaa;', LocaleScripts) <> 0) then
    inherited PrepareMonthNames(Locale, CalendarID)
  else
  begin
    for M := 1 to 12 do
    begin
      MonthNames[M] := EnglishMonthNames[M];
      ShortMonthNames[M] := EnglishMonthNames[M];
      GenitiveMonthNames[M] := EnglishMonthNames[M];
    end;
  end;
end;

initialization
  CalendarTypes.Register(THijriCalendar, ['ar', 'ur-PK', 'dv-MV']);
end.

