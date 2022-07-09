{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements the Thai Buddhist calendar.
unit i18nCalThai;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, i18nCalendar, i18nCalGregorian;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This class implements the Thai Buddhist Era calendar.</summary>
  /// <remarks>
  /// TTaiwanCalendar provides properties and methods to manimuplate dates in
  /// the Thai Buddhist Era calendar as well as the Gregorian calendar.
  ///
  /// The Thai calendar is identical to Gregorian calendar, except that years
  /// are counted from 543 B.C.E. (regarded as year one).
  ///
  /// TKoreanCalendar supports both Common Era (C.E.) and Thai Buddhist Era.</remarks>
  {$endregion}
  TThaiCalendar = class(TGregorianCalendar)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// The difference between years in Thai Era and Common Era.</summary>
    {$endregion}
    const THAI_YEAR_OFFSET = -543;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies index of Thai Era.</summary>
    {$endregion}
    const ThaiEra = 2;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the unique identifier of the calendar.</summary>
    /// <returns>
    /// Returns <see cref="CAL_THAI"/>.</returns>
    {$endregion}
    class function CalendarID: Cardinal; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of eras that the calendar class supports.</summary>
    /// <returns>
    /// Returns 2.</returns>
    {$endregion}
    class function MaxEra: Integer; override;
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
    /// Returns <see cref="TThaiCalendarSettings"/> class.</returns>
    {$endregion}
    class function SettingsClass: TCalendarSettingsClass; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a year from one era to another.</summary>
    /// <param name="Year">
    /// The year to convert.</param>
    /// <param name="FromEra">
    /// The source era.</param>
    /// <param name="ToEra">
    /// The target era.</param>
    /// <returns>
    /// The year expressed in the target era.</returns>
    {$endregion}
    function ConvertYear(Year, FromEra, ToEra: Integer): Integer; override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class provides locale specific settings for the <see cref="TThaiCalendar"/>
  /// class.</summary>
  /// <remarks>
  /// TThaiCalendarSettings class collects the Thai calendar's locale specific
  /// settings, which are required by the <see cref="TThaiCalendar"/> class.</remarks>
  {$endregion}
  TThaiCalendarSettings = class(TGregorianCalendarSettings)
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
    procedure PrepareEraName(Era: Integer; const Locale: String; CalendarID: Cardinal); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that this settings is provided for.</summary>
    /// <returns>
    /// Returns <see cref="TThaiCalendar"/> class.</returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; override;
  end;

implementation

uses
  Math, Types, i18nWinNLS;

{ TThaiCalendar }

class function TThaiCalendar.CalendarID: Cardinal;
begin
  Result := CAL_THAI;
end;

class function TThaiCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := TThaiCalendarSettings;
end;

class function TThaiCalendar.MaxEra: Integer;
begin
  Result := ThaiEra;
end;

class function TThaiCalendar.MinSupportedDateTime: TDateTime;
begin
  Result := Max({0001/01/01 Thai} -891920.0, inherited);
end;

class function TThaiCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min({9999/12/31 Thai} 2760139.99999, inherited)
end;

function TThaiCalendar.ConvertYear(Year, FromEra, ToEra: Integer): Integer;
begin
  if Year = 0 then
    YearError(FromEra, 0)
  else if FromEra <> ToEra then
  begin
    if FromEra = ThaiEra then
      Year := OffsetYear(Year, +THAI_YEAR_OFFSET, FromEra, CommonEra)
    else if FromEra <> CommonEra then
      EraError(FromEra);
    if ToEra = ThaiEra then
      Year := OffsetYear(Year, -THAI_YEAR_OFFSET, CommonEra, ToEra)
    else if ToEra <> CommonEra then
      EraError(ToEra);
  end
  else if not (FromEra in [CommonEra, ThaiEra]) then
    EraError(FromEra);
  Result := Year;
end;

{ TThaiCalendarSettings }

class function TThaiCalendarSettings.CalendarClass: TCalendarClass;
begin
  Result := TThaiCalendar;
end;

procedure TThaiCalendarSettings.PrepareEraName(Era: Integer;
  const Locale: String; CalendarID: Cardinal);
begin
  if Era = TThaiCalendar.CommonEra then
    inherited PrepareEraName(Era, Locale, CAL_GREGORIAN)
  else
    inherited PrepareEraName(Era, Locale, CalendarID);
end;

initialization
  CalendarTypes.Register(TThaiCalendar, ['th-TH']);
end.
