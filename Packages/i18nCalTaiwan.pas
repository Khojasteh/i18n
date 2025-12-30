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
/// This unit implements the Taiwan calendar.
/// </summary>
unit i18nCalTaiwan;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, i18nCalendar, i18nCalGregorian;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This class implements the Taiwan calendar.
  /// </summary>
  /// <remarks>
  /// <para>
  /// TTaiwanCalendar provides properties and methods to manipulate dates in
  /// the Taiwan calendar as well as the Gregorian calendar.
  /// </para>
  /// <para>
  /// The Taiwan calendar is identical to Gregorian calendar, except that years
  /// are counted from 1911 C.E. (regarded as year one).
  /// </para>
  /// <para>
  /// TTaiwanCalendar supports both Common Era (C.E.) and Taiwan Era.
  /// </para>
  /// </remarks>
  {$endregion}
  TTaiwanCalendar = class(TGregorianCalendar)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// The difference between years in Taiwan Era and Common Era.
    /// </summary>
    {$endregion}
    const TAIWAN_YEAR_OFFSET = 1911;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies index of Taiwan Era.
    /// </summary>
    {$endregion}
    const TaiwanEra = 2;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the unique identifier of the calendar.
    /// </summary>
    /// <returns>
    /// Returns <see cref="CAL_TAIWAN"/>.
    /// </returns>
    {$endregion}
    class function CalendarID: Cardinal; override;
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
    /// Returns <see cref="TTaiwanCalendarSettings"/> class.
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
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class provides locale specific settings for the <see cref="TTaiwanCalendar"/>
  /// class.
  /// </summary>
  /// <remarks>
  /// TTaiwanCalendarSettings class collects the Taiwan calendar's locale specific
  /// settings, which are required by the <see cref="TTaiwanCalendar"/> class.
  /// </remarks>
  {$endregion}
  TTaiwanCalendarSettings = class(TGregorianCalendarSettings)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="EraNames"/> and <see cref="ShortEraNames"/> properties for
    /// the specified era based on the given locale and calendar identifier.
    /// </summary>
    /// <param name="Era">
    /// The era that its name properties should be set.
    /// </param>
    /// <param name="Locale">
    /// The locale of the name.
    /// </param>
    /// <param name="CalendarID">
    /// The identifier of calendar system.
    /// </param>
    {$endregion}
    procedure PrepareEraName(Era: Integer; const Locale: String; CalendarID: Cardinal); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that this settings is provided for.
    /// </summary>
    /// <returns>
    /// Returns <see cref="TTaiwanCalendar"/> class.
    /// </returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; override;
  end;

implementation

uses
  Math, Types, i18nWinNLS, Windows;

{ TTaiwanCalendar }

class function TTaiwanCalendar.CalendarID: Cardinal;
begin
  Result := CAL_TAIWAN;
end;

class function TTaiwanCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := TTaiwanCalendarSettings;
end;

class function TTaiwanCalendar.MaxEra: Integer;
begin
  Result := TaiwanEra;
end;

class function TTaiwanCalendar.MinSupportedDateTime: TDateTime;
begin
  Result := Max({0001/01/01 Taiwan} 4384.0, inherited);
end;

class function TTaiwanCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min({9999/12/31 Taiwan} 3656443.99999, inherited);
end;

function TTaiwanCalendar.ConvertYear(Year, FromEra, ToEra: Integer): Integer;
begin
  if Year = 0 then
    YearError(FromEra, 0)
  else if FromEra <> ToEra then
  begin
    if FromEra = TaiwanEra then
      Year := OffsetYear(Year, +TAIWAN_YEAR_OFFSET, FromEra, CommonEra)
    else if FromEra <> CommonEra then
      EraError(FromEra);
    if ToEra = TaiwanEra then
      Year := OffsetYear(Year, -TAIWAN_YEAR_OFFSET, CommonEra, ToEra)
    else if ToEra <> CommonEra then
      EraError(ToEra);
  end
  else if not (FromEra in [CommonEra, TaiwanEra]) then
    EraError(FromEra);
  Result := Year;
end;

{ TTaiwanCalendarSettings }

class function TTaiwanCalendarSettings.CalendarClass: TCalendarClass;
begin
  Result := TTaiwanCalendar;
end;

procedure TTaiwanCalendarSettings.PrepareEraName(Era: Integer;
  const Locale: String; CalendarID: Cardinal);
begin
  if Era = TTaiwanCalendar.CommonEra then
    inherited PrepareEraName(Era, Locale, CAL_GREGORIAN)
  else
    inherited PrepareEraName(Era, Locale, CalendarID);
end;

initialization
  CalendarTypes.Register(TTaiwanCalendar, ['zh-TW']);
end.
