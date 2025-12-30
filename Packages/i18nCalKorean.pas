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
/// This unit implements the Korean Tangun Era calendar.
/// </summary>
unit i18nCalKorean;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, i18nCalendar, i18nCalGregorian;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This class implements the Korean Tangun Era calendar.
  /// </summary>
  /// <remarks>
  /// <para>
  /// TKoreanCalendar provides properties and methods to manipulate dates in
  /// the Korean Tangun Era calendar as well as the Gregorian calendar.
  /// </para>
  /// <para>
  /// The Korean calendar is identical to the Gregorian calendar, except that years
  /// are counted from 2333 B.C.E. (regarded as year one).
  /// </para>
  /// <para>
  /// TKoreanCalendar supports both the Common Era (C.E.) and the Korean Tangun Era.
  /// </para>
  /// </remarks>
  {$endregion}
  TKoreanCalendar = class(TGregorianCalendar)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// The difference between years in the Korean Era and the Common Era.
    /// </summary>
    {$endregion}
    const KOREAN_YEAR_OFFSET = -2333;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies the index of the Korean Era.
    /// </summary>
    {$endregion}
    const KoreanEra = 2;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the unique identifier of the calendar.
    /// </summary>
    /// <returns>
    /// Returns <see cref="CAL_KOREA"/>.
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
    /// Returns the <see cref="TCalendarSettings"/> class that provides locale-
    /// specific settings for the calendar.
    /// </summary>
    /// <returns>
    /// Returns the <see cref="TKoreanCalendarSettings"/> class.
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
  /// This class provides locale-specific settings for the <see cref="TKoreanCalendar"/>
  /// class.
  /// </summary>
  /// <remarks>
  /// The TKoreanCalendarSettings class collects the Korean calendar's locale-specific
  /// settings, which are required by the <see cref="TKoreanCalendar"/> class.
  /// </remarks>
  {$endregion}
  TKoreanCalendarSettings = class(TGregorianCalendarSettings)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="EraNames"/> and <see cref="ShortEraNames"/> properties for
    /// the specified era based on the given locale and calendar identifier.
    /// </summary>
    /// <param name="Era">
    /// The era for which the name properties should be set.
    /// </param>
    /// <param name="Locale">
    /// The locale of the name.
    /// </param>
    /// <param name="CalendarID">
    /// The identifier of the calendar system.
    /// </param>
    {$endregion}
    procedure PrepareEraName(Era: Integer; const Locale: String; CalendarID: Cardinal); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that this settings is provided for.
    /// </summary>
    /// <returns>
    /// Returns the <see cref="TKoreanCalendar"/> class.
    /// </returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; override;
  end;

implementation

uses
  Math, Types, i18nWinNLS;

{ TKoreanCalendar }

class function TKoreanCalendar.CalendarID: Cardinal;
begin
  Result := CAL_KOREA;
end;

class function TKoreanCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := TKoreanCalendarSettings;
end;

class function TKoreanCalendar.MaxEra: Integer;
begin
  Result := KoreanEra;
end;

class function TKoreanCalendar.MinSupportedDateTime: TDateTime;
begin
  Result := Max({0001/01/01 Korean} -1545704.0, inherited);
end;

class function TKoreanCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min({9999/12/31 Korean} 2106355.99999, inherited);
end;

function TKoreanCalendar.ConvertYear(Year, FromEra, ToEra: Integer): Integer;
begin
  if Year = 0 then
    YearError(FromEra, 0)
  else if FromEra <> ToEra then
  begin
    if FromEra = KoreanEra then
      Year := OffsetYear(Year, +KOREAN_YEAR_OFFSET, FromEra, CommonEra)
    else if FromEra <> CommonEra then
      EraError(FromEra);
    if ToEra = KoreanEra then
      Year := OffsetYear(Year, -KOREAN_YEAR_OFFSET, CommonEra, ToEra)
    else if ToEra <> CommonEra then
      EraError(ToEra);
  end
  else if not (FromEra in [CommonEra, KoreanEra]) then
    EraError(FromEra);
  Result := Year;
end;

{ TKoreanCalendarSettings }

class function TKoreanCalendarSettings.CalendarClass: TCalendarClass;
begin
  Result := TKoreanCalendar;
end;

procedure TKoreanCalendarSettings.PrepareEraName(Era: Integer;
  const Locale: String; CalendarID: Cardinal);
begin
  if Era = TKoreanCalendar.CommonEra then
    inherited PrepareEraName(Era, Locale, CAL_GREGORIAN)
  else
    inherited PrepareEraName(Era, Locale, CalendarID);
end;

initialization
  CalendarTypes.Register(TKoreanCalendar, ['ko-KR']);
end.
