{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements the Japanese Emperor Era calendar.
unit i18nCalJapanese;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, i18nCalendar, i18nCalGregorian;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This class implements the Japanese Emperor Era calendar.</summary>
  /// <remarks>
  /// TJapaneseCalendar provides properties and methods to manimuplate dates in
  /// the Japanese Emperor Era calendar as well as the Gregorian calendar.
  ///
  /// The Japanese era calendar scheme is a common calendar scheme used in Japan,
  /// which identifies a year by the combination of the Japanese era name and the
  /// year number within the era. For example, the year 2011 is Heisei 23.
  ///
  /// This calendar supports Common Era (C.E.) as well as Japanese Emperor Eras.</remarks>
  {$endregion}
  TJapaneseCalendar = class(TGregorianCalendar)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the era that the specified year expressed in Common Era is
    /// within it. If the value of <see cref="DefaultEra"/> is
    /// <see cref="CommonEra"/>, this method returns <see cref="CommonEra"/>
    /// anyway.</summary>
    /// <param name="Year">
    /// The year expressed in Common Era.</param>
    /// <returns>
    /// The best era for expressing the year.</returns>
    {$endregion}
    function BestEraOfBaseEraYear(Year: Integer): Integer; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies index of Meiji Era.</summary>
    {$endregion}
    const MeijiEra = 2;
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies index of Taisho Era.</summary>
    {$endregion}
    const TaishoEra = 3;
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies index of Showa Era.</summary>
    {$endregion}
    const ShowaEra = 4;
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies index of Heisei Era.</summary>
    {$endregion}
    const HeiseiEra = 5;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the unique identifier of the calendar.</summary>
    /// <returns>
    /// Returns <see cref="CAL_JAPAN"/>.</returns>
    {$endregion}
    class function CalendarID: Cardinal; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of eras that the calendar class supports.</summary>
    /// <returns>
    /// The maximum number of eras.</returns>
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
    /// Returns <see cref="TJapaneseCalendarSettings"/> class.</returns>
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
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a <see cref="TDateTime"/> value that represents the first expressible
    /// moment of a specified era.</summary>
    /// <param name="Era">
    /// The era.</param>
    /// <returns>
    /// The <see cref="TDateTime"/> value of the first moment.</returns>
    /// <seealso cref="EndOfEra"/>
    {$endregion}
    function StartOfEra(Era: Integer): TDateTime; override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class provides locale specific settings for the <see cref="TJapaneseCalendar"/>
  /// class.</summary>
  /// <remarks>
  /// TJapaneseCalendarSettings class collects the Japanese calendar's locale specific
  /// settings, which are required by the <see cref="TJapaneseCalendar"/> class.</remarks>
  {$endregion}
  TJapaneseCalendarSettings = class(TGregorianCalendarSettings)
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
    {$region 'xmldoc'}
    /// <summary>
    /// Sets <see cref="LongDateFormat"/>, <see cref="ShortDateFormat"/>,
    /// <see cref="YearMonthFormat"/>, <see cref="MonthDayFormat"/> and
    /// <see cref="YearFormat"/> properties based on the given locale and calendar
    /// identifier.</summary>
    /// <param name="Locale">
    /// The locale of the formats.</param>
    /// <param name="CalendarID">
    /// The identifier of calendar system.</param>
    {$endregion}
    procedure PrepareDateFormats(const Locale: String; CalendarID: Cardinal); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the calendar system that this settings is provided for.</summary>
    /// <returns>
    /// Returns <see cref="TJapaneseCalendar"/> class.</returns>
    {$endregion}
    class function CalendarClass: TCalendarClass; override;
  end;

implementation

uses
  Math, Types, i18nWinNLS, Windows, Registry, Classes;

{ Japanese Eras }

type
  // Stores information about a single Japanese era.
  TJapaneseEraInfo = record
    Start: TDateYMD;
    StartDate: TDate;
    NativeName: String;
    ShortNativeName: String;
    EnglishName: String;
    ShortEnglishName: String;
  end;

var
  // Stores minimum supproted TDateTime value for Japanese calendar.
  JapaneseMinDateTime: TDateTime;
  // Stores maximum supproted TDateTime value for Japanese calendar.
  JapaneseMaxDateTime: TDateTime;
  // Lists information about the Japanese eras.
  JapaneseEraInfo: array of TJapaneseEraInfo;

procedure ExtractJapaneseEras(Info: TStrings);
var
  I: Integer;
  S, E: PChar;
  Num: String;
begin
  SetLength(JapaneseEraInfo, Info.Count);
  for I := 0 to Info.Count - 1 do
  begin
    S := PChar(Info[I]);
    with JapaneseEraInfo[I] do
    begin
      // Start.Year
      E := StrScan(S, ' ');
      SetString(Num, S, E - S);
      Start.Year := StrToInt(Num);
      S := E + 1;
      // Start.Month
      E := StrScan(S, ' ');
      SetString(Num, S, E - S);
      Start.Month := StrToInt(Num);
      S := E + 1;
      // Start.Day
      E := StrScan(S, '=');
      SetString(Num, S, E - S);
      Start.Day := StrToInt(Num);
      S := E + 1;
      // NativeName
      E := StrScan(S, '_');
      SetString(NativeName, S, E - S);
      S := E + 1;
      // ShortNativeName
      E := StrScan(S, '_');
      SetString(ShortNativeName, S, E - S);
      S := E + 1;
      // EnglishName
      E := StrScan(S, '_');
      SetString(EnglishName, S, E - S);
      S := E + 1;
      // ShortEnglishName
      SetString(ShortEnglishName, S, StrLen(S));
    end;
  end;
end;

procedure GetJapaneseErasFromKnownInfo;
const
  KnownInfo = '1868 01 01=明治_明_Meiji_M'  + #13#10
            + '1912 07 30=大正_大_Taisho_T' + #13#10
            + '1926 12 25=昭和_昭_Showa_S'  + #13#10
            + '1989 01 08=平成_平_Heisei_H' ;
var
  Info: TStrings;
begin
  Info := TStringList.Create;
  try
    Info.Text := KnownInfo;
    ExtractJapaneseEras(Info);
  finally
    Info.Free;
  end;
end;

function GetJapaneseErasFromRegistry: Boolean;
const
  ErasKey = '\SYSTEM\CurrentControlSet\Control\Nls\Calendars\Japanese\Eras';
var
  R: TRegistry;
  Info: TStrings;
  I: Integer;
begin
  Result := False;
  try
    R := TRegistry.Create(KEY_QUERY_VALUE);
    try
      R.RootKey := HKEY_LOCAL_MACHINE;
      if R.OpenKeyReadOnly(ErasKey) then
      begin
        Info := TStringList.Create;
        try
          R.GetValueNames(Info);
          for I := 0 to Info.Count - 1 do
            Info[I] := Info[I] + '=' + R.ReadString(Info[I]);
          ExtractJapaneseEras(Info);
          Result := (Info.Count <> 0);
        finally
          Info.Free;
        end;
      end;
    finally
      R.Free;
    end;
  except
    Result := False;
  end;
end;

procedure InitianlizeJapaneseEraInfo;
var
  I: Integer;
begin
  if not GetJapaneseErasFromRegistry then
    GetJapaneseErasFromKnownInfo;
  for I := 0 to Length(JapaneseEraInfo) - 1 do
    with JapaneseEraInfo[I] do
      StartDate := EncodeDate(Start.Year, Start.Month, Start.Day);
  JapaneseMinDateTime := JapaneseEraInfo[0].StartDate;
  with JapaneseEraInfo[Length(JapaneseEraInfo) - 1].Start do
    JapaneseMaxDateTime := EncodeDate(Year + 99, 1, 1) - 0.00001;
end;

procedure FinalizeJapaneseEraInfo;
begin
  SetLength(JapaneseEraInfo, 0);
end;

{ TJapaneseCalendar }

class function TJapaneseCalendar.CalendarID: Cardinal;
begin
  Result := CAL_JAPAN;
end;

class function TJapaneseCalendar.SettingsClass: TCalendarSettingsClass;
begin
  Result := TJapaneseCalendarSettings;
end;

class function TJapaneseCalendar.MaxEra: Integer;
begin
  Result := Length(JapaneseEraInfo) + CommonEra;
end;

class function TJapaneseCalendar.MinSupportedDateTime: TDateTime;
begin
  Result := Max(JapaneseMinDateTime, inherited);
end;

class function TJapaneseCalendar.MaxSupportedDateTime: TDateTime;
begin
  Result := Min(JapaneseMaxDateTime, inherited);
end;

function TJapaneseCalendar.ConvertYear(Year, FromEra, ToEra: Integer): Integer;
begin
  if Year = 0 then
    YearError(FromEra, Year)
  else if FromEra <> ToEra then
  begin
    if (FromEra > CommonEra) and (FromEra <= MaxEra) then
      Year := OffsetYear(Year, +(JapaneseEraInfo[FromEra-2].Start.Year - 1), FromEra, CommonEra)
    else if FromEra <> CommonEra then
      EraError(FromEra);
    if (ToEra > CommonEra) and (ToEra <= MaxEra) then
      Year := OffsetYear(Year, -(JapaneseEraInfo[ToEra-2].Start.Year - 1), CommonEra, ToEra)
    else if ToEra <> CommonEra then
      EraError(FromEra);
  end
  else if (FromEra < CommonEra) or (FromEra > MaxEra) then
    EraError(FromEra);
  Result := Year;
end;

function TJapaneseCalendar.StartOfEra(Era: Integer): TDateTime;
begin
  if (Era > CommonEra) and (Era <= MaxEra) then
    Result := JapaneseEraInfo[Era-2].StartDate
  else
    Result := inherited StartOfEra(Era);
end;

function TJapaneseCalendar.BestEraOfBaseEraYear(Year: Integer): Integer;
var
  I: Integer;
begin
  if DefaultEra = CommonEra then
    Result := inherited BestEraOfBaseEraYear(Year)
  else
  begin
    for I := Length(JapaneseEraInfo) - 1 downto 1 do
      if Year >= JapaneseEraInfo[I].Start.Year then
      begin
        Result := I + 2;
        Exit;
      end;
    Result := MeijiEra;
  end;
end;

{ TJapaneseCalendarSettings }

class function TJapaneseCalendarSettings.CalendarClass: TCalendarClass;
begin
  Result := TJapaneseCalendar;
end;

procedure TJapaneseCalendarSettings.PrepareEraName(Era: Integer;
  const Locale: String; CalendarID: Cardinal);
begin
  if Era = TJapaneseCalendar.CommonEra then
    inherited PrepareEraName(Era, Locale, CAL_GREGORIAN)
  else
  begin
    if Pos('Jpan;', GetLocaleScripts(Locale)) <> 0 then
    begin
      EraNames[Era] := JapaneseEraInfo[Era-2].NativeName;
      ShortEraNames[Era] := JapaneseEraInfo[Era-2].ShortNativeName
    end
    else
    begin
      EraNames[Era] := JapaneseEraInfo[Era-2].EnglishName;
      ShortEraNames[Era] := JapaneseEraInfo[Era-2].ShortEnglishName;
    end;
  end;
end;

procedure TJapaneseCalendarSettings.PrepareDateFormats(const Locale: String;
  CalendarID: Cardinal);
begin
  if SameText(Locale, 'ja-JP') then
    inherited PrepareDateFormats(Locale, CalendarID)
  else
  begin
    LongDateFormat := 'gg y, MMMM d';
    ShortDateFormat := 'gg y/M/d';
    YearMonthFormat := 'gg y, MMMM';
    MonthDayFormat := 'MMMM, d';
  end;
  YearFormat := 'gg y';
end;

initialization
  InitianlizeJapaneseEraInfo;
  CalendarTypes.Register(TJapaneseCalendar, ['ja-JP']);
finalization
  FinalizeJapaneseEraInfo;
end.
