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
/// This unit implements components for connecting to the Google online services
/// including the Google online translator.
/// </summary>
unit i18nGoogle;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Classes, SysUtils, Types, i18nHTTP, i18nJSON, i18nCore;

type

  {$region 'xmldoc'}
  /// <summary>
  /// EGoogleServiceError is the exception class for problems that occur while
  /// sending a request to a Google service.
  /// </summary>
  /// <remarks>
  /// EGoogleServiceError represents exceptions that occur when trying to execute
  /// a Google service request. These exceptions include:
  /// <list type="bullet">
  ///   <item>Attempts to send an invalid request to a Google service.</item>
  ///   <item>Failure to connect to the server of a Google service.</item>
  /// </list>
  /// </remarks>
  /// <seealso cref="TCustomGoogleService"/>
  {$endregion}
  EGoogleServiceError = class(Exception);

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the result codes of the Google service
  /// requests.
  /// </summary>
  {$endregion}
  TGoogleResultCode = (
    {$region 'xmldoc'}
    /// The request is processed successfully.
    {$endregion}
    grOK,
    {$region 'xmldoc'}
    /// The request failed to process by the Google service.
    {$endregion}
    grGoogleError,
    {$region 'xmldoc'}
    /// The connection could not be established with the Google service.
    {$endregion}
    grConnectionError
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomGoogleService is the base class for components that connect to an
  /// online Google service.
  /// </summary>
  /// <remarks>
  /// <para>
  /// Use TCustomGoogleService as a base class when defining components that
  /// need to connect to an online Google service.
  /// </para>
  /// <para>
  /// TCustomGoogleService has properties and methods to facilitate sending the
  /// requests to and retrieving the results from the Google services.
  /// </para>
  /// </remarks>
  {$endregion}
  TCustomGoogleService = class abstract(TCustomHTTP)
  private
    fAPIKey: String;
    fUserIP: String;
    fLastGoogleError: String;
    function ValidateResponse(Response: TJSONValue; out ErrorText: String): Boolean;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Embeds <see cref="APIKey"/> and <see cref="UserIP"/> into a specified URL.
    /// </summary>
    /// <param name="URL">
    /// The URL of the request.
    /// </param>
    /// <returns>
    /// The URL that has values of <see cref="APIKey"/> and <see cref="UserIP"/>
    /// as arguments.
    /// </returns>
    {$endregion}
    function Personalize(const URL: String): String;
    {$region 'xmldoc'}
    /// <summary>
    /// <para>
    /// Sends a specified request to the Google's API server (using HTTP GET method)
    /// and retrieves its result as a <see cref="TJSONValue"/> object.
    /// </para>
    ///
    /// <para>
    /// NOTE: When the return value of the method is grOK, it is the caller's
    /// responsibility to release the <paramref name="Response"/> object.
    /// </para>
    /// </summary>
    /// <param name="URL">
    /// The URL of the request. The arguments' values must be properly escaped using
    /// <see cref="EscapeURLArgs"/> function.
    /// </param>
    /// <param name="Response">
    /// The <see cref="TJSONValue"/> object that stores the result of the request.
    /// </param>
    /// <returns>
    /// Returns a <see cref="TGoogleResultCode"/> value that represents the result
    /// code of the request.
    /// </returns>
    {$endregion}
    function SendRequest(const URL: String; out Response: TJSONValue): TGoogleResultCode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the Google API key.
    /// </summary>
    {$endregion}
    property APIKey: String read fAPIKey write fAPIKey;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the IP address of the end-user on whose behalf the requests
    /// are being made.
    /// </summary>
    {$endregion}
    property UserIP: String read fUserIP write fUserIP;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the Google error message of the last failed request.
    /// </summary>
    {$endregion}
    property LastGoogleError: String read fLastGoogleError;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies different types of text for the Google
  /// translator.
  /// </summary>
  {$endregion}
  TTextFormat = (
    {$region 'xmldoc'}
    /// The text is not formatted.
    {$endregion}
    txtPlain,
    {$region 'xmldoc'}
    /// The text is HTML formatted.
    {$endregion}
    txtHTML
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomGoogleTranslator is the base class for components that connect to
  /// the Google translator.
  /// </summary>
  /// <remarks>
  /// <para>
  /// Use TCustomGoogleTranslator as a base class when defining components that
  /// need to connect to the Google translator.
  /// </para>
  ///
  /// <para>
  /// TCustomGoogleTranslator has properties and methods to facilitate automatic
  /// translation of text using the Google translator.
  /// </para>
  /// </remarks>
  {$endregion}
  TCustomGoogleTranslator = class(TCustomGoogleService)
  private
    fHostLang: String;
    fSourceLang: String;
    fTargetLang: String;
    fDetectedSourceLang: String;
    fTextFormat: TTextFormat;
    procedure SetSourceLang(const Value: String);
    procedure SetTargetLang(const Value: String);
    procedure SetHostLang(const Value: String);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the URL that instructs the Google translator to translate a specified
    /// text string.
    /// </summary>
    /// <param name="SourceText">
    /// The text to translate.
    /// </param>
    /// <returns>
    /// Returns the translation request URL for the Google translator.
    /// </returns>
    /// <seealso cref="SendRequest"/>
    {$endregion}
    function BuildRequest(const SourceText: String): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the language of the Google error messages.
    /// </summary>
    /// <remarks>
    /// If a value for HostLang property is specified, it should be one of the
    /// language codes listed in <see cref="GoogleLanguages"/> global variable.
    /// </remarks>
    /// <seealso cref="SourceLang"/>
    /// <seealso cref="TargetLang"/>
    /// <seealso cref="CultureToGoogleLang"/>
    /// <seealso cref="GoogleLangToCulture"/>
    {$endregion}
    property HostLang: String read fHostLang write SetHostLang;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the language of the source text.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The value of SourceLang property should be one of the language codes
    /// listed in <see cref="GoogleLanguages"/> global variable.
    /// </para>
    ///
    /// <para>
    /// If a language is not specified, the Google translator will attempt to
    /// identify the source language automatically.
    /// </para>
    /// </remarks>
    /// <seealso cref="TargetLang"/>
    /// <seealso cref="CultureToGoogleLang"/>
    /// <seealso cref="GoogleLangToCulture"/>
    {$endregion}
    property SourceLang: String read fSourceLang write SetSourceLang;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the language to translate the source text into.
    /// </summary>
    /// <remarks>
    /// The value of TargetLang property should be one of the language codes
    /// listed in <see cref="GoogleLanguages"/> global variable.
    /// </remarks>
    /// <seealso cref="SourceLang"/>
    /// <seealso cref="CultureToGoogleLang"/>
    /// <seealso cref="GoogleLangToCulture"/>
    {$endregion}
    property TargetLang: String read fTargetLang write SetTargetLang;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the format of the source text.
    /// </summary>
    {$endregion}
    property TextFormat: TTextFormat read fTextFormat write fTextFormat default txtPlain;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a text in the language specified by <see cref="SourceLang"/>
    /// can be translated into the language specified by <see cref="TargetLang"/>.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if <see cref="TargetLang"/> is specified, and
    /// <see cref="SourceLang"/> and <see cref="TargetLang"/> are different languages.</returns>
    /// <seealso cref="Translate"/>
    {$endregion}
    function CanTranslate: Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Translates a specified text from the source language specified by the
    /// <see cref="SourceLang"/> property into the language specified by the
    /// <see cref="TargetLang"/> property.
    /// </summary>
    /// <param name="Text">
    /// The source text to translate.
    /// </param>
    /// <param name="TranslatedText">
    /// The translated text if the method returns grOK.
    /// </param>
    /// <returns>
    /// Returns a <see cref="TGoogleResultCode"/> value that represents the result
    /// code of the translation.
    /// </returns>
    {$endregion}
    function Translate(const Text: String;
      out TranslatedText: String): TGoogleResultCode; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Translates a specified text from the source language specified by the
    /// <see cref="SourceLang"/> property into the language specified by the
    /// <see cref="TargetLang"/> property.
    /// </summary>
    /// <param name="Text">
    /// The source text to translate.
    /// </param>
    /// <returns>
    /// Returns the translated text.
    /// </returns>
    /// <exception cref="EGoogleServiceError">
    /// Occurs if the method fails to translate the source text.
    /// </exception>
    {$endregion}
    function Translate(const Text: String): String; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the language of the source text, which is automatically detected
    /// by the Google translator.
    /// </summary>
    /// <remarks>
    /// The value of DetectedSourceLang property will be one of the language
    /// codes listed in <see cref="GoogleLanguages"/> global variable.
    /// </remarks>
    /// <seealso cref="CultureToGoogleLang"/>
    /// <seealso cref="GoogleLangToCulture"/>
    {$endregion}
    property DetectedSourceLang: String read fDetectedSourceLang;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TGoogleTranslator is an interface to the Google translator service for
  /// automatic text translation.
  /// </summary>
  /// <remarks>
  /// <para>
  /// Use TGoogleTranslator to translate text from one language to another using
  /// the Google translator.
  /// </para>
  ///
  /// <para>
  /// TGoogleTranslator publishes many of the properties, events, and methods of
  /// <see cref="TCustomGoogleTranslator"/>, but does not introduce any new behavior.
  /// </para>
  /// </remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TGoogleTranslator = class(TCustomGoogleTranslator)
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the proxy configuration.
    /// </summary>
    {$endregion}
    property Proxy;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the language of the source text.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The value of SourceLang property should be one of the language codes
    /// listed in <see cref="GoogleLanguages"/> global variable.
    /// </para>
    ///
    /// <para>
    /// If a language is not specified, the Google translator will attempt to
    /// identify the source language automatically.
    /// </para>
    /// </remarks>
    /// <seealso cref="TargetLang"/>
    /// <seealso cref="CultureToGoogleLang"/>
    /// <seealso cref="GoogleLangToCulture"/>
    {$endregion}
    property SourceLang;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the language to translate the source text into.
    /// </summary>
    /// <remarks>
    /// The value of TargetLang property should be one of the language codes
    /// listed in <see cref="GoogleLanguages"/> global variable.
    /// </remarks>
    /// <seealso cref="SourceLang"/>
    /// <seealso cref="CultureToGoogleLang"/>
    /// <seealso cref="GoogleLangToCulture"/>
    {$endregion}
    property TargetLang;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the format of the source text.
    /// </summary>
    {$endregion}
    property TextFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the value of the <c>User-Agent</c> header of the HTTP request
    /// message.
    /// </summary>
    {$endregion}
    property UserAgent;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the Google API key.
    /// </summary>
    {$endregion}
    property APIKey;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the IP address of the end-user on whose behalf the requests
    /// are being made.
    /// </summary>
    {$endregion}
    property UserIP;
  end;

{$region 'xmldoc'}
/// <summary>
/// Returns the corresponding Google language code of a specified
/// <see cref="TCultureInfo"/> object.
/// </summary>
/// <param name="Culture">
/// The <see cref="TCultureInfo"/> object.
/// </param>
/// <returns>
/// The language code of the specified <see cref="TCultureInfo"/> object, which is
/// recognizable by the Google language services.
/// </returns>
/// <seealso cref="GoogleLangToCulture"/>
{$endregion}
function CultureToGoogleLang(Culture: TCultureInfo): String;

{$region 'xmldoc'}
/// <summary>
/// <para>
/// Returns the corresponding <see cref="TCultureInfo"/> object of a specified
/// Google language code.
/// </para>
///
/// <para>
/// NOTE: Some Google language codes do not have the corresponding
/// <see cref="TCultureInfo"/> object.
/// </para>
/// </summary>
/// <param name="Lang">
/// The Google language code.
/// </param>
/// <returns>
/// Returns the <see cref="TCultureInfo"/> object of the specified Google language
/// code or <see langword="nil"/> if the Google language code does not have a
/// corresponding <see cref="TCultureInfo"/> object.
/// </returns>
/// <seealso cref="CultureToGoogleLang"/>
{$endregion}
function GoogleLangToCulture(const Lang: String): TCultureInfo;

const
  {$region 'xmldoc'}
  /// <summary>
  /// Lists the language codes, which are recognizable by the Google language services.
  /// </summary>
  {$endregion}
  GoogleLanguages: array[1..107] of String = (
    'af', 'sq', 'am', 'ar', 'hy', 'az', 'eu', 'be', 'bn', 'bh', 'br', 'bg',
    'my', 'ca', 'chr', 'zh', 'zh-CN', 'zh-TW', 'co', 'hr', 'cs', 'da', 'dv',
    'nl', 'en', 'eo', 'et', 'fo', 'tl', 'fi', 'fr', 'fy', 'gl', 'ka', 'de',
    'el', 'gu', 'ht', 'iw', 'hi', 'hu', 'is', 'id', 'iu', 'ga', 'it', 'ja',
    'jw', 'kn', 'kk', 'km', 'ko', 'ku', 'ky', 'lo', 'la', 'lv', 'lt', 'lb',
    'mk', 'ms', 'ml', 'mt', 'mi', 'mr', 'mn', 'ne', 'no', 'oc', 'or', 'ps',
    'fa', 'pl', 'pt', 'pt-PT', 'pa', 'qu', 'ro', 'ru', 'sa', 'gd', 'sr', 'sd',
    'si', 'sk', 'sl', 'es', 'su', 'sw', 'sv', 'syr', 'tg', 'ta', 'tt', 'te',
    'th', 'bo', 'to', 'tr', 'uk', 'ur', 'uz', 'ug', 'vi', 'cy', 'yi', 'yo');

implementation

resourcestring
  SGoogleConnectError = 'Cannot connect to the Google service';

{ Helper Functions }

function IsGoogleLanguage(const Lang: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Lang <> '' then
    for I := Low(GoogleLanguages) to High(GoogleLanguages) do
      if SameText(GoogleLanguages[I], Lang) then
      begin
        Result := True;
        Exit;
      end;
end;

function CultureToGoogleLang(Culture: TCultureInfo): String;
var
  I: Integer;
begin
  Result := '';
  if Assigned(Culture) then
  begin
    for I := High(GoogleLanguages) downto Low(GoogleLanguages) do
      if (GoogleLanguages[I] = Culture.Locale) or
         (GoogleLanguages[I] = Culture.Language2) then
      begin
        Result := GoogleLanguages[I];
        Exit;
      end;
  end;
end;

function GoogleLangToCulture(const Lang: String): TCultureInfo;
begin
  if IsGoogleLanguage(Lang) then
    Result := CultureOf(Lang, False)
  else
    Result := nil;
end;

{ TCustomGoogleService }

function TCustomGoogleService.Personalize(const URL: String): String;
var
  Delimiter: Char;
begin
  if StrPos(PChar(URL), '?') = nil then
    Delimiter := '?'
  else
    Delimiter := '&';
  Result := URL;
  if ApiKey <> '' then
  begin
    Result := Result + Delimiter + 'key=' + EscapeURLArg(APIKey);
    Delimiter := '&';
  end;
  if UserIP <> '' then
    Result := Result + Delimiter + 'userip=' + EscapeURLArg(UserIP);
end;

function TCustomGoogleService.ValidateResponse(Response: TJSONValue;
  out ErrorText: String): Boolean;
var
  V: TJSONValue;
  ResponseStatus: Integer;
begin
  ResponseStatus := 0;
  ErrorText := '';
  if Assigned(Response) and (Response is TJSONValueObject) then
  begin
    V := TJSONValueObject(Response).Values['responseStatus'];
    if Assigned(V) and (V is TJSONValueNumber) then
      ResponseStatus := Trunc(TJSONValueNumber(V).Value);
    V := TJSONValueObject(Response).Values['responseDetails'];
    if Assigned(V) and (V is TJSONValueString) then
      ErrorText := TJSONValueString(V).Value;
  end;
  Result := (ResponseStatus = 200);
end;

function TCustomGoogleService.SendRequest(const URL: String;
  out Response: TJSONValue): TGoogleResultCode;
var
  ResponseStream: TStringStream;
begin
  Result := grConnectionError;
  ResponseStream := TStringStream.Create('', TEncoding.UTF8, False);
  try
    if HttpRequest(URL, ResponseStream) = 200 then
      Response := DecodeJSON(ResponseStream.DataString)
    else
      Response := nil;
  finally
    ResponseStream.Free;
  end;
  if Assigned(Response) then
  begin
    if not ValidateResponse(Response, fLastGoogleError) then
    begin
      FreeAndNil(Response);
      Result := grGoogleError;
    end
    else
      Result := grOK;
  end;
end;

{ TCustomGoogleTranslator }

procedure TCustomGoogleTranslator.SetHostLang(const Value: String);
begin
  if HostLang <> Value then
  begin
    if IsGoogleLanguage(Value) then
      fHostLang := Value
    else
      fHostLang := '';
  end;
end;

procedure TCustomGoogleTranslator.SetSourceLang(const Value: String);
begin
  if SourceLang <> Value then
  begin
    if IsGoogleLanguage(Value) then
      fSourceLang := Value
    else
      fSourceLang := '';
  end;
end;

procedure TCustomGoogleTranslator.SetTargetLang(const Value: String);
begin
  if TargetLang <> Value then
  begin
    if IsGoogleLanguage(Value) then
      fTargetLang := Value
    else
      fTargetLang := '';
  end;
end;

function TCustomGoogleTranslator.CanTranslate: Boolean;
begin
  Result := (SourceLang <> TargetLang) and (TargetLang <> '');
end;

function TCustomGoogleTranslator.BuildRequest(const SourceText: String): String;
const
  BaseURL = 'http://ajax.googleapis.com/ajax/services/language/translate?v=1.0';
var
  Text: String;
begin
  if TextFormat = txtPlain then
    Text := EncodeHtmlEntities(SourceText)
  else
    Text := SourceText;
  Result := BaseURL;
  Result := Result + '&q=' + EscapeURLArg(Text);
  Result := Result + '&langpair=' + EscapeURLArg(SourceLang + '|' + TargetLang);
  if TextFormat = txtHTML then
    Result := Result + '&format=html';
  if HostLang <> '' then
    Result := Result + '&hl=' + EscapeURLArg(HostLang);
  Result := Personalize(Result)
end;

function TCustomGoogleTranslator.Translate(const Text: String;
  out TranslatedText: String): TGoogleResultCode;
var
  Request: String;
  Response: TJSONValue;
  Data, V: TJSONValue;
begin
  Request := BuildRequest(Text);
  Result := SendRequest(Request, Response);
  fDetectedSourceLang := SourceLang;
  if Result = grOK then
    try
      TranslatedText := '';
      Data := TJSONValueObject(Response).Values['responseData'];
      if Assigned(Data) and (Data is TJSONValueObject) then
      begin
        V := TJSONValueObject(Data).Values['detectedSourceLanguage'];
        if Assigned(V) and (V is TJSONValueString) then
          fDetectedSourceLang := TJSONValueString(V).Value;
        V := TJSONValueObject(Data).Values['translatedText'];
        if Assigned(V) and (V is TJSONValueString) then
        begin
          TranslatedText := TJSONValueString(V).Value;
          if TextFormat = txtPlain then
            TranslatedText := DecodeHtmlEntities(TranslatedText);
        end;
      end;
    finally
      Response.Free;
    end;
end;

function TCustomGoogleTranslator.Translate(const Text: String): String;
begin
  case Translate(Text, Result) of
    grGoogleError: raise EGoogleServiceError.Create(LastGoogleError);
    grConnectionError: raise EGoogleServiceError.CreateRes(@SGoogleConnectError);
  end;
end;

end.
