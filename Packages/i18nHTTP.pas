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
/// This unit implements base classes and components for sending HTTP requests.
/// </summary>
unit i18nHTTP;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Classes, SysUtils, WinINet;

type

  {$region 'xmldoc'}
  /// <summary>
  /// TURLParts represents the component parts that build a URL.
  /// </summary>
  {$endregion}
  TURLParts = record
    {$region 'xmldoc'}
    /// The value that indicates the Internet protocol scheme.
    {$endregion}
    Scheme: Word;
    {$region 'xmldoc'}
    /// The scheme name
    {$endregion}
    SchemeName: String;
    {$region 'xmldoc'}
    /// The host name
    {$endregion}
    HostName: String;
    {$region 'xmldoc'}
    /// The port number
    {$endregion}
    Port: Integer;
    {$region 'xmldoc'}
    /// The user name
    {$endregion}
    UserName: String;
    {$region 'xmldoc'}
    /// The password
    {$endregion}
    Password: String;
    {$region 'xmldoc'}
    /// The path
    {$endregion}
    Path: String;
    {$region 'xmldoc'}
    /// The extra information (for example, <c>?something</c> or <c>#something</c>).
    {$endregion}
    ExtraInfo: String;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the access types for a TCP/IP network
  /// connection.
  /// </summary>
  {$endregion}
  TProxyType = (
    {$region 'xmldoc'}
    /// <summary>
    /// Resolves all host names locally.
    /// </summary>
    {$endregion}
    ptDirect,
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the proxy or direct configuration from the registry.
    /// </summary>
    {$endregion}
    ptAuto,
    {$region 'xmldoc'}
    /// <summary>
    /// Passes requests to the specified proxy server unless a proxy bypass
    /// list is supplied and the name to be resolved bypasses the proxy. In
    /// this case, resolves the name locally.
    /// </summary>
    {$endregion}
    ptManual
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TProxyConfig provides proxy configuration for a TCP/IP network connection.
  /// </summary>
  {$endregion}
  TProxyConfig = class(TPersistent)
  private
    fType: TProxyType;
    fHost: String;
    fPort: Word;
    fBypass: TStrings;
    fOnChange: TNotifyEvent;
    procedure SetType(Value: TProxyType);
    procedure SetHost(const Value: String);
    procedure SetPort(Value: Word);
    procedure SetBypass(Value: TStrings);
    function IsStoredBypass: Boolean;
    procedure BypassChanged(Sender: TObject);
    function GetIOType: Cardinal;
    function GetIOName: PChar;
    function GetIOBypass: PChar;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChange"/> event.
    /// </summary>
    {$endregion}
    procedure DoChange; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides the value for the <c>dwAccessType</c> parameter of the InternetOpen
    /// Windows API function.
    /// </summary>
    {$endregion}
    property IOType: Cardinal read GetIOType;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides the value for the <c>lpszProxyName</c> parameter of the InternetOpen
    /// Windows API function.
    /// </summary>
    {$endregion}
    property IOName: PChar read GetIOName;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides the value for the <c>lpszProxyBypass</c> parameter of the InternetOpen
    /// Windows API function.
    /// </summary>
    {$endregion}
    property IOBypass: PChar read GetIOBypass;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when proxy configuration changes.
    /// </summary>
    {$endregion}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the proxy configuration from another object.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the type of access required.
    /// </summary>
    {$endregion}
    property AccessType: TProxyType read fType write SetType default ptAuto;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the host name of the proxy server when <see cref="AccessType"/>
    /// is ptManual.
    /// </summary>
    {$endregion}
    property Host: String read fHost write SetHost;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the port number of the proxy server when <see cref="AccessType"/>
    /// is ptManual.
    /// </summary>
    {$endregion}
    property Port: Word read fPort write SetPort default 0;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the optional list of host names or IP addresses, or both, that
    /// should not be routed through the proxy when <see cref="AccessType"/> is
    /// ptManual. The list can contain wildcards.
    /// </summary>
    {$endregion}
    property Bypass: TStrings read fBypass write SetBypass stored IsStoredByPass;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomHTTP is the base class for components that are based on HTTP get or post
  /// requests.
  /// </summary>
  /// <remarks>
  /// Use TCustomHTTP as a base class when defining components that need to connect
  /// to a network service via HTTP get or post requests.
  /// </remarks>
  {$endregion}
  TCustomHTTP = class abstract(TComponent)
  private
    fProxy: TProxyConfig;
    fUserAgent: String;
    fReferer: String;
    hInternet: HINTERNET;
    hConnection: HINTERNET;
    SessionDetails: TURLParts;
    procedure SetProxy(Value: TProxyConfig);
    procedure SetUserAgent(const Value: String);
    function IsStoredUserAgent: Boolean;
    procedure ConfigChanged(Sender: TObject);
    function PrepareSession(const Details: TURLParts): Boolean;
    procedure UnprepareSession;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Sends a GET or POST request to a server.
    /// </summary>
    /// <param name="URL">
    /// The URL of the request.
    /// </param>
    /// <param name="Response">
    /// Receives the content of the response from the server.
    /// </param>
    /// <param name="PostData">
    /// The optional data to send to the server. If this parameter is omitted or
    /// an empty string is used, HttpRequest uses the GET method to send the HTTP
    /// request. Otherwise, it uses the POST method.
    /// </param>
    /// <returns>
    /// Returns the HTTP status code or zero if the connection is not established
    /// at all.
    /// </returns>
    {$endregion}
    function HttpRequest(const URL: String; Response: TStream;
      const PostData: String = ''): Cardinal;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the proxy configuration.
    /// </summary>
    {$endregion}
    property Proxy: TProxyConfig read fProxy write SetProxy;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the value of the <c>User-Agent</c> header of the HTTP request
    /// message.
    /// </summary>
    {$endregion}
    property UserAgent: String read fUserAgent write SetUserAgent stored IsStoredUserAgent;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the value of the <c>Referer</c> header of the HTTP request
    /// message.
    /// </summary>
    {$endregion}
    property Referer: String read fReferer write fReferer;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the component.
    /// </summary>
    /// <param name="AOwner">
    /// The component that owns this instance.
    /// </param>
    {$endregion}
    constructor Create(AOwner: TComponent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the component's instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
  end;

{$region 'xmldoc'}
/// <summary>
/// Converts the special HTML characters to HTML entities.
/// </summary>
/// <param name="Str">
/// The string containing the HTML special characters.
/// </param>
/// <returns>
/// The string that is safe to render as HTML text.
/// </returns>
/// <seealso cref="DecodeHtmlEntities"/>
{$endregion}
function EncodeHtmlEntities(const Str: String): String;

{$region 'xmldoc'}
/// <summary>
/// Converts the HTML entities to normal characters.
/// </summary>
/// <param name="Str">
/// The string containing the HTML entities.
/// </param>
/// <returns>
/// The string that is safe to render as plain text.
/// </returns>
/// <seealso cref="EncodeHtmlEntities"/>
{$endregion}
function DecodeHtmlEntities(const Str: String): String;

{$region 'xmldoc'}
/// <summary>
/// Escapes a string value, so that it can be used safely in a URL as an argument
/// value.
/// </summary>
/// <param name="Str">
/// The string to escape.
/// </param>
/// <returns>
/// The escaped string.
/// </returns>
{$endregion}
function EscapeURLArg(const Str: String): String;

{$region 'xmldoc'}
/// <summary>
/// Disassembles a URL into its component parts.
/// </summary>
/// <param name="URL">
/// The URL to be cracked.
/// </param>
/// <param name="Components">
/// The component parts of the cracked URL.
/// </param>
{$endregion}
procedure CrackURL(const URL: String; out Components: TURLParts);

implementation

const
  DefaultProxyBypass = '<local>';
  DefaultUserAgent   = 'Mozila/5.0';

const
  CodedChars: array[1..60] of WideString = (
    'nbsp', 'copy', 'reg', 'trade', 'sup1', 'sup2', 'sup3', 'quot', 'amp',
    'lt', 'gt', 'ndash', 'mdash', 'lsquo', 'rsquo', 'ldquo', 'rdquo',
    'bull', 'dagger', 'Dagger', 'prime', 'Prime', 'lsaquo', 'rsaquo',
    'tilde', 'circ', 'spades', 'clubs', 'hearts', 'diams', 'loz', 'larr',
    'rarr', 'uarr', 'darr', 'harr', 'not', 'frac14', 'frac12', 'frac34',
    'plusmn', 'laquo', 'raquo', 'deg', 'ordf', 'ordm', 'iexcl', 'iquest',
    'euro', 'cent', 'pound', 'yen', 'curren', 'sect', 'para', 'macr',
    'middot', 'micro', 'times', 'divide');
  SpecialChars: array[1..60] of WideChar = (
    #$00A0, #$00A9, #$00AE, #$2122, #$00B9, #$00B2, #$00B3, #$0022, #$0026, #$003C,
    #$003E, #$2013, #$2014, #$2018, #$2019, #$201C, #$201D, #$2022, #$2020, #$2021,
    #$2030, #$2031, #$2039, #$203A, #$02DC, #$02C6, #$2660, #$2663, #$2665, #$2666,
    #$25CA, #$2190, #$2192, #$2191, #$2193, #$2194, #$00AC, #$00BC, #$00BD, #$00BE,
    #$00B1, #$00AB, #$00BB, #$00B0, #$00AB, #$00C6, #$00A1, #$00BF, #$20AC, #$00A2,
    #$00A3, #$00A5, #$00A4, #$00A7, #$00B6, #$00AF, #$00B7, #$00B5, #$00D7, #$00F7);

{ Helper Functions }

function EncodeWebChar(C: Char; out Code: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(SpecialChars) to High(SpecialChars) do
    if SpecialChars[I] = C then
    begin
      Code := CodedChars[I];
      Result := True;
      Exit;
    end;
end;

function DecodeWebChar(const Code: String; out C: Char): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Code <> '' then
  begin
    if Code[1] = '#' then
    begin
      C := #0;
      for I := 2 to Length(Code) do
        if (Code[I] >= '0') and (Code[I] <= '9') then
          C := Char(Ord(C) * 10 + Ord(Code[I]) - Ord('0'))
        else
          Exit;
      Result := (C <> #0);
    end
    else
      for I := Low(CodedChars) to High(CodedChars) do
        if CodedChars[I] = Code then
        begin
          C := SpecialChars[I];
          Result := True;
          Exit;
        end;
  end;
end;

function DecodeHtmlEntities(const Str: String): String;
var
  P, S, E: PChar;
  Code: String;
begin
  SetString(Result, nil, Length(Str));
  S := PChar(Str);
  P := PChar(Result);
  while S^ <> #0 do
  begin
    P^ := S^;
    if S^ = '&' then
    begin
      E := S + 1;
      while (E^ <> #0) and (E^ <> ';') do
        Inc(E);
      if E^ = ';' then
      begin
        SetString(Code, S + 1, E - S - 1);
        if DecodeWebChar(Code, P^) then
          S := E;
      end;
    end;
    Inc(P);
    Inc(S);
  end;
  SetLength(Result, P - PChar(Result));
end;

function EncodeHtmlEntities(const Str: String): String;
var
  Code: String;
  P, S: PChar;
  I: Integer;
begin
  SetString(Result, nil, 8 * Length(Str));
  S := PChar(Str);
  P := PChar(Result);
  while S^ <> #0 do
  begin
    P^ := S^;
    if EncodeWebChar(S^, Code) then
    begin
      P^ := '&';
      Inc(P);
      for I := 1 to Length(Code) do
      begin
        P^ := Code[I];
        Inc(P);
      end;
      P^ := ';';
    end;
    Inc(P);
    Inc(S);
  end;
  SetLength(Result, P - PChar(Result));
end;

function EscapeURLArg(const Str: String): String;
var
  P: PAnsiChar;
begin
  Result := '';
  P := PAnsiChar(UTF8Encode(Str));
  while P^ <> #0 do
  begin
    if not CharInSet(P^, ['A'..'Z', 'a'..'z', '0'..'9', '.', '-', '_']) then
      Result := Result + '%' + IntToHex(Ord(P^), 2)
    else
      Result := Result + WideChar(P^);
    Inc(P);
  end;
end;

procedure CrackURL(const URL: String; out Components: TURLParts);
var
  Parts: TURLComponents;
  CanonicalURL: String;
  Size: Cardinal;
begin
  FillChar(Parts, SizeOf(TURLComponents), 0);
  Parts.dwStructSize := SizeOf(TURLComponents);
  if URL <> '' then
  begin
    Size := 3 * Length(URL);
    SetString(CanonicalURL, nil, Size);
    if not InternetCanonicalizeUrl(PChar(URL), PChar(CanonicalURL), Size, ICU_NO_META) then
      Size := 0;
    SetLength(CanonicalURL, Size);
    Parts.dwSchemeLength := 1;
    Parts.dwUserNameLength := 1;
    Parts.dwPasswordLength := 1;
    Parts.dwHostNameLength := 1;
    Parts.dwURLPathLength := 1;
    Parts.dwExtraInfoLength := 1;
    InternetCrackUrl(PChar(CanonicalURL), Size, 0, Parts);
  end;
  with Components do
  begin
    Scheme := Parts.nScheme;
    SetString(SchemeName, Parts.lpszScheme, Parts.dwSchemeLength);
    SetString(UserName, Parts.lpszUserName, Parts.dwUserNameLength);
    SetString(Password, Parts.lpszPassword, Parts.dwPasswordLength);
    SetString(HostName, Parts.lpszHostName, Parts.dwHostNameLength);
    Port := Parts.nPort;
    SetString(Path, Parts.lpszUrlPath, Parts.dwUrlPathLength);
    SetString(ExtraInfo, Parts.lpszExtraInfo, Parts.dwExtraInfoLength);
  end;
end;

{ TProxyConfig }

constructor TProxyConfig.Create;
begin
  fType := ptAuto;
  fBypass := TStringList.Create;
  fBypass.Text := DefaultProxyBypass;
  TStringList(fBypass).OnChange := BypassChanged;
end;

destructor TProxyConfig.Destroy;
begin
  fBypass.Free;
  inherited Destroy;
end;

procedure TProxyConfig.Assign(Source: TPersistent);
begin
  if Source is TProxyConfig then
  begin
    AccessType := TProxyConfig(Source).AccessType;
    Host := TProxyConfig(Source).Host;
    Port := TProxyConfig(Source).Port;
  end
  else
    inherited Assign(Source);
end;

function TProxyConfig.GetIOType: Cardinal;
begin
  case AccessType of
    ptDirect: Result := INTERNET_OPEN_TYPE_DIRECT;
    ptAuto: Result := INTERNET_OPEN_TYPE_PRECONFIG;
  else
    Result := INTERNET_OPEN_TYPE_PROXY;
  end;
end;

function TProxyConfig.GetIOName: PChar;
var
  ProxyName: String;
begin
  Result := nil;
  if AccessType = ptManual then
  begin
    ProxyName := Trim(Host);
    if ProxyName <> '' then
    begin
      if Port <> 0 then
        ProxyName := ProxyName + ':' + IntToStr(Port);
      Result := PChar(ProxyName);
    end;
  end;
end;

function TProxyConfig.GetIOBypass: PChar;
var
  ProxyBypass: String;
begin
  Result := nil;
  if AccessType = ptManual then
  begin
    ProxyBypass := Trim(Bypass.Text);
    if ProxyBypass <> '' then
      Result := PChar(ProxyBypass);
  end;
end;

procedure TProxyConfig.SetType(Value: TProxyType);
begin
  if AccessType <> Value then
  begin
    fType := Value;
    DoChange;
  end;
end;

procedure TProxyConfig.SetHost(const Value: String);
begin
  if Host <> Value then
  begin
    fHost := Value;
    if AccessType = ptManual then
      DoChange;
  end;
end;

procedure TProxyConfig.SetPort(Value: Word);
begin
  if Port <> Value then
  begin
    fPort := Value;
    if AccessType = ptManual then
      DoChange;
  end;
end;

procedure TProxyConfig.SetBypass(Value: TStrings);
begin
  Bypass.Assign(Value);
end;

function TProxyConfig.IsStoredBypass: Boolean;
begin
  Result := (Trim(Bypass.Text) <> DefaultProxyBypass);
end;

procedure TProxyConfig.BypassChanged(Sender: TObject);
begin
  if AccessType = ptManual then
    DoChange;
end;

procedure TProxyConfig.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ TCustomHTTP }

constructor TCustomHTTP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fUserAgent := DefaultUserAgent;
  fProxy := TProxyConfig.Create;
  fProxy.OnChange := ConfigChanged;
end;

destructor TCustomHTTP.Destroy;
begin
  UnprepareSession;
  fProxy.Free;
  inherited Destroy;
end;

procedure TCustomHTTP.ConfigChanged(Sender: TObject);
begin
  UnprepareSession;
end;

procedure TCustomHTTP.SetProxy(Value: TProxyConfig);
begin
  Proxy.Assign(Value);
end;

procedure TCustomHTTP.SetUserAgent(const Value: String);
begin
  if UserAgent <> Value then
  begin
    fUserAgent := Value;
    ConfigChanged(Self);
  end;
end;

function TCustomHTTP.IsStoredUserAgent: Boolean;
begin
  Result := (UserAgent <> DefaultUserAgent);
end;

function TCustomHTTP.PrepareSession(const Details: TURLParts): Boolean;
var
  pUserName, pPassword: PChar;
begin
  Result := False;
  if hInternet = nil then
  begin
    hInternet := InternetOpen(PChar(UserAgent), Proxy.IOType, Proxy.IOName, Proxy.IOBypass, 0);
    if hInternet = nil then
      Exit;
  end;
  if (hConnection = nil) or
     (SessionDetails.Scheme <> Details.Scheme) or
     (SessionDetails.Port <> Details.Port) or
     (SessionDetails.HostName <> Details.HostName) or
     (SessionDetails.UserName <> Details.UserName) or
     (SessionDetails.Password <> Details.Password) then
  begin
    pUserName := nil;
    pPassword := nil;
    if Details.UserName <> '' then
    begin
      pUserName := PChar(Details.UserName);
      pPassword := PChar(Details.Password);
    end;
    hConnection := InternetConnect(hInternet, PChar(Details.HostName), Details.Port,
      pUserName, pPassword, INTERNET_SERVICE_HTTP, 0, 0);
    if hConnection = nil then
      Exit;
    SessionDetails.Scheme := Details.Scheme;
    SessionDetails.HostName := Details.HostName;
    SessionDetails.Port := Details.Port;
    SessionDetails.UserName := Details.UserName;
    SessionDetails.Password := Details.Password;
  end;
  Result := True;
end;

procedure TCustomHTTP.UnprepareSession;
begin
  if hConnection <> nil then
  begin
    InternetCloseHandle(hConnection);
    hConnection := nil;
  end;
  if hInternet <> nil then
  begin
    InternetCloseHandle(hInternet);
    hInternet := nil;
  end;
end;

function TCustomHTTP.HttpRequest(const URL: String;
  Response: TStream; const PostData: String): Cardinal;
const
  HttpMethod: array[Boolean] of PChar = ('GET', 'POST');
  AcceptContentTypes: array[0..1] of PChar = ('*/*', nil);
  PostHeaders = 'Content-Type: application/x-www-form-urlencoded' + #13#10;
var
  URLParts: TURLParts;
  hResource: WinINet.HINTERNET;
  Buffer: array[0..1023] of Byte;
  Flags, Size, Reserved: Cardinal;
  Data: UTF8String;
  ReqOK: Boolean;
begin
  Result := 0;
  CrackURL(URL, URLParts);
  if PrepareSession(URLParts) then
  begin
    Flags := INTERNET_FLAG_RELOAD or INTERNET_FLAG_PRAGMA_NOCACHE
          or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_NO_COOKIES
          or INTERNET_FLAG_NO_UI or INTERNET_FLAG_KEEP_CONNECTION;
    if URLParts.Scheme = INTERNET_SCHEME_HTTPS then
      Flags := Flags or INTERNET_FLAG_SECURE;
    hResource := HttpOpenRequest(hConnection, HttpMethod[PostData <> ''],
      PChar(URLParts.Path + URLParts.ExtraInfo), nil, PChar(Referer),
      @AcceptContentTypes[0], Flags, 0);
    if hResource <> nil then
      try
        if PostData = '' then
          ReqOK := HttpSendRequest(hResource, nil, 0, nil, 0)
        else
        begin
          Data := UTF8Encode(PostData);
          ReqOK := HttpSendRequest(hResource, PostHeaders, Length(PostHeaders),
            PAnsiChar(Data), Length(Data));
        end;
        if ReqOK then
        begin
          Reserved := 0;
          Size := SizeOf(Result);
          HttpQueryInfo(hResource, HTTP_QUERY_FLAG_NUMBER or HTTP_QUERY_STATUS_CODE, @Result, Size, Reserved);
          while InternetReadFile(hResource, @Buffer[0], SizeOf(Buffer), Size) and (Size <> 0) do
            Response.Write(Buffer[0], Size);
        end;
      finally
        InternetCloseHandle(hResource);
      end;
  end;
end;

end.
