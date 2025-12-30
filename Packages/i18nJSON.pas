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
/// This unit implements classes and functions to parse and decode values from
/// a JSON (JavaScript Object Notation) formatted string.
/// </summary>
unit i18nJSON;

{$I DELPHIAREA.INC}

interface

uses
  Windows, SysUtils, Classes, Contnrs;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the different types of JSON tokens.
  /// </summary>
  {$endregion}
  TJSONToken = (
    {$region 'xmldoc'}
    /// Indicates that the token is a left brace character, which signals the start
    /// of a JSON object.
    {$endregion}
    T_ObjectBegin,
    {$region 'xmldoc'}
    /// Indicates that the token is a right brace character, which signals the end
    /// of a JSON object.
    {$endregion}
    T_ObjectEnd,
    {$region 'xmldoc'}
    /// Indicates that the token is a left bracket character, which signals
    /// the start of a JSON array.
    {$endregion}
    T_ArrayBegin,
    {$region 'xmldoc'}
    /// Indicates that the token is a right bracket character, which signals
    /// the end of a JSON array.
    {$endregion}
    T_ArrayEnd,
    {$region 'xmldoc'}
    /// Indicates that the token is a JSON string value enclosed by double
    /// quote characters.
    {$endregion}
    T_String,
    {$region 'xmldoc'}
    /// Indicates that the token is a JSON numeric value.
    {$endregion}
    T_Number,
    {$region 'xmldoc'}
    /// Indicates that the token is the JSON <c>true</c> value.
    {$endregion}
    T_True,
    {$region 'xmldoc'}
    /// Indicates that the token is the JSON <c>false</c> value.
    {$endregion}
    T_False,
    {$region 'xmldoc'}
    /// Indicates that the token is the JSON <c>null</c> value.
    {$endregion}
    T_Null,
    {$region 'xmldoc'}
    /// Indicates that the token is a comma character.
    {$endregion}
    T_ItemSeparator,
    {$region 'xmldoc'}
    /// Indicates that the token is a colon character.
    {$endregion}
    T_KeyValueSeparator,
    {$region 'xmldoc'}
    /// Indicates that the token is any whitespace character.
    {$endregion}
    T_Whitespace,
    {$region 'xmldoc'}
    /// Indicates that the end of the JSON formatted string has been reached.
    {$endregion}
    T_EOJ
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TJSONTokenizer extracts special symbols, reserved words, numerals, and
  /// character strings from a JSON string.
  /// </summary>
  {$endregion}
  TJSONTokenizer = class(TObject)
  private
    fJSON: String;
    fTokenPtr: PChar;
    fTokenLen: Integer;
    fTokenID: TJSONToken;
    function GetToken: String;
    function GetTokenPos: Integer;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the next JSON token from a specified JSON formatted string.
    /// </summary>
    /// <param name="S">
    /// Pointer to the JSON formatted string. When the method exits, the pointer
    /// moves to the end of current token.
    /// </param>
    /// <returns>
    /// The <see cref="TJSONToken"/> value of the token.
    /// </returns>
    /// <seealso cref="Next"/>
    {$endregion}
    class function GetNextToken(var S: PChar): TJSONToken;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for tokenizing a specified JSON formatted
    /// string.
    /// </summary>
    /// <param name="JSON">
    /// The JSON formatted string.
    /// </param>
    {$endregion}
    constructor Create(const JSON: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the properties with the next token.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if no error occurs and the end of the JSON
    /// formatted string is not reached; otherwise, returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Next: Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Restarts parsing the JSON formatted string from the beginning.
    /// </summary>
    {$endregion}
    procedure Reset;
    {$region 'xmldoc'}
    /// <summary>
    /// The JSON formatted string that is being parsed.
    /// </summary>
    {$endregion}
    property JSON: String read fJSON;
    {$region 'xmldoc'}
    /// <summary>
    /// The current token as a string.
    /// </summary>
    {$endregion}
    property Token: String read GetToken;
    {$region 'xmldoc'}
    /// <summary>
    /// The pointer to the current token within the <see cref="JSON"/> property.
    /// </summary>
    {$endregion}
    property TokenPtr: PChar read fTokenPtr;
    {$region 'xmldoc'}
    /// <summary>
    /// The character index of the current token in the <see cref="JSON"/> property.
    /// </summary>
    {$endregion}
    property TokenPos: Integer read GetTokenPos;
    {$region 'xmldoc'}
    /// <summary>
    /// The length of current token in characters.
    /// </summary>
    {$endregion}
    property TokenLen: Integer read fTokenLen;
    {$region 'xmldoc'}
    /// <summary>
    /// The current token as a <see cref="TJSONToken"/> value.
    /// </summary>
    {$endregion}
    property TokenID: TJSONToken read fTokenID;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TJSONValue represents a JSON value.
  /// </summary>
  {$endregion}
  TJSONValue = class abstract(TObject)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Converts the JSON value represented by the object to a JSON formatted
    /// string.
    /// </summary>
    /// <returns>
    /// The JSON value as a JSON formatted string.
    /// </returns>
    {$endregion}
    function GetJSON: String; virtual; abstract;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the JSON value as a JSON formatted string.
    /// </summary>
    {$endregion}
    property JSON: String read GetJSON;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TJSONValueNull represents JSON's <c>null</c> value.
  /// </summary>
  {$endregion}
  TJSONValueNull = class(TJSONValue)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Converts the JSON's <c>null</c> value represented by the object to a
    /// JSON formatted string.
    /// </summary>
    /// <returns>
    /// The <c>null</c> value as a JSON formatted string.
    /// </returns>
    {$endregion}
    function GetJSON: String; override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TJSONValueBoolean represents a JSON boolean value.
  /// </summary>
  {$endregion}
  TJSONValueBoolean = class(TJSONValue)
  private
    fValue: Boolean;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Converts the JSON boolean value represented by the object to a JSON
    /// formatted string.
    /// </summary>
    /// <returns>
    /// The boolean value as a JSON formatted string.
    /// </returns>
    {$endregion}
    function GetJSON: String; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates and initializes an instance of the class.
    /// </summary>
    /// <param name="AValue">
    /// The initial boolean value of the instance.
    /// </param>
    {$endregion}
    constructor Create(AValue: Boolean);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the boolean value represented by the object.
    /// </summary>
    {$endregion}
    property Value: Boolean read fValue write fValue;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TJSONValueNumber represents a JSON numeric value.
  /// </summary>
  {$endregion}
  TJSONValueNumber = class(TJSONValue)
  private
    fValue: Extended;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Converts the JSON numeric value represented by the object to a JSON
    /// formatted string.
    /// </summary>
    /// <returns>
    /// The numeric value as a JSON formatted string.
    /// </returns>
    {$endregion}
    function GetJSON: String; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates and initializes an instance of the class.
    /// </summary>
    /// <param name="AValue">
    /// The initial numeric value of the instance.
    /// </param>
    {$endregion}
    constructor Create(const AValue: Extended);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the numeric value represented by the object.
    /// </summary>
    {$endregion}
    property Value: Extended read fValue write fValue;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TJSONValueString represents a JSON string value.
  /// </summary>
  {$endregion}
  TJSONValueString = class(TJSONValue)
  private
    fValue: String;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Converts the JSON string value represented by the object to a JSON
    /// formatted string.
    /// </summary>
    /// <returns>
    /// The string value as a JSON formatted string.
    /// </returns>
    {$endregion}
    function GetJSON: String; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates and initializes an instance of the class.
    /// </summary>
    /// <param name="AValue">
    /// The initial string value of the instance.
    /// </param>
    {$endregion}
    constructor Create(const AValue: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the string value represented by the object.
    /// </summary>
    {$endregion}
    property Value: String read fValue write fValue;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TJSONValueArray represents a JSON array value.
  /// </summary>
  {$endregion}
  TJSONValueArray = class(TJSONValue)
  private
    fValues: TObjectList;
    function GetCount: Integer;
    procedure SetCount(Value: Integer);
    function GetValues(Index: Integer): TJSONValue;
    procedure SetValues(Index: Integer; Value: TJSONValue);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Converts the JSON array value represented by the object to a JSON
    /// formatted string.
    /// </summary>
    /// <returns>
    /// The array value as a JSON formatted string.
    /// </returns>
    {$endregion}
    function GetJSON: String; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a new value to the end of the array.
    /// </summary>
    /// <param name="Value">
    /// The JSON value to add to the array.
    /// </param>
    /// <seealso cref="Insert"/>
    {$endregion}
    procedure Add(Value: TJSONValue);
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a new value into a specified position in the array.
    /// </summary>
    /// <param name="Index">
    /// Specifies where to insert the value.
    /// </param>
    /// <param name="Value">
    /// The JSON value to add to the array.
    /// </param>
    /// <seealso cref="Add"/>
    {$endregion}
    procedure Insert(Index: Integer; Value: TJSONValue);
    {$region 'xmldoc'}
    /// <summary>
    /// Removes a value at a specific position from the array.
    /// </summary>
    /// <param name="Index">
    /// Identifies the value to remove by its index in the array.
    /// </param>
    {$endregion}
    procedure Delete(Index: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of values in the array.
    /// </summary>
    {$endregion}
    property Count: Integer read GetCount write SetCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the values in the array.
    /// </summary>
    {$endregion}
    property Values[Index: Integer]: TJSONValue read GetValues write SetValues; default;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TJSONValueObject represents a JSON object (key-value pair) value.
  /// </summary>
  {$endregion}
  TJSONValueObject = class(TJSONValue)
  private
    fValues: TStringList;
    function GetCount: Integer;
    function GetNames(Index: Integer): String;
    function GetValues(const Name: String): TJSONValue;
    procedure SetValues(const Name: String; Value: TJSONValue);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Converts the JSON object value represented by the object to a JSON
    /// formatted string.
    /// </summary>
    /// <returns>
    /// The object value as a JSON formatted string.
    /// </returns>
    {$endregion}
    function GetJSON: String; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a value with a specified name exists.
    /// </summary>
    /// <param name="Name">
    /// The name that identifies the value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the value exists, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Exists(const Name: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes a value with a specified name.
    /// </summary>
    /// <param name="Name">
    /// The name that identifies the value to remove.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the value was removed; returns <see langword="false"/>
    /// if the value was not found.
    /// </returns>
    {$endregion}
    function Delete(const Name: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of values in the object.
    /// </summary>
    {$endregion}
    property Count: Integer read GetCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the names of the values in the object.
    /// </summary>
    {$endregion}
    property Names[Index: Integer]: String read GetNames;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the values of the object by their names.
    /// </summary>
    {$endregion}
    property Values[const Name: String]: TJSONValue read GetValues write SetValues; default;
  end;

{$region 'xmldoc'}
/// <summary>
/// Converts a JSON formatted string to a <see cref="TJSONValue"/> object.
/// </summary>
/// <param name="JSON">
/// The JSON formatted string.
/// </param>
/// <param name="ErrorPos">
/// The error location within the <paramref name="JSON"/> parameter, in character
/// position.
/// </param>
/// <returns>
/// The <see cref="TJSONValue"/> object or <see langword="nil"/> if an error
/// occurred.
/// </returns>
/// <seealso cref="TJSONValue.JSON"/>
{$endregion}
function DecodeJSON(const JSON: String; out ErrorPos: Integer): TJSONValue; overload;

{$region 'xmldoc'}
/// <summary>
/// Converts a JSON formatted string to a <see cref="TJSONValue"/> object.
/// </summary>
/// <param name="JSON">
/// The JSON formatted string.
/// </param>
/// <returns>
/// The <see cref="TJSONValue"/> object or <see langword="nil"/> if an error
/// occurred.
/// </returns>
/// <seealso cref="TJSONValue.JSON"/>
{$endregion}
function DecodeJSON(const JSON: String): TJSONValue; overload;

implementation

resourcestring
  SInvalidJSONString = 'Invalid JSON string';

{ Local Helper Functions }

function StrToNumber(const Str: String): Extended;
begin
  {$IFDEF COMPILER_XE2_UP}
  with FormatSettings do
  {$ENDIF}
  if DecimalSeparator <> '.' then
    Result := StrToFloat(StringReplace(Str, '.', DecimalSeparator, []))
  else
    Result := StrToFloat(Str)
end;

function NumberToStr(const Value: Extended): String;
begin
  Result := Format('%g', [Value]);
  {$IFDEF COMPILER_XE2_UP}
  with FormatSettings do
  {$ENDIF}
  if DecimalSeparator <> '.' then
    Result := StringReplace(Result, DecimalSeparator, '.', []);
end;

function ToJSONString(const Str: String): String;
const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';
  Chars8to13: array[#$8..#$D] of Char = 'btnvfr';
var
  I: Integer;
  R: PChar;
  C, Esc: Char;
begin
  {$IFNDEF COMPILER_XE5_UP}
  Esc := #0; // to prevent compiler warning
  {$ENDIF}
  SetString(Result, nil, 4 * Length(Str) + 2);
  R := PChar(Result);
  R^ := '"';
  for I := 1 to Length(Str) do
  begin
    Inc(R);
    C := Str[I];
    case C of
      #$8..#$D: Esc := Chars8to13[C];    // -> \b \t \n \v \f \r
           '/': Esc := '/';              // -> \/
           '\': Esc := '\';              // -> \\
           '"': Esc := '"';              // -> \"
    else
      if C < #$20 then                   // -> \xHH
        Esc := 'x'
      else
      begin
        R^ := C;
        Continue;
      end;
    end;
    R^ := '\';
    Inc(R);
    R^ := Esc;
    if Esc = 'x' then
    begin
      Inc(R);
      R^ := HexDigits[Ord(C) shr 4];
      Inc(R);
      R^ := HexDigits[Ord(C) and $000F];
    end;
  end;
  Inc(R);
  R^ := '"';
  SetLength(Result, R - PChar(Result) + 1);
end;

function FromJSONString(const Str: String): String;

  function GetEscapedChar(var Index: Integer; MaxLen: Integer): Char;
  var
    Count: Integer;
    Value: Word;
  begin
    Value := 0;
    Count := 0;
    while (Count < MaxLen) and (Index < Length(Str)) do
    begin
      Inc(Index);
      case Str[Index] of
        '0'..'9': Value := Value * 16 + Ord(Str[Index]) - Ord('0');
        'A'..'F': Value := Value * 16 + 10 + Ord(Str[Index]) - Ord('A');
        'a'..'f': Value := Value * 16 + 10 + Ord(Str[Index]) - Ord('a');
      else
        Break;
      end;
      Inc(Count);
    end;
    if Count = 0 then
      EConvertError.Create(SInvalidJSONString);
    Result := Char(Value);
  end;

var
  StrLen, I: Integer;
  R: PChar;
begin
  StrLen := Length(Str);
  if (StrLen >= 2) and (Str[1] = Str[StrLen]) and (Str[1] = '"') then
  begin
    SetString(Result, nil, StrLen - 2);
    R := PChar(Result);
    I := 2;
    while I < StrLen do
    begin
      if Str[I] = '\' then
      begin
        Inc(I);
        if I = StrLen then
          EConvertError.Create(SInvalidJSONString);
        case Str[I] of
          'r': R^ := #$D;  // carriage return
          'f': R^ := #$C;  // form feed
          'v': R^ := #$B;  // vertical tab
          'n': R^ := #$A;  // line feed
          't': R^ := #$9;  // horizontal tab
          'b': R^ := #$8;  // backspace
          'x': R^ := GetEscapedChar(I, 2);
          'u': R^ := GetEscapedChar(I, 4);
        else
          R^ := Str[I];
        end;
      end
      else
        R^ := Str[I];
      Inc(R);
      Inc(I);
    end;
    SetLength(Result, R - PChar(Result));
  end
  else
    Result := Str;
end;

{ Global Helper Functions }

function DecodeJSON(const JSON: String; out ErrorPos: Integer): TJSONValue; overload;

  function DecodeValue(Tokenizer: TJSONTokenizer): TJSONValue;
  var
    Name: String;
    ChildValue: TJSONValue;
    Failed: Boolean;
  begin
    case Tokenizer.TokenID of
      T_ObjectBegin:
      begin
        Result := TJSONValueObject.Create;
        repeat
          Failed := True;
          Tokenizer.Next;
          if Tokenizer.TokenID = T_String then
          begin
            Name := FromJSONString(Tokenizer.Token);
            Tokenizer.Next;
            if Tokenizer.TokenID = T_KeyValueSeparator then
            begin
              Tokenizer.Next;
              ChildValue := DecodeValue(Tokenizer);
              if Assigned(ChildValue) then
              begin
                TJSONValueObject(Result).Values[Name] := ChildValue;
                Failed := False;
              end;
            end;
          end;
        until Failed or (Tokenizer.TokenID <> T_ItemSeparator);
        if not Failed and (Tokenizer.TokenID = T_ObjectEnd) then
          Tokenizer.Next
        else
          FreeAndNil(Result);
      end;
      T_ArrayBegin:
      begin
        Result := TJSONValueArray.Create;
        repeat
          Failed := True;
          Tokenizer.Next;
          ChildValue := DecodeValue(Tokenizer);
          if Assigned(ChildValue) then
          begin
            TJSONValueArray(Result).Add(ChildValue);
            Failed := False;
          end;
        until Failed or (Tokenizer.TokenID <> T_ItemSeparator);
        if not Failed and (Tokenizer.TokenID = T_ArrayEnd) then
          Tokenizer.Next
        else
          FreeAndNil(Result);
      end;
      T_String:
      begin
        Result := TJSONValueString.Create(FromJSONString(Tokenizer.Token));
        Tokenizer.Next;
      end;
      T_Number:
      begin
        Result := TJSONValueNumber.Create(StrToNumber(Tokenizer.Token));
        Tokenizer.Next;
      end;
      T_True:
      begin
        Result := TJSONValueBoolean.Create(True);
        Tokenizer.Next;
      end;
      T_False:
      begin
        Result := TJSONValueBoolean.Create(False);
        Tokenizer.Next;
      end;
      T_Null:
      begin
        Result := TJSONValueNull.Create;
        Tokenizer.Next;
      end;
    else
      Result := nil;
    end;
  end;

var
  Tokenizer: TJSONTokenizer;
begin
  Tokenizer := TJSONTokenizer.Create(JSON);
  try
    try
      Result := DecodeValue(Tokenizer);
    except
      // Ignore exceptions
    end;
    if Assigned(Result) and (Tokenizer.TokenID <> T_EOJ) then
      FreeAndNil(Result);
    if not Assigned(Result) then
      ErrorPos := Tokenizer.TokenPos
    else
      ErrorPos := 0;
  finally
    Tokenizer.Free;
  end;
end;

function DecodeJSON(const JSON: String): TJSONValue;
var
  ErrorPos: Integer;
begin
  Result := DecodeJSON(JSON, ErrorPos);
end;

{ TJSONTokenizer }

constructor TJSONTokenizer.Create(const JSON: String);
begin
  fJSON := JSON;
  Reset;
end;

function TJSONTokenizer.GetToken: String;
begin
  if fTokenID <> T_EOJ then
    SetString(Result, fTokenPtr, fTokenLen)
  else
    Result := '';
end;

function TJSONTokenizer.GetTokenPos: Integer;
begin
  Result := fTokenPtr - PChar(fJSON) + 1;
end;

class function TJSONTokenizer.GetNextToken(var S: PChar): TJSONToken;
var
  P: PChar;
begin
  Result := T_EOJ;
  case S^ of
    #9, #10, #13, #32:                             // white space
    begin
      repeat
        Inc(S);
      until not CharInSet(S^, [#9, #10, #13, #32]);
      Result := T_Whitespace;
    end;
    ',':                                           // comma
    begin
      Inc(S);
      Result := T_ItemSeparator;
    end;
    ':':                                           // colon
    begin
      Inc(S);
      Result := T_KeyValueSeparator;
    end;
    '{':                                           // object begin
    begin
      Inc(S);
      Result := T_ObjectBegin;
    end;
    '}':                                           // object end
    begin
      Inc(S);
      Result := T_ObjectEnd;
    end;
    '[':                                           // array begin
    begin
      Inc(S);
      Result := T_ArrayBegin;
    end;
    ']':                                           // array end
    begin
      Inc(S);
      Result := T_ArrayEnd;
    end;
    '"':                                           // string
    begin
      repeat
        Inc(S);
      until ((S^ = '"') and ((S - 1)^ <> '\')) or (S^ = #0);
      if S^ <> #0 then
      begin
        Inc(S);
        Result := T_String;
      end
    end;
    '-', '+', '0'..'9':                            // number
    begin
      if S^ = '-' then
      begin
        Inc(S);
        if not CharInSet(S^, ['0'..'9']) then
          Exit;
      end;
      repeat
        Inc(S);
      until not CharInSet(S^, ['0'..'9']);
      if S^ = '.' then
      begin
        Inc(S);
        if not CharInSet(S^, ['0'..'9']) then
          Exit;
        repeat
          Inc(S);
        until not CharInSet(S^, ['0'..'9']);
      end;
      if CharInSet(S^, ['E', 'e']) then
      begin
        Inc(S);
        if CharInSet(S^, ['+', '-']) then
          Inc(S);
        if not CharInSet(S^, ['0'..'9']) then
          Exit;
        repeat
          Inc(S);
        until not CharInSet(S^, ['0'..'9'])
      end;
      Result := T_Number;
    end;
    'n', 't', 'f':                                 // null, true, false
    begin
      P := S;
      repeat
        Inc(S);
      until not CharInSet(S^, ['a'..'z']);
      if AnsiStrLComp(P, 'null', S - P) = 0 then
        Result := T_Null
      else if AnsiStrLComp(P, 'true', S - P) = 0 then
        Result := T_True
      else if AnsiStrLComp(P, 'false', S - P) = 0 then
        Result := T_False;
    end;
  end;
end;

function TJSONTokenizer.Next: Boolean;
var
  S: PChar;
begin
  repeat
    Inc(fTokenPtr, fTokenLen);
    S := fTokenPtr;
    fTokenID := GetNextToken(S);
    fTokenLen := S - fTokenPtr;
  until (fTokenID <> T_Whitespace);
  Result := (fTokenID <> T_EOJ);
end;

procedure TJSONTokenizer.Reset;
begin
  fTokenPtr := PChar(fJSON);
  fTokenLen := 0;
  Next;
end;

{ TJSONValueNull }

function TJSONValueNull.GetJSON: String;
begin
  Result := 'null';
end;

{ TJSONValueBoolean }

constructor TJSONValueBoolean.Create(AValue: Boolean);
begin
  fValue := AValue;
end;

function TJSONValueBoolean.GetJSON: String;
begin
  if Value then
    Result := 'true'
  else
    Result := 'false';
end;

{ TJSONValueNumber }

constructor TJSONValueNumber.Create(const AValue: Extended);
begin
  fValue := AValue;
end;

function TJSONValueNumber.GetJSON: String;
begin
  Result := NumberToStr(Value);
end;

{ TJSONValueString }

constructor TJSONValueString.Create(const AValue: String);
begin
  fValue := AValue;
end;

function TJSONValueString.GetJSON: String;
begin
  Result := ToJSONString(Value);
end;

{ TJSONValueArray }

constructor TJSONValueArray.Create;
begin
  fValues := TObjectList.Create;
  fValues.OwnsObjects := True;
end;

destructor TJSONValueArray.Destroy;
begin
  fValues.Free;
  inherited Destroy;
end;

function TJSONValueArray.GetJSON: String;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to Count - 1 do
  begin
    if I > 0 then
      Result := Result + ', ';
    Result := Result + Values[I].JSON;
  end;
  Result := Result + ']';
end;

function TJSONValueArray.GetCount: Integer;
begin
  Result := fValues.Count;
end;

procedure TJSONValueArray.SetCount(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  while fValues.Count > Value do
    fValues.Delete(fValues.Count - 1);
  while fValues.Count < Value do
    fValues.Add(TJSONValueNull.Create);
end;

function TJSONValueArray.GetValues(Index: Integer): TJSONValue;
begin
  Result := TJSONValue(fValues[Index]);
end;

procedure TJSONValueArray.SetValues(Index: Integer; Value: TJSONValue);
begin
  fValues[Index] := Value;
end;

procedure TJSONValueArray.Add(Value: TJSONValue);
begin
  fValues.Add(Value);
end;

procedure TJSONValueArray.Insert(Index: Integer; Value: TJSONValue);
begin
  fValues.Insert(Index, Value);
end;

procedure TJSONValueArray.Delete(Index: Integer);
begin
  fValues.Delete(Index);
end;

{ TJSONValueObject }

constructor TJSONValueObject.Create;
begin
  fValues := TStringList.Create;
  fValues.OwnsObjects := True;
  fValues.CaseSensitive := True;
  fValues.Duplicates := dupIgnore;
  fValues.Sorted := True;
end;

destructor TJSONValueObject.Destroy;
begin
  fValues.Free;
  inherited Destroy;
end;

function TJSONValueObject.GetJSON: String;
var
  I: Integer;
begin
  Result := '{';
  for I := 0 to Count - 1 do
  begin
    if I > 0 then
      Result := Result + ', ';
    Result := Result + ToJSONString(fValues.Strings[I]) + ': '
            + TJSONValue(fValues.Objects[I]).JSON;
  end;
  Result := Result + '}';
end;

function TJSONValueObject.GetCount: Integer;
begin
  Result := fValues.Count;
end;

function TJSONValueObject.GetNames(Index: Integer): String;
begin
  Result := fValues[Index];
end;

function TJSONValueObject.GetValues(const Name: String): TJSONValue;
var
  Index: Integer;
begin
  Index := fValues.IndexOf(Name);
  if Index < 0 then
    Index := fValues.AddObject(Name, TJSONValueNull.Create);
  Result := TJSONValue(fValues.Objects[Index]);
end;

procedure TJSONValueObject.SetValues(const Name: String; Value: TJSONValue);
var
  Index: Integer;
begin
  Index := fValues.IndexOf(Name);
  if Index < 0 then
    fValues.AddObject(Name, Value)
  else
    fValues.Objects[Index] := Value;
end;

function TJSONValueObject.Exists(const Name: String): Boolean;
begin
  Result := (fValues.IndexOf(Name) >= 0);
end;

function TJSONValueObject.Delete(const Name: String): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Index := fValues.IndexOf(Name);
  if Index >= 0 then
  begin
    fValues.Delete(Index);
    Result := True;
  end;
end;

end.
