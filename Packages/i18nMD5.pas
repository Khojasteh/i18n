{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements MD5 hashing algorithm.
unit i18nMD5;

{$I DELPHIAREA.INC}

interface

uses
  Classes, SysUtils, Types;

type

  {$region 'xmldoc'}
  /// <summary>
  /// EMD5Error is the exception class of the errors that occur in the instances of
  /// <see cref="TMD5"/> class.</summary>
  /// <remarks>
  /// EMD5Error is raised when an error is made in a <see cref="TMD5"/> object. This
  /// exception commonly occurs when an application appends new data to a <see cref="TMD5"/>
  /// object before reseting it.</remarks>
  /// <seealso cref="TMD5"/>
  {$endregion}
  EMD5Error = class(Exception);

  {$region 'xmldoc'}
  /// <summary>
  /// TMD5 calculates MD5 hash value of data from varius data sources.</summary>
  {$endregion}
  TMD5 = class(TObject)
  private
    Count: Cardinal;                  { message length in bytes }
    Digest: array[0..3] of Cardinal;  { digest buffer }
    Buffer: array[0..63] of Byte;     { accumulate block }
    Appending: Boolean;
    function GetValue: String;
    procedure Process(const Block);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Create an instance of TMD5 class.</summary>
    {$endregion}
    constructor Create;
    {$region 'xmldoc'}
    /// <summary>
    /// Resets the object for appending new set of data.</summary>
    {$endregion}
    procedure Reset;
    {$region 'xmldoc'}
    /// <summary>
    /// Appends a specified number of bytes to the MD5 buffer.</summary>
    /// <param name="Data">
    /// The data to append.</param>
    /// <param name="Size">
    /// The number of bytes of <paramref name="Data"/> to append.</param>
    /// <seealso cref="AppendString"/>
    /// <seealso cref="AppendStream"/>
    /// <seealso cref="AppendFile"/>
    {$endregion}
    procedure Append(const Data; Size: Cardinal);
    {$region 'xmldoc'}
    /// <summary>
    /// Appends characters of a specified string to the MD5 buffer.</summary>
    /// <param name="Str">
    /// The string to append.</param>
    /// <seealso cref="Append"/>
    /// <seealso cref="AppendStream"/>
    /// <seealso cref="AppendFile"/>
    {$endregion}
    procedure AppendString(const Str: String); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Appends characters of a specified ansi string to the MD5 buffer.</summary>
    /// <param name="Str">
    /// The string to append.</param>
    /// <seealso cref="Append"/>
    /// <seealso cref="AppendStream"/>
    /// <seealso cref="AppendFile"/>
    {$endregion}
    procedure AppendString(const Str: AnsiString); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Appends characters of a specified wide string to the MD5 buffer.</summary>
    /// <param name="Str">
    /// The string to append.</param>
    /// <seealso cref="Append"/>
    /// <seealso cref="AppendStream"/>
    /// <seealso cref="AppendFile"/>
    {$endregion}
    procedure AppendString(const Str: WideString); overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Appends content of a specified stream to the MD5 buffer.</summary>
    /// <param name="Stream">
    /// The stream to append.</param>
    /// <seealso cref="Append"/>
    /// <seealso cref="AppendString"/>
    /// <seealso cref="AppendFile"/>
    {$endregion}
    procedure AppendStream(Stream: TStream);
    {$region 'xmldoc'}
    /// <summary>
    /// Appends content of a specified file to the MD5 buffer.</summary>
    /// <param name="FileName">
    /// The path to the file to append.</param>
    /// <seealso cref="Append"/>
    /// <seealso cref="AppendString"/>
    /// <seealso cref="AppendStream"/>
    {$endregion}
    procedure AppendFile(const FileName: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the 128-bit MD5 hash value of the previously appended data as a
    /// hexadecimal string.</summary>
    {$endregion}
    property Value: String read GetValue;
  end;

{$region 'xmldoc'}
/// <summary>
/// Calculates MD5 hash value of a specified number of bytes.</summary>
/// <param name="Data">
/// The data to calculate its hash value.</param>
/// <param name="Size">
/// The number of bytes of <paramref name="Data"/>.</param>
/// <returns>
/// The hash value as a hexadecimal string.</returns>
/// <seealso cref="FileMD5"/>
{$endregion}
function MD5(const Data; Size: Integer): String; overload;

{$region 'xmldoc'}
/// <summary>
/// Calculates MD5 hash value of a specified string.</summary>
/// <param name="Str">
/// The string to calculate its hash value.</param>
/// <returns>
/// The hash value as a hexadecimal string.</returns>
/// <seealso cref="FileMD5"/>
{$endregion}
function MD5(const Str: String): String; overload;

{$region 'xmldoc'}
/// <summary>
/// Calculates MD5 hash value of a specified ansi string.</summary>
/// <param name="Str">
/// The string to calculate its hash value.</param>
/// <returns>
/// The hash value as a hexadecimal string.</returns>
/// <seealso cref="FileMD5"/>
{$endregion}
function MD5(const Str: AnsiString): String; overload;

{$region 'xmldoc'}
/// <summary>
/// Calculates MD5 hash value of a specified wide string.</summary>
/// <param name="Str">
/// The string to calculate its hash value.</param>
/// <returns>
/// The hash value as a hexadecimal string.</returns>
/// <seealso cref="FileMD5"/>
{$endregion}
function MD5(const Str: WideString): String; overload;

{$region 'xmldoc'}
/// <summary>
/// Calculates MD5 hash value of a specified stream.</summary>
/// <param name="Stream">
/// The stream to calculate hash value of its content.</param>
/// <returns>
/// The hash value as a hexadecimal string.</returns>
/// <seealso cref="FileMD5"/>
{$endregion}
function MD5(Stream: TStream): String; overload;

{$region 'xmldoc'}
/// <summary>
/// Calculates MD5 hash value of a specified file.</summary>
/// <param name="FileName">
/// The path to the file that hash value of its content should be calculated.</param>
/// <returns>
/// The hash value as a hexadecimal string.</returns>
/// <seealso cref="MD5"/>
{$endregion}
function FileMD5(const FileName: String): String;

implementation

resourcestring
  SResetRequired = 'You must reset the MD5 object before appending new data';

{ Helper Functions }

function MD5(const Data; Size: Integer): String;
begin
  with TMD5.Create do
    try
      Append(Data, Size);
      Result := Value;
    finally
      Free;
    end;
end;

function MD5(const Str: String): String;
begin
  with TMD5.Create do
    try
      AppendString(Str);
      Result := Value;
    finally
      Free;
    end;
end;

function MD5(const Str: AnsiString): String;
begin
  with TMD5.Create do
    try
      AppendString(Str);
      Result := Value;
    finally
      Free;
    end;
end;

function MD5(const Str: WideString): String;
begin
  with TMD5.Create do
    try
      AppendString(Str);
      Result := Value;
    finally
      Free;
    end;
end;

function MD5(Stream: TStream): String;
begin
  with TMD5.Create do
    try
      AppendStream(Stream);
      Result := Value;
    finally
      Free;
    end;
end;

function FileMD5(const FileName: String): String;
begin
  with TMD5.Create do
    try
      AppendFile(FileName);
      Result := Value;
    finally
      Free;
    end;
end;

{ TMD5 }

const
  T: array[1..64] of Cardinal = (
    $d76aa478, $e8c7b756, $242070db, $c1bdceee, $f57c0faf, $4787c62a,
    $a8304613, $fd469501, $698098d8, $8b44f7af, $ffff5bb1, $895cd7be,
    $6b901122, $fd987193, $a679438e, $49b40821, $f61e2562, $c040b340,
    $265e5a51, $e9b6c7aa, $d62f105d, $02441453, $d8a1e681, $e7d3fbc8,
    $21e1cde6, $c33707d6, $f4d50d87, $455a14ed, $a9e3e905, $fcefa3f8,
    $676f02d9, $8d2a4c8a, $fffa3942, $8771f681, $6d9d6122, $fde5380c,
    $a4beea44, $4bdecfa9, $f6bb4b60, $bebfbc70, $289b7ec6, $eaa127fa,
    $d4ef3085, $04881d05, $d9d4d039, $e6db99e5, $1fa27cf8, $c4ac5665,
    $f4292244, $432aff97, $ab9423a7, $fc93a039, $655b59c3, $8f0ccc92,
    $ffeff47d, $85845dd1, $6fa87e4f, $fe2ce6e0, $a3014314, $4e0811a1,
    $f7537e82, $bd3af235, $2ad7d2bb, $eb86d391);

type
  TBlock = array[0..15] of Cardinal;

procedure TMD5.Process(const Block);

  procedure Set1(const X: TBlock; var a, b, c, d: Cardinal; k, s, i: Cardinal); inline;
  var
    n: Cardinal;
  begin
    n := a + X[k] + T[i] + ((b and c) or (not b and d));
    a := ((n shl s) or (n shr (32 - s))) + b;
  end;

  procedure Set2(const X: TBlock; var a, b, c, d: Cardinal; k, s, i: Cardinal); inline;
  var
    n: Cardinal;
  begin
    n := a + X[k] + T[i] + ((b and d) or (c and not d));
    a := ((n shl s) or (n shr (32 - s))) + b;
  end;

  procedure Set3(const X: TBlock; var a, b, c, d: Cardinal; k, s, i: Cardinal); inline;
  var
    n: Cardinal;
  begin
    n := a + X[k] + T[i] + (b xor c xor d);
    a := ((n shl s) or (n shr (32 - s))) + b;
  end;

  procedure Set4(const X: TBlock; var a, b, c, d: Cardinal; k, s, i: Cardinal); inline;
  var
    n: Cardinal;
  begin
    n := a + X[k] + T[i] + (c xor (b or not d));
    a := ((n shl s) or (n shr (32 - s))) + b;
  end;

var
  X: TBlock absolute Block;
  a, b, c, d: Cardinal;
begin
  a := Digest[0];
  b := Digest[1];
  c := Digest[2];
  d := Digest[3];
  Set1(X, a, b, c, d,  0,  7,  1);
  Set1(X, d, a, b, c,  1, 12,  2);
  Set1(X, c, d, a, b,  2, 17,  3);
  Set1(X, b, c, d, a,  3, 22,  4);
  Set1(X, a, b, c, d,  4,  7,  5);
  Set1(X, d, a, b, c,  5, 12,  6);
  Set1(X, c, d, a, b,  6, 17,  7);
  Set1(X, b, c, d, a,  7, 22,  8);
  Set1(X, a, b, c, d,  8,  7,  9);
  Set1(X, d, a, b, c,  9, 12, 10);
  Set1(X, c, d, a, b, 10, 17, 11);
  Set1(X, b, c, d, a, 11, 22, 12);
  Set1(X, a, b, c, d, 12,  7, 13);
  Set1(X, d, a, b, c, 13, 12, 14);
  Set1(X, c, d, a, b, 14, 17, 15);
  Set1(X, b, c, d, a, 15, 22, 16);
  Set2(X, a, b, c, d,  1,  5, 17);
  Set2(X, d, a, b, c,  6,  9, 18);
  Set2(X, c, d, a, b, 11, 14, 19);
  Set2(X, b, c, d, a,  0, 20, 20);
  Set2(X, a, b, c, d,  5,  5, 21);
  Set2(X, d, a, b, c, 10,  9, 22);
  Set2(X, c, d, a, b, 15, 14, 23);
  Set2(X, b, c, d, a,  4, 20, 24);
  Set2(X, a, b, c, d,  9,  5, 25);
  Set2(X, d, a, b, c, 14,  9, 26);
  Set2(X, c, d, a, b,  3, 14, 27);
  Set2(X, b, c, d, a,  8, 20, 28);
  Set2(X, a, b, c, d, 13,  5, 29);
  Set2(X, d, a, b, c,  2,  9, 30);
  Set2(X, c, d, a, b,  7, 14, 31);
  Set2(X, b, c, d, a, 12, 20, 32);
  Set3(X, a, b, c, d,  5,  4, 33);
  Set3(X, d, a, b, c,  8, 11, 34);
  Set3(X, c, d, a, b, 11, 16, 35);
  Set3(X, b, c, d, a, 14, 23, 36);
  Set3(X, a, b, c, d,  1,  4, 37);
  Set3(X, d, a, b, c,  4, 11, 38);
  Set3(X, c, d, a, b,  7, 16, 39);
  Set3(X, b, c, d, a, 10, 23, 40);
  Set3(X, a, b, c, d, 13,  4, 41);
  Set3(X, d, a, b, c,  0, 11, 42);
  Set3(X, c, d, a, b,  3, 16, 43);
  Set3(X, b, c, d, a,  6, 23, 44);
  Set3(X, a, b, c, d,  9,  4, 45);
  Set3(X, d, a, b, c, 12, 11, 46);
  Set3(X, c, d, a, b, 15, 16, 47);
  Set3(X, b, c, d, a,  2, 23, 48);
  Set4(X, a, b, c, d,  0,  6, 49);
  Set4(X, d, a, b, c,  7, 10, 50);
  Set4(X, c, d, a, b, 14, 15, 51);
  Set4(X, b, c, d, a,  5, 21, 52);
  Set4(X, a, b, c, d, 12,  6, 53);
  Set4(X, d, a, b, c,  3, 10, 54);
  Set4(X, c, d, a, b, 10, 15, 55);
  Set4(X, b, c, d, a,  1, 21, 56);
  Set4(X, a, b, c, d,  8,  6, 57);
  Set4(X, d, a, b, c, 15, 10, 58);
  Set4(X, c, d, a, b,  6, 15, 59);
  Set4(X, b, c, d, a, 13, 21, 60);
  Set4(X, a, b, c, d,  4,  6, 61);
  Set4(X, d, a, b, c, 11, 10, 62);
  Set4(X, c, d, a, b,  2, 15, 63);
  Set4(X, b, c, d, a,  9, 21, 64);
  Inc(Digest[0], a);
  Inc(Digest[1], b);
  Inc(Digest[2], c);
  Inc(Digest[3], d);
end;

procedure TMD5.Append(const Data; Size: Cardinal);
var
  P: PByte;
  Offset: Cardinal;
  LeftBytes: Cardinal;
  CopyBytes: Cardinal;
begin
  if not Appending then
    raise EMD5Error.Create(SResetRequired);
  if Size = 0 then
    Exit;
  P := @Data;
  LeftBytes := Size;
  Offset := Count and 63;
  // Update the message length
  Inc(Count, Size);
  // Process an initial partial block
  if Offset <> 0 then
  begin
    if Offset + Size > 64 then
      CopyBytes := 64 - Offset
    else
      CopyBytes := Size;
    Move(P^, Buffer[Offset], CopyBytes);
    if Offset + CopyBytes < 64 then Exit;
    Inc(P, CopyBytes);
    Dec(LeftBytes, CopyBytes);
    Process(Buffer[0]);
  end;
  // Process full blocks
  while LeftBytes >= 64 do
  begin
    Process(P^);
    Inc(P, 64);
    Dec(LeftBytes, 64);
  end;
  // Save final partial block for future process
  if LeftBytes > 0 then
    Move(P^, Buffer[0], LeftBytes);
end;

procedure TMD5.AppendString(const Str: String);
begin
{$IFDEF UNICODE}
  AppendString(WideString(Str));
{$ELSE}
  AppendString(AnsiString(Str));
{$ENDIF}
end;

procedure TMD5.AppendString(const Str: AnsiString);
begin
  Append(PAnsiChar(Str)^, Length(Str) * SizeOf(AnsiChar));
end;

procedure TMD5.AppendString(const Str: WideString);
var
  Utf8Str: AnsiString;
begin
  Utf8Str := UTF8Encode(Str);
  AppendString(Utf8Str);
end;

procedure TMD5.AppendStream(Stream: TStream);
var
  Bytes: array[1..$FF00] of Byte;
  ByteCount: Integer;
begin
  ByteCount := Stream.Read(Bytes, SizeOf(Bytes));
  while ByteCount > 0 do
  begin
    Append(Bytes, ByteCount);
    ByteCount := Stream.Read(Bytes, SizeOf(Bytes));
  end;
end;

procedure TMD5.AppendFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    AppendStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMD5.Reset;
begin
  Count := 0;
  Digest[0] := $67452301;
  Digest[1] := $efcdab89;
  Digest[2] := $98badcfe;
  Digest[3] := $10325476;
  Appending := True;
end;

function TMD5.GetValue: String;
const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';
  Pad: array[0..63] of Byte = (
    $80, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00);
var
  BitCount: Int64;
  S: PChar;
  B: PByte;
  I: Integer;
begin
  if Appending then
  begin
    // Save the length before padding
    BitCount := Int64(Count) shl 3;
    // Pad to 56 bytes mod 64
    Append(Pad, (55 - (Count and 63)) + 1);
    // Append the length
    Append(BitCount, 8);
    Appending := False;
  end;
  // convert 128-bit digest to string
  SetString(Result, nil, 32);
  S := PChar(Result);
  B := Addr(Digest);
  for I := 0 to 15 do
  begin
    S^ := HexDigits[(B^ shr 4) and 15];
    Inc(S);
    S^ := HexDigits[B^ and 15];
    Inc(S);
    Inc(B);
  end;
end;

constructor TMD5.Create;
begin
  Reset;
end;

end.
