{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements a class to deal with null character separated strings.
unit i18nZStrList;

interface

{$I DELPHIAREA.INC}

uses
  SysUtils, Types;

type

  {$region 'xmldoc'}
  /// <summary>
  /// ZStrings provides a set of class methods to work with null character (#0)
  /// separated strings.</summary>
  {$endregion}
  ZStrings = class
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a list of null separated strings from a specified array of strings.</summary>
    /// <param name="StrArray">
    /// The source array of strings.</param>
    /// <returns>
    /// The list of null character separated strings.</returns>
    {$endregion}
    class function Construct(const StrArray: array of String): String; overload; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a list of null separated strings from a specified string. The
    /// sub-strings of the source string are delimited by a specified delimiter
    /// string.</summary>
    /// <param name="Str">
    /// The source string.</param>
    /// <param name="Delimiter">
    /// The delimiter string that separates sub-strings of the source string.</param>
    /// <returns>
    /// The list of null character separated strings.</returns>
    {$endregion}
    class function Construct(const Str: String; const Delimiter: String): String; overload; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a sub-string at the end of a specified list of null character
    /// separated strings.</summary>
    /// <param name="ZStrs">
    /// The null character separated strings.</param>
    /// <param name="SubStr">
    /// The value for the new sub-string.</param>
    /// <seealso cref="Insert"/>
    {$endregion}
    class procedure Add(var ZStrs: String; const SubStr: String); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Inserts a sub-string at a specified index of a specified list of null character
    /// separated strings.</summary>
    /// <param name="ZStrs">
    /// The null character separated strings.</param>
    /// <param name="Index">
    /// The index where the sub-string must be inserted before.</param>
    /// <param name="SubStr">
    /// The value for the new sub-string.</param>
    /// <seealso cref="Add"/>
    {$endregion}
    class procedure Insert(var ZStrs: String; Index: Integer; const SubStr: String); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a sub-string at a specified index of a specified list of null character
    /// separated strings.</summary>
    /// <param name="ZStrs">
    /// The null character separated strings.</param>
    /// <param name="Index">
    /// The index of the sub-string to delete.</param>
    {$endregion}
    class procedure Delete(var ZStrs: String; Index: Integer); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Replaces a sub-string at a specified index of a specified list of null character
    /// separated strings.</summary>
    /// <param name="ZStrs">
    /// The null character separated strings.</param>
    /// <param name="Index">
    /// The index of the sub-string to replace.</param>
    /// <param name="SubStr">
    /// The new value for the sub-string at the specified index.</param>
    /// <seealso cref="GetSubStrAt"/>
    {$endregion}
    class procedure SetSubStrAt(var ZStrs: String; Index: Integer; const SubStr: String); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the sub-string at a specified index of a specified list of null
    /// character separated strings.</summary>
    /// <param name="ZStrs">
    /// The null character separated strings.</param>
    /// <param name="Index">
    /// The index of the sub-string to retrieve.</param>
    /// <returns>
    /// The sub-string at the specified index.</returns>
    /// <seealso cref="SetSubStrAt"/>
    {$endregion}
    class function GetSubStrAt(const ZStrs: String; Index: Integer): String; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the number of sub-strings in a specified list of null character
    /// separated strings.</summary>
    /// <param name="ZStrs">
    /// The null character separated strings.</param>
    /// <returns>
    /// The number of sub-strings.</returns>
    {$endregion}
    class function Count(const ZStrs: String): Integer; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a specified list of null character separated strings to a list
    /// of delimited sub-strings.</summary>
    /// <param name="ZStrs">
    /// The null character separated strings.</param>
    /// <param name="Delimiter">
    /// The string that separates sub-strings.</param>
    /// <returns>
    /// The string of sub-strings delimited by given <paramref name="Delimiter"/>
    /// string.</returns>
    {$endregion}
    class function Split(const ZStrs: String; const Delimiter: String): String; overload; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a specified list of null character separated strings to an array
    /// of its sub-strings.</summary>
    /// <param name="ZStrs">
    /// The null character separated strings.</param>
    /// <returns>
    /// The array of sub-strings.</returns>
    {$endregion}
    class function Split(const ZStrs: String): TStringDynArray; overload; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the index of sub-string at a specified character position of a
    /// specified list of null character separated strings.</summary>
    /// <param name="ZStrs">
    /// The null character separated strings.</param>
    /// <param name="Pos">
    /// The character position withing the null character separated strings.</param>
    /// <returns>
    /// The zero-based index of sub-strings at the specified character position.
    /// If position given by <paramref name="Pos"/> is less than or equal to zero
    /// or <paramref name="ZStrs"/> is empty, the function returns zero.</returns>
    {$endregion}
    class function IndexFromCharPos(const ZStrs: String; Pos: Integer): Integer; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the start position of a sub-string specified with its index within
    /// a specified list of null character separated strings.</summary>
    /// <param name="ZStrs">
    /// The null character separated strings.</param>
    /// <param name="Index">
    /// The index of the sub-string to find its start position.</param>
    /// <returns>
    /// The character position of the specified sub-strings within the specified
    /// null character separated strings.</returns>
    {$endregion}
    class function CharPosFromIndex(const ZStrs: String; Index: Integer): Integer; static;
  end;

implementation

{ ZStrings }

class function ZStrings.Construct(const StrArray: array of String): String;
var
  I: Integer;
  Len: Integer;
  S: PChar;
begin
  Len := Length(StrArray);
  for I := 0 to Length(StrArray) - 1 do
    Inc(Len, Length(StrArray[I]));
  SetLength(Result, Len);
  S := PChar(Result);
  for I := 0 to Length(StrArray) - 1 do
  begin
    StrCopy(S, PChar(StrArray[I]));
    Inc(S, Length(StrArray[I]) + 1);
  end;
end;

class function ZStrings.Construct(const Str, Delimiter: String): String;
begin
  if Str = '' then
    Result := ''
  else
    Result := StringReplace(Str, Delimiter, #0, [rfReplaceAll]) + #0;
end;

class procedure ZStrings.Add(var ZStrs: String; const SubStr: String);
begin
  ZStrs := ZStrs + SubStr + #0;
end;

class procedure ZStrings.Insert(var ZStrs: String; Index: Integer;
  const SubStr: String);
var
  EndPtr, S: PChar;
begin
  if Index <= 0 then
    ZStrs := SubStr + #0 + ZStrs
  else
  begin
    S := PChar(ZStrs);
    EndPtr := S + Length(ZStrs);
    while (Index > 0) and (S < EndPtr) do
    begin
      S := StrEnd(S) + 1;
      Dec(Index);
      if Index = 0 then
      begin
        System.Insert(#0 + SubStr, ZStrs, S - PChar(ZStrs));
        Exit;
      end;
    end;
    ZStrs := ZStrs + SubStr + #0;
  end;
end;

class procedure ZStrings.Delete(var ZStrs: String; Index: Integer);
var
  EndPtr, S: PChar;
begin
  if ZStrs <> '' then
  begin
    S := PChar(ZStrs);
    EndPtr := S + Length(ZStrs);
    while (Index > 0) and (S < EndPtr) do
    begin
      Dec(Index);
      S := StrEnd(S) + 1;
    end;
    if Index = 0 then
      System.Delete(ZStrs, S - PChar(ZStrs) + 1, StrLen(S) + 1);
  end;
end;

class procedure ZStrings.SetSubStrAt(var ZStrs: String; Index: Integer;
  const SubStr: String);
begin
  ZStrings.Delete(ZStrs, Index);
  ZStrings.Insert(ZStrs, Index, SubStr);
end;

class function ZStrings.GetSubStrAt(const ZStrs: String; Index: Integer): String;
var
  EndPtr, S: PChar;
begin
  S := PChar(ZStrs);
  EndPtr := S + Length(ZStrs);
  while (Index > 0) and (S < EndPtr) do
  begin
    Dec(Index);
    S := StrEnd(S) + 1;
  end;
  SetString(Result, S, StrLen(S));
end;

class function ZStrings.Count(const ZStrs: String): Integer;
var
  EndPtr, S: PChar;
begin
  Result := 0;
  S := PChar(ZStrs);
  EndPtr := S + Length(ZStrs);
  while S < EndPtr do
  begin
    Inc(Result);
    S := StrEnd(S) + 1;
  end;
end;

class function ZStrings.Split(const ZStrs: String; const Delimiter: String): String;
var
  EndPtr, S: PChar;
begin
  Result := '';
  S := PChar(ZStrs);
  EndPtr := S + Length(ZStrs);
  while S < EndPtr do
  begin
    if Result = '' then
      Result := String(S)
    else
      Result := Result + Delimiter + String(S);
    S := StrEnd(S) + 1;
  end;
end;

class function ZStrings.Split(const ZStrs: String): TStringDynArray;
var
  S, E: PChar;
  I: Integer;
begin
  S := PChar(ZStrs);
  SetLength(Result, ZStrings.Count(ZStrs));
  for I := 0 to Length(Result) - 1 do
  begin
    E := StrEnd(S);
    SetString(Result[I], S, E - S);
    S := E + 1;
  end;
end;

class function ZStrings.IndexFromCharPos(const ZStrs: String;
  Pos: Integer): Integer;
var
  EndPtr, S, E: PChar;
begin
  Result := -1;
  S := PChar(ZStrs);
  EndPtr := S + Length(ZStrs);
  while (S < EndPtr) and (Pos > 0) do
  begin
    E := StrEnd(S);
    Dec(Pos, E - S + 1);
    S := E + 1;
    Inc(Result);
  end;
end;

class function ZStrings.CharPosFromIndex(const ZStrs: String;
  Index: Integer): Integer;
var
  EndPtr, S: PChar;
begin
  S := PChar(ZStrs);
  EndPtr := S + Length(ZStrs);
  while (Index > 0) and (S < EndPtr) do
  begin
    Dec(Index);
    S := StrEnd(S) + 1;
  end;
  Result := S - PChar(ZStrs) + 1;
end;

end.
