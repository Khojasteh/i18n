{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz Khojasteh                                                         }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit MRU;

interface

uses
  SysUtils, Classes, Registry;

type

  TRecentList = class(TPersistent)
  private
    List: TStringList;
    fMaxCount: Integer;
    fModified: Boolean;
    fOnChange: TNotifyEvent;
    function GetCount: Integer;
    procedure SetMaxCount(Value: Integer);
    function GetCaptions(Index: Integer): String;
    function GetItems(Index: Integer): String;
    function GetRegNames(const BaseName: String; Index: Integer): String;
    procedure ListChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Add(const Item: String);
    function Remove(const Item: String): Boolean;
    function RemoveNonexistent: Boolean;
    procedure Delete(Index: Integer);
    procedure SaveToRegistry(R: TRegistry; const BaseName: String);
    procedure LoadFromRegistry(R: TRegistry; const BaseName: String);
    property Count: Integer read GetCount;
    property MaxCount: Integer read fMaxCount write SetMaxCount;
    property Captions[Index: Integer]: String read GetCaptions;
    property Items[Index: Integer]: String read GetItems; default;
    property Modified: Boolean read fModified write fModified;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

implementation

{ TRecentList }

constructor TRecentList.Create;
begin
  List := TStringList.Create;
  List.CaseSensitive := False;
  List.Duplicates := dupIgnore;
  List.OnChange := ListChanged;
  fMaxCount := 10;
end;

destructor TRecentList.Destroy;
begin
  List.OnChange := nil;
  List.Free;
  inherited Destroy;
end;

procedure TRecentList.SetMaxCount(Value: Integer);
begin
  fMaxCount := Value;
  while List.Count > MaxCount do
    List.Delete(List.Count - 1);
end;

function TRecentList.GetCount: Integer;
begin
  Result := List.Count;
end;

function TRecentList.GetCaptions(Index: Integer): String;
begin
  if Index < 10 then
    Result := Format('&%u %s', [Index, List[Index]])
  else if Index < 36 then
    Result := Format('&%s %s', [Chr(Ord('A') + (Index - 10)), List[Index]])
  else
    Result := List[Index];
end;

function TRecentList.GetItems(Index: Integer): String;
begin
  Result := List[Index];
end;

function TRecentList.GetRegNames(const BaseName: String; Index: Integer): String;
begin
  Result := Format('%s %u', [BaseName, Index + 1]);
end;

procedure TRecentList.ListChanged(Sender: TObject);
begin
  fModified := True;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TRecentList.SaveToRegistry(R: TRegistry; const BaseName: String);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    R.WriteString(GetRegNames(BaseName, I), Items[I]);
  for I := List.Count to MaxInt do
    if R.ValueExists(GetRegNames(BaseName, I)) then
      R.DeleteValue(GetRegNames(BaseName, I))
    else
      Break;
  fModified := False;
end;

procedure TRecentList.LoadFromRegistry(R: TRegistry; const BaseName: String);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to MaxCount - 1 do
      if R.ValueExists(GetRegNames(BaseName, I)) then
        List.Add(R.ReadString(GetRegNames(BaseName, I)))
      else
        Break;
  finally
    List.EndUpdate;
  end;
  fModified := False;
end;

procedure TRecentList.AssignTo(Dest: TPersistent);
begin
  if Dest is TStrings then
    TStrings(Dest).Assign(List)
  else
    inherited AssignTo(Dest);
end;

procedure TRecentList.BeginUpdate;
begin
  List.BeginUpdate;
end;

procedure TRecentList.EndUpdate;
begin
  List.EndUpdate;
end;

procedure TRecentList.Clear;
begin
  List.Clear;
end;

procedure TRecentList.Add(const Item: String);
var
  Index: Integer;
begin
  if Item <> '' then
  begin
    List.BeginUpdate;
    try
      Index := List.IndexOf(Item);
      if Index >= 0 then
      begin
        List[Index] := Item;
        if Index <> 0 then
          List.Move(Index, 0);
      end
      else
        List.Insert(0, Item);
      while List.Count > MaxCount do
        List.Delete(List.Count - 1);
    finally
      List.EndUpdate;
    end;
  end;
end;

function TRecentList.Remove(const Item: String): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Index := List.IndexOf(Item);
  if Index >= 0 then
  begin
    List.Delete(Index);
    Result := True;
  end;
end;

procedure TRecentList.Delete(Index: Integer);
begin
  List.Delete(Index);
end;

function TRecentList.RemoveNonexistent: Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := List.Count - 1 downto 0 do
    if not FileExists(List[Index]) then
    begin
      List.Delete(Index);
      Result := True;
    end;
end;

end.
