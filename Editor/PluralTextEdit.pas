{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz Khojasteh                                                         }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit PluralTextEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls;

type
  TPluralTextEdit = class
  private
    fText: String;
    fHasPluralForms: Boolean;
    fPluralIndex: Integer;
    fModified: Boolean;
    fEditControl: TMemo;
    function GetText: String;
    procedure SetText(const Value: String);
    procedure SetPluralIndex(Value: Integer);
    function GetModified: Boolean;
    procedure SetModified(Value: Boolean);
  public
    constructor Create(AEditControl: TMemo);
    procedure Clear;
    procedure ReplaceSelection(NewText: String);
    procedure Select(SelStart, SelLength: Integer);
    property EditControl: TMemo read fEditControl;
    property Modified: Boolean read GetModified write SetModified;
    property HasPluralForms: Boolean read fHasPluralForms write fHasPluralForms;
    property PluralIndex: Integer read fPluralIndex write SetPluralIndex;
    property Text: String read GetText write SetText;
  end;

implementation

uses
  i18nZStrList;

{ TPluralTextEdit }

constructor TPluralTextEdit.Create(AEditControl: TMemo);
begin
  fEditControl := AEditControl;
end;

function TPluralTextEdit.GetModified: Boolean;
begin
  if EditControl.Modified then
    fModified := True;
  Result := fModified;
end;

procedure TPluralTextEdit.SetModified(Value: Boolean);
begin
  fModified := Value;
  if not fModified and EditControl.Modified then
    EditControl.Modified := False;
end;

function TPluralTextEdit.GetText: String;
begin
  if EditControl.Modified then
  begin
    if fHasPluralForms then
      ZStrings.SetSubStrAt(fText, fPluralIndex, EditControl.Text)
    else
      fText := EditControl.Text;
    EditControl.Modified := False;
  end;
  Result := fText;
end;

procedure TPluralTextEdit.SetText(const Value: String);
var
  SavedOnChange: TNotifyEvent;
begin
  if fText <> Value then
  begin
    fText := Value;
    SavedOnChange := EditControl.OnChange;
    EditControl.OnChange := nil;
    try
      if fHasPluralForms then
        EditControl.Text := ZStrings.GetSubStrAt(fText, fPluralIndex)
      else
        EditControl.Text := fText;
      EditControl.Modified := False;
    finally
      EditControl.OnChange := SavedOnChange;
    end;
  end;
  EditControl.Modified := False;
  fModified := False;
end;

procedure TPluralTextEdit.SetPluralIndex(Value: Integer);
var
  SavedOnChange: TNotifyEvent;
begin
  if fPluralIndex <> Value then
  begin
    if fHasPluralForms and EditControl.Modified then
    begin
      fModified := True;
      ZStrings.SetSubStrAt(fText, fPluralIndex, EditControl.Text);
      EditControl.Modified := False;
    end;
    fPluralIndex := Value;
    if fHasPluralForms then
    begin
      SavedOnChange := EditControl.OnChange;
      EditControl.OnChange := nil;
      try
        EditControl.Text := ZStrings.GetSubStrAt(fText, fPluralIndex);
        EditControl.Modified := False;
      finally
        EditControl.OnChange := SavedOnChange;
      end;
    end;
  end;
end;

procedure TPluralTextEdit.Clear;
begin
  EditControl.Clear;
  fModified := False;
  fHasPluralForms := False;
  fPluralIndex := 0;
  fText := '';
end;

procedure TPluralTextEdit.ReplaceSelection(NewText: String);
var
  SavedOnChange: TNotifyEvent;
begin
  SavedOnChange := EditControl.OnChange;
  EditControl.OnChange := nil;
  try
    EditControl.Perform(EM_REPLACESEL, 1, LPARAM(PChar(NewText)));
    EditControl.Modified := False;
  finally
    EditControl.OnChange := SavedOnChange;
  end;
  if fHasPluralForms then
    ZStrings.SetSubStrAt(fText, fPluralIndex, EditControl.Text)
  else
    fText := EditControl.Text;
end;

procedure TPluralTextEdit.Select(SelStart, SelLength: Integer);
var
  LineStart: Integer;
begin
  if HasPluralForms then
  begin
    PluralIndex := ZStrings.IndexFromCharPos(Text, SelStart);
    LineStart := ZStrings.CharPosFromIndex(Text, PluralIndex);
    EditControl.SelStart := SelStart - LineStart;
  end
  else
    EditControl.SelStart := SelStart - 1;
  EditControl.SelLength := SelLength;
end;

end.
