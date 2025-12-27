{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz Khojasteh                                                         }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgRename;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCore, StdCtrls, ExtCtrls, ImgList, ComCtrls, i18nLocalizer,
  i18nCatalog, i18nHashList, i18nCtrls;

type
  TRenameDialog = class(TForm)
    Translator: TTranslator;
    ButtonsPanel: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    CurNameLabel: TLabel;
    CurName: TEdit;
    NewNameLabel: TLabel;
    NewName: TEdit;
    ErrorMsg: TImageLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CurNameChange(Sender: TObject);
    procedure NewNameChange(Sender: TObject);
  private
    UsedNames: TStrings;
  public
    class function Execute(const ACaption: String; var AName: String;
      AUsedNames: TStrings): Boolean;
  end;

implementation

{$R *.dfm}

uses
  DataModule, i18nParser;

const
  SNoNameError        = 'Please enter the new name.';
  SInvalidNameError   = 'Please enter a valid identifier name.';
  SDuplicateNameError = 'The name already exists, please enter a unique name.';

{ TRenameDialog }

class function TRenameDialog.Execute(const ACaption: String;
  var AName: String; AUsedNames: TStrings): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      Caption := ACaption;
      CurName.Text := AName;
      UsedNames := AUsedNames;
      if ShowModal = mrOK then
      begin
        AName := NewName.Text;
        Result := True;
      end;
    finally
      Free;
    end;
end;

procedure TRenameDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  I: Integer;
begin
  if ModalResult = mrOk then
  begin
    NewName.Text := Trim(NewName.Text);
    if NewName.GetTextLen = 0 then
    begin
      ErrorMsg.Caption := Translator.GetText(SNoNameError);
      CanClose := False
    end
    else if not IsValidIdentifier(NewName.Text) then
    begin
      ErrorMsg.Caption := Translator.GetText(SInvalidNameError);
      CanClose := False;
    end
    else if SameText(NewName.Text, CurName.Text) then
    begin
      if NewName.Text = CurName.Text then
        ModalResult := mrIgnore;
    end
    else if Assigned(UsedNames) then
    begin
      for I := 0 to UsedNames.Count - 1 do
        if SameText(NewName.Text, UsedNames[I]) then
        begin
          ErrorMsg.Caption := Translator.GetText(SDuplicateNameError);
          CanClose := False;
          Break;
        end;
    end;
    if not CanClose then
    begin
      ActiveControl := NewName;
      ErrorMsg.Visible := True;
    end;
  end;
end;

procedure TRenameDialog.CurNameChange(Sender: TObject);
begin
  ErrorMsg.Visible := False;
end;

procedure TRenameDialog.NewNameChange(Sender: TObject);
begin
  ErrorMsg.Visible := False;
end;

end.
