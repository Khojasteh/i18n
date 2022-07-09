{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgLanguageSelector;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCore, StdCtrls, i18nCtrls, i18nLocalizer, ExtCtrls;

type
  TSelectLanguageDialog = class(TForm)
    Languages: TCultureListBox;
    ButtonsPanel: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    Prompt: TLabel;
    Translator: TTranslator;
    procedure LanguagesDrawItemLabel(Sender: TObject; Index: Integer; var Rect: TRect;
      State: TOwnerDrawState; var TheLabel: string; var DefaultDraw: Boolean);
    procedure FormShow(Sender: TObject);
    procedure LanguagesClick(Sender: TObject);
    procedure LanguagesDblClick(Sender: TObject);
  public
    class function Execute(const APrompt: String; var Culture: TCultureInfo;
      IncCultures: TReadonlyCultureList = nil;
      ExcCultures: TReadonlyCultureList = nil): Boolean;
  end;

implementation

{$R *.dfm}

{ TSelectLanguageDialog }

class function TSelectLanguageDialog.Execute(const APrompt: String;
  var Culture: TCultureInfo; IncCultures, ExcCultures: TReadonlyCultureList): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      if APrompt = '' then
        Prompt.Visible := False
      else
        Prompt.Caption := APrompt;
      if not Assigned(IncCultures) then
        IncCultures := World.Cultures;
      if Assigned(ExcCultures) then
        Languages.Items.Apply(IncCultures, laSrcUnique, ExcCultures)
      else
        Languages.Items.Assign(IncCultures);
      if Assigned(Culture) then
        Languages.ItemSelected := Culture;
      if Languages.ItemIndex < 0 then
        Languages.ItemIndex := 0;
      if ShowModal = mrOk then
      begin
        Culture := Languages.ItemSelected;
        Result := True;
      end;
    finally
      Free;
    end;
end;

procedure TSelectLanguageDialog.FormShow(Sender: TObject);
begin
  btnOK.Enabled := (Languages.ItemIndex >= 0);
end;

procedure TSelectLanguageDialog.LanguagesClick(Sender: TObject);
begin
  btnOK.Enabled := (Languages.ItemIndex >= 0);
end;

procedure TSelectLanguageDialog.LanguagesDblClick(Sender: TObject);
begin
  if Languages.ItemIndex >= 0 then
    ModalResult := mrOk;
end;

procedure TSelectLanguageDialog.LanguagesDrawItemLabel(Sender: TObject; Index: Integer;
  var Rect: TRect; State: TOwnerDrawState; var TheLabel: string;
  var DefaultDraw: Boolean);
begin
  if Languages.Items[Index].IsDefault then
    Languages.Canvas.Font.Style := [fsBold];
end;

end.
