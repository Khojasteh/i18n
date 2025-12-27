{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz Khojasteh                                                         }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgPlainTextEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCore, StdCtrls, i18nLocalizer, ExtCtrls;

type
  TPlainTextEditorDialog = class(TForm)
    ButtonsPanel: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    Translator: TTranslator;
    Memo: TMemo;
  public
    class function Execute(const ACaption: String;
      var Value: String; Culture: TCultureInfo = nil): Boolean;
  end;

implementation

{$R *.dfm}

{ TPlainTextEditorDialog }

class function TPlainTextEditorDialog.Execute(const ACaption: String;
  var Value: String; Culture: TCultureInfo): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      Caption := ACaption;
      Memo.Text := Value;
      if Assigned(Culture) then
        Memo.BiDiMode := Culture.BiDiMode;
      if ShowModal = mrOk then
      begin
        Value := Trim(Memo.Text);
        Result := True;
      end;
    finally
      Free;
    end;
end;

end.
