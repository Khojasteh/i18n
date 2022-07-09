unit DataModule;

interface

uses
  SysUtils, Classes, i18nCore, i18nLocalizer, i18nDialogs, ImgList, i18nCtrls;

type
  TDM = class(TDataModule)
    Flags: TFlagImageList;
    MessageDialog: TMessageDialog;
    Localizer: TLocalizer;
    Translator: TTranslator;
    procedure LocalizerNotification(Sender: TObject;
      Reason: TLocalizerNotification);
  private
    { Private declarations }
    NoBiDiChangeWarning: Boolean;
  public
    { Public declarations }
  end;

var
  DM: TDM;

implementation

{$R *.dfm}

const
  SBiDiWarning =
    'I just wanted to inform you that changing BiDiMode of a form that has a ' +
    'common control as child, may cause an AV exception in ''comctl32.dll''. ' +
    'The i18n package is not responsible for this error. Delphi VCL has some ' +
    'bugs in implementation of the bi-directional controls, and this is one '  +
    'of them unfortunately.';

procedure TDM.LocalizerNotification(Sender: TObject;
  Reason: TLocalizerNotification);
begin
  if (Reason = lnBiDiModeChanging) and not NoBiDiChangeWarning then
  begin
    MessageDialog.CheckBox.Visible := True;
    MessageDialog.ShowWarning(Translator.GetText(SBiDiWarning));
    MessageDialog.CheckBox.Visible := False;
    NoBiDiChangeWarning := MessageDialog.CheckBox.Checked;
  end;
end;

end.
