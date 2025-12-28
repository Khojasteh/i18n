unit DataModule;

interface

uses
  SysUtils, Classes, i18nCore, i18nLocalizer, i18nDialogs, ImgList, i18nCtrls,
  System.ImageList;

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
    'Please be advised that changing the BiDiMode of a form that contains a ' +
    'common control as a child may result in an access violation exception ' +
    'in comctl32.dll. The i18n package is not responsible for this issue. ' +
    #13#10#13#10 +
    'This behavior is caused by known defects in the Delphi VCL ' +
    'implementation of bidirectional controls, of which this is one ' +
    'unfortunate example.';

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
