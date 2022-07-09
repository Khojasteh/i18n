{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

program i18nEditor;

uses
  Forms,
  Dialogs,
  SysUtils,
  i18nCore,
  MRU in 'MRU.pas',
  DefinitionList in 'DefinitionList.pas',
  PluralTextEdit in 'PluralTextEdit.pas',
  DataModule in 'DataModule.pas' {DM: TDataModule},
  frmMain in 'frmMain.pas' {MainForm},
  frmTranslationEditor in 'frmTranslationEditor.pas' {TranslationEditor},
  dlgPlainTextEditor in 'dlgPlainTextEditor.pas' {PlainTextEditorDialog},
  dlgPluralRuleEditor in 'dlgPluralRuleEditor.pas' {PluralRuleEditorDialog},
  dlgRename in 'dlgRename.pas' {RenameDialog},
  dlgSearch in 'dlgSearch.pas' {SearchDialog},
  dlgSuggestion in 'dlgSuggestion.pas' {SuggestionDialog},
  dlgLanguageSelector in 'dlgLanguageSelector.pas' {SelectLanguageDialog},
  dlgImportWizard in 'dlgImportWizard.pas' {ImportWizardDialog},
  dlgExportWizard in 'dlgExportWizard.pas' {ExportWizardDialog},
  dlgTrainWizard in 'dlgTrainWizard.pas' {RespositoryTrainWizardDialog},
  dlgOptions in 'dlgOptions.pas' {OptionsDialog},
  dlgAbout in 'dlgAbout.pas' {AboutDialog};

{$R *.res}

const
  SOldWinVersionError = 'This application runs only on Windows XP and later.';

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.DefaultFont.Size := 9;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'i18n Editor';
  if i18nAvailable then
  begin
    Application.CreateForm(TDM, DM);
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end
  else
    MessageDlg(SOldWinVersionError, mtCustom, [mbOK], 0);
end.
