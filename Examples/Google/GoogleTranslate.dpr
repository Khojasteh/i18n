program GoogleTranslate;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Google Translator';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
