program TransDemo;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  DataModule in 'DataModule.pas' {DM: TDataModule},
  Child in 'Child.pas' {ChildForm},
  Frame in 'Frame.pas' {SampleFrame: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
