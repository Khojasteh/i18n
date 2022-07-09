unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, ImgList, StdActns, ActnList, Dialogs, ComCtrls, ExtCtrls, StdCtrls;

type
  TMainForm = class(TForm)
    SourceCode: TRichEdit;
    Toolbar: TPanel;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    ActionList: TActionList;
    ImageList: TImageList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    PopupMenu: TPopupMenu;
    Undo1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    btnProcess: TButton;
    FilePath: TButtonedEdit;
    Splitter1: TSplitter;
    FoundTranslatables: TRichEdit;
    procedure SourceCodeSelectionChange(Sender: TObject);
    procedure btnProcessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FilePathRightButtonClick(Sender: TObject);
    procedure SourceCodeChange(Sender: TObject);
  private
    procedure Load(const FileName: String);
    function Process: Boolean;
    procedure DisplayCursorLocation;
  end;

var
  MainForm: TMainForm;

implementation

uses
  i18nParser, i18nCatalog;

{$R *.dfm}

function TMainForm.Process: Boolean;
var
  Items: TTextItems;
  Collector: TPascalStringCollector;
begin
  Result := False;
  StatusBar.Panels[1].Text := 'Processing...';
  StatusBar.Update;
  try
    Collector := TPascalStringCollector.Create(SourceCode.Text);
    try
      Items := TTextItems.Create();
      try
        Collector.GetTranslatableStrings(Items);
        SourceCode.Text := Collector.SetTranslatableStrings(Items);
        Items.AssignTo(FoundTranslatables.Lines);
        StatusBar.Panels[1].Text := Format('%d translatable(s) found', [Items.Count]);
      finally
        Items.Free;
      end;
      Result := True;
    finally
      Collector.Free;
    end;
  except
    on E: EPascalParserError do
    begin
      SourceCode.SelStart := SourceCode.Perform(EM_LINEINDEX, E.LineNo - 1, 0) + E.ColNo - 1;
      SourceCode.SelLength := Length(E.Token);
      SourceCode.Perform(EM_SCROLLCARET, 0, 0);
      SourceCode.SetFocus;
      StatusBar.Panels[1].Text := E.Message;
    end
    else
      raise;
  end;
  Update;
end;

procedure TMainForm.Load(const FileName: String);
var
  I: Integer;
  Code: String;
  Lines: TStringList;
begin
  FilePath.Text := FileName;
  FilePath.Update;
  StatusBar.Panels[1].Text := 'Loading file...';
  StatusBar.Update;
  try
    Lines := TStringList.Create;
    try
      try
        Lines.LoadFromFile(FileName);
        Code := Lines.Text;
      except
        Code := '';
        raise;
      end;
    finally
      Lines.Free;
    end;
    // Some old Delphi codes have null character inside!
    // The following loop get rid of them.
    I := Pos(#0, Code);
    while I <> 0 do
    begin
      Delete(Code, I, 1);
      I := Pos(#0, Code);
    end;
  finally
    SourceCode.Text := Code;
    SourceCode.SelStart := 0;
    SourceCode.SelLength := 0;
    DisplayCursorLocation;
    StatusBar.Panels[1].Text := '';
  end;
end;

procedure TMainForm.DisplayCursorLocation;
var
  Line, Col: Integer;
begin
  Line := SourceCode.Perform(EM_LINEFROMCHAR, SourceCode.SelStart, 0);
  Col := SourceCode.SelStart - SourceCode.Perform(EM_LINEINDEX, Line, 0);
  StatusBar.Panels[0].Text := Format('%u:%u', [Line + 1, Col + 1]);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DisplayCursorLocation;
  btnProcess.Enabled := (SourceCode.Text <> '');
end;

procedure TMainForm.FilePathRightButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    Load(OpenDialog.FileName);
end;

procedure TMainForm.btnProcessClick(Sender: TObject);
begin
  Process;
end;

procedure TMainForm.SourceCodeChange(Sender: TObject);
begin
  btnProcess.Enabled := (SourceCode.Text <> '');
end;

procedure TMainForm.SourceCodeSelectionChange(Sender: TObject);
begin
  DisplayCursorLocation;
end;

end.
