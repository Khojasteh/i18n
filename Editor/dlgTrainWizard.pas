{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  Translation Repository Editor for Delphi Applications                       }
{                                                                              }
{  Copyright (c) Kambiz Khojasteh                                              }
{  https://github.com/khojasteh/i18n                                           }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgTrainWizard;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCatalog, i18nCore, i18nLocalizer, StdCtrls, ComCtrls,
  ShellCtrls, ExtCtrls, ImgList;

type
  TRepositoryTrainWizardDialog = class(TForm)
    ClientPanel: TPanel;
    Notebook: TNotebook;
    Header1: TPanel;
    Title1: TLabel;
    Subtitle1: TLabel;
    lblFolder: TLabel;
    Header2: TPanel;
    Title2: TLabel;
    Subtitle2: TLabel;
    Folders: TShellTreeView;
    edFolder: TEdit;
    LastNotice: TLabel;
    Header3: TPanel;
    Title3: TLabel;
    Subtitle3: TLabel;
    Summary: TRichEdit;
    ButtonsPanel: TPanel;
    btnCancel: TButton;
    btnStart: TButton;
    btnBack: TButton;
    btnNext: TButton;
    Translator: TTranslator;
    cbIncludeSubfolders: TCheckBox;
    cb_i18n: TCheckBox;
    cb_m17n: TCheckBox;
    Header4: TPanel;
    Title4: TLabel;
    Subtitle4: TLabel;
    PleaseWait: TLabel;
    ProgressBar: TProgressBar;
    Header5: TPanel;
    Title5: TLabel;
    Subtitle5: TLabel;
    btnFinish: TButton;
    SearchDetails: TPanel;
    lblSearchingFolder: TLabel;
    SearchingFolder: TLabel;
    UsedFiles: TListView;
    TotalPhraseCount: TLabel;
    ImageList: TImageList;
    procedure FormShow(Sender: TObject);
    procedure NotebookPageChanged(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure SourceTypeClick(Sender: TObject);
    procedure edFolderChange(Sender: TObject);
    procedure edFolderExit(Sender: TObject);
    procedure FoldersChange(Sender: TObject; Node: TTreeNode);
    procedure FoldersEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure UsedFilesAdvancedCustomDraw(Sender: TCustomListView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure UsedFilesDblClick(Sender: TObject);
    procedure UsedFilesInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure TranslatorLoaded(Sender: TObject);
  private
    Repository: TTranslationRepository;
    OldWindowProc: TWndMethod;
    Cancelled: Boolean;
    procedure DoTraining;
    procedure UpdateSummary;
    procedure LoadPersistentUserChoices;
    procedure SavePersistentUserChoices;
    procedure NewWindowProc(var Msg: TMessage);
  private
    function LearnByCatalog(const FileName: String): Boolean;
    function LearnByRepository(const FileName: String): Boolean;
  public
    class function Execute(ARepository: TTranslationRepository): Boolean;
  end;

implementation

{$R *.dfm}

uses
  Registry, DataModule, i18nBBCode, i18nUnicode, RichEdit;

const
  SRepositoryFile = 'Repository File';
  SCancelConfirm  = 'Do you want to cancel training?';
  SNoItemToShow   = 'No file has been found to be used for the training.';
  SOnePhrase      = 'one phrase';
  SManyPhrases    = '{0:N0} phrases';
  SOneGorup       = 'one synonymical group';
  SManyGorups     = '{0:N0} synonymical groups';

{ TRepositoryTrainWizardDialog }

class function TRepositoryTrainWizardDialog.Execute(ARepository:
  TTranslationRepository): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      Repository := ARepository;
      LoadPersistentUserChoices;
      if ShowModal = mrOK then
      begin
        SavePersistentUserChoices;
        Result := True;
      end;
    finally
      Free;
    end;
end;

procedure TRepositoryTrainWizardDialog.NewWindowProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_NOTIFY) and (TWMNotify(Msg).NMHdr^.code = EN_LINK) then
    with TENLink(Pointer(TWMNotify(Msg).NMHdr)^) do
      if Msg = WM_LBUTTONDOWN then
      begin
        Summary.Perform(EM_EXSETSEL, 0, Integer(@chrg));
        OpenFileLocation(Summary.SelText);
      end;
  OldWindowProc(Msg);
end;

procedure TRepositoryTrainWizardDialog.LoadPersistentUserChoices;
var
  R: TRegistry;
begin
  R := TRegistry.Create(KEY_QUERY_VALUE);
  try
    if R.OpenKeyReadOnly(AppRegRootKey) then
    begin
      if R.ValueExists('Train.i18n') then
        cb_i18n.Checked := R.ReadBool('Train.i18n');
      if R.ValueExists('Train.m17n') then
        cb_m17n.Checked := R.ReadBool('Train.m17n');
      if R.ValueExists('Train.Path') and DirectoryExists(R.ReadString('Train.Path')) then
        Folders.Path := R.ReadString('Train.Path');
      if R.ValueExists('Train.Subfolders') then
        cbIncludeSubfolders.Checked := R.ReadBool('Train.Subfolders');
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

procedure TRepositoryTrainWizardDialog.SavePersistentUserChoices;
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    if R.OpenKey(AppRegRootKey, True) then
    begin
      R.WriteBool('Train.i18n', cb_i18n.Checked);
      R.WriteBool('Train.m17n', cb_m17n.Checked);
      R.WriteString('Train.Path', edFolder.Text);
      R.WriteBool('Train.Subfolders', cbIncludeSubfolders.Checked);
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

procedure TRepositoryTrainWizardDialog.FormCreate(Sender: TObject);
begin
  OldWindowProc := Summary.Parent.WindowProc;
  Summary.Parent.WindowProc := NewWindowProc;
  Summary.HandleNeeded;
  Summary.Perform(EM_SETEVENTMASK, 0, Summary.Perform(EM_GETEVENTMASK, 0, 0) or ENM_LINK);
end;

procedure TRepositoryTrainWizardDialog.FormShow(Sender: TObject);
begin
  Notebook.PageIndex := 0;
  NotebookPageChanged(nil);
end;

procedure TRepositoryTrainWizardDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrCancel) and (Notebook.PageIndex = Notebook.Pages.Count - 1) then
    ModalResult := mrOk;
end;

procedure TRepositoryTrainWizardDialog.FormDestroy(Sender: TObject);
begin
  Summary.Parent.WindowProc := OldWindowProc;
end;

type TControlHack = class(TControl);

procedure TRepositoryTrainWizardDialog.TranslatorLoaded(Sender: TObject);
begin
  TControlHack(Folders).ParentBiDiMode := False;
  TControlHack(Folders).BiDiMode := bdLeftToRight;
end;

procedure TRepositoryTrainWizardDialog.NotebookPageChanged(Sender: TObject);
begin
  case Notebook.PageIndex of
    0: btnNext.Enabled := cb_i18n.Checked or cb_m17n.Checked;
    1: btnNext.Enabled := DirectoryExists(edFolder.Text);
    2: UpdateSummary;
  end;
  btnBack.Visible := (Notebook.PageIndex > 0) and (Notebook.PageIndex < Notebook.Pages.Count - 2);
  btnNext.Visible := (Notebook.PageIndex < Notebook.Pages.Count - 3);
  btnStart.Visible := (Notebook.PageIndex = Notebook.Pages.Count - 3);
  btnCancel.Visible := (Notebook.PageIndex < Notebook.Pages.Count - 1);
  btnFinish.Visible := (Notebook.PageIndex = Notebook.Pages.Count - 1);
  Notebook.Update;
end;

procedure TRepositoryTrainWizardDialog.btnBackClick(Sender: TObject);
begin
  case Notebook.PageIndex of
    1: Notebook.PageIndex := 0;
    2: Notebook.PageIndex := 1;
  end;
end;

procedure TRepositoryTrainWizardDialog.btnNextClick(Sender: TObject);
begin
  case Notebook.PageIndex of
    0: Notebook.PageIndex := 1;
    1: Notebook.PageIndex := 2;
  end;
end;

procedure TRepositoryTrainWizardDialog.btnStartClick(Sender: TObject);
begin
  Notebook.PageIndex := Notebook.Pages.Count - 2;
  EnableMenuItem(GetSystemMenu(Handle, False), SC_CLOSE, MF_DISABLED);
  try
    DoTraining;
  finally
    EnableMenuItem(GetSystemMenu(Handle, False), SC_CLOSE, MF_ENABLED);
  end;
  if not Cancelled then
    Notebook.PageIndex := Notebook.Pages.Count - 1;
end;

procedure TRepositoryTrainWizardDialog.btnCancelClick(Sender: TObject);
begin
  if not Cancelled and (Notebook.PageIndex = Notebook.Pages.Count - 2) then
  begin
    ProgressBar.State := pbsPaused;
    if DM.MsgDlg.Confirm(Translator.GetText(SCancelConfirm)) then
      Cancelled := True
    else
      ModalResult := mrNone;
    ProgressBar.State := pbsNormal;
  end;
end;

procedure TRepositoryTrainWizardDialog.SourceTypeClick(Sender: TObject);
begin
  btnNext.Enabled := cb_i18n.Checked or cb_m17n.Checked;
end;

procedure TRepositoryTrainWizardDialog.edFolderChange(Sender: TObject);
begin
  btnNext.Enabled := DirectoryExists(edFolder.Text);
end;

procedure TRepositoryTrainWizardDialog.edFolderExit(Sender: TObject);
var
  Folder: String;
begin
  if edFolder.Modified then
  begin
    edFolder.Modified := False;
    Folder := Trim(edFolder.Text);
    while (Length(Folder) > 3) and not DirectoryExists(Folder) do
      Folder := ExtractFilePath(Folder);
    if DirectoryExists(Folder) then
      Folders.Path := Folder
    else
      Folders.Selected := Folders.Items[0];
    if Folders.Selected <> nil then
      Folders.Selected.MakeVisible;
  end;
end;

procedure TRepositoryTrainWizardDialog.FoldersChange(Sender: TObject; Node: TTreeNode);
begin
  if DirectoryExists(Folders.Path) then
  begin
    edFolder.Text := Folders.Path;
    edFolder.Modified := False;
  end
  else
    edFolder.Clear;
end;

procedure TRepositoryTrainWizardDialog.FoldersEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TRepositoryTrainWizardDialog.UsedFilesAdvancedCustomDraw(
  Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
var
  TextRect: TRect;
begin
  if TListView(Sender).Items.Count = 0 then
  begin
    TextRect := ARect;
    InflateRect(TextRect, -40, -50);
    TListView(Sender).Canvas.Font.Color := clGrayText;
    DrawText(TListView(Sender).Canvas.Handle, PChar(Translator.GetText(SNoItemToShow)),
      -1, TextRect, TListView(Sender).DrawTextBiDiModeFlags(DT_CENTER or
      DT_NOPREFIX or DT_WORDBREAK));
  end;
end;

procedure TRepositoryTrainWizardDialog.UsedFilesDblClick(Sender: TObject);
begin
  if Assigned(UsedFiles.Selected) then
    with UsedFiles.Selected do
      OpenFileLocation(IncludeTrailingPathDelimiter(SubItems[1]) + Caption);
end;

procedure TRepositoryTrainWizardDialog.UsedFilesInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: string);
begin
  InfoTip := IncludeTrailingPathDelimiter(Item.SubItems[1]) + Item.Caption;
end;

procedure TRepositoryTrainWizardDialog.UpdateSummary;
begin
  with TStringBuilder.Create do
    try
      // Repository File
      Append('[ls=1.5][b]').Append(Translator.GetText(SRepositoryFile)).Append(':').AppendLine('[/b][/ls]');
      Append('[list]');
      if Summary.IsRightToLeft then
        Append(UCC_LRE);
      if FileExists(DM.RepositoryFileName) then
        Append('[link]').Append(DM.RepositoryFileName).Append('[/link]')
      else
        Append(DM.RepositoryFileName);
      if Summary.IsRightToLeft then
        Append(UCC_PDF);
      AppendLine;
      Append('[/list]').AppendLine.AppendLine;
      // Source Types
      Append('[ls=1.5][b]').Append(Notebook.Pages[0]).Append(':').AppendLine('[/b][/ls]');
      Append('[list]');
      if cb_i18n.Checked then
        AppendLine(cb_i18n.Caption);
      if cb_m17n.Checked then
        AppendLine(cb_m17n.Caption);
      AppendLine('[/list]').AppendLine;
      // Search Path
      Append('[ls=1.5][b]').Append(Notebook.Pages[1]).Append(':').AppendLine('[/b][/ls]');
      Append('[list]');
      if Summary.IsRightToLeft then
        Append(UCC_LRE);
      Append(edFolder.Text);
      if Summary.IsRightToLeft then
        Append(UCC_PDF);
      AppendLine;
      if cbIncludeSubfolders.Checked then
        AppendLine(cbIncludeSubfolders.Caption);
      AppendLine('[/list]');
      BBCodes.Apply(Summary, ToString);
    finally
      Free;
    end;
end;

function TRepositoryTrainWizardDialog.LearnByCatalog(const FileName: String): Boolean;
var
  Catalog: TTranslationCatalog;
begin
  Catalog := TTranslationCatalog.Create;
  try
    try
      Catalog.LoadFromFile(FileName);
      Repository.Learn(Catalog);
      Result := True;
    except
      Result := False;
    end;
  finally
    Catalog.Free;
  end;
end;

function TRepositoryTrainWizardDialog.LearnByRepository(const FileName: String): Boolean;
var
  Repository: TTranslationRepository;
begin
  Result := False;
  if not SameText(FileName, DM.RepositoryFileName) then
  begin
    Repository := TTranslationRepository.Create;
    try
      try
        Repository.LoadFromFile(FileName);
        Repository.Merge(Repository);
        Result := True;
      except
        Result := False;
      end;
    finally
      Repository.Free;
    end;
  end;
end;

procedure TRepositoryTrainWizardDialog.DoTraining;

  procedure LearnFrom(const FileName: String; TrainingMethod: Integer);
  var
    OldPhraseCount: Integer;
    Succeeded: Boolean;
  begin
    OldPhraseCount := Repository.PhraseCount;
    case TrainingMethod of
      0: Succeeded := LearnByCatalog(FileName);
      1: Succeeded := LearnByRepository(FileName);
    else
      Succeeded := False;
    end;
    if Succeeded then
      with UsedFiles.Items.Add do
      begin
        Caption := ExtractFileName(FileName);
        ImageIndex := TrainingMethod;
        SubItems.Add(DM.Localizer.FormatNumber('#,##0', Repository.PhraseCount - OldPhraseCount));
        SubItems.Add(ExcludeTrailingPathDelimiter(ExtractFilePath(FileName)));
      end;
  end;

  procedure Search(const Path: String);
  var
    SR: TSearchRec;
    Ext: String;
  begin
    SearchingFolder.Caption := Path;
    if FindFirst(Path + '\*.*', faNormal or faReadOnly or faDirectory, SR) = 0 then
      try
        repeat
          Application.ProcessMessages;
          if Cancelled then
            Exit;
          if (SR.Attr and faDirectory) <> faDirectory then
          begin
            Ext := LowerCase(ExtractFileExt(SR.Name));
            if (cb_i18n.State = cbChecked) and (Ext = i18nCatalogFileExt) then
              LearnFrom(Path + '\' + SR.Name, 0)
            else if (cb_m17n.State = cbChecked) and (Ext = i18nRepositoryFileExt) then
              LearnFrom(Path + '\' + SR.Name, 1);
          end
          else if (cbIncludeSubfolders.State = cbChecked) and
                  (SR.Name <> '.') and (SR.Name <> '..') then
          begin
            Search(Path + '\' + SR.Name);
            if Cancelled then
              Exit;
            SearchingFolder.Caption := Path;
          end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
  end;

var
  PhraseCountText: String;
  GroupCountText: String;
begin
  Cancelled := False;
  UsedFiles.Items.BeginUpdate;
  try
    Search(ExcludeTrailingPathDelimiter(edFolder.Text));
  finally
    UsedFiles.Items.EndUpdate;
  end;
  PhraseCountText := Translator.GetNText([SOnePhrase, SManyPhrases], Repository.PhraseCount);
  GroupCountText := Translator.GetNText([SOneGorup, SManyGorups], Repository.RelationCount);
  TotalPhraseCount.Caption := DM.Localizer.FormatCS(TotalPhraseCount.Caption,
    [DM.Localizer.FormatCS(PhraseCountText, [Repository.PhraseCount]),
     DM.Localizer.FormatCS(GroupCountText, [Repository.RelationCount])]);
end;

end.
