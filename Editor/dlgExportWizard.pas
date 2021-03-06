{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgExportWizard;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCore, i18nLocalizer, i18nHashList, i18nCatalog, StdCtrls,
  ExtCtrls, ComCtrls, CheckLst, jpeg, ImgList, Buttons, ShellCtrls;

type
  TExportWizardDialog = class(TForm)
    Translator: TTranslator;
    ClientPanel: TPanel;
    Notebook: TNotebook;
    ImageList: TImageList;
    Header1: TPanel;
    Title1: TLabel;
    Subtitle1: TLabel;
    Domains: TListView;
    Header2: TPanel;
    Title2: TLabel;
    Subtitle2: TLabel;
    Languages: TListView;
    Header3: TPanel;
    Title3: TLabel;
    Subtitle3: TLabel;
    gbDetails: TGroupBox;
    cbIgnoreDeveloperComments: TCheckBox;
    cbIgnoreTranslatorNotes: TCheckBox;
    gbOutput: TGroupBox;
    cbGroupByDomain: TCheckBox;
    cbGroupByLanguage: TCheckBox;
    cbCompressed: TCheckBox;
    Header4: TPanel;
    Title4: TLabel;
    Subtitle4: TLabel;
    lblFolder: TLabel;
    Folders: TShellTreeView;
    Header5: TPanel;
    Title5: TLabel;
    Subtitle5: TLabel;
    ButtonsPanel: TPanel;
    btnBack: TButton;
    btnNext: TButton;
    btnStart: TButton;
    btnCancel: TButton;
    cbUsablesOnly: TCheckBox;
    lblBaseName: TLabel;
    edBaseName: TComboBox;
    LastNotice: TLabel;
    Summary: TRichEdit;
    edFolder: TEdit;
    PleaseWait: TLabel;
    Header6: TPanel;
    Title6: TLabel;
    Subtitle6: TLabel;
    ProgressBar: TProgressBar;
    btnFinish: TButton;
    Header7: TPanel;
    Title7: TLabel;
    Subtitle7: TLabel;
    OutputFiles: TListView;
    procedure NotebookPageChanged(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure DomainsItemChecked(Sender: TObject; Item: TListItem);
    procedure LanguagesItemChecked(Sender: TObject; Item: TListItem);
    procedure edBaseNamChange(Sender: TObject);
    procedure edBaseNameExit(Sender: TObject);
    procedure FoldersChange(Sender: TObject; Node: TTreeNode);
    procedure FoldersEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ExportByGroupClick(Sender: TObject);
    procedure edFolderChange(Sender: TObject);
    procedure edFolderExit(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OutputFilesAdvancedCustomDraw(Sender: TCustomListView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure OutputFilesDblClick(Sender: TObject);
    procedure OutputFilesInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure TranslatorLoaded(Sender: TObject);
  private
    FirstPage: Integer;
    Catalog: TTranslationCatalog;
    procedure DoExport;
    procedure UpdateSummary;
    procedure InitControls;
    procedure LoadPersistentUserChoices;
    procedure SavePersistentUserChoices;
  public
    class function Execute(ACatalog: TTranslationCatalog): Boolean;
  end;

implementation

{$R *.dfm}

uses
  Registry, CommCtrl, DataModule, i18nBBCode, i18nUnicode;

const
  SOverwriteConfirm = '{0} already exists.'#13#10#13#10
                    + 'Do you want to replace it?';
  SSaveError        = 'Unable to write to {0}.';
  SFileName         = 'File Name:';
  SBaseFileName     = 'File Name Prefix:';
  SDomainTemplate   = '<DOMAIN>';
  SLocaleTemplate   = '<LOCALE>';
  SNoItemToShow     = 'No file has been generated by the export.';

{ Helper Functions }

function IsDirectoryWritable(const Dir: String): Boolean;
var
  TempFile: array[0..MAX_PATH] of Char;
begin
  if DirectoryExists(Dir) and (GetTempFileName(PChar(Dir), 'DA', 0, TempFile) <> 0) then
    Result := Windows.DeleteFile(TempFile)
  else
    Result := False;
end;

function EnsureValidFileName(const FileName: String;
  IllegalCharReplace: Char = '_'): String;
const
  IllegalChars = [':', '/', '\', '*', '?', '<', '>', '|', '"'];
var
  I: Integer;
begin
  Result := FileName;
  for I := Length(Result) downto 1 do
    if CharInSet(Result[I], IllegalChars) then
      Result[I] := IllegalCharReplace;
end;

function AnyCheckedItem(List: TListView): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to List.Items.Count - 1 do
    if List.Items[I].Checked then
    begin
      Result := True;
      Exit;
    end;
end;

{ TExportWizardDialog }

class function TExportWizardDialog.Execute(ACatalog: TTranslationCatalog): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      Catalog := ACatalog;
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

procedure TExportWizardDialog.LoadPersistentUserChoices;
var
  R: TRegistry;
begin
  R := TRegistry.Create(KEY_QUERY_VALUE);
  try
    if R.OpenKeyReadOnly(AppRegRootKey) then
    begin
      if R.ValueExists('Export.IgnoreComments') then
        cbIgnoreDeveloperComments.Checked := R.ReadBool('Export.IgnoreComments');
      if R.ValueExists('Export.IgnoreNotes') then
        cbIgnoreTranslatorNotes.Checked := R.ReadBool('Export.IgnoreNotes');
      if R.ValueExists('Export.AcceptablesOnly') then
        cbUsablesOnly.Checked := R.ReadBool('Export.AcceptablesOnly');
      if R.ValueExists('Export.GroupByLanguage') then
        cbGroupByLanguage.Checked := R.ReadBool('Export.GroupByLanguage');
      if R.ValueExists('Export.GroupByDomain') then
        cbGroupByDomain.Checked := R.ReadBool('Export.GroupByDomain');
      if R.ValueExists('Export.Compressed') then
        cbCompressed.Checked := R.ReadBool('Export.Compressed');
      if R.ValueExists('Export.Path') and DirectoryExists(R.ReadString('Export.Path')) then
        Folders.Path := R.ReadString('Export.Path');
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
  DM.RecentExportNames.AssignTo(edBaseName.Items);
end;

procedure TExportWizardDialog.SavePersistentUserChoices;
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    if R.OpenKey(AppRegRootKey, True) then
    begin
      R.WriteBool('Export.IgnoreComments', cbIgnoreDeveloperComments.Checked);
      R.WriteBool('Export.IgnoreNotes', cbIgnoreTranslatorNotes.Checked);
      R.WriteBool('Export.AcceptablesOnly', cbUsablesOnly.Checked);
      R.WriteBool('Export.GroupByLanguage', cbGroupByLanguage.Checked);
      R.WriteBool('Export.GroupByDomain', cbGroupByDomain.Checked);
      R.WriteBool('Export.Compressed', cbCompressed.Checked);
      R.WriteString('Export.Path', edFolder.Text);
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
  DM.RecentExportNames.Add(edBaseName.Text);
end;

procedure TExportWizardDialog.InitControls;
var
  Domain: TTextDomain;
  Culture: TCultureInfo;
  I: Integer;
begin
  if cbGroupByDomain.Checked or cbGroupByLanguage.Checked then
    lblBaseName.Caption := Translator.GetText(SBaseFileName)
  else
    lblBaseName.Caption := Translator.GetText(SFileName);
  if Application.MainForm.ActiveMDIChild <> nil then
    edBaseName.Text := ChangeFileExt(ExtractFileName(Application.MainForm.ActiveMDIChild.Caption), '');
  // Text Domain
  Domains.Items.BeginUpdate;
  try
    Domains.Items.Clear;
    Domain := Catalog.TextDomains.First;
    while Assigned(Domain) do
    begin
      with Domains.Items.Add do
      begin
        Data := Domain;
        Caption := Domain.Name;
        SubItems.Add(DM.Localizer.FormatNumber('#,##0;; ', Domain.Properties.Count));
        SubItems.Add(DM.Localizer.FormatNumber('#,##0;; ', Domain.Literals.Count));
        ImageIndex := 1;
        Checked := True;
      end;
      Domain := Domain.Next;
    end;
  finally
    Domains.Items.EndUpdate;
  end;
  // Languages
  Languages.Items.BeginUpdate;
  try
    Languages.Items.Clear;
    for I := 0 to Catalog.Cultures.Count - 1 do
    begin
      Culture := Catalog.Cultures[I];
      with Languages.Items.Add do
      begin
        Data := Culture;
        Caption := Culture.LocalizedDisplayName;
        SubItems.Add(DM.Localizer.FormatNumber('#,##0.00', Catalog.StatsOf(Culture).Progress));
        ImageIndex := ImageList.Count;
        DM.CopyFlagTo(Culture, ImageList, ImageIndex);
        Checked := True;
      end;
    end;
    Culture := Catalog.NativeCulture;
    if Assigned(Culture) and not Catalog.Cultures.Exists(Culture) then
      with Languages.Items.Add do
      begin
        Data := Culture;
        Caption := Culture.LocalizedDisplayName;
        SubItems.Add(DM.Localizer.FormatNumber('#,##0.00', 100.0));
        ImageIndex := ImageList.Count;
        DM.CopyFlagTo(Culture, ImageList, ImageIndex);
        Checked := True;
      end;
  finally
    Languages.Items.EndUpdate;
  end;
  // Initialize
  FirstPage := 0;
  if Domains.Items.Count = 1 then
  begin
    Inc(FirstPage);
    if Languages.Items.Count = 1 then
      Inc(FirstPage);
  end;
  Notebook.PageIndex := FirstPage;
  NotebookPageChanged(nil);
end;

procedure TExportWizardDialog.FormShow(Sender: TObject);
begin
  InitControls;
end;

procedure TExportWizardDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrCancel) and (Notebook.PageIndex = Notebook.Pages.Count - 1) then
    ModalResult := mrOk;
end;

type TControlHack = class(TControl);

procedure TExportWizardDialog.TranslatorLoaded(Sender: TObject);
begin
  TControlHack(Folders).BiDiMode := bdLeftToRight;
  TControlHack(Folders).ParentBiDiMode := False;
end;

procedure TExportWizardDialog.NotebookPageChanged(Sender: TObject);
begin
  case Notebook.PageIndex of
    0: btnNext.Enabled := AnyCheckedItem(Domains);
    1: btnNext.Enabled := AnyCheckedItem(Languages);
    2: btnNext.Enabled := cbGroupByDomain.Checked or cbGroupByLanguage.Checked or (edBaseName.GetTextLen <> 0);
    3: btnNext.Enabled := IsDirectoryWritable(edFolder.Text);
    4: UpdateSummary;
  end;
  btnBack.Visible := (Notebook.PageIndex > FirstPage) and (Notebook.PageIndex < Notebook.Pages.Count - 2);
  btnNext.Visible := (Notebook.PageIndex < Notebook.Pages.Count - 3);
  btnStart.Visible := (Notebook.PageIndex = Notebook.Pages.Count - 3);
  btnCancel.Visible := (Notebook.PageIndex < Notebook.Pages.Count - 1);
  btnCancel.Enabled := (Notebook.PageIndex < Notebook.Pages.Count - 2);
  btnFinish.Visible := (Notebook.PageIndex = Notebook.Pages.Count - 1);
  Notebook.Update;
end;

procedure TExportWizardDialog.btnBackClick(Sender: TObject);
begin
  case Notebook.PageIndex of
    1: if Domains.Items.Count <> 1 then
         Notebook.PageIndex := 0;
    2: if Languages.Items.Count <> 1 then
         Notebook.PageIndex := 1
       else if Domains.Items.Count <> 1 then
         Notebook.PageIndex := 0;
    3: Notebook.PageIndex := 2;
    4: Notebook.PageIndex := 3;
  end;
end;

procedure TExportWizardDialog.btnNextClick(Sender: TObject);
begin
  case Notebook.PageIndex of
    0: if Languages.Items.Count <> 1 then
         Notebook.PageIndex := 1
       else
         Notebook.PageIndex := 2;
    1: Notebook.PageIndex := 2;
    2: Notebook.PageIndex := 3;
    3: Notebook.PageIndex := 4;
  end;
end;

procedure TExportWizardDialog.btnStartClick(Sender: TObject);
begin
  Notebook.PageIndex := Notebook.Pages.Count - 2;
  EnableMenuItem(GetSystemMenu(Handle, False), SC_CLOSE, MF_DISABLED);
  try
    DoExport;
  finally
    EnableMenuItem(GetSystemMenu(Handle, False), SC_CLOSE, MF_ENABLED);
  end;
  Notebook.PageIndex := Notebook.Pages.Count - 1;
end;

procedure TExportWizardDialog.DomainsItemChecked(Sender: TObject; Item: TListItem);
begin
  btnNext.Enabled := Item.Checked or AnyCheckedItem(Domains);
end;

procedure TExportWizardDialog.LanguagesItemChecked(Sender: TObject; Item: TListItem);
begin
  btnNext.Enabled := Item.Checked or AnyCheckedItem(Languages);
end;

procedure TExportWizardDialog.edFolderChange(Sender: TObject);
begin
  btnNext.Enabled := IsDirectoryWritable(edFolder.Text);
end;

procedure TExportWizardDialog.edFolderExit(Sender: TObject);
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

procedure TExportWizardDialog.FoldersChange(Sender: TObject; Node: TTreeNode);
begin
  if DirectoryExists(Folders.Path) then
  begin
    edFolder.Text := Folders.Path;
    edFolder.Modified := False;
  end
  else
    edFolder.Clear;
end;

procedure TExportWizardDialog.FoldersEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TExportWizardDialog.ExportByGroupClick(Sender: TObject);
begin
  if cbGroupByDomain.Checked or cbGroupByLanguage.Checked then
  begin
    btnNext.Enabled := True;
    lblBaseName.Caption := Translator.GetText(SBaseFileName);
  end
  else
  begin
    btnNext.Enabled := (edBaseName.GetTextLen <> 0);
    lblBaseName.Caption := Translator.GetText(SFileName);
  end;
end;

procedure TExportWizardDialog.edBaseNamChange(Sender: TObject);
begin
  btnNext.Enabled := cbGroupByDomain.Checked or cbGroupByLanguage.Checked or (Trim(edBaseName.Text) <> '');
end;

procedure TExportWizardDialog.edBaseNameExit(Sender: TObject);
begin
  edBaseName.Text := EnsureValidFileName(Trim(edBaseName.Text));
end;

procedure TExportWizardDialog.OutputFilesAdvancedCustomDraw(
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

procedure TExportWizardDialog.OutputFilesDblClick(Sender: TObject);
begin
  if Assigned(OutputFiles.Selected) then
    OpenFileLocation(IncludeTrailingPathDelimiter(edFolder.Text) + OutputFiles.Selected.Caption);
end;

procedure TExportWizardDialog.OutputFilesInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: string);
begin
  InfoTip := IncludeTrailingPathDelimiter(edFolder.Text) + Item.Caption;
end;

procedure TExportWizardDialog.UpdateSummary;
var
  I: Integer;
begin
  with TStringBuilder.Create do
    try
      // Text Domains
      Append('[ls=1.5][b]').Append(Notebook.Pages[0]).Append(':').AppendLine('[/b][/ls]');
      Append('[list]');
      for I := 0 to Domains.Items.Count - 1 do
        if Domains.Items[I].Checked then
        begin
          if Summary.IsRightToLeft then
            Append(UCC_LRE);
          Append(Domains.Items[I].Caption);
          if Summary.IsRightToLeft then
            Append(UCC_PDF);
          AppendLine;
        end;
      AppendLine('[/list]').AppendLine;
      // Languages
      Append('[ls=1.5][b]').Append(Notebook.Pages[1]).Append(':').AppendLine('[/b][/ls]');
      Append('[list]');
      for I := 0 to Languages.Items.Count - 1 do
        if Languages.Items[I].Checked then
        begin
          if Summary.IsRightToLeft then
            Append(UCC_LRE);
          Append(Languages.Items[I].Caption);
          if Summary.IsRightToLeft then
            Append(UCC_PDF);
          AppendLine;
        end;
      AppendLine('[/list]').AppendLine;
      // Options
      if cbIgnoreDeveloperComments.Checked or cbIgnoreTranslatorNotes.Checked or
         cbGroupByLanguage.Checked or cbGroupByDomain.Checked or
         cbCompressed.Checked or cbUsablesOnly.Checked then
      begin
        Append('[ls=1.5][b]').Append(Notebook.Pages[2]).Append(':').AppendLine('[/b][/ls]');
        Append('[list]');
        if cbIgnoreDeveloperComments.Checked then
          AppendLine(cbIgnoreDeveloperComments.Caption);
        if cbIgnoreTranslatorNotes.Checked then
          AppendLine(cbIgnoreTranslatorNotes.Caption);
        if cbUsablesOnly.Checked then
          AppendLine(cbUsablesOnly.Caption);
        if cbGroupByLanguage.Checked then
          AppendLine(cbGroupByLanguage.Caption);
        if cbGroupByDomain.Checked then
          AppendLine(cbGroupByDomain.Caption);
        if cbCompressed.Checked then
          AppendLine(cbCompressed.Caption);
        AppendLine('[/list]').AppendLine;
      end;
      // Destination
      Append('[ls=1.5][b]').Append(Notebook.Pages[3]).Append(':').AppendLine('[/b][/ls]');
      Append('[list]');
      if Summary.IsRightToLeft then
        Append(UCC_LRE);
      Append(IncludeTrailingPathDelimiter(edFolder.Text));
      if edBaseName.GetTextLen <> 0 then
        Append(edBaseName.Text).Append('.');
      if cbGroupByDomain.Checked then
        Append('[color=gray]').Append(Translator.GetText(SDomainTemplate)).Append('[/color]').Append('.');
      if cbGroupByLanguage.Checked then
        Append('[color=gray]').Append(Translator.GetText(SLocaleTemplate)).Append('[/color]').Append('.');
      Append(Copy(i18nCatalogFileExt, 2, MaxInt));
      if Summary.IsRightToLeft then
        Append(UCC_PDF);
      AppendLine;
      AppendLine('[/list]');
      BBCodes.Apply(Summary, ToString);
    finally
      Free;
    end;
end;

procedure TExportWizardDialog.DoExport;

  function Save(ACatalog: TTranslationCatalog; const BaseFileName: String;
    LastUserResponse: Integer = mrNone): Integer;
  var
    Saved: Boolean;
    FileName: String;
  begin
    Result := LastUserResponse;
    if Win32MajorVersion > 5 then
    begin // workaround update delay on aero
      ProgressBar.StepBy(2);
      ProgressBar.StepBy(-1);
    end
    else
      ProgressBar.StepBy(1);
    FileName := BaseFileName + i18nCatalogFileExt;
    if FileExists(FileName) then
    begin
      if not (LastUserResponse in [mrYesToAll, mrNoToAll]) then
      begin
        ProgressBar.State := pbsPaused;
        try
          Result := DM.MsgDlg.YesNoAllCancel(
            DM.Localizer.FormatCS(Translator.GetText(SOverwriteConfirm), [FileName]));
        finally
          ProgressBar.State := pbsNormal;
        end;
        Update;
      end;
      if Result in [mrNo, mrNoToAll, mrCancel] then
        Exit;
    end;
    ACatalog.Compressed := cbCompressed.Checked;
    Saved := False;
    repeat
      try
        ACatalog.SaveToFile(FileName);
        with OutputFiles.Items.Add do
        begin
          Caption := ExtractFileName(FileName);
          ImageIndex := 0;
        end;
        Saved := True;
      except
        ProgressBar.State := pbsError;
        try
          case DM.MsgDlg.RetryIgnoreAbort(DM.Localizer.FormatCS(Translator.GetText(SSaveError), [FileName])) of
            mrAbort:
            begin
              Saved := True;
              Result := mrCancel;
            end;
            mrIgnore:
              Saved := True;
          end;
        finally
          ProgressBar.State := pbsNormal;
        end;
        Update;
      end;
    until Saved;
  end;

  function SavePerLocale(ACatalog: TTranslationCatalog; const BaseFileName: String;
    LastUserResponse: Integer = mrNone): Integer;

    procedure DropTranslations(Dictionary: TTextDictionary);
    var
      Definition: TTextDefinition;
    begin
      Definition := Dictionary.First;
      while Assigned(Definition) do
      begin
        Definition.Translations.Clear;
        Definition := Definition.Next;
      end;
    end;

  var
    C: TTranslationCatalog;
    Domain: TTextDomain;
    I: Integer;
  begin
    Result := LastUserResponse;
    C := TTranslationCatalog.Create;
    try
      for I := 0 to ACatalog.Cultures.Count - 1 do
      begin
        C.Assign(ACatalog);
        C.NativeCulture := ACatalog.Cultures[I];
        Domain := C.TextDomains.First;
        while Assigned(Domain) do
        begin
          DropTranslations(Domain.Properties);
          DropTranslations(Domain.Literals);
          Domain := Domain.Next;
        end;
        C.ChangePluralRuleOf(C.NativeCulture, ACatalog.PluralRuleOf(C.NativeCulture));
        Result := Save(C, BaseFileName + C.NativeCulture.Locale, Result);
        if Result = mrCancel then
          Exit;
      end;
    finally
      C.Free;
    end;
  end;

  function SavePerDomain(ACatalog: TTranslationCatalog; const BaseFileName: String;
    AlsoPerLocale: Boolean; LastUserResponse: Integer = mrNone): Integer;
  var
    C: TTranslationCatalog;
    Domain: TTextDomain;
  begin
    Result := LastUserResponse;
    C := TTranslationCatalog.Create;
    try
      Domain := ACatalog.TextDomains.First;
      while Assigned(Domain) do
      begin
        C.BeginUpdate;
        C.Clear;
        C.NativeCulture := ACatalog.NativeCulture;
        C.TextDomains.Add(Domain.Name).Assign(Domain);
        C.EndUpdate;
        if AlsoPerLocale then
          Result := SavePerLocale(C, BaseFileName + Domain.Name + '.', Result)
        else
          Result := Save(C, BaseFileName + Domain.Name, Result);
        if Result = mrCancel then
          Exit;
        Domain := Domain.Next;
      end;
    finally
      C.Free;
    end;
  end;

  function CompactOptions: TCompactOptions;
  begin
    Result := [];
    if cbIgnoreDeveloperComments.Checked then
      Include(Result, coDropComments);
    if cbIgnoreTranslatorNotes.Checked then
      Include(Result, coDropNotes);
    if cbUsablesOnly.Checked then
      Include(Result, coUsablesOnly);
  end;

var
  BaseFileName: String;
  TargetCatalog: TTranslationCatalog;
  Domain: TTextDomain;
  Item: TListItem;
  I: Integer;
begin
  BaseFileName := IncludeTrailingPathDelimiter(edFolder.Text) + edBaseName.Text;
  OutputFiles.Items.BeginUpdate;
  TargetCatalog := TTranslationCatalog.Create;
  try
    TargetCatalog.NativeCulture := Catalog.NativeCulture;
    Domain := Catalog.TextDomains.First;
    while Assigned(Domain) do
    begin
      Item := Domains.FindData(0, Domain, True, False);
      if Assigned(Item) and Item.Checked then
        TargetCatalog.TextDomains.Add(Domain.Name).Assign(Domain);
      Domain := Domain.Next;
    end;
    TargetCatalog.Compact(CompactOptions);
    for I := TargetCatalog.Cultures.Count - 1 downto 0 do
    begin
      Item := Languages.FindData(0, TargetCatalog.Cultures[I], True, False);
      if not Assigned(Item) or not Item.Checked then
        TargetCatalog.Remove(TargetCatalog.Cultures[I]);
    end;
    if cbGroupByDomain.Checked then
    begin
      if edBaseName.GetTextLen <> 0 then
        BaseFileName := BaseFileName + '.';
      if cbGroupByLanguage.Checked then
        ProgressBar.Max := TargetCatalog.TextDomains.Count * TargetCatalog.Cultures.Count
      else
        ProgressBar.Max := TargetCatalog.TextDomains.Count;
      SavePerDomain(TargetCatalog, BaseFileName, cbGroupByLanguage.Checked);
    end
    else if cbGroupByLanguage.Checked then
    begin
      ProgressBar.Max := TargetCatalog.Cultures.Count;
      if edBaseName.GetTextLen <> 0 then
        BaseFileName := BaseFileName + '.';
      SavePerLocale(TargetCatalog, BaseFileName);
    end
    else
    begin
      ProgressBar.Max := 1;
      Save(TargetCatalog, BaseFileName);
    end;
  finally
    TargetCatalog.Free;
    OutputFiles.Items.EndUpdate;
  end;
end;

end.
