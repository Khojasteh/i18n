{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz Khojasteh                                                         }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgImportWizard;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCore, i18nLocalizer, i18nHashList, i18nCatalog, i18nCtrls,
  StdCtrls, ExtCtrls, ComCtrls, CheckLst, jpeg, ImgList, Buttons, ShellCtrls;

type
  TImportWizardDialog = class(TForm)
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
    Header5: TPanel;
    Title5: TLabel;
    Subtitle5: TLabel;
    lblFolder: TLabel;
    Folders: TShellTreeView;
    Summary: TRichEdit;
    ButtonsPanel: TPanel;
    btnBack: TButton;
    btnNext: TButton;
    btnStart: TButton;
    btnCancel: TButton;
    LastNotice: TLabel;
    CheckImages: TImageList;
    edFolder: TEdit;
    Header4: TPanel;
    Title4: TLabel;
    Subtitle4: TLabel;
    rbImportRevise: TRadioButton;
    rbImportMerge: TRadioButton;
    ImportReviseSubOptions: TPanel;
    rbReviseKeepObsoletes: TRadioButton;
    rbReviseDeleteObsoletes: TRadioButton;
    lblImportMerge: TLabel;
    lblImportRevise: TLabel;
    lblReviseKeepObsoletes: TLabel;
    lblReviseDeleteObsoletes: TLabel;
    PleaseWait: TLabel;
    Header6: TPanel;
    Title6: TLabel;
    Subtitle6: TLabel;
    ProgressBar: TProgressBar;
    Header7: TPanel;
    Title7: TLabel;
    Subtitle7: TLabel;
    SuccessMsg: TLabel;
    btnFinish: TButton;
    procedure NotebookPageChanged(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure LanguagesItemChecked(Sender: TObject; Item: TListItem);
    procedure FoldersEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure FoldersChange(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DomainsClick(Sender: TObject);
    procedure DomainsKeyPress(Sender: TObject; var Key: Char);
    procedure ListViewAdvancedCustomDraw(Sender: TCustomListView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure DomainsAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure edFolderChange(Sender: TObject);
    procedure edFolderExit(Sender: TObject);
    procedure ImportTypeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TranslatorLoaded(Sender: TObject);
  private
    SourceFolder: String;
    Catalog: TTranslationCatalog;
    Files: TStringList;
    procedure DoImport;
    procedure UpdateDomains;
    procedure UpdateLanguages;
    procedure UpdateSummary;
    procedure LoadPersistentUserChoices;
    procedure SavePersistentUserChoices;
    procedure ToggleDomainState(Item: TListItem);
  public
    class function Execute(ACatalog: TTranslationCatalog): Boolean;
  end;

implementation

{$R *.dfm}

uses
  Registry, CommCtrl, DataModule, i18nBBCode, i18nUnicode;

const
  SNoItemToShow = 'There are no items to show in this view.';

{ Helper Functions }

procedure LoadCatalogs(const Dir: String; Files: TStrings);
var
  F: TSearchRec;
  Catalog: TTranslationCatalog;
begin
  Screen.Cursor := crHourGlass;
  Files.BeginUpdate;
  try
    Files.Clear;
    if FindFirst(Dir + '*' + i18nCatalogFileExt, faAnyFile and not faDirectory, F) = 0 then
    begin
      repeat
        Catalog := TTranslationCatalog.Create;
        try
          Catalog.LoadFromFile(Dir + F.Name);
          if not Catalog.Empty then
          begin
            Catalog.Add(Catalog.NativeCulture);
            Files.AddObject(F.Name, Catalog);
          end
          else
            Catalog.Free;
        except
          Catalog.Free;
        end;
      until FindNext(F) <> 0;
      FindClose(F);
    end;
  finally
    Files.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure AddCheckBoxImages(ImageList: TImageList);
var
  Bitmap: TBitmap;
  State: TCheckBoxState;
  Rect: TRect;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := ImageList.Width;
    Bitmap.Height := ImageList.Height;
    Bitmap.Canvas.Brush.Color := clFuchsia;
    SetRect(Rect, 0, 0, Bitmap.Width, Bitmap.Height);
    for State := Low(TCheckBoxState) to High(TCheckBoxState) do
    begin
      Bitmap.Canvas.FillRect(Rect);
      DrawCheckBox(Bitmap.Canvas.Handle, Rect, State, pdsNormal, False, True);
      ImageList.AddMasked(Bitmap, clFuchsia);
    end;
  finally
    Bitmap.Free;
  end;
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

{ TImportWizardDialog }

class function TImportWizardDialog.Execute(ACatalog: TTranslationCatalog): Boolean;
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

procedure TImportWizardDialog.FormCreate(Sender: TObject);
begin
  AddCheckBoxImages(CheckImages);
  Files := TStringList.Create;
  Files.OwnsObjects := True;
  Files.Sorted := True;
end;

procedure TImportWizardDialog.FormShow(Sender: TObject);
begin
  Notebook.PageIndex := 0;
  NotebookPageChanged(nil);
end;

procedure TImportWizardDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrCancel) and (Notebook.PageIndex = Notebook.Pages.Count - 1) then
    ModalResult := mrOk;
end;

procedure TImportWizardDialog.FormDestroy(Sender: TObject);
begin
  Files.Free;
end;

type TControlHack = class(TControl);

procedure TImportWizardDialog.TranslatorLoaded(Sender: TObject);
begin
  TControlHack(Folders).ParentBiDiMode := False;
  TControlHack(Folders).BiDiMode := bdLeftToRight;
end;

procedure TImportWizardDialog.NotebookPageChanged(Sender: TObject);
begin
  case Notebook.PageIndex of
    0: btnNext.Enabled := DirectoryExists(edFolder.Text);
    1: btnNext.Enabled := AnyCheckedItem(Domains);
    2: btnNext.Enabled := AnyCheckedItem(Languages);
    3: btnNext.Enabled := True;
    4: UpdateSummary;
  end;
  btnBack.Visible := (Notebook.PageIndex > 0) and (Notebook.PageIndex < Notebook.Pages.Count - 2);
  btnNext.Visible := (Notebook.PageIndex < Notebook.Pages.Count - 3);
  btnStart.Visible := (Notebook.PageIndex = Notebook.Pages.Count - 3);
  btnCancel.Visible := (Notebook.PageIndex < Notebook.Pages.Count - 1);
  btnCancel.Enabled := (Notebook.PageIndex < Notebook.Pages.Count - 2);
  btnFinish.Visible := (Notebook.PageIndex = Notebook.Pages.Count - 1);
  Notebook.Update;
end;

procedure TImportWizardDialog.btnBackClick(Sender: TObject);
begin
  case Notebook.PageIndex of
    1: Notebook.PageIndex := 0;
    2: Notebook.PageIndex := 1;
    3: Notebook.PageIndex := 2;
    4: Notebook.PageIndex := 3;
  end;
end;

procedure TImportWizardDialog.btnNextClick(Sender: TObject);
begin
  case Notebook.PageIndex of
    0:
    begin
      UpdateDomains;
      Notebook.PageIndex := 1;
    end;
    1:
    begin
      UpdateLanguages;
      Notebook.PageIndex := 2;
    end;
    2: Notebook.PageIndex := 3;
    3: Notebook.PageIndex := 4;
  end;
end;

procedure TImportWizardDialog.btnStartClick(Sender: TObject);
begin
  Notebook.PageIndex := Notebook.Pages.Count - 2;
  EnableMenuItem(GetSystemMenu(Handle, False), SC_CLOSE, MF_DISABLED);
  try
    DoImport;
  finally
    EnableMenuItem(GetSystemMenu(Handle, False), SC_CLOSE, MF_ENABLED);
  end;
  Notebook.PageIndex := Notebook.Pages.Count - 1;
end;

procedure TImportWizardDialog.ListViewAdvancedCustomDraw(Sender: TCustomListView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
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

procedure TImportWizardDialog.edFolderChange(Sender: TObject);
begin
  btnNext.Enabled := DirectoryExists(edFolder.Text);
end;

procedure TImportWizardDialog.edFolderExit(Sender: TObject);
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

procedure TImportWizardDialog.FoldersChange(Sender: TObject; Node: TTreeNode);
begin
  if DirectoryExists(Folders.Path) then
  begin
    edFolder.Text := Folders.Path;
    edFolder.Modified := False;
  end
  else
    edFolder.Clear;
end;

procedure TImportWizardDialog.FoldersEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TImportWizardDialog.DomainsAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  if Item.Indent = 0 then
    Domains.Canvas.Font.Color := clHotLight;
end;

procedure TImportWizardDialog.DomainsClick(Sender: TObject);
var
  Pt: TPoint;
begin
  Pt := Domains.ScreenToClient(Mouse.CursorPos);
  if htOnStateIcon in Domains.GetHitTestInfoAt(Pt.X, Pt.Y) then
  begin
    ToggleDomainState(Domains.GetItemAt(Pt.X, Pt.Y));
    btnNext.Enabled := AnyCheckedItem(Domains);
  end;
end;

procedure TImportWizardDialog.DomainsKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ' ') and Assigned(Domains.Selected) then
  begin
    ToggleDomainState(Domains.Selected);
    btnNext.Enabled := AnyCheckedItem(Domains);
  end;
end;

procedure TImportWizardDialog.LanguagesItemChecked(Sender: TObject; Item: TListItem);
begin
  btnNext.Enabled := Item.Checked or AnyCheckedItem(Languages);
end;

procedure TImportWizardDialog.ImportTypeClick(Sender: TObject);
begin
  rbReviseKeepObsoletes.Enabled := rbImportRevise.Checked;
  rbReviseDeleteObsoletes.Enabled := rbImportRevise.Checked;
end;

procedure TImportWizardDialog.LoadPersistentUserChoices;
var
  R: TRegistry;
  RadioState: Integer;
begin
  R := TRegistry.Create(KEY_QUERY_VALUE);
  try
    if R.OpenKeyReadOnly(AppRegRootKey) then
    begin
      if R.ValueExists('Import.Path') and DirectoryExists(R.ReadString('Import.Path')) then
        Folders.Path := R.ReadString('Import.Path');
      if R.ValueExists('Import.Type') then
      begin
        RadioState := R.ReadInteger('Import.Type');
        rbImportMerge.Checked := (RadioState = 0);
        rbImportRevise.Checked := (RadioState <> 0);
      end;
      if R.ValueExists('Import.ReviseType') then
      begin
        RadioState := R.ReadInteger('Import.ReviseType');
        rbReviseDeleteObsoletes.Checked := (RadioState = 0);
        rbReviseKeepObsoletes.Checked := (RadioState <> 0);
      end;
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

procedure TImportWizardDialog.SavePersistentUserChoices;
var
  R: TRegistry;
  RadioState: Integer;
begin
  R := TRegistry.Create;
  try
    if R.OpenKey(AppRegRootKey, True) then
    begin
      R.WriteString('Import.Path', edFolder.Text);
      if rbImportRevise.Checked then
        RadioState := 1
      else
        RadioState := 0;
      R.WriteInteger('Import.Type', RadioState);
      if rbReviseDeleteObsoletes.Checked then
        RadioState := 0
      else
        RadioState := 1;
      R.WriteInteger('Import.ReviseType', RadioState);
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

procedure TImportWizardDialog.ToggleDomainState(Item: TListItem);
var
  ParentItem: TListItem;
  I: Integer;
begin
  if Assigned(Item) then
  begin
    if Item.StateIndex = Ord(cbChecked) then
      Item.StateIndex := Ord(cbUnchecked)
    else
      Item.StateIndex := Ord(cbChecked);
    if Item.Indent = 0 then
    begin
      for I := Item.Index + 1 to Domains.Items.Count - 1 do
      begin
        if Domains.Items[I].Indent = 0 then
          Break;
        Domains.Items[I].StateIndex := Item.StateIndex;
        Domains.Items[I].Checked := (Item.StateIndex = Ord(cbChecked));
      end;
    end
    else
    begin
      ParentItem := nil;
      for I := Item.Index - 1 downto 0 do
      begin
        ParentItem := Domains.Items[I];
        if Domains.Items[I].Indent = 0 then
          Break;
      end;
      for I := ParentItem.Index + 1 to Domains.Items.Count - 1 do
      begin
        if Domains.Items[I].Indent = 0 then
          Break;
        if Domains.Items[I].StateIndex <> Item.StateIndex then
        begin
          ParentItem.StateIndex := Ord(cbGrayed);
          Exit;
        end;
      end;
      ParentItem.StateIndex := Item.StateIndex;
    end;
  end;
end;

procedure TImportWizardDialog.UpdateDomains;
var
  C: TTranslationCatalog;
  Domain: TTextDomain;
  ParentItem: TListItem;
  TotalProperties, TotalLiterals: Integer;
  I: Integer;
begin
  if edFolder.Text = SourceFolder then Exit;
  SourceFolder := edFolder.Text;
  LoadCatalogs(IncludeTrailingPathDelimiter(SourceFolder), Files);
  Domains.Items.BeginUpdate;
  try
    Domains.Items.Clear;
    for I := 0 to Files.Count - 1 do
    begin
      C := TTranslationCatalog(Files.Objects[I]);
      ParentItem := Domains.Items.Add;
      with ParentItem do
      begin
        Data := C;
        Caption := Files[I];
        ImageIndex := 0;
        Indent := 0;
        StateIndex := Ord(cbChecked);
      end;
      TotalProperties := 0;
      TotalLiterals := 0;
      Domain := C.TextDomains.First;
      while Assigned(Domain) do
      begin
        with Domains.Items.Add do
        begin
          Data := Domain;
          Caption := Domain.Name;
          SubItems.Add(DM.Localizer.FormatNumber('#,##0;; ', Domain.Properties.Count));
          SubItems.Add(DM.Localizer.FormatNumber('#,##0;; ', Domain.Literals.Count));
          ImageIndex := 1;
          Indent := 1;
          StateIndex := Ord(cbChecked);
          Checked := True;
        end;
        Inc(TotalProperties, Domain.Properties.Count);
        Inc(TotalLiterals, Domain.Literals.Count);
        Domain := Domain.Next;
      end;
      with ParentItem do
      begin
        SubItems.Add(DM.Localizer.FormatNumber('#,##0;; ', TotalProperties));
        SubItems.Add(DM.Localizer.FormatNumber('#,##0;; ', TotalLiterals));
      end;
    end;
  finally
    Domains.Items.EndUpdate;
  end;
end;

procedure TImportWizardDialog.UpdateLanguages;
var
  C: TTranslationCatalog;
  Culture: TCultureInfo;
  Domain: TTextDomain;
  Item: TListItem;
  I: Integer;
begin
  C := TTranslationCatalog.Create;
  try
    C.BeginUpdate;
    for I := 0 to Domains.Items.Count - 1 do
      if (Domains.Items[I].Indent <> 0) and Domains.Items[I].Checked then
      begin
        Domain := TTextDomain(Domains.Items[I].Data);
        C.TextDomains.Add(Domain.Name).Merge(Domain);
      end;
    C.EndUpdate;
    Languages.Items.BeginUpdate;
    try
      for I := Languages.Items.Count - 1 downto 0 do
      begin
        Culture := TCultureInfo(Languages.Items[I].Data);
        if not C.Cultures.Exists(Culture) then
          Languages.Items.Delete(I);
      end;
      for I := 0 to C.Cultures.Count - 1 do
      begin
        Culture := C.Cultures[I];
        Item := Languages.FindData(0, Culture, True, False);
        if Item = nil then
          with Languages.Items.Add do
          begin
            Data := Culture;
            Caption := Culture.LocalizedDisplayName;
            SubItems.Add(DM.Localizer.FormatNumber('#,##0.00', C.StatsOf(Culture).Progress));
            ImageIndex := ImageList.Count;
            DM.CopyFlagTo(Culture, ImageList, ImageIndex);
            Checked := True;
          end
        else
          Item.SubItems[0] := DM.Localizer.FormatNumber('#,##0.00', C.StatsOf(Culture).Progress);
      end;
    finally
      Languages.Items.EndUpdate;
    end;
  finally
    C.Free;
  end;
end;

procedure TImportWizardDialog.UpdateSummary;
var
  I: Integer;
  DomainNames: TStringList;
begin
  with TStringBuilder.Create do
    try
      // SourceCatalog
      Append('[ls=1.5][b]').Append(Notebook.Pages[0]).Append(':').AppendLine('[/b][/ls]');
      Append('[list]');
      for I := 0 to Domains.Items.Count - 1 do
        if (Domains.Items[I].Indent = 0) and (Domains.Items[I].StateIndex <> Ord(cbUnchecked)) then
        begin
          if Summary.IsRightToLeft then
            Append(UCC_LRE);
          Append(IncludeTrailingPathDelimiter(SourceFolder) + Domains.Items[I].Caption);
          if Summary.IsRightToLeft then
            Append(UCC_PDF);
          AppendLine;
        end;
      AppendLine('[/list]').AppendLine;
      // Text Domains
      Append('[ls=1.5][b]').Append(Notebook.Pages[1]).Append(':').AppendLine('[/b][/ls]');
      Append('[list]');
      DomainNames := TStringList.Create;
      try
        DomainNames.Sorted := True;
        DomainNames.Duplicates := dupIgnore;
        for I := 0 to Domains.Items.Count - 1 do
          if (Domains.Items[I].Indent <> 0) and Domains.Items[I].Checked then
            DomainNames.Add(Domains.Items[I].Caption);
        for I := 0 to DomainNames.Count - 1 do
        begin
          if Summary.IsRightToLeft then
            Append(UCC_LRE);
          Append(DomainNames[I]);
          if Summary.IsRightToLeft then
            Append(UCC_PDF);
          AppendLine;
        end;
      finally
        DomainNames.Free;
      end;
      AppendLine('[/list]').AppendLine;
      // Languages
      Append('[ls=1.5][b]').Append(Notebook.Pages[2]).Append(':').AppendLine('[/b][/ls]');
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
      Append('[ls=1.5][b]').Append(Notebook.Pages[3]).Append(':').AppendLine('[/b][/ls]');
      Append('[list]');
      if rbImportMerge.Checked then
        AppendLine(rbImportMerge.Caption)
      else
      begin
        AppendLine(rbImportRevise.Caption);
        if rbReviseDeleteObsoletes.Checked then
          AppendLine(rbReviseDeleteObsoletes.Caption)
        else
          AppendLine(rbReviseKeepObsoletes.Caption);
      end;
      AppendLine('[/list]');
      BBCodes.Apply(Summary, ToString);
    finally
      Free;
    end;
end;

procedure TImportWizardDialog.DoImport;

  function MergeOptions: TMergeOptions;
  begin
    Result := [];
    if rbImportRevise.Checked then
    begin
      Include(Result, moPreferSource);
      if rbReviseDeleteObsoletes.Checked then
        Include(Result, moDeleteDefinitionsNotInSource);
    end
  end;

var
  SourceCatalog: TTranslationCatalog;
  Domain: TTextDomain;
  Item: TListItem;
  I: Integer;
begin
  SourceCatalog := TTranslationCatalog.Create;
  try
    SourceCatalog.Repository := DM.Repository;
    SourceCatalog.NativeCulture := Catalog.NativeCulture;
    SourceCatalog.BeginUpdate;
    for I := 0 to Domains.Items.Count - 1 do
      if (Domains.Items[I].Indent <> 0) and Domains.Items[I].Checked then
      begin
        Domain := TTextDomain(Domains.Items[I].Data);
        SourceCatalog.TextDomains.Add(Domain.Name).Merge(Domain);
      end;
    SourceCatalog.EndUpdate;
    for I := SourceCatalog.Cultures.Count - 1 downto 0 do
    begin
      Item := Languages.FindData(0, SourceCatalog.Cultures[I], True, False);
      if not Assigned(Item) or not Item.Checked then
        SourceCatalog.Remove(SourceCatalog.Cultures[I]);
    end;
    Catalog.Merge(SourceCatalog, MergeOptions);
  finally
    SourceCatalog.Free;
  end;
end;

end.
