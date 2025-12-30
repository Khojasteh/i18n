{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  Translation Repository Editor for Delphi Applications                       }
{                                                                              }
{  Copyright (c) Kambiz Khojasteh                                              }
{  https://github.com/khojasteh/i18n                                           }
{                                                                              }
{------------------------------------------------------------------------------}

unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PlatformDefaultStyleActnCtrls, ActnList, StdActns, ActnCtrls,
  ActnMenus, ActnMan, ImgList, ComCtrls, ToolWin, i18nCore, i18nLocalizer,
  frmTranslationEditor, System.Actions;

type
  TMainForm = class(TForm)
    Translator: TTranslator;
    ActionImages: TImageList;
    ActionDisabledImages: TImageList;
    MainMenu: TActionMainMenuBar;
    MainToolbar: TActionToolBar;
    StatusBar: TStatusBar;
    UILanguageActions: TActionList;
    ActionManager: TActionManager;
    FileOpenAction: TFileOpen;
    FileSaveAction: TAction;
    FileSaveAsAction: TAction;
    FileCloseAction: TAction;
    FileImportAction: TAction;
    FileExportAction: TAction;
    FileRepositoryTrainAction: TAction;
    FileExitAction: TFileExit;
    EditUndoAction: TEditUndo;
    EditCutAction: TEditCut;
    EditCopyAction: TEditCopy;
    EditPasteAction: TEditPaste;
    EditSelectAllAction: TEditSelectAll;
    SearchFindAction: TAction;
    SearchReplaceAction: TAction;
    SearchAgainAction: TAction;
    LanguageSetNativeAction: TAction;
    LanguageAddAction: TAction;
    LanguageDeleteAction: TAction;
    LanguageSwitchAction: TAction;
    NavigationPriorItemAction: TAction;
    NavigationNextItemAction: TAction;
    TranslationAcceptAction: TAction;
    TranslationRejectAction: TAction;
    TranslationCopyOriginalAction: TAction;
    TranslationSuggestAction: TAction;
    TranslationGoogleAction: TAction;
    TranslationAutoTranslateAction: TAction;
    TranslationNoteEditAction: TAction;
    TranslationNoteClearAction: TAction;
    TranslationRenameDomainAction: TAction;
    TranslationRenameComponentAction: TAction;
    TranslationDeleteDomainAction: TAction;
    TranslationDeleteComponentAction: TAction;
    TranslationDeleteItemAction: TAction;
    ViewToolbarAction: TAction;
    ViewStatusBarAction: TAction;
    SortByOriginalAction: TAction;
    SortByTranslationAction: TAction;
    SortByStateAction: TAction;
    SortByCommentAction: TAction;
    SortByNoteAction: TAction;
    SortByNameAction: TAction;
    SortAscendingAction: TAction;
    SortDescendingAction: TAction;
    GroupArrangementAction: TAction;
    WindowCascadeAction: TWindowCascade;
    WindowTileHorizontalAction: TWindowTileHorizontal;
    WindowTileVerticalAction: TWindowTileVertical;
    HelpAboutAction: TAction;
    SortByNoneAction: TAction;
    GroupExpandAllAction: TAction;
    GroupCollapseAllAction: TAction;
    ViewPreferencesAction: TAction;
    NavigationPriorGroupAction: TAction;
    NavigationNextGroupAction: TAction;
    NavigationPriorUntranslatedAction: TAction;
    NavigationNextUntranslatedAction: TAction;
    NavigationPriorUnacceptedAction: TAction;
    NavigationNextUnacceptedAction: TAction;
    SortByPluralsAction: TAction;
    LanguageEditAction: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FileOpenActionAccept(Sender: TObject);
    procedure FileOpenActionBeforeExecute(Sender: TObject);
    procedure FileReopenActionExecute(Sender: TObject);
    procedure FileReopenActionUpdate(Sender: TObject);
    procedure FileSaveAsActionExecute(Sender: TObject);
    procedure FileSaveAsActionUpdate(Sender: TObject);
    procedure FileSaveActionExecute(Sender: TObject);
    procedure FileSaveActionUpdate(Sender: TObject);
    procedure FileCloseActionExecute(Sender: TObject);
    procedure FileCloseActionUpdate(Sender: TObject);
    procedure FileImportActionExecute(Sender: TObject);
    procedure FileImportActionUpdate(Sender: TObject);
    procedure FileExportActionExecute(Sender: TObject);
    procedure FileExportActionUpdate(Sender: TObject);
    procedure FileRepositoryTrainActionExecute(Sender: TObject);
    procedure SearchFindActionExecute(Sender: TObject);
    procedure SearchFindActionUpdate(Sender: TObject);
    procedure SearchReplaceActionExecute(Sender: TObject);
    procedure SearchReplaceActionUpdate(Sender: TObject);
    procedure SearchAgainActionExecute(Sender: TObject);
    procedure SearchAgainActionUpdate(Sender: TObject);
    procedure SortByOriginalActionUpdate(Sender: TObject);
    procedure SortByOriginalActionExecute(Sender: TObject);
    procedure SortByTranslationActionExecute(Sender: TObject);
    procedure SortByTranslationActionUpdate(Sender: TObject);
    procedure SortByStateActionExecute(Sender: TObject);
    procedure SortByStateActionUpdate(Sender: TObject);
    procedure SortByCommentActionExecute(Sender: TObject);
    procedure SortByCommentActionUpdate(Sender: TObject);
    procedure SortByNoteActionExecute(Sender: TObject);
    procedure SortByNoteActionUpdate(Sender: TObject);
    procedure SortByNameActionExecute(Sender: TObject);
    procedure SortByNameActionUpdate(Sender: TObject);
    procedure SortByPluralsActionExecute(Sender: TObject);
    procedure SortByPluralsActionUpdate(Sender: TObject);
    procedure SortByNoneActionExecute(Sender: TObject);
    procedure SortByNoneActionUpdate(Sender: TObject);
    procedure SortAscendingActionExecute(Sender: TObject);
    procedure SortAscendingActionUpdate(Sender: TObject);
    procedure SortDescendingActionExecute(Sender: TObject);
    procedure SortDescendingActionUpdate(Sender: TObject);
    procedure GroupArrangementActionExecute(Sender: TObject);
    procedure GroupArrangementActionUpdate(Sender: TObject);
    procedure GroupExpandAllActionExecute(Sender: TObject);
    procedure GroupExpandAllActionUpdate(Sender: TObject);
    procedure GroupCollapseAllActionExecute(Sender: TObject);
    procedure GroupCollapseAllActionUpdate(Sender: TObject);
    procedure ViewToolbarActionExecute(Sender: TObject);
    procedure ViewToolbarActionUpdate(Sender: TObject);
    procedure ViewStatusBarActionExecute(Sender: TObject);
    procedure ViewStatusBarActionUpdate(Sender: TObject);
    procedure ViewPreferencesActionExecute(Sender: TObject);
    procedure NavigationPriorGroupActionExecute(Sender: TObject);
    procedure NavigationPriorGroupActionUpdate(Sender: TObject);
    procedure NavigationNextGroupActionExecute(Sender: TObject);
    procedure NavigationNextGroupActionUpdate(Sender: TObject);
    procedure NavigationPriorItemActionExecute(Sender: TObject);
    procedure NavigationPriorItemActionUpdate(Sender: TObject);
    procedure NavigationNextItemActionExecute(Sender: TObject);
    procedure NavigationNextItemActionUpdate(Sender: TObject);
    procedure NavigationPriorUntranslatedActionExecute(Sender: TObject);
    procedure NavigationPriorUntranslatedActionUpdate(Sender: TObject);
    procedure NavigationNextUntranslatedActionExecute(Sender: TObject);
    procedure NavigationNextUntranslatedActionUpdate(Sender: TObject);
    procedure NavigationPriorUnacceptedActionExecute(Sender: TObject);
    procedure NavigationPriorUnacceptedActionUpdate(Sender: TObject);
    procedure NavigationNextUnacceptedActionExecute(Sender: TObject);
    procedure NavigationNextUnacceptedActionUpdate(Sender: TObject);
    procedure LanguageSetNativeActionExecute(Sender: TObject);
    procedure LanguageSetNativeActionUpdate(Sender: TObject);
    procedure LanguageAddActionExecute(Sender: TObject);
    procedure LanguageAddActionUpdate(Sender: TObject);
    procedure LanguageDeleteActionExecute(Sender: TObject);
    procedure LanguageDeleteActionUpdate(Sender: TObject);
    procedure LanguageEditActionExecute(Sender: TObject);
    procedure LanguageEditActionUpdate(Sender: TObject);
    procedure LanguageSwitchActionExecute(Sender: TObject);
    procedure LanguageSwitchActionUpdate(Sender: TObject);
    procedure TranslationAutoTranslateActionExecute(Sender: TObject);
    procedure TranslationAutoTranslateActionUpdate(Sender: TObject);
    procedure TranslationGoogleActionExecute(Sender: TObject);
    procedure TranslationGoogleActionUpdate(Sender: TObject);
    procedure TranslationSuggestActionExecute(Sender: TObject);
    procedure TranslationSuggestActionUpdate(Sender: TObject);
    procedure TranslationCopyOriginalActionExecute(Sender: TObject);
    procedure TranslationCopyOriginalActionUpdate(Sender: TObject);
    procedure TranslationRejectActionExecute(Sender: TObject);
    procedure TranslationRejectActionUpdate(Sender: TObject);
    procedure TranslationAcceptActionExecute(Sender: TObject);
    procedure TranslationAcceptActionUpdate(Sender: TObject);
    procedure TranslationNoteEditActionExecute(Sender: TObject);
    procedure TranslationNoteEditActionUpdate(Sender: TObject);
    procedure TranslationNoteClearActionExecute(Sender: TObject);
    procedure TranslationNoteClearActionUpdate(Sender: TObject);
    procedure TranslationDeleteItemActionExecute(Sender: TObject);
    procedure TranslationDeleteItemActionUpdate(Sender: TObject);
    procedure TranslationDeleteComponentActionExecute(Sender: TObject);
    procedure TranslationDeleteComponentActionUpdate(Sender: TObject);
    procedure TranslationDeleteDomainActionExecute(Sender: TObject);
    procedure TranslationDeleteDomainActionUpdate(Sender: TObject);
    procedure TranslationRenameComponentActionExecute(Sender: TObject);
    procedure TranslationRenameComponentActionUpdate(Sender: TObject);
    procedure TranslationRenameDomainActionExecute(Sender: TObject);
    procedure TranslationRenameDomainActionUpdate(Sender: TObject);
    procedure HelpAboutActionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure StatusBarHint(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure TranslatorBeforeTranslate(Sender: TObject);
    procedure TranslatorAfterTranslate(Sender: TObject);
    procedure TranslatorBeforeFlipLayout(Sender: TObject);
    procedure TranslatorAfterFlipLayout(Sender: TObject);
  private
    fActiveEditor: TTranslationEditor;
    procedure SetActiveEditor(Value: TTranslationEditor);
    procedure UpdateUILanguageChoices;
    procedure UILanguageActionExecute(Sender: TObject);
    procedure UILanguageActionUpdate(Sender: TObject);
  private
    AlreadyActivated: Boolean;
    function OpenRepository(const FileName: String; Readonly: Boolean = False): Boolean;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure SaveSettings;
    procedure LoadSettings;
  private
    procedure TargetLanguageListChanged;
    procedure TargetLanguageChanged;
    procedure StatusChanged;
    procedure ProgressChanged;
  public
    procedure EditorChanged(Editor: TTranslationEditor; NotifyCode: Integer);
    property ActiveEditor: TTranslationEditor read fActiveEditor write SetActiveEditor;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Registry, ShellAPI, DataModule, i18nUnicode, dlgTrainWizard, dlgOptions,
  dlgAbout;

const
  SProgress = '{0:P} Progress';
  SReadonly = 'Readonly';
  SModified = 'Modified';

const
  STATUS_PANEL_HINT     = 0;
  STATUS_PANEL_PROGRESS = 1;
  STATUS_PANEL_STATUS   = 2;

type
  TActionMainMenuBarHack = class(TActionMainMenuBar);

{ Helper Function }

function IsFileWritable(const FileName: String): Boolean;
var
  Handle: THandle;
begin
  Handle := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then
    Result := (GetLastError = ERROR_FILE_NOT_FOUND)
  else
  begin
    FileClose(Handle);
    Result := True;
  end;
end;

procedure UpdateActionClient(Client: TActionClient; Actions: TActionList);
var
  I, Count: Integer;
  Item: TActionClientItem;
begin
  Client.Items.AutoHotKeys := False;
  if Assigned(Actions) then
    Count := Actions.ActionCount
  else
    Count := 0;
  for I := 0 to Count - 1 do
  begin
    Item := nil;
    if I < Client.Items.Count then
    begin
      Item := TActionClientItem(Client.Items[I]);
      if Item.Separator then
        Item := nil;
    end;
    if not Assigned(Item) then
      Item := TActionClientItem(Client.Items.Insert(I));
    Item.Action := Actions[I];
  end;
  while Client.Items.Count > Count do
  begin
    Item := TActionClientItem(Client.Items[Count]);
    if Item.Separator then
      Break;
    Client.Items.Delete(Count);
  end;
  Client.Items.AutoHotKeys := True;
end;

{ TMainForm }

procedure TMainForm.SaveSettings;
var
  R: TRegistry;
  Rect: TRect;
begin
  R := TRegistry.Create;
  try
    if R.OpenKey(AppRegRootKey, True) then
    begin
      R.WriteInteger('Window.State', Ord(WindowState));
      if WindowState = wsNormal then
      begin
        Rect := BoundsRect;
        R.WriteBinaryData('Window.Bounds', Rect, SizeOf(Rect));
      end;
      R.WriteBool('Toolbar', MainToolbar.Visible);
      R.WriteBool('StatusBar', StatusBar.Visible);
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

procedure TMainForm.LoadSettings;
var
  R: TRegistry;
  Rect: TRect;
begin
  R := TRegistry.Create(KEY_QUERY_VALUE);
  try
    if R.OpenKeyReadOnly(AppRegRootKey) then
    begin
      if R.ValueExists('Window.Bounds') then
      begin
        R.ReadBinaryData('Window.Bounds', Rect, SizeOf(Rect));
        BoundsRect := Rect;
      end;
      if R.ValueExists('Window.State') and
        (R.ReadInteger('Window.State') = Ord(wsMaximized))
      then
        WindowState := wsMaximized;
      if R.ValueExists('Toolbar') then
        MainToolbar.Visible := R.ReadBool('Toolbar');
      if R.ValueExists('StatusBar') then
        StatusBar.Visible := R.ReadBool('StatusBar');
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

function TMainForm.OpenRepository(const FileName: String; Readonly: Boolean): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to MDIChildCount - 1 do
    if SameText(TTranslationEditor(MDIChildren[I]).FileName, FileName) then
    begin
      MDIChildren[I].BringToFront;
      Exit;
    end;
  Readonly := Readonly or not IsFileWritable(FileName);
  Screen.Cursor := crHourGlass;
  try
    try
      TTranslationEditor.Create(FileName, Readonly);
    except
      Result := False;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.SetActiveEditor(Value: TTranslationEditor);
begin
  if ActiveEditor <> Value then
  begin
    fActiveEditor := Value;
    TargetLanguageListChanged;
    TargetLanguageChanged;
    ProgressChanged;
    StatusChanged;
  end;
end;

procedure TMainForm.EditorChanged(Editor: TTranslationEditor;
  NotifyCode: Integer);
begin
  if Editor = ActiveEditor then
    case NotifyCode of
      CHANGED_TARGET_LANGUAGES:
        TargetLanguageListChanged;
      CHANGED_TARGET_LANGUAGE:
        TargetLanguageChanged;
      CHANGED_PROGRESS:
        ProgressChanged;
      CHANGED_READONLY, CHANGED_MODIFIED:
        StatusChanged;
    end;
end;

procedure TMainForm.TargetLanguageListChanged;
var
  Menu: TActionClient;
  Actions: TActionList;
begin
  if Assigned(ActiveEditor) then
    Actions := ActiveEditor.LanguageActions
  else
    Actions := nil;
  Menu := ActionManager.FindItemByAction(LanguageAddAction).ParentItem;
  UpdateActionClient(Menu, Actions);
  Menu := ActionManager.FindItemByAction(LanguageSwitchAction);
  UpdateActionClient(Menu, Actions);
end;

procedure TMainForm.TargetLanguageChanged;
const
  DefaultImageIndex = 31;
begin
  if Assigned(ActiveEditor) and Assigned(ActiveEditor.TargetCulture) then
  begin
    LanguageSwitchAction.Caption := ActiveEditor.TargetCulture.LocalizedDisplayName;
    if LanguageSwitchAction.ImageIndex = DefaultImageIndex then
      LanguageSwitchAction.ImageIndex := ActionDisabledImages.Count;
    DM.CopyFlagTo(ActiveEditor.TargetCulture, ActionImages,
      LanguageSwitchAction.ImageIndex);
  end
  else
  begin
    LanguageSwitchAction.Caption := '';
    LanguageSwitchAction.ImageIndex := DefaultImageIndex;
  end;
end;

procedure TMainForm.StatusChanged;
begin
  if Assigned(ActiveEditor) and ActiveEditor.Modified then
    StatusBar.Panels[STATUS_PANEL_STATUS].Text := Translator.GetText(SModified)
  else if Assigned(ActiveEditor) and ActiveEditor.Readonly then
    StatusBar.Panels[STATUS_PANEL_STATUS].Text := Translator.GetText(SReadonly)
  else
    StatusBar.Panels[STATUS_PANEL_STATUS].Text := '';
end;

procedure TMainForm.ProgressChanged;
begin
  if Assigned(ActiveEditor) and Assigned(ActiveEditor.TargetCulture) then
    StatusBar.Panels[STATUS_PANEL_PROGRESS].Text :=
      DM.Localizer.FormatCS(Translator.GetText(SProgress), [ActiveEditor.Progress / 100.0])
  else
    StatusBar.Panels[STATUS_PANEL_PROGRESS].Text := '';
end;

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  I, Count, Size: Integer;
  FileName: String;
begin
  Count := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
  for I := 0 to Count - 1 do
  begin
    Size := DragQueryFile(Msg.Drop, I, nil, 0);
    SetString(FileName, nil, Size);
    DragQueryFile(Msg.Drop, I, PChar(FileName), Size + 1);
    OpenRepository(FileName);
  end;
  DragFinish(Msg.Drop);
end;

procedure TMainForm.UpdateUILanguageChoices;
var
  I: Integer;
  Action: TAction;
  Menu: TActionClient;
begin
  for I := 0 to DM.Localizer.Cultures.Count - 1 do
  begin
    if I < UILanguageActions.ActionCount then
      Action := TAction(UILanguageActions.Actions[I])
    else
    begin
      Action := TAction.Create(Self);
      Action.ActionList := UILanguageActions;
    end;
    Action.Caption := DM.Localizer.Cultures[I].LocalizedDisplayName;
    Action.Tag := DM.Localizer.Cultures[I].LocaleID;
    Action.Checked := (DM.Localizer.Cultures[I] = DM.Localizer.Culture);
    Action.GroupIndex := 10;
    Action.OnExecute := UILanguageActionExecute;
    Action.OnUpdate := UILanguageActionUpdate;
  end;
  for I := UILanguageActions.ActionCount - 1 downto DM.Localizer.Cultures.Count do
    UILanguageActions.Actions[I].Free;
  with TActionMainMenuBarHack(MainMenu) do
    Menu := Items[5].Items[0];
  UpdateActionClient(Menu, UILanguageActions);
  Menu.Visible := (Menu.Items.Count <> 0);
end;

procedure TMainForm.UILanguageActionExecute(Sender: TObject);
begin
  DM.Localizer.Culture := CultureOf(TAction(Sender).Tag);
end;

procedure TMainForm.UILanguageActionUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := (DM.Localizer.Culture <> nil) and
    (DM.Localizer.Culture.LocaleID = LCID(TAction(Sender).Tag));
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Caption := UCC_LRE + Application.Title;
  with TActionMainMenuBarHack(MainMenu) do
    FWindowMenuItem := Items[6];
  for I := 0 to DM.ReopenActions.ActionCount - 1 do
  begin
    DM.ReopenActions[I].OnExecute := FileReopenActionExecute;
    DM.ReopenActions[I].OnUpdate := FileReopenActionUpdate;
  end;
  UpdateUILanguageChoices;
  DragAcceptFiles(Handle, True);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LoadSettings;
end;

procedure TMainForm.FormHide(Sender: TObject);
begin
  SaveSettings;
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  I: Integer;
begin
  if not AlreadyActivated then
  begin
    AlreadyActivated := True;
    StatusBar.ParentBiDiMode := True;
    Update;
    for I := 1 to ParamCount do
      OpenRepository(ParamStr(I));
  end;
end;

procedure TMainForm.TranslatorBeforeTranslate(Sender: TObject);
begin
  ActionManager.ActionBars.BeginUpdate;
  ActionManager.ActionBars.AutoHotKeys := False;
end;

procedure TMainForm.TranslatorAfterTranslate(Sender: TObject);
begin
  ActionManager.ActionBars.AutoHotKeys :=
    not Assigned(Translator.CurrentCulture)
    or (usLatin in Translator.CurrentCulture.Scripts);
  ActionManager.ActionBars.EndUpdate;
  StatusChanged;
  ProgressChanged;
end;

procedure TMainForm.TranslatorBeforeFlipLayout(Sender: TObject);
var
  I: Integer;
begin
  for I := MDIChildCount - 1 downto 0 do
    with MDIChildren[I] do
    begin
      Tag := Ord(WindowState);
      WindowState := wsMinimized;
    end;
end;

procedure TMainForm.TranslatorAfterFlipLayout(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to MDIChildCount - 1 do
    with MDIChildren[I] do
      WindowState := TWindowState(Tag);
end;

procedure TMainForm.StatusBarHint(Sender: TObject);
begin
  StatusBar.Panels[STATUS_PANEL_HINT].Text := GetLongHint(Application.Hint);
end;

procedure TMainForm.StatusBarResize(Sender: TObject);
var
  Panel, W: Integer;
begin
  W := StatusBar.ClientWidth;
  for Panel := 0 to StatusBar.Panels.Count - 1 do
    if Panel <> STATUS_PANEL_HINT then
      Dec(W, StatusBar.Panels[Panel].Width);
  StatusBar.Panels[STATUS_PANEL_HINT].Width := W;
end;

procedure TMainForm.FileOpenActionAccept(Sender: TObject);
var
  I: Integer;
begin
  with FileOpenAction.Dialog do
  begin
    for I := 0 to Files.Count - 1 do
      OpenRepository(Files[I], ofReadOnly in Options);
  end;
end;

procedure TMainForm.FileOpenActionBeforeExecute(Sender: TObject);
begin
  with FileOpenAction.Dialog do
  begin
    Options := Options - [ofReadOnly];
    FileName := '';
  end;
end;

procedure TMainForm.FileReopenActionExecute(Sender: TObject);
begin
  OpenRepository(DM.RecentFiles.Items[TAction(Sender).Tag]);
end;

procedure TMainForm.FileReopenActionUpdate(Sender: TObject);
begin
  if TAction(Sender).Visible then
    TAction(Sender).Enabled := FileExists(DM.RecentFiles.Items[TAction(Sender).Tag]);
end;

procedure TMainForm.FileSaveActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaSave);
end;

procedure TMainForm.FileSaveActionUpdate(Sender: TObject);
begin
  FileSaveAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaSave);
end;

procedure TMainForm.FileSaveAsActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaSaveAs);
end;

procedure TMainForm.FileSaveAsActionUpdate(Sender: TObject);
begin
  FileSaveAsAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaSaveAs);
end;

procedure TMainForm.FileCloseActionExecute(Sender: TObject);
begin
  ActiveEditor.Close;
end;

procedure TMainForm.FileCloseActionUpdate(Sender: TObject);
begin
  FileCloseAction.Enabled := Assigned(ActiveEditor);
end;

procedure TMainForm.FileExportActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaExport);
end;

procedure TMainForm.FileExportActionUpdate(Sender: TObject);
begin
  FileExportAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaExport);
end;

procedure TMainForm.FileImportActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaImport);
end;

procedure TMainForm.FileImportActionUpdate(Sender: TObject);
begin
  FileImportAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaImport);
end;

procedure TMainForm.FileRepositoryTrainActionExecute(Sender: TObject);
begin
  TRepositoryTrainWizardDialog.Execute(DM.Repository);
end;

procedure TMainForm.TranslationDeleteDomainActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaDeleteDomain);
end;

procedure TMainForm.TranslationDeleteDomainActionUpdate(Sender: TObject);
begin
  TranslationDeleteDomainAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaDeleteDomain);
end;

procedure TMainForm.TranslationDeleteComponentActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaDeleteComponent);
end;

procedure TMainForm.TranslationDeleteComponentActionUpdate(Sender: TObject);
begin
  TranslationDeleteComponentAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaDeleteComponent);
end;

procedure TMainForm.TranslationDeleteItemActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaDeleteItem);
end;

procedure TMainForm.TranslationDeleteItemActionUpdate(Sender: TObject);
begin
  TranslationDeleteItemAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaDeleteItem);
end;

procedure TMainForm.TranslationRenameComponentActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaRenameComponent);
end;

procedure TMainForm.TranslationRenameComponentActionUpdate(Sender: TObject);
begin
  TranslationRenameComponentAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaRenameComponent);
end;

procedure TMainForm.TranslationRenameDomainActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaRenameDomain);
end;

procedure TMainForm.TranslationRenameDomainActionUpdate(Sender: TObject);
begin
  TranslationRenameDomainAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaRenameDomain);
end;

procedure TMainForm.SearchFindActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaFind);
end;

procedure TMainForm.SearchFindActionUpdate(Sender: TObject);
begin
  SearchFindAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaFind);
end;

procedure TMainForm.SearchReplaceActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaReplace);
end;

procedure TMainForm.SearchReplaceActionUpdate(Sender: TObject);
begin
  SearchReplaceAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaReplace);
end;

procedure TMainForm.SearchAgainActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaSearchAgain);
end;

procedure TMainForm.SearchAgainActionUpdate(Sender: TObject);
begin
  SearchAgainAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaSearchAgain);
end;

procedure TMainForm.NavigationPriorGroupActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaPreviousGroup);
end;

procedure TMainForm.NavigationPriorGroupActionUpdate(Sender: TObject);
begin
  NavigationPriorGroupAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaPreviousGroup);
end;

procedure TMainForm.NavigationNextGroupActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaNextGroup);
end;

procedure TMainForm.NavigationNextGroupActionUpdate(Sender: TObject);
begin
  NavigationNextGroupAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaNextGroup);
end;

procedure TMainForm.NavigationPriorItemActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaPreviousItem);
end;

procedure TMainForm.NavigationPriorItemActionUpdate(Sender: TObject);
begin
  NavigationPriorItemAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaPreviousItem);
end;

procedure TMainForm.NavigationNextItemActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaNextItem);
end;

procedure TMainForm.NavigationNextItemActionUpdate(Sender: TObject);
begin
  NavigationNextItemAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaNextItem);
end;

procedure TMainForm.NavigationPriorUntranslatedActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaPreviousUntranslated);
end;

procedure TMainForm.NavigationPriorUntranslatedActionUpdate(Sender: TObject);
begin
  NavigationPriorUntranslatedAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaPreviousUntranslated);
end;

procedure TMainForm.NavigationNextUntranslatedActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaNextUntranslated);
end;

procedure TMainForm.NavigationNextUntranslatedActionUpdate(Sender: TObject);
begin
  NavigationNextUntranslatedAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaNextUntranslated);
end;

procedure TMainForm.NavigationPriorUnacceptedActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaPreviousUnaccepted);
end;

procedure TMainForm.NavigationPriorUnacceptedActionUpdate(Sender: TObject);
begin
  NavigationPriorUnacceptedAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaPreviousUnaccepted);
end;

procedure TMainForm.NavigationNextUnacceptedActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaNextUnaccepted);
end;

procedure TMainForm.NavigationNextUnacceptedActionUpdate(Sender: TObject);
begin
  NavigationNextUnacceptedAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaNextUnaccepted);
end;

procedure TMainForm.LanguageSetNativeActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaSetNativeLanguage);
end;

procedure TMainForm.LanguageSetNativeActionUpdate(Sender: TObject);
begin
  LanguageSetNativeAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaSetNativeLanguage);
end;

procedure TMainForm.LanguageAddActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaAddLanguage);
end;

procedure TMainForm.LanguageAddActionUpdate(Sender: TObject);
begin
  LanguageAddAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaAddLanguage);
end;

procedure TMainForm.LanguageDeleteActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaDeleteLanguage);
end;

procedure TMainForm.LanguageDeleteActionUpdate(Sender: TObject);
begin
  LanguageDeleteAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaDeleteLanguage);
end;

procedure TMainForm.LanguageEditActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaEditLanguage);
end;

procedure TMainForm.LanguageEditActionUpdate(Sender: TObject);
begin
  LanguageEditAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaEditLanguage);
end;

procedure TMainForm.LanguageSwitchActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaSwitchLanguage);
end;

procedure TMainForm.LanguageSwitchActionUpdate(Sender: TObject);
begin
  LanguageSwitchAction.Enabled := Assigned(ActiveEditor);
end;

procedure TMainForm.TranslationGoogleActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaGoogleTranslate);
end;

procedure TMainForm.TranslationGoogleActionUpdate(Sender: TObject);
begin
  TranslationGoogleAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaGoogleTranslate);
end;

procedure TMainForm.TranslationSuggestActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaSuggestTranslation);
end;

procedure TMainForm.TranslationSuggestActionUpdate(Sender: TObject);
begin
  TranslationSuggestAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaSuggestTranslation);
end;

procedure TMainForm.TranslationRejectActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaRejectTranslation);
end;

procedure TMainForm.TranslationRejectActionUpdate(Sender: TObject);
begin
  TranslationRejectAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaRejectTranslation);
end;

procedure TMainForm.TranslationAcceptActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaAcceptTranslation);
end;

procedure TMainForm.TranslationAcceptActionUpdate(Sender: TObject);
begin
  TranslationAcceptAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaAcceptTranslation);
end;

procedure TMainForm.TranslationAutoTranslateActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaAutoTranslate);
end;

procedure TMainForm.TranslationAutoTranslateActionUpdate(Sender: TObject);
begin
  TranslationAutoTranslateAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaAutoTranslate);
end;

procedure TMainForm.TranslationCopyOriginalActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaCopyOriginalText);
end;

procedure TMainForm.TranslationCopyOriginalActionUpdate(Sender: TObject);
begin
  TranslationCopyOriginalAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaCopyOriginalText);
end;

procedure TMainForm.TranslationNoteClearActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaClearTranslatorNote);
end;

procedure TMainForm.TranslationNoteClearActionUpdate(Sender: TObject);
begin
  TranslationNoteClearAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaClearTranslatorNote);
end;

procedure TMainForm.TranslationNoteEditActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaEditTranslatorNote);
end;

procedure TMainForm.TranslationNoteEditActionUpdate(Sender: TObject);
begin
  TranslationNoteEditAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaEditTranslatorNote);
end;

procedure TMainForm.SortByOriginalActionExecute(Sender: TObject);
begin
  ActiveEditor.SortColumn := COLUMN_ORIGINAL;
end;

procedure TMainForm.SortByOriginalActionUpdate(Sender: TObject);
begin
  SortByOriginalAction.Enabled := Assigned(ActiveEditor);
  SortByOriginalAction.Checked := Assigned(ActiveEditor) and
    (ActiveEditor.SortColumn = COLUMN_ORIGINAL);
end;

procedure TMainForm.SortByTranslationActionExecute(Sender: TObject);
begin
  ActiveEditor.SortColumn := COLUMN_TRANSLATED;
end;

procedure TMainForm.SortByTranslationActionUpdate(Sender: TObject);
begin
  SortByTranslationAction.Enabled := Assigned(ActiveEditor);
  SortByTranslationAction.Checked := Assigned(ActiveEditor) and
    (ActiveEditor.SortColumn = COLUMN_TRANSLATED);
end;

procedure TMainForm.SortByStateActionExecute(Sender: TObject);
begin
  ActiveEditor.SortColumn := COLUMN_STATE;
end;

procedure TMainForm.SortByStateActionUpdate(Sender: TObject);
begin
  SortByStateAction.Enabled := Assigned(ActiveEditor);
  SortByStateAction.Checked := Assigned(ActiveEditor) and
    (ActiveEditor.SortColumn = COLUMN_STATE);
end;

procedure TMainForm.SortByCommentActionExecute(Sender: TObject);
begin
  ActiveEditor.SortColumn := COLUMN_COMMENT;
end;

procedure TMainForm.SortByCommentActionUpdate(Sender: TObject);
begin
  SortByCommentAction.Enabled := Assigned(ActiveEditor);
  SortByCommentAction.Checked := Assigned(ActiveEditor) and
    (ActiveEditor.SortColumn = COLUMN_COMMENT);
end;

procedure TMainForm.SortByNoteActionExecute(Sender: TObject);
begin
  ActiveEditor.SortColumn := COLUMN_NOTE;
end;

procedure TMainForm.SortByNoteActionUpdate(Sender: TObject);
begin
  SortByNoteAction.Enabled := Assigned(ActiveEditor);
  SortByNoteAction.Checked := Assigned(ActiveEditor) and
    (ActiveEditor.SortColumn = COLUMN_NOTE);
end;

procedure TMainForm.SortByNameActionExecute(Sender: TObject);
begin
  ActiveEditor.SortColumn := COLUMN_NAME;
end;

procedure TMainForm.SortByNameActionUpdate(Sender: TObject);
begin
  SortByNameAction.Enabled := Assigned(ActiveEditor);
  SortByNameAction.Checked := Assigned(ActiveEditor) and
    (ActiveEditor.SortColumn = COLUMN_NAME);
end;

procedure TMainForm.SortByPluralsActionExecute(Sender: TObject);
begin
  ActiveEditor.SortColumn := COLUMN_PLURALS;
end;

procedure TMainForm.SortByPluralsActionUpdate(Sender: TObject);
begin
  SortByPluralsAction.Enabled := Assigned(ActiveEditor);
  SortByPluralsAction.Checked := Assigned(ActiveEditor) and
    (ActiveEditor.SortColumn = COLUMN_PLURALS);
end;

procedure TMainForm.SortByNoneActionExecute(Sender: TObject);
begin
  ActiveEditor.SortColumn := -1;
end;

procedure TMainForm.SortByNoneActionUpdate(Sender: TObject);
begin
  SortByNoneAction.Enabled := Assigned(ActiveEditor);
  SortByNoneAction.Checked := Assigned(ActiveEditor) and
    (ActiveEditor.SortColumn < 0);
end;

procedure TMainForm.SortAscendingActionExecute(Sender: TObject);
begin
  ActiveEditor.SortAscending := True;
end;

procedure TMainForm.SortAscendingActionUpdate(Sender: TObject);
begin
  SortAscendingAction.Enabled := Assigned(ActiveEditor);
  SortAscendingAction.Checked := Assigned(ActiveEditor) and
    ActiveEditor.SortAscending;
end;

procedure TMainForm.SortDescendingActionExecute(Sender: TObject);
begin
  ActiveEditor.SortAscending := False;
end;

procedure TMainForm.SortDescendingActionUpdate(Sender: TObject);
begin
  SortDescendingAction.Enabled := Assigned(ActiveEditor);
  SortDescendingAction.Checked := Assigned(ActiveEditor) and
    not ActiveEditor.SortAscending;
end;

procedure TMainForm.GroupArrangementActionExecute(Sender: TObject);
begin
  ActiveEditor.SortGrouping := not ActiveEditor.SortGrouping;
end;

procedure TMainForm.GroupArrangementActionUpdate(Sender: TObject);
begin
  GroupArrangementAction.Enabled := Assigned(ActiveEditor);
  GroupArrangementAction.Checked := Assigned(ActiveEditor) and
    ActiveEditor.SortGrouping;
end;

procedure TMainForm.GroupCollapseAllActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaCollapseGroups);
end;

procedure TMainForm.GroupCollapseAllActionUpdate(Sender: TObject);
begin
  GroupCollapseAllAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaCollapseGroups);
end;

procedure TMainForm.GroupExpandAllActionExecute(Sender: TObject);
begin
  ActiveEditor.Perform(eaExpandGroups);
end;

procedure TMainForm.GroupExpandAllActionUpdate(Sender: TObject);
begin
  GroupExpandAllAction.Enabled := Assigned(ActiveEditor) and
    ActiveEditor.CanPerform(eaExpandGroups);
end;

procedure TMainForm.ViewToolbarActionExecute(Sender: TObject);
begin
  MainToolbar.Visible := not MainToolbar.Visible;
end;

procedure TMainForm.ViewToolbarActionUpdate(Sender: TObject);
begin
  ViewToolbarAction.Checked := MainToolbar.Visible;
end;

procedure TMainForm.ViewStatusBarActionExecute(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TMainForm.ViewStatusBarActionUpdate(Sender: TObject);
begin
  ViewStatusBarAction.Checked := StatusBar.Visible;
end;

procedure TMainForm.ViewPreferencesActionExecute(Sender: TObject);
var
  I: Integer;
begin
  if TOptionsDialog.Execute then
  begin
    for I := 0 to MDIChildCount - 1 do
      TTranslationEditor(MDIChildren[I]).SettingsChanged;
  end;
end;

procedure TMainForm.HelpAboutActionExecute(Sender: TObject);
begin
  TAboutDialog.Execute;
end;

end.
