{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit frmTranslationEditor;

interface

uses
  Windows, Messages, SysUtils, Types, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Tabs, Menus, ActnList, ImgList,
  i18nCore, i18nGoogle, i18nHTTP, i18nLocalizer, i18nCtrls, i18nCatalog,
  i18nHashList, i18nPlurals, DefinitionList, PluralTextEdit, dlgSearch,
  System.Actions;

const
  CM_ADJUST_LIST_COLUMNS = WM_USER;

const
  GROUP_BY_NONE     = 0;
  GROUP_BY_DOMAIN   = 1;
  GROUP_BY_CATEGORY = 2;
  GROUP_BY_STATE    = 3;

const // Definitions.Columns[i].Tag
  COLUMN_ORIGINAL   = 0;
  COLUMN_COMMENT    = 1;
  COLUMN_TRANSLATED = 2;
  COLUMN_NOTE       = 3;
  COLUMN_STATE      = 4;
  COLUMN_NAME       = 5;
  COLUMN_PLURALS    = 6;

const
  CHANGED_TARGET_LANGUAGES = 1;
  CHANGED_TARGET_LANGUAGE  = 2;
  CHANGED_SOURCE_LANGUAGE  = 3;
  CHANGED_PROGRESS         = 4;
  CHANGED_READONLY         = 5;
  CHANGED_MODIFIED         = 6;

type
  TEditorAction = (eaSave, eaSaveAs, eaImport, eaExport, eaSetNativeLanguage,
    eaAddLanguage, eaDeleteLanguage, eaEditLanguage, eaSwitchLanguage,
    eaDeleteDomain, eaDeleteComponent, eaDeleteItem, eaRenameDomain,
    eaRenameComponent, eaFind, eaReplace, eaSearchAgain, eaPreviousItem,
    eaNextItem, eaPreviousGroup, eaNextGroup, eaPreviousUntranslated,
    eaNextUntranslated, eaPreviousUnaccepted, eaNextUnaccepted,
    eaEditTranslatorNote, eaClearTranslatorNote, eaAcceptTranslation,
    eaRejectTranslation, eaCopyOriginalText, eaSuggestTranslation,
    eaGoogleTranslate, eaAutoTranslate, eaExpandGroups, eaCollapseGroups);

  TEditorBulkAction = function(Node: TListNode): Boolean of object;

  TSearchDetails = record
    Params: TSearchParams;
    StopIndex: Integer;
    CurrentIndex: Integer;
    Active: Boolean;
  end;

  TTranslationEditor = class(TForm)
    Translator: TTranslator;
    ListImages: TImageList;
    GoogleTranslator: TGoogleTranslator;
    DefinitionSelectionTimer: TTimer;
    LanguageActions: TActionList;
    SaveDialog: TSaveDialog;
    SourceCulturePopup: TPopupMenu;
    TargetCulturePopup: TPopupMenu;
    DefinitionItemPopup: TPopupMenu;
    NotePopup: TPopupMenu;
    EditNote1: TMenuItem;
    ClearNote1: TMenuItem;
    AcceptTranslation1: TMenuItem;
    RejectTranslation1: TMenuItem;
    N1: TMenuItem;
    GoogleTranslate1: TMenuItem;
    SuggestTranslation1: TMenuItem;
    N2: TMenuItem;
    EditNote2: TMenuItem;
    ClearNote2: TMenuItem;
    SortByPopup: TMenuItem;
    OriginalText1: TMenuItem;
    TranslatedText1: TMenuItem;
    DevelopersComments1: TMenuItem;
    TranslatorsNote1: TMenuItem;
    TranslationState1: TMenuItem;
    N4: TMenuItem;
    Ascending1: TMenuItem;
    Descending1: TMenuItem;
    PropertyName1: TMenuItem;
    DefinitionsHeaderPopup: TPopupMenu;
    N3: TMenuItem;
    RenameComponent1: TMenuItem;
    Delete1: TMenuItem;
    DomainPopup: TPopupMenu;
    DeleteDomain1: TMenuItem;
    RenameDomain1: TMenuItem;
    DeleteComponent1: TMenuItem;
    CopyOriginalText1: TMenuItem;
    None1: TMenuItem;
    DetailsPanel: TPanel;
    PanelItemDetails: TPanel;
    HSplitter: TSplitter;
    TranslationDetailsPanel: TPanel;
    TranslatorNote: TImageLabel;
    TranslationHeader: TPanel;
    TranslatedTextLabel: TLabel;
    TargetCultureLabel: TCultureLabel;
    OriginalDetailsPanel: TPanel;
    DeveloperComment: TImageLabel;
    OriginalHeader: TPanel;
    OriginalTextLabel: TLabel;
    SourceCultureLabel: TCultureLabel;
    VSplitter: TSplitter;
    Definitions: TListView;
    ControlBar: TPanel;
    Domains: TComboBox;
    N5: TMenuItem;
    ExpandAllGroups1: TMenuItem;
    CollapseAllGroups1: TMenuItem;
    DefinitionGroupPopup: TPopupMenu;
    ExpandMenuItem: TMenuItem;
    CollapseMenuItem: TMenuItem;
    AutoTranslate1: TMenuItem;
    N6: TMenuItem;
    RejectTranslation2: TMenuItem;
    AcceptTranslation2: TMenuItem;
    N7: TMenuItem;
    CopyOriginalText2: TMenuItem;
    GoogleTranslate2: TMenuItem;
    AutoTranslate2: TMenuItem;
    EditNote3: TMenuItem;
    ClearNote3: TMenuItem;
    Delete2: TMenuItem;
    ProgressBar: TProgressBar;
    ArrangeinGroups1: TMenuItem;
    N10: TMenuItem;
    N9: TMenuItem;
    TranslationTextDetails: TPanel;
    TranslatedText: TMemo;
    TranslationPlurals: TTabSet;
    OriginalTextDetails: TPanel;
    OriginalPlurals: TTabSet;
    OriginalText: TMemo;
    procedure PanelItemDetailsResize(Sender: TObject);
    procedure DefinitionsColumnClick(Sender: TObject; Column: TListColumn);
    procedure DefinitionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DefinitionSelectionTimerTimer(Sender: TObject);
    procedure SourceLocaleChoicesClick(Sender: TObject);
    procedure TargetLocaleChoicesClick(Sender: TObject);
    procedure SaveDialogCanClose(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure TranslatorAfterTranslate(Sender: TObject);
    procedure TranslatorAfterFlipLayout(Sender: TObject);
    procedure SourceCultureLabelClick(Sender: TObject);
    procedure SourceCulturePopupPopup(Sender: TObject);
    procedure TargetCulturePopupPopup(Sender: TObject);
    procedure TargetCultureLabelClick(Sender: TObject);
    procedure OriginalDetailsPanelResize(Sender: TObject);
    procedure TranslationDetailsPanelResize(Sender: TObject);
    procedure DefinitionsResize(Sender: TObject);
    procedure DefinitionsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DefinitionsContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure DefinitionsColumnRightClick(Sender: TObject; Column: TListColumn;
      Point: TPoint);
    procedure TranslatorNoteDblClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure TranslatedTextChange(Sender: TObject);
    procedure DomainsChange(Sender: TObject);
    procedure DomainsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure DefinitionsDataHint(Sender: TObject; StartIndex, EndIndex: Integer);
    procedure DefinitionsData(Sender: TObject; Item: TListItem);
    procedure DefinitionsDblClick(Sender: TObject);
    procedure DefinitionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ExpandMenuItemClick(Sender: TObject);
    procedure CollapseMenuItemClick(Sender: TObject);
    procedure DefinitionGroupPopupPopup(Sender: TObject);
    procedure DefinitionsClick(Sender: TObject);
    procedure DefinitionsCustomDraw(Sender: TCustomListView; const ARect: TRect;
      var DefaultDraw: Boolean);
    procedure DefinitionsDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure OriginalPluralsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure TranslationPluralsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure TranslatedTextExit(Sender: TObject);
  private
    fFileName: String;
    fNativeCulture: TCultureInfo;
    fTargetCulture: TCultureInfo;
    fReadonly: Boolean;
    fModified: Boolean;
    fProgress: Double;
    fEditingItem: TItemNode;
    function GetModified: Boolean; inline;
    procedure SetModified(Value: Boolean);
    procedure SetFileName(const Value: String);
    procedure SetReadonly(Value: Boolean);
    function GetSortColumn: Integer; inline;
    procedure SetSortColumn(Value: Integer); inline;
    function GetSortAscending: Boolean; inline;
    procedure SetSortAscending(Value: Boolean); inline;
    function GetSortGrouping: Boolean; inline;
    procedure SetSortGrouping(Value: Boolean); inline;
    procedure SetNativeCulture(Value: TCultureInfo);
    procedure SetTargetCulture(Value: TCultureInfo);
    function GetCurrentDomain: TTextDomain; inline;
    function GetCurrentSelection: TListNode; inline;
    function GetCurrentGroup: TGroupNode; inline;
    function GetCurrentItem: TItemNode; inline;
    function GetColumnByID(ID: Integer): TListColumn;
    procedure AdjustListColumns(var Message: TMessage); message CM_ADJUST_LIST_COLUMNS;
    procedure TargetLanguageActionExecute(Sender: TObject);
    procedure TargetLanguageActionUpdate(Sender: TObject);
  private
    StateNames: array [TTranslationState] of String;
    Catalog: TTranslationCatalog;
    List: TDefinitionList;
    TargetEditingText: TPluralTextEdit;
    LastWindowState: TWindowState;
    SearchDetails: TSearchDetails;
    AllStats: TTranslationStats;
    HeaderRightClicked: Boolean;
    PromptForNativeLanguage: Boolean;
    procedure RebuildLanguageActions;
    function GetGroupDisplayName(Group: TGroupNode): String;
    procedure DrawListGroup(Canvas: TCanvas; var Rect: TRect; Group: TGroupNode);
    procedure DrawListItem(Canvas: TCanvas; var Rect: TRect; Item: TItemNode);
    procedure DrawListBackground(Canvas: TCanvas; var Rect: TRect;
      ListItem: TListItem; Color: TColor);
    procedure ReloadDefinitions;
    procedure ReloadDomains;
    procedure ReloadCurrentItemDetails;
    procedure AutoTranslateUsingItem(Item: TItemNode);
    function UpdateItem(Item: TItemNode; NewValue: String;
      NewState: TTranslationState; ValidatePlurals: Boolean): Boolean;
    function UpdateItemState(Item: TItemNode; NewState: TTranslationState): Boolean;
    function UpdateItemNote(Item: TItemNode; const NewNote: String): Boolean;
    procedure UpdateTranslations(Translations: TList);
    procedure UpdateProgress(Recalc: Boolean);
    procedure ValidateNumOfPlurals(var PluralForms: String; NumOfPlurals: Integer);
    procedure PreparePluralChoices(Control: TTabSet; Culture: TCultureInfo);
    procedure PrepareStateNames;
    function CanDiscardChanges: Boolean;
    function CommitEditingItem: Boolean;
    procedure SavePersistentUserChoices;
    procedure LoadPersistentUserChoices;
    function Apply(Node: TListNode; Action: TEditorBulkAction): Boolean;
    function UpdateBulkProgress(Item: TItemNode): Boolean;
    property ColumnByID[ID: Integer]: TListColumn read GetColumnByID;
  private
    function DoSave: Boolean;
    function DoSaveAs: Boolean;
    function DoImport: Boolean;
    function DoExport: Boolean;
    function DoSetNativeLanguage: Boolean;
    function DoAddLanguage: Boolean;
    function DoDeleteLanguage: Boolean;
    function DoEditLanguage: Boolean;
    function DoSwitchLanguage: Boolean;
    function DoFind: Boolean;
    function DoReplace: Boolean;
    function DoSearch(IsNew: Boolean): Boolean;
    function DoNavigateItem(Backward: Boolean): Boolean;
    function DoNavigateGroup(Backward: Boolean): Boolean;
    function DoNavigateConditional(Backward: Boolean; Condition: TTranslationStates): Boolean;
    function DoDeleteDomain(Domain: TTextDomain): Boolean;
    function DoDeleteComponent(Item: TItemNode): Boolean;
    function DoDeleteItem(Item: TItemNode): Boolean;
    function DoDeleteGroupItems(Group: TGroupNode): Boolean;
    function DoRenameDomain(Domain: TTextDomain): Boolean;
    function DoRenameComponent(Item: TItemNode): Boolean;
    function DoEditTranslatorNote(Node: TListNode): Boolean;
    function DoClearTranslatorNote(Node: TListNode): Boolean;
    function DoAcceptTranslation(Node: TListNode): Boolean;
    function DoRejectTranslation(Node: TListNode): Boolean;
    function DoCopyOriginalText(Node: TListNode): Boolean;
    function DoGoogleTranslate(Node: TListNode): Boolean;
    function DoAutoTranslate(Node: TListNode): Boolean;
    function DoSuggestTranslation(Item: TItemNode): Boolean;
    function DoExpandCollapseAll(Expand: Boolean): Boolean;
  public
    constructor CreateNew; reintroduce;
    constructor Create(const AFileName: String; AReadonly: Boolean = False); reintroduce;
    destructor Destroy; override;
    procedure SettingsChanged;
    function CanPerform(Act: TEditorAction): Boolean;
    function Perform(Act: TEditorAction): Boolean;
    property Progress: Double read fProgress;
    property Modified: Boolean read GetModified write SetModified;
    property FileName: String read fFileName write SetFileName;
    property Readonly: Boolean read fReadonly write SetReadonly;
    property SortColumn: Integer read GetSortColumn write SetSortColumn;
    property SortAscending: Boolean read GetSortAscending write SetSortAscending;
    property SortGrouping: Boolean read GetSortGrouping write SetSortGrouping;
    property NativeCulture: TCultureInfo read fNativeCulture write SetNativeCulture;
    property TargetCulture: TCultureInfo read fTargetCulture write SetTargetCulture;
    property CurrentDomain: TTextDomain read GetCurrentDomain;
    property CurrentSelection: TListNode read GetCurrentSelection;
    property CurrentGroup: TGroupNode read GetCurrentGroup;
    property CurrentItem: TItemNode read GetCurrentItem;
    property EditingItem: TItemNode read fEditingItem;
  end;

implementation

{$R *.dfm}

uses
  Registry, CommCtrl, Themes, UxTheme, Contnrs, StrUtils, DataModule, frmMain,
  i18nUtils, i18nZStrList, i18nUnicode, i18nMD5, dlgLanguageSelector, dlgRename,
  dlgExportWizard, dlgImportWizard, dlgSuggestion, dlgPlainTextEditor,
  dlgPluralRuleEditor;

const
  clAlternative = clWebSnow;

const
  SNotTranslated             = 'Not Translated';
  SFuzzyTranslated           = 'Fuzzy';
  SGoogleTranslated          = 'Google Translated';
  SAutoTranslated            = 'Auto Translated';
  SUserTranslated            = 'Translated';

  SProperties                = 'Properties';
  SLiterals                  = 'Literals';

  SWithoutPluralForms        = 'Single Form only';
  SWithPluralForms           = 'With Plural Forms';

  SWithoutComment            = 'Without Developer''s Comment';
  SWithComment               = 'With Developer''s Comment';

  SWithoutNote               = 'Without Translator''s Note';
  SWithNote                  = 'With Translator''s Note';

  SShortText                 = 'Short Size';
  SMediumText                = 'Medium Size';
  SLongText                  = 'Long Size';

  SOriginal                  = 'Original Text:';
  SApprovedTranslation       = 'Approved Translation:';

  SNoItemToShow              = 'There are no items to show in this view.';

  SSaveChangesConfirm        = '{0} has been modified.'#13#10#13#10
                             + 'Do you want to save changes?';
  SSaveOverwriteConfirm      = '{0} already exists.'#13#10#13#10
                             + 'Do you want to replace it?';
  SSavePathNotFoundError     = 'Folder {0} does not exist.';
  SSaveFileError             = '{0} is read-only or is in use by another application.';

  SOpenFileNotFoundError     = '{0} does not exist.';
  SOpenFileError             = '{0} is not a valid i18n file.';

  SSearchRestartFirstConfirm = 'Restart the search from the first item?';
  SSearchRestartLastConfirm  = 'Restart the search from the last item?';
  SSearchReplaceConfirm      = 'Replace this occurrence of ''{0}''?';
  SSearchNotFound            = 'Search string ''{0}'' not found.';

  SSetBaseLanguage           = 'Base language of the current translation file is not specified. Please select the base language from the given list.';
  SAddLanguage               = 'Please select the language that you want to add to the list of available translations.';
  SDelLanguage               = 'Please select the language that you want to remove from the list of translations.';
  SDelLanguageConfirm        = 'Do you really want to remove {0} from the translation languages?'#13#10#13#10
                             + 'This operation cannot be reverted.';

  SRenameDomain              = 'Rename Text Domain';
  SRenameComponent           = 'Rename Component';

  SDelDomainConfirm          = 'Do you really want to permanently remove domain ''{0}''?';
  SDelComponentConfirm       = 'Do you really want to permanently remove component ''{0}''?';
  SDelGroupConfirm           = 'Do you really want to permanently remove all items in the selected group?';
  SDelItemConfirm            = 'Do you really want to permanently remove the selected item?';

  SEditNote                  = 'Translator''s Note';

  SGoogleLanguagePairError   = 'The language pair is not supported by the Google translator.';
  SGoogleConnectError        = 'Cannot reach to the Google translator.'#13#10
                             + 'Make sure your computer is connected to the Internet.';

  SBulkActionConfirm         = 'This operation will be applied to all the items of the selected group.'#13#10
                             + 'Do you want to continue?'#13#10#13#10
                             + 'You can press <ESCAPE> key to cancel the operation at anytime.';

{ TTranslationEditor }

procedure TTranslationEditor.SetFileName(const Value: String);
var
  NewFileName: String;
begin
  NewFileName := ExpandFileName(Value);
  if FileName <> NewFileName then
  begin
    DM.RecentFiles.Add(FileName);
    fFileName := NewFileName;
    Caption := FileName;
    DM.RecentFiles.Remove(FileName);
    SaveDialog.FileName := FileName;
  end;
end;

procedure TTranslationEditor.SetReadonly(Value: Boolean);
begin
  if Readonly <> Value then
  begin
    fReadonly := Value;
    if Assigned(EditingItem) and Assigned(EditingItem.Translation) then
    begin
      TranslatedText.ReadOnly := Readonly;
      TargetEditingText.Text := EditingItem.Translation.Value;
    end;
    if Readonly and SearchDetails.Active and (SearchDetails.Params.Mode = smReplace) then
      SearchDetails.Active := False;
    MainForm.EditorChanged(Self, CHANGED_READONLY);
  end;
end;

function TTranslationEditor.GetSortColumn: Integer;
begin
  Result := List.SortColumn;
end;

procedure TTranslationEditor.SetSortColumn(Value: Integer);
begin
  if List.SortColumn = Value then
    List.SortAscending := not List.SortAscending
  else
    List.SortColumn := Value;
  DM.SortColumn := List.SortColumn;
  DM.SortAscending := List.SortAscending;
end;

function TTranslationEditor.GetSortAscending: Boolean;
begin
  Result := List.SortAscending;
  DM.SortAscending := List.SortAscending;
end;

procedure TTranslationEditor.SetSortAscending(Value: Boolean);
begin
  List.SortAscending := Value;
end;

function TTranslationEditor.GetSortGrouping: Boolean;
begin
  Result := List.SortGrouping;
end;

procedure TTranslationEditor.SetSortGrouping(Value: Boolean);
begin
  List.SortGrouping := Value;
  DM.SortGrouping := List.SortGrouping;
end;

function TTranslationEditor.GetModified: Boolean;
begin
  Result := not Readonly and (fModified or TargetEditingText.Modified);
end;

procedure TTranslationEditor.SetModified(Value: Boolean);
begin
  if not Readonly and (fModified <> Value) then
  begin
    if not Value then
      TargetEditingText.Modified := False;
    fModified := Value;
    if Modified then
      Caption := '*' + Caption
    else
      Caption := Copy(Caption, 2, Length(Caption) - 1);
    MainForm.EditorChanged(Self, CHANGED_MODIFIED);
  end;
end;

procedure TTranslationEditor.SetNativeCulture(Value: TCultureInfo);
begin
  if NativeCulture <> Value then
  begin
    fNativeCulture := Value;
    Catalog.NativeCulture := Value;
    OriginalTextLabel.Caption := Translator.GetText(SOriginal);
    SourceCultureLabel.Culture := NativeCulture;
    if Assigned(NativeCulture) then
    begin
      if ColumnByID[COLUMN_ORIGINAL].ImageIndex = -1 then
        ColumnByID[COLUMN_ORIGINAL].ImageIndex := ListImages.Count;
      DM.CopyFlagTo(NativeCulture, ListImages,
        ColumnByID[COLUMN_ORIGINAL].ImageIndex, 0, 1);
      OriginalText.BiDiMode := NativeCulture.BiDiMode;
      DeveloperComment.BiDiMode := NativeCulture.BiDiMode;
      PreparePluralChoices(OriginalPlurals, NativeCulture);
      GoogleTranslator.SourceLang := CultureToGoogleLang(NativeCulture);
    end
    else
    begin
      if ColumnByID[COLUMN_ORIGINAL].ImageIndex <> -1 then
      begin
        if ColumnByID[COLUMN_ORIGINAL].ImageIndex = ListImages.Count - 1 then
          ListImages.Delete(ColumnByID[COLUMN_ORIGINAL].ImageIndex);
        ColumnByID[COLUMN_ORIGINAL].ImageIndex := -1;
      end;
      OriginalText.ParentBiDiMode := True;
      DeveloperComment.ParentBiDiMode := True;
      OriginalPlurals.Visible := False;
      GoogleTranslator.SourceLang := '';
    end;
    with ColumnByID[COLUMN_ORIGINAL] do
    begin
      AutoSize := False;
      AutoSize := True;
    end;
    MainForm.EditorChanged(Self, CHANGED_SOURCE_LANGUAGE);
  end;
end;

procedure TTranslationEditor.SetTargetCulture(Value: TCultureInfo);
begin
  if TargetCulture <> Value then
  begin
    fTargetCulture := Value;
    fEditingItem := nil;
    TargetCultureLabel.Culture := TargetCulture;
    if Assigned(TargetCulture) then
    begin
      if ColumnByID[COLUMN_TRANSLATED].ImageIndex = -1 then
        ColumnByID[COLUMN_TRANSLATED].ImageIndex := ListImages.Count;
      DM.CopyFlagTo(TargetCulture, ListImages,
        ColumnByID[COLUMN_TRANSLATED].ImageIndex, 0, 1);
      if TargetCultureLabel.Culture = SourceCultureLabel.Culture then
      begin
        OriginalTextLabel.Caption := Translator.GetText(SOriginal);
        SourceCultureLabel.Culture := NativeCulture;
        if Assigned(NativeCulture) then
        begin
          OriginalText.BiDiMode := NativeCulture.BiDiMode;
          PreparePluralChoices(OriginalPlurals, NativeCulture);
        end
        else
        begin
          OriginalText.ParentBiDiMode := True;
          OriginalPlurals.Visible := False;
        end;
      end;
      TranslatedText.BiDiMode := TargetCulture.BiDiMode;
      TranslatorNote.BiDiMode := TargetCulture.BiDiMode;
      PreparePluralChoices(TranslationPlurals, TargetCulture);
      TargetEditingText.PluralIndex := TranslationPlurals.TabIndex;
      GoogleTranslator.TargetLang := CultureToGoogleLang(TargetCulture);
      VSplitter.Visible := True;
      DetailsPanel.Visible := True;
      DetailsPanel.Top := VSplitter.BoundsRect.Bottom;
    end
    else
    begin
      if ColumnByID[COLUMN_TRANSLATED].ImageIndex <> -1 then
      begin
        if ColumnByID[COLUMN_TRANSLATED].ImageIndex = ListImages.Count - 1 then
          ListImages.Delete(ColumnByID[COLUMN_TRANSLATED].ImageIndex);
        ColumnByID[COLUMN_TRANSLATED].ImageIndex := -1;
      end;
      TranslatedText.ParentBiDiMode := True;
      TranslatorNote.ParentBiDiMode := True;
      TranslationPlurals.Visible := False;
      GoogleTranslator.TargetLang := '';
      DetailsPanel.Visible := False;
      VSplitter.Visible := False;
    end;
    List.ActiveCulture := TargetCulture;
    ReloadCurrentItemDetails;
    UpdateProgress(True);
    with ColumnByID[COLUMN_TRANSLATED] do
    begin
      AutoSize := False;
      AutoSize := True;
    end;
    MainForm.EditorChanged(Self, CHANGED_TARGET_LANGUAGE);
  end;
end;

function TTranslationEditor.GetCurrentDomain: TTextDomain;
begin
  Result := TTextDomain(Domains.Items.Objects[Domains.ItemIndex]);
end;

function TTranslationEditor.GetCurrentSelection: TListNode;
var
  ListItem: TListItem;
begin
  ListItem := Definitions.Selected;
  if Assigned(ListItem)then
    Result := TListNode(ListItem.Data)
  else
    Result := nil;
end;

function TTranslationEditor.GetCurrentGroup: TGroupNode;
var
  Node: TListNode absolute Result;
begin
  Node := CurrentSelection;
  if not (Node is TGroupNode) then
    Node := nil;
end;

function TTranslationEditor.GetCurrentItem: TItemNode;
var
  Node: TListNode absolute Result;
begin
  Node := CurrentSelection;
  if not (Node is TItemNode) then
    Node := nil;
end;

function TTranslationEditor.GetColumnByID(ID: Integer): TListColumn;
var
  I: Integer;
begin
  for I := 0 to Definitions.Columns.Count - 1 do
  begin
    Result := Definitions.Columns[I];
    if Result.Tag = ID then
      Exit;
  end;
  Result := nil;
end;

procedure TTranslationEditor.AdjustListColumns(var Message: TMessage);
begin
  Definitions.Width := Definitions.Width + 1;
  Definitions.Width := Definitions.Width - 1;
  Definitions.Update;
  if PromptForNativeLanguage then
  begin
    PromptForNativeLanguage := False;
    DoSetNativeLanguage;
  end;
end;

function TTranslationEditor.GetGroupDisplayName(Group: TGroupNode): String;
begin
  case List.SortField of
    dfDomain:
      Result := Group.Items[0].TextDomain.Name;
    dfOriginal, dfTranslated:
      case TTextSizeGroup(Group.ID) of
        tgShort: Result := Translator.GetText(SShortText);
        tgMedium: Result := Translator.GetText(SMediumText);
      else
        Result := Translator.GetText(SLongText);
      end;
    dfState:
      Result := StateNames[TTranslationState(Group.ID)];
    dfComment:
      if Group.ID = 0 then
        Result := Translator.GetText(SWithoutComment)
      else
        Result := Translator.GetText(SWithComment);
    dfNote:
      if Group.ID = 0 then
        Result := Translator.GetText(SWithoutNote)
      else
        Result := Translator.GetText(SWithNote);
    dfName:
      if Group.ID = 0 then
        Result := Translator.GetText(SProperties)
      else
        Result := Translator.GetText(SLiterals);
    dfPluralForms:
      if Group.ID = 0 then
        Result := SWithoutPluralForms
      else
        Result := SWithPluralForms;
  else
    Result := '';
  end;
end;

procedure TTranslationEditor.DrawListGroup(Canvas: TCanvas; var Rect: TRect;
  Group: TGroupNode);
var
  ImageIndex: Integer;
  X, Y: Integer;
  Text: String;
  AlterForeColor: Boolean;
begin
  AlterForeColor := not Group.LVItem.Selected
    or ((Win32MajorVersion > 5) and ThemeControl(Definitions));
  // draw expand/collapse icon
  ImageIndex := 10 + Ord(Group.Expanded);
  if Definitions.IsRightToLeft then
  begin
    X := Rect.Right - Definitions.SmallImages.Width;
    Rect.Right := X - 2;
    Inc(Rect.Left, 2);
  end
  else
  begin
    X := Rect.Left;
    Rect.Left := X + Definitions.SmallImages.Width + 2;
    Dec(Rect.Right, 2);
  end;
  Y := (Rect.Top + Rect.Bottom - Definitions.SmallImages.Height) div 2;
  Definitions.SmallImages.Draw(Canvas, X, Y, ImageIndex);
  // draw label
  Canvas.Font.Style := [fsBold];
  SelectObject(Canvas.Handle, Canvas.Font.Handle); // workaround Delphi bug
  if AlterForeColor then
    SetTextColor(Canvas.Handle, ColorToRGB(clHotLight));
  Text := GetGroupDisplayName(Group);
  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect,
    Definitions.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or
    DT_NOPREFIX or DT_SINGLELINE));
  X := Canvas.TextWidth(Text) + 8;
  if Definitions.IsRightToLeft then
    Dec(Rect.Right, X)
  else
    Inc(Rect.Left, X);
  // draw stats
  Canvas.Font.Style := [];
  SelectObject(Canvas.Handle, Canvas.Font.Handle); // workaround Delphi bug
  if AlterForeColor then
    SetTextColor(Canvas.Handle, ColorToRGB(clHotLight));
  Text := DM.Localizer.FormatNumber('#,##0', Group.Count);
  if List.SortField <> dfState then
  begin
    Text := '/' + Text;
    Text := DM.Localizer.FormatNumber('#,##0', Group.ApprovedCount) + Text;
  end;
  Text := '(' + Text + ')';
  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect,
    Definitions.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or
    DT_NOPREFIX or DT_SINGLELINE));
  X := Canvas.TextWidth(Text) + 8;
  if Definitions.IsRightToLeft then
    Dec(Rect.Right, X)
  else
    Inc(Rect.Left, X);
  // draw separator line
  Canvas.Pen.Color := cl3DLight;
  Canvas.MoveTo(Rect.Left, (Rect.Top + Rect.Bottom) div 2);
  Canvas.LineTo(Rect.Right, (Rect.Top + Rect.Bottom) div 2);
end;

procedure TTranslationEditor.DrawListItem(Canvas: TCanvas; var Rect: TRect;
  Item: TItemNode);

  procedure DrawColumn(Column: Integer);
  var
    DrawFlags: Integer;
    ImageIndex: Integer;
    Text: String;
    Rect: TRect;
    X, Y: Integer;
  begin
    // get column bounding rectangle
    if Column = 0 then
      UnionRect(Rect, Item.LVItem.DisplayRect(drIcon), Item.LVItem.DisplayRect(drLabel))
    else
      ListView_GetSubItemRect(Definitions.Handle, Item.LVItem.Index, Column, LVIR_BOUNDS, @Rect);
    // get label and icon
    Text := '';
    ImageIndex := -1;
    case Column of
      COLUMN_ORIGINAL:
      begin
        if Item.Definition.HasPluralForms then
          Text := ZStrings.Split(Item.Definition.Value, '|')
        else
          Text := Item.Definition.Value;
        Text := StringReplace(Text, #13#10, ' ', [rfReplaceAll]);
      end;
      COLUMN_COMMENT:
        if Item.Definition.Comment <> '' then
          ImageIndex := ColumnByID[COLUMN_COMMENT].ImageIndex;
      COLUMN_TRANSLATED:
        if Assigned(Item.Translation) then
        begin
          if Item.Definition.HasPluralForms then
            Text := ZStrings.Split(Item.Translation.Value, '|')
          else
            Text := Item.Translation.Value;
          Text := StringReplace(Text, #13#10, ' ', [rfReplaceAll]);
        end;
      COLUMN_STATE:
        if Assigned(Item.Translation) then
        begin
          Text := StateNames[Item.Translation.State];
          ImageIndex := Ord(Item.Translation.State);
        end;
      COLUMN_NOTE:
        if Assigned(Item.Translation) and (Item.Translation.Note <> '') then
          ImageIndex := ColumnByID[COLUMN_NOTE].ImageIndex;
      COLUMN_NAME:
        if Item.Definition.Dictionary.Kind = dkProperty then
          Text := Item.Definition.ID;
      COLUMN_PLURALS:
        if Item.Definition.HasPluralForms then
          ImageIndex := ColumnByID[COLUMN_PLURALS].ImageIndex;
    end;
    // determine text drawing flags
    DrawFlags := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_WORD_ELLIPSIS;
    if not (Column in [COLUMN_ORIGINAL, COLUMN_TRANSLATED]) then
      DrawFlags := DrawFlags or DT_NOPREFIX;
    case Column of
      COLUMN_ORIGINAL, COLUMN_COMMENT:
        if Assigned(NativeCulture) then
          DrawFlags := NativeCulture.DrawTextBiDiModeFlags(DrawFlags);
      COLUMN_TRANSLATED, COLUMN_NOTE:
        if Assigned(TargetCulture) then
          DrawFlags := TargetCulture.DrawTextBiDiModeFlags(DrawFlags);
      COLUMN_STATE:
        DrawFlags := Definitions.DrawTextBiDiModeFlags(DrawFlags);
    end;
    InflateRect(Rect, -4, 0);
    // draw icon
    if ImageIndex >= 0 then
    begin
      if (DrawFlags and DT_RIGHT) = DT_RIGHT then
      begin
        X := Rect.Right - Definitions.SmallImages.Width;
        Rect.Right := X - 4;
      end
      else
      begin
        X := Rect.Left;
        Rect.Left := X + Definitions.SmallImages.Width + 4;
      end;
      Y := (Rect.Top + Rect.Bottom - Definitions.SmallImages.Height) div 2;
      if Rect.Right >= Rect.Left then
        Definitions.SmallImages.Draw(Canvas, X, Y, ImageIndex);
    end;
    // draw label
   if (Length(Text) <> 0) and (Rect.Right > Rect.Left) then
     DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, DrawFlags);
  end;

var
  Column: Integer;
begin
  if not (Item.LVItem.Selected or Item.IsApproved) then
    SetTextColor(Canvas.Handle, ColorToRGB(clGrayText));
  for Column := 0 to Definitions.Columns.Count - 1 do
    DrawColumn(Definitions.Columns[Column].Tag);
end;

procedure TTranslationEditor.DrawListBackground(Canvas: TCanvas;
  var Rect: TRect; ListItem: TListItem; Color: TColor);
var
  Theme: HTHEME;
  StateId: Integer;
  ForeColor: Cardinal;
begin
  if (Win32MajorVersion > 5) and ThemeControl(Definitions) and ListItem.Selected then
  begin
    if Definitions.Focused then
      StateId := LIS_SELECTED
    else
      StateId := LIS_SELECTEDNOTFOCUS;
    ForeColor := ColorToRGB(Definitions.Font.Color);
    Theme := OpenThemeData(Definitions.Handle, 'listview');
    try
      DrawThemeBackground(Theme, Canvas.Handle, LVP_LISTITEM, StateId, Rect, @Rect);
      GetThemeColor(Theme, LVP_LISTITEM, StateId, TMT_TEXTCOLOR, ForeColor);
    finally
      CloseThemeData(Theme);
    end;
    Canvas.Font.Color := ForeColor;
  end
  else
  begin
    if ListItem.Selected then
    begin
      if Definitions.Focused then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end
      else
      begin
        Canvas.Brush.Color := clBtnFace;
        Canvas.Font.Color := clBtnText;
      end;
    end
    else
      Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);
  end;
  SetBkMode(Canvas.Handle, TRANSPARENT);
  InflateRect(Rect, -1, -1);
end;

procedure TTranslationEditor.ReloadDefinitions;
begin
  if Assigned(CurrentDomain) then
    List.Load(CurrentDomain)
  else
    List.Load(Catalog.TextDomains);
end;

procedure TTranslationEditor.ReloadDomains;
var
  TextDomain: TTextDomain;
  I: Integer;
begin
  Definitions.Items.Clear;
  Domains.Items.BeginUpdate;
  try
    for I := Domains.Items.Count - 1 downto 1 do
      Domains.Items.Delete(I);
    TextDomain := Catalog.TextDomains.First;
    while Assigned(TextDomain) do
    begin
      Domains.Items.AddObject(TextDomain.Name, TextDomain);
      TextDomain := TextDomain.Next;
    end;
  finally
    Domains.Items.EndUpdate;
  end;
  Domains.ItemIndex := 0;
  ReloadDefinitions;
end;

procedure TTranslationEditor.ReloadCurrentItemDetails;
var
  ApprovedTranslation: TTextTranslation;
  SourceCulture: TCultureInfo;
  SourceText: String;
begin
  DefinitionSelectionTimer.Enabled := False;
  if CurrentItem = EditingItem then
    Exit;
  fEditingItem := CurrentItem;
  if Assigned(EditingItem) then
  begin
    SourceText := '';
    SourceCulture := SourceCultureLabel.Culture;
    if not Assigned(SourceCulture) then
      SourceCulture := NativeCulture;
    if SourceCulture <> NativeCulture then
    begin
      ApprovedTranslation := EditingItem.Definition.Translations.Find(SourceCulture.Locale);
      if not Assigned(ApprovedTranslation) or (ApprovedTranslation.State <> tsUser) then
        SourceCulture := NativeCulture
      else if EditingItem.Definition.HasPluralForms then
        SourceText := ApprovedTranslation.Plurals[OriginalPlurals.TabIndex]
      else
        SourceText := ApprovedTranslation.Value;
    end;
    if SourceCulture <> SourceCultureLabel.Culture then
    begin
      SourceCultureLabel.Culture := NativeCulture;
      PreparePluralChoices(OriginalPlurals, NativeCulture);
    end;
    if SourceCulture = NativeCulture then
    begin
      if EditingItem.Definition.HasPluralForms then
        SourceText := EditingItem.Definition.Plurals[OriginalPlurals.TabIndex]
      else
        SourceText := EditingItem.Definition.Value;
    end;
    OriginalText.Text := SourceText;
    OriginalText.BiDiMode := SourceCulture.BiDiMode;
    OriginalPlurals.Visible := EditingItem.Definition.HasPluralForms;
    if EditingItem.Definition.Comment <> '' then
    begin
      DeveloperComment.Caption := EditingItem.Definition.Comment;
      DeveloperComment.Visible := True;
    end
    else
      DeveloperComment.Visible := False;
    if Assigned(EditingItem.Translation) then
    begin
      TargetEditingText.HasPluralForms := EditingItem.Definition.HasPluralForms;
      TargetEditingText.Text := EditingItem.Translation.Value;
      TranslatedText.ReadOnly := Readonly;
      TranslatedText.Modified := False;
      TranslationPlurals.Visible := EditingItem.Definition.HasPluralForms;
      if EditingItem.Translation.Note <> '' then
      begin
        TranslatorNote.Caption := EditingItem.Translation.Note;
        TranslatorNote.Visible := True;
      end
      else
        TranslatorNote.Visible := False;
    end
    else
    begin
      TargetEditingText.Clear;
      TranslatedText.ReadOnly := True;
      TranslatorNote.Visible := False;
      TranslationPlurals.Visible := False;
    end;
  end
  else
  begin
    OriginalText.Clear;
    OriginalPlurals.Visible := False;
    DeveloperComment.Visible := False;
    TargetEditingText.Clear;
    TranslatedText.ReadOnly := True;
    TranslationPlurals.Visible := False;
    TranslatorNote.Visible := False;
  end;
end;

procedure TTranslationEditor.RebuildLanguageActions;
var
  Action: TAction;
  I: Integer;
begin
  for I := 0 to Catalog.Cultures.Count - 1 do
  begin
    if I < LanguageActions.ActionCount then
      Action := TAction(LanguageActions.Actions[I])
    else
    begin
      Action := TAction.Create(Self);
      Action.ActionList := LanguageActions;
    end;
    Action.Caption := Catalog.Cultures[I].LocalizedDisplayName;
    Action.Tag := Catalog.Cultures[I].LocaleID;
    Action.Checked := (Catalog.Cultures[I] = TargetCulture);
    Action.GroupIndex := 5;
    Action.OnExecute := TargetLanguageActionExecute;
    Action.OnUpdate := TargetLanguageActionUpdate;
  end;
  for I := LanguageActions.ActionCount - 1 downto Catalog.Cultures.Count do
    LanguageActions.Actions[I].Free;
  MainForm.EditorChanged(Self, CHANGED_TARGET_LANGUAGES);
end;

procedure TTranslationEditor.AutoTranslateUsingItem(Item: TItemNode);
var
  AffectedTranslations: TList;
begin
  AffectedTranslations := TList.Create;
  try
    if Catalog.UseTranslation(Item.Definition.Value,
      Item.Translation.Value, TargetCulture, AffectedTranslations) <> 0
    then
      UpdateTranslations(AffectedTranslations);
  finally
    AffectedTranslations.Free;
  end;
end;

function TTranslationEditor.UpdateItem(Item: TItemNode;
  NewValue: String; NewState: TTranslationState;
  ValidatePlurals: Boolean): Boolean;

begin
  if ValidatePlurals and Item.Definition.HasPluralForms then
    ValidateNumOfPlurals(NewValue, TranslationPlurals.Tabs.Count);
  if NewValue <> Item.Translation.Value then
  begin
    if Item = EditingItem then
      TargetEditingText.Text := NewValue;
    Item.Translation.Value := NewValue;
    if not UpdateItemState(Item, NewState) then
    begin
      Item.Update;
      if NewState = tsUser then
        AutoTranslateUsingItem(Item);
    end;
    Modified := True;
    Result := True;
  end
  else
    Result := UpdateItemState(Item, NewState);
end;

function TTranslationEditor.UpdateItemState(Item: TItemNode;
  NewState: TTranslationState): Boolean;
begin
  Result := False;
  if NewState <> Item.Translation.State then
  begin
    Dec(AllStats.SubTotal[Item.Translation.State]);
    Item.Translation.State := NewState;
    Inc(AllStats.SubTotal[Item.Translation.State]);
    Item.Update;
    Modified := True;
    UpdateProgress(False);
    if NewState = tsUser then
      AutoTranslateUsingItem(Item);
    Result := True;
  end;
end;

function TTranslationEditor.UpdateItemNote(Item: TItemNode;
  const NewNote: String): Boolean;
begin
  Result := False;
  if NewNote <> Item.Translation.Note then
  begin
    Item.Translation.Note := NewNote;
    Item.Update;
    Modified := True;
    if Item = EditingItem then
    begin
      if NewNote <> '' then
      begin
        TranslatorNote.Caption := NewNote;
        TranslatorNote.Visible := True;
      end
      else
        TranslatorNote.Visible := False;
    end;
    Result := True;
  end;
end;

procedure TTranslationEditor.UpdateTranslations(Translations: TList);
begin
  if Translations.Count <> 0 then
  begin
    List.UpdateBy(Translations);
    UpdateProgress(True);
  end;
end;

procedure TTranslationEditor.UpdateProgress(Recalc: Boolean);
var
  NewProgress: Double;
begin
  if Recalc then
    AllStats := Catalog.StatsOf(TargetCulture);
  if Assigned(TargetCulture) then
    NewProgress := AllStats.Progress
  else
    NewProgress := -1;
  if Progress <> NewProgress then
  begin
    fProgress := NewProgress;
    MainForm.EditorChanged(Self, CHANGED_PROGRESS);
  end;
  Domains.Invalidate;
end;

procedure TTranslationEditor.PrepareStateNames;
begin { caching for quick access }
  StateNames[tsNone] := Translator.GetText(SNotTranslated);
  StateNames[tsFuzzy] := Translator.GetText(SFuzzyTranslated);
  StateNames[tsGoogle] := Translator.GetText(SGoogleTranslated);
  StateNames[tsAuto] := Translator.GetText(SAutoTranslated);
  StateNames[tsUser] := Translator.GetText(SUserTranslated);
end;

procedure TTranslationEditor.PreparePluralChoices(Control: TTabSet;
  Culture: TCultureInfo);
var
  NumOfPlurals: Integer;
begin
  NumOfPlurals := TPluralForms.ExtractNumOfPlurals(Catalog.PluralRuleOf(Culture));
  if Control.Tabs.Count <> NumOfPlurals then
    AddPluralChoices(Control.Tabs, NumOfPlurals);
end;

procedure TTranslationEditor.ValidateNumOfPlurals(var PluralForms: String;
  NumOfPlurals: Integer);
var
  Count: Integer;
begin
  Count := ZStrings.Count(PluralForms);
  if Count > NumOfPlurals then
  begin
    if NumOfPlurals = 1 then // use the last one
      PluralForms := ZStrings.GetSubStrAt(PluralForms, Count - 1)
    else                     // truncate extra ones
      repeat
        Dec(Count);
        ZStrings.Delete(PluralForms, Count);
      until Count = NumOfPlurals;
  end;
end;

procedure TTranslationEditor.TargetLanguageActionExecute(Sender: TObject);
begin
  TargetCulture := CultureOf(TAction(Sender).Tag);
end;

procedure TTranslationEditor.TargetLanguageActionUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := Assigned(TargetCulture) and
    (TargetCulture.LocaleID = LCID(TAction(Sender).Tag));
end;

procedure TTranslationEditor.SettingsChanged;
begin
  List.SortImmediately := DM.SortImmediately;
end;

function TTranslationEditor.CanDiscardChanges: Boolean;
var
  MsgText: String;
begin
  CommitEditingItem;
  if Modified then
  begin
    MsgText := DM.Localizer.FormatCS(Translator.GetText(SSaveChangesConfirm), [FileName]);
    case DM.MsgDlg.YesNoCancel(MsgText) of
      mrYes: Perform(eaSave);
      mrNo: Modified := False;
    end;
  end;
  Result := not Modified;
end;

procedure TTranslationEditor.LoadPersistentUserChoices;
var
  R: TRegistry;
  FileID: String;
  Locale: String;
begin
  if (Catalog.Cultures.Count <> 0) and (FileName <> '') then
  begin
    Locale := '';
    R := TRegistry.Create(KEY_QUERY_VALUE);
    try
      FileID := MD5(FileName);
      if R.OpenKeyReadOnly(AppRegRootKey) then
      begin
        if R.ValueExists(FileID) then
          Locale := R.ReadString(FileID);
        R.CloseKey;
      end;
    finally
      R.Free;
    end;
    if (Locale <> '') and Catalog.Cultures.Exists(Locale) then
      TargetCulture := CultureOf(Locale)
    else if Catalog.Cultures.Exists(GetUserDefaultUICulture) then
      TargetCulture := GetUserDefaultUICulture
    else if Catalog.Cultures.Exists(GetSystemDefaultCulture) then
      TargetCulture := GetSystemDefaultCulture
    else
      TargetCulture := Catalog.Cultures[0];
  end;
end;

procedure TTranslationEditor.SavePersistentUserChoices;
var
  R: TRegistry;
  FileID: String;
begin
  if Assigned(TargetCulture) and (FileName <> '') then
  begin
    R := TRegistry.Create;
    try
      FileID := MD5(FileName);
      if R.OpenKey(AppRegRootKey, True) then
      begin
        R.WriteString(FileID, TargetCulture.Locale);
        R.CloseKey;
      end;
    finally
      R.Free;
    end;
  end;
end;

function TTranslationEditor.Apply(Node: TListNode; Action: TEditorBulkAction): Boolean;
var
  Answer: Boolean;
begin
  if Node is TGroupNode then
  begin
    Result := False;
    Answer := (DM.BulkActions <> 0);
    if DM.BulkActions < 0 then
    begin
      DM.MsgDlg.CheckBox.Checked := False;
      DM.MsgDlg.CheckBox.Visible := True;
      try
        Answer := DM.MsgDlg.Confirm(Translator.GetText(SBulkActionConfirm));
      finally
        DM.MsgDlg.CheckBox.Visible := False;
      end;
      if DM.MsgDlg.CheckBox.Checked then
        DM.BulkActions := Ord(Answer);
    end;
    if Answer then
    begin
      ProgressBar.Position := 0;
      ProgressBar.Max := TGroupNode(Node).Count;
      ProgressBar.State := pbsNormal;
      MainForm.ActionManager.State := TActionListState.asSuspended;
      List.SortImmediately := False;
      try
        Application.ProcessMessages;
        try
          Result := Action(Node);
        except
          // ignore exceptions
        end;
      finally
        ProgressBar.Visible := False;
        MainForm.ActionManager.State := TActionListState.asNormal;
        List.SortImmediately := DM.SortImmediately;
      end;
    end;
  end
  else
    Result := Action(Node);
end;

function TTranslationEditor.UpdateBulkProgress(Item: TItemNode): Boolean;
begin
  ProgressBar.Visible := True;
  Application.ProcessMessages;
  if GetAsyncKeyState(VK_ESCAPE) = 1 then
  begin
    Item.Select;
    Result := False;
  end
  else
  begin
    if Win32MajorVersion > 5 then
    begin // workaround update delay on aero
      ProgressBar.StepBy(2);
      ProgressBar.StepBy(-1);
    end
    else
      ProgressBar.StepBy(1);
    Result := True;
  end;
end;

function TTranslationEditor.DoSave: Boolean;
var
  MsgText: String;
begin
  try
    Screen.Cursor := crHourGlass;
    try
      Catalog.SaveToFile(FileName);
    finally
      Screen.Cursor := crDefault;
    end;
    Modified := False;
    Result := True;
  except
    if not DirectoryExists(ExtractFilePath(FileName)) then
      MsgText := DM.Localizer.FormatCS(Translator.GetText(SSavePathNotFoundError), [ExtractFilePath(FileName)])
    else
      MsgText := DM.Localizer.FormatCS(Translator.GetText(SSaveFileError), [FileName]);
    DM.MsgDlg.ShowError(MsgText);
    Result := False;
  end;
end;

function TTranslationEditor.DoSaveAs: Boolean;
begin
  Result := False;
  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
    if DoSave then
      Result := True
    else
      Modified := True;
  end;
end;

function TTranslationEditor.DoImport: Boolean;
begin
  Result := False;
  if TImportWizardDialog.Execute(Catalog) then
  begin
    if not Assigned(TargetCulture) and (Catalog.Cultures.Count <> 0) then
      TargetCulture := Catalog.Cultures[0];
    RebuildLanguageActions;
    ReloadDomains;
    UpdateProgress(True);
    Modified := True;
    Result := True;
    PostMessage(Handle, CM_ADJUST_LIST_COLUMNS, 0, 0);
  end;
end;

function TTranslationEditor.DoExport: Boolean;
begin
  Result := TExportWizardDialog.Execute(Catalog);
end;

function TTranslationEditor.DoSetNativeLanguage: Boolean;
var
  Culture: TCultureInfo;
begin
  Result := False;
  if Catalog.Cultures.Count = 1 then
    Culture := Catalog.Cultures[0]
  else
    Culture := GetUserDefaultUICulture;
  if TSelectLanguageDialog.Execute(Translator.GetText(SSetBaseLanguage), Culture, World.Cultures) then
  begin
    NativeCulture := Culture;
    Modified := True;
    Result := True;
  end;
end;

function TTranslationEditor.DoAddLanguage: Boolean;
var
  Culture: TCultureInfo;
begin
  Result := False;
  Culture := GetUserDefaultUICulture;
  if TSelectLanguageDialog.Execute(Translator.GetText(SAddLanguage), Culture, World.Cultures, Catalog.Cultures) then
  begin
    Catalog.Add(Culture);
    TargetCulture := Culture;
    RebuildLanguageActions;
    Modified := True;
    Result := True;
  end;
end;

function TTranslationEditor.DoDeleteLanguage: Boolean;
var
  Culture: TCultureInfo;
  MsgText: String;
begin
  Result := False;
  Culture := TargetCulture;
  MsgText := DM.Localizer.FormatCS(Translator.GetText(SDelLanguageConfirm), [Culture.LocalizedDisplayName]);
  if TSelectLanguageDialog.Execute(Translator.GetText(SDelLanguage), Culture, Catalog.Cultures) and
     DM.MsgDlg.Confirm(MsgText, mbNo) then
  begin
    Catalog.Remove(Culture);
    if Culture = TargetCulture then
    begin
      if Catalog.Cultures.Count <> 0 then
        TargetCulture := Catalog.Cultures[0]
      else
        TargetCulture := nil;
    end;
    RebuildLanguageActions;
    Modified := True;
    Result := True;
  end;
end;

function TTranslationEditor.DoEditLanguage: Boolean;
var
  PluralRule: String;
begin
  Result := False;
  if Assigned(TargetCulture) then
  begin
    PluralRule := Catalog.PluralRuleOf(TargetCulture);
    if TPluralRuleEditorDialog.Execute(TargetCulture, PluralRule) then
    begin
      Catalog.ChangePluralRuleOf(TargetCulture, PluralRule);
      PreparePluralChoices(TranslationPlurals, TargetCulture);
      TargetEditingText.PluralIndex := TranslationPlurals.TabIndex;
      Modified := True;
      Result := True;
    end;
  end;
end;

function TTranslationEditor.DoSwitchLanguage: Boolean;
var
  Index: Integer;
begin
  Result := False;
  if Catalog.Cultures.Count > 1 then
  begin
    Index := (Catalog.Cultures.IndexOf(TargetCulture) + 1) mod Catalog.Cultures.Count;
    TargetCulture := Catalog.Cultures[Index];
    Result := True;
  end;
end;

function TTranslationEditor.DoFind: Boolean;
var
  UnavailableSearchFields: TSearchFields;
begin
  if not Assigned(TargetCulture) then
    UnavailableSearchFields := [sfTranslated, sfNote]
  else
    UnavailableSearchFields := [];
  if TSearchDialog.Execute(smFind, SearchDetails.Params, UnavailableSearchFields, not Readonly) then
    Result := DoSearch(True)
  else
    Result := False;
end;

function TTranslationEditor.DoReplace: Boolean;
var
  UnavailableSearchFields: TSearchFields;
begin
  if not Assigned(TargetCulture) then
    UnavailableSearchFields := [sfTranslated, sfNote]
  else
    UnavailableSearchFields := [];
  if TSearchDialog.Execute(smReplace, SearchDetails.Params, UnavailableSearchFields, not Readonly) then
    Result := DoSearch(True)
  else
    Result := False;
end;

function TTranslationEditor.DoSearch(IsNew: Boolean): Boolean;

  function FindPos(const Str: String; LastPos: Integer = 1): Integer;
  var
    I: Integer;
  begin
    with SearchDetails.Params do
      repeat
        if stMatchCase in Options.Types then
          Result := PosEx(Target, Str, LastPos)
        else
          Result := PosEx(UpperCase(Target), UpperCase(Str), LastPos);
        if (Result <> 0) and (stWholeWord in Options.Types) then
        begin
          LastPos := Result + 1;
          I := Result - 1;
          if (I > 0) and IsCharAlphaNumeric(Str[I]) then
            Result := 0
          else
          begin
            I := Result + Length(Target);
            if (I < Length(Str)) and IsCharAlphaNumeric(Str[I]) then
              Result := 0;
          end
        end
        else
          Exit;
      until Result <> 0;
  end;

  procedure InitializeSearch;
  begin
    with SearchDetails, Definitions do
    begin
      if (Params.Options.Origin = soEntireScope) or (ItemIndex < 0) then
        case Params.Options.Direction of
          sdForward: CurrentIndex := 0;
          sdBackward: CurrentIndex := Items.Count - 1;
        end
      else
      begin
        CurrentIndex := ItemIndex;
        case Params.Options.Direction of
          sdForward:
          begin
            Inc(CurrentIndex);
            if CurrentIndex >= Items.Count then
              CurrentIndex := 0;
          end;
          sdBackward:
          begin
            Dec(CurrentIndex);
            if CurrentIndex < 0 then
              CurrentIndex := Items.Count - 1;
          end;
        end
      end;
      StopIndex := CurrentIndex;
      Params.Options.Origin := soCurrent;
      Active := True;
    end;
  end;

  function AdvanceCurrentIndex: Boolean;
  begin
    with SearchDetails, Definitions do
    begin
      Result := True;
      case Params.Options.Direction of
        sdForward:
        begin
          Inc(CurrentIndex);
          if CurrentIndex >= Items.Count then
          begin
            CurrentIndex := 0;
            if CurrentIndex <> StopIndex then
              Result := IsNew or DM.MsgDlg.Confirm(Translator.GetText(SSearchRestartFirstConfirm));
          end;
        end;
        sdBackward:
        begin
          Dec(CurrentIndex);
          if CurrentIndex < 0 then
          begin
            CurrentIndex := Items.Count - 1;
            if CurrentIndex <> StopIndex then
              Result := IsNew or DM.MsgDlg.Confirm(Translator.GetText(SSearchRestartLastConfirm));
          end;
        end;
      end;
      if CurrentIndex = StopIndex then
        Result := False;
    end;
  end;

  function IsMatched(const Str: String): Boolean; inline;
  begin
    Result := (FindPos(Str) <> 0);
  end;

  function FindInItem(Item: TItemNode): Boolean;
  begin
    with SearchDetails.Params.Options, Item do
    begin
      if ((sfOriginal in Fields) and IsMatched(Definition.Value)) or
         ((sfName in Fields) and IsMatched(Definition.ID)) or
         ((sfComment in Fields) and IsMatched(Definition.Comment))
      then
        Result := True
      else if Assigned(Translation) then
        Result := ((sfTranslated in Fields) and IsMatched(Item.Translation.Value))
               or ((sfNote in SearchDetails.Params.Options.Fields))
      else
        Result := False;
    end;
  end;

  procedure FindAndReplaceInItem(Item: TItemNode; var UserResponse: Integer);
  var
    P: Integer;
    MsgText: String;
  begin
    if not Assigned(Item.Translation) then
      Exit;
    P := FindPos(Item.Translation.Value);
    if P <> 0 then
    begin
      Item.Select;
      ReloadCurrentItemDetails;
      repeat
        TranslatedText.SetFocus;
        TargetEditingText.Select(P, Length(SearchDetails.Params.Target));
        TranslationPlurals.TabIndex := TargetEditingText.PluralIndex;
        if UserResponse <> mrAll then
        begin
          MsgText := DM.Localizer.FormatCS(Translator.GetText(SSearchReplaceConfirm),
            [SearchDetails.Params.Target, SearchDetails.Params.Replace]);
          UserResponse := DM.MsgDlg.Show(MsgText, mtConfirmation, [mbYes, mbNo, mbAll, mbCancel], mbYes);
        end;
        if UserResponse in [mrYes, mrAll] then
        begin
          TargetEditingText.ReplaceSelection(SearchDetails.Params.Replace);
          UpdateItem(Item, TargetEditingText.Text, tsFuzzy, False);
        end
        else if UserResponse = mrCancel then
          Exit;
        P := FindPos(Item.Translation.Value, P + 1);
      until P = 0;
    end;
  end;

var
  Item: TListNode;
  MsgText: String;
  UserResponse: Integer;
begin
  if not (IsNew or SearchDetails.Active) or (Definitions.Items.Count = 0) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  InitializeSearch;
  UserResponse := mrNone;
  repeat
    Item := TListNode(Definitions.Items[SearchDetails.CurrentIndex].Data);
    if Item is TItemNode then
    begin
      if SearchDetails.Params.Mode = smReplace then
      begin
        FindAndReplaceInItem(TItemNode(Item), UserResponse);
        if UserResponse = mrCancel then
          Exit;
      end
      else if FindInItem(TItemNode(Item)) then
      begin
        Item.Select;
        Definitions.SetFocus;
        Exit;
      end;
    end;
  until not AdvanceCurrentIndex;
  if (UserResponse <> mrAll) and
     (SearchDetails.CurrentIndex = SearchDetails.StopIndex) then
  begin
    MsgText := DM.Localizer.FormatCS(Translator.GetText(SSearchNotFound), [SearchDetails.Params.Target]);
    DM.MsgDlg.Inform(MsgText);
  end;
end;

function TTranslationEditor.DoNavigateItem(Backward: Boolean): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if List.Count <> 0 then
  begin
    Index := Definitions.ItemIndex;
    repeat
      if Backward then
        Dec(Index)
      else
        Inc(Index);
      if Index < 0 then
        Index := Definitions.Items.Count - 1
      else if Index >= Definitions.Items.Count then
        Index := 0;
    until List.NoteAt(Index) is TItemNode;
    if Index <> Definitions.ItemIndex then
    begin
      CommitEditingItem;
      with Definitions.Items[Index] do
      begin
        Selected := True;
        Focused := True;
        MakeVisible(False);
      end;
      Result := True;
    end;
  end;
end;

function TTranslationEditor.DoNavigateGroup(Backward: Boolean): Boolean;
var
  Index: Integer;
  CurSel: TListNode;
  Group: TGroupNode;
begin
  Result := False;
  if List.SortGrouping and (List.Groups.Count <> 0) then
  begin
    CurSel := CurrentSelection;
    if Assigned(CurSel) then
    begin
      if CurSel is TGroupNode then
        Group := TGroupNode(CurSel)
      else
        Group := TItemNode(CurSel).Group;
      Index := List.Groups.IndexOf(Group);
      if Backward xor List.SortAscending  then
        Inc(Index)
      else
        Dec(Index);
      if Index < 0 then
        Index := List.Groups.Count - 1
      else if Index >= List.Groups.Count then
        Index := 0;
    end
    else
      Index := 0;
    Group := List.Groups[Index];
    Group.Select;
    Result := True;
  end;
end;

function TTranslationEditor.DoNavigateConditional(Backward: Boolean;
  Condition: TTranslationStates): Boolean;
var
  Index: Integer;
  Node: TListNode;
  Item: TItemNode absolute Node;
begin
  Result := False;
  if List.Count <> 0 then
  begin
    Index := Definitions.ItemIndex;
    repeat
      if Backward then
        Dec(Index)
      else
        Inc(Index);
      if Index < 0 then
        Index := Definitions.Items.Count - 1
      else if Index >= Definitions.Items.Count then
        Index := 0;
      Node := List.NoteAt(Index);
      if (Node is TItemNode) and Assigned(Item.Translation) and
         (Item.Translation.State in Condition)
      then
        Break;
    until Index = Definitions.ItemIndex;
    if Index <> Definitions.ItemIndex then
    begin
      CommitEditingItem;
      with Definitions.Items[Index] do
      begin
        Selected := True;
        Focused := True;
        MakeVisible(False);
      end;
      Result := True;
    end;
  end;
end;

function TTranslationEditor.DoDeleteDomain(Domain: TTextDomain): Boolean;
var
  MsgText: String;
begin
  Result := False;
  if Assigned(Domain) then
  begin
    MsgText := DM.Localizer.FormatCS(Translator.GetText(SDelDomainConfirm), [Domain.Name]);
    if DM.MsgDlg.Confirm(MsgText) then
    begin
      Domain.Delete;
      ReloadDomains;
      UpdateProgress(True);
      Modified := True;
      Result := True;
    end;
  end;
end;

function TTranslationEditor.DoDeleteComponent(Item: TItemNode): Boolean;
var
  Domain: TTextDomain;
  MsgText: String;
begin
  Result := False;
  if Assigned(Item) and Item.IsComponent then
  begin
    Domain := Item.TextDomain;
    MsgText := DM.Localizer.FormatCS(Translator.GetText(SDelComponentConfirm), [Item.ComponentName]);
    if DM.MsgDlg.Confirm(MsgText) and Item.TextDomain.DeleteComponent(Item.ComponentName) then
    begin
      if Domain.Empty then
      begin
        Domain.Delete;
        ReloadDomains;
      end
      else
        ReloadDefinitions;
      UpdateProgress(True);
      Modified := True;
      Result := True;
    end;
  end;
end;

function TTranslationEditor.DoDeleteItem(Item: TItemNode): Boolean;
var
  Domain: TTextDomain;
begin
  Result := False;
  if Assigned(Item) and DM.MsgDlg.Confirm(Translator.GetText(SDelItemConfirm)) then
  begin
    Dec(AllStats.Total);
    Dec(AllStats.SubTotal[Item.Translation.State]);
    Domain := Item.TextDomain;
    Item.Definition.Delete;
    if Domain.Empty then
    begin
      Domain.Delete;
      ReloadDomains;
    end
    else
      Item.Delete;
    UpdateProgress(False);
    Modified := True;
    Result := True;
  end;
end;

function TTranslationEditor.DoDeleteGroupItems(Group: TGroupNode): Boolean;
var
  Domain: TTextDomain;
  NeedsDomainReload: Boolean;
  Item: TItemNode;
  I: Integer;
begin
  Result := False;
  if Assigned(Group) and DM.MsgDlg.Confirm(Translator.GetText(SDelGroupConfirm)) then
  begin
    NeedsDomainReload := False;
    Dec(AllStats.Total, Group.Count);
    for I := 0 to Group.Count - 1 do
    begin
      Item := Group.Items[I];
      Dec(AllStats.SubTotal[Item.Translation.State]);
      Domain := Item.TextDomain;
      Item.Definition.Delete;
      if Domain.Empty then
      begin
        Domain.Delete;
        NeedsDomainReload := True;
      end;
    end;
    if NeedsDomainReload then
      ReloadDomains
    else
      Group.Delete;
    UpdateProgress(False);
    Modified := True;
    Result := True;
  end;
end;

function TTranslationEditor.DoRenameDomain(Domain: TTextDomain): Boolean;
var
  NewDomain: TTextDomain;
  DomainName: String;
  Index, ItemIndex: Integer;
begin
  Result := False;
  if Assigned(Domain) then
  begin
    DomainName := Domain.Name;
    if TRenameDialog.Execute(Translator.GetText(SRenameDomain), DomainName, Domains.Items) then
    begin
      NewDomain := Domain.Rename(DomainName);
      if Assigned(NewDomain) then
      begin
        ItemIndex := Domains.ItemIndex;
        Index := Domains.Items.IndexOfObject(Domain);
        Domains.Items.Strings[Index] := NewDomain.Name;
        Domains.Items.Objects[Index] := NewDomain;
        Domains.ItemIndex := ItemIndex;
        if (NewDomain <> Domain) and (Index = ItemIndex) then
          ReloadDefinitions;
        Modified := True;
        Result := True;
      end;
    end;
  end;
end;

function TTranslationEditor.DoRenameComponent(Item: TItemNode): Boolean;
var
  ComponentNames: TStrings;
  ComponentName, OldComponentName: String;
  Domain: TTextDomain;
  TopIndex, ItemIndex: Integer;
begin
  Result := False;
  if Assigned(Item) and Item.IsComponent then
  begin
    ComponentNames := TStringList.Create;
    try
      Domain := Item.TextDomain;
      Domain.GetComponents(ComponentNames);
      ComponentName := Item.ComponentName;
      OldComponentName := ComponentName;
      if TRenameDialog.Execute(Translator.GetText(SRenameComponent), ComponentName, ComponentNames) and
         Item.TextDomain.RenameComponent(OldComponentName, ComponentName) then
      begin
        if (Domain = CurrentDomain) or not Assigned(CurrentDomain) then
        begin
          TopIndex := Definitions.TopItem.Index;
          ItemIndex := Definitions.ItemIndex;
          ReloadDefinitions;
          Definitions.ItemIndex := ItemIndex;
          Definitions.Items[TopIndex].MakeVisible(False);
          Definitions.Items[ItemIndex].MakeVisible(False);
          Definitions.Items[ItemIndex].Focused := True;
        end;
        Modified := True;
        Result := True;
      end;
    finally
      ComponentNames.Free;
    end;
  end;
end;

function TTranslationEditor.DoEditTranslatorNote(Node: TListNode): Boolean;
var
  TheNote: String;
  Group: TGroupNode absolute Node;
  Item: TItemNode;
begin
  Result := False;
  if Node is TItemNode then
    TheNote := TItemNode(Node).Translation.Note
  else
    TheNote := Group.First.Translation.Note;
  if TPlainTextEditorDialog.Execute(Translator.GetText(SEditNote), TheNote, TargetCulture) then
  begin
    if Node is TItemNode then
      Result := UpdateItemNote(TItemNode(Node), TheNote)
    else
    begin
      for Item in Group do
      begin
        if not UpdateBulkProgress(Item) then
          Exit;
        if UpdateItemNote(Item, TheNote) then
          Result := True;
      end;
    end;
  end;
end;

function TTranslationEditor.DoClearTranslatorNote(Node: TListNode): Boolean;
var
  Group: TGroupNode absolute Node;
  Item: TItemNode;
begin
  if Node is TItemNode then
    Result := UpdateItemNote(TItemNode(Node), '')
  else
  begin
    Result := False;
    for Item in Group do
    begin
      if not UpdateBulkProgress(Item) then
        Exit;
      if UpdateItemNote(Item, '') then
        Result := True;
    end;
  end;
end;

function TTranslationEditor.DoAcceptTranslation(Node: TListNode): Boolean;
var
  Group: TGroupNode absolute Node;
  Item: TItemNode;
begin
  if Node is TItemNode then
    Result := UpdateItemState(TItemNode(Node), tsUser)
  else
  begin
    Result := False;
    for Item in Group do
    begin
      if not UpdateBulkProgress(Item) then
        Exit;
      if UpdateItemState(Item, tsUser) then
        Result := True;
    end;
  end;
end;

function TTranslationEditor.DoRejectTranslation(Node: TListNode): Boolean;
var
  Group: TGroupNode absolute Node;
  Item: TItemNode;
begin
  if Node is TItemNode then
    Result := UpdateItemState(TItemNode(Node), tsFuzzy)
  else
  begin
    Result := False;
    for Item in Group do
    begin
      if not UpdateBulkProgress(Item) then
        Exit;
      if UpdateItemState(Item, tsFuzzy) then
        Result := True;
    end;
  end;
end;

function TTranslationEditor.DoCopyOriginalText(Node: TListNode): Boolean;
var
  Group: TGroupNode absolute Node;
  Item: TItemNode;
begin
  if Node is TItemNode then
  begin
    Item := TItemNode(Node);
    Result := UpdateItem(Item, Item.Definition.Value, tsFuzzy, True)
  end
  else
  begin
    Result := False;
    for Item in Group do
    begin
      if not UpdateBulkProgress(Item) then
        Exit;
      if UpdateItem(Item, Item.Definition.Value, tsFuzzy, True) then
        Result := True;
    end;
  end;
end;

function TTranslationEditor.DoGoogleTranslate(Node: TListNode): Boolean;
var
  TextToTranslate: String;
  GoogleResult: String;
  GoogleResultCode: TGoogleResultCode;
  Group: TGroupNode absolute Node;
  Item: TItemNode;
begin
  if Node is TItemNode then
  begin
    Item := TItemNode(Node);
    TextToTranslate := Item.Definition.Value;
    if Item.Definition.HasPluralForms then
      TextToTranslate := ZStrings.Split(TextToTranslate, UCC_US);
    Screen.Cursor := crHourGlass;
    try
      GoogleResultCode := GoogleTranslator.Translate(TextToTranslate, GoogleResult);
    finally
      Screen.Cursor := crDefault;
    end;
    if GoogleResultCode = grOK then
    begin
      if Item.Definition.HasPluralForms then
        GoogleResult := ZStrings.Construct(GoogleResult, UCC_US);
      Result := UpdateItem(Item, GoogleResult, tsGoogle, True)
    end
    else
    begin
      ProgressBar.State := pbsError;
      if GoogleResultCode = grGoogleError then
        DM.MsgDlg.ShowWarning(Translator.GetText(SGoogleLanguagePairError))
      else
        DM.MsgDlg.ShowError(Translator.GetText(SGoogleConnectError));
      Result := False;
      Abort;
    end;
  end
  else
  begin
    Result := False;
    for Item in Group do
    begin
      if not UpdateBulkProgress(Item) then
        Exit;
      if not (Item.IsApproved or Item.IsGoogleTranslated) and DoGoogleTranslate(Item) then
        Result := True;
    end;
  end;
end;

function TTranslationEditor.DoAutoTranslate(Node: TListNode): Boolean;
var
  Synonym: String;
  Group: TGroupNode absolute Node;
  Item: TItemNode;
begin
  if Node is TItemNode then
  begin
    Item := TItemNode(Node);
    Screen.Cursor := crHourGlass;
    try
      if DM.Repository.FindSynonym(Item.Definition.Value, NativeCulture, TargetCulture, Synonym) then
        Result := UpdateItem(Item, Synonym, tsAuto, True)
      else
        Result := False;
    finally
      Screen.Cursor := crDefault;
    end;
  end
  else
  begin
    Result := False;
    for Item in Group do
    begin
      if not UpdateBulkProgress(Item) then
        Exit;
      if not Item.IsApproved and DoAutoTranslate(Item) then
        Result := True;
    end;
  end;
end;

function TTranslationEditor.DoSuggestTranslation(Item: TItemNode): Boolean;
var
  Suggestion: String;
begin
  Result := False;
  if Assigned(Item) and Assigned(NativeCulture) and Assigned(TargetCulture) and
     TSuggestionDialog.Execute(Item.Definition, TargetCulture, Suggestion)
  then
    Result := UpdateItem(Item, Suggestion, tsUser, True);
end;

function TTranslationEditor.DoExpandCollapseAll(Expand: Boolean): Boolean;
begin
  if Expand then
    List.ExpandAll
  else
    List.CollapseAll;
  PostMessage(Handle, CM_ADJUST_LIST_COLUMNS, 0, 0);
  Result := True;
end;

constructor TTranslationEditor.CreateNew;
const
  ColMap: TDefinitionFieldColumnMap = (-1, COLUMN_ORIGINAL, COLUMN_TRANSLATED,
    COLUMN_STATE, COLUMN_COMMENT, COLUMN_NOTE, COLUMN_NAME, COLUMN_PLURALS);
begin
  inherited Create(Application);
  SearchDetails.Params.Options.Fields := [sfOriginal, sfTranslated, sfName];
  Catalog := TTranslationCatalog.Create;
  Catalog.Repository := DM.Repository;
  List := TDefinitionList.Create(Definitions, @ColMap);
  List.SortColumn := DM.SortColumn;
  List.SortAscending := DM.SortAscending;
  List.SortGrouping := DM.SortGrouping;
  List.SortImmediately := DM.SortImmediately;
  TargetEditingText := TPluralTextEdit.Create(TranslatedText);
  PrepareStateNames;
end;

constructor TTranslationEditor.Create(const AFileName: String; AReadonly: Boolean);
var
  MsgText: String;
begin
  CreateNew;
  Readonly := AReadonly;
  FileName := AFileName;
  try
    Catalog.LoadFromFile(FileName);
  except
    if not FileExists(FileName) then
      MsgText := DM.Localizer.FormatCS(Translator.GetText(SOpenFileNotFoundError), [FileName])
    else
      MsgText := DM.Localizer.FormatCS(Translator.GetText(SOpenFileError), [FileName]);
    DM.MsgDlg.ShowError(MsgText);
    Abort;
  end;
  Modified := False;
  NativeCulture := Catalog.NativeCulture;
  LoadPersistentUserChoices;
  RebuildLanguageActions;
  ReloadDomains;
  if not Assigned(TargetCulture) then
    UpdateProgress(True);
  if not Catalog.Empty and not Assigned(NativeCulture) then
    PromptForNativeLanguage := True;
end;

destructor TTranslationEditor.Destroy;
begin
  if MainForm.ActiveEditor = Self then
    MainForm.ActiveEditor := nil;
  FreeAndNil(TargetEditingText);
  FreeAndNil(List);
  FreeAndNil(Catalog);
  inherited Destroy;
end;

procedure TTranslationEditor.FormActivate(Sender: TObject);
begin
  MainForm.ActiveEditor := Self;
end;

procedure TTranslationEditor.FormDeactivate(Sender: TObject);
begin
  if MainForm.ActiveEditor = Self then
    MainForm.ActiveEditor := nil;
end;

procedure TTranslationEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TTranslationEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if CanDiscardChanges then
  begin
    DM.RecentFiles.Add(FileName);
    SavePersistentUserChoices;
  end
  else
    CanClose := False;
end;

procedure TTranslationEditor.FormResize(Sender: TObject);
begin
  if LastWindowState = wsMaximized then
    PostMessage(Handle, CM_ADJUST_LIST_COLUMNS, 0, 0);
  LastWindowState := WindowState;
end;

procedure TTranslationEditor.TranslatorAfterTranslate(Sender: TObject);
begin
  if FileName <> '' then
  begin
    if Modified then
      Caption := '*' + FileName
    else
      Caption := FileName;
  end;
  // base text label
  if SourceCultureLabel.Culture = NativeCulture then
    OriginalTextLabel.Caption := Translator.GetText(SOriginal)
  else
    OriginalTextLabel.Caption := Translator.GetText(SApprovedTranslation);
  // state names
  PrepareStateNames;
end;

procedure TTranslationEditor.TranslatorAfterFlipLayout(Sender: TObject);
begin
  PostMessage(Handle, CM_ADJUST_LIST_COLUMNS, 0, 0);
end;

procedure TTranslationEditor.PanelItemDetailsResize(Sender: TObject);
begin
  OriginalDetailsPanel.Width := (PanelItemDetails.ClientWidth - HSplitter.Width) div 2;
end;

procedure TTranslationEditor.OriginalDetailsPanelResize(Sender: TObject);
begin
  if DeveloperComment.Visible then
  begin
    DeveloperComment.AutoSize := False;
    DeveloperComment.AutoSize := True;
  end;
end;

procedure TTranslationEditor.OriginalPluralsChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
var
  Translation: TTextTranslation;
begin
  if SourceCultureLabel.Culture = NativeCulture then
    OriginalText.Text := EditingItem.Definition.Plurals[NewTab]
  else
  begin
    Translation := EditingItem.Definition.Translations[SourceCultureLabel.Culture.Locale];
    OriginalText.Text := Translation.Plurals[NewTab];
  end;
end;

procedure TTranslationEditor.TranslationPluralsChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  TargetEditingText.PluralIndex := NewTab;
end;

procedure TTranslationEditor.TranslationDetailsPanelResize(Sender: TObject);
begin
  if TranslatorNote.Visible then
  begin
    TranslatorNote.AutoSize := False;
    TranslatorNote.AutoSize := True;
  end;
end;

procedure TTranslationEditor.TranslatorNoteDblClick(Sender: TObject);
begin
  if CanPerform(eaEditTranslatorNote) then
    Perform(eaEditTranslatorNote);
end;

procedure TTranslationEditor.DomainsChange(Sender: TObject);
begin
  if Catalog.TextDomains.Count > 1 then
    ReloadDefinitions;
end;

procedure TTranslationEditor.DomainsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Canvas: TCanvas;
  Domain: TTextDomain;
  TotalCount, ApprovedCount: Integer;
  Info, Locale: String;
  C, S: Integer;
  R: TRect;
begin
  Domain := TTextDomain(Domains.Items.Objects[Index]);
  Canvas := Domains.Canvas;
  // fill background
  if Odd(Index) and (([odComboBoxEdit, odSelected] * State) = []) then
    Canvas.Brush.Color := clAlternative;
  Canvas.FillRect(Rect);
  InflateRect(Rect, -2, -1);
  // draw icon and label
  ListImages.Draw(Canvas, Rect.Left,
    (Rect.Top + Rect.Bottom - ListImages.Height) div 2, 8 + Ord(Index <> 0));
  Inc(Rect.Left, ListImages.Width + 4);
  Dec(Rect.Right, 2);
  if Assigned(Domain) then
    Info := Domain.Name
  else
    Info := Domains.Items[0];
  DrawText(Canvas.Handle, PChar(Info), Length(Info), Rect,
    DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
  // draw stats
  if Index = 0 then
  begin
    TotalCount := AllStats.Total;
    ApprovedCount := AllStats.Approved;
  end
  else
  begin
    if Assigned(TargetCulture) then
      Locale := TargetCulture.Locale
    else
      Locale := '';
    with Domain.StatsOf(Locale) do
    begin
      TotalCount := Total;
      ApprovedCount := Approved;
    end;
  end;
  if ([odSelected, odFocused] * State) = [] then
  begin
    if ApprovedCount = TotalCount then
      Canvas.Font.Color := clGreen
    else
      Canvas.Font.Color := clMaroon;
  end;
  Canvas.Font.Style := Canvas.Font.Style - [fsBold];
  if odComboBoxEdit in State then
  begin
    Info := DM.Localizer.FormatNumber('#,##0', ApprovedCount) + ' / '
          + DM.Localizer.FormatNumber('#,##0', TotalCount);
    DrawText(Canvas.Handle, PChar(Info), Length(Info), Rect,
      DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
  end
  else
  begin
    S := Canvas.TextWidth(' ');
    Info := DM.Localizer.FormatNumber('#,##0', AllStats.Total);
    C := Rect.Right - Canvas.TextWidth(Info) - 2 * S;
    SetRect(R, C - S, Rect.Top, C + S, Rect.Bottom);
    DrawText(Canvas.Handle, '/', 1, R,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    SetRect(R, Rect.Left, Rect.Top, C - S - S div 2, Rect.Bottom);
    Info := DM.Localizer.FormatNumber('#,##0', ApprovedCount);
    DrawText(Canvas.Handle, PChar(Info), Length(Info), R,
      DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    SetRect(R, C + S, Rect.Top, Rect.Right, Rect.Bottom);
    Info := DM.Localizer.FormatNumber('#,##0', TotalCount);
    DrawText(Canvas.Handle, PChar(Info), Length(Info), R,
      DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
  end;
end;

procedure TTranslationEditor.SourceCulturePopupPopup(Sender: TObject);

  function AddCulture(Culture: TCultureInfo): TMenuItem;
  begin
    Result := SourceCulturePopup.CreateMenuItem;
    Result.Caption := Culture.LocalizedDisplayName;
    Result.ImageIndex := DM.Flags.ImageIndexOf(Culture);
    Result.Checked := (SourceCultureLabel.Culture = Culture);
    Result.RadioItem := True;
    Result.Tag := Culture.LocaleID;
    Result.OnClick := SourceLocaleChoicesClick;
    SourceCulturePopup.Items.Add(Result);
  end;

var
  Translation: TTextTranslation;
  Culture: TCultureInfo;
begin
  SourceCulturePopup.Items.Clear;
  if Assigned(NativeCulture) then
    AddCulture(NativeCulture).Default := True;
  if Assigned(EditingItem) then
  begin
    Translation := EditingItem.Definition.Translations.First;
    while Assigned(Translation) do
    begin
      if Translation.IsApproved then
      begin
        Culture := CultureOf(Translation.Locale);
        if (Culture <> NativeCulture) and (Culture <> TargetCulture) then
          AddCulture(Culture);
      end;
      Translation := Translation.Next;
    end;
  end;
end;

procedure TTranslationEditor.SourceLocaleChoicesClick(Sender: TObject);
var
  Culture: TCultureInfo;
  Translation: TTextTranslation;
begin
  if TMenuItem(Sender).Checked then
    Exit; // already selected
  Culture := CultureOf(TMenuItem(Sender).Tag);
  SourceCultureLabel.Culture := Culture;
  PreparePluralChoices(OriginalPlurals, Culture);
  if not Assigned(Culture) or (NativeCulture = Culture) then
  begin
    OriginalTextLabel.Caption := Translator.GetText(SOriginal);
    if Assigned(EditingItem) then
    begin
      if EditingItem.Definition.HasPluralForms then
        OriginalText.Text := EditingItem.Definition.Plurals[OriginalPlurals.TabIndex]
      else
        OriginalText.Text := EditingItem.Definition.Value;
    end;
  end
  else
  begin
    OriginalTextLabel.Caption := Translator.GetText(SApprovedTranslation);
    if Assigned(EditingItem) then
    begin
      Translation := EditingItem.Definition.Translations[Culture.Locale];
      if EditingItem.Definition.HasPluralForms then
        OriginalText.Text := Translation.Plurals[OriginalPlurals.TabIndex]
      else
        OriginalText.Text := Translation.Value;
    end;
  end;
  if Assigned(Culture) then
    OriginalText.BiDiMode := Culture.BiDiMode
  else
    OriginalText.ParentBiDiMode := True;
end;

procedure TTranslationEditor.SourceCultureLabelClick(Sender: TObject);
var
  Pt: TPoint;
begin
  if SourceCultureLabel.IsRightToLeft then
    Pt.X := SourceCultureLabel.Width + 1
  else
    Pt.X := 0;
  Pt.Y := SourceCultureLabel.Height + 1;
  Pt := SourceCultureLabel.ClientToScreen(Pt);
  SourceCulturePopup.Popup(Pt.X, Pt.Y);
end;

procedure TTranslationEditor.TargetCulturePopupPopup(Sender: TObject);

  function AddCulture(Culture: TCultureInfo): TMenuItem;
  begin
    Result := TargetCulturePopup.CreateMenuItem;
    Result.Caption := Culture.LocalizedDisplayName;
    Result.ImageIndex := DM.Flags.ImageIndexOf(Culture);
    Result.Checked := (Culture = TargetCulture);
    Result.RadioItem := True;
    Result.Tag := Culture.LocaleID;
    Result.OnClick := TargetLocaleChoicesClick;
    TargetCulturePopup.Items.Add(Result);
  end;

var
  I: Integer;
begin
  TargetCulturePopup.Items.Clear;
  for I := 0 to Catalog.Cultures.Count - 1 do
    AddCulture(Catalog.Cultures[I]);
end;

procedure TTranslationEditor.TargetCultureLabelClick(Sender: TObject);
var
  Pt: TPoint;
begin
  if TargetCultureLabel.IsRightToLeft then
    Pt.X := TargetCultureLabel.Width + 1
  else
    Pt.X := 0;
  Pt.Y := TargetCultureLabel.Height + 1;
  Pt := TargetCultureLabel.ClientToScreen(Pt);
  TargetCulturePopup.Popup(Pt.X, Pt.Y);
end;

procedure TTranslationEditor.TargetLocaleChoicesClick(Sender: TObject);
begin
  if TMenuItem(Sender).Checked then
    Exit; // already selected
  TargetCulture := CultureOf(TMenuItem(Sender).Tag);
end;

procedure TTranslationEditor.DefinitionSelectionTimerTimer(Sender: TObject);
begin
  ReloadCurrentItemDetails;
end;

procedure TTranslationEditor.DefinitionsCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
var
  TextRect: TRect;
begin
  if Definitions.Items.Count = 0 then
  begin
    TextRect := ARect;
    InflateRect(TextRect, -40, -50);
    Definitions.Canvas.Font.Color := clGrayText;
    DrawText(Definitions.Canvas.Handle, PChar(Translator.GetText(SNoItemToShow)), -1,
      TextRect, Definitions.DrawTextBiDiModeFlags(DT_CENTER or DT_NOPREFIX or DT_WORDBREAK));
  end;
end;

procedure TTranslationEditor.DefinitionsDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  C: TCanvas;
  BackColor: TColor;
begin
  BackColor := Definitions.Color;
  if (TObject(Item.Data) is TItemNode) and not Odd(TItemNode(Item.Data).GroupIndex) then
    BackColor := clAlternative;
  C := Definitions.Canvas;
  DrawListBackground(C, Rect, Item, BackColor);
  if TObject(Item.Data) is TItemNode then
    DrawListItem(C, Rect, TItemNode(Item.Data))
  else if TObject(Item.Data) is TGroupNode then
    DrawListGroup(C, Rect, TGroupNode(Item.Data));
end;

procedure TTranslationEditor.DefinitionsClick(Sender: TObject);
var
  ClickItem: TListItem;
  Pt: TPoint;
  Rect: TRect;
begin
  if SortGrouping then
  begin
    Pt := Definitions.ScreenToClient(Mouse.CursorPos);
    ClickItem := Definitions.GetItemAt(Pt.X, Pt.Y);
    if Assigned(ClickItem) and (TObject(ClickItem.Data) is TGroupNode) then
    begin
      ListView_GetItemRect(Definitions.Handle, ClickItem.Index, Rect, LVIR_SELECTBOUNDS);
      InflateRect(Rect, -2, 0);
      if Definitions.IsRightToLeft then
        Rect.Left := Rect.Right - Definitions.SmallImages.Width
      else
        Rect.Right := Rect.Left + Definitions.SmallImages.Width;
      if PtInRect(Rect, Pt) then
        TGroupNode(ClickItem.Data).Expanded := not TGroupNode(ClickItem.Data).Expanded;
    end;
  end;
end;

procedure TTranslationEditor.DefinitionsColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  SortColumn := Column.Tag;
end;

procedure TTranslationEditor.DefinitionsColumnRightClick(Sender: TObject;
  Column: TListColumn; Point: TPoint);
begin
  HeaderRightClicked := True;
end;

procedure TTranslationEditor.DefinitionsContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Definitions.GetItemAt(MousePos.X, MousePos.Y) = nil then
    Definitions.Selected := Definitions.ItemFocused;
  if HeaderRightClicked then
  begin
    Definitions.PopupMenu := DefinitionsHeaderPopup;
    HeaderRightClicked := False;
  end
  else if Assigned(CurrentGroup) then
    Definitions.PopupMenu := DefinitionGroupPopup
  else
    Definitions.PopupMenu := DefinitionItemPopup;
  Handled := False;
end;

procedure TTranslationEditor.DefinitionsData(Sender: TObject; Item: TListItem);
begin
  List.Prepare(Item);
  Item.Caption := ' ';
end;

procedure TTranslationEditor.DefinitionsDataHint(Sender: TObject;
  StartIndex, EndIndex: Integer);
begin
  List.PrepareRange(StartIndex, EndIndex);
end;

procedure TTranslationEditor.DefinitionsDblClick(Sender: TObject);
var
  CurGroup: TGroupNode;
begin
  CurGroup := CurrentGroup;
  if Assigned(CurGroup) then
    CurGroup.Expanded := not CurGroup.Expanded;
end;

procedure TTranslationEditor.DefinitionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CurItem: TItemNode;
  CurGroup: TGroupNode;
begin
  if SortGrouping and (Shift = []) and ((Key = VK_LEFT) or (Key = VK_RIGHT)) then
  begin
    CurGroup := CurrentGroup;
    if not Assigned(CurGroup) then
    begin
      CurItem := CurrentItem;
      if Assigned(CurItem) then
      begin
        CommitEditingItem;
        CurGroup := CurItem.Group;
      end;
    end;
    if Assigned(CurGroup) then
    begin
      CurGroup.Expanded := (Key = VK_RIGHT) xor Definitions.IsRightToLeft;
      if not CurGroup.Expanded then
        CurGroup.Select;
    end;
    Key := 0;
  end;
end;

procedure TTranslationEditor.DefinitionsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Definitions.GetItemAt(X, Y) = nil then
    Definitions.Selected := Definitions.ItemFocused;
end;

procedure TTranslationEditor.DefinitionsResize(Sender: TObject);
begin
  if Assigned(List) then
    List.UpdateHeaderArrow;
end;

procedure TTranslationEditor.DefinitionsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  DefinitionSelectionTimer.Enabled := False;
  DefinitionSelectionTimer.Enabled := True;
end;

procedure TTranslationEditor.TranslatedTextChange(Sender: TObject);
begin
  if (ActiveControl = TranslatedText) and (Modified <> fModified) then
    MainForm.EditorChanged(Self, CHANGED_MODIFIED);
end;

procedure TTranslationEditor.TranslatedTextExit(Sender: TObject);
begin
  CommitEditingItem;
end;

function TTranslationEditor.CommitEditingItem: Boolean;
var
  Value: String;
  State: TTranslationState;
begin
  Result := False;
  if Assigned(EditingItem) and TargetEditingText.Modified then
  begin
    Value := TargetEditingText.Text;
    TargetEditingText.Modified := False;
    if Value <> '' then
      State := tsUser
    else
      State := tsNone;
    Result := UpdateItem(EditingItem, Value, State, False);
    if not Modified then
      MainForm.EditorChanged(Self, CHANGED_MODIFIED);
  end;
end;

procedure TTranslationEditor.DefinitionGroupPopupPopup(Sender: TObject);
begin
  ExpandMenuItem.Enabled := not CurrentGroup.Expanded;
  CollapseMenuItem.Enabled := CurrentGroup.Expanded;
end;

procedure TTranslationEditor.ExpandMenuItemClick(Sender: TObject);
begin
  CurrentGroup.Expanded := True;
end;

procedure TTranslationEditor.CollapseMenuItemClick(Sender: TObject);
begin
  CurrentGroup.Expanded := False;
end;

procedure TTranslationEditor.SaveDialogCanClose(Sender: TObject;
  var CanClose: Boolean);
var
  MsgText: String;
begin
  MsgText := DM.Localizer.FormatCS(Translator.GetText(SSaveOverwriteConfirm), [SaveDialog.FileName]);
  CanClose := not FileExists(SaveDialog.FileName) or DM.MsgDlg.Confirm(MsgText, mbNo);
end;

function TTranslationEditor.CanPerform(Act: TEditorAction): Boolean;
begin
  case Act of
    eaSave:
      Result := Modified;
    eaSaveAs,
    eaExport:
      Result := not Catalog.Empty;
    eaImport:
      Result := not Readonly;
    eaSetNativeLanguage:
      Result := not Readonly and not Catalog.Empty and not Assigned(NativeCulture);
    eaAddLanguage:
      Result := not Readonly and (Catalog.Cultures.Count < World.Cultures.Count);
    eaDeleteLanguage:
      Result := not Readonly and (Catalog.Cultures.Count > 0);
    eaEditLanguage:
      Result := not Readonly and Assigned(TargetCulture);
    eaSwitchLanguage:
      Result := (Catalog.Cultures.Count > 1);
    eaDeleteDomain,
    eaRenameDomain:
      Result := not Readonly and Assigned(CurrentDomain);
    eaDeleteComponent,
    eaRenameComponent:
      Result := not Readonly and Assigned(CurrentItem) and CurrentItem.IsComponent;
    eaDeleteItem:
      Result := not Readonly and Assigned(CurrentSelection);
    eaFind:
      Result := (Definitions.Items.Count > 0);
    eaReplace:
      Result := not Readonly and Assigned(TargetCulture) and
        (Definitions.Items.Count > 0);
    eaSearchAgain:
      Result := SearchDetails.Active and (Definitions.Items.Count > 0);
    eaPreviousItem,
    eaNextItem:
      Result := (Definitions.Items.Count > 1) or
        ((Definitions.ItemIndex < 0) and (Definitions.Items.Count = 1));
    eaPreviousGroup,
    eaNextGroup:
      Result := List.SortGrouping and ((List.Groups.Count > 1) or
        ((List.Groups.Count = 1) and not Assigned(CurrentGroup)));
    eaPreviousUntranslated,
    eaNextUntranslated:
      Result := ((Definitions.Items.Count > 1) or
        ((Definitions.ItemIndex < 0) and (Definitions.Items.Count = 1))) and
        (List.ConditionalCount([tsNone], True) <> 0);
    eaPreviousUnaccepted,
    eaNextUnaccepted:
      Result := ((Definitions.Items.Count > 1) or
        ((Definitions.ItemIndex < 0) and (Definitions.Items.Count = 1))) and
        (List.ConditionalCount([tsFuzzy..tsAuto], True) <> 0);
    eaEditTranslatorNote:
      Result := not Readonly and Assigned(TargetCulture) and
        (((DM.BulkActions <> 0) and Assigned(CurrentGroup)) or Assigned(CurrentItem));
    eaClearTranslatorNote:
      Result := not Readonly and Assigned(TargetCulture) and
        (((DM.BulkActions <> 0) and Assigned(CurrentGroup)) or
        (Assigned(CurrentItem) and (CurrentItem.Translation.Note <> '')));
    eaAcceptTranslation:
      Result := not Readonly and Assigned(TargetCulture) and
        (((DM.BulkActions <> 0) and Assigned(CurrentGroup)) or
        (Assigned(CurrentItem) and not CurrentItem.IsApproved and
         (CurrentItem.Translation.Value <> '')));
    eaRejectTranslation:
      Result := not Readonly and Assigned(TargetCulture) and
        (((DM.BulkActions <> 0) and Assigned(CurrentGroup)) or
        (Assigned(CurrentItem) and CurrentItem.IsApproved));
    eaCopyOriginalText:
      Result := not Readonly and Assigned(TargetCulture) and
        (((DM.BulkActions <> 0) and Assigned(CurrentGroup)) or
        (Assigned(CurrentItem) and (CurrentItem.Definition.Value <> CurrentItem.Translation.Value)));
    eaGoogleTranslate:
      Result := not Readonly and Assigned(TargetCulture) and GoogleTranslator.CanTranslate and
        (((DM.BulkActions <> 0) and Assigned(CurrentGroup)) or
        (Assigned(CurrentItem) and not (CurrentItem.IsApproved or CurrentItem.IsGoogleTranslated)));
    eaAutoTranslate:
      Result := not Readonly and Assigned(NativeCulture) and Assigned(TargetCulture) and not DM.Repository.Empty and
        (((DM.BulkActions <> 0) and Assigned(CurrentGroup)) or
        (Assigned(CurrentItem) and not CurrentItem.IsApproved and DM.Repository.HasSynonym(CurrentItem.Definition.Value, NativeCulture, TargetCulture)));
    eaSuggestTranslation:
      Result := not Readonly and Assigned(CurrentItem) and
        DM.Repository.HasSynonym(CurrentItem.Definition.Value, NativeCulture, TargetCulture);
    eaExpandGroups:
      Result := SortGrouping and List.IsAnyGroupCollapsed;
    eaCollapseGroups:
      Result := SortGrouping and List.IsAnyGroupExpanded;
  else
    Result := False;
  end;
end;

function TTranslationEditor.Perform(Act: TEditorAction): Boolean;
begin
  CommitEditingItem;
  case Act of
    eaSave:
      Result := DoSave;
    eaSaveAs:
      Result := DoSaveAs;
    eaExport:
      Result := DoExport;
    eaImport:
      Result := DoImport;
    eaSetNativeLanguage:
      Result := DoSetNativeLanguage;
    eaAddLanguage:
      Result := DoAddLanguage;
    eaDeleteLanguage:
      Result := DoDeleteLanguage;
    eaEditLanguage:
      Result := DoEditLanguage;
    eaSwitchLanguage:
      Result := DoSwitchLanguage;
    eaDeleteDomain:
      Result := DoDeleteDomain(CurrentDomain);
    eaDeleteComponent:
      Result := DoDeleteComponent(CurrentItem);
    eaDeleteItem:
      if Assigned(CurrentItem) then
        Result := DoDeleteItem(CurrentItem)
      else
        Result := DoDeleteGroupItems(CurrentGroup);
    eaRenameDomain:
      Result := DoRenameDomain(CurrentDomain);
    eaRenameComponent:
      Result := DoRenameComponent(CurrentItem);
    eaFind:
      Result := DoFind;
    eaReplace:
      Result := DoReplace;
    eaSearchAgain:
      Result := DoSearch(False);
    eaPreviousItem:
      Result := DoNavigateItem(True);
    eaNextItem:
      Result := DoNavigateItem(False);
    eaPreviousGroup:
      Result := DoNavigateGroup(True);
    eaNextGroup:
      Result := DoNavigateGroup(False);
    eaPreviousUntranslated:
      Result := DoNavigateConditional(True, [tsNone]);
    eaNextUntranslated:
      Result := DoNavigateConditional(False, [tsNone]);
    eaPreviousUnaccepted:
      Result := DoNavigateConditional(True, [tsFuzzy..tsAuto]);
    eaNextUnaccepted:
      Result := DoNavigateConditional(False, [tsFuzzy..tsAuto]);
    eaEditTranslatorNote:
      Result := Apply(CurrentSelection, DoEditTranslatorNote);
    eaClearTranslatorNote:
      Result := Apply(CurrentSelection, DoClearTranslatorNote);
    eaAcceptTranslation:
      Result := Apply(CurrentSelection, DoAcceptTranslation);
    eaRejectTranslation:
      Result := Apply(CurrentSelection, DoRejectTranslation);
    eaCopyOriginalText:
      Result := Apply(CurrentSelection, DoCopyOriginalText);
    eaGoogleTranslate:
      Result := Apply(CurrentSelection, DoGoogleTranslate);
    eaAutoTranslate:
      Result := Apply(CurrentSelection, DoAutoTranslate);
    eaSuggestTranslation:
      Result := DoSuggestTranslation(CurrentItem);
    eaExpandGroups:
      Result := DoExpandCollapseAll(True);
    eaCollapseGroups:
      Result := DoExpandCollapseAll(False);
  else
    Result := False;
  end;
end;

end.
