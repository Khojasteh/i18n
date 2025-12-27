{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  Internationalization and Localization for Delphi                            }
{                                                                              }
{  Copyright (c) Kambiz Khojasteh                                              }
{  https://github.com/khojasteh/i18n                                           }
{                                                                              }
{------------------------------------------------------------------------------}

/// <summary>
/// This unit implements an editor for the <see cref="TTranslatable"/> class
/// to allow the developer to select the translatable properties and string
/// literals.
/// </summary>
/// <remarks>
/// This unit cannot be referenced in the runtime packages.
/// </remarks>
unit i18nPropEditor;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  i18nCore, i18nLocalizer, i18nCatalog, i18nHashList, i18nCtrls, i18nParser,
  StdCtrls, ExtCtrls, ComCtrls, ImgList, ActnList, Menus, StdActns, Tabs,
  System.Actions;

type

  {$region 'xmldoc'}
  /// <summary>
  /// TTranslatablesUI engages some UI gadgets to display a list of translatable
  /// strings, so that the user can edit them and select which ones should be
  /// available to a translator.
  /// </summary>
  {$endregion}
  TTranslatablesUI = class
  public
    type
      {$region 'xmldoc'}
      /// <summary>
      /// This enumeration type identifies the ways that a list of translatable
      /// strings can be filtered to display.
      /// </summary>
      {$endregion}
      TFilterType = (
        {$region 'xmldoc'}
        /// Displays all strings without filtering.
        {$endregion}
        ftNone = 1,
        {$region 'xmldoc'}
        /// Only lists strings with alphabetic characters.
        {$endregion}
        ftAlphaValue,
        {$region 'xmldoc'}
        /// Only lists strings with any non-alphabetic character.
        {$endregion}
        ftNonAlphaValue,
        {$region 'xmldoc'}
        /// Only lists strings with one character in length.
        {$endregion}
        ftCharValue,
        {$region 'xmldoc'}
        /// For properties, only lists those that their property path contains 'name'.
        /// For literals, only lists those that have plural forms.
        {$endregion}
        ftNamePropertyOrPluralLiteral,
        {$region 'xmldoc'}
        /// Only lists strings that satisfy a user-defined condition.
        {$endregion}
        ftCustom
      );
      {$region 'xmldoc'}
      /// <summary>
      /// TStats keeps the number of total and selected translatable strings in
      /// each filtering group.
      /// </summary>
      {$endregion}
      TStats = record
        {$region 'xmldoc'}
        /// Lists the total number of items for each filtering group.
        {$endregion}
        Total: array[TFilterType] of Integer;
        {$region 'xmldoc'}
        /// Lists the number of selected items for each filtering group.
        {$endregion}
        Selected: array[TFilterType] of Integer;
      end;
  private
    fKind: TTextDitionaryKind;
    fListControl: TListView;
    fFilterControl: TCustomListControl;
    fValueControl: TCustomMemo;
    fCommentControl: TCustomMemo;
    fActiveFilter: TFilterType;
    fCustomFilter: String;
    fSelectedItem: TTextItem;
    fPluralIndex: Integer;
    procedure SetActiveFilter(Value: TFilterType);
    procedure SetCustomFilter(const Value: String);
    procedure SetSelectedItem(Value: TTextItem);
    procedure SetPluralIndex(Value: Integer);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Updates content of the linked controls to reflect the current
    /// state of the strings.
    /// </summary>
    {$endregion}
    procedure UpdateControls;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines which filtering options is the most suited for a specified
    /// string item.
    /// </summary>
    /// <param name="Item">
    /// The string item to examine.
    /// </param>
    /// <returns>
    /// Returns the <see cref="TFilterType"/> value that is the
    /// most suited for the given <paramref name="Item"/>.
    /// </returns>
    {$endregion}
    function FilterTagOf(Item: TTextItem): TFilterType;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the image index of a specified string item.
    /// </summary>
    /// <param name="Item">
    /// The string item to get its image index.
    /// </param>
    /// <returns>
    /// The image index of the given <paramref name="Item"/>.
    /// </returns>
    {$endregion}
    function ImageIndexOf(Item: TTextItem): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a specified filter value to a zero-based integer value.
    /// </summary>
    /// <param name="Filter">
    /// The filter value to convert.
    /// </param>
    /// <returns>
    /// Returns the zero-based integer value of <paramref name="Filter"/>.
    /// </returns>
    /// <seealso cref="FilterOfIndex"/>
    {$endregion}
    function IndexOfFilter(Filter: TFilterType): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a specified filter as a zero-based integer value to a filter
    /// value.
    /// </summary>
    /// <param name="Index">
    /// The integer value to convert.
    /// </param>
    /// <returns>
    /// Returns filter value of <paramref name="Index"/>.
    /// </returns>
    /// <seealso cref="IndexOfFilter"/>
    {$endregion}
    function FilterOfIndex(Index: Integer): TFilterType;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// The list of all string items.
    /// </summary>
    {$endregion}
    All: TTextItems;
    {$region 'xmldoc'}
    /// <summary>
    /// The list of item IDs that indicates which string items are selected.
    /// </summary>
    {$endregion}
    Selection: TStringList;
    {$region 'xmldoc'}
    /// <summary>
    /// The number of total and selected string items in each filtering group.
    /// </summary>
    {$endregion}
    Stats: TStats;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    /// <param name="AKind">
    /// Indicates whether the class is instantiated for a list of properties or
    /// a list of string constants and literals.
    /// </param>
    /// <param name="AListControl">
    /// The control that displays the list of items.
    /// </param>
    /// <param name="AFilterControl">
    /// The control that provides filtering options.
    /// </param>
    /// <param name="AValueControl">
    /// The control that displays the value of an item.
    /// </param>
    /// <param name="ACommentControl">
    /// The control that displays the comment of an item.
    /// </param>
    {$endregion}
    constructor Create(AKind: TTextDitionaryKind;
      AListControl: TListView; AFilterControl: TCustomListControl;
      AValueControl, ACommentControl: TCustomMemo);
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Selects a specified list of items.
    /// </summary>
    /// <param name="SelectedItems">
    /// The list of items to be selected.
    /// </param>
    /// <seealso cref="GetSelection"/>
    {$endregion}
    procedure SetSelection(SelectedItems: TTextItems);
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the list of selected items.
    /// </summary>
    /// <param name="SelectedItems">
    /// Returns the list of selected items.
    /// </param>
    /// <seealso cref="SetSelection"/>
    {$endregion}
    procedure GetSelection(SelectedItems: TTextItems);
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the list of items whose value has been edited by the user.
    /// </summary>
    /// <param name="UpdatedItems">
    /// Returns the list of edited items.
    /// </param>
    /// <seealso cref="GetSelection"/>
    {$endregion}
    procedure GetUpdated(UpdatedItems: TTextItems);
    {$region 'xmldoc'}
    /// <summary>
    /// Selects or deselects a specified item.
    /// </summary>
    /// <param name="Item">
    /// The item to select or deselect.
    /// </param>
    /// <param name="Checked">
    /// Indicates whether the item should be selected or deselected.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if selection state of the item is changed.
    /// Otherwise, returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="GetChecked"/>
    /// <seealso cref="SetAllState"/>
    {$endregion}
    function SetChecked(Item: TTextItem; Checked: Boolean): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether an item is selected.
    /// </summary>
    /// <param name="Item">
    /// The item to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the item is selected, Otherwise, returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="SetChecked"/>
    {$endregion}
    function GetChecked(Item: TTextItem): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Changes selection state of all items according to a specified state.
    /// </summary>
    /// <remarks>
    /// SetAllState changes state of all the items (<see cref="All"/>) according
    /// to the given <paramref name="State"/> value.
    /// <list type="table">
    ///   <listheader>
    ///     <term>State</term>
    ///     <description>Description</description>
    ///   </listheader>
    ///   <item>
    ///     <term>cbUnchecked</term>
    ///     <description>Deselects all the items.</description>
    ///   </item>
    ///   <item>
    ///     <term>cbChecked</term>
    ///     <description>Selects all the items.</description>
    ///   </item>
    ///   <item>
    ///     <term>cbGrayed</term>
    ///     <description>Toggles selection of all the items.</description>
    ///   </item>
    /// </list>
    /// </remarks>
    /// <param name="State">
    /// Specifies the new state of the items.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if selection state of any item is changed.
    /// Otherwise, returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="SetChecked"/>
    /// <seealso cref="CanSetAllState"/>
    {$endregion}
    function SetAllState(State: TCheckBoxState): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether any of the items in the list can change its state to a
    /// specified state.
    /// </summary>
    /// <param name="State">
    /// Specifies the new state of the items.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if state of any item can be changed, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="SetAllState"/>
    {$endregion}
    function CanSetAllState(State: TCheckBoxState): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Reevaluates the information regarding to a specified item.
    /// </summary>
    /// <param name="Item">
    /// The item to reevaluate.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the reevaluation caused any changes,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Reevaluate(Item: TTextItem): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified item is visible by the currently active
    /// filter.
    /// </summary>
    /// <param name="Item">
    /// The item to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the item satisfies the currently active
    /// filter, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsVisible(Item: TTextItem): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Activates a filter using its zero-based index.
    /// </summary>
    /// <param name="Index">
    /// The zero-based index of the filter to activate.
    /// </param>
    /// <param name="CustomFilterPrompt">
    /// The prompt string that will be displayed to the user if the activated
    /// filter is a custom filter.
    /// </param>
    /// <seealso cref="ActiveFilter"/>
    {$endregion}
    procedure ActivateFilterByIndex(Index: Integer; const CustomFilterPrompt: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the value of the selected item using the attached edit control.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the value of the item is changed, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="UpdateSelectedItemComment"/>
    /// <seealso cref="SelectedItem"/>
    {$endregion}
    function UpdateSelectedItemValue: Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the comment of the selected item using the attached edit control.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the comment of the item is changed, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="UpdateSelectedItemValue"/>
    /// <seealso cref="SelectedItem"/>
    {$endregion}
    function UpdateSelectedItemComment: Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the object is representing a list of translatable properties
    /// or a list of translatable string constants and literals.
    /// </summary>
    {$endregion}
    property Kind: TTextDitionaryKind read fKind;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the control that lists the translatable strings.
    /// </summary>
    {$endregion}
    property ListControl: TListView read fListControl;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the control that provides filtering options.
    /// </summary>
    {$endregion}
    property FilterControl: TCustomListControl read fFilterControl;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the control that displays value of the selected item.
    /// </summary>
    /// <seealso cref="SelectedItem"/>
    {$endregion}
    property ValueControl: TCustomMemo read fValueControl;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the control that displays comment of the selected item.
    /// </summary>
    /// <seealso cref="SelectedItem"/>
    {$endregion}
    property CommentControl: TCustomMemo read fCommentControl;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets of sets the currently active filter.
    /// </summary>
    {$endregion}
    property ActiveFilter: TFilterType read fActiveFilter write SetActiveFilter;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the user-defined filtering string.
    /// </summary>
    {$endregion}
    property CustomFilter: String read fCustomFilter write SetCustomFilter;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the translatable string that its value and comment is currently
    /// displayed.
    /// </summary>
    {$endregion}
    property SelectedItem: TTextItem read fSelectedItem write SetSelectedItem;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which of the plural forms of the selected item is currently
    /// displayed.
    /// </summary>
    /// <seealso cref="SelectedItem"/>
    {$endregion}
    property PluralIndex: Integer read fPluralIndex write SetPluralIndex;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TTranslatableEditor displays a dialog box to select the translatable strings
  /// that can be translated by a <see cref="TTranslator"/> component.
  /// </summary>
  {$endregion}
  TTranslatableEditor = class(TForm)
    PageControl: TPageControl;
    tabProperties: TTabSheet;
    tabLiterals: TTabSheet;
    tabExport: TTabSheet;
    btnOK: TButton;
    btnCancel: TButton;
    PropertyList: TListView;
    ImageList: TImageList;
    PropertyFilter: TComboBox;
    LiteralList: TListView;
    TextLanguageLabel: TLabel;
    SaveDialog: TSaveDialog;
    btnExportTo: TButton;
    ActionList: TActionList;
    PropsCheckAll: TAction;
    PropsUncheckAll: TAction;
    PropsInvertCheck: TAction;
    PropertiesPopup: TPopupMenu;
    CheckAll1: TMenuItem;
    UncheckAll1: TMenuItem;
    Invert1: TMenuItem;
    LiteralsPopup: TPopupMenu;
    btnPropsCheckAll: TButton;
    btnPropsUncheckAll: TButton;
    btnPropsInvertCheck: TButton;
    LiteralsCheckAll: TAction;
    LiteralsUncheckAll: TAction;
    LiteralsInvertCheck: TAction;
    LiteralFilter: TComboBox;
    btnLiteralsCheckAll: TButton;
    btnLiteralsUncheckAll: TButton;
    btnLiteralsInvertCheck: TButton;
    CheckAll2: TMenuItem;
    UncheckAll2: TMenuItem;
    Invert2: TMenuItem;
    ExportCommentsLabel: TLabel;
    LanguageBoxPlaceHolder: TComboBox;
    TextEditorPopupMenu: TPopupMenu;
    EditCutAction: TEditCut;
    EditCopyAction: TEditCopy;
    EditPasteAction: TEditPaste;
    EditSelectAllAction: TEditSelectAll;
    EditUndoAction: TEditUndo;
    Undo1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    PropertiesTabSet: TTabSet;
    PropertyValue: TMemo;
    PropertyComments: TMemo;
    LiteralValue: TMemo;
    LiteralComments: TMemo;
    LiteralsTabSet: TTabSet;
    LiteralPlurals: TTabSet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PropertyFilterSelect(Sender: TObject);
    procedure FilterDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListViewCustomDraw(Sender: TCustomListView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure ListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure PropertyListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure PropsCheckAllExecute(Sender: TObject);
    procedure PropsCheckAllUpdate(Sender: TObject);
    procedure PropsUncheckAllExecute(Sender: TObject);
    procedure PropsUncheckAllUpdate(Sender: TObject);
    procedure PropsInvertCheckExecute(Sender: TObject);
    procedure PropsInvertCheckUpdate(Sender: TObject);
    procedure PropertyValueExit(Sender: TObject);
    procedure PropertyCommentsExit(Sender: TObject);
    procedure PropertiesTabSetChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure LiteralFilterSelect(Sender: TObject);
    procedure LiteralListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure LiteralsCheckAllExecute(Sender: TObject);
    procedure LiteralsCheckAllUpdate(Sender: TObject);
    procedure LiteralsUncheckAllExecute(Sender: TObject);
    procedure LiteralsUncheckAllUpdate(Sender: TObject);
    procedure LiteralsInvertCheckExecute(Sender: TObject);
    procedure LiteralsInvertCheckUpdate(Sender: TObject);
    procedure LiteralValueExit(Sender: TObject);
    procedure LiteralCommentsExit(Sender: TObject);
    procedure LiteralsTabSetChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure LiteralListCustomDraw(Sender: TCustomListView; const ARect: TRect;
      var DefaultDraw: Boolean);
    procedure btnExportToClick(Sender: TObject);
    procedure PropertyListItemChecked(Sender: TObject; Item: TListItem);
    procedure LiteralListItemChecked(Sender: TObject; Item: TListItem);
    procedure LanguageBoxSelect(Sender: TObject);
    procedure LiteralPluralsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
  private
    const FILTER_DEFAULT = TTranslatablesUI.TFilterType.ftNone;
  private
    Properties: TTranslatablesUI;
    Literals: TTranslatablesUI;
    Translatables: TTranslatables;
    TextLanguage: TCultureBox;
    LiteralCollector: TPascalStringCollector;
    CollectorErrorMsg: String;
    procedure ReadProperties;
    procedure WriteProperties;
    procedure ReadLiterals;
    procedure WriteLiterals;
    procedure AddTranslatablesTo(Catalog: TTranslationCatalog;
      KeepObsoleteEntries: Boolean);
    procedure ExportTo(const FileName: String);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Displays the editor for a specified <see cref="TTranslatables"/> object.
    /// </summary>
    /// <param name="ATranslatables">
    /// The <see cref="TTranslatables"/> object to edit.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TTranslatables"/>
    /// object is modified, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    class function Execute(ATranslatables: TTranslatables): Boolean;
  end;

implementation

{$R *.dfm}

uses
  Types, StrUtils, Character, CommCtrl, i18nZStrList, i18nIDE;

resourcestring
  SEditorCaption        = 'Translatables of %s';
  SCustomFilter         = 'Custom Filter';
  SPropertyFilterPrompt = 'Property''s path contains:';
  SLiteralFilterPrompt  = 'String contains:';
  SNoItemsToView        = 'There are no items to show in this view.';
  SCollectError         = 'Constant and literal strings can not be collected from the source code.';
  SParserError          = 'The parser found error in your code: %s';
  SFileExists           = '%s alreay exists.';
  SContentMismatch      = 'The selected file is not a translation catalog file.';
  SLanguageMismatch     = 'The base language of the selected translation catalog file is not same as the selected language.';
  SUpdateAction         = 'Press [OK] to update the content of the file.';
  SReplaceAction        = 'Press [OK] to replace the file.';
  SCancelAction         = 'Press [Cancel] to stop the operation.';

const
  clAlternative = $F9FBFB;

{ Helper Functions }

function IsStringAlpha(const Str: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(Str) do
    if IsLetter(Str[I]) then
    begin
      Result := True;
      Exit;
    end;
end;

{ TStringsInfo }

constructor TTranslatablesUI.Create(AKind: TTextDitionaryKind;
  AListControl: TListView; AFilterControl: TCustomListControl;
  AValueControl, ACommentControl: TCustomMemo);
begin
  All := TTextItems.Create;
  Selection := TStringList.Create;
  Selection.Duplicates := dupIgnore;
  Selection.Sorted := True;
  fKind := AKind;
  fListControl := AListControl;
  fFilterControl := AFilterControl;
  fValueControl := AValueControl;
  fCommentControl := ACommentControl;
end;

destructor TTranslatablesUI.Destroy;
begin
  All.Free;
  Selection.Free;
  inherited Destroy;
end;

procedure TTranslatablesUI.SetActiveFilter(Value: TFilterType);
begin
  if ActiveFilter <> Value then
  begin
    fActiveFilter := Value;
    UpdateControls;
  end;
end;

procedure TTranslatablesUI.SetCustomFilter(const Value: String);
begin
  if (CustomFilter <> Value) or ((Value <> '') and (ActiveFilter <> ftCustom)) then
  begin
    fActiveFilter := ftCustom;
    fCustomFilter := Value;
    UpdateControls;
  end;
end;

procedure TTranslatablesUI.SetSelectedItem(Value: TTextItem);
begin
  if SelectedItem <> Value then
  begin
    fSelectedItem := Value;
    fPluralIndex := 0;
    if Assigned(SelectedItem) then
    begin
      if SelectedItem.HasPluralForms then
        ValueControl.Text := SelectedItem.Plurals[PluralIndex]
      else
        ValueControl.Text := SelectedItem.Value;
      ValueControl.Modified := False;
      ValueControl.ReadOnly := (Kind = dkLiteral);
      CommentControl.Text := SelectedItem.Comment;
      CommentControl.Modified := False;
      CommentControl.ReadOnly := False;
    end
    else
    begin
      ValueControl.Clear;
      ValueControl.ReadOnly := True;
      CommentControl.Clear;
      CommentControl.ReadOnly := True;
    end;
  end;
end;

procedure TTranslatablesUI.SetPluralIndex(Value: Integer);
begin
  if not (Assigned(SelectedItem) and SelectedItem.HasPluralForms) then
    Value := 0;
  if PluralIndex <> Value then
  begin
    fPluralIndex := Value;
    ValueControl.Text := SelectedItem.Plurals[PluralIndex];
  end;
end;

procedure TTranslatablesUI.SetSelection(SelectedItems: TTextItems);
var
  Item, Source: TTextItem;
  Filter: TFilterType;
begin
  // prepare selection list
  Item := SelectedItems.First;
  while Item <> nil do
  begin
    Source := All.Find(Item.ID);
    if Source <> nil then
    begin
      Source.Comment := Item.Comment;
      Selection.Add(Source.ID)
    end;
    Item := Item.Next;
  end;
  // prepare filter information
  Stats.Total[ftNone] := All.Count;
  Stats.Selected[ftNone] := Selection.Count;
  Item := All.First;
  while Item <> nil do
  begin
    Filter := FilterTagOf(Item);
    Item.Tag := Ord(Filter);
    Inc(Stats.Total[Filter]);
    if GetChecked(Item) then
      Inc(Stats.Selected[Filter]);
    Item := Item.Next;
  end;
end;

procedure TTranslatablesUI.GetSelection(SelectedItems: TTextItems);
var
  Item: TTextItem;
begin
  SelectedItems.Clear;
  Item := All.First;
  while Item <> nil do
  begin
    if GetChecked(Item) then
      SelectedItems.Add(Item.ID).Assign(Item);
    Item := Item.Next;
  end;
end;

procedure TTranslatablesUI.GetUpdated(UpdatedItems: TTextItems);
var
  Item: TTextItem;
begin
  UpdatedItems.Clear;
  Item := All.First;
  while Item <> nil do
  begin
    if Item.IsTranslated then
      UpdatedItems.Add(Item.ID).Assign(Item);
    Item := Item.Next;
  end;
end;

function TTranslatablesUI.FilterTagOf(Item: TTextItem): TFilterType;
begin
  if (Kind = dkProperty) and AnsiEndsText('Name', Item.ID) then
    Result := ftNamePropertyOrPluralLiteral
  else if (Kind = dkLiteral) and Item.HasPluralForms then
    Result := ftNamePropertyOrPluralLiteral
  else if Length(Item.Value) = 1 then
    Result := ftCharValue
  else if IsStringAlpha(Item.Value) then
    Result := ftAlphaValue
  else
    Result := ftNonAlphaValue;
end;

function TTranslatablesUI.ImageIndexOf(Item: TTextItem): Integer;
begin
  if Item.Tag = 0 then
    Item.Tag := Ord(FilterTagOf(Item));
  Result := Item.Tag;
  if Item.Comment <> '' then
    Inc(Result, 7);
end;

function TTranslatablesUI.FilterOfIndex(Index: Integer): TFilterType;
begin
  Result := TFilterType(Index + 1);
end;

function TTranslatablesUI.IndexOfFilter(Filter: TFilterType): Integer;
begin
  Result := Ord(Filter) - 1;
end;

function TTranslatablesUI.IsVisible(Item: TTextItem): Boolean;
begin
  if ActiveFilter = ftCustom then
    if Kind = dkProperty then
      Result := (Pos(LowerCase(CustomFilter), LowerCase(Item.ID)) <> 0)
    else
      Result := (Pos(LowerCase(CustomFilter), LowerCase(Item.Value)) <> 0)
  else
    Result := (ActiveFilter in [ftNone, TFilterType(Item.Tag)]);
end;

procedure TTranslatablesUI.UpdateControls;
var
  Item: TTextItem;
  SavedOnItemChecked: TLVCheckedItemEvent;
begin
  if ActiveFilter = ftCustom then
  begin
    Stats.Total[ftCustom] := 0;
    Stats.Selected[ftCustom] := 0;
  end;
  SelectedItem := nil;
  SavedOnItemChecked := ListControl.OnItemChecked;
  ListControl.OnItemChecked := nil;
  ListControl.Items.BeginUpdate;
  try
    ListControl.Items.Clear;
    Item := All.First;
    while Item <> nil do
    begin
      if IsVisible(Item) then
        with ListControl.Items.Add do
        begin
          Data := Item;
          if Kind = dkProperty then
          begin
            Caption := Item.ID;
            SubItems.Add(Item.Value);
          end
          else if Item.HasPluralForms then
          begin
            Caption := ZStrings.Split(Item.Value, '|');
            SubItems.Add(IntToStr(Item.PluralCount));
          end
          else
            Caption := Item.Value;
          ImageIndex := ImageIndexOf(Item);
          Checked := GetChecked(Item);
          if ActiveFilter = ftCustom then
          begin
            Inc(Stats.Total[ftCustom]);
            if Checked then
              Inc(Stats.Selected[ftCustom]);
          end;
        end;
      Item := Item.Next;
    end;
  finally
    ListControl.Items.EndUpdate;
    ListControl.OnItemChecked := SavedOnItemChecked;
  end;
  if ListControl.Items.Count > 0 then
    ListControl.ItemIndex := 0;
  ListControl.Width := ListControl.Width + 1;
  ListControl.Width := ListControl.Width - 1;
  FilterControl.ItemIndex := IndexOfFilter(ActiveFilter);
  if ActiveFilter = ftCustom then
    FilterControl.Invalidate;
end;

function TTranslatablesUI.SetChecked(Item: TTextItem; Checked: Boolean): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if Item.HasPluralForms then
    Exit;
  Index := Selection.IndexOf(Item.ID);
  if Checked xor (Index >= 0) then
  begin
    if Checked then
    begin
      Selection.Add(Item.ID);
      Inc(Stats.Selected[TFilterType(Item.Tag)]);
      Inc(Stats.Selected[ftNone]);
    end
    else
    begin
      Selection.Delete(Index);
      Dec(Stats.Selected[TFilterType(Item.Tag)]);
      Dec(Stats.Selected[ftNone]);
    end;
    FilterControl.Invalidate;
    Result := True;
  end;
end;

function TTranslatablesUI.GetChecked(Item: TTextItem): Boolean;
begin
  Result := Item.HasPluralForms or (Selection.IndexOf(Item.ID) >= 0);
end;

function TTranslatablesUI.SetAllState(State: TCheckBoxState): Boolean;
var
  I: Integer;
  SavedOnItemChecked: TLVCheckedItemEvent;
begin
  Result := False;
  SavedOnItemChecked := ListControl.OnItemChecked;
  try
    for I := 0 to ListControl.Items.Count - 1 do
      with ListControl.Items[I] do
      begin
        if TTextItem(Data).HasPluralForms then
          Continue;
        case State of
          cbUnchecked:
          begin
            if SetChecked(TTextItem(Data), False) then
            begin
              Checked := False;
              Result := True;
            end;
          end;
          cbChecked:
          begin
            if SetChecked(TTextItem(Data), True) then
            begin
              Checked := True;
              Result := True;
            end;
          end;
        else
          Checked := not Checked;
          if SetChecked(TTextItem(Data), Checked) then
            Result := True;
        end;
      end;
  finally
    ListControl.OnItemChecked := SavedOnItemChecked;
  end;
end;

function TTranslatablesUI.CanSetAllState(State: TCheckBoxState): Boolean;
var
  StatsFixed: Integer;
begin
  StatsFixed := 0;
  if (Kind = dkLiteral) and (ActiveFilter in [ftNone, ftNamePropertyOrPluralLiteral]) then
    StatsFixed := Stats.Total[ftNamePropertyOrPluralLiteral];
  case State of
    cbUnchecked:
      Result := (Stats.Selected[ActiveFilter] <> StatsFixed);
    cbChecked:
      Result := (Stats.Selected[ActiveFilter] <> Stats.Total[ActiveFilter]);
  else
    Result := (Stats.Total[ActiveFilter] <> StatsFixed);
  end;
end;

function TTranslatablesUI.Reevaluate(Item: TTextItem): Boolean;
var
  OldFilter, NewFilter: TFilterType;
begin
  Result := False;
  OldFilter := TFilterType(Item.Tag);
  NewFilter := FilterTagOf(Item);
  if NewFilter <> OldFilter then
  begin
    Dec(Stats.Total[OldFilter]);
    Inc(Stats.Total[NewFilter]);
    if Selection.IndexOf(Item.ID) >= 0 then
    begin
      Dec(Stats.Selected[OldFilter]);
      Inc(Stats.Selected[NewFilter]);
    end;
    Item.Tag := Ord(NewFilter);
    FilterControl.Invalidate;
    Result := True;
  end;
end;

procedure TTranslatablesUI.ActivateFilterByIndex(Index: Integer;
  const CustomFilterPrompt: String);
var
  Filter: String;
begin
  ActiveFilter := FilterOfIndex(Index);
  if ActiveFilter = ftCustom then
  begin
    Filter := CustomFilter;
    if InputQuery(SCustomFilter, CustomFilterPrompt, Filter) then
      CustomFilter := Filter;
  end;
end;

function TTranslatablesUI.UpdateSelectedItemValue: Boolean;
begin
  Result := False;
  if Assigned(SelectedItem) and ValueControl.Modified and (Kind = dkProperty) then
  begin
    if SelectedItem.Value <> ValueControl.Text then
    begin
      SelectedItem.Value := ValueControl.Text;
      if Kind = dkProperty then
        ListControl.Selected.SubItems[0] := SelectedItem.Value
      else
        ListControl.Selected.Caption := SelectedItem.Value;
      if Reevaluate(SelectedItem) then
        ListControl.Selected.ImageIndex := ImageIndexOf(SelectedItem);
      Result := True;
    end;
    ValueControl.Modified := False;
  end;
end;

function TTranslatablesUI.UpdateSelectedItemComment: Boolean;
begin
  Result := False;
  if Assigned(SelectedItem) and CommentControl.Modified then
  begin
    CommentControl.Text := Trim(CommentControl.Text);
    if SelectedItem.Comment <> CommentControl.Text then
    begin
      SelectedItem.Comment := CommentControl.Text;
      ListControl.Selected.ImageIndex := ImageIndexOf(SelectedItem);
      Result := True;
    end;
    CommentControl.Modified := False;
  end;
end;

{ TTranslatableEditor }

class function TTranslatableEditor.Execute(ATranslatables: TTranslatables): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      Translatables := ATranslatables;
      Caption := Format(SEditorCaption, [Translatables.DomainName]);
      ReadProperties;
      ReadLiterals;
      if ShowModal = mrOK then
      begin
        WriteProperties;
        WriteLiterals;
        Result := True;
      end;
    finally
      Free;
    end;
end;

procedure TTranslatableEditor.FormCreate(Sender: TObject);
begin
  TextLanguage := TCultureBox.Create(Self);
  TextLanguage.Parent := LanguageBoxPlaceHolder.Parent;
  TextLanguage.Flags := TFlagImageList.Create(Self);
  TextLanguage.BoundsRect := LanguageBoxPlaceHolder.BoundsRect;
  TextLanguage.TabOrder := LanguageBoxPlaceHolder.TabOrder;
  TextLanguage.OnSelect := LanguageBoxPlaceHolder.OnSelect;
  TextLanguage.Sorted := True;
  TextLanguage.CollectAll;
  TextLanguageLabel.FocusControl := TextLanguage;
  LanguageBoxPlaceHolder.Free;
  Properties := TTranslatablesUI.Create(dkProperty, PropertyList, PropertyFilter, PropertyValue, PropertyComments);
  Literals := TTranslatablesUI.Create(dkLiteral, LiteralList, LiteralFilter, LiteralValue, LiteralComments);
  PropertyValue.Visible := (PropertiesTabSet.TabIndex = 0);
  PropertyComments.Visible := (PropertiesTabSet.TabIndex = 1);
  LiteralValue.Visible := (LiteralsTabSet.TabIndex = 0);
  LiteralComments.Visible := (LiteralsTabSet.TabIndex = 1);
  PageControl.ActivePageIndex := 0;
end;

procedure TTranslatableEditor.FormDestroy(Sender: TObject);
begin
  if Assigned(LiteralCollector) then
    LiteralCollector.Free;
  Properties.Free;
  Literals.Free;
end;

procedure TTranslatableEditor.FormShow(Sender: TObject);
begin
  Properties.ActiveFilter := FILTER_DEFAULT;
  Literals.ActiveFilter := FILTER_DEFAULT;
  TextLanguage.ItemSelected := GetApplicationDefaultCulture;
  if not Assigned(TextLanguage.ItemSelected) then
    TextLanguage.ItemSelected := GetUserDefaultUICulture;
  btnExportTo.Enabled := Assigned(TextLanguage.ItemSelected);
end;

procedure TTranslatableEditor.ListViewCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
var
  TextRect: TRect;
begin
  if TListView(Sender).Items.Count = 0 then
  begin
    TextRect := ARect;
    InflateRect(TextRect, -40, -50);
    TListView(Sender).Canvas.Font.Color := clGrayText;
    DrawText(TListView(Sender).Canvas.Handle, PChar(SNoItemsToView),
      Length(SNoItemsToView), TextRect, DT_CENTER or DT_NOPREFIX or DT_WORDBREAK);
  end;
end;

procedure TTranslatableEditor.ListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Odd(Item.Index) then
    TListView(Sender).Canvas.Brush.Color := clAlternative;
end;

procedure TTranslatableEditor.FilterDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Target: TTranslatablesUI;
  Filter: TTranslatablesUI.TFilterType;
  Info: String;
  C, S: Integer;
  R: TRect;
begin
  if Control = PropertyFilter then
    Target := Properties
  else
    Target := Literals;
  with TComboBox(Control) do
  begin
    Filter := Target.FilterOfIndex(Index);
    if Odd(Index) and (([odComboBoxEdit, odSelected] * State) = []) then
      Canvas.Brush.Color := clAlternative;
    Canvas.FillRect(Rect);
    InflateRect(Rect, -2, -1);
    ImageList.Draw(Canvas, Rect.Left,
      (Rect.Top + Rect.Bottom - ImageList.Height) div 2, Ord(Filter) - 1);
    Inc(Rect.Left, ImageList.Width + 2);
    Dec(Rect.Right, 2);
    Info := Items[Index];
    DrawText(Canvas.Handle, PChar(Info), Length(Info), Rect,
      DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    if Filter = ftCustom then
    begin
      Inc(Rect.Left, Canvas.TextWidth(Info) + 8);
      if not (odSelected in State) then
        Canvas.Font.Color := clGrayText;
      Canvas.Font.Style := [];
      Info := Target.CustomFilter;
      DrawText(Canvas.Handle, PChar(Info), Length(Info), Rect,
        DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
      Canvas.Font.Style := [fsBold];
    end;
    if not (odSelected in State) then
      Canvas.Font.Color := clHotLight;
    Canvas.Font.Size := Canvas.Font.Size - 1;
    if odComboBoxEdit in State then
    begin
      Info := FormatFloat('#,##0', Target.Stats.Selected[Filter]) + ' / '
            + FormatFloat('#,##0', Target.Stats.Total[Filter]);
      DrawText(Canvas.Handle, PChar(Info), Length(Info), Rect,
        DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    end
    else
    begin
      S := Canvas.TextWidth(' ');
      C := Rect.Right - Length(FormatFloat('#,##0', Target.Stats.Total[ftNone])) * S * 3;
      SetRect(R, C - S, Rect.Top, C + S, Rect.Bottom);
      DrawText(Canvas.Handle, '/', 1, R,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
      SetRect(R, C + S, Rect.Top, Rect.Right, Rect.Bottom);
      Info := FormatFloat('#,##0', Target.Stats.Total[Filter]);
      DrawText(Canvas.Handle, PChar(Info), Length(Info), R,
        DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
      SetRect(R, Rect.Left, Rect.Top, C - S - S div 2, Rect.Bottom);
      Info := FormatFloat('#,##0', Target.Stats.Selected[Filter]);
      DrawText(Canvas.Handle, PChar(Info), Length(Info), R,
        DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    end;
  end;
end;

procedure TTranslatableEditor.LanguageBoxSelect(Sender: TObject);
begin
  btnExportTo.Enabled := Assigned(TextLanguage.ItemSelected);
end;

// Properties

procedure TTranslatableEditor.ReadProperties;
begin
  GetTranslatableProperties(Translatables.DomainOwner, Properties.All);
  Properties.SetSelection(Translatables.Properties);
  Properties.All.Sort;
end;

procedure TTranslatableEditor.WriteProperties;
var
  UpdatedProperties: TTextItems;
begin
  Properties.GetSelection(Translatables.Properties);
  UpdatedProperties := TTextItems.Create;
  try
    Properties.GetUpdated(UpdatedProperties);
    SetTranslatableProperties(Translatables.DomainOwner, UpdatedProperties);
  finally
    UpdatedProperties.Free;
  end;
end;

procedure TTranslatableEditor.PropertyFilterSelect(Sender: TObject);
begin
  Properties.ActivateFilterByIndex(PropertyFilter.ItemIndex, SPropertyFilterPrompt);
end;

procedure TTranslatableEditor.PropertyListItemChecked(Sender: TObject;
  Item: TListItem);
begin
  if not Properties.SetChecked(TTextItem(Item.Data), Item.Checked) then
    Item.Checked := Properties.GetChecked(TTextItem(Item.Data));
end;

procedure TTranslatableEditor.PropertyListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected and Assigned(Item) then
    Properties.SelectedItem := TTextItem(Item.Data)
  else
  begin
    Properties.SelectedItem := nil;
    if Assigned(PropertyList.ItemFocused) then
      PropertyList.ItemFocused.Focused := False;
  end;
end;

procedure TTranslatableEditor.PropertyValueExit(Sender: TObject);
begin
  Properties.UpdateSelectedItemValue;
end;

procedure TTranslatableEditor.PropertyCommentsExit(Sender: TObject);
begin
  Properties.UpdateSelectedItemComment;
end;

procedure TTranslatableEditor.PropertiesTabSetChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  PropertyValue.Visible := (NewTab = 0);
  PropertyComments.Visible := (NewTab = 1);
end;

procedure TTranslatableEditor.PropsCheckAllExecute(Sender: TObject);
begin
  Properties.SetAllState(cbChecked);
end;

procedure TTranslatableEditor.PropsCheckAllUpdate(Sender: TObject);
begin
  PropsCheckAll.Enabled := Properties.CanSetAllState(cbChecked);
end;

procedure TTranslatableEditor.PropsUncheckAllExecute(Sender: TObject);
begin
  Properties.SetAllState(cbUnchecked);
end;

procedure TTranslatableEditor.PropsUncheckAllUpdate(Sender: TObject);
begin
  PropsUncheckAll.Enabled := Properties.CanSetAllState(cbUnchecked);
end;

procedure TTranslatableEditor.PropsInvertCheckExecute(Sender: TObject);
begin
  Properties.SetAllState(cbGrayed);
end;

procedure TTranslatableEditor.PropsInvertCheckUpdate(Sender: TObject);
begin
  PropsInvertCheck.Enabled := Properties.CanSetAllState(cbGrayed);
end;

// Literals

procedure TTranslatableEditor.ReadLiterals;
var
  SourceCode: String;
  ParseError: String;
begin
  if ideGetActiveEditorText(SourceCode) then
  begin
    try
      LiteralCollector := TPascalStringCollector.Create(SourceCode, Translatables.Translator);
    except
      on E: Exception do
      begin
        CollectorErrorMsg := Format(SParserError, [ParseError, E.Message]);
        LiteralCollector := nil;
      end;
    end;
    if Assigned(LiteralCollector) then
    begin
      LiteralCollector.GetTranslatableStrings(Literals.All);
      Literals.SetSelection(Translatables.Literals);
    end;
  end;
end;

procedure TTranslatableEditor.WriteLiterals;
var
  SourceCode: String;
begin
  if Assigned(LiteralCollector) then
  begin
    Literals.GetSelection(Translatables.Literals);
    SourceCode := LiteralCollector.SetTranslatableStrings(Translatables.Literals);
    ideSetActiveEditorText(TrimRight(SourceCode));
  end;
end;

procedure TTranslatableEditor.LiteralFilterSelect(Sender: TObject);
begin
  Literals.ActivateFilterByIndex(LiteralFilter.ItemIndex, SLiteralFilterPrompt);
end;

procedure TTranslatableEditor.LiteralListCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
var
  TextRect: TRect;
  ErrorMsg: String;
begin
  if not Assigned(LiteralCollector) then
  begin
    TextRect := ARect;
    InflateRect(TextRect, -40, -50);
    TListView(Sender).Canvas.Font.Color := clRed;
    TListView(Sender).Canvas.Font.Size := 12;
    ErrorMsg := SCollectError;
    if CollectorErrorMsg <> '' then
      ErrorMsg := ErrorMsg + #13#10 + CollectorErrorMsg;
    DrawText(TListView(Sender).Canvas.Handle, PChar(ErrorMsg), Length(ErrorMsg),
      TextRect, DT_CENTER or DT_NOPREFIX or DT_WORDBREAK);
  end
  else
    ListViewCustomDraw(Sender, ARect, DefaultDraw);
end;

procedure TTranslatableEditor.LiteralListItemChecked(Sender: TObject;
  Item: TListItem);
begin
  if not Literals.SetChecked(TTextItem(Item.Data), Item.Checked) then
    Item.Checked := Literals.GetChecked(TTextItem(Item.Data));
end;

procedure TTranslatableEditor.LiteralListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  I: Integer;
begin
  if Selected and Assigned(Item) then
    Literals.SelectedItem := TTextItem(Item.Data)
  else
  begin
    Literals.SelectedItem := nil;
    if Assigned(LiteralList.ItemFocused) then
      LiteralList.ItemFocused.Focused := False;
  end;
  if Assigned(Literals.SelectedItem) and Literals.SelectedItem.HasPluralForms then
  begin
    if LiteralPlurals.Tabs.Count <> Literals.SelectedItem.PluralCount then
    begin
      LiteralPlurals.Tabs.BeginUpdate;
      LiteralPlurals.Tabs.Clear;
      for I := 0 to Literals.SelectedItem.PluralCount - 1 do
        LiteralPlurals.Tabs.Add(Format(' plural=%d ', [I]));
      LiteralPlurals.Tabs.EndUpdate;
    end;
    LiteralPlurals.TabIndex := Literals.PluralIndex;
    LiteralValue.Height := LiteralPlurals.BoundsRect.Top - LiteralValue.Top + 1;
    LiteralPlurals.Visible := True;
  end
  else if LiteralPlurals.Visible then
  begin
    LiteralValue.Height := LiteralPlurals.BoundsRect.Bottom - LiteralValue.Top - 1;
    LiteralPlurals.Visible := False;
  end;
end;

procedure TTranslatableEditor.LiteralPluralsChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  Literals.PluralIndex := NewTab;
end;

procedure TTranslatableEditor.LiteralValueExit(Sender: TObject);
begin
  Literals.UpdateSelectedItemValue;
end;

procedure TTranslatableEditor.LiteralCommentsExit(Sender: TObject);
begin
  Literals.UpdateSelectedItemComment;
end;

procedure TTranslatableEditor.LiteralsTabSetChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  LiteralValue.Visible := (NewTab = 0);
  LiteralComments.Visible := (NewTab = 1);
end;

procedure TTranslatableEditor.LiteralsCheckAllExecute(Sender: TObject);
begin
  Literals.SetAllState(cbChecked);
end;

procedure TTranslatableEditor.LiteralsCheckAllUpdate(Sender: TObject);
begin
  LiteralsCheckAll.Enabled := Literals.CanSetAllState(cbChecked);
end;

procedure TTranslatableEditor.LiteralsUncheckAllExecute(Sender: TObject);
begin
  Literals.SetAllState(cbUnchecked);
end;

procedure TTranslatableEditor.LiteralsUncheckAllUpdate(Sender: TObject);
begin
  LiteralsUncheckAll.Enabled := Literals.CanSetAllState(cbUnchecked);
end;

procedure TTranslatableEditor.LiteralsInvertCheckExecute(Sender: TObject);
begin
  Literals.SetAllState(cbGrayed);
end;

procedure TTranslatableEditor.LiteralsInvertCheckUpdate(Sender: TObject);
begin
  LiteralsInvertCheck.Enabled := Literals.CanSetAllState(cbGrayed);
end;

// Export

procedure TTranslatableEditor.AddTranslatablesTo(Catalog: TTranslationCatalog;
  KeepObsoleteEntries: Boolean);
var
  TextItems: TTextItems;
  TextDomain: TTextDomain;
begin
  TextItems := TTextItems.Create;
  try
    TextDomain := Catalog.TextDomains.Add(Translatables.DomainName);
    Properties.GetSelection(TextItems);
    TextDomain.Properties.Revise(TextItems, KeepObsoleteEntries);
    Literals.GetSelection(TextItems);
    TextDomain.Literals.Revise(TextItems, KeepObsoleteEntries);
  finally
    TextItems.Free;
  end;
end;

procedure TTranslatableEditor.ExportTo(const FileName: String);
var
  Catalog: TTranslationCatalog;
  ReplaceReason: String;
  WarningMsg: String;
begin
  Catalog := TTranslationCatalog.Create;
  try
    if FileExists(FileName) then
    begin
      ReplaceReason := '';
      try
        Catalog.LoadFromFile(FileName);
        if not Catalog.Empty and (Catalog.NativeCulture <> TextLanguage.ItemSelected) then
          ReplaceReason := SLanguageMismatch;
      except
        ReplaceReason := SContentMismatch;
      end;
      WarningMsg := Format(SFileExists, [FileName]);
      if ReplaceReason <> '' then
        WarningMsg := WarningMsg + ' ' + ReplaceReason;
      WarningMsg := WarningMsg + #13#10#13#10;
      if ReplaceReason <> '' then
        WarningMsg := WarningMsg + SReplaceAction
      else
        WarningMsg := WarningMsg + SUpdateAction;
      WarningMsg := WarningMsg + #13#10 + SCancelAction;
      if MessageDlg(WarningMsg, mtWarning, [mbOK, mbCancel], 0) <> mrOK then
        Exit;
      if ReplaceReason <> '' then
        Catalog.Clear;
    end;
    Catalog.NativeCulture := TextLanguage.ItemSelected;
    AddTranslatablesTo(Catalog, False);
    Catalog.SaveToFile(FileName);
  finally
    Catalog.Free;
  end;
end;

procedure TTranslatableEditor.btnExportToClick(Sender: TObject);
var
  FileName: String;
begin
  if SaveDialog.FileName = '' then
  begin
    FileName := TextLanguage.ItemSelected.Locale;
    if Assigned(Translatables.DomainOwner) and
      (Translatables.DomainOwner.UnitName <> '')
    then
      FileName := Translatables.DomainOwner.UnitName + '.' + FileName;
    SaveDialog.FileName := FileName;
  end;
  if SaveDialog.Execute then
  begin
    ExportTo(SaveDialog.FileName);
    ModalResult := mrOK;
  end;
end;

end.
