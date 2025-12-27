{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz Khojasteh                                                         }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit DefinitionList;

interface

uses
  Windows, SysUtils, Types, Classes, Contnrs, ComCtrls, i18nCore,
  i18nHashList, i18nCatalog;

type

  TDefinitionField = (dfDomain, dfOriginal, dfTranslated, dfState, dfComment,
    dfNote, dfName, dfPluralForms);

  TDefinitionFieldColumnMap = array[TDefinitionField] of Integer;
  PDefinitionFieldColumnMap = ^TDefinitionFieldColumnMap;

  TTextSizeGroup = (tgShort, tgMedium, tgLong);

  TDefinitionList = class;
  TGroupNode = class;
  TItemNode = class;

  TGroupItemsEnumerator = class(TObject)
  private
    Index: Integer;
    Group: TGroupNode;
  public
    constructor Create(AGroup: TGroupNode);
    function GetCurrent: TItemNode;
    function MoveNext: Boolean;
    property Current: TItemNode read GetCurrent;
  end;

  TListNode = class(TObject)
  private
    fOwner: TDefinitionList;
    fLVItem: TListItem;
    function GetShowing: Boolean; inline;
  protected
    function GetListIndex: Integer; virtual; abstract;
  public
    constructor Create(AOwner: TDefinitionList);
    procedure Update; virtual;
    procedure Delete; virtual;
    procedure Select; virtual;
    property Owner: TDefinitionList read fOwner;
    property Showing: Boolean read GetShowing;
    property ListIndex: Integer read GetListIndex;
    property LVItem: TListItem read fLVItem write fLVItem;
  end;

  TGroupNode = class(TListNode)
  private
    fID: Integer;
    fExpanded: Boolean;
    fItems: TObjectList;
    function GetItems(Index: Integer): TItemNode; inline;
    function GetCount: Integer; inline;
    function GetApprovedCount: Integer;
    procedure SetExpanded(Value: Boolean);
    function GetOwnsItems: Boolean; inline;
    procedure SetOwnItems(Value: Boolean); inline;
  protected
    function GetListIndex: Integer; override;
    procedure SetActiveCulture(Culture: TCultureInfo);
  public
    constructor Create(AOwner: TDefinitionList; ID: Integer); reintroduce;
    destructor Destroy; override;
    function GetEnumerator: TGroupItemsEnumerator;
    function First: TItemNode;
    function Last: TItemNode;
    procedure AddItem(Item: TItemNode);
    procedure RemoveItem(Item: TItemNode); inline;
    procedure DeleteItem(Index: Integer); inline;
    function IndexOf(Item: TItemNode): Integer; inline;
    function UpdateItemsBy(Translations: TList): Integer;
    function ConditionalCount(Condition: TTranslationStates): Integer;
    property ID: Integer read fID;
    property Count: Integer read GetCount;
    property ApprovedCount: Integer read GetApprovedCount;
    property Expanded: Boolean read fExpanded write SetExpanded;
    property OwnsItems: Boolean read GetOwnsItems write SetOwnItems;
    property Items[Index: Integer]: TItemNode read GetItems;
  end;

  TItemNode = class(TListNode)
  private
    fDefinition: TTextDefinition;
    fTranslation: TTextTranslation;
    fComponentName: String;
    fGroup: TGroupNode;
    function GetTextDomain: TTextDomain; inline;
    function GetGroupIndex: Integer; inline;
  protected
    function GetListIndex: Integer; override;
    procedure SetGroup(Group: TGroupNode); inline;
    procedure SetActiveCulture(Culture: TCultureInfo); inline;
    function FindGroupID(Field: TDefinitionField): Integer;
  public
    constructor Create(AOwner: TDefinitionList; ADefinition: TTextDefinition); reintroduce;
    procedure Update; override;
    function Compare(Item: TItemNode; Field: TDefinitionField): Integer;
    function IsComponent: Boolean; inline;
    function IsApproved: Boolean; inline;
    function IsGoogleTranslated: Boolean; inline;
    property Definition: TTextDefinition read fDefinition;
    property Translation: TTextTranslation read fTranslation;
    property TextDomain: TTextDomain read GetTextDomain;
    property ComponentName: String read fComponentName;
    property Group: TGroupNode read fGroup;
    property GroupIndex: Integer read GetGroupIndex;
  end;

  TGroupList = class(TObjectList)
  private
    function GetItems(Index: Integer): TGroupNode; inline;
    function GetItemsByID(ID: Integer): TGroupNode;
  public
    procedure Add(Group: TGroupNode);
    property ByID[ID: Integer]: TGroupNode read GetItemsByID;
    property Items[Index: Integer]: TGroupNode read GetItems; default;
  end;

  TDefinitionList = class(TObject)
  private
    fListView: TListView;
    fSortField: TDefinitionField;
    fSortAscending: Boolean;
    fSortGrouping: Boolean;
    fActiveCulture: TCultureInfo;
    fGroups: TGroupList;
    fSortImmediately: Boolean;
    NeedsReporder: Boolean;
    UpdateCount: Integer;
    ColMap: PDefinitionFieldColumnMap;
    function GetSortColumn: Integer;
    procedure SetSortColumn(Value: Integer);
    procedure SetSortField(Value: TDefinitionField);
    procedure SetSortAscending(Value: Boolean);
    procedure SetSortGrouping(Value: Boolean);
    procedure SetSortImmediately(Value: Boolean);
    procedure SetActiveCulture(Value: TCultureInfo);
    function GetCount: Integer;
    function GetListCount: Integer;
  protected
    procedure AddItem(Item: TItemNode);
    procedure LoadDictionary(TextDictionary: TTextDictionary);
  public
    constructor Create(ListView: TListView; pColMap: PDefinitionFieldColumnMap = nil);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure Load(TextDomains: TTextDomains); overload;
    procedure Load(TextDomain: TTextDomain); overload;
    procedure Add(Item: TItemNode);
    procedure Remove(Node: TListNode);
    function Reorder(Item: TItemNode): Boolean;
    procedure ExpandAll;
    procedure CollapseAll;
    function IsAnyGroupExpanded: Boolean;
    function IsAnyGroupCollapsed: Boolean;
    procedure Refresh;
    procedure Invalidate;
    procedure Rearrange;
    procedure UpdateHeaderArrow;
    procedure Prepare(LVItem: TListItem);
    procedure PrepareRange(StartIndex, EndIndex: Integer);
    function NoteAt(ListIndex: Integer): TListNode;
    function IsShowing(Node: TListNode): Boolean;
    procedure Select(Node: TListNode);
    procedure Update(Node: TListNode);
    procedure UpdateBy(Translations: TList);
    function ConditionalCount(Condition: TTranslationStates; ExpandedOnly: Boolean): Integer;
    property ListView: TListView read fListView;
    property Groups: TGroupList read fGroups;
    property SortColumn: Integer read GetSortColumn write SetSortColumn;
    property SortField: TDefinitionField read fSortField write SetSortField;
    property SortAscending: Boolean read fSortAscending write SetSortAscending;
    property SortGrouping: Boolean read fSortGrouping write SetSortGrouping;
    property ActiveCulture: TCultureInfo read fActiveCulture write SetActiveCulture;
    property SortImmediately: Boolean read fSortImmediately write SetSortImmediately;
    property Count: Integer read GetCount;
    property ListCount: Integer read GetListCount;
  end;

implementation

uses
  i18nUtils, CommCtrl;

{ Helper Functions }

type
  THeaderArrow = (haNoArrow, haDownArrow, haUpArrow);

function SetSortArrow(hListView: THandle; idxColumn: Integer;
  Arrow: THeaderArrow): Boolean;
const
  HDF_SORTDOWN = $0200;
  HDF_SORTUP   = $0400;
var
  hHeader: THandle;
  hdrItem: HD_ITEM;
begin
  Result := False;
	hHeader := ListView_GetHeader(hListView);
	if hHeader <> 0 then
  begin
		hdrItem.mask := HDI_FORMAT;
		if Header_GetItem(hHeader, idxColumn, hdrItem) then
    begin
      case Arrow of
        haUpArrow:
  				hdrItem.fmt := (hdrItem.fmt and not HDF_SORTDOWN) or HDF_SORTUP;
        haDownArrow:
  				hdrItem.fmt := (hdrItem.fmt and not HDF_SORTUP) or HDF_SORTDOWN;
			else
				hdrItem.fmt := hdrItem.fmt and not (HDF_SORTDOWN or HDF_SORTUP);
      end;
      Result := Header_SetItem(hHeader, idxColumn, hdrItem);
		end;
	end;
end;

{ TGroupItemsEnumerator }

constructor TGroupItemsEnumerator.Create(AGroup: TGroupNode);
begin
  Group := AGroup;
  Index := -1;
end;

function TGroupItemsEnumerator.GetCurrent: TItemNode;
begin
  if Group.Owner.SortAscending then
    Result := Group.Items[Index]
  else
    Result := Group.Items[Group.Count - Index - 1];
end;

function TGroupItemsEnumerator.MoveNext: Boolean;
begin
  Result := (Index < Group.Count - 1);
  if Result then
    Inc(Index);
end;

{ TListNode }

constructor TListNode.Create(AOwner: TDefinitionList);
begin
  fOwner := AOwner;
end;

function TListNode.GetShowing: Boolean;
begin
  Result := Owner.IsShowing(Self);
end;

procedure TListNode.Update;
begin
  Owner.Update(Self);
end;

procedure TListNode.Delete;
begin
  Owner.Remove(Self);
end;

procedure TListNode.Select;
begin
  Owner.Select(Self);
end;

{ TGroupNode }

constructor TGroupNode.Create(AOwner: TDefinitionList; ID: Integer);
begin
  inherited Create(AOwner);
  fID := ID;
  fExpanded := True;
  fItems := TObjectList.Create(False);
end;

destructor TGroupNode.Destroy;
begin
  fItems.Free;
  inherited Destroy;
end;

function TGroupNode.GetEnumerator: TGroupItemsEnumerator;
begin
  Result := TGroupItemsEnumerator.Create(Self);
end;

function TGroupNode.GetItems(Index: Integer): TItemNode;
begin
  Result := TItemNode(fItems[Index]);
end;

function TGroupNode.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TGroupNode.GetApprovedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].IsApproved then
      Inc(Result);
end;

function TGroupNode.GetListIndex: Integer;
var
  I: Integer;
  Group: TGroupNode;
begin
  Result := -1;
  if Owner.SortGrouping then
  begin
    if Owner.SortAscending then
    begin
      for I := 0 to Owner.Groups.Count - 1 do
      begin
        Inc(Result);
        Group := Owner.Groups[I];
        if Group = Self then
          Break
        else if Group.Expanded then
          Inc(Result, Group.Count);
      end;
    end
    else // if not SortAscending
    begin
      for I := Owner.Groups.Count - 1 downto 0 do
      begin
        Inc(Result);
        Group := Owner.Groups[I];
        if Group = Self then
          Break
        else if Group.Expanded then
          Inc(Result, Group.Count);
      end;
    end;
  end;
end;

procedure TGroupNode.SetActiveCulture(Culture: TCultureInfo);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].SetActiveCulture(Culture);
end;

procedure TGroupNode.SetExpanded(Value: Boolean);
begin
  if Expanded <> Value then
  begin
    fExpanded := Value;
    Owner.Refresh;
  end;
end;

function TGroupNode.GetOwnsItems: Boolean;
begin
  Result := fItems.OwnsObjects;
end;

procedure TGroupNode.SetOwnItems(Value: Boolean);
begin
  fItems.OwnsObjects := Value;
end;

function TGroupNode.First: TItemNode;
begin
  if Count = 0 then
    Result := nil
  else if Owner.SortAscending then
    Result := Items[0]
  else
    Result := Items[Count - 1];
end;

function TGroupNode.Last: TItemNode;
begin
  if Count = 0 then
    Result := nil
  else if Owner.SortAscending then
    Result := Items[Count - 1]
  else
    Result := Items[0];
end;

procedure TGroupNode.AddItem(Item: TItemNode);
var
  FirstIndex, LastIndex, Index: Integer;
  Compare: Integer;
begin
  FirstIndex := 0;
  LastIndex := Count - 1;
  Index := (FirstIndex + LastIndex + 1) shr 1;
  while FirstIndex <= LastIndex do
  begin
    Compare := Item.Compare(Items[Index], Owner.SortField);
    if Compare < 0 then
      LastIndex := Index - 1
    else if Compare > 0 then
      FirstIndex := Index + 1
    else
      Break;
    Index := (FirstIndex + LastIndex + 1) shr 1;
  end;
  fItems.Insert(Index, Item);
  Item.SetGroup(Self);
end;

procedure TGroupNode.RemoveItem(Item: TItemNode);
begin
  Item.SetGroup(nil);
  fItems.Remove(Item);
end;

procedure TGroupNode.DeleteItem(Index: Integer);
begin
  Items[Index].SetGroup(nil);
  fItems.Delete(Index);
end;

function TGroupNode.IndexOf(Item: TItemNode): Integer;
begin
  Result := fItems.IndexOf(Item);
end;

function TGroupNode.UpdateItemsBy(Translations: TList): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Translations.Count <> 0 then
  begin
    for I := 0 to Count - 1 do
      if Translations.IndexOf(Items[I].Translation) >= 0 then
      begin
        Items[I].Update;
        Inc(Result);
        if Result = Translations.Count then
          Exit;
      end;
  end;
end;

function TGroupNode.ConditionalCount(Condition: TTranslationStates): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    with Items[I] do
      if Assigned(Translation) and (Translation.State in Condition) then
        Inc(Result);
end;

{ TItemNode }

constructor TItemNode.Create(AOwner: TDefinitionList;
  ADefinition: TTextDefinition);
begin
  inherited Create(AOwner);
  fDefinition := ADefinition;
  if fDefinition.Dictionary.Kind = dkProperty then
    fComponentName := ExtractComponentName(fDefinition.ID);
end;

function TItemNode.GetListIndex: Integer;
begin
  if Owner.SortAscending then
    Result := Group.ListIndex + GroupIndex + 1
  else
    Result := Group.ListIndex + Group.Count - GroupIndex;
end;

function TItemNode.GetGroupIndex: Integer;
begin
  Result := Group.IndexOf(Self);
end;

function TItemNode.GetTextDomain: TTextDomain;
begin
  Result := Definition.Dictionary.TextDomain;
end;

procedure TItemNode.SetGroup(Group: TGroupNode);
begin
  fGroup := Group;
end;

procedure TItemNode.SetActiveCulture(Culture: TCultureInfo);
begin
  if Assigned(Culture) then
    fTranslation := Definition.Translations[Culture.Locale]
  else
    fTranslation := nil;
end;

function TItemNode.IsComponent: Boolean;
begin
  Result := (ComponentName <> '');
end;

function TItemNode.IsApproved: Boolean;
begin
  Result := Assigned(Translation) and Translation.IsApproved;
end;

function TItemNode.IsGoogleTranslated: Boolean;
begin
  Result := Assigned(Translation) and (Translation.State = tsGoogle);
end;

function TItemNode.Compare(Item: TItemNode; Field: TDefinitionField): Integer;
begin
  case Field of
    dfDomain:
      Result := CompareStr(TextDomain.Name, Item.TextDomain.Name);
    dfOriginal:
      Result := CompareText(Definition.Value, Item.Definition.Value);
    dfComment:
      Result := CompareText(Definition.Comment, Item.Definition.Comment);
    dfTranslated:
      if Assigned(Translation) then
        if Assigned(Item.Translation) then
          Result := CompareText(Translation.Value, Item.Translation.Value)
        else
          Result := +1
      else if Assigned(Item.Translation) then
        Result := -1
      else
        Result := 0;
    dfState:
      if Assigned(Translation) then
        if Assigned(Item.Translation) then
          Result := Ord(Translation.State) - Ord(Item.Translation.State)
        else
          Result := +1
      else if Assigned(Item.Translation) then
        Result := -1
      else
        Result := 0;
    dfNote:
      if Assigned(Translation) then
        if Assigned(Item.Translation) then
          Result := CompareText(Translation.Note, Item.Translation.Note)
        else
          Result := +1
      else if Assigned(Item.Translation) then
        Result := -1
      else
        Result := 0;
    dfName:
      if Definition.Dictionary.Kind = dkProperty then
        if Item.Definition.Dictionary.Kind = dkProperty then
          Result := CompareText(Definition.ID, Item.Definition.ID)
        else
          Result := +1
      else if Item.Definition.Dictionary.Kind = dkProperty then
        Result := -1
      else
        Result := 0;
    dfPluralForms:
      Result := Ord(Definition.HasPluralForms) - Ord(Item.Definition.HasPluralForms);
  else
    Result := 0;
  end;
  if Result = 0 then
    if Cardinal(Self) > Cardinal(Item) then
      Result := +1
    else
      Result := -1;
end;

function TItemNode.FindGroupID(Field: TDefinitionField): Integer;

  function GetTextGroup(const Text: String): Integer;
  var
    Len: Integer;
  begin
    Len := Length(Text);
    if Len <= 80 then
      Result := Ord(tgShort)
    else if Len <= 256 then
      Result := Ord(tgMedium)
    else
      Result := Ord(tgLong);
  end;

begin
  case Field of
    dfDomain:
      Result := TextDomain.Index;
    dfOriginal:
      Result := GetTextGroup(Definition.Value);
    dfComment:
      Result := Ord(Definition.Comment <> '');
    dfTranslated:
      if Assigned(Translation) then
        Result := GetTextGroup(Translation.Value)
      else
        Result := 0;
    dfState:
      if Assigned(Translation) then
        Result := Ord(Translation.State)
      else
        Result := 0;
    dfNote:
      if Assigned(Translation) then
        Result := Ord(Translation.Note <> '')
      else
        Result := 0;
    dfName:
      Result := Ord(Definition.Dictionary.Kind);
    dfPluralForms:
      Result := Ord(Definition.HasPluralForms);
  else
    Result := 0;
  end;
end;

procedure TItemNode.Update;
begin
  if not (Owner.SortField in [dfTranslated, dfState, dfNote]) or
     not Owner.Reorder(Self) then
  begin
    inherited Update;
    if Owner.SortGrouping then
      Group.Update;
  end
  else
    Select;
end;

{ TGroupList }

function TGroupList.GetItems(Index: Integer): TGroupNode;
begin
  Result := TGroupNode(inherited Items[Index]);
end;

function TGroupList.GetItemsByID(ID: Integer): TGroupNode;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.ID = ID then
      Exit;
  end;
  Result := nil;
end;

procedure TGroupList.Add(Group: TGroupNode);
var
  I, Index: Integer;
begin
  Index := 0;
  for I := Count - 1 downto 0 do
    if Group.ID > Items[I].ID then
    begin
      Index := I + 1;
      Break;
    end;
  Insert(Index, Group);
end;

{ TDefinitionList }

constructor TDefinitionList.Create(ListView: TListView;
  pColMap: PDefinitionFieldColumnMap);
begin
  fGroups := TGroupList.Create(True);
  fListView := ListView;
  fSortField := dfDomain;
  fSortAscending := True;
  fSortGrouping := True;
  fSortImmediately := True;
  ColMap := pColMap;
end;

destructor TDefinitionList.Destroy;
begin
  Clear;
  fGroups.Free;
  inherited Destroy;
end;

function TDefinitionList.GetSortColumn: Integer;
begin
  if Assigned(ColMap) then
    Result := ColMap^[SortField]
  else
    Result := Ord(SortField);
end;

procedure TDefinitionList.SetSortColumn(Value: Integer);
var
  Field: TDefinitionField;
begin
  if Assigned(ColMap) then
  begin
    for Field := Low(TDefinitionField) to High(TDefinitionField) do
      if ColMap^[Field] = Value then
      begin
        SortField := Field;
        Exit;
      end;
  end
  else
    SortField := TDefinitionField(Value);
end;

procedure TDefinitionList.SetSortField(Value: TDefinitionField);
begin
  if SortField <> Value then
  begin
    if (SortColumn >= 0) and ListView.HandleAllocated then
      SetSortArrow(ListView.Handle, SortColumn, haNoArrow);
    fSortField := Value;
    UpdateHeaderArrow;
    Rearrange;
  end;
end;

procedure TDefinitionList.SetSortAscending(Value: Boolean);
begin
  if SortAscending <> Value then
  begin
    fSortAscending := Value;
    UpdateHeaderArrow;
    if NeedsReporder then
      Rearrange
    else
      Invalidate;
  end;
end;

procedure TDefinitionList.SetSortGrouping(Value: Boolean);
begin
  if SortGrouping <> Value then
  begin
    fSortGrouping := Value;
    Rearrange;
  end;
end;

procedure TDefinitionList.SetSortImmediately(Value: Boolean);
begin
  if SortImmediately <> Value then
  begin
    fSortImmediately := Value;
    if SortImmediately and NeedsReporder then
      Rearrange;
  end;
end;

procedure TDefinitionList.SetActiveCulture(Value: TCultureInfo);
var
  I: Integer;
begin
  if ActiveCulture <> Value then
  begin
    fActiveCulture := Value;
    for I := 0 to Groups.Count - 1 do
      Groups[I].SetActiveCulture(ActiveCulture);
    if SortField in [dfTranslated, dfState, dfNote] then
      Rearrange
    else
      Invalidate;
  end;
end;

function TDefinitionList.GetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Groups.Count - 1 do
    Inc(Result, Groups[I].Count);
end;

function TDefinitionList.GetListCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Groups.Count - 1 do
  begin
    Inc(Result);
    if Groups[I].Expanded then
      Inc(Result, Groups[I].Count);
  end;
  if not SortGrouping and (Result <> 0) then
    Dec(Result);
end;

procedure TDefinitionList.AddItem(Item: TItemNode);
var
  GroupID: Integer;
  Group: TGroupNode;
begin
  if SortGrouping then
    GroupID := Item.FindGroupID(SortField)
  else
    GroupID := 0;
  Group := Groups.ByID[GroupID];
  if not Assigned(Group) then
  begin
    Group := TGroupNode.Create(Self, GroupID);
    Groups.Add(Group);
  end;
  Group.AddItem(Item);
end;

procedure TDefinitionList.Rearrange;
var
  OldGroups: TGroupList;
  I, J: Integer;
begin
  NeedsReporder := False;
  if Groups.Count <> 0 then
  begin
    OldGroups := Groups;
    fGroups := TGroupList.Create(True);
    for I := 0 to OldGroups.Count - 1 do
      for J := 0 to OldGroups[I].Count - 1 do
        AddItem(OldGroups[I].Items[J]);
    OldGroups.Free;
    Refresh;
  end;
end;

procedure TDefinitionList.LoadDictionary(TextDictionary: TTextDictionary);
var
  Definition: TTextDefinition;
  Item: TItemNode;
begin
  Definition := TextDictionary.First;
  while Assigned(Definition) do
  begin
    Item := TItemNode.Create(Self, Definition);
    Item.SetActiveCulture(ActiveCulture);
    AddItem(Item);
    Definition := Definition.Next;
  end;
end;

procedure TDefinitionList.BeginUpdate;
begin
  if UpdateCount = 0 then
    ListView.Items.BeginUpdate;
  Inc(UpdateCount);
end;

procedure TDefinitionList.EndUpdate;
begin
  Dec(UpdateCount);
  if UpdateCount = 0 then
  begin
    Refresh;
    ListView.Items.EndUpdate;
  end;
end;

procedure TDefinitionList.Clear;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Groups.Count - 1 do
      Groups[I].OwnsItems := True;
    Groups.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TDefinitionList.Load(TextDomains: TTextDomains);
var
  TextDomain: TTextDomain;
begin
  BeginUpdate;
  try
    Clear;
    TextDomain := TextDomains.First;
    while Assigned(TextDomain) do
    begin
      LoadDictionary(TextDomain.Properties);
      LoadDictionary(TextDomain.Literals);
      TextDomain := TextDomain.Next;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TDefinitionList.Load(TextDomain: TTextDomain);
begin
  BeginUpdate;
  try
    Clear;
    LoadDictionary(TextDomain.Properties);
    LoadDictionary(TextDomain.Literals);
  finally
    EndUpdate;
  end;
end;

procedure TDefinitionList.Add(Item: TItemNode);
begin
  Item.SetActiveCulture(ActiveCulture);
  AddItem(Item);
  Refresh;
end;

procedure TDefinitionList.Remove(Node: TListNode);
var
  Group: TGroupNode;
  Item: TItemNode;
begin
  if Node is TGroupNode then
  begin
    Group := TGroupNode(Node);
    Group.OwnsItems := True;
    Groups.Remove(Group);
  end
  else
  begin
    Item := TItemNode(Node);
    Group := Item.Group;
    Group.OwnsItems := True;
    if Group.Count = 1 then
      Groups.Remove(Group)
    else
    begin
      Group.RemoveItem(Item);
      Group.OwnsItems := False;
    end;
  end;
  Refresh;
end;

function TDefinitionList.Reorder(Item: TItemNode): Boolean;
var
  OldGroup: TGroupNode;
  OldGroupIndex: Integer;
begin
  if SortImmediately then
  begin
    Result := True;
    OldGroup := Item.Group;
    OldGroupIndex := Item.GroupIndex;
    OldGroup.DeleteItem(OldGroupIndex);
    AddItem(Item);
    if OldGroup.Count = 0 then
      Groups.Remove(OldGroup);
    if OldGroup <> Item.Group then
      Refresh
    else if OldGroupIndex <> Item.GroupIndex then
      Invalidate
    else
      Result := False;
  end
  else
  begin
    NeedsReporder := True;
    Result := False;
  end;
end;

procedure TDefinitionList.ExpandAll;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Groups.Count - 1 do
      Groups[I].Expanded := True;
  finally
    EndUpdate;
  end;
end;

procedure TDefinitionList.CollapseAll;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Groups.Count - 1 do
      Groups[I].Expanded := False;
  finally
    EndUpdate;
  end;
end;

function TDefinitionList.IsAnyGroupExpanded: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Groups.Count - 1 do
    if Groups[I].Expanded then
    begin
      Result := True;
      Exit;
    end;
end;

function TDefinitionList.IsAnyGroupCollapsed: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Groups.Count - 1 do
    if not Groups[I].Expanded then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TDefinitionList.Refresh;
begin
  if UpdateCount = 0 then
  begin
    ListView.Items.Count := ListCount;
    ListView.Refresh;
  end;
end;

procedure TDefinitionList.Invalidate;
begin
  if (UpdateCount = 0) and ListView.HandleAllocated then
    InvalidateRect(ListView.Handle, nil, False);
end;

procedure TDefinitionList.UpdateHeaderArrow;
begin
  if (SortColumn >= 0) and ListView.HandleAllocated then
    SetSortArrow(ListView.Handle, SortColumn, THeaderArrow(Ord(SortAscending) + 1));
end;

function TDefinitionList.NoteAt(ListIndex: Integer): TListNode;
var
  I, Index: Integer;
  Group: TGroupNode;
begin
  Result := nil;
  Index := ListIndex;
  if not SortGrouping then
    Inc(Index);
  if SortAscending then
  begin
    for I := 0 to Groups.Count - 1 do
    begin
      Group := Groups[I];
      if Index = 0 then
      begin
        Result := Group;
        Exit;
      end;
      Dec(Index);
      if not Group.Expanded then
        Continue;
      if Index < Group.Count then
      begin
        Result := Group.Items[Index];
        Exit;
      end;
      Dec(Index, Group.Count);
    end;
  end
  else // if not SortAscending
  begin
    for I := Groups.Count - 1 downto 0 do
    begin
      Group := Groups[I];
      if Index = 0 then
      begin
        Result := Group;
        Exit;
      end;
      Dec(Index);
      if not Group.Expanded then
        Continue;
      if Index < Group.Count then
      begin
        Result := Group.Items[Group.Count - Index - 1];
        Exit;
      end;
      Dec(Index, Group.Count);
    end;
  end;
end;

procedure TDefinitionList.Prepare(LVItem: TListItem);
var
  Node: TListNode;
begin
  Node := NoteAt(LVItem.Index);
  LVItem.Data := Node;
  Node.LVItem := LVItem;
end;

procedure TDefinitionList.PrepareRange(StartIndex, EndIndex: Integer);
var
  I, ItemIndex, GroupIndex: Integer;
  Group: TGroupNode;
  Item: TItemNode;
begin
  Group := nil;
  GroupIndex := -1;
  ItemIndex := StartIndex;
  if not SortGrouping then
    Inc(ItemIndex);
  if SortAscending then
  begin
    // find start item
    for I := 0 to Groups.Count - 1 do
    begin
      Group := Groups[I];
      Dec(ItemIndex);
      if ItemIndex < 0 then
      begin
        GroupIndex := I;
        Break;
      end;
      if not Group.Expanded then
        Continue;
      if ItemIndex < Group.Count then
      begin
        GroupIndex := I;
        Break;
      end;
      Dec(ItemIndex, Group.Count);
    end;
    // if start item found, prepare until end item
    if GroupIndex >= 0 then
      for I := StartIndex to EndIndex do
      begin
        if ItemIndex >= 0 then
        begin
          Item := Group.Items[ItemIndex];
          ListView.Items[I].Data := Item;
          Item.LVItem := ListView.Items[I];
        end
        else if Assigned(Group) then
        begin
          ListView.Items[I].Data := Group;
          Group.LVItem := ListView.Items[I];
        end
        else
          Exit;
        Inc(ItemIndex);
        if ItemIndex = Group.Count then
        begin
          Inc(GroupIndex);
          if GroupIndex = Groups.Count then
            Exit;
          Group := Groups[GroupIndex];
          ItemIndex := -1;
        end
      end;
  end
  else // if not SortAscending
  begin
    // find start item
    for I := Groups.Count - 1 downto 0 do
    begin
      Group := Groups[I];
      Dec(ItemIndex);
      if ItemIndex < 0 then
      begin
        GroupIndex := I;
        Break;
      end;
      if not Group.Expanded then
        Continue;
      if ItemIndex < Group.Count then
      begin
        GroupIndex := I;
        Break;
      end;
      Dec(ItemIndex, Group.Count);
    end;
    // if start item found, prepare until end item
    if GroupIndex >= 0 then
      for I := StartIndex to EndIndex do
      begin
        if ItemIndex >= 0 then
        begin
          Item := Group.Items[Group.Count - ItemIndex - 1];
          ListView.Items[I].Data := Item;
          Item.LVItem := ListView.Items[I];
        end
        else if Assigned(Group) then
        begin
          ListView.Items[I].Data := Group;
          Group.LVItem := ListView.Items[I];
        end
        else
          Exit;
        Inc(ItemIndex);
        if ItemIndex = Group.Count then
        begin
          Dec(GroupIndex);
          if GroupIndex < 0 then
            Exit;
          Group := Groups[GroupIndex];
          ItemIndex := -1;
        end
      end;
  end;
end;

function TDefinitionList.IsShowing(Node: TListNode): Boolean;
var
  Index: Integer;
begin
  Index := Node.ListIndex;
  Dec(Index, ListView_GetTopIndex(ListView.Handle));
  Result := (Index >= 0) and (Index < ListView_GetCountPerPage(ListView.Handle));
end;

procedure TDefinitionList.Select(Node: TListNode);
begin
  ListView.ItemIndex := Node.ListIndex;
  ListView.Selected.MakeVisible(False);
  ListView.Selected.Focused := True;
end;

procedure TDefinitionList.Update(Node: TListNode);
begin
  ListView_Update(ListView.Handle, Node.ListIndex);
end;

procedure TDefinitionList.UpdateBy(Translations: TList);
var
  I: Integer;
  Count: Integer;
begin
  Count := Translations.Count;
  if Count <> 0 then
  begin
    for I := 0 to Groups.Count - 1 do
    begin
      Dec(Count, Groups[I].UpdateItemsBy(Translations));
      if Count = 0 then
        Exit;
    end;
  end;
end;

function TDefinitionList.ConditionalCount(Condition: TTranslationStates;
  ExpandedOnly: Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Groups.Count - 1 do
    if not ExpandedOnly or Groups[I].Expanded then
      Inc(Result, Groups[I].ConditionalCount(Condition));
end;

end.

