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
/// This unit registers components, component editors, and property editors of
/// the i18n package.
/// </summary>
/// <remarks>
/// This unit cannot be referenced in the runtime packages.
/// </remarks>
unit i18nReg;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Classes, SysUtils, DesignIntf, DesignEditors, Forms,
  i18nCore, i18nCtrls;

type

  TTranslatorComponentEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TTranslatablePropertyEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TURIPropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TCheckListClass = class of TCustomFlagCheckListBox;

  TCustomCheckListPropertyEditor = class(TClassProperty)
  private
    procedure SelectAllClicked(Sender: TObject);
    procedure ClearSelectionClicked(Sender: TObject);
    procedure InvertSelectionClicked(Sender: TObject);
  protected
    CheckList: TCustomFlagCheckListBox;
    function CreateDialog(CheckListClass: TCheckListClass): TForm;
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TCultureListPropertyEditor = class(TCustomCheckListPropertyEditor)
  public
    procedure Edit; override;
  end;

  TCulturePropertyEditor = class(TPropertyEditor)
  protected
    function GetCultures: TReadonlyCultureList; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TTerritoryListPropertyEditor = class(TCustomCheckListPropertyEditor)
  public
    procedure Edit; override;
  end;

  TTerritoryPropertyEditor = class(TPropertyEditor)
  protected
    function GetTerritories: TReadonlyTerritoryList; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TCurrencyListPropertyEditor = class(TCustomCheckListPropertyEditor)
  public
    procedure Edit; override;
  end;

  TCurrencyPropertyEditor = class(TPropertyEditor)
  protected
    function GetCurrencies: TReadonlyCurrencyList; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TGoogleLangPropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TCalendarSystemPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TIntlDateTimePropertyEditor = class(TDateProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
    procedure Edit; override;
  end;

  TIntlDatePropertyEditor = class(TDateProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
    procedure Edit; override;
  end;

  TCoreSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TCalendarSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  i18nCatalog, i18nPropEditor, i18nLocalizer, i18nDialogs, i18nGoogle,
  i18nCalendar, i18nDateCtrls, Types, TypInfo, Graphics, Dialogs, Controls,
  StdCtrls, ExtCtrls, Menus, SysConst, i18nDBCtrls, i18nDBDateCtrls;

resourcestring
  SSelectTranslatables       = 'Select translatables...';
  STranslationFileOpenTitle  = 'Select a translation file';
  STranslationFileFileFilter = 'Translation Catalog Files|*' + i18nCatalogFileExt + '|All Files|*.*';
  SLocalizerManagedList      = 'The languages in this list are managed by the localizer component and cannot be altered manually.';
  SLocalizerManagedProp      = 'This property is managed by the linked localizer component and cannot be altered manually.';
  SDialogButtonOK            = 'OK';
  SDialogButtonCancel        = 'Cancel';
  SDialogItemSelect          = 'Select All';
  SDialogItemDeselect        = 'Clear Selection';
  SDialogItemInvert          = 'Invert Selection';

procedure Register;
begin
  RegisterComponents('i18n', [TLocalizer, TTranslator]);
  RegisterComponents('i18n', [TMessageDialog, TInputQueryDialog]);
  RegisterComponents('i18n', [TGoogleTranslator, TFlagImageList, TImageLabel]);
  RegisterComponents('i18n', [TCultureLabel, TCultureBox, TCultureListBox, TCultureCheckListBox]);
  RegisterComponents('i18n', [TTerritoryLabel, TTerritoryBox, TTerritoryListBox, TTerritoryCheckListBox]);
  RegisterComponents('i18n', [TCurrencyLabel, TCurrencyBox, TCurrencyListBox, TCurrencyCheckListBox]);
  RegisterComponents('i18n', [TIntlDateTimeLabel, TIntlMonthCalendar, TIntlDatePicker]);

  RegisterComponents('i18n DB', [TDBImageLabel]);
  RegisterComponents('i18n DB', [TDBCultureLabel, TDBCultureBox, TDBCultureListBox]);
  RegisterComponents('i18n DB', [TDBTerritoryLabel, TDBTerritoryBox, TDBTerritoryListBox]);
  RegisterComponents('i18n DB', [TDBCurrencyLabel, TDBCurrencyBox, TDBCurrencyListBox]);
  RegisterComponents('i18n DB', [TDBIntlDateTimeLabel, TDBIntlDatePicker]);

  RegisterComponentEditor(TTranslator, TTranslatorComponentEditor);

  RegisterPropertyEditor(TypeInfo(TCultureInfo), nil, '', TCulturePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTerritoryInfo), nil, '', TTerritoryPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCurrencyInfo), nil, '', TCurrencyPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TCultureList), nil, '', TCultureListPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTerritoryList), nil, '', TTerritoryListPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCurrencyList), nil, '', TCurrencyListPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TTranslatables), TTranslator, 'Translatables', TTranslatablePropertyEditor);

  RegisterPropertyEditor(TypeInfo(String), TLocalizer, 'URI', TURIPropertyEditor);

  RegisterPropertyEditor(TypeInfo(String), TCustomGoogleService, 'HostLang', TGoogleLangPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TCustomGoogleService, 'SourceLang', TGoogleLangPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TCustomGoogleService, 'TargetLang', TGoogleLangPropertyEditor);

  {$IFDEF COMPILER2010_UP}
  RegisterPropertyEditor(TypeInfo(TCalendarClass), nil, '', TCalendarSystemPropertyEditor);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TDateTime), TCustomIntlDateTimeLabel, '', TIntlDateTimePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TCustomIntlMonthCalendar, '', TIntlDatePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TCustomIntlDatePicker, '', TIntlDatePropertyEditor);

  RegisterSelectionEditor(TLocalizer, TCoreSelectionEditor);
  RegisterSelectionEditor(TTranslator, TCoreSelectionEditor);
  RegisterSelectionEditor(TCustomGoogleTranslator, TCoreSelectionEditor);
  RegisterSelectionEditor(TCustomFlagImageList, TCoreSelectionEditor);
  RegisterSelectionEditor(TCustomCultureLabel, TCoreSelectionEditor);
  RegisterSelectionEditor(TCustomCultureBox, TCoreSelectionEditor);
  RegisterSelectionEditor(TCustomCultureListBox, TCoreSelectionEditor);
  RegisterSelectionEditor(TCustomCultureCheckListBox, TCoreSelectionEditor);
  RegisterSelectionEditor(TCustomTerritoryLabel, TCoreSelectionEditor);
  RegisterSelectionEditor(TCustomTerritoryBox, TCoreSelectionEditor);
  RegisterSelectionEditor(TCustomTerritoryListBox, TCoreSelectionEditor);
  RegisterSelectionEditor(TCustomTerritoryCheckListBox, TCoreSelectionEditor);

  RegisterSelectionEditor(TCustomIntlMonthCalendar, TCalendarSelectionEditor);
  RegisterSelectionEditor(TCustomIntlDatePicker, TCalendarSelectionEditor);
end;

{ Helper Functions }

function IsManagedByLocalizer(Obj: TPersistent): Boolean;
begin
  if Obj is TCustomCultureLabel then
    Result := TCustomCultureLabel(Obj).IsManagedByLocalizer
  else if Obj is TCustomCultureBox then
    Result := TCustomCultureBox(Obj).IsManagedByLocalizer
  else if Obj is TCustomCultureListBox then
    Result := TCustomCultureListBox(Obj).IsManagedByLocalizer
  else if Obj is TCustomCultureCheckListBox then
    Result := TCustomCultureCheckListBox(Obj).IsManagedByLocalizer
  else if Obj is TCustomIntlDateTimeLabel then
    Result := TCustomIntlDateTimeLabel(Obj).IsManagedByLocalizer
  else if Obj is TCustomIntlMonthCalendar then
    Result := TCustomIntlMonthCalendar(Obj).IsManagedByLocalizer
  else if Obj is TCustomIntlDatePicker then
    Result := TCustomIntlDatePicker(Obj).IsManagedByLocalizer
  else
    Result := False;
end;

function PromptForDate(const ACaption: String; var DateTime: TDateTime): Boolean;
var
  Dialog: TForm;
  IntlCal: TIntlMonthCalendar;
begin
  Result := False;
  Dialog := TForm.Create(Application);
  try
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poMainFormCenter;
    Dialog.ClientWidth := 360;
    Dialog.ClientHeight := 240;
    Dialog.BorderWidth := 8;
    Dialog.Caption := ACaption;
    IntlCal := TIntlMonthCalendar.Create(Dialog);
    with IntlCal do
    begin
      SetBounds(0, 0, Dialog.ClientWidth, Dialog.ClientHeight - 33);
      FontHeader.Style := [fsBold];
      FontHeader.Size := FontHeader.Size + 2;
      FontFooter.Style := [fsBold];
      FontDayOfWeek.Color := clNavy;
      if not Calendar.IsNoDate(DateTime) then
        Date := DateTime;
      Parent := Dialog;
    end;
    with TButton.Create(Dialog) do
    begin
      SetBounds(Dialog.ClientWidth - 158, Dialog.ClientHeight - 25, 75, 25);
      Caption := SDialogButtonOK;
      ModalResult := mrOk;
      Default := True;
      Parent := Dialog;
    end;
    with TButton.Create(Dialog) do
    begin
      SetBounds(Dialog.ClientWidth - 75, Dialog.ClientHeight - 25, 75, 25);
      Caption := SDialogButtonCancel;
      ModalResult := mrCancel;
      Cancel := True;
      Parent := Dialog;
    end;
    if Dialog.ShowModal = mrOK then
    begin
      DateTime := IntlCal.Date;
      Result := True;
    end;
  finally
    Dialog.Free;
  end;
end;

{ TTranslatorComponentEditor }

function TTranslatorComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TTranslatorComponentEditor.GetVerb(Index: Integer): String;
begin
  if Index = 0 then
    Result := SSelectTranslatables
  else
    Result := inherited GetVerb(Index);
end;

procedure TTranslatorComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    Edit
  else
    inherited ExecuteVerb(Index);
end;

procedure TTranslatorComponentEditor.Edit;
begin
  if TTranslatableEditor.Execute(TTranslator(GetComponent).Translatables) then
    Designer.Modified;
end;

{ TTranslatablePropertyEditor }

function TTranslatablePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TTranslatablePropertyEditor.Edit;
begin
  if TTranslatableEditor.Execute(TTranslatables(GetOrdValue)) then
    Designer.Modified;
end;

{ TKnowledgeBasePropertyEditor }

function TURIPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TURIPropertyEditor.Edit;
var
  FileName: String;
begin
  FileName := GetValue;
  if Pos('res:', LowerCase(FileName)) = 1 then
    FileName := ''
  else if Pos('dirx:', LowerCase(FileName)) = 1 then
    FileName := Copy(FileName, 6, Length(FileName) - 5)
  else if Pos('dir:', LowerCase(FileName)) = 1 then
    FileName := Copy(FileName, 5, Length(FileName) - 4)
  else if Pos('file:', LowerCase(FileName)) = 1 then
    FileName := Copy(FileName, 6, Length(FileName) - 5);
  if PromptForFileName(FileName, STranslationFileFileFilter, i18nCatalogFileExt, STranslationFileOpenTitle) then
    SetValue(FileName);
end;

{ TCustomCheckListPropertyEditor }

function TCustomCheckListPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

type TCheckListHack = class(TCustomFlagCheckListBox);

function TCustomCheckListPropertyEditor.CreateDialog(CheckListClass: TCheckListClass): TForm;
var
  Dialog: TForm;
  MenuItem: TMenuItem;
begin
  Dialog := TForm.CreateNew(Application);
  try
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poMainFormCenter;
    Dialog.ClientWidth := 350;
    Dialog.ClientHeight := 353;
    Dialog.BorderWidth := 8;
    Dialog.Caption := GetComponent(0).GetNamePath + '.' + GetName;
    CheckList := CheckListClass.Create(Dialog);
    with TCheckListHack(CheckList) do
    begin
      PopupMenu := TPopupMenu.Create(Dialog);
      Flags := TFlagImageList.Create(Dialog);
      Align := alClient;
      Sorted := True;
      Parent := Dialog;
      CollectAll;
    end;
    with TButton.Create(Dialog) do
    begin
      SetBounds(0, Dialog.ClientHeight - 25, 120, 25);
      Caption := SDialogItemInvert;
      OnClick := InvertSelectionClicked;
      Parent := Dialog;
    end;
    with TButton.Create(Dialog) do
    begin
      SetBounds(Dialog.ClientWidth - 158, Dialog.ClientHeight - 25, 75, 25);
      Caption := SDialogButtonOK;
      ModalResult := mrOk;
      Default := True;
      Parent := Dialog;
    end;
    with TButton.Create(Dialog) do
    begin
      SetBounds(Dialog.ClientWidth - 75, Dialog.ClientHeight - 25, 75, 25);
      Caption := SDialogButtonCancel;
      ModalResult := mrCancel;
      Cancel := True;
      Parent := Dialog;
    end;
    with TBevel.Create(Dialog) do
    begin
      Shape := bsSpacer;
      Align := alBottom;
      Height := 33;
      Parent := Dialog;
    end;
    with TCheckListHack(CheckList).PopupMenu do
    begin
      MenuItem := TMenuItem.Create(Dialog);
      MenuItem.Caption := SDialogItemSelect;
      MenuItem.OnClick := SelectAllClicked;
      Items.Add(MenuItem);
      MenuItem := TMenuItem.Create(Dialog);
      MenuItem.Caption := SDialogItemDeselect;
      MenuItem.OnClick := ClearSelectionClicked;
      Items.Add(MenuItem);
      MenuItem := TMenuItem.Create(Dialog);
      MenuItem.Caption := SDialogItemInvert;
      MenuItem.OnClick := InvertSelectionClicked;
      Items.Add(MenuItem);
    end;
  except
    Dialog.Free;
    Dialog := nil;
  end;
  Result := Dialog;
end;

procedure TCustomCheckListPropertyEditor.SelectAllClicked(Sender: TObject);
begin
  CheckList.RestateAll(cbChecked);
end;

procedure TCustomCheckListPropertyEditor.ClearSelectionClicked(Sender: TObject);
begin
  CheckList.RestateAll(cbUnchecked);
end;

procedure TCustomCheckListPropertyEditor.InvertSelectionClicked(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to CheckList.Count - 1 do
    CheckList.Checked[I] := not CheckList.Checked[I];
end;

{ TCultureListPropertyEditor }

procedure TCultureListPropertyEditor.Edit;
var
  Dialog: TForm;
  Cultures: TCultureList;
begin
  if IsManagedByLocalizer(GetComponent(0)) then
  begin
    MessageDlg(SLocalizerManagedList, mtWarning, [mbOK], 0);
    Exit;
  end;
  Cultures := TCultureList(GetOrdValue);
  Dialog := CreateDialog(TCultureCheckListBox);
  try
    TCultureCheckListBox(CheckList).SetSelection(Cultures);
    if Dialog.ShowModal = mrOK then
    begin
      TCultureCheckListBox(CheckList).GetSelection(Cultures);
      Modified;
    end;
  finally
    Dialog.Free;
  end;
end;

{ TCulturePropertyEditor }

function TCulturePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

procedure TCulturePropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Cultures: TReadonlyCultureList;
begin
  Cultures := GetCultures;
  for I := 0 to Cultures.Count - 1 do
    Proc(Cultures[I].EnglishDisplayName);
end;

function TCulturePropertyEditor.GetValue: String;
var
  Culture: TCultureInfo;
begin
  Culture := TCultureInfo(GetOrdValue);
  if Assigned(Culture) then
    Result := Culture.EnglishDisplayName
  else
    Result := '';
end;

procedure TCulturePropertyEditor.SetValue(const Value: String);
var
  I: Integer;
  Cultures: TReadonlyCultureList;
begin
  if IsManagedByLocalizer(GetComponent(0)) then
  begin
    MessageDlg(SLocalizerManagedProp, mtWarning, [mbOK], 0);
    Exit;
  end;
  Cultures := GetCultures;
  I := Cultures.IndexOfName(Value, cnEnglishDisplayName);
  if I < 0 then
    I := Cultures.NearestIndexOf(Value);
  if I >= 0 then
    SetOrdValue(Integer(Cultures[I]))
  else
    SetOrdValue(0);
end;

function TCulturePropertyEditor.GetCultures: TReadonlyCultureList;
var
  Component: TPersistent;
begin
  Component := GetComponent(0);
  if Component is TLocalizer then
    Result := TLocalizer(Component).Cultures
  else if Component is TCustomCultureBox then
    Result := TCustomCultureBox(Component).Items
  else if Component is TCustomCultureListBox then
    Result := TCustomCultureListBox(Component).Items
  else if Component is TCustomCultureCheckListBox then
    Result := TCustomCultureCheckListBox(Component).Items
  else
    Result := World.Cultures;
end;

{ TTerritoryListPropertyEditor }

procedure TTerritoryListPropertyEditor.Edit;
var
  Dialog: TForm;
  Territories: TTerritoryList;
begin
  Territories := TTerritoryList(GetOrdValue);
  Dialog := CreateDialog(TTerritoryCheckListBox);
  try
    TTerritoryCheckListBox(CheckList).SetSelection(Territories);
    if Dialog.ShowModal = mrOK then
    begin
      TTerritoryCheckListBox(CheckList).GetSelection(Territories);
      Modified;
    end;
  finally
    Dialog.Free;
  end;
end;

{ TTerritoryPropertyEditor }

function TTerritoryPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

procedure TTerritoryPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Territories: TReadonlyTerritoryList;
begin
  Territories := GetTerritories;
  for I := 0 to Territories.Count - 1 do
    Proc(Territories[I].FriendlyName);
end;

function TTerritoryPropertyEditor.GetValue: String;
var
  Territory: TTerritoryInfo;
begin
  Territory := TTerritoryInfo(GetOrdValue);
  if Assigned(Territory) then
    Result := Territory.FriendlyName
  else
    Result := '';
end;

procedure TTerritoryPropertyEditor.SetValue(const Value: String);
var
  I: Integer;
  Territories: TReadonlyTerritoryList;
begin
  Territories := GetTerritories;
  I := Territories.IndexOfName(Value, tnFriendlyName);
  if I < 0 then
    I := Territories.IndexOf(Value);
  if I >= 0 then
    SetOrdValue(Integer(Territories[I]))
  else
    SetOrdValue(0);
end;

function TTerritoryPropertyEditor.GetTerritories: TReadonlyTerritoryList;
var
  Component: TPersistent;
begin
  Component := GetComponent(0);
  if Component is TCustomTerritoryBox then
    Result := TCustomTerritoryBox(Component).Items
  else if Component is TCustomTerritoryListBox then
    Result := TCustomTerritoryListBox(Component).Items
  else if Component is TCustomTerritoryCheckListBox then
    Result := TCustomTerritoryCheckListBox(Component).Items
  else
    Result := World.Territories;
end;

{ TCurrencyListPropertyEditor }

procedure TCurrencyListPropertyEditor.Edit;
var
  Dialog: TForm;
  Currencies: TCurrencyList;
begin
  Currencies := TCurrencyList(GetOrdValue);
  Dialog := CreateDialog(TCurrencyCheckListBox);
  try
    TCurrencyCheckListBox(CheckList).SetSelection(Currencies);
    if Dialog.ShowModal = mrOK then
    begin
      TCurrencyCheckListBox(CheckList).GetSelection(Currencies);
      Modified;
    end;
  finally
    Dialog.Free;
  end;
end;

{ TCurrencyPropertyEditor }

function TCurrencyPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

procedure TCurrencyPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Currencies: TReadonlyCurrencyList;
begin
  Currencies := GetCurrencies;
  for I := 0 to Currencies.Count - 1 do
    Proc(Currencies[I].EnglishName);
end;

function TCurrencyPropertyEditor.GetValue: String;
var
  Currency: TCurrencyInfo;
begin
  Currency := TCurrencyInfo(GetOrdValue);
  if Assigned(Currency) then
    Result := Currency.EnglishName
  else
    Result := '';
end;

procedure TCurrencyPropertyEditor.SetValue(const Value: String);
var
  I: Integer;
  Currencies: TReadonlyCurrencyList;
begin
  Currencies := GetCurrencies;
  I := Currencies.IndexOfName(Value, crnEnglishName);
  if I < 0 then
    I := Currencies.IndexOf(Value);
  if I >= 0 then
    SetOrdValue(Integer(Currencies[I]))
  else
    SetOrdValue(0);
end;

function TCurrencyPropertyEditor.GetCurrencies: TReadonlyCurrencyList;
var
  Component: TPersistent;
begin
  Component := GetComponent(0);
  if Component is TCustomCurrencyBox then
    Result := TCustomCurrencyBox(Component).Items
  else if Component is TCustomCurrencyListBox then
    Result := TCustomCurrencyListBox(Component).Items
  else if Component is TCustomCurrencyCheckListBox then
    Result := TCustomCurrencyCheckListBox(Component).Items
  else
    Result := World.Currencies;
end;

{ TGoogleLangPropertyEditor }

function TGoogleLangPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

function TGoogleLangPropertyEditor.GetValue: String;
var
  Culture: TCultureInfo;
  Lang, Name: String;
begin
  Lang := inherited GetValue;
  Culture := GoogleLangToCulture(Lang);
  if Assigned(Culture) then
  begin
    if Lang = Culture.Language2 then
      Name := Culture.EnglishLanguageName
    else
      Name := Culture.EnglishDisplayName;
    Result := Lang + ' -- ' + Name;
  end
  else
    Result := Lang;
end;

procedure TGoogleLangPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Lang, Name: String;
  Culture: TCultureInfo;
begin
  for I := Low(GoogleLanguages) to High(GoogleLanguages) do
  begin
    Lang := GoogleLanguages[I];
    Culture := GoogleLangToCulture(Lang);
    if Assigned(Culture) then
    begin
      if Lang = Culture.Language2 then
        Name := Culture.EnglishLanguageName
      else
        Name := Culture.EnglishDisplayName;
      Lang := Lang + ' -- ' + Name;
    end;
    Proc(Lang);
  end;
end;

procedure TGoogleLangPropertyEditor.SetValue(const Value: String);
var
  Lang: String;
begin
  if Pos(' ', Value) <> 0 then
    Lang := Copy(Value, 1, Pos(' ', Value) - 1)
  else
    Lang := Value;
  inherited SetValue(Lang);
end;

{ TCalendarSystemPropertyEditor }

function TCalendarSystemPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

procedure TCalendarSystemPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to CalendarTypes.Count - 1 do
    Proc(CalendarTypes.ByIndex(I).CalendarName);
end;

function TCalendarSystemPropertyEditor.GetValue: String;
var
  CalendarClass: TCalendarClass;
begin
  CalendarClass := TCalendarClass(GetOrdValue);
  if Assigned(CalendarClass) then
    Result := CalendarClass.CalendarName
  else
    Result := '';
end;

procedure TCalendarSystemPropertyEditor.SetValue(const Value: String);
var
  CalendarClass: TCalendarClass;
begin
  CalendarClass := CalendarTypes.ByName(Trim(Value));
  if Assigned(CalendarClass) then
    SetOrdValue(Integer(CalendarClass))
  else
    SetOrdValue(0);
end;

{ TIntlDateTimePropertyEditor }

function TIntlDateTimePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TIntlDateTimePropertyEditor.GetValue: String;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if TCalendar.IsNoDate(DT) then
    Result := ''
  else
    Result := DefaultCalendar.Format('C', DT);
end;

procedure TIntlDateTimePropertyEditor.SetValue(const Value: String);
var
  DT: TDateTime;
begin
  if Value = '' then
    SetFloatValue(TCalendar.NoDate)
  else if DefaultCalendar.TryParse('C', Value, DT) then
    SetFloatValue(DT)
  else
    raise EConvertError.CreateResFmt(@SInvalidDateTime, [Value]);
end;

procedure TIntlDateTimePropertyEditor.Edit;
var
  DT: TDateTime;
  Caption: String;
begin
  DT := GetFloatValue;
  Caption := GetComponent(0).GetNamePath + '.' + GetName;
  if PromptForDate(Caption, DT) then
    SetFloatValue(DT);
end;

{ TIntlDatePropertyEditor }

function TIntlDatePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TIntlDatePropertyEditor.GetValue: String;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if TCalendar.IsNoDate(DT) then
    Result := ''
  else
    Result := DefaultCalendar.Format('C', Trunc(DT));
end;

procedure TIntlDatePropertyEditor.SetValue(const Value: String);
var
  DT: TDateTime;
begin
  if Value = ''then
    SetFloatValue(TCalendar.NoDate)
  else if DefaultCalendar.TryParse('C', Value, DT) then
    SetFloatValue(Trunc(DT))
  else
    raise EConvertError.CreateResFmt(@SInvalidDate, [Value]);
end;

procedure TIntlDatePropertyEditor.Edit;
var
  DT: TDateTime;
  Caption: String;
begin
  DT := GetFloatValue;
  Caption := GetComponent(0).GetNamePath + '.' + GetName;
  if PromptForDate(Caption, DT) then
    SetFloatValue(DT);
end;

{ TCoreSelectionEditor }

procedure TCoreSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('i18nCore');
end;

{ TCalendarSelectionEditor }

procedure TCalendarSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('i18nCore');
  Proc('i18nLocalizer');
  Proc('i18nCalendar');
end;

end.
