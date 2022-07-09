{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, i18nCore, i18nLocalizer, ExtCtrls;

type

  TSearchField = (sfOriginal, sfTranslated, sfName, sfComment, sfNote);
  TSearchFields = set of TSearchField;

  TSearchOrigin = (soCurrent, soEntireScope);
  TSearchDirection = (sdForward, sdBackward);
  TSearchMode = (smFind, smReplace);

  TSearchOptions = record
    Types: TSearchTypes;
    Origin: TSearchOrigin;
    Direction: TSearchDirection;
    Fields: TSearchFields;
  end;

  TSearchParams = record
    Target: String;
    Replace: String;
    Mode: TSearchMode;
    Options: TSearchOptions;
  end;

  TSearchDialog = class(TForm)
    TabControl: TTabControl;
    ClientPanel: TPanel;
    TargetPhraseLabel: TLabel;
    ReplacePhraseLabel: TLabel;
    TargetPhrase: TComboBox;
    gbWhere: TGroupBox;
    cbWhereOriginal: TCheckBox;
    cbWhereTranslated: TCheckBox;
    cbWhereComment: TCheckBox;
    cbWhereNote: TCheckBox;
    cbWhereName: TCheckBox;
    ReplacePhrase: TComboBox;
    FlowPanel: TPanel;
    gbTypes: TGroupBox;
    cbTypeMatchCase: TCheckBox;
    cbTypeWholeWord: TCheckBox;
    gbOrigin: TGroupBox;
    rbOriginEntire: TRadioButton;
    rbOriginCurrent: TRadioButton;
    gbDirection: TGroupBox;
    rbDirBackward: TRadioButton;
    rbDirForward: TRadioButton;
    ButtonsPanel: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    Translator: TTranslator;
    procedure FormShow(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure TargetPhraseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    SaveFields: TSearchFields;
    DisabledFields: TSearchFields;
    function GetTypes: TSearchTypes;
    procedure SetTypes(Value: TSearchTypes);
    function GetOrigin: TSearchOrigin;
    procedure SetOrigin(Value: TSearchOrigin);
    function GetDirection: TSearchDirection;
    procedure SetDirection(Value: TSearchDirection);
    function GetFields: TSearchFields;
    procedure SetFields(Value: TSearchFields);
    function GetMode: TSearchMode;
    procedure SetMode(Value: TSearchMode);
    function GetOptions: TSearchOptions;
    procedure SetOptions(const Value: TSearchOptions);
    property Types: TSearchTypes read GetTypes write SetTypes;
    property Origin: TSearchOrigin read GetOrigin write SetOrigin;
    property Direction: TSearchDirection read GetDirection write SetDirection;
    property Fields: TSearchFields read GetFields write SetFields;
    property Mode: TSearchMode read GetMode write SetMode;
    property Options: TSearchOptions read GetOptions write SetOptions;
  public
    class function Execute(SearchMode: TSearchMode; var Params: TSearchParams;
      UnavailableFields: TSearchFields = []; CanReplace: Boolean = True): Boolean;
  end;

implementation

{$R *.dfm}

uses
  DataModule;

{ TSearchDialog }

class function TSearchDialog.Execute(SearchMode: TSearchMode;
  var Params: TSearchParams; UnavailableFields: TSearchFields;
  CanReplace: Boolean): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      if sfTranslated in UnavailableFields then
        CanReplace := False;
      if not CanReplace then
      begin
        if SearchMode = smFind then
          TabControl.Tabs.Delete(1)
        else
          Exit;
      end;
      DisabledFields := UnavailableFields;
      TargetPhrase.Text := Params.Target;
      ReplacePhrase.Text := Params.Replace;
      Options := Params.Options;
      Mode := SearchMode;
      if ShowModal = mrOK then
      begin
        Params.Target := TargetPhrase.Text;
        Params.Replace := ReplacePhrase.Text;
        Params.Options := Options;
        Params.Mode := Mode;
        Result := True;
      end;
    finally
      Free;
    end;
end;

procedure TSearchDialog.FormCreate(Sender: TObject);
begin
  DM.RecentFindPhrases.AssignTo(TargetPhrase.Items);
  DM.RecentReplacePhrases.AssignTo(ReplacePhrase.Items);
end;

procedure TSearchDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    DM.RecentFindPhrases.Add(TargetPhrase.Text);
    if Mode = smReplace then
      DM.RecentReplacePhrases.Add(ReplacePhrase.Text);
  end;
end;

procedure TSearchDialog.FormShow(Sender: TObject);
begin
  TabControlChange(nil);
  TargetPhraseChange(nil);
end;

procedure TSearchDialog.TabControlChange(Sender: TObject);
var
  OldHeight: Integer;
  I: Integer;
begin
  OldHeight := ClientPanel.Height;
  ClientPanel.AutoSize := False;
  case TabControl.TabIndex of
    0: // Find
    begin
      ReplacePhrase.Visible := False;
      ReplacePhraseLabel.Visible := False;
      for I := 0 to gbWhere.ControlCount - 1 do
        with gbWhere.Controls[I] do
          Enabled := not (TSearchField(Tag) in DisabledFields);
      if SaveFields <> [] then
        Fields := SaveFields;
    end;
    1: // Replace
    begin
      ReplacePhraseLabel.Visible := True;
      ReplacePhrase.Visible := True;
      for I := 0 to gbWhere.ControlCount - 1 do
        gbWhere.Controls[I].Enabled := False;
      SaveFields := Fields;
      Fields := [sfTranslated] - DisabledFields;
    end;
  end;
  ClientPanel.AutoSize := True;
  TabControl.Height := TabControl.Height + (ClientPanel.Height - OldHeight);
  Update;
end;

procedure TSearchDialog.TargetPhraseChange(Sender: TObject);
begin
  btnOK.Enabled := (TargetPhrase.GetTextLen <> 0);
end;

function TSearchDialog.GetTypes: TSearchTypes;
begin
  Result := [];
  if cbTypeMatchCase.Checked then
    Include(Result, stMatchCase);
  if cbTypeWholeWord.Checked then
    Include(Result, stWholeWord);
end;

procedure TSearchDialog.SetTypes(Value: TSearchTypes);
begin
  cbTypeMatchCase.Checked := (stMatchCase in Value);
  cbTypeWholeWord.Checked := (stWholeWord in Value);
end;

function TSearchDialog.GetOrigin: TSearchOrigin;
begin
  if rbOriginEntire.Checked then
    Result := soEntireScope
  else
    Result := soCurrent;
end;

procedure TSearchDialog.SetOrigin(Value: TSearchOrigin);
begin
  rbOriginCurrent.Checked := (soCurrent = Value);
  rbOriginEntire.Checked := (soEntireScope = Value);
end;

function TSearchDialog.GetDirection: TSearchDirection;
begin
  if rbDirForward.Checked then
    Result := sdForward
  else
    Result := sdBackward;
end;

procedure TSearchDialog.SetDirection(Value: TSearchDirection);
begin
  rbDirForward.Checked := (sdForward = Value);
  rbDirBackward.Checked := (sdBackward = Value);
end;

function TSearchDialog.GetFields: TSearchFields;
begin
  Result := [];
  if cbWhereOriginal.Checked then
    Include(Result, sfOriginal);
  if cbWhereTranslated.Checked then
    Include(Result, sfTranslated);
  if cbWhereComment.Checked then
    Include(Result, sfComment);
  if cbWhereNote.Checked then
    Include(Result, sfNote);
  if cbWhereName.Checked then
    Include(Result, sfName);
end;

procedure TSearchDialog.SetFields(Value: TSearchFields);
begin
  Value := Value - DisabledFields;
  cbWhereOriginal.Checked := (sfOriginal in Value);
  cbWhereTranslated.Checked := (sfTranslated in Value);
  cbWhereComment.Checked := (sfComment in Value);
  cbWhereNote.Checked := (sfNote in Value);
  cbWhereName.Checked := (sfName in Value);
end;

function TSearchDialog.GetMode: TSearchMode;
begin
  if TabControl.TabIndex = 0 then
    Result := smFind
  else
    Result := smReplace;
end;

procedure TSearchDialog.SetMode(Value: TSearchMode);
begin
  if Value = smFind then
    TabControl.TabIndex := 0
  else
    TabControl.TabIndex := 1;
end;

function TSearchDialog.GetOptions: TSearchOptions;
begin
  Result.Types := Types;
  Result.Origin := Origin;
  Result.Direction := Direction;
  Result.Fields := Fields;
end;

procedure TSearchDialog.SetOptions(const Value: TSearchOptions);
begin
  Types := Value.Types;
  Origin := Value.Origin;
  Direction := Value.Direction;
  Fields := Value.Fields;
end;

end.
