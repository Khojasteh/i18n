{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz Khojasteh                                                         }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgSuggestion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCore, i18nLocalizer, StdCtrls, ExtCtrls, i18nCtrls, ImgList,
  i18nCatalog, i18nHashList, Menus, Tabs;

type
  TSuggestionDialog = class(TForm)
    Translator: TTranslator;
    ButtonsPanel: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    gbSource: TGroupBox;
    gbTarget: TGroupBox;
    Suggestions: TListBox;
    SourceLanguage: TCultureLabel;
    SourceText: TMemo;
    TargetLanguage: TCultureLabel;
    ImageList: TImageList;
    Prompt: TLabel;
    PopupMenu: TPopupMenu;
    RemoveSuggestion: TMenuItem;
    TargetPlurals: TTabSet;
    SourcePlurals: TTabSet;
    procedure SuggestionsClick(Sender: TObject);
    procedure SuggestionsDblClick(Sender: TObject);
    procedure SuggestionsMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure SuggestionsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormShow(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure RemoveSuggestionClick(Sender: TObject);
    procedure SourcePluralsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure TargetPluralsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
  private
    const HorzSpacing = 4;
    const VertSpacing = 3;
  private
    IsFirstUnremovable: Boolean;
    Definition: TTextDefinition;
    procedure PrepareFor(TargetCulture: TCultureInfo);
  public
    class function Execute(ADefinition: TTextDefinition;
      TargetCulture: TCultureInfo; out Suggestion: String): Boolean;
  end;

implementation

{$R *.dfm}

uses
  DataModule, i18nPlurals, i18nZStrList, i18nUtils;

{ Helper Function }

function CompareHits(L: TStringList; I1, I2: Integer): Integer;
begin
  Result := Integer(L.Objects[I2]) - Integer(L.Objects[I1]);
end;

{ TSuggestionDialog }

class function TSuggestionDialog.Execute(ADefinition: TTextDefinition;
  TargetCulture: TCultureInfo; out Suggestion: String): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      Definition := ADefinition;
      PrepareFor(TargetCulture);
      if ShowModal = mrOK then
      begin
        Suggestion := UnescapeString(Suggestions.Items[Suggestions.ItemIndex]);
        Result := True;
      end;
    finally
      Free;
    end;
end;

procedure TSuggestionDialog.PrepareFor(TargetCulture: TCultureInfo);
var
  Catalog: TTranslationCatalog;
  SourceCulture: TCultureInfo;
  Translation: TTextTranslation;
  Synonyms: TStringList;
  Index, NumOfPlurals: Integer;
begin
  Catalog := Definition.Dictionary.TextDomain.Owner.Catalog;
  SourceCulture := Catalog.NativeCulture;
  SourceLanguage.Culture := SourceCulture;
  TargetLanguage.Culture := TargetCulture;
  SourceText.BiDiMode := SourceCulture.BiDiMode;
  Suggestions.BiDiMode := TargetCulture.BiDiMode;
  Suggestions.PopupMenu.BiDiMode := BiDiMode;
  Synonyms := TStringList.Create;
  try
    DM.Repository.FindAllSynonyms(Definition.Value, SourceCulture, TargetCulture, Synonyms);
    Synonyms.CustomSort(CompareHits);
    Translation := Definition.Translations.Find(TargetCulture.Locale);
    if Assigned(Translation) and Translation.IsApproved then
    begin
      Index := Synonyms.IndexOf(EscapeString(Translation.Value));
      if Index < 0 then
      begin
        Synonyms.Insert(0, EscapeString(Translation.Value));
        IsFirstUnremovable := True;
      end
      else if Index > 0 then
        Synonyms.Move(Index, 0);
    end;
    Suggestions.Items.Assign(Synonyms);
  finally
    Synonyms.Free;
  end;
  if Definition.HasPluralForms then
  begin
    NumOfPlurals := TPluralForms.ExtractNumOfPlurals(Catalog.PluralRuleOf(TargetCulture));
    AddPluralChoices(TargetPlurals.Tabs, NumOfPlurals);
    TargetPlurals.Visible := True;
    Suggestions.Margins.Bottom := 0;
    Suggestions.BevelEdges := Suggestions.BevelEdges - [beBottom];
    NumOfPlurals := TPluralForms.ExtractNumOfPlurals(Catalog.PluralRuleOf(SourceCulture));
    AddPluralChoices(SourcePlurals.Tabs, NumOfPlurals);
    SourcePlurals.Visible := True;
    SourceText.Margins.Bottom := 0;
    SourceText.BevelEdges := SourceText.BevelEdges - [beBottom];
    SourceText.Text := Definition.Plurals[SourcePlurals.TabIndex];
  end
  else
    SourceText.Text := Definition.Value;
end;

procedure TSuggestionDialog.PopupMenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := (Suggestions.ItemIndex >= Ord(IsFirstUnremovable));
end;

procedure TSuggestionDialog.RemoveSuggestionClick(Sender: TObject);
var
  Index: Integer;
  TextToDelete: String;
begin
  Index := Suggestions.ItemIndex;
  if Index >= 0 then
  begin
    TextToDelete := UnescapeString(Suggestions.Items[Index]);
    if DM.Repository.Remove(TextToDelete, TargetLanguage.Culture) then
    begin
      Suggestions.Items.Delete(Index);
      if Index >= Suggestions.Items.Count then
        Dec(Index);
      Suggestions.ItemIndex := Index;
      btnOK.Enabled := (Suggestions.ItemIndex >= 0);
    end;
  end;
end;

procedure TSuggestionDialog.FormShow(Sender: TObject);
begin
  if Suggestions.Count <> 0 then
  begin
    Suggestions.ItemIndex := 0;
    btnOK.Enabled := True;
  end;
end;

procedure TSuggestionDialog.SuggestionsClick(Sender: TObject);
begin
  btnOK.Enabled := (Suggestions.ItemIndex >= 0);
end;

procedure TSuggestionDialog.SuggestionsDblClick(Sender: TObject);
begin
  if Suggestions.ItemIndex >= 0 then
    ModalResult := mrOk;
end;

procedure TSuggestionDialog.SuggestionsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Text: String;
  ImageIndex, X: Integer;
begin
  if Index >= 0 then
  begin
    Text := UnescapeString(Suggestions.Items[Index]);
    if Definition.HasPluralForms then
      Text := ZStrings.GetSubStrAt(Text, TargetPlurals.TabIndex);
    ImageIndex := Ord(odSelected in State);
    Suggestions.Canvas.FillRect(Rect);
    SetBkMode(Suggestions.Canvas.Handle, TRANSPARENT);
    InflateRect(Rect, -HorzSpacing, -VertSpacing);
    if Suggestions.UseRightToLeftAlignment then
    begin
      X := Rect.Right - ImageList.Width;
      Dec(Rect.Right, ImageList.Width + HorzSpacing);
    end
    else
    begin
      X := Rect.Left;
      Inc(Rect.Left, ImageList.Width + HorzSpacing);
    end;
    ImageList.Draw(Suggestions.Canvas, X, Rect.Top + 3, ImageIndex);
    DrawText(Suggestions.Canvas.Handle, PChar(Text), Length(Text), Rect,
      Suggestions.DrawTextBiDiModeFlags(DT_WORDBREAK));
  end;
end;

procedure TSuggestionDialog.SuggestionsMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
var
  Text: String;
  Rect: TRect;
begin
  if Index >= 0 then
  begin
    Text := UnescapeString(Suggestions.Items[Index]);
    if Definition.HasPluralForms then
      Text := ZStrings.GetSubStrAt(Text, TargetPlurals.TabIndex);
    Rect := Suggestions.ClientRect;
    Dec(Rect.Right, 3 * HorzSpacing + ImageList.Width);
    Suggestions.Canvas.Font := Suggestions.Font;
    DrawText(Suggestions.Canvas.Handle, PChar(Text), Length(Text), Rect,
      Suggestions.DrawTextBiDiModeFlags(DT_WORDBREAK or DT_CALCRECT));
    Height := Rect.Bottom + 2 * VertSpacing;
  end;
end;

procedure TSuggestionDialog.TargetPluralsChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  Suggestions.Invalidate;
end;

procedure TSuggestionDialog.SourcePluralsChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  SourceText.Text := Definition.Plurals[NewTab];
end;

end.
