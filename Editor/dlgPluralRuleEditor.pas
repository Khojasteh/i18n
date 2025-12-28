{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  Translation Repository Editor for Delphi Applications                       }
{                                                                              }
{  Copyright (c) Kambiz Khojasteh                                              }
{  https://github.com/khojasteh/i18n                                           }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgPluralRuleEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCore, ComCtrls, i18nLocalizer, StdCtrls, ExtCtrls, i18nPlurals,
  i18nCtrls;

type
  TPluralRuleEditorDialog = class(TForm)
    ButtonsPanel: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    Translator: TTranslator;
    PageControl: TPageControl;
    tabGeneral: TTabSheet;
    ClientPanel: TPanel;
    lblNumOfPlurals: TLabel;
    lblNumOfPluralsDesc: TLabel;
    lblFormula: TLabel;
    Formula: TEdit;
    lblFormulaDesc1: TLabel;
    pnlNumOfPlurals: TPanel;
    edNumOfPlurals: TEdit;
    NumOfPlurals: TUpDown;
    ErrorMsg: TImageLabel;
    lblFormulaSample: TLabel;
    lblFormulaDesc2: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ClientPanelResize(Sender: TObject);
    procedure FormulaChange(Sender: TObject);
  public
    class function Execute(Culture: TCultureInfo; var PluralRule: String): Boolean;
  end;

implementation

{$R *.dfm}

uses
  DataModule, i18nCExpr;

const
  SNoFormulaError  = 'Please enter an expression for the plural form selection.';
  SFormulaError    = 'There is syntax error in your expression of plural form selection.';
  SFormulaVarError = 'You can use only variable ''n'' in the expression of plural form selection.';

{ TPluralRuleEditorDialog }

class function TPluralRuleEditorDialog.Execute(Culture: TCultureInfo;
  var PluralRule: String): Boolean;
var
  PluralForms: TPluralForms;
begin
  Result := False;
  with Create(Application) do
    try
      tabGeneral.Caption := Culture.DisplayNames[cnLocalizedDisplayName];
      tabGeneral.ImageIndex := DM.Flags.ImageIndexOf(Culture);
      PluralForms := TPluralForms.Create(PluralRule);
      try
        NumOfPlurals.Position := PluralForms.NumOfPlurals;
        Formula.Text := PluralForms.Formula.Source;
        if ShowModal = mrOK then
        begin
          PluralForms.Apply(NumOfPlurals.Position, Formula.Text);
          if PluralRule <> PluralForms.Rule then
          begin
            PluralRule := PluralForms.Rule;
            Result := True;
          end;
        end;
      finally
        PluralForms.Free;
      end;
    finally
      Free;
    end;
end;

procedure TPluralRuleEditorDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  Expr: TCExpression;
begin
  if ModalResult = mrOK then
  begin
    CanClose := False;
    Formula.Text := Trim(Formula.Text);
    if Formula.Text <> '' then
      try
        Expr := TCExpression.Create(Formula.Text);
        try
          if (Expr.VarCount <> 0) and
             ((Expr.VarCount > 1) or (Expr.Vars['n'].RefCount = 0))
          then
            ErrorMsg.Caption := Translator.GetText(SFormulaVarError)
          else
            CanClose := True;
        finally
          Expr.Free;
        end;
      except
        on E: Exception do
        begin
          if E is ECExpressionError then
          begin
            Formula.SetFocus;
            Formula.SelStart := ECExpressionError(E).Offset - Length(ECExpressionError(E).Token);
            Formula.SelLength := Length(ECExpressionError(E).Token);
          end;
          ErrorMsg.Caption := Translator.GetText(SFormulaError);
        end;
      end
    else
      ErrorMsg.Caption := Translator.GetText(SNoFormulaError);
    if not CanClose then
    begin
      ErrorMsg.Visible := True;
      ActiveControl := Formula;
    end;
  end;
end;

procedure TPluralRuleEditorDialog.FormulaChange(Sender: TObject);
begin
  ErrorMsg.Visible := False;
end;

procedure TPluralRuleEditorDialog.ClientPanelResize(Sender: TObject);
begin
  PageControl.Height := PageControl.Height + (ClientPanel.Height - tabGeneral.Height);
end;

end.
