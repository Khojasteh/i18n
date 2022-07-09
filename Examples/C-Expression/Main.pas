unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, i18nCExpr, Grids, ValEdit;

type
  TMainForm = class(TForm)
    GroupBox1: TGroupBox;
    btnPrepare: TButton;
    Info: TLabel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    ResultValue: TLabel;
    btnEvaluate: TButton;
    Variables: TValueListEditor;
    Expression: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure ExpressionChange(Sender: TObject);
    procedure btnPrepareClick(Sender: TObject);
    procedure btnEvaluateClick(Sender: TObject);
  private
    Expr: TCExpression;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(Expr) then
    FreeAndNil(Expr);
end;

procedure TMainForm.ExpressionChange(Sender: TObject);
begin
  if Assigned(Expr) then
  begin
    FreeAndNil(Expr);
    Variables.Strings.Clear;
    ResultValue.Caption := '';
    Variables.Enabled := False;
    btnEvaluate.Enabled := False;
  end;
  Info.Visible := False;
  btnPrepare.Enabled := (Expression.GetTextLen <> 0);
end;

procedure TMainForm.btnPrepareClick(Sender: TObject);
var
  I: Integer;
begin
  try
    Expr := TCExpression.Create(Expression.Text);
    btnPrepare.Enabled := False;
    for I := 0 to Expr.VarCount - 1 do
      Variables.Strings.Values[Expr.VarNames[I]] := '0';
    Variables.Enabled := True;
    btnEvaluate.Enabled := True;
    Info.Caption := Expr.FormattedSource;
    Info.ParentFont := True;
  except
    on E: ECExpressionError do
    begin
      Expression.SetFocus;
      Expression.SelStart := E.Offset - Length(E.Token);
      Expression.SelLength := Length(E.Token);
      Info.Caption := E.Message;
      Info.Font.Color := clRed;
    end;
  end;
  Info.Visible := True;
end;

procedure TMainForm.btnEvaluateClick(Sender: TObject);
var
  I: Integer;
begin
  with Variables.Strings do
    for I := 0 to Count - 1 do
      Expr.Vars[Names[I]].Value := StrToInt(Values[Names[I]]);
  try
    ResultValue.Caption := IntToStr(Expr.Evaluate);
  except
    on E: Exception do
      ResultValue.Caption := E.Message;
  end;
end;

end.
