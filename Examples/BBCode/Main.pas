unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    RichEdit1: TRichEdit;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    btnApply: TButton;
    Splitter1: TSplitter;
    procedure btnApplyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  i18nBBCode;

procedure TForm1.btnApplyClick(Sender: TObject);
begin
  BBCodes.Apply(RichEdit1, Memo1.Text);
end;

end.
