unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  i18nCore, i18nLocalizer, i18nCalendar, StdCtrls, Buttons, i18nDateCtrls,
  i18nCtrls, ComCtrls, Grids, ValEdit, CheckLst, ExtCtrls, ToolWin, Frame,
  Vcl.OleCtrls, SHDocVw;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    lblLanguageBox: TLabel;
    LanguageBox: TCultureBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Edit1: TEdit;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ListBox1: TListBox;
    RadioGroup1: TRadioGroup;
    TabSheet2: TTabSheet;
    CheckListBox1: TCheckListBox;
    ValueListEditor1: TValueListEditor;
    Panel2: TPanel;
    TreeView1: TTreeView;
    ListView1: TListView;
    Label1: TLabel;
    Translator: TTranslator;
    HeaderControl1: THeaderControl;
    BitBtn1: TBitBtn;
    TabSheet3: TTabSheet;
    SampleFrame1: TSampleFrame;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BitBtn1Click(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  DataModule, Child, i18nCatalog;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
const
  SExitConfirmMsg = 'Do you want to exit this demo application?';
begin
  CanClose := DM.MessageDialog.Confirm(Translator.GetText(SExitConfirmMsg));
end;

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin
  with TChildForm.Create(Application) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

end.
