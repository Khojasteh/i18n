unit Child;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCore, i18nLocalizer, i18nCalendar, StdCtrls, Buttons, i18nCtrls,
  i18nDateCtrls;

type
  TChildForm = class(TForm)
    IntlMonthCalendar1: TIntlMonthCalendar;
    IntlDatePicker1: TIntlDatePicker;
    IntlDateTimeLabel1: TIntlDateTimeLabel;
    Label2: TLabel;
    Label1: TLabel;
    Translator: TTranslator;
    procedure IntlMonthCalendar1SelectionChange(Sender: TObject);
    procedure TranslatorAfterTranslate(Sender: TObject);
  end;

var
  ChildForm: TChildForm;

implementation

{$R *.dfm}

uses
  DataModule;

procedure TChildForm.IntlMonthCalendar1SelectionChange(Sender: TObject);
const
  SOneDay   = 'One day is selected.';
  SManyDays = '%d days are selected.';
var
  FmtStr: String;
begin
  FmtStr := Translator.GetNText([SOneDay, SManyDays], IntlMonthCalendar1.SelectionCount);
  Label2.Caption := DM.Localizer.Format(FmtStr, [IntlMonthCalendar1.SelectionCount]);
end;

procedure TChildForm.TranslatorAfterTranslate(Sender: TObject);
begin
  IntlMonthCalendar1SelectionChange(nil);
end;

end.
