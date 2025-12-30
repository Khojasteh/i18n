unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  i18nCore, i18nLocalizer, i18nCalendar, ImgList, i18nCtrls, i18nDateCtrls,
  StdCtrls, ExtCtrls, System.ImageList;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    lblCulture: TLabel;
    Label1: TLabel;
    Cultures: TCultureBox;
    Calendars: TComboBox;
    cbCultureDigits: TCheckBox;
    FlagImageList: TFlagImageList;
    CalendarName: TLabel;
    IntlMonthCalendar: TIntlMonthCalendar;
    procedure IntlMonthCalendarDateHint(Sender: TObject; const ADate: TDate;
      var HintStr: string);
    procedure CulturesSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CalendarsSelect(Sender: TObject);
    procedure cbCultureDigitsClick(Sender: TObject);
    procedure IntlMonthCalendarCalendarChange(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.CalendarsSelect(Sender: TObject);
var
  CalendarClass: TCalendarClass;
begin
  CalendarClass := TCalendarClass(Calendars.Items.Objects[Calendars.ItemIndex]);
  IntlMonthCalendar.CalendarType := CalendarClass;
end;

procedure TMainForm.cbCultureDigitsClick(Sender: TObject);
begin
  if cbCultureDigits.Checked then
    IntlMonthCalendar.CultureDigits := lsAlways
  else
    IntlMonthCalendar.CultureDigits := lsNever;
end;

procedure TMainForm.CulturesSelect(Sender: TObject);
begin
  IntlMonthCalendar.Culture := Cultures.ItemSelected;
  IntlMonthCalendar.CalendarType := IntlMonthCalendar.Culture.NativeCalendarType;
  cbCultureDigits.Enabled := not IntlMonthCalendar.Culture.IsUsingNominalDigits;
  CalendarName.BiDiMode := IntlMonthCalendar.Culture.BiDiMode;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
  CalendarClass: TCalendarClass;
begin
  Calendars.Items.BeginUpdate;
  try
    Calendars.Items.Clear;
    for I := 0 to CalendarTypes.Count - 1 do
    begin
      CalendarClass := CalendarTypes.ByIndex(I);
      Calendars.Items.AddObject(CalendarClass.CalendarName, TObject(CalendarClass));
    end;
  finally
    Calendars.Items.EndUpdate;
  end;
  Cultures.ItemSelected := GetUserDefaultUICulture;

  if Cultures.ItemSelected <> nil then
    CulturesSelect(nil);
  if Calendars.ItemIndex <> -1 then
    CalendarsSelect(nil);
end;

procedure TMainForm.IntlMonthCalendarCalendarChange(Sender: TObject);
var
  CalendarClass: TCalendarClass;
begin
  CalendarClass := TCalendarClass(IntlMonthCalendar.Calendar.ClassType);
  Calendars.ItemIndex := Calendars.Items.IndexOfObject(TObject(CalendarClass));
  CalendarName.Caption := IntlMonthCalendar.Calendar.Settings.CalendarName;
end;

procedure TMainForm.IntlMonthCalendarDateHint(Sender: TObject; const ADate: TDate;
  var HintStr: string);
begin
  HintStr := IntlMonthCalendar.Format('dddddd', ADate);
end;

end.
