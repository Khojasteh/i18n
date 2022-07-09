object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderWidth = 16
  Caption = 'i18n Package - IntlMonthCalendar'
  ClientHeight = 220
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 209
    Height = 220
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 16
    Margins.Bottom = 0
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object lblCulture: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 209
      Height = 13
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Align = alTop
      Caption = 'Culture:'
      FocusControl = Cultures
      Transparent = False
      ExplicitWidth = 39
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 45
      Width = 209
      Height = 13
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Align = alTop
      Caption = 'Calendar Type:'
      FocusControl = Calendars
      Transparent = False
      ExplicitWidth = 74
    end
    object CalendarName: TLabel
      Left = 0
      Top = 204
      Width = 209
      Height = 16
      Align = alBottom
      AutoSize = False
      Caption = 'CalendarName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
      ExplicitLeft = -101
      ExplicitTop = 224
      ExplicitWidth = 374
    end
    object Cultures: TCultureBox
      AlignWithMargins = True
      Left = 0
      Top = 16
      Width = 209
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 8
      Align = alTop
      Color = clWindow
      FlagPosition = fpAfterLabel
      Flags = FlagImageList
      Items.Cultures = (
        '*')
      Sorted = True
      TabOrder = 0
      OnSelect = CulturesSelect
    end
    object Calendars: TComboBox
      AlignWithMargins = True
      Left = 0
      Top = 61
      Width = 209
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 8
      Align = alTop
      Style = csDropDownList
      DropDownCount = 20
      Sorted = True
      TabOrder = 1
      OnSelect = CalendarsSelect
    end
    object cbCultureDigits: TCheckBox
      AlignWithMargins = True
      Left = 0
      Top = 90
      Width = 209
      Height = 17
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 8
      Align = alTop
      Caption = 'Use culture'#39's native digits'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbCultureDigitsClick
    end
  end
  object IntlMonthCalendar: TIntlMonthCalendar
    Left = 225
    Top = 0
    Width = 291
    Height = 220
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    FontHeader.Charset = DEFAULT_CHARSET
    FontHeader.Color = clWindowText
    FontHeader.Height = -13
    FontHeader.Name = 'Tahoma'
    FontHeader.Style = [fsBold]
    FontFooter.Charset = DEFAULT_CHARSET
    FontFooter.Color = clWindowText
    FontFooter.Height = -13
    FontFooter.Name = 'Tahoma'
    FontFooter.Style = []
    FontDayOfWeek.Charset = DEFAULT_CHARSET
    FontDayOfWeek.Color = clNavy
    FontDayOfWeek.Height = -11
    FontDayOfWeek.Name = 'Tahoma'
    FontDayOfWeek.Style = [fsBold]
    FontWeekNumbers.Charset = DEFAULT_CHARSET
    FontWeekNumbers.Color = clNavy
    FontWeekNumbers.Height = -11
    FontWeekNumbers.Name = 'Tahoma'
    FontWeekNumbers.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    WeekNumbers = True
    OnCalendarChange = IntlMonthCalendarCalendarChange
    OnDateHint = IntlMonthCalendarDateHint
  end
  object FlagImageList: TFlagImageList
    Top = 152
  end
end
