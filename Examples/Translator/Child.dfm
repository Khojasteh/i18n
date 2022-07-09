object ChildForm: TChildForm
  Left = 0
  Top = 0
  BiDiMode = bdLeftToRight
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Calendar Controls'
  ClientHeight = 265
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ParentBiDiMode = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object IntlDateTimeLabel1: TIntlDateTimeLabel
    AlignWithMargins = True
    Left = 16
    Top = 16
    Width = 481
    Height = 21
    Hint = 'TIntlDateTimeLabel'
    Margins.Left = 16
    Margins.Top = 16
    Margins.Right = 16
    Margins.Bottom = 16
    Align = alTop
    BorderColor = cl3DLight
    BorderWidth = 1
    DateTimeFormat = 'dddddd'
    Localizer = DM.Localizer
    Padding.Left = 4
    Padding.Top = 2
    Padding.Right = 4
    Padding.Bottom = 4
    ExplicitWidth = 133
  end
  object Label2: TLabel
    Left = 16
    Top = 239
    Width = 31
    Height = 13
    Caption = 'Label2'
    ShowAccelChar = False
  end
  object Label1: TLabel
    Left = 296
    Top = 56
    Width = 56
    Height = 13
    Caption = 'Pick a date:'
    FocusControl = IntlDatePicker1
  end
  object IntlMonthCalendar1: TIntlMonthCalendar
    Left = 16
    Top = 56
    Width = 257
    Height = 177
    Hint = 'TIntlMonthCalendar'
    Localizer = DM.Localizer
    SelectionStyle = ssFree
    TabOrder = 0
    OnSelectionChange = IntlMonthCalendar1SelectionChange
  end
  object IntlDatePicker1: TIntlDatePicker
    Left = 296
    Top = 72
    Width = 201
    Height = 21
    Hint = 'TIntlDatePicker'
    DateFormat = 'dddddd'
    Localizer = DM.Localizer
    ShowCheckBox = True
    TabOrder = 1
  end
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      '.Caption'
      'Label1.Caption')
    Translatables.Literals = (
      '#B52E99C67568AE512C68A5E0A3DBBDEF')
    OnAfterTranslate = TranslatorAfterTranslate
    Left = 296
    Top = 152
  end
end
