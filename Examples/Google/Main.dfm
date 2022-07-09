object MainForm: TMainForm
  Left = 0
  Top = 0
  ActiveControl = SourceText
  Caption = 'i18n Package - Google Translator'
  ClientHeight = 561
  ClientWidth = 487
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    487
    561)
  PixelsPerInch = 96
  TextHeight = 16
  object lblSourceLanguage: TLabel
    Left = 8
    Top = 8
    Width = 104
    Height = 16
    Caption = 'Source Language:'
    FocusControl = SourceLanguage
  end
  object lblTargetLanguage: TLabel
    Left = 240
    Top = 8
    Width = 102
    Height = 16
    Caption = 'Target Language:'
    FocusControl = TargetLanguage
  end
  object lblSourceText: TLabel
    Left = 8
    Top = 58
    Width = 74
    Height = 16
    Caption = 'Source Text:'
    FocusControl = SourceText
  end
  object lblTranslatedText: TLabel
    Left = 8
    Top = 358
    Width = 95
    Height = 16
    Caption = 'Translated Text:'
    FocusControl = TranslatedText
  end
  object SourceText: TMemo
    Left = 8
    Top = 77
    Width = 471
    Height = 150
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'Enter something here to translate it.')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object TranslatedText: TMemo
    Left = 8
    Top = 378
    Width = 471
    Height = 174
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object btnTranslate: TButton
    Left = 167
    Top = 311
    Width = 153
    Height = 36
    Anchors = [akTop]
    Caption = 'Translate'
    TabOrder = 3
    OnClick = btnTranslateClick
  end
  object SourceLanguage: TCultureBox
    Left = 8
    Top = 29
    Width = 217
    Height = 23
    Color = clWindow
    Flags = Flags
    Items.Cultures = (
      '*')
    Sorted = True
    TabOrder = 0
    OnKeyDown = SourceLanguageKeyDown
    OnSelect = SourceLanguageSelect
  end
  object TargetLanguage: TCultureBox
    Left = 240
    Top = 27
    Width = 239
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Color = clWindow
    Flags = Flags
    Items.Cultures = (
      '*')
    Sorted = True
    TabOrder = 1
    OnSelect = TargetLanguageSelect
  end
  object rgTextFormat: TRadioGroup
    Left = 8
    Top = 233
    Width = 471
    Height = 56
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Text  Format '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Plain Text'
      'HTML Text')
    TabOrder = 5
    OnClick = rgTextFormatClick
  end
  object GoogleTranslator: TGoogleTranslator
    Left = 16
    Top = 392
  end
  object Flags: TFlagImageList
    Left = 48
    Top = 392
  end
end
