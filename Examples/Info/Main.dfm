object Form1: TForm1
  Left = 0
  Top = 0
  BorderWidth = 16
  Caption = 'i18nPackage - Info Demo'
  ClientHeight = 416
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 570
    Height = 416
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Cultures'
      object CultureBox1: TCultureBox
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 530
        Height = 21
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alTop
        Color = clWindow
        Flags = FlagImageList1
        Items.Cultures = (
          '*')
        Sorted = True
        TabOrder = 0
        OnSelect = CultureBox1Select
      end
      object ValueListEditor1: TValueListEditor
        AlignWithMargins = True
        Left = 16
        Top = 54
        Width = 530
        Height = 318
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alClient
        DefaultRowHeight = 20
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
        TabOrder = 1
        TitleCaptions.Strings = (
          'Name'
          'Value')
        ColWidths = (
          150
          374)
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Territories'
      ImageIndex = 1
      object Label1: TLabel
        AlignWithMargins = True
        Left = 16
        Top = 259
        Width = 530
        Height = 13
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 16
        Align = alBottom
        Caption = 'Languages:'
        ExplicitLeft = 3
        ExplicitTop = 372
        ExplicitWidth = 56
      end
      object TerritoryBox1: TTerritoryBox
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 530
        Height = 21
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alTop
        Color = clWindow
        DisplayName = tnEnglishName
        Flags = FlagImageList1
        Items.Territories = (
          '*')
        Sorted = True
        TabOrder = 0
        OnSelect = TerritoryBox1Select
        ExplicitLeft = 272
        ExplicitTop = 176
        ExplicitWidth = 145
      end
      object ValueListEditor2: TValueListEditor
        AlignWithMargins = True
        Left = 16
        Top = 53
        Width = 530
        Height = 190
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alClient
        DefaultRowHeight = 20
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
        TabOrder = 1
        TitleCaptions.Strings = (
          'Name'
          'Value')
        ExplicitTop = 54
        ExplicitHeight = 318
        ColWidths = (
          150
          374)
      end
      object CultureListBox1: TCultureListBox
        AlignWithMargins = True
        Left = 16
        Top = 275
        Width = 530
        Height = 97
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alBottom
        Color = clWindow
        Columns = 3
        DisplayName = cnEnglishLanguageName
        Sorted = True
        TabOrder = 2
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Currencies'
      ImageIndex = 2
      object Label2: TLabel
        AlignWithMargins = True
        Left = 16
        Top = 259
        Width = 530
        Height = 13
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 16
        Align = alBottom
        Caption = 'Countries:'
        ExplicitTop = 372
        ExplicitWidth = 50
      end
      object CurrencyBox1: TCurrencyBox
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 530
        Height = 21
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alTop
        Color = clWindow
        Flags = FlagImageList1
        Items.Currencies = (
          '*')
        Sorted = True
        TabOrder = 0
        OnSelect = CurrencyBox1Select
        ExplicitLeft = 320
        ExplicitTop = 128
        ExplicitWidth = 145
      end
      object ValueListEditor3: TValueListEditor
        AlignWithMargins = True
        Left = 16
        Top = 53
        Width = 530
        Height = 190
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alClient
        DefaultRowHeight = 20
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
        TabOrder = 1
        TitleCaptions.Strings = (
          'Name'
          'Value')
        ExplicitTop = 54
        ExplicitHeight = 318
        ColWidths = (
          150
          374)
      end
      object TerritoryListBox1: TTerritoryListBox
        AlignWithMargins = True
        Left = 16
        Top = 275
        Width = 530
        Height = 97
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alBottom
        Color = clWindow
        Columns = 3
        DisplayName = tnEnglishName
        Flags = FlagImageList1
        Sorted = True
        TabOrder = 2
      end
    end
  end
  object FlagImageList1: TFlagImageList
    Left = 538
    Top = 385
  end
end
