object SuggestionDialog: TSuggestionDialog
  Left = 0
  Top = 0
  ActiveControl = Suggestions
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Suggested Translations'
  ClientHeight = 455
  ClientWidth = 555
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  ParentBiDiMode = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Prompt: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 0
    Width = 549
    Height = 13
    Margins.Top = 0
    Margins.Bottom = 0
    Align = alTop
    Caption = 
      'Please select the appropriate translation of the original text f' +
      'rom the following list of suggested translations.'
    ShowAccelChar = False
    WordWrap = True
    ExplicitWidth = 520
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 422
    Width = 555
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      555
      33)
    object btnCancel: TButton
      Left = 460
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 359
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
    end
  end
  object gbSource: TGroupBox
    Left = 0
    Top = 303
    Width = 555
    Height = 119
    Align = alBottom
    Caption = ' Original Text '
    TabOrder = 1
    object SourceLanguage: TCultureLabel
      AlignWithMargins = True
      Left = 12
      Top = 23
      Width = 531
      Height = 13
      Margins.Left = 10
      Margins.Top = 8
      Margins.Right = 10
      Margins.Bottom = 0
      Align = alTop
      DisplayName = cnLocalizedDisplayName
      Flags = DM.Flags
      ShowAccelChar = False
      ExplicitLeft = 0
      ExplicitTop = 20
      ExplicitWidth = 430
    end
    object SourceText: TMemo
      AlignWithMargins = True
      Left = 12
      Top = 40
      Width = 531
      Height = 40
      Margins.Left = 10
      Margins.Top = 4
      Margins.Right = 10
      Margins.Bottom = 10
      TabStop = False
      Align = alClient
      BevelInner = bvLowered
      BevelKind = bkFlat
      BevelOuter = bvRaised
      BiDiMode = bdLeftToRight
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBiDiMode = False
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WantReturns = False
    end
    object SourcePlurals: TTabSet
      AlignWithMargins = True
      Left = 12
      Top = 90
      Width = 531
      Height = 17
      Margins.Left = 10
      Margins.Top = 0
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alBottom
      DitherBackground = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = True
      SoftTop = True
      Style = tsSoftTabs
      Tabs.Strings = (
        ' plural=0 '
        ' plural=1 ')
      TabIndex = 0
      UnselectedColor = cl3DLight
      Visible = False
      OnChange = SourcePluralsChange
    end
  end
  object gbTarget: TGroupBox
    AlignWithMargins = True
    Left = 0
    Top = 21
    Width = 555
    Height = 274
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 8
    Align = alClient
    Caption = ' Translation '
    TabOrder = 2
    object TargetLanguage: TCultureLabel
      AlignWithMargins = True
      Left = 12
      Top = 23
      Width = 531
      Height = 13
      Margins.Left = 10
      Margins.Top = 8
      Margins.Right = 10
      Margins.Bottom = 0
      Align = alTop
      DisplayName = cnLocalizedDisplayName
      Flags = DM.Flags
      ShowAccelChar = False
      ExplicitLeft = 0
      ExplicitTop = 20
      ExplicitWidth = 430
    end
    object TargetPlurals: TTabSet
      AlignWithMargins = True
      Left = 12
      Top = 245
      Width = 531
      Height = 17
      Margins.Left = 10
      Margins.Top = 0
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alBottom
      DitherBackground = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = True
      SelectedColor = clWindow
      SoftTop = True
      Style = tsSoftTabs
      Tabs.Strings = (
        ' plural=0 '
        ' plural=1 ')
      TabIndex = 0
      UnselectedColor = cl3DLight
      Visible = False
      OnChange = TargetPluralsChange
    end
    object Suggestions: TListBox
      AlignWithMargins = True
      Left = 12
      Top = 40
      Width = 531
      Height = 195
      Margins.Left = 10
      Margins.Top = 4
      Margins.Right = 10
      Margins.Bottom = 10
      Style = lbOwnerDrawVariable
      Align = alClient
      BevelInner = bvLowered
      BevelKind = bkFlat
      BiDiMode = bdLeftToRight
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBiDiMode = False
      ParentFont = False
      PopupMenu = PopupMenu
      TabOrder = 0
      OnClick = SuggestionsClick
      OnDblClick = SuggestionsDblClick
      OnDrawItem = SuggestionsDrawItem
      OnMeasureItem = SuggestionsMeasureItem
    end
  end
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      '.Caption'
      'btnCancel.Caption'
      'btnOK.Caption'
      'gbSource.Caption'
      'gbTarget.Caption'
      'Prompt.Caption'
      'RemoveSuggestion.Caption'
      'RemoveSuggestion.Hint')
    Left = 3
    Top = 427
  end
  object ImageList: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Height = 8
    Width = 8
    Left = 35
    Top = 427
    Bitmap = {
      494C010102000800C00008000800FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000200000000800000001002000000000000004
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001717171A414141802222
      22D5222222D53F3F3F801717171A00000000000000001919191A78787880BEBE
      BED5BDBDBDD5767676801919191A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000444444805E5E5EFF8181
      81FF7C7C7CFF555555FF3F3F3F80000000000000000078787880F0F0F0FFF6F6
      F6FFF5F5F5FFEFEFEFFF77777780000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000333333D5878787FF6060
      60FF545454FF7C7C7CFF222222D50000000000000000C1C1C1D5F7F7F7FFF0F0
      F0FFEFEFEFFFF6F6F6FFBEBEBED5000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000363636D5898989FF6565
      65FF5B5B5BFF808080FF242424D50000000000000000C2C2C2D5F7F7F7FFF2F2
      F2FFF0F0F0FFF6F6F6FFBFBFBFD5000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000048484880686868FF8989
      89FF858585FF616161FF42424280000000000000000079797980F2F2F2FFF7F7
      F7FFF7F7F7FFF1F1F1FF78787880000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001717171A484848803737
      37D5353535D5454545801717171A00000000000000001919191A79797980C2C2
      C2D5C1C1C1D5787878801919191A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000020000000080000000100010000000000200000000000000000000000
      000000000000000000000000FFFFFF00FFFF0000818100008181000081810000
      818100008181000081810000FFFF000000000000000000000000000000000000
      000000000000}
  end
  object PopupMenu: TPopupMenu
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    OnPopup = PopupMenuPopup
    Left = 67
    Top = 427
    object RemoveSuggestion: TMenuItem
      Caption = 'Remove'
      Hint = 
        'Remove|Permanently removes the selected item from the list of su' +
        'ggestions.'
      ShortCut = 16430
      OnClick = RemoveSuggestionClick
    end
  end
end
