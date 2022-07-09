object SelectLanguageDialog: TSelectLanguageDialog
  Left = 0
  Top = 0
  ActiveControl = Languages
  AutoSize = True
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Select Language'
  ClientHeight = 272
  ClientWidth = 356
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
    Left = 0
    Top = 4
    Width = 356
    Height = 13
    Margins.Left = 0
    Margins.Top = 4
    Margins.Right = 0
    Margins.Bottom = 8
    Align = alTop
    FocusControl = Languages
    ShowAccelChar = False
    WordWrap = True
    ExplicitWidth = 3
  end
  object Languages: TCultureListBox
    Left = 0
    Top = 25
    Width = 356
    Height = 214
    Align = alTop
    Color = clWindow
    DisplayName = cnLocalizedDisplayName
    FlagPosition = fpAfterLabel
    Flags = DM.Flags
    Sorted = True
    TabOrder = 1
    OnClick = LanguagesClick
    OnDblClick = LanguagesDblClick
    OnDrawItemLabel = LanguagesDrawItemLabel
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 239
    Width = 356
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      356
      33)
    object btnCancel: TButton
      Left = 261
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
      Left = 160
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
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      '.Caption'
      'btnCancel.Caption'
      'btnOK.Caption')
    Left = 1
    Top = 244
  end
end
