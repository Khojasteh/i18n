object OptionsDialog: TOptionsDialog
  Left = 0
  Top = 0
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Preferences'
  ClientHeight = 276
  ClientWidth = 412
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  ParentBiDiMode = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonsPanel: TPanel
    Left = 0
    Top = 243
    Width = 412
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      412
      33)
    object btnCancel: TButton
      Left = 317
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
      Left = 216
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 412
    Height = 243
    ActivePage = tabGeneral
    Align = alClient
    TabOrder = 1
    object tabGeneral: TTabSheet
      Caption = 'General'
      object cbSortImmediately: TCheckBox
        AlignWithMargins = True
        Left = 16
        Top = 12
        Width = 372
        Height = 28
        Margins.Left = 16
        Margins.Top = 12
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Reorder the list immediately when an item is edited'
        TabOrder = 0
        WordWrap = True
      end
      object cbAskConfirmations: TCheckBox
        AlignWithMargins = True
        Left = 16
        Top = 44
        Width = 372
        Height = 28
        Margins.Left = 16
        Margins.Top = 4
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Ask remembered confirmations again'
        TabOrder = 1
        WordWrap = True
      end
      object btnAssociate: TButton
        AlignWithMargins = True
        Left = 16
        Top = 151
        Width = 372
        Height = 40
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 16
        Margins.Bottom = 24
        Align = alBottom
        Caption = 'Associate this program with the translation catalog files'
        TabOrder = 2
        WordWrap = True
        OnClick = btnAssociateClick
      end
    end
  end
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      '.Caption'
      'btnAssociate.Caption'
      'btnCancel.Caption'
      'btnOK.Caption'
      'cbAskConfirmations.Caption'
      'cbSortImmediately.Caption'
      'tabGeneral.Caption')
    Left = 2
    Top = 247
  end
end
