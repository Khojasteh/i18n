object OptionsDialog: TOptionsDialog
  Left = 0
  Top = 0
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Preferences'
  ClientHeight = 290
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
    Top = 257
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
      Left = 305
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
      Left = 204
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
    Height = 257
    ActivePage = tabGeneral
    Align = alClient
    TabOrder = 1
    object tabGeneral: TTabSheet
      Caption = 'General'
      object lblGoogleAPIKey: TLabel
        AlignWithMargins = True
        Left = 16
        Top = 12
        Width = 372
        Height = 15
        Margins.Left = 16
        Margins.Top = 12
        Margins.Right = 16
        Margins.Bottom = 4
        Align = alTop
        Caption = 'Google cloud translation API key:'
        FocusControl = txtGoogleAPIKey
      end
      object txtGoogleAPIKey: TEdit
        AlignWithMargins = True
        Left = 16
        Top = 31
        Width = 372
        Height = 27
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 16
        Margins.Bottom = 8
        Align = alTop
        BiDiMode = bdLeftToRight
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Consolas'
        Font.Style = []
        ParentBiDiMode = False
        ParentFont = False
        TabOrder = 0
      end
      object cbSortImmediately: TCheckBox
        AlignWithMargins = True
        Left = 16
        Top = 78
        Width = 372
        Height = 28
        Margins.Left = 16
        Margins.Top = 12
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Reorder the list immediately when an item is edited'
        TabOrder = 1
        WordWrap = True
      end
      object cbAskConfirmations: TCheckBox
        AlignWithMargins = True
        Left = 16
        Top = 110
        Width = 372
        Height = 28
        Margins.Left = 16
        Margins.Top = 4
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Ask remembered confirmations again'
        TabOrder = 2
        WordWrap = True
      end
      object btnAssociate: TButton
        AlignWithMargins = True
        Left = 16
        Top = 163
        Width = 372
        Height = 40
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 16
        Margins.Bottom = 24
        Align = alBottom
        Caption = 'Associate this program with the translation catalog files'
        TabOrder = 3
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
      'lblGoogleAPIKey.Caption'
      'tabGeneral.Caption')
    Left = 2
    Top = 263
  end
end
