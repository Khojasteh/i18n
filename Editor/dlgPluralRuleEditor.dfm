object PluralRuleEditorDialog: TPluralRuleEditorDialog
  Left = 0
  Top = 0
  AutoSize = True
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Edit Plural Rule'
  ClientHeight = 294
  ClientWidth = 437
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  ParentBiDiMode = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonsPanel: TPanel
    Left = 0
    Top = 261
    Width = 437
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      437
      33)
    object btnCancel: TButton
      Left = 342
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
      Left = 241
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
    Width = 437
    Height = 261
    ActivePage = tabGeneral
    Align = alTop
    Images = DM.Flags
    TabOrder = 1
    object tabGeneral: TTabSheet
      ImageIndex = -1
      object ClientPanel: TPanel
        Left = 0
        Top = 0
        Width = 429
        Height = 241
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 16
        ParentColor = True
        ShowCaption = False
        TabOrder = 0
        OnResize = ClientPanelResize
        object lblNumOfPlurals: TLabel
          AlignWithMargins = True
          Left = 16
          Top = 16
          Width = 397
          Height = 13
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Align = alTop
          Caption = 'Number of plural forms:'
          FocusControl = NumOfPlurals
          ExplicitWidth = 113
        end
        object lblNumOfPluralsDesc: TLabel
          AlignWithMargins = True
          Left = 16
          Top = 56
          Width = 397
          Height = 11
          Margins.Left = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Please enter the number of plural forms that the language have.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -9
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ShowAccelChar = False
          WordWrap = True
          ExplicitWidth = 265
        end
        object lblFormula: TLabel
          AlignWithMargins = True
          Left = 16
          Top = 83
          Width = 397
          Height = 13
          Margins.Left = 0
          Margins.Top = 16
          Margins.Right = 0
          Align = alTop
          Caption = 'Formula of plural form selection:'
          FocusControl = Formula
          ExplicitWidth = 154
        end
        object lblFormulaDesc1: TLabel
          AlignWithMargins = True
          Left = 16
          Top = 123
          Width = 397
          Height = 22
          Margins.Left = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 
            'Please enter an expression in C language syntax that returns ind' +
            'ex of the right plural form based on the variable '#39'n'#39'. For examp' +
            'le, for English language this expression is:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -9
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ShowAccelChar = False
          WordWrap = True
          ExplicitWidth = 395
        end
        object ErrorMsg: TImageLabel
          AlignWithMargins = True
          Left = 16
          Top = 209
          Width = 397
          Height = 16
          Margins.Left = 0
          Margins.Top = 16
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          BorderColor = 9816831
          BorderWidth = 1
          Color = 15334142
          ImageIndex = 0
          Images = DM.MsgImages
          Padding.Left = 4
          Padding.Top = 2
          Padding.Right = 4
          Padding.Bottom = 2
          ParentColor = False
          ShowAccelChar = False
          Transparent = False
          Visible = False
          WordWrap = True
          ExplicitWidth = 20
        end
        object lblFormulaSample: TLabel
          AlignWithMargins = True
          Left = 16
          Top = 151
          Width = 397
          Height = 14
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 0
          Margins.Bottom = 6
          Align = alTop
          BiDiMode = bdLeftToRight
          Caption = '(n == 1) ? 0 : 1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentBiDiMode = False
          ParentFont = False
          ShowAccelChar = False
          WordWrap = True
          ExplicitWidth = 112
        end
        object lblFormulaDesc2: TLabel
          AlignWithMargins = True
          Left = 16
          Top = 171
          Width = 397
          Height = 22
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 
            'The resulting value of the expression must be greater than or eq' +
            'ual to zero and smaller than the value given as the number of pl' +
            'ural forms.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -9
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ShowAccelChar = False
          WordWrap = True
          ExplicitTop = 169
          ExplicitWidth = 360
        end
        object Formula: TEdit
          AlignWithMargins = True
          Left = 16
          Top = 99
          Width = 397
          Height = 21
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          BiDiMode = bdLeftToRight
          ParentBiDiMode = False
          TabOrder = 0
          OnChange = FormulaChange
        end
        object pnlNumOfPlurals: TPanel
          AlignWithMargins = True
          Left = 16
          Top = 32
          Width = 397
          Height = 21
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          ParentColor = True
          ShowCaption = False
          TabOrder = 1
          object edNumOfPlurals: TEdit
            Left = 0
            Top = 0
            Width = 138
            Height = 21
            TabOrder = 0
            Text = '1'
          end
          object NumOfPlurals: TUpDown
            Left = 138
            Top = 0
            Width = 16
            Height = 21
            Associate = edNumOfPlurals
            Min = 1
            Max = 9
            Position = 1
            TabOrder = 1
          end
        end
      end
    end
  end
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      '.Caption'
      'btnCancel.Caption'
      'btnOK.Caption'
      'lblFormula.Caption'
      'lblFormulaDesc1.Caption'
      'lblFormulaDesc2.Caption'
      'lblNumOfPlurals.Caption'
      'lblNumOfPluralsDesc.Caption')
    Translatables.Literals = (
      '1C875336182FA9D0663448CE55D31039'
      '925DF3A7A250CD3CB24CBA56B0B58C20'
      'A6DCF3F59F8F1B72AAB276138F5FEAD9')
    Left = 2
    Top = 265
  end
end
