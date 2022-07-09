object ImportWizardDialog: TImportWizardDialog
  Left = 0
  Top = 0
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  Caption = 'Import'
  ClientHeight = 418
  ClientWidth = 458
  Color = clBtnFace
  DoubleBuffered = True
  ParentFont = True
  OldCreateOrder = False
  ParentBiDiMode = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 458
    Height = 376
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    object Notebook: TNotebook
      Left = 0
      Top = 0
      Width = 458
      Height = 376
      Align = alClient
      TabOrder = 0
      OnPageChanged = NotebookPageChanged
      object TPage
        Left = 0
        Top = 0
        Caption = 'Import Files'
        object lblFolder: TLabel
          AlignWithMargins = True
          Left = 22
          Top = 65
          Width = 414
          Height = 13
          Margins.Left = 22
          Margins.Top = 16
          Margins.Right = 22
          Align = alTop
          Caption = 'Source Folder:'
          FocusControl = Folders
          ExplicitWidth = 70
        end
        object Header1: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title1: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 442
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Select Import Source'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
            ExplicitWidth = 130
          end
          object Subtitle1: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 442
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Where the import files are located?'
            ShowAccelChar = False
            ExplicitWidth = 168
          end
        end
        object Folders: TShellTreeView
          AlignWithMargins = True
          Left = 22
          Top = 105
          Width = 414
          Height = 249
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 22
          ObjectTypes = [otFolders]
          Root = 'rfMyComputer'
          UseShellImages = True
          Align = alClient
          AutoRefresh = False
          HideSelection = False
          Indent = 19
          ParentColor = False
          RightClickSelect = True
          ShowRoot = False
          TabOrder = 2
          OnChange = FoldersChange
          OnEditing = FoldersEditing
        end
        object edFolder: TEdit
          AlignWithMargins = True
          Left = 22
          Top = 81
          Width = 414
          Height = 21
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Align = alTop
          BiDiMode = bdLeftToRight
          ParentBiDiMode = False
          TabOrder = 1
          OnChange = edFolderChange
          OnExit = edFolderExit
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Text Domains'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Header2: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title2: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 124
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Select Text Domains'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle2: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 193
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Which text domains should be imported?'
            ShowAccelChar = False
          end
        end
        object Domains: TListView
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 414
          Height = 283
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 22
          Align = alClient
          BiDiMode = bdLeftToRight
          Columns = <
            item
              Caption = 'Text Domain'
              Width = 185
            end
            item
              Alignment = taRightJustify
              Caption = '# of Properties'
              Width = 100
            end
            item
              Alignment = taRightJustify
              Caption = '# of Literals'
              Width = 100
            end>
          ColumnClick = False
          ReadOnly = True
          RowSelect = True
          ParentBiDiMode = False
          SmallImages = ImageList
          StateImages = CheckImages
          TabOrder = 1
          ViewStyle = vsReport
          OnAdvancedCustomDraw = ListViewAdvancedCustomDraw
          OnAdvancedCustomDrawItem = DomainsAdvancedCustomDrawItem
          OnClick = DomainsClick
          OnKeyPress = DomainsKeyPress
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Languages'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Header3: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title3: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 107
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Select Languages'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle3: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 234
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Which translation languages should be imported?'
            ShowAccelChar = False
          end
        end
        object Languages: TListView
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 414
          Height = 283
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 22
          Align = alClient
          BiDiMode = bdLeftToRight
          Checkboxes = True
          Columns = <
            item
              Caption = 'Language'
              Width = 275
            end
            item
              Alignment = taRightJustify
              Caption = '% of Progress'
              Width = 110
            end>
          ColumnClick = False
          ReadOnly = True
          RowSelect = True
          ParentBiDiMode = False
          SmallImages = ImageList
          SortType = stText
          TabOrder = 1
          ViewStyle = vsReport
          OnAdvancedCustomDraw = ListViewAdvancedCustomDraw
          OnItemChecked = LanguagesItemChecked
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Options'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblImportMerge: TLabel
          AlignWithMargins = True
          Left = 37
          Top = 99
          Width = 382
          Height = 11
          Margins.Left = 37
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 24
          Align = alTop
          Caption = 
            'Use this option if you are going to add more domains and definit' +
            'ions to the current document'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -9
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object lblImportRevise: TLabel
          AlignWithMargins = True
          Left = 37
          Top = 162
          Width = 312
          Height = 11
          Margins.Left = 37
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 12
          Align = alTop
          Caption = 
            'Use this option to apply changes in the source code to the curre' +
            'nt document'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -9
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object Header4: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title4: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 144
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Choose Import Options'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle4: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 198
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'How the translations should be imported?'
            ShowAccelChar = False
          end
        end
        object rbImportRevise: TRadioButton
          AlignWithMargins = True
          Left = 22
          Top = 134
          Width = 414
          Height = 28
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Revise the currently active document with the imported items'
          Checked = True
          TabOrder = 2
          TabStop = True
          WordWrap = True
          OnClick = ImportTypeClick
        end
        object rbImportMerge: TRadioButton
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 414
          Height = 28
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 0
          Align = alTop
          Caption = 
            'Merge the imported items with the items of currently active docu' +
            'ment'
          TabOrder = 1
          WordWrap = True
          OnClick = ImportTypeClick
        end
        object ImportReviseSubOptions: TPanel
          AlignWithMargins = True
          Left = 22
          Top = 185
          Width = 414
          Height = 92
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 0
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          ParentColor = True
          ShowCaption = False
          TabOrder = 3
          object lblReviseKeepObsoletes: TLabel
            AlignWithMargins = True
            Left = 31
            Top = 28
            Width = 343
            Height = 11
            Margins.Left = 31
            Margins.Top = 0
            Margins.Right = 16
            Margins.Bottom = 10
            Align = alTop
            Caption = 
              'By selecting this option, the items no more present in the sourc' +
              'e will not be deleted'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -9
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object lblReviseDeleteObsoletes: TLabel
            AlignWithMargins = True
            Left = 31
            Top = 77
            Width = 327
            Height = 11
            Margins.Left = 31
            Margins.Top = 0
            Margins.Right = 16
            Margins.Bottom = 4
            Align = alTop
            Caption = 
              'By selecting this option, the items no more present in the sourc' +
              'e will be deleted'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -9
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object rbReviseKeepObsoletes: TRadioButton
            AlignWithMargins = True
            Left = 16
            Top = 0
            Width = 382
            Height = 28
            Margins.Left = 16
            Margins.Top = 0
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Keep the obsolete definitions'
            Checked = True
            TabOrder = 0
            TabStop = True
            WordWrap = True
          end
          object rbReviseDeleteObsoletes: TRadioButton
            AlignWithMargins = True
            Left = 16
            Top = 49
            Width = 382
            Height = 28
            Margins.Left = 16
            Margins.Top = 0
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Delete the obsolete definitions'
            TabOrder = 1
            WordWrap = True
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Before Process'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object LastNotice: TLabel
          AlignWithMargins = True
          Left = 22
          Top = 65
          Width = 405
          Height = 26
          Margins.Left = 22
          Margins.Top = 16
          Margins.Right = 22
          Margins.Bottom = 8
          Align = alTop
          Caption = 
            'Click <Import> to continue with the import, or click <Back> if y' +
            'ou want to review or change any settings.'
          ShowAccelChar = False
          WordWrap = True
        end
        object Header5: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title5: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 103
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Ready to Import'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle5: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 275
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Program is now ready to begin importing the translations.'
            ShowAccelChar = False
          end
        end
        object Summary: TRichEdit
          AlignWithMargins = True
          Left = 22
          Top = 99
          Width = 414
          Height = 255
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 22
          Align = alClient
          Font.Charset = ARABIC_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Processing'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object PleaseWait: TLabel
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 264
          Height = 13
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 4
          Align = alTop
          Caption = 'Please wait while the translations are being imported...'
          ShowAccelChar = False
          WordWrap = True
        end
        object Header6: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title6: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 62
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Importing'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle6: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 182
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'The translations are being imported...'
            ShowAccelChar = False
          end
        end
        object ProgressBar: TProgressBar
          AlignWithMargins = True
          Left = 22
          Top = 88
          Width = 414
          Height = 18
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 0
          Align = alTop
          DoubleBuffered = False
          ParentDoubleBuffered = False
          Style = pbstMarquee
          MarqueeInterval = 2
          TabOrder = 1
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'After Process'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object SuccessMsg: TLabel
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 371
          Height = 13
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 4
          Align = alTop
          Caption = 
            'The selected translation files have been imported in to the curr' +
            'ent document.'
          ShowAccelChar = False
          WordWrap = True
        end
        object Header7: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title7: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 99
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Congratulations'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle7: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 207
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'The import process completed successfully.'
            ShowAccelChar = False
          end
        end
      end
    end
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 376
    Width = 458
    Height = 42
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      458
      40)
    object btnFinish: TButton
      Left = 354
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Finish'
      Default = True
      ModalResult = 1
      TabOrder = 4
      Visible = False
    end
    object btnCancel: TButton
      Left = 354
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnStart: TButton
      Left = 253
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Import'
      Default = True
      TabOrder = 2
      Visible = False
      OnClick = btnStartClick
    end
    object btnBack: TButton
      Left = 152
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Back'
      TabOrder = 0
      Visible = False
      OnClick = btnBackClick
    end
    object btnNext: TButton
      Left = 253
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Next'
      Enabled = False
      TabOrder = 1
      OnClick = btnNextClick
    end
  end
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      '.Caption'
      'btnBack.Caption'
      'btnCancel.Caption'
      'btnFinish.Caption'
      'btnNext.Caption'
      'btnStart.Caption'
      'Domains.Columns[0].Caption'
      'Domains.Columns[1].Caption'
      'Domains.Columns[2].Caption'
      'Languages.Columns[0].Caption'
      'Languages.Columns[1].Caption'
      'LastNotice.Caption'
      'lblFolder.Caption'
      'lblImportMerge.Caption'
      'lblImportRevise.Caption'
      'lblReviseDeleteObsoletes.Caption'
      'lblReviseKeepObsoletes.Caption'
      'Notebook.Pages[0]'
      'Notebook.Pages[1]'
      'Notebook.Pages[2]'
      'Notebook.Pages[3]'
      'PleaseWait.Caption'
      'rbImportMerge.Caption'
      'rbImportRevise.Caption'
      'rbReviseDeleteObsoletes.Caption'
      'rbReviseKeepObsoletes.Caption'
      'Subtitle1.Caption'
      'Subtitle2.Caption'
      'Subtitle3.Caption'
      'Subtitle4.Caption'
      'Subtitle5.Caption'
      'Subtitle6.Caption'
      'Subtitle7.Caption'
      'SuccessMsg.Caption'
      'Title1.Caption'
      'Title2.Caption'
      'Title3.Caption'
      'Title4.Caption'
      'Title5.Caption'
      'Title6.Caption'
      'Title7.Caption')
    Translatables.Literals = (
      '3C4AC5B30DE29F343D7DF767B48B7B28')
    OnLoaded = TranslatorLoaded
    Left = 343
    Top = 7
  end
  object ImageList: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Height = 20
    Left = 374
    Top = 7
    Bitmap = {
      494C010102000800FC0110001400FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001400000001002000000000000014
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003535354C4D4D
      4D844D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D
      4D864D4D4D864D4D4D864D4D4D843535354C000000005151515CC9C9C9FFC9C9
      C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9
      C9FFC9C9C9FFCCCCCCFF5151515C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004D4D4D84EFEF
      EFF5FAFAFAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFAFAFAFDEDEDEDF34D4D4D8400000000CCCCCCFFF9F9F9FFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFF9F9F9FFCCCCCCFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000014F6054A2A0C7
      AAFF70AA7EFF599D6AFF71AB80FFA1C8AAFFD6E6DAFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFAFAFAFD4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000464F495D5C8C67DC72AA
      7AFFA5B48AFFBAB58CFF90B991FF589D6AFF77AD84FFC0D9C7FFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFDEDEDEFFDEDEDEFFDEDEDEFFD5D5D5FFD5D5D5FFD5D5D5FFD5D5D5FFD5D5
      D5FFFBFBFBFFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003236333C608E6CD17DC299FFAECA
      8DFFCEAD6CFFD9AB66FFA1A465FF6BB889FF68A67DFF76AC83FFD4E4D9FFFAFA
      FAFFFAFAFAFFFAFAFAFFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005A6F5F8E76B78CFF72C47AFFA9B4
      68FFD5A456FFE1A854FFBB974BFF89A054FF7AB187FF5A9B6BFFA0C7AAFFFAFA
      FAFFF8F8F8FFF8F8F8FFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFD5D5D5FFD5D5D5FFCCCCCCFFCCCCCCFFC9C9C9FFC9C9C9FFC9C9C9FFC9C9
      C9FFF8F8F8FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000060916DD88CD1A0FFAFD582FFC8AD
      61FFDBAA55FFD3A04EFFBF8843FFBF904DFF82A264FF79B995FF71AB81FFF9F9
      F9FFF9F9F9FFF8F8F8FFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF9F9F9FFF9F9
      F9FFF8F8F8FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000599D6BFE8EDB9EFF94D977FFCAC5
      81FFCDA557FFBFBB6EFFABCF6AFF9BB853FF62A450FF8EC8A8FF599D6AFFF9F9
      F9FFF6F6F6FFF6F6F6FFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFCCCCCCFFC9C9C9FFC9C9C9FFC2C2C2FFC2C2C2FFBBBBBBFFBBBBBBFFBBBB
      BBFFF8F8F8FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000061916ED893D7A2FFA0E78BFFD5EA
      BCFFCCB368FFC4C875FF7BDB59FF5DDA43FF5FC259FF87C19EFF71AA80FFF6F6
      F6FFF3F3F3FFF2F2F2FFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF8F8F8FFF2F2
      F2FFF2F2F2FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005A70608F84C29AFFBAF1AAFFC9EC
      B9FFD8D599FFD6BF78FFB6BB65FF92D66FFF8ADF90FF6AAB7EFF9EC4A7FFF2F2
      F2FFEFEFEFFFEDEDEDFFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFC9C9C9FFC2C2C2FFBBBBBBFFBBBBBBFFB3B3B3FFB3B3B3FFABABABFFABAB
      ABFFEBEBEBFFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003236333C61906ED2ADE2C1FFCDF2
      CFFFD4E4BDFFDBDBA2FFDAC283FFD1C28BFF99CAA1FF75AD84FFCDDDD2FFECEC
      ECFFEAEAEAFFE6E6E6FFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF2F2F2FFF2F2F2FFEBEBEBFFEBEB
      EBFFE4E4E4FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000464F495D5E8F6BDC90C3
      A2FFB8DABBFFD4DCB4FFB9CB9EFF83B281FF76AE85FFBAD4C1FFEBEBEBFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFF9F9
      F9FFC2C2C2FFBBBBBBFFB3B3B3FFABABABFFABABABFFABABABFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000014F6054A2A0C8
      AAFF70AB80FF599E6BFF71AB80FF9EC4A8FFCFDFD3FFF0F0F0FFEAEAEAFFFCFC
      FCFFF6F6F6FFF4F4F4FF6B6B6BBD2B2B2B3800000000C9C9C9FFFCFCFCFFF8F8
      F8FFF9F9F9FFF8F8F8FFF8F8F8FFF2F2F2FFF2F2F2FFEBEBEBFFFCFCFCFFF8F8
      F8FFF2F2F2FFC2C2C2FF5151515C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004D4D4D86F9F9
      F9FDF4F4F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFC
      FCFFE7E7E7FF6A6A6ABD2B2B2B380101010200000000C9C9C9FFFBFBFBFFF2F2
      F2FFF2F2F2FFF8F8F8FFF2F2F2FFF2F2F2FFEBEBEBFFEBEBEBFFFCFCFCFFE4E4
      E4FFC2C2C2FF5151515C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004D4D4D84E9E9
      E9F0F9F9F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8
      F8FF6A6A6ABD2B2B2B38010101020000000000000000CCCCCCFFF8F8F8FFFBFB
      FBFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FFC2C2
      C2FF5151515C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003535354B4D4D
      4D844D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D
      4D862B2B2B38010101020000000000000000000000005151515CCCCCCCFFC9C9
      C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FF5151
      515C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000140000000100010000000000A00000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      C000800100000000C00080010000000080008001000000008000800100000000
      0000800100000000000080010000000000008001000000000000800100000000
      0000800100000000000080010000000000008001000000008000800100000000
      8000800100000000C000800300000000C001800700000000C003800F00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object CheckImages: TImageList
    Height = 20
    Left = 405
    Top = 7
  end
end
