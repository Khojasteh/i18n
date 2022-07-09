object MainForm: TMainForm
  Left = 132
  Top = 140
  ActiveControl = LanguageBox
  BiDiMode = bdLeftToRight
  BorderWidth = 16
  Caption = 'i18n Package - Translation Demo'
  ClientHeight = 609
  ClientWidth = 706
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ParentBiDiMode = False
  Position = poScreenCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 706
    Height = 55
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 16
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      706
      55)
    object lblLanguageBox: TLabel
      Left = 0
      Top = 0
      Width = 124
      Height = 13
      Caption = 'User Interface Language:'
      FocusControl = LanguageBox
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 0
      Top = 39
      Width = 403
      Height = 16
      Caption = 
        'NOTE: Italian translation is the result of the Google translator' +
        '!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
    object LanguageBox: TCultureBox
      Left = 0
      Top = 14
      Width = 169
      Height = 21
      BiDiMode = bdLeftToRight
      Color = clWindow
      Flags = DM.Flags
      Items.Cultures = (
        '*')
      Localizer = DM.Localizer
      ParentBiDiMode = False
      TabOrder = 0
    end
    object BitBtn1: TBitBtn
      Left = 544
      Top = 8
      Width = 158
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'Calendar Controls'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333FFFFFFFFFFFFFFF000000000000000077777777777777770FF7FF7FF7FF
        7FF07FF7FF7FF7F37F3709F79F79F7FF7FF077F77F77F7FF7FF7077777777777
        777077777777777777770FF7FF7FF7FF7FF07FF7FF7FF7FF7FF709F79F79F79F
        79F077F77F77F77F77F7077777777777777077777777777777770FF7FF7FF7FF
        7FF07FF7FF7FF7FF7FF709F79F79F79F79F077F77F77F77F77F7077777777777
        777077777777777777770FFFFF7FF7FF7FF07F33337FF7FF7FF70FFFFF79F79F
        79F07FFFFF77F77F77F700000000000000007777777777777777CCCCCC8888CC
        CCCC777777FFFF777777CCCCCCCCCCCCCCCC7777777777777777}
      NumGlyphs = 2
      Spacing = 8
      TabOrder = 1
      WordWrap = True
      OnClick = BitBtn1Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 71
    Width = 706
    Height = 538
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Standard Controls'
      object Edit1: TEdit
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 666
        Height = 19
        Hint = 'TEdit'
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alTop
        TabOrder = 0
        Text = 
          #8220'Computers are useless.  They can only give you answers.'#8221' - Pabl' +
          'o Picasso'
      end
      object Memo1: TMemo
        AlignWithMargins = True
        Left = 16
        Top = 51
        Width = 666
        Height = 207
        Hint = 'TMemo'
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alClient
        Lines.Strings = (
          
            #8220'No matter how slick the demo is in rehearsal, when you do it in' +
            ' front of a live audience, the probability of a flawless present' +
            'ation is '
          
            'inversely proportional to the number of people watching, raised ' +
            'to the power of the amount of money involved.'#8221
          ''
          '- Mark Gibbs')
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object CheckBox1: TCheckBox
        AlignWithMargins = True
        Left = 16
        Top = 274
        Width = 666
        Height = 25
        Hint = 'TCheckBox'
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alBottom
        Caption = 
          'The best thing about a boolean is even if you are wrong, you are' +
          ' only off by a bit.'
        TabOrder = 2
        WordWrap = True
      end
      object ComboBox1: TComboBox
        AlignWithMargins = True
        Left = 16
        Top = 407
        Width = 666
        Height = 21
        Hint = 'TComboBox'
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alBottom
        Style = csDropDownList
        TabOrder = 3
        TextHint = 'Click to select...'
        Items.Strings = (
          
            #8220'Programming is like sex: one mistake and you'#8217're providing suppo' +
            'rt for a lifetime.'#8221' - Michael Sinz'
          
            #8220'Software is like sex: It'#8217's better when it'#8217's free.'#8221' - Linus Torv' +
            'alds'
          
            #8220'Code generation, like drinking alcohol, is good in moderation.'#8221 +
            ' - Alex Lowe ')
      end
      object ListBox1: TListBox
        AlignWithMargins = True
        Left = 16
        Top = 444
        Width = 666
        Height = 50
        Hint = 'TListBox'
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Style = lbOwnerDrawFixed
        Align = alBottom
        Items.Strings = (
          
            #8220'Before software should be reusable, it should be usable.'#8221' - Ral' +
            'ph Johnson'
          
            #8220'If you automate a mess, you get an automated mess.'#8221' - Rod Micha' +
            'el'
          
            #8220'If you have a procedure with ten parameters, you probably misse' +
            'd some.'#8221' - Alan Perlis ')
        TabOrder = 4
      end
      object RadioGroup1: TRadioGroup
        AlignWithMargins = True
        Left = 16
        Top = 315
        Width = 666
        Height = 76
        Hint = 'TRadioGroup'
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alBottom
        Caption = ' Bill Gates '
        Items.Strings = (
          #8220'640K ought to be enough for anybody.'#8221' (1991)'
          #8220'The Internet?  We are not interested in it.'#8221' (1993)'
          #8220'Two years from now, spam will be solved.'#8221' (2004)')
        TabOrder = 5
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Extra Controls'
      ImageIndex = 1
      object CheckListBox1: TCheckListBox
        AlignWithMargins = True
        Left = 16
        Top = 49
        Width = 666
        Height = 50
        Hint = 'TCheckListBox'
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alTop
        Items.Strings = (
          #8220'Any fool can use a computer.  Many do.'#8221' - Ted Nelson'
          
            #8220'To err is human, but to really foul things up you need a comput' +
            'er.'#8221' - Paul Ehrlich'
          #8220'To iterate is human, to recurse divine.'#8221' - L. Peter Deutsch')
        Style = lbOwnerDrawFixed
        TabOrder = 0
      end
      object ValueListEditor1: TValueListEditor
        AlignWithMargins = True
        Left = 16
        Top = 115
        Width = 666
        Height = 78
        Hint = 'TValueListEditor'
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 0
        Align = alTop
        Strings.Strings = (
          
            'Grady Booch='#8220'The function of good software is to make the comple' +
            'x appear to be simple.'#8221
          
            'Brian Kernigan='#8220'Controlling complexity is the essence of compute' +
            'r programming.'#8221
          
            'Jeff Pesis='#8220'Hardware: The parts of a computer system that can be' +
            ' kicked.'#8221)
        TabOrder = 1
        TitleCaptions.Strings = (
          'Name'
          'Quotation')
        ColWidths = (
          150
          512)
      end
      object Panel2: TPanel
        AlignWithMargins = True
        Left = 16
        Top = 209
        Width = 666
        Height = 285
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        ShowCaption = False
        TabOrder = 2
        object TreeView1: TTreeView
          Left = 0
          Top = 0
          Width = 177
          Height = 285
          Hint = 'TTreeView'
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alLeft
          HideSelection = False
          Indent = 19
          TabOrder = 0
          Items.NodeData = {
            03020000002E0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
            0002000000010843006F006D00700075007400650072002E0000000000000000
            000000FFFFFFFFFFFFFFFF000000000000000002000000010848006100720064
            007700610072006500380000000000000000000000FFFFFFFFFFFFFFFF000000
            000000000003000000010D49006E007000750074002000440065007600690063
            00650073002E0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
            000000000001084B006500790062006F00610072006400280000000000000000
            000000FFFFFFFFFFFFFFFF00000000000000000000000001054D006F00750073
            0065002C0000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
            00000001075300630061006E006E00650072003A0000000000000000000000FF
            FFFFFFFFFFFFFF000000000000000002000000010E4F00750074007000750074
            00200044006500760069006300650073002C0000000000000000000000FFFFFF
            FFFFFFFFFF00000000000000000000000001074D006F006E00690074006F0072
            002C0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
            0001075000720069006E007400650072002E0000000000000000000000FFFFFF
            FFFFFFFFFF000000000000000002000000010853006F00660074007700610072
            0065003E0000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
            00000001104F007000650072006100740069006E006700200053007900730074
            0065006D00360000000000000000000000FFFFFFFFFFFFFFFF00000000000000
            0000000000010C4100700070006C00690063006100740069006F006E00730026
            0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000200000001
            045500730065007200300000000000000000000000FFFFFFFFFFFFFFFF000000
            000000000000000000010941006E006F006E0079006D006F0075007300380000
            000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000010D41
            0075007400680065006E007400690063006100740065006400}
        end
        object ListView1: TListView
          AlignWithMargins = True
          Left = 193
          Top = 0
          Width = 473
          Height = 285
          Hint = 'TListView'
          Margins.Left = 16
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          Columns = <
            item
              Caption = 'Name'
              Width = 100
            end
            item
              AutoSize = True
              Caption = 'Quotation'
            end>
          HideSelection = False
          Items.ItemData = {
            056B0300000500000000000000FFFFFFFFFFFFFFFF01000000FFFFFFFF000000
            000C4900730061006100630020004100730069006D006F0076002F1C20490020
            0064006F0020006E006F00740020006600650061007200200063006F006D0070
            00750074006500720073002E00200049002000660065006100720020006C0061
            0063006B0020006F00660020007400680065006D002E001D2000000000000000
            00FFFFFFFFFFFFFFFF01000000FFFFFFFF000000000B45006D006F0020005000
            680069006C006900700073004E1C204100200063006F006D0070007500740065
            00720020006F006E00630065002000620065006100740020006D006500200061
            0074002000630068006500730073002C00200062007500740020006900740020
            0077006100730020006E006F0020006D006100740063006800200066006F0072
            0020006D00650020006100740020006B00690063006B00200062006F00780069
            006E0067002E001D200000000000000000FFFFFFFFFFFFFFFF01000000FFFFFF
            FF000000000A420069006C006C00200047006100740065007300441C20540068
            006500200063006F006D00700075007400650072002000770061007300200062
            006F0072006E00200074006F00200073006F006C00760065002000700072006F
            0062006C0065006D00730020007400680061007400200064006900640020006E
            006F00740020006500780069007300740020006200650066006F00720065002E
            001D200000000000000000FFFFFFFFFFFFFFFF01000000FFFFFFFF000000000F
            4E0061007400680061006E0020004D0079006800720076006F006C006400361C
            2053006F00660074007700610072006500200069007300200061002000670061
            0073003B00200069007400200065007800700061006E0064007300200074006F
            002000660069006C006C002000690074007300200063006F006E007400610069
            006E00650072002E001D200000000000000000FFFFFFFFFFFFFFFF01000000FF
            FFFFFF000000000F530074006500760065006E00200052002000470061007200
            6D0061006E002D1C205000680079007300690063007300200069007300200074
            0068006500200075006E006900760065007200730065001920730020006F0070
            00650072006100740069006E0067002000730079007300740065006D002E001D
            2000000000FFFFFFFFFFFFFFFFFFFF}
          TabOrder = 1
          ViewStyle = vsReport
          ExplicitHeight = 284
        end
      end
      object HeaderControl1: THeaderControl
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 666
        Height = 17
        Hint = 'THeaderControl'
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 0
        Sections = <
          item
            ImageIndex = -1
            Text = 'Red'
            Width = 100
          end
          item
            ImageIndex = -1
            Text = 'Green'
            Width = 100
          end
          item
            ImageIndex = -1
            Text = 'Blue'
            Width = 100
          end>
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Frame'
      ImageIndex = 2
      inline SampleFrame1: TSampleFrame
        Left = 0
        Top = 0
        Width = 698
        Height = 510
        Align = alClient
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        TabOrder = 0
        ExplicitWidth = 698
        ExplicitHeight = 510
        inherited RichEdit1: TRichEdit
          Width = 665
          ExplicitWidth = 665
        end
        inherited RichEdit2: TRichEdit
          Width = 665
          ExplicitWidth = 665
        end
        inherited Translator_of_SampleFrame: TTranslator
          Translatables.Properties = (
            'RichEdit1.Lines.Text'
            'RichEdit2.Lines.Text')
        end
      end
    end
  end
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      '.Caption'
      'BitBtn1.Caption'
      'CheckBox1.Caption'
      'CheckListBox1.Items[0]'
      'CheckListBox1.Items[1]'
      'CheckListBox1.Items[2]'
      'ComboBox1.Items[0]'
      'ComboBox1.Items[1]'
      'ComboBox1.Items[2]'
      'ComboBox1.TextHint'
      'Edit1.Text'
      'HeaderControl1.Sections[0].Text'
      'HeaderControl1.Sections[1].Text'
      'HeaderControl1.Sections[2].Text'
      'lblLanguageBox.Caption'
      'ListBox1.Items[0]'
      'ListBox1.Items[1]'
      'ListBox1.Items[2]'
      'ListView1.Columns[0].Caption'
      'ListView1.Columns[1].Caption'
      'ListView1.Items[0].Caption'
      'ListView1.Items[0].SubItems[0]'
      'ListView1.Items[1].Caption'
      'ListView1.Items[1].SubItems[0]'
      'ListView1.Items[2].Caption'
      'ListView1.Items[2].SubItems[0]'
      'ListView1.Items[3].Caption'
      'ListView1.Items[3].SubItems[0]'
      'ListView1.Items[4].Caption'
      'ListView1.Items[4].SubItems[0]'
      'Memo1.Lines.Text'
      'RadioGroup1.Caption'
      'RadioGroup1.Items[0]'
      'RadioGroup1.Items[1]'
      'RadioGroup1.Items[2]'
      'TabSheet1.Caption'
      'TabSheet2.Caption'
      'TabSheet3.Caption'
      'TreeView1.Items[0].Text'
      'TreeView1.Items[0][0].Text'
      'TreeView1.Items[0][0][0].Text'
      'TreeView1.Items[0][0][0][0].Text'
      'TreeView1.Items[0][0][0][1].Text'
      'TreeView1.Items[0][0][0][2].Text'
      'TreeView1.Items[0][0][1].Text'
      'TreeView1.Items[0][0][1][0].Text'
      'TreeView1.Items[0][0][1][1].Text'
      'TreeView1.Items[0][1].Text'
      'TreeView1.Items[0][1][0].Text'
      'TreeView1.Items[0][1][1].Text'
      'TreeView1.Items[1].Text'
      'TreeView1.Items[1][0].Text'
      'TreeView1.Items[1][1].Text'
      'ValueListEditor1.Strings[0]'
      'ValueListEditor1.Strings[1]'
      'ValueListEditor1.Strings[2]'
      'ValueListEditor1.TitleCaptions[0]'
      'ValueListEditor1.TitleCaptions[1]')
    Translatables.Literals = (
      '9CBCFC526C6D6BC0DF5F4FE8092A869F')
    Left = 464
    Top = 16
  end
end
