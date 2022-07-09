object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'i18n Package - DB Controls'
  ClientHeight = 716
  ClientWidth = 705
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox4: TGroupBox
    Left = 8
    Top = 12
    Width = 225
    Height = 213
    Caption = ' Field: Territory_GeoID '
    TabOrder = 2
    DesignSize = (
      225
      213)
    object DBText4: TDBText
      Left = 86
      Top = 185
      Width = 123
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      DataField = 'Territory_GeoID'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
      ExplicitWidth = 115
    end
    object Label4: TLabel
      Left = 18
      Top = 185
      Width = 62
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'RAW VALUE:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
    end
    object DBTerritoryLabel1: TDBTerritoryLabel
      Left = 18
      Top = 166
      Width = 191
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = '<Invalid Value>'
      DataField = 'Territory_GeoID'
      DataSource = DataSource1
      Flags = FlagImageList1
    end
    object DBTerritoryBox1: TDBTerritoryBox
      Left = 16
      Top = 24
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clWindow
      DataField = 'Territory_GeoID'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Territories = (
        '*')
      Sorted = True
      TabOrder = 0
    end
    object DBTerritoryListBox1: TDBTerritoryListBox
      Left = 16
      Top = 51
      Width = 193
      Height = 109
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clWindow
      DataField = 'Territory_GeoID'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Territories = (
        '*')
      Sorted = True
      TabOrder = 1
    end
  end
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 8
    Top = 572
    Width = 689
    Height = 136
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabStop = False
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Width = 40
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Territory_GeoID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Territory_Code2'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Territory_Code3'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Culture_LocaleID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Culture_LocaleName'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Currency_IntlSymbol'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TheDate'
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    AlignWithMargins = True
    Left = 467
    Top = 512
    Width = 225
    Height = 25
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    DataSource = DataSource1
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
    Kind = dbnHorizontal
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 450
    Width = 456
    Height = 111
    Caption = ' Field: TheDate '
    TabOrder = 8
    DesignSize = (
      456
      111)
    object DBIntlDateTimeLabel1: TDBIntlDateTimeLabel
      Left = 18
      Top = 58
      Width = 191
      Height = 13
      DataField = 'TheDate'
      DataSource = DataSource1
      DateTimeFormat = 'dddddd'
    end
    object DBText1: TDBText
      Left = 86
      Top = 83
      Width = 123
      Height = 17
      Anchors = [akLeft, akBottom]
      DataField = 'TheDate'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
    end
    object Label1: TLabel
      Left = 18
      Top = 83
      Width = 62
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'RAW VALUE:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
    end
    object Label8: TLabel
      Left = 248
      Top = 58
      Width = 192
      Height = 38
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = 
        'Use the above list to change the calender system of the date pic' +
        'ker control.'
      WordWrap = True
    end
    object DBIntlDatePicker1: TDBIntlDatePicker
      Left = 18
      Top = 31
      Width = 191
      Height = 21
      DataField = 'TheDate'
      DataSource = DataSource1
      DateFormat = 'ddddd'
      ShowCheckBox = True
      TabOrder = 0
    end
    object CalendarSystems: TComboBox
      Left = 247
      Top = 32
      Width = 193
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akRight]
      TabOrder = 1
      OnSelect = CalendarSystemsSelect
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 231
    Width = 225
    Height = 213
    Caption = ' Field: Culture_LocaleID '
    TabOrder = 5
    DesignSize = (
      225
      213)
    object DBText2: TDBText
      Left = 86
      Top = 185
      Width = 123
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      DataField = 'Culture_LocaleID'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
      ExplicitWidth = 115
    end
    object Label2: TLabel
      Left = 18
      Top = 185
      Width = 62
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'RAW VALUE:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
    end
    object DBCultureLabel1: TDBCultureLabel
      Left = 16
      Top = 166
      Width = 193
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = '<Invalid Value>'
      DataField = 'Culture_LocaleID'
      DataSource = DataSource1
      Flags = FlagImageList1
    end
    object DBCultureBox1: TDBCultureBox
      Left = 16
      Top = 24
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clWindow
      DataField = 'Culture_LocaleID'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Cultures = (
        '*')
      Sorted = True
      TabOrder = 0
    end
    object DBCultureListBox1: TDBCultureListBox
      Left = 16
      Top = 51
      Width = 193
      Height = 109
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clWindow
      DataField = 'Culture_LocaleID'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Cultures = (
        '*')
      Sorted = True
      TabOrder = 1
    end
  end
  object GroupBox5: TGroupBox
    Left = 239
    Top = 12
    Width = 225
    Height = 213
    Caption = ' Field: Territory_Code2 '
    TabOrder = 3
    DesignSize = (
      225
      213)
    object DBText5: TDBText
      Left = 86
      Top = 185
      Width = 123
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      DataField = 'Territory_Code2'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
      ExplicitWidth = 115
    end
    object Label5: TLabel
      Left = 18
      Top = 185
      Width = 62
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'RAW VALUE:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
    end
    object DBTerritoryLabel2: TDBTerritoryLabel
      Left = 18
      Top = 166
      Width = 191
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = '<Invalid Value>'
      DataField = 'Territory_Code2'
      DataSource = DataSource1
      Flags = FlagImageList1
    end
    object DBTerritoryBox2: TDBTerritoryBox
      Left = 16
      Top = 24
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clWindow
      DataField = 'Territory_Code2'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Territories = (
        '*')
      Sorted = True
      TabOrder = 0
    end
    object DBTerritoryListBox2: TDBTerritoryListBox
      Left = 16
      Top = 51
      Width = 193
      Height = 109
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clWindow
      DataField = 'Territory_Code2'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Territories = (
        '*')
      Sorted = True
      TabOrder = 1
    end
  end
  object GroupBox6: TGroupBox
    Left = 470
    Top = 12
    Width = 225
    Height = 213
    Caption = ' Field: Territory_Code3 '
    TabOrder = 4
    DesignSize = (
      225
      213)
    object DBText6: TDBText
      Left = 86
      Top = 185
      Width = 123
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      DataField = 'Territory_Code3'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
      ExplicitWidth = 115
    end
    object Label6: TLabel
      Left = 18
      Top = 185
      Width = 62
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'RAW VALUE:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
    end
    object DBTerritoryLabel3: TDBTerritoryLabel
      Left = 18
      Top = 166
      Width = 191
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = '<Invalid Value>'
      DataField = 'Territory_Code3'
      DataSource = DataSource1
      Flags = FlagImageList1
    end
    object DBTerritoryBox3: TDBTerritoryBox
      Left = 16
      Top = 24
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clWindow
      DataField = 'Territory_Code3'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Territories = (
        '*')
      Sorted = True
      TabOrder = 0
    end
    object DBTerritoryListBox3: TDBTerritoryListBox
      Left = 16
      Top = 51
      Width = 193
      Height = 109
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clWindow
      DataField = 'Territory_Code3'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Territories = (
        '*')
      Sorted = True
      TabOrder = 1
    end
  end
  object GroupBox7: TGroupBox
    Left = 470
    Top = 231
    Width = 225
    Height = 213
    Caption = ' Field: Currency_IntlSymbol '
    TabOrder = 7
    DesignSize = (
      225
      213)
    object DBText7: TDBText
      Left = 86
      Top = 185
      Width = 123
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      DataField = 'Currency_IntlSymbol'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
      ExplicitWidth = 115
    end
    object Label7: TLabel
      Left = 18
      Top = 185
      Width = 62
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'RAW VALUE:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
    end
    object DBCurrencyLabel1: TDBCurrencyLabel
      Left = 16
      Top = 166
      Width = 193
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = '<Invalid Value>'
      DataField = 'Currency_IntlSymbol'
      DataSource = DataSource1
      Flags = FlagImageList1
    end
    object DBCurrencyListBox1: TDBCurrencyListBox
      Left = 16
      Top = 51
      Width = 193
      Height = 109
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clWindow
      DataField = 'Currency_IntlSymbol'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Currencies = (
        '*')
      Sorted = True
      TabOrder = 1
    end
    object DBCurrencyBox1: TDBCurrencyBox
      Left = 16
      Top = 24
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clWindow
      DataField = 'Currency_IntlSymbol'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Currencies = (
        '*')
      Sorted = True
      TabOrder = 0
    end
  end
  object GroupBox3: TGroupBox
    Left = 239
    Top = 231
    Width = 225
    Height = 213
    Caption = ' Field: Culture_LocaleName '
    TabOrder = 6
    DesignSize = (
      225
      213)
    object DBText3: TDBText
      Left = 86
      Top = 185
      Width = 123
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      DataField = 'Culture_LocaleName'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
      ExplicitWidth = 115
    end
    object Label3: TLabel
      Left = 18
      Top = 185
      Width = 62
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'RAW VALUE:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 85
    end
    object DBCultureLabel2: TDBCultureLabel
      Left = 16
      Top = 166
      Width = 193
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = '<Invalid Value>'
      DataField = 'Culture_LocaleName'
      DataSource = DataSource1
      Flags = FlagImageList1
    end
    object DBCultureBox2: TDBCultureBox
      Left = 16
      Top = 24
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clWindow
      DataField = 'Culture_LocaleName'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Cultures = (
        '*')
      Sorted = True
      TabOrder = 0
    end
    object DBCultureListBox2: TDBCultureListBox
      Left = 16
      Top = 51
      Width = 193
      Height = 109
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clWindow
      DataField = 'Culture_LocaleName'
      DataSource = DataSource1
      Flags = FlagImageList1
      Items.Cultures = (
        '*')
      Sorted = True
      TabOrder = 1
    end
  end
  object ReadonlyDataSet: TCheckBox
    Left = 470
    Top = 484
    Width = 65
    Height = 17
    Caption = 'Readonly'
    TabOrder = 9
    OnClick = ReadonlyDataSetClick
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 120
    Top = 656
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    FileName = 'Table.dat'
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faReadonly]
        DataType = ftAutoInc
      end
      item
        Name = 'Culture_LocaleID'
        DataType = ftInteger
      end
      item
        Name = 'Culture_LocaleName'
        DataType = ftString
        Size = 10
      end
      item
        Name = 'Territory_GeoID'
        DataType = ftInteger
      end
      item
        Name = 'Territory_Code2'
        DataType = ftString
        Size = 2
      end
      item
        Name = 'Territory_Code3'
        DataType = ftString
        Size = 3
      end
      item
        Name = 'Currency_IntlSymbol'
        DataType = ftString
        Size = 3
      end
      item
        Name = 'TheDate'
        DataType = ftDate
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 48
    Top = 656
    object ClientDataSet1ID: TAutoIncField
      FieldName = 'ID'
    end
    object ClientDataSet1Culture_LocaleID: TIntegerField
      FieldName = 'Culture_LocaleID'
    end
    object ClientDataSet1Culture_LocaleName: TStringField
      FieldName = 'Culture_LocaleName'
      Size = 10
    end
    object ClientDataSet1Territory_GeoID: TIntegerField
      FieldName = 'Territory_GeoID'
    end
    object ClientDataSet1Territory_Code2: TStringField
      FieldName = 'Territory_Code2'
      Size = 2
    end
    object ClientDataSet1Territory_Code3: TStringField
      FieldName = 'Territory_Code3'
      Size = 3
    end
    object ClientDataSet1Currency_IntlSymbol: TStringField
      FieldName = 'Currency_IntlSymbol'
      Size = 3
    end
    object ClientDataSet1TheDate: TDateField
      FieldName = 'TheDate'
    end
  end
  object FlagImageList1: TFlagImageList
    Left = 192
    Top = 656
  end
end
