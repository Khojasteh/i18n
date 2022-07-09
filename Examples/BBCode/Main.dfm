object Form1: TForm1
  Left = 0
  Top = 0
  BorderWidth = 16
  Caption = 'BBCodes'
  ClientHeight = 608
  ClientWidth = 658
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 658
    Height = 16
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Align = alTop
    Caption = 'BBCode Text:'
    FocusControl = Memo1
    ExplicitWidth = 77
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 0
    Top = 336
    Width = 658
    Height = 16
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Align = alTop
    Caption = 'Rich Text:'
    FocusControl = RichEdit1
    ExplicitTop = 333
    ExplicitWidth = 58
  end
  object Splitter1: TSplitter
    Left = 0
    Top = 292
    Width = 658
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = 8
    ExplicitTop = 289
  end
  object RichEdit1: TRichEdit
    AlignWithMargins = True
    Left = 0
    Top = 355
    Width = 658
    Height = 253
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    Font.Charset = ARABIC_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    ExplicitTop = 352
    ExplicitHeight = 256
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 0
    Top = 19
    Width = 658
    Height = 273
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    Lines.Strings = (
      
        'Using [link]BBCodes.Apply[/link] class method, you can use [b][b' +
        'ackcolor=yellow]BBCode[/backcolor][/b] styled tags to format tex' +
        't of a [color=green][b][u=red,dotted]rich edit[/u][/b][/color] c' +
        'ontrol.'
      ''
      'For example, you can:'
      ''
      '[list]'
      
        'Alter [size=14]size[/size] and [font=Impact]face name[/font] of ' +
        'font to use for displaying text.'
      
        'Markup text as [b]bold[/b], [i]italic[/i], [s]strikeout[/s], [u=' +
        'wave,blue]underline[/u] in different color and styles. '
      
        'Change [color=#008040]foreground[/color] and [backcolor=moneygre' +
        'en]background[/backcolor] color of text.'
      
        'Use [sup]superscript[/sup] and [sub]subscript[/sub] to display t' +
        'ext.'
      '[/list]'
      ''
      
        'You can also use BBCode tags to display bulleted (like above) an' +
        'd numbered lists.'
      ''
      'Here is an example of a numbered list:'
      ''
      '[list=1]'
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit.'
      'Phasellus posuere nunc eget quam aliquam eu ornare sem pulvinar.'
      
        'Etiam a sem non nibh vulputate sollicitudin interdum blandit nib' +
        'h.'
      
        'Etiam tempus augue a neque malesuada non tristique tortor venena' +
        'tis.'
      'Nullam vel tellus ut felis varius tempus ut vulputate metus.'
      
        'Aenean luctus nulla varius justo tincidunt sed accumsan odio ele' +
        'ifend.'
      '[/list]'
      ''
      'The following list is also numbered, but using Roman numbers:'
      ''
      '[list=I,simple,@18]'
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit.'
      'Phasellus posuere nunc eget quam aliquam eu ornare sem pulvinar.'
      
        'Etiam a sem non nibh vulputate sollicitudin interdum blandit nib' +
        'h.'
      
        'Etiam tempus augue a neque malesuada non tristique tortor venena' +
        'tis.'
      'Nullam vel tellus ut felis varius tempus ut vulputate metus.'
      
        'Aenean luctus nulla varius justo tincidunt sed accumsan odio ele' +
        'ifend.'
      '[/list]'
      ''
      
        'Here is another numbered list that uses letters for numberig, an' +
        'd starting from '#39'hi'#39':'
      ''
      '[list=hi,period]'
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit.'
      'Phasellus posuere nunc eget quam aliquam eu ornare sem pulvinar.'
      
        'Etiam a sem non nibh vulputate sollicitudin interdum blandit nib' +
        'h.'
      
        'Etiam tempus augue a neque malesuada non tristique tortor venena' +
        'tis.'
      'Nullam vel tellus ut felis varius tempus ut vulputate metus.'
      
        'Aenean luctus nulla varius justo tincidunt sed accumsan odio ele' +
        'ifend.'
      '[/list]'
      ''
      
        'Some BBCode tags affect the whole paragraph. Here are some examp' +
        'les:'
      ''
      
        '[align=justify][b]This paragraph is justified.[/b] Lorem ipsum d' +
        'olor sit amet, consectetur adipiscing elit. Curabitur fringilla ' +
        'egestas eleifend. Donec id turpis a nulla interdum viverra. In a' +
        'ccumsan venenatis ipsum ut blandit. Nullam gravida luctus tortor' +
        ', quis interdum elit accumsan vel. Nam a velit at augue elementu' +
        'm tincidunt ut id erat. Donec in quam id quam placerat lacinia v' +
        'el eget massa. Pellentesque ullamcorper elementum diam. Aenean s' +
        'odales egestas tortor, eget tincidunt sapien consectetur at. Ali' +
        'quam volutpat risus vel sem imperdiet laoreet. Ut sollicitudin d' +
        'ignissim leo nec molestie. Mauris varius ultrices urna ac suscip' +
        'it. Pellentesque vitae lectus non ipsum pulvinar lobortis. Pelle' +
        'ntesque et nulla ac massa elementum condimentum non quis mi.[/al' +
        'ign]'
      ''
      
        '[center][b]And this one is centered.[/b] Lorem ipsum dolor sit a' +
        'met, consectetur adipiscing elit. Curabitur fringilla egestas el' +
        'eifend. Donec id turpis a nulla interdum viverra. In accumsan ve' +
        'nenatis ipsum ut blandit. Nullam gravida luctus tortor, quis int' +
        'erdum elit accumsan vel. Nam a velit at augue elementum tincidun' +
        't ut id erat. Donec in quam id quam placerat lacinia vel eget ma' +
        'ssa. Pellentesque ullamcorper elementum diam. Aenean sodales ege' +
        'stas tortor, eget tincidunt sapien consectetur at. Aliquam volut' +
        'pat risus vel sem imperdiet laoreet. Ut sollicitudin dignissim l' +
        'eo nec molestie. Mauris varius ultrices urna ac suscipit. Pellen' +
        'tesque vitae lectus non ipsum pulvinar lobortis. Pellentesque et' +
        ' nulla ac massa elementum condimentum non quis mi.[/center]'
      ''
      
        '[indent=80][b]This one is indented 80 points.[/b] Lorem ipsum do' +
        'lor sit amet, consectetur adipiscing elit. Curabitur fringilla e' +
        'gestas eleifend. Donec id turpis a nulla interdum viverra. In ac' +
        'cumsan venenatis ipsum ut blandit. Nullam gravida luctus tortor,' +
        ' quis interdum elit accumsan vel. Nam a velit at augue elementum' +
        ' tincidunt ut id erat. Donec in quam id quam placerat lacinia ve' +
        'l eget massa. Pellentesque ullamcorper elementum diam. Aenean so' +
        'dales egestas tortor, eget tincidunt sapien consectetur at. Aliq' +
        'uam volutpat risus vel sem imperdiet laoreet. Ut sollicitudin di' +
        'gnissim leo nec molestie. Mauris varius ultrices urna ac suscipi' +
        't. Pellentesque vitae lectus non ipsum pulvinar lobortis. Pellen' +
        'tesque et nulla ac massa elementum condimentum non quis mi.[/ind' +
        'ent]'
      ''
      
        '[rindent=80][b]And this one is [u]right[/u] indented 80 points.[' +
        '/b] Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cur' +
        'abitur fringilla egestas eleifend. Donec id turpis a nulla inter' +
        'dum viverra. In accumsan venenatis ipsum ut blandit. Nullam grav' +
        'ida luctus tortor, quis interdum elit accumsan vel. Nam a velit ' +
        'at augue elementum tincidunt ut id erat. Donec in quam id quam p' +
        'lacerat lacinia vel eget massa. Pellentesque ullamcorper element' +
        'um diam. Aenean sodales egestas tortor, eget tincidunt sapien co' +
        'nsectetur at. Aliquam volutpat risus vel sem imperdiet laoreet. ' +
        'Ut sollicitudin dignissim leo nec molestie. Mauris varius ultric' +
        'es urna ac suscipit. Pellentesque vitae lectus non ipsum pulvina' +
        'r lobortis. Pellentesque et nulla ac massa elementum condimentum' +
        ' non quis mi.[/rindent]')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 295
    Width = 658
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    ExplicitTop = 292
    DesignSize = (
      658
      41)
    object btnApply: TButton
      Left = 199
      Top = 6
      Width = 260
      Height = 29
      Anchors = []
      Caption = 'Convert to Rich Text'
      TabOrder = 0
      OnClick = btnApplyClick
    end
  end
end
