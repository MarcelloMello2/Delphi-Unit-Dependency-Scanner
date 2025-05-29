object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Settings'
  ClientHeight = 645
  ClientWidth = 1179
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object pcSettings: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1173
    Height = 611
    ActivePage = tab_Defines
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 569
    object tabRootFiles: TTabSheet
      Caption = 'Root Files'
      object Panel4: TPanel
        Left = 1052
        Top = 0
        Width = 113
        Height = 459
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitHeight = 417
        object btnAddFile: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 107
          Height = 25
          Align = alTop
          Caption = 'Add Root Files'
          TabOrder = 0
          OnClick = btnAddFileClick
        end
        object chkIncludeProjectSearchPaths: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 65
          Width = 107
          Height = 40
          Align = alTop
          Caption = 'Automatically add Project Search Paths'
          Checked = True
          State = cbChecked
          TabOrder = 2
          WordWrap = True
        end
        object btnScanForProjects: TButton
          AlignWithMargins = True
          Left = 3
          Top = 34
          Width = 107
          Height = 25
          Align = alTop
          Caption = 'Scan for Projects'
          TabOrder = 1
          OnClick = btnScanForProjectsClick
        end
      end
      object pnlFTSInfo: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 462
        Width = 1159
        Height = 118
        Align = alBottom
        BevelOuter = bvNone
        Color = clGray
        ParentBackground = False
        TabOrder = 2
        ExplicitTop = 420
        object Panel14: TPanel
          AlignWithMargins = True
          Left = 1
          Top = 1
          Width = 1157
          Height = 116
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          BevelOuter = bvNone
          Color = 12910591
          ParentBackground = False
          TabOrder = 0
          object lnkFTSInfo: TLabel
            AlignWithMargins = True
            Left = 1
            Top = 1
            Width = 1155
            Height = 114
            Margins.Left = 1
            Margins.Top = 1
            Margins.Right = 1
            Margins.Bottom = 1
            Align = alClient
            Caption = 
              'Enter at least one root file. The following files types are supp' +
              'orted:'#13#10'-  Pascal Files (.pas)'#13#10'-  Delphi Projects (.dpr)'#13#10'-  De' +
              'lphi Packages (.dpk)'#13#10'-  Group Projects(.groupproj).  '#13#10'Each of ' +
              'the root file directories will be searched for matching units. A' +
              'dd additional paths under the Search Paths tab.'#13#10#13#10'Empty lines w' +
              'ill be ignored. "//" Comments will be ignored.'
            Color = 14548991
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = True
            WordWrap = True
            ExplicitWidth = 603
            ExplicitHeight = 104
          end
        end
      end
      object memRootFiles: TRichEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 1046
        Height = 453
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
        OnChange = OnSettingChange
        ExplicitHeight = 411
      end
    end
    object tabSearchPaths: TTabSheet
      Caption = 'Search Paths'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 1005
        Top = 0
        Width = 160
        Height = 471
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitHeight = 429
        object btnAddPath: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 154
          Height = 25
          Align = alTop
          Caption = 'Add Search Paths'
          TabOrder = 0
          OnClick = btnAddPathClick
        end
        object chkRecursive: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 34
          Width = 154
          Height = 15
          Hint = 
            'This will add also subfolders when you pick a path with "Add Sea' +
            'rch Paths".'#13#10#13#10'If you want to perform recursion at scan-time, ad' +
            'd <+> behind the path.'
          Align = alTop
          Caption = 'Add recursive to settings'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          WordWrap = True
        end
      end
      object memSearchPaths: TRichEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 999
        Height = 465
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
        OnChange = OnSettingChange
        ExplicitHeight = 423
      end
      object Panel3: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 474
        Width = 1159
        Height = 106
        Align = alBottom
        BevelOuter = bvNone
        Color = clGray
        ParentBackground = False
        TabOrder = 2
        ExplicitTop = 432
        object Panel5: TPanel
          AlignWithMargins = True
          Left = 1
          Top = 1
          Width = 1157
          Height = 104
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          BevelOuter = bvNone
          Color = 12910591
          ParentBackground = False
          TabOrder = 0
          object Label1: TLabel
            AlignWithMargins = True
            Left = 1
            Top = 1
            Width = 1155
            Height = 102
            Margins.Left = 1
            Margins.Top = 1
            Margins.Right = 1
            Margins.Bottom = 1
            Align = alClient
            Caption = 
              'Each of the directories will be searched for matching files (*.p' +
              'as, *.dpr, *.dpk). Directories of root files are searched automa' +
              'tically.'#13#10'Add <+> behind a search path to perform a recusive sea' +
              'rch in subdirectories. '#13#10'E.g. C:\MyProject\source<+>'#13#10#13#10'Search p' +
              'aths will also be used to resolve includes ({$I ...}, {$INCLUDE ' +
              '...}).'#13#10#13#10'Empty lines will be ignored. "//" Comments will be ign' +
              'ored.'
            Color = 14548991
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = True
            WordWrap = True
            ExplicitWidth = 661
            ExplicitHeight = 91
          end
        end
      end
    end
    object tab_Defines: TTabSheet
      Caption = 'Compiler Switches'
      ImageIndex = 6
      object memDefines: TRichEdit
        AlignWithMargins = True
        Left = 196
        Top = 3
        Width = 966
        Height = 423
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnChange = OnSettingChange
        ExplicitHeight = 407
      end
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 193
        Height = 429
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitHeight = 413
        object chk_UseDefines: TCheckBox
          AlignWithMargins = True
          Left = 15
          Top = 15
          Width = 175
          Height = 15
          Hint = 
            'This will add also subfolders when you pick a path with "Add Sea' +
            'rch Paths".'#13#10#13#10'If you want to perform recursion at scan-time, ad' +
            'd <+> behind the path.'
          Margins.Left = 15
          Margins.Top = 15
          Align = alTop
          Caption = 'Uso DefineS'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          WordWrap = True
          OnClick = OnSettingChange
        end
      end
      object Panel7: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 432
        Width = 1159
        Height = 148
        Align = alBottom
        BevelOuter = bvNone
        Color = clGray
        ParentBackground = False
        TabOrder = 2
        object Panel8: TPanel
          AlignWithMargins = True
          Left = 1
          Top = 1
          Width = 1157
          Height = 146
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          BevelOuter = bvNone
          Color = 12910591
          ParentBackground = False
          TabOrder = 0
          ExplicitHeight = 120
          object lbl_DefinesHint: TLabel
            AlignWithMargins = True
            Left = 1
            Top = 1
            Width = 1155
            Height = 144
            Margins.Left = 1
            Margins.Top = 1
            Margins.Right = 1
            Margins.Bottom = 1
            Align = alClient
            Caption = 
              'DICA: N'#195'O h'#225' defini'#231#245'es internas, ent'#227'o voc'#234' precisa definir, po' +
              'r exemplo, a vers'#227'o do compilador tamb'#233'm - por exemplo, VER330 p' +
              'ara Delphi 10.3 Rio. Consulte https://docwiki.embarcadero.com/RA' +
              'DStudio/en/Conditional_compilation_(Delphi)'#13#10#13#10'Ativar "Usar Dire' +
              'tivas" produzir'#225' um resultado de an'#225'lise sint'#225'tica semelhante ao' +
              ' de um compilador - os switches devem estar ativos ou n'#227'o ativos' +
              '. O c'#243'digo dentro de um switch inativo ser'#225' ignorado.'#13#10#13#10'Desativ' +
              'ar "Usar Diretivas" tentar'#225' ignorar todas as "Diretivas" e anali' +
              'sar todo o c'#243'digo - para ver as depend'#234'ncias de uso que est'#227'o "o' +
              'cultas" nos switches do compilador.'#13#10'CUIDADO: Isso s'#243' funciona q' +
              'uando o c'#243'digo ainda '#233' v'#225'lido - portanto, depende do "estilo de ' +
              'codifica'#231#227'o" de uso dos switches.'#13#10#13#10'Linhas em branco ser'#227'o igno' +
              'radas. Coment'#225'rios "//" ser'#227'o ignorados.'
            Color = 14548991
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = True
            WordWrap = True
            ExplicitWidth = 1027
            ExplicitHeight = 117
          end
        end
      end
    end
    object tabModules: TTabSheet
      Caption = 'Modules'
      ImageIndex = 5
      object edt_ModulesDefinitionFile: TLabeledEdit
        Left = 208
        Top = 17
        Width = 585
        Height = 21
        EditLabel.Width = 173
        EditLabel.Height = 21
        EditLabel.Caption = 'Arquivo de Defini'#231#227'o de M'#243'dulos'
        LabelPosition = lpLeft
        LabelSpacing = 7
        TabOrder = 0
        Text = ''
        OnChange = edt_ModulesDefinitionFileChange
      end
    end
    object tabUnitScopes: TTabSheet
      Caption = 'Unit Scope Names'
      ImageIndex = 4
      object memUnitScopeNames: TRichEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 1159
        Height = 577
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Winapi'
          'System.Win'
          'Data.Win'
          'Datasnap.Win'
          'Web.Win'
          'Soap.Win'
          'Xml.Win'
          'Bde'
          'System'
          'Xml'
          'Data'
          'Datasnap'
          'Web'
          'Soap'
          'Vcl'
          'Vcl.Imaging'
          'Vcl.Touch'
          'Vcl.Samples'
          'Vcl.Shell')
        ParentFont = False
        PlainText = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnChange = OnSettingChange
        ExplicitHeight = 535
      end
    end
    object tabScan: TTabSheet
      Caption = 'Scan'
      ImageIndex = 3
      object chkLinkUnits: TCheckBox
        Left = 16
        Top = 16
        Width = 233
        Height = 17
        Caption = 'Link Units that are found multiple times'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = OnSettingChange
      end
      object chkRunScanOnLoad: TCheckBox
        Left = 16
        Top = 39
        Width = 233
        Height = 17
        Caption = 'Start the scan when a project is loaded'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
    object tabEnvironment: TTabSheet
      Caption = 'Environment'
      ImageIndex = 2
      object chkLoadLastProject: TCheckBox
        Left = 16
        Top = 16
        Width = 233
        Height = 17
        Caption = 'Open last Project on Startup'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 617
    Width = 1179
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 575
    object lblStatus: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 5
      Width = 1014
      Height = 20
      Margins.Top = 5
      Align = alClient
      AutoSize = False
      EllipsisPosition = epPathEllipsis
      ExplicitLeft = 112
      ExplicitTop = 8
      ExplicitWidth = 33
      ExplicitHeight = 13
    end
    object btnOK: TButton
      AlignWithMargins = True
      Left = 1023
      Top = 0
      Width = 75
      Height = 25
      Margins.Top = 0
      Margins.Right = 0
      Align = alRight
      Caption = '&OK'
      Default = True
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 1101
      Top = 0
      Width = 75
      Height = 25
      Margins.Top = 0
      Align = alRight
      Cancel = True
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.pas'
    Filter = 
      'Delphi Files (*.pas;*.dpr;*.dpk;*.groupproj)|*.pas;*.dpr;*.dpk;*' +
      '.groupproj|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Select Root File'
    Left = 120
    Top = 168
  end
  object popDelphiPaths: TPopupMenu
    Left = 209
    Top = 168
    object miWindowsPaths: TMenuItem
      Caption = 'Browse for Path...'
      OnClick = miWindowsPathsClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miDelphiXE1: TMenuItem
      Tag = 15
      Caption = 'Delphi XE'
      OnClick = miDelphiXE3Click
    end
    object miDelphiXE2: TMenuItem
      Tag = 16
      Caption = 'Delphi XE2'
      OnClick = miDelphiXE3Click
    end
    object miDelphiXE3: TMenuItem
      Tag = 17
      Caption = 'Delphi XE3'
      OnClick = miDelphiXE3Click
    end
    object miDelphiXE41: TMenuItem
      Tag = 18
      Caption = 'Delphi XE4'
      OnClick = miDelphiXE3Click
    end
    object DelphiXE51: TMenuItem
      Tag = 19
      Caption = 'Delphi XE5'
      OnClick = miDelphiXE3Click
    end
    object DelphiXE61: TMenuItem
      Tag = 20
      Caption = 'Delphi XE6'
      OnClick = miDelphiXE3Click
    end
    object DelphiXE71: TMenuItem
      Tag = 21
      Caption = 'Delphi XE7'
      OnClick = miDelphiXE3Click
    end
  end
end
