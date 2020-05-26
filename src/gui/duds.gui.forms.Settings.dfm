object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Settings'
  ClientHeight = 603
  ClientWidth = 1179
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pcSettings: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1173
    Height = 569
    ActivePage = tab_Defines
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 1110
    object tabRootFiles: TTabSheet
      Caption = 'Root Files'
      ExplicitWidth = 1102
      object Panel4: TPanel
        Left = 1052
        Top = 0
        Width = 113
        Height = 417
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 989
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
        Top = 420
        Width = 1159
        Height = 118
        Align = alBottom
        BevelOuter = bvNone
        Color = clGray
        ParentBackground = False
        TabOrder = 2
        ExplicitWidth = 1096
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
          ExplicitWidth = 1094
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
        Height = 411
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
        Zoom = 100
        OnChange = OnSettingChange
        ExplicitWidth = 983
      end
    end
    object tabSearchPaths: TTabSheet
      Caption = 'Search Paths'
      ImageIndex = 1
      ExplicitWidth = 1102
      object Panel2: TPanel
        Left = 1005
        Top = 0
        Width = 160
        Height = 429
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 942
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
        TabOrder = 1
        WordWrap = False
        Zoom = 100
        OnChange = OnSettingChange
        ExplicitWidth = 936
      end
      object Panel3: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 432
        Width = 1159
        Height = 106
        Align = alBottom
        BevelOuter = bvNone
        Color = clGray
        ParentBackground = False
        TabOrder = 2
        ExplicitWidth = 1096
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
          ExplicitWidth = 1094
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
      ExplicitWidth = 1102
      object memDefines: TRichEdit
        AlignWithMargins = True
        Left = 196
        Top = 3
        Width = 966
        Height = 407
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
        Zoom = 100
        OnChange = OnSettingChange
        ExplicitWidth = 903
      end
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 193
        Height = 413
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
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
          Caption = 'Use Defines'
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
        Top = 416
        Width = 1159
        Height = 122
        Align = alBottom
        BevelOuter = bvNone
        Color = clGray
        ParentBackground = False
        TabOrder = 2
        ExplicitWidth = 1096
        object Panel8: TPanel
          AlignWithMargins = True
          Left = 1
          Top = 1
          Width = 1157
          Height = 120
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          BevelOuter = bvNone
          Color = 12910591
          ParentBackground = False
          TabOrder = 0
          ExplicitWidth = 1094
          object lbl_DefinesHint: TLabel
            AlignWithMargins = True
            Left = 1
            Top = 1
            Width = 1155
            Height = 118
            Margins.Left = 1
            Margins.Top = 1
            Margins.Right = 1
            Margins.Bottom = 1
            Align = alClient
            Caption = 
              'HINT: There are NO built-in defines, so you have to set e.g. the' +
              ' compiler version define also - e.g. VER330 for Delphi 10.3 Rio.' +
              ' See http://docwiki.embarcadero.com/RADStudio/Rio/en/Conditional' +
              '_compilation_(Delphi)'#13#10#13#10'Activating "Use Directives" will produc' +
              'e a parsing result like a compiler behaves - switches must eithe' +
              'r be Active or no Active. Code inside an inactive switch will be' +
              ' ignored.'#13#10#13#10'Not activating "Use Directives" will try to ignore ' +
              'all "Directives" and parse all the code - in order to see uses d' +
              'ependencies that are "hidden"  in compiler switches. '#13#10'CAUTION: ' +
              'This only works, when the code is still valid - so it depends on' +
              ' the "coding style" of using switches.'#13#10#13#10'Empty lines will be ig' +
              'nored. "//" Comments will be ignored.'
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
            ExplicitWidth = 878
            ExplicitHeight = 117
          end
        end
      end
    end
    object tabModules: TTabSheet
      Caption = 'Modules'
      ImageIndex = 5
      ExplicitWidth = 1102
      object edt_ModulesDefinitionFile: TLabeledEdit
        Left = 144
        Top = 17
        Width = 585
        Height = 21
        EditLabel.Width = 121
        EditLabel.Height = 13
        EditLabel.Caption = 'Modules Definition File'
        LabelPosition = lpLeft
        LabelSpacing = 7
        TabOrder = 0
        OnChange = edt_ModulesDefinitionFileChange
      end
    end
    object tabUnitScopes: TTabSheet
      Caption = 'Unit Scope Names'
      ImageIndex = 4
      ExplicitWidth = 1102
      object memUnitScopeNames: TRichEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 1159
        Height = 535
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
        Zoom = 100
        OnChange = OnSettingChange
        ExplicitWidth = 1096
      end
    end
    object tabScan: TTabSheet
      Caption = 'Scan'
      ImageIndex = 3
      ExplicitWidth = 1102
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
      ExplicitWidth = 1102
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
    Top = 575
    Width = 1179
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 1116
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
      ExplicitLeft = 960
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
      ExplicitLeft = 1038
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
