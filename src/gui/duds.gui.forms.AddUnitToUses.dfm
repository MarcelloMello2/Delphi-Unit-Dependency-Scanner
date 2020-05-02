object frmAddNewUnit: TfrmAddNewUnit
  Left = 0
  Top = 0
  ActiveControl = edtNewName
  Caption = 'Add uses to all units that currently use "xyz"'
  ClientHeight = 783
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 427
    Height = 70
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      427
      70)
    object Label1: TLabel
      Left = 8
      Top = 7
      Width = 114
      Height = 13
      Caption = 'Add this unit to uses list'
    end
    object chkDummyRun: TCheckBox
      Left = 8
      Top = 44
      Width = 248
      Height = 17
      Caption = 'Dummy Run - No files will be updated'
      TabOrder = 0
    end
    object edtNewName: TEdit
      Left = 8
      Top = 21
      Width = 415
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edtNewNameChange
      OnKeyPress = edtNewNameKeyPress
    end
  end
  object pnlButtons: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 745
    Width = 427
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 759
    object btnOK: TButton
      Left = 142
      Top = 8
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 222
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object infoRenameCSV: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 586
    Width = 427
    Height = 153
    Align = alBottom
    BevelOuter = bvNone
    Color = clGray
    ParentBackground = False
    TabOrder = 2
    ExplicitLeft = -2
    object Panel5: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 421
      Height = 147
      Align = alClient
      BevelOuter = bvNone
      Color = 12910591
      ParentBackground = False
      TabOrder = 0
      ExplicitTop = 4
      ExplicitHeight = 151
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 415
        Height = 141
        Align = alClient
        Caption = 
          'This will insert the defined unit as a uses entry for every unit' +
          ' that currently uses the marked unit. '#13#10#13#10'The new unit will only' +
          ' be added if not already referenced. '#13#10'The new unit will be adde' +
          'd directly BEFORE the marked unit name. '#13#10#13#10'E.g. Mark Unit "func' +
          'tions.misc". Call this dialog and type "functions.basic.text" in' +
          'to the edit above. Every unit that currently uses "functions.mis' +
          'c" but not "functions.basic.text" will use "functions.basic.text' +
          '" afterwards.'
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
        ExplicitWidth = 411
        ExplicitHeight = 117
      end
    end
  end
  object pnlOnlyForUnits: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 79
    Width = 427
    Height = 501
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 279
    ExplicitHeight = 194
    object Label3: TLabel
      Left = 8
      Top = 7
      Width = 266
      Height = 39
      Caption = 
        'Restrict operation to units. One unit name a line. '#13#10'Names can c' +
        'ontain ".pas" endings, they will be ignored.'#13#10'(if left empty, al' +
        'l units will be processed)'
    end
    object mem_OnlyApplyToUnits: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 50
      Width = 421
      Height = 448
      Margins.Top = 50
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 160
      ExplicitTop = 88
      ExplicitWidth = 185
      ExplicitHeight = 89
    end
  end
end
